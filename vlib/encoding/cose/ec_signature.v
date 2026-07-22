// Conversion between the DER-encoded ECDSA signatures produced by
// vlib/crypto/ecdsa (which delegates to OpenSSL) and the fixed-width
// `R || S` form mandated by COSE for ES256/ES384/ES512 (RFC 9053 §2.1).
//
// Wire format reminder (ASN.1 DER, X9.62 §5.4):
//
//     SEQUENCE {                       0x30 LEN
//         INTEGER R                    0x02 R_LEN R_BYTES
//         INTEGER S                    0x02 S_LEN S_BYTES
//     }
//
// R and S are non-negative big-endian integers. DER requires the
// shortest encoding, so:
//   - a leading 0x00 octet is present iff the next octet's MSB is 1
//     (otherwise the value would be interpreted as negative);
//   - leading 0x00 octets that are not required for sign disambiguation
//     are forbidden.
//
// COSE raw form is just `R_padded || S_padded` where each integer is
// left-padded with zeros to `coordinate_size` bytes (32 / 48 / 66).
module cose

// der_to_raw converts a DER-encoded ECDSA signature into the
// `R || S` fixed-width representation used by COSE.
fn der_to_raw(der []u8, coord_size int) ![]u8 {
	if der.len < 8 || der[0] != 0x30 {
		return MalformedMessage{
			reason: 'ECDSA DER: missing SEQUENCE tag'
		}
	}
	seq_len, body_start := read_der_length(der, 1)!
	if body_start + seq_len != der.len {
		return MalformedMessage{
			reason: 'ECDSA DER: SEQUENCE length mismatch'
		}
	}
	r, after_r := read_der_integer(der, body_start)!
	s, after_s := read_der_integer(der, after_r)!
	if after_s != der.len {
		return MalformedMessage{
			reason: 'ECDSA DER: trailing bytes'
		}
	}

	mut out := []u8{len: 2 * coord_size}
	pad_left(mut out, 0, coord_size, r)!
	pad_left(mut out, coord_size, coord_size, s)!
	return out
}

// raw_to_der converts a `R || S` COSE signature back to the DER form
// expected by vlib/crypto/ecdsa for verification.
fn raw_to_der(raw []u8, coord_size int) ![]u8 {
	if raw.len != 2 * coord_size {
		return MalformedMessage{
			reason: 'ECDSA raw: wrong length, expected ${2 * coord_size} got ${raw.len}'
		}
	}
	r := raw[..coord_size]
	s := raw[coord_size..]
	r_int := encode_der_integer(r)
	s_int := encode_der_integer(s)
	body_len := r_int.len + s_int.len

	mut out := []u8{cap: 4 + body_len}
	out << 0x30
	out << encode_der_length(body_len)
	out << r_int
	out << s_int
	return out
}

// read_der_length parses an ASN.1 length encoding starting at `idx` and
// returns (length, next_index). Supports both the short form (0..127)
// and the long form (0x81..0x84 prefix).
fn read_der_length(buf []u8, idx int) !(int, int) {
	if idx >= buf.len {
		return MalformedMessage{
			reason: 'DER: truncated length'
		}
	}
	first := buf[idx]
	if first < 0x80 {
		return int(first), idx + 1
	}
	n := int(first & 0x7f)
	if n == 0 || n > 4 {
		return MalformedMessage{
			reason: 'DER: invalid long-form length'
		}
	}
	if idx + 1 + n > buf.len {
		return MalformedMessage{
			reason: 'DER: truncated long-form length'
		}
	}
	mut length := u32(0)
	for i in 0 .. n {
		length = (length << u32(8)) | u32(buf[idx + 1 + i])
	}
	return int(length), idx + 1 + n
}

// encode_der_length emits an ASN.1 length octet sequence. Used only for
// the SEQUENCE wrapper; the inner INTEGER lengths produced by
// `encode_der_integer` are always small enough for the short form.
fn encode_der_length(n int) []u8 {
	if n < 0x80 {
		return [u8(n)]
	}
	if n <= 0xff {
		return [u8(0x81), u8(n)]
	}
	if n <= 0xffff {
		return [u8(0x82), u8(n >> 8), u8(n)]
	}
	// P-521 max body is ~140 bytes — the >0xffff arms are never reached
	// in practice but kept defensive for completeness.
	return [u8(0x83), u8(n >> 16), u8(n >> 8), u8(n)]
}

// read_der_integer parses one INTEGER TLV at `idx`, returning
// (raw magnitude bytes with leading sign-disambiguation 0x00 stripped,
// next_index after the TLV).
fn read_der_integer(buf []u8, idx int) !([]u8, int) {
	if idx + 2 > buf.len || buf[idx] != 0x02 {
		return MalformedMessage{
			reason: 'DER: missing INTEGER tag'
		}
	}
	length, body_start := read_der_length(buf, idx + 1)!
	if length <= 0 || body_start + length > buf.len {
		return MalformedMessage{
			reason: 'DER: bad INTEGER length'
		}
	}
	end := body_start + length
	mut start := body_start
	// Drop the leading 0x00 inserted purely to keep the value positive.
	if length > 1 && buf[start] == 0x00 && (buf[start + 1] & 0x80) != 0 {
		start++
	}
	return buf[start..end].clone(), end
}

// encode_der_integer wraps `value` as an ASN.1 INTEGER TLV. Leading
// zeros in the magnitude are stripped, then a sign-disambiguation 0x00
// is prepended if the first remaining byte has its MSB set.
fn encode_der_integer(value []u8) []u8 {
	mut start := 0
	for start < value.len - 1 && value[start] == 0x00 {
		start++
	}
	payload := value[start..].clone()
	mut out := []u8{cap: 2 + payload.len + 1}
	out << 0x02
	if payload.len > 0 && (payload[0] & 0x80) != 0 {
		out << u8(payload.len + 1)
		out << 0x00
		out << payload
	} else {
		out << u8(payload.len)
		out << payload
	}
	return out
}

// pad_left writes `value` into `out[off..off+width]` right-aligned, with
// zero left-padding. Errors out if `value` is too long for the width
// (which would mean the input integer doesn't fit the curve). Assumes
// `out` is already zero-filled (true for `[]u8{len: N}` allocations).
fn pad_left(mut out []u8, off int, width int, value []u8) ! {
	if value.len > width {
		return MalformedMessage{
			reason: 'ECDSA: integer larger than curve coordinate (${value.len} > ${width})'
		}
	}
	pad := width - value.len
	for i in 0 .. value.len {
		out[off + pad + i] = value[i]
	}
}
