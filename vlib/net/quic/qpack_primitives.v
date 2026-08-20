module quic

// This file implements the two QPACK wire primitives RFC 9204 §4.1 reuses
// from HPACK (RFC 7541 §5.1/§5.2) "unmodified", generalized to the variable
// prefix widths QPACK uses that HPACK never needed (as narrow as 2 bits).
// The algorithm is verified against this codebase's own already-shipped,
// tested HPACK implementation (`h2_hpack.v`'s `read_int`/`h2_hpack_write_int`)
// before being written here, not reinvented from the RFC text alone.

// qpack_max_prefixed_int is this implementation's own decode limit for a
// QPACK prefixed integer (RFC 9204 §4.1.1 requires support up to and
// including 62 bits; §7.4 requires SOME implementation limit be chosen so
// that oversized values cannot create security weaknesses). Matches this
// project's own QUIC varint range (`varint.v`'s `max_varint`) rather than
// picking an unrelated arbitrary bound.
pub const qpack_max_prefixed_int = u64(0x3fff_ffff_ffff_ffff) // 2^62 - 1

// qpack_max_string_literal_len bounds a single decoded QPACK string
// literal's on-wire (encoded) length (RFC 9204 §7.4). 1 MiB is far larger
// than any real HTTP field value while still being a small, fixed cost for
// a malicious peer to force allocated.
pub const qpack_max_string_literal_len = 1 << 20

// qpack_err_need_more_data is a private sentinel error code marking "buf
// does not yet hold enough bytes" (RFC 9204's resumable-parsing
// requirement -- encoder/decoder-stream data can arrive fragmented across
// QUIC STREAM frames). Every OTHER error these primitives return
// represents genuinely malformed data (an integer/string exceeding this
// implementation's limits, or invalid Huffman coding) and MUST be
// propagated as a real error by any caller distinguishing the two cases
// -- conflating them let a malformed encoder-stream instruction be
// silently retried forever instead of raising QPACK_ENCODER_STREAM_ERROR
// (RFC 9204 §2.2.3, §3.2.2) and closing the connection. Self-found via
// GPT-5.6 Luna review, Phase-R reproduced before this fix.
const qpack_err_need_more_data = 1

// decode_prefixed_int decodes an RFC 7541 §5.1 prefixed integer (reused
// unmodified by RFC 9204 §4.1.1) whose low `prefix_bits` bits of `buf[0]`
// carry the initial value, with any continuation bytes following. The
// caller owns whatever flag/pattern bits occupy the high `8-prefix_bits`
// bits of that same first byte -- this function only ever reads the low
// bits and continuation bytes. Returns the decoded value and the number of
// bytes consumed.
pub fn decode_prefixed_int(buf []u8, prefix_bits int) !(u64, int) {
	if buf.len == 0 {
		return error_with_code('qpack: prefixed integer truncated', qpack_err_need_more_data)
	}
	max_prefix := u64((1 << prefix_bits) - 1)
	mut value := u64(buf[0]) & max_prefix
	if value < max_prefix {
		return value, 1
	}
	mut consumed := 1
	mut m := 0
	for {
		if consumed >= buf.len {
			return error_with_code('qpack: prefixed integer continuation truncated',
				qpack_err_need_more_data)
		}
		b := buf[consumed]
		consumed++
		value += u64(b & 0x7f) << u32(m)
		m += 7
		// Checked well before u64 could actually wrap (worst case ~2^63 at
		// m=56, still far under u64::max) -- see this file's module doc. This
		// is genuinely malformed data, not truncation: no amount of
		// additional buffered bytes makes an already-over-limit value valid.
		if value > qpack_max_prefixed_int || m > 63 {
			return error('qpack: prefixed integer exceeds implementation limit')
		}
		if b & 0x80 == 0 {
			break
		}
	}
	return value, consumed
}

// encode_prefixed_int appends an RFC 7541 §5.1 prefixed integer to `out`,
// using a prefix of `prefix_bits` bits whose high bits are pre-set via
// `high_bits` (the caller's pattern/flag bits, already shifted into
// position -- mirrors `h2_hpack_write_int`'s signature).
pub fn encode_prefixed_int(mut out []u8, value u64, prefix_bits int, high_bits u8) {
	max_prefix := u64((1 << prefix_bits) - 1)
	if value < max_prefix {
		out << high_bits | u8(value)
		return
	}
	out << high_bits | u8(max_prefix)
	mut v := value - max_prefix
	for v >= 0x80 {
		out << u8((v & 0x7f) | 0x80)
		v >>= 7
	}
	out << u8(v)
}

// decode_prefixed_string decodes an RFC 7541 §5.2 string literal (reused,
// generalized to a mid-byte start, by RFC 9204 §4.1.2): a Huffman flag bit
// immediately above the length's own `length_prefix_bits`-bit prefix
// integer, followed by that many bytes of (possibly Huffman-coded) string
// data. `length_prefix_bits` is the number written in each wire figure's
// "Length (N+)" annotation (e.g. 7 for a byte-aligned value, 5 or 3 for a
// name sharing its first byte with preceding flag bits) -- the caller
// already knows this from which representation it is decoding, the same
// division of labor as `decode_prefixed_int`'s `prefix_bits`. Returns the
// decoded string, whether it was Huffman-coded, and bytes consumed.
pub fn decode_prefixed_string(buf []u8, length_prefix_bits int) !(string, bool, int) {
	if buf.len == 0 {
		return error_with_code('qpack: string literal truncated', qpack_err_need_more_data)
	}
	huffman := (buf[0] >> u32(length_prefix_bits)) & 1 != 0
	// decode_prefixed_int's own `!` propagation preserves whichever error
	// (and code) it raised, truncated or malformed alike -- no re-wrapping
	// needed here to keep that distinction intact.
	length, len_consumed := decode_prefixed_int(buf, length_prefix_bits)!
	if length > u64(qpack_max_string_literal_len) {
		return error('qpack: string literal exceeds implementation limit')
	}
	n := int(length)
	if n > buf.len - len_consumed {
		// The declared length is itself within limits; we simply don't have
		// all of it buffered yet -- genuine truncation, not malformed data.
		return error_with_code('qpack: string literal length exceeds buffer',
			qpack_err_need_more_data)
	}
	raw := buf[len_consumed..len_consumed + n]
	consumed := len_consumed + n
	if huffman {
		// `raw` is already a complete, length-validated slice at this point,
		// so any failure qpack_huffman_decode raises is real malformed data,
		// never truncation.
		decoded := qpack_huffman_decode(raw)!
		return decoded.bytestr(), true, consumed
	}
	return raw.bytestr(), false, consumed
}

// encode_prefixed_string appends an RFC 7541 §5.2 string literal to `out`
// (RFC 9204 §4.1.2), choosing Huffman coding when it is shorter than the
// raw bytes, using an `length_prefix_bits`-bit length prefix (see
// `decode_prefixed_string` for what that width means). `high_bits` carries
// any preceding flag/pattern bits the caller needs set in the same first
// byte, NOT including the Huffman flag itself (this function sets that bit
// on its own, at position `length_prefix_bits`).
pub fn encode_prefixed_string(mut out []u8, s string, length_prefix_bits int, high_bits u8) {
	raw := s.bytes()
	huff := qpack_huffman_encode(raw)
	if huff.len < raw.len {
		encode_prefixed_int(mut out, u64(huff.len), length_prefix_bits,
			high_bits | (u8(1) << u32(length_prefix_bits)))
		out << huff
	} else {
		encode_prefixed_int(mut out, u64(raw.len), length_prefix_bits, high_bits)
		out << raw
	}
}
