module pkcs12

// der_length decodes a definite DER length at `pos` (which points at the
// first length byte). Returns the number of length bytes and the content
// length. Every access is bounds-checked; callers get errors, never panics.
fn der_read_length(data []u8, pos int) !(int, int) {
	if pos < 0 || pos >= data.len {
		return error('pkcs12: truncated length')
	}
	first := data[pos]
	if first < 0x80 {
		return 1, int(first)
	}
	n := int(first & 0x7f)
	if n == 0 || n > 4 {
		return error('pkcs12: bad length encoding')
	}
	if n > data.len - pos - 1 {
		return error('pkcs12: truncated length bytes')
	}
	if data[pos + 1] == 0 {
		return error('pkcs12: non-minimal length')
	}
	mut length := u64(0)
	for i in 1 .. n + 1 {
		if pos + i < 0 || pos + i >= data.len {
			return error('pkcs12: truncated length bytes')
		}
		length = length * 256 + u64(data[pos + i])
	}
	if length < 128 {
		return error('pkcs12: non-minimal length')
	}
	if length > u64(data.len - pos - 1 - n) {
		return error('pkcs12: length exceeds input')
	}
	if length > u64(16 * 1024 * 1024) {
		return error('pkcs12: length too large')
	}
	return 1 + n, int(length)
}

// tlv_total_len returns tag+length+content bytes starting at `pos`.
fn tlv_total_len(data []u8, pos int) !int {
	if pos < 0 || pos >= data.len || data.len - pos < 2 {
		return error('pkcs12: truncated TLV header')
	}
	// High-tag-number form is not used by PKCS#12; reject it.
	if data[pos] & 0x1f == 0x1f {
		return error('pkcs12: unsupported high-tag-number')
	}
	len_len, content_len := der_read_length(data, pos + 1)!
	header_len := 1 + len_len
	if header_len > data.len - pos || content_len > data.len - pos - header_len {
		return error('pkcs12: truncated TLV content')
	}
	return header_len + content_len
}

// split_tlv splits one TLV at `pos` into its full bytes and the next position.
fn split_tlv(data []u8, pos int) !([]u8, int) {
	total := tlv_total_len(data, pos)!
	if pos < 0 || total <= 0 || pos + total > data.len {
		return error('pkcs12: bad TLV range')
	}
	return data[pos..pos + total], pos + total
}

// skip_tlv returns the position just past the TLV at `pos`.
fn skip_tlv(data []u8, pos int) !int {
	return pos + tlv_total_len(data, pos)!
}

// tlv_content returns the tag, content bytes and total TLV length at `pos`.
fn tlv_content(data []u8, pos int) !(u8, []u8, int) {
	total := tlv_total_len(data, pos)!
	len_len, content_len := der_read_length(data, pos + 1)!
	header_len := 1 + len_len
	if pos + header_len + content_len != pos + total {
		return error('pkcs12: length mismatch')
	}
	tag := data[pos]
	content := data[pos + header_len..pos + total]
	return tag, content, total
}

// expect_tag returns the content of the TLV at `pos` when its tag matches.
fn expect_tag(data []u8, pos int, tag u8, what string) ![]u8 {
	if pos < 0 || pos >= data.len {
		return error('pkcs12: expected ${what}: truncated')
	}
	if data[pos] != tag {
		return error('pkcs12: expected ${what}')
	}
	_, content, _ := tlv_content(data, pos)!
	return content
}

// expect_tag_full returns tag, content and total length.
fn expect_tag_full(data []u8, pos int, tag u8, what string) !(u8, []u8, int) {
	if pos < 0 || pos >= data.len {
		return error('pkcs12: expected ${what}: truncated')
	}
	if data[pos] != tag {
		return error('pkcs12: expected ${what}')
	}
	return tlv_content(data, pos)!
}

// --- DER encoding helpers (writers, no failure modes) ---

fn der_encode_length(n int) []u8 {
	if n < 0 {
		return [u8(0)]
	}
	if n < 128 {
		return [u8(n)]
	}
	mut tmp := []u8{}
	mut v := n
	for v > 0 {
		tmp.prepend(u8(v & 0xff))
		v >>= 8
	}
	mut out := [u8(0x80 | tmp.len)]
	out << tmp
	return out
}

fn der_tlv(tag u8, content []u8) []u8 {
	mut out := [tag]
	out << der_encode_length(content.len)
	out << content
	return out
}

fn der_seq(content []u8) []u8 {
	return der_tlv(0x30, content)
}

fn der_set(content []u8) []u8 {
	return der_tlv(0x31, content)
}

fn der_octet_string(content []u8) []u8 {
	return der_tlv(0x04, content)
}

fn der_null() []u8 {
	return [u8(0x05), 0x00]
}

fn der_integer_from_int(n int) []u8 {
	if n < 0 {
		return der_tlv(0x02, [u8(0)])
	}
	if n == 0 {
		return der_tlv(0x02, [u8(0)])
	}
	mut tmp := []u8{}
	mut v := n
	for v > 0 {
		tmp.prepend(u8(v & 0xff))
		v >>= 8
	}
	if tmp[0] & 0x80 != 0 {
		tmp.prepend(0)
	}
	return der_tlv(0x02, tmp)
}

// der_integer_bytes encodes already-minimal positive integer bytes.
fn der_integer_bytes(content []u8) []u8 {
	if content.len == 0 {
		return der_tlv(0x02, [u8(0)])
	}
	mut c := content.clone()
	for c.len > 1 && c[0] == 0 && (c[1] & 0x80) == 0 {
		c = c[1..]
	}
	if c[0] & 0x80 != 0 {
		c.prepend(0)
	}
	return der_tlv(0x02, c)
}

// der_parse_integer parses a DER INTEGER content into an int with guards.
fn der_parse_integer(content []u8) !int {
	if content.len == 0 || content.len > 4 {
		return error('pkcs12: bad integer length')
	}
	if content[0] & 0x80 != 0 {
		return error('pkcs12: negative integer')
	}
	if content.len > 1 && content[0] == 0 && (content[1] & 0x80) == 0 {
		return error('pkcs12: non-minimal integer')
	}
	mut v := 0
	for b in content {
		v = v * 256 + int(b)
	}
	return v
}

// oid_encode converts dotted form to DER content bytes.
fn oid_encode(s string) ![]u8 {
	parts := s.split('.')
	if parts.len < 2 {
		return error('pkcs12: bad OID ${s}')
	}
	mut nums := []u64{}
	for p in parts {
		if p.len == 0 {
			return error('pkcs12: bad OID ${s}')
		}
		for c in p.bytes() {
			if c < `0` || c > `9` {
				return error('pkcs12: bad OID ${s}')
			}
		}
		v := p.u64()
		// u64() returns 0 on failure; disambiguate '0' itself.
		if v == 0 && p != '0' {
			return error('pkcs12: bad OID ${s}')
		}
		nums << v
	}
	if nums[0] > 2 || (nums[0] < 2 && nums[1] >= 40) {
		return error('pkcs12: bad OID arcs ${s}')
	}
	mut out := []u8{}
	out << encode_base128(nums[0] * 40 + nums[1])
	for i in 2 .. nums.len {
		out << encode_base128(nums[i])
	}
	return out
}

fn encode_base128(v u64) []u8 {
	mut vv := v
	if vv == 0 {
		return [u8(0)]
	}
	mut tmp := []u8{}
	for vv > 0 {
		tmp.prepend(u8(vv & 0x7f))
		vv >>= 7
	}
	for i in 0 .. tmp.len - 1 {
		tmp[i] |= 0x80
	}
	return tmp
}

// oid_decode converts DER content bytes to dotted form.
fn oid_decode(b []u8) !string {
	if b.len == 0 {
		return error('pkcs12: empty OID')
	}
	mut parts := []string{}
	mut value := u64(0)
	mut beginning := true
	for by in b {
		if beginning && by == 0x80 {
			return error('pkcs12: nonminimal OID arc')
		}
		if value > (u64(0xffffffffffffffff) - u64(by & 0x7f)) / 128 {
			return error('pkcs12: OID arc overflow')
		}
		value = value * 128 + u64(by & 0x7f)
		beginning = by & 0x80 == 0
		if beginning {
			if parts.len == 0 {
				if value < 40 {
					parts << '0'
					parts << value.str()
				} else if value < 80 {
					parts << '1'
					parts << (value - 40).str()
				} else {
					parts << '2'
					parts << (value - 80).str()
				}
			} else {
				parts << value.str()
			}
			value = 0
		}
	}
	if !beginning {
		return error('pkcs12: truncated OID')
	}
	return parts.join('.')
}

fn der_oid_tlv(s string) ![]u8 {
	return der_tlv(0x06, oid_encode(s)!)
}

// bmpstring_encode encodes a V UTF-8 string as BMPString content (UTF-16BE).
// Characters outside the BMP become surrogate pairs; lone surrogates in the
// input cannot occur because V strings are UTF-8.
fn bmpstring_encode(s string) []u8 {
	mut out := []u8{}
	runes := s.runes()
	for r in runes {
		v := u32(r)
		if v < 0x10000 {
			// Reject surrogates that could arise from invalid input.
			if v >= 0xd800 && v <= 0xdfff {
				continue
			}
			out << u8(v >> 8)
			out << u8(v & 0xff)
		} else if v <= 0x10ffff {
			w := v - 0x10000
			hi := u32(0xd800) + (w >> 10)
			lo := u32(0xdc00) + (w & 0x3ff)
			out << u8(hi >> 8)
			out << u8(hi & 0xff)
			out << u8(lo >> 8)
			out << u8(lo & 0xff)
		}
	}
	return out
}

// bmpstring_decode decodes BMPString content to a V string.
fn bmpstring_decode(b []u8) !string {
	if b.len % 2 != 0 {
		return error('pkcs12: invalid BMPString length')
	}
	mut out := []rune{}
	mut i := 0
	for i < b.len {
		if i + 1 >= b.len {
			return error('pkcs12: truncated BMPString')
		}
		hi := u32(b[i]) << 8 | u32(b[i + 1])
		i += 2
		if hi >= 0xd800 && hi <= 0xdbff {
			if i + 1 >= b.len {
				return error('pkcs12: truncated surrogate')
			}
			lo := u32(b[i]) << 8 | u32(b[i + 1])
			i += 2
			if lo < 0xdc00 || lo > 0xdfff {
				return error('pkcs12: invalid low surrogate')
			}
			cp := 0x10000 + ((hi - 0xd800) << 10) + (lo - 0xdc00)
			out << rune(cp)
		} else if hi >= 0xdc00 && hi <= 0xdfff {
			return error('pkcs12: lone low surrogate')
		} else {
			out << rune(hi)
		}
	}
	return out.string()
}

// password_to_ucs2 converts a password to PKCS#12 BMP bytes with terminator.
// Empty passwords encode as the two-byte BMP terminator (matching OpenSSL:
// `openssl pkcs12 -export -passout pass:` verifies only with 0x00 0x00).
fn password_to_ucs2(password string) []u8 {
	if password.len == 0 {
		return [u8(0), 0]
	}
	mut out := bmpstring_encode(password)
	out << 0
	out << 0
	return out
}

// explicit_wraps wraps `inner` (already a full TLV) in [n] EXPLICIT.
fn explicit_wrap(n int, inner []u8) []u8 {
	if n < 0 || n > 30 {
		return inner
	}
	return der_tlv(u8(0xa0 | n), inner)
}

// implicit_octet wraps raw bytes in [n] IMPLICIT primitive (e.g. [0] OCTET).
fn implicit_wrap(n int, raw []u8) []u8 {
	return der_tlv(u8(0x80 | n), raw)
}
