module asn1

// encode_int encodes an integer as DER INTEGER.
pub fn encode_int(n i64) []u8 {
	mut val := n
	mut buf := []u8{}
	if val >= 0 {
		for {
			buf << u8(val & 0xff)
			val >>= 8
			if val == 0 {
				break
			}
		}
	} else {
		for {
			buf << u8(val & 0xff)
			val >>= 8
			if val == -1 {
				break
			}
		}
	}
	mut out := []u8{}
	for i := buf.len - 1; i >= 0; i-- {
		out << buf[i]
	}
	if out.len > 0 && (out[0] & 0x80) != 0 {
		if n >= 0 {
			out.insert(0, u8(0))
		}
	}
	return wrap(0x02, out)
}

pub fn decode_int(data []u8) !i64 {
	tag, content := unwrap(data)!
	if tag != 0x02 {
		return error('asn1: not an integer')
	}
	if content.len == 0 {
		return error('asn1: empty integer')
	}
	mut res := i64(0)
	for b in content {
		res = (res << 8) | i64(b)
	}
	if content[0] & 0x80 != 0 {
		for _ in 0 .. (8 - content.len) {
			res = (res << 8) | 0xff
		}
	}
	return res
}

pub fn encode_bool(b bool) []u8 {
	v := if b { u8(0xff) } else { u8(0x00) }
	return wrap(0x01, [v])
}

// encode_null encodes a DER NULL.
pub fn encode_null() []u8 {
	return wrap(0x05, []u8{})
}

// decode_null validates a DER NULL.
pub fn decode_null(data []u8) ! {
	tag, content := unwrap(data)!
	if tag != 0x05 || content.len != 0 {
		return error('asn1: not a null')
	}
}

// encode_enumerated encodes an integer as DER ENUMERATED.
pub fn encode_enumerated(n i64) []u8 {
	// Same content octets as INTEGER, different tag.
	inner := encode_int(n)
	return wrap(0x0a, inner[2..].clone())
}

// decode_enumerated decodes a DER ENUMERATED.
pub fn decode_enumerated(data []u8) !i64 {
	tag, content := unwrap(data)!
	if tag != 0x0a {
		return error('asn1: not an enumerated')
	}
	return decode_int(wrap(0x02, content.clone()))
}

// encode_utf8string encodes s as DER UTF8String.
pub fn encode_utf8string(s string) []u8 {
	return wrap(0x0c, s.bytes())
}

// decode_utf8string decodes a DER UTF8String.
pub fn decode_utf8string(data []u8) !string {
	tag, content := unwrap(data)!
	if tag != 0x0c {
		return error('asn1: not a utf8string')
	}
	return content.bytestr()
}

// encode_bitstring encodes data with unused_bits trailing pad bits
// as DER BIT STRING.
pub fn encode_bitstring(data []u8, unused_bits u8) ![]u8 {
	if unused_bits > 7 {
		return error('asn1: unused bits out of range')
	}
	if data.len == 0 && unused_bits != 0 {
		return error('asn1: unused bits with empty data')
	}
	mut content := []u8{cap: data.len + 1}
	content << unused_bits
	content << data
	return wrap(0x03, content)
}

// decode_bitstring decodes a DER BIT STRING into data and unused bits.
pub fn decode_bitstring(data []u8) !([]u8, u8) {
	tag, content := unwrap(data)!
	if tag != 0x03 || content.len == 0 {
		return error('asn1: not a bit string')
	}
	if content[0] > 7 {
		return error('asn1: bad unused bits count')
	}
	return content[1..].clone(), content[0]
}

// encode_oid encodes a dotted OID (e.g. "1.2.840.113549") as DER.
pub fn encode_oid(oid string) ![]u8 {
	parts := oid.split('.')
	if parts.len < 2 {
		return error('asn1: bad oid `${oid}`')
	}
	mut arcs := []u64{}
	for p in parts {
		arcs << p.u64()
	}
	if arcs[0] > 2 || (arcs[0] < 2 && arcs[1] >= 40) {
		return error('asn1: bad oid arcs `${oid}`')
	}
	mut body := []u8{}
	body << u8(arcs[0] * 40 + arcs[1])
	for a in arcs[2..] {
		body << encode_base128(a)
	}
	return wrap(0x06, body)
}

// decode_oid decodes a DER OID into dotted form.
pub fn decode_oid(data []u8) !string {
	tag, content := unwrap(data)!
	if tag != 0x06 || content.len == 0 {
		return error('asn1: not an oid')
	}
	first := u64(content[0])
	mut arcs := if first < 40 {
		[u64(0), first]
	} else if first < 80 {
		[u64(1), first - 40]
	} else {
		[u64(2), first - 80]
	}
	mut pos := 1
	for pos < content.len {
		mut v := u64(0)
		for {
			if pos >= content.len {
				return error('asn1: truncated oid')
			}
			b := content[pos]
			pos++
			v = (v << 7) | u64(b & 0x7f)
			if b & 0x80 == 0 {
				break
			}
		}
		arcs << v
	}
	return arcs.map(fn (a u64) string { return a.str() }).join('.')
}

fn encode_base128(n u64) []u8 {
	if n == 0 {
		return [u8(0)]
	}
	mut v := n
	mut tmp := []u8{}
	for v > 0 {
		tmp << u8(v & 0x7f)
		v >>= 7
	}
	mut out := []u8{}
	for i := tmp.len - 1; i >= 0; i-- {
		mut b := tmp[i]
		if i != 0 {
			b |= 0x80
		}
		out << b
	}
	return out
}

// encode_set encodes parts as DER SET.
pub fn encode_set(parts [][]u8) []u8 {
	mut content := []u8{}
	for p in parts {
		content << p
	}
	return wrap(0x30 + 1, content)
}

// decode_set decodes a DER SET into its element encodings.
pub fn decode_set(data []u8) ![][]u8 {
	tag, content := unwrap(data)!
	if tag != 0x31 {
		return error('asn1: not a set')
	}
	return split_tlvs(content)
}

// encode_explicit wraps inner (a complete TLV) in context-specific
// constructed tag [n].
pub fn encode_explicit(n u8, inner []u8) ![]u8 {
	if n > 30 {
		return error('asn1: explicit tag out of range')
	}
	return wrap(0xa0 + n, inner.clone())
}

// is_valid_utctime reports whether s looks like DER UTCTime
// (YYMMDDHHMMSSZ, 13 bytes).
pub fn is_valid_utctime(s string) bool {
	if s.len != 13 || !s.ends_with('Z') {
		return false
	}
	for c in s[..12].bytes() {
		if c < `0` || c > `9` {
			return false
		}
	}
	return true
}

// encode_utctime encodes s as DER UTCTime.
pub fn encode_utctime(s string) ![]u8 {
	if !is_valid_utctime(s) {
		return error('asn1: bad utctime `${s}`')
	}
	return wrap(0x17, s.bytes())
}

// decode_utctime decodes a DER UTCTime.
pub fn decode_utctime(data []u8) !string {
	tag, content := unwrap(data)!
	if tag != 0x17 {
		return error('asn1: not a utctime')
	}
	s := content.bytestr()
	if !is_valid_utctime(s) {
		return error('asn1: bad utctime content')
	}
	return s
}

// is_valid_generalizedtime reports whether s looks like DER
// GeneralizedTime (YYYYMMDDHHMMSSZ, 15 bytes).
pub fn is_valid_generalizedtime(s string) bool {
	if s.len != 15 || !s.ends_with('Z') {
		return false
	}
	for c in s[..14].bytes() {
		if c < `0` || c > `9` {
			return false
		}
	}
	return true
}

// encode_generalizedtime encodes s as DER GeneralizedTime.
pub fn encode_generalizedtime(s string) ![]u8 {
	if !is_valid_generalizedtime(s) {
		return error('asn1: bad generalizedtime `${s}`')
	}
	return wrap(0x18, s.bytes())
}

// decode_generalizedtime decodes a DER GeneralizedTime.
pub fn decode_generalizedtime(data []u8) !string {
	tag, content := unwrap(data)!
	if tag != 0x18 {
		return error('asn1: not a generalizedtime')
	}
	s := content.bytestr()
	if !is_valid_generalizedtime(s) {
		return error('asn1: bad generalizedtime content')
	}
	return s
}

pub fn decode_bool(data []u8) !bool {
	tag, content := unwrap(data)!
	if tag != 0x01 || content.len != 1 {
		return error('asn1: not a boolean')
	}
	return content[0] != 0
}

pub fn encode_bytes(b []u8) []u8 {
	return wrap(0x04, b.clone())
}

pub fn decode_bytes(data []u8) ![]u8 {
	tag, content := unwrap(data)!
	if tag != 0x04 {
		return error('asn1: not an octet string')
	}
	return content.clone()
}

pub fn encode_sequence(parts [][]u8) []u8 {
	mut content := []u8{}
	for p in parts {
		content << p
	}
	return wrap(0x30, content)
}

pub fn decode_sequence(data []u8) ![][]u8 {
	tag, content := unwrap(data)!
	if tag != 0x30 {
		return error('asn1: not a sequence')
	}
	return split_tlvs(content)
}

fn split_tlvs(content []u8) ![][]u8 {
	mut out := [][]u8{}
	mut pos := 0
	for pos < content.len {
		t := content[pos]
		l, hdr := read_length(content[pos + 1..])
		start := pos + 1 + hdr
		end := start + l
		if end > content.len {
			return error('asn1: truncated')
		}
		out << content[pos..end].clone()
		pos = end
		_ = t
	}
	return out
}

fn wrap(tag u8, content []u8) []u8 {
	mut out := []u8{cap: content.len + 4}
	out << tag
	out << encode_length(content.len)
	out << content
	return out
}

fn unwrap(data []u8) !(u8, []u8) {
	if data.len < 2 {
		return error('asn1: too short')
	}
	tag := data[0]
	l, hdr := read_length(data[1..])
	start := 1 + hdr
	end := start + l
	if end > data.len {
		return error('asn1: truncated')
	}
	return tag, data[start..end]
}

fn encode_length(n int) []u8 {
	if n < 128 {
		return [u8(n)]
	}
	mut buf := []u8{}
	mut v := n
	for v > 0 {
		buf << u8(v & 0xff)
		v >>= 8
	}
	mut out := []u8{}
	for i := buf.len - 1; i >= 0; i-- {
		out << buf[i]
	}
	mut res := [u8(0x80 | u8(out.len))]
	res << out
	return res
}

fn read_length(data []u8) (int, int) {
	if data.len == 0 {
		return 0, 0
	}
	if data[0] < 128 {
		return int(data[0]), 1
	}
	n := int(data[0] & 0x7f)
	if n == 0 || data.len < 1 + n {
		return 0, 1
	}
	mut l := 0
	for i in 0 .. n {
		l = (l << 8) | int(data[1 + i])
	}
	return l, 1 + n
}
