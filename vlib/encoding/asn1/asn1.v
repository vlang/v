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
