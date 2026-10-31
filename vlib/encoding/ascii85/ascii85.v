module ascii85

// ascii85 encodes binary data using ASCII85 (Base85), as used by Go's
// encoding/ascii85 and Python's base64.a85encode. Groups of 4 bytes are
// encoded as 5 ASCII characters in the range '!' .. 'u'. A run of four zero
// bytes may be encoded as a single 'z'.
const encode_table = '!\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu'

// encode returns the ASCII85 encoding of `data`. The output contains no
// delimiters and no whitespace.
pub fn encode(data []u8) string {
	mut out := []u8{}
	mut i := 0
	for i + 4 <= data.len {
		// check for zero group
		if data[i] == 0 && data[i + 1] == 0 && data[i + 2] == 0 && data[i + 3] == 0 {
			out << `z`
			i += 4
			continue
		}
		mut val := u32(data[i]) << 24 | u32(data[i + 1]) << 16 | u32(data[i + 2]) << 8 | u32(data[i + 3])
		mut buf := [u8(0), u8(0), u8(0), u8(0), u8(0)]
		for k := 4; k >= 0; k-- {
			buf[k] = u8(val % 85)
			val /= 85
		}
		for k in 0 .. 5 {
			out << encode_table[buf[k]]
		}
		i += 4
	}
	// remainder
	rem := data.len - i
	if rem > 0 {
		mut val := u32(0)
		for k in 0 .. rem {
			val |= u32(data[i + k]) << u32((3 - k) * 8)
		}
		mut buf := [u8(0), u8(0), u8(0), u8(0), u8(0)]
		for k := 4; k >= 0; k-- {
			buf[k] = u8(val % 85)
			val /= 85
		}
		// emit rem+1 chars
		for k in 0 .. rem + 1 {
			out << encode_table[buf[k]]
		}
	}
	return out.bytestr()
}

// decode returns the bytes represented by the ASCII85 string `s`.
// Whitespace is ignored. 'z' expands to four zero bytes.
pub fn decode(s string) ![]u8 {
	mut out := []u8{}
	mut buf := []u8{}
	for ch in s {
		if ch == ` ` || ch == `\t` || ch == `\n` || ch == `\r` || ch == `\f` || ch == `\v` {
			continue
		}
		if ch == `z` {
			if buf.len != 0 {
				return error('ascii85: z inside group')
			}
			out << u8(0)
			out << u8(0)
			out << u8(0)
			out << u8(0)
			continue
		}
		idx := int(ch) - int(`!`)
		if idx < 0 || idx >= 85 {
			return error('ascii85: invalid character ${ch}')
		}
		buf << u8(idx)
		if buf.len == 5 {
			mut val := u32(0)
			for b in buf {
				val = val * 85 + u32(b)
			}
			out << u8(val >> 24)
			out << u8((val >> 16) & 0xFF)
			out << u8((val >> 8) & 0xFF)
			out << u8(val & 0xFF)
			buf.clear()
		}
	}
	if buf.len != 0 {
		if buf.len == 1 {
			return error('ascii85: invalid trailing group')
		}
		orig := buf.len
		// pad with 'u' (84)
		for buf.len < 5 {
			buf << u8(84)
		}
		mut val := u32(0)
		for b in buf {
			val = val * 85 + u32(b)
		}
		n := orig - 1
		if n >= 1 {
			out << u8(val >> 24)
		}
		if n >= 2 {
			out << u8((val >> 16) & 0xFF)
		}
		if n >= 3 {
			out << u8((val >> 8) & 0xFF)
		}
		if n >= 4 {
			out << u8(val & 0xFF)
		}
	}
	return out
}
