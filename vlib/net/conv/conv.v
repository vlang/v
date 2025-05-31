module conv

union ConversionUnion {
mut:
	as_int64    u64
	as_int32    u32
	as_double64 f64
	as_double32 f32
}

// htonf32 converts the 32 bit double `host` to the net format
pub fn htonf32(host f32) f32 {
	$if little_endian {
		mut convert := ConversionUnion{
			as_double32: host
		}
		convert.as_int32 = unsafe { hton32(convert.as_int32) }
		return unsafe { convert.as_double32 }
	} $else {
		return host
	}
}

// htonf64 converts the 64 bit double `host` to the net format
pub fn htonf64(host f64) f64 {
	$if little_endian {
		mut convert := ConversionUnion{
			as_double64: host
		}
		convert.as_int64 = unsafe { hton64(convert.as_int64) }
		return unsafe { convert.as_double64 }
	} $else {
		return host
	}
}

// hton64 converts the 64 bit value `host` to the net format (htonll)
pub fn hton64(host u64) u64 {
	$if little_endian {
		return reverse_bytes_u64(host)
	} $else {
		return host
	}
}

// hton32 converts the 32 bit value `host` to the net format (htonl)
pub fn hton32(host u32) u32 {
	$if little_endian {
		return reverse_bytes_u32(host)
	} $else {
		return host
	}
}

// hton16 converts the 16 bit value `host` to the net format (htons)
pub fn hton16(host u16) u16 {
	$if little_endian {
		return reverse_bytes_u16(host)
	} $else {
		return host
	}
}

// ntoh64 converts the 64 bit value `net` to the host format (ntohll)
pub fn ntoh64(net u64) u64 {
	return hton64(net)
}

// ntoh32 converts the 32 bit value `net` to the host format (ntohl)
pub fn ntoh32(net u32) u32 {
	return hton32(net)
}

// ntoh16 converts the 16 bit value `net` to the host format (ntohs)
pub fn ntoh16(net u16) u16 {
	return hton16(net)
}

// u64tovarint converts the given 64 bit number `n`, where n < 2^62 to a byte array,
// using the variable length unsigned integer encoding from:
// https://datatracker.ietf.org/doc/html/rfc9000#section-16 .
// The returned array length .len, will be in [1,2,4,8] .
pub fn u64tovarint(n u64) ![]u8 {
	if n > u64(1) << 62 {
		return error('cannot encode more than 2^62-1')
	}
	msb := match true {
		n < 64 {
			u8(0b00)
		}
		n < 16384 {
			u8(0b01)
		}
		n < 1073741824 {
			u8(0b10)
		}
		else {
			u8(0b11)
		}
	}
	len := 1 << msb
	mut result := []u8{len: len}
	mut tn := n
	for i in 0 .. len {
		result[len - 1 - i] = u8(tn % 256)
		tn /= 256
	}
	result[0] |= msb << 6

	return result
}

// varinttou64 parses a variable length number from the start of the
// given byte array `b`. If it succeeds, it returns the decoded number,
// and the length of the parsed byte span.
pub fn varinttou64(b []u8) !(u64, u8) {
	if b.len == 0 {
		return error('cannot parse vluint from empty byte array')
	}
	msb := b[0] >> 6
	len := u8(1 << msb)
	if len > b.len {
		return error('expected ${len} bytes but got ${b.len} bytes')
	}
	mut n := u64(b[0] & 0b00111111)

	for i in 1 .. len {
		n = n * 256 + b[i]
	}
	return n, len
}

// reverse_bytes_u64 reverse a u64's byte order
@[inline]
pub fn reverse_bytes_u64(a u64) u64 {
	// vfmt off
	return ((a >> 56) & 0x00000000_000000FF) |
		   ((a >> 40) & 0x00000000_0000FF00) |
		   ((a >> 24) & 0x00000000_00FF0000) |
		   ((a >> 8)  & 0x00000000_FF000000) |
		   ((a << 8)  & 0x000000FF_00000000) |
		   ((a << 24) & 0x0000FF00_00000000) |
		   ((a << 40) & 0x00FF0000_00000000) |
		   ((a << 56) & 0xFF000000_00000000)
	// vfmt on
}

// reverse_bytes_u32 reverse a u32's byte order
@[inline]
pub fn reverse_bytes_u32(a u32) u32 {
	// vfmt off
	return ((a >> 24) & 0x0000_00FF) |
		   ((a >> 8)  & 0x0000_FF00) |
		   ((a << 8)  & 0x00FF_0000) |
		   ((a << 24) & 0xFF00_0000)
	// vfmt on
}

// reverse_bytes_u16 reverse a u16's byte order
@[inline]
pub fn reverse_bytes_u16(a u16) u16 {
	// vfmt off
	return ((a >> 8) & 0x00FF) |
		   ((a << 8) & 0xFF00)
	// vfmt on
}
