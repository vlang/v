module conv

import encoding.binary

// host to net 64 (htonll)
pub fn htn64(host u64) u64 {
	mut b := []u8{len: 8}
	binary.big_endian_put_u64(mut b, host)
	return binary.little_endian_u64(b)
}

// host to net 32 (htonl)
pub fn htn32(host u32) u32 {
	mut b := []u8{len: 4}
	binary.big_endian_put_u32(mut b, host)
	return binary.little_endian_u32(b)
}

// host to net 16 (htons)
pub fn htn16(host u16) u16 {
	mut b := []u8{len: 2}
	binary.big_endian_put_u16(mut b, host)
	return binary.little_endian_u16(b)
}

// net to host 64 (ntohll)
pub fn nth64(net u64) u64 {
	return htn64(net)
}

// net to host 32 (ntohl)
pub fn nth32(net u32) u32 {
	return htn32(net)
}

// net to host 16 (ntohs)
pub fn nth16(net u16) u16 {
	return htn16(net)
}
