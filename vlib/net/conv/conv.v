module conv

import encoding.binary

// host to net 64 (htonll)
pub fn hton64(host u64) u64 {
	$if little_endian {
		mut b := []u8{len: 8}
		binary.little_endian_put_u64(mut b, host)
		return binary.big_endian_u64(b)
	} $else {
		return host
	}
}

// host to net 32 (htonl)
pub fn hton32(host u32) u32 {
	$if little_endian {
		mut b := []u8{len: 4}
		binary.big_endian_put_u32(mut b, host)
		return binary.little_endian_u32(b)
	} $else {
		return host
	}
}

// host to net 16 (htons)
pub fn hton16(host u16) u16 {
	$if little_endian {
		mut b := []u8{len: 2}
		binary.big_endian_put_u16(mut b, host)
		return binary.little_endian_u16(b)
	} $else {
		return host
	}
}

// net to host 64 (ntohll)
pub fn ntoh64(net u64) u64 {
	return hton64(net)
}

// net to host 32 (ntohl)
pub fn ntoh32(net u32) u32 {
	return hton32(net)
}

// net to host 16 (ntohs)
pub fn ntoh16(net u16) u16 {
	return hton16(net)
}
