module conv

// host to net 32 (htonl)
pub fn htn32(host &u32) u32 {
	return C.htonl(host)
}

// host to net 16 (htons)
pub fn htn16(host &u16) u16 {
	return C.htons(host)
}

// net to host 32 (ntohl)
pub fn nth32(host &u32) u32 {
	return C.ntohl(host)
}

// net to host 16 (ntohs)
pub fn nth16(host &u16) u16 {
	return C.ntohs(host)
}
