module conv

// host to net 32 (htonl)
pub fn htn32(host u32) u32 {
	return C.htonl(host)
}

// host to net 16 (htons)
pub fn htn16(host u16) u16 {
	return C.htons(host)
}

// net to host 32 (ntohl)
pub fn nth32(host u32) u32 {
	return C.ntohl(host)
}

// net to host 16 (ntohs)
pub fn nth16(host u16) u16 {
	return C.ntohs(host)
}

//******************************

struct Bytes {
mut:
	first u32
	last  u32
}

union LongLong {
	Bytes
	ll u64
}

// host to net 64 (htonll)
pub fn htn64(host u64) u64 {
	mut ll := LongLong{
		ll: host
	}

	unsafe {
		ll.first = htn32(ll.first)
		ll.last = htn32(ll.last)
	}
	return unsafe { ll.ll }
}

// net to host 64 (ntohll)
pub fn nth64(net u64) u64 {
	mut ll := LongLong{
		ll: net
	}

	unsafe {
		ll.first = nth32(ll.first)
		ll.last = nth32(ll.last)
	}
	return unsafe { ll.ll }
}
