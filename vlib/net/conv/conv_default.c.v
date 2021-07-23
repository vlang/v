module conv

#include <arpa/inet.h>

fn C.htonl(host u32) u32
fn C.htons(host u16) u16

fn C.ntohl(net u32) u32
fn C.ntohs(net u16) u16

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
pub fn htn64(host &u64) u64 {
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
pub fn nth64(net &u64) u64 {
	mut ll := LongLong{
		ll: net
	}

	unsafe {
		ll.first = nth32(ll.first)
		ll.last = nth32(ll.last)
	}
	return unsafe { ll.ll }
}
