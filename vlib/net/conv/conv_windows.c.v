module conv

#include <winsock2.h>

fn C.htonll(host u64) u64
fn C.htonl(host u32) u32
fn C.htons(host u16) u16

fn C.ntohll(net u32) u32
fn C.ntohl(net u32) u32
fn C.ntohs(net u16) u16

// host to net 64 (htonll)
pub fn htn64(host &u64) u64 {
	return C.htonll(host)
}

// net to host 64 (htonll)
pub fn nth64(host &u64) u64 {
	return C.ntohll(host)
}
