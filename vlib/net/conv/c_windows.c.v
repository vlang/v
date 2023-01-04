module conv

#include <winsock2.h>

#flag -lws2_32

fn C.htonl(host u32) u32
fn C.htons(host u16) u16

fn C.ntohl(net u32) u32
fn C.ntohs(net u16) u16

//** These are not supplied with mingw/gcc **
// fn C.htonll(host u64) u64
// fn C.ntohll(net u64) u64
