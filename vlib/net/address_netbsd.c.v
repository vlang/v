module net

// TODO: test this on NetBSD

#include <sys/socket.h>
#include <netinet/in.h>

const max_unix_path = 104

struct C.addrinfo {
mut:
	ai_family    int
	ai_socktype  int
	ai_flags     int
	ai_protocol  int
	ai_addrlen   int
	ai_addr      voidptr
	ai_canonname voidptr
	ai_next      voidptr
}

struct C.sockaddr_in6 {
mut:
	// 1 + 1 + 2 + 4 + 16 + 4 = 28;
	sin6_len      byte     // 1
	sin6_family   byte     // 1
	sin6_port     u16      // 2
	sin6_flowinfo u32      // 4
	sin6_addr     [16]byte // 16
	sin6_scope_id u32      // 4
}

struct C.sockaddr_in {
mut:
	sin_len    byte
	sin_family byte
	sin_port   u16
	sin_addr   u32
	sin_zero   [8]char
}

struct C.sockaddr_un {
mut:
	sun_len    byte
	sun_family byte
	sun_path   [max_unix_path]char
}

[_pack: '1']
struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]byte
	scope_id  u32
}

[_pack: '1']
struct Ip {
	port u16
	addr [4]byte
	// Pad to size so that socket functions
	// dont complain to us (see  in.h and bind())
	// TODO(emily): I would really like to use
	// some constant calculations here
	// so that this doesnt have to be hardcoded
	sin_pad [8]byte
}

struct Unix {
	path [max_unix_path]char
}

[_pack: '1']
struct Addr {
pub:
	len  u8
	f    u8
	addr AddrData
}
