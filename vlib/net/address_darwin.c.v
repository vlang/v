module net

const max_unix_path = 104

const addr_offset_fix = 0

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
	// 1 + 2 + 2 + 4 + 16 + 4 = 29;
	// actual size: 28 according to sizeof(C.sockaddr_in6);
	// sin6_len should be ignored?
	sin6_len      byte     // 1
	sin6_family   u16      // 2
	sin6_port     u16      // 2
	sin6_flowinfo u32      // 4
	sin6_addr     [16]byte // 16
	sin6_scope_id u32      // 4
}

struct C.sockaddr_in {
	sin_len    byte
	sin_family u16
	sin_port   u16
	sin_addr   u32
	sin_zero   [8]char
}

struct C.sockaddr_un {
	sun_len    byte
	sun_family u16
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
	sin_pad [10]byte
}

[_pack: '1']
struct Unix {
	path [max_unix_path]char
}

[_pack: '1']
struct Addr {
pub:
	len  u8
	f    u16
	addr AddrData
}
