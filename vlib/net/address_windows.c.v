module net

const max_unix_path = 108

pub struct C.addrinfo {
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

pub struct C.sockaddr_in {
mut:
	sin_family u16
	sin_port   u16
	sin_addr   u32
}

pub struct C.sockaddr_in6 {
mut:
	sin6_family u16
	sin6_port   u16
	sin6_addr   [4]u32
}

pub struct C.sockaddr_un {
mut:
	sun_family u16
	sun_path   [max_unix_path]char
}

[_pack: '1']
pub struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]u8
	scope_id  u32
}

[_pack: '1']
pub struct Ip {
	port    u16
	addr    [4]u8
	sin_pad [8]u8
}

pub struct Unix {
	path [max_unix_path]u8
}

[_pack: '1']
pub struct Addr {
pub:
	f    u16
	addr AddrData
}
