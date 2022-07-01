module net

const max_unix_path = 104

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
	sin_family u8
	sin_port   u16
	sin_addr   u32
}

pub struct C.sockaddr_in6 {
mut:
	sin6_family u8
	sin6_port   u16
	sin6_addr   [4]u32
}

pub struct C.sockaddr_un {
mut:
	sun_family u8
	sun_path   [max_unix_path]char
}
