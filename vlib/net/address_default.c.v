module net

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

struct C.sockaddr_in {
mut:
	sin_family byte
	sin_port   u16
	sin_addr   u32
}

struct C.sockaddr_in6 {
mut:
	sin6_family byte
	sin6_port   u16
	sin6_addr   [4]u32
}

struct C.sockaddr_un {
mut:
	sun_family byte
	sun_path   [max_unix_path]char
}
