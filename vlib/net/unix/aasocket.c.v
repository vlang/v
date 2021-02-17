module unix

// Select represents a select operation
enum Select {
	read
	write
	except
}

// SocketType are the available sockets
enum SocketType {
	dgram = C.SOCK_DGRAM
	stream = C.SOCK_STREAM
	seqpacket = C.SOCK_SEQPACKET
}

struct C.sockaddr {
	sa_family u16
}

const max_sun_path = 104

// 104 for macos, 108 for linux => use the minimum

struct C.sockaddr_un {
mut:
	//	sun_len    byte // only on macos
	sun_family int
	sun_path   [104]char // on linux that is 108
}

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

struct C.sockaddr_storage {
}
