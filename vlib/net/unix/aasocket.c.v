module unix

// Select represents a select operation
enum Select {
	read
	write
	except
}

// SocketType are the available sockets
// enum SocketType {
// 	dgram = C.SOCK_DGRAM
// 	stream = C.SOCK_STREAM
// 	seqpacket = C.SOCK_SEQPACKET
// }

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

// fn C.socket() int

// fn C.setsockopt() int

// fn C.htonl() int

// fn C.htons() int

// fn C.bind() int

// fn C.listen() int

// fn C.accept() int

// fn C.getaddrinfo() int

// fn C.connect() int

// fn C.send() int

// fn C.sendto() int

// fn C.recv() int

// fn C.recvfrom() int

// fn C.shutdown() int

// fn C.ntohs() int

// fn C.getpeername() int

// fn C.inet_ntop(af int, src voidptr, dst charptr, dst_size int) charptr

fn C.WSAAddressToStringA() int

// fn C.getsockname() int

// defined in builtin
// fn C.read() int
// fn C.close() int

fn C.ioctlsocket() int

// fn C.fcntl() int

// fn C.@select() int

// fn C.FD_ZERO()

// fn C.FD_SET()

// fn C.FD_ISSET() bool

[typedef]
struct C.fd_set {}
