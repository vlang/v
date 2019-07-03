module net

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

struct Socket {
pub:
	sockfd int
	family int
	_type int
	proto int
}

import const (
	AF_INET
	AF_INET6
	AF_UNSPEC
	SOCK_STREAM
	SOL_SOCKET
	SO_REUSEADDR
	SO_REUSEPORT
	INADDR_ANY
	AI_PASSIVE
	SHUT_RD
	SHUT_WR
	SHUT_RDWR
)

struct C.in_addr {
mut:
	s_addr int
}

struct C.sockaddr_in {
mut:
	sin_family int
	sin_port int
	sin_addr C.in_addr
}

struct C.addrinfo {
mut:
	ai_family int
	ai_socktype int
	ai_flags int
	ai_protocol int
	ai_addrlen int
	ai_next voidptr
	ai_addr voidptr
}

struct C.sockaddr_storage {}

// create socket
pub fn socket(family int, _type int, proto int) Socket {
	sockfd := C.socket(family, _type, proto)
	s := Socket {
		sockfd: sockfd
		family: family
		_type: _type
		proto: proto
	}
	return s
}

// set socket options
pub fn (s Socket) setsockopt(level int, optname int, optvalue *int) int {
	res := C.setsockopt(s.sockfd, level, optname, optvalue, C.sizeof(optvalue))
	return res
}

// bind socket to port
pub fn (s Socket) bind(port int) int {
	mut addr := C.sockaddr_in{}
	addr.sin_family = s.family
	addr.sin_port = C.htons(port)
	addr.sin_addr.s_addr = C.htonl(INADDR_ANY)
	size := 16 // sizeof(C.sockaddr_in)
	res := C.bind(s.sockfd, &addr, size)
	return res
}

// put socket into passive mode and wait to receive
pub fn (s Socket) listen() int {
	backlog := 128
	res := C.listen(s.sockfd, backlog)
	return res
}

// put socket into passive mode with user specified backlog and wait to receive
pub fn (s Socket) listen_backlog(backlog int) int {
	mut n := 0
	if backlog > 0 {
		n = backlog
	}
	res := C.listen(s.sockfd, n)
	return res
}

// helper method to create, bind, and listen given port number
pub fn listen(port int) Socket {
	s := socket(AF_INET, SOCK_STREAM, 0)
	if s.sockfd == 0 {
		println('socket: init socket failed')
		return s
	}
	bind_res := s.bind(port)
	if bind_res < 0 {
		println('socket: bind failed')
		return s
	}
	listen_res := s.listen()
	if listen_res < 0 {
		println('socket: listen failed')
		return s
	}
	return s
}

// accept first connection request from socket queue
pub fn (s Socket) accept() Socket {
	addr := C.sockaddr_storage{}
	size := 128 // sizeof(sockaddr_storage)
	sockfd := C.accept(s.sockfd, &addr, &size)
	if sockfd < 0 {
		println('socket: accept failed')
	}
	c := Socket {
		sockfd: sockfd
		family: s.family
		_type: s._type
		proto: s.proto
	}
	return c
}

// connect to given addrress and port
pub fn (s Socket) connect(address string, port int) int {
	mut hints := C.addrinfo{}
	hints.ai_family = AF_UNSPEC
	hints.ai_socktype = SOCK_STREAM
	hints.ai_flags = AI_PASSIVE

	info := &C.addrinfo{!}
	sport := '$port'
	info_res := C.getaddrinfo(address.cstr(), sport.cstr(), &hints, &info)
	if info_res != 0 {
		println('socket: getaddrinfo failed')
		return info_res
	}

	res := C.connect(s.sockfd, info.ai_addr, info.ai_addrlen)
	return res
}

// helper method to create socket and connect
pub fn dial(address string, port int) Socket {
	s := socket(AF_INET, SOCK_STREAM, 0)
	res := s.connect(address, port)
	if res < 0 {
		println('socket: failed to connect')
	}
	return s
}

// send string data to socket
pub fn (s Socket) send(buf byteptr, len int) int {
	res := C.send(s.sockfd, buf, len, 0)
	if res < 0 {
		println('socket: send failed')
	}
	return res
}

// receive string data from socket
pub fn (s Socket) recv(bufsize int) byteptr {
	buf := malloc(bufsize)
	res := C.recv(s.sockfd, buf, bufsize, 0)
	if res < 0 {
		println('socket: recv failed')
	}
	return buf
}

// shutdown and close socket
pub fn (s Socket) close() int {
	shutdown_res := C.shutdown(s.sockfd, SHUT_RDWR)
	if shutdown_res < 0 {
		println('socket: shutdown failed')
	}
	res := C.close(s.sockfd)
	if res < 0 {
		println('socket: close failed')
	}
	return 0
}

