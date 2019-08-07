module net

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
	SD_BOTH
)

struct C.WSAData {
mut:
	wVersion u16
	wHighVersion u16
	szDescription [257]byte
	szSystemStatus [129]byte
	iMaxSockets u16
	iMaxUdpDg u16
	lpVendorInfo byteptr
}

const (
	WSA_V1  = 0x100 // C.MAKEWORD(1, 0)
	WSA_V11 = 0x101 // C.MAKEWORD(1, 1)
	WSA_V2  = 0x200 // C.MAKEWORD(2, 0)
	WSA_V21 = 0x201 // C.MAKEWORD(2, 1)
	WSA_V22 = 0x202 // C.MAKEWORD(2, 2)
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
pub fn socket(family int, _type int, proto int) ?Socket {
	$if windows {
		mut wsadata := C.WSAData{}
		res := C.WSAStartup(WSA_V22, &wsadata)
		if res != 0 {
			return error('socket: WSAStartup failed')
		}
	}

	sockfd := C.socket(family, _type, proto)
	one:=1
	// This is needed so that there are no problems with reusing the
	// same port after the application exits.
	C.setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int))
	if sockfd == 0 {
		return error('socket: init failed')
	}
	s := Socket {
		sockfd: sockfd
		family: family
		_type: _type
		proto: proto
	}
	return s
}

// set socket options
pub fn (s Socket) setsockopt(level int, optname int, optvalue *int) ?int {
	res := C.setsockopt(s.sockfd, level, optname, optvalue, C.sizeof(optvalue))
	if res < 0 {
		return error('socket: setsockopt failed')
	}
	return int(res)
}

// bind socket to port
pub fn (s Socket) bind(port int) ?int {
	mut addr := C.sockaddr_in{}
	addr.sin_family = s.family
	addr.sin_port = C.htons(port)
	addr.sin_addr.s_addr = C.htonl(INADDR_ANY)
	size := 16 // sizeof(C.sockaddr_in)
	res := C.bind(s.sockfd, &addr, size)
	if res < 0 {
		return error('socket: bind failed')
	}
	return int(res)
}

// put socket into passive mode and wait to receive
pub fn (s Socket) listen() ?int {
	backlog := 128
	res := int(C.listen(s.sockfd, backlog))
	if res < 0 {
		return error('socket: listen failed')
	}
println('liisten res = $res')
	return res
}

// put socket into passive mode with user specified backlog and wait to receive
pub fn (s Socket) listen_backlog(backlog int) ?int {
	mut n := 0
	if backlog > 0 {
		n = backlog
	}
	res := C.listen(s.sockfd, n)
	if res < 0 {
		return error('socket: listen_backlog failed')
	}
	return int(res)
}

// helper method to create, bind, and listen given port number
pub fn listen(port int) ?Socket {
println('net.listen($port)')
	s := socket(AF_INET, SOCK_STREAM, 0) or {
		return error(err)
	}
	bind_res := s.bind(port) or {
		return error(err)
	}
	listen_res := s.listen() or {
		return error(err)
	}
	return s
}

// accept first connection request from socket queue
pub fn (s Socket) accept() ?Socket {
println('accept()')
	addr := C.sockaddr_storage{}
	size := 128 // sizeof(sockaddr_storage)
	sockfd := C.accept(s.sockfd, &addr, &size)
	if sockfd < 0 {
		return error('socket: accept failed')
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
pub fn (s Socket) connect(address string, port int) ?int {
	mut hints := C.addrinfo{}
	hints.ai_family = AF_UNSPEC
	hints.ai_socktype = SOCK_STREAM
	hints.ai_flags = AI_PASSIVE

	info := &C.addrinfo{!}
	sport := '$port'
	info_res := C.getaddrinfo(address.str, sport.str, &hints, &info)
	if info_res != 0 {
		return error('socket: connect failed')
	}
	res := int(C.connect(s.sockfd, info.ai_addr, info.ai_addrlen))
	if res < 0 {
		return error('socket: connect failed')
	}
	return int(res)
}

// helper method to create socket and connect
pub fn dial(address string, port int) ?Socket {
	s := socket(AF_INET, SOCK_STREAM, 0) or {
		return error(err)
	}
	res := s.connect(address, port) or {
		return error(err)
	}
	return s
}

// send string data to socket
pub fn (s Socket) send(buf byteptr, len int) int {
	res := C.send(s.sockfd, buf, len, 0)
//	if res < 0 {
//		return error('socket: send failed')
//	}
	return res
}

// receive string data from socket
pub fn (s Socket) recv(bufsize int) byteptr {
	buf := malloc(bufsize)
	res := C.recv(s.sockfd, buf, bufsize, 0)
//	if res < 0 {
//		return error('socket: recv failed')
//	}
	return buf
}

// shutdown and close socket
pub fn (s Socket) close() ?int {
	mut shutdown_res := 0
	$if windows {
		shutdown_res = C.shutdown(s.sockfd, SD_BOTH)
	}
	$else {
		shutdown_res = C.shutdown(s.sockfd, SHUT_RDWR)
	}
	// TODO: should shutdown throw an error? close will
	// continue even if shutdown failed
//	if shutdown_res < 0 {
//		return error('socket: shutdown failed')
//	}

	mut res := 0
	$if windows {
		res = C.closesocket(s.sockfd)
	}
	$else {
		res = C.close(s.sockfd)
	}
	if res < 0 {
		return error('socket: close failed')
	}

	return 0
}

const (
	MAX_READ = 400
)
pub fn (s Socket) write(str string) {
	line := '$str\r\n'
	C.write(s.sockfd, line.str, line.len)
}

pub fn (s Socket) read_line() string {
	mut res := ''
	for {
		println('.')
		mut buf := malloc(MAX_READ)
		n := int(C.recv(s.sockfd, buf, MAX_READ-1, 0))
		println('numbytes=$n')
		if n == -1 {
			println('recv failed')
			// TODO
			return ''
		}
		if n == 0 {
			break
		}
		// println('resp len=$numbytes')
		buf[n] = `\0`
		//  C.printf('!!buf= "%s" n=%d\n', buf,n)
		line := string(buf)
		res += line
		// Reached a newline. That's an end of an IRC message
		// TODO dont need ends_with check ?
		if line.ends_with('\n') || n < MAX_READ - 1 {
			// println('NL')
			break
		}
		if line.ends_with('\r\n') {
			// println('RNL')
			break
		}
	}
	return res
}


