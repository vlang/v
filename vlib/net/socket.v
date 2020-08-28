module net

import os

pub struct Socket {
pub:
	sockfd int
	family int
	typ    int
	proto  int
pub mut:
	max_single_send_size int = 64000
}

struct C.in_addr {
mut:
	s_addr int
}

struct C.sockaddr {
}

struct C.sockaddr_in {
mut:
	sin_family int
	sin_port   int
	sin_addr   C.in_addr
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

fn C.socket() int

fn C.setsockopt() int

fn C.htonl() int

fn C.htons() int

fn C.bind() int

fn C.listen() int

fn C.accept() int

fn C.getaddrinfo() int

fn C.connect() int

fn C.send() int

fn C.recv() int

fn C.shutdown() int

fn C.ntohs() int

fn C.getsockname() int

fn C.inet_ntop(af int, src voidptr, dst charptr, dst_size int) charptr

fn C.getpeername(sockfd int, addr &C.sockaddr_in, addrsize &int) int

// create socket
pub fn new_socket(family, typ, proto int) ?Socket {
	sockfd := C.socket(family, typ, proto)
	one := 1
	// This is needed so that there are no problems with reusing the
	// same port after the application exits.
	C.setsockopt(sockfd, C.SOL_SOCKET, C.SO_REUSEADDR, &one, sizeof(int))
	if sockfd == -1 {
		return error('net.socket: failed')
	}
	s := Socket{
		sockfd: sockfd
		family: family
		typ: typ
		proto: proto
	}
	return s
}

pub fn socket_udp() ?Socket {
	return new_socket(C.AF_INET, C.SOCK_DGRAM, C.IPPROTO_UDP)
}

// set socket options
pub fn (s Socket) setsockopt(level, optname int, optvalue &int) ?int {
	res := C.setsockopt(s.sockfd, level, optname, optvalue, sizeof(&int))
	if res < 0 {
		return error('net.setsocketopt: failed with $res')
	}
	return res
}

// bind socket to port
pub fn (s Socket) bind(port int) ?int {
	mut addr := C.sockaddr_in{}
	addr.sin_family = s.family
	addr.sin_port = C.htons(port)
	addr.sin_addr.s_addr = C.htonl(C.INADDR_ANY)
	size := 16 // sizeof(C.sockaddr_in)
	tmp := voidptr(&addr)
	skaddr := &C.sockaddr(tmp)
	res := C.bind(s.sockfd, skaddr, size)
	if res < 0 {
		return error('net.bind: failed with $res')
	}
	return res
}

// put socket into passive mode and wait to receive
pub fn (s Socket) listen() ?int {
	backlog := 128
	res := C.listen(s.sockfd, backlog)
	if res < 0 {
		return error('net.listen: failed with $res')
	}
	$if debug {
		println('listen res = $res')
	}
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
		return error('net.listen_backlog: failed with $res')
	}
	return res
}

// helper method to create, bind, and listen given port number
pub fn listen(port int) ?Socket {
	$if debug {
		println('net.listen($port)')
	}
	s := new_socket(C.AF_INET, C.SOCK_STREAM, 0)?
	s.bind(port)?
	s.listen()?
	return s
}

// accept first connection request from socket queue
pub fn (s Socket) accept() ?Socket {
	$if debug {
		println('accept()')
	}
	addr := C.sockaddr{}
	size := sizeof(addr)
	sockfd := C.accept(s.sockfd, &addr, &size)
	if sockfd < 0 {
		return error('net.accept: failed with $sockfd')
	}
	c := Socket{
		sockfd: sockfd
		family: s.family
		typ: s.typ
		proto: s.proto
	}
	return c
}

pub fn (s Socket) peer_ip() ?string {
	buf := [44]byte{}
	peeraddr := C.sockaddr_in{}
	speeraddr := sizeof(peeraddr)
	ok := C.getpeername(s.sockfd, &C.sockaddr(&peeraddr), &speeraddr)
	if ok == -1 {
		return error('net.peer_ip: getpeername failed')
	}
	cstr := C.inet_ntop(C.AF_INET, &peeraddr.sin_addr, buf, sizeof(buf))
	if cstr == 0 {
		return error('net.peer_ip: inet_ntop failed')
	}    
	return cstring_to_vstring(cstr)
}

// connect to given addrress and port
pub fn (s Socket) connect(address string, port int) ?int {
	mut hints := C.addrinfo{}
	hints.ai_family = s.family
	hints.ai_socktype = s.typ
	hints.ai_flags = C.AI_PASSIVE
	hints.ai_protocol = s.proto
	hints.ai_addrlen = 0
	hints.ai_canonname = C.NULL
	hints.ai_addr = C.NULL
	hints.ai_next = C.NULL
	info := &C.addrinfo(0)
	sport := '$port'
	info_res := C.getaddrinfo(address.str, sport.str, &hints, &info)
	if info_res != 0 {
		error_message := os.get_error_msg(error_code())
		return error('net.connect: getaddrinfo failed "$error_message"')
	}
	res := C.connect(s.sockfd, info.ai_addr, info.ai_addrlen)
	if res < 0 {
		error_message := os.get_error_msg(error_code())
		return error('net.connect: connect failed "$error_message"')
	}
	return res
}

// helper method to create socket and connect
pub fn dial(address string, port int) ?Socket {
	s := new_socket(C.AF_INET, C.SOCK_STREAM, 0)?
	s.connect(address, port)?
	return s
}

// send data to socket (when you have a memory buffer)
pub fn (s Socket) send(buf byteptr, len int) ?int {
	mut dptr := buf
	mut dlen := len
	for {
		send_size := if dlen > s.max_single_send_size { s.max_single_send_size } else { dlen }
		sbytes := C.send(s.sockfd, dptr, send_size, msg_nosignal)
		if sbytes < 0 {
			return error('net.send: failed with $sbytes')
		}
		dlen -= sbytes
		if dlen <= 0 {
			break
		}
		unsafe {
			dptr += sbytes
		}
	}
	return len
}

// send string data to socket (when you have a v string)
pub fn (s Socket) send_string(sdata string) ?int {
	return s.send(sdata.str, sdata.len)
}

// receive string data from socket. NB: you are responsible for freeing the returned byteptr
pub fn (s Socket) recv(bufsize int) (byteptr, int) {
	mut buf := byteptr(0)
	unsafe {
		buf = malloc(bufsize)
	}
	res := C.recv(s.sockfd, buf, bufsize, 0)
	return buf, res
}

// TODO: remove cread/2 and crecv/2 when the Go net interface is done
pub fn (s Socket) cread(buffer byteptr, buffersize int) int {
	return C.read(s.sockfd, buffer, buffersize)
}

// Receive a message from the socket, and place it in a preallocated buffer buf,
// with maximum message size bufsize. Returns the length of the received message.
pub fn (s Socket) crecv(buffer voidptr, buffersize int) int {
	return C.recv(s.sockfd, byteptr(buffer), buffersize, 0)
}

// shutdown and close socket
pub fn (s Socket) close() ?int {
	mut shutdown_res := 0
	$if windows {
		shutdown_res = C.shutdown(s.sockfd, C.SD_BOTH)
	} $else {
		shutdown_res = C.shutdown(s.sockfd, C.SHUT_RDWR)
	}
	// TODO: should shutdown throw an error? close will
	// continue even if shutdown failed
	// if shutdown_res < 0 {
	// return error('net.close: shutdown failed with $shutdown_res')
	// }
	mut res := 0
	$if windows {
		res = C.closesocket(s.sockfd)
	} $else {
		res = C.close(s.sockfd)
	}
	if res < 0 {
		return error('net.close: failed with $res')
	}
	return 0
}

pub const (
	crlf     = '\r\n'
	max_read = 400
	msg_peek = 0x02
)

// write - write a string with CRLF after it over the socket s
pub fn (s Socket) write(str string) ?int {
	line := '$str$crlf'
	res := C.send(s.sockfd, line.str, line.len, msg_nosignal)
	if res < 0 {
		return error('net.write: failed with $res')
	}
	return res
}

// read_line - retrieves a line from the socket s (i.e. a string ended with \n)
pub fn (s Socket) read_line() string {
	mut buf := [max_read]byte{} // where C.recv will store the network data
	mut res := '' // The final result, including the ending \n.
	for {
		mut line := '' // The current line. Can be a partial without \n in it.
		n := C.recv(s.sockfd, buf, max_read - 1, msg_peek)
		if n == -1 {
			return res
		}
		if n == 0 {
			return res
		}
		buf[n] = `\0`
		mut eol_idx := -1
		for i in 0 .. n {
			if int(buf[i]) == `\n` {
				eol_idx = i
				// Ensure that tos_clone(buf) later,
				// will return *only* the first line (including \n),
				// and ignore the rest
				buf[i + 1] = `\0`
				break
			}
		}
		bufbp := byteptr(buf)
		line = tos_clone(bufbp)
		if eol_idx > 0 {
			// At this point, we are sure that recv returned valid data,
			// that contains *at least* one line.
			// Ensure that the block till the first \n (including it)
			// is removed from the socket's receive queue, so that it does
			// not get read again.
			C.recv(s.sockfd, buf, eol_idx + 1, 0)
			res += line
			break
		}
		// recv returned a buffer without \n in it .
		C.recv(s.sockfd, buf, n, 0)
		res += line
		res += crlf
		break
	}
	return res
}

// TODO
pub fn (s Socket) read_all() string {
	mut buf := [max_read]byte{} // where C.recv will store the network data
	mut res := '' // The final result, including the ending \n.
	for {
		n := C.recv(s.sockfd, buf, max_read - 1, 0)
		if n == -1 {
			return res
		}
		if n == 0 {
			return res
		}
		bufbp := byteptr(buf)
		res += tos_clone(bufbp)
	}
	return res
}

pub fn (s Socket) get_port() int {
	mut addr := C.sockaddr_in{}
	size := 16 // sizeof(sockaddr_in)
	tmp := voidptr(&addr)
	skaddr := &C.sockaddr(tmp)
	C.getsockname(s.sockfd, skaddr, &size)
	return C.ntohs(addr.sin_port)
}
