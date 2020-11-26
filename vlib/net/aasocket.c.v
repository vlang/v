module net

// Select represents a select operation
enum Select {
	read write except
}

// SocketType are the available sockets
pub enum SocketType {
	udp = C.SOCK_DGRAM
	tcp = C.SOCK_STREAM
}

// SocketFamily are the available address families
pub enum SocketFamily {
	inet = C. AF_INET
}

struct C.in_addr {
mut:
	s_addr int
}

struct C.sockaddr {
	sa_family u16
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
fn C.sendto() int

fn C.recv() int
fn C.recvfrom() int

fn C.shutdown() int

fn C.ntohs() int

fn C.getpeername() int

fn C.inet_ntop(af int, src voidptr, dst charptr, dst_size int) charptr

fn C.WSAAddressToStringA() int

fn C.getsockname() int

// defined in builtin
// fn C.read() int
// fn C.close() int

fn C.ioctlsocket() int
fn C.fcntl() int

fn C.@select() int
fn C.FD_ZERO()
fn C.FD_SET()
fn C.FD_ISSET() bool

[typedef]
pub struct C.fd_set {}
