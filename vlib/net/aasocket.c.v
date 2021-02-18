module net

// Select represents a select operation
enum Select {
	read
	write
	except
}

// SocketType are the available sockets
pub enum SocketType {
	udp = C.SOCK_DGRAM
	tcp = C.SOCK_STREAM
}

// SocketFamily are the available address families
pub enum SocketFamily {
	inet = C.AF_INET
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

struct C.sockaddr_un {
mut:
	sun_family int
	sun_path   charptr
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

fn C.getaddrinfo(node charptr, service charptr, hints &C.addrinfo, res &&C.addrinfo) int

fn C.connect() int

// fn C.send(sockfd int, buf voidptr, len size_t, flags int) size_t
fn C.send(sockfd int, buf voidptr, len size_t, flags int) int

// fn C.sendto(sockfd int, buf voidptr, len size_t, flags int, dest_add &C.sockaddr, addrlen C.socklen_t) size_t
fn C.sendto(sockfd int, buf voidptr, len size_t, flags int, dest_add &C.sockaddr, addrlen int) int

// fn C.recv(sockfd int, buf voidptr, len size_t, flags int) size_t
fn C.recv(sockfd int, buf voidptr, len size_t, flags int) int

// fn C.recvfrom(sockfd int, buf voidptr, len size_t, flags int, src_addr &C.sockaddr, addrlen &C.socklen_t) size_t
fn C.recvfrom(sockfd int, buf voidptr, len size_t, flags int, src_addr &C.sockaddr, addrlen &C.socklen_t) int

fn C.shutdown(socket int, how int) int

fn C.ntohs(netshort u16) int

fn C.getpeername() int

fn C.inet_ntop(af int, src voidptr, dst charptr, dst_size int) charptr

fn C.WSAAddressToStringA() int

fn C.getsockname() int

// defined in builtin
// fn C.read() int
// fn C.close() int

fn C.ioctlsocket() int

fn C.fcntl() int

fn C.@select(ndfs int, readfds &C.fd_set, writefds &C.fd_set, exceptfds &C.fd_set, timeout &C.timeval) int

fn C.FD_ZERO(fdset &C.fd_set)

fn C.FD_SET(fd int, fdset &C.fd_set)

fn C.FD_ISSET(fd int, fdset &C.fd_set) bool

[typedef]
pub struct C.fd_set {}
