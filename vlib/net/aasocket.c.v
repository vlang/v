module net

$if windows {
	// This is mainly here for tcc on windows
	// which apparently doesnt have this definition
	#include "@VMODROOT/vlib/net/ipv6_v6only.h"
}

$if windows {
	$if msvc {
		// Force these to be included before afunix!
		#include <winsock2.h>
		#include <ws2tcpip.h>
		#include <afunix.h>
	} $else {
		#include "@VMODROOT/vlib/net/afunix.h"
	}
} $else {
	#include <sys/un.h>
}

// Select represents a select operation
enum Select {
	read
	write
	except
}

// SocketType are the available sockets
pub enum SocketType {
	udp       = C.SOCK_DGRAM
	tcp       = C.SOCK_STREAM
	seqpacket = C.SOCK_SEQPACKET
	raw       = C.SOCK_RAW
}

// AddrFamily are the available address families
pub enum AddrFamily {
	unix   = C.AF_UNIX
	ip     = C.AF_INET
	ip6    = C.AF_INET6
	unspec = C.AF_UNSPEC
}

fn C.socket(domain AddrFamily, typ SocketType, protocol i32) i32

// fn C.setsockopt(sockfd int, level int, optname int, optval voidptr, optlen C.socklen_t) int
fn C.setsockopt(sockfd i32, level i32, optname i32, optval voidptr, optlen u32) i32

fn C.htonl(host u32) u32
fn C.htons(host u16) u16

fn C.ntohl(net u32) u32
fn C.ntohs(net u16) u16

// fn C.bind(sockfd int, addr &C.sockaddr, addrlen C.socklen_t) int
// use voidptr for arg 2 because sockaddr is a generic descriptor for any kind of socket operation,
// it can also take sockaddr_in depending on the type of socket used in arg 1
fn C.bind(sockfd i32, addr &Addr, addrlen u32) i32

fn C.listen(sockfd i32, backlog i32) i32

// fn C.accept(sockfd int, addr &C.sockaddr, addrlen &C.socklen_t) int
fn C.accept(sockfd i32, addr &Addr, addrlen &u32) i32

fn C.getaddrinfo(node &char, service &char, hints &C.addrinfo, res &&C.addrinfo) i32

fn C.freeaddrinfo(info &C.addrinfo)

// fn C.connect(sockfd int, addr &C.sockaddr, addrlen C.socklen_t) int
fn C.connect(sockfd i32, addr &Addr, addrlen u32) i32

// fn C.send(sockfd int, buf voidptr, len usize, flags int) usize
fn C.send(sockfd i32, buf voidptr, len usize, flags i32) i32

// fn C.sendto(sockfd int, buf voidptr, len usize, flags int, dest_add &C.sockaddr, addrlen C.socklen_t) usize
fn C.sendto(sockfd i32, buf voidptr, len usize, flags i32, dest_add &Addr, addrlen u32) i32

// fn C.recv(sockfd int, buf voidptr, len usize, flags int) usize
fn C.recv(sockfd i32, buf voidptr, len usize, flags i32) i32

// fn C.recvfrom(sockfd int, buf voidptr, len usize, flags int, src_addr &C.sockaddr, addrlen &C.socklen_t) usize
fn C.recvfrom(sockfd i32, buf voidptr, len usize, flags i32, src_addr &Addr, addrlen &u32) i32

fn C.shutdown(socket i32, how i32) i32

// fn C.getpeername(sockfd int, addr &C.sockaddr, addlen &C.socklen_t) int
fn C.getpeername(sockfd i32, addr &Addr, addlen &u32) i32

fn C.inet_ntop(af AddrFamily, src voidptr, dst &char, dst_size i32) &char

fn C.WSAAddressToStringA(lpsaAddress &Addr, dwAddressLength u32, lpProtocolInfo voidptr, lpszAddressString &char,
	lpdwAddressStringLength &u32) i32

// fn C.getsockname(sockfd int, addr &C.sockaddr, addrlen &C.socklen_t) int
fn C.getsockname(sockfd i32, addr &C.sockaddr, addrlen &u32) i32

fn C.getsockopt(sockfd i32, level i32, optname i32, optval voidptr, optlen &u32) i32

// defined in builtin
// fn C.read() int
// fn C.close() int

fn C.ioctlsocket(s i32, cmd i32, argp &u32) i32

fn C.fcntl(fd i32, cmd i32, arg ...voidptr) i32

fn C.select(ndfs i32, readfds &C.fd_set, writefds &C.fd_set, exceptfds &C.fd_set, timeout &C.timeval) i32

fn C.FD_ZERO(fdset &C.fd_set)

fn C.FD_SET(fd i32, fdset &C.fd_set)

fn C.FD_ISSET(fd i32, fdset &C.fd_set) i32

fn C.inet_pton(family AddrFamily, saddr &char, addr voidptr) i32

fn C.photon_socket(domain AddrFamily, typ SocketType, protocol i32) i32
fn C.photon_connect(i32, &Addr, u32, timeout u64) i32
fn C.photon_accept(i32, voidptr, i32, timeout u64) i32
fn C.photon_send(i32, voidptr, i32, i32, timeout u64) i32
fn C.photon_recv(i32, voidptr, i32, i32, timeout u64) i32

@[typedef]
pub struct C.fd_set {}
