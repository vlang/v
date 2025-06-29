module picoev

import net
import picohttpparser

#include <errno.h>
$if windows {
	#include <winsock2.h>
	#include <ws2tcpip.h>
} $else $if freebsd || macos {
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
} $else {
	#include <netinet/tcp.h>
	#include <sys/resource.h>
}

@[inline]
fn get_time() i64 {
	// time.now() is slow
	return i64(C.time(C.NULL))
}

@[inline]
fn accept(fd int) int {
	return C.accept(fd, 0, 0)
}

@[inline]
fn close_socket(fd int) {
	trace_fd('close ${fd}')
	$if windows {
		C.closesocket(fd)
	} $else {
		C.close(fd)
	}
}

@[inline]
fn setup_sock(fd int) ! {
	flag := 1
	if C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_NODELAY, &flag, sizeof(int)) < 0 {
		return error('setup_sock.setup_sock failed')
	}
	$if freebsd {
		if C.fcntl(fd, C.F_SETFL, C.SOCK_NONBLOCK) != 0 {
			return error('fcntl failed')
		}
	} $else $if windows {
		non_blocking_mode := u32(1)
		if C.ioctlsocket(fd, C.FIONBIO, &non_blocking_mode) == C.SOCKET_ERROR {
			return error('icotlsocket failed')
		}
	} $else {
		// linux and macos
		if C.fcntl(fd, C.F_SETFL, C.O_NONBLOCK) != 0 {
			return error('fcntl failed')
		}
	}
}

@[inline]
fn req_read(fd int, buffer &u8, max_len int, offset int) int {
	// use `recv` instead of `read` for windows compatibility
	return unsafe { C.recv(fd, buffer + offset, max_len - offset, 0) }
}

fn fatal_socket_error(fd int) bool {
	if C.errno == C.EAGAIN {
		// try again later
		return false
	}
	$if windows {
		if C.errno == C.WSAEWOULDBLOCK {
			// try again later
			return false
		}
	} $else {
		if C.errno == C.EWOULDBLOCK {
			// try again later
			return false
		}
	}
	trace_fd('fatal error ${fd}: ${C.errno}')
	return true
}

// listen creates a listening tcp socket and returns its file descriptor.
fn listen(config Config) !int {
	// not using the `net` modules sockets, because not all socket options are defined
	fd := C.socket(config.family, net.SocketType.tcp, 0)
	if fd == -1 {
		return error('Failed to create socket')
	}
	trace_fd('listen: ${fd}')
	// Setting flags for socket
	flag := 1
	flag_zero := 0
	net.socket_error(C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEADDR, &flag, sizeof(int)))!
	if config.family == .ip6 {
		// set socket to dualstack so connections to both ipv4 and ipv6 addresses
		// can be accepted
		net.socket_error(C.setsockopt(fd, C.IPPROTO_IPV6, C.IPV6_V6ONLY, &flag_zero, sizeof(int)))!
	}
	$if linux || termux {
		// epoll socket options
		net.socket_error(C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEPORT, &flag, sizeof(int)))!
		net.socket_error(C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_QUICKACK, &flag, sizeof(int)))!
		$if !support_wsl1 ? {
			net.socket_error(C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_DEFER_ACCEPT, &config.timeout_secs,
				sizeof(int)))!
			queue_len := max_queue
			net.socket_error(C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_FASTOPEN, &queue_len,
				sizeof(int)))!
		}
	}
	// addr settings
	saddr := '${config.host}:${config.port}'
	addrs := net.resolve_addrs(saddr, config.family, .tcp) or {
		panic('Error while resolving `${saddr}`, err: ${err}')
	}
	addr := addrs[0]
	alen := addr.len()
	net.socket_error_message(C.bind(fd, voidptr(&addr), alen), 'binding to ${saddr} failed')!
	net.socket_error_message(C.listen(fd, C.SOMAXCONN), 'listening on ${saddr} with maximum backlog pending queue of ${C.SOMAXCONN}, failed')!
	setup_sock(fd) or {
		config.err_cb(config.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
			err)
	}
	return fd
}
