module picoev

import net
import net.conv
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

[inline]
fn get_time() i64 {
	// time.now() is slow
	return i64(C.time(C.NULL))
}

[inline]
fn accept(fd int) int {
	return C.accept(fd, 0, 0)
}

[inline]
fn close_socket(fd int) {
	$if trace_fd ? {
		eprintln('close ${fd}')
	}

	$if windows {
		C.closesocket(fd)
	} $else {
		C.close(fd)
	}
}

[inline]
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

[inline]
fn req_read(fd int, b &u8, max_len int, idx int) int {
	// use `recv` instead of `read` for windows compatibility
	unsafe {
		return C.recv(fd, b + idx, max_len - idx, 0)
	}
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

	$if trace_fd ? {
		eprintln('fatal error ${fd}: ${C.errno}')
	}

	return true
}

// listen creates a listening tcp socket and returns its file decriptor
fn listen(config Config) int {
	// not using the `net` modules sockets, because not all socket options are defined
	fd := C.socket(net.AddrFamily.ip, net.SocketType.tcp, 0)
	assert fd != -1

	$if trace_fd ? {
		eprintln('listen: ${fd}')
	}

	// Setting flags for socket
	flag := 1
	assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEADDR, &flag, sizeof(int)) == 0

	$if linux {
		// epoll socket options
		assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEPORT, &flag, sizeof(int)) == 0
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_QUICKACK, &flag, sizeof(int)) == 0
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_DEFER_ACCEPT, &config.timeout_secs,
			sizeof(int)) == 0
		queue_len := max_queue
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_FASTOPEN, &queue_len, sizeof(int)) == 0
	}

	// addr settings

	sin_port := $if tinyc {
		conv.hton16(u16(config.port))
	} $else {
		C.htons(config.port)
	}

	sin_addr := $if tinyc {
		conv.hton32(u32(C.INADDR_ANY))
	} $else {
		C.htonl(C.INADDR_ANY)
	}

	mut addr := C.sockaddr_in{
		sin_family: u8(C.AF_INET)
		sin_port: sin_port
		sin_addr: sin_addr
	}
	size := sizeof(C.sockaddr_in)
	bind_res := C.bind(fd, voidptr(unsafe { &net.Addr(&addr) }), size)
	assert bind_res == 0

	listen_res := C.listen(fd, C.SOMAXCONN)
	assert listen_res == 0
	setup_sock(fd) or {
		config.err_cb(config.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
			err)
	}

	return fd
}
