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
	$if trace_fd ? {
		eprintln('close ${fd}')
	}

	$if windows {
		C.closesocket(fd)
	} $else {
		C.close(fd)
	}
}

@[inline]
fn setup_sock(fd int) ! {
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

	$if trace_fd ? {
		eprintln('fatal error ${fd}: ${C.errno}')
	}

	return true
}

// listen creates a listening tcp socket and returns its file descriptor
fn listen(config Config) !int {
	// not using the `net` modules sockets, because not all socket options are defined
	mut socket := net.new_tcp_socket(config.family) or { return error('Failed to create socket') }
	fd := socket.handle
	if fd == -1 {
		return error('Failed to create socket')
	}

	$if trace_fd ? {
		eprintln('listen: ${fd}')
	}

	if config.family == .ip6 {
		// set socket to dualstack so connections to both ipv4 and ipv6 addresses can be accepted
		socket.set_dualstack(true)!
	}

	$if linux {
		// epoll socket options
		socket.set_option_bool(.reuse_port, true)!
		socket.set_option_bool(.tcp_quickack, true)!
		socket.set_option_int(.tcp_defer_accept, config.timeout_secs)!
		socket.set_option_int(.tcp_fastopen, max_queue)!
	}

	// addr settings
	saddr := '${config.host}:${config.port}'

	socket.bind(saddr) or { panic('binding to ${saddr} failed, already in use?') }
	net.socket_error_message(C.listen(fd, C.SOMAXCONN), 'listening on ${saddr} with maximum backlog pending queue of ${C.SOMAXCONN}, failed')!

	setup_sock(fd) or {
		config.err_cb(config.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
			err)
	}

	return fd
}
