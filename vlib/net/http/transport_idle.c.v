module http

import net

// idle_tcp_closed checks for peer EOF without consuming data or waiting. The
// connection is checked out exclusively, so no other reader can race the peek.
fn (c &H1PooledConn) idle_tcp_closed() bool {
	if c.tcp == unsafe { nil } {
		return false
	}
	handle := c.tcp.sock.handle
	if handle < 0 {
		return true
	}
	$if !windows {
		// FD_SET cannot represent larger descriptors on POSIX.
		if handle >= C.FD_SETSIZE {
			return false
		}
	}
	read_set := C.fd_set{}
	C.FD_ZERO(&read_set)
	C.FD_SET(handle, &read_set)
	timeout := C.timeval{}
	if C.select(handle + 1, &read_set, unsafe { nil }, unsafe { nil }, &timeout) <= 0 {
		return false
	}
	mut byte := u8(0)
	// MSG_PEEK is 0x02 in both POSIX sockets and Winsock. A readable idle
	// socket with EOF stays readable; no request bytes have been written yet.
	result := C.recv(handle, &byte, 1, 0x02 | net.msg_dontwait)
	if result >= 0 {
		return result == 0
	}
	code := net.error_code()
	$if windows {
		return code !in [net.error_ewouldblock, int(net.WsaError.wsaeintr)]
	} $else {
		return code !in [net.error_ewouldblock, net.error_eagain, C.EINTR]
	}
}
