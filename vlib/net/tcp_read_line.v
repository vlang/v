module net

import strings

const (
	crlf              = '\r\n'
	msg_peek          = 0x02
	max_read          = 400
	max_read_line_len = 1048576
)

// get_blocking returns whether the connection is in a blocking state,
// that is calls to .read_line, C.recv etc will block till there is new
// data arrived, instead of returning immediately.
pub fn (mut con TcpConn) get_blocking() bool {
	// flags := C.fcntl(con.sock.handle, C.F_GETFL, 0)
	// return 0 == flags & C.O_NONBLOCK
	return con.is_blocking
}

// set_blocking will change the state of the connection to either blocking,
// when state is true, or non blocking (false).
// The default for `net` tcp connections is the non blocking mode.
// Calling .read_line will set the connection to blocking mode.
pub fn (mut con TcpConn) set_blocking(state bool) ? {
	con.is_blocking = state
	$if windows {
		mut t := u32(0)
		if !con.is_blocking {
			t = 1
		}
		socket_error(C.ioctlsocket(con.sock.handle, fionbio, &t)) ?
	} $else {
		mut flags := C.fcntl(con.sock.handle, C.F_GETFL, 0)
		if state {
			flags &= ~C.O_NONBLOCK
		} else {
			flags |= C.O_NONBLOCK
		}
		socket_error(C.fcntl(con.sock.handle, C.F_SETFL, flags)) ?
	}
}

// read_line is a *simple*, *non customizable*, blocking line reader.
// It will return a line, ending with LF, or just '', on EOF.
// Note: if you want more control over the buffer, please use a buffered IO
// reader instead: `io.new_buffered_reader({reader: io.make_reader(con)})`
pub fn (mut con TcpConn) read_line() string {
	return con.read_line_max(net.max_read_line_len)
}

// read_line_max is a *simple*, *non customizable*, blocking line reader.
// It will return a line, ending with LF, '' on EOF.
// It stops reading, when the result line length exceeds max_line_len.
[manualfree]
pub fn (mut con TcpConn) read_line_max(max_line_len int) string {
	if !con.is_blocking {
		con.set_blocking(true) or {}
	}
	mut buf := [net.max_read]byte{} // where C.recv will store the network data
	mut res := strings.new_builder(net.max_read) // The final result, including the ending \n.
	defer {
		unsafe { res.free() }
	}
	bstart := unsafe { &buf[0] }
	for {
		n := C.recv(con.sock.handle, bstart, net.max_read - 1, net.msg_peek | msg_nosignal)
		if n <= 0 {
			return res.str()
		}
		buf[n] = `\0`
		mut eol_idx := -1
		mut lend := n
		for i in 0 .. n {
			if buf[i] == `\n` {
				eol_idx = i
				lend = i + 1
				buf[lend] = `\0`
				break
			}
		}
		if eol_idx > 0 {
			// At this point, we are sure that recv returned valid data,
			// that contains *at least* one line.
			// Ensure that the block till the first \n (including it)
			// is removed from the socket's receive queue, so that it does
			// not get read again.
			C.recv(con.sock.handle, bstart, lend, msg_nosignal)
			unsafe { res.write_ptr(bstart, lend) }
			break
		}
		// recv returned a buffer without \n in it, just store it for now:
		C.recv(con.sock.handle, bstart, n, msg_nosignal)
		unsafe { res.write_ptr(bstart, lend) }
		if res.len > max_line_len {
			break
		}
	}
	return res.str()
}
