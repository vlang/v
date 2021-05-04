module net

const (
	crlf     = '\r\n'
	msg_peek = 0x02
	max_read = 400
)

// read_line is a *simple*, *non customizable*, blocking line reader.
// It will *always* return a line, ending with CRLF, or just '', on EOF.
// NB: if you want more control over the buffer, please use a buffered IO
// reader instead: `io.new_buffered_reader({reader: io.make_reader(con)})`
pub fn (mut con TcpConn) read_line() string {
	mut buf := [max_read]byte{} // where C.recv will store the network data
	mut res := '' // The final result, including the ending \n.
	for {
		mut line := '' // The current line. Can be a partial without \n in it.
		n := C.recv(con.sock.handle, &buf[0], max_read - 1, msg_peek | msg_nosignal)
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
		line = unsafe { tos_clone(&buf[0]) }
		if eol_idx > 0 {
			// At this point, we are sure that recv returned valid data,
			// that contains *at least* one line.
			// Ensure that the block till the first \n (including it)
			// is removed from the socket's receive queue, so that it does
			// not get read again.
			C.recv(con.sock.handle, &buf[0], eol_idx + 1, msg_nosignal)
			res += line
			break
		}
		// recv returned a buffer without \n in it .
		C.recv(con.sock.handle, &buf[0], n, msg_nosignal)
		res += line
		res += crlf
		break
	}
	return res
}
