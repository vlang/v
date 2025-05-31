// vtest build: !solaris
module main

import net
import picoev

const port = 8080

const http_response = 'HTTP/1.1 200 OK\r\nContent-type: text/html\r\nContent-length: 18\r\n\r\nHello from Picoev!'

fn main() {
	println('Starting webserver on http://localhost:${port}/ ...')
	mut pico := picoev.new(
		port:   port
		raw_cb: handle_conn
	)!
	pico.serve()
}

fn handle_conn(mut pv picoev.Picoev, fd int, events int) {
	// setup a nonblocking tcp connection
	mut conn := &net.TcpConn{
		sock:        net.tcp_socket_from_handle_raw(fd)
		handle:      fd
		is_blocking: false
	}

	mut buf := []u8{len: 4096}
	// read data from the tcp connection
	conn.read(mut buf) or { eprintln('could not read data from socket') }

	println('received data:')
	println(buf.bytestr())

	conn.write(http_response.bytes()) or { eprintln('could not write response') }

	// remove the socket from picoev's event loop and close the connection
	pv.close_conn(fd)
}
