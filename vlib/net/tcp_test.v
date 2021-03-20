import net

const (
	test_port = 45123
)

fn handle_conn(mut c net.TcpConn) {
	for {
		mut buf := []byte{len: 100, init: 0}
		read := c.read(mut buf) or {
			println('Server: connection dropped')
			return
		}
		c.write(buf[..read]) or {
			println('Server: connection dropped')
			return
		}
	}
}

fn echo_server(mut l net.TcpListener) ? {
	for {
		mut new_conn := l.accept() or { continue }
		go handle_conn(mut new_conn)
	}
	return none
}

fn echo() ? {
	mut c := net.dial_tcp('127.0.0.1:$test_port') ?
	defer {
		c.close() or { }
	}
	data := 'Hello from vlib/net!'
	c.write_string(data) ?
	mut buf := []byte{len: 4096}
	read := c.read(mut buf) ?
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	println('Got "$buf.bytestr()"')
	return none
}

fn test_tcp() {
	mut l := net.listen_tcp(test_port) or { panic(err) }
	go echo_server(mut l)
	echo() or { panic(err) }
	l.close() or { }
}
