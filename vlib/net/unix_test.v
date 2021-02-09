import net

const (
	test_port = './test'
)

fn handle_conn(mut c net.UnixConn) {
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

fn echo_server(mut l net.UnixListener) ? {
	for {
		mut new_conn := l.accept() or { continue }
		go handle_conn(mut new_conn)
	}
	return none
}

fn echo() ? {
	mut c := net.connect_unix('./test') ?
	defer {
		c.close() or { }
	}
	data := 'Hello from vlib/net!'
	c.write_str(data) ?
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
	mut l := net.listen_unix(test_port) or { panic(err) }
	go echo_server(mut l)
	echo() or { panic(err) }
	l.close() or { }
}
