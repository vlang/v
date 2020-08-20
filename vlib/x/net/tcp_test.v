import x.net as net
import time

fn handle_conn(_c net.TcpConn) {
	mut c := _c
	// arbitrary timeouts to ensure that it doesnt
	// instantly throw its hands in the air and give up
	c.set_read_timeout(10 * time.second)
	c.set_write_timeout(10 * time.second)
	for {
		buf := []byte{ len: 100, init: 0 }
		read := c.read_into(mut buf) or {
			println('Server: connection dropped')
			return
		}

		c.write(buf[..read]) or {
			println('Server: connection dropped')
			return
		}
	}
}

fn echo_server(l net.TcpListener) ? {
	for {
		new_conn := l.accept() or { continue }
		go handle_conn(new_conn)
	}

	return none
}

fn echo() ? {
	mut c := net.dial_tcp('127.0.0.1:30000')?
	defer { c.close() or {} }
	
	// arbitrary timeouts to ensure that it doesnt
	// instantly throw its hands in the air and give up
	c.set_read_timeout(10 * time.second)
	c.set_write_timeout(10 * time.second)

	data := 'Hello from vlib/net!'
	c.write_string(data)?

	buf := []byte{ len: 100, init: 0 }
	read := c.read_into(mut buf)?

	assert read == data.len

	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}

	println('Got "${buf.bytestr()}"')

	return none
}

fn test_tcp() {
	l := net.listen_tcp(30000) or {
		panic(err)
	}

	go echo_server(l)
	echo() or {
		panic(err)
	}

	l.close() or {
	}
}

fn main() {
	test_tcp()
}