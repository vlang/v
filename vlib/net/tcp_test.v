import net
import time

const (
	test_port = 45123
)

fn handle_conn(_c net.TcpConn) {
	mut c := _c
	// arbitrary timeouts to ensure that it doesnt
	// instantly throw its hands in the air and give up
	c.set_read_timeout(10 * time.second)
	c.set_write_timeout(10 * time.second)
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

fn echo_server(l net.TcpListener) ? {
	for {
		new_conn := l.accept() or {
			continue
		}
		go handle_conn(new_conn)
	}
	return none
}

fn echo() ? {
	mut c := net.dial_tcp('127.0.0.1:$test_port')?
	defer {
		c.close() or { }
	}
	// arbitrary timeouts to ensure that it doesnt
	// instantly throw its hands in the air and give up
	c.set_read_timeout(10 * time.second)
	c.set_write_timeout(10 * time.second)
	data := 'Hello from vlib/net!'
	c.write_str(data)?
	mut buf := []byte{len: 4096}
	read := c.read(mut buf)?
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	println('Got "$buf.bytestr()"')
	return none
}

fn test_tcp() {
	l := net.listen_tcp(test_port) or {
		panic(err)
	}
	go echo_server(l)
	echo() or {
		panic(err)
	}
	l.close() or { }
}

fn main() {
	test_tcp()
}
