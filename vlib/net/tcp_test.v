import net

import os

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

fn echo(address string) ? {
	mut c := net.dial_tcp(address) ?
	defer {
		c.close() or { }
	}

	println('local: ${c.addr()?}')
	println(' peer: ${c.peer_addr()?}')

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

fn test_tcp_ip6() {
	address := 'localhost:$test_port'
	mut l := net.listen_tcp(.ip6, ':$test_port') or { panic(err) }
	go echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or { }
}

fn test_tcp_ip() {
	address := 'localhost:$test_port'
	mut l := net.listen_tcp(.ip, address) or { panic(err) }
	go echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or { }
}

fn test_tcp_unix() {
	address := os.real_path('./tcp-test.sock')
	println('$address')

	mut l := net.listen_tcp(.unix, address) or { panic(err) }
	go echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or { }

	os.rm(address) or { panic('failed to remove socket file') }
}
