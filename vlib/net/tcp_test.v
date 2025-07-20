// vtest flaky: true
// vtest retry: 5
import net

const test_port = 45123

fn handle_conn(mut c net.TcpConn) {
	for {
		mut buf := []u8{len: 100, init: 0}
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

fn one_shot_echo_server(mut l net.TcpListener, ch_started chan int) ! {
	eprintln('> one_shot_echo_server')
	ch_started <- 1
	mut new_conn := l.accept() or { return error('could not accept') }
	eprintln('    > new_conn: ${new_conn}')
	handle_conn(mut new_conn)
	new_conn.close() or {}
}

fn echo(address string) ! {
	mut c := net.dial_tcp(address)!
	defer {
		c.close() or {}
	}

	println('local: ' + c.addr()!.str())
	println(' peer: ' + c.peer_addr()!.str())
	ip := c.peer_ip()!
	println('   ip: ${ip}')
	assert ip in ['::1', 'localhost', '127.0.0.1']

	data := 'Hello from vlib/net!'
	c.write_string(data)!
	mut buf := []u8{len: 4096}
	read := c.read(mut buf) or { panic(err) }
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	println('Got "${buf.bytestr()}"')
}

fn test_tcp_ip6() {
	eprintln('\n>>> ${@FN}')
	address := 'localhost:${test_port}'
	mut l := net.listen_tcp(.ip6, ':${test_port}') or { panic(err) }
	dump(l)
	start_echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or {}
	// ensure there is at least one new socket created before the next test
	l = net.listen_tcp(.ip6, ':${test_port + 1}') or { panic(err) }
}

fn start_echo_server(mut l net.TcpListener) {
	ch_server_started := chan int{}
	spawn one_shot_echo_server(mut l, ch_server_started)
	_ := <-ch_server_started
}

fn test_tcp_ip() {
	eprintln('\n>>> ${@FN}')
	address := 'localhost:${test_port}'
	mut l := net.listen_tcp(.ip, address) or { panic(err) }
	dump(l)
	start_echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or {}
}

fn test_tcp_unix() {
	eprintln('\n>>> ${@FN}')
	address := 'tcp-test.sock'

	mut l := net.listen_tcp(.unix, address) or { return }
	assert false
}

fn testsuite_end() {
	eprintln('\ndone')
}
