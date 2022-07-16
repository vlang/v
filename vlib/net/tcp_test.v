// vtest flaky: true
// vtest retry: 8
import net
import os

const (
	test_port = 45123
)

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

fn one_shot_echo_server(mut l net.TcpListener, ch_started chan int) ? {
	eprintln('> one_shot_echo_server')
	ch_started <- 1
	mut new_conn := l.accept() or { return error('could not accept') }
	eprintln('    > new_conn: $new_conn')
	handle_conn(mut new_conn)
	new_conn.close() or {}
}

fn echo(address string) ? {
	mut c := net.dial_tcp(address)?
	defer {
		c.close() or {}
	}

	println('local: ' + c.addr()?.str())
	println(' peer: ' + c.peer_addr()?.str())

	data := 'Hello from vlib/net!'
	c.write_string(data)?
	mut buf := []u8{len: 4096}
	read := c.read(mut buf)?
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	println('Got "$buf.bytestr()"')
}

fn test_tcp_ip6() {
	eprintln('\n>>> ${@FN}')
	address := 'localhost:$test_port'
	mut l := net.listen_tcp(.ip6, ':$test_port') or { panic(err) }
	dump(l)
	start_echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or {}
	// ensure there is at least one new socket created before the next test
	l = net.listen_tcp(.ip6, ':${test_port + 1}') or { panic(err) }
}

fn start_echo_server(mut l net.TcpListener) {
	ch_server_started := chan int{}
	go one_shot_echo_server(mut l, ch_server_started)
	_ := <-ch_server_started
}

fn test_tcp_ip() {
	eprintln('\n>>> ${@FN}')
	address := 'localhost:$test_port'
	mut l := net.listen_tcp(.ip, address) or { panic(err) }
	dump(l)
	start_echo_server(mut l)
	echo(address) or { panic(err) }
	l.close() or {}
}

fn test_tcp_unix() {
	eprintln('\n>>> ${@FN}')
	// TODO(emily):
	// whilst windows supposedly supports unix sockets
	// this doesnt work (wsaeopnotsupp at the call to bind())
	$if !windows {
		address := os.real_path('tcp-test.sock')
		// address := 'tcp-test.sock'
		println('$address')

		mut l := net.listen_tcp(.unix, address) or { panic(err) }
		start_echo_server(mut l)
		echo(address) or { panic(err) }
		l.close() or {}

		os.rm(address) or { panic('failed to remove socket file') }
	}
}

fn testsuite_end() {
	eprintln('\ndone')
}

fn test_bind() {
    $if !network ? {
        return
    }
	conn := net.dial_tcp_with_bind('vlang.io:80', '127.0.0.1:0')
	conn.close()
}
