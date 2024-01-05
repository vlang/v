import net

const test_port = 45123

fn handle_conn(mut c net.TcpConn) ! {
	for {
		mut buf := []u8{len: 100, init: 0}
		read := c.read(mut buf) or {
			eprintln('Server: connection dropped')
			return err
		}
		c.write(buf[..read]) or {
			eprintln('Server: connection dropped')
			return err
		}
	}
}

fn one_shot_echo_server(mut l net.TcpListener, ch_started chan int) ! {
	eprintln('> one_shot_echo_server')
	ch_started <- 1
	mut new_conn := l.accept() or { return error('could not accept') }
	eprintln('    > new_conn: ${new_conn}')
	handle_conn(mut new_conn)!
	new_conn.close()!
}

fn echo(address string) ! {
	mut c := net.dial_tcp(address)!

	eprintln('local: ' + c.addr()!.str())
	eprintln(' peer: ' + c.peer_addr()!.str())

	data := 'Hello from vlib/net!'
	c.write_string(data)!
	mut buf := []u8{len: 4096}
	read := c.read(mut buf)!
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	c.close()!
}

fn test_tcp_ip6() {
	eprintln('\n>>> ${@FN}')
	saddr := ':${test_port}'
	addr := '[::1]:${test_port}'
	mut l := net.listen_tcp(.ip6, saddr)!
	dump(l)
	start_echo_server(mut l)
	echo(addr)!
	l.close()!
}

fn test_tcp_ip6_localhost() {
	eprintln('\n>>> ${@FN}')
	address := '[::1]:${test_port}'
	mut l := net.listen_tcp(.ip6, address)!
	dump(l)
	start_echo_server(mut l)
	echo(address)!
	l.close()!
}

fn start_echo_server(mut l net.TcpListener) {
	ch_server_started := chan int{}
	spawn one_shot_echo_server(mut l, ch_server_started)
	_ := <-ch_server_started
}

fn test_tcp_ip() {
	eprintln('\n>>> ${@FN}')
	saddr := ':${test_port}'
	addr := '127.0.0.1:${test_port}'
	mut l := net.listen_tcp(.ip, saddr)!
	dump(l)
	start_echo_server(mut l)
	echo(addr)!
	l.close()!
}

fn test_tcp_ip_localhost() {
	eprintln('\n>>> ${@FN}')
	address := '127.0.0.1:${test_port}'
	mut l := net.listen_tcp(.ip, address)!
	dump(l)
	start_echo_server(mut l)
	echo(address)!
	l.close()!
}

fn test_tcp_unix() {
	eprintln('\n>>> ${@FN}')
	address := 'tcp-test.sock'
	eprintln('${address}')

	mut l := net.listen_tcp(.unix, address) or { return }
	assert false
}

fn testsuite_end() {
	eprintln('\ndone')
}

fn test_bind() {
	$if !network ? {
		return
	}
	mut conn := net.dial_tcp_with_bind('vlang.io:80', '127.0.0.1:0')!
	conn.close()!
}
