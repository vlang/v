import os
import net.unix
import net

// ensure that `net` is used, i.e. no warnings
const use_net = net.no_timeout

const test_port = os.join_path(os.temp_dir(), 'unix_domain_socket')

fn test_that_net_and_net_unix_can_be_imported_together_without_conflicts() ? {
	mut l := unix.listen_stream(test_port) or { panic(err) }
	go echo_server(mut l)
	defer {
		l.close() or {}
	}
	//
	mut c := unix.connect_stream(test_port) ?
	defer {
		c.close() or {}
	}
	//
	data := 'Hello from vlib/net!'
	c.write_string(data) ?
	mut buf := []u8{len: 100}
	assert c.read(mut buf) ? == data.len
	eprintln('< client read back buf: |${buf[0..data.len].bytestr()}|')
	assert buf[0..data.len] == data.bytes()
}

fn perror(s string) ? {
	println(s)
}

fn handle_conn(mut c unix.StreamConn) ? {
	for {
		mut buf := []u8{len: 100, init: 0}
		read := c.read(mut buf) or { return perror('Server: connection dropped') }
		eprintln('> server read ${read:3}, buf: |$buf.bytestr()|')
		c.write(buf[..read]) or { return perror('Server: connection dropped') }
	}
}

fn echo_server(mut l unix.StreamListener) ? {
	for {
		mut new_conn := l.accept() or { continue }
		handle_conn(mut new_conn) or {}
	}
}
