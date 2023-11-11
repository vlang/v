import os
import net.unix

const (
	tfolder     = os.join_path(os.vtmp_dir(), 'unix_test')
	socket_path = os.join_path(tfolder, 'v_unix.sock')
)

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn handle_conn(mut c unix.StreamConn) {
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

fn echo_server(mut l unix.StreamListener) ! {
	for {
		mut new_conn := l.accept() or { continue }
		spawn handle_conn(mut new_conn)
	}
}

fn echo() ! {
	mut c := unix.connect_stream(socket_path)!
	defer {
		c.close() or {}
	}
	data := 'Hello from vlib/net!'
	c.write_string(data)!
	mut buf := []u8{len: 4096}
	read := c.read(mut buf)!
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	println('Got "${buf.bytestr()}"')
	return
}

fn test_tcp() {
	assert os.exists(socket_path) == false

	mut l := unix.listen_stream(socket_path) or { panic(err) }
	spawn echo_server(mut l)
	echo() or { panic(err) }
	l.close() or {}

	// test if socket file is removed/unlinked
	assert os.exists(socket_path) == false
}
