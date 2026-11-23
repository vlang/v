// vtest flaky: true
// vtest retry: 3
import os
import io
import time
import net.unix

const tfolder = os.join_path(os.temp_dir(), 'unix_socket_${os.getpid()}')
const socket_path = os.join_path(tfolder, 'v_unix.sock')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

const echo_connections = 3
const echo_payload = 'Hello from vlib/net!'
const echo_timeout = 2 * time.second

fn echo_connection(mut conn unix.StreamConn) ! {
	defer { conn.close() or {} }
	conn.set_read_timeout(echo_timeout)
	mut received := []u8{len: echo_payload.len}
	io.read_exact(mut conn, mut received)!
	conn.write(received)!
}

fn echo_server(mut listener unix.StreamListener) ! {
	for _ in 0 .. echo_connections {
		mut conn := listener.accept()!
		echo_connection(mut conn)!
	}
}

fn echo_client() ! {
	mut conn := unix.connect_stream(socket_path)!
	defer { conn.close() or {} }
	conn.set_read_timeout(echo_timeout)
	conn.write_string(echo_payload)!
	assert io.read_all(reader: conn)! == echo_payload.bytes()
}

fn test_tcp() {
	assert !os.exists(socket_path)
	mut listener := unix.listen_stream(socket_path)!
	listener.set_accept_timeout(echo_timeout)
	server := spawn echo_server(mut listener)

	for _ in 0 .. echo_connections {
		echo_client()!
	}
	server.wait()!
	listener.close()!
	assert !os.exists(socket_path)
}
