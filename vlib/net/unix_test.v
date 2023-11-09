import net
import os

const (
	test_socket_path = os.vtmp_dir() + '/v_unix.sock'
)

fn handle_conn(mut c net.UnixConn) {
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

fn one_shot_echo_server(mut l net.UnixListener, ch_started chan int) ! {
	ch_started <- 1
	mut conn := l.accept()!
	handle_conn(mut conn)
	conn.close() or {}
}

fn echo(socket_path string) ! {
	mut c := net.dial_unix(.tcp, socket_path)!
	defer {
		c.close() or {}
	}

	data := 'Hello from unix sockets!'
	c.write_string(data)!
	mut buf := []u8{len: 4096}
	read := c.read(mut buf) or { panic(err) }
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}

	println('received "${buf.bytestr()}"')
}

fn start_echo_server(mut l net.UnixListener) {
	ch_server_started := chan int{}
	spawn one_shot_echo_server(mut l, ch_server_started)
	_ := <-ch_server_started
}

fn test_unix() {
	mut l := net.listen_unix(.tcp, test_socket_path) or { panic(err) }
	start_echo_server(mut l)
	// test if socket path is created
	assert os.exists(test_socket_path) == true

	echo(test_socket_path) or { panic(err) }
	l.close() or {}
	// test if file is unlinked successfully
	assert os.exists(test_socket_path) == false
}
