import io
import net
import time
import strings

const (
	server_port = 22334
)

fn setup() (net.TcpListener, net.TcpConn, net.TcpConn) {
	server := net.listen_tcp(server_port) or {
		panic(err)
	}
	mut client := net.dial_tcp('127.0.0.1:$server_port') or {
		panic(err)
	}
	mut socket := server.accept() or {
		panic(err)
	}
	$if debug_peer_ip ? {
		ip := con.peer_ip() or {
			'$err'
		}
		eprintln('connection peer_ip: $ip')
	}
	assert true
	return server, client, socket
}

fn cleanup(server &net.TcpListener, client &net.TcpConn, socket &net.TcpConn) {
	server.close() or { }
	client.close() or { }
	socket.close() or { }
}

fn test_socket() {
	server, client, socket := setup()
	defer {
		cleanup(server, client, socket)
	}
	message := 'Hello World'
	socket.write_str(message) or {
		assert false
		return
	}
	assert true
	$if debug {
		println('message send: $message')
	}
	$if debug {
		println('send socket: $socket.sock.handle')
	}
	mut buf := []byte{len: 1024}
	nbytes := client.read(mut buf) or {
		assert false
		return
	}
	received := buf[0..nbytes].bytestr()
	$if debug {
		println('message received: $received')
	}
	$if debug {
		println('client: $client.sock.handle')
	}
	assert message == received
}

fn test_socket_write_and_read() {
	server, client, socket := setup()
	defer {
		cleanup(server, client, socket)
	}
	message1 := 'a message 1'
	socket.write_str(message1) or {
		assert false
	}
	mut rbuf := []byte{len: message1.len}
	client.read(mut rbuf)
	line := rbuf.bytestr()
	assert line == message1
}

fn test_socket_read_line() {
	server, client, socket := setup()
	mut reader := io.new_buffered_reader({
		reader: io.make_reader(client)
	})
	defer {
		cleanup(server, client, socket)
	}
	message1, message2 := 'message1', 'message2'
	message := '$message1\n$message2\n'
	socket.write_str(message) or {
		assert false
	}
	assert true
	//
	line1 := reader.read_line() or {
		// println(reader.buf)
		assert false
		return
	}
	line2 := reader.read_line() or {
		// println(reader.buf)
		assert false
		return
	}
	assert line1 == message1
	assert line2 == message2
}

fn test_socket_write_fail_without_panic() {
	server, client, socket := setup()
	defer {
		cleanup(server, client, socket)
	}
	message2 := 'a message 2'
	// ensure that socket.write (i.e. done on the server side)
	// continues to work, even when the client side has been disconnected
	// this test is important for a stable long standing server
	client.close() or { }
	$if solaris {
		return
	}
	// TODO: fix segfaulting on Solaris
	for i := 0; i < 3; i++ {
		socket.write_str(message2) or {
			println('write to a socket without a recipient should produce an option fail: $err | $message2')
			assert true
		}
	}
}

fn test_socket_read_line_long_line_without_eol() {
	server, client, socket := setup()
	mut reader := io.new_buffered_reader({
		reader: io.make_reader(client)
	})
	defer {
		cleanup(server, client, socket)
	}
	message := strings.repeat_string('123', 400)
	socket.write_str(message)
	socket.write_str('\n')
	line := reader.read_line() or {
		assert false
		return
	}
	assert line == message
}
