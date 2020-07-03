import net

fn setup() (net.Socket, net.Socket, net.Socket) {
	server := net.listen(0) or { panic(err)	}
	server_port := server.get_port()
	client := net.dial('127.0.0.1', server_port) or { panic(err) }
	socket := server.accept() or { panic(err) }
	$if debug_peer_ip ? {
		ip := socket.peer_ip() or { '$err' }
		eprintln('socket peer_ip: $ip')
	}        
	return server, client, socket
}

fn cleanup(server &net.Socket, client &net.Socket, socket &net.Socket) {
	server.close() or {}
	client.close() or {}
	socket.close() or {}
}

fn test_socket() {
	server, client, socket := setup()
	message := 'Hello World'
	socket.send(message.str, message.len) or { assert false }
	$if debug {	println('message send: $message')	}
	$if debug {	println('send socket: $socket.sockfd')	}

	bytes, blen := client.recv(1024)
	received := tos(bytes, blen)
	$if debug {	println('message received: $received')	}
	$if debug {	println('client: $client.sockfd')	}

	assert message == received
	cleanup(server, client, socket)
}

fn test_socket_write() {
	server, client, socket := setup()
	message1 := 'a message 1'
	socket.write(message1) or { assert false }
	line1 := client.read_line()
	assert line1 != message1
	assert line1.trim_space() == message1
	cleanup(server, client, socket)
}

fn test_socket_write_fail_without_panic() {
	server, client, socket := setup()
	message2 := 'a message 2'
	// ensure that socket.write (i.e. done on the server side)
	// continues to work, even when the client side has been disconnected
	// this test is important for a stable long standing server
	client.close() or {}
	$if solaris { return } // TODO: fix segfaulting on Solaris
	for i:=0; i<3; i++ {
		socket.write(message2) or {
			println('write to a socket without a recipient should produce an option fail: $err | $message2')
			assert true
		}
	}
	cleanup(server, client, socket)
}
