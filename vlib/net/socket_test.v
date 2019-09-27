import net

fn test_socket() {
	mut server := net.listen(0) or {
		panic(err)
	}
	server_port := server.get_port()
	mut client := net.dial('127.0.0.1', server_port) or {
		panic(err)
	}
	mut socket := server.accept() or {
		panic(err)
	}
	
	message := 'Hello World'
	socket.send(message.str, message.len)	
	$if debug {	println('message send: $message')	}
	$if debug {	println('send socket: $socket.sockfd')	}

	bytes, blen := client.recv(1024)
	received := tos(bytes, blen)
	$if debug {	println('message received: $received')	}
	$if debug {	println('client: $client.sockfd')	}

	assert message == received

	server.close()
	client.close()
	socket.close()
}
