import net 

fn test_socket() {
	mut server := net.listen(0) or {
		println(err)
		return
	}    
	server_port := server.get_port()
	mut client := net.dial('127.0.0.1', server_port) or {
		println(err)
		return
	}
	mut socket := server.accept() or {
		println(err)
		return
	}
	
	message := 'Hello World'
	socket.send(message.str, message.len)	

	bytes := client.recv(1024)
	received := tos(bytes, message.len)

	assert message == received

	server.close()
	client.close()
	socket.close()
} 
