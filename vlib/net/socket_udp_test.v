import net

fn start_socket_udp_server() {
	bufsize := 1024
	bytes := [1024]byte{}
	s := net.socket_udp() or { panic(err) }
	s.bind( 9876 ) or { panic(err) }
	println('Waiting for udp packets:')
	for {
		res := s.crecv(bytes, bufsize)
		if res < 0 { break }
		print('Received $res bytes: ' + tos(bytes, res))
	}
}

fn test_udp_server() {
	// start_socket_udp_server()
}
