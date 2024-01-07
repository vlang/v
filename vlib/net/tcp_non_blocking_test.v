import net

fn server_thread() {
	mut buf := []u8{len: 128}
	mut listener := net.listen_tcp(.ip, ':8901') or { panic(err) }
	mut server := listener.accept() or { panic(err) }
	server.read_nb(mut buf) or {
		$if windows {
			assert err.code() == net.WsaError.wsaewouldblock
		} $else {
			assert err.code() == net.error_ewouldblock
		}
	}
}

fn test_non_blocking_read() {
	server := spawn server_thread()
	net.dial_tcp('localhost:8901') or { panic(err) }
	server.wait()
}
