module tls

fn serve_once(mut l Listener) {
	mut conn := l.accept() or { return }
	defer { conn.close() or {} }
	mut buf := []u8{len: 4}
	n := conn.read(mut buf) or { return }
	if n > 0 {
		conn.write('pong'.bytes()) or {}
	}
}

fn test_listener_echo() {
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	server := spawn serve_once(mut l)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or { panic(err) }
	c.handshake() or { panic(err) }
	c.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { panic(err) }
	assert n == 4
	assert buf == 'pong'.bytes()
	c.close() or {}
	l.close() or {}
	_ = server
}
