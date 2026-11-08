module websocket

import net

fn test_plain_client_reset_keeps_tls_uninitialized() ! {
	mut client := new_client('ws://127.0.0.1:1')!
	defer { client.dispose() }
	assert !client.is_ssl
	assert isnil(client.ssl_conn)
	client.reset_state()!
	assert isnil(client.ssl_conn)
}

fn test_accepted_plain_client_close_keeps_tls_uninitialized() ! {
	mut server := new_server(.ip, 0, '')
	server.ls = net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { server.ls.close() or {} }
	mut peer := net.dial_tcp(server.ls.addr()!.str())!
	defer { peer.close() or {} }
	mut client := server.accept_new_client()!
	defer { client.dispose() }
	assert client.is_server
	assert !client.is_ssl
	assert isnil(client.ssl_conn)
	client.close(1000, 'done')!
	assert client.get_state() == .closed
	assert isnil(client.ssl_conn)
}

fn test_tls_client_reset_initializes_reconnect_context() ! {
	mut client := new_client('wss://127.0.0.1:1')!
	defer { client.dispose() }
	assert client.is_ssl
	assert !isnil(client.ssl_conn)
	previous := client.ssl_conn
	client.ssl_conn.shutdown() or {}
	client.reset_state()!
	assert !isnil(client.ssl_conn)
	assert client.ssl_conn != previous
}
