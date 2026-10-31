module websocket

import net
import net.mbedtls
import time

fn owner_tls_peer(mut listener mbedtls.SSLListener) {
	mut conn := listener.accept() or { panic(err) }
	defer { conn.shutdown() or {} }
	mut buf := []u8{len: 16}
	// A clean TLS shutdown must reach the peer without application data.
	n := conn.read(mut buf) or { return }
	assert n == 0
}

fn test_configured_tls_socket_is_owned_until_shutdown() ! {
	cert_dir := @VEXEROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/'
	mut reservation := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := reservation.addr()!.port()!
	reservation.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:     cert_dir + 'server.crt'
		cert_key: cert_dir + 'server.key'
		validate: false
	})!
	defer { listener.shutdown() or {} }
	peer := spawn owner_tls_peer(mut listener)
	mut client := new_client('wss://127.0.0.1:${port}',
		read_timeout:  time.second
		write_timeout: time.second
	)!
	mut tcp := net.dial_tcp('127.0.0.1:${port}')!
	client.conn = client.configure_socket(mut tcp)!
	assert client.ssl_conn.owns_socket
	client.ssl_conn.shutdown()!
	if _ := client.conn.peer_addr() {
		assert false, 'TLS shutdown left the owned TCP socket open'
	}
	peer.wait()
}
