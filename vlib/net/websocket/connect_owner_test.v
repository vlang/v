module websocket

import io
import net
import time

fn test_dispose_releases_initial_tls_context_for_plain_and_tls_clients() {
	for address in ['ws://127.0.0.1:1', 'wss://127.0.0.1:1'] {
		mut client := new_client(address) or { panic(err) }
		client.dispose()
		client.dispose()
		assert client.get_state() == .closed
		assert isnil(client.conn)
		$if use_openssl ? {
			assert isnil(client.ssl_conn.ssl)
			assert isnil(client.ssl_conn.sslctx)
		}
	}
}

fn test_dispose_releases_failed_custom_dial_initial_context() {
	mut client := new_client('wss://127.0.0.1:1') or { panic(err) }
	mut rejected := false
	client.connect_with_dialer(fn (_ string) !&net.TcpConn {
		return error('injected dial failure')
	}) or {
		rejected = true
		assert err.msg() == 'injected dial failure'
	}
	assert rejected
	assert isnil(client.conn)
	client.dispose()
	client.dispose()
	$if use_openssl ? {
		assert isnil(client.ssl_conn.ssl)
		assert isnil(client.ssl_conn.sslctx)
	}
}

fn test_custom_dialer_rejects_proxy_before_attempt_instead_of_bypassing_it() {
	mut client := new_client('ws://127.0.0.1:1', proxy_url: 'http://127.0.0.1:2') or { panic(err) }
	defer { client.dispose() }
	mut rejected := false
	client.connect_with_dialer(fn (_ string) !&net.TcpConn {
		panic('proxy bypassed')
	}) or {
		rejected = true
		assert err.msg() == 'custom websocket dialer cannot use proxy_url'
	}
	assert rejected
	assert client.get_state() == .closed
}

fn owner_upgrade_peer(mut listener net.TcpListener) {
	mut peer := listener.accept() or { panic(err) }
	defer { peer.close() or {} }
	peer.set_read_timeout(2 * time.second)
	mut buf := []u8{len: 4096}
	mut head := ''
	for !head.contains('\r\n\r\n') {
		n := peer.read(mut buf) or { panic(err) }
		head += buf[..n].bytestr()
	}
	key := head.all_after('Sec-WebSocket-Key: ').all_before('\r\n')
	challenge := create_key_challenge_response(key) or { panic(err) }
	peer.write_string('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ${challenge}\r\n\r\n') or { panic(err) }
	mut received := []u8{}
	for {
		n := peer.read(mut buf) or {
			assert err is io.Eof
			break
		}
		received << buf[..n]
	}
	assert received.len >= 6
	assert received[0] == 0x88 // close frame precedes EOF
}

fn test_custom_dialer_upgrade_and_dispose_closes_owned_socket() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer { listener.close() or {} }
	port := listener.addr()!.port()!
	peer := spawn owner_upgrade_peer(mut listener)
	mut client := new_client('ws://127.0.0.1:${port}',
		read_timeout:  time.second
		write_timeout: time.second
	)!
	client.connect_with_dialer(fn (address string) !&net.TcpConn { return net.dial_tcp(address) })!
	assert client.get_state() == .open
	client.dispose()
	client.dispose()
	assert client.get_state() == .closed
	peer.wait()
}
