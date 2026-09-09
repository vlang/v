// vtest build: present_openssl?
import net
import net.openssl as _

fn test_importing_openssl_does_not_break_tcp_listener_accept() {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0', net.ListenOptions{}) or { panic(err) }
	defer {
		listener.close() or {}
	}

	address := listener.addr() or { panic(err) }
	mut client := net.dial_tcp(address.str()) or { panic(err) }
	defer {
		client.close() or {}
	}

	mut server := listener.accept() or { panic(err) }
	defer {
		server.close() or {}
	}

	peer := server.peer_addr() or { panic(err) }
	assert peer.str().len > 0
}
