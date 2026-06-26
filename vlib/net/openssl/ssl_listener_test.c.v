module openssl

import net
import os
import time

fn new_test_ssl_listener(address string, family net.AddrFamily) !&SSLListener {
	cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
	key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
	return new_ssl_listener(address, SSLConnectConfig{
		cert:     cert_path
		cert_key: key_path
	},
		family: family
	)
}

fn load_test_certificate_pem() !string {
	cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
	return os.read_file(cert_path)
}

fn load_test_private_key_pem() !string {
	key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
	return os.read_file(key_path)
}

fn accept_one_ssl_connection(mut listener SSLListener) {
	mut conn := listener.accept() or { return }
	conn.duration = 500 * time.millisecond
	mut buffer := []u8{len: 1}
	_ := conn.read(mut buffer) or { 0 }
	conn.shutdown() or {}
}

fn close_tcp_connection_after(mut conn net.TcpConn, delay time.Duration) {
	time.sleep(delay)
	conn.close() or {}
}

fn test_ssl_listener_honors_ipv6_family() ! {
	$if macos || linux {
		mut listener := new_test_ssl_listener('[::1]:0', .ip6)!
		defer {
			listener.shutdown() or {}
		}
		assert listener.tcp_listener.addr()!.family() == .ip6
	}
}

fn test_ssl_listener_handshake_honors_timeout() ! {
	mut listener := new_test_ssl_listener('127.0.0.1:0', .ip)!
	defer {
		listener.shutdown() or {}
	}
	address := listener.tcp_listener.addr()!.str()
	mut tcp_client := net.dial_tcp(address)!
	close_thread := spawn close_tcp_connection_after(mut tcp_client, time.second)

	mut ssl_conn := listener.accept_without_handshake()!
	defer {
		ssl_conn.shutdown() or {}
	}
	ssl_conn.duration = 100 * time.millisecond
	stopwatch := time.new_stopwatch()
	ssl_conn.accept_handshake() or {
		assert stopwatch.elapsed() < 500 * time.millisecond
		close_thread.wait()
		return
	}
	close_thread.wait()
	assert false, 'TLS handshake unexpectedly succeeded without receiving client TLS data'
}

fn test_ssl_remaining_timeout_clamps_expired_finite_deadline() {
	// A finite deadline that has already expired must report an immediate
	// timeout (a minimal positive duration), not net.infinite_timeout, so that
	// select()/wait_for() return net.err_timed_out instead of blocking forever
	// after a peer stalls mid-handshake/read past the configured timeout.
	expired := time.now().add(-time.second)
	remaining := ssl_remaining_timeout(expired)
	assert remaining > 0
	assert remaining != net.infinite_timeout
	// An unset deadline still means "wait forever".
	assert ssl_remaining_timeout(time.unix(0)) == net.infinite_timeout
	// A live finite deadline returns its actual remaining time.
	live := ssl_remaining_timeout(time.now().add(5 * time.second))
	assert live > 4 * time.second
	assert live <= 5 * time.second
}

fn test_ssl_listener_loads_in_memory_credentials_without_temp_files() ! {
	start := time.now().unix()
	mut listener := new_ssl_listener('127.0.0.1:0', SSLConnectConfig{
		cert:                   load_test_certificate_pem()!
		cert_key:               load_test_private_key_pem()!
		in_memory_verification: true
	})!
	listener.shutdown()!
	finish := time.now().unix()
	for timestamp in start .. finish + 1 {
		for name in ['v_srv_cert', 'v_srv_cert_key', 'v_srv_ca'] {
			assert !os.exists(os.join_path(os.temp_dir(), name + timestamp.str()))
		}
	}
}

fn test_ssl_listener_file_credentials_load_full_certificate_chain() ! {
	cert_path := os.join_path(os.temp_dir(),
		'v_openssl_listener_fullchain_${time.now().unix_nano()}.pem')
	os.write_file(cert_path, load_test_certificate_pem()! + load_test_certificate_pem()!)!
	defer {
		os.rm(cert_path) or {}
	}
	mut listener := new_ssl_listener('127.0.0.1:0', SSLConnectConfig{
		cert:     cert_path
		cert_key: os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
	})!
	defer {
		listener.shutdown() or {}
	}
	assert C.v_net_openssl_SSL_CTX_extra_chain_certs_count(listener.sslctx) == 1
}

fn test_ssl_listener_negotiates_alpn() ! {
	mut listener := new_ssl_listener('127.0.0.1:0', SSLConnectConfig{
		cert:                   load_test_certificate_pem()!
		cert_key:               load_test_private_key_pem()!
		in_memory_verification: true
		alpn_protocols:         ['h2', 'http/1.1']
	})!
	defer {
		listener.shutdown() or {}
	}
	port := listener.tcp_listener.addr()!.port()!
	server_thread := spawn accept_one_ssl_connection(mut listener)
	mut client := new_ssl_conn(SSLConnectConfig{
		alpn_protocols: ['http/1.1']
	})!
	client.dial('127.0.0.1', port)!
	assert client.negotiated_alpn() == 'http/1.1'
	client.duration = 500 * time.millisecond
	client.shutdown() or {}
	server_thread.wait()
}
