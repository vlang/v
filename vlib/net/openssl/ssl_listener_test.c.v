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
	spawn close_tcp_connection_after(mut tcp_client, time.second)

	mut ssl_conn := listener.accept_without_handshake()!
	defer {
		ssl_conn.shutdown() or {}
	}
	ssl_conn.duration = 100 * time.millisecond
	stopwatch := time.new_stopwatch()
	ssl_conn.accept_handshake() or {
		assert stopwatch.elapsed() < 500 * time.millisecond
		return
	}
	assert false, 'TLS handshake unexpectedly succeeded without receiving client TLS data'
}
