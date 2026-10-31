// vtest build: present_openssl? && !(windows && tinyc)
module openssl

import net
import os
import time

struct SSLUpgradeTestResult {
	accepted        bool
	error_message   string
	alpn            string
	client_ca_names int
	socket_open     bool
	client_received string
}

fn upgrade_test_config(in_memory bool, validate bool) !SSLConnectConfig {
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	cert := os.join_path(cert_dir, 'server.crt')
	key := os.join_path(cert_dir, 'server.key')
	ca := os.join_path(cert_dir, 'client.crt')
	return SSLConnectConfig{
		cert:                   if in_memory { os.read_file(cert)! } else { cert }
		cert_key:               if in_memory { os.read_file(key)! } else { key }
		verify:                 if in_memory { os.read_file(ca)! } else { ca }
		validate:               validate
		in_memory_verification: in_memory
	}
}

fn accept_upgrade_for_test(mut listener net.TcpListener, config SSLConnectConfig) SSLUpgradeTestResult {
	mut tcp_conn := listener.accept() or {
		return SSLUpgradeTestResult{
			error_message: err.msg()
		}
	}
	defer {
		tcp_conn.close() or {}
	}
	tcp_conn.set_read_timeout(2 * time.second)
	mut conn := new_ssl_server_conn(mut tcp_conn, config) or {
		return SSLUpgradeTestResult{
			error_message: err.msg()
			socket_open:   if _ := tcp_conn.addr() { true } else { false }
		}
	}
	alpn := conn.negotiated_alpn()
	ca_names := C.v_net_openssl_SSL_CTX_client_CA_names_count(conn.sslctx)
	conn.write('ok'.bytes()) or {}
	conn.duration = 100 * time.millisecond
	conn.shutdown() or {}
	return SSLUpgradeTestResult{
		accepted:        true
		alpn:            alpn
		client_ca_names: ca_names
		socket_open:     if _ := tcp_conn.addr() { true } else { false }
	}
}

fn run_upgrade_for_test(server_config SSLConnectConfig, client_config SSLConnectConfig) !SSLUpgradeTestResult {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	listener.set_accept_timeout(2 * time.second)
	mut tcp_client := net.dial_tcp(listener.addr()!.str())!
	tcp_client.set_read_timeout(2 * time.second)
	server_thread := spawn accept_upgrade_for_test(mut listener, server_config)
	mut client := new_ssl_conn(client_config)!
	mut received := ''
	if _ := client.connect(mut tcp_client, 'localhost') {
		mut buffer := []u8{len: 2}
		n := client.read(mut buffer) or { 0 }
		received = buffer[..n].bytestr()
	}
	client.duration = 100 * time.millisecond
	client.shutdown() or {}
	tcp_client.close() or {}
	result := server_thread.wait()
	return SSLUpgradeTestResult{
		...result
		client_received: received
	}
}

fn test_ssl_upgrade_requires_client_certificate() ! {
	for in_memory in [false, true] {
		result := run_upgrade_for_test(upgrade_test_config(in_memory, true)!, SSLConnectConfig{})!
		assert !result.accepted, 'server accepted a client without its required certificate'
		assert result.socket_open, 'a failed TLS upgrade closed the borrowed TCP socket'
	}
}

fn test_ssl_upgrade_accepts_trusted_client_certificate() ! {
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	for in_memory in [false, true] {
		result := run_upgrade_for_test(upgrade_test_config(in_memory, true)!, SSLConnectConfig{
			cert:     os.join_path(cert_dir, 'client.crt')
			cert_key: os.join_path(cert_dir, 'client.key')
		})!
		assert result.accepted, result.error_message
		assert result.client_received == 'ok'
		assert result.client_ca_names > 0
		assert result.socket_open, 'TLS shutdown closed the borrowed TCP socket'
	}
}

fn test_ssl_upgrade_rejects_untrusted_client_certificate() ! {
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	for in_memory in [false, true] {
		config := upgrade_test_config(in_memory, true)!
		server_cert := os.join_path(cert_dir, 'server.crt')
		result := run_upgrade_for_test(SSLConnectConfig{
			...config
			verify: if in_memory { os.read_file(server_cert)! } else { server_cert }
		}, SSLConnectConfig{
			cert:     os.join_path(cert_dir, 'client.crt')
			cert_key: os.join_path(cert_dir, 'client.key')
		})!
		assert !result.accepted, 'server accepted an untrusted client certificate'
		assert result.socket_open
	}
}

fn test_ssl_upgrade_negotiates_alpn() ! {
	config := upgrade_test_config(true, false)!
	result := run_upgrade_for_test(SSLConnectConfig{
		...config
		alpn_protocols: ['h2', 'http/1.1']
	}, SSLConnectConfig{
		alpn_protocols: ['http/1.1']
	})!
	assert result.accepted, result.error_message
	assert result.client_received == 'ok'
	assert result.alpn == 'http/1.1'
	assert result.socket_open
}

fn test_ssl_upgrade_rejects_invalid_configuration() ! {
	config := upgrade_test_config(true, true)!
	file_config := upgrade_test_config(false, true)!
	configs := [SSLConnectConfig{
		...config
		verify: ''
	}, SSLConnectConfig{
		...config
		verify: 'invalid CA certificate'
	}, SSLConnectConfig{
		...file_config
		verify: os.join_path(@VMODROOT, 'missing-upgrade-ca.pem')
	}, SSLConnectConfig{
		...config
		alpn_protocols: ['']
	}, SSLConnectConfig{
		...config
		alpn_protocols: ['a'.repeat(256)]
	}]
	expected_errors := ['no root CA provided', 'could not load root CA from memory',
		'SSL_CTX_load_verify_locations failed', 'invalid ALPN protocol', 'invalid ALPN protocol']
	for i, server_config in configs {
		result := run_upgrade_for_test(server_config, SSLConnectConfig{})!
		assert !result.accepted
		assert result.error_message.contains(expected_errors[i]), result.error_message
		assert result.socket_open
	}
}
