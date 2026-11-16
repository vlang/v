module mbedtls

import net
import os
import time

#include "@VMODROOT/vlib/net/mbedtls/testdata/server_read_timeout.h"

fn C.v_mbedtls_test_config_read_timeout(&C.mbedtls_ssl_config) u32
fn C.v_mbedtls_test_active_read_timeout(&C.mbedtls_ssl_context) u32
fn C.v_mbedtls_test_remove_rng(&C.mbedtls_ssl_config)

fn new_read_timeout_test_listener() !&SSLListener {
	cert_dir := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert')
	return new_ssl_listener('127.0.0.1:0', SSLConnectConfig{
		cert:         os.join_path(cert_dir, 'server.crt')
		cert_key:     os.join_path(cert_dir, 'server.key')
		read_timeout: 2 * time.second
	})
}

fn accept_read_timeout_test_peer(mut listener SSLListener, timed_accept bool) &SSLConn {
	if timed_accept {
		return listener.accept_with_timeouts(2 * time.second, 2 * time.second) or { panic(err) }
	}
	return listener.accept() or { panic(err) }
}

fn assert_listener_accepts_after_peer_cleanup(mut listener SSLListener, port int) ! {
	mut server := spawn accept_read_timeout_test_peer(mut listener, false)
	mut client := new_ssl_conn(validate: false)!
	defer { client.shutdown() or {} }
	client.dial('127.0.0.1', port)!
	mut conn := server.wait()
	defer { conn.shutdown() or {} }
	assert C.v_mbedtls_test_active_read_timeout(&conn.ssl) == 2000
	assert client.write_string('alive')! == 5
	mut data := []u8{len: 5}
	assert conn.read(mut data)! == 5
	assert data.bytestr() == 'alive'
}

fn test_accepted_connections_have_independent_native_read_timeouts() ! {
	mut listener := new_read_timeout_test_listener()!
	defer { listener.shutdown() or {} }
	port := net.addr_from_socket_handle(listener.server_fd.fd).port()!
	mut first := spawn accept_read_timeout_test_peer(mut listener, false)
	mut first_client := new_ssl_conn(validate: false)!
	defer { first_client.shutdown() or {} }
	first_client.dial('127.0.0.1', port)!
	mut first_conn := first.wait()
	defer { first_conn.shutdown() or {} }

	mut second := spawn accept_read_timeout_test_peer(mut listener, true)
	mut second_client := new_ssl_conn(validate: false)!
	defer { second_client.shutdown() or {} }
	second_client.dial('127.0.0.1', port)!
	mut second_conn := second.wait()
	defer { second_conn.shutdown() or {} }

	first_conn.set_read_timeout(120 * time.millisecond)
	second_conn.set_read_timeout(240 * time.millisecond)
	// Inspect the configuration used by the native read, not just the V fields.
	assert C.v_mbedtls_test_active_read_timeout(&first_conn.ssl) == 120
	assert C.v_mbedtls_test_active_read_timeout(&second_conn.ssl) == 240
	assert C.v_mbedtls_test_active_read_timeout(&listener.ssl) == 2000

	mut data := []u8{len: 5}
	mut first_error := 0
	first_conn.read(mut data) or { first_error = err.code() }
	assert first_error == C.MBEDTLS_ERR_SSL_TIMEOUT
	mut second_error := 0
	second_conn.read(mut data) or { second_error = err.code() }
	assert second_error == C.MBEDTLS_ERR_SSL_TIMEOUT
	// Deadline-bound reads must not leave the native configuration shortened.
	assert C.v_mbedtls_test_active_read_timeout(&first_conn.ssl) == 120
	assert C.v_mbedtls_test_active_read_timeout(&second_conn.ssl) == 240

	first_conn.shutdown()!
	assert C.v_mbedtls_test_config_read_timeout(&first_conn.conf) == 0
	assert C.v_mbedtls_test_active_read_timeout(&second_conn.ssl) == 240
	assert C.v_mbedtls_test_active_read_timeout(&listener.ssl) == 2000
	assert second_client.write_string('alive')! == 5
	assert second_conn.read(mut data)! == 5
	assert data.bytestr() == 'alive'
	second_conn.shutdown()!
	assert_listener_accepts_after_peer_cleanup(mut listener, port)!

	// Client configurations still own their allocations and are cleared on close.
	first_client.set_read_timeout(360 * time.millisecond)
	assert C.v_mbedtls_test_active_read_timeout(&first_client.ssl) == 360
	first_client.shutdown()!
	assert C.v_mbedtls_test_config_read_timeout(&first_client.conf) == 0
}

fn accept_read_timeout_test_error(mut listener SSLListener, timed_accept bool) int {
	mut conn := if timed_accept {
		listener.accept_with_timeouts(2 * time.second, 2 * time.second) or { return err.code() }
	} else {
		listener.accept() or { return err.code() }
	}
	conn.shutdown() or {}
	return 0
}

fn test_accepted_setup_failure_preserves_listener_configuration() ! {
	mut listener := new_read_timeout_test_listener()!
	defer { listener.shutdown() or {} }
	port := net.addr_from_socket_handle(listener.server_fd.fd).port()!
	for timed_accept in [false, true] {
		original := listener.conf
		C.v_mbedtls_test_remove_rng(&listener.conf)
		server := spawn accept_read_timeout_test_error(mut listener, timed_accept)
		mut client := net.dial_tcp('127.0.0.1:${port}')!
		client.close()!
		setup_error := server.wait()
		listener.conf = original
		assert setup_error == C.MBEDTLS_ERR_SSL_NO_RNG
		assert C.v_mbedtls_test_active_read_timeout(&listener.ssl) == 2000
		assert_listener_accepts_after_peer_cleanup(mut listener, port)!
	}
}

fn test_accepted_handshake_failure_preserves_listener_configuration() ! {
	mut listener := new_read_timeout_test_listener()!
	defer { listener.shutdown() or {} }
	port := net.addr_from_socket_handle(listener.server_fd.fd).port()!
	for timed_accept in [false, true] {
		server := spawn accept_read_timeout_test_error(mut listener, timed_accept)
		mut client := net.dial_tcp('127.0.0.1:${port}')!
		client.write_string('not a TLS record')!
		client.close()!
		assert server.wait() != 0
		assert C.v_mbedtls_test_active_read_timeout(&listener.ssl) == 2000
		assert_listener_accepts_after_peer_cleanup(mut listener, port)!
	}
}
