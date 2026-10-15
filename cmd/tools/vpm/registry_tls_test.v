module main

import net
import net.mbedtls
import os
import time

// These existing fixtures contain a CA and a leaf for localhost, valid until 2050.
const registry_tls_fixtures = @VMODROOT + '/examples/ssl_server/cert'
const registry_tls_test_root = os.join_path(os.vtmp_dir(), 'vpm_registry_tls_tests')

struct RunningRegistryTls {
	base     string
	port     int
	requests chan string
	stop     chan bool
mut:
	worker thread
	closed bool
}

fn registry_tls_clear_env() {
	for name in [registry_url_env, registry_ca_file_env, registry_insecure_loopback_env, 'VPM_TOKEN',
		'VPM_REGISTRY_TOKEN_HOST_6C6F63616C686F7374', 'VPM_REGISTRY_TOKEN_HOST_3132372E302E302E31',
		'SSL_CERT_FILE', 'SSL_CERT_DIR'] {
		os.unsetenv(name)
	}
}

fn testsuite_begin() {
	registry_tls_clear_env()
	os.mkdir_all(registry_tls_test_root)!
}

fn testsuite_end() {
	registry_tls_clear_env()
	os.rmdir_all(registry_tls_test_root)!
}

// The listener uses mbedTLS directly so the same real peer also works when the
// client is built with -d use_openssl. Its owner joins the worker before checking
// an empty capture channel, so rejected handshakes cannot hide a late HTTP request.
fn start_registry_tls() !RunningRegistryTls {
	return start_registry_tls_redirect('/capture')
}

fn start_registry_tls_redirect(redirect_target string) !RunningRegistryTls {
	mut reservation := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := reservation.addr()!.port()!
	reservation.close()!
	mut listener := mbedtls.new_ssl_listener('127.0.0.1:${port}', mbedtls.SSLConnectConfig{
		cert:         os.join_path(registry_tls_fixtures, 'server.crt')
		cert_key:     os.join_path(registry_tls_fixtures, 'server.key')
		read_timeout: 2 * time.second
	})!
	requests := chan string{cap: 8}
	stop := chan bool{cap: 1}
	worker := spawn serve_registry_tls(mut listener, requests, stop, redirect_target)
	return RunningRegistryTls{
		base:     'https://localhost:${port}'
		port:     port
		requests: requests
		stop:     stop
		worker:   worker
	}
}

fn serve_registry_tls(mut listener mbedtls.SSLListener, requests chan string, stop chan bool, redirect_target string) {
	defer { listener.shutdown() or {} }
	for {
		select {
			_ := <-stop {
				return
			}
			else {
			}
		}
		mut connection := listener.accept_with_timeouts(50 * time.millisecond, 10 * time.second) or {
			continue
		}
		serve_registry_tls_connection(mut connection, requests, redirect_target)
		connection.shutdown() or {}
	}
}

fn serve_registry_tls_connection(mut connection mbedtls.SSLConn, requests chan string, redirect_target string) {
	mut request := ''
	mut buffer := []u8{len: 4096}
	for !request.contains('\r\n\r\n') {
		count := connection.read(mut buffer) or { return }
		if count == 0 { return }
		request += buffer[..count].bytestr()
		if request.len > 16384 { panic('unexpected oversized loopback request') }
	}
	mut authorization := ''
	for line in request.split('\r\n') {
		if line.to_lower().starts_with('authorization:') {
			authorization = line.all_after(':').trim_space()
		}
	}
	requests <- authorization
	response := if request.all_before('\r\n').contains(' /redirect ') {
		'HTTP/1.1 302 Found\r\nLocation: ${redirect_target}\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'
	} else {
		'HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok'
	}
	mut written := 0
	for written < response.len {
		count := connection.write_string(response[written..]) or { return }
		if count == 0 { return }
		written += count
	}
}

fn (mut running RunningRegistryTls) close() {
	if running.closed { return }
	running.closed = true
	running.stop <- true
	running.worker.wait()
}

fn registry_tls_configure_token(base string) {
	os.setenv(registry_url_env, base, true)
	os.setenv('VPM_TOKEN', 'tls-test-token', true)
}

fn registry_tls_capture(requests chan string, expected string) {
	select {
		seen := <-requests {
			assert seen == expected
		}
		else {
			assert false, 'the TLS peer did not receive the expected HTTP request'
		}
	}
}

fn registry_tls_no_http(requests chan string) {
	select {
		seen := <-requests {
			assert false, 'a rejected TLS peer received HTTP Authorization `${seen}`'
		}
		else {
			assert true
		}
	}
}

fn registry_tls_expect_rejection(base string) {
	mut rejected := false
	registry_http_get(base + '/capture') or { rejected = true }
	assert rejected, 'registry HTTPS must reject an unverified TLS connection'
}

fn test_registry_default_system_bundle_is_loadable() {
	registry_tls_clear_env()
	roots := registry_tls_ca_file()!
	$if windows && !no_vschannel ? {
		assert roots == ''
	} $else $if use_openssl ? {
		assert roots == ''
	} $else {
		assert roots != ''
		mut certs := mbedtls.new_sslcerts_from_file(roots, '', '')!
		certs.cleanup()
	}
}

fn test_registry_rejects_an_untrusted_tls_peer_before_sending_the_token() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	registry_tls_configure_token(running.base)
	// The development HTTP exception must not relax any HTTPS validation.
	os.setenv(registry_insecure_loopback_env, '1', true)
	registry_tls_expect_rejection(running.base)
	running.close()
	registry_tls_no_http(running.requests)
}

fn test_registry_trusted_ca_authenticates_the_tls_peer_and_sends_the_token() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	registry_tls_configure_token(running.base)
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
	$if windows && !no_vschannel ? {
		registry_tls_expect_rejection(running.base)
		running.close()
		registry_tls_no_http(running.requests)
	} $else {
		response := registry_http_get(running.base + '/capture')!
		assert response.status_code == 200
		assert response.body == 'ok'
		running.close()
		registry_tls_capture(running.requests, 'Bearer tls-test-token')
	}
}

fn test_registry_trusted_ca_still_rejects_the_wrong_hostname() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	base := 'https://127.0.0.1:${running.port}'
	registry_tls_configure_token(base)
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
	registry_tls_expect_rejection(base)
	running.close()
	registry_tls_no_http(running.requests)
}

fn test_registry_rejects_a_ca_that_did_not_issue_the_tls_certificate() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	registry_tls_configure_token(running.base)
	os.setenv(registry_ca_file_env,
		@VMODROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt', true)
	registry_tls_expect_rejection(running.base)
	running.close()
	registry_tls_no_http(running.requests)
}

fn test_registry_invalid_custom_ca_files_fail_closed() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	registry_tls_configure_token(running.base)
	invalid := os.join_path(registry_tls_test_root, 'invalid-ca.pem')
	empty := os.join_path(registry_tls_test_root, 'empty-ca.pem')
	os.write_file(invalid, 'this is not a certificate')!
	os.write_file(empty, '')!
	for path in ['', os.join_path(registry_tls_test_root, 'missing-ca.pem'), empty, invalid] {
		os.setenv(registry_ca_file_env, path, true)
		registry_tls_expect_rejection(running.base)
	}
	running.close()
	registry_tls_no_http(running.requests)
}

fn test_registry_tls_redirect_retains_trust_validation_and_the_token() {
	$if windows && !no_vschannel ? {
		return // The explicit-CA rejection is covered above; native SChannel ignores verify.
	}
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	registry_tls_configure_token(running.base)
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
	response := registry_http_get(running.base + '/redirect')!
	assert response.status_code == 200
	assert response.body == 'ok'
	running.close()
	registry_tls_capture(running.requests, 'Bearer tls-test-token')
	registry_tls_capture(running.requests, 'Bearer tls-test-token')
}

fn test_registry_ca_setting_does_not_add_credentials_to_public_https() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	registry_tls_configure_token(running.base)
	os.setenv('VPM_REGISTRY_TOKEN_HOST_6C6F63616C686F7374', 'scoped-tls-token', true)
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_test_root, 'missing-ca.pem'), true)
	response := vpm_http_get(running.base + '/health')!
	assert response.status_code == 200
	running.close()
	registry_tls_capture(running.requests, '')
}

fn test_tokenless_registry_https_rejects_untrusted_ca_and_wrong_hostname() {
	for mode in ['untrusted', 'wrong-ca', 'wrong-hostname'] {
		registry_tls_clear_env()
		mut running := start_registry_tls()!
		defer {
			running.close()
			registry_tls_clear_env()
		}
		mut base := running.base
		if mode == 'wrong-ca' {
			os.setenv(registry_ca_file_env,
				@VMODROOT + '/vlib/net/websocket/tests/autobahn/fuzzing_server_wss/config/server.crt', true)
		} else if mode == 'wrong-hostname' {
			os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
			base = 'https://127.0.0.1:${running.port}'
		}
		assert registry_token(base) == ''
		registry_tls_expect_rejection(base)
		running.close()
		registry_tls_no_http(running.requests)
	}
}

fn test_tokenless_registry_https_uses_the_trusted_ca_and_keeps_redirect_validation() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
	assert registry_token(running.base) == ''
	$if windows && !no_vschannel ? {
		registry_tls_expect_rejection(running.base)
		running.close()
		registry_tls_no_http(running.requests)
	} $else {
		for path in ['/capture', '/redirect'] {
			response := registry_http_get(running.base + path)!
			assert response.status_code == 200
			assert response.body == 'ok'
		}
		running.close()
		for _ in 0 .. 3 {
			registry_tls_capture(running.requests, '')
		}
	}
}

fn test_tokenless_registry_https_rejects_invalid_custom_ca_settings() {
	registry_tls_clear_env()
	mut running := start_registry_tls()!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	invalid := os.join_path(registry_tls_test_root, 'tokenless-invalid-ca.pem')
	empty := os.join_path(registry_tls_test_root, 'tokenless-empty-ca.pem')
	os.write_file(invalid, 'this is not a certificate')!
	os.write_file(empty, '')!
	for path in ['', os.join_path(registry_tls_test_root, 'tokenless-missing-ca.pem'), empty, invalid] {
		os.setenv(registry_ca_file_env, path, true)
		registry_tls_expect_rejection(running.base)
	}
	running.close()
	registry_tls_no_http(running.requests)
}

fn test_tokenless_registry_https_redirect_rejects_an_unverified_target() {
	$if windows && !no_vschannel ? {
		return
	}
	registry_tls_clear_env()
	mut target := start_registry_tls()!
	mut source := start_registry_tls_redirect('https://127.0.0.1:${target.port}/capture')!
	defer {
		source.close()
		target.close()
		registry_tls_clear_env()
	}
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
	mut rejected := false
	registry_http_get(source.base + '/redirect') or { rejected = true }
	assert rejected, 'a tokenless redirect accepted the wrong TLS hostname'
	source.close()
	target.close()
	registry_tls_capture(source.requests, '')
	registry_tls_no_http(target.requests)
}

fn test_tokenless_registry_https_redirect_refuses_plaintext_downgrade() {
	$if windows && !no_vschannel ? {
		return
	}
	registry_tls_clear_env()
	mut running := start_registry_tls_redirect('http://127.0.0.1:1/capture')!
	defer {
		running.close()
		registry_tls_clear_env()
	}
	os.setenv(registry_ca_file_env, os.join_path(registry_tls_fixtures, 'ca.crt'), true)
	registry_http_get(running.base + '/redirect') or {
		assert err.msg().contains('refusing to downgrade registry HTTPS'), err.msg()
		running.close()
		registry_tls_capture(running.requests, '')
		return
	}
	assert false, 'a tokenless HTTPS request followed a plaintext redirect'
}
