module http

import net.urllib
import time

fn test_tls13_adapter_rejects_http2() {
	req := Request{
		enable_http2: true
		use_tls13:    true
	}
	req.method_and_url_to_response(.get, urllib.parse('https://example.com/')!, '', Header{}) or {
		assert err.msg() == 'net.http TLS 1.3 adapter: HTTP/2 is not supported'
		return
	}
	assert false, 'TLS 1.3 HTTP/1 adapter accepted an HTTP/2 request'
}

fn test_tls13_adapter_rejects_http3() {
	req := Request{
		enable_http3: true
		use_tls13:    true
	}
	req.dial_tls13(443, 'example.com') or {
		assert err.msg() == 'net.http TLS 1.3 adapter: HTTP/3 is not supported'
		return
	}
	assert false, 'TLS 1.3 TCP adapter accepted an HTTP/3 request'
}

fn test_tls13_adapter_rejects_client_certificates() {
	req := Request{
		cert:      'client.pem'
		use_tls13: true
	}
	req.method_and_url_to_response(.get, urllib.parse('https://example.com/')!, '', Header{}) or {
		assert err.msg() == 'net.http TLS 1.3 adapter: client certificates are not supported'
		return
	}
	assert false, 'TLS 1.3 HTTP adapter ignored a configured client certificate'
}

fn test_tls13_adapter_rejects_proxy() {
	req := Request{
		proxy:     &HttpProxy{}
		use_tls13: true
	}
	req.method_and_url_to_response(.get, urllib.parse('https://example.com/')!, '', Header{}) or {
		assert err.msg() == 'net.http TLS 1.3 adapter: proxies are not supported'
		return
	}
	assert false, 'TLS 1.3 HTTP adapter accepted a proxy'
}

fn test_tls13_option_is_copied_from_fetch_config() {
	req := prepare(url: 'https://example.com/', use_tls13: true) or {
		assert false, 'prepare failed: ${err}'
		return
	}
	assert req.use_tls13
}

fn test_tls13_pool_key_separates_backend_and_timeouts() {
	standard := Request{}
	tls13_req := Request{
		use_tls13: true
	}
	assert transport_pool_key(standard, 'https', 'example.com', 443) != transport_pool_key(tls13_req,
		'https', 'example.com', 443)
	tls13_different_timeout := Request{
		use_tls13:    true
		read_timeout: 5 * time.second
	}
	assert transport_pool_key(tls13_req, 'https', 'example.com', 443) != transport_pool_key(tls13_different_timeout,
		'https', 'example.com', 443)
}

fn test_tls13_timeout_uses_shorter_positive_bound() {
	assert tls13_timeout(5 * time.second, 2 * time.second) == 2 * time.second
	assert tls13_timeout(0, 3 * time.second) == 3 * time.second
	assert tls13_timeout(0, 0) == 30 * time.second
}

fn test_tls13_config_maps_validation_and_timeouts() {
	config := tls13_config(Request{
		validate:      true
		read_timeout:  5 * time.second
		write_timeout: 2 * time.second
	}, 'example.com') or {
		assert false, 'TLS config failed: ${err}'
		return
	}
	assert config.server_name == 'example.com'
	assert !config.insecure_skip_verify
	assert config.timeout == 2 * time.second
}
