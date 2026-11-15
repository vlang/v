module tls

import crypto.pem
import crypto.x509
import os

fn tls_testdata(name string) string {
	return os.join_path(@VEXEROOT, 'vlib/net/tls/testdata/' + name)
}

fn load_test_ca(name string) x509.Certificate {
	txt := os.read_file(tls_testdata(name)) or { panic(err) }
	block, _ := pem.decode(txt) or { panic('pem decode failed') }
	return x509.parse_der(block.data) or { panic(err) }
}

fn test_certificate_request_codec() {
	schemes_data := signature_algorithms_data()
	body := encode_certificate_request(schemes_data) or { panic(err) }
	schemes, ctx := parse_certificate_request(body) or { panic(err) }
	assert ctx.len == 0
	assert schemes.len > 0
	assert schemes[0] == u16(0x0403)
	// trailing garbage is rejected
	mut bad := body.clone()
	bad << u8(0)
	parse_certificate_request(bad) or { return }
	panic('trailing data accepted')
}

fn test_certificate_body_codec() {
	leaf_txt := os.read_file(tls_testdata('p384_leaf_cert.pem')) or { panic(err) }
	block, _ := pem.decode(leaf_txt) or { panic('pem decode failed') }
	body := encode_certificate_body([]u8{}, [block.data]) or { panic(err) }
	chain, ctx := parse_client_cert_chain(body) or { panic(err) }
	assert ctx.len == 0
	assert chain.len == 1
	assert chain[0] == block.data
	// empty chains round-trip (the no-credential signal)
	empty := encode_certificate_body([]u8{}, [][]u8{}) or { panic(err) }
	echain, _ := parse_client_cert_chain(empty) or { panic(err) }
	assert echain.len == 0
}

fn test_server_flight_includes_cr() {
	cfg := Config{
		verify:        false
		cert:          tls_testdata('rsa_leaf_cert.pem')
		key:           tls_testdata('rsa_leaf_pkcs8_key.pem')
		verify_client: true
	}
	mut server := new_server_handshake_with_config(.off, cfg) or { panic(err) }
	assert server.verify_client
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	ch_msg := chs.build_client_hello() or { panic(err) }
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	mut types := []HandshakeType{}
	mut off := 0
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		types << parsed.typ
		off += n
	}
	assert types == [HandshakeType.server_hello, .encrypted_extensions, .certificate_request,
		.certificate, .certificate_verify, .finished]
}

fn serve_mtls_once(mut l Listener, ch chan string) {
	mut conn := l.accept() or {
		ch <- 'accept err: ${err}'
		return
	}
	defer {
		conn.close() or {}
	}
	mut buf := []u8{len: 4}
	n := conn.read(mut buf) or {
		ch <- 'read err: ${err}'
		return
	}
	if n > 0 {
		conn.write(buf[..n]) or {
			ch <- 'write err: ${err}'
			return
		}
	}
	ch <- 'ok'
}

fn test_mtls_loopback() {
	server_ca := load_test_ca('p384_ca_cert.pem')
	client_ca := load_test_ca('rsa_ca_cert.pem')
	mut l := listen(Config{
		verify:        false
		cert:          tls_testdata('rsa_leaf_cert.pem')
		key:           tls_testdata('rsa_leaf_pkcs8_key.pem')
		verify_client: true
		verify_chain:  true
		ca_store:      CertStore{
			roots: [server_ca]
		}
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn serve_mtls_once(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{
		verify:       true
		server_name:  'localhost'
		verify_chain: true
		ca_store:     CertStore{
			roots: [client_ca]
		}
		client_cert:  tls_testdata('p384_leaf_cert.pem')
		client_key:   tls_testdata('p384_leaf_key.pem')
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	c.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { panic(err) }
	assert n == 4
	assert buf == 'ping'.bytes()
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

fn test_mtls_rejects_missing_client_cert() {
	server_ca := load_test_ca('p384_ca_cert.pem')
	mut l := listen(Config{
		verify:        false
		cert:          tls_testdata('rsa_leaf_cert.pem')
		key:           tls_testdata('rsa_leaf_pkcs8_key.pem')
		verify_client: true
		verify_chain:  true
		ca_store:      CertStore{
			roots: [server_ca]
		}
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn serve_mtls_once(mut l, ch)
	// No client_cert configured: the client sends an empty Certificate and
	// the server must abort the handshake instead of echoing.
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	c.write('ping'.bytes()) or {}
	mut buf := []u8{len: 4}
	c.read(mut buf) or {
		assert (<-ch).contains('certificate required')
		c.close() or {}
		l.close() or {}
		_ = server
		return
	}
	assert false, 'server echoed despite missing client certificate'
}

fn test_mtls_rejects_untrusted_client_cert() {
	// Server trusts only the RSA CA; the client presents a P-384 leaf from a
	// different CA. A server that skipped chain verification would echo.
	server_ca := load_test_ca('rsa_ca_cert.pem')
	mut l := listen(Config{
		verify:        false
		cert:          tls_testdata('rsa_leaf_cert.pem')
		key:           tls_testdata('rsa_leaf_pkcs8_key.pem')
		verify_client: true
		verify_chain:  true
		ca_store:      CertStore{
			roots: [server_ca]
		}
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn serve_mtls_once(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	// Present a valid P-384 leaf the server does not trust.
	c.cfg.client_cert = tls_testdata('p384_leaf_cert.pem')
	c.cfg.client_key = tls_testdata('p384_leaf_key.pem')
	c.handshake() or { panic(err) }
	c.write('ping'.bytes()) or {}
	mut buf := []u8{len: 4}
	c.read(mut buf) or {
		assert (<-ch).contains('untrusted')
		c.close() or {}
		l.close() or {}
		_ = server
		return
	}
	assert false, 'server echoed for an untrusted client certificate'
}

fn test_mtls_verify_tampered_certificate_verify() {
	server_ca := load_test_ca('p384_ca_cert.pem')
	client_cfg := Config{
		client_cert: tls_testdata('p384_leaf_cert.pem')
		client_key:  tls_testdata('p384_leaf_key.pem')
	}
	// Drive a server handshake to the point of client auth, reusing the real
	// transcript so only the CV signature is at fault.
	server_cfg := Config{
		verify:        false
		cert:          tls_testdata('rsa_leaf_cert.pem')
		key:           tls_testdata('rsa_leaf_pkcs8_key.pem')
		verify_client: true
		verify_chain:  true
		ca_store:      CertStore{
			roots: [server_ca]
		}
	}
	mut server := new_server_handshake_with_config(.off, server_cfg) or { panic(err) }
	mut client := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	ch_msg := client.build_client_hello() or { panic(err) }
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	mut raws := [][]u8{}
	mut o := 0
	for o < flight.len {
		_, n := parse_handshake_message(flight[o..]) or { panic(err) }
		raws << flight[o..o + n]
		o += n
	}
	client.process_server_hello(raws[0]) or { panic(err) }
	mut ctrans := ch_msg.clone()
	ctrans << raws[0]
	mut schemes := []u16{}
	mut ctx := []u8{}
	for r in raws[1..] {
		parsed, _ := parse_handshake_message(r) or { panic(err) }
		if parsed.typ == .certificate_request {
			schemes, ctx = parse_certificate_request(parsed.body) or { panic(err) }
		}
		ctrans << r
	}
	assert schemes.len > 0
	auth := build_client_auth_messages(client_cfg, ctx, schemes, client.suite, ctrans) or {
		panic(err)
	}
	assert auth.len == 2
	p0, _ := parse_handshake_message(auth[0]) or { panic(err) }
	p1, _ := parse_handshake_message(auth[1]) or { panic(err) }
	// Valid flight verifies.
	server.verify_client_certificate(p0.body, p1.body) or { panic(err) }
	// Tampered signature must fail.
	mut bad_cv := p1.body.clone()
	bad_cv[bad_cv.len - 1] ^= u8(1)
	mut server2 := new_server_handshake_with_config(.off, server_cfg) or { panic(err) }
	_ = server2.process_client_hello(ch_msg) or { panic(err) }
	server2.verify_client_certificate(p0.body, bad_cv) or { return }
	panic('tampered CertificateVerify accepted')
	// Missing CertificateVerify with a non-empty Certificate must fail.
	mut server3 := new_server_handshake_with_config(.off, server_cfg) or { panic(err) }
	_ = server3.process_client_hello(ch_msg) or { panic(err) }
	server3.verify_client_certificate(p0.body, []u8{}) or { return }
	panic('missing CertificateVerify accepted')
	// A non-empty request-context echo must fail (the server always sends an
	// empty context).
	chain0, _ := parse_client_cert_chain(p0.body) or { panic(err) }
	bad_ctx_msg := encode_handshake_message(.certificate, encode_certificate_body([u8(9)],
		chain0)!)!
	bp, _ := parse_handshake_message(bad_ctx_msg) or { panic(err) }
	mut server4 := new_server_handshake_with_config(.off, server_cfg) or { panic(err) }
	_ = server4.process_client_hello(ch_msg) or { panic(err) }
	server4.verify_client_certificate(bp.body, p1.body) or { return }
	panic('bad request context accepted')
}
