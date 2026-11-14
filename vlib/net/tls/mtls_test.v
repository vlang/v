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

fn serve_mtls_once(mut l Listener) {
	mut conn := l.accept() or { return }
	defer {
		conn.close() or {}
	}
	mut buf := []u8{len: 4}
	n := conn.read(mut buf) or { return }
	if n > 0 {
		conn.write(buf[..n]) or {}
	}
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
	server := spawn serve_mtls_once(mut l)
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
	server := spawn serve_mtls_once(mut l)
	// No client_cert configured: the client sends an empty Certificate and
	// the server must abort the handshake instead of echoing.
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	c.write('ping'.bytes()) or {}
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { return }
	assert n != 4 || buf != 'ping'.bytes()
	c.close() or {}
	l.close() or {}
	_ = server
}
