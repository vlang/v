// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Tests for the standalone TLS 1.2 module: unit checks (PRF, record layer,
// P-256 ECDH, RSA sign/verify, cert parsing, message framing), in-process
// loopback handshakes (RSA/ECDSA, GCM/CBC), and openssl cross-checks
// (s_server for our client, s_client for our server) when openssl exists.
module tls12

import crypto.sha256
import net
import os
import time

fn testdata_path(name string) string {
	return os.join_path(os.dir(@FILE), 'testdata', name)
}

fn openssl_bin() string {
	if p := os.find_abs_path_of_executable('openssl') {
		return p
	}
	return ''
}

// ---------- unit tests ----------

fn test_prf_sha256_shape() {
	a := prf_sha256('secret'.bytes(), 'label'.bytes(), 'seed'.bytes(), 48)
	assert a.len == 48
	b := prf_sha256('secret'.bytes(), 'label'.bytes(), 'seed'.bytes(), 48)
	assert a == b
	c := prf_sha256('other'.bytes(), 'label'.bytes(), 'seed'.bytes(), 48)
	assert a != c
	// Output length is honored exactly (multi-block P_hash).
	d := prf_sha256('secret'.bytes(), 'label'.bytes(), 'seed'.bytes(), 100)
	assert d.len == 100
	assert d[..48] == a
	// Finished verify_data is 12 bytes.
	ms := master_secret([]u8{len: 32}, []u8{len: 32}, []u8{len: 32})
	assert ms.len == 48
	vd := finished_verify_data(ms, 'client finished', []u8{len: 32, init: 0xab})
	assert vd.len == 12
}

fn test_record_gcm_roundtrip() {
	key := []u8{len: 16, init: 0x0b}
	salt := []u8{len: 4, init: 0x0d}
	pt := 'hello tls12 gcm'.bytes()
	sealed := gcm_seal(key, salt, 7, rectype_application_data, pt)!
	assert sealed.len == 8 + pt.len + 16
	opened := gcm_open(key, salt, 7, rectype_application_data, sealed)!
	assert opened == pt
	// Wrong sequence number fails authentication.
	mut gcm_bad_seq := false
	gcm_open(key, salt, 8, rectype_application_data, sealed) or { gcm_bad_seq = true }
	assert gcm_bad_seq, 'GCM opened with wrong seq'
	// Wrong content type fails authentication.
	mut gcm_bad_type := false
	gcm_open(key, salt, 7, rectype_handshake, sealed) or { gcm_bad_type = true }
	assert gcm_bad_type, 'GCM opened with wrong type'
	// Empty plaintext round-trips.
	empty_sealed := gcm_seal(key, salt, 0, rectype_handshake, []u8{})!
	empty_open := gcm_open(key, salt, 0, rectype_handshake, empty_sealed)!
	assert empty_open.len == 0
}

fn test_record_cbc_roundtrip() {
	mac_key := []u8{len: 32, init: 0x11}
	enc_key := []u8{len: 16, init: 0x22}
	for msg in ['hi', 'exactly sixteen!!', 'a longer message spanning several AES blocks!!!'] {
		pt := msg.bytes()
		sealed := cbc_seal(mac_key, enc_key, 3, rectype_application_data, pt)!
		assert sealed.len % 16 == 0
		opened := cbc_open(mac_key, enc_key, 3, rectype_application_data, sealed)!
		assert opened == pt
	}
	// Tampered ciphertext fails.
	mut bad := cbc_seal(mac_key, enc_key, 3, rectype_application_data, 'tamper me'.bytes())!
	bad[20] ^= 0x01
	mut cbc_tamper_failed := false
	cbc_open(mac_key, enc_key, 3, rectype_application_data, bad) or { cbc_tamper_failed = true }
	assert cbc_tamper_failed, 'CBC opened tampered record'
	// Wrong sequence fails MAC.
	good := cbc_seal(mac_key, enc_key, 3, rectype_application_data, 'seq'.bytes())!
	mut cbc_seq_failed := false
	cbc_open(mac_key, enc_key, 4, rectype_application_data, good) or { cbc_seq_failed = true }
	assert cbc_seq_failed, 'CBC opened with wrong seq'
}

fn test_p256_ecdh_agreement() {
	priv_a, xa, ya := p256_keygen()!
	priv_b, xb, yb := p256_keygen()!
	sa := p256_ecdh(priv_a, xb, yb)!
	sb := p256_ecdh(priv_b, xa, ya)!
	assert sa.len == 32
	assert sa == sb
	// Shared secret is not trivially the private key or pubkey.
	assert sa != priv_a[..32]
}

fn test_p256_ecdsa_sign_verify() {
	priv, px, py := p256_keygen()!
	hash := sha256.sum256('ecdsa test'.bytes())
	r, s := p256_ecdsa_sign(priv, hash)!
	assert p256_ecdsa_verify(px, py, hash, r, s)
	// Wrong hash fails.
	bad_hash := sha256.sum256('ecdsa test!'.bytes())
	assert !p256_ecdsa_verify(px, py, bad_hash, r, s)
	// Swapped coordinates fail.
	assert !p256_ecdsa_verify(py, px, hash, r, s)
}

fn test_rsa_sign_verify_roundtrip() {
	key_der := os.read_bytes(testdata_path('rsa_key_pkcs8.der'))!
	key := parse_server_key_pkcs8(key_der)!
	assert key is RsaServerKey
	if key is RsaServerKey {
		msg := 'rsa test message'.bytes()
		sig := rsa_sign_pkcs1_v15(key.n, key.d, msg, oid_sha256_rsa)!
		nb, _ := key.n.bytes()
		assert sig.len == nb.len
		rsa_verify_pkcs1_v15(RsaPublicKey{
			n: key.n
			e: key.e
		}, sig, msg, oid_sha256_rsa)!
		bad := 'rsa test messagf'.bytes()
		mut rsa_tamper_failed := false
		rsa_verify_pkcs1_v15(RsaPublicKey{ n: key.n, e: key.e }, sig, bad, oid_sha256_rsa) or {
			rsa_tamper_failed = true
		}
		assert rsa_tamper_failed, 'RSA verified tampered message'
	}
}

fn test_parse_test_certs() {
	rsa_der := os.read_bytes(testdata_path('rsa_cert.der'))!
	rsa_cert := parse_certificate(rsa_der)!
	assert rsa_cert.common_name == 'localhost'
	assert rsa_cert.pubkey is RsaPublicKey
	// Self-signed: signature verifies with its own key.
	verify_cert_signature(rsa_cert, rsa_cert.pubkey)!
	check_validity(rsa_cert, time.utc())!
	ec_der := os.read_bytes(testdata_path('ec_cert.der'))!
	ec_cert := parse_certificate(ec_der)!
	assert ec_cert.pubkey is EcPublicKey
	verify_cert_signature(ec_cert, ec_cert.pubkey)!
	check_validity(ec_cert, time.utc())!
}

fn test_client_hello_roundtrip() {
	mut rnd := []u8{len: 32}
	for i in 0 .. 32 {
		rnd[i] = u8(i * 3 + 1)
	}
	msg := build_client_hello(rnd, default_suites, 'example.com')
	assert msg[0] == hstype_client_hello
	ln := get_u24(msg[1..4])
	assert ln == msg.len - 4
	info := parse_client_hello(msg[4..])!
	assert info.client_random == rnd
	assert info.suites == default_suites
	assert info.server_name == 'example.com'
}

fn test_suite_selection() {
	rsa_der := os.read_bytes(testdata_path('rsa_key_pkcs8.der'))!
	rsa_key := parse_server_key_pkcs8(rsa_der)!
	// Server with RSA key picks the first RSA suite offered.
	picked := pick_suite(default_suites, rsa_key)!
	assert picked == suite_ecdhe_rsa_aes128_gcm_sha256
	ec_der := os.read_bytes(testdata_path('ec_key_pkcs8.der'))!
	ec_key := parse_server_key_pkcs8(ec_der)!
	picked_ec := pick_suite(default_suites, ec_key)!
	assert picked_ec == suite_ecdhe_ecdsa_aes128_gcm_sha256
	// CBC-only client against RSA server negotiates CBC.
	picked_cbc := pick_suite([u16(suite_ecdhe_rsa_aes128_cbc_sha256)], rsa_key)!
	assert picked_cbc == suite_ecdhe_rsa_aes128_cbc_sha256
	// RSA-only client against EC key fails.
	mut suite_mismatch_failed := false
	pick_suite([u16(suite_ecdhe_rsa_aes128_gcm_sha256)], ec_key) or { suite_mismatch_failed = true }
	assert suite_mismatch_failed, 'suite mismatch should fail'
}

// ---------- loopback tests ----------

fn loopback_client_cfg() Config {
	return Config{
		server_name: 'localhost'
		skip_verify: true
	}
}

fn run_rsa_server(port int, cert []u8, key []u8) {
	mut ln := net.listen_tcp(.ip, '127.0.0.1:${port}', net.ListenOptions{}) or {
		panic('listen: ${err}')
	}
	defer {
		ln.close() or {}
	}
	mut tcp := ln.accept() or { panic('accept: ${err}') }
	mut s := accept(tcp, Config{
		cert_der: cert
		key_der:  key
	}) or { panic('wrap: ${err}') }
	s.handshake() or { panic('server hs: ${err}') }
	mut buf := []u8{len: 256}
	n := s.read(mut buf) or { panic('server read: ${err}') }
	s.write(buf[..n]) or { panic('server write: ${err}') }
	s.close() or {}
}

fn run_ec_server(port int, cert []u8, key []u8, want_suite u16) {
	mut ln := net.listen_tcp(.ip, '127.0.0.1:${port}', net.ListenOptions{}) or {
		panic('listen: ${err}')
	}
	defer {
		ln.close() or {}
	}
	mut tcp := ln.accept() or { panic('accept: ${err}') }
	mut s := accept(tcp, Config{
		cert_der: cert
		key_der:  key
	}) or { panic('wrap: ${err}') }
	s.handshake() or { panic('server hs: ${err}') }
	assert s.cipher_suite() == want_suite
	mut buf := []u8{len: 256}
	n := s.read(mut buf) or { panic('server read: ${err}') }
	s.write(buf[..n]) or { panic('server write: ${err}') }
	s.close() or {}
}

fn echo_check(port int, client_cfg Config, msg string, want_suite u16) {
	time.sleep(700 * time.millisecond)
	mut c := dial('127.0.0.1', port, client_cfg) or { panic('dial: ${err}') }
	c.handshake() or { panic('client hs: ${err}') }
	assert c.cipher_suite() == want_suite
	c.write(msg.bytes()) or { panic('client write: ${err}') }
	mut buf := []u8{len: 256}
	mut got := []u8{}
	for got.len < msg.len {
		n := c.read(mut buf) or { panic('client read: ${err}') }
		got << buf[..n]
	}
	assert got.bytestr() == msg
	c.close() or {}
}

fn test_loopback_rsa_gcm() {
	port := 18461
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	spawn run_rsa_server(port, cert, key)
	echo_check(port, loopback_client_cfg(), 'ping-tls12-gcm', suite_ecdhe_rsa_aes128_gcm_sha256)
}

fn test_loopback_rsa_cbc() {
	port := 18462
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	cfg := Config{
		server_name:   'localhost'
		skip_verify:   true
		cipher_suites: [u16(suite_ecdhe_rsa_aes128_cbc_sha256)]
	}
	spawn run_rsa_server(port, cert, key)
	echo_check(port, cfg, 'ping-tls12-cbc', suite_ecdhe_rsa_aes128_cbc_sha256)
}

fn test_loopback_ecdsa_gcm() {
	port := 18463
	cert := os.read_bytes(testdata_path('ec_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('ec_key_pkcs8.der')) or { panic(err) }
	spawn run_ec_server(port, cert, key, suite_ecdhe_ecdsa_aes128_gcm_sha256)
	echo_check(port, loopback_client_cfg(), 'ping-tls12-ecdsa', suite_ecdhe_ecdsa_aes128_gcm_sha256)
}

// ---------- openssl cross-checks ----------

fn run_openssl_s_server(bin string, port int, cert string, key string) {
	os.exec([bin, 's_server', '-tls1_2', '-accept', port.str(), '-cert', cert, '-key', key, '-cipher',
		'ECDHE-RSA-AES128-GCM-SHA256', '-sigalgs', 'RSA+SHA256', '-naccept', '1'])
}

fn test_openssl_s_server_client() {
	bin := openssl_bin()
	if bin == '' {
		println('skip openssl s_server test: no openssl binary')
		return
	}
	port := 18464
	cert := testdata_path('rsa_cert.pem')
	key := testdata_path('rsa_key.pem')
	spawn run_openssl_s_server(bin, port, cert, key)
	time.sleep(1500 * time.millisecond)
	mut c := dial('127.0.0.1', port, Config{
		server_name: 'localhost'
		skip_verify: true
	}) or { panic('dial openssl s_server: ${err}') }
	c.handshake() or { panic('hs vs openssl s_server: ${err}') }
	assert c.cipher_suite() == suite_ecdhe_rsa_aes128_gcm_sha256
	// Our sealed app data must be readable by openssl (exercises our
	// GCM seal path against a real peer); then a clean close.
	c.write('hello openssl'.bytes()) or { panic('write vs s_server: ${err}') }
	c.close() or {}
}

fn test_openssl_s_client_server() {
	bin := openssl_bin()
	if bin == '' {
		println('skip openssl s_client test: no openssl binary')
		return
	}
	port := 18465
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	spawn run_rsa_server(port, cert, key)
	time.sleep(1000 * time.millisecond)
	mut out := ''
	for _ in 0 .. 6 {
		res := run_s_client(bin, port)
		out = res.output
		if res.exit_code == 0 {
			break
		}
		time.sleep(500 * time.millisecond)
	}
	assert out.contains('TLSv1.2'), 'expected TLSv1.2 in s_client output, got:\n${out}'
	assert out.contains('ping-tls12-ssl'), 'expected echo in s_client output, got:\n${out}'
}

// run_s_client connects openssl s_client, sends a probe line on stdin and
// returns the result. Stdin feeding needs a shell pipe, so platform shells
// are used (powershell on Windows, sh elsewhere).
fn run_s_client(bin string, port int) os.Result {
	$if windows {
		return os.exec(['powershell', '-NoProfile', '-Command', "Write-Output 'ping-tls12-ssl' | & '" +
			bin + "' s_client -tls1_2 -connect 127.0.0.1:${port} -cipher ECDHE-RSA-AES128-GCM-SHA256"])
	} $else {
		return os.exec(['sh', '-c',
			"echo ping-tls12-ssl | '" + bin + "' s_client -tls1_2 -connect 127.0.0.1:${port} -cipher ECDHE-RSA-AES128-GCM-SHA256"])
	}
}
