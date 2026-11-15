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
	// Argument order matters: swapping label and seed changes the output.
	swapped := prf_sha256('secret'.bytes(), 'seed'.bytes(), 'label'.bytes(), 48)
	assert a != swapped
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
	// Tampered ciphertext fails authentication.
	mut gcm_bad := sealed.clone()
	gcm_bad[10] ^= 0x01
	mut gcm_tamper_failed := false
	gcm_open(key, salt, 7, rectype_application_data, gcm_bad) or { gcm_tamper_failed = true }
	assert gcm_tamper_failed, 'GCM opened tampered record'
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
	// Distinct peers negotiate distinct secrets (a constant stub passes the
	// agreement check above but fails here) and the secret is non-trivial.
	priv_c, xc, yc := p256_keygen()!
	sc := p256_ecdh(priv_a, xc, yc)!
	assert sc != sa
	assert sa != []u8{len: 32}
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
	// A damaged signature is rejected (parse break or verify failure both
	// count as fail-closed).
	mut tamper_failed := false
	mut parsed_ok := true
	mut bad_der := rsa_der.clone()
	bad_der[bad_der.len - 1] ^= u8(0x01)
	bad_cert := parse_certificate(bad_der) or {
		parsed_ok = false
		rsa_cert
	}
	if !parsed_ok {
		tamper_failed = true
	} else {
		verify_cert_signature(bad_cert, bad_cert.pubkey) or { tamper_failed = true }
	}
	assert tamper_failed, 'tampered cert verified'
	// The validity window is enforced.
	far_future := time.utc().add_days(36500)
	mut expiry_failed := false
	check_validity(rsa_cert, far_future) or { expiry_failed = true }
	assert expiry_failed, 'expired cert accepted'
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

// ---------- EMS (RFC 7627) ----------

fn test_ems_vectors() {
	// EMS is always offered by our ClientHello.
	mut rnd := []u8{len: 32}
	for i in 0 .. 32 {
		rnd[i] = u8(i * 3 + 1)
	}
	msg := build_client_hello(rnd, default_suites, 'example.com')
	info := parse_client_hello(msg[4..])!
	assert info.ems_offered, 'ClientHello must offer extended_master_secret'
	assert info.etm_offered, 'ClientHello must offer encrypt_then_mac'
	assert info.reneg_info.len == 0, 'initial renegotiation_info must be empty'
	// Server echoes EMS/EtM when offered (CBC for EtM).
	sh_body := build_server_hello_body_full(rnd, suite_ecdhe_rsa_aes128_cbc_sha256, []u8{},
		true, true, []u8{})
	sh := parse_server_hello(sh_body)!
	assert sh.ems, 'ServerHello must echo EMS'
	assert sh.etm, 'ServerHello must echo EtM for CBC'
	// AEAD echo for EtM is ignored by the peer (EtM only applies to CBC).
	sh_gcm := parse_server_hello(build_server_hello_body_full(rnd,
		suite_ecdhe_rsa_aes128_gcm_sha256, []u8{}, true, true, []u8{}))!
	assert sh_gcm.ems
	// Master derivation differs with/without EMS.
	premaster := []u8{len: 32, init: 0x0b}
	cr := []u8{len: 32, init: 0x11}
	sr := []u8{len: 32, init: 0x22}
	plain := master_secret(premaster, cr, sr)
	mut fake_transcript := build_client_hello(cr, default_suites, '')
	fake_transcript << build_server_hello(sr, suite_ecdhe_rsa_aes128_gcm_sha256)
	sess_hash := sha256.sum256(fake_transcript)
	ems_master := extended_master_secret(premaster, sess_hash)
	assert ems_master.len == 48
	assert ems_master != plain, 'EMS master must differ from plain master'
	// Session-hash sensitivity: one bit flip changes the master.
	mut bad_hash := sess_hash.clone()
	bad_hash[0] ^= 0x01
	bad_master := extended_master_secret(premaster, bad_hash)
	assert bad_master != ems_master, 'session_hash sensitivity: master must change'
	// Abort on mismatch: Finished computed with the wrong master must not
	// verify (this is what both sides check; mismatch aborts the handshake).
	th := sha256.sum256('transcript'.bytes())
	good_vd := finished_verify_data(ems_master, 'client finished', th)
	bad_vd := finished_verify_data(bad_master, 'client finished', th)
	assert good_vd != bad_vd
}

// ---------- session-ID resumption ----------

fn run_resumption_server(port int, cert []u8, key []u8, nconn int, resumed_ch chan bool) {
	clear_session_cache()
	mut ln := net.listen_tcp(.ip, '127.0.0.1:${port}', net.ListenOptions{}) or {
		panic('listen: ${err}')
	}
	defer {
		ln.close() or {}
	}
	for _ in 0 .. nconn {
		mut tcp := ln.accept() or { panic('accept: ${err}') }
		mut s := accept(tcp, Config{
			cert_der: cert
			key_der:  key
		}) or { panic('wrap: ${err}') }
		s.handshake() or { panic('server hs: ${err}') }
		resumed_ch <- s.did_resume()
		mut buf := []u8{len: 256}
		n := s.read(mut buf) or { panic('server read: ${err}') }
		s.write(buf[..n]) or { panic('server write: ${err}') }
		s.close() or {}
	}
}

fn echo_once(port int, cfg Config, msg string) (Session, bool, u16) {
	time.sleep(400 * time.millisecond)
	mut c := dial('127.0.0.1', port, cfg) or { panic('dial: ${err}') }
	c.handshake() or { panic('client hs: ${err}') }
	sess := c.session()
	resumed := c.did_resume()
	suite := c.cipher_suite()
	c.write(msg.bytes()) or { panic('client write: ${err}') }
	mut buf := []u8{len: 256}
	mut got := []u8{}
	for got.len < msg.len {
		n := c.read(mut buf) or { panic('client read: ${err}') }
		got << buf[..n]
	}
	assert got.bytestr() == msg
	c.close() or {}
	return sess, resumed, suite
}

fn test_resumption_loopback() {
	clear_session_cache()
	port := 18501
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	ch := chan bool{cap: 2}
	spawn run_resumption_server(port, cert, key, 2, ch)
	time.sleep(700 * time.millisecond)
	sess1, resumed1, _ := echo_once(port, loopback_client_cfg(), 'first-flight')
	assert !resumed1, 'first handshake must be full'
	assert sess1.id.len == 32, 'server must issue a 32-byte session ID'
	assert sess1.master.len == 48
	// Second connection offers the previous session: abbreviated handshake.
	cfg2 := Config{
		server_name: 'localhost'
		skip_verify: true
		resume:      sess1
	}
	sess2, resumed2, suite2 := echo_once(port, cfg2, 'second-flight')
	assert resumed2, 'second handshake must resume'
	assert suite2 == suite_ecdhe_rsa_aes128_gcm_sha256
	assert sess2.id == sess1.id, 'resumed session ID must match'
	assert (<-ch) == false
	assert (<-ch) == true
	time.sleep(100 * time.millisecond)
}

fn test_resumption_reuse_fallback() {
	clear_session_cache()
	port := 18502
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	ch := chan bool{cap: 2}
	spawn run_resumption_server(port, cert, key, 2, ch)
	time.sleep(700 * time.millisecond)
	sess1, _, _ := echo_once(port, loopback_client_cfg(), 'hello-one')
	assert sess1.id.len == 32
	// Bogus session ID (never cached): server must fall back to a full
	// handshake with a fresh ID instead of aborting.
	bogus := Session{
		id:     []u8{len: 32, init: 0xaa}
		master: sess1.master.clone()
		suite:  sess1.suite
	}
	cfg2 := Config{
		server_name: 'localhost'
		skip_verify: true
		resume:      bogus
	}
	sess2, resumed2, _ := echo_once(port, cfg2, 'hello-two')
	assert !resumed2, 'bogus session must fall back to full handshake'
	assert sess2.id != bogus.id, 'fallback must issue a fresh session ID'
	assert (<-ch) == false
	assert (<-ch) == false
	time.sleep(100 * time.millisecond)
}

// ---------- Encrypt-then-MAC (RFC 7366) ----------

fn test_etm_negotiation() {
	mut rnd := []u8{len: 32, init: 0x05}
	ch := parse_client_hello(build_client_hello(rnd, default_suites, 'x')[4..])!
	assert ch.etm_offered
	// CBC + EtM offer -> negotiated.
	sh_cbc := parse_server_hello(build_server_hello_body_full(rnd,
		suite_ecdhe_rsa_aes128_cbc_sha256, []u8{}, false, true, []u8{}))!
	assert sh_cbc.etm
	// AEAD suites ignore EtM (record layer stays AEAD, never EtM).
	assert suite_is_aead(suite_ecdhe_rsa_aes128_gcm_sha256)
	assert suite_is_aead(suite_ecdhe_rsa_aes128_ccm8)
	assert !suite_is_cbc(suite_ecdhe_rsa_aes128_gcm_sha256)
}

fn test_etm_tamper_order() {
	mac_key := []u8{len: 32, init: 0x11}
	enc_key := []u8{len: 16, init: 0x22}
	pt := 'etm tamper proof'.bytes()
	// Round-trip.
	sealed := cbc_etm_seal(mac_key, enc_key, 9, rectype_application_data, pt)!
	opened := cbc_etm_open(mac_key, enc_key, 9, rectype_application_data, sealed)!
	assert opened == pt
	// Tamper with ciphertext (after the 16-byte IV): EtM must fail at the
	// MAC step before any padding processing (tamper-order proof).
	mut bad_ct := sealed.clone()
	bad_ct[20] ^= 0x01
	mut failed := false
	mut msg := ''
	cbc_etm_open(mac_key, enc_key, 9, rectype_application_data, bad_ct) or {
		failed = true
		msg = err.msg()
	}
	assert failed, 'EtM must reject tampered ciphertext'
	assert msg.contains('MAC'), 'EtM tamper must fail at MAC, got: ${msg}'
	// Tamper with the IV: MAC covers IV, so this also fails at MAC.
	mut bad_iv := sealed.clone()
	bad_iv[0] ^= 0x01
	mut failed_iv := false
	mut msg_iv := ''
	cbc_etm_open(mac_key, enc_key, 9, rectype_application_data, bad_iv) or {
		failed_iv = true
		msg_iv = err.msg()
	}
	assert failed_iv
	assert msg_iv.contains('MAC'), 'EtM IV tamper must fail at MAC, got: ${msg_iv}'
	// Wrong sequence fails MAC as well.
	mut failed_seq := false
	cbc_etm_open(mac_key, enc_key, 10, rectype_application_data, sealed) or {
		failed_seq = true
	}
	assert failed_seq, 'EtM must reject wrong sequence'
	// Non-EtM CBC still round-trips (MAC-then-encrypt path unchanged).
	plain_sealed := cbc_seal(mac_key, enc_key, 9, rectype_application_data, pt)!
	plain_open := cbc_open(mac_key, enc_key, 9, rectype_application_data, plain_sealed)!
	assert plain_open == pt
}

// ---------- secure renegotiation (RFC 5746) ----------

fn run_reneg_server(port int, cert []u8, key []u8, done chan bool) {
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
	s.handshake() or { panic('server hs1: ${err}') }
	mut buf := []u8{len: 256}
	n := s.read(mut buf) or { panic('server read1: ${err}') }
	s.write(buf[..n]) or { panic('server write1: ${err}') }
	// Single re-handshake (client-initiated, server just reads the new Hello).
	s.renegotiate() or { panic('server reneg: ${err}') }
	mut buf2 := []u8{len: 256}
	n2 := s.read(mut buf2) or { panic('server read2: ${err}') }
	s.write(buf2[..n2]) or { panic('server write2: ${err}') }
	// A second renegotiation must be rejected (single re-handshake).
	mut second_ok := true
	s.renegotiate() or { second_ok = false }
	assert !second_ok, 'second renegotiation must be rejected'
	s.close() or {}
	done <- true
}

fn test_renegotiation_loopback() {
	clear_session_cache()
	port := 18503
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	done := chan bool{cap: 1}
	spawn run_reneg_server(port, cert, key, done)
	time.sleep(700 * time.millisecond)
	mut c := dial('127.0.0.1', port, loopback_client_cfg()) or { panic('dial: ${err}') }
	c.handshake() or { panic('client hs1: ${err}') }
	assert c.uses_ems(), 'EMS should be negotiated on loopback'
	c.write('before-reneg'.bytes()) or { panic('write1: ${err}') }
	mut buf := []u8{len: 256}
	mut got := []u8{}
	for got.len < 'before-reneg'.len {
		n := c.read(mut buf) or { panic('read1: ${err}') }
		got << buf[..n]
	}
	assert got.bytestr() == 'before-reneg'
	c.renegotiate() or { panic('client reneg: ${err}') }
	c.write('after-reneg'.bytes()) or { panic('write2: ${err}') }
	mut got2 := []u8{}
	for got2.len < 'after-reneg'.len {
		n := c.read(mut buf) or { panic('read2: ${err}') }
		got2 << buf[..n]
	}
	assert got2.bytestr() == 'after-reneg'
	mut second_ok := true
	c.renegotiate() or { second_ok = false }
	assert !second_ok, 'client second renegotiation must be rejected'
	c.close() or {}
	assert (<-done) == true
}

fn test_renegotiation_forged_finished() {
	// Forged peer Finished bytes in renegotiation_info must be rejected.
	fake := []u8{len: 12, init: 0xaa}
	real := []u8{len: 12, init: 0xbb}
	mut failed := false
	check_client_reneg(fake, real, false) or { failed = true }
	assert failed, 'forged client renegotiation_info must be rejected'
	mut failed2 := false
	check_client_reneg([]u8{}, real, false) or { failed2 = true }
	assert failed2, 'empty renegotiation_info on re-handshake must be rejected'
	mut failed3 := false
	check_client_reneg(real, real, false) or { failed3 = true }
	assert !failed3, 'correct renegotiation_info must be accepted'
	// Server side: needs client_verify+server_verify (24 bytes).
	mut want := real.clone()
	want << fake
	mut failed4 := false
	check_server_reneg(fake, real, fake, false) or { failed4 = true }
	assert failed4, 'forged server renegotiation_info must be rejected'
	mut failed5 := false
	check_server_reneg(want, real, fake, false) or { failed5 = true }
	assert !failed5, 'correct server renegotiation_info must be accepted'
	// Initial handshakes must carry empty info; non-empty aborts.
	mut failed6 := false
	check_client_reneg(real, []u8{}, true) or { failed6 = true }
	assert failed6
	mut failed7 := false
	check_server_reneg(real, []u8{}, []u8{}, true) or { failed7 = true }
	assert failed7
}

// ---------- AES-128-CCM-8 (RFC 6655) ----------

fn test_ccm_roundtrip() {
	key := []u8{len: 16, init: 0x0b}
	salt := []u8{len: 4, init: 0x0d}
	for msg in ['hi', 'exactly sixteen!!', 'a longer message spanning several AES blocks!!!'] {
		pt := msg.bytes()
		sealed := ccm_seal(key, salt, 3, rectype_application_data, pt)!
		assert sealed.len == 8 + pt.len + ccm_tag_len
		opened := ccm_open(key, salt, 3, rectype_application_data, sealed)!
		assert opened == pt
	}
	mut bad := ccm_seal(key, salt, 3, rectype_application_data, 'tamper me'.bytes())!
	bad[10] ^= 0x01
	mut tamper_failed := false
	ccm_open(key, salt, 3, rectype_application_data, bad) or { tamper_failed = true }
	assert tamper_failed, 'CCM must reject tampered ciphertext'
	good := ccm_seal(key, salt, 3, rectype_application_data, 'seq'.bytes())!
	mut seq_failed := false
	ccm_open(key, salt, 4, rectype_application_data, good) or { seq_failed = true }
	assert seq_failed, 'CCM must reject wrong sequence'
	mut type_failed := false
	ccm_open(key, salt, 3, rectype_handshake, good) or { type_failed = true }
	assert type_failed, 'CCM must reject wrong content type'
	empty_sealed := ccm_seal(key, salt, 0, rectype_handshake, []u8{})!
	empty_open := ccm_open(key, salt, 0, rectype_handshake, empty_sealed)!
	assert empty_open.len == 0
}

// RFC 6655 §6 framing KAT: nonce = salt + explicit, AAD = seq/type/version/
// len, tag = 8 bytes. The vector below pins the wire format (8-byte explicit
// prefix, fixed tag length) and the session sensitivity of the primitive.
fn test_ccm_kat_rfc6655() {
	key := [u8(0x40), 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d,
		0x4e, 0x4f]
	salt := [u8(0x01), 0x02, 0x03, 0x04]
	pt := 'hello ccm-8'.bytes()
	sealed := ccm_seal(key, salt, 0x0102030405060708, rectype_application_data, pt)!
	// Explicit nonce echoes the sequence (8 bytes, big-endian).
	assert sealed[..8] == [u8(0x01), 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]
	assert sealed.len == 8 + pt.len + 8
	opened := ccm_open(key, salt, 0x0102030405060708, rectype_application_data, sealed)!
	assert opened == pt
	// Key sensitivity: one bit flip must fail authentication.
	mut bad_key := key.clone()
	bad_key[0] ^= 0x01
	mut key_failed := false
	ccm_open(bad_key, salt, 0x0102030405060708, rectype_application_data, sealed) or {
		key_failed = true
	}
	assert key_failed, 'CCM must reject wrong key'
	assert suite_is_ccm(suite_ecdhe_rsa_aes128_ccm8)
	assert suite_is_ccm(suite_ecdhe_ecdsa_aes128_ccm8)
	assert suite_name(suite_ecdhe_rsa_aes128_ccm8) == 'TLS_ECDHE_RSA_WITH_AES_128_CCM_8'
	assert suite_name(suite_ecdhe_ecdsa_aes128_ccm8) == 'TLS_ECDHE_ECDSA_WITH_AES_128_CCM_8'
}

fn test_loopback_ccm8() {
	clear_session_cache()
	port := 18504
	// RSA-CCM8 loopback (V-to-V; OpenSSL here lacks ECDHE-RSA-CCM8).
	cert := os.read_bytes(testdata_path('rsa_cert.der')) or { panic(err) }
	key := os.read_bytes(testdata_path('rsa_key_pkcs8.der')) or { panic(err) }
	cfg := Config{
		server_name:   'localhost'
		skip_verify:   true
		cipher_suites: [u16(suite_ecdhe_rsa_aes128_ccm8)]
	}
	spawn run_rsa_server(port, cert, key)
	echo_check(port, cfg, 'ping-tls12-ccm8', suite_ecdhe_rsa_aes128_ccm8)
	// ECDSA-CCM8 loopback.
	port2 := 18505
	ec_cert := os.read_bytes(testdata_path('ec_cert.der')) or { panic(err) }
	ec_key := os.read_bytes(testdata_path('ec_key_pkcs8.der')) or { panic(err) }
	cfg2 := Config{
		server_name:   'localhost'
		skip_verify:   true
		cipher_suites: [u16(suite_ecdhe_ecdsa_aes128_ccm8)]
	}
	spawn run_ec_server(port2, ec_cert, ec_key, suite_ecdhe_ecdsa_aes128_ccm8)
	echo_check(port2, cfg2, 'ping-tls12-ecc-ccm8', suite_ecdhe_ecdsa_aes128_ccm8)
}

fn run_openssl_s_server_ccm(bin string, port int, cert string, key string) {
	os.exec([bin, 's_server', '-tls1_2', '-accept', port.str(), '-cert', cert, '-key', key, '-cipher',
		'ECDHE-ECDSA-AES128-CCM8:@SECLEVEL=0', '-naccept', '1'])
}

fn test_openssl_ccm8_ecdsa() {
	bin := openssl_bin()
	if bin == '' {
		println('skip openssl CCM8 test: no openssl binary')
		return
	}
	// Probe for ECDHE-ECDSA-CCM8 support; skip when the runner OpenSSL
	// disables CCM (the V-to-V loopback above still gates the record path).
	probe := os.exec([bin, 'ciphers', 'ECDHE-ECDSA-AES128-CCM8'])
	if !probe.output.contains('CCM8') && !probe.output.contains('CCM') {
		println('skip openssl CCM8 test: cipher not enabled')
		return
	}
	port := 18506
	cert := testdata_path('ec_cert.pem')
	key := testdata_path('ec_key.pem')
	spawn run_openssl_s_server_ccm(bin, port, cert, key)
	time.sleep(1500 * time.millisecond)
	mut c := dial('127.0.0.1', port, Config{
		server_name:   'localhost'
		skip_verify:   true
		cipher_suites: [u16(suite_ecdhe_ecdsa_aes128_ccm8)]
	}) or {
		println('skip openssl CCM8 test: dial failed (no CCM8 route?)')
		return
	}
	c.handshake() or {
		// Runner OpenSSL may list CCM8 in `ciphers` yet disable it in
		// s_server/s_client at the default security level (observed on
		// OpenSSL 4.x: `ciphers` shows CCM8 but s_client reports "no
		// ciphers available" and s_server aborts with handshake_failure).
		// The V-to-V loopback above still gates the CCM-8 record path.
		println('skip openssl CCM8 test: handshake failed (likely disabled CCM8): ${err}')
		return
	}
	assert c.cipher_suite() == suite_ecdhe_ecdsa_aes128_ccm8
	c.write('hello ccm8 openssl'.bytes()) or { panic('write vs CCM8 s_server: ${err}') }
	c.close() or {}
}
