module tls

import crypto.ecdsa
import crypto.ed25519
import crypto.pem
import crypto.rand
import crypto.x509
import encoding.hex
import net
import os
import time
import x.crypto.mldsa
import x.crypto.mlkem
import crypto.slh_dsa

// tls13x_test_ecdsa_key generates a throwaway P-256 keypair for tests.
fn tls13x_test_ecdsa_key() (ecdsa.PublicKey, ecdsa.PrivateKey) {
	return ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
}

fn tls13x_testdata(name string) string {
	return os.join_path(@VEXEROOT, 'vlib/net/tls/testdata/' + name)
}

fn tls13x_load_ca(name string) x509.Certificate {
	txt := os.read_file(tls13x_testdata(name)) or { panic(err) }
	block, _ := pem.decode(txt) or { panic('pem decode failed') }
	return x509.parse_der(block.data) or { panic(err) }
}

// tls13x_sigalgs_contain reports whether the ClientHello message offers a
// signature scheme.
fn tls13x_sigalgs_contain(ch_msg []u8, scheme u16) bool {
	parsed, _ := parse_handshake_message(ch_msg) or { return false }
	ch := parse_client_hello(parsed.body) or { return false }
	for e in ch.extensions {
		if e.typ == ext_signature_algorithms {
			if e.data.len < 2 {
				return false
			}
			slen := (int(e.data[0]) << 8) | int(e.data[1])
			if slen + 2 != e.data.len || slen % 2 != 0 {
				return false
			}
			mut so := 2
			for so + 1 < e.data.len {
				if (u16(e.data[so]) << 8) | u16(e.data[so + 1]) == scheme {
					return true
				}
				so += 2
			}
		}
	}
	return false
}

fn tls13x_ext_present(ch_msg []u8, typ u16) bool {
	parsed, _ := parse_handshake_message(ch_msg) or { return false }
	ch := parse_client_hello(parsed.body) or { return false }
	for e in ch.extensions {
		if e.typ == typ {
			return true
		}
	}
	return false
}

fn tls13x_serve_echo(mut l Listener, ch chan string, rounds int) {
	mut conn := l.accept() or {
		ch <- 'accept err: ${err}'
		return
	}
	defer {
		conn.close() or {}
	}
	for _ in 0 .. rounds {
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
	}
	ch <- 'ok'
}

fn tls13x_echo_round(mut c Conn, msg string) {
	c.write(msg.bytes()) or { panic(err) }
	mut buf := []u8{len: msg.len}
	mut got := 0
	for got < msg.len {
		mut tmp := []u8{len: msg.len - got}
		n := c.read(mut tmp) or { panic(err) }
		for i in 0 .. n {
			buf[got + i] = tmp[i]
		}
		got += n
	}
	assert buf == msg.bytes()
}

// HRR over TCP: a hybrid_require server meets a classical-only client.
// The server must retry for the hybrid group; the client answers and the
// handshake completes with the hybrid share.
fn test_tls13x_hrr_loopback() {
	mut l := listen(Config{ verify: false, pq_mode: .hybrid_require }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
		pq_mode:     .off
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	chs := c.client_hs or { panic('no client handshake') }
	// is_hrr is transient: the second (real) ServerHello clears it
	// (client.v). The retained hrr_group/first_ch_msg/selected_group
	// below are the durable proof the retry happened.
	assert chs.hrr_group == named_group_x25519_mlkem768
	assert chs.first_ch_msg.len > 0
	assert chs.selected_group == named_group_x25519_mlkem768
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// HRR at handshake-struct level: client offers only pure ML-KEM-512 (valid
// but unsupported by the server preference list), server retries for
// X25519; the second flight negotiates X25519.
fn test_tls13x_hrr_groups() {
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs.offer_groups = [named_group_mlkem512]
	ch1 := chs.build_client_hello() or { panic(err) }
	mut srv := new_server_handshake(.off) or { panic(err) }
	hrr := srv.process_client_hello(ch1) or { panic(err) }
	assert is_hrr_flight(hrr)
	// Client ingests the retry, rebuilds with the requested share.
	chs.process_server_hello(hrr) or { panic(err) }
	assert chs.is_hrr
	assert chs.hrr_group == named_group_x25519
	ch2 := chs.build_second_client_hello() or { panic(err) }
	flight := srv.process_client_hello(ch2) or { panic(err) }
	mut types := []HandshakeType{}
	mut off := 0
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		types << parsed.typ
		off += n
	}
	assert types == [HandshakeType.server_hello, .encrypted_extensions, .certificate,
		.certificate_verify, .finished]
	assert srv.selected_group == named_group_x25519
	// A second flight with the wrong group is rejected.
	mut bad := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	bad.offer_groups = [named_group_secp384r1]
	bad_ch := bad.build_client_hello() or { panic(err) }
	srv.process_client_hello(bad_ch) or {
		assert err.msg().contains('HRR')
		return
	}
	assert false, 'wrong HRR group accepted'
}

// KeyUpdate over TCP: both sides rotate and the stream stays intact.
fn test_tls13x_key_update_loopback() {
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 2}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or {
			ch <- 'read1 err: ${err}'
			return
		}
		conn.write(buf[..n]) or {
			ch <- 'write1 err: ${err}'
			return
		}
		conn.rekey() or {
			ch <- 'rekey err: ${err}'
			return
		}
		mut buf2 := []u8{len: 4}
		n2 := conn.read(mut buf2) or {
			ch <- 'read2 err: ${err}'
			return
		}
		conn.write(buf2[..n2]) or {
			ch <- 'write2 err: ${err}'
			return
		}
		ch <- 'ok'
	}(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	tls13x_echo_round(mut c, 'ping')
	c.rekey() or { panic(err) }
	tls13x_echo_round(mut c, 'pong')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// ALPN: server selects its first overlap; both sides expose it.
fn test_tls13x_alpn_loopback() {
	mut l := listen(Config{ verify: false, alpn_protocols: ['h2', 'http/1.1'] }) or {
		panic(err)
	}
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		ch <- conn.negotiated_alpn()
	}(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		alpn_protocols: ['http/1.1', 'h2']
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	assert c.negotiated_alpn() == 'h2'
	assert <-ch == 'h2'
	c.close() or {}
	l.close() or {}
	_ = server
}

// ALPN default: nothing offered, nothing negotiated.
fn test_tls13x_alpn_default_empty() {
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	assert c.negotiated_alpn() == ''
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// SNI: b.example selects the mapped credential (trusted), unknown names
// fail closed to the default credential.
fn test_tls13x_sni_selection() {
	ca_b := tls13x_load_ca('tls13x_sni_b_ca.pem')
	mut l := listen(Config{
		verify:       false
		cert:         tls13x_testdata('tls13x_sni_a_cert.pem')
		key:          tls13x_testdata('tls13x_sni_a_leaf_key.pem')
		cert_map:     {
			'b.example': CertEntry{
				cert: tls13x_testdata('tls13x_sni_b_cert.pem')
				key:  tls13x_testdata('tls13x_sni_b_leaf_key.pem')
			}
		}
		verify_chain: true
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	// Positive: b.example gets credential B (CA_B verifies it).
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{
		verify:       true
		server_name:  'b.example'
		verify_chain: true
		ca_store:     CertStore{
			roots: [ca_b]
		}
	}) or { panic(err) }
	c.handshake() or { panic('SNI-selected credential rejected: ${err}') }
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	// Fail closed: an unknown name gets the default (A), which CA_B does
	// not trust, so a verifying client aborts.
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l, ch2, 1)
	mut c2 := dial('127.0.0.1', port, Config{
		verify:       true
		server_name:  'unknown.example'
		verify_chain: true
		ca_store:     CertStore{
			roots: [ca_b]
		}
	}) or { panic(err) }
	c2.handshake() or {
		c2.close() or {}
		l.close() or {}
		_ = server
		_ = server2
		return
	}
	assert false, 'unknown SNI name did not fail closed'
}

// External PSK: configured identity+key resume without tickets.
fn test_tls13x_external_psk_loopback() {
	clear_used_tickets()
	id := rand.bytes(16) or { panic(err) }
	key := rand.bytes(32) or { panic(err) }
	mut l := listen(Config{ verify: false, psk_identity: id, psk_key: key }) or {
		panic(err)
	}
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{
		verify:       false
		server_name:  'localhost'
		psk_identity: id
		psk_key:      key
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	assert c.resumed()
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	// Wrong key falls back to a full handshake (not resumed).
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l, ch2, 1)
	mut c2 := dial('127.0.0.1', port, Config{
		verify:       false
		server_name:  'localhost'
		psk_identity: id
		psk_key:      rand.bytes(32) or { panic(err) }
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert !c2.resumed()
	tls13x_echo_round(mut c2, 'ping')
	assert <-ch2 == 'ok'
	c2.close() or {}
	l.close() or {}
	_ = server
	_ = server2
}

// Post-handshake auth: server requests, client answers, server verifies,
// traffic keys unchanged (echo still works).
fn test_tls13x_post_handshake_auth() {
	server_ca := tls13x_load_ca('p384_ca_cert.pem')
	mut l := listen(Config{
		verify:       false
		cert:         tls13x_testdata('rsa_leaf_cert.pem')
		key:          tls13x_testdata('rsa_leaf_pkcs8_key.pem')
		verify_chain: true
		ca_store:     CertStore{
			roots: [server_ca]
		}
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		conn.request_client_auth_post() or {
			ch <- 'pha req err: ${err}'
			return
		}
		// Write first so the client's read (which auto-answers the CR)
		// returns: wire order S->C is CR, go; C->S is Cert, CV, ping, so
		// the server's single read consumes the auth flight then ping.
		conn.write('gooo'.bytes()) or {
			ch <- 'write err: ${err}'
			return
		}
		mut buf := []u8{len: 4}
		mut got := 0
		for got < 4 {
			mut tmp := []u8{len: 4 - got}
			n := conn.read(mut tmp) or {
				ch <- 'read err: ${err}'
				return
			}
			for i in 0 .. n {
				buf[got + i] = tmp[i]
			}
			got += n
		}
		if buf != 'ping'.bytes() || conn.has_pha_pending {
			ch <- 'bad data or PHA unverified'
			return
		}
		conn.write(buf) or {
			ch <- 'write err: ${err}'
			return
		}
		ch <- 'ok'
	}(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
		client_cert: tls13x_testdata('p384_leaf_cert.pem')
		client_key:  tls13x_testdata('p384_leaf_key.pem')
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	// Read the server's go-ahead first: the read auto-answers the
	// CertificateRequest encountered along the way, then returns gooo.
	mut go_buf := []u8{len: 4}
	mut go_got := 0
	for go_got < 4 {
		mut tmp := []u8{len: 4 - go_got}
		n := c.read(mut tmp) or { panic(err) }
		for i in 0 .. n {
			go_buf[go_got + i] = tmp[i]
		}
		go_got += n
	}
	assert go_buf == 'gooo'.bytes()
	c.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	mut got := 0
	for got < 4 {
		mut tmp := []u8{len: 4 - got}
		n := c.read(mut tmp) or { panic(err) }
		for i in 0 .. n {
			buf[got + i] = tmp[i]
		}
		got += n
	}
	assert buf == 'ping'.bytes()
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// Exporter: both sides derive identical bytes; labels isolate.
fn test_tls13x_exporter_loopback() {
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		a := conn.exporter('tls13x-test', 'ctx'.bytes(), 32) or {
			ch <- 'exporter err: ${err}'
			return
		}
		b := conn.exporter('tls13x-test', 'ctx'.bytes(), 32) or {
			ch <- 'exporter err: ${err}'
			return
		}
		if a != b || a.len != 32 {
			ch <- 'exporter unstable'
			return
		}
		ch <- a.hex()
	}(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	mine := c.exporter('tls13x-test', 'ctx'.bytes(), 32) or { panic(err) }
	assert mine.hex() == <-ch
	other := c.exporter('tls13x-other', 'ctx'.bytes(), 32) or { panic(err) }
	assert other != mine
	c.close() or {}
	l.close() or {}
	_ = server
}

// SSLKEYLOGFILE: CLIENT_RANDOM lines with client random + master secret.
fn test_tls13x_keylog_shape() {
	path := os.join_path(os.temp_dir(), 'tls13x_keylog.txt')
	os.rm(path) or {}
	os.setenv('SSLKEYLOGFILE', path, true)
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
	os.unsetenv('SSLKEYLOGFILE')
	lines := os.read_file(path) or { panic('no keylog file: ${err}') }
	os.rm(path) or {}
	mut found := false
	for line in lines.split_into_lines() {
		if line.starts_with('CLIENT_RANDOM ') {
			parts := line.split(' ')
			assert parts.len == 3
			assert parts[1].len == 64
			assert parts[2].len == 64 || parts[2].len == 96
			found = true
		}
	}
	assert found, 'no CLIENT_RANDOM line logged'
}

// Ticket rotation + persistence: [k2,k1] opens k1-sealed tickets sealed by
// [0]=k2; save/load round-trips the raw 32B concat file.
fn test_tls13x_ticket_rotation() {
	clear_used_tickets()
	k1 := rand.bytes(32) or { panic(err) }
	k2 := rand.bytes(32) or { panic(err) }
	mut l1 := listen(Config{ verify: false, ticket_keys: [k1] }) or { panic(err) }
	p1 := l1.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l1, ch, 1)
	mut c1 := dial('127.0.0.1', p1, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c1.handshake() or { panic(err) }
	tls13x_echo_round(mut c1, 'ping')
	assert <-ch == 'ok'
	st := c1.session_state() or { panic('no ticket') }
	c1.close() or {}
	l1.close() or {}
	_ = server
	// New listener rotates to [k2,k1]: the k1 ticket still resumes, and
	// the fresh ticket is sealed with k2 (l1 with only k1 rejects it).
	mut l2 := listen(Config{ verify: false, ticket_keys: [k2, k1] }) or { panic(err) }
	p2 := l2.addr() or { panic(err) }.port() or { panic(err) }
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l2, ch2, 1)
	mut c2 := dial('127.0.0.1', p2, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: st
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert c2.resumed()
	tls13x_echo_round(mut c2, 'ping')
	assert <-ch2 == 'ok'
	st2 := c2.session_state() or { panic('no rotated ticket') }
	c2.close() or {}
	l2.close() or {}
	_ = server2
	clear_used_tickets()
	mut l3 := listen(Config{ verify: false, ticket_keys: [k1] }) or { panic(err) }
	p3 := l3.addr() or { panic(err) }.port() or { panic(err) }
	ch3 := chan string{cap: 1}
	server3 := spawn tls13x_serve_echo(mut l3, ch3, 1)
	mut c3 := dial('127.0.0.1', p3, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: st2
	}) or { panic(err) }
	c3.handshake() or { panic(err) }
	assert !c3.resumed()
	tls13x_echo_round(mut c3, 'ping')
	assert <-ch3 == 'ok'
	c3.close() or {}
	l3.close() or {}
	_ = server3
	// Persistence round trip.
	tpath := os.join_path(os.temp_dir(), 'tls13x_tkeys.bin')
	os.rm(tpath) or {}
	save_ticket_keys(tpath, [k1, k2]) or { panic(err) }
	loaded := load_ticket_keys(tpath) or { panic(err) }
	assert loaded == [k1, k2]
	os.rm(tpath) or {}
	load_ticket_keys(tpath) or {
		return
	}
	assert false, 'missing key file accepted'
}

// Groups: every new named group completes offer/select at struct level.
fn test_tls13x_group_offer_select() {
	for g in [u16(named_group_secp256r1), named_group_secp384r1, named_group_ffdhe2048,
		named_group_ffdhe3072, named_group_mlkem768, named_group_mlkem1024] {
		mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
			panic(err)
		}
		chs.offer_groups = [g]
		ch_msg := chs.build_client_hello() or { panic(err) }
		mut srv := new_server_handshake(.off) or { panic(err) }
		flight := srv.process_client_hello(ch_msg) or { panic('group ${g}: ${err}') }
		assert !is_hrr_flight(flight)
		assert srv.selected_group == g
	}
	// Hybrid requires ML-KEM-capable server state.
	mut chs := new_client_handshake('localhost', []string{}, .hybrid_prefer, []CipherSuite{}) or {
		panic(err)
	}
	chs.offer_groups = [named_group_x25519_mlkem768]
	ch_msg := chs.build_client_hello() or { panic(err) }
	mut srv := new_server_handshake(.hybrid_prefer) or { panic(err) }
	flight := srv.process_client_hello(ch_msg) or { panic(err) }
	assert srv.selected_group == named_group_x25519_mlkem768
	_ = flight
	// FFDHE shared secrets agree both ways.
	priv_a, pub_a := ffdhe_generate(named_group_ffdhe2048) or { panic(err) }
	priv_b, pub_b := ffdhe_generate(named_group_ffdhe2048) or { panic(err) }
	ss_a := ffdhe_compute(priv_a, pub_b, named_group_ffdhe2048) or { panic(err) }
	ss_b := ffdhe_compute(priv_b, pub_a, named_group_ffdhe2048) or { panic(err) }
	assert ss_a == ss_b
	assert ss_a.len == 256
	priv_c, pub_c := ffdhe_generate(named_group_ffdhe3072) or { panic(err) }
	priv_d, pub_d := ffdhe_generate(named_group_ffdhe3072) or { panic(err) }
	assert ffdhe_compute(priv_c, pub_d, named_group_ffdhe3072)! == ffdhe_compute(priv_d,
		pub_c, named_group_ffdhe3072)!
	// Pure ML-KEM KEM round trip through the handshake helpers.
	ek, dk := mlkem.generate_keypair(.ml_kem_768) or { panic(err) }
	ct, ss := mlkem.encapsulate(ek) or { panic(err) }
	assert mlkem.decapsulate(ct, dk)! == ss
}

// Fragmentation: a ~50KB SLH-DSA flight spans several records and still
// completes (old code failed sealing >2^14 plaintext).
fn test_tls13x_fragmentation_loopback() {
	mut l := listen(Config{
		verify: false
		cert:   tls13x_testdata('slhshake192f_leaf_cert.pem')
		key:    tls13x_testdata('slhshake192f_leaf_key.pem')
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// OCSP transport: the stapled bytes cross the handshake untouched, but
// only when the client opts in (request_ocsp); otherwise no status_request
// goes on the wire and nothing is stapled.
fn test_tls13x_ocsp_loopback() {
	want := os.read_bytes(tls13x_testdata('tls13x_ocsp_resp.der')) or { panic(err) }
	mut l := listen(Config{ verify: false, ocsp_response_pem: want }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{
		verify:       false
		server_name:  'localhost'
		request_ocsp: true
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	assert c.ocsp_response() == want
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
	// Unconfigured servers staple nothing, and a client that does not opt
	// in never offers status_request (message-level proof below).
	mut l2 := listen(Config{ verify: false, ocsp_response_pem: want }) or { panic(err) }
	p2 := l2.addr() or { panic(err) }.port() or { panic(err) }
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l2, ch2, 1)
	mut c2 := dial('127.0.0.1', p2, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c2.handshake() or { panic(err) }
	assert c2.ocsp_response().len == 0
	tls13x_echo_round(mut c2, 'ping')
	assert <-ch2 == 'ok'
	c2.close() or {}
	l2.close() or {}
	_ = server2
	// Message level: the extension is present only with the flag set.
	mut chs_on := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs_on.request_ocsp = true
	assert tls13x_ext_present(chs_on.build_client_hello() or { panic(err) }, ext_status_request)
	mut chs_off := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	assert !tls13x_ext_present(chs_off.build_client_hello() or { panic(err) }, ext_status_request)
}

// Unknown post-handshake types are ignored (forward compatibility).
fn test_tls13x_ignore_unknown_post_handshake() {
	mut c := new_conn(Config{}) or { panic(err) }
	ignored := encode_handshake_message(.message_hash, []u8{len: 32}) or { panic(err) }
	c.consume_post_handshake(ignored) or { panic(err) }
	assert !c.has_ticket
}

// Ed25519 (scheme 0x0807): offer, select, sign and verify.
fn test_tls13x_ed25519() {
	mut chs0 := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	ch0 := chs0.build_client_hello() or { panic(err) }
	assert tls13x_sigalgs_contain(ch0, u16(0x0807))
	assert select_cv_scheme([u16(0x0807)], .ed25519)! == u16(0x0807)
	_, priv := ed25519.generate_key() or { panic(err) }
	content := 'tls13x ed25519 content'.bytes()
	sig := sign_cv_with_ed(.ed25519, u16(0x0807), none, RsaPrivateKey{}, mldsa.PrivateKey{},
		slh_dsa.PrivateKey{}, priv, content) or { panic(err) }
	assert ed25519.verify(priv.public_key(), content, sig) or { panic('ed25519 verify') }
	// Loopback with a real Ed25519 leaf (client skips chain verify).
	mut l := listen(Config{
		verify: false
		cert:   tls13x_testdata('tls13x_ed25519_cert.pem')
		key:    tls13x_testdata('tls13x_ed25519_key.pem')
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// HRR reverse direction (struct level): a classical-only client share
// against a hybrid_require server must retry for the hybrid group; the
// second flight negotiates it. Mirror of test_tls13x_hrr_groups.
fn test_tls13x_hrr_reverse_struct() {
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs.offer_groups = [named_group_x25519]
	ch1 := chs.build_client_hello() or { panic(err) }
	mut srv := new_server_handshake(.hybrid_require) or { panic(err) }
	hrr := srv.process_client_hello(ch1) or { panic(err) }
	assert is_hrr_flight(hrr)
	chs.process_server_hello(hrr) or { panic(err) }
	assert chs.is_hrr
	assert chs.hrr_group == named_group_x25519_mlkem768
	ch2 := chs.build_second_client_hello() or { panic(err) }
	flight := srv.process_client_hello(ch2) or { panic(err) }
	mut types := []HandshakeType{}
	mut off := 0
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		types << parsed.typ
		off += n
	}
	assert types == [HandshakeType.server_hello, .encrypted_extensions, .certificate,
		.certificate_verify, .finished]
	assert srv.selected_group == named_group_x25519_mlkem768
}

// KeyUpdate: the rekey flight carries update_not_requested (parsed back),
// rotation changes the sending keys so the old keys cannot open the new
// flight, and a second rekey on the same connection still works.
fn test_tls13x_keyupdate_request_flag_and_rotation() {
	ku_body := encode_key_update(false)
	assert ku_body == [u8(0)]
	assert parse_key_update(ku_body)! == false
	assert encode_key_update(true) == [u8(1)]
	assert parse_key_update(encode_key_update(true))! == true
	ku_flight := encode_handshake_message(.key_update, ku_body) or { panic(err) }
	parsed, n := parse_handshake_message(ku_flight) or { panic(err) }
	assert parsed.typ == .key_update
	assert n == ku_flight.len
	assert parse_key_update(parsed.body)! == false
	// Live rotation over TCP.
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 2)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	old_key := c.client_app.key.clone()
	old_iv := c.client_app.iv.clone()
	c.rekey() or { panic(err) }
	assert c.client_app.key != old_key
	wire := seal_record(c.suite, c.client_app.key, c.client_app.iv, c.write_seq,
		.application_data, 'post-rekey'.bytes()) or { panic(err) }
	rec, _ := decode_cipher_record(wire) or { panic(err) }
	if _, _ := open_record(c.suite, old_key, old_iv, c.write_seq, rec) {
		assert false, 'old keys opened post-rekey flight'
	} else {
		assert err.msg().len > 0
	}
	_, pt := open_record(c.suite, c.client_app.key, c.client_app.iv, c.write_seq,
		rec) or { panic(err) }
	assert pt == 'post-rekey'.bytes()
	tls13x_echo_round(mut c, 'ping')
	c.rekey() or { panic(err) }
	tls13x_echo_round(mut c, 'pong')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
}

// ALPN no overlap: disjoint offers complete with an empty negotiation on
// both sides (no abort, no protocol selected).
fn test_tls13x_alpn_no_overlap_loopback() {
	mut l := listen(Config{ verify: false, alpn_protocols: ['h2'] }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		alpn := conn.negotiated_alpn()
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or {
			ch <- 'read err: ${err}'
			return
		}
		conn.write(buf[..n]) or {
			ch <- 'write err: ${err}'
			return
		}
		ch <- 'alpn=${alpn}'
	}(mut l, ch)
	mut c := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		alpn_protocols: ['http/1.1']
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	assert c.negotiated_alpn() == ''
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'alpn='
	c.close() or {}
	l.close() or {}
	_ = server
}

// SNI selection (struct level): no SNI keeps the default credential, an
// unknown name fails closed to the default, and b.example selects the
// mapped credential.
fn test_tls13x_sni_selection_struct() {
	a_cert_pem := os.read_file(tls13x_testdata('tls13x_sni_a_cert.pem')) or { panic(err) }
	a_block, _ := pem.decode(a_cert_pem) or { panic('no A cert pem') }
	b_cert_pem := os.read_file(tls13x_testdata('tls13x_sni_b_cert.pem')) or { panic(err) }
	b_block, _ := pem.decode(b_cert_pem) or { panic('no B cert pem') }
	new_srv := fn () ServerHandshake {
		return new_server_handshake_with_config(.off, Config{
			verify:   false
			cert:     tls13x_testdata('tls13x_sni_a_cert.pem')
			key:      tls13x_testdata('tls13x_sni_a_leaf_key.pem')
			cert_map: {
				'b.example': CertEntry{
					cert: tls13x_testdata('tls13x_sni_b_cert.pem')
					key:  tls13x_testdata('tls13x_sni_b_leaf_key.pem')
				}
			}
		}) or { panic(err) }
	}
	no_sni_ch := fn () ClientHello {
		mut chs := new_client_handshake('', []string{}, .off, []CipherSuite{}) or {
			panic(err)
		}
		msg := chs.build_client_hello() or { panic(err) }
		parsed, _ := parse_handshake_message(msg) or { panic(err) }
		return parse_client_hello(parsed.body) or { panic(err) }
	}
	named_ch := fn (name string) ClientHello {
		mut chs := new_client_handshake(name, []string{}, .off, []CipherSuite{}) or {
			panic(err)
		}
		msg := chs.build_client_hello() or { panic(err) }
		parsed, _ := parse_handshake_message(msg) or { panic(err) }
		return parse_client_hello(parsed.body) or { panic(err) }
	}
	// No SNI keeps the default chain (A).
	mut s := new_srv()
	s.select_sni_credential(no_sni_ch()) or { panic(err) }
	assert s.cert_chain.len > 0
	assert s.cert_chain[0] == a_block.data
	// Unknown name fails closed to the default (A), never B.
	mut s2 := new_srv()
	s2.select_sni_credential(named_ch('unknown.example')) or { panic(err) }
	assert s2.cert_chain.len > 0
	assert s2.cert_chain[0] == a_block.data
	// b.example selects the mapped credential (B).
	mut s3 := new_srv()
	s3.select_sni_credential(named_ch('b.example')) or { panic(err) }
	assert s3.cert_chain.len > 0
	assert s3.cert_chain[0] == b_block.data
}

// SNI-only server (no default credential): an unknown name aborts the
// handshake; loopback proof that no fallback credential exists.
fn test_tls13x_sni_only_unknown_aborts() {
	// Struct level: selection itself errors.
	mut s := new_server_handshake_with_config(.off, Config{
		verify:   false
		cert_map: {
			'b.example': CertEntry{
				cert: tls13x_testdata('tls13x_sni_b_cert.pem')
				key:  tls13x_testdata('tls13x_sni_b_leaf_key.pem')
			}
		}
	}) or { panic(err) }
	mut chs := new_client_handshake('unknown.example', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	msg := chs.build_client_hello() or { panic(err) }
	parsed, _ := parse_handshake_message(msg) or { panic(err) }
	ch := parse_client_hello(parsed.body) or { panic(err) }
	s.select_sni_credential(ch) or {
		assert err.msg().contains('no certificate for server name')
		// Loopback: the handshake aborts end to end (Listener.accept
		// runs the server handshake, so its error is the TLS-layer
		// proof; the client handshake fails as well).
		mut l := listen(Config{
			verify:   false
			cert_map: {
				'b.example': CertEntry{
					cert: tls13x_testdata('tls13x_sni_b_cert.pem')
					key:  tls13x_testdata('tls13x_sni_b_leaf_key.pem')
				}
			}
		}) or { panic(err) }
		port := l.addr() or { panic(err) }.port() or { panic(err) }
		sch := chan string{cap: 1}
		server := spawn fn (mut l Listener, sch chan string) {
			l.accept() or {
				sch <- 'hs err: ${err}'
				return
			}
			sch <- 'unexpected ok'
		}(mut l, sch)
		mut c := dial('127.0.0.1', port, Config{
			verify:      false
			server_name: 'unknown.example'
		}) or { panic(err) }
		c.handshake() or {
			c.close() or {}
			got := <-sch
			assert got.contains('no certificate for server name')
			l.close() or {}
			_ = server
			return
		}
		c.close() or {}
		l.close() or {}
		_ = server
		assert false, 'SNI-only server accepted an unknown name'
	}
	assert false, 'SNI-only selection accepted an unknown name'
}

// External PSK negatives (loopback): a wrong or unknown identity, an
// empty (unoffered) identity, and a PSK-less server all fall back to a
// full handshake instead of resuming.
fn test_tls13x_ext_psk_negatives_loopback() {
	clear_used_tickets()
	srv_id := rand.bytes(16) or { panic(err) }
	srv_key := rand.bytes(32) or { panic(err) }
	other_id := rand.bytes(16) or { panic(err) }
	// Wrong identity: client offers an identity the server does not know.
	mut l := listen(Config{ verify: false, psk_identity: srv_id, psk_key: srv_key }) or {
		panic(err)
	}
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{
		verify:       false
		server_name:  'localhost'
		psk_identity: other_id
		psk_key:      srv_key
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	assert !c.resumed()
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	// Empty identity: the client offers no PSK at all.
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l, ch2, 1)
	mut c2 := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c2.handshake() or { panic(err) }
	assert !c2.resumed()
	tls13x_echo_round(mut c2, 'ping')
	assert <-ch2 == 'ok'
	c2.close() or {}
	l.close() or {}
	_ = server
	_ = server2
	// PSK-less server ignores a PSK offer: full handshake, not resumed.
	mut l3 := listen(Config{ verify: false }) or { panic(err) }
	p3 := l3.addr() or { panic(err) }.port() or { panic(err) }
	ch3 := chan string{cap: 1}
	server3 := spawn tls13x_serve_echo(mut l3, ch3, 1)
	mut c3 := dial('127.0.0.1', p3, Config{
		verify:       false
		server_name:  'localhost'
		psk_identity: srv_id
		psk_key:      srv_key
	}) or { panic(err) }
	c3.handshake() or { panic(err) }
	assert !c3.resumed()
	tls13x_echo_round(mut c3, 'ping')
	assert <-ch3 == 'ok'
	c3.close() or {}
	l3.close() or {}
	_ = server3
}

// Post-handshake auth negatives: a client without a credential answers
// with an empty Certificate and the server fails the read; a client
// certificate from the wrong CA is rejected.
fn test_tls13x_pha_absent_client_cert_fails() {
	server_ca := tls13x_load_ca('p384_ca_cert.pem')
	mut l := listen(Config{
		verify:       false
		cert:         tls13x_testdata('rsa_leaf_cert.pem')
		key:          tls13x_testdata('rsa_leaf_pkcs8_key.pem')
		verify_chain: true
		ca_store:     CertStore{
			roots: [server_ca]
		}
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		conn.request_client_auth_post() or {
			ch <- 'pha req err: ${err}'
			return
		}
		conn.write('gooo'.bytes()) or {
			ch <- 'write err: ${err}'
			return
		}
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or {
			ch <- 'read err: ${err}'
			return
		}
		_ = n
		ch <- 'unexpected ok'
	}(mut l, ch)
	// No client credential configured: the auto-answer is empty and the
	// server must fail, never report success.
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	mut go_buf := []u8{len: 4}
	mut go_got := 0
	for go_got < 4 {
		mut tmp := []u8{len: 4 - go_got}
		n := c.read(mut tmp) or { panic(err) }
		for i in 0 .. n {
			go_buf[go_got + i] = tmp[i]
		}
		go_got += n
	}
	assert go_buf == 'gooo'.bytes()
	c.close() or {}
	res := <-ch
	assert res != 'unexpected ok', 'server accepted PHA without client cert'
	assert res.contains('read err'), 'server must fail the PHA read, got: ${res}'
	l.close() or {}
	_ = server
}

fn test_tls13x_pha_wrong_ca_fails() {
	server_ca := tls13x_load_ca('p384_ca_cert.pem')
	mut l := listen(Config{
		verify:       false
		cert:         tls13x_testdata('rsa_leaf_cert.pem')
		key:          tls13x_testdata('rsa_leaf_pkcs8_key.pem')
		verify_chain: true
		ca_store:     CertStore{
			roots: [server_ca]
		}
	}) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn fn (mut l Listener, ch chan string) {
		mut conn := l.accept() or {
			ch <- 'accept err: ${err}'
			return
		}
		defer {
			conn.close() or {}
		}
		conn.request_client_auth_post() or {
			ch <- 'pha req err: ${err}'
			return
		}
		conn.write('gooo'.bytes()) or {
			ch <- 'write err: ${err}'
			return
		}
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or {
			ch <- 'read err: ${err}'
			return
		}
		_ = n
		ch <- 'unexpected ok'
	}(mut l, ch)
	// The client presents a leaf from another CA: identity check fails.
	mut c := dial('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
		client_cert: tls13x_testdata('rsa_leaf_cert.pem')
		client_key:  tls13x_testdata('rsa_leaf_pkcs8_key.pem')
	}) or { panic(err) }
	c.handshake() or { panic(err) }
	mut go_buf := []u8{len: 4}
	mut go_got := 0
	for go_got < 4 {
		mut tmp := []u8{len: 4 - go_got}
		n := c.read(mut tmp) or { break }
		for i in 0 .. n {
			go_buf[go_got + i] = tmp[i]
		}
		go_got += n
	}
	c.close() or {}
	res := <-ch
	assert res != 'unexpected ok', 'server accepted a wrong-CA PHA client cert'
	assert res.contains('read err'), 'server must fail the wrong-CA PHA read, got: ${res}'
	l.close() or {}
	_ = server
}

// Exporter isolation, lengths, pre-handshake failure and session
// separation: same label with different contexts differs, output length
// is honored (zero/oversize rejected), use before the handshake fails,
// and two sessions derive different bytes.
fn test_tls13x_exporter_isolation_lengths() {
	// Pre-handshake use must fail.
	mut c0 := new_conn(Config{}) or { panic(err) }
	if _ := c0.exporter('tls13x-test', 'ctx'.bytes(), 32) {
		assert false, 'pre-handshake exporter must fail'
	} else {
		assert err.msg().contains('handshake not complete')
	}
	// Live session: lengths, context isolation.
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	a16 := c.exporter('tls13x-test', 'ctx'.bytes(), 16) or { panic(err) }
	assert a16.len == 16
	a64 := c.exporter('tls13x-test', 'ctx'.bytes(), 64) or { panic(err) }
	assert a64.len == 64
	assert a64[..16] != a16, 'different lengths must derive different prefixes'
	other_ctx := c.exporter('tls13x-test', 'other'.bytes(), 32) or { panic(err) }
	mine := c.exporter('tls13x-test', 'ctx'.bytes(), 32) or { panic(err) }
	assert other_ctx != mine, 'different contexts must isolate'
	if _ := c.exporter('tls13x-test', 'ctx'.bytes(), 0) {
		assert false, 'zero-length exporter must fail'
	} else {
		assert err.msg().len > 0
	}
	if _ := c.exporter('tls13x-test', 'ctx'.bytes(), 0x10000) {
		assert false, 'oversize exporter must fail'
	} else {
		assert err.msg().len > 0
	}
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
	// A second session with the same label/context derives different bytes.
	mut l2 := listen(Config{ verify: false }) or { panic(err) }
	p2 := l2.addr() or { panic(err) }.port() or { panic(err) }
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l2, ch2, 1)
	mut c2 := dial('127.0.0.1', p2, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c2.handshake() or { panic(err) }
	mine2 := c2.exporter('tls13x-test', 'ctx'.bytes(), 32) or { panic(err) }
	assert mine2 != mine, 'two sessions must derive different exporter bytes'
	tls13x_echo_round(mut c2, 'ping')
	assert <-ch2 == 'ok'
	c2.close() or {}
	l2.close() or {}
	_ = server2
}

// SSLKEYLOGFILE shape: both endpoints log the same single CLIENT_RANDOM
// line, the random matches the handshake client_random, and the secret
// hex decodes to the master secret length.
fn test_tls13x_keylog_exact_shape() {
	path := os.join_path(os.temp_dir(), 'tls13x_keylog_exact.txt')
	os.rm(path) or {}
	os.setenv('SSLKEYLOGFILE', path, true)
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	client_random := c.client_random.clone()
	master_len := c.master_secret.len
	c.close() or {}
	l.close() or {}
	_ = server
	os.unsetenv('SSLKEYLOGFILE')
	lines := os.read_file(path) or { panic('no keylog file: ${err}') }
	os.rm(path) or {}
	mut non_empty := []string{}
	for line in lines.split_into_lines() {
		if line.trim_space().len > 0 {
			non_empty << line
		}
	}
	// Client and server each log the same line for the connection.
	assert non_empty.len == 2, 'expected client+server keylog lines, got ${non_empty.len}'
	assert non_empty[0] == non_empty[1]
	parts := non_empty[0].split(' ')
	assert parts.len == 3
	assert parts[0] == 'CLIENT_RANDOM'
	assert parts[1].len == 64
	assert parts[1] == client_random.hex(), 'keylog random must match the handshake client_random'
	secret := hex.decode(parts[2]) or { panic('bad keylog secret hex: ${err}') }
	assert secret.len == master_len, 'keylog secret must decode to the master secret length'
	assert master_len == 32 || master_len == 48
}

// Ticket rotation with persistence: keys loaded from disk actually
// resume, while tampered or expired tickets fall back to a full handshake.
fn test_tls13x_ticket_rotation_loaded_resume_and_fallbacks() {
	clear_used_tickets()
	k1 := rand.bytes(32) or { panic(err) }
	k2 := rand.bytes(32) or { panic(err) }
	mut l1 := listen(Config{ verify: false, ticket_keys: [k1] }) or { panic(err) }
	p1 := l1.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l1, ch, 1)
	mut c1 := dial('127.0.0.1', p1, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c1.handshake() or { panic(err) }
	tls13x_echo_round(mut c1, 'ping')
	assert <-ch == 'ok'
	st := c1.session_state() or { panic('no ticket') }
	c1.close() or {}
	l1.close() or {}
	_ = server
	// Persist [k2,k1] and resume through the LOADED keys (rotation
	// where the old key still opens).
	tpath := os.join_path(os.temp_dir(), 'tls13x_tkeys_loaded.bin')
	os.rm(tpath) or {}
	save_ticket_keys(tpath, [k2, k1]) or { panic(err) }
	loaded := load_ticket_keys(tpath) or { panic(err) }
	assert loaded == [k2, k1]
	os.rm(tpath) or {}
	mut l2 := listen(Config{ verify: false, ticket_keys: loaded }) or { panic(err) }
	p2 := l2.addr() or { panic(err) }.port() or { panic(err) }
	ch2 := chan string{cap: 1}
	server2 := spawn tls13x_serve_echo(mut l2, ch2, 1)
	mut c2 := dial('127.0.0.1', p2, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: st
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert c2.resumed(), 'loaded rotation keys must resume'
	tls13x_echo_round(mut c2, 'ping')
	assert <-ch2 == 'ok'
	st2 := c2.session_state() or { panic('no rotated ticket') }
	c2.close() or {}
	l2.close() or {}
	_ = server2
	// Tampered ticket bytes cannot open: fallback, not resumed.
	mut bad_ticket := st2.ticket.clone()
	bad_ticket[0] ^= u8(0x01)
	bad_st := SessionState{
		ticket:         bad_ticket
		psk:            st2.psk.clone()
		suite:          st2.suite
		created_unix:   st2.created_unix
		lifetime:       st2.lifetime
		age_add:        st2.age_add
		max_early_data: st2.max_early_data
	}
	mut l3 := listen(Config{ verify: false, ticket_keys: [k2, k1] }) or { panic(err) }
	p3 := l3.addr() or { panic(err) }.port() or { panic(err) }
	ch3 := chan string{cap: 1}
	server3 := spawn tls13x_serve_echo(mut l3, ch3, 1)
	mut c3 := dial('127.0.0.1', p3, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: bad_st
	}) or { panic(err) }
	c3.handshake() or { panic(err) }
	assert !c3.resumed(), 'tampered ticket must fall back to a full handshake'
	tls13x_echo_round(mut c3, 'ping')
	assert <-ch3 == 'ok'
	c3.close() or {}
	l3.close() or {}
	_ = server3
	// Expired ticket (ancient creation age) cannot resume either.
	clear_used_tickets()
	mut old_st := st2
	old_st.created_unix = st2.created_unix - 100000
	mut l4 := listen(Config{ verify: false, ticket_keys: [k2, k1] }) or { panic(err) }
	p4 := l4.addr() or { panic(err) }.port() or { panic(err) }
	ch4 := chan string{cap: 1}
	server4 := spawn tls13x_serve_echo(mut l4, ch4, 1)
	mut c4 := dial('127.0.0.1', p4, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: old_st
	}) or { panic(err) }
	c4.handshake() or { panic(err) }
	assert !c4.resumed(), 'expired ticket must fall back to a full handshake'
	tls13x_echo_round(mut c4, 'ping')
	assert <-ch4 == 'ok'
	c4.close() or {}
	l4.close() or {}
	_ = server4
}

// Fragmentation over a raw record stream: a ~50KB flight spans several
// records (each within the 2^14 cap) and reassembles byte-identical.
fn test_tls13x_fragmentation_multi_record_count() {
	c_secret := rand.bytes(32) or { panic(err) }
	s_secret := rand.bytes(32) or { panic(err) }
	c_keys := derive_traffic_keys(c_secret, .tls_aes_128_gcm_sha256) or { panic(err) }
	s_keys := derive_traffic_keys(s_secret, .tls_aes_128_gcm_sha256) or { panic(err) }
	mut ln := net.listen_tcp(.ip, '127.0.0.1:0') or { panic(err) }
	port := ln.addr() or { panic(err) }.port() or { panic(err) }
	done := chan string{cap: 1}
	spawn fn (mut ln net.TcpListener, s_keys TrafficKeys, done chan string) {
		mut srv_tcp := ln.accept() or {
			done <- 'accept err: ${err}'
			return
		}
		defer {
			srv_tcp.close() or {}
		}
		mut srv := &Conn{
			is_server:      true
			handshake_done: true
			suite:          .tls_aes_128_gcm_sha256
			server_app:     s_keys
			client_app:     s_keys
		}
		srv.tcp = *srv_tcp
		big := []u8{len: 50000, init: 0xab}
		// Two identical flights: the first is consumed through the real
		// read path (proves reassembly + decryption), the second is
		// counted raw off the wire (proves multi-record framing).
		srv.write_cipher_fragmented(.application_data, big) or {
			done <- 'write err: ${err}'
			return
		}
		srv.write_cipher_fragmented(.application_data, big) or {
			done <- 'write err: ${err}'
			return
		}
		done <- 'ok'
	}(mut ln, s_keys, done)
	mut cli_tcp := net.dial_tcp('127.0.0.1:${port}') or { panic(err) }
	defer {
		cli_tcp.close() or {}
	}
	mut c := &Conn{
		handshake_done: true
		suite:          .tls_aes_128_gcm_sha256
		server_app:     s_keys
		client_app:     c_keys
	}
	c.tcp = *cli_tcp
	// Read-path proof: the first flight must reassemble through
	// Conn.read and decrypt byte-identical.
	mut got_big := []u8{}
	for got_big.len < 50000 {
		mut chunk := []u8{len: 50000 - got_big.len}
		n := c.read(mut chunk) or { panic(err) }
		got_big << chunk[..n]
	}
	assert got_big == []u8{len: 50000, init: 0xab}
	// Count records on the wire: header framing only, no decryption yet.
	mut header := []u8{len: 5}
	mut rn := cli_tcp.read(mut header) or { panic(err) }
	for rn < 5 {
		d := cli_tcp.read(mut header[rn..]) or { panic(err) }
		rn += d
	}
	frag_len := (int(header[3]) << 8) | int(header[4])
	assert frag_len <= max_ciphertext_len
	// Drain the rest of the first record plus all following records
	// through the real read path; every record must stay within cap and
	// the flight must need more than one record.
	mut total_frag := frag_len
	mut nrec := 1
	mut rest := []u8{len: frag_len}
	mut got := 0
	for got < frag_len {
		n := cli_tcp.read(mut rest[got..]) or { panic(err) }
		got += n
	}
	for total_frag < 50000 + 1024 {
		mut hdr := []u8{len: 5}
		mut hn := cli_tcp.read(mut hdr) or { break }
		for hn < 5 {
			d := cli_tcp.read(mut hdr[hn..]) or { break }
			hn += d
			if hn <= 0 {
				break
			}
		}
		if hn < 5 {
			break
		}
		fl := (int(hdr[3]) << 8) | int(hdr[4])
		assert fl <= max_ciphertext_len
		mut body := []u8{len: fl}
		mut bg := 0
		for bg < fl {
			n := cli_tcp.read(mut body[bg..]) or { panic(err) }
			bg += n
		}
		total_frag += fl
		nrec++
		if nrec > 16 {
			break
		}
	}
	assert nrec > 1, 'a ~50KB flight must span more than one record'
	ln.close() or {}
	assert <-done == 'ok'
}

// Oversize single records are rejected by the read path (unit level,
// crafted bytes, no allocation proportional to the length prefix).
fn test_tls13x_oversize_record_rejected() {
	big_plain := [u8(0x16), 0x03, 0x03, 0x40, 0x01]
	if _, _ := decode_plain_record(big_plain) {
		assert false, 'oversize plaintext record must be rejected'
	} else {
		assert err.msg().contains('too large')
	}
	// 0x4100 equals max_ciphertext_len exactly, so the boundary probe
	// needs 0x4101 to exceed it.
	big_cipher := [u8(0x17), 0x03, 0x03, 0x41, 0x01]
	if _, _ := decode_cipher_record(big_cipher) {
		assert false, 'oversize ciphertext record must be rejected'
	} else {
		assert err.msg().contains('too large')
	}
	// A 0xFFFFFF handshake length prefix errors (truncated) instead of
	// hanging or allocating.
	if _, _ := parse_handshake_message([u8(1), 0xff, 0xff, 0xff, 0xaa]) {
		assert false, '0xFFFFFF handshake prefix must fail'
	} else {
		assert err.msg().len > 0
	}
}

// OCSP through verify_chain (V-generated CA/leaf): a tampered staple
// fails the chain, and the same chain without a staple succeeds.
fn test_tls13x_ocsp_chain_tamper_and_skip() {
	_, ca_priv := tls13x_test_ecdsa_key()
	now := time.now()
	ca, _ := x509.generate_self_signed(x509.CertGenOptions{
		common_name: 'TLS13x OCSP CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ca_priv) or { panic('ca gen: ${err}') }
	_, leaf_priv := tls13x_test_ecdsa_key()
	leaf, leaf_der := x509.generate_ca_signed(x509.CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	chain := x509.build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	signer := x509.CertSigner{
		kind:       .ecdsa
		ecdsa_priv: ca_priv
	}
	_, good := x509.make_ocsp_response(&leaf, &ca, x509.OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	// Skipped staple: the chain verifies without OCSP input.
	x509.verify_chain(chain, x509.VerifyOptions{}) or { panic('plain verify: ${err}') }
	// Tampered staple fails through the chain input.
	mut bad := good.clone()
	bad[bad.len - 1] ^= u8(0x01)
	x509.verify_chain(chain, x509.VerifyOptions{
		ocsp_resp: bad
	}) or {
		assert err.msg().contains('signature') || err.msg().contains('bad')
			|| err.msg().contains('mismatch')
		return
	}
	assert false, 'tampered OCSP staple must fail verify_chain'
	_ = leaf_der
}

// Ed25519 CertificateVerify negatives: a tampered signature fails, a
// valid signature under the wrong scheme fails, and an Ed25519 signature
// presented with an ECDSA key fails.
fn test_tls13x_ed25519_cv_negatives() {
	_, priv := ed25519.generate_key() or { panic(err) }
	ed_pub := priv.public_key()
	content := 'tls13x ed25519 cv negatives'.bytes()
	sig := sign_cv_with_ed(.ed25519, u16(0x0807), none, RsaPrivateKey{}, mldsa.PrivateKey{},
		slh_dsa.PrivateKey{}, priv, content) or { panic(err) }
	ed_pk := x509.PublicKey(x509.Ed25519PublicKey{ raw: ed_pub.clone() })
	mut body := []u8{}
	body << u8(0x08)
	body << u8(0x07)
	body << u8(sig.len >> 8)
	body << u8(sig.len)
	body << sig
	// Tampered signature fails.
	mut bad_sig := sig.clone()
	bad_sig[0] ^= u8(0x01)
	mut bad_body := []u8{}
	bad_body << u8(0x08)
	bad_body << u8(0x07)
	bad_body << u8(bad_sig.len >> 8)
	bad_body << u8(bad_sig.len)
	bad_body << bad_sig
	if _, _ := verify_certificate_verify(bad_body, ed_pk, content) {
		assert false, 'tampered Ed25519 CV must fail'
	} else {
		assert err.msg().len > 0
	}
	// Right signature, wrong scheme (ECDSA P-256) fails.
	mut wrong_scheme := []u8{}
	wrong_scheme << u8(0x04)
	wrong_scheme << u8(0x03)
	wrong_scheme << u8(sig.len >> 8)
	wrong_scheme << u8(sig.len)
	wrong_scheme << sig
	if _, _ := verify_certificate_verify(wrong_scheme, ed_pk, content) {
		assert false, 'Ed25519 CV under an ECDSA scheme must fail'
	} else {
		assert err.msg().contains('mismatch')
	}
	// Right scheme, wrong key type (ECDSA P-256 key) fails.
	_, epriv := tls13x_test_ecdsa_key()
	epub := epriv.public_key() or { panic(err) }
	epub_bytes := epub.bytes() or { panic(err) }
	ec_pk := x509.PublicKey(x509.EcdsaP256PublicKey{ point: epub_bytes })
	if _, _ := verify_certificate_verify(body, ec_pk, content) {
		assert false, 'Ed25519 CV under an ECDSA key must fail'
	} else {
		assert err.msg().contains('mismatch')
	}
	// Positive control: the untouched body verifies.
	verify_certificate_verify(body, ed_pk, content) or { panic(err) }
}
