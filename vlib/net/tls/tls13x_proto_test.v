module tls

import crypto.ed25519
import crypto.pem
import crypto.rand
import crypto.x509
import os
import x.crypto.mldsa
import x.crypto.mlkem
import crypto.slh_dsa

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

// OCSP transport: the stapled bytes cross the handshake untouched.
fn test_tls13x_ocsp_loopback() {
	want := os.read_bytes(tls13x_testdata('tls13x_ocsp_resp.der')) or { panic(err) }
	mut l := listen(Config{ verify: false, ocsp_response_pem: want }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan string{cap: 1}
	server := spawn tls13x_serve_echo(mut l, ch, 1)
	mut c := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c.handshake() or { panic(err) }
	assert c.ocsp_response() == want
	tls13x_echo_round(mut c, 'ping')
	assert <-ch == 'ok'
	c.close() or {}
	l.close() or {}
	_ = server
	// Unconfigured servers staple nothing.
	mut l2 := listen(Config{ verify: false }) or { panic(err) }
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
