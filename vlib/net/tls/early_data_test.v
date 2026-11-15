module tls

import crypto.rand
import time

fn serve_early_n(mut l Listener, n int, resumed_ch chan bool, got_ch chan string) {
	for _ in 0 .. n {
		mut conn := l.accept() or {
			eprintln('TRACE serve accept err: ${err}')
			resumed_ch <- false
			got_ch <- 'accept err: ${err}'
			continue
		}
		resumed_ch <- conn.resumed()
		mut buf := []u8{len: 32}
		m := conn.read(mut buf) or {
			got_ch <- 'read err: ${err}'
			conn.close() or {}
			continue
		}
		got_ch <- buf[..m].bytestr()
		conn.write(buf[..m]) or {}
		conn.close() or {}
	}
}

// serve_early_double serves the accept-loopback shape: a plain full
// handshake echo first, then a resumed connection whose first read returns
// the 0-RTT bytes and whose second read returns later application data.
// Reading both flights keeps the close graceful (no unread bytes -> no RST).
fn serve_early_double(mut l Listener, resumed_ch chan bool, got_ch chan string) {
	mut c1 := l.accept() or {
		eprintln('TRACE serve accept err: ${err}')
		resumed_ch <- false
		got_ch <- 'accept err: ${err}'
		return
	}
	resumed_ch <- c1.resumed()
	mut b0 := []u8{len: 32}
	m0 := c1.read(mut b0) or {
		got_ch <- 'read err: ${err}'
		c1.close() or {}
		return
	}
	got_ch <- b0[..m0].bytestr()
	c1.write(b0[..m0]) or {}
	c1.close() or {}
	mut c2 := l.accept() or {
		eprintln('TRACE serve accept err: ${err}')
		resumed_ch <- false
		got_ch <- 'accept err: ${err}'
		return
	}
	resumed_ch <- c2.resumed()
	mut b1 := []u8{len: 32}
	m1 := c2.read(mut b1) or {
		got_ch <- 'read err: ${err}'
		c2.close() or {}
		return
	}
	got_ch <- b1[..m1].bytestr()
	c2.write(b1[..m1]) or {}
	mut b2 := []u8{len: 32}
	m2 := c2.read(mut b2) or {
		got_ch <- 'read err: ${err}'
		c2.close() or {}
		return
	}
	got_ch <- b2[..m2].bytestr()
	c2.write(b2[..m2]) or {}
	c2.close() or {}
}

fn early_test_ticket(mut server ServerHandshake) SessionState {
	suite := CipherSuite.tls_aes_128_gcm_sha256
	master := rand.bytes(32) or { panic(err) }
	tnonce := rand.bytes(8) or { panic(err) }
	age_add := u32(0x0a0b0c0d)
	blob := seal_ticket(master, suite, age_add, tnonce, server.ticket_key) or { panic(err) }
	psk := derive_psk(master, tnonce, suite) or { panic(err) }
	return SessionState{
		ticket:       blob.clone()
		psk:          psk
		suite:        suite
		created_unix: time.now().unix()
		lifetime:     ticket_lifetime
		age_add:      age_add
	}
}

fn test_early_data_codec_roundtrip() {
	nonce := rand.bytes(8) or { panic(err) }
	ticket := rand.bytes(64) or { panic(err) }
	// A zero allowance encodes exactly like the legacy constructor.
	legacy := encode_new_session_ticket(ticket_lifetime, u32(1), nonce, ticket) or { panic(err) }
	sized0 := encode_new_session_ticket_ex(ticket_lifetime, u32(1), nonce, ticket, 0) or {
		panic(err)
	}
	assert sized0 == legacy
	assert parse_new_session_ticket(legacy) or { panic(err) }.max_early_data == 0
	// A non-zero allowance round-trips through the ticket extensions.
	sized := encode_new_session_ticket_ex(ticket_lifetime, u32(1), nonce, ticket, 16384) or {
		panic(err)
	}
	nst := parse_new_session_ticket(sized) or { panic(err) }
	assert nst.max_early_data == 16384
	assert nst.nonce == nonce
	assert nst.ticket == ticket
	// Corrupt extension framing is rejected (flip the extension length byte).
	mut bad := sized.clone()
	bad[bad.len - 6] ^= u8(0xff)
	parse_new_session_ticket(bad) or { return }
	panic('corrupt ticket extensions accepted')
}

fn test_early_data_ee_codec() {
	assert !ee_accepts_early_data([u8(0), 0])
	assert !ee_accepts_early_data([u8(0)])
	assert !ee_accepts_early_data([]u8{})
	// Empty early_data (accept signal) and sized early_data both count.
	assert ee_accepts_early_data([u8(0), 4, 0, 42, 0, 0])
	size_data := encode_early_data_size(1024)
	mut ee := [u8(0), 8, 0, 42, 0, 4]
	ee << size_data
	assert ee_accepts_early_data(ee)
	// A different extension does not count.
	assert !ee_accepts_early_data([u8(0), 4, 0, 43, 0, 0])
	// Truncated extension framing does not count.
	assert !ee_accepts_early_data([u8(0), 8, 0, 42, 0, 4, 0])
	// ClientHello offer detection.
	ch_without := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32}
		session_id:          []u8{}
		cipher_suites:       [.tls_aes_128_gcm_sha256]
		compression_methods: [u8(0)]
		extensions:          []Extension{}
	}
	assert !ch_offers_early_data(ch_without)
	ch_with := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32}
		session_id:          []u8{}
		cipher_suites:       [.tls_aes_128_gcm_sha256]
		compression_methods: [u8(0)]
		extensions:          [
			Extension{ typ: ext_early_data, data: encode_early_data_offer() },
		]
	}
	assert ch_offers_early_data(ch_with)
}

fn test_early_secret_derivation() {
	suite := CipherSuite.tls_aes_128_gcm_sha256
	psk := rand.bytes(32) or { panic(err) }
	es := derive_early_secret_with_psk(psk, suite) or { panic(err) }
	h1 := transcript_hash_for_suite('hello'.bytes(), suite)
	h2 := transcript_hash_for_suite('other'.bytes(), suite)
	s1 := derive_client_early_traffic_secret(es, h1, suite) or { panic(err) }
	s1b := derive_client_early_traffic_secret(es, h1, suite) or { panic(err) }
	assert s1 == s1b
	s2 := derive_client_early_traffic_secret(es, h2, suite) or { panic(err) }
	assert s1 != s2
	// Traffic keys derive from the secret and seal a record the peer opens.
	tk := derive_traffic_keys(s1, suite) or { panic(err) }
	rec := seal_record(suite, tk.key, tk.iv, 0, .application_data, 'ping'.bytes()) or {
		panic(err)
	}
	crec, _ := decode_cipher_record(rec) or { panic(err) }
	ok, ict, pt := try_open_record(suite, tk.key, tk.iv, 0, crec)
	assert ok && ict == .application_data && pt == 'ping'.bytes()
	// Wrong keys fail.
	bad_key := rand.bytes(cipher_suite_key_len(suite)) or { panic(err) }
	nok, _, _ := try_open_record(suite, bad_key, tk.iv, 0, crec)
	assert !nok
}

fn test_early_data_tampered_fails_closed() {
	clear_used_tickets()
	mut server := new_server_handshake(.off) or { panic(err) }
	server.max_early_data = 1024
	st := early_test_ticket(mut server)
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs.resume_offer = st
	chs.max_early_data = 1024
	ch_msg := chs.build_client_hello() or { panic(err) }
	assert chs.offered_early_data
	assert chs.has_early_traffic
	parsed_ch, _ := parse_handshake_message(ch_msg) or { panic(err) }
	ch := parse_client_hello(parsed_ch.body) or { panic(err) }
	assert ch_offers_early_data(ch)
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	assert server.accepted_psk
	assert server.accepted_early_data
	// The server flight carries the early_data accept with the limit.
	mut saw_ee := false
	mut off := 0
	for off < flight.len {
		p, n := parse_handshake_message(flight[off..]) or { panic(err) }
		if p.typ == .encrypted_extensions {
			assert ee_accepts_early_data(p.body)
			saw_ee = true
		}
		off += n
	}
	assert saw_ee
	// Positive control: the server opens the client's early record.
	rec := seal_record(st.suite, chs.early_traffic.key, chs.early_traffic.iv, 0, .application_data,
		'hello'.bytes()) or { panic(err) }
	crec, _ := decode_cipher_record(rec) or { panic(err) }
	ok, ict, pt := try_open_record(st.suite, server.early_traffic.key, server.early_traffic.iv,
		0, crec)
	assert ok && ict == .application_data && pt == 'hello'.bytes()
	// Tampered records verify under neither the early keys nor the
	// handshake keys, which is what makes the server abort the handshake.
	mut bad := rec.clone()
	bad[10] ^= u8(1)
	bad_rec, _ := decode_cipher_record(bad) or { panic(err) }
	bad_early, _, _ := try_open_record(st.suite, server.early_traffic.key, server.early_traffic.iv,
		0, bad_rec)
	assert !bad_early
	hs_client := derive_traffic_keys(server.handshake_keys.client_secret, st.suite) or {
		panic(err)
	}
	bad_hs, _, _ := try_open_record(st.suite, hs_client.key, hs_client.iv, 0, bad_rec)
	assert !bad_hs
}

fn test_no_early_data_default_unchanged() {
	clear_used_tickets()
	// Wire level: with the default (zero) limit neither side signals early data.
	mut server := new_server_handshake(.off) or { panic(err) }
	st := early_test_ticket(mut server)
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs.resume_offer = st
	ch_msg := chs.build_client_hello() or { panic(err) }
	assert !chs.offered_early_data
	parsed_ch, _ := parse_handshake_message(ch_msg) or { panic(err) }
	ch := parse_client_hello(parsed_ch.body) or { panic(err) }
	assert !ch_offers_early_data(ch)
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	assert server.accepted_psk
	assert !server.offered_early_data
	assert !server.accepted_early_data
	mut off := 0
	for off < flight.len {
		p, n := parse_handshake_message(flight[off..]) or { panic(err) }
		if p.typ == .encrypted_extensions {
			assert !ee_accepts_early_data(p.body)
			assert p.body == [u8(0), 0]
		}
		off += n
	}
	// Behavior level: default resumption still works end to end.
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	resumed_ch := chan bool{cap: 2}
	got_ch := chan string{cap: 2}
	srv := spawn serve_early_n(mut l, 2, resumed_ch, got_ch)
	mut c1 := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c1.handshake() or { panic(err) }
	assert !c1.resumed()
	c1.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	assert c1.read(mut buf) or { panic(err) } == 4
	assert buf == 'ping'.bytes()
	assert <-resumed_ch == false
	assert <-got_ch == 'ping'
	st2 := c1.session_state() or { panic('no ticket received') }
	assert st2.max_early_data == 0
	c1.close() or {}
	mut c2 := dial('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: st2
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert c2.resumed()
	c2.write('pong'.bytes()) or { panic(err) }
	mut buf2 := []u8{len: 4}
	assert c2.read(mut buf2) or { panic(err) } == 4
	assert buf2 == 'pong'.bytes()
	assert <-resumed_ch == true
	assert <-got_ch == 'pong'
	c2.close() or {}
	l.close() or {}
	_ = srv
}

fn test_early_data_accept_loopback() {
	clear_used_tickets()
	mut l := listen(Config{ verify: false, max_early_data: 1024 }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	resumed_ch := chan bool{cap: 2}
	got_ch := chan string{cap: 3}
	srv := spawn serve_early_double(mut l, resumed_ch, got_ch)
	mut c1 := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		max_early_data: 1024
	}) or { panic(err) }
	c1.handshake() or { panic(err) }
	assert !c1.resumed()
	c1.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	assert c1.read(mut buf) or { panic(err) } == 4
	assert buf == 'ping'.bytes()
	assert <-resumed_ch == false
	assert <-got_ch == 'ping'
	st := c1.session_state() or { panic('no ticket received') }
	assert st.max_early_data == 1024
	c1.close() or {}
	mut c2 := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		resume_from:    st
		max_early_data: 1024
		early_data:     'hello'.bytes()
	}) or { panic(err) }
	c2.handshake() or { panic('c2 hs: ${err}') }
	assert c2.resumed()
	// The server delivers the 0-RTT bytes before later application data.
	mut buf2 := []u8{len: 8}
	n2 := c2.read(mut buf2) or { panic('c2 read1: ${err}') }
	assert buf2[..n2].bytestr() == 'hello'
	c2.write('world'.bytes()) or { panic('c2 write: ${err}') }
	mut buf3 := []u8{len: 8}
	n3 := c2.read(mut buf3) or { panic('c2 read2: ${err}') }
	assert buf3[..n3].bytestr() == 'world'
	assert <-resumed_ch == true
	assert <-got_ch == 'hello'
	assert <-got_ch == 'world'
	c2.close() or {}
	l.close() or {}
	_ = srv
}

fn test_early_data_reject_fallback_loopback() {
	clear_used_tickets()
	// The server disables 0-RTT: the PSK still resumes, the early bytes are
	// discarded, and the handshake completes normally.
	mut l := listen(Config{ verify: false, max_early_data: 0 }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	resumed_ch := chan bool{cap: 2}
	got_ch := chan string{cap: 2}
	srv := spawn serve_early_n(mut l, 2, resumed_ch, got_ch)
	mut c1 := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c1.handshake() or { panic(err) }
	c1.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	assert c1.read(mut buf) or { panic(err) } == 4
	assert <-resumed_ch == false
	assert <-got_ch == 'ping'
	st := c1.session_state() or { panic('no ticket received') }
	c1.close() or {}
	mut c2 := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		resume_from:    st
		max_early_data: 1024
		early_data:     'hello'.bytes()
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert c2.resumed()
	c2.write('world'.bytes()) or { panic(err) }
	mut buf2 := []u8{len: 8}
	n2 := c2.read(mut buf2) or { panic(err) }
	// The rejected early bytes are never delivered; only app data echoes.
	assert buf2[..n2].bytestr() == 'world'
	assert <-resumed_ch == true
	assert <-got_ch == 'world'
	c2.close() or {}
	l.close() or {}
	_ = srv
}

// serve_early_replay serves the single-use shape: a plain full handshake,
// a resumed handshake with early data (two reads: 0-RTT then app data),
// and a replayed ticket that falls back to a full handshake.
fn serve_early_replay(mut l Listener, resumed_ch chan bool, got_ch chan string) {
	mut c1 := l.accept() or {
		eprintln('TRACE serve accept err: ${err}')
		resumed_ch <- false
		got_ch <- 'accept err: ${err}'
		return
	}
	resumed_ch <- c1.resumed()
	mut b0 := []u8{len: 32}
	m0 := c1.read(mut b0) or {
		got_ch <- 'read err: ${err}'
		c1.close() or {}
		return
	}
	got_ch <- b0[..m0].bytestr()
	c1.write(b0[..m0]) or {}
	c1.close() or {}
	mut c2 := l.accept() or {
		eprintln('TRACE serve accept err: ${err}')
		resumed_ch <- false
		got_ch <- 'accept err: ${err}'
		return
	}
	resumed_ch <- c2.resumed()
	mut b1 := []u8{len: 32}
	m1 := c2.read(mut b1) or {
		got_ch <- 'read err: ${err}'
		c2.close() or {}
		return
	}
	got_ch <- b1[..m1].bytestr()
	c2.write(b1[..m1]) or {}
	mut b2 := []u8{len: 32}
	m2 := c2.read(mut b2) or {
		got_ch <- 'read err: ${err}'
		c2.close() or {}
		return
	}
	got_ch <- b2[..m2].bytestr()
	c2.write(b2[..m2]) or {}
	c2.close() or {}
	mut c3 := l.accept() or {
		eprintln('TRACE serve accept err: ${err}')
		resumed_ch <- false
		got_ch <- 'accept err: ${err}'
		return
	}
	resumed_ch <- c3.resumed()
	mut b3 := []u8{len: 32}
	m3 := c3.read(mut b3) or {
		got_ch <- 'read err: ${err}'
		c3.close() or {}
		return
	}
	got_ch <- b3[..m3].bytestr()
	c3.write(b3[..m3]) or {}
	c3.close() or {}
}

fn test_early_data_replay_single_use() {
	clear_used_tickets()
	mut l := listen(Config{ verify: false, max_early_data: 1024 }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	resumed_ch := chan bool{cap: 3}
	got_ch := chan string{cap: 4}
	srv := spawn serve_early_replay(mut l, resumed_ch, got_ch)
	mut c1 := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		max_early_data: 1024
	}) or { panic(err) }
	c1.handshake() or { panic(err) }
	c1.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	assert c1.read(mut buf) or { panic(err) } == 4
	assert <-resumed_ch == false
	assert <-got_ch == 'ping'
	st := c1.session_state() or { panic('no ticket received') }
	c1.close() or {}
	// First resumption consumes the ticket and delivers early data.
	mut c2 := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		resume_from:    st
		max_early_data: 1024
		early_data:     'hello'.bytes()
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert c2.resumed()
	mut buf2 := []u8{len: 8}
	n2 := c2.read(mut buf2) or { panic(err) }
	assert buf2[..n2].bytestr() == 'hello'
	c2.write('w2'.bytes()) or { panic(err) }
	mut buf2b := []u8{len: 4}
	n2b := c2.read(mut buf2b) or { panic(err) }
	assert buf2b[..n2b].bytestr() == 'w2'
	assert <-resumed_ch == true
	assert <-got_ch == 'hello'
	assert <-got_ch == 'w2'
	c2.close() or {}
	// Replaying the same ticket falls back to a full handshake: no
	// resumption, no early delivery, but application data still flows.
	mut c3 := dial('127.0.0.1', port, Config{
		verify:         false
		server_name:    'localhost'
		resume_from:    st
		max_early_data: 1024
		early_data:     'again'.bytes()
	}) or { panic(err) }
	c3.handshake() or { panic(err) }
	assert !c3.resumed()
	c3.write('w3'.bytes()) or { panic(err) }
	mut buf3 := []u8{len: 4}
	n3 := c3.read(mut buf3) or { panic(err) }
	assert buf3[..n3].bytestr() == 'w3'
	assert <-resumed_ch == false
	assert <-got_ch == 'w3'
	c3.close() or {}
	l.close() or {}
	_ = srv
}

fn test_ticket_replay_cache_bounded() {
	clear_used_tickets()
	defer {
		clear_used_tickets()
	}
	// Fill past the cap with distinct nonces; the cache must stay bounded
	// and remain functional for fresh nonces.
	for i in 0 .. max_replay_cache_entries + 100 {
		mut nonce := []u8{len: 8}
		nonce[0] = u8(i & 0xff)
		nonce[1] = u8((i >> 8) & 0xff)
		nonce[2] = u8((i >> 16) & 0xff)
		nonce[3] = u8((i >> 24) & 0xff)
		mark_ticket_used(nonce)
	}
	assert used_ticket_nonces.len <= max_replay_cache_entries
	fresh := [u8(0xde), 0xad, 0xbe, 0xef, 0x01, 0x02, 0x03, 0x04]
	assert !is_ticket_used(fresh)
	mark_ticket_used(fresh)
	assert is_ticket_used(fresh)
}
