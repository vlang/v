module tls

import crypto.aes
import crypto.rand
import time

fn test_nst_codec_roundtrip() {
	nonce := rand.bytes(8) or { panic(err) }
	ticket := rand.bytes(64) or { panic(err) }
	body := encode_new_session_ticket(ticket_lifetime, u32(0x01020304), nonce, ticket) or {
		panic(err)
	}
	nst := parse_new_session_ticket(body) or { panic(err) }
	assert nst.lifetime == ticket_lifetime
	assert nst.age_add == u32(0x01020304)
	assert nst.nonce == nonce
	assert nst.ticket == ticket
	// trailing garbage is rejected
	mut bad := body.clone()
	bad << u8(0)
	parse_new_session_ticket(bad) or { return }
	panic('trailing data accepted')
}

fn test_ticket_seal_open_roundtrip() {
	key := rand.bytes(32) or { panic(err) }
	master := rand.bytes(32) or { panic(err) }
	nonce := rand.bytes(8) or { panic(err) }
	blob := seal_ticket(master, .tls_aes_128_gcm_sha256, u32(12345), nonce, key) or {
		panic(err)
	}
	opened, ononce, suite, created, age := open_ticket(blob, key) or { panic(err) }
	assert opened == master
	assert ononce == nonce
	assert suite == .tls_aes_128_gcm_sha256
	assert age == u32(12345)
	assert created > 0 && time.now().unix() - created < 60
	// tampered blobs are rejected
	mut bad := blob.clone()
	bad[20] ^= u8(1)
	open_ticket(bad, key) or { return }
	panic('tampered ticket accepted')
	// wrong keys are rejected
	other := rand.bytes(32) or { panic(err) }
	open_ticket(blob, other) or { return }
	panic('wrong ticket key accepted')
}

fn test_psk_binder_roundtrip() {
	suite := CipherSuite.tls_aes_128_gcm_sha256
	key := rand.bytes(32) or { panic(err) }
	master := rand.bytes(32) or { panic(err) }
	nonce := rand.bytes(8) or { panic(err) }
	blob := seal_ticket(master, suite, u32(999), nonce, key) or { panic(err) }
	opened, ononce, _, _, _ := open_ticket(blob, key) or { panic(err) }
	res_master := derive_resumption_master(opened, [u8(1), 2, 3], suite) or { panic(err) }
	_ = res_master
	psk := derive_psk(opened, ononce, suite) or { panic(err) }
	es := derive_early_secret_with_psk(psk, suite) or { panic(err) }
	bk := binder_key_for(es, suite) or { panic(err) }
	offer := encode_psk_offer(blob, u32(1000), suite)
	ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32, init: 7}
		session_id:          []u8{}
		cipher_suites:       [suite]
		compression_methods: [u8(0)]
		extensions:          [
			Extension{ typ: ext_psk_key_exchange_modes, data: encode_psk_modes() },
			Extension{ typ: ext_pre_shared_key, data: offer },
		]
	}
	ch_msg := encode_handshake_message(.client_hello, encode_client_hello(ch)!) or {
		panic(err)
	}
	trunc := binder_prefix(ch_msg, offer) or { panic(err) }
	th := transcript_hash_for_suite(trunc, suite)
	binder := compute_binder(bk, th, suite) or { panic(err) }
	assert verify_binder(bk, th, binder, suite)
	mut bad := binder.clone()
	bad[0] ^= u8(1)
	assert !verify_binder(bk, th, bad, suite)
	assert parse_psk_modes(encode_psk_modes())
	assert parse_psk_selected(encode_psk_selected()) or { panic(err) } == 0
}

fn test_psk_server_flight() { // Full ticket issued by this server, offered back by a client: the
	// server flight must be SH + EE + Finished with no certificate flight.
	mut server := new_server_handshake(.off) or { panic(err) }
	suite := CipherSuite.tls_aes_128_gcm_sha256
	master := rand.bytes(32) or { panic(err) }
	tnonce := rand.bytes(8) or { panic(err) }
	age_add := u32(0x0a0b0c0d)
	blob := seal_ticket(master, suite, age_add, tnonce, server.ticket_key) or { panic(err) }
	psk := derive_psk(master, tnonce, suite) or { panic(err) }
	st := SessionState{
		ticket:       blob.clone()
		psk:          psk
		suite:        suite
		created_unix: time.now().unix()
		lifetime:     ticket_lifetime
		age_add:      age_add
	}
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs.resume_offer = st
	ch_msg := chs.build_client_hello() or { panic(err) }
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	assert server.accepted_psk
	mut types := []HandshakeType{}
	mut off := 0
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		types << parsed.typ
		off += n
	}
	assert types == [HandshakeType.server_hello, .encrypted_extensions, .finished]
	// A tampered binder must fall back to the full certificate flight.
	mut chs2 := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs2.resume_offer = st
	mut bad_msg := chs2.build_client_hello() or { panic(err) }
	bad_msg[bad_msg.len - 1] ^= u8(1)
	mut server2 := new_server_handshake(.off) or { panic(err) }
	server2.ticket_key = server.ticket_key.clone()
	flight2 := server2.process_client_hello(bad_msg) or { panic(err) }
	assert !server2.accepted_psk
	mut n2 := 0
	mut o2 := 0
	for o2 < flight2.len {
		_, n := parse_handshake_message(flight2[o2..]) or { panic(err) }
		n2++
		o2 += n
	}
	assert n2 == 5
	// A ticket the server cannot open (wrong ticket key) also falls back.
	mut server3 := new_server_handshake(.off) or { panic(err) }
	flight3 := server3.process_client_hello(ch_msg) or { panic(err) }
	assert !server3.accepted_psk
	mut n3 := 0
	mut o3 := 0
	for o3 < flight3.len {
		_, n := parse_handshake_message(flight3[o3..]) or { panic(err) }
		n3++
		o3 += n
	}
	assert n3 == 5
}

fn serve_resume_twice(mut l Listener, ch chan bool) {
	for _ in 0 .. 2 {
		mut conn := l.accept() or {
			ch <- false
			continue
		}
		resumed := conn.resumed()
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or {
			ch <- false
			conn.close() or {}
			continue
		}
		if n > 0 {
			conn.write(buf[..n]) or {}
		}
		conn.close() or {}
		ch <- resumed
	}
}

fn test_resumption_loopback() {
	mut l := listen(Config{ verify: false }) or { panic(err) }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	ch := chan bool{cap: 2}
	server := spawn serve_resume_twice(mut l, ch)
	mut c1 := dial('127.0.0.1', port, Config{ verify: false, server_name: 'localhost' }) or {
		panic(err)
	}
	c1.handshake() or { panic(err) }
	assert !c1.resumed()
	c1.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	n := c1.read(mut buf) or { panic(err) }
	assert n == 4
	assert buf == 'ping'.bytes()
	assert <-ch == false
	// The first read consumed the post-handshake ticket, so resumption
	// state is available without ever hanging a ticket-less handshake.
	st := c1.session_state() or { panic('no ticket received') }
	assert st.ticket.len > 0
	assert st.psk.len > 0
	c1.close() or {}
	mut c2 := dial('127.0.0.1', port, Config{
		verify:      false
		server_name: 'localhost'
		resume_from: st
	}) or { panic(err) }
	c2.handshake() or { panic(err) }
	assert c2.resumed()
	c2.write('pong'.bytes()) or { panic(err) }
	mut buf2 := []u8{len: 4}
	n2 := c2.read(mut buf2) or { panic(err) }
	assert n2 == 4
	assert buf2 == 'pong'.bytes()
	assert <-ch == true
	c2.close() or {}
	l.close() or {}
	_ = server
}

fn test_psk_fallback_branches() {
	suite := CipherSuite.tls_aes_128_gcm_sha256
	mut server := new_server_handshake(.off) or { panic(err) }
	master := rand.bytes(32) or { panic(err) }
	tnonce := rand.bytes(8) or { panic(err) }
	age_add := u32(0x0a0b0c0d)
	blob := seal_ticket(master, suite, age_add, tnonce, server.ticket_key) or { panic(err) }
	psk := derive_psk(master, tnonce, suite) or { panic(err) }
	st := SessionState{
		ticket:       blob.clone()
		psk:          psk
		suite:        suite
		created_unix: time.now().unix()
		lifetime:     ticket_lifetime
		age_add:      age_add
	}
	new_offering_client := fn (s SessionState) ClientHello {
		mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
			panic(err)
		}
		chs.resume_offer = s
		ch_msg := chs.build_client_hello() or { panic(err) }
		parsed, _ := parse_handshake_message(ch_msg) or { panic(err) }
		return parse_client_hello(parsed.body) or { panic(err) }
	}
	_ = new_offering_client
	// No CertificateRequest is sent on a resumed handshake even when the
	// server wants client certs.
	mut vserver := new_server_handshake(.off) or { panic(err) }
	vserver.ticket_key = server.ticket_key.clone()
	vserver.verify_client = true
	mut chs := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	chs.resume_offer = st
	ch_msg := chs.build_client_hello() or { panic(err) }
	flight := vserver.process_client_hello(ch_msg) or { panic(err) }
	assert vserver.accepted_psk
	mut types := []HandshakeType{}
	mut off := 0
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		types << parsed.typ
		off += n
	}
	assert types == [HandshakeType.server_hello, .encrypted_extensions, .finished]
	// A skewed ticket age falls back to a full handshake.
	mut chs_old := new_client_handshake('localhost', []string{}, .off, []CipherSuite{}) or {
		panic(err)
	}
	mut st_old := st
	st_old.created_unix = time.now().unix() - 100000
	chs_old.resume_offer = st_old
	old_age_msg := chs_old.build_client_hello() or { panic(err) }
	mut server_old := new_server_handshake(.off) or { panic(err) }
	server_old.ticket_key = server.ticket_key.clone()
	_ = server_old.process_client_hello(old_age_msg) or { panic(err) }
	assert !server_old.accepted_psk
	// A multi-identity offer is rejected.
	parsed_full, _ := parse_handshake_message(ch_msg) or { panic(err) }
	ch0 := parse_client_hello(parsed_full.body) or { panic(err) }
	mut ident := []u8{}
	for _ in 0 .. 2 {
		ident << u8(st.ticket.len >> 8)
		ident << u8(st.ticket.len & 0xff)
		ident << st.ticket
		ident << u8(0)
		ident << u8(0)
		ident << u8(0)
		ident << u8(0)
	}
	mut multi := []u8{}
	multi << u8(ident.len >> 8)
	multi << u8(ident.len & 0xff)
	multi << ident
	blen := 2 * (1 + cipher_suite_hash_len(suite))
	multi << u8(blen >> 8)
	multi << u8(blen & 0xff)
	for _ in 0 .. 2 {
		multi << u8(cipher_suite_hash_len(suite))
		for _ in 0 .. cipher_suite_hash_len(suite) {
			multi << u8(0)
		}
	}
	ok, _ := server.try_accept_psk(ch_msg, multi)
	assert !ok
	// An expired ticket (sealed box with an old timestamp) is rejected.
	mut plain := []u8{}
	plain << u8(int(suite) >> 8)
	plain << u8(int(suite))
	created_old := time.now().unix() - 100000
	for i in 0 .. 8 {
		plain << u8((u64(created_old) >> (8 * (7 - i))) & 0xff)
	}
	plain << u8(age_add >> 24)
	plain << u8(age_add >> 16)
	plain << u8(age_add >> 8)
	plain << u8(age_add)
	plain << u8(tnonce.len)
	plain << tnonce
	plain << u8(master.len)
	plain << master
	gcm := aes.new_aes_gcm(server.ticket_key)!
	tn := rand.bytes(12)!
	ct := gcm.encrypt(plain, tn, []u8{})!
	mut oldblob := []u8{}
	oldblob << tn
	oldblob << ct
	mut exts := ch0.extensions.clone()
	exts[exts.len - 1] = Extension{
		typ:  ext_pre_shared_key
		data: encode_psk_offer(oldblob, u32(100), suite)
	}
	ch_old := ClientHello{
		legacy_version:      ch0.legacy_version
		random:              ch0.random.clone()
		session_id:          ch0.session_id.clone()
		cipher_suites:       ch0.cipher_suites.clone()
		compression_methods: ch0.compression_methods.clone()
		extensions:          exts
	}
	old_msg := encode_handshake_message(.client_hello, encode_client_hello(ch_old)!)!
	mut server_exp := new_server_handshake(.off) or { panic(err) }
	server_exp.ticket_key = server.ticket_key.clone()
	_ = server_exp.process_client_hello(old_msg) or { panic(err) }
	assert !server_exp.accepted_psk
	// Overlong ticket material is rejected at encode time.
	mut failed := false
	encode_new_session_ticket(0, 0, []u8{len: 300}, []u8{len: 1}) or {
		failed = true
		[]u8{}
	}
	assert failed
	mut failed2 := false
	seal_ticket([]u8{}, suite, 0, tnonce, server.ticket_key) or {
		failed2 = true
		[]u8{}
	}
	assert failed2
}

fn test_psk_offer_with_unknown_suite() {
	// Peers (e.g. browsers) send GREASE/unknown cipher suites. The binder
	// covers the raw wire prefix, so unknown suites must not break resumption.
	suite := CipherSuite.tls_aes_128_gcm_sha256
	mut server := new_server_handshake(.off) or { panic(err) }
	master := rand.bytes(32) or { panic(err) }
	tnonce := rand.bytes(8) or { panic(err) }
	age_add := u32(0x0a0b0c0d)
	blob := seal_ticket(master, suite, age_add, tnonce, server.ticket_key) or { panic(err) }
	psk := derive_psk(master, tnonce, suite) or { panic(err) }
	st := SessionState{
		ticket:       blob.clone()
		psk:          psk
		suite:        suite
		created_unix: time.now().unix()
		lifetime:     ticket_lifetime
		age_add:      age_add
	}
	mut offered := [CipherSuite.tls_aes_128_gcm_sha256]
	offered << unsafe { CipherSuite(0x0a0a) }
	mut chs := new_client_handshake('localhost', []string{}, .off, offered) or { panic(err) }
	chs.resume_offer = st
	ch_msg := chs.build_client_hello() or { panic(err) }
	// Sanity: the unknown suite really is on the wire, and parsing drops it.
	parsed, _ := parse_handshake_message(ch_msg) or { panic(err) }
	pch := parse_client_hello(parsed.body) or { panic(err) }
	assert pch.cipher_suites == [CipherSuite.tls_aes_128_gcm_sha256]
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	assert server.accepted_psk
	mut types := []HandshakeType{}
	mut off := 0
	for off < flight.len {
		p, n := parse_handshake_message(flight[off..]) or { panic(err) }
		types << p.typ
		off += n
	}
	assert types == [HandshakeType.server_hello, .encrypted_extensions, .finished]
}

fn hex_to_bytes(s string) []u8 {
	mut out := []u8{cap: s.len / 2}
	for i := 0; i + 1 < s.len; i += 2 {
		hi := if s[i] >= u8(`a`) { s[i] - u8(`a`) + 10 } else { s[i] - u8(`0`) }
		lo := if s[i + 1] >= u8(`a`) { s[i + 1] - u8(`a`) + 10 } else { s[i + 1] - u8(`0`) }
		out << (hi << 4) | lo
	}
	return out
}

// test_binder_openssl_vector pins PSK binder verification against a real
// OpenSSL 3 s_client resumption offer captured on the wire. The resumption
// master, ticket nonce and offered binder below come from a live handshake
// against a net.tls listener; the transcript prefix a binder covers is the
// raw ClientHello with the binders list chopped off (RFC 8446 §4.2.11.2),
// not a zeroed copy — the zeroed copy fails against OpenSSL.
fn test_binder_openssl_vector() {
	suite := CipherSuite.tls_aes_128_gcm_sha256
	raw := hex_to_bytes('0100063d03033ab5e91fda4b7f26c73014bfd62b7d53b2b578c1831976fd8979b9b94fa3b7d52088cf10cd62f7da39dee2c2aad243a531e754be39c0e638767ed5c0d6779b33fb0006130213031301010005ee000a0018001611ec11eb11ee001d0017001e00180019002901000101002300000016000000170000000d0044004209050906090404030503060308070808081a081b081c0809080a080b080408050806040105010601091109120913091409150916091709180919091a091b091c0708002b0003020304002d00020101003304ea04e811ec04c04a7a7ec4936a34ca1cf7b4421302bc27d163ee8397288892570019c9d5575b12a146018b464cc3a7764be3e59b744a5c08622d92d97e7a533ab61b8ea9470735e93079a2aa61735841539659a74a951c8a8f40246fa028f82b9e3666850a936f9cb9a2cd45bd39726a8e27c28f653baf07c664b24f4a5bab09dc72b0a83ace1a580ac4a0dca6ba3f2c7ccd410451d89d9b5145d4539bc0552569283185573b34535c973c625ef04c103b174166cf0746479fa4643881b109f61135f25e24220281460c4947849718116da669a4c5600265347ae9b9bc73cb80591f6be939dec06f5adc2e2da121b3b101f6598346750ca00028e0b47a0e7ca7ef59878ddaaf37f71d4ac34e0b8671c5337e4e38a9d592ce8c8361851a3c33d1602e4a7e5d52c6926035217b2a13e06f329036db632662e2a82bb75aa71c927e3baa3ff07db2a93965e997c6949c372328744a6828742421168c06f34097a85fc05905fb9baa0e3aad491bbaaec916aab7b69f69c5dd24c97c45bc337941bc931dfc3c2d2d7c6563d1441a510a8907ccc3722353e86e504c0aaf390987510e8e5458722c98b6288a4d0ba039c42d59f0320c373070806699f027838bcf6eb028c8823b1327ca9fca5ee88233a0886bf970adb0c852dd770b90878c1f110defb6516f252c622bac72e732dd2250d2d3b003445a68f9a0e470cd910a9094453e1de6321f973307352265943c631521edd0299719869090106e98ca23c72ba0d346d6fc6e4af0520a34a0f421c65d910abd29955f294a0f56cd1345500527c27357625aa8ad2383ced22ca9630cc72dac18b4972e232951059053a61a755aab4db2333a57f326418b682ac38880eb0d10e921cb854bc1566c6837971e57139ef33e00cb90b9e47ef8a4703db4b1d55b7e7dcc8251c856c9fc757f8b851b12976907a6cf582303ebc68a8557b30a09844233ef15812ff925769c8f69b65924b45ac37ab1f4f050a3fc32c4d091bcd5842337311847744d07b4eb28470c446c7c648e9841cd1e29bdbb585d6f44196a37cffb1a7451872d66333031f33c5caca292622092744565cb0ec5b564da61319ac0835157b8ccab05eb1a69df791ca240537a993fe1212854c252e8868eb3c574732344dfc2a1b8177a8930b938b944f5db30a50c69d6401fb36ca6f0608cea63cc17d300fffaca60b46cafe6bddc9a3c1df2b55856634bb1c2b9344935fb2fc5d17d8d849ae4c76fb117b08f59471cc78b35351ccc56baa1a944e0fbb9c35717993b21163b7115a03d57713bd3402e324b040765b7780a0ab7d0a61e1c35c8249d311b4225dc49356b3c9d2340c24410b6713afb6acc86991b29f87e6f2c59d8e73c1e3cc1c2b213ec76a5fc16387ba47c56ea7435466b76c66686605f6ec4c8eaa3b0c44b6b22700695808cabcb5e78938aa9dc6f348a478d057143d9b516f73eddc68d270a4d6b2365c9026fad330a1d7257b3c49aea14b61727095145c3f9f908f10a7566d395aaa16717973a13d5ca04b7bd842ca028d1c222786d26414d0f994717a577138697c3b2238c795c9f8a6a05d19da35a89a56aa6ac88abb0852f4f954886a5ba225090a39b2f1458a307f10c5865272b204c59e48e4fd089ccc334dab4fc17968ff96514bad03b34ffaddc5eac38b8e0c1143446e4958edbf2cae3e06f7ca05640fcec40f01e67823d7fc0c048c80f91fd13001d0020038cb513910d5bca8e1b518456a0bd4bfb79df85ed00715b0f5d7b641da3d8120029007f005a0054584274d06e84223d2a622d9e16f72ec1b671decc50ebd0883f055e78aea0f1b8aae52dc7f777511f89b22d77ec8ce6923286443527177e10db11a51fa8da950eb98166555ae10041dd72f869f8bb0ea07f8590b6240c344f002120add7b7e69afdb5e212678603215752c049639e191dbcd2dd896e64b34213e321')
	assert raw.len == 1601
	res_master := hex_to_bytes('b241c67b19cb745f243a0d3378747411265b7c1013773296081b8f2d76be403f')
	tnonce := hex_to_bytes('2a9e2dece0c66339')
	offered := hex_to_bytes('add7b7e69afdb5e212678603215752c049639e191dbcd2dd896e64b34213e321')
	parsed, n := parse_handshake_message(raw) or { panic(err) }
	assert n == raw.len
	assert parsed.typ == .client_hello
	ch := parse_client_hello(parsed.body) or { panic(err) }
	// The re-encoding must be byte-exact or binder verification cannot work.
	reenc := encode_handshake_message(.client_hello, encode_client_hello(ch)!)!
	assert reenc == raw
	mut psk_data := []u8{}
	for e in ch.extensions {
		if e.typ == ext_pre_shared_key {
			psk_data = e.data.clone()
		}
	}
	ids, binders := parse_psk_offer(psk_data) or { panic(err) }
	assert ids.len == 1 && binders.len == 1
	assert binders[0] == offered
	psk := derive_psk(res_master, tnonce, suite) or { panic(err) }
	es := derive_early_secret_with_psk(psk, suite) or { panic(err) }
	bk := binder_key_for(es, suite) or { panic(err) }
	trunc := binder_prefix(raw, psk_data) or { panic(err) }
	assert trunc.len == raw.len - 35
	assert verify_binder(bk, transcript_hash_for_suite(trunc, suite), offered, suite)
	// Negative controls: a flipped binder and a wrong-length binder fail.
	mut bad := offered.clone()
	bad[0] ^= u8(1)
	assert !verify_binder(bk, transcript_hash_for_suite(trunc, suite), bad, suite)
	assert !verify_binder(bk, transcript_hash_for_suite(trunc, suite), bad[..16], suite)
}
