// retransmit and record unit tests (no network).
module dtls12

fn test_dtls12_record_roundtrip() {
	rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 42
		fragment:        [u8(1), 2, 3, 4]
	}
	enc := encode_dtls12_record(rec)!
	dec := decode_dtls12_record(enc)!
	assert dec.content_type == ContentType.handshake
	assert dec.epoch == dtls12_epoch_handshake
	assert dec.sequence_number == 42
	assert dec.fragment == [u8(1), 2, 3, 4]
}

fn test_dtls12_record_app_epoch() {
	rec := Dtls12Record{
		content_type:    .application_data
		epoch:           dtls12_epoch_application
		sequence_number: 7
		fragment:        'hi'.bytes()
	}
	enc := encode_dtls12_record(rec)!
	dec := decode_dtls12_record(enc)!
	assert dec.content_type == ContentType.application_data
	assert dec.epoch == dtls12_epoch_application
	assert dec.sequence_number == 7
	assert dec.fragment == 'hi'.bytes()
}

fn test_dtls12_record_rejects_bad() {
	mut bad := [u8(99), 0xfe, 0xfd, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0xaa]
	decode_dtls12_record(bad) or {
		assert err.msg().contains('content type')
		mut bad_ver := [u8(22), 0x03, 0x03, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xaa]
		decode_dtls12_record(bad_ver) or {
			assert err.msg().contains('version')
			decode_dtls12_record(bad_ver[..10]) or {
				mut bad_len := [u8(22), 0xfe, 0xfd, 0, 1, 0, 0, 0, 0, 0, 0, 0, 5, 0xaa]
				decode_dtls12_record(bad_len) or {
					mut big := []u8{len: 2050, init: u8(0xbb)}
					encode_dtls12_record(Dtls12Record{
						content_type:    .handshake
						epoch:           dtls12_epoch_handshake
						sequence_number: 0
						fragment:        big
					}) or {
						return
					}
					assert false, 'expected fragment-size error'
				}
				return
			}
			assert false, 'expected truncation error'
		}
		assert false, 'expected version error'
		return
	}
	assert false, 'expected content-type error'
}

fn test_dtls12_record_seq_limit() {
	rec := Dtls12Record{
		content_type:    .handshake
		epoch:           0
		sequence_number: u64(1) << 48
		fragment:        [u8(1)]
	}
	encode_dtls12_record(rec) or {
		assert err.msg().contains('sequence')
		return
	}
	assert false, 'expected seq limit error'
}

fn test_dtls12_replay_window() {
	mut w := ReplayWindow{}
	assert !w.is_replay(0)
	assert w.mark(0)
	assert w.is_replay(0)
	assert !w.mark(0)
	assert !w.is_replay(1)
	assert w.mark(1)
	assert w.is_replay(0)
	assert w.is_replay(1)
	assert w.mark(2)
	assert !w.is_replay(3)
	assert w.mark(4)
	assert !w.is_replay(3)
	assert w.mark(3)
	assert w.is_replay(3)
	assert w.mark(100)
	assert w.is_replay(100)
	assert w.is_replay(36)
	assert w.is_replay(4)
	assert w.mark(101)
}

fn test_dtls12_replay_reset() {
	mut w := ReplayWindow{}
	assert w.mark(5)
	w.reset()
	assert !w.is_replay(5)
	assert w.mark(5)
}

fn test_dtls12_backoff() {
	assert backoff_for_attempt(100, 1000, 0) == 100
	assert backoff_for_attempt(100, 1000, 1) == 200
	assert backoff_for_attempt(100, 1000, 2) == 400
	assert backoff_for_attempt(100, 1000, 3) == 800
	assert backoff_for_attempt(100, 1000, 4) == 1000
	assert backoff_for_attempt(100, 1000, 5) == 1000
	mut t := new_retransmit_timer(100, 1000)
	_ = t.next_timeout()
	_ = t.next_timeout()
	assert t.attempts == 2
	t.reset()
	assert t.attempts == 0
}

fn test_dtls12_suite_negotiation() {
	offered := [u16(suite_ecdhe_rsa_aes128_gcm_sha256), u16(suite_ecdhe_rsa_aes128_cbc_sha256)]
	prefs := [u16(suite_ecdhe_rsa_aes128_cbc_sha256), u16(suite_ecdhe_rsa_aes128_gcm_sha256)]
	sel := select_suite(offered, prefs)!
	assert sel == suite_ecdhe_rsa_aes128_cbc_sha256
	select_suite([u16(suite_ecdhe_rsa_aes128_gcm_sha256)], [
		u16(suite_ecdhe_ecdsa_aes128_cbc_sha256),
	]) or {
		assert err.msg().contains('no supported')
		return
	}
	assert false, 'expected suite error'
}

fn test_dtls12_hello_codec() {
	cr := []u8{len: 32, init: 7}
	suites := [u16(suite_ecdhe_rsa_aes128_gcm_sha256)]
	ch := build_client_hello_msg(cr, suites, []u8{})!
	pr, ps, pc := parse_client_hello_msg(ch)!
	assert pr == cr
	assert ps.len == 1 && ps[0] == suite_ecdhe_rsa_aes128_gcm_sha256
	assert pc.len == 0
	sr := []u8{len: 32, init: 9}
	sh := build_server_hello_msg(sr, suites[0])!
	qr, qs := parse_server_hello_msg(sh)!
	assert qr == sr
	assert qs == suites[0]
	// Cookie round-trips.
	cookie := [u8(1), 2, 3, 4, 5]
	ch2 := build_client_hello_msg(cr, suites, cookie)!
	_, _, pc2 := parse_client_hello_msg(ch2)!
	assert pc2 == cookie
	hvr := build_hello_verify_request(cookie)!
	parsed_cookie := parse_hello_verify_request(hvr)!
	assert parsed_cookie == cookie
}

fn test_dtls12_finished_roundtrip() {
	suite := suite_ecdhe_rsa_aes128_gcm_sha256
	cr := []u8{len: 32, init: 1}
	sr := []u8{len: 32, init: 2}
	// ECDHE premaster: both sides agree on the ECDH secret.
	spriv, sx, sy := p256_keygen()!
	cpriv, cx, cy := p256_keygen()!
	pre_s := p256_ecdh(spriv, cx, cy)!
	pre_c := p256_ecdh(cpriv, sx, sy)!
	assert pre_s == pre_c
	ch := build_client_hello_msg(cr, [u16(suite)], []u8{})!
	sh := build_server_hello_msg(sr, suite)!
	mut trans := ch.clone()
	trans << sh
	master := derive_master(pre_s, cr, sr)!
	master2 := derive_master(pre_c, cr, sr)!
	assert master == master2
	sfin := build_finished_msg(master, 'server finished', trans)!
	verify_finished_msg(sfin, master, 'server finished', trans)!
	verify_finished_msg(sfin, master, 'client finished', trans) or { return }
	assert false, 'expected Finished label mismatch'
}

fn test_dtls12_gcm_cbc_roundtrip() {
	// GCM.
	gcm_key := []u8{len: 16, init: 0x0b}
	gcm_salt := []u8{len: 4, init: 0x0d}
	pt := 'hello dtls12 gcm'.bytes()
	sealed_gcm := gcm_seal(gcm_key, gcm_salt, dtls12_epoch_application, 7, u8(ContentType.application_data),
		pt)!
	opened_gcm := gcm_open(gcm_key, gcm_salt, dtls12_epoch_application, 7, u8(ContentType.application_data),
		sealed_gcm)!
	assert opened_gcm == pt
	mut bad_gcm := sealed_gcm.clone()
	bad_gcm[10] ^= 0x01
	mut gcm_failed := false
	gcm_open(gcm_key, gcm_salt, dtls12_epoch_application, 7, u8(ContentType.application_data),
		bad_gcm) or { gcm_failed = true }
	assert gcm_failed
	// CBC+HMAC.
	mac_key := []u8{len: 32, init: 0x11}
	enc_key := []u8{len: 16, init: 0x22}
	sealed_cbc := cbc_seal(mac_key, enc_key, dtls12_epoch_application, 3, u8(ContentType.application_data),
		pt)!
	opened_cbc := cbc_open(mac_key, enc_key, dtls12_epoch_application, 3, u8(ContentType.application_data),
		sealed_cbc)!
	assert opened_cbc == pt
	mut bad_cbc := sealed_cbc.clone()
	bad_cbc[20] ^= 0x01
	mut cbc_failed := false
	cbc_open(mac_key, enc_key, dtls12_epoch_application, 3, u8(ContentType.application_data),
		bad_cbc) or { cbc_failed = true }
	assert cbc_failed
}

fn test_dtls12_loss_simulation_flags() {
	// The loss-simulation knobs only drop datagrams; they must not affect
	// codecs. A config with drops still negotiates the same suites.
	cfg := default_config()
	assert cfg.initial_rto_ms == 100
	mut lossy := default_config()
	lossy.drop_tx_first_n = 5
	lossy.drop_rx_first_n = 5
	assert lossy.drop_tx_first_n == 5
	assert effective_suites([]u16{}) == default_suites
	assert effective_suites([u16(suite_ecdhe_rsa_aes128_cbc_sha256)]) == [
		u16(suite_ecdhe_rsa_aes128_cbc_sha256),
	]
}
