// retransmit and record unit tests (no network).
module dtls

import net.tls

fn test_dtls_record_roundtrip() {
	rec := DtlsRecord{
		content_type:    tls.ContentType.handshake
		epoch:           dtls_epoch_handshake
		sequence_number: 42
		fragment:        [u8(1), 2, 3, 4]
	}
	enc := encode_dtls_record(rec)!
	dec := decode_dtls_record(enc)!
	assert dec.content_type == tls.ContentType.handshake
	assert dec.epoch == dtls_epoch_handshake
	assert dec.sequence_number == 42
	assert dec.fragment == [u8(1), 2, 3, 4]
}

fn test_dtls_record_app_epoch() {
	rec := DtlsRecord{
		content_type:    tls.ContentType.application_data
		epoch:           dtls_epoch_application
		sequence_number: 7
		fragment:        'hi'.bytes()
	}
	enc := encode_dtls_record(rec)!
	dec := decode_dtls_record(enc)!
	assert dec.epoch == dtls_epoch_application
	assert dec.sequence_number == 7
}

fn test_dtls_record_rejects_bad() {
	mut bad := [u8(99), 0xfe, 0xfd, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0xaa]
	decode_dtls_record(bad) or {
		assert err.msg().contains('content type')
		return
	}
	assert false, 'expected content-type error'
}

fn test_dtls_record_seq_limit() {
	rec := DtlsRecord{
		content_type:    tls.ContentType.handshake
		epoch:           0
		sequence_number: u64(1) << 48
		fragment:        [u8(1)]
	}
	encode_dtls_record(rec) or {
		assert err.msg().contains('sequence')
		return
	}
	assert false, 'expected seq limit error'
}

fn test_dtls_replay_window() {
	mut w := ReplayWindow{}
	assert !w.is_replay(0)
	assert w.mark(0)
	assert w.is_replay(0)
	assert !w.mark(0)
	assert !w.is_replay(1)
	assert w.mark(1)
	assert w.is_replay(0)
	assert w.is_replay(1)
	// gap: 3 missing, 2 and 4 seen
	assert w.mark(2)
	assert !w.is_replay(3)
	assert w.mark(4)
	assert !w.is_replay(3)
	assert w.mark(3)
	assert w.is_replay(3)
	// slide far ahead: old seqs become too-old replays
	assert w.mark(100)
	assert w.is_replay(100)
	assert w.is_replay(36)
	assert w.is_replay(4)
	assert w.mark(101)
}

fn test_dtls_replay_reset() {
	mut w := ReplayWindow{}
	assert w.mark(5)
	w.reset()
	assert !w.is_replay(5)
	assert w.mark(5)
}

fn test_dtls_backoff() {
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

fn test_dtls_suite_negotiation() {
	offered := [tls.CipherSuite.tls_aes_128_gcm_sha256, tls.CipherSuite.tls_chacha20_poly1305_sha256]
	prefs := [tls.CipherSuite.tls_chacha20_poly1305_sha256, tls.CipherSuite.tls_aes_128_gcm_sha256]
	sel := select_suite(offered, prefs)!
	assert sel == tls.CipherSuite.tls_chacha20_poly1305_sha256
	select_suite([tls.CipherSuite.tls_aes_128_gcm_sha256], [
		tls.CipherSuite.tls_aes_256_gcm_sha384,
	]) or {
		assert err.msg().contains('no supported')
		return
	}
	assert false, 'expected suite error'
}

fn test_dtls_hello_codec() {
	cr := []u8{len: 32, init: 7}
	suites := [tls.CipherSuite.tls_aes_128_gcm_sha256]
	ch := build_client_hello_msg(cr, suites)!
	pr, ps := parse_client_hello_msg(ch)!
	assert pr == cr
	assert ps.len == 1 && ps[0] == tls.CipherSuite.tls_aes_128_gcm_sha256
	sr := []u8{len: 32, init: 9}
	sh := build_server_hello_msg(sr, suites[0])!
	qr, qs := parse_server_hello_msg(sh)!
	assert qr == sr
	assert qs == suites[0]
}

fn test_dtls_finished_roundtrip() {
	suite := tls.CipherSuite.tls_aes_128_gcm_sha256
	cr := []u8{len: 32, init: 1}
	sr := []u8{len: 32, init: 2}
	ch := build_client_hello_msg(cr, [suite])!
	sh := build_server_hello_msg(sr, suite)!
	mut trans := ch.clone()
	trans << sh
	hs := derive_handshake_secrets(suite, trans, cr, sr)!
	sfin := build_finished_msg(hs.server_secret, trans, suite)!
	verify_finished_msg(sfin, hs.server_secret, trans, suite)!
	// wrong secret must fail
	verify_finished_msg(sfin, hs.client_secret, trans, suite) or { return }
	assert false, 'expected Finished mismatch'
}
