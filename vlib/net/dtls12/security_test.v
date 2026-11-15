// ECDHE / epoch-1 Finished security tests: tampered shares must fail,
// passive observers with only randoms must not decrypt, Finished must not
// fly in epoch-0 plaintext, and distinct peers must get distinct secrets.
module dtls12

import crypto.rand
import net
import time

// dtls12_sec_echo_server runs one echo handshake (self-contained: _test.v
// files compile separately, so the loopback helper is duplicated here).
fn dtls12_sec_echo_server(cfg Config, addr_ch chan string, done chan bool) {
	mut srv := bind('127.0.0.1:0', cfg) or {
		eprintln('server bind err: ${err}')
		addr_ch <- ''
		done <- false
		return
	}
	addr := srv.local_addr() or {
		eprintln('server addr err: ${err}')
		addr_ch <- ''
		done <- false
		return
	}
	addr_ch <- addr.str()
	srv.handshake() or {
		eprintln('server hs err: ${err}')
		done <- false
		return
	}
	mut buf := []u8{len: 256}
	n := srv.read(mut buf) or {
		eprintln('server read err: ${err}')
		done <- false
		return
	}
	srv.write(buf[..n]) or {
		eprintln('server write err: ${err}')
		done <- false
		return
	}
	srv.close() or {}
	done <- true
}

fn test_dtls12_ecdhe_tamper_fails_finished() {
	spriv, sx, sy := p256_keygen()!
	cpriv, cx, cy := p256_keygen()!
	// Both sides agree with correct shares.
	sec_s := p256_ecdh(spriv, cx, cy)!
	sec_c := p256_ecdh(cpriv, sx, sy)!
	assert sec_s == sec_c
	cr := []u8{len: 32, init: 1}
	sr := []u8{len: 32, init: 2}
	master := derive_master(sec_c, cr, sr)!
	trans := 'test transcript'.bytes()
	sfin := build_finished_msg(master, 'server finished', trans)!
	verify_finished_msg(sfin, master, 'server finished', trans)!
	// Wrong peer share: tampered X must derive a different master (or fail),
	// and a Finished under the wrong master must not verify.
	mut bad_x := sx.clone()
	bad_x[0] ^= 0x01
	bad_x[31] ^= 0x80
	tampered := p256_ecdh(cpriv, bad_x, sy) or { []u8{} }
	mut wrong_master := []u8{len: 32, init: 9}
	if tampered.len == 32 && tampered != sec_c {
		wrong_master = derive_master(tampered, cr, sr)!
	}
	assert wrong_master != master
	verify_finished_msg(sfin, wrong_master, 'server finished', trans) or { return }
	assert false, 'tampered ECDHE share must fail Finished'
}

fn test_dtls12_ske_signature_tamper_fails() {
	sign_priv, px, py := p256_keygen()!
	mut peer_pub := [u8(0x04)]
	peer_pub << px
	peer_pub << py
	cr := []u8{len: 32, init: 3}
	sr := []u8{len: 32, init: 4}
	eph_priv, ex, ey := p256_keygen()!
	_ = eph_priv
	ske_body := build_server_key_exchange(cr, sr, ex, ey, sign_priv)!
	ske := parse_server_key_exchange(ske_body)!
	verify_server_key_exchange(ske, cr, sr, peer_pub)!
	// Flipped signature byte must not verify.
	mut bad_sig := ske.sig.clone()
	bad_sig[bad_sig.len - 1] ^= 0x01
	bad := Dtls12Ske{
		params:   ske.params.clone()
		eph_x:    ske.eph_x.clone()
		eph_y:    ske.eph_y.clone()
		hash_alg: ske.hash_alg
		sig_alg:  ske.sig_alg
		sig:      bad_sig
	}
	verify_server_key_exchange(bad, cr, sr, peer_pub) or { return }
	assert false, 'forged ServerKeyExchange signature must fail'
	// Signature is bound to the randoms: swapped randoms must not verify.
	verify_server_key_exchange(ske, sr, cr, peer_pub) or { return }
	assert false, 'ServerKeyExchange signature must bind the randoms'
}

fn test_dtls12_passive_observer_cannot_decrypt() {
	for suite in [u16(suite_ecdhe_rsa_aes128_gcm_sha256), u16(suite_ecdhe_rsa_aes128_cbc_sha256)] {
		mut scfg := default_config()
		scfg.cipher_suites = [suite]
		mut ccfg := default_config()
		ccfg.cipher_suites = [suite]
		addr_ch := chan string{cap: 1}
		done := chan bool{cap: 1}
		spawn dtls12_sec_echo_server(scfg, addr_ch, done)
		srv_addr := <-addr_ch
		assert srv_addr != ''
		mut cli := dial(srv_addr, ccfg) or { panic('dial: ${err}') }
		cli.handshake() or { panic('client hs: ${err}') }
		pt := 'passive observer target'.bytes()
		ct := cli.seal_app(dtls12_epoch_application, cli.app_write_seq, pt)!
		// Legitimate keys open it (self-check with the sealing keys; the
		// echo round-trip below proves the peer's keys work too).
		opened := if suite_is_gcm(suite) {
			gcm_open(cli.client_keys.key, cli.client_keys.iv, dtls12_epoch_application,
				cli.app_write_seq, u8(ContentType.application_data), ct)!
		} else {
			cbc_open(cli.client_keys.mac, cli.client_keys.key, dtls12_epoch_application,
				cli.app_write_seq, u8(ContentType.application_data), ct)!
		}
		assert opened == pt
		// Passive observer: only the cleartext randoms. The old broken
		// derivation (premaster = client_random || server_random) must fail.
		mut obs_premaster := cli.client_random.clone()
		obs_premaster << cli.server_random
		obs_master := master_secret(obs_premaster, cli.client_random, cli.server_random)
		obs_cli, _ := derive_traffic(cli.cipher_suite(), obs_master, cli.server_random,
			cli.client_random)!
		if suite_is_gcm(suite) {
			mut rejected := false
			gcm_open(obs_cli.key, obs_cli.iv, dtls12_epoch_application, cli.app_write_seq,
				u8(ContentType.application_data), ct) or { rejected = true }
			assert rejected, 'passive observer decrypted GCM app data'
		} else {
			mut rejected := false
			cbc_open(obs_cli.mac, obs_cli.key, dtls12_epoch_application, cli.app_write_seq,
				u8(ContentType.application_data), ct) or { rejected = true }
			assert rejected, 'passive observer decrypted CBC app data'
		}
		// Transport still works: echo round-trip through the peer.
		cli.write(pt) or { panic('client write: ${err}') }
		mut buf := []u8{len: 256}
		n := cli.read(mut buf) or { panic('client read: ${err}') }
		assert buf[..n] == pt
		cli.close() or {}
		assert <-done == true
		time.sleep(50 * time.millisecond)
	}
}

// dtls12_sec_parse_typ_body parses one framed handshake message, returning
// (255, empty) when the bytes are not a framed message at all.
fn dtls12_sec_parse_typ_body(b []u8) (u8, []u8) {
	typ, body := parse_hs_msg(b) or { return 255, []u8{} }
	return typ, body
}

fn test_dtls12_finished_epoch1_encrypted() {
	suite := suite_ecdhe_rsa_aes128_gcm_sha256
	cr := []u8{len: 32, init: 11}
	sr := []u8{len: 32, init: 12}
	spriv, sx, sy := p256_keygen()!
	cpriv, cx, cy := p256_keygen()!
	premaster := p256_ecdh(cpriv, sx, sy)!
	_ = spriv
	master := derive_master(premaster, cr, sr)!
	ckeys, _ := derive_traffic(suite, master, sr, cr)!
	ch := build_client_hello_msg(cr, [u16(suite)], []u8{})!
	sh := build_server_hello_msg(sr, suite)!
	ske_body := build_server_key_exchange(cr, sr, sx, sy, cpriv)!
	ske := hs_msg(hstype_server_key_exchange, ske_body)
	shd := build_server_hello_done()
	cke_body := build_client_key_exchange(cx, cy)!
	cke := hs_msg(hstype_client_key_exchange, cke_body)
	mut trans := ch.clone()
	trans << sh
	trans << ske
	trans << shd
	trans << cke
	cfin_plain := build_finished_msg(master, 'client finished', trans)!
	// Seal exactly like the handshake does: epoch 1, seq 0, handshake type.
	sealed := if suite_is_gcm(suite) {
		gcm_seal(ckeys.key, ckeys.iv, dtls12_epoch_application, 0, u8(ContentType.handshake),
			cfin_plain)!
	} else {
		cbc_seal(ckeys.mac, ckeys.key, dtls12_epoch_application, 0, u8(ContentType.handshake),
			cfin_plain)!
	}
	rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_application
		sequence_number: 0
		fragment:        sealed
	}
	wire := encode_dtls12_record(rec)!
	dec := decode_dtls12_record(wire)!
	// The Finished flight is epoch 1, not epoch-0 plaintext.
	assert dec.epoch == dtls12_epoch_application
	assert dec.epoch != dtls12_epoch_handshake
	assert dec.content_type == ContentType.handshake
	assert dec.fragment != cfin_plain
	// The sealed bytes are not a verifiable plaintext Finished.
	typ, body := dtls12_sec_parse_typ_body(dec.fragment)
	if typ == hstype_finished && body.len == 12 {
		mut rejected := false
		verify_finished_msg(dec.fragment, master, 'client finished', trans) or { rejected = true }
		assert rejected, 'sealed Finished must not verify as plaintext'
	}
	// ...and the peer recovers the plaintext Finished.
	opened := if suite_is_gcm(suite) {
		gcm_open(ckeys.key, ckeys.iv, dtls12_epoch_application, 0, u8(ContentType.handshake),
			dec.fragment)!
	} else {
		cbc_open(ckeys.mac, ckeys.key, dtls12_epoch_application, 0, u8(ContentType.handshake),
			dec.fragment)!
	}
	verify_finished_msg(opened, master, 'client finished', trans)!
}

// dtls12_sec_raw_server runs a server handshake for the plaintext-Finished
// rejection test.
fn dtls12_sec_raw_server(addr_ch chan string, done chan bool) {
	mut srv := bind('127.0.0.1:0', default_config()) or {
		addr_ch <- ''
		done <- false
		return
	}
	addr := srv.local_addr() or {
		addr_ch <- ''
		done <- false
		return
	}
	addr_ch <- addr.str()
	srv.handshake() or {
		done <- false
		return
	}
	done <- true
	srv.close() or {}
}

// dtls12_sec_send wraps one record into a datagram and writes it.
fn dtls12_sec_send(mut sock net.UdpConn, rec Dtls12Record) ! {
	wire := encode_dtls12_record(rec)!
	sock.write(wire)!
}

// dtls12_sec_recv reads one datagram and decodes it (timeout is an error).
fn dtls12_sec_recv(mut sock net.UdpConn) !Dtls12Record {
	mut buf := []u8{len: dtls12_max_datagram}
	n, _ := sock.read(mut buf)!
	return decode_dtls12_record(buf[..n])!
}

fn test_dtls12_server_rejects_plaintext_finished() {
	addr_ch := chan string{cap: 1}
	done := chan bool{cap: 1}
	spawn dtls12_sec_raw_server(addr_ch, done)
	srv_addr := <-addr_ch
	assert srv_addr != ''
	mut sock := net.dial_udp(srv_addr) or { panic('raw dial: ${err}') }
	sock.set_read_timeout(5 * time.second)
	sock.set_write_timeout(5 * time.second)
	cr := rand.bytes(32)!
	suites := default_suites.clone()
	ch := build_client_hello_msg(cr, suites, []u8{})!
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 0
		fragment:        ch
	})!
	// Collect the server flight SH(0)/SKE(1)/SHD(2) in epoch 0.
	mut sh_raw := []u8{}
	mut ske_raw := []u8{}
	mut shd_raw := []u8{}
	for sh_raw.len == 0 || ske_raw.len == 0 || shd_raw.len == 0 {
		rec := dtls12_sec_recv(mut sock)!
		if rec.content_type != ContentType.handshake || rec.epoch != dtls12_epoch_handshake {
			continue
		}
		if rec.sequence_number == 0 && sh_raw.len == 0 {
			sh_raw = rec.fragment.clone()
		} else if rec.sequence_number == 1 && ske_raw.len == 0 {
			ske_raw = rec.fragment.clone()
		} else if rec.sequence_number == 2 && shd_raw.len == 0 {
			shd_raw = rec.fragment.clone()
		}
	}
	sr, suite := parse_server_hello_msg(sh_raw)!
	ske_typ, ske_body := parse_hs_msg(ske_raw)!
	assert ske_typ == hstype_server_key_exchange
	parsed_ske := parse_server_key_exchange(ske_body)!
	shd_typ, _ := parse_hs_msg(shd_raw)!
	assert shd_typ == hstype_server_hello_done
	// ECDHE as the real client would.
	cpriv, cx, cy := p256_keygen()!
	premaster := p256_ecdh(cpriv, parsed_ske.eph_x, parsed_ske.eph_y)!
	master := derive_master(premaster, cr, sr)!
	ckeys, skeys := derive_traffic(suite, master, sr, cr)!
	cke_body := build_client_key_exchange(cx, cy)!
	cke := hs_msg(hstype_client_key_exchange, cke_body)
	mut transcript := ch.clone()
	transcript << sh_raw
	transcript << ske_raw
	transcript << shd_raw
	transcript << cke
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        cke
	})!
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .change_cipher_spec
		epoch:           dtls12_epoch_handshake
		sequence_number: 2
		fragment:        [u8(1)]
	})!
	// Attack: send a VALID Finished but in cleartext epoch 0. The server
	// must ignore it (proving Finished is not epoch-0 plaintext).
	bad_fin := build_finished_msg(master, 'client finished', transcript)!
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        bad_fin
	})!
	// The server must keep waiting: over a short window it only retransmits
	// its epoch-0 flight and never answers with CCS + epoch-1 Finished.
	// (A window, not a single datagram, so stale flight replays buffered
	// while we computed ECDHE cannot cause a false pass.)
	sock.set_read_timeout(200 * time.millisecond)
	mut saw_epoch0_flight := false
	mut saw_flight2 := false
	for _ in 0 .. 4 {
		rec := dtls12_sec_recv(mut sock) or { break }
		if rec.content_type == ContentType.change_cipher_spec && rec.fragment == [u8(1)] {
			saw_flight2 = true
		}
		if rec.content_type == ContentType.handshake && rec.epoch == dtls12_epoch_handshake
			&& rec.sequence_number <= 2 {
			saw_epoch0_flight = true
		}
		if rec.content_type == ContentType.handshake && rec.epoch == dtls12_epoch_application {
			saw_flight2 = true
		}
	}
	sock.set_read_timeout(5 * time.second)
	assert saw_epoch0_flight, 'server must retransmit its epoch-0 flight'
	assert !saw_flight2, 'server must not answer a plaintext Finished'
	// Now complete the handshake properly: resend CKE + CCS + encrypted
	// epoch-1 Finished.
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        cke
	})!
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .change_cipher_spec
		epoch:           dtls12_epoch_handshake
		sequence_number: 2
		fragment:        [u8(1)]
	})!
	cfin_plain := build_finished_msg(master, 'client finished', transcript)!
	is_gcm := suite_is_gcm(suite)
	cfin_ct := if is_gcm {
		gcm_seal(ckeys.key, ckeys.iv, dtls12_epoch_application, 0, u8(ContentType.handshake),
			cfin_plain)!
	} else {
		cbc_seal(ckeys.mac, ckeys.key, dtls12_epoch_application, 0, u8(ContentType.handshake),
			cfin_plain)!
	}
	dtls12_sec_send(mut sock, Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_application
		sequence_number: 0
		fragment:        cfin_ct
	})!
	// Expect CCS + encrypted epoch-1 server Finished; verify it.
	mut got_ccs := false
	mut got_sfin := false
	for !got_ccs || !got_sfin {
		rec := dtls12_sec_recv(mut sock)!
		if rec.content_type == ContentType.change_cipher_spec {
			if rec.fragment == [u8(1)] {
				got_ccs = true
			}
			continue
		}
		if rec.content_type != ContentType.handshake || rec.epoch != dtls12_epoch_application
			|| rec.sequence_number != 0 {
			continue
		}
		pt := if is_gcm {
			gcm_open(skeys.key, skeys.iv, rec.epoch, rec.sequence_number, u8(ContentType.handshake),
				rec.fragment)!
		} else {
			cbc_open(skeys.mac, skeys.key, rec.epoch, rec.sequence_number, u8(ContentType.handshake),
				rec.fragment)!
		}
		mut full := transcript.clone()
		full << cfin_plain
		verify_finished_msg(pt, master, 'server finished', full)!
		got_sfin = true
	}
	assert got_ccs && got_sfin
	assert <-done == true
	sock.close() or {}
	time.sleep(50 * time.millisecond)
}

fn test_dtls12_distinct_peers_distinct_secrets() {
	mut masters := [][]u8{}
	for _ in 0 .. 2 {
		addr_ch := chan string{cap: 1}
		done := chan bool{cap: 1}
		spawn dtls12_sec_echo_server(default_config(), addr_ch, done)
		srv_addr := <-addr_ch
		assert srv_addr != ''
		mut cli := dial(srv_addr, default_config()) or { panic('dial: ${err}') }
		cli.handshake() or { panic('client hs: ${err}') }
		masters << cli.master.clone()
		msg := 'distinct secrets'.bytes()
		cli.write(msg) or { panic('client write: ${err}') }
		mut buf := []u8{len: 256}
		n := cli.read(mut buf) or { panic('client read: ${err}') }
		assert buf[..n] == msg
		cli.close() or {}
		assert <-done == true
		time.sleep(50 * time.millisecond)
	}
	assert masters.len == 2
	assert masters[0].len == 48 && masters[1].len == 48
	assert masters[0] != masters[1]
}
