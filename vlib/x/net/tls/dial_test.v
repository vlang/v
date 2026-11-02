// Copyright (c) 2026 blackshirt.
//
// dial_test.v runs the TLS client through dial() over real TCP sockets against
// scripted in-process peers: the full-handshake twin of the fakeserver (record
// layer and all), plus plaintext negatives that exercise the record-layer gates
// end to end.
module tls

import net
import time
import x.net.tls.fakeserver
import x.net.tls.handshake
import x.net.tls.wire

// scripted_read_full fills buf[start..] from the socket until full, so record
// headers and bodies split by TCP are reassembled like Conn.read_raw_record does.
fn scripted_read_full(mut c net.TcpConn, mut buf []u8, start int) ! {
	mut pos := start
	for pos < buf.len {
		n := c.read(mut buf[pos..]) or { return err }
		if n <= 0 {
			return error('tls test: unexpected EOF (read ${pos}/${buf.len} bytes)')
		}
		pos += n
	}
}

// scripted_read_record reads exactly one TLS record off the socket using the
// production parser, so scripts see the same Record the client would.
fn scripted_read_record(mut c net.TcpConn) !Record {
	mut hdr := []u8{len: record_header_len}
	scripted_read_full(mut c, mut hdr, 0)!
	plen := int(hdr[3]) << 8 | int(hdr[4])
	if plen > max_ciphertext_record_len {
		return error('tls test: record length ${plen} exceeds maximum')
	}
	mut full := []u8{len: record_header_len + plen}
	copy(mut full[..record_header_len], hdr)
	scripted_read_full(mut c, mut full, record_header_len)!
	mut r := wire.new_reader(full)
	return read_record(mut r)!
}

// start_scripted_server binds an ephemeral loopback listener, returns its
// address, and runs script against the first connection on a background thread.
fn start_scripted_server(script fn (mut c net.TcpConn) !) (string, chan string) {
	addr_ch := chan string{cap: 1}
	res_ch := chan string{cap: 1}
	spawn scripted_server_worker(script, addr_ch, res_ch)
	addr := <-addr_ch
	return addr, res_ch
}

// scripted_server_worker is the listener thread body: accept once, run the
// script, report the outcome on both channels so the test side never blocks.
fn scripted_server_worker(script fn (mut c net.TcpConn) !, addr_ch chan string, res_ch chan string) {
	mut l := net.listen_tcp(.ip, '127.0.0.1:0', net.ListenOptions{}) or {
		msg := 'ERR listen: ${err.msg()}'
		addr_ch <- msg
		res_ch <- msg
		return
	}
	a := l.addr() or {
		msg := 'ERR addr: ${err.msg()}'
		addr_ch <- msg
		res_ch <- msg
		return
	}
	port := a.port() or {
		msg := 'ERR port: ${err.msg()}'
		addr_ch <- msg
		res_ch <- msg
		return
	}
	addr_ch <- '127.0.0.1:${port}'
	mut c := l.accept() or {
		res_ch <- 'ERR accept: ${err.msg()}'
		l.close() or {}
		return
	}
	script(mut c) or {
		res_ch <- 'ERR script: ${err.msg()}'
		c.close() or {}
		l.close() or {}
		return
	}
	c.close() or {}
	l.close() or {}
	res_ch <- 'OK'
}

// expect_dial_failure dials addr expecting an error whose message contains
// want, waits for the scripted peer to finish, and reports a clean outcome.
fn expect_dial_failure(addr string, want string, res_ch chan string) ! {
	ts := fakeserver.test_trust_store() or { return error('fakeserver trust store: ${err.msg()}') }
	dial(addr, server_name: fakeserver.server_name, root_cas: ts, timeout: 5 * time.second) or {
		if !err.msg().contains(want) {
			return error('dial failed with "${err.msg()}", expected it to contain "${want}"')
		}
		res := <-res_ch
		if res != 'OK' {
			return error('scripted server did not finish cleanly: ${res}')
		}
		return
	}
	return error('dial unexpectedly succeeded; wanted a failure containing "${want}"')
}

// ---- Full handshake over TCP ----
// full_handshake_script is the record-layer twin of the fakeserver flight.
fn full_handshake_script(mut c net.TcpConn) ! {
	run_full_handshake(mut c, false, false, []u8{})!
}

// chacha_handshake_script is the full flight under TLS_CHACHA20_POLY1305_SHA256 —
// the software-preferred suite every AES-only test never reached, so it drives the
// record layer's install/rekey §7.5 key-ownership path end to end.
fn chacha_handshake_script(mut c net.TcpConn) ! {
	run_flight(mut c, handshake.CipherSuite.tls_chacha20_poly1305_sha256, false, false, []u8{})!
}

// split_ticket_script is run_full_handshake with the NewSessionTicket fragmented.
fn split_ticket_script(mut c net.TcpConn) ! {
	run_full_handshake(mut c, true, false, []u8{})!
}

// coalesced_ticket_script is run_full_handshake with the NewSessionTicket riding
// the same handshake-key record as the server Finished (§4.6.1).
fn coalesced_ticket_script(mut c net.TcpConn) ! {
	run_full_handshake(mut c, false, true, []u8{})!
}

// key_update_pair_script is the standard flight plus two consecutive KeyUpdate
// messages coalesced into one application-key record (§4.5 forbids the run).
fn key_update_pair_script(mut c net.TcpConn) ! {
	mut ku_pair := [u8(24), 0, 0, 1, 0] // KeyUpdate(update_not_requested), 1-byte body
	ku_pair << [u8(24), 0, 0, 1, 0] // second in a row: the violation
	run_full_handshake(mut c, false, false, ku_pair)!
}

// silent_peer_script accepts and immediately closes: the client must fail on its
// own oversized ClientHello before any byte reaches the peer.
fn silent_peer_script(mut c net.TcpConn) ! {
	c.close() or {}
}

// run_full_handshake drives the whole server flight under TLS_AES_128_GCM_SHA256,
// the AEAD the majority of these tests assert; it delegates to run_flight, which
// also lets a caller select TLS_CHACHA20_POLY1305_SHA256.
fn run_full_handshake(mut c net.TcpConn, split_ticket bool, coalesce_ticket bool, extra_post_msg []u8) ! {
	run_flight(mut c, handshake.CipherSuite.tls_aes_128_gcm_sha256, split_ticket, coalesce_ticket, extra_post_msg)!
}

// run_flight drives the whole server flight under the given cipher suite; split_ticket
// fragments the ticket across two app-key records, coalesce_ticket instead rides it
// behind the server Finished in one handshake-key record (§4.6.1), and extra_post_msg
// (when non-empty) is appended as one coalesced handshake record under the server
// application keys, after the app data. The suite selects both the fakeserver's
// ServerHello choice and the transport's AEAD, so a TLS_CHACHA20_POLY1305_SHA256 run
// exercises the record layer's §7.5 key-ownership path end to end.
fn run_flight(mut c net.TcpConn, suite handshake.CipherSuite, split_ticket bool, coalesce_ticket bool, extra_post_msg []u8) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	ccs_rec := scripted_read_record(mut c)!
	if ccs_rec.content_type != .change_cipher_spec {
		return error('expected the client dummy CCS second, got ${ccs_rec.content_type}')
	}

	mut srv := fakeserver.new_server(alpn: ['h2', 'http/1.1'], cipher_suite: u16(suite)) or {
		return error('fakeserver: ${err.msg()}')
	}
	fl := srv.handle_client_hello(ch_rec.payload) or {
		return error('fakeserver CH: ${err.msg()}')
	}
	if fl.is_hrr {
		return error('unexpected HelloRetryRequest in the default configuration')
	}

	// Plaintext ServerHello plus the server's dummy CCS (App. D.4).
	sh_rec := new_record(.handshake, fl.server_hello)
	c.write(sh_rec.serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	// The rest of the flight, one encrypted record per message, under the server handshake keys.
	skey, siv := srv.server_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aead := handshake.new_aead_for_suite(suite, skey) or { return err }
	mut senc := new_tls_protection(aead, siv)!
	nst := srv.new_session_ticket() or { return error('fakeserver NST: ${err.msg()}') }
	for i, m in fl.rest {
		mut payload := m.clone() // loop elements are immutable; append needs an owned copy
		if coalesce_ticket && i == fl.rest.len - 1 {
			payload << nst // the ticket trails Finished in the same record
		}
		enc := senc.encrypt_record(.handshake, payload)!
		c.write(enc.serialize()) or { return err }
	}

	// The client's Finished arrives encrypted under the client handshake keys.
	cfin_rec := scripted_read_record(mut c)!
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(suite, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	dec := cdec.decrypt_record(cfin_rec)!
	if dec.content_type != .handshake {
		return error('client Finished has inner type ${dec.content_type}, expected handshake')
	}
	srv.handle_client_finished(dec.plaintext) or { return error('fakeserver Finished: ${err.msg()}') }

	// Post-handshake: a NewSessionTicket and one application record under the server application keys.
	akey, aiv := srv.server_app_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aaead := handshake.new_aead_for_suite(suite, akey) or { return err }
	mut aenc := new_tls_protection(aaead, aiv)!
	if !coalesce_ticket {
		if split_ticket {
			// Split the ticket across two records: RFC 8446 §5.2 permits a message to span records.
			cut := 8
			c.write(aenc.encrypt_record(.handshake, nst[..cut])!.serialize()) or { return err }
			c.write(aenc.encrypt_record(.handshake, nst[cut..])!.serialize()) or { return err }
		} else {
			c.write(aenc.encrypt_record(.handshake, nst)!.serialize()) or { return err }
		}
	}
	payload := 'hello from fakeserver'.bytes()
	c.write(aenc.encrypt_record(.application_data, payload)!.serialize()) or { return err }
	if extra_post_msg.len > 0 {
		c.write(aenc.encrypt_record(.handshake, extra_post_msg)!.serialize()) or { return err }
	}
}

// test_dial_completes_full_handshake_and_reads_app_data checks a full handshake
// negotiates ALPN, delivers app data, and surfaces the post-handshake ticket.
fn test_dial_completes_full_handshake_and_reads_app_data() ! {
	addr, res_ch := start_scripted_server(full_handshake_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		hsk_options: handshake.HandshakeOptions{
			alpn: ['h2']
		}
		root_cas:    ts
		timeout:     10 * time.second
	)!

	assert conn.negotiated_alpn() == 'h2'

	// read() consumes the NewSessionTicket as post-handshake traffic, then the app bytes.
	mut buf := []u8{len: 64}
	n := conn.read(mut buf)!
	assert buf[..n] == 'hello from fakeserver'.bytes()

	assert conn.session_tickets.len == 1
	assert conn.session_tickets[0].lifetime() == 3600
	assert conn.session_tickets[0].bytes() == [u8(0xaa), 0xbb, 0xcc, 0xdd]

	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// test_dial_chacha20_end_to_end negotiates TLS_CHACHA20_POLY1305_SHA256 alone and
// completes the full handshake plus one application read over it. This is the
// regression guard for the §7.5 key-ownership bug: new_tls_protection_from_keys scrubs the
// key bytes it takes from the core's generation
// only after building the ChaCha cipher, which used to zero the AEAD's aliased
// key and fail every record with an 'unmatching tag' that no AES suite exhibits and
// that AES-only tests could never surface. Offering ChaCha exclusively means merely
// reaching connected proves the flight decrypted under ChaCha handshake keys.
fn test_dial_chacha20_end_to_end() ! {
	addr, res_ch := start_scripted_server(chacha_handshake_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		hsk_options: handshake.HandshakeOptions{
			cipher_suites: [handshake.CipherSuite.tls_chacha20_poly1305_sha256]
		}
		root_cas:    ts
		timeout:     10 * time.second
	)!

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)! // drains the ticket, then the ChaCha app record
	assert buf[..n] == 'hello from fakeserver'.bytes()

	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// test_dial_reassembles_post_handshake_message_across_records checks a
// NewSessionTicket fragmented over two records is reassembled (RFC 8446 §5.2).
fn test_dial_reassembles_post_handshake_message_across_records() ! {
	addr, res_ch := start_scripted_server(split_ticket_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		hsk_options: handshake.HandshakeOptions{
			alpn: ['h2']
		}
		root_cas:    ts
		timeout:     10 * time.second
	)!

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)!
	assert buf[..n] == 'hello from fakeserver'.bytes()
	assert conn.session_tickets.len == 1
	assert conn.session_tickets[0].bytes() == [u8(0xaa), 0xbb, 0xcc, 0xdd]

	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// test_dial_accepts_ticket_coalesced_after_finished proves the §4.6.1 route: a
// NewSessionTicket riding the server-Finished record must reach the ticket
// backlog during dial, not strand unread in the core's handshake buffer.
fn test_dial_accepts_ticket_coalesced_after_finished() ! {
	addr, res_ch := start_scripted_server(coalesced_ticket_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!

	assert conn.session_tickets.len == 1 // handled inside do_handshake itself
	assert conn.session_tickets[0].bytes() == [u8(0xaa), 0xbb, 0xcc, 0xdd]
	assert conn.hs.reassembly_len()! == 0 // drained, not orphaned

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)!
	assert buf[..n] == 'hello from fakeserver'.bytes()

	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// test_dial_rejects_consecutive_key_update pins RFC 8446 §4.5 end to end: after
// the app record, one application-key record packs two KeyUpdate messages. The
// first must rekey cleanly; the second must abort at the run guard, before its
// rekey derivation is paid for.
fn test_dial_rejects_consecutive_key_update() ! {
	addr, res_ch := start_scripted_server(key_update_pair_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)! // the ticket and the app record precede the KU pair
	assert buf[..n] == 'hello from fakeserver'.bytes()

	conn.read(mut buf) or {
		assert err.msg().contains('consecutive KeyUpdate')
		assert conn.closed
		conn.close() or {}
		res := <-res_ch
		assert res == 'OK'
		return
	}
	assert false, 'two KeyUpdate messages in a row must be terminal'
}

// key_update_rekey_script is the standard flight, then one peer-initiated
// KeyUpdate(update_not_requested) followed by a tail record under the NEXT
// server application key generation (§4.6.3/§7.2): the tail must open only
// after the client has rekeyed its read leg.
fn key_update_rekey_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)! // client dummy CCS

	mut srv := fakeserver.new_server() or {
		return error('fakeserver: ${err.msg()}')
	}
	fl := srv.handle_client_hello(ch_rec.payload) or {
		return error('fakeserver CH: ${err.msg()}')
	}
	c.write(new_record(.handshake, fl.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	skey, siv := srv.server_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	saead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, skey) or { return err }
	mut senc := new_tls_protection(saead, siv)!
	nst := srv.new_session_ticket() or { return error('fakeserver NST: ${err.msg()}') }
	for m in fl.rest {
		c.write(senc.encrypt_record(.handshake, m)!.serialize()) or { return err }
	}

	cfin_rec := scripted_read_record(mut c)!
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	dec := cdec.decrypt_record(cfin_rec)!
	srv.handle_client_finished(dec.plaintext) or { return error('fakeserver Finished: ${err.msg()}') }

	akey, aiv := srv.server_app_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aaead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, akey) or { return err }
	mut aenc := new_tls_protection(aaead, aiv)!
	c.write(aenc.encrypt_record(.handshake, nst)!.serialize()) or { return err }
	c.write(aenc.encrypt_record(.application_data, 'hello from fakeserver'.bytes())!.serialize()) or {
		return err
	}

	// §7.2 KeyUpdate chain: traffic upd(secret), then the "key"/"iv" labels, the
	// same derivations the client's core performs when it takes the update.
	c.write(aenc.encrypt_record(.handshake, handshake.build_key_update(false))!.serialize()) or {
		return err
	}
	hash := handshake.suite_hash_algo(.tls_aes_128_gcm_sha256)
	next_secret := handshake.hkdf_expand_label(hash, srv.server_app_secret()!, 'traffic upd', []u8{}, hash.hash_len())!
	next_key := handshake.hkdf_expand_label(hash, next_secret, 'key', []u8{}, handshake.suite_key_len(.tls_aes_128_gcm_sha256))!
	next_iv := handshake.hkdf_expand_label(hash, next_secret, 'iv', []u8{}, 12)!
	mut next_enc := new_tls_protection(handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, next_key)!, next_iv)!
	c.write(next_enc.encrypt_record(.application_data, 'rekeyed tail'.bytes())!.serialize()) or {
		return err
	}
}

// test_dial_key_update_rekeys_and_wipes_superseded_protection proves the §7.5
// wipe of the replaced generation end to end: after a peer KeyUpdate, the read
// protection state's old nonce base must be zeroed in place (an alias taken before the
// rekey reads back all-zero), and the tail record must open only under the
// next key generation.
fn test_dial_key_update_rekeys_and_wipes_superseded_protection() ! {
	addr, res_ch := start_scripted_server(key_update_rekey_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)! // drains the ticket, then the app record
	assert buf[..n] == 'hello from fakeserver'.bytes()

	// Alias the live read protection state's nonce base before the rekey: V arrays share one
	// underlying buffer, so `unsafe` here (the compiler's sanctioned opt-out from the
	// `.clone()` guard) keeps a genuine alias rather than a copy. swap_application_leg's
	// wipe then zeroes this same buffer in place, which the alias observes; a `.clone()`
	// would instead snapshot the pre-rekey nonzero bytes and defeat the assertion below.
	mut old_iv := unsafe { conn.rec.s_app_protection.iv }
	assert old_iv.len == 12 && old_iv[0] != 0

	mut tail := []u8{len: 32}
	m := conn.read(mut tail)! // this read takes the KeyUpdate and rekeys
	assert tail[..m] == 'rekeyed tail'.bytes()

	mut nonzero := false
	for b in old_iv {
		if b != 0 {
			nonzero = true
		}
	}
	assert !nonzero, 'the superseded read protection state must zero its nonce base (§7.5)'
	assert conn.rec.s_app_protection.iv.len == 12 // the new generation is live

	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// test_dial_rejects_oversized_client_hello checks the §5.1 single-record rule
// locally: an ALPN flood the option validator accepts (each name ≤255, total
// within the extension cap) still cannot exceed one plaintext record, and the
// adapter must abort instead of framing a non-compliant ClientHello.
fn test_dial_rejects_oversized_client_hello() ! {
	addr, res_ch := start_scripted_server(silent_peer_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	mut alpn := []string{}
	for _ in 0 .. 80 {
		alpn << 'a'.repeat(250) // 80 × 251 framed bytes: past 2^14 once the CH is built
	}
	ts := fakeserver.test_trust_store()!
	dial(addr,
		server_name: fakeserver.server_name
		hsk_options: handshake.HandshakeOptions{
			alpn: alpn
		}
		root_cas:    ts
		timeout:     5 * time.second
	) or {
		assert err.msg().contains('ClientHello exceeds one record')
		assert <-res_ch == 'OK'
		return
	}
	assert false, 'an oversized ClientHello must abort before framing'
}

// ---- Plaintext / malformed-record negatives ----

// alert_before_sh_script rejects the ClientHello with a plaintext fatal alert,
// the only kind of alert legal before the ServerHello (RFC 8446 §6).
fn alert_before_sh_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)! // client dummy CCS
	// fatal (2) handshake_failure (40).
	c.write(new_record(.alert, [u8(2), 40]).serialize()) or { return err }
}

// test_dial_rejects_plaintext_alert_before_server_hello checks that a
// plaintext alert before the ServerHello aborts the dial.
fn test_dial_rejects_plaintext_alert_before_server_hello() ! {
	addr, res_ch := start_scripted_server(alert_before_sh_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	expect_dial_failure(addr, 'received alert', res_ch)!
}

// sh_trailing_junk_script answers the ClientHello with a ServerHello record
// carrying one extra trailing byte, violating the RFC 8446 §5.1 boundary rule.
fn sh_trailing_junk_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!
	mut srv := fakeserver.new_server() or { return error('fakeserver: ${err.msg()}') }
	fl := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH: ${err.msg()}') }
	mut payload := fl.server_hello.clone()
	payload << 0x00 // the forbidden trailing byte
	c.write(new_record(.handshake, payload).serialize()) or { return err }
}

// test_dial_rejects_trailing_bytes_after_server_hello checks that bytes past
// the ServerHello abort the dial.
fn test_dial_rejects_trailing_bytes_after_server_hello() ! {
	addr, res_ch := start_scripted_server(sh_trailing_junk_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	expect_dial_failure(addr, 'misframed ServerHello', res_ch)!
}

// ---- Slow-loris and malformed-name negatives ----

// slow_loris_script answers the ClientHello, then dribbles an incomplete
// handshake record in small chunks every 300ms: each socket read completes well
// under the per-read timeout, so only the absolute handshake deadline can end it.
fn slow_loris_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!
	mut srv := fakeserver.new_server() or { return error('fakeserver: ${err.msg()}') }
	fl := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH: ${err.msg()}') }
	c.write(new_record(.handshake, fl.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }
	// A declared 1000-byte record dribbled 8 bytes at a time, never completed.
	c.write([u8(0x16), 0x03, 0x03, 0x03, 0xe8]) or { return err }
	mut sent := 0
	for sent < 400 {
		time.sleep(300 * time.millisecond)
		// Best-effort: the client aborts at its handshake deadline, and the
		// closing RST makes later writes fail — the dribble merely keeps the
		// per-read timeout from firing first.
		c.write([u8(0), 0, 0, 0, 0, 0, 0, 0]) or { break }
		sent += 8
	}
}

// test_dial_handshake_deadline_ends_slow_loris checks that a peer dribbling valid
// bytes under every per-read timeout is cut off by the absolute handshake deadline.
fn test_dial_handshake_deadline_ends_slow_loris() ! {
	addr, res_ch := start_scripted_server(slow_loris_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store() or { return error('fakeserver trust store: ${err.msg()}') }
	start := time.now()
	dial(addr, server_name: fakeserver.server_name, root_cas: ts, timeout: 1 * time.second) or {
		if !err.msg().contains('handshake deadline exceeded') {
			return error('dial failed with "${err.msg()}", expected the handshake deadline')
		}
		// The deadline is 4x the 1s timeout; the dial must end near it, not hang.
		// The lower bound catches a fast clock scale (wrong ns divisor) just as
		// the upper bound catches a slow one — the 16.8s miss this pins against.
		elapsed := time.now() - start
		assert 3 * time.second <= elapsed
		assert elapsed < 10 * time.second
		res := <-res_ch
		if res != 'OK' {
			return error('scripted server did not finish cleanly: ${res}')
		}
		return
	}
	return error('dial unexpectedly succeeded; wanted the handshake deadline failure')
}

// test_dial_rejects_server_name_with_stray_whitespace pins the pre-socket name
// gate: a padded name must fail before a TCP connection is opened, so it can
// never travel verbatim into SNI or hostname verification.
fn test_dial_rejects_server_name_with_stray_whitespace() ! {
	dial('127.0.0.1:1', server_name: 'example.com ') or {
		assert err.msg().contains('stray whitespace')
		return
	}
	return error('dial unexpectedly accepted a padded server name')
}

// test_dial_rejects_overlong_server_name pins the DNS-ceiling bound on the
// verification name before any ClientHello encoding is attempted.
fn test_dial_rejects_overlong_server_name() ! {
	host := 'a'.repeat(max_server_name_len + 1) + '.example.com'
	dial('127.0.0.1:1', server_name: host) or {
		assert err.msg().contains('exceeds 253 characters')
		return
	}
	return error('dial unexpectedly accepted an overlong server name')
}

// excess_ccs_script floods the client with three dummy CCS records; RFC 8446
// App. D.4 allows one per ClientHello, so the third must abort the handshake.
fn excess_ccs_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)! // client dummy CCS
	for _ in 0 .. 3 {
		c.write(change_cipher_spec().serialize()) or { return err }
	}
}

// test_dial_rejects_third_ccs_record checks that a third dummy CCS record
// aborts the handshake.
fn test_dial_rejects_third_ccs_record() ! {
	addr, res_ch := start_scripted_server(excess_ccs_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	expect_dial_failure(addr, 'too many change_cipher_spec', res_ch)!
}

// garbage_type_script writes a well-framed record whose content type is not a
// TLS content type at all.
fn garbage_type_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	// [type=99] [version 0x0303] [length 0]
	c.write([u8(99), 0x03, 0x03, 0x00, 0x00]) or { return err }
}

// test_dial_rejects_invalid_record_content_type checks that a record with an
// undefined content type is refused.
fn test_dial_rejects_invalid_record_content_type() ! {
	addr, res_ch := start_scripted_server(garbage_type_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	expect_dial_failure(addr, 'invalid content type', res_ch)!
}

// ---- HelloRetryRequest over TCP ----

// hrr_script performs the change_group HelloRetryRequest round trip at the
// record layer: HRR+CCS, retry ClientHello+CCS, then the normal flight.
fn hrr_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!

	mut srv := fakeserver.new_server(hrr: .change_group) or { return error('fakeserver: ${err.msg()}') }
	fl1 := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH1: ${err.msg()}') }
	if !fl1.is_hrr {
		return error('expected a HelloRetryRequest')
	}
	c.write(new_record(.handshake, fl1.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	// The client's retry ClientHello + a fresh dummy CCS (App. D.4).
	ch2_rec := scripted_read_record(mut c)!
	if ch2_rec.content_type != .handshake {
		return error('expected the retry ClientHello as a handshake record')
	}
	_ := scripted_read_record(mut c)!

	fl2 := srv.handle_client_hello(ch2_rec.payload) or { return error('fakeserver CH2: ${err.msg()}') }
	if fl2.is_hrr {
		return error('unexpected second HelloRetryRequest')
	}
	c.write(new_record(.handshake, fl2.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	skey, siv := srv.server_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, skey) or { return err }
	mut senc := new_tls_protection(aead, siv)!
	for m in fl2.rest {
		enc := senc.encrypt_record(.handshake, m)!
		c.write(enc.serialize()) or { return err }
	}

	cfin_rec := scripted_read_record(mut c)!
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	dec := cdec.decrypt_record(cfin_rec)!
	srv.handle_client_finished(dec.plaintext) or { return error('fakeserver Finished: ${err.msg()}') }
}

// test_dial_completes_hello_retry_request_round_trip checks that an HRR round
// trip completes and an empty ALPN offer yields an empty pick.
fn test_dial_completes_hello_retry_request_round_trip() ! {
	addr, res_ch := start_scripted_server(hrr_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!
	assert conn.negotiated_alpn() == ''
	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// ---- Failure alerts over TCP ----
// alert_back_script drives the client to fail on a bad CertificateVerify and
// checks its own fatal alert: encrypted, and carrying the expected code.
fn alert_back_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!

	mut srv := fakeserver.new_server(bad_certificate_verify: true) or {
		return error('fakeserver: ${err.msg()}')
	}
	fl := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH: ${err.msg()}') }
	c.write(new_record(.handshake, fl.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	skey, siv := srv.server_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, skey) or { return err }
	mut senc := new_tls_protection(aead, siv)!
	for m in fl.rest {
		c.write(senc.encrypt_record(.handshake, m)!.serialize()) or { return err }
	}

	// The client answers with a fatal alert encrypted under its client handshake keys.
	alert_rec := scripted_read_record(mut c)!
	if alert_rec.content_type != .application_data {
		return error('client alert has outer type ${alert_rec.content_type}, expected application_data (encrypted)')
	}
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	dec := cdec.decrypt_record(alert_rec)!
	if dec.content_type != .alert {
		return error('client sent inner type ${dec.content_type}, expected alert')
	}
	peer_alert := handshake.parse_alert(dec.plaintext)!
	if peer_alert.level != .fatal {
		return error('client alert level is ${peer_alert.level}, expected fatal')
	}
	if peer_alert.description != .decrypt_error {
		return error('client alert is ${peer_alert.description}, expected decrypt_error')
	}
}

// test_dial_sends_fatal_alert_on_certificate_verify_failure checks that the
// client answers a bad CertificateVerify with a fatal alert.
fn test_dial_sends_fatal_alert_on_certificate_verify_failure() ! {
	addr, res_ch := start_scripted_server(alert_back_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	expect_dial_failure(addr, 'CertificateVerify failed', res_ch)!
}

// bad_record_mac_script drives the client to fail AEAD decryption of a bogus record
// sent under the handshake keys, and checks it answers with a fatal bad_record_mac
// alert — the handshake phase must classify a non-opening record exactly like the
// read() path does, not fall back to the stale .internal_error default.
fn bad_record_mac_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!

	mut srv := fakeserver.new_server() or {
		return error('fakeserver: ${err.msg()}')
	}
	fl := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH: ${err.msg()}') }
	// A valid ServerHello so the client derives and installs its handshake keys.
	c.write(new_record(.handshake, fl.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }
	// Then an outer application_data record that cannot AEAD-open under those keys.
	c.write(new_record(.application_data, []u8{len: 28, init: 0xa5}).serialize()) or { return err }

	// The client answers with a fatal alert encrypted under its client handshake keys.
	alert_rec := scripted_read_record(mut c)!
	if alert_rec.content_type != .application_data {
		return error('client alert has outer type ${alert_rec.content_type}, expected application_data (encrypted)')
	}
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	dec := cdec.decrypt_record(alert_rec)!
	if dec.content_type != .alert {
		return error('client sent inner type ${dec.content_type}, expected alert')
	}
	peer_alert := handshake.parse_alert(dec.plaintext)!
	if peer_alert.level != .fatal {
		return error('client alert level is ${peer_alert.level}, expected fatal')
	}
	if peer_alert.description != .bad_record_mac {
		return error('client alert is ${peer_alert.description}, expected bad_record_mac')
	}
}

// test_dial_sends_bad_record_mac_on_undecryptable_flight checks that a record that
// will not open under the handshake keys yields a fatal bad_record_mac alert.
fn test_dial_sends_bad_record_mac_on_undecryptable_flight() ! {
	addr, res_ch := start_scripted_server(bad_record_mac_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	expect_dial_failure(addr, 'AEAD decryption failed', res_ch)!
}

// alert_back_pre_sh_script answers the ClientHello with a malformed ServerHello
// body that fails before any keys are derived, then reads the plaintext alert.
fn alert_back_pre_sh_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!
	// A complete ServerHello header with an empty body: the client fails in parse,
	// before installing keys.
	c.write(new_record(.handshake, [u8(0x02), 0x00, 0x00, 0x00]).serialize()) or { return err }

	alert_rec := scripted_read_record(mut c)!
	if alert_rec.content_type != .alert {
		return error('client pre-SH alert has type ${alert_rec.content_type}, expected plaintext alert')
	}
	peer_alert := handshake.parse_alert(alert_rec.payload)!
	if peer_alert.level != .fatal {
		return error('client alert level is ${peer_alert.level}, expected fatal')
	}
}

// test_dial_sends_plaintext_fatal_alert_before_server_hello checks that a
// malformed ServerHello yields a plaintext fatal alert.
fn test_dial_sends_plaintext_fatal_alert_before_server_hello() ! {
	addr, res_ch := start_scripted_server(alert_back_pre_sh_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	// A malformed ServerHello fails before classification: the surfaced message is the parse error.
	expect_dial_failure(addr, 'unexpected EOF', res_ch)!
}

// ---- Client authentication (CertificateRequest) over TCP ----

// client_auth_script sends a CertificateRequest, reads the client's encrypted
// empty Certificate, then streams the deferred Cert/CV/Fin tail.
fn client_auth_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)!

	mut srv := fakeserver.new_server(client_auth: true) or {
		return error('fakeserver: ${err.msg()}')
	}
	fl := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH: ${err.msg()}') }
	c.write(new_record(.handshake, fl.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	skey, siv := srv.server_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, skey) or { return err }
	mut senc := new_tls_protection(aead, siv)!
	// fl.rest is [EncryptedExtensions, CertificateRequest].
	for m in fl.rest {
		c.write(senc.encrypt_record(.handshake, m)!.serialize()) or { return err }
	}

	// The client answers with its empty Certificate under the client handshake keys.
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	ccert_rec := scripted_read_record(mut c)!
	ccert := cdec.decrypt_record(ccert_rec)!
	if ccert.content_type != .handshake {
		return error('client Certificate inner type ${ccert.content_type}, expected handshake')
	}
	tail := srv.handle_client_certificate(ccert.plaintext) or {
		return error('fakeserver client Certificate: ${err.msg()}')
	}

	// The deferred tail continues under the same server handshake key generation.
	for m in tail {
		c.write(senc.encrypt_record(.handshake, m)!.serialize()) or { return err }
	}

	cfin_rec := scripted_read_record(mut c)!
	cfin := cdec.decrypt_record(cfin_rec)!
	if cfin.content_type != .handshake {
		return error('client Finished inner type ${cfin.content_type}, expected handshake')
	}
	srv.handle_client_finished(cfin.plaintext) or { return error('fakeserver Finished: ${err.msg()}') }
}

// test_dial_answers_certificate_request_with_empty_certificate checks that a
// CertificateRequest is answered with an empty Certificate.
fn test_dial_answers_certificate_request_with_empty_certificate() ! {
	addr, res_ch := start_scripted_server(client_auth_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!
	assert conn.negotiated_alpn() == ''
	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// ---- Truncation and clean-shutdown defenses (RFC 8446 §6.1) ----

// close_notify_script is the standard flight, then one application record followed by an
// encrypted close_notify under the server application keys — the clean-shutdown signal a
// well-behaved peer sends before closing (RFC 8446 §6.1).
fn close_notify_script(mut c net.TcpConn) ! {
	ch_rec := scripted_read_record(mut c)!
	if ch_rec.content_type != .handshake {
		return error('expected a handshake record first, got ${ch_rec.content_type}')
	}
	_ := scripted_read_record(mut c)! // client dummy CCS

	mut srv := fakeserver.new_server() or {
		return error('fakeserver: ${err.msg()}')
	}
	fl := srv.handle_client_hello(ch_rec.payload) or { return error('fakeserver CH: ${err.msg()}') }
	c.write(new_record(.handshake, fl.server_hello).serialize()) or { return err }
	c.write(change_cipher_spec().serialize()) or { return err }

	skey, siv := srv.server_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	saead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, skey) or { return err }
	mut senc := new_tls_protection(saead, siv)!
	nst := srv.new_session_ticket() or { return error('fakeserver NST: ${err.msg()}') }
	for m in fl.rest {
		c.write(senc.encrypt_record(.handshake, m)!.serialize()) or { return err }
	}

	cfin_rec := scripted_read_record(mut c)!
	ckey, civ := srv.client_handshake_keys() or { return error('fakeserver keys: ${err.msg()}') }
	caead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, ckey) or { return err }
	mut cdec := new_tls_protection(caead, civ)!
	dec := cdec.decrypt_record(cfin_rec)!
	srv.handle_client_finished(dec.plaintext) or { return error('fakeserver Finished: ${err.msg()}') }

	akey, aiv := srv.server_app_keys() or { return error('fakeserver keys: ${err.msg()}') }
	aaead := handshake.new_aead_for_suite(.tls_aes_128_gcm_sha256, akey) or { return err }
	mut aenc := new_tls_protection(aaead, aiv)!
	c.write(aenc.encrypt_record(.handshake, nst)!.serialize()) or { return err }
	c.write(aenc.encrypt_record(.application_data, 'hello from fakeserver'.bytes())!.serialize()) or {
		return err
	}
	// Encrypted close_notify under the server application keys, inner type alert (§6.1).
	notify := handshake.new_alert(.close_notify)
	c.write(aenc.encrypt_record(.alert, notify.encode())!.serialize()) or { return err }
}

// test_dial_clean_close_notify_returns_zero proves the §6.1 clean-shutdown path end to end:
// after delivering app data, an encrypted close_notify makes read() return 0 — and it stays 0
// on the next read, never flipping to an error, so a graceful peer is distinguishable from a
// truncating one.
fn test_dial_clean_close_notify_returns_zero() ! {
	addr, res_ch := start_scripted_server(close_notify_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)! // drains the ticket, then the app record
	assert buf[..n] == 'hello from fakeserver'.bytes()

	// The encrypted close_notify surfaces as a clean EOF (0).
	mut b2 := []u8{len: 64}
	assert conn.read(mut b2)! == 0
	// It stays 0 (a sticky clean close), never an error (§6.1).
	assert conn.read(mut b2)! == 0

	conn.close() or {}
	res := <-res_ch
	assert res == 'OK'
}

// test_dial_truncation_without_close_notify_is_sticky proves the RFC 8446 §6.1 truncation
// defense end to end: a peer that delivers one application record and then closes the TCP
// connection WITHOUT a close_notify must leave the client with a sticky error, re-reported on
// every later read — never a silent clean EOF (0).
fn test_dial_truncation_without_close_notify_is_sticky() ! {
	// full_handshake_script delivers app data and then the worker closes the socket with no
	// close_notify: exactly the abrupt-FIN truncation vector §6.1 defends against.
	addr, res_ch := start_scripted_server(full_handshake_script)
	if addr.starts_with('ERR') {
		return error(addr)
	}
	ts := fakeserver.test_trust_store()!
	mut conn := dial(addr,
		server_name: fakeserver.server_name
		root_cas:    ts
		timeout:     10 * time.second
	)!

	mut buf := []u8{len: 64}
	n := conn.read(mut buf)! // drains the ticket, then the app record
	assert buf[..n] == 'hello from fakeserver'.bytes()

	// The abrupt close (no close_notify) must surface as an error, not 0.
	mut truncated := false
	mut sticky := ''
	conn.read(mut buf) or {
		assert err.msg().contains('connection closed')
		sticky = conn.fatal_error
		assert sticky == err.msg()
		truncated = true
	}
	assert truncated, 'an abrupt close without close_notify must not read as a clean EOF'

	// And it must stay sticky on the next read — never reduce to a silent 0 (§6.1).
	mut re_reported := false
	conn.read(mut buf) or {
		assert err.msg() == sticky
		re_reported = true
	}
	assert re_reported, 'truncation must remain a sticky error across reads'
	res := <-res_ch
	assert res == 'OK'
}
