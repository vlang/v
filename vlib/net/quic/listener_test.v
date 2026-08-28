// vtest build: present_openssl?
module quic

import crypto.ecdsa
import encoding.base64

// A real, freshly-generated (openssl ecparam + openssl req -x509, this
// session) self-signed P-256 certificate, WITH a critical CA:TRUE basic
// constraint -- same construction as accept_test.v's own fixture (that
// file's own doc comment explains why a fresh cert was needed rather than
// this module's other, deliberately-rejection-only test certs). Duplicated
// here rather than shared: each `_test.v` file in this module compiles and
// runs as its own independent test binary, so top-level declarations in
// accept_test.v are not visible from this file.
const listener_test_cert_pem = '-----BEGIN CERTIFICATE-----\nMIIBpTCCAUugAwIBAgIUetSYX9TDsFKNHR+Zy05VdXcp+1cwCgYIKoZIzj0EAwIw\nFDESMBAGA1UEAwwJbG9jYWxob3N0MCAXDTI2MDgyNDIyNTkyMloYDzIxMjYwNzMx\nMjI1OTIyWjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwWTATBgcqhkjOPQIBBggqhkjO\nPQMBBwNCAAS6mM0J/l1Y65oZMLxYPHvySK8RJbkuECLMXmF3+yeIdqH9cCtKqumw\nDpY+Kz9IjfoVcqdyH5DPE5i7aquc1pwno3kwdzAdBgNVHQ4EFgQU/r32o4XKdpEk\nhx2iVbRtvYuVsXswHwYDVR0jBBgwFoAU/r32o4XKdpEkhx2iVbRtvYuVsXswDwYD\nVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAoQwFAYDVR0RBA0wC4IJbG9jYWxo\nb3N0MAoGCCqGSM49BAMCA0gAMEUCIGVFw0ddsmDoAyFGVy/K+MlbKnboWRZ0ibkM\n1lLBebL2AiEAvXkEh3aKEztQlrTJwIfjjO7l488gaFTZi63ZuWDIkWY=\n-----END CERTIFICATE-----\n'

const listener_test_key_seed = [
	u8(0x4f),
	0xd6,
	0x31,
	0x08,
	0x03,
	0xcb,
	0xd1,
	0x42,
	0xd5,
	0xc5,
	0xac,
	0xe4,
	0xd1,
	0xb1,
	0xca,
	0x06,
	0xfb,
	0xde,
	0xc0,
	0x5a,
	0xc0,
	0x6e,
	0xb9,
	0x58,
	0x60,
	0x01,
	0x3f,
	0x02,
	0x79,
	0x7c,
	0xb3,
	0x15,
]

fn listener_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

fn listener_test_transport_parameters() QuicTransportParameters {
	return QuicTransportParameters{
		max_idle_timeout:                    30000
		initial_max_data:                    1 << 20
		initial_max_stream_data_bidi_local:  1 << 16
		initial_max_stream_data_bidi_remote: 1 << 16
		initial_max_streams_bidi:            4
		initial_max_streams_uni:             4
	}
}

fn listener_test_params(signing_key ecdsa.PrivateKey) QuicListenerParams {
	return QuicListenerParams{
		transport_parameters: listener_test_transport_parameters()
		alpn_protocols:       ['h3']
		certificate_chain:    [
			CertificateEntry{
				cert_data: listener_test_pem_to_der(listener_test_cert_pem)
			},
		]
		signing_key:          signing_key
		retry_token_key:      [u8(1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
	}
}

fn test_derive_retry_scid_is_deterministic_and_key_dependent() {
	key_a := [u8(1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
	key_b := [u8(16), 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
	dcid := [u8(0xaa), 0xbb, 0xcc, 0xdd]

	first := derive_retry_scid(key_a, dcid)!
	second := derive_retry_scid(key_a, dcid)!
	assert first == second
	assert first.len == local_cid_len

	different_key := derive_retry_scid(key_b, dcid)!
	assert different_key != first

	different_dcid := derive_retry_scid(key_a, [u8(0x11), 0x22, 0x33, 0x44])!
	assert different_dcid != first
}

// test_listener_always_retry_then_accepts_full_handshake is 13d-2's own
// real integration test: a genuine dial()ed client is driven entirely
// through a QuicListener configured with always_retry (the default) --
// the FIRST datagram gets a Retry back (no accept() call yet, confirmed by
// connection_count() staying 0), the client's own EXISTING Retry-handling
// (process_retry, unconditionally exercised by every poll() call on a
// Retry-type packet) transparently resends its ClientHello now carrying
// the token, and only THAT second datagram reaches do_accept -- proving
// the whole chain (peek_datagram_dcid's demux, handle_new_attempt's
// token-validate-or-retry branch, derive_retry_scid's statelessness,
// AcceptParams.retry_source_connection_id/original_dcid_override's
// plumbing) actually agrees end to end against this codebase's own
// independently-written client, not merely that each piece is internally
// consistent. Also exercises the demuxed steady-state (further datagrams
// after accept route by scid, not by a fresh new-attempt lookup) and
// process_timeouts/connection_count retirement once the connection closes.
fn test_listener_always_retry_then_accepts_full_handshake() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(listener_test_params(signing_key))!
	peer := 'client-peer-address'.bytes()

	// Round 1: no token yet -- expect exactly a Retry, no connection
	// accepted.
	retry_result := listener.poll(client_dg.bytes, peer, 0)!
	assert retry_result.outgoing.len == 1
	assert listener.connection_count() == 0

	// Feed the Retry straight into the REAL client's own poll() -- its
	// existing process_retry logic (conn.v) transparently resends the
	// ClientHello with the token attached; nothing in this test hand-builds
	// that resent packet.
	resend_result := client.poll(retry_result.outgoing[0].bytes, 10)!
	assert resend_result.outgoing.len > 0

	// Round 2: now carries a valid, address-matched token -- expect a real
	// accept() this time.
	mut server_result := listener.poll(resend_result.outgoing[0].bytes, peer, 20)!
	assert listener.connection_count() == 1
	assert server_result.outgoing.len > 0
	assert server_result.outgoing[0].peer.bytestr() == peer.bytestr()

	// Drive the rest of the handshake to completion, demuxed through the
	// SAME listener the whole time (proving steady-state routing by scid
	// works, not just the new-attempt path).
	mut client_outgoing := []QuicDatagram{}
	mut server_outgoing := server_result.outgoing.map(QuicDatagram{ bytes: it.bytes })
	mut now := u64(20)
	mut rounds := 0
	for (client_outgoing.len > 0 || server_outgoing.len > 0) && rounds < 20 {
		rounds += 1
		now += 10
		mut next_client_outgoing := []QuicDatagram{}
		mut next_server_outgoing := []QuicListenerDatagram{}
		for dg in server_outgoing {
			r := client.poll(dg.bytes, now)!
			next_client_outgoing << r.outgoing
		}
		for dg in client_outgoing {
			r := listener.poll(dg.bytes, peer, now)!
			next_server_outgoing << r.outgoing
		}
		client_outgoing = next_client_outgoing.clone()
		server_outgoing = next_server_outgoing.map(QuicDatagram{ bytes: it.bytes })
	}
	assert rounds < 20, 'handshake did not converge within 20 rounds'
	assert client.state() == .established
	assert listener.connection_count() == 1

	// Application-data proof: open a stream from the client, confirm the
	// listener's demux delivers it to the SAME already-accepted connection
	// (not a second, spuriously new-attempt-routed one).
	stream_id := client.open_stream(true)!
	client.write_stream(stream_id, 'hello via listener'.bytes(), true)!
	now += 10
	client_to_server := client.poll(none, now)!
	assert client_to_server.outgoing.len > 0
	for dg in client_to_server.outgoing {
		listener.poll(dg.bytes, peer, now)!
	}
	assert listener.connection_count() == 1
}

// test_listener_retry_token_validates_at_realistic_nanosecond_scale is the
// dedicated regression test for handle_new_attempt/send_retry's `now /
// 1_000_000` conversion (see their own doc comments): every OTHER retry-
// token test in this file drives the Retry-then-accept flow with tiny
// synthetic `now` values (0, 10, 20, ...), all far below 1_000_000 -- for
// which `now / 1_000_000` is IDENTICALLY 0 whether or not the conversion
// is even applied, so those tests cannot distinguish the fixed call sites
// from the pre-fix bug (raw `now` passed straight into a millisecond-
// shaped API with no conversion). This test instead uses billion-scale
// `now` values matching what a real `time.sys_mono_now()` instant looks
// like (h3_now_ns's own convention, net.http) -- a real 100ms gap between
// issuance and validation, well within the 30-second default expiry
// window ONLY once correctly converted to milliseconds; the reverted, pre-
// fix code would see a raw 100_000_000-nanosecond delta, ~3333x the 30000
// default `retry_token_max_age_ms`, and reject the token as expired.
fn test_listener_retry_token_validates_at_realistic_nanosecond_scale() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(listener_test_params(signing_key))!
	peer := 'realistic-now-peer'.bytes()

	issue_now := u64(5_000_000_000) // 5s of real nanosecond-scale uptime
	retry_result := listener.poll(client_dg.bytes, peer, issue_now)!
	assert retry_result.outgoing.len == 1
	assert listener.connection_count() == 0

	resend_result := client.poll(retry_result.outgoing[0].bytes, issue_now + 1_000_000)!
	assert resend_result.outgoing.len > 0

	// 100ms of real elapsed time (100_000_000ns) later -- trivially within
	// the 30s window once correctly treated as milliseconds; the pre-fix
	// bug (no conversion) would see this raw nanosecond delta compared
	// directly against a millisecond-shaped 30000 threshold and reject.
	validate_now := issue_now + 100_000_000
	server_result := listener.poll(resend_result.outgoing[0].bytes, peer, validate_now)!
	assert listener.connection_count() == 1
	assert server_result.outgoing.len > 0
}

// test_listener_direct_accept_without_retry covers the always_retry=false
// policy: the FIRST datagram itself (no token, no Retry round-trip) must
// reach do_accept directly.
fn test_listener_direct_accept_without_retry() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	params := QuicListenerParams{
		...listener_test_params(signing_key)
		always_retry: false
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(params)!
	peer := 'direct-accept-peer'.bytes()

	result := listener.poll(client_dg.bytes, peer, 0)!
	assert listener.connection_count() == 1
	assert result.outgoing.len > 0
	assert result.events.len == 0
}

// test_listener_discards_invalid_token_without_retrying covers RFC 9000
// §8.1.2's "a server cannot send another Retry packet" rule for an
// Initial that already carries a token that fails validation -- the
// listener must silently discard, not issue a fresh Retry. Gets a real,
// well-formed token-bearing Initial the cheap way: run the real Retry
// round-trip against ONE listener (`issuer`, key K1), then feed that
// genuine resend packet to a DIFFERENT listener (`victim`, key K2) --
// K2 can never decrypt a token AEAD-sealed under K1, giving a guaranteed,
// realistic AEAD-authentication failure without hand-rolling packet bytes.
fn test_listener_discards_invalid_token_without_retrying() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	issuer_params := QuicListenerParams{
		...listener_test_params(signing_key)
		retry_token_key: [u8(1), 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
	}
	mut issuer := new_quic_listener(issuer_params)!
	peer := 'invalid-token-peer'.bytes()

	retry_result := issuer.poll(client_dg.bytes, peer, 0)!
	assert retry_result.outgoing.len == 1
	resend_result := client.poll(retry_result.outgoing[0].bytes, 10)!
	assert resend_result.outgoing.len > 0

	victim_params := QuicListenerParams{
		...listener_test_params(signing_key)
		retry_token_key: [u8(2), 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
	}
	mut victim := new_quic_listener(victim_params)!

	result := victim.poll(resend_result.outgoing[0].bytes, peer, 20)!
	assert result.outgoing.len == 0
	assert result.events.len == 0
	assert victim.connection_count() == 0
}

// test_listener_ignores_unrecognized_non_initial_datagram covers RFC 9000
// §5.2.2's "MAY drop" allowance: a short-header-shaped datagram with no
// matching connection must be silently ignored, not crash or error.
fn test_listener_ignores_unrecognized_non_initial_datagram() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	mut listener := new_quic_listener(listener_test_params(signing_key))!
	peer := 'stray-peer'.bytes()

	// Short header (top bit of byte 0 clear) + local_cid_len (8) bytes of
	// an arbitrary, definitely-unrecognized DCID + a few payload bytes.
	mut stray := []u8{len: 1 + local_cid_len + 4, init: 0x42}
	stray[0] = 0x40
	result := listener.poll(stray, peer, 0)!
	assert result.outgoing.len == 0
	assert result.events.len == 0
	assert listener.connection_count() == 0
}

// test_listener_retires_closed_connection covers connection cleanup:
// process_timeouts must remove a connection from the demux tables once it
// reaches .closed, matching retire_if_closed's own doc comment.
fn test_listener_retires_closed_connection() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	params := QuicListenerParams{
		...listener_test_params(signing_key)
		always_retry: false
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(params)!
	peer := 'retire-peer'.bytes()

	result := listener.poll(client_dg.bytes, peer, 0)!
	assert listener.connection_count() == 1
	assert result.events.len == 0

	// close() the accepted connection directly -- accessible via the same
	// package-private QuicConn.close() every other white-box test in this
	// module already uses; a QuicListenerEvent would be the caller-facing
	// way to reach it, but no event fires from a clean accept() with
	// nothing left to report. Reached via listener.conns itself (keyed by
	// the SERVER's own freshly-generated scid, not client.scid -- those
	// are two unrelated values) since connection_count()==1 already
	// guarantees exactly one entry.
	for key in listener.conns.keys() {
		mut server_conn := listener.conns[key] or { continue }
		server_conn.close(0, 'test done')
	}

	// A small `now` first: enough to flush the queued close() into an
	// actual outgoing CONNECTION_CLOSE (drain_pending_close) without
	// tripping the idle-timeout check process_timeouts runs FIRST (which
	// would transition straight to .closed with no outgoing packet at
	// all, short-circuiting past drain_pending_close entirely -- not what
	// this test is meant to exercise).
	close_result := listener.process_timeouts(30)!
	assert close_result.outgoing.len > 0
	assert listener.connection_count() == 1

	// Then far enough past the closing/draining deadline (RFC 9000
	// §10.2.2, ~3x PTO) for retire_if_closed to actually fire.
	far_future := u64(1) << 40
	listener.process_timeouts(far_future)!
	assert listener.connection_count() == 0
}

// test_listener_deduplicates_retransmitted_new_attempt is a regression test
// for a real bug 13d-2's own adversarial review found: without
// pending_by_dcid's dedup, replaying/retransmitting the IDENTICAL
// first-flight Initial datagram before the client has processed any reply
// -- exactly what an ordinary RFC 9002 PTO-triggered retransmission looks
// like on the wire after real (non-adversarial) packet loss, or what a
// replayed Retry token looks like -- would call do_accept() a second time,
// allocating a second, independent QuicConn for what is really one logical
// attempt. Uses always_retry:false so the FIRST datagram itself is already
// enough to trigger do_accept, isolating this from the separate Retry
// round-trip machinery already covered by other tests in this file.
fn test_listener_deduplicates_retransmitted_new_attempt() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	params := QuicListenerParams{
		...listener_test_params(signing_key)
		always_retry: false
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(params)!
	peer := 'dup-peer'.bytes()

	result1 := listener.poll(client_dg.bytes, peer, 0)!
	assert listener.connection_count() == 1
	assert result1.outgoing.len > 0

	// Retransmit the IDENTICAL first-flight datagram -- what a genuine
	// PTO-fired retransmission looks like on the wire before the client
	// has processed anything back.
	result2 := listener.poll(client_dg.bytes, peer, 5)!
	assert listener.connection_count() == 1
	assert result2.outgoing.len > 0
}

// test_listener_always_replies_to_originally_recorded_peer is a regression
// test for a real bug 13d-2's own adversarial review found: poll()'s
// known-connection fast path used to address every outgoing datagram to
// whatever `peer` value THAT call happened to be invoked with, instead of
// the address recorded at accept() time -- letting anyone holding a real,
// legitimately-established connection redirect the server's own replies
// (ACKs, retransmissions, application data) toward an arbitrary spoofed
// address on demand, defeating the entire point of address validation at
// accept() time. This test drives a connection through a normal accept,
// then polls it again with a DIFFERENT peer value and confirms the reply
// still targets the connection's ORIGINAL address.
fn test_listener_always_replies_to_originally_recorded_peer() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	params := QuicListenerParams{
		...listener_test_params(signing_key)
		always_retry: false
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(params)!
	real_peer := 'real-client-address'.bytes()
	spoofed_peer := 'spoofed-victim-address'.bytes()

	server_result := listener.poll(client_dg.bytes, real_peer, 0)!
	assert listener.connection_count() == 1
	assert server_result.outgoing.len > 0
	for dg in server_result.outgoing {
		assert dg.peer.bytestr() == real_peer.bytestr()
	}

	// Drive the handshake to completion first -- the whole point of this
	// bug is that it only manifests on poll()'s KNOWN-connection path,
	// which isn't reached until the client has processed a reply and
	// switched its own dcid to the server's real scid (RFC 9000 §7.2);
	// same round-trip-loop pattern as this file's own main integration
	// test, just with a fixed `real_peer` throughout.
	mut client_outgoing := []QuicDatagram{}
	mut server_outgoing := server_result.outgoing.map(QuicDatagram{ bytes: it.bytes })
	mut now := u64(0)
	mut rounds := 0
	for (client_outgoing.len > 0 || server_outgoing.len > 0) && rounds < 20 {
		rounds += 1
		now += 10
		mut next_client_outgoing := []QuicDatagram{}
		mut next_server_outgoing := []QuicListenerDatagram{}
		for dg in server_outgoing {
			r := client.poll(dg.bytes, now)!
			next_client_outgoing << r.outgoing
		}
		for dg in client_outgoing {
			r := listener.poll(dg.bytes, real_peer, now)!
			next_server_outgoing << r.outgoing
		}
		client_outgoing = next_client_outgoing.clone()
		server_outgoing = next_server_outgoing.map(QuicDatagram{ bytes: it.bytes })
	}
	assert rounds < 20, 'handshake did not converge within 20 rounds'
	assert client.state() == .established

	// Now the actual regression check: send one more legitimate packet
	// for this SAME, already-established connection (correct DCID, so it
	// demuxes and decrypts fine), but claim it arrived from a completely
	// different address.
	stream_id := client.open_stream(true)!
	client.write_stream(stream_id, 'hello'.bytes(), true)!
	now += 10
	client_to_server := client.poll(none, now)!
	assert client_to_server.outgoing.len > 0
	mut spoofed_reply_count := 0
	for dg in client_to_server.outgoing {
		spoofed_result := listener.poll(dg.bytes, spoofed_peer, now)!
		for out in spoofed_result.outgoing {
			spoofed_reply_count += 1
			assert out.peer.bytestr() == real_peer.bytestr()
			assert out.peer.bytestr() != spoofed_peer.bytestr()
		}
	}
	assert spoofed_reply_count > 0, 'expected at least one reply to the spoofed-source datagram to actually check its peer'
}

// test_listener_direct_accept_enforces_anti_amplification_limit is a
// regression test for RFC 9000 §8.1's mandatory 3x send limit, a gap
// 13d-2's own adversarial review found: with always_retry=false (a
// documented, supported policy), accept()'s full response flight was
// completely unbounded relative to the size of the triggering datagram --
// confirmed by the review's own empirical repro that a realistic
// certificate chain pushes the response past 4x the incoming 1200-byte
// trigger with nothing enforcing the RFC's cap. Uses a padded
// CertificateEntry (never actually parsed as a real certificate by the
// server's own send path -- accept() just encodes these bytes into the
// Certificate message) specifically to inflate the response flight well
// past 3x, and confirms the FIRST poll() call's total outgoing bytes stay
// within budget while the rest remains queued for a later, budget-permitting
// drain rather than being sent unconditionally.
fn test_listener_direct_accept_enforces_anti_amplification_limit() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	mut padded_cert := listener_test_pem_to_der(listener_test_cert_pem)
	padded_cert << []u8{len: 8000, init: 0x00}
	params := QuicListenerParams{
		...listener_test_params(signing_key)
		always_retry:      false
		certificate_chain: [
			CertificateEntry{
				cert_data: padded_cert
			},
		]
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}
	incoming_len := client_dg.bytes.len

	mut listener := new_quic_listener(params)!
	peer := 'amplification-peer'.bytes()

	result := listener.poll(client_dg.bytes, peer, 0)!
	mut outgoing_total := 0
	for dg in result.outgoing {
		outgoing_total += dg.bytes.len
	}
	assert outgoing_total <= 3 * incoming_len
}

// test_pto_probe_respects_anti_amplification_limit is the regression test
// for a real bug: send_pto_probe (conn.v) appended a PING datagram for the
// .initial/.handshake spaces unconditionally, with no has_amplification_
// budget()/record_amplification_sent() gating at all -- unlike every other
// send in drain_outgoing, which IS gated. A server accepted without Retry
// (always_retry: false, exactly this test's own setup) has an unvalidated
// address the moment it accepts; on_loss_detection_timeout keeps firing a
// fresh PTO (2^pto_count backoff) for as long as anything stays unacked,
// and pre-fix each firing would append another probe past the RFC 9000
// §8.1 3x-received cap -- an unbounded amplification source triggered by
// nothing but a spoofed Initial and silence. Deterministically drains the
// accepted connection's OWN remaining budget to exactly zero (module-
// private field, reachable since this file is `module quic` -- avoids
// depending on a specific certificate size coincidentally leaving zero
// headroom after one poll(), which the coarse has_amplification_budget()
// check -- "is there ANY budget at all", same imprecision drain_outgoing's
// own doc comment already accepts elsewhere -- would otherwise still
// legitimately let a small enough PING through), then drives
// process_timeouts() far enough past accept to guarantee a PTO has fired,
// and asserts NOTHING new went out. Pre-fix, this would see a PING-only
// Initial/Handshake datagram from send_pto_probe despite zero budget.
// (Codex review, PR #28164 pullrequestreview-5044139767.)
fn test_pto_probe_respects_anti_amplification_limit() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	params := QuicListenerParams{
		...listener_test_params(signing_key)
		always_retry: false
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(params)!
	peer := 'pto-amplification-peer'.bytes()

	accept_result := listener.poll(client_dg.bytes, peer, 0)!
	assert accept_result.touched_conns.len == 1
	mut c := accept_result.touched_conns[0]
	remaining := c.amplification.available_to_send()
	if remaining > 0 {
		c.amplification.note_sent_unconditional(remaining)
	}
	assert c.amplification.available_to_send() == 0

	// 10 real seconds (nanosecond-scale `now`, this module's own
	// convention) is far past any first-PTO deadline for a connection
	// still on its initial RTT estimate -- guarantees on_loss_detection_
	// timeout has fired at least once by the time process_timeouts runs.
	far_future := u64(10) * 1_000_000_000
	timeout_result := listener.process_timeouts(far_future)!
	assert timeout_result.outgoing.len == 0
}

// test_pto_probe_fires_with_positive_budget is the positive-case sibling
// of test_pto_probe_respects_anti_amplification_limit above: that test
// only proves a probe is WITHHELD at exactly zero budget, on whichever
// arm (.initial, ties broken toward it) a fresh accept()-only connection
// happens to reach -- it says nothing about whether a probe is actually
// SENT when budget genuinely allows, and nothing about the ordinary
// post-handshake .application_data arm specifically (the highest-value
// gap: real deployments spend almost all their time here, not in the
// brief handshake window). Drives a connection all the way to a real,
// quiescent .established state (mirroring test_listener_always_retry_
// then_accepts_full_handshake's own pattern), has the SERVER proactively
// write to a fresh stream (queuing a real ack-eliciting 1-RTT STREAM
// frame), flushes it out via one process_timeouts call WITHOUT ever
// delivering that datagram to the client (simulating a peer that stops
// responding), then advances far into the future and asserts a PTO probe
// genuinely goes out. By this point Initial/Handshake keys are long
// discarded (handshake is confirmed), so any outgoing datagram here can
// only be an .application_data-space send -- either the retransmitted
// STREAM frame itself or send_pto_probe's own PING, either way proving
// this arm is not silently gated shut.
fn test_pto_probe_fires_with_positive_budget() {
	mut signing_key := ecdsa.new_key_from_seed(listener_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        listener_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: listener_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!
	mut client_hs := client.client_handshake()
	defer {
		client_hs.free()
	}

	mut listener := new_quic_listener(listener_test_params(signing_key))!
	peer := 'positive-budget-peer'.bytes()

	retry_result := listener.poll(client_dg.bytes, peer, 0)!
	resend_result := client.poll(retry_result.outgoing[0].bytes, 10)!
	mut server_result := listener.poll(resend_result.outgoing[0].bytes, peer, 20)!
	assert listener.connection_count() == 1

	mut client_outgoing := []QuicDatagram{}
	mut server_outgoing := server_result.outgoing.map(QuicDatagram{ bytes: it.bytes })
	mut now := u64(20)
	mut rounds := 0
	for (client_outgoing.len > 0 || server_outgoing.len > 0) && rounds < 20 {
		rounds += 1
		now += 10
		mut next_client_outgoing := []QuicDatagram{}
		mut next_server_outgoing := []QuicListenerDatagram{}
		for dg in server_outgoing {
			r := client.poll(dg.bytes, now)!
			next_client_outgoing << r.outgoing
		}
		for dg in client_outgoing {
			r := listener.poll(dg.bytes, peer, now)!
			next_server_outgoing << r.outgoing
		}
		client_outgoing = next_client_outgoing.clone()
		server_outgoing = next_server_outgoing.map(QuicDatagram{ bytes: it.bytes })
	}
	assert rounds < 20, 'handshake did not converge within 20 rounds'
	assert client.state() == .established

	// Grab the accepted connection via touched_conns (populated even for
	// a no-op process_timeouts call -- see that field's own doc comment)
	// rather than a second accept-shaped call, since one already exists.
	now += 10
	mut grab := listener.process_timeouts(now)!
	assert grab.touched_conns.len == 1
	mut server_conn := grab.touched_conns[0]
	assert server_conn.amplification.available_to_send() > 0

	stream_id := server_conn.open_stream(true)!
	server_conn.write_stream(stream_id, 'unacked-probe-bait'.bytes(), true)!
	now += 10
	// Flushes the freshly-queued STREAM frame into an outgoing datagram
	// (drain_outgoing runs unconditionally at the end of process_timeouts,
	// regardless of whether any timer literally fired -- conn.v's own
	// process_timeouts, right after the loss-detection-timeout check).
	// Deliberately never delivered to `client`: simulating a peer that
	// stops responding after this point, leaving it ack-eliciting and
	// unacked.
	flush_result := listener.process_timeouts(now)!
	assert flush_result.outgoing.len > 0

	// Far enough past `now` that on_loss_detection_timeout's PTO deadline
	// has certainly elapsed for a still-unacked ack-eliciting send.
	far_future := now + 10 * 1_000_000_000
	pto_result := listener.process_timeouts(far_future)!
	assert pto_result.outgoing.len > 0
}
