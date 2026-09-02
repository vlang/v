// vtest build: present_openssl?
module quic

import crypto.ecdsa
import encoding.base64

// h3_server_test.v: Phase 13e regression coverage for h3_conn.v's new
// SERVER-role request handling -- a real client (dial()) and a real server
// (accept()) QuicConn pair, each wrapped in its own H3Conn, driven against
// each other with actual UDP-datagram-shaped bytes as the only channel
// between them (mirroring accept_test.v's own
// test_dial_and_accept_full_handshake_and_stream_exchange), then a real
// HTTP/3 request/response exchange layered on top. This proves the new
// server-role branch (handle_quic_events' peer_stream_opened, deliver_
// decoded_headers, dispatch_request_stream_frames' DataFrame arm,
// finalize_request_stream_if_done) actually interoperates with the
// EXISTING, already-reviewed client-role code on the other end -- not just
// that the server side is internally consistent in isolation.

// h3_server_test_cert_pem/h3_server_test_key_seed are copied verbatim from
// accept_test.v's own fixture (the established per-file-duplication
// pattern in this module -- each _test.v file compiles standalone, so a
// const defined in one is not visible from another; see listener_test.v's
// identical duplication for the same reason). A REAL, self-signed P-256
// certificate/key pair a real client can actually verify end to end.
const h3_server_test_cert_pem = '-----BEGIN CERTIFICATE-----\nMIIBpTCCAUugAwIBAgIUetSYX9TDsFKNHR+Zy05VdXcp+1cwCgYIKoZIzj0EAwIw\nFDESMBAGA1UEAwwJbG9jYWxob3N0MCAXDTI2MDgyNDIyNTkyMloYDzIxMjYwNzMx\nMjI1OTIyWjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwWTATBgcqhkjOPQIBBggqhkjO\nPQMBBwNCAAS6mM0J/l1Y65oZMLxYPHvySK8RJbkuECLMXmF3+yeIdqH9cCtKqumw\nDpY+Kz9IjfoVcqdyH5DPE5i7aquc1pwno3kwdzAdBgNVHQ4EFgQU/r32o4XKdpEk\nhx2iVbRtvYuVsXswHwYDVR0jBBgwFoAU/r32o4XKdpEkhx2iVbRtvYuVsXswDwYD\nVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAoQwFAYDVR0RBA0wC4IJbG9jYWxo\nb3N0MAoGCCqGSM49BAMCA0gAMEUCIGVFw0ddsmDoAyFGVy/K+MlbKnboWRZ0ibkM\n1lLBebL2AiEAvXkEh3aKEztQlrTJwIfjjO7l488gaFTZi63ZuWDIkWY=\n-----END CERTIFICATE-----\n'

const h3_server_test_key_seed = [
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

fn h3_server_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

fn h3_server_test_transport_parameters() QuicTransportParameters {
	return QuicTransportParameters{
		max_idle_timeout:                    30000
		initial_max_data:                    1 << 20
		initial_max_stream_data_bidi_local:  1 << 16
		initial_max_stream_data_bidi_remote: 1 << 16
		initial_max_stream_data_uni:         1 << 16
		initial_max_streams_bidi:            4
		initial_max_streams_uni:             4
	}
}

// h3_server_test_settings is this test's fixed, minimal own-SETTINGS
// content for both ends -- mirrors h3_udp_dial.v's real
// h3_default_own_settings shape without duplicating net.http's own
// constants (a different module).
fn h3_server_test_settings() []H3Setting {
	return [
		H3Setting{
			identifier: qpack_settings_max_table_capacity_id
			value:      4096
		},
		H3Setting{
			identifier: qpack_settings_blocked_streams_id
			value:      100
		},
	]
}

// h3_server_test_pair drives a REAL dial()/accept() pair to a fully
// established QUIC connection (identical shape to accept_test.v's own
// test_dial_and_accept_full_handshake_and_stream_exchange), wraps each
// side in its own H3Conn, and runs one extra poll(none, now) round on each
// so open_own_streams_if_ready has actually queued (not yet necessarily
// flushed) both connections' own control/QPACK stream headers -- matching
// h3_test_conn()'s identical extra-poll rationale in h3_conn_test.v.
fn h3_server_test_pair() !(&QuicConn, &H3Conn, &QuicConn, &H3Conn, u64) {
	mut signing_key := ecdsa.new_key_from_seed(h3_server_test_key_seed, fixed_size: true)!

	dial_params := DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        h3_server_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: h3_server_test_transport_parameters()
	}
	mut client, client_dg := dial(dial_params, 0)!

	accept_params := AcceptParams{
		transport_parameters: h3_server_test_transport_parameters()
		alpn_protocols:       ['h3']
		certificate_chain:    [
			CertificateEntry{
				cert_data: h3_server_test_pem_to_der(h3_server_test_cert_pem)
			},
		]
		signing_key:          signing_key
	}
	mut server, mut server_result := accept(client_dg.bytes, accept_params, 0)!

	mut client_outgoing := []QuicDatagram{}
	mut server_outgoing := server_result.outgoing.clone()
	mut now := u64(0)
	mut rounds := 0
	for (client_outgoing.len > 0 || server_outgoing.len > 0) && rounds < 20 {
		rounds += 1
		now += 10
		mut next_client_outgoing := []QuicDatagram{}
		mut next_server_outgoing := []QuicDatagram{}
		for dg in server_outgoing {
			r := client.poll(dg.bytes, now)!
			next_client_outgoing << r.outgoing
		}
		for dg in client_outgoing {
			r := server.poll(dg.bytes, now)!
			next_server_outgoing << r.outgoing
		}
		client_outgoing = next_client_outgoing.clone()
		server_outgoing = next_server_outgoing.clone()
	}
	assert rounds < 20, 'handshake did not converge within 20 rounds'
	assert client.state() == .established
	assert server.state() == .established

	mut client_h3 := new_h3_conn(mut client, H3ConnParams{
		settings:                     h3_server_test_settings()
		own_qpack_max_table_capacity: 4096
	})
	mut server_h3 := new_h3_conn(mut server, H3ConnParams{
		settings:                     h3_server_test_settings()
		own_qpack_max_table_capacity: 4096
	})
	client_h3.poll(none, now)!
	server_h3.poll(none, now)!
	now += 10
	return client, client_h3, server, server_h3, now
}

// pump_h3_pair_until_quiet drives BOTH H3Conns against each other by
// feeding every outgoing datagram from one side into the other's poll(),
// accumulating every H3Event either side ever produces, until neither has
// anything left to send. Bounded, matching every other convergence loop in
// this module's tests.
//
// Starts with one PRIMING poll(none, now) round on each side -- a caller
// normally invokes this right after queuing new writes on one side (e.g.
// send_request_headers/send_request_data), which only enqueue bytes into
// the wrapped QuicConn's own send buffers (queue-now-drain-later); nothing
// has actually been drained into a real outgoing datagram yet at that
// point, so starting the ping-pong loop from two empty outgoing lists
// would never move at all.
fn pump_h3_pair_until_quiet(mut a H3Conn, mut b H3Conn, start_now u64) !([]H3Event, []H3Event, u64) {
	mut now := start_now
	mut a_events := []H3Event{}
	mut b_events := []H3Event{}

	prime_a := a.poll(none, now)!
	mut a_outgoing := prime_a.outgoing.clone()
	a_events << prime_a.events

	prime_b := b.poll(none, now)!
	mut b_outgoing := prime_b.outgoing.clone()
	b_events << prime_b.events

	mut rounds := 0
	for (a_outgoing.len > 0 || b_outgoing.len > 0) && rounds < 40 {
		rounds += 1
		now += 10
		mut next_a_outgoing := []QuicDatagram{}
		mut next_b_outgoing := []QuicDatagram{}
		for dg in b_outgoing {
			r := a.poll(dg.bytes, now)!
			next_a_outgoing << r.outgoing
			a_events << r.events
		}
		for dg in a_outgoing {
			r := b.poll(dg.bytes, now)!
			next_b_outgoing << r.outgoing
			b_events << r.events
		}
		a_outgoing = next_a_outgoing.clone()
		b_outgoing = next_b_outgoing.clone()
	}
	assert rounds < 40, 'h3 pair did not converge within 40 rounds'
	return a_events, b_events, now
}

// test_h3_conn_server_role_receives_request_and_sends_response is this
// phase's core end-to-end proof: a real client opens a request stream and
// sends real request headers+body via the CLIENT-role API
// (open_request_stream/send_request_headers/send_request_data, unchanged
// by this phase), and a real server -- using ONLY the NEW server-role
// surface (peer_stream_opened's new branch, request_headers/request_data/
// request_ended events, send_response_headers/send_response_data) --
// receives it and answers, with the client then observing a real
// response_headers/response_data/response_ended sequence back.
fn test_h3_conn_server_role_receives_request_and_sends_response() {
	mut client, mut client_h3, mut server, mut server_h3, now0 := h3_server_test_pair()!
	defer {
		mut client_hs := client.client_handshake()
		client_hs.free()
		if mut sh := server.server_handshake {
			sh.free()
		}
	}

	// Let both sides' own control/QPACK stream headers actually reach the
	// wire before layering a request on top -- open_own_streams_if_ready
	// only QUEUES them (queue-now-drain-later); this drains that queue on
	// both ends via an empty pump round.
	_, _, now1 := pump_h3_pair_until_quiet(mut client_h3, mut server_h3, now0)!

	stream_id := client_h3.open_request_stream()!
	client_h3.send_request_headers(stream_id, [
		QpackFieldLine{
			name:  ':method'
			value: 'GET'
		},
		QpackFieldLine{
			name:  ':path'
			value: '/hello'
		},
		QpackFieldLine{
			name:  ':scheme'
			value: 'https'
		},
		QpackFieldLine{
			name:  ':authority'
			value: 'localhost'
		},
	], false)!
	client_h3.send_request_data(stream_id, 'ping'.bytes(), true)!

	client_events, server_events, now2 :=
		pump_h3_pair_until_quiet(mut client_h3, mut server_h3, now1)!
	// The client side produced no request_* events of its own -- it is a
	// CLIENT-role connection, so deliver_decoded_headers/finalize_request_
	// stream_if_done must only ever emit response_* on it, even though the
	// SAME code path now branches on role.
	assert client_events.filter(it.kind in [.request_headers, .request_data, .request_ended]).len == 0

	req_headers_ev := server_events.filter(it.kind == .request_headers)
	assert req_headers_ev.len == 1, 'server: exactly one request_headers event: ${server_events.str()}'
	assert req_headers_ev[0].stream_id or { 0 } == stream_id
	mut got_method := ''
	mut got_path := ''
	for f in req_headers_ev[0].headers {
		if f.name == ':method' {
			got_method = f.value
		}
		if f.name == ':path' {
			got_path = f.value
		}
	}
	assert got_method == 'GET'
	assert got_path == '/hello'

	req_data_ev := server_events.filter(it.kind == .request_data)
	assert req_data_ev.len == 1
	assert req_data_ev[0].data.bytestr() == 'ping'

	req_ended_ev := server_events.filter(it.kind == .request_ended)
	assert req_ended_ev.len == 1, 'server: exactly one request_ended event: ${server_events.str()}'

	// The server side must never have seen a response_* event -- it is a
	// SERVER-role connection, its own request/response traffic on this
	// stream is entirely request_*.
	assert server_events.filter(it.kind in [.response_headers, .response_data, .response_ended]).len == 0

	server_h3.send_response_headers(stream_id, [
		QpackFieldLine{
			name:  ':status'
			value: '200'
		},
	], false)!
	server_h3.send_response_data(stream_id, 'pong'.bytes(), true)!

	final_client_events, _, _ := pump_h3_pair_until_quiet(mut client_h3, mut server_h3, now2)!

	resp_headers_ev := final_client_events.filter(it.kind == .response_headers)
	assert resp_headers_ev.len == 1, 'client: exactly one response_headers event: ${final_client_events.str()}'
	assert resp_headers_ev[0].headers.len == 1
	assert resp_headers_ev[0].headers[0].name == ':status'
	assert resp_headers_ev[0].headers[0].value == '200'

	resp_data_ev := final_client_events.filter(it.kind == .response_data)
	assert resp_data_ev.len == 1
	assert resp_data_ev[0].data.bytestr() == 'pong'

	resp_ended_ev := final_client_events.filter(it.kind == .response_ended)
	assert resp_ended_ev.len == 1
}

// test_h3_conn_open_request_stream_rejected_on_server_role is a regression
// test for open_request_stream's new role guard: RFC 9114 §6.1 request
// streams are always client-initiated, so a server calling this on its own
// H3Conn (instead of answering on the stream the peer opened) is a caller
// bug this must reject rather than silently open a stream HTTP/3 gives no
// meaning to.
fn test_h3_conn_open_request_stream_rejected_on_server_role() {
	mut client, _, mut server, mut server_h3, _ := h3_server_test_pair()!
	defer {
		mut client_hs := client.client_handshake()
		client_hs.free()
		if mut sh := server.server_handshake {
			sh.free()
		}
	}
	server_h3.open_request_stream() or {
		assert err.msg().contains('client-role only')
		return
	}
	assert false, 'expected open_request_stream to fail on a server-role H3Conn'
}
