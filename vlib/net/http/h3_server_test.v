// vtest build: present_openssl?
module http

import net
import net.quic
import crypto.ecdsa
import encoding.base64
import time

// h3_server_test.v: Phase 13e end-to-end coverage for H3Server -- a REAL
// UDP socket, a REAL server goroutine (new_h3_server + serve(), exactly as
// a real caller would run it), and a REAL client-side driving loop
// (mirroring h3_udp_dial.v's own dial+drive shape, but inlined here rather
// than reusing H3MuxConn's own driver thread, since this test needs to
// inspect individual H3Events directly rather than going through
// H3MuxConn's Request/Response conversion). No shortcuts: the request
// travels as actual UDP datagrams over loopback, through this server's
// own accept/demux/dispatch loop, same as it would for a real client.

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

fn h3_server_test_transport_parameters() quic.QuicTransportParameters {
	return quic.QuicTransportParameters{
		max_idle_timeout:                    30000
		initial_max_data:                    1 << 20
		initial_max_stream_data_bidi_local:  1 << 16
		initial_max_stream_data_bidi_remote: 1 << 16
		initial_max_stream_data_uni:         1 << 16
		initial_max_streams_bidi:            4
		initial_max_streams_uni:             4
	}
}

// H3ServerTestEchoHandler answers every request with a fixed 200 response
// -- this test only needs to prove the request reached the Handler and the
// response reached the client, not exercise Handler-authoring variety.
struct H3ServerTestEchoHandler {}

fn (mut h H3ServerTestEchoHandler) handle(req Request) Response {
	return Response{
		status_code: 200
		body:        'pong:${req.data}'
	}
}

// h3_server_test_run is a NAMED function (not a closure) for the spawned
// server goroutine -- mirrors this project's own established caution
// around spawned closures with mut captures (see conn.v/h3_mux_conn.v's
// own doc comments on that pattern) by taking `s` as an explicit
// parameter instead. Swallows serve()'s error return: this test always
// ends the server via close(), which serve() reports as a clean (nil)
// return per its own doc comment, so there is nothing here worth
// asserting on if it ever DID return an error.
fn h3_server_test_run(mut s H3Server) {
	s.serve() or {}
}

// test_h3_server_real_udp_request_response_round_trip is 13e's own no-
// shortcuts integration test: a real H3Server bound to a real loopback UDP
// socket, driven by its own serve() goroutine exactly as a real deployment
// would run it, and a real client-side QUIC/H3 dial+drive loop (mirroring
// h3_udp_dial.v's own shape) sending an actual request and reading an
// actual response back over the wire.
fn test_h3_server_real_udp_request_response_round_trip() {
	mut signing_key := ecdsa.new_key_from_seed(h3_server_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}

	mut server := new_h3_server(':0', H3ServerParams{
		alpn_protocols:       ['h3']
		certificate_chain:    [
			quic.CertificateEntry{
				cert_data: h3_server_test_pem_to_der(h3_server_test_cert_pem)
			},
		]
		signing_key:          signing_key
		transport_parameters: h3_server_test_transport_parameters()
		handler:              H3ServerTestEchoHandler{}
	})!
	server_addr := server.local_addr()!
	port := server_addr.port()!

	server_thread := spawn h3_server_test_run(mut server)
	defer {
		server.close() or {}
		server_thread.wait()
	}

	mut udp := net.dial_udp('127.0.0.1:${port}')!
	defer {
		udp.close() or {}
	}
	udp.set_read_timeout(500 * time.millisecond)

	now0 := h3_now_ms()
	mut qc, first_dg := quic.dial(quic.DialParams{
		server_name:          'localhost'
		ca_bundle_pem:        h3_server_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: h3_server_test_transport_parameters()
	}, now0)!
	udp.write(first_dg.bytes)!
	mut h3 := quic.new_h3_conn(mut qc, quic.H3ConnParams{
		settings:                     [
			quic.H3Setting{
				identifier: quic.qpack_settings_max_table_capacity_id
				value:      4096
			},
		]
		own_qpack_max_table_capacity: 4096
	})

	mut buf := []u8{len: 65535}
	mut stream_id := u64(0)
	mut got_body := ''
	mut got_status := ''
	mut done := false
	mut rounds := 0
	mut read_failed := false
	for !done && rounds < 200 {
		rounds += 1
		n, _ := udp.read(mut buf) or {
			if err.code() != net.err_timed_out_code {
				read_failed = true
			}
			0, net.Addr{}
		}
		if read_failed {
			break
		}
		now := h3_now_ms()
		result := if n > 0 {
			h3.poll(buf[..n].clone(), now)!
		} else {
			h3.process_timeouts(now)!
		}
		for dg in result.outgoing {
			udp.write(dg.bytes)!
		}
		if h3.established() && stream_id == 0 {
			stream_id = h3.open_request_stream()!
			h3.send_request_headers(stream_id, [
				quic.QpackFieldLine{
					name:  ':method'
					value: 'GET'
				},
				quic.QpackFieldLine{
					name:  ':path'
					value: '/echo'
				},
				quic.QpackFieldLine{
					name:  ':scheme'
					value: 'https'
				},
				quic.QpackFieldLine{
					name:  ':authority'
					value: 'localhost'
				},
			], false)!
			h3.send_request_data(stream_id, 'hi'.bytes(), true)!
		}
		for ev in result.events {
			match ev.kind {
				.response_headers {
					for f in ev.headers {
						if f.name == ':status' {
							got_status = f.value
						}
					}
				}
				.response_data {
					got_body += ev.data.bytestr()
				}
				.response_ended {
					done = true
				}
				else {}
			}
		}
	}
	assert done, 'expected a response within ${rounds} rounds'
	assert got_status == '200'
	assert got_body == 'pong:hi'
}

// test_h3_server_local_addr_reports_os_assigned_port is a small,
// dedicated regression test for local_addr() itself, independent of the
// full round-trip test above -- a caller binding with ':0' has no other
// way to discover which port new_h3_server actually got.
fn test_h3_server_local_addr_reports_os_assigned_port() {
	mut signing_key := ecdsa.new_key_from_seed(h3_server_test_key_seed, fixed_size: true)!
	defer {
		signing_key.free()
	}
	mut server := new_h3_server(':0', H3ServerParams{
		alpn_protocols:       ['h3']
		certificate_chain:    [
			quic.CertificateEntry{
				cert_data: h3_server_test_pem_to_der(h3_server_test_cert_pem)
			},
		]
		signing_key:          signing_key
		transport_parameters: h3_server_test_transport_parameters()
		handler:              H3ServerTestEchoHandler{}
	})!
	defer {
		server.close() or {}
	}
	addr := server.local_addr()!
	port := addr.port()!
	assert port != 0, 'an OS-assigned port must never report as 0'
}
