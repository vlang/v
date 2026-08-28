// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.quic

// h3_validate_request_pseudo's own doc comment (h3_server.v) covers the
// full RFC 9114 §4.3.1/§4.4 shape it enforces; these are pure, fast unit
// tests for it directly, independent of h3_server_test.v's real-socket
// end-to-end coverage -- in particular the CONNECT-specific branch, which
// nothing else in this module exercises.

fn test_h3_validate_request_pseudo_accepts_an_ordinary_get() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'GET'
		},
		quic.QpackFieldLine{
			name:  ':path'
			value: '/'
		},
		quic.QpackFieldLine{
			name:  ':scheme'
			value: 'https'
		},
		quic.QpackFieldLine{
			name:  ':authority'
			value: 'example.com'
		},
	])!
}

fn test_h3_validate_request_pseudo_rejects_ordinary_request_missing_scheme() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'GET'
		},
		quic.QpackFieldLine{
			name:  ':path'
			value: '/'
		},
	]) or { return }
	assert false, 'expected an error for a GET request missing :scheme'
}

// A conforming CONNECT request (RFC 9114 §4.4): :method=CONNECT,
// :authority present, :scheme and :path both OMITTED. Regression test for
// a real bug: an earlier version of h3_validate_request_pseudo required
// :path/:scheme unconditionally, so this exact conforming shape was
// rejected as malformed and never reached the Handler. (Codex review, PR
// #28164 pullrequestreview-5044139767.)
fn test_h3_validate_request_pseudo_accepts_a_conforming_connect() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name:  ':authority'
			value: 'example.com:443'
		},
	])!
}

fn test_h3_validate_request_pseudo_rejects_connect_with_scheme() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name:  ':scheme'
			value: 'https'
		},
		quic.QpackFieldLine{
			name:  ':authority'
			value: 'example.com:443'
		},
	]) or { return }
	assert false, 'expected an error for a CONNECT request carrying :scheme'
}

fn test_h3_validate_request_pseudo_rejects_connect_with_path() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name:  ':path'
			value: '/'
		},
		quic.QpackFieldLine{
			name:  ':authority'
			value: 'example.com:443'
		},
	]) or { return }
	assert false, 'expected an error for a CONNECT request carrying :path'
}

fn test_h3_validate_request_pseudo_rejects_connect_missing_authority() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'CONNECT'
		},
	]) or { return }
	assert false, 'expected an error for a CONNECT request missing :authority'
}

// Extended CONNECT (RFC 9220-style WebSockets-over-HTTP/3, a `:protocol`
// pseudo-header) is explicitly out of scope for this fix -- see
// h3_validate_request_pseudo's own doc comment (h3_server.v). This is the
// regression test that scope-limit claim needs: :protocol must still fall
// through the generic "unknown request pseudo-header" rejection, not be
// silently accepted as if it were a recognized field. Without this test,
// a future change adding a `:protocol` match arm while implementing
// Extended CONNECT -- without also wiring up its required semantics --
// would compile and ship with :protocol-bearing requests silently passing
// validation instead of being rejected.
fn test_h3_validate_request_pseudo_rejects_protocol_pseudo_header() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name:  ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name:  ':protocol'
			value: 'websocket'
		},
		quic.QpackFieldLine{
			name:  ':authority'
			value: 'example.com:443'
		},
	]) or { return }
	assert false, 'expected an error for a request carrying the unimplemented :protocol pseudo-header'
}

// h3_build_request end-to-end for CONNECT: method decodes correctly and
// url stays empty (there is no :path to populate it from) -- the Handler,
// not this layer, is responsible for CONNECT-specific behavior.
fn test_h3_build_request_connect_yields_connect_method_and_empty_url() {
	st := &H3ServerStream{
		headers: [
			quic.QpackFieldLine{
				name:  ':method'
				value: 'CONNECT'
			},
			quic.QpackFieldLine{
				name:  ':authority'
				value: 'example.com:443'
			},
		]
	}
	req := h3_build_request(st)!
	assert req.method == .connect
	assert req.url == ''
}
