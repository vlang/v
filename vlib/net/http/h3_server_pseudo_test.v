// vtest build: present_openssl?
// vtest vflags: -d http3
// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net.quic

// h3_validate_request_pseudo's own doc comment (h3_server.v) covers the
// full RFC 9114 §4.3.1 shape it enforces; these are pure, fast unit tests
// for it directly, independent of h3_server_test.v's real-socket end-to-end
// coverage.

fn test_h3_validate_request_pseudo_accepts_an_ordinary_get() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'GET'
		},
		quic.QpackFieldLine{
			name: ':path'
			value: '/'
		},
		quic.QpackFieldLine{
			name: ':scheme'
			value: 'https'
		},
		quic.QpackFieldLine{
			name: ':authority'
			value: 'example.com'
		},
	])!
}

fn test_h3_validate_request_pseudo_rejects_ordinary_request_missing_scheme() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'GET'
		},
		quic.QpackFieldLine{
			name: ':path'
			value: '/'
		},
	]) or {
		assert err.msg().contains('mandatory pseudo-header')
		return
	}
	assert false, 'expected an error for a GET request missing :scheme'
}

fn test_h3_validate_request_pseudo_rejects_connect_until_tunnels_are_supported() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name: ':authority'
			value: 'example.com:443'
		},
	]) or {
		assert err.msg().contains('CONNECT is unsupported')
		return
	}
	assert false, 'expected CONNECT to be rejected until tunnel dispatch is supported'
}

fn test_h3_validate_request_pseudo_rejects_connect_even_with_scheme() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name: ':scheme'
			value: 'https'
		},
		quic.QpackFieldLine{
			name: ':authority'
			value: 'example.com:443'
		},
	]) or {
		assert err.msg().contains('CONNECT is unsupported')
		return
	}
	assert false, 'expected CONNECT to remain unsupported when :scheme is present'
}

fn test_h3_validate_request_pseudo_rejects_connect_even_with_path() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name: ':path'
			value: '/'
		},
		quic.QpackFieldLine{
			name: ':authority'
			value: 'example.com:443'
		},
	]) or {
		assert err.msg().contains('CONNECT is unsupported')
		return
	}
	assert false, 'expected CONNECT to remain unsupported when :path is present'
}

fn test_h3_validate_request_pseudo_rejects_connect_without_authority() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'CONNECT'
		},
	]) or {
		assert err.msg().contains('CONNECT is unsupported')
		return
	}
	assert false, 'expected CONNECT without :authority to remain unsupported'
}

fn test_h3_validate_request_pseudo_rejects_connect_with_empty_authority() {
	h3_validate_request_pseudo([
		quic.QpackFieldLine{
			name: ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name: ':authority'
		},
	]) or {
		assert err.msg().contains('CONNECT is unsupported')
		return
	}
	assert false, 'expected CONNECT with an empty :authority to remain unsupported'
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
			name: ':method'
			value: 'CONNECT'
		},
		quic.QpackFieldLine{
			name: ':protocol'
			value: 'websocket'
		},
		quic.QpackFieldLine{
			name: ':authority'
			value: 'example.com:443'
		},
	]) or { return }
	assert false, 'expected an error for a request carrying the unimplemented :protocol pseudo-header'
}

fn test_h3_build_request_rejects_unknown_method_before_get_conversion() {
	st := &H3ServerStream{
		headers: [
			quic.QpackFieldLine{
				name: ':method'
				value: 'FOO'
			},
			quic.QpackFieldLine{
				name: ':path'
				value: '/'
			},
			quic.QpackFieldLine{
				name: ':scheme'
				value: 'https'
			},
		]
	}
	h3_build_request(st) or {
		assert err.msg().contains('unsupported method')
		return
	}
	assert false, 'expected an unknown method to be rejected before conversion to GET'
}
