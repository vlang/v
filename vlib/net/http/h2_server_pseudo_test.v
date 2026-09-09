// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net

// h2_validate_request_pseudo's own doc comment covers the full RFC 9113
// §8.1.2/§8.3/§8.5 shape it enforces; these are pure, fast unit tests for
// it directly, independent of h2_server_test.v's own connection-level
// coverage -- in particular the CONNECT-specific branch, which nothing
// else in this module exercises. Mirrors h3_server_pseudo_test.v's shape
// exactly (RFC 9114 §4.1.1/§4.4 mirror RFC 9113 §8.1.2.2/§8.5 almost
// verbatim -- the same precedent h3_validate_request_pseudo's own doc
// comment already states for the outgoing side).
fn test_h2_validate_request_pseudo_accepts_an_ordinary_get() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':path', '/'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'example.com'},
	])!
}

fn test_h2_validate_request_pseudo_rejects_ordinary_request_missing_scheme() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':path', '/'},
	]) or { return }
	assert false, 'expected an error for a GET request missing :scheme'
}

// A conforming CONNECT request (RFC 9113 §8.5): :method=CONNECT,
// :authority present, :scheme and :path both OMITTED. Regression test for
// a real bug: h2_validate_request_pseudo required :path/:scheme
// unconditionally, so this exact conforming shape was rejected as
// malformed (400) and never reached the Handler -- the same bug already
// found and fixed in h3_server.v's h3_validate_request_pseudo (Codex
// review, PR #28164 pullrequestreview-5044139767); h2_server.v had the
// identical gap, left as an explicitly out-of-scope follow-up in that
// same round.
fn test_h2_validate_request_pseudo_accepts_a_conforming_connect() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'CONNECT'},
		H2HeaderField{':authority', 'example.com:443'},
	])!
}

fn test_h2_validate_request_pseudo_rejects_connect_with_scheme() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'CONNECT'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'example.com:443'},
	]) or { return }
	assert false, 'expected an error for a CONNECT request carrying :scheme'
}

fn test_h2_validate_request_pseudo_rejects_connect_with_path() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'CONNECT'},
		H2HeaderField{':path', '/'},
		H2HeaderField{':authority', 'example.com:443'},
	]) or { return }
	assert false, 'expected an error for a CONNECT request carrying :path'
}

fn test_h2_validate_request_pseudo_rejects_connect_missing_authority() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'CONNECT'},
	]) or { return }
	assert false, 'expected an error for a CONNECT request missing :authority'
}

// :authority's own match arm (above) only checks for duplication, never
// emptiness -- unlike :method/:path/:scheme, which all reject an empty
// value inline. That is fine for an ORDINARY request (RFC 9113 doesn't
// mandate a non-empty :authority when present at all), but RFC 9113 §8.5
// says a CONNECT request's :authority "MUST be provided" and "contains
// the host and port to connect to" -- an empty string satisfies neither.
// Found while reviewing this same diff's own CONNECT branch (Angle D,
// full-file read): a CONNECT request with a present-but-empty :authority
// would have set has_authority=true and slipped past the new
// !has_authority check.
fn test_h2_validate_request_pseudo_rejects_connect_with_empty_authority() {
	h2_validate_request_pseudo([
		H2HeaderField{':method', 'CONNECT'},
		H2HeaderField{':authority', ''},
	]) or { return }
	assert false, 'expected an error for a CONNECT request with an empty :authority'
}

// H2NoopPseudoTestTransport is a minimal, do-nothing H2Transport stand-in
// -- build_request (below) never touches c.transport at all (only
// s.headers/s.body), so no real pipe/socket is needed, just something
// satisfying the interface. A local, file-scoped fake rather than reusing
// h2_server_test.v's own new_pipe()/PipeEnd: each _test.v file compiles as
// its own independent unit (confirmed while writing this test -- a first
// attempt to reuse new_pipe() here failed with "unknown function", the
// same isolation h3_server_pseudo_test.v's own module doc comment already
// notes for its file), so a cross-file helper isn't visible here.
struct H2NoopPseudoTestTransport {}

fn (mut t H2NoopPseudoTestTransport) read(mut buf []u8) !int {
	return error_with_code('unused in this test', net.err_timed_out_code)
}

fn (mut t H2NoopPseudoTestTransport) write(buf []u8) !int {
	return buf.len
}

// build_request end-to-end for CONNECT: method decodes correctly and url
// stays empty (there is no :path to populate it from) -- the Handler, not
// this layer, is responsible for CONNECT-specific behavior. Mirrors
// test_h3_build_request_connect_yields_connect_method_and_empty_url
// (h3_server_pseudo_test.v).
fn test_h2_build_request_connect_yields_connect_method_and_empty_url() {
	mut c := &H2ServerConn{
		transport: H2Transport(&H2NoopPseudoTestTransport{})
	}
	s := &H2ServerStream{
		headers: [
			H2HeaderField{':method', 'CONNECT'},
			H2HeaderField{':authority', 'example.com:443'},
		]
	}
	req := c.build_request(s)!
	assert req.method == .connect
	assert req.url == ''
}
