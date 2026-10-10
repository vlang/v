module main

import json2
import net.http
import os

// These tests never bind a port. The protocol is exercised through the two pure
// functions the fetches are built from, so a failure here names a field or a
// status rule rather than a networking problem. What that leaves uncovered is the
// request itself, which route is asked for and under which token:
// registry_server_test.v asserts the routes against a bound port, but no test in
// this tree asserts that the `Authorization` header `vpm_http_request` builds
// actually reaches the server. The tests at the foot of this file cover that hop.
//
// The bodies below are copied from what `registry.v` puts on the wire. Keeping
// them literal rather than deriving them from `json2.encode` is the point: only a
// hand-copied body can fail when the two halves of the protocol drift.

// What `/<module>/@v/1.0.0.info` answers for a version that declares one
// dependency and one feature.
const client_info_body = '{"name":"alpha","version":"1.0.0","description":"A demo module","license":"MIT","dependencies":{"vtray":"^1.2.0"},"checksum":"sha256:deadbeef","published_at":"2024-01-01T00:00:00Z","features":{"default":["net","os"]},"yanked":false}'

const client_registry_url = 'https://vpm.example.com'

fn client_demo_info() ModuleInfo {
	return ModuleInfo{
		name:         'alpha'
		version:      '1.0.0'
		description:  'A demo module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'sha256:deadbeef'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	}
}

fn client_demo_module() RegistryModule {
	return RegistryModule{
		name:         'alpha'
		version:      '1.0.0'
		description:  'A demo module'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'sha256:deadbeef'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	}
}

// The single assertion that keeps the client's type in step with the server's.
// Both structs are encoded by the same encoder, so a field renamed, dropped or
// added on either side changes one of these two strings and fails here, whatever
// the field happens to be called.
fn test_the_client_and_server_field_names_agree() {
	server_bytes := json2.encode(client_demo_info())
	client_bytes := json2.encode(client_demo_module())
	assert server_bytes == client_bytes, 'the client decodes a different module than the server serves:\n served:  ${server_bytes}\n decoded: ${client_bytes}'
}

// The literal body must be the one the server actually emits, or the test above
// would be checking a fixture nobody serves. This is the one comparison that
// fails when `json2` changes its layout rather than when the protocol does; the
// remedy there is to re-copy the body, not to loosen the test.
fn test_the_info_body_is_the_one_the_server_encodes() {
	assert json2.encode(client_demo_info()) == '{"name":"alpha","version":"1.0.0","description":"A demo module","license":"MIT","dependencies":{},"checksum":"sha256:deadbeef","published_at":"2024-01-01T00:00:00Z","features":{},"yanked":false}', json2.encode(client_demo_info())
}

fn test_the_info_body_decodes_field_by_field() {
	info := decode_registry_module(client_registry_url, 'alpha', client_info_body) or {
		assert false, 'the info body did not decode: ${err.msg()}'
		return
	}
	assert info.name == 'alpha'
	assert info.version == '1.0.0'
	assert info.description == 'A demo module'
	assert info.license == 'MIT'
	assert info.dependencies['vtray'] == '^1.2.0'
	assert info.checksum == 'sha256:deadbeef'
	assert info.published_at == '2024-01-01T00:00:00Z'
	assert info.features['default'] == ['net', 'os']
	assert info.yanked == false
}

// The status line is the contract, and a 404 is the only failure status the
// router can produce.
fn test_a_404_answer_is_absent() {
	assert registry_answer_is_absent(404, client_info_body)
	assert registry_answer_is_absent(404, not_found_body)
}

// A registry reached through something that rewrites status lines answers the
// not-found body with a 200. Reading it as a module would hand back a struct with
// an empty name, which no caller can tell from a real one.
fn test_a_not_found_body_is_noticed_whatever_the_status() {
	assert registry_answer_is_absent(200, not_found_body)
	assert !registry_answer_is_absent(200, client_info_body)
	// A 304 is "your copy is current", which this client cannot act on because it
	// never sends a conditional request; it must be reported, not read as absence.
	assert !registry_answer_is_absent(304, '')
}

// What the fetches do with an absent answer: an error, so a caller can go on to
// the next candidate registry, rather than a module that installs nothing.
fn test_the_not_found_body_is_an_error_not_an_empty_module() {
	decode_registry_module(client_registry_url, 'alpha', not_found_body) or { return }
	assert false, 'the not-found body was decoded as a module'
}

fn test_the_not_found_body_is_not_an_empty_version_list() {
	decode_registry_versions(client_registry_url, 'alpha', not_found_body) or { return }
	assert false, 'the not-found body was decoded as a version list'
}

// An answer that is not JSON at all is a proxy's page or a truncated body, and is
// reported as such rather than as a module.
fn test_a_body_that_is_not_a_module_is_an_error() {
	decode_registry_module(client_registry_url, 'alpha', '<html>502 Bad Gateway</html>') or { return }
	assert false, 'an html error page was decoded as a module'
}

fn test_the_version_list_body_decodes_highest_first() {
	versions := decode_registry_versions(client_registry_url, 'alpha', '["2.0.0","1.5.0","1.0.0"]') or {
		assert false, 'the version list did not decode: ${err.msg()}'
		return
	}
	assert versions == ['2.0.0', '1.5.0', '1.0.0']
}

// An unknown module answers 200 with an empty array, so a registry that has never
// heard of it is a usable answer rather than a failure.
fn test_an_empty_version_list_is_not_an_error() {
	versions := decode_registry_versions(client_registry_url, 'unknown', '[]') or {
		assert false, 'an empty version list was reported as an error: ${err.msg()}'
		return
	}
	assert versions.len == 0
}

// --- the token on the wire ---

// Everything above is either a pure function or a route answered without a
// credential, so nothing has yet observed the one hop that matters for a private
// registry: the header `vpm_http_request` builds from `registry_token` leaving
// the process. These tests bind a real port, send a real request through
// `vpm_http_get` and read back the header the server received, so the assertion
// is about bytes on a socket rather than about which variable was parsed.
//
// The handler answers with the `Authorization` header it was sent instead of
// recording it in a field. The value has then crossed a socket in both
// directions, which is the thing under test, and a value read back from a
// response needs no lock against the thread that served it.
struct TokenEchoHandler {}

fn (mut h TokenEchoHandler) handle(req http.Request) http.Response {
	return http.Response{
		status_code: 200
		body:        req.header.get(.authorization) or { '' }
	}
}

// RunningTokenEcho is an echo server that has finished binding its port.
struct RunningTokenEcho {
mut:
	base   string
	server &http.Server
}

fn start_token_echo() !RunningTokenEcho {
	// The server is held by reference for the reason `start_registry` holds it:
	// `listen_and_serve` takes it by `mut`, and spawning a method on a by-value
	// receiver does not compile.
	mut srv := &http.Server{
		addr:                 ':0'
		handler:              TokenEchoHandler{}
		show_startup_message: false
	}
	spawn srv.listen_and_serve()
	srv.wait_till_running()!
	return RunningTokenEcho{
		base:   'http://127.0.0.1:${srv.listener.addr()!.port()!}'
		server: srv
	}
}

// The echo server is reached as `127.0.0.1`, which folds to the scoped name
// below. The other two are named so a case can set one without disturbing the
// next: the scoped variable is this host's, the other one is a token that
// belongs to a different registry entirely.
const wire_scoped_token_var = 'VPM_TOKEN_127_0_0_1'
const wire_fallback_token_var = 'VPM_TOKEN'
const wire_other_host_token_var = 'VPM_TOKEN_VPM_EXAMPLE_COM'

// clear_wire_tokens removes every variable any case below sets, so the
// environment the suite started with cannot decide a case and one case cannot
// leak its token into the next.
fn clear_wire_tokens() {
	os.unsetenv(wire_scoped_token_var)
	os.unsetenv(wire_fallback_token_var)
	os.unsetenv(wire_other_host_token_var)
}

// authorization_on_the_wire asks the echo server for a protocol route and
// returns the `Authorization` header it received, or '' when it received none.
fn authorization_on_the_wire(base string) !string {
	resp := vpm_http_get('${base}/alpha/@v/list')!
	return resp.body.trim_space()
}

// A token scoped to the host being asked is what the server reads, so the
// scoped name is not only parsed but carried across the connection.
fn test_the_scoped_token_reaches_the_server() {
	clear_wire_tokens()
	mut running := start_token_echo() or {
		assert false, 'the echo server did not start: ${err.msg()}'
		return
	}
	os.setenv(wire_scoped_token_var, 'tok-scoped', true)
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	seen := authorization_on_the_wire(running.base) or {
		assert false, 'the request did not reach the server: ${err.msg()}'
		return
	}
	assert seen == 'Bearer tok-scoped', 'the server received `${seen}`, not the scoped token'
}

// The unqualified variable is the fallback for a machine with one private
// registry, and it is carried the same way.
fn test_the_fallback_token_reaches_the_server() {
	clear_wire_tokens()
	mut running := start_token_echo() or {
		assert false, 'the echo server did not start: ${err.msg()}'
		return
	}
	os.setenv(wire_fallback_token_var, 'tok-fallback', true)
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	seen := authorization_on_the_wire(running.base) or {
		assert false, 'the request did not reach the server: ${err.msg()}'
		return
	}
	assert seen == 'Bearer tok-fallback', 'the server received `${seen}`, not the fallback token'
}

// With nothing configured the header is absent rather than empty, because
// `vpm_http_request` adds it only when `registry_token` returns something.
fn test_no_configured_token_sends_no_authorization_header() {
	clear_wire_tokens()
	mut running := start_token_echo() or {
		assert false, 'the echo server did not start: ${err.msg()}'
		return
	}
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	seen := authorization_on_the_wire(running.base) or {
		assert false, 'the request did not reach the server: ${err.msg()}'
		return
	}
	assert seen == '', 'the server received `${seen}` with no token configured'
}

// A token scoped to another host must not be sent here. That is the leak the
// scoped name exists to prevent, and only the wire shows it does not happen.
fn test_a_token_scoped_to_another_host_is_not_sent() {
	clear_wire_tokens()
	mut running := start_token_echo() or {
		assert false, 'the echo server did not start: ${err.msg()}'
		return
	}
	os.setenv(wire_other_host_token_var, 'tok-elsewhere', true)
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	seen := authorization_on_the_wire(running.base) or {
		assert false, 'the request did not reach the server: ${err.msg()}'
		return
	}
	assert seen == '', 'the server received `${seen}`, a token scoped to another host'
}
