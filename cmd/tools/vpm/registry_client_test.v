module main

import json2
import net.http
import os

// The initial cases exercise protocol decoding through pure functions. The
// loopback tests below also check which requests carry registry credentials.
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
// registry: the header `registry_http_get` builds from `registry_token` leaving
// the process. These tests bind a real port, send a real request through
// `registry_get` and read back the header the server received, so the assertion
// is about bytes on a socket rather than about which variable was parsed.
//
// The handler answers with the `Authorization` header it was sent instead of
// recording it in a field. The value has then crossed a socket in both
// directions, which is the thing under test, and a value read back from a
// response needs no lock against the thread that served it.
struct TokenEchoHandler {
	response_body   string
	redirect_to     string
	status_code     int = 200
	record_requests bool
	requests        chan string
}

const wire_seen_authorization_header = 'X-Vpm-Seen-Authorization'

fn (mut h TokenEchoHandler) handle(req http.Request) http.Response {
	authorization := req.header.get(.authorization) or { '' }
	if h.record_requests {
		h.requests <- authorization
	}
	mut header := http.Header{}
	header.add_custom(wire_seen_authorization_header, if authorization == '' {
		'absent'
	} else {
		authorization
	}) or { panic(err) }
	if req.url == '/redirect' && h.redirect_to != '' {
		header.add(.location, h.redirect_to)
		return http.Response{
			status_code: 302
			header:      header
		}
	}
	return http.Response{
		status_code: h.status_code
		header:      header
		body:        if h.response_body == '' { authorization } else { h.response_body }
	}
}

// RunningTokenEcho is an echo server that has finished binding its port.
struct RunningTokenEcho {
mut:
	base   string
	server &http.Server
}

fn start_token_echo() !RunningTokenEcho {
	return start_token_echo_handler(TokenEchoHandler{})
}

fn start_token_echo_handler(handler TokenEchoHandler) !RunningTokenEcho {
	return start_token_echo_handler_at(handler, '127.0.0.1:0')
}

fn start_token_echo_handler_at(handler TokenEchoHandler, addr string) !RunningTokenEcho {
	// The server is held by reference for the reason `start_registry` holds it:
	// `listen_and_serve` takes it by `mut`, and spawning a method on a by-value
	// receiver does not compile.
	mut srv := &http.Server{
		addr:                 addr
		handler:              handler
		show_startup_message: false
	}
	spawn srv.listen_and_serve()
	srv.wait_till_running()!
	return RunningTokenEcho{
		base:   'http://127.0.0.1:${srv.listener.addr()!.port()!}'
		server: srv
	}
}

// The echo server is reached as `127.0.0.1`, which encodes to the scoped name
// below. The other two are named so a case can set one without disturbing the
// next: the scoped variable is this host's, the other one is a token that
// belongs to a different registry entirely.
const wire_scoped_token_var = 'VPM_REGISTRY_TOKEN_HOST_3132372E302E302E31'
const wire_fallback_token_var = 'VPM_TOKEN'
const wire_other_host_token_var = 'VPM_REGISTRY_TOKEN_HOST_76706D2E6578616D706C652E636F6D'

// clear_wire_tokens removes every variable any case below sets, so the
// environment the suite started with cannot decide a case and one case cannot
// leak its token into the next.
fn clear_wire_tokens() {
	os.unsetenv(registry_url_env)
	os.unsetenv(registry_insecure_loopback_env)
	os.unsetenv(wire_scoped_token_var)
	os.unsetenv(wire_fallback_token_var)
	os.unsetenv(wire_other_host_token_var)
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_6C6F63616C686F7374')
	os.unsetenv('VPM_REGISTRY_TOKEN_HOST_3132372E302E302E32')
	os.unsetenv('VPM_TOKEN_127_0_0_1')
}

// authorization_on_the_wire asks the echo server for a protocol route and
// returns the `Authorization` header it received, or '' when it received none.
fn authorization_on_the_wire(base string) !string {
	return registry_get(base, '/alpha/@v/list')!.trim_space()
}

// A token scoped to the host being asked is what the server reads, so the
// scoped name is not only parsed but carried across the connection.
fn test_the_scoped_token_reaches_the_server() {
	clear_wire_tokens()
	os.setenv(registry_insecure_loopback_env, '1', true)
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

fn test_legacy_host_token_is_ignored_on_the_wire_but_encoded_token_is_sent() {
	clear_wire_tokens()
	os.setenv(registry_insecure_loopback_env, '1', true)
	mut running := start_token_echo()!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, running.base, true)
	os.setenv('VPM_TOKEN_127_0_0_1', 'unsafe-legacy-token', true)
	assert authorization_on_the_wire(running.base)! == ''
	os.setenv('VPM_REGISTRY_TOKEN_HOST_3132372E302E302E31', 'encoded-token', true)
	assert authorization_on_the_wire(running.base)! == 'Bearer encoded-token'
}

// The unqualified variable is the fallback for a machine with one private
// registry, and it is carried the same way.
fn test_the_fallback_token_reaches_the_server() {
	clear_wire_tokens()
	os.setenv(registry_insecure_loopback_env, '1', true)
	mut running := start_token_echo() or {
		assert false, 'the echo server did not start: ${err.msg()}'
		return
	}
	os.setenv(wire_fallback_token_var, 'tok-fallback', true)
	os.setenv(registry_url_env, running.base, true)
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
// `registry_http_get` adds it only when `registry_token` returns something.
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

fn test_public_transports_do_not_send_global_or_scoped_registry_tokens() {
	clear_wire_tokens()
	mut running := start_token_echo()!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, running.base, true)
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	os.setenv(wire_scoped_token_var, 'tok-scoped', true)
	for path in ['/jsmod/alpha', '/search?q=alpha', '/health', '/archives/alpha.zip', '/alpha/@v/list'] {
		url := running.base + path
		responses := [vpm_http_get(url)!, vpm_http_head(url)!, vpm_http_post(url, 'payload')!]
		for response in responses {
			assert response.status_code == 200
			seen := response.header.get_custom(wire_seen_authorization_header) or {
				panic('missing captured Authorization header')
			}
			assert seen == 'absent', path
		}
	}
}

fn test_unqualified_token_is_not_sent_without_the_matching_registry_origin() {
	clear_wire_tokens()
	os.setenv(registry_insecure_loopback_env, '1', true)
	mut running := start_token_echo()!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	assert authorization_on_the_wire(running.base)! == ''
	os.setenv(registry_url_env, running.base.replace('http://', 'https://'), true)
	assert authorization_on_the_wire(running.base)! == ''
	os.setenv(registry_url_env, running.base, true)
	assert authorization_on_the_wire(running.base)! == 'Bearer tok-global'
}

fn test_public_401_is_not_reported_as_registry_authentication() {
	clear_wire_tokens()
	mut running := start_token_echo_handler(TokenEchoHandler{ status_code: 401 })!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, running.base, true)
	os.setenv(wire_scoped_token_var, 'tok-scoped', true)
	response := vpm_http_get(running.base + '/health')!
	assert response.status_code == 401
	seen := response.header.get_custom(wire_seen_authorization_header) or {
		panic('missing captured Authorization header')
	}
	assert seen == 'absent'
	os.setenv(registry_insecure_loopback_env, '1', true)
	mut rejected := false
	registry_get(running.base, '/alpha/@v/list') or {
		rejected = true
		assert err.msg().contains('rejected the configured token'), err.msg()
	}
	assert rejected
}

fn assert_token_wire_capture(requests chan string, expected string) {
	select {
		seen := <-requests {
			assert seen == expected
		}
		else {
			assert false, 'the loopback server did not capture the expected request'
		}
	}
}

fn test_authenticated_cross_origin_redirect_never_contacts_the_target() {
	clear_wire_tokens()
	os.setenv(registry_insecure_loopback_env, '1', true)
	target_requests := chan string{cap: 4}
	source_requests := chan string{cap: 4}
	mut target := start_token_echo_handler(TokenEchoHandler{
		record_requests: true
		requests:        target_requests
	})!
	defer { target.server.close() }
	mut source := start_token_echo_handler(TokenEchoHandler{
		redirect_to:     target.base + '/capture'
		record_requests: true
		requests:        source_requests
	})!
	defer {
		source.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, source.base, true)
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	os.setenv(wire_scoped_token_var, 'tok-scoped', true)
	mut rejected := false
	registry_get(source.base, '/redirect') or {
		rejected = true
		assert err.msg().contains('different origin'), err.msg()
	}
	assert rejected
	assert_token_wire_capture(source_requests, 'Bearer tok-scoped')
	select {
		seen := <-target_requests {
			assert false, 'a rejected redirect reached the target with `${seen}`'
		}
		else {
		}
	}
}

fn test_authenticated_same_origin_redirect_retains_its_token() {
	clear_wire_tokens()
	os.setenv(registry_insecure_loopback_env, '1', true)
	requests := chan string{cap: 4}
	mut running := start_token_echo_handler(TokenEchoHandler{
		redirect_to:     '/capture'
		record_requests: true
		requests:        requests
	})!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, running.base, true)
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	assert registry_get(running.base, '/redirect')! == 'Bearer tok-global'
	assert_token_wire_capture(requests, 'Bearer tok-global')
	assert_token_wire_capture(requests, 'Bearer tok-global')
}

fn test_public_cross_origin_redirect_carries_no_registry_token() {
	clear_wire_tokens()
	requests := chan string{cap: 4}
	mut target := start_token_echo_handler(TokenEchoHandler{
		record_requests: true
		requests:        requests
	})!
	defer { target.server.close() }
	mut source := start_token_echo_handler(TokenEchoHandler{
		redirect_to: target.base + '/capture'
	})!
	defer {
		source.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, source.base, true)
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	os.setenv(wire_scoped_token_var, 'tok-scoped', true)
	response := vpm_http_get(source.base + '/redirect')!
	assert response.status_code == 200
	seen := response.header.get_custom(wire_seen_authorization_header) or {
		panic('missing captured Authorization header')
	}
	assert seen == 'absent'
	assert_token_wire_capture(requests, '')
}

fn assert_registry_wire_transport_rejected(base string) {
	mut rejected := false
	registry_get(base, '/alpha/@v/list') or {
		assert err.msg().contains('require HTTPS'), err.msg()
		rejected = true
	}
	assert rejected, 'an insecure request unexpectedly reached `${base}`'
}

fn assert_no_token_wire_requests(requests chan string) {
	select {
		seen := <-requests {
			assert false, 'a rejected HTTP peer received a request with Authorization `${seen}`'
		}
		else {
			assert true
		}
	}
}

fn test_http_loopback_bearer_is_rejected_before_network_until_explicit_opt_in() {
	clear_wire_tokens()
	requests := chan string{cap: 8}
	mut running := start_token_echo_handler(TokenEchoHandler{
		record_requests: true
		requests:        requests
	})!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	os.setenv(registry_url_env, running.base, true)
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	os.setenv(wire_scoped_token_var, 'tok-scoped', true)
	assert_registry_wire_transport_rejected(running.base)
	for value in ['', '0', 'true', 'yes', '01', '1 '] {
		os.setenv(registry_insecure_loopback_env, value, true)
		assert_registry_wire_transport_rejected(running.base)
	}
	assert_no_token_wire_requests(requests)
	os.setenv(registry_insecure_loopback_env, '1', true)
	assert authorization_on_the_wire(running.base)! == 'Bearer tok-scoped'
	assert_token_wire_capture(requests, 'Bearer tok-scoped')
}

fn test_http_dns_names_and_other_addresses_are_rejected_even_with_opt_in() {
	clear_wire_tokens()
	requests := chan string{cap: 8}
	mut running := start_token_echo_handler_at(TokenEchoHandler{
		record_requests: true
		requests:        requests
	}, '0.0.0.0:0')!
	defer {
		running.server.close()
		clear_wire_tokens()
	}
	port := running.server.listener.addr()!.port()!
	os.setenv(registry_insecure_loopback_env, '1', true)
	os.setenv(wire_fallback_token_var, 'tok-global', true)
	for host in ['localhost', '127.0.0.2'] {
		base := 'http://${host}:${port}'
		os.setenv(registry_url_env, base, true)
		assert_registry_wire_transport_rejected(base)
	}
	assert_no_token_wire_requests(requests)
	// Prove the capture peer is live using the allowed literal address.
	os.setenv(registry_url_env, running.base, true)
	assert authorization_on_the_wire(running.base)! == 'Bearer tok-global'
	assert_token_wire_capture(requests, 'Bearer tok-global')
}

fn test_fetch_registry_info_rejects_mismatched_or_missing_identities_over_http() {
	clear_wire_tokens()
	defer { clear_wire_tokens() }
	for body in [
		'{"name":"beta","version":"1.0.0"}',
		'{"name":"Alpha","version":"1.0.0"}',
		'{"name":"alpha","version":"2.0.0"}',
		'{"name":"alpha","version":"1.0"}',
		'{"name":"alpha"}',
		'{"version":"1.0.0"}',
	] {
		mut running := start_token_echo_handler(TokenEchoHandler{ response_body: body })!
		defer { running.server.close() }
		if info := fetch_registry_info(running.base, 'alpha', '1.0.0') {
			assert false, 'accepted wrong identity: ' + info.str()
		} else {
			assert err.msg().contains('alpha'), err.msg()
		}
	}
}

fn test_fetch_registry_latest_rejects_another_module_or_an_empty_version_over_http() {
	clear_wire_tokens()
	defer { clear_wire_tokens() }
	for body in [
		'{"name":"beta","version":"1.0.0"}',
		'{"name":"Alpha","version":"1.0.0"}',
		'{"name":"alpha","version":""}',
		'{"version":"1.0.0"}',
	] {
		mut running := start_token_echo_handler(TokenEchoHandler{ response_body: body })!
		defer { running.server.close() }
		if info := fetch_registry_latest(running.base, 'alpha') {
			assert false, 'accepted wrong latest identity: ' + info.str()
		} else {
			assert err.msg().contains('alpha'), err.msg()
		}
	}
}

fn test_fetch_registry_info_preserves_exact_opaque_and_build_versions_over_http() {
	clear_wire_tokens()
	defer { clear_wire_tokens() }
	for version in ['release_2026', '1.0.0+build.7'] {
		body := json2.encode(RegistryModule{ name: 'alpha', version: version })
		mut running := start_token_echo_handler(TokenEchoHandler{ response_body: body })!
		defer { running.server.close() }
		info := fetch_registry_info(running.base, 'alpha', version)!
		assert info.name == 'alpha'
		assert info.version == version
		latest := fetch_registry_latest(running.base, 'alpha')!
		assert latest.name == 'alpha'
		assert latest.version == version
		if unexpected := fetch_registry_info(running.base, 'alpha', version + '-different') {
			assert false, 'accepted different version: ' + unexpected.str()
		} else {
			assert err.msg().contains(version), err.msg()
		}
	}
}

fn test_http_registry_requires_an_explicit_https_url_instead_of_an_upgrade_redirect() {
	clear_wire_tokens()
	defer { clear_wire_tokens() }
	mut running := start_token_echo_handler(TokenEchoHandler{
		redirect_to: 'https://127.0.0.1:1/capture'
	})!
	defer { running.server.close() }
	registry_get(running.base, '/redirect') or {
		assert err.msg().contains('configure an HTTPS registry URL directly'), err.msg()
		return
	}
	assert false, 'registry accepted a scheme-changing redirect without selecting HTTPS trust roots'
}
