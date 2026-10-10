module main

import json2

// These tests never bind a port. The protocol is exercised through the two pure
// functions the fetches are built from, so a failure here names a field or a
// status rule rather than a networking problem. What that leaves uncovered is the
// request itself, which route is asked for and under which token; the server side
// of that is what registry_server_test.v asserts against a bound port.
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
