module main

import net.http
import json2
import os

const srv_original_dir = os.getwd()
const srv_root = os.join_path(os.vtmp_dir(), 'vpm_server_tests')

fn srv_info(name string, version string, checksum string) ModuleInfo {
	return ModuleInfo{
		name:         name
		version:      version
		description:  'served over http'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     checksum
		features:     map[string][]string{}
	}
}

// RunningRegistry is a server that has finished binding its port, so a test can
// make requests against it and then stop it.
struct RunningRegistry {
mut:
	base   string
	server &http.Server
}

fn start_registry(r Registry) !RunningRegistry {
	// The server is held by reference: `listen_and_serve` takes it by `mut`, and
	// spawning a method on a by-value receiver does not compile.
	mut srv := &http.Server{
		addr:                 ':0'
		handler:              RegistryHandler{
			registry: r
		}
		show_startup_message: false
	}
	spawn srv.listen_and_serve()
	srv.wait_till_running()!
	return RunningRegistry{
		base:   'http://127.0.0.1:${srv.listener.addr()!.port()!}'
		server: srv
	}
}

fn testsuite_begin() {
	os.rmdir_all(srv_root) or {}
	os.mkdir_all(srv_root)!
	os.chdir(srv_root)!
}

fn testsuite_end() {
	os.chdir(srv_original_dir)!
	os.rmdir_all(srv_root) or {}
}

// A published module is reachable over the protocol, which is the only thing
// that makes a registry hostable.
fn test_a_registry_answers_over_http() {
	mut r := new_registry()
	archive := os.join_path(srv_root, 'a.zip')
	os.write_file(archive, 'module sources') or { panic(err) }
	r.publish(srv_info('served', '1.0.0', 'sha256:${sha256_hex('module sources'.bytes())}'), archive)!

	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/served/@latest')!
	assert rsp.status_code == 200, rsp.status_code.str()
	assert rsp.body.contains('served'), rsp.body
	assert rsp.body.contains('1.0.0'), rsp.body
}

fn test_the_version_list_is_reachable() {
	mut r := new_registry()
	archive := os.join_path(srv_root, 'b.zip')
	os.write_file(archive, 'x') or { panic(err) }
	r.publish(srv_info('listed', '1.0.0', 'sha256:${sha256_hex('x'.bytes())}'), archive)!
	r.publish(srv_info('listed', '2.0.0', 'sha256:${sha256_hex('x'.bytes())}'), archive)!

	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/listed/@v/list')!
	assert rsp.status_code == 200
	assert json2.decode[[]string](rsp.body)! == ['2.0.0', '1.0.0']
	assert rsp.header.get(.content_type) or { '' } == 'application/json'
}

// The entity tag must come back as a header, or a client cannot revalidate.
fn test_the_response_carries_an_entity_tag() {
	mut r := new_registry()
	r.add_module(srv_info('tagged', '1.0.0', ''))
	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/tagged/@latest')!
	assert rsp.status_code == 200
	etag := rsp.header.get(.etag) or {
		assert false, 'the response carried no ETag header'
		return
	}
	assert etag.len > 2, etag
	// Sending it back must produce a 304 rather than the body again.
	mut revalidate := http.Header{}
	revalidate.add_custom('If-None-Match', etag) or { panic(err) }
	second := http.fetch(url: '${base}/tagged/@latest', header: revalidate)!
	assert second.status_code == 304, second.status_code.str()
	assert second.body == '', second.body
}

fn test_an_unknown_module_is_a_404() {
	r := new_registry()
	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/missing/@latest')!
	assert rsp.status_code == 404, rsp.status_code.str()
}

// The archive route serves the module's bytes, and the sign of it working is
// that the client gets back exactly what was published.
fn test_the_archive_route_serves_the_published_bytes() {
	mut r := new_registry()
	archive := os.join_path(srv_root, 'c.zip')
	content := [u8(0x50), 0x4b, 3, 4, 0, 0xff, 0x80, 0x0a].bytestr()
	os.write_file(archive, content) or { panic(err) }
	r.publish(srv_info('archived', '1.0.0', 'sha256:${sha256_hex(content.bytes())}'), archive)!

	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/archived/@v/1.0.0.zip')!
	assert rsp.status_code == 200, rsp.status_code.str()
	assert rsp.body == content, rsp.body
}

// The SBOM and signature routes are part of the same protocol, so a hosted
// registry serves them too.
fn test_the_sbom_route_is_reachable() {
	mut r := new_registry()
	r.add_module(srv_info('bommed', '1.0.0', ''))
	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/sbom.spdx.json')!
	assert rsp.status_code == 200
	assert rsp.body.contains('SPDX-2.3'), rsp.body
}

fn test_the_config_route_is_reachable() {
	r := new_registry()
	mut running := start_registry(r)!
	defer { running.server.close() }
	base := running.base
	rsp := http.get('${base}/config.json')!
	assert rsp.status_code == 200
	assert rsp.body.contains('dl'), rsp.body
	assert rsp.body.contains('public_key'), rsp.body
}

fn test_lowercase_if_none_match_revalidates_over_http() {
	mut r := new_registry()
	r.add_module(srv_info('lowercase', '1.0.0', ''))
	mut running := start_registry(r)!
	defer { running.server.close() }
	first := http.get('${running.base}/lowercase/@latest')!
	etag := first.header.get(.etag) or { panic('missing ETag') }
	mut header := http.Header{}
	header.add_custom('if-none-match', etag)!
	assert header.render().contains('if-none-match:')
	second := http.fetch(url: '${running.base}/lowercase/@latest', header: header)!
	assert second.status_code == 304, second.status_code.str()
	assert second.body == ''
}

fn test_conditional_unknown_response_stays_404_over_http() {
	r := new_registry()
	mut running := start_registry(r)!
	defer { running.server.close() }
	url := '${running.base}/unknown/@latest'
	first := http.get(url)!
	assert first.status_code == 404
	etag := first.header.get(.etag) or { panic('missing ETag') }
	mut header := http.Header{}
	header.add(.if_none_match, etag)
	second := http.fetch(url: url, header: header)!
	assert second.status_code == 404, second.status_code.str()
	assert second.body == first.body
}

fn test_archive_transport_rechecks_the_bytes_after_routing() {
	mut r := new_registry()
	archive := os.join_path(srv_root, 'rechecked.zip')
	os.write_file(archive, 'verified bytes')!
	r.publish(srv_info('rechecked', '1.0.0', ''), archive)!
	resp := serve(r, 'GET', '/rechecked/@v/1.0.0.zip', map[string]string{}, map[string]string{})
	assert resp.artifact_path != ''
	content := read_registry_artifact(r, 'rechecked', '1.0.0', resp.artifact_path) or { panic('missing verified content') }
	assert content == 'verified bytes'
	os.write_file(resp.artifact_path, 'replaced after routing')!
	if changed := read_registry_artifact(r, 'rechecked', '1.0.0', resp.artifact_path) {
		assert false, 'transport accepted changed bytes: ${changed}'
	}
	mut h := RegistryHandler{ registry: r }
	response := h.handle(http.Request{ method: .get, url: '/rechecked/@v/1.0.0.zip' })
	assert response.status_code == 404
	assert !response.body.contains('replaced after routing')
}

fn test_registry_port_parses_the_entire_decimal_value() {
	assert parse_registry_port('1')! == 1
	assert parse_registry_port('9090')! == 9090
	assert parse_registry_port('65535')! == 65535
	mut accepted := []string{}
	for value in ['', 'bad', '0', '-1', '65536', '12345x', '12345.0', '0x2328', '9_099', '+9090'] {
		if port := parse_registry_port(value) {
			accepted << '${value} -> ${port}'
		}
	}
	assert accepted.len == 0, 'invalid ports accepted: ${accepted}'
}

fn test_registry_client_fetches_the_live_server_routes() {
	mut r := new_registry()
	for version in ['1.0.0', '2.0.0'] {
		r.add_module(ModuleInfo{
			...srv_info('client', version, 'sha256:demo')
			dependencies: {
				'dependency': '^1.0.0'
			}
			features:     {
				'default': ['net']
			}
		})
	}
	mut running := start_registry(r)!
	defer { running.server.close() }
	assert fetch_registry_versions(running.base, 'client')! == ['2.0.0', '1.0.0']
	latest := fetch_registry_latest(running.base, 'client')!
	assert latest.name == 'client'
	assert latest.version == '2.0.0'
	assert latest.dependencies['dependency'] == '^1.0.0'
	assert latest.features['default'] == ['net']
	info := fetch_registry_info(running.base, 'client', '1.0.0')!
	assert info.version == '1.0.0'
	assert info.checksum == 'sha256:demo'
}

fn test_registry_client_distinguishes_absent_modules_and_versions() {
	mut r := new_registry()
	r.add_module(srv_info('present', '1.0.0', ''))
	mut running := start_registry(r)!
	defer { running.server.close() }
	assert fetch_registry_versions(running.base, 'absent')! == []string{}
	if info := fetch_registry_latest(running.base, 'absent') {
		assert false, 'absent module decoded as ${info}'
	}
	if info := fetch_registry_info(running.base, 'present', '9.0.0') {
		assert false, 'absent version decoded as ${info}'
	}
}
