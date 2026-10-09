module main

import net.http
import os

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
	assert rsp.body.contains('2.0.0'), rsp.body
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
	content := 'the actual module sources'
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
