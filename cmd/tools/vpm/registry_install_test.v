module main

import json2
import net.http
import os

// The registry is one metadata source among several, so these tests answer each
// source in-process on loopback and record every path it was asked for. What
// proves the wiring is not that a module resolves, but which source answered
// which route, and in which order.

// SourceLog records the paths a test endpoint was asked for. It is held by
// reference, because the server answers requests from a copy of the handler it
// was given and a copy would otherwise record into a struct nobody reads.
struct SourceLog {
mut:
	asked []string
}

// SourceHandler answers the routes of one metadata source and 404s anything
// else, which is both what a server that does not hold a module does and what a
// host that does not speak a route at all does.
struct SourceHandler {
	paths map[string]string
mut:
	log &SourceLog
}

fn (mut h SourceHandler) handle(req http.Request) http.Response {
	path := req.url.all_before('?')
	h.log.asked << path
	body := h.paths[path] or {
		return http.new_response(http.ResponseConfig{
			status: .not_found
			body:   '{"error": "not found"}'
		})
	}
	return http.new_response(http.ResponseConfig{
		status: .ok
		body:   body
	})
}

// RunningSource is a test endpoint that has finished binding its port.
struct RunningSource {
	base   string
	server &http.Server
	log    &SourceLog
}

fn start_source(paths map[string]string) !RunningSource {
	mut log := &SourceLog{}
	// The server is held by reference: `listen_and_serve` takes it by `mut`, and
	// spawning a method on a by-value receiver does not compile.
	mut srv := &http.Server{
		addr:                 ':0'
		handler:              SourceHandler{
			paths: paths
			log:   log
		}
		show_startup_message: false
	}
	spawn srv.listen_and_serve()
	srv.wait_till_running()!
	return RunningSource{
		base:   'http://127.0.0.1:${srv.listener.addr()!.port()!}'
		server: srv
		log:    log
	}
}

// vpm_module_body is what `/api/packages/<name>` answers: the metadata the
// existing install path clones a repository from.
fn vpm_module_body(name string, url string) string {
	return json2.encode(ModuleVpmInfo{
		name:         name
		url:          url
		vcs:          'git'
		nr_downloads: 7
	})
}

fn registry_module_body(name string, version string) string {
	return json2.encode(RegistryModule{
		name:         name
		version:      version
		description:  'served by a registry'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     'sha256:demo'
		published_at: '2024-01-01T00:00:00Z'
		features:     map[string][]string{}
	})
}

// What `registry.v` serves at `/config.json` today: fixed placeholder values
// for `dl` and `api`, so the archive base of the reference registry is not one.
const placeholder_config_body = '{"dl":"https://example.com/downloads","api":"https://example.com/api","auth_required":false,"public_key":""}'

const served_config_body = '{"dl":"https://artifacts.alpharegistry.io/modules","api":"https://alpharegistry.io/api","auth_required":false,"public_key":""}'

const registry_test_module_url = 'https://github.com/example/alpha'

// resolve_from_sources resolves `name` with `registry_base` as the only
// configured registry and `selector` naming the vpm servers, and restores the
// environment afterwards: a resolution records the server it used there, and a
// recorded port this test has closed by now would send a later test looking for
// a server that is gone.
fn resolve_from_sources(name string, registry_base string, mut selector VpmInstallServerSelector) !ModuleVpmInfo {
	saved_registry := os.getenv_opt(registry_url_env) or { '' }
	saved_selected := os.getenv(selected_server_url_env)
	defer {
		os.setenv(registry_url_env, saved_registry, true)
		os.setenv(selected_server_url_env, saved_selected, true)
	}
	if registry_base == '' {
		os.unsetenv(registry_url_env)
	} else {
		os.setenv(registry_url_env, registry_base, true)
	}
	// The selected server starts unset, or the loop prefers a server an earlier
	// resolution recorded instead of the candidates this one gave it.
	os.setenv(selected_server_url_env, '', true)
	return get_mod_vpm_info_with_selector(name, mut selector)
}

// A registry that answers `200` with an empty version list has never heard of
// the module. Reading that as a hit would pin the registry that answered first
// as the source of a module it does not hold, and the install would then fail
// naming neither a module nor a registry.
fn test_an_empty_version_list_is_not_a_hit() {
	mut registry := start_source({
		'/alpha/@v/list': '[]'
	})!
	defer {
		registry.server.close()
	}
	mut server := start_source({
		'/api/packages/alpha': vpm_module_body('alpha', registry_test_module_url)
	})!
	defer {
		server.server.close()
	}
	mut selector := VpmInstallServerSelector{
		candidate_urls: [server.base]
	}
	mod := resolve_from_sources('alpha', registry.base, mut selector) or {
		assert false, 'a registry that does not hold the module stopped the resolution: ${err.msg()}'
		return
	}
	assert mod.url == registry_test_module_url, mod.url
	// The registry was asked, and the server that holds the module answered.
	assert registry.log.asked == ['/alpha/@v/list'], registry.log.asked.join(', ')
	assert server.log.asked == ['/api/packages/alpha'], server.log.asked.join(', ')
	assert selector.selected_url == server.base, selector.selected_url
}

// A host that answers 404 for the version list does not speak the registry
// protocol at all. That is a miss rather than a failure: the resolution has to
// reach the servers that do answer, or a registry that is merely not a registry
// would make every module uninstallable.
fn test_a_registry_404_does_not_abort_the_resolution() {
	mut absent := start_source(map[string]string{})!
	defer {
		absent.server.close()
	}
	mut server := start_source({
		'/api/packages/alpha': vpm_module_body('alpha', registry_test_module_url)
	})!
	defer {
		server.server.close()
	}
	mut selector := VpmInstallServerSelector{
		candidate_urls: [server.base]
	}
	mod := resolve_from_sources('alpha', absent.base, mut selector) or {
		assert false, 'a registry that does not speak the protocol stopped the resolution: ${err.msg()}'
		return
	}
	assert mod.url == registry_test_module_url, mod.url
	assert absent.log.asked == ['/alpha/@v/list'], absent.log.asked.join(', ')
	assert server.log.asked == ['/api/packages/alpha'], server.log.asked.join(', ')
}

// With no registry configured the resolution asks exactly the route it asked
// before registries existed: one GET of `/api/packages/<name>` against the
// selected server, and no request for a registry route at all. A registry route
// asked here would mean an unset `VPM_REGISTRY` is not the no-registry case it
// is documented to be.
fn test_no_registry_keeps_the_existing_call_sequence() {
	mut server := start_source({
		'/api/packages/alpha': vpm_module_body('alpha', registry_test_module_url)
	})!
	defer {
		server.server.close()
	}
	mut selector := VpmInstallServerSelector{
		candidate_urls: [server.base]
	}
	mod := resolve_from_sources('alpha', '', mut selector) or {
		assert false, 'resolving a module with no registry configured failed: ${err.msg()}'
		return
	}
	assert mod.name == 'alpha', mod.name
	assert mod.url == registry_test_module_url, mod.url
	assert server.log.asked == ['/api/packages/alpha'], server.log.asked.join(', ')
	assert selector.selected_url == server.base, selector.selected_url
}

// A registry that holds the module but names a placeholder archive base had it
// and still cannot say where its sources are. The resolution has to say that,
// naming the module, instead of falling back to cloning the base as if it were
// a repository.
fn test_a_placeholder_archive_base_fails_clearly() {
	mut registry := start_source({
		'/alpha/@v/list':       '["1.0.0"]'
		'/alpha/@v/1.0.0.info': registry_module_body('alpha', '1.0.0')
		'/config.json':         placeholder_config_body
	})!
	defer {
		registry.server.close()
	}
	mut server := start_source(map[string]string{})!
	defer {
		server.server.close()
	}
	mut selector := VpmInstallServerSelector{
		candidate_urls: [server.base]
	}
	resolve_from_sources('alpha', registry.base, mut selector) or {
		assert err.msg().contains('alpha@1.0.0'), err.msg()
		assert err.msg().contains('placeholder archive base'), err.msg()
		// Version list, metadata, then configuration: that order is what tells a
		// registry that holds the module from one that missed.
		assert registry.log.asked == ['/alpha/@v/list', '/alpha/@v/1.0.0.info', '/config.json'], registry.log.asked.join(', ')
		return
	}
	assert false, 'a registry whose archive base is a placeholder was read as installable'
}

// A real archive base is still not an install, because nothing here downloads
// and unpacks an archive. The resolution reports that and names the base,
// because inventing a download path would point `v install` at a url the
// protocol never promised.
fn test_a_real_archive_base_is_reported_rather_than_invented() {
	mut registry := start_source({
		'/alpha/@v/list':       '["1.0.0"]'
		'/alpha/@v/1.0.0.info': registry_module_body('alpha', '1.0.0')
		'/config.json':         served_config_body
	})!
	defer {
		registry.server.close()
	}
	mut server := start_source(map[string]string{})!
	defer {
		server.server.close()
	}
	mut selector := VpmInstallServerSelector{
		candidate_urls: [server.base]
	}
	resolve_from_sources('alpha', registry.base, mut selector) or {
		assert err.msg().contains('alpha@1.0.0'), err.msg()
		assert err.msg().contains('https://artifacts.alpharegistry.io/modules'), err.msg()
		return
	}
	assert false, 'a registry archive base was turned into an installable source'
}
