module main

import os

// The registry stores under the working directory, so these tests chdir into
// their own tree rather than leaving `.vpm-registry` behind in the checkout.
const surface_root = os.join_path(os.vtmp_dir(), 'vpm_surface_tests')
const surface_original_dir = os.getwd()

fn surface_info(name string, version string, checksum string) ModuleInfo {
	return ModuleInfo{
		name:         name
		version:      version
		description:  'demo'
		license:      'MIT'
		dependencies: map[string]string{}
		checksum:     checksum
		features:     map[string][]string{}
	}
}

fn write_archive(content string) string {
	path := os.join_path(surface_root, 'archive_${content.len}.zip')
	os.write_file(path, content) or { panic(err) }
	return path
}

fn testsuite_begin() {
	os.rmdir_all(surface_root) or {}
	os.mkdir_all(surface_root)!
	os.chdir(surface_root)!
}

fn testsuite_end() {
	os.chdir(surface_original_dir) or {}
	os.rmdir_all(surface_root) or {}
}

// publish refuses an archive whose bytes do not match the claimed checksum.
// A registry that accepted it would hand out sources nobody vouched for.
fn test_publish_refuses_a_mismatched_checksum() {
	mut r := new_registry()
	archive := write_archive('the real bytes')
	claimed := 'sha256:${'0'.repeat(64)}'
	r.publish(surface_info('alpha', '1.0.0', claimed), archive) or {
		assert err.msg().contains('hashes to'), err.msg()
		return
	}
	assert false, 'publish accepted an archive that does not match its checksum'
}

fn test_publish_records_a_checksum_it_did_not_claim() {
	mut r := new_registry()
	archive := write_archive('the real bytes')
	r.publish(surface_info('alpha', '1.0.0', ''), archive)!
	info := r.get_info('alpha', '1.0.0') or {
		assert false, 'no version was recorded'
		return
	}
	// The bytes are the truth when nothing was claimed.
	assert info.checksum.trim_string_left('sha256:').len == sha256_hex('the real bytes'.bytes()).len, 'no checksum was recorded: `${info.checksum}`'
}

// A published version is immutable: replacing it would change what an existing
// lockfile points at. The second publish is self-consistent — its own bytes
// match its own checksum — so it reaches the immutability check rather than
// failing the integrity one.
fn test_publish_refuses_to_replace_a_version() {
	mut r := new_registry()
	first := write_archive('first bytes')
	checksum := 'sha256:${sha256_hex('first bytes'.bytes())}'
	r.publish(surface_info('alpha', '1.0.0', checksum), first)!
	second := write_archive('different bytes entirely')
	newer := 'sha256:${sha256_hex('different bytes entirely'.bytes())}'
	r.publish(surface_info('alpha', '1.0.0', newer), second) or {
		assert err.msg().contains('already published'), err.msg()
		return
	}
	assert false, 'publish replaced a version that was already published'
}

// Re-publishing identical bytes is a no-op rather than an error, so a retry
// after a partial failure is safe.
fn test_republishing_identical_bytes_is_a_no_op() {
	mut r := new_registry()
	archive := write_archive('same bytes')
	checksum := 'sha256:${sha256_hex('same bytes'.bytes())}'
	r.publish(surface_info('alpha', '1.0.0', checksum), archive)!
	r.publish(surface_info('alpha', '1.0.0', checksum), archive)!
	assert r.changes.len == 1, 'a no-op republish recorded a change'
}

// publish stores the archive where the download route finds it.
fn test_the_artifact_endpoint_serves_a_published_archive() {
	mut r := new_registry()
	archive := write_archive('module sources')
	checksum := 'sha256:${sha256_hex('module sources'.bytes())}'
	r.publish(surface_info('alpha', '1.0.0', checksum), archive)!

	resp := artifact_request(r, '/alpha/@v/1.0.0.zip') or {
		assert false, 'the artifact endpoint did not serve a published archive'
		return
	}
	assert os.read_file(resp.artifact_path)! == 'module sources'
	assert resp.body == 'module sources'
}

fn test_the_artifact_endpoint_refuses_an_unpublished_version() {
	r := new_registry()
	assert artifact_request(r, '/alpha/@v/9.9.9.zip') == none
}

fn test_the_artifact_endpoint_ignores_a_non_artifact_path() {
	r := new_registry()
	assert artifact_request(r, '/alpha/@latest') == none
	assert artifact_request(r, '/alpha/@v/list') == none
	assert artifact_request(r, '/alpha/@v/1.0.0.info') == none
}

// An archive carries verified buffered bytes, while artifact routes retain their
// separate conditional-request policy and never return a 304 response.
fn test_serve_returns_verified_artifact_bytes_without_an_entity_tag() {
	mut r := new_registry()
	archive := write_archive('module sources')
	checksum := 'sha256:${sha256_hex('module sources'.bytes())}'
	r.publish(surface_info('alpha', '1.0.0', checksum), archive)!

	resp := serve(r, 'GET', '/alpha/@v/1.0.0.zip', map[string]string{}, map[string]string{})
	assert resp.artifact_path != '', 'a zip route lost its artifact marker'
	assert resp.body == 'module sources'
	assert resp.status_code == 200
	assert resp.etag == '', 'an artifact carried an entity tag'
	conditional := serve(r, 'GET', '/alpha/@v/1.0.0.zip', map[string]string{}, {
		'If-None-Match': '*'
	})
	assert conditional.status_code == 200
	assert conditional.body == 'module sources'
	assert conditional.etag == ''
}

// The bytes on disk are checked against the index before serving, so a store
// tampered with after publishing is caught here rather than at the client.
fn test_the_artifact_endpoint_refuses_a_tampered_store() {
	mut r := new_registry()
	archive := write_archive('module sources')
	checksum := 'sha256:${sha256_hex('module sources'.bytes())}'
	r.publish(surface_info('alpha', '1.0.0', checksum), archive)!

	os.write_file(artifact_path('alpha', '1.0.0'), 'replaced with something else') or { panic(err) }
	assert artifact_request(r, '/alpha/@v/1.0.0.zip') == none, 'a tampered store was served rather than refused'
}

fn test_publish_rejects_artifact_paths_outside_the_store() {
	archive := write_archive('module sources')
	for name in ['..', '.. ', 'name.', '../outside', '..\\outside', '/outside', 'C:\\outside',
		''] {
		mut r := new_registry()
		r.publish(surface_info(name, '1.0.0', ''), archive) or {
			assert r.modules.len == 0
			continue
		}
		assert false, 'unsafe module name `${name}` was published'
	}
	for version in ['', '..', '.. ', '1.0.0.', '../outside', '..\\outside', 'C:\\outside'] {
		mut r := new_registry()
		r.publish(surface_info('alpha', version, ''), archive) or {
			assert r.modules.len == 0
			continue
		}
		assert false, 'unsafe version `${version}` was published'
	}
}

fn test_publish_rejects_components_that_cannot_round_trip_in_raw_http_paths() {
	archive := write_archive('module sources')
	mut invalid := ['name?query', 'name#fragment', 'name%2Fpart', 'name%25part', 'name%ZZ',
		'white space', ' leading', 'trailing ', 'quote"', 'angle<', 'angle>', 'pipe|', 'caret^',
		'brace{', 'brace}', 'tick`', 'café', 'space\u00a0here']
	for code in 0 .. 33 {
		byte := [u8(code)].bytestr()
		invalid << 'before${byte}after'
	}
	invalid << 'before${[u8(127)].bytestr()}after'
	for value in invalid {
		for info in [surface_info(value, '1.0.0', ''), surface_info('valid', value, '')] {
			mut registry := new_registry()
			before := registry.export_json()
			registry.publish(info, archive) or {
				assert err.msg().contains('artifact path components'), err.msg()
				assert registry.export_json() == before
				continue
			}
			assert false, 'an unroutable name or version was published: ${value.bytes()}'
		}
	}
}

fn test_artifact_components_preserve_routable_punctuation_and_opaque_versions() {
	for value in ["module-._~!$&'()*+,;=@[]", 'release_2026+build.7', 'not-a-semver'] {
		assert registry_artifact_component(value), value
	}
}

fn test_artifact_endpoint_requires_metadata_for_existing_files() {
	r := new_registry()
	path := artifact_path('orphan', '1.0.0')
	os.mkdir_all(os.dir(path))!
	os.write_file(path, 'unpublished bytes')!
	assert artifact_request(r, '/orphan/@v/1.0.0.zip') == none
	assert artifact_request(r, '/../@v/1.0.0.zip') == none
}

fn test_artifact_endpoint_accepts_an_uppercase_checksum_claim() {
	mut r := new_registry()
	archive := write_archive('module sources')
	r.publish(surface_info('alpha', '1.0.0', 'SHA256:${sha256_hex('module sources'.bytes())}'), archive)!
	assert artifact_request(r, '/alpha/@v/1.0.0.zip') != none
}

fn test_artifact_endpoint_requires_a_get_request() {
	mut r := new_registry()
	archive := write_archive('module sources')
	r.publish(surface_info('alpha', '1.0.0', 'sha256:${sha256_hex('module sources'.bytes())}'), archive)!
	response := serve(r, 'POST', '/alpha/@v/1.0.0.zip', map[string]string{}, map[string]string{})
	assert response.artifact_path == ''
}

fn test_artifact_endpoint_requires_a_recorded_checksum() {
	mut r := new_registry()
	archive := write_archive('module sources')
	r.publish(surface_info('alpha', '1.0.0', 'sha256:${sha256_hex('module sources'.bytes())}'), archive)!
	r.modules['alpha'].versions[0] = surface_info('alpha', '1.0.0', '')
	assert artifact_request(r, '/alpha/@v/1.0.0.zip') == none
}

fn test_checksumless_publish_preserves_metadata_and_can_be_retried() {
	mut r := new_registry()
	archive := write_archive('module sources')
	info := ModuleInfo{
		...surface_info('alpha', '1.0.0', '')
		published_at: '2024-01-01T00:00:00Z'
		yanked:       true
	}
	r.publish(info, archive)!
	published := r.get_info('alpha', '1.0.0') or { panic('publication was not recorded') }
	assert published.published_at == info.published_at
	assert published.yanked
	r.publish(info, archive)!
	assert r.changes.len == 1
}

fn test_accepted_checksum_spellings_reach_spdx_and_preserve_immutability() {
	archive := write_archive('verified sources')
	actual := sha256_hex('verified sources'.bytes())
	for claimed in [actual, 'SHA256:${actual}', ''] {
		mut r := new_registry()
		info := surface_info('normalized', '1.0.0', claimed)
		r.publish(info, archive)!
		document := r.spdx_document('https://example.com/spdx', 'https://example.com/downloads')
		assert document.packages.len == 1
		assert document.packages[0].checksums.len == 1, 'SPDX lost checksum `${claimed}`'
		assert document.packages[0].checksums[0].algorithm == 'SHA256'
		assert document.packages[0].checksums[0].checksum_value == actual
		published := r.get_info('normalized', '1.0.0') or { panic('publication was not recorded') }
		assert published.checksum == 'sha256:${actual}'
		before := r.export_json()
		for retry in ['', actual, 'sha256:${actual}', 'SHA256:${actual}'] {
			r.publish(surface_info('normalized', '1.0.0', retry), archive)!
			assert r.export_json() == before
		}
		replacement := write_archive('replacement sources')
		r.publish(surface_info('normalized', '1.0.0', ''), replacement) or {
			assert err.msg().contains('already published')
			assert r.export_json() == before
			assert os.read_file(artifact_path('normalized', '1.0.0'))! == 'verified sources'
			continue
		}
		assert false, 'replacement bytes overwrote a published version'
	}
}
