module main

import net.http
import os

const repair_root = os.join_path(os.vtmp_dir(), 'vpm_registry_repair_${os.getpid()}')
const repair_original_dir = os.getwd()

fn testsuite_begin() {
	os.mkdir_all(repair_root)!
	os.chdir(repair_root)!
}

fn testsuite_end() {
	os.chdir(repair_original_dir) or {}
	os.rmdir_all(repair_root) or {}
}

fn repair_archive_get(registry Registry, name string, version string) !http.Response {
	mut server := &http.Server{
		addr:                 '127.0.0.1:0'
		handler:              RegistryHandler{ registry: registry }
		show_startup_message: false
	}
	spawn server.listen_and_serve()
	server.wait_till_running()!
	defer { server.close() }
	base := 'http://127.0.0.1:${server.listener.addr()!.port()!}'
	return http.get('${base}/${name}/@v/${version}.zip')!
}

fn test_matching_retries_repair_missing_and_corrupt_archives_without_changing_metadata() {
	content := [u8(0x50), 0x4b, 0, 0xff, 0x0a].bytestr()
	archive := os.join_path(repair_root, 'verified.zip')
	os.write_file(archive, content)!
	for state in ['missing', 'corrupt'] {
		mut registry := new_registry()
		info := ModuleInfo{
			name:         'repair-${state}'
			version:      'release+build.1'
			description:  'original metadata'
			license:      'MIT'
			published_at: '2024-01-01T00:00:00Z'
			dependencies: {
				'dependency': '^1'
			}
			features:     {
				'default': ['dependency']
			}
		}
		registry.publish(info, archive)!
		assert registry.yank(info.name, info.version)
		before := registry.export_json()
		destination := artifact_path(info.name, info.version)
		if state == 'missing' {
			os.rm(destination)!
		} else {
			os.write_file(destination, 'corrupted bytes')!
		}
		assert artifact_request(registry, '/${info.name}/@v/${info.version}.zip') == none
		registry.publish(ModuleInfo{
			...info
			description:  'must not replace the original'
			license:      'BSD-2-Clause'
			published_at: '2030-01-01T00:00:00Z'
			dependencies: {
				'replacement': '^9'
			}
			features:     {
				'changed': ['replacement']
			}
			yanked:       false
		}, archive)!
		assert registry.export_json() == before
		published := registry.get_info(info.name, info.version) or { panic('publication disappeared') }
		assert published.yanked
		assert registry.changes.len == 2
		assert os.read_file(destination)! == content
		response := repair_archive_get(registry, info.name, info.version)!
		assert response.status_code == 200
		assert response.body == content
		assert registry.export_json() == before
	}
}

fn test_missing_archives_are_not_repaired_from_unverified_or_different_bytes() {
	archive := os.join_path(repair_root, 'original.zip')
	replacement := os.join_path(repair_root, 'different.zip')
	os.write_file(archive, 'the verified original')!
	os.write_file(replacement, 'different bytes')!
	info := ModuleInfo{ name: 'immutable', version: '1.0.0' }
	mut registry := new_registry()
	registry.publish(info, archive)!
	destination := artifact_path(info.name, info.version)
	os.rm(destination)!
	before := registry.export_json()
	mut refused_replacement := false
	registry.publish(info, replacement) or {
		assert err.msg().contains('already published')
		refused_replacement = true
	}
	assert refused_replacement
	mut refused_checksum := false
	registry.publish(ModuleInfo{ ...info, checksum: 'sha256:${'0'.repeat(64)}' }, archive) or {
		assert err.msg().contains('hashes to')
		refused_checksum = true
	}
	assert refused_checksum
	assert registry.export_json() == before
	assert !os.exists(destination)
}
