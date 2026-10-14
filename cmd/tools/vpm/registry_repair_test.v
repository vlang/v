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

fn test_casefold_alias_publications_preserve_the_original_artifact_and_metadata() {
	original_archive := os.join_path(repair_root, 'casefold-original.zip')
	alias_archive := os.join_path(repair_root, 'casefold-alias.zip')
	os.write_file(original_archive, 'original case-sensitive identity bytes')!
	os.write_file(alias_archive, 'different alias bytes must never overwrite the original')!
	for index, pair in [[ModuleInfo{ name: 'AliasModule', version: '1.0.0' },
		ModuleInfo{ name: 'aliasmodule', version: '1.0.0' }], [ModuleInfo{ name: 'module-version', version: 'Release+Build.1' },
		ModuleInfo{ name: 'module-version', version: 'release+build.1' }]] {
		mut registry := new_registry()
		original := pair[0]
		alias := pair[1]
		registry.publish(original, original_archive)!
		assert registry.yank(original.name, original.version)
		before := registry.export_json()
		destination := artifact_path(original.name, original.version)
		before_names := os.ls(os.dir(destination))!
		before_modules := os.ls(artifacts_dir())!
		mut rejected := false
		registry.publish(alias, alias_archive) or {
			assert err.msg().contains('case-insensitive registry'), err.msg()
			rejected = true
		}
		assert rejected, 'case-only alias publication was accepted (${index})'
		assert registry.export_json() == before
		assert os.read_file(destination)! == 'original case-sensitive identity bytes'
		assert os.ls(os.dir(destination))! == before_names
		assert os.ls(artifacts_dir())! == before_modules
		response := repair_archive_get(registry, original.name, original.version)!
		assert response.status_code == 200
		assert response.body == 'original case-sensitive identity bytes'
		assert registry.export_json() == before
	}
}

fn repair_directory_sync_fail(directory string) ! {
	return error('injected repair directory synchronization failure')
}

fn test_matching_retries_replace_verified_bytes_after_uncertain_directory_synchronization() {
	archive := os.join_path(repair_root, 'durability-retry.zip')
	content := 'verified bytes survive a retry after an uncertain replacement'
	os.write_file(archive, content)!
	info := ModuleInfo{ name: 'durability-retry', version: '1.0.0' }
	mut registry := new_registry()
	registry.publish(info, archive)!
	assert registry.yank(info.name, info.version)
	before := registry.export_json()
	destination := artifact_path(info.name, info.version)
	mut uncertain := false
	registry_store_file_using(destination, content, registry_write_file, registry_sync_file,
		registry_close_file, registry_replace_file, repair_directory_sync_fail) or {
		assert err.msg().contains('was replaced')
		assert err.msg().contains('durability is uncertain')
		uncertain = true
	}
	assert uncertain
	assert os.read_file(destination)! == content
	link := os.join_path(repair_root, 'before-durability-retry')
	$if !windows {
		os.link(destination, link)!
	}
	registry.publish(info, archive)!
	assert registry.export_json() == before
	assert os.read_file(destination)! == content
	$if !windows {
		// A checksum-only early return would still share this old inode.
		os.write_file(link, 'old inode after replacement')!
		assert os.read_file(destination)! == content
	}
}
