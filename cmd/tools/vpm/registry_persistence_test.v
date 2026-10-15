module main

import os
import json2

const persistence_root = os.join_path(os.vtmp_dir(), 'vpm_registry_persistence_${os.getpid()}')
const persistence_original_dir = os.getwd()

fn testsuite_begin() {
	os.mkdir_all(persistence_root)!
	os.chdir(persistence_root)!
}

fn testsuite_end() {
	os.chdir(persistence_original_dir) or {}
	os.rmdir_all(persistence_root) or {}
}

fn reset_persisted_registry() {
	os.rmdir_all(registry_dir()) or {}
}

fn test_loading_a_missing_registry_returns_empty() {
	reset_persisted_registry()
	missing_directory := load_registry()!
	assert missing_directory.modules.len == 0
	assert missing_directory.changes.len == 0
	assert !os.exists(registry_dir())
	os.mkdir_all(registry_dir())!
	missing_index := load_registry()!
	assert missing_index.modules.len == 0
	assert missing_index.changes.len == 0
	assert !os.exists(registry_index_path())
}

fn test_loading_a_saved_registry_preserves_its_contents() {
	reset_persisted_registry()
	mut registry := new_registry()
	registry.add_module(ModuleInfo{
		name:    'alpha'
		version: '1.0.0'
	})
	registry.save()!
	loaded := load_registry()!
	assert loaded.export_json() == registry.export_json()
	assert loaded.get_info('alpha', '1.0.0') != none
	assert loaded.changes.len == 1
}

fn test_loading_a_corrupt_existing_registry_fails_without_overwriting_it() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir())!
	corrupt := '{"modules":'
	os.write_file(registry_index_path(), corrupt)!
	load_registry() or {
		assert err.msg().contains('failed to load registry index'), err.msg()
		assert err.msg().contains('failed to parse registry JSON'), err.msg()
		assert os.read_file(registry_index_path())! == corrupt
		return
	}
	assert false, 'an existing corrupt index was treated as an empty registry'
}

fn test_loading_an_empty_existing_registry_file_fails() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir())!
	os.write_file(registry_index_path(), '')!
	load_registry() or {
		assert err.msg().contains('failed to load registry index'), err.msg()
		assert os.read_file(registry_index_path())! == ''
		return
	}
	assert false, 'an existing empty index file was treated as a missing registry'
}

fn test_loading_an_index_that_is_a_directory_fails() {
	reset_persisted_registry()
	os.mkdir_all(registry_index_path())!
	load_registry() or {
		assert err.msg().contains('failed to read registry index'), err.msg()
		assert os.is_dir(registry_index_path())
		return
	}
	assert false, 'an unreadable existing index was treated as an empty registry'
}

fn test_loading_a_registry_with_a_nondirectory_parent_fails() {
	reset_persisted_registry()
	os.write_file(registry_dir(), 'not a directory')!
	defer {
		os.rm(registry_dir()) or {}
	}
	load_registry() or {
		assert err.msg().contains('is not a directory'), err.msg()
		assert os.read_file(registry_dir())! == 'not a directory'
		return
	}
	assert false, 'a registry path blocked by a file was treated as missing'
}

fn test_loading_a_registry_with_a_dangling_directory_link_fails() {
	$if !windows {
		reset_persisted_registry()
		os.symlink(os.join_path(persistence_root, 'missing'), registry_dir())!
		defer {
			os.rm(registry_dir()) or {}
		}
		load_registry() or {
			assert err.msg().contains('failed to inspect registry directory'), err.msg()
			assert os.is_link(registry_dir())
			return
		}
		assert false, 'an existing dangling registry directory link was treated as missing'
	}
}

fn test_loading_an_existing_dangling_index_link_fails() {
	$if !windows {
		reset_persisted_registry()
		os.mkdir_all(registry_dir())!
		os.symlink(os.join_path(registry_dir(), 'missing.json'), registry_index_path())!
		load_registry() or {
			assert err.msg().contains('failed to read registry index'), err.msg()
			assert os.is_link(registry_index_path())
			return
		}
		assert false, 'an existing dangling index link was treated as a missing registry'
	}
}

fn persistence_registry_identity(key string, entry_name string, module_name string, version string) Registry {
	mut modules := map[string]RegistryEntry{}
	modules[key] = RegistryEntry{
		name:     entry_name
		versions: [ModuleInfo{ name: module_name, version: version }]
	}
	return Registry{ modules: modules }
}

fn assert_invalid_persisted_registry(registry Registry) {
	data := registry.export_json()
	mut refused_import := false
	import_json(data) or {
		assert err.msg().contains('registry'), err.msg()
		refused_import = true
	}
	assert refused_import, 'import accepted inconsistent or unroutable registry metadata'
	os.write_file(registry_index_path(), data) or { panic(err) }
	load_registry() or {
		assert err.msg().contains('failed to load registry index'), err.msg()
		stored := os.read_file(registry_index_path()) or { panic(err) }
		assert stored == data
		return
	}
	assert false, 'loading accepted invalid persisted registry metadata'
}

fn test_import_and_load_reject_unroutable_identity_fields() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir())!
	for value in ['', '.', '..', 'name.', 'name?query', 'name#fragment', 'name%2fpart', 'white space',
		'name/part', 'name\\part', 'name:part', 'control\tbyte', 'café'] {
		assert_invalid_persisted_registry(persistence_registry_identity(value, value, value, '1.0.0'))
		assert_invalid_persisted_registry(persistence_registry_identity('valid', value, 'valid', '1.0.0'))
		assert_invalid_persisted_registry(persistence_registry_identity('valid', 'valid', value, '1.0.0'))
		assert_invalid_persisted_registry(persistence_registry_identity('valid', 'valid', 'valid', value))
	}
}

fn test_import_and_load_reject_mismatched_but_individually_routable_names() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir())!
	assert_invalid_persisted_registry(persistence_registry_identity('alpha', 'beta', 'alpha', '1.0.0'))
	assert_invalid_persisted_registry(persistence_registry_identity('alpha', 'alpha', 'beta', '1.0.0'))
}

fn test_import_preserves_routable_names_and_opaque_versions() {
	registry := persistence_registry_identity('module+meta', 'module+meta', 'module+meta',
		'release_2026+build.7')
	imported := import_json(registry.export_json())!
	assert imported.export_json() == registry.export_json()
	assert imported.get_info('module+meta', 'release_2026+build.7') != none
}

fn assert_duplicate_registry_version_rejected(registry Registry, name string, version string) {
	data := registry.export_json()
	expected := 'duplicate registry version `${version}` for module `${name}`'
	mut refused_import := false
	import_json(data) or {
		assert err.msg() == expected, err.msg()
		refused_import = true
	}
	assert refused_import, 'import accepted a duplicate module/version identity'
	os.write_file(registry_index_path(), data) or { panic(err) }
	mut refused_load := false
	load_registry() or {
		assert err.msg().contains('failed to load registry index'), err.msg()
		assert err.msg().contains(expected), err.msg()
		refused_load = true
	}
	assert refused_load, 'load accepted a duplicate module/version identity'
	stored := os.read_file(registry_index_path()) or { panic(err) }
	assert stored == data, 'rejecting duplicate versions changed the invalid index'
}

fn test_import_and_load_reject_identical_duplicate_versions() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir()) or { panic(err) }
	info := ModuleInfo{
		name:     'alpha'
		version:  'release_2026+build.7'
		checksum: 'sha256:${'0'.repeat(64)}'
	}
	mut registry := new_registry()
	registry.add_module(info)
	registry.add_module(info)
	assert_duplicate_registry_version_rejected(registry, info.name, info.version)
}

fn test_import_and_load_reject_duplicate_versions_with_conflicting_metadata() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir()) or { panic(err) }
	info := ModuleInfo{
		name:     'alpha'
		version:  '1.0.0+build.7'
		checksum: 'sha256:${'0'.repeat(64)}'
	}
	for duplicate in [ModuleInfo{ ...info, yanked: true },
		ModuleInfo{ ...info, checksum: 'sha256:${'1'.repeat(64)}' },
		ModuleInfo{ ...info, yanked: true, checksum: 'sha256:${'1'.repeat(64)}' }] {
		for reverse_order in [false, true] {
			mut registry := new_registry()
			if reverse_order {
				registry.add_module(duplicate)
				registry.add_module(info)
			} else {
				registry.add_module(info)
				registry.add_module(duplicate)
			}
			assert_duplicate_registry_version_rejected(registry, info.name, info.version)
		}
	}
}

fn assert_unique_registry_version_routes(registry Registry, names []string, versions []string, active string) {
	for name in names {
		list := route(registry, 'GET', '/${name}/@v/list', map[string]string{})
		assert list.status_code == 200
		listed := json2.decode[[]string](list.body) or { panic(err) }
		assert listed.len == versions.len
		for version in versions {
			assert version in listed
			response := route(registry, 'GET', '/${name}/@v/${version}.info', map[string]string{})
			assert response.status_code == 200
			info := json2.decode[ModuleInfo](response.body) or { panic(err) }
			expected := registry.get_info(name, version) or { panic('missing unique version') }
			assert info.name == name
			assert info.version == version
			assert info.checksum == expected.checksum
			assert info.yanked == expected.yanked
		}
		latest := route(registry, 'GET', '/${name}/@latest', map[string]string{})
		assert latest.status_code == 200
		info := json2.decode[ModuleInfo](latest.body) or { panic(err) }
		assert info.name == name
		assert info.version == active
		assert !info.yanked
	}
}

fn test_import_and_load_preserve_exact_opaque_versions_and_per_module_identity() {
	reset_persisted_registry()
	names := ['alpha', 'beta']
	// Opaque spelling is preserved; case-only aliases cannot share portable paths.
	versions := ['release_2026+build.7', 'Release_2026+candidate.7', 'release_2026+build.8',
		'1.0.0+build.7', '1.0.0+build.8']
	active := 'release_2026+build.8'
	mut registry := new_registry()
	for name in names {
		for version in versions {
			registry.add_module(ModuleInfo{
				name:     name
				version:  version
				checksum: 'sha256:${sha256_hex('${name}@${version}'.bytes())}'
				yanked:   version != active
			})
		}
	}
	data := registry.export_json()
	imported := import_json(data) or { panic(err) }
	assert imported.export_json() == data
	registry.save() or { panic(err) }
	loaded := load_registry() or { panic(err) }
	assert loaded.export_json() == data
	for candidate in [imported, loaded] {
		assert_unique_registry_version_routes(candidate, names, versions, active)
	}
}

fn test_import_and_load_reject_casefold_colliding_module_names() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir())!
	for names in [['Alpha', 'alpha'], ['alpha', 'Alpha']] {
		mut registry := new_registry()
		for name in names {
			registry.add_module(ModuleInfo{ name: name, version: 'release+build.1' })
		}
		assert_invalid_persisted_registry(registry)
	}
}

fn test_import_and_load_reject_casefold_colliding_versions() {
	reset_persisted_registry()
	os.mkdir_all(registry_dir())!
	for versions in [['Release+Build.1', 'release+build.1'], ['release+build.1', 'Release+Build.1']] {
		mut registry := new_registry()
		for version in versions {
			registry.add_module(ModuleInfo{ name: 'alpha', version: version })
		}
		assert_invalid_persisted_registry(registry)
	}
}

fn assert_invalid_save_preserves_storage(registry Registry, expected string) ! {
	reset_persisted_registry()
	valid := persistence_registry_identity('retained', 'retained', 'retained', 'release+build.1')
	valid.save()!
	previous := os.read_file(registry_index_path())!
	previous_entries := os.ls(registry_dir())!
	invalid_before := registry.export_json()
	mut rejected := false
	registry.save() or {
		assert err.msg().contains(expected), err.msg()
		rejected = true
	}
	assert rejected, 'invalid in-memory registry was saved over the valid index'
	assert os.read_file(registry_index_path())! == previous
	assert os.ls(registry_dir())! == previous_entries
	loaded := load_registry()!
	assert loaded.export_json() == valid.export_json()
	assert registry.export_json() == invalid_before

	reset_persisted_registry()
	mut rejected_without_directory := false
	registry.save() or {
		assert err.msg().contains(expected), err.msg()
		rejected_without_directory = true
	}
	assert rejected_without_directory, 'invalid in-memory registry was saved into a new directory'
	assert !os.exists(registry_dir())
	assert !os.is_link(registry_dir())
	assert registry.export_json() == invalid_before
}

fn test_save_rejects_duplicate_add_module_versions_without_touching_storage() {
	mut registry := new_registry()
	info := ModuleInfo{ name: 'alpha', version: '1.0.0' }
	registry.add_module(info)
	registry.add_module(info)
	assert_invalid_save_preserves_storage(registry, 'duplicate registry version')!
}

fn test_save_rejects_direct_invalid_module_maps_without_touching_storage() {
	for registry in [
		persistence_registry_identity('invalid/name', 'invalid/name', 'invalid/name', '1.0.0'),
		persistence_registry_identity('alpha', 'beta', 'alpha', '1.0.0'),
		persistence_registry_identity('alpha', 'alpha', 'beta', '1.0.0'),
		persistence_registry_identity('alpha', 'alpha', 'alpha', '1.0.0?query'),
	] {
		assert_invalid_save_preserves_storage(registry, 'registry')!
	}
	module_aliases := Registry{
		modules: {
			'Alpha': RegistryEntry{ name: 'Alpha', versions: [ModuleInfo{ name: 'Alpha', version: '1.0.0' }] }
			'alpha': RegistryEntry{ name: 'alpha', versions: [ModuleInfo{ name: 'alpha', version: '1.0.0' }] }
		}
	}
	assert_invalid_save_preserves_storage(module_aliases, 'case-insensitive registry module collision')!
	version_aliases := Registry{
		modules: {
			'alpha': RegistryEntry{
				name:     'alpha'
				versions: [ModuleInfo{ name: 'alpha', version: 'Release+Build.1' },
					ModuleInfo{ name: 'alpha', version: 'release+build.1' }]
			}
		}
	}
	assert_invalid_save_preserves_storage(version_aliases, 'case-insensitive registry version collision')!
}
