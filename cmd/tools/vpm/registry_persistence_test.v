module main

import os

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
