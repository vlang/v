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
