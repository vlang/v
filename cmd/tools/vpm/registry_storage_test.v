module main

import os

const storage_root = os.join_path(os.vtmp_dir(), 'vpm_registry_storage_${os.getpid()}')
const storage_original_dir = os.getwd()

fn testsuite_begin() {
	os.mkdir_all(storage_root)!
	os.chdir(storage_root)!
}

fn testsuite_end() {
	os.chdir(storage_original_dir) or {}
	os.rmdir_all(storage_root) or {}
}

fn storage_demo_registry() Registry {
	mut registry := new_registry()
	registry.add_module(ModuleInfo{ name: 'alpha', version: '1.0.0' })
	return registry
}

fn assert_no_storage_temporary_files(directory string) {
	names := os.ls(directory) or { panic(err) }
	for name in names {
		assert !(name.starts_with('.vpm-') && name.ends_with('.tmp')), 'temporary file was left behind: ${name}'
	}
}

fn test_save_atomically_replaces_an_existing_index_and_cleans_temporary_files() {
	mut registry := storage_demo_registry()
	registry.save()!
	old_bytes := os.read_file(registry_index_path())!
	$if !windows {
		os.chmod(registry_index_path(), 0o600)!
		os.link(registry_index_path(), os.join_path(storage_root, 'old-index-link'))!
	}
	registry.add_module(ModuleInfo{ name: 'beta', version: 'release+build.1' })
	registry.save()!
	assert os.read_file(registry_index_path())! == registry.export_json()
	assert load_registry()!.modules.len == 2
	assert_no_storage_temporary_files(registry_dir())
	$if !windows {
		// A truncate-in-place write would also change this link to the old inode.
		assert os.read_file(os.join_path(storage_root, 'old-index-link'))! == old_bytes
		assert os.stat(registry_index_path())!.mode & 0o777 == 0o600
	}
}

fn test_native_temporary_creation_is_exclusive_and_preserves_an_existing_file() {
	path := os.join_path(storage_root, 'already-present.tmp')
	os.write_file(path, 'someone else owns these bytes')!
	registry_create_file(path) or {
		assert err.msg().contains('failed to create registry temporary file')
		assert os.read_file(path)! == 'someone else owns these bytes'
		return
	}
	assert false, 'temporary creation overwrote a preexisting path'
}

fn test_new_temporary_file_is_private_before_any_content_is_written() {
	path := os.join_path(storage_root, 'private-new.tmp')
	mut file := registry_create_file(path)!
	defer {
		file.close() or {}
		os.rm(path) or {}
	}
	$if !windows {
		assert os.stat(path)!.mode & 0o777 == 0o600
	}
	file.write('complete data')!
	file.close()!
	assert os.read_file(path)! == 'complete data'
}

fn storage_write_partial_then_fail(mut file RegistryTemporaryFile, content string) ! {
	file.write(content[..content.len / 2])!
	return error('injected partial write failure')
}

fn storage_close_then_fail(mut file RegistryTemporaryFile) ! {
	file.close()!
	return error('injected close failure')
}

fn storage_replace_fail(source string, target string) ! {
	assert os.dir(source) == os.dir(target)
	assert os.file_name(source).starts_with('.vpm-')
	assert os.read_file(source)! == 'a complete replacement'
	assert os.read_file(target)! == 'the old index'
	return error('injected rename failure')
}

fn test_partial_write_failure_preserves_the_old_index_and_removes_temporary_file() {
	path := os.join_path(storage_root, 'write-failure.json')
	os.write_file(path, 'the old index')!
	registry_store_file_using(path, 'a complete replacement', storage_write_partial_then_fail,
		registry_close_file, registry_replace_file) or {
		assert err.msg() == 'injected partial write failure'
		assert os.read_file(path)! == 'the old index'
		assert_no_storage_temporary_files(storage_root)
		return
	}
	assert false, 'partial write failure was ignored'
}

fn test_close_failure_preserves_the_old_index_and_removes_temporary_file() {
	path := os.join_path(storage_root, 'close-failure.json')
	os.write_file(path, 'the old index')!
	registry_store_file_using(path, 'a complete replacement', registry_write_file,
		storage_close_then_fail, registry_replace_file) or {
		assert err.msg() == 'injected close failure'
		assert os.read_file(path)! == 'the old index'
		assert_no_storage_temporary_files(storage_root)
		return
	}
	assert false, 'close failure was ignored'
}

fn test_rename_failure_preserves_the_old_index_and_removes_temporary_file() {
	path := os.join_path(storage_root, 'rename-failure.json')
	os.write_file(path, 'the old index')!
	registry_store_file_using(path, 'a complete replacement', registry_write_file,
		registry_close_file, storage_replace_fail) or {
		assert err.msg() == 'injected rename failure'
		assert os.read_file(path)! == 'the old index'
		assert_no_storage_temporary_files(storage_root)
		return
	}
	assert false, 'rename failure was ignored'
}

fn test_save_refuses_a_directory_destination_without_moving_a_file_inside_it() {
	path := registry_index_path()
	os.mkdir_all(registry_dir())!
	os.rm(path) or {}
	os.mkdir(path)!
	marker := os.join_path(path, 'old-data')
	os.write_file(marker, 'preserve existing destination contents')!
	registry := storage_demo_registry()
	registry.save() or {
		assert err.msg().contains('failed to replace registry file')
		assert os.read_file(marker)! == 'preserve existing destination contents'
		assert os.ls(path)! == ['old-data']
		assert_no_storage_temporary_files(registry_dir())
		return
	}
	assert false, 'save renamed a temporary file inside a directory destination'
}

fn test_atomic_storage_does_not_extend_the_destination_basename() {
	// Adding a UUID suffix to this already legal name would exceed NAME_MAX.
	path := os.join_path(storage_root, 'x'.repeat(240))
	$if !windows {
		registry_store_file(path, 'complete bytes')!
		assert os.read_file(path)! == 'complete bytes'
		assert_no_storage_temporary_files(storage_root)
	}
}
