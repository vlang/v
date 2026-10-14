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
	file.sync()!
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
		registry_sync_file, registry_close_file, registry_replace_file, registry_sync_directory) or {
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
		registry_sync_file, storage_close_then_fail, registry_replace_file, registry_sync_directory) or {
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
		registry_sync_file, registry_close_file, storage_replace_fail, registry_sync_directory) or {
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

fn storage_sync_file_fail(mut file RegistryTemporaryFile) ! {
	file.sync()!
	return error('injected file synchronization failure')
}

fn storage_sync_directory_fail(path string) ! {
	registry_sync_directory(path)!
	return error('injected directory synchronization failure')
}

fn test_file_synchronization_failure_preserves_old_bytes_and_cleans_the_temporary_file() {
	path := os.join_path(storage_root, 'sync-file-failure.json')
	os.write_file(path, 'the old index')!
	registry_store_file_using(path, 'a complete replacement', registry_write_file,
		storage_sync_file_fail, registry_close_file, registry_replace_file,
		registry_sync_directory) or {
		assert err.msg() == 'injected file synchronization failure'
		assert os.read_file(path)! == 'the old index'
		assert_no_storage_temporary_files(storage_root)
		return
	}
	assert false, 'file synchronization failure was ignored'
}

fn test_directory_synchronization_failure_reports_the_completed_replacement() {
	path := os.join_path(storage_root, 'sync-directory-failure.json')
	os.write_file(path, 'the old index')!
	registry_store_file_using(path, 'a complete replacement', registry_write_file,
		registry_sync_file, registry_close_file, registry_replace_file,
		storage_sync_directory_fail) or {
		assert err.msg().contains('was replaced')
		assert err.msg().contains('durability is uncertain')
		assert err.msg().contains('injected directory synchronization failure')
		assert os.read_file(path)! == 'a complete replacement'
		assert_no_storage_temporary_files(storage_root)
		return
	}
	assert false, 'post-replacement directory synchronization failure was ignored'
}

fn storage_record_operation(directory string, operation string) ! {
	path := os.join_path(directory, 'operations.log')
	previous := os.read_file(path) or { '' }
	os.write_file(path, previous + operation + '\n')!
}

fn storage_ordered_write(mut file RegistryTemporaryFile, content string) ! {
	file.write(content)!
	storage_record_operation(os.dir(file.path), 'write')!
}

fn storage_ordered_sync(mut file RegistryTemporaryFile) ! {
	$if !windows {
		assert os.stat(file.path)!.mode & 0o777 == 0o640
	}
	file.sync()!
	assert os.read_file(file.path)! == 'a complete replacement'
	storage_record_operation(os.dir(file.path), 'file-sync')!
}

fn storage_ordered_close(mut file RegistryTemporaryFile) ! {
	file.close()!
	storage_record_operation(os.dir(file.path), 'close')!
}

fn storage_ordered_replace(source string, target string) ! {
	assert os.read_file(target)! == 'the old index'
	registry_replace_file(source, target)!
	storage_record_operation(os.dir(target), 'rename')!
}

fn storage_ordered_directory_sync(directory string) ! {
	assert os.read_file(os.join_path(directory, 'index.json'))! == 'a complete replacement'
	registry_sync_directory(directory)!
	storage_record_operation(directory, 'directory-sync')!
}

fn test_file_mode_and_data_are_synchronized_before_rename_then_directory_afterward() {
	directory := os.join_path(storage_root, 'synchronization-order')
	os.mkdir_all(directory)!
	path := os.join_path(directory, 'index.json')
	os.write_file(path, 'the old index')!
	$if !windows {
		os.chmod(path, 0o640)!
	}
	registry_store_file_using(path, 'a complete replacement', storage_ordered_write,
		storage_ordered_sync, storage_ordered_close, storage_ordered_replace,
		storage_ordered_directory_sync)!
	assert os.read_file(os.join_path(directory, 'operations.log'))! == 'write\nfile-sync\nclose\nrename\ndirectory-sync\n'
	assert os.read_file(path)! == 'a complete replacement'
	assert_no_storage_temporary_files(directory)
}

fn test_directory_synchronization_rejects_a_regular_file() {
	$if !windows {
		path := os.join_path(storage_root, 'not-a-directory')
		os.write_file(path, 'preserve these bytes')!
		registry_sync_directory(path) or {
			assert err.msg().contains('failed to synchronize registry directory')
			assert os.read_file(path)! == 'preserve these bytes'
			return
		}
		assert false, 'directory synchronization accepted a regular file'
	}
}

fn storage_record_created_directory_sync(directory string) ! {
	registry_sync_directory(directory)!
	path := os.join_path(storage_root, 'created-directory-sync.log')
	previous := os.read_file(path) or { '' }
	os.write_file(path, previous + directory + '\n')!
}

fn test_new_directory_creation_synchronizes_each_new_inode_and_parent_entry() {
	os.rm(os.join_path(storage_root, 'created-directory-sync.log')) or {}
	parent := os.join_path(storage_root, 'new-parent')
	child := os.join_path(parent, 'new-child')
	registry_mkdir_all_using(child, storage_record_created_directory_sync)!
	assert os.is_dir(child)
	expected := [child, parent, storage_root].join('\n') + '\n'
	assert os.read_file(os.join_path(storage_root, 'created-directory-sync.log'))! == expected
}

fn storage_created_directory_sync_fail(directory string) ! {
	assert os.is_dir(directory)
	return error('injected new directory synchronization failure')
}

fn test_new_directory_synchronization_failure_is_reported_before_storing_data() {
	path := os.join_path(storage_root, 'failed-new-directory', 'child')
	os.rm(os.join_path(storage_root, 'created-directory-sync.log')) or {}
	registry_mkdir_all_using(path, storage_created_directory_sync_fail) or {
		assert err.msg() == 'injected new directory synchronization failure'
		assert os.is_dir(path)
		assert os.ls(path)!.len == 0
		// A retry must sync parents even though the first attempt created them.
		registry_mkdir_all_using(path, storage_record_created_directory_sync)!
		parent := os.dir(path)
		expected := [path, parent, storage_root].join('\n') + '\n'
		assert os.read_file(os.join_path(storage_root, 'created-directory-sync.log'))! == expected
		return
	}
	assert false, 'new directory synchronization failure was ignored'
}
