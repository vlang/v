module driver

import os
import v.modulecache
import v.pref

fn test_msvc_native_objects_fit_a_nested_build_directory() ! {
	$if windows && !msvc {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_msvc_path_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// The compiler keeps Windows build directories shorter than 240 characters,
	// but an original source basename plus a unique token can still exceed MAX_PATH.
	build_dir := os.join_path(root, 'x'.repeat(239 - root.len - 1))
	os.mkdir_all(build_dir)!
	mut compiler := ''
	mut saved := []MsvcSavedVariable{}
	$if windows {
		compiler = v3_c_compiler_command_alias('msvc', 'windows')
		saved = msvc_require_cl(compiler, 'windows', pref.host_target())
	} $else {
		compiler = os.join_path(root, 'mock-msvc')
		// Model cl's output path limit on hosts where MSVC cannot run.
		os.write_file(compiler, '#!/bin/sh\nfor arg do\n case "$arg" in\n /Fo*) output=\${arg#/Fo};;\n *.c) source=$arg;;\n esac\ndone\n[ "\${#output}" -lt 260 ] || exit 1\ncp "$source" "$output"\n')!
		os.chmod(compiler, 0o755)!
	}
	defer { msvc_restore_environment(saved) }
	mut stats := CObjectCacheStats{}
	flags := CObjectFlagPlan{}
	for index in 0 .. 2 {
		source := os.join_path(root, 'psa_crypto_driver_wrappers_no_static_${index}.c')
		os.write_file(source, 'int native_value_${index}(void) { return ${index}; }\n')!
		object := compile_cached_c_source_object(source + '.o', source, 'c', &flags, false,
			false, '', [], pref.target_from('windows', 'amd64')!, compiler, false,
			build_dir, mut stats)!
		assert object.len < 260, object
		assert os.is_file(object), object
		assert stats.temporary_objects.len == index + 1
		if index == 1 {
			assert stats.temporary_objects[0] != object
		}
	}
	// C-only exports have no private build directory and start with fresh stats.
	// Their saved linker inputs must survive a later export in the same directory.
	mut exports := []string{}
	defer {
		for object in exports {
			os.rm(object) or {}
		}
	}
	mut first_object := []u8{}
	for index in 0 .. 2 {
		source := os.join_path(root, 'export.c')
		os.write_file(source, 'int exported_value(void) { return ${index}; }\n')!
		mut export_stats := CObjectCacheStats{}
		object := compile_cached_c_source_object(source + '.o', source, 'c', &flags, false,
			false, '', [], pref.target_from('windows', 'amd64')!, compiler, false, '', mut
			export_stats)!
		assert object !in exports, 'a later C-only export overwrote ${object}'
		exports << object
		assert export_stats.temporary_objects == [object]
		if index == 0 {
			first_object = os.read_bytes(object)!
			assert first_object.len > 0
		} else {
			assert os.read_bytes(exports[0])! == first_object
		}
	}
}

fn native_cache_publish_worker(root string, index int, gate chan bool) bool {
	temporary := os.join_path(root, 'writer_${index}.o')
	os.write_file(temporary, 'object from writer ${index}') or { return false }
	_ := <-gate
	return publish_native_c_object(temporary, os.join_path(root, 'published.o')) or { false }
}

fn test_native_object_publication_keeps_the_first_complete_object() ! {
	root := os.join_path(os.vtmp_dir(), 'native_cache_publish_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	gate := chan bool{cap: 16}
	mut publishers := []thread bool{}
	for index in 0 .. 16 {
		publishers << spawn native_cache_publish_worker(root, index, gate)
	}
	for _ in 0 .. 16 { gate <- true }
	results := publishers.wait()
	assert results.filter(it).len == 1
	destination := os.join_path(root, 'published.o')
	first := os.read_file(destination)!
	assert first.starts_with('object from writer ')
	late := os.join_path(root, 'late.o')
	os.write_file(late, 'different LTO symbol IDs')!
	assert !publish_native_c_object(late, destination)!
	assert os.read_file(destination)! == first
	assert os.ls(root)! == ['published.o']
}

fn test_native_object_publication_reports_failure_without_a_published_object() ! {
	root := os.join_path(os.vtmp_dir(), 'native_cache_publish_error_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	temporary := os.join_path(root, 'temporary.o')
	os.write_file(temporary, 'complete object')!
	destination := os.join_path(root, 'missing', 'published.o')
	publish_native_c_object(temporary, destination) or {
		assert !os.exists(destination)
		assert !os.exists(temporary)
		return
	}
	assert false, 'publication without a valid destination must fail'
}

fn test_native_object_file_signature_preserves_content_and_refreshes() ! {
	root := os.join_path(os.vtmp_dir(), 'native_cache_signature_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'dependency.h')
	mut stats := CObjectCacheStats{}
	assert c_object_file_signature(path, false, mut stats) == ''
	assert stats.dependency_file_reads == 0
	for size in [0, 1, 128 * 1024 + 1] {
		os.write_file_array(path, []u8{len: size, init: u8(index % 251)})!
		expected := modulecache.file_signature(path)
		previous_reads := stats.dependency_file_reads
		signature := c_object_file_signature(path, true, mut stats)
		assert signature == expected
		assert c_object_file_signature(path, false, mut stats) == signature
		assert stats.dependency_file_reads == previous_reads + 1
	}
	previous := stats.file_signatures[os.real_path(path)]
	// Same-sized edits must be detected by the post-compilation refresh.
	os.write_file_array(path, []u8{len: 128 * 1024 + 1, init: u8((index + 1) % 251)})!
	assert c_object_file_signature(path, false, mut stats) == previous
	refreshed := c_object_file_signature(path, true, mut stats)
	assert refreshed != previous
	assert refreshed == modulecache.file_signature(path)
	assert c_object_file_signature(path, false, mut stats) == refreshed
}

fn native_cache_directory_worker(path string, gate chan bool) bool {
	_ := <-gate
	ensure_native_object_cache_dir(path) or { return false }
	return os.is_dir(path)
}

fn test_native_object_cache_directory_creation_is_safe_for_parallel_builds() ! {
	root := os.join_path(os.vtmp_dir(), 'native_cache_mkdir_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for round in 0 .. 16 {
		path := os.join_path(root, 'cache_${round}')
		gate := chan bool{cap: 32}
		mut workers := []thread bool{}
		for _ in 0 .. 32 { workers << spawn native_cache_directory_worker(path, gate) }
		for _ in 0 .. 32 { gate <- true }
		results := workers.wait()
		assert results.len == 32
		assert results.all(it)
		assert os.is_dir(path)
	}
}

fn test_native_object_cache_directory_preserves_non_directory_errors() ! {
	root := os.join_path(os.vtmp_dir(), 'native_cache_not_directory_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'cache')
	os.write_file(path, 'existing file')!
	ensure_native_object_cache_dir(path) or {
		assert !os.is_dir(path)
		assert os.read_file(path)! == 'existing file'
		assert err.msg().len > 0
		return
	}
	assert false, 'a regular file must not be accepted as a cache directory'
}
