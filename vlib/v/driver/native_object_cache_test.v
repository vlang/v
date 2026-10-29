module driver

import os
import v.modulecache

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
