module driver

import os
import crypto.sha256

fn test_file_sha256_streams_large_inputs() {
	root := os.join_path(os.vtmp_dir(), 'v3_file_sha256_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(root)!
	path := os.join_path(root, 'large.bin')
	content := []u8{len: 256 * 1024, init: u8(`v`)}
	os.write_file_array(path, content)!
	digest := v3_file_sha256_hex(path)?
	assert digest == sha256.sum(content).hex()
}

fn test_unchanged_build_cache_restores_and_invalidates_source_change() {
	root := os.join_path(os.vtmp_dir(), 'v3_unchanged_build_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	source_dir := os.join_path(root, 'source')
	cache_dir := os.join_path(root, 'cache')
	source := os.join_path(source_dir, 'main.v')
	executable := os.join_path(root, 'app')
	destination := os.join_path(root, 'restored')
	os.mkdir_all(source_dir)!
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(executable, 'executable bytes')!
	assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
	assert v3_restore_unchanged_build(cache_dir, 'test', destination)
	assert os.read_file(destination)! == 'executable bytes'
	os.write_file(source, 'fn main() { println("changed") }\n')!
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
}

fn test_unchanged_build_cache_invalidates_new_source_file() {
	root := os.join_path(os.vtmp_dir(), 'v3_unchanged_build_new_file_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	source_dir := os.join_path(root, 'source')
	cache_dir := os.join_path(root, 'cache')
	source := os.join_path(source_dir, 'main.v')
	executable := os.join_path(root, 'app')
	destination := os.join_path(root, 'restored')
	os.mkdir_all(source_dir)!
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(executable, 'executable bytes')!
	assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
	os.write_file(os.join_path(source_dir, 'added.v'), 'fn added() {}\n')!
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
}

fn test_unchanged_build_cache_requires_explicit_opt_in() {
	old_enable := os.getenv('V3_ENABLE_UNCHANGED_BUILD_CACHE')
	old_disable := os.getenv('V3_DISABLE_UNCHANGED_BUILD_CACHE')
	defer {
		os.setenv('V3_ENABLE_UNCHANGED_BUILD_CACHE', old_enable, true)
		os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', old_disable, true)
	}
	os.unsetenv('V3_ENABLE_UNCHANGED_BUILD_CACHE')
	os.unsetenv('V3_DISABLE_UNCHANGED_BUILD_CACHE')
	assert !v3_unchanged_build_cache_opted_in(false)
	os.setenv('V3_ENABLE_UNCHANGED_BUILD_CACHE', '1', true)
	assert v3_unchanged_build_cache_opted_in(false)
	assert !v3_unchanged_build_cache_opted_in(true)
	os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', '1', true)
	assert !v3_unchanged_build_cache_opted_in(false)
}

fn test_unchanged_build_cache_rejects_unavailable_metadata() {
	root := os.join_path(os.vtmp_dir(), 'v3_unchanged_build_no_metadata_${os.getpid()}')
	old_disabled := os.getenv('V3_TEST_NO_FILE_METADATA')
	defer {
		os.setenv('V3_TEST_NO_FILE_METADATA', old_disabled, true)
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'app')
	cache_dir := os.join_path(root, 'cache')
	destination := os.join_path(root, 'restored')
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(executable, 'executable bytes')!
	os.setenv('V3_TEST_NO_FILE_METADATA', source, true)
	assert !v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [root])
	os.unsetenv('V3_TEST_NO_FILE_METADATA')
	assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [root])
	os.setenv('V3_TEST_NO_FILE_METADATA', source, true)
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
	// Reject old receipts which recorded an empty signature as though it were valid.
	stamp := os.join_path(cache_dir, 'test.stamp')
	lines := os.read_file(stamp)!.split_into_lines()
	os.write_file(stamp, lines[..2].join('\n') + '\nfile\t${source}\t\n')!
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
}
