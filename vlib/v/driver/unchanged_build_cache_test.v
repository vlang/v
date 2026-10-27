module driver

import os
import crypto.sha256
import time
import v.modulecache

fn age_unchanged_build_source(path string) ! {
	// Recent whole-second mtimes cannot safely identify an unchanged source.
	timestamp := time.utc().unix() - 10
	os.utime(path, timestamp, timestamp)!
}

fn assert_unchanged_build_rejects_coarse_executable(cache_dir string, executable string, source string, source_dir string, destination string) ! {
	assert !v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
	cached_executable := os.join_path(cache_dir, 'test.exe')
	assert os.read_file(cached_executable)! == os.read_file(executable)!
	assert modulecache.file_metadata_signature(cached_executable) == ''
	assert !os.exists(os.join_path(cache_dir, 'test.stamp'))
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
}

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
	age_unchanged_build_source(source)!
	os.write_file(executable, 'executable bytes')!
	assert modulecache.file_metadata_signature(source) != ''
	has_executable_metadata := modulecache.file_metadata_signature(executable) != ''
	if has_executable_metadata {
		assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
		assert v3_restore_unchanged_build(cache_dir, 'test', destination)
		assert os.read_file(destination)! == 'executable bytes'
		os.write_file(source, 'fn main() { println("changed") }\n')!
		assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
	} else {
		assert_unchanged_build_rejects_coarse_executable(cache_dir, executable, source, source_dir,
			destination)!
	}
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
	age_unchanged_build_source(source)!
	os.write_file(executable, 'executable bytes')!
	assert modulecache.file_metadata_signature(source) != ''
	has_executable_metadata := modulecache.file_metadata_signature(executable) != ''
	if has_executable_metadata {
		assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
		os.write_file(os.join_path(source_dir, 'added.v'), 'fn added() {}\n')!
		assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
	} else {
		assert_unchanged_build_rejects_coarse_executable(cache_dir, executable, source, source_dir,
			destination)!
	}
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
	age_unchanged_build_source(source)!
	os.write_file(executable, 'executable bytes')!
	assert modulecache.file_metadata_signature(source) != ''
	has_executable_metadata := modulecache.file_metadata_signature(executable) != ''
	os.setenv('V3_TEST_NO_FILE_METADATA', source, true)
	assert !v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [root])
	os.unsetenv('V3_TEST_NO_FILE_METADATA')
	if has_executable_metadata {
		assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [root])
		os.setenv('V3_TEST_NO_FILE_METADATA', source, true)
		assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
		// Reject old receipts which recorded an empty signature as though it were valid.
		stamp := os.join_path(cache_dir, 'test.stamp')
		lines := os.read_file(stamp)!.split_into_lines()
		os.write_file(stamp, lines[..2].join('\n') + '\nfile\t${source}\t\n')!
		assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
	} else {
		assert_unchanged_build_rejects_coarse_executable(cache_dir, executable, source, root,
			destination)!
	}
}

fn test_parallel_monomorphize_requires_explicit_opt_in() {
	old := os.getenv('V3_PARALLEL_MONOMORPHIZE')
	defer { os.setenv('V3_PARALLEL_MONOMORPHIZE', old, true) }
	os.unsetenv('V3_PARALLEL_MONOMORPHIZE')
	assert !should_parallel_monomorphize()
	os.setenv('V3_PARALLEL_MONOMORPHIZE', '0', true)
	assert !should_parallel_monomorphize()
	os.setenv('V3_PARALLEL_MONOMORPHIZE', '1', true)
	$if tinyc {
		$if linux && amd64 {
			assert should_parallel_monomorphize()
		} $else {
			assert !should_parallel_monomorphize()
		}
	} $else {
		assert should_parallel_monomorphize()
	}
}

fn test_default_build_rechecks_compile_time_environment() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_env_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'app')
	os.write_file(source, "fn main() { println(\$env('V3_RECEIPT_ENV_REGRESSION')) }\n")!
	for expected in ['first', 'second'] {
		mut environment := os.environ()
		environment.delete('VFLAGS')
		environment.delete('V3_ENABLE_UNCHANGED_BUILD_CACHE')
		environment['V3CACHE'] = os.join_path(root, 'cache')
		environment['V3_RECEIPT_ENV_REGRESSION'] = expected
		environment['V_MACOS_V3_NO_FALLBACK'] = '1'
		mut compiler := os.new_process(@VEXE)
		compiler.set_environment(environment)
		compiler.set_args(['-cc', 'clang', '-o', executable, source])
		compiler.set_redirect_stdio_merged()
		compiler.run()
		output := compiler.stdout_slurp()
		compiler.wait()
		code := compiler.code
		compiler.close()
		assert code == 0, output
		mut program := os.new_process(executable)
		program.set_redirect_stdio()
		program.run()
		result := program.stdout_slurp()
		program.wait()
		program_code := program.code
		program.close()
		assert program_code == 0
		assert result.trim_space() == expected
	}
}
