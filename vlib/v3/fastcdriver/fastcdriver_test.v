module fastcdriver

import os

fn test_fastc_tcc_backtrace_enabled() {
	assert !fastc_tcc_backtrace_enabled('macos', 'arm64')
	assert fastc_tcc_backtrace_enabled('macos', 'amd64')
	assert fastc_tcc_backtrace_enabled('linux', 'arm64')
	assert fastc_tcc_backtrace_enabled('linux', 'amd64')
}

fn test_in_place_self_chain_survives_a_compiler_named_v2() {
	dir := os.join_path(os.temp_dir(), 'fastc_self_v2_chain_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { assert false, err.msg() }
	defer {
		os.rmdir_all(dir) or {}
	}
	compiler := os.join_path_single(dir, 'v2')
	os.write_file(compiler, 'GEN0') or { assert false, err.msg() }
	replacement := self_replacement_path(compiler)
	assert replacement != compiler
	assert os.dir(replacement) == dir
	for generation in 1 .. 3 {
		os.write_file(replacement, 'GEN${generation}') or { assert false, err.msg() }
		replace_self_compiler(compiler, replacement)
		assert os.read_file(compiler) or { '' } == 'GEN${generation}'
		assert !os.exists(replacement)
	}
	assert os.read_file(compiler) or { '' } == 'GEN2'
	assert os.read_file(os.join_path_single(dir, 'v_old')) or { '' } == 'GEN1'
	mut entries := os.ls(dir) or { []string{} }
	entries.sort()
	assert entries == ['v2', 'v_old'], entries.str()
}
