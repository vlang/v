module fastcdriver

import os

fn test_fastc_tcc_backtrace_enabled() {
	assert !fastc_tcc_backtrace_enabled('macos', 'arm64')
	assert fastc_tcc_backtrace_enabled('macos', 'amd64')
	assert fastc_tcc_backtrace_enabled('linux', 'arm64')
	assert fastc_tcc_backtrace_enabled('linux', 'amd64')
}

fn assert_in_place_self_chain_survives(compiler_name string) {
	dir := os.join_path(os.temp_dir(), 'fastc_self_${compiler_name}_chain_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { assert false, err.msg() }
	defer {
		os.rmdir_all(dir) or {}
	}
	compiler := os.join_path_single(dir, compiler_name)
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
	mut entries := os.ls(dir) or { []string{} }
	entries.sort()
	backup_name := if os.user_os() == 'windows' { 'v_old.exe' } else { 'v_old' }
	if compiler_name == backup_name {
		assert entries == [compiler_name], entries.str()
	} else {
		assert os.read_file(os.join_path_single(dir, backup_name)) or { '' } == 'GEN1'
		assert entries == [compiler_name, backup_name], entries.str()
	}
}

fn test_in_place_self_chain_survives_a_compiler_named_v2() {
	assert_in_place_self_chain_survives(if os.user_os() == 'windows' { 'v2.exe' } else { 'v2' })
}

fn test_in_place_self_chain_survives_a_compiler_named_v_old() {
	assert_in_place_self_chain_survives(if os.user_os() == 'windows' {
		'v_old.exe'
	} else {
		'v_old'
	})
}
