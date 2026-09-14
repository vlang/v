module main

import os

fn test_compiler_selection_flags_are_not_forwarded() {
	assert clean_compiler_selection_flags(['-silent', '-new-compiler', 'main.v']) == [
		'-silent',
		'main.v',
	]
	assert clean_compiler_selection_flags(['-old-compiler', 'run', 'main.v']) == [
		'run',
		'main.v',
	]
}

fn test_launcher_finds_the_source_root() {
	root := find_vroot(@FILE) or { panic(err) }
	assert os.is_file(os.join_path(root, 'GNUmakefile'))
	assert os.is_dir(os.join_path(root, 'vlib', 'v'))
	directory_root := find_vroot(root) or { panic(err) }
	assert directory_root == root
}

fn test_launcher_finds_external_commands() {
	index, command := find_command(['-cc', 'clang', 'fmt', '-w', 'main.v'])
	assert index == 2
	assert command == 'fmt'
	missing_index, missing := find_command(['-silent', 'main.v'])
	assert missing_index == -1
	assert missing == ''
	program_arg_index, program_arg := find_command(['run', 'main.v', 'fmt'])
	assert program_arg_index == -1
	assert program_arg == ''
	option_value_index, option_value := find_command(['-o', 'fmt', 'main.v'])
	assert option_value_index == -1
	assert option_value == ''
}

fn test_json_quote_escapes_report_content() {
	assert json_quote('a\n"b"\\c\t') == '"a\\n\\"b\\"\\\\c\\t"'
}

fn test_v1_fallback_installer_exposes_crypto_subtle() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	compatibility := source.all_after('install_crypto_subtle_compatibility() {').all_before('\n}')
	assert compatibility.contains('vlib/crypto/internal/subtle')
	assert compatibility.contains('vlib/crypto/subtle')
	assert compatibility.contains('aliasing.v')
	assert compatibility.contains('comparison.v')
	assert source.count('install_crypto_subtle_compatibility "$cache_root" || return 1') == 2
	assert source.contains('mkdir -p ./vlib/crypto/subtle')
	assert source.contains('mkdir .\\vlib\\crypto\\subtle')
}

fn test_v1_fallback_resolution_requires_crypto_subtle_compatibility() {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_crypto_subtle_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	fallback := os.join_path(root, v1_fallback_binary + $if windows { '.exe' } $else { '' })
	fallback_root := os.join_path(root, 'release')
	cached_fallback := os.join_path(fallback_root, 'v' + $if windows { '.exe' } $else { '' })
	os.mkdir_all(fallback_root)!
	os.cp(@VEXE, fallback)!
	os.cp(@VEXE, cached_fallback)!
	os.chmod(fallback, 0o755)!
	os.chmod(cached_fallback, 0o755)!
	os.write_file(fallback + '.vroot', fallback_root)!
	assert resolve_v1_fallback(fallback) == none
	module_dir := os.join_path(fallback_root, 'vlib', 'crypto', 'subtle')
	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'aliasing.v'), 'module subtle\n')!
	assert resolve_v1_fallback(fallback) == none
	os.write_file(os.join_path(module_dir, 'comparison.v'), 'module subtle\n')!
	assert resolve_v1_fallback(fallback) or { panic(err) } == cached_fallback
}

fn test_cached_fallback_root_is_preferred_when_installed() {
	root := find_vroot(@FILE) or { panic(err) }
	fallback := os.join_path(root, v1_fallback_binary + $if windows { '.exe' } $else { '' })
	root_file := fallback + '.vroot'
	if !os.is_executable(fallback) || !os.is_file(root_file) {
		return
	}
	resolved := ensure_v1_fallback('test') or { panic(err) }
	assert os.dir(resolved) == os.read_file(root_file)!.trim_space()
}

fn test_fallback_installer_writes_a_native_windows_root() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	writer := source.all_after('write_candidate_root() {').all_before('\n}\n\nsha256_of()')
	assert writer.contains('MSYS*|MINGW*')
	assert writer.contains('cygpath -w')
	assert writer.contains('pwd -W')
	assert source.count('write_candidate_root || return 1') == 2
}
