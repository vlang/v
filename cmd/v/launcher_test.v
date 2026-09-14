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

fn test_v1_fallback_installer_uses_last_v1_snapshot() {
	root := find_vroot(@FILE) or { panic(err) }
	installer := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh')) or {
		panic(err)
	}
	assert installer.contains('fallback_revision=0613e1f6fc68573f5b679e406ce58a00d6ebeb30')
	assert installer.contains('fallback_vc_revision=e658629fc4bd59826bd7637cde498d0c6236b14b')
	assert installer.contains("OLDV_VFLAGS='-d v1_fallback'")
	assert installer.contains('set -- "$@" "$fallback_revision"')
	assert !installer.contains('releases/download')
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
	assert os.is_dir(os.join_path(os.dir(resolved), 'vlib', 'json2'))
}

fn test_fallback_installer_writes_a_native_windows_root() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	writer := source.all_after('write_candidate_root() {').all_before('\n}\n\nlocal_git_repo()')
	assert writer.contains('MSYS*|MINGW*')
	assert writer.contains('cygpath -w')
	assert writer.contains('pwd -W')
	assert source.count('write_candidate_root "$candidate_root" || return 1') == 1
}
