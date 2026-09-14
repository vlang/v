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

fn test_fallback_failure_notes_name_the_stage_v_stopped_in() {
	notes := v1_fallback_failure_notes('compiler_error\nsemantic checking')
	assert notes.len == 2
	assert notes[0].contains('compatibility compiler failed too')
	assert notes[1].contains('V stopped during semantic checking')
	assert notes[1].contains('-new-compiler')
	stageless := v1_fallback_failure_notes('inline_asm')
	assert stageless[1].contains('V stopped and kept its diagnostics quiet')
}

fn test_moved_modules_are_shimmed_into_the_fallback_vlib() {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_shims_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	previous := os.join_path(root, 'vlib', 'x', 'json2')
	os.mkdir_all(os.join_path(previous, 'decoder2'))!
	os.write_file(os.join_path(previous, 'json2.v'), 'module json2\n')!
	os.write_file(os.join_path(previous, 'decoder2', 'decoder.v'), 'module decoder2\n')!

	ensure_v1_fallback_module_shims(root)
	shim := os.join_path(root, 'vlib', 'json2')
	assert os.read_file(os.join_path(shim, 'json2.v'))! == 'module json2\n'
	assert os.read_file(os.join_path(shim, 'decoder2', 'decoder.v'))! == 'module decoder2\n'
	// The original location has to stay importable as `x.json2`.
	assert os.is_dir(previous)
	// Staging directories must not survive as importable modules.
	assert os.ls(os.join_path(root, 'vlib'))!.filter(it.starts_with('.')).len == 0

	// An already shimmed tree is left untouched, so a local edit is never clobbered.
	os.write_file(os.join_path(shim, 'json2.v'), 'module json2 // kept\n')!
	ensure_v1_fallback_module_shims(root)
	assert os.read_file(os.join_path(shim, 'json2.v'))! == 'module json2 // kept\n'
}

fn test_shimming_skips_a_fallback_tree_without_the_previous_location() {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_noshims_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'vlib'))!
	ensure_v1_fallback_module_shims(root)
	assert os.ls(os.join_path(root, 'vlib'))! == []
}
