module c

import os
import v3.parser
import v3.pref

fn test_c_directive_targets_use_requested_platform() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	assert c_flag_args('macos -DMACOS', '', '', target) == ['-DMACOS']
	assert c_flag_args('arm64 -DARM64', '', '', target) == ['-DARM64']
	assert c_flag_args('linux -DLINUX', '', '', target).len == 0
	assert c_flag_args('amd64 -DAMD64', '', '', target).len == 0
	assert c_include_arg_for_target('macos <TargetConditionals.h>', '', '', target) == '<TargetConditionals.h>'
	assert c_include_arg_for_target('windows <windows.h>', '', '', target) == ''
}

fn test_termux_c_directive_target_is_distinct_from_android() {
	termux := pref.target_from('termux', 'arm64') or { panic(err) }
	android := pref.target_from('android', 'arm64') or { panic(err) }
	assert c_flag_args('termux -DTERMUX', '', '', termux) == ['-DTERMUX']
	assert c_flag_args('termux -DTERMUX', '', '', android).len == 0
}

fn test_emscripten_c_directive_target_is_distinct_from_host() {
	wasm := pref.target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	assert c_flag_args('wasm32_emscripten --embed-file asset.txt', '', '', wasm) == [
		'--embed-file',
		'asset.txt',
	]
	assert c_flag_args('wasm32_emscripten --embed-file asset.txt', '', '', linux).len == 0
}

fn test_split_relative_c_flag_paths_resolve_from_source_directory() {
	source_dir := os.join_path(os.vtmp_dir(), 'v3_split_c_flag_paths', 'source')
	source_file := os.join_path(source_dir, 'main.v')
	include_dir := os.real_path(os.join_path(source_dir, 'include dir'))
	lib_dir := os.real_path(os.join_path(source_dir, 'lib'))
	flags := c_flag_args('-I "include dir" -L lib -DVALUE=1', '', source_file, pref.host_target())
	assert flags == [
		'-I',
		include_dir,
		'-L',
		lib_dir,
		'-DVALUE=1',
	]
	assert c_flag_include_dirs(flags) == [include_dir]
}

fn test_split_forced_include_flags_are_cache_inputs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_split_forced_include_flags')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	cfg := os.join_path(dir, 'cfg header.h')
	defs := os.join_path(dir, 'defs.h')
	os.write_file(cfg, '#define CFG_VALUE 1\n') or { panic(err) }
	os.write_file(defs, '#define DEFS_VALUE 2\n') or { panic(err) }
	flags := c_flag_args('-include "./cfg header.h" -imacros ./defs.h', '', os.join_path(dir,
		'main.v'), pref.host_target())
	assert flags == ['-include', os.real_path(cfg), '-imacros', os.real_path(defs)]
	mut expected := [os.real_path(cfg), os.real_path(defs)]
	expected.sort()
	assert cache_c_flag_input_files(flags) == expected
}

fn test_termux_comptime_branch_uses_canonical_target() {
	dir := os.join_path(os.vtmp_dir(), 'v3_termux_comptime_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source,
		'module main\n\n\$if termux {\nfn termux_selected() {}\n} \$else {\nfn android_selected() {}\n}\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('termux', 'arm64') or { panic(err) }
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([source])
	fn_names := a.nodes.filter(it.kind == .fn_decl).map(it.value)
	assert 'termux_selected' in fn_names
	assert 'android_selected' !in fn_names
}

fn test_emscripten_comptime_branch_uses_canonical_target() {
	dir := os.join_path(os.vtmp_dir(), 'v3_emscripten_comptime_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source,
		'module main\n\n\$if wasm32_emscripten {\nfn wasm_selected() {}\n} \$else {\nfn host_selected() {}\n}\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([source])
	fn_names := a.nodes.filter(it.kind == .fn_decl).map(it.value)
	assert 'wasm_selected' in fn_names
	assert 'host_selected' !in fn_names
}
