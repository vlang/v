module c

import os
import v3.parser
import v3.pref

fn test_c_directive_targets_use_requested_platform() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	android := pref.target_from('android', 'arm64') or { panic(err) }
	dragonfly := pref.target_from('dragonfly', 'amd64') or { panic(err) }
	ios := pref.target_from('ios', 'arm64') or { panic(err) }
	assert c_flag_args('macos -DMACOS', '', '', target) == ['-DMACOS']
	assert c_flag_args('arm64 -DARM64', '', '', target) == ['-DARM64']
	assert c_flag_args('linux -DLINUX', '', '', target).len == 0
	assert c_flag_args('amd64 -DAMD64', '', '', target).len == 0
	assert c_flag_args('android -laaudio', '', '', target).len == 0
	assert c_flag_args('android -laaudio', '', '', android) == ['-laaudio']
	assert c_flag_args('dragonfly -lncurses', '', '', target).len == 0
	assert c_flag_args('dragonfly -lncurses', '', '', dragonfly) == ['-lncurses']
	assert c_flag_args('ios -framework AudioToolbox', '', '', target).len == 0
	assert c_flag_args('ios -framework AudioToolbox', '', '', ios) == ['-framework', 'AudioToolbox']
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

fn test_c_directive_arch_aliases_use_canonical_targets() {
	riscv64 := pref.target_from('linux', 'riscv64') or { panic(err) }
	x86 := pref.target_from('linux', 'x86') or { panic(err) }
	arm64 := pref.target_from('linux', 'arm64') or { panic(err) }
	assert c_flag_args('rv64 -DRV64', '', '', riscv64) == ['-DRV64']
	assert c_flag_args('risc-v64 -DRISCV64', '', '', riscv64) == ['-DRISCV64']
	assert c_flag_args('rv64 -DRV64', '', '', arm64).len == 0
	assert c_flag_args('i386 -DI386', '', '', x86) == ['-DI386']
	assert c_flag_args('i686 -DI686', '', '', x86) == ['-DI686']
	assert c_flag_args('i386 -DI386', '', '', arm64).len == 0
}

fn test_split_relative_c_flag_paths_resolve_from_source_directory() {
	source_dir := os.join_path(os.vtmp_dir(), 'v3_split_c_flag_paths', 'source')
	source_file := os.join_path(source_dir, 'main.v')
	include_dir := os.real_path(os.join_path(source_dir, 'include dir'))
	lib_dir := os.real_path(os.join_path(source_dir, 'lib'))
	system_dir := os.real_path(os.join_path(source_dir, 'system'))
	cfg_file := os.real_path(os.join_path(source_dir, 'cfg.h'))
	defs_file := os.real_path(os.join_path(source_dir, 'defs.h'))
	flags := c_flag_args('-I "include dir" -L lib -isystem system -include cfg.h -imacros defs.h -DVALUE=1',
		'', source_file, pref.host_target())
	assert flags == [
		'-I',
		include_dir,
		'-L',
		lib_dir,
		'-isystem',
		system_dir,
		'-include',
		cfg_file,
		'-imacros',
		defs_file,
		'-DVALUE=1',
	]
	assert c_flag_include_dirs(flags) == [include_dir, system_dir]
}

fn test_c_flag_existing_path_macros() {
	dir := os.join_path(os.vtmp_dir(), 'v3 c flag existing path ${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	missing := os.join_path(dir, 'missing')
	assert c_flag_args('-I\$when_first_existing(\'${missing}\', \'${dir}\')', '', '',
		pref.host_target()) == ['-I${dir}']
	assert c_flag_args('-I\$when_first_existing(\'${missing}\')', '', '', pref.host_target()).len == 0
	assert c_flag_args('\$first_existing(\'${missing}\', \'${dir}\')', '', '', pref.host_target()) == [
		dir,
	]
}

fn test_disabled_c_flag_does_not_expand_existing_path_macros() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	missing := os.join_path(os.vtmp_dir(), 'v3_disabled_c_flag_missing_${os.getpid()}')
	os.rmdir_all(missing) or {}
	assert c_flag_args('linux \$first_existing(\'${missing}\')', '', '', target).len == 0
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
	flags := c_flag_args('-include "cfg header.h" -imacros defs.h', '',
		os.join_path(dir, 'main.v'), pref.host_target())
	assert flags == ['-include', os.real_path(cfg), '-imacros', os.real_path(defs)]
	mut expected := [os.real_path(cfg), os.real_path(defs)]
	expected.sort()
	assert cache_c_flag_input_files(flags) == expected
}

fn test_cache_input_scan_uses_requested_target_flags() {
	dir := os.join_path(os.vtmp_dir(), 'v3_target_cache_inputs_${os.getpid()}')
	include_dir := os.join_path(dir, 'target_include')
	os.rmdir_all(dir) or {}
	os.mkdir_all(include_dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(include_dir, 'target.h')
	os.write_file(header, '#define TARGET_VALUE 1\n') or { panic(err) }
	host_arch := pref.host_target().arch
	target_arch := if host_arch == 'arm64' { 'amd64' } else { 'arm64' }
	target := pref.target_from('linux', target_arch) or { panic(err) }
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample
#flag ${target_arch} -I target_include
#include "target.h"
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = target
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	inputs, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, target)
	assert !has_untracked
	assert inputs['sample'] == [os.real_path(header)]
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
