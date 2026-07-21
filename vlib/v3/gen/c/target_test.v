module c

import os
import v3.parser
import v3.pref

fn test_cached_support_declarations_ignore_comments_and_literals() {
	mut g := FlatGen.new()
	g.set_cached_support_declarations('typedef int ExistingSupport;
// Array_fixed_comment_only
/* _fn_ptr_block_comment_only */
const char *text = "Option_string_only with \\"escaped\\" text";
char quote = \'A\';
')
	assert g.cached_support_identifiers['ExistingSupport']
	assert !g.cached_support_identifiers['Array_fixed_comment_only']
	assert !g.cached_support_identifiers['_fn_ptr_block_comment_only']
	assert !g.cached_support_identifiers['Option_string_only']
	assert !g.cached_support_identifiers['escaped']
}

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

fn test_bare_macro_preprocessor_conditions_use_target_and_definition_state() {
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	empty := map[string]bool{}
	known_apple, active_apple := c_preprocessor_condition_state('__APPLE__', empty, empty, empty,
		false, false, linux)
	assert known_apple
	assert !active_apple
	known_linux, active_linux := c_preprocessor_condition_state('linux', empty, empty, empty,
		false, false, linux)
	assert known_linux
	assert active_linux
	known_negated_unix, active_negated_unix := c_preprocessor_condition_state('!unix', empty,
		empty, empty, false, false, linux)
	assert known_negated_unix
	assert !active_negated_unix
	assert !c_native_source_context_definitely_inactive(['#if linux'], []string{}, false, linux,
		false)
	assert c_native_source_context_definitely_inactive(['#if !unix'], []string{}, false, linux,
		false)
	assert c_native_source_context_definitely_inactive(['#if linux', '#else'], []string{}, false,
		linux, false)
	known_c99_linux, active_c99_linux := c_preprocessor_condition_state('linux', empty, empty,
		empty, false, true, linux)
	assert !known_c99_linux
	assert active_c99_linux
	known_c99_unix, active_c99_unix := c_preprocessor_condition_state('unix', empty, empty, empty,
		false, true, linux)
	assert !known_c99_unix
	assert active_c99_unix
	known_c99_underscored, active_c99_underscored := c_preprocessor_condition_state('__linux__',
		empty, empty, empty, false, true, linux)
	assert known_c99_underscored
	assert active_c99_underscored
	assert !c_native_source_context_definitely_inactive(['#if linux'], []string{}, true, linux,
		false)
	assert !c_native_source_context_definitely_inactive(['#if !unix'], []string{}, true, linux,
		false)
	assert !c_native_source_context_definitely_inactive(['#if linux', '#else'], [
		'-std=c99',
	], false, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if !unix'], ['-std=c11'], false, linux,
		false)
	assert c_native_source_context_definitely_inactive(['#if !unix'], ['-std=c99', '-std=gnu11'],
		false, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if !unix'], ['-std=gnu11', '-std=c99'],
		false, linux, false)
	assert c_native_source_context_definitely_inactive(['#if !unix'], ['-std=gnu11'], true, linux,
		false)
	assert !c_native_source_context_definitely_inactive(['#if SOURCE_FEATURE'], []string{}, false,
		linux, true)
	known_unset, active_unset := c_preprocessor_condition_state('SOME_UNSET_MACRO', empty, empty,
		empty, false, false, linux)
	assert known_unset
	assert !active_unset
	known_negated, active_negated := c_preprocessor_condition_state('!SOME_UNSET_MACRO', empty,
		empty, empty, false, false, linux)
	assert known_negated
	assert active_negated
	known_defined, active_defined := c_preprocessor_condition_state('SOME_DEFINED_MACRO', {
		'SOME_DEFINED_MACRO': true
	}, empty, empty, false, false, linux)
	assert !known_defined
	assert active_defined
	known_negated_defined, active_negated_defined := c_preprocessor_condition_state('!FEATURE', {
		'FEATURE': true
	}, empty, empty, false, false, linux)
	assert !known_negated_defined
	assert active_negated_defined
	known_presence, active_presence := c_preprocessor_condition_state('defined(FEATURE)', {
		'FEATURE': true
	}, empty, empty, false, false, linux)
	assert known_presence
	assert active_presence
	known_compound, active_compound := c_preprocessor_condition_state('SOME_UNSET_MACRO || 1',
		empty, empty, empty, false, false, linux)
	assert !known_compound
	assert active_compound
	known_external, active_external := c_preprocessor_condition_state('HEADER_FEATURE', empty,
		empty, empty, true, false, linux)
	assert !known_external
	assert active_external
	known_external_target, active_external_target := c_preprocessor_condition_state('__APPLE__',
		empty, empty, empty, true, false, linux)
	assert known_external_target
	assert !active_external_target
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
#include macos <TargetConditionals.h>
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = target
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	inputs, _, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, [], target)
	assert !has_untracked
	assert inputs['sample'] == [os.real_path(header)]
}

fn test_cache_input_scan_uses_initial_cflags() {
	dir := os.join_path(os.vtmp_dir(), 'v3_target_cli_cache_inputs_${os.getpid()}')
	include_dir := os.join_path(dir, 'CLI include with spaces')
	os.rmdir_all(dir) or {}
	os.mkdir_all(include_dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(include_dir, 'cli_only.h')
	forced_header := os.join_path(include_dir, 'forced_cli.h')
	forced_nested_header := os.join_path(include_dir, 'forced_nested.h')
	os.write_file(header, '#include FORCED_CLI_HEADER\n') or { panic(err) }
	os.write_file(forced_header, '#define FORCED_CLI_HEADER "forced_nested.h"\n') or { panic(err) }
	os.write_file(forced_nested_header, '#define FORCED_CLI_VALUE 2\n') or { panic(err) }
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample
#include "cli_only.h"
') or { panic(err) }
	target := pref.host_target()
	mut prefs := pref.new_preferences()
	prefs.target = target
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	inputs, _, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, ['-I', include_dir, '-include', 'forced_cli.h'], target)
	assert !has_untracked
	mut expected := [os.real_path(header), os.real_path(forced_nested_header)]
	expected.sort()
	assert inputs['sample'] == expected
	assert inputs['__v3_c_flags__'] == [os.real_path(forced_header)]
}

fn test_cache_input_scan_tracks_imported_headers() {
	dir := os.join_path(os.vtmp_dir(), 'v3_imported_header_cache_inputs_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	outer_header := os.join_path(dir, 'outer.h')
	imported_header := os.join_path(dir, 'imported.h')
	os.write_file(outer_header, '#import "imported.h"\n') or { panic(err) }
	os.write_file(imported_header, '#define IMPORTED_VALUE 1\n') or { panic(err) }
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample
#include "outer.h"
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	inputs, _, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, [], prefs.target)
	assert !has_untracked
	mut expected := [os.real_path(outer_header), os.real_path(imported_header)]
	expected.sort()
	assert inputs['sample'] == expected
}

fn test_cache_input_scan_tracks_literal_include_macros() {
	dir := os.join_path(os.vtmp_dir(), 'v3_macro_header_cache_inputs_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	definitions_header := os.join_path(dir, 'definitions.h')
	outer_header := os.join_path(dir, 'outer.h')
	nested_header := os.join_path(dir, 'nested.h')
	os.write_file(definitions_header, '#define V3_NESTED_HEADER "nested.h"\n') or { panic(err) }
	os.write_file(outer_header, '#include V3_NESTED_HEADER\n') or { panic(err) }
	os.write_file(nested_header, '#define NESTED_VALUE 1\n') or { panic(err) }
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample\n#include "definitions.h"\n#include "outer.h"\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	inputs, _, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, [], prefs.target)
	assert !has_untracked
	mut expected := [os.real_path(definitions_header), os.real_path(outer_header),
		os.real_path(nested_header)]
	expected.sort()
	assert inputs['sample'] == expected
}

fn test_cache_input_scan_rejects_dynamic_include_macros() {
	dir := os.join_path(os.vtmp_dir(), 'v3_dynamic_macro_cache_inputs_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	header := os.join_path(dir, 'outer.h')
	os.write_file(header,
		'#define V3_NESTED_HEADER V3_SELECTED_HEADER\n#include V3_NESTED_HEADER\n') or {
		panic(err)
	}
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample\n#include "outer.h"\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	_, _, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, [], prefs.target)
	assert has_untracked
}

fn test_cache_input_scan_separates_native_source_roots_from_dependencies() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_source_root_inputs_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	root_source := os.join_path(dir, 'root.c')
	nested_source := os.join_path(dir, 'nested.c')
	os.write_file(root_source, '#include "nested.c"\n') or { panic(err) }
	os.write_file(nested_source, 'static int nested_value(void) { return 42; }\n') or { panic(err) }
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample\n#include "root.c"\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	inputs, native_roots, has_untracked := cache_external_input_files(a, '', {
		'sample': true
	}, [], prefs.target)
	assert !has_untracked
	mut expected_inputs := [os.real_path(root_source), os.real_path(nested_source)]
	expected_inputs.sort()
	assert inputs['sample'] == expected_inputs
	assert native_roots['sample'] == [os.real_path(root_source)]
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
