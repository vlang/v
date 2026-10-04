module c

import os
import v.parser
import v.pref

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
	assert g.cached_support_has_c_type('ExistingSupport')
	assert g.cached_support_has_c_type('struct ExistingSupport')
	assert !g.cached_support_has_c_type('struct MissingSupport')
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

fn test_c_directive_environment_macros_are_expanded() {
	name := 'V3_C_DIRECTIVE_ENV_TEST'
	old_value := os.getenv(name)
	was_set := name in os.environ()
	defer {
		if was_set {
			os.setenv(name, old_value, true)
		} else {
			os.unsetenv(name)
		}
	}
	os.setenv(name, 'v3-sdk', true)

	assert c_flag_args("-I\$env('${name}')/include -L\$env('${name}')/lib", '', '', pref.host_target()) == [
		'-Iv3-sdk/include',
		'-Lv3-sdk/lib',
	]
	assert c_include_arg('"\$env(\'${name}\')/include/header.h"', '', '') == '"v3-sdk/include/header.h"'
}

fn test_c_flag_default_define_macros_stay_single_arguments() {
	target := pref.host_target()
	assert c_flag_args("-DNUMBER=\$d('N', 1234 ) ##", '', '', target) == [
		'-DNUMBER=1234',
	]
	assert c_flag_args('-DFNAME=\$d(\'A1\', \'"check_d\')\$d(\'A2\',\'flags_fn"\')', '', '', target) == [
		'-DFNAME=check_dflags_fn',
	]
	assert c_flag_args("-DMIXED=\$d('A1', 'mixed' )_\$d('A2', 4 ) ##", '', '', target) == [
		'-DMIXED=mixed_4',
	]
	assert c_flag_args(r'-DJSON=$d("JSON", "\"value\"")', '', '', target) == [
		'-DJSON="value"',
	]
	assert c_flag_args("-DMESSAGE=\$d('MSG', 'hello world')", '', '', target) == [
		'-DMESSAGE=hello world',
	]
	assert c_flag_args("-DPASTE=\$d('PASTE', 'a##b') ## source comment", '', '', target) == [
		'-DPASTE=a##b',
	]
	assert c_flag_args(r'-DQUOTED=$d("QUOTED", "\"a##b\"") ## source comment', '', '', target) == [
		'-DQUOTED="a##b"',
	]
	assert c_flag_args('-DTEXT=\'"\$d(x)"\'', '', '', target) == [
		'-DTEXT="$d(x)"',
	]
	assert c_flag_args('-DNAME=\$d(\'name\', \'a"b\')', '', '', target) == [
		'-DNAME=a"b',
	]
}

fn test_c_flag_default_define_macros_honor_configured_values() {
	target := pref.host_target()
	// A matching `-d name=value` override wins over the `$d(...)` fallback.
	assert c_flag_args_with_values("-DNUMBER=\$d('N', 1234 )", '', '', target, {
		'N': '42'
	}) == [
		'-DNUMBER=42',
	]
	// Only the configured define is substituted; the other keeps its fallback.
	assert c_flag_args_with_values("-DMIXED=\$d('A1', 'mixed' )_\$d('A2', 4 )", '', '', target, {
		'A2': '9'
	}) == [
		'-DMIXED=mixed_9',
	]
	// A bare `-d name` has the configured value `true`, matching v.pref semantics.
	assert c_flag_args_with_values("-DVALUE=\$d('enabled', false)", '', '', target, {
		'enabled': 'true'
	}) == [
		'-DVALUE=true',
	]
	// An absent define also falls back.
	assert c_flag_args_with_values("-DNUMBER=\$d('N', 1234 )", '', '', target, map[string]string{}) == [
		'-DNUMBER=1234',
	]
	assert c_flag_args_with_values("-DMESSAGE=\$d('MSG', 'fallback')", '', '', target, {
		'MSG': 'configured value'
	}) == [
		'-DMESSAGE=configured value',
	]
	assert c_flag_args_with_values("-DPASTE=\$d('PASTE', 'fallback') ## source comment", '', '', target, {
		'PASTE': 'a##b'
	}) == [
		'-DPASTE=a##b',
	]
	assert c_flag_args_with_values("-DNAME=\$d('name', 'fallback')", '', '', target, {
		'name': 'a"b'
	}) == [
		'-DNAME=a"b',
	]
	assert c_flag_args_with_values("-DPATH=\$d('path', 'fallback')", '', '', target, {
		'path': r'a\b'
	}) == [
		r'-DPATH=a\b',
	]
}

fn test_bare_macro_preprocessor_conditions_use_target_and_definition_state() {
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	empty := map[string]bool{}
	known_apple, active_apple := c_preprocessor_condition_state('__APPLE__', empty, empty, empty, false, false, linux)
	assert known_apple
	assert !active_apple
	known_linux, active_linux := c_preprocessor_condition_state('linux', empty, empty, empty, false, false, linux)
	assert known_linux
	assert active_linux
	known_negated_unix, active_negated_unix := c_preprocessor_condition_state('!unix', empty, empty, empty, false, false, linux)
	assert known_negated_unix
	assert !active_negated_unix
	assert !c_native_source_context_definitely_inactive(['#if linux'], []string{}, false, linux, false)
	assert c_native_source_context_definitely_inactive(['#if !unix'], []string{}, false, linux, false)
	assert c_native_source_context_definitely_inactive(['#if linux', '#else'], []string{}, false, linux, false)
	known_c99_linux, active_c99_linux := c_preprocessor_condition_state('linux', empty, empty, empty, false, true, linux)
	assert !known_c99_linux
	assert active_c99_linux
	known_c99_unix, active_c99_unix := c_preprocessor_condition_state('unix', empty, empty, empty, false, true, linux)
	assert !known_c99_unix
	assert active_c99_unix
	known_c99_underscored, active_c99_underscored := c_preprocessor_condition_state('__linux__', empty, empty, empty, false, true, linux)
	assert known_c99_underscored
	assert active_c99_underscored
	assert !c_native_source_context_definitely_inactive(['#if linux'], []string{}, true, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if !unix'], []string{}, true, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if linux', '#else'], [
		'-std=c99',
	], false, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if !unix'], ['-std=c11'], false, linux, false)
	assert c_native_source_context_definitely_inactive(['#if !unix'], ['-std=c99', '-std=gnu11'], false, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if !unix'], ['-std=gnu11', '-std=c99'], false, linux, false)
	assert c_native_source_context_definitely_inactive(['#if !unix'], ['-std=gnu11'], true, linux, false)
	assert !c_native_source_context_definitely_inactive(['#if SOURCE_FEATURE'], []string{}, false, linux, true)
	known_unset, active_unset := c_preprocessor_condition_state('SOME_UNSET_MACRO', empty, empty, empty, false, false, linux)
	assert known_unset
	assert !active_unset
	known_negated, active_negated := c_preprocessor_condition_state('!SOME_UNSET_MACRO', empty, empty, empty, false, false, linux)
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
	known_compound, active_compound := c_preprocessor_condition_state('SOME_UNSET_MACRO || 1', empty, empty, empty, false, false, linux)
	assert !known_compound
	assert active_compound
	known_external, active_external := c_preprocessor_condition_state('HEADER_FEATURE', empty, empty, empty, true, false, linux)
	assert !known_external
	assert active_external
	known_external_defined, active_external_defined := c_preprocessor_condition_state('defined(HEADER_FEATURE)', empty, empty, empty, true, false, linux)
	assert !known_external_defined
	assert active_external_defined
	known_external_target, active_external_target := c_preprocessor_condition_state('__APPLE__', empty, empty, empty, true, false, linux)
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
	riscv32 := pref.target_from('linux', 'riscv32') or { panic(err) }
	riscv64 := pref.target_from('linux', 'riscv64') or { panic(err) }
	ppc := pref.target_from('linux', 'ppc') or { panic(err) }
	sparc64 := pref.target_from('solaris', 'sparc64') or { panic(err) }
	x86 := pref.target_from('linux', 'x86') or { panic(err) }
	arm64 := pref.target_from('linux', 'arm64') or { panic(err) }
	assert c_flag_args('rv32 -DRV32', '', '', riscv32) == ['-DRV32']
	assert c_flag_args('risc-v32 -DRISCV32', '', '', riscv32) == ['-DRISCV32']
	assert c_flag_args('rv64 -DRV64', '', '', riscv64) == ['-DRV64']
	assert c_flag_args('risc-v64 -DRISCV64', '', '', riscv64) == ['-DRISCV64']
	assert c_flag_args('ppc32 -DPPC', '', '', ppc) == ['-DPPC']
	assert c_flag_args('powerpc -DPOWERPC', '', '', ppc) == ['-DPOWERPC']
	assert c_flag_args('sparc64 -DSPARC64', '', '', sparc64) == ['-DSPARC64']
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
	flags := c_flag_args('-I "include dir" -L lib -isystem system -include cfg.h -imacros defs.h -DVALUE=1', '', source_file, pref.host_target())
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
	assert c_flag_args("-I\$when_first_existing('${missing}', '${dir}')", '', '', pref.host_target()) == [
		'-I${dir}',
	]
	assert c_flag_args("-I\$when_first_existing('${missing}')", '', '', pref.host_target()).len == 0
	assert c_flag_args("\$first_existing('${missing}', '${dir}')", '', '', pref.host_target()) == [
		dir,
	]
}

fn test_disabled_c_flag_does_not_expand_existing_path_macros() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	missing := os.join_path(os.vtmp_dir(), 'v3_disabled_c_flag_missing_${os.getpid()}')
	os.rmdir_all(missing) or {}
	assert c_flag_args("linux \$first_existing('${missing}')", '', '', target).len == 0
}

fn test_cache_native_input_language_detects_implicit_objective_c_sources() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_objective_c_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	objective_c_source := os.join_path(dir, 'implementation.m')
	objective_c_header := os.join_path(dir, 'implementation.h')
	plain_header := os.join_path(dir, 'plain.h')
	os.write_file(objective_c_source, 'int implementation(void) { return 1; }\n')!
	os.write_file(objective_c_header, '@interface V3CacheImplementation\n@end\n')!
	os.write_file(plain_header, 'int plain_declaration(void);\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('macos', 'arm64') or { panic(err) }
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	assert cache_native_input_path_needs_objective_c(objective_c_source, []string{}, false, prefs.target)
	// Header language is selected by the consuming C compiler, without reading it in V.
	assert !cache_native_input_path_needs_objective_c(objective_c_header, []string{}, false, prefs.target)
	assert !cache_native_input_path_needs_objective_c(plain_header, []string{}, false, prefs.target)
	for include, expected in {
		'"implementation.m"': true
		'"implementation.h"': true
		'"plain.h"':          true
	} {
		source := os.join_path(dir, 'sample_${expected}_${include.len}.v')
		os.write_file(source, 'module sample\n#include ${include}\n')!
		mut p := parser.Parser.new(prefs)
		a := p.parse_file(source)
		assert cache_native_inputs_need_objective_c(a, '', []string{}, false, 'clang', prefs.target) == expected
		linux_expected := include == '"implementation.m"'
		assert cache_native_inputs_need_objective_c(a, '', []string{}, false, 'clang', linux) == linux_expected
	}
}

fn test_cache_native_input_language_reports_source_language() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_language_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	objc_cpp_source := os.join_path(dir, 'impl.mm')
	objc_source := os.join_path(dir, 'impl.m')
	cpp_source := os.join_path(dir, 'impl.cpp')
	objc_header := os.join_path(dir, 'objc.h')
	plain_header := os.join_path(dir, 'plain.h')
	os.write_file(objc_cpp_source, 'int impl(void) { return 1; }\n')!
	os.write_file(objc_source, 'int impl(void) { return 1; }\n')!
	os.write_file(cpp_source, 'int impl(void) { return 1; }\n')!
	os.write_file(objc_header, '@interface V3CacheLang\n@end\n')!
	os.write_file(plain_header, 'int plain(void);\n')!
	assert cache_native_input_language(objc_cpp_source, []string{}, false, prefs.target) == 'objective-c++'
	assert cache_native_input_language(objc_source, []string{}, false, prefs.target) == 'objective-c'
	assert cache_native_input_language(cpp_source, []string{}, false, prefs.target) == 'c++'
	assert cache_native_input_language(os.join_path(dir, 'cpp_source.C'), []string{}, false,
		prefs.target) == 'c++'
	assert cache_native_input_language(objc_header, []string{}, false, prefs.target) == 'c'
	assert cache_native_input_language(plain_header, []string{}, false, prefs.target) == 'c'
	// An .mm source is Objective-C++, so the shared probe language must carry both
	// __OBJC__ and __cplusplus rather than a plain Objective-C choice.
	mm_program := os.join_path(dir, 'mm_program.v')
	os.write_file(mm_program, 'module sample\n#include "impl.mm"\n')!
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(mm_program)
	assert cache_native_inputs_language(a, '', []string{}, false, 'clang', prefs.target) == 'objective-c++'
}

// Darwin header directives select Objective-C without opening the headers, while
// other targets keep using C. This keeps the cache probe and final compile aligned.
fn test_cache_native_inputs_language_uses_objective_c_for_darwin_headers() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_language_header_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mut macos_prefs := pref.new_preferences()
	macos_prefs.target = pref.target_from('macos', 'arm64') or { panic(err) }
	ios := pref.target_from('ios', 'arm64') or { panic(err) }
	linux := pref.target_from('linux', 'amd64') or { panic(err) }

	objc_dir := os.join_path(dir, 'objc_mod')
	os.mkdir_all(objc_dir) or { panic(err) }
	os.write_file(os.join_path(objc_dir, 'shared.h'), '@interface V3HeaderBranch\n@end\n')!
	objc_program := os.join_path(objc_dir, 'prog.v')
	os.write_file(objc_program, 'module objc_mod\n#include "shared.h"\n')!
	mut p1 := parser.Parser.new(macos_prefs)
	a1 := p1.parse_file(objc_program)
	assert cache_native_inputs_language(a1, '', []string{}, false, 'clang', macos_prefs.target) == 'objective-c'
	assert cache_native_inputs_language(a1, '', []string{}, false, 'clang', ios) == 'objective-c'
	assert cache_native_inputs_language(a1, '', []string{}, false, 'tinyc', macos_prefs.target) == 'c'
	assert cache_native_inputs_language(a1, '', []string{}, false, 'clang', linux) == 'c'

	// The choice is based only on the directive and target, not header contents.
	plain_dir := os.join_path(dir, 'plain_mod')
	os.mkdir_all(plain_dir) or { panic(err) }
	os.write_file(os.join_path(plain_dir, 'shared.h'), 'int plain_decl(void);\n')!
	plain_program := os.join_path(plain_dir, 'prog.v')
	os.write_file(plain_program, 'module plain_mod\n#include "shared.h"\n')!
	mut p2 := parser.Parser.new(macos_prefs)
	a2 := p2.parse_file(plain_program)
	assert cache_native_inputs_language(a2, '', []string{}, false, 'clang', macos_prefs.target) == 'objective-c'
	assert cache_native_inputs_language(a2, '', []string{}, false, 'clang', linux) == 'c'

	// Missing pre/postinclude headers prove the language choice does not read them.
	placed_program := os.join_path(dir, 'placed.v')
	os.write_file(placed_program, 'module placed\n#preinclude "missing_early.h"\n#postinclude <missing_late.h>\n')!
	mut p3 := parser.Parser.new(macos_prefs)
	a3 := p3.parse_file(placed_program)
	assert cache_native_inputs_language(a3, '', []string{}, false, 'clang', macos_prefs.target) == 'objective-c'
	assert cache_native_inputs_language(a3, '', []string{}, false, 'clang', linux) == 'c'
}

fn test_termux_comptime_branch_uses_canonical_target() {
	dir := os.join_path(os.vtmp_dir(), 'v3_termux_comptime_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'module main\n\n\$if termux {\nfn termux_selected() {}\n} \$else {\nfn android_selected() {}\n}\n')!
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
	os.write_file(source, 'module main\n\n\$if wasm32_emscripten {\nfn wasm_selected() {}\n} \$else {\nfn host_selected() {}\n}\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([source])
	fn_names := a.nodes.filter(it.kind == .fn_decl).map(it.value)
	assert 'wasm_selected' in fn_names
	assert 'host_selected' !in fn_names
}

fn test_cross_c_condition_translates_retained_comptime_conditions() {
	mut g := FlatGen.new()
	g.set_output_cross_c(true)
	assert g.cross_c_condition('linux') == '(defined(__linux__) && !defined(__ANDROID__))'
	assert g.cross_c_condition('!(windows)') == '(!defined(_WIN32))'
	assert g.cross_c_condition('(macos || linux)') == '((defined(__APPLE__) && !defined(__ENVIRONMENT_IPHONE_OS_VERSION_MIN_REQUIRED__)) || (defined(__linux__) && !defined(__ANDROID__)))'
	assert g.cross_c_condition('(arm64 && !(tinyc))') == '(defined(__V_arm64) && (!defined(__TINYC__)))'
	// The parser folds target-independent parts of a retained condition before
	// codegen, so only `true`/`false` reach this translation.
	assert g.cross_c_condition('(true && x64)') == '(1 && defined(TARGET_IS_64BIT))'
	assert g.cross_c_condition('(false || linux)') == '(0 || (defined(__linux__) && !defined(__ANDROID__)))'
}

fn test_cross_directive_target_prefix_conditions() {
	// `#include linux <sys/timerfd.h>` has to stay in portable output, guarded,
	// instead of being resolved against the generating host.
	// `#include linux <...>` uses the same mutually exclusive condition as `$if
	// linux`: `c_flag_target_enabled` compares V's target OS, where `android` is
	// not `linux` either.
	assert c_directive_target_condition('linux <sys/timerfd.h>') or { '' } == '(defined(__linux__) && !defined(__ANDROID__))'
	assert c_directive_strip_target_prefix('linux <sys/timerfd.h>') == '<sys/timerfd.h>'
	assert c_directive_target_condition('<stdio.h>') == none
	assert c_directive_strip_target_prefix('<stdio.h>') == '<stdio.h>'
}

fn test_cross_embeds_transitive_local_includes() {
	// A header embedded into portable output takes its own quoted includes with
	// it: the generated C is compiled far from the source tree, where a sibling
	// `#include "..."` would no longer resolve.
	dir := os.join_path(os.vtmp_dir(), 'v3_cross_embed_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'sibling.h'), '#define SIBLING_MARKER 1\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'outer.h'), '#include "sibling.h"\n#include <stdio.h>\n#define OUTER_MARKER 1\n') or {
		panic(err)
	}

	mut g := FlatGen.new()
	g.set_output_cross_c(true)
	embedded := g.cross_embedded_header_text(os.join_path(dir, 'outer.h'), []string{}) or {
		panic('header not embedded')
	}
	assert embedded.contains('SIBLING_MARKER')
	assert embedded.contains('OUTER_MARKER')
	assert !embedded.contains('#include "sibling.h"')
	// A system header stays an include; the consumer's C compiler supplies it.
	assert embedded.contains('#include <stdio.h>')
}

fn test_cross_embedding_stops_at_an_include_cycle() {
	dir := os.join_path(os.vtmp_dir(), 'v3_cross_embed_cycle_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'a.h'), '#include "b.h"\n#define A_MARKER 1\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'b.h'), '#include "a.h"\n#define B_MARKER 1\n') or { panic(err) }

	mut g := FlatGen.new()
	g.set_output_cross_c(true)
	embedded := g.cross_embedded_header_text(os.join_path(dir, 'a.h'), []string{}) or {
		panic('header not embedded')
	}
	assert embedded.contains('A_MARKER')
	assert embedded.contains('B_MARKER')
}

// A `#flag` can name a native source or object outright -- `vlib/db/sqlite/sqlite.c.v` builds
// `@VEXEROOT/thirdparty/sqlite/sqlite3.c` that way, and uses a prebuilt `sqlite3.o` on
// Windows. The include scan never sees those, so they need their own collection or a cached
// build keeps running against a stale amalgamation.
fn test_native_flag_inputs_are_collected() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_flag_inputs_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	native_source := os.join_path(dir, 'amalgamation.c')
	os.write_file(native_source, 'int amalgamation(void) { return 0; }\n') or { panic(err) }
	prebuilt := os.join_path(dir, 'prebuilt.o')
	os.write_file(prebuilt, 'not really an object, only its path matters\n') or { panic(err) }

	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample\n#flag ${native_source}\n#flag ${prebuilt}\n#flag -lm\n#flag -I${dir}\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)

	collected := cache_native_flag_input_files(a, '', prefs.target)
	assert collected == [os.real_path(native_source), os.real_path(prebuilt)], collected.str()
}

// Only files are inputs: a library name, an option, and a path that does not exist must not
// be reported, or every lookup would stat something that is never there and count it changed.
fn test_native_flag_inputs_ignore_options_and_missing_paths() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_flag_skips_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'sample.v')
	os.write_file(source, 'module sample\n#flag -lsqlite3\n#flag -L/usr/local/lib\n#flag -DSOME_MACRO\n#flag ${dir}/absent.c\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)

	assert cache_native_flag_input_files(a, '', prefs.target) == []
}

fn test_c_flag_start_markers_are_stripped() {
	target := pref.host_target()
	args, at_start := c_flag_args_with_start_marker('-lraylib@START_LIBS', '', '', target, map[string]string{})
	assert args == ['-lraylib']
	assert at_start
	plain_args, plain_at_start := c_flag_args_with_start_marker('-lraylib', '', '', target, map[string]string{})
	assert plain_args == ['-lraylib']
	assert !plain_at_start
	commented_args, commented_at_start := c_flag_args_with_start_marker('-lraylib ## not @START_LIBS',
		'', '', target, map[string]string{})
	assert commented_args == ['-lraylib']
	assert !commented_at_start
	assert c_flag_args('-DFIRST@START_DEFINES', '', '', target) == ['-DFIRST']
	assert c_flag_args('-I/opt/include@START_OTHERS', '', '', target) == ['-I/opt/include']
	dir := os.join_path(os.vtmp_dir(), 'v3_c_flag_start_marker_path_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	assert c_flag_args('@DIR/lib/libraylib.a@START_LIBS', '', os.join_path(dir, 'main.v'), target) == [
		'${os.real_path(dir)}/lib/libraylib.a',
	]
}

// `-lraylib@START_LIBS` must be searched before the libraries of every module,
// including vlib's `-luser32`; mingw otherwise resolves `CloseWindow` from user32
// first and then reports a duplicate definition from raylib's rcore.o.
fn test_c_flag_start_markers_move_flags_before_module_flags() {
	assert_c_flag_directive_order('#flag -lwinmm\n#flag -lraylib@START_LIBS\n#flag -lglfw@START_LIBS\n',
		'#flag -luser32\n', ['-lraylib', '-lglfw', '-luser32', '-lwinmm'])
}

// A marked directive promotes an identical unmarked one, whichever comes first,
// so an application can move a library that a wrapper module already links.
fn test_c_flag_start_markers_promote_unmarked_duplicates() {
	expected := ['-lraylib', '-lglfw', '-luser32', '-lwinmm']
	assert_c_flag_directive_order('#flag -lwinmm\n#flag -lraylib\n#flag -lraylib@START_LIBS\n#flag -lglfw@START_LIBS\n',
		'#flag -luser32\n', expected)
	assert_c_flag_directive_order('#flag -lwinmm\n#flag -lraylib@START_LIBS\n#flag -lraylib\n#flag -lglfw@START_LIBS\n',
		'#flag -luser32\n', expected)
	assert_c_flag_directive_order('#flag -lwinmm\n#flag -lraylib@START_LIBS\n#flag -lglfw@START_LIBS\n',
		'#flag -lraylib\n#flag -luser32\n', expected)
	// Without a marker, the first occurrence still decides the position: main is
	// parsed first here, so `-luser32` stays with the main module flags.
	assert_c_flag_directive_order('#flag -lwinmm\n#flag -luser32\n', '#flag -luser32\n', [
		'-lwinmm',
		'-luser32',
	])
}

// A start marker must not hide a linked object file, which ships no header, so its
// `fn C.` declarations still need their generated prototypes.
fn test_c_flag_start_markers_keep_linked_object_files() {
	dir := os.join_path(os.vtmp_dir(), 'v3_c_flag_start_marker_object_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'module main\n#flag @VMODROOT/helper.o@START_LIBS\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	mut g := FlatGen.new()
	g.a = p.parse_files([source])
	g.target = prefs.target
	g.collect_c_flags_from_directives()
	assert g.files_linking_c_sources[source]
}

fn assert_c_flag_directive_order(main_flags string, sys_flags string, expected []string) {
	dir := os.join_path(os.vtmp_dir(), 'v3_c_flag_start_markers_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'sys')) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	main_source := os.join_path(dir, 'main.v')
	sys_source := os.join_path(dir, 'sys', 'sys.v')
	os.write_file(main_source, 'module main\n${main_flags}') or { panic(err) }
	os.write_file(sys_source, 'module sys\n${sys_flags}') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([main_source, sys_source])
	assert cache_directive_flags(a, '', prefs.target, map[string]string{}) == expected
	mut g := FlatGen.new()
	g.a = a
	g.target = prefs.target
	g.collect_c_flags_from_directives()
	assert g.c_flags() == expected
}
