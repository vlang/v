module cleanc

import os
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn new_target_test_gen(target_os string, user_defines []string) &Gen {
	return new_target_test_gen_with_freestanding(target_os, user_defines, false)
}

fn new_target_test_gen_with_freestanding(target_os string, user_defines []string, freestanding bool) &Gen {
	return new_target_test_gen_with_options(target_os, user_defines, freestanding, false)
}

fn new_target_test_gen_with_options(target_os string, user_defines []string, freestanding bool, prealloc bool) &Gen {
	return new_target_test_gen_with_options_and_hooks(target_os, user_defines, freestanding,
		prealloc, [])
}

fn new_target_test_gen_with_options_and_hooks(target_os string, user_defines []string, freestanding bool, prealloc bool, freestanding_hooks []string) &Gen {
	return new_target_test_gen_with_options_hooks_and_skip_builtin(target_os, user_defines,
		freestanding, prealloc, freestanding_hooks, false)
}

fn new_target_test_gen_with_options_hooks_and_skip_builtin(target_os string, user_defines []string, freestanding bool, prealloc bool, freestanding_hooks []string, skip_builtin bool) &Gen {
	prefs := &vpref.Preferences{
		backend:               .cleanc
		target_os:             target_os
		freestanding:          freestanding
		prealloc:              prealloc
		skip_builtin:          skip_builtin
		user_defines:          user_defines
		explicit_user_defines: user_defines.clone()
		freestanding_hooks:    freestanding_hooks
		no_parallel:           true
	}
	env := types.Environment.new()
	return Gen.new_with_env_and_pref([]ast.File{}, env, prefs)
}

fn freestanding_hook_defines(hooks []string) []string {
	mut defines := ['freestanding', 'freestanding_hooks']
	for hook in hooks {
		defines << 'freestanding_${hook}'
		defines << 'freestanding_hooks_${hook}'
	}
	return defines
}

fn c_directive_output_for_target(cond string, target_os string, user_defines []string) string {
	return c_directive_output_for_target_with_explicit(cond, target_os, user_defines,
		user_defines.clone(), false)
}

fn c_directive_output_for_target_with_explicit(cond string, target_os string, user_defines []string, explicit_user_defines []string, freestanding bool) string {
	prefs := &vpref.Preferences{
		backend:               .cleanc
		target_os:             target_os
		freestanding:          freestanding
		user_defines:          user_defines
		explicit_user_defines: explicit_user_defines
		no_parallel:           true
	}
	env := types.Environment.new()
	mut g := Gen.new_with_env_and_pref([]ast.File{}, env, prefs)
	mut seen := map[string]bool{}
	g.emit_directive(ast.Directive{
		name:    'include'
		value:   '<target_marker.h>'
		ct_cond: cond
	}, 'target_test.v', true, mut seen)
	return g.sb.str()
}

fn c_directive_output_for_freestanding_target(cond string, target_os string) string {
	return c_directive_output_for_target_with_explicit(cond, target_os, [
		'freestanding',
	], [], true)
}

fn preamble_for_target(target_os string, user_defines []string) string {
	mut g := new_target_test_gen(target_os, user_defines)
	g.set_emit_modules(['main'])
	g.write_preamble()
	return g.sb.str()
}

fn preamble_for_freestanding_field(target_os string) string {
	mut g := new_target_test_gen_with_freestanding(target_os, [], true)
	g.set_emit_modules(['main'])
	g.write_preamble()
	return g.sb.str()
}

fn full_preamble_for_target(target_os string, user_defines []string) string {
	mut g := new_target_test_gen(target_os, user_defines)
	g.write_preamble()
	return g.sb.str()
}

fn full_preamble_for_freestanding_field(target_os string) string {
	mut g := new_target_test_gen_with_freestanding(target_os, [], true)
	g.write_preamble()
	return g.sb.str()
}

fn full_preamble_for_options(target_os string, user_defines []string, freestanding bool, prealloc bool) string {
	mut g := new_target_test_gen_with_options(target_os, user_defines, freestanding, prealloc)
	g.write_preamble()
	return g.sb.str()
}

fn full_preamble_for_freestanding_hooks(hooks []string) string {
	mut g := new_target_test_gen_with_options_and_hooks('linux', freestanding_hook_defines(hooks),
		true, false, hooks)
	g.write_preamble()
	return g.sb.str()
}

fn runtime_fallbacks_for_called_functions(names []string) string {
	mut g := new_target_test_gen('linux', [])
	g.add_called_fn_names(names)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_called_functions_for_target(target_os string, names []string) string {
	mut g := new_target_test_gen(target_os, [])
	g.add_called_fn_names(names)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_called_functions_with_options(names []string, user_defines []string, freestanding bool) string {
	mut g := new_target_test_gen_with_freestanding('linux', user_defines, freestanding)
	g.add_called_fn_names(names)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_called_functions_with_hooks(names []string, hooks []string) string {
	mut g := new_target_test_gen_with_options_and_hooks('linux', freestanding_hook_defines(hooks),
		true, false, hooks)
	g.add_called_fn_names(names)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_existing_c_source(csrc string) string {
	mut g := new_target_test_gen('linux', [])
	g.sb.write_string(csrc)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_existing_c_source_with_options(csrc string, user_defines []string, freestanding bool) string {
	mut g := new_target_test_gen_with_freestanding('linux', user_defines, freestanding)
	g.sb.write_string(csrc)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_existing_c_source_with_hooks(csrc string, hooks []string) string {
	mut g := new_target_test_gen_with_options_and_hooks('linux', freestanding_hook_defines(hooks),
		true, false, hooks)
	g.sb.write_string(csrc)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin(csrc string, hooks []string) string {
	mut g := new_target_test_gen_with_options_hooks_and_skip_builtin('linux',
		freestanding_hook_defines(hooks), true, false, hooks, true)
	g.sb.write_string(csrc)
	g.emit_missing_runtime_fallbacks()
	return g.sb.str()
}

fn soa_companion_for_options(user_defines []string, freestanding bool) string {
	mut g := new_target_test_gen_with_freestanding('linux', user_defines, freestanding)
	g.gen_soa_companion('Point', types.Struct{
		name:   'Point'
		fields: [
			types.Field{
				name: 'x'
				typ:  types.Type(types.Primitive{
					props: .integer
					size:  32
				})
			},
		]
	})
	return g.sb.str()
}

fn soa_companion_for_hooks(hooks []string) string {
	mut g := new_target_test_gen_with_options_and_hooks('linux', freestanding_hook_defines(hooks),
		true, false, hooks)
	g.gen_soa_companion('Point', types.Struct{
		name:   'Point'
		fields: [
			types.Field{
				name: 'x'
				typ:  types.Type(types.Primitive{
					props: .integer
					size:  32
				})
			},
		]
	})
	return g.sb.str()
}

fn fixed_array_voidptr_str_write_for_options(user_defines []string, freestanding bool) string {
	mut g := new_target_test_gen_with_freestanding('linux', user_defines, freestanding)
	g.emit_fixed_array_str_write('items', 'voidptr', 2)
	return g.sb.str()
}

fn fixed_array_voidptr_str_write_for_hooks(hooks []string) string {
	mut g := new_target_test_gen_with_options_and_hooks('linux', freestanding_hook_defines(hooks),
		true, false, hooks)
	g.emit_fixed_array_str_write('items', 'voidptr', 2)
	return g.sb.str()
}

fn unresolved_generic_stub_for_options(user_defines []string, freestanding bool) string {
	mut g := new_target_test_gen_with_freestanding('linux', user_defines, freestanding)
	g.emit_unresolved_generic_stub('missing_generic_T', 'int')
	return g.sb.str()
}

fn unresolved_generic_stub_for_hooks(hooks []string) string {
	mut g := new_target_test_gen_with_options_and_hooks('linux', freestanding_hook_defines(hooks),
		true, false, hooks)
	g.emit_unresolved_generic_stub('missing_generic_T', 'int')
	return g.sb.str()
}

fn generated_c_for_target_program(name string, source string) string {
	return generated_c_for_target_program_with_options(name, source, 'linux', false, false)
}

fn generated_c_for_target_program_with_options(name string, source string, target_os string, freestanding bool, skip_builtin bool) string {
	return generated_c_for_target_program_with_defines(name, source, target_os, [], freestanding,
		skip_builtin)
}

fn generated_c_for_target_program_with_defines(name string, source string, target_os string, user_defines []string, freestanding bool, skip_builtin bool) string {
	return generated_c_for_target_program_with_defines_and_hooks(name, source, target_os,
		user_defines, freestanding, skip_builtin, [])
}

fn generated_c_for_target_program_with_hooks(name string, source string, hooks []string) string {
	return generated_c_for_target_program_with_defines_and_hooks(name, source, 'linux',
		freestanding_hook_defines(hooks), true, true, hooks)
}

fn generated_c_for_target_program_with_defines_and_hooks(name string, source string, target_os string, user_defines []string, freestanding bool, skip_builtin bool, freestanding_hooks []string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_cleanc_target_codegen_${name}_${os.getpid()}.v')
	os.write_file(tmp_file, source) or { panic(err) }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:               .cleanc
		target_os:             target_os
		freestanding:          freestanding
		skip_builtin:          skip_builtin
		user_defines:          user_defines
		explicit_user_defines: user_defines.clone()
		freestanding_hooks:    freestanding_hooks
		no_parallel:           true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	trans.set_file_set(file_set)
	transformed_files := trans.transform_files(files)
	mut gen := Gen.new_with_env_and_pref(transformed_files, env, prefs)
	return gen.gen()
}

fn assert_no_hosted_runtime_fallbacks(csrc string) {
	for marker in [
		'__attribute__((weak)) u64 __at_least_one',
		'__attribute__((weak)) Array_string arguments()',
		'__attribute__((weak)) void _write_buf_to_fd',
		'__attribute__((weak)) void _writeln_to_fd',
		'__attribute__((weak)) void eprint',
		'__attribute__((weak)) void flush_stdout',
		'__attribute__((weak)) void flush_stderr',
		'__attribute__((weak)) bool Array_int_contains',
		'__attribute__((weak)) bool Array_string_contains',
		'__attribute__((weak)) int Array_string_index',
		'__attribute__((weak)) string Array_int_str',
		'__attribute__((weak)) void DenseArray__zeros_to_end',
		'__attribute__((weak)) int bits__leading_zeros_64',
		'__attribute__((weak)) int bits__trailing_zeros_32',
		'__attribute__((weak)) int bits__trailing_zeros_64',
		'__attribute__((weak)) u32 bits__rotate_left_32',
		'__attribute__((weak)) u8* malloc_noscan',
		'__attribute__((weak)) void* memdup',
		'__attribute__((weak)) f64 f64_abs',
		'__attribute__((weak)) string f64__str',
		'__attribute__((weak)) string f64__strg',
		'__attribute__((weak)) string f32__str',
		'__attribute__((weak)) string f32__strg',
	] {
		assert !csrc.contains(marker), marker
	}
}

fn assert_no_hosted_output_runtime(csrc string) {
	assert !csrc.contains('isize written = write(fd, ptr, remaining_bytes)')
	assert !csrc.contains('fflush(')
	assert !csrc.contains('fflush(stdout)')
	assert !csrc.contains('fflush(stderr)')
}

fn assert_no_hosted_alloc_runtime(csrc string) {
	assert !csrc.contains('return malloc(n);')
	assert !csrc.contains(' = malloc(')
	assert !csrc.contains('\tfree(')
	assert !csrc.contains('realloc_data(')
	assert !csrc.contains(' = realloc(')
}

fn assert_no_raw_heap_runtime(csrc string) {
	for marker in ['*)malloc(', ' = malloc(', '\tmalloc(', 'return malloc(', 'calloc(', '*)realloc(',
		' = realloc(', '\trealloc(', '\tfree(', ' free('] {
		assert !csrc.contains(marker), marker
	}
}

fn assert_freestanding_alloc_refusal_without_hosted_heap(csrc string) {
	assert csrc.contains('_Static_assert(0, "${freestanding_missing_alloc_hook_message}")')
	assert_no_raw_heap_runtime(csrc)
}

fn assert_freestanding_format_refusal_without_hosted_formatting(csrc string) {
	assert csrc.contains('_Static_assert(0, "${freestanding_missing_format_hook_message}")')
	assert_no_hosted_formatting_or_abort_runtime(csrc)
}

fn assert_freestanding_heap_runtime_refusal(csrc string, helper string) {
	assert csrc.contains('_Static_assert(0, "${freestanding_missing_heap_runtime_message}: ${helper}")')
}

fn assert_freestanding_output_refusal_without_hosted_io(csrc string) {
	assert csrc.contains('_Static_assert(0, "${freestanding_missing_output_hook_message}")')
	assert_no_hosted_output_runtime(csrc)
}

fn assert_freestanding_panic_refusal_without_hosted_exit(csrc string) {
	assert csrc.contains('_Static_assert(0, "${freestanding_missing_panic_hook_message}")')
	assert !csrc.contains('v_platform_panic')
	assert !csrc.contains('exit(')
}

fn assert_no_hosted_formatting_or_abort_runtime(csrc string) {
	for marker in ['snprintf(', 'sprintf(', 'printf(', 'fprintf(', 'fputs(', 'stderr', 'abort('] {
		assert !csrc.contains(marker), marker
	}
}

fn assert_no_os_runtime_headers(csrc string) {
	for marker in [
		'#include <stdio.h>',
		'#include <stdlib.h>',
		'#include <unistd.h>',
		'#include <windows.h>',
		'#include <dirent.h>',
		'#include <pthread.h>',
		'#include <mach/mach.h>',
		'#include <termios.h>',
		'#include <sys/wait.h>',
		'#include <sys/ioctl.h>',
		'extern char** environ;',
		'pthread_rwlock_t',
		'SRWLOCK',
	] {
		assert !csrc.contains(marker), marker
	}
}

fn test_eval_comptime_flag_uses_target_os_preference() {
	windows_gen := new_target_test_gen('windows', [])
	assert windows_gen.eval_comptime_flag('windows')
	assert !windows_gen.eval_comptime_flag('linux')
	assert !windows_gen.eval_comptime_flag('macos')

	linux_gen := new_target_test_gen('linux', [])
	assert linux_gen.eval_comptime_flag('linux')
	assert !linux_gen.eval_comptime_flag('windows')

	macos_gen := new_target_test_gen('darwin', [])
	assert macos_gen.eval_comptime_flag('macos')
	assert macos_gen.eval_comptime_flag('darwin')
	assert macos_gen.eval_comptime_flag('mac')
	assert macos_gen.eval_comptime_flag('bsd')

	freebsd_gen := new_target_test_gen('freebsd', [])
	assert freebsd_gen.eval_comptime_flag('bsd')
}

fn test_eval_comptime_flag_uses_target_os_preference_for_extended_targets() {
	for target in [
		'freebsd',
		'openbsd',
		'netbsd',
		'dragonfly',
		'android',
		'termux',
		'ios',
		'solaris',
		'qnx',
		'serenity',
		'plan9',
		'vinix',
	] {
		gen := new_target_test_gen(target, [])
		assert gen.eval_comptime_flag(target)
		assert !gen.eval_comptime_flag('linux')
	}

	cross_gen := new_target_test_gen('cross', [])
	assert cross_gen.eval_comptime_flag('cross')
	assert !cross_gen.eval_comptime_flag('linux')

	freestanding_gen := new_target_test_gen_with_freestanding('linux', [], true)
	assert freestanding_gen.eval_comptime_flag('freestanding')
}

fn test_freestanding_none_comptime_selects_no_concrete_os() {
	gen := new_target_test_gen_with_freestanding('none', [], true)
	assert gen.eval_comptime_flag('freestanding')
	assert gen.eval_comptime_flag('none')
	assert !gen.eval_comptime_flag('linux')
	assert !gen.eval_comptime_flag('windows')
	assert !gen.eval_comptime_flag('macos')
	assert !gen.eval_comptime_flag('darwin')
	assert !gen.eval_comptime_flag('cross')

	hosted_with_define := new_target_test_gen('linux', ['none'])
	assert !hosted_with_define.eval_comptime_flag('none')
}

fn test_c_directives_use_target_os_preference() {
	assert c_directive_output_for_target('windows', 'windows', []).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('linux', 'windows', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('linux', 'linux', []).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('windows', 'linux', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('mac', 'macos', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('darwin', 'macos', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('bsd', 'freebsd', []).contains('#include <target_marker.h>')
}

fn test_c_directives_use_extended_target_os_preference() {
	for target in [
		'freebsd',
		'openbsd',
		'netbsd',
		'dragonfly',
		'android',
		'termux',
		'ios',
		'solaris',
		'qnx',
		'serenity',
		'plan9',
		'vinix',
	] {
		assert c_directive_output_for_target(target, target, []).contains('#include <target_marker.h>')
		assert !c_directive_output_for_target('linux', target, []).contains('#include <target_marker.h>')
	}
}

struct CrossGuardCase {
	cond  string
	guard string
}

fn test_c_directives_keep_cross_portable() {
	assert c_directive_output_for_target('', 'cross', []).contains('#include <target_marker.h>')
	cross_src := c_directive_output_for_target('cross', 'cross', [])
	assert cross_src.contains('#include <target_marker.h>')
	assert !cross_src.contains('#if')

	linux_src := c_directive_output_for_target('linux', 'cross', [])
	assert linux_src.contains('#if defined(__linux__)')
	assert linux_src.contains('#include <target_marker.h>')
	assert linux_src.contains('#endif')

	windows_src := c_directive_output_for_target('windows', 'cross', [])
	assert windows_src.contains('#if defined(_WIN32)')
	assert windows_src.contains('#include <target_marker.h>')
	assert windows_src.contains('#endif')

	macos_src := c_directive_output_for_target('macos', 'cross', [])
	assert macos_src.contains('#if ${apple_macos_cross_guard}')
	assert macos_src.contains('#include <target_marker.h>')
	assert macos_src.contains('#endif')
}

fn test_c_directives_keep_cross_extended_targets_portable() {
	for case in [
		CrossGuardCase{'mac', apple_macos_cross_guard},
		CrossGuardCase{'darwin', apple_macos_cross_guard},
		CrossGuardCase{'freebsd', 'defined(__FreeBSD__)'},
		CrossGuardCase{'openbsd', 'defined(__OpenBSD__)'},
		CrossGuardCase{'netbsd', 'defined(__NetBSD__)'},
		CrossGuardCase{'dragonfly', 'defined(__DragonFly__)'},
		CrossGuardCase{'android', 'defined(__ANDROID__)'},
		CrossGuardCase{'termux', 'defined(__TERMUX__)'},
		CrossGuardCase{'ios', apple_ios_cross_guard},
		CrossGuardCase{'solaris', 'defined(__sun)'},
		CrossGuardCase{'qnx', 'defined(__QNX__)'},
		CrossGuardCase{'serenity', 'defined(__serenity__)'},
		CrossGuardCase{'plan9', 'defined(__plan9__)'},
		CrossGuardCase{'vinix', 'defined(__vinix__)'},
	] {
		src := c_directive_output_for_target(case.cond, 'cross', [])
		assert src.contains('#if ${case.guard}')
		assert src.contains('#include <target_marker.h>')
		assert src.contains('#endif')
	}

	bsd_src := c_directive_output_for_target('bsd', 'cross', [])
	assert bsd_src.contains(apple_macos_cross_guard)
	assert bsd_src.contains('defined(__FreeBSD__)')
	assert bsd_src.contains('defined(__OpenBSD__)')
	assert bsd_src.contains('defined(__NetBSD__)')
	assert bsd_src.contains('defined(__DragonFly__)')
	assert !bsd_src.contains('defined(__APPLE__) ||')
	assert bsd_src.contains('#include <target_marker.h>')
}

fn test_c_directives_keep_cross_complex_os_conditions_portable() {
	not_linux_src := c_directive_output_for_target('!linux', 'cross', [])
	assert not_linux_src.contains('#if !(defined(__linux__))')
	assert not_linux_src.contains('#include <target_marker.h>')

	and_src := c_directive_output_for_target('linux && !windows', 'cross', [])
	assert and_src.contains('#if (defined(__linux__)) && (!(defined(_WIN32)))')
	assert and_src.contains('#include <target_marker.h>')

	cross_and_src := c_directive_output_for_target('cross && linux', 'cross', [])
	assert cross_and_src.contains('#if defined(__linux__)')
	assert cross_and_src.contains('#include <target_marker.h>')
	assert !cross_and_src.contains('cross')

	feature_and_src := c_directive_output_for_target('feature && linux', 'cross', [
		'feature',
	])
	assert feature_and_src.contains('#if defined(__linux__)')
	assert feature_and_src.contains('#include <target_marker.h>')
	assert !feature_and_src.contains('feature')

	not_feature_and_src := c_directive_output_for_target('!feature && linux', 'cross', [
		'feature',
	])
	assert !not_feature_and_src.contains('#include <target_marker.h>')

	missing_feature_and_src := c_directive_output_for_target('feature && linux', 'cross', [])
	assert !missing_feature_and_src.contains('#include <target_marker.h>')

	or_src := c_directive_output_for_target('linux || windows', 'cross', [])
	assert or_src.contains('#if (defined(__linux__)) || (defined(_WIN32))')
	assert or_src.contains('#include <target_marker.h>')

	not_and_src := c_directive_output_for_target('!windows && linux', 'cross', [])
	assert not_and_src.contains('#if (!(defined(_WIN32))) && (defined(__linux__))')
	assert not_and_src.contains('#include <target_marker.h>')

	or_and_src := c_directive_output_for_target('linux || windows && macos', 'cross', [])
	assert or_and_src.contains('#if (defined(__linux__)) || ((defined(_WIN32)) && (${apple_macos_cross_guard}))')
	assert or_and_src.contains('#include <target_marker.h>')

	not_group_src := c_directive_output_for_target('!(linux || windows) && macos', 'cross', [])
	assert not_group_src.contains('#if (!((defined(__linux__)) || (defined(_WIN32)))) && (${apple_macos_cross_guard})')
	assert not_group_src.contains('#include <target_marker.h>')

	assert c_directive_output_for_target('linux || windows && macos', 'linux', []).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('linux || windows && macos', 'macos', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('!(linux || windows) && macos', 'macos', []).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('!(linux || windows) && macos', 'windows', []).contains('#include <target_marker.h>')
}

fn test_c_directives_keep_termux_distinct_from_android() {
	assert c_directive_output_for_target('termux', 'termux', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('linux || termux', 'termux', []).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('android', 'termux', []).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('android && !termux', 'termux', []).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('android && !termux', 'android', []).contains('#include <target_marker.h>')
}

fn test_cross_optional_termux_flag_named_like_os_is_not_os_guarded() {
	assert !c_directive_output_for_target('termux ?', 'cross', []).contains('#include <target_marker.h>')
	optional_directive_src := c_directive_output_for_target('termux ?', 'cross', [
		'termux',
	])
	assert optional_directive_src.contains('#include <target_marker.h>')
	assert !optional_directive_src.contains('defined(__TERMUX__)')
}

fn test_optional_target_mode_directives_ignore_synthesized_defines() {
	assert !c_directive_output_for_target_with_explicit('cross ?', 'cross', ['cross'], [], false).contains('#include <target_marker.h>')
	assert c_directive_output_for_target_with_explicit('cross ?', 'cross', ['cross'], [
		'cross',
	], false).contains('#include <target_marker.h>')

	assert !c_directive_output_for_freestanding_target('freestanding ?', 'linux').contains('#include <target_marker.h>')
	assert c_directive_output_for_target_with_explicit('freestanding ?', 'linux', [
		'freestanding',
	], ['freestanding'], true).contains('#include <target_marker.h>')

	assert !c_directive_output_for_target_with_explicit('none ?', 'none', [
		'freestanding',
	], [], true).contains('#include <target_marker.h>')
	assert c_directive_output_for_target_with_explicit('none ?', 'none', [
		'freestanding',
	], ['none'], true).contains('#include <target_marker.h>')
}

fn test_comptime_if_directives_support_infix_os_conditions() {
	source := 'module main

\$if linux || windows {
	#include <active_or_marker.h>
}

	\$if linux && !windows {
		#include <active_and_marker.h>
	}

	\$if macos {
		#include <active_macos_marker.h>
	}

	\$if cross && linux {
		#include <cross_linux_marker.h>
	}

fn main() {}
'
	linux_src := generated_c_for_target_program_with_options('comptime_if_infix_linux', source,
		'linux', false, false)
	assert linux_src.contains('#include <active_or_marker.h>')
	assert linux_src.contains('#include <active_and_marker.h>')
	assert !linux_src.contains('#include <active_macos_marker.h>')
	assert !linux_src.contains('#include <cross_linux_marker.h>')

	macos_src := generated_c_for_target_program_with_options('comptime_if_infix_macos', source,
		'macos', false, false)
	assert !macos_src.contains('#include <active_or_marker.h>')
	assert !macos_src.contains('#include <active_and_marker.h>')
	assert macos_src.contains('#include <active_macos_marker.h>')
	assert !macos_src.contains('#include <cross_linux_marker.h>')

	windows_src := generated_c_for_target_program_with_options('comptime_if_infix_windows', source,
		'windows', false, false)
	assert windows_src.contains('#include <active_or_marker.h>')
	assert !windows_src.contains('#include <active_and_marker.h>')
	assert !windows_src.contains('#include <active_macos_marker.h>')
	assert !windows_src.contains('#include <cross_linux_marker.h>')

	cross_src := generated_c_for_target_program_with_options('comptime_if_infix_cross', source,
		'cross', false, false)
	assert cross_src.contains('defined(__linux__)')
	assert cross_src.contains('defined(_WIN32)')
	assert cross_src.contains('#include <active_or_marker.h>')
	assert cross_src.contains('!(defined(_WIN32))')
	assert cross_src.contains('#include <active_and_marker.h>')
	assert cross_src.contains(apple_macos_cross_guard)
	assert cross_src.contains('#include <active_macos_marker.h>')
	assert cross_src.contains('#if defined(__linux__)\n#include <cross_linux_marker.h>\n#endif')
}

fn test_comptime_if_pkgconfig_directives_select_available_branch() {
	if !vpref.comptime_pkgconfig_value('sqlite3') {
		return
	}
	source := 'module main

\$if \$pkgconfig("sqlite3") {
	#include "sqlite3.h"
} \$else \$if darwin {
	#include <sqlite_darwin_fallback_marker.h>
}

fn main() {}
'
	macos_src := generated_c_for_target_program_with_options('pkgconfig_sqlite3_directive_macos',
		source, 'macos', false, false)
	assert macos_src.contains('#include "sqlite3.h"')
	assert !macos_src.contains('#include <sqlite_darwin_fallback_marker.h>')
}

fn test_cross_comptime_if_else_directives_do_not_emit_selected_else_unguarded() {
	source := 'module main

\$if linux {
	#include <linux_marker.h>
} \$else {
	#include <fallback_marker.h>
}

fn main() {}
'
	cross_src := generated_c_for_target_program_with_options('comptime_if_else_cross', source,
		'cross', false, false)
	assert cross_src.contains('#if defined(__linux__)\n#include <linux_marker.h>\n#endif')
	assert cross_src.contains('#if !(defined(__linux__))\n#include <fallback_marker.h>\n#endif')
	assert cross_src.count('#include <fallback_marker.h>') == 1
	assert cross_src.count('#include <linux_marker.h>') == 1
}

fn test_cross_comptime_directive_scope_preserves_nested_preprocessor_blocks() {
	source := 'module main

\$if linux {
	#ifdef HAVE_FOO
	#include <foo.h>
	#endif
}

fn main() {}
'
	cross_src := generated_c_for_target_program_with_options('nested_preprocessor_cross', source,
		'cross', false, false)
	assert cross_src.contains('#if defined(__linux__)\n#ifdef HAVE_FOO\n#include <foo.h>\n#endif\n#endif')
	assert !cross_src.contains('#if defined(__linux__)\n#ifdef HAVE_FOO\n#endif\n#if defined(__linux__)')
	assert cross_src.count('#include <foo.h>') == 1
}

fn test_cross_user_define_comptime_directives_reduce_to_os_guards() {
	source := 'module main

\$if feature && linux {
	#include <feature_linux.h>
}

\$if !feature && linux {
	#include <disabled_feature_linux.h>
}

\$if missing_feature && linux {
	#include <missing_feature_linux.h>
}

fn main() {}
'
	feature_src := generated_c_for_target_program_with_defines('feature_directive_cross', source,
		'cross', ['feature'], false, false)
	assert feature_src.contains('#if defined(__linux__)\n#include <feature_linux.h>\n#endif')
	assert feature_src.count('#include <feature_linux.h>') == 1
	assert !feature_src.contains('#include <disabled_feature_linux.h>')
	assert !feature_src.contains('#include <missing_feature_linux.h>')
	assert !feature_src.contains('feature && linux')
}

fn test_cross_optional_user_flag_named_like_os_is_not_os_guarded() {
	assert !c_directive_output_for_target('linux ?', 'cross', []).contains('#include <target_marker.h>')
	optional_directive_src := c_directive_output_for_target('linux ?', 'cross', [
		'linux',
	])
	assert optional_directive_src.contains('#include <target_marker.h>')
	assert !optional_directive_src.contains('defined(__linux__)')

	source := 'module main

\$if linux ? {
	#include <optional_linux_marker.h>
}

fn main() {}
'
	missing_src := generated_c_for_target_program_with_options('optional_linux_missing_cross',
		source, 'cross', false, false)
	assert !missing_src.contains('#include <optional_linux_marker.h>')

	defined_src := generated_c_for_target_program_with_defines('optional_linux_defined_cross',
		source, 'cross', ['linux'], false, false)
	assert defined_src.contains('#include <optional_linux_marker.h>')
	assert !defined_src.contains('#if defined(__linux__)\n#include <optional_linux_marker.h>')
}

fn test_optional_user_flag_named_like_os_is_not_target_os_flag() {
	source := 'module main

\$if linux {
	#include <linux_marker.h>
}

\$if linux ? {
	#include <optional_linux_marker.h>
}

\$if linux ? || windows {
	#include <optional_linux_or_windows_marker.h>
}

fn main() {}
'
	linux_src := generated_c_for_target_program_with_options('optional_linux_plain_linux', source,
		'linux', false, false)
	assert linux_src.contains('#include <linux_marker.h>')
	assert !linux_src.contains('#include <optional_linux_marker.h>')
	assert !linux_src.contains('#include <optional_linux_or_windows_marker.h>')

	linux_defined_src := generated_c_for_target_program_with_defines('optional_linux_defined_linux',
		source, 'linux', ['linux'], false, false)
	assert linux_defined_src.contains('#include <linux_marker.h>')
	assert linux_defined_src.contains('#include <optional_linux_marker.h>')
	assert linux_defined_src.contains('#include <optional_linux_or_windows_marker.h>')

	windows_src := generated_c_for_target_program_with_options('optional_linux_windows', source,
		'windows', false, false)
	assert !windows_src.contains('#include <linux_marker.h>')
	assert !windows_src.contains('#include <optional_linux_marker.h>')
	assert windows_src.contains('#include <optional_linux_or_windows_marker.h>')
}

fn test_c_directives_keep_freestanding_user_os_directives_for_concrete_target() {
	assert c_directive_output_for_target('', 'linux', ['freestanding']).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('freestanding', 'linux', ['freestanding']).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('linux', 'linux', ['freestanding']).contains('#include <target_marker.h>')
	assert !c_directive_output_for_target('windows', 'linux', ['freestanding']).contains('#include <target_marker.h>')
	assert c_directive_output_for_target('windows', 'windows', ['freestanding']).contains('#include <target_marker.h>')
	assert c_directive_output_for_freestanding_target('freestanding', 'linux').contains('#include <target_marker.h>')
}

fn test_freestanding_none_c_directives_select_only_freestanding() {
	assert c_directive_output_for_freestanding_target('freestanding', 'none').contains('#include <target_marker.h>')
	for cond in ['linux', 'windows', 'macos', 'darwin', 'cross'] {
		src := c_directive_output_for_freestanding_target(cond, 'none')
		assert !src.contains('#include <target_marker.h>'), cond
	}
}

fn test_freestanding_none_comptime_if_directives_select_only_freestanding() {
	source := 'module main

\$if freestanding {
	#include <freestanding_none_marker.h>
}

\$if linux {
	#include <linux_none_marker.h>
}

\$if windows {
	#include <windows_none_marker.h>
}

\$if macos {
	#include <macos_none_marker.h>
}

\$if cross {
	#include <cross_none_marker.h>
}

fn main() {}
'
	src := generated_c_for_target_program_with_options('freestanding_none_comptime_if', source,
		'none', true, true)
	assert src.contains('#include <freestanding_none_marker.h>')
	assert !src.contains('#include <linux_none_marker.h>')
	assert !src.contains('#include <windows_none_marker.h>')
	assert !src.contains('#include <macos_none_marker.h>')
	assert !src.contains('#include <cross_none_marker.h>')
	assert_no_os_runtime_headers(src)
}

fn test_preamble_specializes_apple_includes_by_target() {
	linux_src := preamble_for_target('linux', [])
	assert !linux_src.contains('__APPLE__')
	assert !linux_src.contains('#include <mach/mach.h>')

	windows_src := preamble_for_target('windows', [])
	assert !windows_src.contains('__APPLE__')
	assert !windows_src.contains('#include <mach/mach.h>')

	macos_src := preamble_for_target('macos', [])
	assert macos_src.contains('#include <mach/mach.h>')
	assert !macos_src.contains('#ifdef __APPLE__')

	cross_src := preamble_for_target('cross', [])
	assert cross_src.contains('#if ${apple_macos_cross_guard}')
	assert !cross_src.contains('#ifdef __APPLE__')
	assert cross_src.contains('#include <mach/mach.h>')
	assert cross_src.contains('#if defined(_WIN32)\n#include <windows.h>\n#else\n#include <dirent.h>\n#include <pthread.h>\n#endif')
	assert cross_src.contains('#if defined(_WIN32)\ntypedef struct sync__RwMutex { SRWLOCK mutex; u32 inited; } sync__RwMutex;')
	assert cross_src.contains('#else\ntypedef struct sync__RwMutex { pthread_rwlock_t mutex; u32 inited; } sync__RwMutex;')
	full_cross_src := full_preamble_for_target('cross', [])
	assert full_cross_src.contains('#if defined(_WIN32)\n#include <windows.h>\n#else\n#include <unistd.h>')
	assert full_cross_src.contains('#include <pthread.h>\n#include <sys/time.h>')
	assert full_cross_src.contains('extern char** environ;\n#endif')
}

fn test_freestanding_minimal_preamble_avoids_implicit_os_runtime_headers() {
	src := preamble_for_freestanding_field('linux')
	assert src.contains('#include <stdbool.h>')
	assert src.contains('#include <stdint.h>')
	assert src.contains('#include <stddef.h>')
	assert src.contains('#include <string.h>')
	assert !src.contains('#include <stdio.h>')
	assert !src.contains('#include <stdlib.h>')
	assert !src.contains('#include <windows.h>')
	assert !src.contains('#include <dirent.h>')
	assert !src.contains('#include <pthread.h>')
	assert !src.contains('#include <mach/mach.h>')
	assert src.contains('typedef struct sync__RwMutex { u32 inited; } sync__RwMutex;')
	assert !src.contains('pthread_rwlock_t')
}

fn test_freestanding_full_preamble_avoids_implicit_os_runtime_headers() {
	src := full_preamble_for_freestanding_field('linux')
	assert src.contains('#include <stdbool.h>')
	assert src.contains('#include <stdint.h>')
	assert src.contains('#include <stddef.h>')
	assert src.contains('#include <string.h>')
	assert !src.contains('#include <unistd.h>')
	assert !src.contains('#include <windows.h>')
	assert !src.contains('#include <pthread.h>')
	assert !src.contains('#include <dirent.h>')
	assert !src.contains('#include <termios.h>')
	assert !src.contains('#include <sys/wait.h>')
	assert src.contains('typedef struct sync__RwMutex { u32 inited; } sync__RwMutex;')
	assert !src.contains('pthread_rwlock_t')
}

fn test_freestanding_field_full_preamble_avoids_implicit_os_runtime_headers() {
	src := full_preamble_for_freestanding_field('linux')
	assert src.contains('#include <stdbool.h>')
	assert src.contains('#include <stdint.h>')
	assert src.contains('#include <stddef.h>')
	assert src.contains('#include <string.h>')
	assert !src.contains('#include <unistd.h>')
	assert !src.contains('#include <windows.h>')
	assert !src.contains('#include <pthread.h>')
	assert !src.contains('#include <dirent.h>')
	assert !src.contains('#include <termios.h>')
	assert !src.contains('#include <sys/wait.h>')
	assert src.contains('typedef struct sync__RwMutex { u32 inited; } sync__RwMutex;')
	assert !src.contains('pthread_rwlock_t')
}

fn test_user_define_freestanding_does_not_enable_freestanding_codegen() {
	src := full_preamble_for_target('linux', ['freestanding'])
	assert src.contains('#include <unistd.h>')
	assert src.contains('#include <pthread.h>')
	assert src.contains('#include <stdio.h>')
	assert src.contains('#include <stdlib.h>')
	assert src.contains('pthread_rwlock_t')
	assert !src.contains('typedef struct sync__RwMutex { u32 inited; } sync__RwMutex;')
}

fn test_freestanding_none_preamble_avoids_implicit_os_runtime_headers() {
	src := full_preamble_for_options('none', [], true, false)
	assert src.contains('#include <stdbool.h>')
	assert src.contains('#include <stdint.h>')
	assert src.contains('#include <stddef.h>')
	assert src.contains('#include <string.h>')
	assert src.contains('typedef struct sync__RwMutex { u32 inited; } sync__RwMutex;')
	assert_no_os_runtime_headers(src)
}

fn test_freestanding_prealloc_preamble_avoids_implicit_free_contract() {
	freestanding_src := full_preamble_for_options('linux', [], true, true)
	assert !freestanding_src.contains('#define _VPREALLOC (1)')
	assert !freestanding_src.contains('static inline void _v_cfree')
	assert !freestanding_src.contains('#define free(p)')

	hosted_src := full_preamble_for_options('linux', [], false, true)
	assert hosted_src.contains('#define _VPREALLOC (1)')
	assert hosted_src.contains('static inline void _v_cfree(void *p) { free(p); }')
	assert hosted_src.contains('#define free(p) ((void)(p), (void)0)')
}

fn test_freestanding_platform_hooks_preamble_declares_requested_hooks_without_hosted_headers() {
	generic_src := full_preamble_for_options('linux', ['freestanding_hooks'], true, false)
	assert !generic_src.contains('v_platform_write')
	assert !generic_src.contains('v_platform_panic')
	assert !generic_src.contains('v_platform_malloc')

	spoofed_src := full_preamble_for_options('linux', ['freestanding_hooks', 'freestanding_output',
		'freestanding_panic', 'freestanding_alloc'], true, false)
	assert !spoofed_src.contains('v_platform_write')
	assert !spoofed_src.contains('v_platform_panic')
	assert !spoofed_src.contains('v_platform_malloc')

	output_src := full_preamble_for_freestanding_hooks(['output'])
	assert output_src.contains('isize v_platform_write(int stream, const u8* buf, isize len);')
	assert !output_src.contains('v_platform_panic')
	assert !output_src.contains('v_platform_malloc')
	assert !output_src.contains('#include <stdio.h>')
	assert !output_src.contains('#include <stdlib.h>')
	assert !output_src.contains('#include <unistd.h>')

	panic_src := full_preamble_for_freestanding_hooks(['panic'])
	assert panic_src.contains('void v_platform_panic(const u8* msg, isize len);')
	assert !panic_src.contains('v_platform_write')
	assert !panic_src.contains('v_platform_malloc')

	alloc_src := full_preamble_for_freestanding_hooks(['alloc'])
	assert alloc_src.contains('void* v_platform_malloc(isize n);')
	assert alloc_src.contains('void* v_platform_realloc(void* ptr, isize n);')
	assert alloc_src.contains('void v_platform_free(void* ptr);')
	assert !alloc_src.contains('v_platform_write')
	assert !alloc_src.contains('v_platform_panic')

	minimal_src := full_preamble_for_freestanding_hooks(['output', 'panic', 'alloc'])
	assert minimal_src.contains('isize v_platform_write(int stream, const u8* buf, isize len);')
	assert minimal_src.contains('void v_platform_panic(const u8* msg, isize len);')
	assert minimal_src.contains('void* v_platform_malloc(isize n);')
	assert minimal_src.contains('void* v_platform_realloc(void* ptr, isize n);')
	assert minimal_src.contains('void v_platform_free(void* ptr);')
	assert !minimal_src.contains('#include <stdio.h>')
	assert !minimal_src.contains('#include <stdlib.h>')
	assert !minimal_src.contains('#include <unistd.h>')
}

fn test_windows_minimal_preamble_avoids_posix_headers() {
	src := preamble_for_target('windows', [])
	assert !src.contains('#include <dirent.h>')
	assert !src.contains('#include <pthread.h>')
	assert !src.contains('#include <mach/mach.h>')
	assert src.contains('#include <windows.h>')
	assert src.contains('typedef struct sync__RwMutex { SRWLOCK mutex; u32 inited; } sync__RwMutex;')
	assert src.contains('AcquireSRWLockExclusive')
	assert !src.contains('static inline void sync__RwMutex_lock(sync__RwMutex* m) { (void)m; }')
	assert !src.contains('pthread_rwlock_t')
	assert src.contains('#include <stdio.h>')
	assert src.contains('#include <stdlib.h>')
}

fn test_windows_full_preamble_avoids_posix_headers() {
	src := full_preamble_for_target('windows', [])
	assert !src.contains('#include <unistd.h>')
	assert !src.contains('#include <sys/wait.h>')
	assert !src.contains('#include <termios.h>')
	assert !src.contains('#include <sys/ioctl.h>')
	assert !src.contains('#include <dirent.h>')
	assert !src.contains('#include <pthread.h>')
	assert src.contains('#include <windows.h>')
	assert src.contains('typedef struct sync__RwMutex { SRWLOCK mutex; u32 inited; } sync__RwMutex;')
	assert src.contains('AcquireSRWLockExclusive')
	assert !src.contains('static inline void sync__RwMutex_lock(sync__RwMutex* m) { (void)m; }')
	assert !src.contains('pthread_rwlock_t')
	assert src.contains('#include <stdio.h>')
	assert src.contains('#include <stdlib.h>')
	assert src.contains('#include <time.h>')
}

fn test_minimal_generated_c_does_not_emit_unused_runtime_fallbacks() {
	csrc := generated_c_for_target_program('minimal_runtime_fallbacks', 'module main

fn main() {}
')
	assert csrc.contains('int main(')
	assert_no_hosted_runtime_fallbacks(csrc)
	assert !csrc.contains('__v_live_init')
}

fn test_freestanding_skip_builtin_minimal_does_not_emit_hosted_runtime_fallbacks() {
	csrc := generated_c_for_target_program_with_options('freestanding_minimal_runtime_fallbacks', 'module main

fn main() {}
',
		'linux', true, true)
	assert csrc.contains('int main(')
	assert_no_hosted_runtime_fallbacks(csrc)
	assert !csrc.contains('__v_live_init')
}

fn test_live_reload_detects_live_functions_in_active_comptime_blocks() {
	csrc := generated_c_for_target_program('active_live_comptime', 'module main

\$if linux {
	@[live]
	fn hot_reload() {}
}

fn main() {}
')
	assert csrc.contains('void __v_live_init(void);')
	assert csrc.contains('__v_live_init();')
}

fn test_live_reload_ignores_live_functions_in_inactive_comptime_blocks() {
	csrc := generated_c_for_target_program('inactive_live_comptime', 'module main

\$if windows {
	@[live]
	fn hot_reload() {}
}

fn main() {}
')
	assert !csrc.contains('__v_live_init')
}

fn test_runtime_fallbacks_emit_arguments_only_when_referenced() {
	minimal_src := runtime_fallbacks_for_called_functions([])
	assert !minimal_src.contains('__attribute__((weak)) Array_string arguments()')

	args_src := runtime_fallbacks_for_called_functions(['arguments'])
	assert args_src.contains('__attribute__((weak)) Array_string arguments()')
	assert !args_src.contains('__attribute__((weak)) void _write_buf_to_fd')
}

fn test_freestanding_without_alloc_hook_arguments_refuses_without_hosted_alloc() {
	src := runtime_fallbacks_for_called_functions_with_hooks(['arguments'], ['output'])
	assert src.contains('__attribute__((weak)) Array_string arguments()')
	assert !src.contains('__new_array_with_default_noscan')
	assert !src.contains('tos_clone')
	assert !src.contains('array__push')
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_runtime_fallbacks_emit_stdout_helpers_only_when_printing_is_referenced() {
	print_src := runtime_fallbacks_for_called_functions(['println'])
	assert print_src.contains('__attribute__((weak)) void _write_buf_to_fd')
	assert print_src.contains('__attribute__((weak)) void _writeln_to_fd')
	assert print_src.contains('isize written = write(fd, ptr, remaining_bytes)')
	assert !print_src.contains('WriteFile(')
	assert !print_src.contains('__attribute__((weak)) Array_string arguments()')
	assert !print_src.contains('__attribute__((weak)) void eprint')
	assert !print_src.contains('__attribute__((weak)) void flush_stdout')
	assert !print_src.contains('__attribute__((weak)) void flush_stderr')
}

fn test_windows_runtime_fallbacks_use_writefile_for_stdout_helpers() {
	src := runtime_fallbacks_for_called_functions_for_target('windows', ['println'])
	assert src.contains('__attribute__((weak)) void _write_buf_to_fd')
	assert !src.contains('write(fd, ptr, remaining_bytes)')
	assert src.contains('GetStdHandle(fd == 2 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE)')
	assert src.contains('WriteFile(handle, ptr, (DWORD)remaining_bytes, &win_written, NULL)')
}

fn test_cross_runtime_fallbacks_guard_windows_stdout_helpers() {
	src := runtime_fallbacks_for_called_functions_for_target('cross', ['println'])
	assert src.contains('__attribute__((weak)) void _write_buf_to_fd')
	assert src.contains('#if defined(_WIN32)')
	assert src.contains('GetStdHandle(fd == 2 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE)')
	assert src.contains('WriteFile(handle, ptr, (DWORD)remaining_bytes, &win_written, NULL)')
	assert src.contains('#else\n\t\tisize written = write(fd, ptr, remaining_bytes);\n#endif')
}

fn test_runtime_fallbacks_emit_eprint_and_flush_only_when_referenced() {
	minimal_src := runtime_fallbacks_for_called_functions([])
	assert !minimal_src.contains('__attribute__((weak)) void eprint')
	assert !minimal_src.contains('__attribute__((weak)) void flush_stdout')
	assert !minimal_src.contains('__attribute__((weak)) void flush_stderr')

	eprint_src := runtime_fallbacks_for_called_functions(['eprint'])
	assert eprint_src.contains('__attribute__((weak)) void eprint')
	assert eprint_src.contains('__attribute__((weak)) void _write_buf_to_fd')
	assert eprint_src.contains('__attribute__((weak)) void flush_stdout')
	assert eprint_src.contains('__attribute__((weak)) void flush_stderr')
}

fn test_runtime_fallbacks_emit_structural_helpers_only_when_referenced() {
	minimal_src := runtime_fallbacks_for_called_functions([])
	assert_no_hosted_runtime_fallbacks(minimal_src)

	memdup_src := runtime_fallbacks_for_called_functions(['memdup'])
	assert memdup_src.contains('__attribute__((weak)) u8* malloc_noscan')
	assert memdup_src.contains('__attribute__((weak)) void* memdup')

	array_src := runtime_fallbacks_for_called_functions(['Array_string_contains',
		'Array_string_index', 'Array_int_str'])
	assert array_src.contains('__attribute__((weak)) bool Array_string_contains')
	assert array_src.contains('__attribute__((weak)) int Array_string_index')
	assert array_src.contains('__attribute__((weak)) string Array_int_str')
	assert !array_src.contains('__attribute__((weak)) u8* malloc_noscan')

	bits_src := runtime_fallbacks_for_called_functions(['__at_least_one', 'bits__leading_zeros_64',
		'bits__rotate_left_32', 'f32__str'])
	assert bits_src.contains('__attribute__((weak)) u64 __at_least_one')
	assert bits_src.contains('__attribute__((weak)) int bits__leading_zeros_64')
	assert bits_src.contains('__attribute__((weak)) u32 bits__rotate_left_32')
	assert bits_src.contains('__attribute__((weak)) string f64__str')
	assert bits_src.contains('__attribute__((weak)) string f32__str')
}

fn test_runtime_fallbacks_emit_float_strg_dependency_closure() {
	f32_strg_src := runtime_fallbacks_for_called_functions(['f32__strg'])
	f64_str_idx := f32_strg_src.index('__attribute__((weak)) string f64__str(f64 x)') or {
		panic('missing f64__str fallback')
	}
	f64_strg_idx := f32_strg_src.index('__attribute__((weak)) string f64__strg') or {
		panic('missing f64__strg fallback')
	}
	f32_strg_idx := f32_strg_src.index('__attribute__((weak)) string f32__strg') or {
		panic('missing f32__strg fallback')
	}
	assert f32_strg_src.contains('__attribute__((weak)) u8* malloc_noscan')
	assert f64_str_idx < f64_strg_idx
	assert f64_strg_idx < f32_strg_idx
	assert f32_strg_src.contains('\treturn f64__str(x);')
	assert f32_strg_src.contains('\treturn f64__strg((f64)x);')

	f64_strg_src := runtime_fallbacks_for_called_functions(['f64__strg'])
	f64_str_direct_idx := f64_strg_src.index('__attribute__((weak)) string f64__str(f64 x)') or {
		panic('missing f64__str fallback for f64__strg')
	}
	f64_strg_direct_idx := f64_strg_src.index('__attribute__((weak)) string f64__strg') or {
		panic('missing f64__strg fallback for direct reference')
	}
	assert f64_strg_src.contains('__attribute__((weak)) u8* malloc_noscan')
	assert f64_str_direct_idx < f64_strg_direct_idx

	existing_c_src :=
		runtime_fallbacks_for_existing_c_source('void keep_generated_code(void) { f32__strg((f32)0); }')
	existing_f64_str_idx := existing_c_src.index('__attribute__((weak)) string f64__str(f64 x)') or {
		panic('missing f64__str fallback for existing C reference')
	}
	existing_f64_strg_idx := existing_c_src.index('__attribute__((weak)) string f64__strg') or {
		panic('missing f64__strg fallback for existing C reference')
	}
	existing_f32_strg_idx := existing_c_src.index('__attribute__((weak)) string f32__strg') or {
		panic('missing f32__strg fallback for existing C reference')
	}
	assert existing_f64_str_idx < existing_f64_strg_idx
	assert existing_f64_strg_idx < existing_f32_strg_idx
}

fn test_float_str_fallbacks_rely_on_kept_builtin_float_runtime() {
	for path in [
		'vlib/builtin/float.c.v',
		'vlib/strconv/ftoa.c.v',
		'vlib/strconv/f32_str.c.v',
		'vlib/strconv/f64_str.c.v',
	] {
		assert is_builtin_runtime_keep_file(path), path
	}
}

fn test_runtime_fallbacks_emit_structural_helpers_for_existing_c_references() {
	src :=
		runtime_fallbacks_for_existing_c_source('void keep_generated_code(void) { memdup(0, 0); Array_string_contains((Array_string){0}, (string){0}); }')
	assert src.contains('void keep_generated_code(void)')
	assert src.contains('__attribute__((weak)) u8* malloc_noscan')
	assert src.contains('__attribute__((weak)) void* memdup')
	assert src.contains('__attribute__((weak)) bool Array_string_contains')
	assert !src.contains('__attribute__((weak)) bool Array_int_contains')
}

fn test_freestanding_output_hooks_runtime_fallbacks_avoid_hosted_io() {
	src := runtime_fallbacks_for_called_functions_with_hooks(['println', 'eprint'], [
		'output',
	])
	assert src.contains('__attribute__((weak)) void _write_buf_to_fd')
	assert src.contains('v_platform_write(fd, ptr, remaining_bytes)')
	assert src.contains('__attribute__((weak)) void _writeln_to_fd')
	assert src.contains('__attribute__((weak)) void eprint')
	assert src.contains('__attribute__((weak)) void flush_stdout')
	assert src.contains('__attribute__((weak)) void flush_stderr')
	assert_no_hosted_output_runtime(src)
}

fn test_freestanding_without_output_hook_runtime_fallbacks_refuse_without_hosted_io() {
	src := runtime_fallbacks_for_called_functions_with_hooks(['println', 'eprint'], [
		'panic',
	])
	assert src.contains('__attribute__((weak)) void _write_buf_to_fd')
	assert src.contains('__attribute__((weak)) void _writeln_to_fd')
	assert src.contains('__attribute__((weak)) void eprint')
	assert src.contains('__attribute__((weak)) void flush_stdout')
	assert src.contains('__attribute__((weak)) void flush_stderr')
	assert !src.contains('v_platform_write')
	assert_freestanding_output_refusal_without_hosted_io(src)
}

fn test_freestanding_spoofed_hook_defines_do_not_call_platform_hooks() {
	src := runtime_fallbacks_for_called_functions_with_options(['println', 'panic', 'memdup'], [
		'freestanding_hooks',
		'freestanding_output',
		'freestanding_panic',
		'freestanding_alloc',
	], true)
	assert !src.contains('v_platform_write')
	assert !src.contains('v_platform_panic')
	assert !src.contains('v_platform_malloc')
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_freestanding_output_hook_non_string_print_refuses_without_hosted_formatting() {
	src := generated_c_for_target_program_with_hooks('freestanding_output_print_non_string', 'module main

fn println(v int) {}

fn main() {
	println(123)
}
		', [
		'output',
	])
	assert !src.contains('int__str(')
	assert_freestanding_format_refusal_without_hosted_formatting(src)
}

fn test_freestanding_output_hook_voidptr_print_refuses_without_hosted_formatting() {
	src := generated_c_for_target_program_with_hooks('freestanding_output_print_voidptr', 'module main

fn print(v voidptr) {}

fn main() {
	print(voidptr(0))
}
		', [
		'output',
	])
	assert !src.contains('int__str(')
	assert_freestanding_format_refusal_without_hosted_formatting(src)
}

fn test_freestanding_string_interpolation_refuses_without_hosted_formatting() {
	src := generated_c_for_target_program_with_hooks('freestanding_string_interpolation', "module main

fn main() {
	_ := '\${1}'
}
		", [
		'alloc',
	])
	assert !src.contains('snprintf(')
	assert !src.contains('memdup(')
	assert_freestanding_format_refusal_without_hosted_formatting(src)
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_alloc_hooks_runtime_fallbacks_avoid_hosted_alloc() {
	src := runtime_fallbacks_for_called_functions_with_hooks(['memdup', 'DenseArray__zeros_to_end'], [
		'alloc',
	])
	assert src.contains('__attribute__((weak)) u8* malloc_noscan')
	assert src.contains('return (u8*)v_platform_malloc(n);')
	assert src.contains('__attribute__((weak)) void* memdup')
	assert src.contains('void* res = v_platform_malloc(sz);')
	assert src.contains('__attribute__((weak)) void DenseArray__zeros_to_end')
	assert src.contains('void* tmp_value = v_platform_malloc(d->value_bytes);')
	assert src.contains('v_platform_free(tmp_value);')
	assert src.contains('v_platform_realloc(d->values, d->value_bytes * d->cap);')
	assert_no_hosted_alloc_runtime(src)
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_without_alloc_hook_heap_helpers_refuse_without_hosted_alloc() {
	g := new_target_test_gen_with_freestanding('linux', [
		'freestanding_hooks',
		'freestanding_output',
		'freestanding_panic',
	], true)
	malloc_expr := g.c_heap_malloc_call('n')
	realloc_expr := g.c_heap_realloc_call('p', 'n')
	free_expr := g.c_heap_free_call('p')
	helper_src := '${malloc_expr}\n${realloc_expr}\n${free_expr}'
	assert helper_src.contains('(void*)0')
	assert helper_src.contains('(void)0')
	assert_freestanding_alloc_refusal_without_hosted_heap(helper_src)
}

fn test_freestanding_without_alloc_hook_runtime_heap_paths_refuse_without_hosted_alloc() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks('void keep_generated_code(void) { memdup(0, 1); DenseArray__zeros_to_end(0); }', [
		'output',
		'panic',
	])
	assert src.contains('__attribute__((weak)) void* memdup')
	assert src.contains('__attribute__((weak)) void DenseArray__zeros_to_end')
	assert src.contains('void* res = ({ _Static_assert')
	assert src.contains('d->values = ({ _Static_assert')
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_freestanding_skip_builtin_runtime_heap_helpers_refuse_in_generated_c() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) { array__push(0, 0); string__plus((string){0}, (string){0}); new_map(); }', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'array__push')
	assert_freestanding_heap_runtime_refusal(src, 'string__plus')
	assert_freestanding_heap_runtime_refusal(src, 'new_map')
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_skip_builtin_runtime_heap_helper_table_refuses_each_helper() {
	for helper in freestanding_heap_runtime_helper_names() {
		src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) { ${helper}(); }', [
			'output',
			'panic',
			'alloc',
		])
		assert_freestanding_heap_runtime_refusal(src, helper)
		assert !src.contains('__attribute__((weak))')
	}
}

fn test_runtime_symbol_scan_ignores_comments_and_string_literals() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) {
	const char* literal = "array__push(0, 0)";
	/* array__push(0, 0); */
	// &array__push
	array__push_many(0, 0, 0);
}', [
		'output',
		'panic',
		'alloc',
	])
	assert !src.contains('_Static_assert(0, "${freestanding_missing_heap_runtime_message}: array__push")')
	assert_freestanding_heap_runtime_refusal(src, 'array__push_many')
}

fn test_runtime_symbol_scan_detects_bare_callback_identifier() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) {
	array__sort_with_compare(0, compare_strings);
}', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'array__sort_with_compare')
	assert_freestanding_heap_runtime_refusal(src, 'compare_strings')
}

fn test_freestanding_skip_builtin_structural_array_fallbacks_refuse_clearly() {
	for helper in ['Array_int_contains', 'Array_string_contains', 'Array_string_index',
		'Array_int_str'] {
		src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) { ${helper}(); }', [
			'output',
			'panic',
			'alloc',
		])
		assert_freestanding_heap_runtime_refusal(src, helper)
	}
}

fn test_freestanding_skip_builtin_ierror_str_refuses_with_panic_hook() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) { v_panic(IError__str((IError){0})); }', [
		'output',
		'panic',
		'alloc',
	])
	assert src.contains('v_platform_panic(s.str, s.len);')
	assert_freestanding_heap_runtime_refusal(src, 'IError__str')
}

fn test_freestanding_skip_builtin_string_lt_from_v_code_refuses_runtime_helper() {
	src := generated_c_for_target_program_with_hooks('freestanding_string_lt_runtime_helper', 'module main

fn main() {
	a := "a"
	b := "b"
	_ := a < b
}
		', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'string__lt')
}

fn test_freestanding_skip_builtin_string_concat_from_v_code_refuses_runtime_helper() {
	src := generated_c_for_target_program_with_hooks('freestanding_string_concat_runtime_helper', 'module main

fn main() {
	a := "a"
	b := "b"
	c := "c"
	_ := a + b + c
}
		', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'string__plus_two')
}

fn test_freestanding_skip_builtin_string_method_runtime_helpers_refuse_in_generated_c() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) {
	string__contains((string){0}, (string){0});
	string__starts_with((string){0}, (string){0});
	string__ends_with((string){0}, (string){0});
	string__clone((string){0});
	string__split((string){0}, (string){0});
}', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'string__contains')
	assert_freestanding_heap_runtime_refusal(src, 'string__starts_with')
	assert_freestanding_heap_runtime_refusal(src, 'string__ends_with')
	assert_freestanding_heap_runtime_refusal(src, 'string__clone')
	assert_freestanding_heap_runtime_refusal(src, 'string__split')
}

fn test_freestanding_skip_builtin_array_equality_from_v_code_refuses_runtime_helper() {
	src := generated_c_for_target_program_with_hooks('freestanding_array_eq_runtime_helper', 'module main

fn main() {
	a := [1]
	b := [1]
	_ := a == b
}
		', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, '__v2_array_eq')
}

fn test_freestanding_skip_builtin_array_sort_runtime_helper_refuses_in_generated_c() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) { array__sort(0, 0); array__sort_with_compare(0, 0); }', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'array__sort')
	assert_freestanding_heap_runtime_refusal(src, 'array__sort_with_compare')
}

fn test_freestanding_skip_builtin_map_equality_from_v_code_refuses_runtime_helpers() {
	src := generated_c_for_target_program_with_hooks('freestanding_map_eq_runtime_helpers', 'module main

fn main() {
	a := {
		"x": 1
	}
	b := {
		"x": 1
	}
	_ := a == b
}
		', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'map__exists')
	assert_freestanding_heap_runtime_refusal(src, 'map__get')
	assert_freestanding_heap_runtime_refusal(src, 'DenseArray__has_index')
	assert_freestanding_heap_runtime_refusal(src, 'DenseArray__key')
	assert_freestanding_heap_runtime_refusal(src, 'DenseArray__value')
}

fn test_freestanding_skip_builtin_map_method_runtime_helpers_refuse_in_generated_c() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) {
	map__clear(0);
	map__delete(0, 0);
	map__keys(0);
	map__reserve(0, 0);
	map__move(0);
	map__values(0);
}', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'map__clear')
	assert_freestanding_heap_runtime_refusal(src, 'map__delete')
	assert_freestanding_heap_runtime_refusal(src, 'map__keys')
	assert_freestanding_heap_runtime_refusal(src, 'map__reserve')
	assert_freestanding_heap_runtime_refusal(src, 'map__move')
	assert_freestanding_heap_runtime_refusal(src, 'map__values')
}

fn test_freestanding_skip_builtin_generic_array_contains_from_v_code_refuses_runtime_helper() {
	src := generated_c_for_target_program_with_hooks('freestanding_generic_array_contains_runtime_helper', 'module main

struct Point {
	x int
}

fn main() {
	points := [Point{
		x: 1
	}]
	needle := Point{
		x: 1
	}
	_ := needle in points
}
		', [
		'output',
		'panic',
		'alloc',
	])
	assert src.contains('_Static_assert(0, "${freestanding_missing_heap_runtime_message}: Array_')
	assert src.contains('_contains")')
	assert !src.contains('__attribute__((weak)) bool Array_')
	assert !src.contains('array__get(')
}

fn test_freestanding_skip_builtin_string_substr_or_runtime_helper_refuses_in_generated_c() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks_skip_builtin('void keep_generated_code(void) { string__substr_or((string){0}, 0, 1, (string){0}); }', [
		'output',
		'panic',
		'alloc',
	])
	assert_freestanding_heap_runtime_refusal(src, 'string__substr_or')
}

fn test_freestanding_panic_hook_runtime_fallback_avoids_hosted_exit() {
	src := runtime_fallbacks_for_called_functions_with_hooks(['panic'], ['panic'])
	assert src.contains('__attribute__((weak)) void v_panic(string s)')
	assert src.contains('v_platform_panic(s.str, s.len);')
	assert src.contains('for (;;) {}')
	assert !src.contains('C.exit')
	assert !src.contains('exit(')
}

fn test_freestanding_without_panic_hook_runtime_fallback_refuses_without_unresolved_symbol() {
	src := runtime_fallbacks_for_called_functions_with_hooks(['panic'], ['output'])
	assert src.contains('__attribute__((weak)) void v_panic(string s)')
	assert src.contains('for (;;) {}')
	assert_freestanding_panic_refusal_without_hosted_exit(src)
}

fn test_freestanding_hooks_for_existing_c_references_do_not_drop_generated_c() {
	src := runtime_fallbacks_for_existing_c_source_with_hooks('void keep_generated_code(void) { v_panic((string){0}); memdup(0, 1); }', [
		'output',
		'panic',
		'alloc',
	])
	assert src.contains('void keep_generated_code(void)')
	assert src.contains('v_platform_panic(s.str, s.len);')
	assert src.contains('void* res = v_platform_malloc(sz);')
	assert_no_hosted_alloc_runtime(src)
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_alloc_hook_generated_c_heap_address_uses_platform_alloc() {
	src := generated_c_for_target_program_with_hooks('freestanding_alloc_heap_address', 'module main

struct Point {
	x int
}

fn main() {
	p := &Point{
		x: 1
	}
	_ := p.x
}
		', [
		'alloc',
	])
	assert src.contains('v_platform_malloc(sizeof(Point))')
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_without_alloc_hook_heap_address_refuses_without_hosted_alloc() {
	src := generated_c_for_target_program_with_hooks('freestanding_missing_alloc_heap_address', 'module main

struct Point {
	x int
}

fn main() {
	p := &Point{
		x: 1
	}
	_ := p.x
}
		', [
		'output',
		'panic',
	])
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_freestanding_without_alloc_hook_returned_local_clone_refuses_without_hosted_alloc() {
	src := generated_c_for_target_program_with_hooks('freestanding_missing_alloc_returned_local_clone', 'module main

struct Point {
	x int
}

fn make() &Point {
	value := Point{
		x: 1
	}
	ptr := &value
	return ptr
}

fn main() {
	p := make()
	_ := p.x
}
		', [
		'output',
		'panic',
	])
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_freestanding_alloc_hook_interface_heap_cast_uses_platform_alloc() {
	src := generated_c_for_target_program_with_hooks('freestanding_alloc_interface_heap_cast', 'module main

interface Runner {
	next() int
}

struct Concrete {}

fn (mut c Concrete) next() int {
	_ = c
	return 7
}

fn make() &Runner {
	mut c := Concrete{}
	return &Runner(c)
}

fn main() {
	r := make()
	_ := r.next()
}
		', [
		'alloc',
	])
	assert src.contains('Runner* _iface_t = (Runner*)v_platform_malloc(sizeof(Runner))')
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_without_alloc_hook_interface_heap_cast_refuses_without_hosted_alloc() {
	src := generated_c_for_target_program_with_hooks('freestanding_missing_alloc_interface_heap_cast', 'module main

interface Runner {
	next() int
}

struct Concrete {}

fn (mut c Concrete) next() int {
	_ = c
	return 7
}

fn make() &Runner {
	mut c := Concrete{}
	return &Runner(c)
}

fn main() {
	r := make()
	_ := r.next()
}
		', [
		'output',
		'panic',
	])
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_freestanding_alloc_hook_soa_helpers_use_platform_alloc() {
	src := soa_companion_for_hooks(['alloc'])
	assert src.contains('v_platform_malloc(cap * sizeof(int))')
	assert src.contains('memset(soa.x, 0, cap * sizeof(int));')
	assert src.contains('v_platform_realloc(soa->x, new_cap * sizeof(int))')
	assert src.contains('v_platform_free(soa->x);')
	assert_no_raw_heap_runtime(src)
}

fn test_freestanding_without_alloc_hook_soa_helpers_refuse_without_hosted_alloc() {
	src := soa_companion_for_hooks(['output', 'panic'])
	assert src.contains('soa.x = (int*)({ _Static_assert')
	assert src.contains('soa->x = (int*)({ _Static_assert')
	assert src.contains('({ _Static_assert(0, "${freestanding_missing_alloc_hook_message}"); (void)0; });')
	assert_freestanding_alloc_refusal_without_hosted_heap(src)
}

fn test_freestanding_minimal_hooks_runtime_and_heap_paths_avoid_raw_alloc() {
	hooks := ['output', 'panic', 'alloc']
	runtime_src := runtime_fallbacks_for_existing_c_source_with_hooks('void keep_generated_code(void) { v_panic((string){0}); memdup(0, 1); DenseArray__zeros_to_end(0); }',
		hooks)
	assert runtime_src.contains('v_platform_panic(s.str, s.len);')
	assert runtime_src.contains('v_platform_malloc(sz);')
	assert runtime_src.contains('v_platform_realloc(d->values, d->value_bytes * d->cap);')
	assert_no_raw_heap_runtime(runtime_src)

	soa_src := soa_companion_for_hooks(hooks)
	assert soa_src.contains('v_platform_malloc(cap * sizeof(int))')
	assert soa_src.contains('v_platform_realloc(soa->x, new_cap * sizeof(int))')
	assert_no_raw_heap_runtime(soa_src)
}

fn test_freestanding_fixed_array_voidptr_stringification_avoids_hosted_formatting() {
	src := fixed_array_voidptr_str_write_for_hooks(['alloc'])
	assert src.contains('uintptr_t _addr = (uintptr_t)(items[0]);')
	assert src.contains('"0123456789abcdef"')
	assert src.contains('strings__Builder__write_string(&sb')
	assert_no_hosted_formatting_or_abort_runtime(src)
}

fn test_freestanding_unresolved_generic_stub_uses_panic_hook_without_hosted_abort() {
	src := unresolved_generic_stub_for_hooks(['panic'])
	assert src.contains('v_platform_panic((const u8*)')
	assert src.contains('for (;;) {}')
	assert !src.contains('#error')
	assert_no_hosted_formatting_or_abort_runtime(src)
}

fn test_freestanding_unresolved_generic_stub_without_panic_hook_refuses_without_hosted_abort() {
	src := unresolved_generic_stub_for_hooks(['output'])
	assert src.contains('#error "v2: unresolved generic stub requires freestanding panic platform hook"')
	assert_no_hosted_formatting_or_abort_runtime(src)
}
