module driver

import os
import time
import v.cmdexec
import v.pref
import v.gen.c as cgen
import v.util.cache_refresh

fn test_refresh_records_entry_only_for_project_interface_scopes() {
	vroot := os.dir(@VEXE)
	state := V3ModuleCacheState{
		module_sources: {
			'io':    [os.join_path(vroot, 'vlib', 'io', 'io.v')]
			'os':    [os.join_path(vroot, 'vlib', 'os', 'os.v')]
			'local': [os.join_path(os.vtmp_dir(), 'project', 'local', 'local.v')]
		}
	}
	standard := 'IError=Error,MessageError,io.Eof\nio.Writer=os.File\n#module IError=builtin\n#module Error=builtin\n#module MessageError=builtin'
	assert !v3_cache_refresh_needs_entry(standard, &state, vroot)
	assert !v3_cache_refresh_needs_entry(standard + '\nShape=Circle\n#module Shape=main\n#module Circle=main', &state, vroot)
	assert v3_cache_refresh_needs_entry(standard.replace('io.Writer=os.File', 'io.Writer=os.File,Sink') + '\n#module Sink=main', &state, vroot)
	assert v3_cache_refresh_needs_entry(standard.replace('io.Writer=os.File', 'io.Writer=os.File,local.Sink'), &state, vroot)
}

fn v3_driver_test_executable() string {
	if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		return os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	}
	return @VEXE
}

fn test_input_is_cmd_v_accepts_relative_entry_file() {
	assert input_is_cmd_v('cmd/v')
	assert input_is_cmd_v('cmd/v/v.v')
}

fn test_v3_tcc_backtrace_enabled() {
	assert !v3_tcc_backtrace_enabled('macos', 'arm64', false)
	assert v3_tcc_backtrace_enabled('macos', 'amd64', false)
	assert v3_tcc_backtrace_enabled('linux', 'arm64', false)
	assert !v3_tcc_backtrace_enabled('linux', 'arm64', true)
}

fn test_v3_default_compiler_uses_implicit_tcc() {
	bundled_tcc := os.join_path(os.temp_dir(), 'thirdparty', 'tcc', 'tcc.exe')
	system_tcc := os.join_path(os.temp_dir(), 'bin', 'tcc.exe')
	assert v3_select_implicit_c_compiler('cc', false, bundled_tcc) == bundled_tcc
	assert v3_select_implicit_c_compiler('cc', false, system_tcc) == system_tcc
	assert v3_select_implicit_c_compiler('cc', false, '') == 'cc'
	assert v3_select_implicit_c_compiler('clang', true, bundled_tcc) == 'clang'
}

fn test_v3_platform_c_compiler() {
	assert v3_platform_c_compiler('windows') == 'gcc'
	assert v3_platform_c_compiler('linux') == 'cc'
	assert v3_platform_c_compiler('macos') == 'cc'
}

fn test_v3_cache_accepts_default_cc_alias() {
	assert v3_c_compiler_matches_default_cc('cc')
	assert !v3_c_compiler_matches_default_cc('v3-definitely-missing-c-compiler')
	$if macos {
		assert v3_c_compiler_matches_default_cc('clang')
	}
}

fn test_module_cache_refresh_keeps_build_flags_and_discards_program_arguments() {
	args := ['-cc', 'tcc', '-prod', '-g', '-cflags', '-DVALUE="a b"', '-d', 'feature', '-o',
		'my program', '-ldflags', '-L../libs -lanswer', '-show-timings', 'run', 'main.v', '-prod',
		'program argument']
	compiler := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	flags := v3_module_cache_refresh_args(args, compiler, [], [])
	assert flags == ['-prod', '-g', '-cflags', '"-DVALUE=a b"', '-d', 'feature', '-cc', compiler]
}

fn test_module_cache_refresh_preserves_expanded_absolute_search_paths() {
	paths := pref.expand_module_search_paths('modules/../modules|@vlib|@vmodules', @VEXEROOT).map(os.abs_path(it))
	compiler := os.find_abs_path_of_executable('cc') or { os.abs_path('cc') }
	flags := v3_module_cache_refresh_args(['-path', 'ignored|@vlib', 'main.v'], 'cc', paths, [])
	assert flags == ['-path', paths.join('|'), '-cc', compiler]
	assert paths[0] == os.join_path(os.getwd(), 'modules')
	assert paths[1] == os.join_path(@VEXEROOT, 'vlib')
}

fn test_module_cache_refresh_filters_link_operands_across_cflags_and_keeps_debug_order() ! {
	args := ['-cflags', '-L', '-cflags', '../libs -l answer -I "include path" -D NAME=foo.o', '-g',
		'-cflags', '-DVALUE="a b" -Wl,-rpath,../libs extra.o -framework Cocoa -O2', '-cflags',
		'-U', '-cflags', 'REMOVED', 'main.v']
	flags := v3_module_cache_refresh_args(args, 'cc', [], [])
	mut compiled := []string{}
	mut i := 0
	for i < flags.len {
		if flags[i] == '-cflags' {
			compiled << cmdexec.split_args(flags[i + 1])!
			i += 2
			continue
		}
		if flags[i] == '-g' { compiled << '-g' }
		i++
	}
	assert compiled == ['-I', 'include path', '-D', 'NAME=foo.o', '-g', '-DVALUE=a b', '-O2', '-U',
		'REMOVED']
	for value in ['-DNAME="quoted value"', "-DNAME=it's fine", '-IC:\\Program Files\\SDK',
		'-DNAME=first\nsecond', ''] {
		assert cmdexec.split_args(v3_cache_refresh_c_flag(value))! == [value]
	}
}

fn test_module_cache_refresh_preserves_supported_modes_and_aliases() {
	modes := ['-musl', '-glibc', '-ownership', '--ownership', '-no-std', '--no-std', '-strict',
		'-cstrict', '--c99', '-dfeature=value', '-building-v', '-building_v', macos_v3_compat_c99_flag]
	for mode in modes {
		flags := v3_module_cache_refresh_args([mode, 'main.v'], 'cc', [], [])
		assert flags[0] == mode, flags.str()
	}
}

fn test_module_cache_refresh_reads_only_project_flags_in_refresh_children() ! {
	previous := os.environ()
	defer {
		for name in ['V3_CACHE_REFRESH', 'V3_CACHE_REFRESH_PROJECT_FLAGS'] {
			if name in previous { os.setenv(name, previous[name], true) } else { os.unsetenv(name) }
		}
	}
	groups := [cache_refresh.FlagGroup{
		flags:   ['-DPROJECT', '-include', 'config.h', '-I', 'include path']
		in_main: true
	}]
	os.setenv('V3_CACHE_REFRESH_PROJECT_FLAGS', cache_refresh.encode_project_flags(groups), true)
	os.unsetenv('V3_CACHE_REFRESH')
	assert v3_cache_refresh_project_flags()! == []cache_refresh.FlagGroup{}
	os.setenv('V3_CACHE_REFRESH', '1', true)
	assert v3_cache_refresh_project_flags()! == groups
	os.unsetenv('V3_CACHE_REFRESH_PROJECT_FLAGS')
	assert v3_cache_refresh_project_flags()! == []cache_refresh.FlagGroup{}
}

fn test_cached_tcc_main_command_refuses_production_mode() ! {
	compiler := v3_driver_test_executable()
	if !os.is_executable(os.join_path(os.dir(compiler), 'thirdparty', 'tcc', 'tcc.exe')) { return }
	root := os.join_path(os.vtmp_dir(), 'vup_tcc_optimization_${os.getpid()}')
	os.mkdir_all(root)!
	saved := os.environ()
	defer {
		for name in ['V3CACHE', 'VEXE', 'VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH',
			'V_SKIP_VVMRC'] {
			if name in saved { os.setenv(name, saved[name], true) } else { os.unsetenv(name) }
		}
		os.rmdir_all(root) or {}
	}
	os.setenv('VEXE', compiler, true)
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	os.setenv('V_SKIP_VVMRC', '1', true)
	for name in ['VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_REFRESH'] { os.unsetenv(name) }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import math\nfn main() { println(math.abs(-3.0)) }\n')!
	// TinyCC is not a -prod compiler, so the same build with -prod stops before compiling.
	for mode in [['-prod'], ['-prod', '-no-prod-options']] {
		refused := os.exec([compiler, '-cc', 'tcc', '-gc', 'none', '-showcc', ...mode, '-o',
			os.join_path(root, 'program'), source])
		assert refused.exit_code == 1, refused.output
		assert refused.output.contains('tcc is not allowed with -prod'), refused.output
	}
	for mode in [[]string{}] {
		result := os.exec([compiler, '-cc', 'tcc', '-gc', 'none', '-showcc', ...mode, '-o',
			os.join_path(root, 'program'), source])
		assert result.exit_code == 0, result.output
		commands := result.output.split_into_lines().filter(it.trim_space().starts_with('> ')
			&& it.contains('src.c') && it.contains('tcc'))
		assert commands.len == 1, result.output
		command := cmdexec.split_args(commands[0].trim_space()[2..])!
		assert command.any(it.contains('v3_module_cache_') && it.ends_with('.o')), commands[0]
		assert ('-O3' in command) == ('-prod' in mode && '-no-prod-options' !in mode), commands[0]
		assert '-flto' !in command, commands[0]
	}
}

fn test_module_cache_refresh_preserves_effective_run_only_selectors() {
	previous := os.getenv_opt('VTEST_ONLY_FN')
	defer {
		if value := previous {
			os.setenv('VTEST_ONLY_FN', value, true)
		} else {
			os.unsetenv('VTEST_ONLY_FN')
		}
	}
	os.setenv('VTEST_ONLY_FN', 'test_first,test_last*', true)
	effective := v3_environment_run_only()
	flags := v3_module_cache_refresh_args(['main_test.v'], 'cc', [], effective)
	index := flags.index('-run-only')
	assert index >= 0 && flags[index + 1] == 'test_first,test_last*', flags.str()
	// The effective selector overrides the raw option after environment/CLI parsing.
	assert v3_module_cache_refresh_args(['-run-only', 'ignored', 'main_test.v'], 'cc', [], effective) == flags
	assert '-run-only' !in v3_module_cache_refresh_args(['-run-only', 'ignored', 'main_test.v'], 'cc', [], [])
}

fn test_module_cache_refresh_pins_the_compiler_before_path_changes() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vup_compiler_path_${os.getpid()}')
	previous := os.getenv('PATH')
	defer {
		os.setenv('PATH', previous, true)
		os.rmdir_all(root) or {}
	}
	for name in ['first', 'second'] {
		dir := os.join_path(root, name)
		os.mkdir_all(dir)!
		path := os.join_path(dir, 'clang')
		os.write_file(path, '#!/bin/sh\nprintf "${name}\\n"\n')!
		os.chmod(path, 0o755)!
	}
	os.setenv('PATH', os.join_path(root, 'first'), true)
	flags := v3_module_cache_refresh_args(['-cc', 'clang', 'main.v'], 'clang', [], [])
	os.setenv('PATH', os.join_path(root, 'second'), true)
	assert flags == ['-cc', os.join_path(root, 'first', 'clang')]
	result := os.exec([flags[1], '--version'])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'first', result.output
}

fn test_v3_cache_rejects_a_different_compiler_than_cc() {
	// The bundled TCC is never the same program as a `cc` found on PATH. Windows
	// reports no inode, so an identity based on `os.stat` alone equated them.
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	default_cc := os.find_abs_path_of_executable('cc') or { return }
	if !os.is_file(bundled_tcc) || os.real_path(default_cc) == os.real_path(bundled_tcc) {
		return
	}
	assert !v3_c_compiler_matches_default_cc(bundled_tcc), '${bundled_tcc} must not be treated as ${default_cc}'
}

fn test_v3_same_c_compiler_executable_compares_files_not_drives() {
	root := os.join_path(os.temp_dir(), 'v3_same_cc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first_cc')
	second := os.join_path(root, 'second_cc')
	os.write_file(first, 'first')!
	os.write_file(second, 'second')!
	assert v3_same_c_compiler_executable(first, first)
	assert v3_same_c_compiler_executable(first, os.join_path(root, '.', 'first_cc'))
	assert !v3_same_c_compiler_executable(first, second)
	assert !v3_same_c_compiler_executable(first, os.join_path(root, 'missing_cc'))
	$if !windows {
		link := os.join_path(root, 'link_cc')
		os.symlink(first, link)!
		assert v3_same_c_compiler_executable(first, link)
		assert !v3_same_c_compiler_executable(second, link)
		// A hard link keeps its own path, so only the device/inode fallback can
		// recognise it as the same program.
		hard_link := os.join_path(root, 'hard_link_cc')
		os.link(first, hard_link)!
		assert os.real_path(hard_link) != os.real_path(first)
		assert v3_same_c_compiler_executable(first, hard_link)
		assert !v3_same_c_compiler_executable(second, hard_link)
	}
}

fn test_v3_windows_cross_compiler_replaces_host_tcc() {
	linux := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	windows_amd64 := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	windows_x86 := pref.Target{
		os:   'windows'
		arch: 'x86'
	}
	windows_arm64 := pref.Target{
		os:   'windows'
		arch: 'arm64'
	}
	assert v3_windows_cross_c_compiler('tcc', true, linux, windows_amd64) == 'x86_64-w64-mingw32-gcc'
	assert v3_windows_cross_c_compiler('tinyc', true, linux, windows_x86) == 'i686-w64-mingw32-gcc'
	assert v3_windows_cross_c_compiler('clang', true, linux, windows_amd64) == 'clang'
	assert v3_windows_cross_c_compiler('tcc', true, windows_amd64, windows_amd64) == 'tcc'
	// The platform default `cc` has no Windows headers; an explicit `-cc cc` is kept.
	assert v3_windows_cross_c_compiler('cc', false, linux, windows_amd64) == 'x86_64-w64-mingw32-gcc'
	assert v3_windows_cross_c_compiler('cc', true, linux, windows_amd64) == 'cc'
	assert v3_windows_cross_c_compiler('cc', false, linux, windows_arm64) == 'cc'
	assert v3_windows_cross_c_compiler('cc', false, windows_amd64, windows_amd64) == 'cc'
}

fn test_v3_windows_target_defaults_to_amd64_on_other_hosts() {
	macos_arm64 := pref.Target{
		os:   'macos'
		arch: 'arm64'
	}
	linux_arm64 := pref.Target{
		os:   'linux'
		arch: 'arm64'
	}
	windows_arm64 := pref.Target{
		os:   'windows'
		arch: 'arm64'
	}
	assert v3_target_arch_for_request(macos_arm64, 'windows', 'arm64', false, false) == 'amd64'
	assert v3_target_arch_for_request(linux_arm64, 'Windows', 'arm64', false, false) == 'amd64'
	assert v3_target_arch_for_request(macos_arm64, 'windows', 'arm64', true, false) == 'arm64'
	assert v3_target_arch_for_request(macos_arm64, 'windows', 'arm64', false, true) == 'arm64'
	assert v3_target_arch_for_request(windows_arm64, 'windows', 'arm64', false, false) == 'arm64'
	assert v3_target_arch_for_request(macos_arm64, 'macos', 'arm64', false, false) == 'arm64'
}

fn test_v3_msvc_alias_selects_cl_on_windows() {
	assert v3_c_compiler_command_alias('msvc', 'windows') == 'cl'
	assert v3_c_compiler_command_alias('MSVC', 'windows') == 'cl'
	assert v3_c_compiler_command_alias('msvc', 'linux') == 'msvc'
	assert v3_c_compiler_command_alias('clang', 'windows') == 'clang'
}

fn test_v3_no_std_omits_default_c_and_cpp_standards() {
	assert c_standard_flag(false, false) == '-std=gnu11'
	assert c_standard_flag(true, false) == '-std=c99'
	assert c_standard_flag(false, true) == ''
	assert cxx_standard_flag(false, false) == '-std=gnu++17'
	assert cxx_standard_flag(true, false) == '-std=c++17'
	assert cxx_standard_flag(false, true) == ''
}

fn test_v3_no_std_command_keeps_the_user_standard_only() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_no_std_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'fn main() {}\n')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-cc', 'c++',
		'-no-std', '-cflags', '-std=c++11', '-dump-c-flags', '-', '-o', output, source])
	assert build.exit_code == 0, build.output
	assert '-std=c++11' in build.output.split_into_lines(), build.output
	assert !build.output.contains('-std=gnu11'), build.output
	assert !build.output.contains('-std=gnu++17'), build.output
}

fn test_v3_tcc_linux_output_declares_backtrace() {
	root := os.join_path(os.vtmp_dir(), 'v3_tcc_backtrace_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'fn main() {}\n')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-cc', 'tcc',
		'-os', 'linux', '-arch', 'amd64', '-o', output, source])
	assert build.exit_code == 0, build.output
	c_source := os.read_file(output)!
	assert c_source.contains('tcc_backtrace(char* fmt);')
	assert c_source.contains('tcc_backtrace("Backtrace");')
}

fn test_macos_linux_cross_compile_uses_bundled_amd64_sysroot() {
	host := pref.Target{
		os:   'macos'
		arch: 'arm64'
	}
	target := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	assert v3_target_arch_for_request(host, 'linux', 'arm64', false, false) == 'amd64'
	assert v3_target_arch_for_request(host, 'linux', 'arm64', true, false) == 'arm64'
	assert v3_macos_linux_cross_compile(host, target, 'c', 'cc')
	assert v3_macos_linux_cross_compile(host, target, 'c', 'clang')
	assert v3_macos_linux_cross_compile(host, target, 'c', '/usr/bin/clang')
	assert !v3_macos_linux_cross_compile(host, target, 'c', 'x86_64-linux-gnu-gcc')
	assert !v3_macos_linux_cross_compile(host, target, 'fastc', 'clang')
	$if macos {
		assert c_compiler_target_args(target, 'clang', true, '/tmp/linuxroot')! == [
			'-target',
			'x86_64-linux-gnu',
			'-I',
			'/tmp/linuxroot/include',
		]
	}
}

fn test_macos_linux_cross_compile_selects_postgres_sysroot_headers() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_linux_cross_pg_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'import db.pg\nfn main() { println(pg.Config{}) }\n')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	// C output exercises conditional headers without requiring a cross toolchain or libpq.
	for target_os in ['macos', 'linux'] {
		build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-cc',
			'clang', '-os', target_os, '-o', output, source])
		assert build.exit_code == 0, build.output
		c_source := os.read_file(output)!
		assert c_source.contains('#include <libpq/libpq-fe.h>') == (target_os == 'linux')
		assert c_source.contains('#include <libpq/pg_config.h>') == (target_os == 'linux')
		assert c_source.contains('#include <libpq-fe.h>') == (target_os == 'macos')
		assert c_source.contains('#include <pg_config.h>') == (target_os == 'macos')
	}
}

fn test_linux_cross_link_flags_unwrap_clang_driver_flags() {
	assert v3_linux_cross_link_flags(['-I/include', '-pthread', '-Wl,-z,relro', '-Xlinker',
		'--as-needed', '-L/lib', '-lssl']) == [
		'-lpthread',
		'-z',
		'relro',
		'--as-needed',
		'-L/lib',
		'-lssl',
	]
}

fn test_linux_cross_compiles_native_sources_in_flag_plan() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_linux_cross_sources_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	build_dir := os.join_path(root, 'build space')
	os.mkdir_all(build_dir)!
	for source in ['before.c', 'objc.m', 'main.c', 'upper.C', 'side car.mm', 'side.cpp', 'signal.c',
		'forced.c', 'retained.o'] {
		os.write_file(os.join_path(build_dir, source), '')!
	}
	compiler := write_windows_prod_test_tool(os.join_path(root, 'fake-cc'), r'
set -eu
source=""
output=""
language=""
while [ "$#" -gt 0 ]; do
	case "$1" in
		-o) output="$2"; shift 2 ;;
		-x) language="$2"; shift 2 ;;
		-I|-D|-include|-target) shift 2 ;;
		-*) shift ;;
		*) [ -z "$source" ] || exit 91; source="$1"; shift ;;
	esac
done
[ -f "$source" ] && [ -n "$output" ] || exit 92
printf "%s|%s\n" "$source" "$language" >> compiled-sources.log
printf "%s|%s\n" "$source" "$language" > "$output"
')
	write_windows_prod_test_tool(os.join_path(root, 'sysroot', 'ld.lld'), r'
set -eu
output=""
objects=0
retained=0
while [ "$#" -gt 0 ]; do
	case "$1" in
		-o) output="$2"; shift 2 ;;
		-L|-l|-m|-dynamic-linker) shift 2 ;;
		linux_cross_*.o)
			[ -f "$1" ] || exit 93
			IFS= read -r identity < "$1"
			printf "%s\n" "$identity" >> linked-sources.log
			objects=$((objects + 1))
			shift ;;
		retained.o) [ -f "$1" ] || exit 94; retained=1; shift ;;
		*.c|*.C|*.cpp|*.m|*.mm|*.S) exit 95 ;;
		*) shift ;;
	esac
done
[ "$objects" -eq 7 ] && [ "$retained" -eq 1 ] && [ -n "$output" ] || exit 96
: > "$output"
')
	// Language selectors span argument sections; option operands are never inputs.
	plan := V3CCompilerFlagPlan{
		before_inputs: ['-I', 'includes.c', '-D', '-x', '-include', 'forced.c', '-x', 'c', 'before.c',
			'-xobjective-c']
		after_inputs:  ['-D', 'NOT_A_SOURCE.cpp', '-x', 'objective-c++', 'side car.mm', '-x', 'none',
			'side.cpp', '-x', 'c', 'signal.c', '-x', 'none', '-L', 'lib.c', '-l', 'dep', 'retained.o']
	}
	result := compile_v3_macos_linux_cross(compiler, &plan, ['objc.m', '-x', 'none', 'main.c',
		'upper.C'], os.join_path(root, 'sysroot'), root,
		build_dir, 'program', false, false)
	assert result.exit_code == 0, result.output
	expected := ['before.c|c', 'objc.m|objective-c', 'main.c|', 'upper.C|', 'side car.mm|objective-c++',
		'side.cpp|', 'signal.c|c']
	assert os.read_file(os.join_path(build_dir, 'compiled-sources.log'))!.split_into_lines() == expected
	assert os.read_file(os.join_path(build_dir, 'linked-sources.log'))!.split_into_lines() == expected
	assert os.is_file(os.join_path(build_dir, 'program'))
}

fn test_v3_implicit_tcc_uses_platform_compiler_for_non_c_objects() {
	implicit_tcc := os.join_path(os.temp_dir(), 'bin', 'tcc')
	assert c_source_object_compiler('', implicit_tcc, true, 'linux') == implicit_tcc
	assert c_source_object_compiler('objective-c', implicit_tcc, true, 'linux') == 'cc'
	assert c_source_object_compiler('objective-c', implicit_tcc, true, 'windows') == 'gcc'
	assert c_source_object_compiler('c++', implicit_tcc, true, 'linux') == 'c++'
	assert c_source_object_compiler('objective-c++', implicit_tcc, true, 'linux') == 'c++'
	assert c_source_object_compiler('objective-c', 'cc', false, 'linux') == 'cc'
	assert c_source_object_compiler('c++', 'cc', false, 'linux') == 'c++'
	assert c_source_object_compiler('objective-c', 'clang', false, 'linux') == 'clang'
	assert c_source_object_compiler('c++', 'clang', false, 'linux') == 'clang'
	assert c_source_object_compiler('objective-c', 'tcc', false, 'linux') == 'tcc'
	assert c_source_object_compiler('c++', 'tcc', false, 'linux') == 'c++'
	assert c_source_object_compiler('c++', '/opt/toolchains/tcc', false, 'linux') == 'c++'
	assert c_source_object_compiler('c++', 'tinyc', false, 'linux') == 'c++'
}

fn test_c_object_flag_plan_limits_primary_compiler_flags() {
	plan := CObjectFlagPlan{
		environment_flags:      ['-DENVIRONMENT']
		primary_compiler:       '/v/thirdparty/tcc/tcc.exe'
		primary_compiler_flags: ['-B/bundled/tcc', '-I/bundled/tcc/include']
		common_flags:           ['-fPIC', '-I/module/include']
	}
	assert plan.flags_for_compiler('/v/thirdparty/tcc/tcc.exe') == ['-DENVIRONMENT', '-B/bundled/tcc',
		'-I/bundled/tcc/include', '-fPIC', '-I/module/include']
	assert plan.flags_for_compiler('c++') == [
		'-DENVIRONMENT',
		'-fPIC',
		'-I/module/include',
	]
}

fn test_v3_bundled_tcc_probe_eligibility() {
	linux_target := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	bundled_tcc := os.join_path(os.vtmp_dir(), 'v3_probe_eligibility', 'thirdparty', 'tcc', 'tcc.exe')
	base := V3BundledTccProbeOptions{
		backend:     'c'
		c_compiler:  'cc'
		host_os:     'linux'
		host_target: linux_target
		target:      linux_target
		bundled_tcc: bundled_tcc
	}
	assert v3_should_probe_bundled_tcc(base)
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		backend: 'wasm'
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_only: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_c_debug: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		dump_c_flags: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		parallel_cc: true
	})
	windows_target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		parallel_cc: true
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          'tcc'
		c_compiler_explicit: true
		dump_c_flags:        true
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          'tcc'
		c_compiler_explicit: true
		parallel_cc:         true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          'clang'
		c_compiler_explicit: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		target: pref.Target{
			os:   'linux'
			arch: 'arm64'
		}
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:             true
		c_compiler:          'tcc'
		c_compiler_explicit: true
	})
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          bundled_tcc
		c_compiler_explicit: true
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		c_compiler:          os.join_path(os.vtmp_dir(), 'bin', 'tcc')
		c_compiler_explicit: true
	})
	// Windows keeps its bundled TCC as the default for debug builds.
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_c_debug:  true
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	// -prod needs the optimizations TCC cannot do, so it never defaults to TCC, on
	// Windows either. Selecting it would only generate for TCC and then regenerate.
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:     true
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:     true
		is_c_debug:  true
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:     true
		parallel_cc: true
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:     true
		is_shared:   true
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
	})
	assert !v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:       true
		is_shared:     true
		is_liveshared: true
		host_os:       'windows'
		host_target:   windows_target
		target:        windows_target
	})
	// The probe still finds the bundled TinyCC for an explicit `-cc tcc` with -prod on Windows;
	// the driver then refuses that build (see v3_tcc_prod_rejected).
	assert v3_should_probe_bundled_tcc(V3BundledTccProbeOptions{
		...base
		is_prod:             true
		c_compiler:          'tcc'
		c_compiler_explicit: true
		host_os:             'windows'
		host_target:         windows_target
		target:              windows_target
	})
}

fn test_effective_c_compiler_name_does_not_read_tinycc_from_names_that_only_contain_tcc() {
	root := os.join_path(os.vtmp_dir(), 'v3_notcc_driver_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	// The V executable is a real compiler driver that is not TinyCC.
	not_tinycc := os.join_path(root, 'distcc.exe')
	os.cp(@VEXE, not_tinycc) or { panic(err) }
	assert effective_c_compiler_name(not_tinycc, pref.host_target()) != 'tinyc'
}

fn test_v3_tcc_prod_rejected_only_for_a_prod_c_build_that_resolves_to_tinycc() {
	assert v3_tcc_prod_rejected(true, 'c', 'tinyc', 'tcc')
	assert !v3_tcc_prod_rejected(true, 'c', 'gcc', 'gcc')
	assert !v3_tcc_prod_rejected(true, 'c', 'clang', 'clang')
	assert !v3_tcc_prod_rejected(true, 'arm64', 'tinyc', 'tcc')
	assert !v3_tcc_prod_rejected(false, 'c', 'tinyc', 'tcc')
}

fn test_v3_tcc_prod_rejected_checks_actual_banner_for_known_compiler_names() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_prod_compiler_banner_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for name in ['clang-alias', 'gcc-alias'] {
		alias := os.join_path(root, name)
		os.write_file(alias, '#!/bin/sh\nprintf "tcc version 0.9.27\\n"\n')!
		os.chmod(alias, 0o700)!
		effective := effective_c_compiler_name(alias, pref.host_target())
		assert effective != 'tinyc'
		assert v3_tcc_prod_rejected(true, 'c', effective, alias)
		assert !v3_tcc_prod_rejected(false, 'c', effective, alias)
		assert !v3_tcc_prod_rejected(true, 'arm64', effective, alias)
	}
	clang := os.join_path(root, 'cc')
	os.write_file(clang, '#!/bin/sh\nprintf "clang version 19.1.7\\nInstalledDir: /opt/tinycc-tcc version-compatible-sdk/bin\\n"\n')!
	os.chmod(clang, 0o700)!
	assert effective_c_compiler_name(clang, pref.host_target()) == 'clang'
	assert !v3_tcc_prod_rejected(true, 'c', 'clang', clang)
}

fn test_v3_windows_prod_msvc_ready_needs_a_matching_developer_environment() {
	ready := V3WindowsProdToolchain{
		cl:      'C:/VS/bin/cl.exe'
		include: 'C:/VS/include'
		lib:     'C:/VS/lib'
	}
	assert v3_windows_prod_msvc_ready(ready)
	// A Developer Command Prompt that builds for x64 says so; older scripts say nothing.
	assert v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
		...ready
		target_arch: 'x64'
	})
	assert v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
		...ready
		target_arch: 'X64'
	})
	// An x86 or ARM prompt puts a `cl` on PATH that cannot build V's amd64 C: it lacks
	// the intrinsics V's MSVC code uses (`_umul128`).
	for arch in ['x86', 'arm', 'arm64'] {
		assert !v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
			...ready
			target_arch: arch
		}), arch
	}
	// `cl` on PATH alone cannot find its headers or libraries.
	assert !v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
		...ready
		cl: ''
	})
	assert !v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
		...ready
		include: ''
	})
	assert !v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
		...ready
		lib: ''
	})
	// `cl` cannot produce the object file of `-o x.o`; V builds that with gcc or clang.
	assert !v3_windows_prod_msvc_ready(V3WindowsProdToolchain{
		...ready
		is_o: true
	})
}

fn test_v3_windows_prod_clang_ready_needs_an_amd64_mingw_target() {
	clang := 'C:/llvm-mingw/bin/clang.exe'
	for triple in ['x86_64-w64-windows-gnu', 'x86_64-w64-mingw32', 'x86_64-pc-windows-gnu'] {
		assert v3_windows_prod_clang_ready(V3WindowsProdToolchain{
			clang:        clang
			clang_triple: triple
		}), triple
	}
	// The MSVC ABI cannot link V's MinGW flags, a 32-bit or ARM MinGW clang builds the
	// wrong architecture, and a failed or noisy probe leaves no triple at all.
	for triple in ['x86_64-pc-windows-msvc', 'i686-w64-windows-gnu', 'aarch64-w64-mingw32',
		'x86_64-pc-linux-gnu', ''] {
		assert !v3_windows_prod_clang_ready(V3WindowsProdToolchain{
			clang:        clang
			clang_triple: triple
		}), triple
	}
	assert !v3_windows_prod_clang_ready(V3WindowsProdToolchain{
		clang_triple: 'x86_64-w64-windows-gnu'
	})
}

fn test_v3_windows_prod_c_compiler_order() {
	clang := 'C:/llvm-mingw/bin/clang.exe'
	gcc := 'C:/mingw64/bin/gcc.exe'
	all := V3WindowsProdToolchain{
		cl:           'C:/VS/bin/cl.exe'
		include:      'C:/VS/include'
		lib:          'C:/VS/lib'
		clang:        clang
		clang_triple: 'x86_64-w64-windows-gnu'
		gcc:          gcc
	}
	assert v3_windows_prod_c_compiler(all) == 'cl'
	// Without a usable MSVC it is Clang, whatever the reason MSVC cannot be used.
	assert v3_windows_prod_c_compiler(V3WindowsProdToolchain{
		...all
		cl: ''
	}) == clang
	assert v3_windows_prod_c_compiler(V3WindowsProdToolchain{
		...all
		target_arch: 'x86'
	}) == clang
	assert v3_windows_prod_c_compiler(V3WindowsProdToolchain{
		...all
		is_o: true
	}) == clang
	// Without a usable Clang it is GCC, the last resort.
	assert v3_windows_prod_c_compiler(V3WindowsProdToolchain{
		...all
		cl:    ''
		clang: ''
	}) == gcc
	assert v3_windows_prod_c_compiler(V3WindowsProdToolchain{
		...all
		cl:           ''
		clang_triple: 'x86_64-pc-windows-msvc'
	}) == gcc
	assert v3_windows_prod_c_compiler(V3WindowsProdToolchain{
		...all
		target_arch:  'x86'
		clang_triple: ''
	}) == gcc
}

// write_windows_prod_test_tool writes an executable shell script named `path`.
fn write_windows_prod_test_tool(path string, body string) string {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, '#!/bin/sh\n${body}\n') or { panic(err) }
	os.chmod(path, 0o700) or { panic(err) }
	return path
}

// with_windows_prod_test_environment runs callback with `values` set in the process
// environment (an empty value unsets the variable) and puts the old values back after.
fn with_windows_prod_test_environment(values map[string]string, callback fn ()) {
	mut saved := map[string]string{}
	for name, _ in values {
		if old := os.getenv_opt(name) {
			saved[name] = old
		}
	}
	defer {
		for name, _ in values {
			if old := saved[name] {
				os.setenv(name, old, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	for name, value in values {
		if value == '' {
			os.unsetenv(name)
		} else {
			os.setenv(name, value, true)
		}
	}
	callback()
}

fn test_v3_windows_prod_toolchain_reads_the_environment_and_probes_clang_only_when_needed() {
	$if windows {
		// The tools are shell scripts. A Windows machine exercises the real ones in
		// test_v3_windows_prod_build_uses_the_platform_compiler_in_one_build.
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_prod_toolchain_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	bin := os.join_path(root, 'bin')
	marker := os.join_path(root, 'clang_ran')
	cl := write_windows_prod_test_tool(os.join_path(bin, 'cl'), 'exit 0')
	gcc := write_windows_prod_test_tool(os.join_path(bin, 'gcc'), 'exit 0')
	clang_path := os.join_path(bin, 'clang')
	write_clang := fn [clang_path, marker] (body string) {
		write_windows_prod_test_tool(clang_path, 'echo ran > ${os.quoted_path(marker)}\n${body}')
	}
	write_clang('echo x86_64-w64-windows-gnu')
	// PATH holds only the fake tools, so no real compiler of this machine is involved.
	developer_environment := {
		'PATH':               os.dir(cl)
		'INCLUDE':            '/vs/include'
		'LIB':                '/vs/lib'
		'VSCMD_ARG_TGT_ARCH': ''
	}
	with_windows_prod_test_environment(developer_environment, fn [marker, cl] () {
		// A ready MSVC is chosen without running clang.
		tc := v3_windows_prod_toolchain(false)
		assert os.real_path(tc.cl) == os.real_path(cl)
		assert v3_windows_prod_c_compiler(tc) == 'cl'
		assert !os.exists(marker)
	})
	// An x86 Developer Command Prompt: no MSVC, so clang is probed and chosen.
	x86_environment := {
		'PATH':               os.dir(cl)
		'INCLUDE':            '/vs/include'
		'LIB':                '/vs/lib'
		'VSCMD_ARG_TGT_ARCH': 'x86'
	}
	with_windows_prod_test_environment(x86_environment, fn [marker, clang_path] () {
		tc := v3_windows_prod_toolchain(false)
		assert os.exists(marker)
		assert os.real_path(tc.clang) == os.real_path(clang_path)
		assert tc.clang_triple == 'x86_64-w64-windows-gnu'
		assert os.real_path(v3_windows_prod_c_compiler(tc)) == os.real_path(clang_path)
	})
	os.rm(marker) or {}
	with_windows_prod_test_environment(developer_environment, fn [clang_path] () {
		// Object output is built by clang, never by cl.
		chosen := v3_windows_prod_c_compiler(v3_windows_prod_toolchain(true))
		assert os.real_path(chosen) == os.real_path(clang_path)
	})
	no_msvc_environment := {
		'PATH':               os.dir(cl)
		'INCLUDE':            ''
		'LIB':                ''
		'VSCMD_ARG_TGT_ARCH': ''
	}
	// A clang that fails, prints noise around its triple, or targets the MSVC ABI is
	// not used: the build falls through to gcc.
	// The last body is an MSVC-ABI clang that mentions MinGW in a diagnostic: matching the
	// merged output as a substring would take it for a MinGW one.
	for body in ['echo x86_64-w64-windows-gnu\nexit 1', 'echo warning: odd\necho x86_64-w64-windows-gnu',
		'echo x86_64-pc-windows-msvc', 'echo x86_64-pc-windows-msvc\necho note: not a mingw build'] {
		write_clang(body)
		with_windows_prod_test_environment(no_msvc_environment, fn [gcc, body] () {
			chosen := v3_windows_prod_c_compiler(v3_windows_prod_toolchain(false))
			assert os.real_path(chosen) == os.real_path(gcc), body
		})
	}
	// A clang that never answers must not hang the build: the probe gives up and gcc is used.
	// PATH holds only the fake tools, so the stub has to name sleep by its full path.
	write_clang('exec /bin/sleep 60')
	started := time.ticks()
	with_windows_prod_test_environment(no_msvc_environment, fn [gcc] () {
		chosen := v3_windows_prod_c_compiler(v3_windows_prod_toolchain(false))
		assert os.real_path(chosen) == os.real_path(gcc)
	})
	assert time.ticks() - started < 30000
}

fn test_v3_windows_prod_needs_default_c_compiler_only_for_native_default_builds() {
	windows_target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	linux_target := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	base := V3BundledTccProbeOptions{
		backend:     'c'
		is_prod:     true
		c_compiler:  'cc'
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
		bundled_tcc: os.join_path(os.vtmp_dir(), 'v3_windows_prod_default', 'thirdparty', 'tcc', 'tcc.exe')
	}
	assert v3_windows_prod_needs_default_c_compiler(base)
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		is_prod: false
	})
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		c_compiler:          'gcc'
		c_compiler_explicit: true
	})
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		backend: 'wasm'
	})
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		c_only: true
	})
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		dump_c_flags: true
	})
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		host_os:     'linux'
		host_target: linux_target
		target:      linux_target
	})
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		target: linux_target
	})
	// A cross-architecture build is left alone.
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		target: pref.Target{
			os:   'windows'
			arch: 'arm64'
		}
	})
	// A native build of another architecture needs one too: it no longer gets the
	// platform GCC by regenerating after the skipped implicit TCC.
	for arch in ['arm64', 'x86'] {
		native := pref.Target{
			os:   'windows'
			arch: arch
		}
		assert v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
			...base
			host_target: native
			target:      native
		}), arch
	}
	// Nor is an amd64 target built from another architecture's host.
	assert !v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		host_target: pref.Target{
			os:   'windows'
			arch: 'arm64'
		}
	})
	// Every other mode of a native amd64 build gets a default too, so none of them can
	// fall back to the bare `cc`.
	assert v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		is_c_debug: true
	})
	assert v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		is_shared: true
	})
	assert v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		is_shared:     true
		is_liveshared: true
	})
	assert v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		parallel_cc: true
	})
	assert v3_windows_prod_needs_default_c_compiler(V3BundledTccProbeOptions{
		...base
		is_o: true
	})
}

fn test_v3_select_c_compiler_windows_prod_never_uses_tcc_or_the_bare_cc() {
	windows_target := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_prod_select_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	// A usable bundled TCC and a usable system TCC are both on offer: what keeps -prod
	// off them is the selection, not their absence.
	mut vroot := root
	mut path := os.getenv('PATH')
	$if windows {
		// This checkout's own bundled TCC.
		vroot = @VEXEROOT
	} $else {
		os.mkdir_all(os.join_path(root, 'thirdparty', 'tcc', 'lib'))!
		write_v3_test_tcc(os.join_path(root, 'thirdparty', 'tcc', 'tcc.exe'), 0)
		os.write_file(os.join_path(root, 'thirdparty', 'tcc', 'lib', 'openlibm.o'), '')!
		system_tcc := write_v3_test_tcc(os.join_path(root, 'bin', 'tcc'), 0)
		path = os.dir(system_tcc) + os.path_delimiter + path
	}
	bundled_tcc := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	if !v3_usable_tcc_compiler(bundled_tcc) {
		eprintln('> skipping: no usable bundled tcc at ${bundled_tcc}')
		return
	}
	base := V3BundledTccProbeOptions{
		backend:     'c'
		c_compiler:  'cc'
		host_os:     'windows'
		host_target: windows_target
		target:      windows_target
		bundled_tcc: bundled_tcc
	}
	with_windows_prod_test_environment({
		'PATH': path
	}, fn [vroot, base, bundled_tcc] () {
		// Positive control: without -prod the same options do select a TCC.
		control := v3_select_c_compiler(vroot, base)
		assert control.implicit_tcc != ''
		assert control.use_implicit_tcc_semantics
		assert control.effective_c_compiler == 'tinyc'
		selection := v3_select_c_compiler(vroot, V3BundledTccProbeOptions{
			...base
			is_prod: true
		})
		assert selection.implicit_tcc == ''
		assert !selection.use_implicit_tcc_semantics
		assert selection.c_compiler != 'cc'
		assert selection.c_compiler != bundled_tcc
		assert selection.effective_c_compiler != 'tinyc'
	})
	// An explicit compiler is kept as given.
	explicit := v3_select_c_compiler(vroot, V3BundledTccProbeOptions{
		...base
		c_compiler:          'gcc'
		c_compiler_explicit: true
	})
	assert explicit.c_compiler == 'gcc'
}

fn test_v3_select_c_compiler_native_windows_prod_outside_amd64_uses_the_platform_gcc() {
	$if windows {
		// The tools are shell scripts.
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_prod_select_gcc_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	// A usable bundled and system TCC, a gcc, and an amd64 MinGW clang, but no `cc`.
	os.mkdir_all(os.join_path(root, 'thirdparty', 'tcc', 'lib'))!
	bundled_tcc := write_v3_test_tcc(os.join_path(root, 'thirdparty', 'tcc', 'tcc.exe'), 0)
	os.write_file(os.join_path(root, 'thirdparty', 'tcc', 'lib', 'openlibm.o'), '')!
	bin := os.join_path(root, 'bin')
	write_v3_test_tcc(os.join_path(bin, 'tcc'), 0)
	gcc := write_windows_prod_test_tool(os.join_path(bin, 'gcc'), 'exit 0')
	marker := os.join_path(root, 'clang_ran')
	write_windows_prod_test_tool(os.join_path(bin, 'clang'),
		'echo ran > ${os.quoted_path(marker)}\necho x86_64-w64-windows-gnu')
	// PATH holds only the fake tools, so no real compiler of this machine is involved.
	with_windows_prod_test_environment({
		'PATH': bin
	}, fn [root, bundled_tcc, gcc, marker] () {
		assert (os.find_abs_path_of_executable('cc') or { '' }) == ''
		for arch in ['x86', 'arm64'] {
			native := pref.Target{
				os:   'windows'
				arch: arch
			}
			base := V3BundledTccProbeOptions{
				backend:     'c'
				c_compiler:  'cc'
				host_os:     'windows'
				host_target: native
				target:      native
				bundled_tcc: bundled_tcc
			}
			// Positive control: without -prod the same options do select the bundled TCC.
			control := v3_select_c_compiler(root, base)
			assert control.implicit_tcc == bundled_tcc, arch
			assert control.effective_c_compiler == 'tinyc', arch
			// With -prod it is the platform GCC up front, not the bare `cc`, and not the
			// amd64 Clang.
			selection := v3_select_c_compiler(root, V3BundledTccProbeOptions{
				...base
				is_prod: true
			})
			assert selection.implicit_tcc == '', arch
			assert !selection.use_implicit_tcc_semantics, arch
			assert os.real_path(selection.c_compiler) == os.real_path(gcc), arch
			assert selection.effective_c_compiler == 'gcc', arch
		}
		// The amd64 toolchain probe never ran.
		assert !os.exists(marker)
	})
}

fn test_v3_bundled_tcc_probe_does_not_run_when_ineligible() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v3_bundled_tcc_probe_${os.getpid()}')
	probe_marker := os.join_path(test_root, 'tcc_was_probed')
	bundled_tcc := os.join_path(test_root, 'thirdparty', 'tcc', 'tcc.exe')
	os.mkdir_all(os.dir(bundled_tcc)) or { panic(err) }
	os.write_file(bundled_tcc, '#!/bin/sh\nprintf probed > ${os.quoted_path(probe_marker)}\nexit 0\n') or { panic(err) }
	os.chmod(bundled_tcc, 0o700) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	linux_target := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	base := V3BundledTccProbeOptions{
		backend:     'c'
		c_compiler:  'cc'
		host_os:     'linux'
		host_target: linux_target
		target:      linux_target
		bundled_tcc: bundled_tcc
	}
	assert !v3_bundled_tcc_available(V3BundledTccProbeOptions{
		...base
		is_prod: true
	})
	assert !os.is_file(probe_marker)
	assert v3_bundled_tcc_available(base)
	assert os.is_file(probe_marker)
}

fn test_v3_default_tcc_compiler_uses_working_system_tcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v3_default_system_tcc_${os.getpid()}')
	system_tcc := write_v3_test_tcc(os.join_path(test_root, 'bin', 'tcc'), 0)
	old_path := os.getenv_opt('PATH')
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if value := old_path {
			os.setenv('PATH', value, true)
		} else {
			os.unsetenv('PATH')
		}
		os.rmdir_all(test_root) or {}
	}
	bundled_tcc := write_v3_test_tcc(os.join_path(test_root, 'thirdparty', 'tcc', 'tcc.exe'), 1)
	assert !v3_usable_tcc_compiler(bundled_tcc)
	implicit_tcc := v3_default_tcc_compiler(bundled_tcc, v3_usable_tcc_compiler(bundled_tcc), true, false, 'linux')
	assert implicit_tcc == system_tcc
	assert v3_effective_c_compiler_for_codegen('c', 'cc', implicit_tcc != '', pref.host_target()) == 'tinyc'
	assert v3_effective_c_compiler_for_codegen('x64', 'cc', false, pref.host_target()) == 'tinyc'
	write_v3_test_tcc(bundled_tcc, 0)
	assert v3_usable_tcc_compiler(bundled_tcc)
	assert v3_default_tcc_compiler(bundled_tcc, v3_usable_tcc_compiler(bundled_tcc), true, false, 'linux') == bundled_tcc
	assert v3_default_tcc_compiler(bundled_tcc, true, true, true, 'linux') == ''
	assert v3_default_tcc_compiler(bundled_tcc, false, false, false, 'linux') == ''
	assert v3_default_tcc_compiler(bundled_tcc, false, true, false, 'macos') == ''
}

fn test_v3_default_tcc_compiler_skips_broken_system_tcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v3_broken_system_tcc_${os.getpid()}')
	system_tcc := write_v3_test_tcc(os.join_path(test_root, 'bin', 'tcc'), 1)
	old_path := os.getenv_opt('PATH')
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if value := old_path {
			os.setenv('PATH', value, true)
		} else {
			os.unsetenv('PATH')
		}
		os.rmdir_all(test_root) or {}
	}
	bundled_tcc := os.join_path(test_root, 'missing', 'tcc.exe')
	assert v3_default_tcc_compiler(bundled_tcc, false, true, false, 'linux') == ''
}

fn test_v3_system_tcc_does_not_use_bundled_resources() {
	vroot := os.join_path(os.temp_dir(), 'v3_system_tcc_resources')
	bundled_tcc := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	system_tcc := os.join_path(os.path_separator, 'usr', 'bin', 'tcc')
	resources := v3_tcc_resource_flags_for_compiler(vroot, system_tcc, bundled_tcc, false)
	assert resources == V3TccResourceFlags{}
	assert v3_tcc_object_compile_flags(vroot, system_tcc, bundled_tcc, false, 'linux', '') == []
	bundled_resources := v3_tcc_resource_flags_for_compiler(vroot, bundled_tcc, bundled_tcc, true)
	assert bundled_resources.base_arg.contains('thirdparty')
	object_flags := v3_tcc_object_compile_flags(vroot, bundled_tcc, bundled_tcc, true, 'linux', '')
	assert bundled_resources.base_arg in object_flags
	assert bundled_resources.include_arg in object_flags
	assert bundled_resources.library_arg !in object_flags
}

fn test_v3_bundled_tcc_native_object_build_is_cwd_independent() {
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_executable(bundled_tcc) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_tcc_native_object_cwd_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	c_source := os.join_path(root, 'native.c')
	c_object := os.join_path(root, 'native.o')
	v_source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	os.write_file(c_source, '#include <stddef.h>\nsize_t v3_tcc_native_object_probe(void) { return sizeof(size_t); }\n')!
	os.write_file(v_source, '#flag ${c_object}\n\nfn C.v3_tcc_native_object_probe() uint\n\nfn main() {\n\tassert C.v3_tcc_native_object_probe() > 0\n}\n')!
	build := cmdexec.run_in(v3_driver_test_executable(), ['-new-compiler', '-nocache',
		'-no-retry-compilation', '-cc', 'tcc', '-o', output, v_source], root)
	assert build.exit_code == 0, build.output
}

fn test_v3_implicit_tcc_cpp_native_object_uses_platform_headers() {
	$if windows {
		return
	}
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_executable(bundled_tcc) {
		return
	}
	os.find_abs_path_of_executable('c++') or { return }
	root := os.join_path(os.vtmp_dir(), 'v3_tcc_cpp_object_headers_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	cpp_source := os.join_path(root, 'native.cpp')
	cpp_object := os.join_path(root, 'native.o')
	v_source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main')
	os.write_file(cpp_source, '#include <cstddef>\nextern "C" size_t v3_cpp_object_probe(void) { return sizeof(std::max_align_t); }\n')!
	os.write_file(v_source, '#flag ${cpp_object}\n\nfn C.v3_cpp_object_probe() uint\n\nfn main() {\n\tassert C.v3_cpp_object_probe() > 0\n}\n')!
	old_vflags := os.getenv_opt('VFLAGS')
	os.unsetenv('VFLAGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
	}
	build := cmdexec.run_in(v3_driver_test_executable(), ['-new-compiler', '-nocache',
		'-no-retry-compilation', '-o', output, v_source], root)
	assert !build.output.contains('failed to build C object'), build.output
	assert build.exit_code == 0
		|| build.output.contains('implicit tcc could not be used for this build'), build.output
}

fn test_v3_system_tcc_runtime_requires_windows_openlibm() {
	test_root := os.join_path(os.vtmp_dir(), 'v3_system_tcc_runtime_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert v3_system_tcc_runtime_available(test_root, 'linux')
	assert !v3_system_tcc_runtime_available(test_root, 'windows')
	openlibm := os.join_path(test_root, 'thirdparty', 'tcc', 'lib', 'openlibm.o')
	os.mkdir_all(os.dir(openlibm)) or { panic(err) }
	os.write_file(openlibm, '') or { panic(err) }
	assert v3_system_tcc_runtime_available(test_root, 'windows')
}

fn test_v3_regenerates_cc_fallback_after_implicit_tcc() {
	assert !v3_should_regenerate_after_implicit_tcc(true, false, false, 0)
	assert !v3_should_regenerate_after_implicit_tcc(true, false, true, 1)
	assert !v3_should_regenerate_after_implicit_tcc(true, true, true, 0)
	assert v3_should_regenerate_after_implicit_tcc(true, true, true, 1)
	assert v3_should_regenerate_after_implicit_tcc(true, true, false, 0)
	assert !v3_should_regenerate_after_implicit_tcc(false, true, true, 1)
	assert !v3_should_regenerate_after_implicit_tcc(false, true, false, 0)
}

fn test_v3_retry_compilation_preserves_internal_quiet() {
	args := [macos_v3_internal_quiet_flag, macos_v3_compat_c99_flag, '-autofree', 'run', 'main.v']
	retry := v3_retry_compilation_args(args, -1, 'cc')
	assert macos_v3_internal_quiet_flag in retry
	assert macos_v3_compat_c99_flag !in retry
	assert retry[0] == '-no-retry-compilation'
}

fn write_v3_test_tcc(tcc_path string, exit_code int) string {
	os.mkdir_all(os.dir(tcc_path)) or { panic(err) }
	os.write_file(tcc_path, '#!/bin/sh\nexit ${exit_code}\n') or { panic(err) }
	os.chmod(tcc_path, 0o700) or { panic(err) }
	return tcc_path
}

fn test_v3_tcc_flag_plan_skips_backtrace_on_macos_arm64() {
	vroot := os.join_path(os.temp_dir(), 'v3_tcc_flag_plan')
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc:      true
		target_os:   'macos'
		target_arch: 'arm64'
		vroot:       vroot
	})
	assert '-bt25' !in plan.before_inputs
	tcc_install_dir := os.join_path(vroot, 'thirdparty', 'tcc', 'lib')
	assert '-B${tcc_install_dir}' in plan.before_inputs
	assert '-I${os.join_path_single(tcc_install_dir, 'include')}' in plan.before_inputs
	assert '-L${tcc_install_dir}' in plan.before_inputs
}

fn test_v3_freestanding_flag_plan_links_no_host_libraries() {
	for is_tcc in [false, true] {
		plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
			is_tcc:          is_tcc
			is_prod:         !is_tcc
			is_freestanding: true
			target_os:       'linux'
			target_arch:     'amd64'
			c_compiler:      if is_tcc { 'tinyc' } else { 'gcc' }
			vroot:           os.join_path(os.temp_dir(), 'v3_freestanding_flag_plan')
		})
		assert '-ffreestanding' in plan.before_inputs, 'tcc=${is_tcc}'
		assert '-bt25' !in plan.before_inputs, 'tcc=${is_tcc}'
		// `_start` calls `main` from a top-level asm block, which LTO does not see.
		assert '-flto' !in plan.before_inputs, 'tcc=${is_tcc}'
		assert '-static' in plan.after_inputs && '-nostdlib' in plan.after_inputs, 'tcc=${is_tcc}'
		assert '-lm' !in plan.after_inputs && '-lpthread' !in plan.after_inputs, 'tcc=${is_tcc}'
		assert ('-ltcc1' in plan.after_inputs) == is_tcc
	}
}

fn test_v3_linux_shared_flag_plan_hides_static_archive_symbols() {
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_shared:  true
		target_os:  'linux'
		c_compiler: 'cc'
	})
	assert '-fvisibility=hidden' in plan.before_inputs
	assert '-Wl,--exclude-libs,ALL' in plan.before_inputs
	tcc_plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc:     true
		is_shared:  true
		target_os:  'linux'
		c_compiler: 'tinyc'
		vroot:      os.join_path(os.temp_dir(), 'v3_linux_shared_tcc_flag_plan')
	})
	assert '-Wl,--exclude-libs,ALL' !in tcc_plan.before_inputs
}

fn test_tcc_monolithic_dependency_flags_put_native_sources_before_archives() {
	flags := ['-DGC_THREADS=1', '-I', '/tmp/include', '/tmp/libgc.a', '-ldl', '/tmp/native.c',
		'-lm']
	assert tcc_monolithic_dependency_flags(flags, false) == [
		'-DGC_THREADS=1',
		'-I',
		'/tmp/include',
		'/tmp/native.c',
		'/tmp/libgc.a',
		'-ldl',
		'-lm',
	]
	assert tcc_monolithic_dependency_flags(flags, true) == [
		'-DGC_THREADS=1',
		'-I',
		'/tmp/include',
	]
}

fn test_v3_object_flag_plan_omits_link_inputs() {
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_o:         true
		target_os:    'linux'
		dependencies: ['-DGC_THREADS=1', '-I', '/tmp/include', '/tmp/libgc.a', '-ldl', '/tmp/native.c',
			'-lm']
	})
	assert plan.after_inputs == ['-DGC_THREADS=1', '-I', '/tmp/include']
}

fn test_v3_tcc_resource_flags_use_windows_bundle_root() {
	vroot := os.join_path(os.temp_dir(), 'v3_windows_tcc_flag_plan_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	tcc_root := os.join_path(vroot, 'thirdparty', 'tcc')
	tcc_lib := os.join_path_single(tcc_root, 'lib')
	tcc_include := os.join_path_single(tcc_root, 'include')
	os.mkdir_all(tcc_lib)!
	os.mkdir_all(os.join_path_single(tcc_include, 'winapi'))!
	defer {
		os.rmdir_all(vroot) or {}
	}
	resources := v3_tcc_resource_flags(vroot)
	assert resources.base_arg == '-B${tcc_root}'
	assert resources.include_arg == '-I${tcc_include}'
	assert resources.library_arg == '-L${tcc_lib}'
}

fn test_v3_windows_tcc_prod_flag_plan_uses_tcc_resources() {
	vroot := os.join_path(os.vtmp_dir(), 'v3_windows_tcc_prod_flag_plan_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	tcc_root := os.join_path(vroot, 'thirdparty', 'tcc')
	tcc_lib := os.join_path_single(tcc_root, 'lib')
	tcc_include := os.join_path_single(tcc_root, 'include')
	os.mkdir_all(tcc_lib)!
	os.mkdir_all(os.join_path_single(tcc_include, 'winapi'))!
	defer {
		os.rmdir_all(vroot) or {}
	}
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc:      true
		is_prod:     true
		target_os:   'windows'
		target_arch: 'amd64'
		c_compiler:  'tinyc'
		vroot:       vroot
	})
	assert '-O3' in plan.before_inputs
	assert '-flto' !in plan.before_inputs
	assert '-B${tcc_root}' in plan.before_inputs
	assert '-I${tcc_include}' in plan.before_inputs
	assert '-L${tcc_lib}' in plan.before_inputs
}

fn test_v3_tcc_flag_plan_restores_native_local_prefix() {
	host_os := os.user_os()
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		is_tcc:      true
		target_os:   host_os
		target_arch: 'amd64'
		vroot:       os.join_path(os.temp_dir(), 'v3_tcc_native_flag_plan')
	})
	if host_os == 'windows' {
		assert '-I/usr/local/include' !in plan.before_inputs
		assert '-L/usr/local/lib' !in plan.before_inputs
	} else {
		assert '-I/usr/local/include' in plan.before_inputs
		assert '-L/usr/local/lib' in plan.before_inputs
	}
}

fn test_v3_windows_prod_build_uses_the_platform_compiler_in_one_build() {
	$if !windows {
		return
	}
	chosen := v3_windows_prod_c_compiler(v3_windows_prod_toolchain(false))
	if (os.find_abs_path_of_executable(chosen) or { '' }) == '' {
		eprintln('> skipping: no platform C compiler (MSVC, clang or gcc) is available')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_prod_platform_cc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.exe')
	os.write_file(source, 'fn main() {\n\texit(42)\n}\n')!
	// A failing V3 build must fail this test. The launcher would otherwise rebuild
	// with the legacy compiler, report success, and file a bug report.
	mut saved := map[string]string{}
	for name, value in {
		'VFLAGS':                        ''
		'V_MACOS_V3_NO_FALLBACK':        '1'
		'V_C_ERROR_BUG_REPORT_DISABLED': '1'
		'V3_TEST_ISOLATE_CACHE':         '1'
	} {
		if old := os.getenv_opt(name) {
			saved[name] = old
		}
		if value == '' {
			os.unsetenv(name)
		} else {
			os.setenv(name, value, true)
		}
	}
	defer {
		for name in ['VFLAGS', 'V_MACOS_V3_NO_FALLBACK', 'V_C_ERROR_BUG_REPORT_DISABLED',
			'V3_TEST_ISOLATE_CACHE'] {
			if old := saved[name] {
				os.setenv(name, old, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	// With and without -nocache. The module cache is only used when the compiler is the
	// executable of the bare `cc`, so the first build is monolithic unless it is.
	for cache_args in [[]string{}, ['-nocache']] {
		os.rm(output) or {}
		mut args := ['-new-compiler']
		args << cache_args
		args << ['-prod', '-showcc', '-o', output, source]
		build := cmdexec.run(v3_driver_test_executable(), args)
		assert build.exit_code == 0, build.output
		normalized_output := build.output.replace('\\', '/')
		// -prod needs optimizations TCC cannot do: MSVC, Clang or GCC builds it ...
		assert !normalized_output.contains('thirdparty/tcc/tcc.exe'), build.output
		assert normalized_output.contains('-O3') || normalized_output.contains('-O2')
			|| normalized_output.contains('/O2'), build.output
		// ... not the bare `cc`, whichever compiler that happens to be ...
		assert !normalized_output.contains('> cc '), build.output
		// ... in one V3 build: not by generating for TCC first and re-running the whole
		// compilation, and not through the legacy compiler after V3 failed.
		assert !normalized_output.contains('regenerating'), build.output
		assert !normalized_output.contains('retrying with'), build.output
		assert !normalized_output.contains('V 0.5.2'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 42, run_result.output
	}
}

fn test_v3_windows_auto_gui_build_uses_windows_subsystem() {
	$if !windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_windows_auto_gui_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.exe')
	os.write_file(source, '#flag windows -lgdi32\n\nfn main() {}\n')!
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-showcc',
		'-o', output, source])
	assert build.exit_code == 0, build.output
	assert build.output.contains('-mwindows'), build.output
}

fn test_add_v3_tcc_compat_defines() {
	mut macos_arm64 := []string{}
	add_v3_tcc_compat_defines(mut macos_arm64, 'macos', 'arm64', false, true)
	assert macos_arm64 == ['no_backtrace']

	mut shared_defines := ['custom']
	add_v3_tcc_compat_defines(mut shared_defines, 'linux', 'amd64', true, true)
	assert shared_defines == ['custom', 'no_backtrace']

	mut supported := []string{}
	add_v3_tcc_compat_defines(mut supported, 'linux', 'arm64', false, true)
	assert supported.len == 0

	mut other_compiler := []string{}
	add_v3_tcc_compat_defines(mut other_compiler, 'macos', 'arm64', false, false)
	assert other_compiler.len == 0
}

fn test_v3_default_linker_flags() {
	assert v3_default_linker_flags('windows', false) == []
	assert v3_default_linker_flags('linux', false) == ['-lm', '-lpthread']
	assert v3_default_linker_flags('freebsd', false) == ['-lm', '-lpthread', '-lexecinfo', '-lelf']
	assert v3_default_linker_flags('netbsd', false) == ['-lm', '-lpthread', '-lexecinfo', '-lelf']
	assert v3_default_linker_flags('linux', true) == []
}

fn test_v3_default_linker_flags_do_not_duplicate_existing_flags() {
	mut flags := ['-lpthread', '-lm']
	add_v3_default_linker_flags(mut flags, 'linux', false)
	assert flags == ['-lpthread', '-lm']
}

fn test_x64_backend_aliases_are_normalized() {
	for alias in ['amd64', 'x86_64', 'x86-64'] {
		assert normalize_backend_name(alias) == 'x64'
	}
	assert normalize_backend_name('x64') == 'x64'
}

fn test_v3_fastc_default_linker_flags() {
	assert v3_fastc_default_linker_flags('windows', true) == []
	assert v3_fastc_default_linker_flags('linux', false) == ['-lm']
	assert v3_fastc_default_linker_flags('linux', true) == ['-lpthread', '-lm']
}

fn test_v3_windows_executable_linker_flags() {
	expected := ['-municode', '-Wl,--stack=33554432']
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .auto, false) == expected
	assert v3_windows_executable_linker_flags('windows', 'gcc', false, false, .auto, false) == expected
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .auto, true) == [
		'-municode',
		'-mwindows',
		'-Wl,--stack=33554432',
	]
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .console, true) == [
		'-municode',
		'-mconsole',
		'-Wl,--stack=33554432',
	]
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, false, .windows, false) == [
		'-municode',
		'-mwindows',
		'-Wl,--stack=33554432',
	]
	assert v3_windows_executable_linker_flags('windows', 'msvc', false, false, .auto, true) == []
	assert v3_windows_executable_linker_flags('windows', 'tinyc', true, false, .auto, true) == []
	assert v3_windows_executable_linker_flags('windows', 'tinyc', false, true, .auto, true) == []
	assert v3_windows_executable_linker_flags('linux', 'tinyc', false, false, .auto, true) == []
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		target_os:       'windows'
		c_compiler:      'tinyc'
		windows_gui_app: true
	})
	assert '-municode' in plan.before_inputs
	assert '-mwindows' in plan.before_inputs
	assert '-Wl,--stack=33554432' in plan.before_inputs
}

fn test_v3_cgen_metadata_preserves_windows_gui_entry_point() {
	encoded := encode_v3_cgen_metadata(['-lgdi32'], 'interfaces', 'prefix', true, []V3CachedTypeDiagnostic{})
	decoded := decode_v3_cgen_metadata(encoded) or { panic('could not decode Cgen metadata') }
	assert decoded.windows_gui_entry_point
	assert decoded.flags == ['-lgdi32']
}

fn test_v3_fastc_rejects_windows_gui_subsystem() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_windows_subsystem_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	build := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-nocache', '-b', 'fastc',
		'-os', 'windows', '-subsystem', 'windows', source])
	assert build.exit_code != 0, build.output
	assert build.output.contains('the V3 fastc backend does not support `-subsystem windows`'), build.output
}

fn test_add_c_language_runtime_link_flags() {
	target := pref.Target{
		os: 'linux'
	}
	mut objective_c := []string{}
	add_c_language_runtime_link_flags(mut objective_c, [], 'objective-c', target)
	assert objective_c == ['-lobjc']

	mut objective_cpp := []string{}
	add_c_language_runtime_link_flags(mut objective_cpp, [], 'objective-c++', target)
	assert objective_cpp == ['-lstdc++', '-lobjc']

	mut existing := ['-lstdc++', '-lobjc']
	add_c_language_runtime_link_flags(mut existing, existing.clone(), 'objective-c++', target)
	assert existing == ['-lstdc++', '-lobjc']
}

fn test_v3_cache_failure_artifacts_needs_a_cached_path_and_a_whole_file_failure() {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	object := os.join_path(cache_dir, 'atomic_deadbeef_cafe.o')
	rejected := 'tcc: error: ${object}: unrecognized file type'
	assert v3_cache_failure_artifacts(rejected) == [
		os.join_path_single(os.real_path(cache_dir), os.base(object)),
	]
	assert v3_cache_failure_artifacts('/usr/bin/ld:${object}: file format not recognized') == [
		os.join_path_single(os.real_path(cache_dir), os.base(object)),
	]

	// A line-scoped diagnostic in a cached unit is a real compile error, not a
	// poisoned entry; spending a rebuild on it would only reproduce it.
	module_source := os.join_path(os.vtmp_dir(), 'v3_module_cache_1234', 'abcd', 'main_9.c')
	compile_error := '${module_source}:41:7: error: use of undeclared identifier'
	assert v3_cache_failure_artifacts(compile_error) == []

	// The build directory sits next to the caches but holds generated source.
	build_source := os.join_path(os.vtmp_dir(), 'prog.01M2AH.tmp.c')
	assert v3_cache_failure_artifacts('cc: ${build_source}: no such file or directory') == []

	// A whole-file failure about a file V does not own is the user's to fix.
	assert v3_cache_failure_artifacts('ld: file not found: /usr/local/lib/libfoo.a') == []

	// Every marker has to be paired with a cached path.
	assert v3_cache_failure_artifacts('ld: duplicate symbol _main') == []

	// A warning about a healthy cache entry and a separate linker error do not
	// identify that entry as the cause of the failure.
	unrelated_failure := 'ld: warning: using ${object}\nld: cannot find -lfoo: No such file or directory'
	assert v3_cache_failure_artifacts(unrelated_failure) == []
}

fn test_v3_cache_failure_artifacts_reads_multiline_duplicate_symbols() {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_fastc_unit_cache')
	os.mkdir_all(cache_dir)!
	object := os.join_path(cache_dir, 'duplicate_${os.getpid()}.o')
	defer { os.rm(object) or {} }
	os.write_file(object, 'broken')!
	canonical := os.real_path(object)
	lld := 'ld.lld: error: duplicate symbol: value\n>>> defined at src.c:4\n>>> ${object}\n'
	assert v3_cache_failure_artifacts(lld) == [canonical]
	ld64 := 'duplicate symbol _value in:\n    ${object}\nld: 1 duplicate symbols\n'
	assert v3_cache_failure_artifacts(ld64) == [canonical]
}

fn test_v3_fastc_cache_failure_maps_restored_build_object_to_owned_entry() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_failure_map_${os.getpid()}')
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_fastc_unit_cache')
	os.mkdir_all(cache_dir)!
	cache_object := os.join_path(cache_dir, 'unit_${os.getpid()}.o')
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rm(cache_object) or {}
		os.rmdir_all(root) or {}
	}
	os.write_file(cache_object, 'broken')!
	build_object := os.join_path(root, 'build', 'unit.o')
	output := "ld: '${build_object}': file format not recognized"
	assert v3_cache_failure_artifacts(output) == []
	mapped := v3_fastc_cache_failure_output(output, {
		build_object: cache_object
	})
	assert v3_cache_failure_artifacts(mapped) == [os.real_path(cache_object)]
}

fn test_v3_cache_error_artifacts_reads_paths_out_of_toolchain_diagnostics() {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_fastc_unit_cache')
	os.mkdir_all(cache_dir)!
	first := os.join_path(cache_dir, 'unit_1.o')
	second := os.join_path(cache_dir, 'unit_2.o')
	output := "ld: warning: ignoring file '${first}', building for macOS-arm64\n" + 'ld: ${second}: file format not recognized\n' + 'ld: ${first}: not an object file\n'
	// Quoting and punctuation differ per toolchain, and one path can be blamed
	// more than once.
	canonical_dir := os.real_path(cache_dir)
	assert v3_cache_error_artifacts(output) == [
		os.join_path_single(canonical_dir, os.base(first)),
		os.join_path_single(canonical_dir, os.base(second)),
	]
	assert v3_cache_error_artifacts('') == []
}

fn test_v3_cache_artifact_detection_rejects_paths_outside_owned_directories() {
	root := os.join_path(os.vtmp_dir(), 'v3_cache_recovery_guard_${os.getpid()}')
	cache_dir := os.join_path(root, 'v3_module_cache_ab12')
	outside_dir := os.join_path(root, 'v3_thirdparty_objs_backup')
	os.mkdir_all(cache_dir)!
	os.mkdir_all(outside_dir)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	outside := os.join_path(outside_dir, 'user.o')
	os.write_file(outside, 'keep')!
	traversal := '${cache_dir}/../v3_thirdparty_objs_backup/user.o'
	for path in [outside, traversal] {
		assert v3_cache_failure_artifacts('ld: ${path}: file format not recognized') == []
	}
	$if !windows {
		link := os.join_path(cache_dir, 'linked-user.o')
		os.symlink(outside, link)!
		assert v3_cache_failure_artifacts('ld: ${link}: file format not recognized') == []
	}
	assert v3_discard_cache_artifacts([outside, traversal]) == 0
	assert os.read_file(outside)! == 'keep'
	for name in v3_cache_artifact_dir_names {
		user_cache := os.join_path(root, name)
		os.mkdir_all(user_cache)!
		user_object := os.join_path(user_cache, 'user.o')
		os.write_file(user_object, 'keep')!
		assert v3_cache_failure_artifacts('ld: ${user_object}: file format not recognized') == []
		assert v3_discard_cache_artifacts([user_object]) == 0
		assert os.read_file(user_object)! == 'keep'
	}
}

fn test_v3_cache_artifact_detection_accepts_quoted_paths_with_spaces() {
	root := os.join_path(os.vtmp_dir(), 'v3 cache recovery ${os.getpid()}')
	cache_dir := os.join_path(root, 'v3_module_cache_ab12')
	os.mkdir_all(cache_dir)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	object := os.join_path(cache_dir, 'cached object.o')
	os.write_file(object, 'broken')!
	source := os.join_path(cache_dir, 'main.c')
	os.write_file(source, '#include "missing.h"\n')!
	clang_missing := v3_cache_failure_artifacts("${source}:42:10: fatal error: 'missing.h' file not found")
	assert clang_missing == [], clang_missing.str()
	assert v3_cache_failure_artifacts("${source}:42:10: fatal error: 'missing.h': No such file or directory") == []
	assert v3_cache_failure_artifacts('clang: ${source}: missing.h: file not found') == []
	assert v3_cache_failure_artifacts("ld: '${object}': file format not recognized") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts('/usr/bin/ld:${object}: file format not recognized') == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("ld: file too small (length=0) in '${object}'") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("ld: file too short: '${object}'") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts('ld.lld: ${object}: section table goes past the end of file') == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("ld: empty file '${object}'") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("ld: i386 architecture of input file `${object}' is incompatible with i386:x86-64 output") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("link.exe: LNK1136: invalid or corrupt file '${object}'") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("link.exe: LNK1107: invalid or corrupt file '${object}'") == [
		os.real_path(object),
	]
	assert v3_cache_failure_artifacts("ld: warning: ignoring file '${object}', building for macOS-arm64 but attempting to link with file built for macOS-x86_64") == [
		os.real_path(object),
	]
	$if windows {
		assert v3_cache_failure_artifacts('link.exe: ${object}: file format not recognized') == [
			os.real_path(object),
		]
	}
	assert !v3_cache_recovery_should_retry([object], 0)
	module_dir := os.join_path(root, 'v3_module_cache_ab12', 'config')
	os.mkdir_all(module_dir)!
	module_object := os.join_path(module_dir, 'module.o')
	os.write_file(module_object, 'broken')!
	assert v3_cache_failure_artifacts("ld: '${module_object}': file format not recognized") == [
		os.real_path(module_object),
	]
	missing := os.join_path(module_dir, 'missing object.o')
	assert v3_cache_failure_artifacts("ld: '${missing}': No such file or directory") == [
		os.join_path_single(os.real_path(module_dir), os.base(missing)),
	]
	assert v3_cache_failure_artifacts("link.exe: fatal error LNK1104: cannot open file '${missing}'") == [
		os.join_path_single(os.real_path(module_dir), os.base(missing)),
	]
	missing_dylib := os.join_path(module_dir, 'module.dylib')
	assert v3_cache_failure_artifacts("ld: '${missing_dylib}': No such file or directory") == [
		os.join_path_single(os.real_path(module_dir), os.base(missing_dylib)),
	]
	assert v3_cache_recovery_should_retry([missing], 0)
	os.rmdir_all(cache_dir)!
	recovered := v3_cache_failure_artifacts("ld: '${missing}': No such file or directory")
	assert recovered == [
		os.join_path(os.real_path(root), 'v3_module_cache_ab12', 'config', 'missing object.o'),
	]
	assert v3_cache_failure_artifacts("ld: '${missing_dylib}': No such file or directory") == [
		os.join_path(os.real_path(root), 'v3_module_cache_ab12', 'config', 'module.dylib'),
	]
	assert v3_cache_failure_artifacts("ld: '${source}': No such file or directory") == []
	unowned := os.join_path(root, 'v3_module_cache_nothex', 'missing.o')
	assert v3_cache_failure_artifacts("ld: '${unowned}': No such file or directory") == []
	discarded := v3_discard_cache_artifacts([missing])
	assert discarded == 0
	assert v3_cache_recovery_should_retry([missing], discarded)
}

fn test_v3_cache_failure_artifacts_recognizes_lld_truncated_objects_as_scripts() {
	root := os.join_path(os.vtmp_dir(), 'v3_lld_script_cache_${os.getpid()}')
	cache_dir := os.join_path(root, 'v3_module_cache_ab12')
	os.mkdir_all(cache_dir)!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := previous {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	object := os.join_path(cache_dir, 'truncated.o')
	os.write_file(object, '\x7fELF')!
	for message in ['unexpected EOF', 'unknown directive'] {
		assert v3_cache_failure_artifacts('ld.lld: ${object}:1: ${message}') == [
			os.real_path(object),
		]
	}
	os.write_file(object, 'this is a larger linker script, not a truncated object')!
	assert v3_cache_failure_artifacts('ld.lld: ${object}:1: unknown directive') == []
}

fn test_v3_cache_unquoted_windows_path_candidates_keep_spaces() {
	drive := 'C:\\Users\\First Last\\cached.o'
	unc := '\\\\server\\share\\First Last\\cached.o'
	assert v3_cache_unquoted_path_candidates('link.exe: ${drive}') == [drive]
	assert v3_cache_unquoted_path_candidates('link.exe: ${unc}') == [unc]
}

fn test_v3_cache_artifact_detection_includes_isolated_test_modules() {
	root := os.join_path(os.vtmp_dir(), 'v3_test_cache_${os.getpid()}')
	cache_dir := os.join_path(root, 'v3_module_cache_ab12')
	os.mkdir_all(cache_dir)!
	previous_cache := os.getenv_opt('V3CACHE')
	previous_isolate := os.getenv_opt('V3_TEST_ISOLATE_CACHE')
	os.unsetenv('V3CACHE')
	os.setenv('V3_TEST_ISOLATE_CACHE', '1', true)
	defer {
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		}
		if value := previous_isolate {
			os.setenv('V3_TEST_ISOLATE_CACHE', value, true)
		} else {
			os.unsetenv('V3_TEST_ISOLATE_CACHE')
		}
		os.rmdir_all(root) or {}
	}
	object := os.join_path(cache_dir, 'cached object.o')
	os.write_file(object, 'broken')!
	assert v3_cache_failure_artifacts("ld: '${object}': file format not recognized") == [
		os.real_path(object),
	]
}

fn test_v3_discard_cache_artifacts_drops_sidecars_and_link_plans() {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	os.mkdir_all(cache_dir)!
	token := 'v3_discard_test_${os.getpid()}'
	object := os.join_path(cache_dir, '${token}.o')
	stamp := '${object}.stamp'
	deps := '${object}.deps'
	plan := os.join_path(cache_dir, 'link_${token}.manifest')
	unrelated := os.join_path(cache_dir, '${token}_keep.o')
	for path in [object, stamp, deps, plan, unrelated] {
		os.write_file(path, 'x')!
	}
	defer {
		for path in [object, stamp, deps, plan, unrelated] {
			os.rm(path) or {}
		}
	}
	discarded := v3_discard_cache_artifacts([object])
	assert discarded >= 4, '${discarded}'
	assert !os.exists(object)
	// A stamp still certifies a deleted object, and a link plan replays its path
	// into the next link, so both have to go with it.
	assert !os.exists(stamp)
	assert !os.exists(deps)
	assert !os.exists(plan)
	assert os.exists(unrelated)
}

fn test_tcc_atomic_object_key_separates_compilers_targets_and_args() {
	root := os.join_path(os.vtmp_dir(), 'v3_atomic_object_key_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	target := pref.Target{
		os:            'macos'
		arch:          'arm64'
		object_format: 'macho'
	}
	first_tcc := os.join_path(root, 'first_tcc')
	second_tcc := os.join_path(root, 'second_tcc')
	os.write_file(first_tcc, 'first')!
	os.write_file(second_tcc, 'second compiler build')!
	args := ['-std=gnu11', '-fwrapv']
	key := tcc_atomic_object_key('source-signature', first_tcc, args, target)

	assert key == tcc_atomic_object_key('source-signature', first_tcc, args, target)
	// A different compiler build must not reuse the object: tcc emits ELF
	// objects even on macOS, and rejects a Mach-O object with
	// "unrecognized file type" instead of falling back to reassembling atomic.S.
	assert key != tcc_atomic_object_key('source-signature', second_tcc, args, target)
	assert key != tcc_atomic_object_key('other-signature', first_tcc, args, target)
	assert key != tcc_atomic_object_key('source-signature', first_tcc, ['-std=gnu11'], target)
	assert key != tcc_atomic_object_key('source-signature', first_tcc, args, pref.Target{
		...target
		arch: 'amd64'
	})
}

fn test_tcc_compiler_identity_tracks_rebuilds() {
	root := os.join_path(os.vtmp_dir(), 'v3_atomic_compiler_identity_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	compiler := os.join_path(root, 'tcc.exe')
	os.write_file(compiler, 'first')!
	first := tcc_compiler_identity(compiler)
	assert first == tcc_compiler_identity(compiler)
	// `make` re-pulls and rebuilds thirdparty/tcc in place, so the identity has
	// to change when the executable at the same path does.
	os.write_file(compiler, 'a rebuilt compiler with a different size')!
	assert first != tcc_compiler_identity(compiler)
}

fn test_tcc_compiler_identity_tracks_same_size_rebuilds() {
	root := os.join_path(os.vtmp_dir(), 'v3_atomic_identity_same_size_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	compiler := os.join_path(root, 'tcc.exe')
	os.write_file(compiler, 'first build')!
	first := tcc_compiler_identity(compiler)
	// Rewritten in place to the same byte count, within one filesystem timestamp
	// second. Size plus a second-resolution mtime cannot separate these two
	// compilers, and reusing an object across them is what produces an
	// "unrecognized file type" link failure that no later build clears.
	os.write_file(compiler, 'other build')!
	assert os.file_size(compiler) == 11
	assert first != tcc_compiler_identity(compiler)
}

// Native macOS builds use the bundled TCC by default on both supported architectures.
fn test_v3_native_macos_selects_bundled_tcc_by_default() {
	$if windows {
		// The fake compiler is a shell script.
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_macos_default_tcc_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	bundled_tcc := write_v3_test_tcc(os.join_path(root, 'thirdparty', 'tcc', 'tcc.exe'), 0)
	for arch in ['arm64', 'amd64'] {
		target := pref.Target{
			os:   'macos'
			arch: arch
		}
		options := V3BundledTccProbeOptions{
			backend:     'c'
			c_compiler:  'cc'
			host_os:     'macos'
			host_target: target
			target:      target
			bundled_tcc: bundled_tcc
		}
		assert v3_should_probe_bundled_tcc(options), arch
		selection := v3_select_c_compiler(root, options)
		assert selection.bundled_tcc_available, arch
		assert selection.implicit_tcc == bundled_tcc, arch
		assert selection.c_compiler == bundled_tcc, arch
		assert selection.use_implicit_tcc_semantics, arch
		assert selection.effective_c_compiler == 'tinyc', arch
		// An explicit compiler still overrides the platform default.
		explicit := v3_select_c_compiler(root, V3BundledTccProbeOptions{
			...options
			c_compiler:          'clang'
			c_compiler_explicit: true
		})
		assert explicit.c_compiler == 'clang', arch
		assert explicit.implicit_tcc == '', arch
		assert !explicit.use_implicit_tcc_semantics, arch
	}
}

fn test_v3_cstrict_flag_plan_has_one_conversion_warning_override() {
	for compiler in ['gcc', 'clang', 'tinyc'] {
		for strict in [false, true] {
			plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
				c_compiler: compiler
				is_tcc:     compiler == 'tinyc'
				target_os:  'linux'
				warn_args:  if strict {
					['-Wall', '-Werror=implicit-function-declaration', '-Wno-int-conversion']
				} else {
					['-w']
				}
			})
			assert plan.before_inputs.filter(it == '-Wno-int-conversion').len == 1
		}
	}
}

fn test_v3_tcc_flags_dump_matches_monolithic_command() {
	$if windows {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'tcc_flag_dump_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main')
	dump_file := os.join_path(root, 'flags.txt')
	mut sources := ['fn main() {}\n']
	$if macos {
		sources << '#flag -F${root}\n#flag -framework CoreFoundation\nfn main() {}\n'
	}
	for source_text in sources {
		os.write_file(source, source_text)!
		for strict in [false, true] {
			mut args := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', 'tcc', '-gc',
				'none']
			if strict {
				args << '-cstrict'
			}
			mut dump_args := args.clone()
			dump_args << ['-dump-c-flags', '-', '-o', output, source]
			flags_dump := cmdexec.run(v3_driver_test_executable(), dump_args)
			assert flags_dump.exit_code == 0, flags_dump.output
			dump_lines := flags_dump.output.split_into_lines().filter(it.len > 0)
			// value-taking flags share a line with their quoted value (`-I "/path"`)
			mut expected := []string{}
			for line in dump_lines {
				for token in cgen.tokenize_c_flag(line) {
					expected << if token.len > 1 && token.starts_with('"') && token.ends_with('"') {
						token[1..token.len - 1]
					} else {
						token
					}
				}
			}
			for with_dump in [false, true] {
				mut build_args := args.clone()
				build_args << ['-showcc', '-o', output, source]
				if with_dump {
					build_args << ['-dump-c-flags', dump_file]
				}
				build := cmdexec.run(v3_driver_test_executable(), build_args)
				assert build.exit_code == 0, build.output
				commands := build.output.split_into_lines().filter(it.trim_space().starts_with('> ')
					&& it.contains('-o out'))
				assert commands.len == 1, build.output
				command := cgen.tokenize_c_flag(commands[0].trim_space().trim_left('> '))
				output_index := command.index('-o')
				assert output_index > 0, commands[0]
				mut actual := command[1..output_index].clone()
				actual << command[output_index + 3..]
				assert actual == expected, '${flags_dump.output}\n${commands[0]}'
				if source_text.contains('-framework') {
					assert actual.any(it.contains('CoreFoundation.framework'))
					assert !actual.any(it.starts_with('-F'))
				}
				if with_dump {
					assert os.read_lines(dump_file)! == dump_lines
				}
			}
		}
	}
}

fn test_v3_production_object_flag_plan_retains_machine_code() {
	for compiler in ['gcc', 'clang', 'msvc', 'tinyc'] {
		for large in [false, true] {
			options := V3CCompilerFlagOptions{
				is_prod:      true
				is_o:         true
				target_os:    'windows'
				c_compiler:   compiler
				large_c_unit: large
				is_tcc:       compiler == 'tinyc'
			}
			plan := v3_c_compiler_flag_plan(options)
			assert '-flto' !in plan.before_inputs
			assert '-c' in plan.before_inputs
			assert (if large { '-O2' } else { '-O3' }) in plan.before_inputs
		}
	}
	program := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{ is_prod: true })
	assert '-flto' in program.before_inputs
}

fn test_v3_production_object_links_with_independent_c_main() {
	$if windows {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'prod_object_link_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'object_probe.v')
	object := os.join_path(root, 'object_probe.o')
	main_c := os.join_path(root, 'main.c')
	exe := os.join_path(root, 'main')
	os.write_file(source, "module object_probe\n\n@[export: 'v3_object_probe']\n@[markused]\nfn add(a i32, b i32) i32 { return a + b }\n")!
	compiled := cmdexec.run(v3_driver_test_executable(), ['-new-compiler', '-prod', '-cc', clang,
		'-gc', 'none', '-o', object, source])
	assert compiled.exit_code == 0, compiled.output
	data := os.read_bytes(object)!
	assert data.len >= 4
	// Clang's raw bitcode and bitcode-wrapper magic must not replace machine code.
	assert data[..4] != [u8(0x42), 0x43, 0xc0, 0xde]
	assert data[..4] != [u8(0xde), 0xc0, 0x17, 0x0b]
	os.write_file(main_c, '#include <stdint.h>\nextern int32_t v3_object_probe(int32_t, int32_t);\nint main(void) { return v3_object_probe(20, 22) == 42 ? 0 : 1; }\n')!
	linked := cmdexec.run(clang, [main_c, object, '-lm', '-o', exe])
	assert linked.exit_code == 0, linked.output
	ran := cmdexec.run(exe, [])
	assert ran.exit_code == 0, ran.output
}
