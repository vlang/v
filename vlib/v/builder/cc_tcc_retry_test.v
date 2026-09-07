module builder

import os
import v.pref

fn execute_tcc_retry_test_command(cmd string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	// These tests exercise the established C builder's TinyCC retry path. Linux
	// now defaults to V3, so select V1 explicitly after isolating ambient flags.
	os.setenv('VFLAGS', '-old-compiler', true)
	os.unsetenv('VOSARGS')
	defer {
		if vflags := old_vflags {
			os.setenv('VFLAGS', vflags, true)
		}
		if vosargs := old_vosargs {
			os.setenv('VOSARGS', vosargs, true)
		}
	}
	return os.execute(cmd)
}

fn test_is_tcc_compilation_failure_detects_tcc_kind() {
	assert is_tcc_compilation_failure('cc', .tcc, '')
}

fn test_is_tcc_compilation_failure_detects_tcc_compiler_name() {
	assert is_tcc_compilation_failure('tcc', .unknown, '')
	assert is_tcc_compilation_failure('/opt/v/thirdparty/tcc/tcc.exe', .unknown, '')
	assert is_tcc_compilation_failure('/usr/local/bin/tcc-0.9.27', .unknown, '')
	assert !is_tcc_compilation_failure('/usr/bin/clang', .unknown, '')
}

fn test_is_tcc_compilation_failure_detects_tcc_output() {
	assert is_tcc_compilation_failure('cc', .unknown, 'tcc: error: bad architecture')
	assert is_tcc_compilation_failure('cc', .unknown, 'line 1\nline 2\ntcc: error: lib not found')
	assert !is_tcc_compilation_failure('cc', .unknown, 'clang: error: unsupported option')
}

fn test_is_tcc_compilation_failure_detects_tcc_alias_compiler() {
	if os.user_os() == 'windows' {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_cc_tcc_retry_test_${os.getpid()}')
	cc_path := os.join_path(test_root, 'cc')
	old_path := os.getenv('PATH')
	os.mkdir_all(test_root) or { panic(err) }
	os.write_file(cc_path, '#!/bin/sh\necho "Tiny C Compiler"\n') or { panic(err) }
	os.chmod(cc_path, 0o700) or { panic(err) }
	os.setenv('PATH', '${test_root}${os.path_delimiter}${old_path}', true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert is_tcc_compilation_failure('cc', .unknown, '')
}

fn test_tcc_retry_warning_is_visible() {
	if os.user_os() == 'windows' {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_tcc_retry_warning_${os.getpid()}')
	fake_tcc := os.join_path(test_root, 'fake-tcc')
	source_path := os.join_path(test_root, 'main.v')
	exe_path := os.join_path(test_root, 'main')
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.write_file(fake_tcc,
		'#!/bin/sh\necho "tcc: error: _Thread_local is not implemented"\nexit 1\n') or {
		panic(err)
	}
	os.chmod(fake_tcc, 0o700) or { panic(err) }
	os.write_file(source_path, 'fn main() {}\n') or { panic(err) }
	res :=
		execute_tcc_retry_test_command('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(fake_tcc)} -d run -o ${os.quoted_path(exe_path)} run ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
	assert res.output.contains('warning: tcc compilation failed, falling back to cc'), res.output
}

fn test_tcc_retry_inserts_fallback_flags_before_implicit_vsh_script() {
	script_path := os.join_path(os.vtmp_dir(), 'implicit_retry_script.vsh')
	builder := &Builder{
		pref: &pref.Preferences{
			is_crun:  true
			is_vsh:   true
			path:     script_path
			run_args: ['script-argument']
		}
	}
	args := ['-cc', 'tcc', script_path, 'script-argument']
	assert builder.retry_command_boundary(args) == 2
	assert builder.retry_command_boundary([script_path, 'script-argument']) == 0
	assert builder.retry_command_boundary(['-d', 'crun', 'crun', script_path, 'script-argument']) == 2
}

fn test_tcc_retry_finds_run_boundary_for_executable_alias() {
	executable_path := os.join_path(os.vtmp_dir(), 'retry_executable_alias')
	builder := &Builder{
		pref: &pref.Preferences{
			is_run:   true
			path:     '${executable_path}.v'
			run_args: ['program-argument']
		}
	}
	args := ['-d', 'run', '-cc', 'tcc', 'run', executable_path, 'program-argument']
	assert builder.retry_command_boundary(args) == 4
}

fn test_tcc_retry_filters_build_module_compilers_after_target() {
	module_path := os.join_path(os.vtmp_dir(), 'retry_build_module')
	builder := &Builder{
		pref: &pref.Preferences{
			build_mode: .build_module
			path:       module_path
		}
	}
	args := ['-cc=tcc', 'build-module', module_path, '-d', 'retry_feature', '-cc', 'tcc']
	assert builder.retry_compilation_args(args, 'clang') == [
		'-cc',
		'clang',
		'-no-retry-compilation',
		'build-module',
		module_path,
		'-d',
		'retry_feature',
	]
}

fn test_tcc_retry_forwards_stdout_producing_modes() {
	mut preferences := &pref.Preferences{}
	builder := &Builder{
		pref: preferences
	}
	assert !builder.should_forward_retry_output()
	preferences.dump_c_flags = '-'
	assert builder.should_forward_retry_output()
	preferences.dump_c_flags = ''
	preferences.is_stats = true
	assert builder.should_forward_retry_output()
	preferences.is_stats = false
	preferences.dump_modules = '-'
	assert builder.should_forward_retry_output()
	preferences.dump_modules = ''
	preferences.dump_files = '-'
	assert builder.should_forward_retry_output()
	preferences.dump_files = ''
	preferences.dump_defines = '-'
	assert builder.should_forward_retry_output()
}

fn test_tcc_retry_forwards_corrected_dump_c_flags() {
	if os.user_os() == 'windows' {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_tcc_retry_dump_flags_${os.getpid()}')
	fake_tcc := os.join_path(test_root, 'fake-tcc')
	source_path := os.join_path(test_root, 'main.v')
	exe_path := os.join_path(test_root, 'main')
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.write_file(fake_tcc, '#!/bin/sh\necho "tcc: error: first compiler failed"\nexit 1\n') or {
		panic(err)
	}
	os.chmod(fake_tcc, 0o700) or { panic(err) }
	os.write_file(source_path,
		'$if tinyc {\n\t#flag -D V_RETRY_TINYC_CFLAGS\n} $else {\n\t#flag -D V_RETRY_SYSTEM_CFLAGS\n}\n\nfn main() {}\n') or {
		panic(err)
	}
	res :=
		execute_tcc_retry_test_command('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(fake_tcc)} -dump-c-flags - -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
	assert res.output.contains('V_RETRY_TINYC_CFLAGS'), res.output
	assert res.output.contains('V_RETRY_SYSTEM_CFLAGS'), res.output
}

fn test_tcc_retry_reports_final_compiler_failure() {
	if os.user_os() == 'windows' {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_tcc_retry_failure_${os.getpid()}')
	fake_tcc := os.join_path(test_root, 'fake-tcc')
	header_path := os.join_path(test_root, 'retry_failure.h')
	source_path := os.join_path(test_root, 'retry_test.v')
	exe_path := os.join_path(test_root, 'retry_test')
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.write_file(fake_tcc, '#!/bin/sh\necho "tcc: error: first compiler failed"\nexit 1\n') or {
		panic(err)
	}
	os.chmod(fake_tcc, 0o700) or { panic(err) }
	os.write_file(header_path, '#error retry_system_compiler_failure\n') or { panic(err) }
	os.write_file(source_path, '#include "${header_path}"\nfn test_retry() {}\n') or { panic(err) }
	res :=
		execute_tcc_retry_test_command('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(fake_tcc)} -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code != 0, res.output
	assert res.output.contains('warning: tcc compilation failed, falling back to cc'), res.output
	assert res.output.contains('C compilation error (from cc)'), res.output
	assert res.output.contains('retry_system_compiler_failure'), res.output
	assert !res.output.contains('C compilation error (from tcc)'), res.output
}

fn test_tcc_retry_preserves_shared_and_enable_globals_flags() {
	if os.user_os() == 'windows' {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_builder_tcc_retry_shared_${os.getpid()}')
	fake_tcc := os.join_path(test_root, 'fake-tcc')
	source_path := os.join_path(test_root, 'library.v')
	library_path := os.join_path(test_root, if os.user_os() == 'macos' {
		'libretry.dylib'
	} else {
		'libretry.so'
	})
	os.mkdir_all(test_root) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	os.write_file(fake_tcc, '#!/bin/sh\necho "tcc: error: first compiler failed"\nexit 1\n') or {
		panic(err)
	}
	os.chmod(fake_tcc, 0o700) or { panic(err) }
	os.write_file(source_path,
		'__global (\n\tretry_value = 7\n)\n\n@[export: "tcc_retry_value"]\npub fn tcc_retry_value() int {\n\treturn retry_value\n}\n') or {
		panic(err)
	}
	res :=
		execute_tcc_retry_test_command('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(fake_tcc)} -shared -enable-globals -o ${os.quoted_path(library_path)} ${os.quoted_path(source_path)}')
	assert res.exit_code == 0, res.output
	assert res.output.contains('warning: tcc compilation failed, falling back to cc'), res.output
	assert os.is_file(library_path)
}

fn fake_windows_short_path(path string) string {
	return path.replace(r'C:\Program Files', r'C:\PROGRA~1').replace(r'C:\Users\Léo', r'C:\Users\LEO~1')
}

fn test_rewrite_windows_path_arg_rewrites_quoted_object_paths() {
	arg := r'"C:\Users\Léo\.vmodules\.cache\bc\artifact.o"'
	expected := r'"C:\Users\LEO~1\.vmodules\.cache\bc\artifact.o"'
	assert rewrite_windows_path_arg(arg, fake_windows_short_path) == expected
}

fn test_rewrite_windows_path_arg_rewrites_prefixed_paths() {
	assert rewrite_windows_path_arg(r'-I"C:\Users\Léo\include"', fake_windows_short_path) == r'-I"C:\Users\LEO~1\include"'
	assert rewrite_windows_path_arg(r'-L"C:\Users\Léo\lib"', fake_windows_short_path) == r'-L"C:\Users\LEO~1\lib"'
	assert rewrite_windows_path_arg(r'-o "C:\Users\Léo\bin\tool.exe"', fake_windows_short_path) == r'-o "C:\Users\LEO~1\bin\tool.exe"'
}

fn test_rewrite_windows_path_arg_leaves_non_paths_alone() {
	for arg in ['-bt25', '-std=c99', '-D_DEFAULT_SOURCE'] {
		assert rewrite_windows_path_arg(arg, fake_windows_short_path) == arg
	}
}

fn test_rewrite_windows_path_operand_arg_rewrites_path_operands() {
	assert rewrite_windows_path_operand_arg(r'-I"C:\Users\Léo\include"', fake_windows_short_path) == r'-I"C:\Users\LEO~1\include"'
	assert rewrite_windows_path_operand_arg(r"-I'C:\Users\Léo\include'", fake_windows_short_path) == r'-I"C:\Users\LEO~1\include"'
	assert rewrite_windows_path_operand_arg(r'-L"C:\Users\Léo\lib"', fake_windows_short_path) == r'-L"C:\Users\LEO~1\lib"'
	assert rewrite_windows_path_operand_arg(r'-B"C:\Users\Léo\bin"', fake_windows_short_path) == r'-B"C:\Users\LEO~1\bin"'
	assert rewrite_windows_path_operand_arg(r'-o "C:\Users\Léo\bin\tool.exe"', fake_windows_short_path) == r'-o "C:\Users\LEO~1\bin\tool.exe"'
	obj := r'"C:\Users\Léo\.vmodules\.cache\bc\artifact.o"'
	expected := r'"C:\Users\LEO~1\.vmodules\.cache\bc\artifact.o"'
	assert rewrite_windows_path_operand_arg(obj, fake_windows_short_path) == expected
	assert rewrite_windows_path_operand_arg(r"'C:\Users\Léo\input.o'", fake_windows_short_path) == r'"C:\Users\LEO~1\input.o"'
	assert rewrite_windows_path_operand_arg(r'-B"C:\toolchain\"', fake_windows_short_path) == r'-B"C:\toolchain\\"'
}

fn test_rewrite_windows_path_operand_arg_leaves_path_like_values_alone() {
	// `-DROOT="C:\Program Files\SDK"` is macro data, not a filesystem operand:
	// rewriting it would silently change the compiled macro value (see the
	// review of issue #28126), so it must be left byte for byte untouched.
	for arg in [r'-DROOT="C:\Program Files\SDK"', '-D_DEFAULT_SOURCE', '-std=c99', '-bt25'] {
		assert rewrite_windows_path_operand_arg(arg, fake_windows_short_path) == arg
	}
}

fn test_rewrite_windows_path_operand_arg_leaves_compound_environment_flags_alone() {
	for arg in [r'-IC:\sdk -DFOO=1', r'-I"C:\Program Files\SDK" -DFOO=1', r'-LC:\sdk -lfoo',
		r'"C:\sdk\input.o" -DFOO=1'] {
		assert rewrite_windows_path_operand_arg(arg, fake_windows_short_path) == arg
	}
}

fn test_rewrite_windows_path_arg_rewrites_path_like_values() {
	// the broader tcc rewrite still rewrites quoted path substrings, even in
	// data bearing options; only rewrite_windows_path_operand_arg (gcc) leaves
	// them alone
	arg := r'-DROOT="C:\Program Files\SDK"'
	expected := r'-DROOT="C:\PROGRA~1\SDK"'
	assert rewrite_windows_path_arg(arg, fake_windows_short_path) == expected
}

fn test_cc_uses_short_windows_paths() {
	// tcc and the MinGW GCC toolchain both read response files with the ANSI C
	// runtime, so both prefer ASCII 8.3 short paths on Windows (see issue #28126).
	assert cc_uses_short_windows_paths(.tcc, .tinyc)
	assert cc_uses_short_windows_paths(.gcc, .gcc)
	assert cc_uses_short_windows_paths(.unknown, .cplusplus)
	// clang, msvc, icc, emcc and unknown compilers are excluded:
	// msvc uses the wide CreateProcessW command line directly, while the
	// clang/LLVM toolchain handles Unicode paths on its own.
	assert !cc_uses_short_windows_paths(.clang, .clang)
	assert !cc_uses_short_windows_paths(.msvc, .msvc)
	assert !cc_uses_short_windows_paths(.icc, .gcc)
	assert !cc_uses_short_windows_paths(.emcc, .emcc)
	assert !cc_uses_short_windows_paths(.unknown, .gcc)
}

fn test_gcc_rsp_args_require_ascii_paths() {
	assert gcc_rsp_args_are_ascii([
		r'-o "C:\PROGRA~1\main.exe"',
		r'"C:\Users\RUNNER~1\main.c"',
	])
	assert !gcc_rsp_args_are_ascii([r'-o "D:\a\_temp\工作目录\main.exe"'])
}

fn test_ccompiler_exec_args_split_shell_formatted_options() {
	assert ccompiler_exec_args('gcc', [r'-o "C:\Users\工作\main.exe"', r'"C:\Users\工作\main.c"',
		r'-I"C:\Program Files\SDK"', '-DFOO=1 -DBAR=2', r'-B"C:\toolchain\\"', r'-DNAME=\"foo\"']) == [
		'gcc',
		'-o',
		r'C:\Users\工作\main.exe',
		r'C:\Users\工作\main.c',
		r'-IC:\Program Files\SDK',
		'-DFOO=1',
		'-DBAR=2',
		r'-BC:\toolchain\',
		r'-DNAME="foo"',
	]
}

fn test_gcc_response_file_content_quotes_exact_arguments() {
	assert gcc_response_file_content([r'-B"C:\toolchain\\"', r'-DNAME=\"foo\"']) == r'"-BC:\\toolchain\\" "-DNAME=\"foo\""'
}

fn test_windows_exec_arg_escaping_preserves_embedded_quotes() {
	assert windows_quote_exec_arg(r'-DNAME="café"') == r'"-DNAME=\"café\""'
	assert windows_quote_exec_arg(r'C:\work\') == r'"C:\work\\"'
}

fn test_windows_batch_compilers_keep_the_command_interpreter_path() {
	assert ccompiler_is_windows_batch_file(r'C:\toolchains\gcc-wrapper.cmd')
	assert ccompiler_is_windows_batch_file(r'"C:\Program Files\GCC\gcc-wrapper.BAT"')
	assert !ccompiler_is_windows_batch_file(r'C:\toolchains\gcc.exe')
	$if windows {
		test_root := os.join_path(os.vtmp_dir(), 'v_gcc_batch_resolve_${os.getpid()}')
		wrapper := os.join_path(test_root, 'v-gcc-wrapper.cmd')
		os.mkdir_all(test_root) or { panic(err) }
		defer {
			os.rmdir_all(test_root) or {}
		}
		os.write_file(wrapper, '@echo off\r\nexit /b 0\r\n') or { panic(err) }
		old_path := os.getenv('PATH')
		defer {
			os.setenv('PATH', old_path, true)
		}
		os.setenv('PATH', test_root + os.path_delimiter + old_path, true)
		assert ccompiler_is_windows_batch_file('v-gcc-wrapper')
	}
}

fn test_windows_batch_execution_preserves_percent_literals() {
	$if windows {
		test_root := os.join_path(os.vtmp_dir(), 'v_gcc_batch_percent_${os.getpid()}')
		wrapper := os.join_path(test_root, 'v-gcc-wrapper.cmd')
		os.mkdir_all(test_root) or { panic(err) }
		defer {
			os.rmdir_all(test_root) or {}
		}
		os.write_file(wrapper, '@echo off\r\necho %*\r\n') or { panic(err) }
		old_keep := os.getenv_opt('KEEP')
		os.setenv('KEEP', 'expanded', true)
		defer {
			if keep := old_keep {
				os.setenv('KEEP', keep, true)
			} else {
				os.unsetenv('KEEP')
			}
		}
		arg := r'-DROOT=D:\工作\%KEEP%\main.c'
		cmd := '${windows_quote_exec_arg(wrapper)} ${windows_quote_exec_arg(arg)}'
		res := execute_windows_batch_ccompiler(cmd)
		assert res.exit_code == 0, res.output
		assert res.output.contains(r'%KEEP%'), res.output
		assert !res.output.contains('expanded'), res.output
	}
}

fn test_gcc_unicode_response_plan_keeps_large_ascii_runs_out_of_the_command_line() {
	mut args := []string{}
	for i in 0 .. 2000 {
		args << '-DV_WINDOWS_UNICODE_PATH_LONG_COMMAND_${i}=1'
	}
	assert args.join(' ').len > 32767
	args << r'-o "D:\工作目录\main.exe"'
	args << r'"D:\工作目录\main.c"'
	args << '-lm'
	plan := gcc_unicode_response_plan(r'D:\工作目录\main.c.rsp', args, 30000, false)!
	assert plan.args == [
		r'@D:\工作目录\main.c.rsp.0',
		r'D:\工作目录\main.exe',
		r'D:\工作目录\main.c',
		r'@D:\工作目录\main.c.rsp.1',
	]
	assert plan.response_files == [r'D:\工作目录\main.c.rsp.0', r'D:\工作目录\main.c.rsp.1']
	assert plan.response_contents[0].contains('V_WINDOWS_UNICODE_PATH_LONG_COMMAND_1999')
	assert plan.response_contents[1] == '"-lm"'
	assert plan.args.join(' ').len < 8191
}

fn test_gcc_unicode_response_plan_uses_ansi_when_it_preserves_oversized_unicode_runs() {
	mut args := [r'-o "D:\工作目录\main.exe"']
	for i in 0 .. 1200 {
		args << '"D:\\工作目录\\cached_${i}.o"'
	}
	assert args.join(' ').len > 32767
	plan := gcc_unicode_response_plan(r'D:\工作目录\main.c.rsp', args, 30000, true)!
	assert plan.args == [r'@D:\工作目录\main.c.rsp']
	assert plan.response_files == [r'D:\工作目录\main.c.rsp']
	assert plan.response_contents[0].contains(r'D:\\工作目录\\cached_1199.o')
}

fn test_gcc_unicode_response_plan_sizes_windows_escaped_arguments() {
	arg := r'-DNAME=\"工作\"'
	exact_arg := ccompiler_exec_args('', [arg])[1]
	plan := gcc_unicode_response_plan(r'D:\工作目录\main.c.rsp', [arg], exact_arg.len + 3,
		true)!
	assert plan.args == [r'@D:\工作目录\main.c.rsp']
}

fn test_gcc_unicode_response_plan_rejects_an_unrepresentable_oversized_command() {
	mut args := []string{}
	for i in 0 .. 1200 {
		args << '"D:\\工作目录\\cached_${i}.o"'
	}
	if plan := gcc_unicode_response_plan(r'D:\工作目录\main.c.rsp', args, 30000, false) {
		assert false, '${plan.args}'
	} else {
		assert err.msg().contains('cannot be represented in the active ANSI code page')
	}
}

fn test_windows_gnu_compilers_compile_in_a_non_ascii_directory() {
	if os.user_os() != 'windows' {
		return
	}
	for compiler_name in ['gcc', 'g++', 'c++'] {
		compiler := os.find_abs_path_of_executable(compiler_name) or { continue }
		test_root := os.join_path(os.vtmp_dir(), 'v_builder_${compiler_name}_工作目录_${os.getpid()}')
		source_path := os.join_path(test_root, 'main.v')
		exe_path := os.join_path(test_root, 'main.exe')
		os.mkdir_all(test_root) or { panic(err) }
		defer {
			os.rmdir_all(test_root) or {}
		}
		mut source := ''
		for i in 0 .. 400 {
			source += '#flag -DV_WINDOWS_UNICODE_PATH_LONG_COMMAND_${i}=1\n'
		}
		source += "fn main() { println('unicode-path-ok') }\n"
		assert source.len > 8191
		os.write_file(source_path, source) or {
			panic(err)
		}
		res :=
			execute_tcc_retry_test_command('${os.quoted_path(@VEXE)} -cc ${os.quoted_path(compiler)} -gc none -no-retry-compilation -o ${os.quoted_path(exe_path)} ${os.quoted_path(source_path)}')
		assert res.exit_code == 0, '${compiler_name}: ${res.output}'
		run := os.execute(os.quoted_path(exe_path))
		assert run.exit_code == 0, '${compiler_name}: ${run.output}'
		assert run.output.trim_space() == 'unicode-path-ok', '${compiler_name}: ${run.output}'
	}
}
