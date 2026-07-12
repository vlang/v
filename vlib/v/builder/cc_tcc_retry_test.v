module builder

import os
import v.pref

fn execute_tcc_retry_test_command(cmd string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	os.unsetenv('VFLAGS')
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
	return path.replace(r'C:\Users\Léo', r'C:\Users\LEO~1')
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
