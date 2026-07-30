module main

import os
import v.pref

fn test_macos_v3_embedded_driver_matches_cross_source_selection() {
	$if cross ? {
		assert !macos_v3_driver_is_available()
	} $else $if macos {
		assert macos_v3_driver_is_available()
	}
}

fn test_macos_v3_driver_source_selection_matches_cross_define() {
	driver_files := ['macos_v3_driver_d_cross.v', 'macos_v3_driver_notd_cross.v']
	native_prefs := &pref.Preferences{}
	native_files :=
		native_prefs.should_compile_filtered_files('cmd/v', driver_files).map(os.base(it))
	assert native_files == ['macos_v3_driver_notd_cross.v']

	cross_prefs := &pref.Preferences{
		compile_defines:     ['cross']
		compile_defines_all: ['cross']
	}
	cross_files := cross_prefs.should_compile_filtered_files('cmd/v', driver_files).map(os.base(it))
	assert cross_files == ['macos_v3_driver_d_cross.v']
}

fn test_macos_v3_relevant_command_only_selects_supported_native_c_builds() {
	$if macos {
		mut prefs := &pref.Preferences{
			path:      'main.v'
			backend:   .c
			os:        .macos
			is_script: true
			gc_mode:   .no_gc
		}
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.old_compiler = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.old_compiler = false
		prefs.coverage_dir = '/tmp/vcovdir'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.coverage_dir = ''
		prefs.show_cc = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.output_mode = .silent
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.output_mode = .stdout
		prefs.show_cc = false
		prefs.is_o = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_o = false
		prefs.is_vlines = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = false
		prefs.gc_mode = .boehm_full_opt
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.gc_set_by_flag = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.gc_mode = .no_gc
		prefs.is_run = true
		assert is_macos_v3_relevant_command('run', prefs)
		prefs.gc_set_by_flag = false
		prefs.is_shared = true
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.is_run = false
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.path = 'script.vsh'
		prefs.is_crun = true
		assert !is_macos_v3_relevant_command('script.vsh', prefs)
		prefs.is_crun = false
		prefs.is_shared = false
		prefs.is_run = true
		prefs.path = 'main.v'
		prefs.os = .linux
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.os = .macos
		prefs.path = 'cmd/v'
		assert !is_macos_v3_relevant_command('cmd/v', prefs)
		prefs.path = 'cmd/tools/vfmt.v'
		assert !is_macos_v3_relevant_command('cmd/tools/vfmt.v', prefs)
		prefs.path = 'main.v'
		prefs.is_cstrict = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_cstrict = false
		prefs.is_test = true
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.is_test = false
		prefs.autofree = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.autofree = false
		prefs.is_prod = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prod = false
		prefs.path = 'version'
		assert !is_macos_v3_relevant_command('version', prefs)
		prefs.path = 'vlib/v3'
		assert is_macos_v3_relevant_command('vlib/v3', prefs)
		prefs.output_cross_c = true
		assert !is_macos_v3_relevant_command('vlib/v3', prefs)
		prefs.output_cross_c = false
		assert is_macos_v3_relevant_command('vlib/v3', prefs)
		prefs.path = 'fixture.vv'
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.path = 'program.txt'
		assert !is_macos_v3_relevant_command('run', prefs)
		assert !is_macos_v3_relevant_command('build', prefs)
		prefs.path = 'script.vsh'
		prefs.is_crun = true
		assert !is_macos_v3_relevant_command('script.vsh', prefs)
		assert !is_macos_v3_relevant_command('crun', prefs)
		prefs.is_crun = false
		prefs.path = 'main.v'
		prefs.out_name_is_dir = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.out_name_is_dir = false
		for path in ['foo.c.v', 'foo.js.v', 'foo.wasm.v', '.v'] {
			prefs.path = path
			assert !is_macos_v3_relevant_command(path, prefs)
		}
		root := os.join_path(os.vtmp_dir(), 'macos_v3_symlink_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'source.v')
		alias := os.join_path(root, 'alias.v')
		os.write_file(source, 'fn main() {}\n') or { panic(err) }
		os.symlink(source, alias) or { panic(err) }
		prefs.path = alias
		assert !is_macos_v3_relevant_command(alias, prefs)
	}
}

fn test_macos_v3_environment_flags_require_compatibility_compiler() {
	$if macos {
		assert macos_v3_environment_flags_are_supported('', '')
		assert !macos_v3_environment_flags_are_supported('-DMACOS_V3_CFLAGS', '')
		assert !macos_v3_environment_flags_are_supported('', '-framework Cocoa')
	}
}

fn test_macos_v3_forwards_environment_driven_skip_running() {
	$if macos {
		prefs := &pref.Preferences{
			skip_running: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['script.vsh'])
		assert '-skip-running' in forwarded
		assert forwarded.count(it == '-skip-running') == 1
		already_explicit := macos_v3_forwarded_args(prefs, ['-skip-running', 'script.vsh'])
		assert already_explicit.count(it == '-skip-running') == 1
	}
}

fn test_macos_v3_forwards_showcc_with_quiet_benchmarks() {
	$if macos {
		prefs := &pref.Preferences{
			show_cc: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['-showcc', 'main.v'])
		assert '-silent' in forwarded
		assert '-showcc' in forwarded
	}
}

fn test_macos_v3_forwards_compatibility_c99_mode() {
	$if macos {
		prefs := &pref.Preferences{}
		forwarded := macos_v3_forwarded_args(prefs, ['main.v'])
		assert macos_v3_compat_c99_flag in forwarded
		assert '-nocache' in forwarded
		assert '-no-memory-limit' in forwarded
		assert '-no-parallel' !in forwarded
		assert forwarded.count(it == macos_v3_compat_c99_flag) == 1
		already_present := macos_v3_forwarded_args(prefs, [macos_v3_compat_c99_flag, 'main.v'])
		assert already_present.count(it == macos_v3_compat_c99_flag) == 1
		assert already_present.count(it == '-nocache') == 1
		assert already_present.count(it == '-no-memory-limit') == 1
		explicit_memory_limit := macos_v3_forwarded_args(prefs, ['--no-memory-limit', 'main.v'])
		assert explicit_memory_limit.count(it in ['-no-memory-limit', '--no-memory-limit']) == 1
	}
}

fn test_macos_v3_args_only_accept_options_implemented_by_v3() {
	$if macos {
		assert macos_v3_args_are_supported(['main.v'])
		assert macos_v3_args_are_supported(['-keepc', '-o', 'main', 'build', 'main.v'])
		assert macos_v3_args_are_supported(['-o', 'new.c', 'cmd/excel'])
		assert macos_v3_args_are_supported(['-d', 'spaced_define', 'main.v'])
		assert macos_v3_args_are_supported(['-dcompact_define', 'main.v'])
		assert !macos_v3_args_are_supported(['-d', 'spaced_value=enabled', 'main.v'])
		assert !macos_v3_args_are_supported(['-dcompact_value=enabled', 'main.v'])
		assert macos_v3_args_are_supported(['run', 'main.v', '--program-option'])
		assert macos_v3_args_are_supported(['script.vsh', '--script-option'])
		assert !macos_v3_args_are_supported(['-ldflags', '-framework Cocoa', 'main.v'])
		assert !macos_v3_args_are_supported(['-path', '@vlib', 'main.v'])
		assert !macos_v3_args_are_supported(['-cc', 'clang', 'main.v'])
		assert !macos_v3_args_are_supported(['-show-c-output', 'main.v'])
		assert !macos_v3_args_are_supported(['-output', 'main', 'main.v'])
		assert !macos_v3_args_are_supported(['-o', '-', 'main.v'])
		assert !macos_v3_args_are_supported(['-o', '-foo', 'main.v'])
		assert !macos_v3_args_are_supported(['-g', 'main.v'])
		assert macos_v3_args_are_supported(['-cg', 'main.v'])
		for arch in ['x86', 'rv32', 'riscv32', 'sparc64', 'ppc', 'ppc32', 'powerpc', 'js', 'js_node',
			'js_browser', 'js_freestanding'] {
			assert !macos_v3_args_are_supported(['-arch', arch, 'main.v'])
		}
		for arch in ['amd64', 'x86_64', 'x64', 'arm64', 'aarch64', 'arm32', 'aarch32', 'arm', 'rv64',
			'riscv64', 'risc-v64', 'riscv', 'risc-v', 'i386', 'x86_32', 'x32', 'IA-32', 'ia-32',
			'ia32', 's390x', 'ppc64le', 'loongarch64', 'ppc64', 'wasm32', 'wasm'] {
			assert macos_v3_args_are_supported(['-arch', arch, 'main.v'])
		}
		assert macos_v3_args_are_supported(['-no-memory-limit', 'main.v'])
		assert macos_v3_args_are_supported(['--no-memory-limit', 'main.v'])
		assert !macos_v3_args_are_supported(['-no-retry-compilation', 'main.v'])
		assert !macos_v3_args_are_supported(['-silent', 'main.v'])
		assert !macos_v3_args_are_supported(['-w', 'main.v'])
		for named_d_flag in ['-debug', '-debug-tcc', '-define', '-disable-explicit-mutability',
			'-div-by-zero-is-zero', '-dump-c-flags', '-dump-modules', '-dump-files', '-dump-defines'] {
			assert !macos_v3_args_are_supported([named_d_flag, 'main.v'])
		}
		for help_flag in ['-?', '-h', '-help', '--help'] {
			assert !macos_v3_args_are_supported(['-gc', 'none', help_flag, 'main.v'])
		}
	}
}

fn test_macos_v3_child_environment_forwards_compiler_hashes() {
	$if macos {
		caller_environment := {
			'PATH':   '/usr/bin'
			'VEXE':   'caller-vexe'
			'VCHILD': 'caller-vchild'
		}
		environment := macos_v3_child_environment(@VEXE, '/tmp/macos_v3_fallback',
			caller_environment)
		assert environment[macos_v3_vhash_env] == @VHASH
		assert environment[macos_v3_vcurrent_hash_env] == @VCURRENTHASH
		assert environment[macos_v3_c_error_dir_env] == '/tmp/macos_v3_fallback.c_error'
		assert environment[macos_v3_embedded_env] == '1'
		assert environment['VEXE'] == os.real_path(@VEXE)
		assert environment['VCHILD'] == 'true'
		assert environment[macos_v3_caller_vexe_present_env] == '1'
		assert environment[macos_v3_caller_vexe_env] == 'caller-vexe'
		assert environment[macos_v3_caller_vchild_present_env] == '1'
		assert environment[macos_v3_caller_vchild_env] == 'caller-vchild'

		unset_environment := macos_v3_child_environment(@VEXE, '/tmp/macos_v3_fallback', {
			'PATH': '/usr/bin'
		})
		assert unset_environment[macos_v3_caller_vexe_present_env] == '0'
		assert unset_environment[macos_v3_caller_vexe_env] == ''
		assert unset_environment[macos_v3_caller_vchild_present_env] == '0'
		assert unset_environment[macos_v3_caller_vchild_env] == ''
	}
}

fn test_macos_v3_reads_c_error_fallback_report() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'macos_v3_c_error_report_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, macos_v3_c_error_source_name_file), 'src.c')!
		os.write_file(os.join_path(root, macos_v3_c_error_compiler_file), 'clang')!
		os.write_file(os.join_path(root, macos_v3_c_error_output_file),
			'src.c:2:1: error: generated failure')!
		os.write_file(os.join_path(root, 'src.c'), 'int main(void) { return missing; }\n')!
		report := read_macos_v3_c_error_report(root) or {
			assert false
			return
		}
		assert report.ccompiler == 'clang'
		assert report.c_output.contains('generated failure')
		assert report.c_file == os.join_path(root, 'src.c')
		assert report.report_dir == root
	}
}

fn test_macos_v3_compiler_failures_fall_back_to_old_compiler() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_c_error_retry_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		target := os.join_path(root, 'target.v')
		output := os.join_path(root, 'target')
		asm_body := $if arm64 {
			'asm arm64 {
		mov output, 1
		; +r (output)
	}'
		} $else {
			'asm amd64 {
		mov eax, 1
		mov output, eax
		; =r (output)
		; ; eax
	}'
		}
		os.write_file(target, 'fn main() {
	mut output := 0
	${asm_body}
	assert output == 1
}
')!
		mut environment := os.environ()
		environment['GITHUB_ACTIONS'] = 'true'
		environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-o', output, target])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert !compiler_output.contains('Launching macOS V3 compiler:'), compiler_output
		assert compiler_output.contains('compatibility compiler for inline assembly'), compiler_output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0

		os.rm(output)!
		failing_target := os.join_path(root, 'failing_target.v')
		failing_output := os.join_path(root, 'failing_target')
		os.write_file(failing_target, '#flag -lmacos_v3_missing_library_${os.getpid()}

fn main() {}
')!
		mut failing_process := os.new_process(@VEXE)
		failing_process.set_args(['-gc', 'none', '-o', failing_output, failing_target])
		failing_process.set_environment(environment)
		failing_process.set_redirect_stdio()
		failing_process.run()
		failing_compiler_pid := failing_process.pid
		failing_process.wait()
		failing_output_text := failing_process.stdout_slurp() + failing_process.stderr_slurp()
		failing_exit_code := failing_process.code
		failing_process.close()
		assert failing_exit_code != 0, failing_output_text
		assert !failing_output_text.contains('V3 C compilation failed; retrying with `-old-compiler`.')
		assert failing_output_text.contains('macos_v3_missing_library_')
		failing_report_dir := os.join_path(os.vtmp_dir(),
			'macos_v3_fallback_${failing_compiler_pid}.c_error')
		assert !os.exists(failing_report_dir), 'failed compatibility build left staged report directory: ${failing_report_dir}'
	}
}

fn test_macos_v3_directory_c_output_differs_from_old_compiler() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_directory_${os.getpid()}')
		source_dir := os.join_path(root, 'app')
		v3_output := os.join_path(root, 'new.c')
		old_output := os.join_path(root, 'old.c')
		os.rmdir_all(root) or {}
		os.mkdir_all(source_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(source_dir, 'main.v'), 'fn main() {\n\tprintln("v3")\n}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut v3_process := os.new_process(@VEXE)
		v3_process.set_args(['-v', '-o', v3_output, source_dir])
		v3_process.set_environment(environment)
		v3_process.set_redirect_stdio()
		v3_process.run()
		v3_process.wait()
		v3_build_output := v3_process.stdout_slurp() + v3_process.stderr_slurp()
		v3_exit_code := v3_process.code
		v3_process.close()
		assert v3_exit_code == 0, v3_build_output
		assert v3_build_output.contains('Running macOS V3 compiler in process:'), v3_build_output
		mut old_process := os.new_process(@VEXE)
		old_process.set_args(['-o', old_output, '-old-compiler', source_dir])
		old_process.set_environment(environment)
		old_process.set_redirect_stdio()
		old_process.run()
		old_process.wait()
		old_build_output := old_process.stdout_slurp() + old_process.stderr_slurp()
		old_exit_code := old_process.code
		old_process.close()
		assert old_exit_code == 0, old_build_output
		assert os.read_file(v3_output)! != os.read_file(old_output)!
	}
}

fn test_macos_v3_default_executable_excludes_temporary_self_hosted_compilers() {
	$if macos {
		assert is_macos_v3_default_executable('/tmp/v')
		assert is_macos_v3_default_executable('/tmp/vnew')
		assert !is_macos_v3_default_executable('/tmp/v2')
		assert !is_macos_v3_default_executable('/tmp/vstrict1')
		assert !is_macos_v3_default_executable('/tmp/vp')
	}
}
