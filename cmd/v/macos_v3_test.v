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

fn test_macos_v3_relevant_command_selects_user_compilation_and_tests() {
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

		prefs.path = ''
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.path = 'main.v'

		// V3 owns user compilation modes even when it will reject an option with
		// its own diagnostic. Unsupported modes no longer silently run V1.
		prefs.coverage_dir = '/tmp/vcovdir'
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_o = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.gc_mode = .boehm_full_opt
		prefs.gc_set_by_flag = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_shared = true
		assert is_macos_v3_relevant_command('run', prefs)
		prefs.is_cstrict = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.autofree = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prod = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.out_name_is_dir = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .js_node
		prefs.os = .linux
		assert is_macos_v3_relevant_command('main.v', prefs)

		prefs.path = 'vlib/v3'
		prefs.is_test = true
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.path = 'vlib/v3/tests/review_transform_regressions_test.v'
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.path = 'program.txt'
		assert is_macos_v3_relevant_command('run', prefs)
		assert is_macos_v3_relevant_command('build', prefs)
		prefs.path = 'script.vsh'
		assert is_macos_v3_relevant_command('script.vsh', prefs)
		assert !is_macos_v3_relevant_command('crun', prefs)
		for path in ['foo.c.v', 'foo.js.v', 'foo.wasm.v', '.v', 'fixture.vv'] {
			prefs.path = path
			assert is_macos_v3_relevant_command(path, prefs)
		}

		prefs.path = 'cmd/v'
		assert !is_macos_v3_relevant_command('cmd/v', prefs)
		prefs.path = 'cmd/v/macos_v3_test.v'
		assert !is_macos_v3_relevant_command(prefs.path, prefs)
		prefs.path = 'cmd/tools/vfmt.v'
		assert is_macos_v3_relevant_command('cmd/tools/vfmt.v', prefs) == (os.getenv('VCHILD') != 'true')
		assert is_macos_v3_internal_tool_bootstrap('cmd/tools/vfmt.v', true)
		assert !is_macos_v3_internal_tool_bootstrap('cmd/tools/vfmt.v', false)
		prefs.path = 'vlib/v3/v3.v'
		assert !is_macos_v3_relevant_command(prefs.path, prefs)
		prefs.path = 'version'
		assert !is_macos_v3_relevant_command('version', prefs)
	}
}

fn test_macos_v3_forwards_driver_defaults_once() {
	$if macos {
		mut prefs := &pref.Preferences{
			skip_running: true
		}
		forwarded := macos_v3_forwarded_args(prefs, ['-showcc', 'script.vsh'])
		for flag in [macos_v3_compat_c99_flag, '-skip-running', '-silent', '-nocache',
			'-no-memory-limit', '-showcc'] {
			assert flag in forwarded
			assert forwarded.count(it == flag) == 1
		}
		already_explicit := macos_v3_forwarded_args(prefs, ['--no-cache', '--no-memory-limit',
			'--no-parallel', '-silent', '-skip-running', macos_v3_compat_c99_flag, 'script.vsh'])
		assert already_explicit.count(it in ['-nocache', '--no-cache']) == 1
		assert already_explicit.count(it in ['-no-memory-limit', '--no-memory-limit']) == 1
		assert already_explicit.count(it == '-silent') == 1
		assert already_explicit.count(it == '-skip-running') == 1
		assert already_explicit.count(it == macos_v3_compat_c99_flag) == 1
	}
}

fn test_macos_v3_child_environment_preserves_caller_without_fallback_state() {
	$if macos {
		caller_environment := {
			'PATH':                     '/usr/bin'
			'VEXE':                     'caller-vexe'
			'VCHILD':                   'caller-vchild'
			'V_MACOS_V3_FALLBACK_FILE': '/tmp/stale-fallback'
			'V_MACOS_V3_C_ERROR_DIR':   '/tmp/stale-c-error'
			'V_MACOS_V3_RETRY':         '1'
		}
		environment := macos_v3_child_environment(@VEXE, caller_environment)
		assert environment[macos_v3_vhash_env] == @VHASH
		assert environment[macos_v3_vcurrent_hash_env] == @VCURRENTHASH
		assert environment[macos_v3_embedded_env] == '1'
		assert environment['VEXE'] == os.real_path(@VEXE)
		assert environment['VCHILD'] == 'true'
		assert environment[macos_v3_caller_vexe_present_env] == '1'
		assert environment[macos_v3_caller_vexe_env] == 'caller-vexe'
		assert environment[macos_v3_caller_vchild_present_env] == '1'
		assert environment[macos_v3_caller_vchild_env] == 'caller-vchild'
		assert 'V_MACOS_V3_FALLBACK_FILE' !in environment
		assert 'V_MACOS_V3_C_ERROR_DIR' !in environment
		assert 'V_MACOS_V3_RETRY' !in environment

		unset_environment := macos_v3_child_environment(@VEXE, {
			'PATH': '/usr/bin'
		})
		assert unset_environment[macos_v3_caller_vexe_present_env] == '0'
		assert unset_environment[macos_v3_caller_vexe_env] == ''
		assert unset_environment[macos_v3_caller_vchild_present_env] == '0'
		assert unset_environment[macos_v3_caller_vchild_env] == ''
	}
}

fn test_macos_v3_compiler_failures_do_not_fall_back_to_v1() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_no_fallback_${os.getpid()}')
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
		mut environment := clean_macos_v3_test_environment()
		compile := run_macos_v3_process(['-v', '-o', output, target], environment)
		assert compile.exit_code != 0, compile.output
		assert compile.output.contains('Running macOS V3 compiler in process:'), compile.output
		assert compile.output.contains('inline assembly is not supported'), compile.output
		assert !compile.output.contains('retrying with `-old-compiler`'), compile.output
		assert !os.exists(output)

		failing_target := os.join_path(root, 'failing_target.v')
		failing_output := os.join_path(root, 'failing_target')
		os.write_file(failing_target, '#flag -lmacos_v3_missing_library_${os.getpid()}

fn main() {}
')!
		c_failure := run_macos_v3_process(['-o', failing_output, failing_target], environment)
		assert c_failure.exit_code != 0, c_failure.output
		assert c_failure.output.contains('C compilation failed'), c_failure.output
		assert !c_failure.output.contains('retrying with `-old-compiler`'), c_failure.output
	}
}

fn test_macos_v3_test_command_uses_v3() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_test_command_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		test_file := os.join_path(root, 'sample_test.v')
		os.write_file(test_file, 'fn test_v3_default() {
	assert 2 + 2 == 4
}
')!
		result := run_macos_v3_process(['-v', 'test', test_file], clean_macos_v3_test_environment())
		assert result.exit_code == 0, result.output
		assert result.output.contains('Running macOS V3 compiler in process:'), result.output
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
		environment := clean_macos_v3_test_environment()
		v3_build := run_macos_v3_process(['-v', '-o', v3_output, source_dir], environment)
		assert v3_build.exit_code == 0, v3_build.output
		assert v3_build.output.contains('Running macOS V3 compiler in process:'), v3_build.output
		old_build := run_macos_v3_process(['-o', old_output, '-old-compiler', source_dir],
			environment)
		assert old_build.exit_code == 0, old_build.output
		assert os.read_file(v3_output)! != os.read_file(old_output)!
	}
}

fn clean_macos_v3_test_environment() map[string]string {
	mut environment := os.environ()
	environment['CFLAGS'] = ''
	environment['LDFLAGS'] = ''
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
	return environment
}

fn run_macos_v3_process(args []string, environment map[string]string) os.Result {
	mut process := os.new_process(@VEXE)
	process.set_args(args)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	result := os.Result{
		exit_code: process.code
		output:    output
	}
	process.close()
	return result
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
