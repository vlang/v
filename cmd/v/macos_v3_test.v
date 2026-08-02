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

		// V3 owns supported user compilation modes.
		prefs.coverage_dir = '/tmp/vcovdir'
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_o = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_vlines = false
		prefs.gc_mode = .boehm_full_opt
		prefs.gc_set_by_flag = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.gc_mode = .no_gc
		prefs.gc_set_by_flag = false
		prefs.sanitize = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.sanitize = false
		prefs.is_livemain = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_livemain = false
		prefs.is_liveshared = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_liveshared = false
		prefs.is_prof = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prof = false
		prefs.output_cross_c = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.output_cross_c = false
		prefs.is_apk = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_apk = false
		prefs.json_errors = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.json_errors = false
		prefs.no_preludes = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.no_preludes = false
		prefs.skip_warnings = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.skip_warnings = false
		prefs.print_watched_files = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.print_watched_files = false
		prefs.warn_impure_v = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.warn_impure_v = false
		prefs.test_runner = 'tap'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.test_runner = ''
		prefs.exclude = ['@vlib/math/*.c.v']
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.exclude.clear()
		prefs.ldflags = '-L/custom/lib -lcustom'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.ldflags = ''
		prefs.nofloat = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.nofloat = false
		prefs.fast_math = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.fast_math = false
		prefs.compress = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.compress = false
		prefs.is_bare = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_bare = false
		prefs.build_options << '-m32'
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.build_options.clear()
		prefs.is_run = true
		prefs.autofree = true
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.autofree = false
		assert is_macos_v3_relevant_command('run', prefs)
		prefs.backend = .wasm
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.is_run = false
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .c
		prefs.is_shared = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_cstrict = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prod = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.out_name_is_dir = true
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .js_node
		prefs.os = .linux
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.backend = .c
		prefs.os = .macos

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

fn test_macos_v3_dispatch_requires_effective_no_gc_mode() {
	$if macos {
		implicit_gc, _ := pref.parse_args_and_show_errors([], ['', 'main.v'], false)
		assert implicit_gc.gc_mode == .boehm_full_opt
		assert !implicit_gc.gc_set_by_flag
		assert !is_macos_v3_relevant_command('main.v', implicit_gc)

		explicit_none, _ := pref.parse_args_and_show_errors([], ['', '-gc', 'none', 'main.v'],
			false)
		assert explicit_none.gc_mode == .no_gc
		assert explicit_none.gc_set_by_flag
		assert is_macos_v3_relevant_command('main.v', explicit_none)

		prealloc, _ := pref.parse_args_and_show_errors([], ['', '-prealloc', 'main.v'], false)
		assert prealloc.gc_mode == .no_gc
		assert !prealloc.gc_set_by_flag
		assert is_macos_v3_relevant_command('main.v', prealloc)
	}
}

fn test_macos_v3_detects_v1_only_leading_options() {
	$if macos {
		assert macos_v3_has_v1_only_leading_option(['-message-limit', '0', 'main.v'], 'main.v')
		assert macos_v3_has_v1_only_leading_option(['-message-limit', '5', 'run', 'main.v'], 'run')
		assert !macos_v3_has_v1_only_leading_option(['run', 'main.v', '-message-limit', '5'], 'run')
		assert !macos_v3_has_v1_only_leading_option(['--', '-message-limit', '5', 'main.v'],
			'main.v')
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
		assert macos_v3_internal_quiet_flag in forwarded
		assert '-silent' !in forwarded
		assert '-showcc' in forwarded
		explicit_silent := macos_v3_forwarded_args(prefs, ['-silent', '-showcc', 'main.v'])
		assert '-silent' in explicit_silent
		assert macos_v3_internal_quiet_flag !in explicit_silent
	}
}

fn test_macos_v3_show_c_output_prints_successful_compiler_output() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_show_c_output_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		compiler := os.join_path(root, 'cc')
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(compiler,
			'#!/bin/sh\necho "V3_SHOW_C_OUTPUT_MARKER" >&2\nexec /usr/bin/cc "\$@"\n')!
		os.chmod(compiler, 0o700)!
		os.write_file(source, 'fn main() {}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-nocache', '-show-c-output', '-cc', compiler, '-o',
			output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert compiler_output.contains('Output of the C Compiler'), compiler_output
		assert compiler_output.contains('V3_SHOW_C_OUTPUT_MARKER'), compiler_output
		assert os.is_executable(output)
	}
}

fn test_macos_v3_forwards_compatibility_c99_mode() {
	$if macos {
		mut prefs := &pref.Preferences{}
		forwarded := macos_v3_forwarded_args(prefs, ['main.v'])
		assert macos_v3_compat_c99_flag in forwarded
		assert '-nocache' !in forwarded
		assert '--no-cache' !in forwarded
		assert '-no-memory-limit' in forwarded
		assert '-no-parallel' !in forwarded
		assert forwarded.count(it == macos_v3_compat_c99_flag) == 1
		already_present := macos_v3_forwarded_args(prefs, [macos_v3_compat_c99_flag, 'main.v'])
		assert already_present.count(it == macos_v3_compat_c99_flag) == 1
		assert already_present.count(it in ['-nocache', '--no-cache']) == 0
		assert already_present.count(it == '-no-memory-limit') == 1
		explicit_no_cache := macos_v3_forwarded_args(prefs, ['--no-cache', 'main.v'])
		assert explicit_no_cache.count(it in ['-nocache', '--no-cache']) == 1
		explicit_memory_limit := macos_v3_forwarded_args(prefs, ['--no-memory-limit', 'main.v'])
		assert explicit_memory_limit.count(it in ['-no-memory-limit', '--no-memory-limit']) == 1
	}
}

fn test_autofree_non_direct_commands_stay_on_the_standard_command_path() {
	mut prefs := &pref.Preferences{
		path:   'app.v'
		is_run: true
	}
	assert !is_ownership_relevant_command('run', prefs)
	assert !is_ownership_relevant_command('test', prefs)
	prefs.autofree = true
	assert !is_macos_v3_relevant_command('run', prefs)
	prefs.autofree = false
	prefs.is_run = false
	assert is_ownership_relevant_command('app.v', prefs)
}

fn test_ownership_delegation_is_platform_scoped_and_honors_old_compiler() {
	assert !ownership_delegation_is_requested(false, false, false, 'macos')
	assert ownership_delegation_is_requested(true, false, false, 'linux')
	assert ownership_delegation_is_requested(true, false, false, 'windows')
	assert ownership_delegation_is_requested(true, true, false, 'linux')
	assert ownership_delegation_is_requested(false, true, false, 'macos')
	assert !ownership_delegation_is_requested(false, true, false, 'linux')
	assert !ownership_delegation_is_requested(false, true, false, 'windows')
	assert !ownership_delegation_is_requested(false, true, true, 'macos')
	assert !ownership_delegation_is_requested(true, false, true, 'macos')
}

fn test_macos_v3_manualfree_overrides_vflags_autofree() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_manualfree_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main')
		os.write_file(source,
			"\$if autofree {\n\t\$compile_error('autofree remained enabled')\n}\n\nfn main() {}\n")!
		mut environment := os.environ()
		environment['VFLAGS'] = '-autofree'
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-manualfree', '-gc', 'none', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert os.is_executable(output)
	}
}

fn test_autofree_delegation_detects_and_forwards_vflags() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_autofree_vflags_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		output := os.join_path(root, 'main.c')
		os.write_file(source,
			"\$if ownership_vflags_feature ? {\n} \$else {\n\t\$compile_error('VFLAGS define was not forwarded')\n}\n\nfn main() {}\n")!
		mut environment := os.environ()
		environment['VFLAGS'] = '-autofree -d ownership_vflags_feature'
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', '-o', output, source])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Launching v3_ownership:'), compiler_output
		assert !compiler_output.contains('ownership support is not compiled into this v3 executable'), compiler_output

		assert os.is_file(output)
	}
}

fn test_macos_v3_child_environment_forwards_compiler_hashes() {
	$if macos {
		caller_environment := {
			'PATH':                     '/usr/bin'
			'CFLAGS':                   '-I/caller/include -DCALLER_FLAG=1'
			'LDFLAGS':                  '-L/caller/lib -lcaller'
			'VEXE':                     'caller-vexe'
			'VCHILD':                   'caller-vchild'
			'V_MACOS_V3_FALLBACK_FILE': '/tmp/stale-fallback'
			'V_MACOS_V3_C_ERROR_DIR':   '/tmp/stale-c-error'
			'V_MACOS_V3_RETRY':         '1'
		}
		environment := macos_v3_child_environment(@VEXE, '/tmp/macos_v3_fallback',
			caller_environment)
		assert environment[macos_v3_vhash_env] == @VHASH
		assert environment[macos_v3_vcurrent_hash_env] == @VCURRENTHASH
		assert environment[macos_v3_c_error_dir_env] == '/tmp/macos_v3_fallback.c_error'
		assert macos_v3_retry_env !in environment
		assert environment[macos_v3_embedded_env] == '1'
		assert environment['VEXE'] == os.real_path(@VEXE)
		assert environment['VCHILD'] == 'true'
		assert environment['CFLAGS'] == '-I/caller/include -DCALLER_FLAG=1'
		assert environment['LDFLAGS'] == '-L/caller/lib -lcaller'
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

fn test_macos_v3_embedded_driver_reuses_module_cache() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_embedded_cache_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(os.join_path(root, 'wrapper')) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		main_file := os.join_path(root, 'main.v')
		os.write_file(os.join_path(root, 'wrapper', 'wrapper.v'), 'module wrapper

pub fn value() int {
	return 42
}
')!
		os.write_file(main_file, 'module main

import wrapper

fn main() {
	println(wrapper.value())
}
')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		environment['V3CACHE'] = os.join_path(root, 'cache')
		environment['V3_CACHE_TRACE'] = '1'
		mut outputs := []string{}
		for name in ['first', 'second'] {
			output := os.join_path(root, name)
			mut process := os.new_process(@VEXE)
			process.set_args(['-gc', 'none', '-o', output, main_file])
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			process.wait()
			compiler_output := process.stdout_slurp() + process.stderr_slurp()
			exit_code := process.code
			process.close()
			assert exit_code == 0, compiler_output
			assert os.is_executable(output)
			outputs << compiler_output
		}
		assert outputs[0].contains('V3 module cache miss:'), outputs[0]
		assert !outputs[1].contains('V3 module cache miss:'), outputs[1]
		cache_headers := os.walk_ext(environment['V3CACHE'], '.vh')
		assert cache_headers.any(os.base(it).starts_with('wrapper_')), cache_headers.str()
		run := os.execute(os.quoted_path(os.join_path(root, 'second')))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '42'
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
		process.set_args(['-v', '-gc', 'none', '-o', output, target])
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

fn test_macos_v3_test_command_uses_v3() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()), 'macos_v3_test_command_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		test_file := os.join_path(root, 'sample_test.v')
		os.write_file(test_file, 'fn test_v3_default() {\n\tassert 2 + 2 == 4\n}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', 'test', test_file])
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
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
		v3_process.set_args(['-v', '-gc', 'none', '-o', v3_output, source_dir])
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

fn test_macos_v3_directory_default_output_is_source_adjacent() {
	$if macos {
		root := os.join_path(os.real_path(os.vtmp_dir()),
			'macos_v3_directory_output_${os.getpid()}')
		source_dir := os.join_path(root, 'app')
		caller_dir := os.join_path(root, 'caller')
		expected_output := os.join_path(source_dir, 'app')
		wrong_output := os.join_path(caller_dir, 'app')
		os.rmdir_all(root) or {}
		os.mkdir_all(source_dir) or { panic(err) }
		os.mkdir_all(caller_dir) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(source_dir, 'main.v'), 'fn main() {}\n')!
		mut environment := os.environ()
		environment['CFLAGS'] = ''
		environment['LDFLAGS'] = ''
		environment['VFLAGS'] = ''
		environment['VOSARGS'] = ''
		mut process := os.new_process(@VEXE)
		process.set_args(['-v', '-gc', 'none', source_dir])
		process.set_environment(environment)
		process.set_work_folder(caller_dir)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		compiler_output := process.stdout_slurp() + process.stderr_slurp()
		exit_code := process.code
		process.close()
		assert exit_code == 0, compiler_output
		assert compiler_output.contains('Running macOS V3 compiler in process:'), compiler_output
		assert os.is_executable(expected_output)
		assert !os.exists(wrong_output)
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
