module main

import os
import v.pref

const macos_v3_test_vroot = os.dir(@VEXE)

fn run_macos_v3_test_process(executable string, args []string, work_dir string, overrides map[string]string) os.Result {
	mut environment := os.environ()
	environment['VFLAGS'] = ''
	environment['VOSARGS'] = ''
	for name, value in overrides {
		environment[name] = value
	}
	mut process := os.new_process(executable)
	process.set_args(args)
	process.set_work_folder(work_dir)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	result := os.Result{
		exit_code: process.code
		output: output
	}
	process.close()
	return result
}

fn test_macos_v3_embedded_driver_matches_target_selection() {
	$if cross ? {
		assert !macos_v3_driver_is_available()
	} $else $if musl ? {
		assert !macos_v3_driver_is_available()
	} $else $if macos || linux {
		assert macos_v3_driver_is_available()
	} $else {
		assert !macos_v3_driver_is_available()
	}
}

fn test_macos_v3_driver_source_selection_matches_cross_define() {
	driver_files := ['macos_v3_driver_d_cross.v', 'macos_v3_driver_notd_cross.v']
	native_prefs := &pref.Preferences{}
	native_files :=
		native_prefs.should_compile_filtered_files('cmd/v', driver_files).map(os.base(it))
	assert native_files == ['macos_v3_driver_notd_cross.v']

	cross_prefs := &pref.Preferences{
		compile_defines: ['cross']
		compile_defines_all: ['cross']
	}
	cross_files :=
		cross_prefs.should_compile_filtered_files('cmd/v', driver_files).map(os.base(it))
	assert cross_files == ['macos_v3_driver_d_cross.v']
}

fn test_macos_v3_relevant_command_owns_every_direct_c_build() {
	mut prefs := &pref.Preferences{
		path: 'main.v'
		backend: .c
	}
	for command in ['main.v', 'build', 'run'] {
		assert is_macos_v3_relevant_command(command, prefs)
	}
	for path in ['cmd/v', 'cmd/v/v.v', 'vlib/v3/v3.v', 'vlib/v/compiler_errors_test.v', 'fixture.vv'] {
		prefs.path = path
		assert is_macos_v3_relevant_command(path, prefs)
	}
	prefs.old_compiler = true
	assert is_macos_v3_relevant_command('cmd/v', prefs)
	prefs.old_compiler = false
	prefs.path = ''
	assert !is_macos_v3_relevant_command('build', prefs)
	prefs.path = 'main.v'
	prefs.backend = .js_node
	assert !is_macos_v3_relevant_command('main.v', prefs)
	prefs.backend = .c
	for command in ['test', 'fmt', 'version', 'crun', 'build-module'] {
		assert !is_macos_v3_relevant_command(command, prefs)
	}
}

fn test_macos_v3_cmd_source_unlinks_v1_on_supported_hosts() {
	source := os.read_file(os.join_path(macos_v3_test_vroot, 'cmd', 'v', 'v.v'))!
	assert source.contains('\$if v1_fallback ? {')
	assert source.contains('} \$else \$if !macos && !linux {')
	assert source.contains('import v.builder')
	assert source.contains('import v.builder.cbuilder')
	assert source.contains('\$if v1_fallback ? {\n\t\t\tbuilder.compile')
	driver := os.read_file(os.join_path(macos_v3_test_vroot, 'cmd', 'v',
		'macos_v3_driver_notd_cross.v'))!
	assert driver.contains('\$if v1_fallback ? {')
	assert driver.contains('fn macos_v3_driver_is_available() bool')
}

fn test_macos_v3_old_compiler_uses_external_v1_command() {
	$if macos || linux {
		fallback := os.join_path(macos_v3_test_vroot, macos_v3_v1_fallback_binary)
		if !os.is_executable(fallback) {
			return
		}
		root := os.join_path(os.vtmp_dir(), 'v3_external_old_compiler_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() { println("v1") }\n')!
		output := os.join_path(root, 'main')
		result := run_macos_v3_test_process(@VEXE, ['-old-compiler', '-o', output, source],
			macos_v3_test_vroot, {})
		assert result.exit_code == 0, result.output
		assert os.is_executable(output)
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'v1', run.output
	}
}

fn test_macos_v3_invalid_program_still_reports_an_error_after_v1_retry() {
	$if macos || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_invalid_program_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() { missing_name() }\n')!
		result := run_macos_v3_test_process(@VEXE, ['-o', os.join_path(root, 'main'), source], macos_v3_test_vroot, {
			'V_MACOS_V3_FALLBACK_FILE': os.join_path(root, 'stale_fallback')
			'V_MACOS_V3_C_ERROR_DIR':   os.join_path(root, 'stale_report')
			'V_MACOS_V3_RETRY':         '1'
		})
		assert result.exit_code == 1, result.output
		assert result.output.contains('missing_name'), result.output
		assert !os.exists(os.join_path(root, 'stale_fallback'))
	}
}

fn test_macos_v3_fatal_errors_reports_only_the_first_error() {
	$if macos || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_fatal_errors_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() {\n\tmissing_first()\n\tmissing_second()\n}\n')!
		result := run_macos_v3_test_process(@VEXE, ['-Wfatal-errors', '-o',
			os.join_path(root, 'main'), source], macos_v3_test_vroot, {})
		assert result.exit_code == 1, result.output
		assert result.output.contains('missing_first'), result.output
		assert !result.output.contains('unknown function `missing_second`'), result.output
	}
}

fn test_macos_v3_forwarded_args_strip_only_compiler_selection() {
	prefs := &pref.Preferences{}
	forwarded := macos_v3_forwarded_args(prefs, ['-new-compiler', 'main.v'])
	assert '-new-compiler' !in forwarded
	assert 'main.v' in forwarded
	assert macos_v3_compat_c99_flag in forwarded

	run_prefs := &pref.Preferences{
		is_run: true
		run_args: ['-new-compiler']
	}
	run_forwarded := macos_v3_forwarded_args(run_prefs, [
		'-new-compiler',
		'run',
		'main.v',
		'-new-compiler',
	])
	assert run_forwarded.count(it == '-new-compiler') == 1
	assert run_forwarded.last() == '-new-compiler'
}

fn test_macos_v3_forwarded_args_preserve_compatibility_aliases() {
	mut arch_prefs := &pref.Preferences{
		arch: .amd64
	}
	arch_prefs.build_options << '-arch x86'
	arch_args := macos_v3_forwarded_args(arch_prefs, ['-arch', 'x86', 'main.v'])
	arch_index := arch_args.index('-arch')
	assert arch_index >= 0
	assert arch_args[arch_index + 1] == 'amd64'

	global_args := macos_v3_forwarded_args(&pref.Preferences{
		enable_globals: true
	}, ['--enable-globals', 'main.v'])
	assert '-enable-globals' in global_args
	assert '--enable-globals' !in global_args

	skip_args := macos_v3_forwarded_args(&pref.Preferences{
		skip_running: true
	}, ['main.v'])
	assert skip_args.count(it == '-skip-running') == 1
}

fn test_macos_v3_child_environment_preserves_the_original_caller() {
	caller_environment := {
		'PATH':                     '/usr/bin'
		'VEXE':                     'caller-vexe'
		'VCHILD':                   'caller-vchild'
		'V_MACOS_V3_NO_FALLBACK':   'caller-no-fallback'
		'V_MACOS_V3_FALLBACK_FILE': '/tmp/stale-fallback'
		'V_MACOS_V3_C_ERROR_DIR':   '/tmp/stale-c-error'
		'V_MACOS_V3_RETRY':         '1'
	}
	environment := macos_v3_child_environment(@VEXE, caller_environment, caller_environment)
	assert environment['VEXE'] == os.real_path(@VEXE)
	assert environment['VCHILD'] == 'true'
	assert environment[macos_v3_vhash_env] == @VHASH
	assert environment[macos_v3_vcurrent_hash_env] == @VCURRENTHASH
	assert environment[macos_v3_embedded_env] == '1'
	assert macos_v3_fallback_file_env !in environment
	assert macos_v3_c_error_dir_env !in environment
	assert macos_v3_retry_env !in environment

	restored := macos_v3_original_caller_environment(environment)
	assert restored['PATH'] == '/usr/bin'
	assert restored['VEXE'] == 'caller-vexe'
	assert restored['VCHILD'] == 'caller-vchild'
	assert restored[macos_v3_no_fallback_env] == 'caller-no-fallback'
	assert macos_v3_embedded_env !in restored
	assert macos_v3_caller_vexe_env !in restored
	assert macos_v3_caller_vchild_env !in restored
}

fn test_macos_v3_fastc_rejects_incompatible_gc() {
	fastc_boehm, _ := pref.parse_args_and_show_errors([], ['-b', 'fastc', '-gc', 'boehm', 'main.v'], false)
	message := macos_v3_fastc_incompatibility(fastc_boehm) or {
		assert false, 'expected explicit FastC with Boehm GC to be rejected'
		return
	}
	assert message.contains('`-b fastc` only supports `-gc none`')

	overridden, _ := pref.parse_args_and_show_errors([], ['-b', 'fastc', '-gc', 'boehm', '-b', 'c',
		'main.v'], false)
	assert macos_v3_fastc_incompatibility(overridden) == none
}

fn test_macos_v3_ownership_delegation_never_selects_v1() {
	assert ownership_delegation_is_requested(true, false, false, false, 'linux')
	assert ownership_delegation_is_requested(false, true, false, false, 'macos')
	assert ownership_delegation_is_requested(false, true, false, false, 'linux')
	assert !ownership_delegation_is_requested(false, true, false, false, 'windows')
	assert !ownership_delegation_is_requested(true, false, true, false, 'macos')
	direct_prefs := &pref.Preferences{
		path: 'main.v'
	}
	assert is_ownership_relevant_command('main.v', direct_prefs)
	run_prefs := &pref.Preferences{
		path: 'main.v'
		is_run: true
	}
	assert is_ownership_relevant_command('run', run_prefs)
	assert !is_ownership_relevant_command('test', run_prefs)
	assert macos_v3_explicit_autofree_is_unsupported(&pref.Preferences{
		new_compiler: true
		autofree: true
		path: 'main.v'
	})
}

fn test_macos_v3_autofree_direct_and_run_use_ownership_compiler() {
	$if macos || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_autofree_run_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, "fn main() { println('autofree run') }\n")!
		output := os.join_path(root, 'direct')
		direct := run_macos_v3_test_process(@VEXE, ['-autofree', '-o', output, source], macos_v3_test_vroot, {})
		assert direct.exit_code == 0, direct.output
		direct_run := run_macos_v3_test_process(output, [], macos_v3_test_vroot, {})
		assert direct_run.exit_code == 0, direct_run.output
		assert direct_run.output.trim_space() == 'autofree run', direct_run.output
		result := run_macos_v3_test_process(@VEXE, ['-autofree', 'run', source], macos_v3_test_vroot, {})
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'autofree run', result.output
	}
}

fn test_macos_v3_parallel_cc_ignores_inactive_header_definitions() {
	$if macos || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_inactive_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		os.write_file(os.join_path(root, 'inactive_impl.h'), 'int inactive_impl(void) { return 1; }\n')!
		source := os.join_path(root, 'main.v')
		os.write_file(source, '\$if windows {\n#include "@DIR/inactive_impl.h"\n}\n\nfn main() { println("ok") }\n')!
		output := os.join_path(root, 'main')
		result := run_macos_v3_test_process(@VEXE, ['-gc', 'none', '-parallel-cc', '-nocache', '-o',
			output, source], macos_v3_test_vroot, {})
		assert result.exit_code == 0, result.output
		assert !result.output.contains('failed to link after parallel C compilation'), result.output
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'ok'
	}
}

fn test_macos_v3_compiles_cmd_v_without_v1_modules() {
	$if macos || linux {
		compiler := os.join_path(macos_v3_test_vroot, '.v3_only_cmd_test_${os.getpid()}')
		defer {
			os.rm(compiler) or {}
		}
		result := run_macos_v3_test_process(@VEXE, ['-no-memory-limit', '-no-parallel', '-o',
			compiler, 'cmd/v'], macos_v3_test_vroot, {})
		assert result.exit_code == 0, result.output
		assert os.is_executable(compiler)
		version := run_macos_v3_test_process(compiler, ['version'], macos_v3_test_vroot, {})
		assert version.exit_code == 0, version.output
		assert version.output.starts_with('V '), version.output
	}
}
