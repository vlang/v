module main

import os
import v.cmdexec

const compiler_error_probe_env = 'VTEST_COMPILER_ERROR_FALLBACK_ROOT'
const compiler_error_probe_message = 'sample.vsh:2: error: unknown function `missing_v3_diagnostic`'

// The copied test executable acts as the launcher and the diagnostic compiler.
// A local compatibility stub avoids downloads and automatic bug-report uploads.
fn testsuite_begin() {
	root := os.getenv(compiler_error_probe_env)
	if root == '' {
		return
	}
	if os.getenv(v3_no_fallback_env) == '1' {
		assert os.args[1..] == ['run', os.join_path(root, 'sample.vsh'), 'ci']
		assert os.getenv(v3_retry_env) == '1'
		assert os.getenv('VNORUN') == '1'
		assert os.getenv('VFLAGS') == ''
		assert os.getenv(v3_fallback_file_env) == ''
		assert os.getenv(v3_c_error_dir_env) == ''
		assert !os.exists(os.join_path(root, 'ran'))
		assert !os.exists(os.join_path(root, 'replayed'))
		os.write_file(os.join_path(root, 'replayed'), 'yes') or { panic(err) }
		eprint(compiler_error_probe_message)
		exit(1)
	}
	state := RetryState{
		fallback_file: os.join_path(root, 'request')
		c_error_dir:   os.join_path(root, 'c_error')
		args:          os.args[1..].clone()
	}
	launch_v1(state.args, 'V compilation failed (compiler_error)', state)
}

fn test_compiler_error_is_printed_before_successful_or_unsuccessful_fallback() {
	$if windows {
		return
	}
	previous := os.environ()
	names := [compiler_error_probe_env, v3_no_fallback_env, v3_retry_env, v3_fallback_file_env,
		v3_c_error_dir_env, 'VNORUN', 'VFLAGS', 'V_C_ERROR_BUG_REPORT_DISABLED']
	defer {
		for name in names {
			if name in previous {
				os.setenv(name, previous[name], true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	for status in [0, 23] {
		root := os.join_path(os.vtmp_dir(), 'v3_compiler_error_launch_${os.getpid()}_${status}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		launcher := os.join_path(root, 'v')
		os.cp(os.executable(), launcher)!
		os.chmod(launcher, 0o700)!
		os.mkdir_all(os.join_path(root, 'vlib', 'v'))!
		os.write_file(os.join_path(root, 'GNUmakefile'), '')!
		os.write_file(os.join_path(root, 'sample.vsh'), 'missing_v3_diagnostic()\n')!
		compat := os.join_path(root, 'compat')
		os.mkdir_all(os.join_path(compat, 'vlib', 'crypto', 'subtle'))!
		for name in ['aliasing.v', 'comparison.v'] {
			os.write_file(os.join_path(compat, 'vlib', 'crypto', 'subtle', name), '')!
		}
		for name in v1_fallback_compatibility_modules {
			module_dir := os.join_path(compat, 'vlib', name)
			os.mkdir_all(module_dir)!
			os.write_file(os.join_path(module_dir, '${name}.v'), '')!
			os.write_file(os.join_path(module_dir, v1_fallback_compatibility_marker), v_version)!
		}
		stub := '#!/bin/sh\nif [ "\$1" = version ]; then echo "V ${v_version} probe"; exit 0; fi\n'
			+ 'test -z "\$VNORUN" || exit 91\n'
			+ 'test "\$VFLAGS" = "already merged" || exit 92\n'
			+ 'test -z "\$V_MACOS_V3_FALLBACK_FILE" || exit 93\n'
			+ 'test -z "\$V_MACOS_V3_C_ERROR_DIR" || exit 94\n'
			+ 'test "\$1" = run && test "\$3" = ci || exit 95\n'
			+ 'printf "ran\\n" >> "\$${compiler_error_probe_env}/ran"\n'
			+ 'echo "compatibility program ran" >&2\nexit ${status}\n'
		for path in [os.join_path(compat, 'v'), os.join_path(root, v1_fallback_binary)] {
			os.write_file(path, stub)!
			os.chmod(path, 0o700)!
		}
		os.write_file(os.join_path(root, v1_fallback_binary + '.vroot'), compat)!
		request := os.join_path(root, 'request')
		c_error_dir := os.join_path(root, 'c_error')
		os.write_file(request, 'compiler_error\nsemantic checking')!
		os.mkdir_all(c_error_dir)!
		os.setenv(compiler_error_probe_env, root, true)
		os.unsetenv(v3_no_fallback_env)
		os.setenv(v3_retry_env, '1', true)
		os.setenv(v3_fallback_file_env, request, true)
		os.setenv(v3_c_error_dir_env, c_error_dir, true)
		os.unsetenv('VNORUN')
		os.setenv('VFLAGS', 'already merged', true)
		os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
		result := cmdexec.run_with_timeout(launcher, ['run', os.join_path(root, 'sample.vsh'),
			'ci'], 15_000)
		assert result.exit_code == status, result.output
		assert result.output.starts_with('Compiler output from the default V compiler:\n${compiler_error_probe_message}\n'), result.output
		assert result.output.count(compiler_error_probe_message) == 1, result.output
		assert result.output.all_before('retrying with').contains(compiler_error_probe_message), result.output
		assert result.output.all_after('retrying with').contains('compatibility program ran'), result.output
		assert !result.output.contains('kept its diagnostics quiet'), result.output
		assert os.read_file(os.join_path(root, 'ran'))! == 'ran\n'
		assert os.read_file(os.join_path(root, 'replayed'))! == 'yes'
		assert !os.exists(request)
		assert !os.exists(c_error_dir)
	}
}

fn test_fallback_diagnostics_replays_vsh_errors_without_running_the_script() {
	dispatcher := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	if !os.is_executable(dispatcher) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_compiler_error_script_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	state := RetryState{
		fallback_file: os.join_path(root, 'request')
	}
	bad_source := os.join_path(root, 'bad.vsh')
	os.write_file(bad_source, 'missing_v3_diagnostic()\n')!
	for payload in ['compiler_error\nsemantic checking', 'inline_asm', 'c_compilation_error'] {
		os.write_file(state.fallback_file, payload)!
		diagnostics := v3_fallback_diagnostics(dispatcher, ['-nocache', '-no-parallel', 'run',
			bad_source, 'ci'], state)
		assert diagnostics.contains('missing_v3_diagnostic'), diagnostics
		assert !diagnostics.contains('retrying with'), diagnostics
	}
	marker := os.join_path(root, 'ran')
	good_source := os.join_path(root, 'good.vsh')
	os.write_file(good_source, 'import os\n\nos.write_file(os.args[1], "ran") or {}\n')!
	os.write_file(state.fallback_file, 'compiler_error\nsemantic checking')!
	_ := v3_fallback_diagnostics(dispatcher, ['-nocache', '-no-parallel', 'run', good_source,
		marker], state)
	assert !os.exists(marker)
}

fn test_fallback_diagnostics_skips_replay_for_saved_c_output_and_unrelated_requests() {
	root := os.join_path(os.vtmp_dir(), 'v3_compiler_error_selection_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	missing_compiler := os.join_path(root, 'must_not_be_executed')
	assert v3_fallback_diagnostics(missing_compiler, [], RetryState{}) == ''
	state := RetryState{
		fallback_file: os.join_path(root, 'request')
		c_error_dir:   os.join_path(root, 'c_error')
	}
	assert v3_fallback_diagnostics(missing_compiler, [], state) == ''
	for payload in ['', 'compiler_error_partial', 'unknown'] {
		os.write_file(state.fallback_file, payload)!
		assert v3_fallback_diagnostics(missing_compiler, [], state) == ''
	}
	os.write_file(state.fallback_file, 'c_compilation_error')!
	os.mkdir_all(state.c_error_dir)!
	output_file := os.join_path(state.c_error_dir, 'output')
	os.write_file(output_file, 'original C error\n')!
	assert v3_fallback_diagnostics(missing_compiler, [], state) ==
		'C compiler output from the default V compiler:\noriginal C error\n'
	assert os.read_file(output_file)! == 'original C error\n'
}
