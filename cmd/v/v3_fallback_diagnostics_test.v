module main

import os
import v.cmdexec

fn test_v1_fallback_failure_message_separates_the_v3_reason() {
	reason := 'V compilation failed (compiler_error)'
	assert v1_fallback_failure_message('${reason}, but no usable V 0.5.2 fallback was found', reason) ==
		'Fallback unavailable: no usable V 0.5.2 fallback was found'
	assert v1_fallback_failure_message('`make v1` failed with exit code 2.', reason) ==
		'`make v1` failed with exit code 2.'
}

fn test_v3_c_error_diagnostics_preserves_the_original_output() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_error_diagnostics_${os.getpid()}')
	os.rmdir_all(root) or {}
	state := RetryState{
		fallback_file: os.join_path(root, 'fallback')
		c_error_dir:   os.join_path(root, 'c_error')
	}
	os.mkdir_all(state.c_error_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(state.fallback_file, 'c_compilation_error')!
	output_file := os.join_path(state.c_error_dir, 'output')
	outputs := [
		'test.c:7: error: unknown type\n\tbad_type value;\n\t^\n',
		'test.c:7: error: unknown type',
		'test.c:7: error: unknown type\r\n',
		'line: C compiler error\n'.repeat(4096) + 'last diagnostic\n',
	]
	for output in outputs {
		os.write_file(output_file, output)!
		newline := if output.ends_with('\n') { '' } else { '\n' }
		assert v3_c_error_diagnostics(state) ==
			'C compiler output from the default V compiler:\n${output}${newline}'
		// Displaying diagnostics must not consume the staged bug-report data.
		assert os.read_file(output_file)! == output
	}
}

fn test_v3_c_error_diagnostics_ignores_missing_or_unrelated_state() {
	assert v3_c_error_diagnostics(RetryState{}) == ''
	root := os.join_path(os.vtmp_dir(), 'v3_c_error_diagnostics_missing_${os.getpid()}')
	os.rmdir_all(root) or {}
	state := RetryState{
		fallback_file: os.join_path(root, 'fallback')
		c_error_dir:   os.join_path(root, 'c_error')
	}
	os.mkdir_all(state.c_error_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	assert v3_c_error_diagnostics(state) == ''
	os.write_file(state.fallback_file, 'c_compilation_error')!
	assert v3_c_error_diagnostics(state) == ''
	output_file := os.join_path(state.c_error_dir, 'output')
	os.write_file(output_file, '')!
	assert v3_c_error_diagnostics(state) == ''
	os.write_file(output_file, 'stale C compiler output\n')!
	for payload in ['compiler_error\nsemantic checking', 'inline_asm', 'c_compilation_error_partial',
		''] {
		os.write_file(state.fallback_file, payload)!
		assert v3_c_error_diagnostics(state) == ''
	}
}

fn diagnostics_test_dispatcher() string {
	dispatcher := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	assert os.is_executable(dispatcher), dispatcher
	return dispatcher
}

fn test_v3_diagnostics_output_replays_the_error_without_running_user_code() {
	dispatcher := diagnostics_test_dispatcher()
	root := os.join_path(os.vtmp_dir(), 'v3_fallback_diagnostics_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	bad_source := os.join_path(root, 'bad.v')
	os.write_file(bad_source, 'fn main() {\n\tmissing_v3_diagnostic()\n}\n')!
	diagnostics := v3_diagnostics_output(dispatcher, ['-nocache', '-no-parallel', 'run', bad_source])
	assert diagnostics.contains('missing_v3_diagnostic'), diagnostics
	assert !diagnostics.contains('retrying with'), diagnostics

	marker := os.join_path(root, 'ran')
	good_source := os.join_path(root, 'good.v')
	os.write_file(good_source, 'import os\n\nfn main() {\n\tos.write_file(os.args[1], "ran") or {}\n}\n')!
	_ := v3_diagnostics_output(dispatcher, ['-nocache', '-no-parallel', 'run', good_source, marker])
	assert !os.exists(marker)
}

fn test_v3_diagnostics_output_compiles_programs_and_scripts_without_running_them() {
	dispatcher := diagnostics_test_dispatcher()
	root := os.join_path(os.vtmp_dir(), 'v3 replay compiled ${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	extension := $if windows { '.exe' } $else { '' }
	body := 'os.write_file(@FILE + ".ran", "ran") or { panic(err) }'
	for suffix in ['v', 'vsh'] {
		source := os.join_path(root, 'good.${suffix}')
		content := if suffix == 'vsh' {
			'import os\n${body}\n'
		} else {
			'import os\nfn main() {\n${body}\n}\n'
		}
		os.write_file(source, content)!
		marker := source + '.ran'
		for explicit_run in [false, true] {
			binary := os.join_path(root, 'compiled_${suffix}_${explicit_run}${extension}')
			mut args := ['-nocache', '-no-parallel', '-gc', 'none', '-cc', @CCOMPILER,
				'-o', binary]
			if explicit_run {
				args << 'run'
			}
			args << source
			if explicit_run || suffix == 'vsh' {
				// These are program arguments, not compiler options. In particular,
				// finding -skip-running here must not suppress the replay's prefix.
				args << ['argument with spaces', '', '-skip-running']
			}
			original_args := args.clone()
			diagnostics := v3_diagnostics_output(dispatcher, args)
			assert args == original_args
			assert !os.exists(marker), diagnostics
			assert os.is_executable(binary), diagnostics
			// Prove that replay actually produced a working executable: an early
			// compiler failure must not satisfy the no-side-effects assertion.
			manual := cmdexec.run_with_timeout(binary, [], 5_000)
			assert manual.exit_code == 0, manual.output
			assert os.read_file(marker)! == 'ran'
			os.rm(marker)!
		}
	}
}

fn test_v3_diagnostics_output_still_runs_the_native_compiler() {
	dispatcher := diagnostics_test_dispatcher()
	root := os.join_path(os.vtmp_dir(), 'v3 replay native ${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'native_error.h').replace('\\', '/')
	os.write_file(header, '#error V3_DIAGNOSTIC_NATIVE_ERROR\n')!
	source := os.join_path(root, 'bad_native.v')
	os.write_file(source, '#include "${header}"\nfn main() {}\n')!
	diagnostics := v3_diagnostics_output(dispatcher, ['-nocache', '-no-parallel', '-gc',
		'none', '-cc', @CCOMPILER, '-no-retry-compilation', 'run', source])
	// -check would prevent execution too, but would lose this C diagnostic.
	assert diagnostics.contains('V3_DIAGNOSTIC_NATIVE_ERROR'), diagnostics
	assert !diagnostics.contains('retrying with'), diagnostics
}

const diagnostics_probe_env = 'VTEST_V3_DIAGNOSTICS_PIPE_PROBE'
const diagnostics_probe_bytes = 2 * 1024 * 1024

// Reuse this test executable as both the noisy child and its collecting parent.
// The outer test bounds the entire process group, so the old wait-before-read
// implementation fails without leaving a blocked compiler behind.
fn testsuite_begin() {
	mode := os.getenv(diagnostics_probe_env)
	if mode == 'emit' {
		if os.args[1..] != ['-skip-running', 'run', 'source with spaces.v', '', '-skip-running'] {
			eprintln('diagnostic replay lost its compile-only prefix or changed the original arguments')
			exit(1)
		}
		if os.getenv('VFLAGS') != '' || os.getenv('VNORUN') != '1'
			|| os.getenv(v3_no_fallback_env) != '1' || os.getenv(v3_retry_env) != '1'
			|| os.getenv(v3_fallback_file_env) != '' || os.getenv(v3_c_error_dir_env) != '' {
			eprintln('diagnostic replay did not isolate its environment')
			exit(1)
		}
		print('o'.repeat(diagnostics_probe_bytes))
		flush_stdout()
		eprint('e'.repeat(diagnostics_probe_bytes))
		flush_stderr()
		exit(23)
	}
	if mode == 'collect' {
		os.setenv(diagnostics_probe_env, 'emit', true)
		os.setenv('VFLAGS', 'must not be forwarded', true)
		os.setenv(v3_fallback_file_env, 'must be removed', true)
		os.setenv(v3_c_error_dir_env, 'must be removed', true)
		output := v3_diagnostics_output(os.executable(), ['run', 'source with spaces.v', '',
			'-skip-running'])
		expected := 'o'.repeat(diagnostics_probe_bytes) + 'e'.repeat(diagnostics_probe_bytes)
		if output != expected {
			eprintln('incomplete diagnostic replay: expected ${expected.len} bytes, got ${output.len}')
			exit(1)
		}
		exit(0)
	}
}

fn test_v3_diagnostics_output_drains_large_stdout_and_stderr_before_waiting() {
	previous := os.getenv_opt(diagnostics_probe_env)
	os.setenv(diagnostics_probe_env, 'collect', true)
	defer {
		if value := previous {
			os.setenv(diagnostics_probe_env, value, true)
		} else {
			os.unsetenv(diagnostics_probe_env)
		}
	}
	result := cmdexec.run_with_timeout(os.executable(), [], 15_000)
	assert result.exit_code == 0, result.output
}
