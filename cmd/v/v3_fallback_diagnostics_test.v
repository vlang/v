module main

import os
import v.ansi
import v.cmdexec

fn test_v1_fallback_failure_message_separates_the_v3_reason() {
	reason := 'V compilation failed (compiler_error)'
	assert v1_fallback_failure_message('${reason}, but no usable V 0.5.2 fallback was found', reason) ==
		'Fallback unavailable: no usable V 0.5.2 fallback was found'
	assert v1_fallback_failure_message('`make v1` failed with exit code 2.', reason) ==
		'`make v1` failed with exit code 2.'
}

fn test_v1_fallback_make_hint_names_the_make_this_platform_has() {
	hint := v1_fallback_make_hint()
	// A stock Windows install has no `make` at all, so advice to "install make"
	// is not actionable there; MSYS2's `mingw32-make` is what it can use.
	$if windows {
		assert hint.contains('mingw32-make'), hint
	} $else {
		assert hint == 'Install make.', hint
	}
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
			mut args := ['-nocache', '-no-parallel', '-gc', 'none', '-cc', @CCOMPILER, '-o', binary]
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
	diagnostics := v3_diagnostics_output(dispatcher, ['-nocache', '-no-parallel', '-gc', 'none',
		'-cc', @CCOMPILER, '-no-retry-compilation', 'run', source])
	// -check would prevent execution too, but would lose this C diagnostic.
	assert diagnostics.contains('V3_DIAGNOSTIC_NATIVE_ERROR'), diagnostics
	assert !diagnostics.contains('retrying with'), diagnostics
}

const diagnostics_probe_env = 'VTEST_V3_DIAGNOSTICS_PIPE_PROBE'
const diagnostics_probe_bytes = 2 * 1024 * 1024

const replay_color_probe_env = 'VTEST_V3_REPLAY_COLOR_PROBE'

fn testsuite_begin() {
	run_compiler_error_probe()
	if os.getenv(replay_color_probe_env) == '1' {
		print(os.getenv('VCOLORS'))
		exit(0)
	}
	run_diagnostics_pipe_probe()
}

// Reuse this test executable as both the noisy child and its collecting parent.
// The outer test bounds the entire process group, so the old wait-before-read
// implementation fails without leaving a blocked compiler behind.
fn run_diagnostics_pipe_probe() {
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

const compiler_error_probe_env = 'VTEST_COMPILER_ERROR_FALLBACK_ROOT'
const compiler_error_probe_message = 'sample.vsh:2: error: unknown function `missing_v3_diagnostic`'
const compiler_error_probe_exit = 17

// The copied test executable acts as the launcher and the diagnostic compiler.
// A local compatibility stub avoids downloads and automatic bug-report uploads.
fn run_compiler_error_probe() {
	root := os.getenv(compiler_error_probe_env)
	if root == '' {
		return
	}
	if os.getenv(v3_no_fallback_env) == '1' {
		assert os.args[1..] == ['-skip-running', 'run', os.join_path(root, 'sample.vsh'), 'ci']
		assert os.getenv(v3_retry_env) == '1'
		assert os.getenv('VNORUN') == '1'
		assert os.getenv('VFLAGS') == ''
		assert os.getenv(v3_fallback_file_env) == ''
		assert os.getenv(v3_c_error_dir_env) == ''
		// A replay in a child of a diagnostics server is a compilation, not a
		// server that waits for requests.
		assert os.getenv('V_DIAGNOSTICS_SERVER') == ''
		assert !os.exists(os.join_path(root, 'ran'))
		assert !os.exists(os.join_path(root, 'replayed'))
		os.write_file(os.join_path(root, 'replayed'), 'yes') or { panic(err) }
		eprint(compiler_error_probe_message)
		exit(1)
	}
	state := &RetryState{
		fallback_file: os.join_path(root, 'request')
		c_error_dir:   os.join_path(root, 'c_error')
		args:          os.args[1..].clone()
	}
	// Exercise the real automatic retry decision, not launch_v1 directly.
	unsafe { retry_state(state) }
	at_exit(retry_with_v1_at_exit) or { panic(err) }
	exit(compiler_error_probe_exit)
}

fn test_only_c_errors_launch_the_compatibility_compiler() {
	$if windows {
		return
	}
	previous := os.environ()
	names := [compiler_error_probe_env, v3_no_fallback_env, v3_retry_env, v3_fallback_file_env,
		v3_c_error_dir_env, 'VNORUN', 'VFLAGS', 'V_C_ERROR_BUG_REPORT_DISABLED', 'V_DIAGNOSTICS_SERVER',
		'V3_INTERNAL_CACHE_FORCE_SOURCE', 'V3_CACHE_FORCE_SOURCE']
	defer {
		for name in names {
			if name in previous {
				os.setenv(name, previous[name], true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	payloads := ['compiler_error\nparsing', 'compiler_error\nsemantic checking',
		'compiler_error\nC generation', 'inline_asm', 'c_compilation_error']
	for case_index, payload in payloads {
		for status in [0, 23] {
			caller_value := if status == 0 { '' } else { 'caller-control' }
			caller_presence := if status == 0 { '' } else { 'x' }
			root := os.join_path(os.vtmp_dir(), 'v3_compiler_error_launch_${os.getpid()}_${case_index}_${status}')
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
			stub := '#!/bin/sh\nif [ "\$1" = version ]; then\n' +
				'printf "queried\\n" >> "\$${compiler_error_probe_env}/queried"\n' +
				'echo "V ${v_version} probe"; exit 0; fi\n' +
				'test -z "\$VNORUN" || exit 91\n' +
				'test "\$VFLAGS" = "already merged" || exit 92\n' +
				'test -z "\$V_MACOS_V3_FALLBACK_FILE" || exit 93\n' +
				'test -z "\$V_MACOS_V3_C_ERROR_DIR" || exit 94\n' +
				'test -z "\${V3_INTERNAL_CACHE_FORCE_SOURCE+x}" || { echo "private source-retry sentinel leaked" >&2; exit 96; }\n' +
				'test "\${V3_CACHE_FORCE_SOURCE+x}" = "${caller_presence}" || exit 97\n' +
				'test "\$V3_CACHE_FORCE_SOURCE" = "${caller_value}" || exit 98\n' +
				'printf "private=%s\\ncaller_present=%s\\ncaller=%s\\n" "\${V3_INTERNAL_CACHE_FORCE_SOURCE+x}" "\${V3_CACHE_FORCE_SOURCE+x}" "\$V3_CACHE_FORCE_SOURCE" > "\$${compiler_error_probe_env}/compat_env"\n' +
				'test "\$1" = run && test "\$3" = ci || exit 95\n' +
				'printf "%s\\n" "\$@" > "\$${compiler_error_probe_env}/compat_args"\n' +
				'printf "ran\\n" >> "\$${compiler_error_probe_env}/ran"\n' +
				'echo "compatibility program ran" >&2\nexit ${status}\n'
			for path in [os.join_path(compat, 'v'), os.join_path(root, v1_fallback_binary)] {
				os.write_file(path, stub)!
				os.chmod(path, 0o700)!
			}
			os.write_file(os.join_path(root, v1_fallback_binary + '.vroot'), compat)!
			request := os.join_path(root, 'request')
			c_error_dir := os.join_path(root, 'c_error')
			os.write_file(request, payload)!
			os.mkdir_all(c_error_dir)!
			// A saved C diagnostic must not make a V error eligible for fallback.
			os.write_file(os.join_path(c_error_dir, 'output'), 'original C error\n')!
			os.setenv(compiler_error_probe_env, root, true)
			os.unsetenv(v3_no_fallback_env)
			os.unsetenv(v3_retry_env)
			os.setenv(v3_fallback_file_env, request, true)
			os.setenv(v3_c_error_dir_env, c_error_dir, true)
			os.unsetenv('VNORUN')
			os.setenv('VFLAGS', 'already merged', true)
			// Model the compiler state after a source retry, retaining the caller's flag.
			os.setenv('V3_INTERNAL_CACHE_FORCE_SOURCE', '1', true)
			if status == 0 {
				os.unsetenv('V3_CACHE_FORCE_SOURCE')
			} else {
				os.setenv('V3_CACHE_FORCE_SOURCE', caller_value, true)
			}
			os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
			// As in a child of a diagnostics server, which runs a one-shot compilation.
			os.setenv('V_DIAGNOSTICS_SERVER', '1', true)
			mut launch_args := ['run', os.join_path(root, 'sample.vsh'), 'ci']
			if payload == 'c_compilation_error' {
				private_args := [
					'-v3-internal-implicit-tcc-warning=warning: implicit tcc failed',
					'-v3-internal-parser-diagnostics-printed',
				]
				// The restart markers are compiler options at the front, and user
				// data after the script. Only the compiler options may be removed.
				launch_args << private_args
				launch_args.prepend(private_args)
			}
			result := cmdexec.run_with_timeout(launcher, launch_args, 15_000)
			if payload == 'c_compilation_error' {
				assert result.exit_code == status, result.output
				assert result.output.starts_with('C compiler output from the default V compiler:\noriginal C error\n'), result.output
				assert result.output.count('original C error') == 1, result.output
				assert result.output.all_after('retrying with').contains('compatibility program ran'), result.output
				assert os.read_file(os.join_path(root, 'ran'))! == 'ran\n'
				assert os.read_file(os.join_path(root, 'compat_env'))! ==
					'private=\ncaller_present=${caller_presence}\ncaller=${caller_value}\n'
				assert os.read_lines(os.join_path(root, 'compat_args'))! == launch_args[2..]
				assert os.exists(os.join_path(root, 'queried'))
				assert !os.exists(os.join_path(root, 'replayed'))
			} else {
				// Neither the replay's exit 1 nor a successful V1 stub may replace
				// the original V3 failure status.
				assert result.exit_code == compiler_error_probe_exit, result.output
				assert result.output == 'Compiler output from the default V compiler:\n${compiler_error_probe_message}\n', result.output
				assert !os.exists(os.join_path(root, 'ran'))
				assert !os.exists(os.join_path(root, 'compat_env'))
				assert !os.exists(os.join_path(root, 'queried'))
				assert os.read_file(os.join_path(root, 'replayed'))! == 'yes'
			}
			assert !result.output.contains('kept its diagnostics quiet'), result.output
			assert !os.exists(request)
			assert !os.exists(c_error_dir)
		}
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
	_ := v3_fallback_diagnostics(dispatcher, ['-nocache', '-no-parallel', 'run', good_source, marker], state)
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
	for payload in ['', 'compiler_error_partial', 'c_compilation_error_partial', 'unknown'] {
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

fn test_replay_forwards_the_parent_terminal_color_decision() {
	old_colors := os.getenv_opt('VCOLORS')
	old_probe := os.getenv_opt(replay_color_probe_env)
	defer {
		if value := old_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
		if value := old_probe {
			os.setenv(replay_color_probe_env, value, true)
		} else {
			os.unsetenv(replay_color_probe_env)
		}
	}
	os.unsetenv('VCOLORS')
	os.setenv(replay_color_probe_env, '1', true)
	expected := if ansi.stderr_supports_escape_sequences() { 'always' } else { 'never' }
	assert v3_diagnostics_output(os.executable(), []) == expected
	assert os.getenv('VCOLORS') == ''
}

fn test_replayed_duplicate_diagnostics_respect_color_flags_and_environment() {
	dispatcher := diagnostics_test_dispatcher()
	path := os.join_path(os.vtmp_dir(), 'v3_replay_colors_${os.getpid()}.v')
	os.write_file(path, 'fn main() {}\nfn lighten() {}\nfn lighten() {}\n')!
	old_colors := os.getenv_opt('VCOLORS')
	defer {
		os.rm(path) or {}
		if value := old_colors {
			os.setenv('VCOLORS', value, true)
		} else {
			os.unsetenv('VCOLORS')
		}
	}
	for mode in ['always', 'never'] {
		os.setenv('VCOLORS', mode, true)
		for flag in ['', '-color', '-nocolor'] {
			mut args := ['-nocache', '-check']
			if flag != '' {
				args << flag
			}
			args << path
			output := v3_diagnostics_output(dispatcher, args)
			assert output.contains('redefinition of function `lighten`'), output
			assert output.count('conflicting declaration:') == 2, output
			colored := flag == '-color' || (flag == '' && mode == 'always')
			if colored {
				assert output.contains('\x1b[1m\x1b[31mbuilder error:\x1b[39m\x1b[22m'), output
				assert output.contains('\x1b[1m\x1b[35mconflicting declaration:\x1b[39m\x1b[22m'), output
			} else {
				assert !output.contains('\x1b['), output
			}
			assert os.getenv('VCOLORS') == mode
		}
	}
}
