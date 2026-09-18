module main

import os

fn test_v1_fallback_failure_message_separates_the_v3_reason() {
	reason := 'V compilation failed (compiler_error)'
	assert v1_fallback_failure_message('${reason}, but no usable V 0.5.2 fallback was found', reason) ==
		'Fallback unavailable: no usable V 0.5.2 fallback was found'
	assert v1_fallback_failure_message('`make v1` failed with exit code 2.', reason) ==
		'`make v1` failed with exit code 2.'
}

fn test_v3_diagnostics_output_replays_the_error_without_running_user_code() {
	dispatcher := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	if !os.is_executable(dispatcher) {
		return
	}
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
