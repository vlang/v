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
