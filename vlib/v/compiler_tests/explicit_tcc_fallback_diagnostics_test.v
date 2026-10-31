import os
import v.cmdexec

fn test_explicit_tcc_fallback_reports_original_diagnostics() {
	$if windows {
		return
	}
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	defer {
		if value := old_vflags { os.setenv('VFLAGS', value, true) }
		if value := old_vosargs { os.setenv('VOSARGS', value, true) }
	}
	root := os.join_path(os.vtmp_dir(), 'v_explicit_tcc_error_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fake_tcc := os.join_path(root, 'tcc')
	os.write_file(fake_tcc, '#!/bin/sh\nif [ "$1" = -v ]; then echo \'tcc version 0.9.28\'; exit 0; fi\necho \'In file included from src.c:1:\' >&2\necho "src.c:2: error: unresolved reference to \'v_issue_29374\'" >&2\necho \'tcc: error: second diagnostic\' >&2\nexit 1\n')!
	os.chmod(fake_tcc, 0o755)!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { answer := 40 + 2; println(answer) }\n')!
	output := os.join_path(root, 'program')
	args := ['-new-compiler', '-nocache', '-gc', 'none', '-cc', fake_tcc, '-o', output, source]
	build := cmdexec.run(@VEXE, args)
	assert build.exit_code == 0, build.output
	assert build.output.contains("warning: tcc compilation failed (src.c:2: error: unresolved reference to 'v_issue_29374'), falling back to "), build.output
	assert !build.output.contains('second diagnostic'), build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	shown := cmdexec.run(@VEXE, ['-show-c-output', ...args])
	assert shown.exit_code == 0, shown.output
	assert shown.output.contains('second diagnostic'), shown.output
	failed := cmdexec.run(@VEXE, ['-no-retry-compilation', ...args])
	assert failed.exit_code != 0, failed.output
	assert failed.output.contains('v_issue_29374'), failed.output
	assert !failed.output.contains('falling back to'), failed.output
}
