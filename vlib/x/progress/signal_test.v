import os

// The display hides the cursor, so it must give it back however the program
// ends, while leaving the application's own signal handling alone (the same
// contract as term.show_cursor_on_exit).
fn test_cursor_is_restored_and_application_signal_handling_is_preserved() {
	$if windows {
		return
	}
	fixture := os.join_path(@VMODROOT, 'vlib', 'x', 'progress', 'testdata', 'signal_fixture.c.v')
	binary := os.join_path(os.vtmp_dir(), 'x_progress_signal_${os.getpid()}')
	defer {
		os.rm(binary) or {}
	}
	compiled := os.exec([@VEXE, '-o', binary, fixture])
	assert compiled.exit_code == 0, compiled.output
	hide := '\x1b[?25l'
	show := '\x1b[?25h'
	for mode in ['default-int', 'default-term', 'handler-int', 'handler-term', 'ignore-int',
		'ignore-term'] {
		result := os.exec([binary, mode])
		assert result.output.contains(hide), '${mode}: ${result.output}'
		assert result.output.contains(show), '${mode}: ${result.output}'
		if mode.starts_with('default') {
			// no application handling: we restore the cursor, move to a fresh
			// line, and exit like a shell expects after the signal (128 + N)
			assert result.exit_code == if mode.ends_with('int') { 130 } else { 143 }, '${mode}: ${result.output}'
			assert result.output.contains('${show}\n'), '${mode}: ${result.output}'
		} else if mode.starts_with('handler') {
			// the application's handler ran, not ours; the cursor still came
			// back through the at-exit hook
			assert result.exit_code == 42, '${mode}: ${result.output}'
		} else {
			// the application ignores the signal, so the program ran to the end
			assert result.exit_code == 0, '${mode}: ${result.output}'
			assert result.output.count(show) == 1, '${mode}: ${result.output}'
		}
	}
}
