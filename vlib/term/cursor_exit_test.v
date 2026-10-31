import os

fn test_show_cursor_on_exit_preserves_exit_status_and_signal_handlers() {
	$if windows {
		return
	}
	fixture := os.join_path(@VMODROOT, 'vlib', 'term', 'testdata', 'cursor_exit.c.v')
	binary := os.join_path(os.vtmp_dir(), 'cursor_exit_${os.getpid()}')
	defer {
		os.rm(binary) or {}
	}
	compiled := os.exec([@VEXE, '-o', binary, '${fixture}'])
	assert compiled.exit_code == 0, compiled.output
	for mode in ['normal', 'exit', 'default-int', 'default-term', 'handler-int', 'handler-term',
		'ignore'] {
		result := os.exec([binary, ...(os.split_args(mode) or { panic(err) })])
		if mode in ['normal', 'exit', 'ignore'] {
			assert result.exit_code == if mode == 'exit' { 7 } else { 0 }, result.output
			assert result.output == '\x1b[?25l\x1b[?25h', result.output
		} else {
			assert result.exit_code != 0, mode
			if mode.starts_with('handler') {
				assert result.exit_code == 23, result.output
			}
			assert !result.output.contains('\x1b[?25h'), result.output
		}
	}
}
