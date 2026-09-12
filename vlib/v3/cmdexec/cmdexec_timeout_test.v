module cmdexec

import time

// assert_no_unreaped_child fails when the run that just finished left a child
// process behind. `waitpid(-1, WNOHANG)` returns -1/ECHILD when this process has
// no children at all, 0 when it has children that are all still running, and a
// pid > 0 when it just reaped a zombie - which is exactly what a killed child
// that was never waited for leaves behind.
fn assert_no_unreaped_child(label string) {
	$if !windows {
		mut status := 0
		reaped := C.waitpid(-1, &status, C.WNOHANG)
		assert reaped <= 0, '${label} left an unreaped child process (pid ${reaped})'
	}
}

fn test_run_with_timeout_kills_a_child_that_never_finishes() {
	$if windows {
		// `sleep` is not a standalone executable there.
		assert true
	} $else {
		sw := time.new_stopwatch()
		result := run_with_timeout('sleep', ['300'], 500)
		elapsed := sw.elapsed().milliseconds()
		assert result.exit_code != 0
		assert elapsed < 60000, 'run_with_timeout waited ${elapsed}ms, instead of giving up'
		assert_no_unreaped_child('a timed out quiet child')
	}
}

fn test_run_with_timeout_kills_a_child_that_keeps_writing() {
	$if windows {
		assert true
	} $else {
		sw := time.new_stopwatch()
		result := run_with_timeout('sh', ['-c', 'while :; do echo v-cmdexec-timeout-stream; done'], 500)
		elapsed := sw.elapsed().milliseconds()
		assert result.exit_code != 0
		assert elapsed < 60000, 'run_with_timeout waited ${elapsed}ms for a child that keeps writing'
		assert result.output.contains('v-cmdexec-timeout-stream')
		assert_no_unreaped_child('a timed out chatty child')
	}
}

fn test_run_with_timeout_still_returns_the_output_of_a_fast_child() {
	$if windows {
		assert true
	} $else {
		result := run_with_timeout('echo', ['v-cmdexec-timeout-probe'], 60000)
		assert result.exit_code == 0
		assert result.output.trim_space() == 'v-cmdexec-timeout-probe'
		assert_no_unreaped_child('a fast child')
	}
}
