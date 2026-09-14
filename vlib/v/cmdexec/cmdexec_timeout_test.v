module cmdexec

import os
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

fn test_run_with_timeout_kills_descendants_that_hold_the_pipes() {
	$if windows {
		assert true
	} $else {
		marker := os.join_path(os.vtmp_dir(), 'v_cmdexec_descendant_${os.getpid()}.marker')
		os.rm(marker) or {}
		// The command starts a descendant that inherits its stdout/stderr and
		// outlives it. Killing only the direct child leaves those pipe writers
		// open, and the slurps at the end of a run block until every writer is
		// gone - so the bound would be ignored. The descendant also creates a
		// marker file after 2s, which must never appear.
		script := 'echo v-cmdexec-descendant; { sleep 2; touch ${os.quoted_path(marker)}; sleep 300; } & wait'
		sw := time.new_stopwatch()
		result := run_with_timeout('sh', ['-c', script], 500)
		elapsed := sw.elapsed().milliseconds()
		assert result.exit_code != 0
		assert elapsed < 60000, 'run_with_timeout waited ${elapsed}ms for a command with a live descendant'
		assert result.output.contains('v-cmdexec-descendant')
		assert_no_unreaped_child('a timed out command with a descendant')
		time.sleep(3 * time.second)
		assert !os.exists(marker), 'the descendant outlived the timed out command'
		os.rm(marker) or {}
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
