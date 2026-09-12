module cmdexec

import time

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
	}
}

fn test_run_with_timeout_still_returns_the_output_of_a_fast_child() {
	$if windows {
		assert true
	} $else {
		result := run_with_timeout('echo', ['v-cmdexec-timeout-probe'], 60000)
		assert result.exit_code == 0
		assert result.output.trim_space() == 'v-cmdexec-timeout-probe'
	}
}
