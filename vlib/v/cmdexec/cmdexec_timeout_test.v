module cmdexec

import os
import os.filelock
import time

const timeout_direct_child_ms = i64(200)

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
		// A missed deadline should fail the test after the child exits successfully.
		result := run_with_timeout('sleep', ['2'], timeout_direct_child_ms)
		elapsed := sw.elapsed().milliseconds()
		assert result.exit_code != 0
		assert elapsed >= timeout_direct_child_ms
		assert elapsed < 60000, 'run_with_timeout waited ${elapsed}ms, instead of giving up'
		assert_no_unreaped_child('a timed out quiet child')
	}
}

fn test_run_with_timeout_kills_a_child_that_keeps_writing() {
	sw := time.new_stopwatch()
	result := run_with_timeout(os.executable(), [timeout_pipe_probe, 'chatty', '', ''],
		timeout_direct_child_ms)
	elapsed := sw.elapsed().milliseconds()
	assert result.exit_code != 0
	assert elapsed >= timeout_direct_child_ms
	assert elapsed < 60000, 'run_with_timeout waited ${elapsed}ms for a child that keeps writing'
	assert result.output.contains('v-cmdexec-timeout-stream')
	assert_no_unreaped_child('a timed out chatty child')
}

fn test_run_with_timeout_kills_descendants_that_hold_the_pipes() {
	$if windows {
		assert true
	} $else {
		marker := os.join_path(os.vtmp_dir(), 'v_cmdexec_descendant_${os.getpid()}.marker')
		os.rm(marker) or {}
		defer {
			os.rm(marker) or {}
			os.rm(marker + '.lock') or {}
		}
		// The command starts a descendant that inherits its stdout/stderr and
		// outlives it. Killing only the direct child leaves those pipe writers
		// open, and the slurps at the end of a run block until every writer is
		// gone - so the bound would be ignored. The descendant also creates a
		// marker file after 2s, which must never appear.
		script := r'echo v-cmdexec-descendant; "$1" "$2" writer "$3" 2000 & wait'
		sw := time.new_stopwatch()
		result := run_with_timeout('sh', ['-c', script, 'sh', os.executable(), timeout_pipe_probe,
			marker], 500)
		elapsed := sw.elapsed().milliseconds()
		assert result.exit_code != 0
		assert elapsed < 60000, 'run_with_timeout waited ${elapsed}ms for a command with a live descendant'
		assert result.output.contains('v-cmdexec-descendant')
		assert_no_unreaped_child('a timed out command with a descendant')
		wait_for_timeout_descendant(marker, result.output)
		assert !os.exists(marker), 'the descendant outlived the timed out command'
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

const timeout_pipe_probe = 'v-cmdexec-timeout-pipe-probe'
const timeout_descendant_ready = 'v-cmdexec-descendant-ready'

fn wait_for_timeout_descendant(marker string, output string) {
	assert output.contains(timeout_descendant_ready), output
	mut writer_lock := filelock.new(marker + '.lock')
	assert writer_lock.wait_acquire(3 * time.second), 'the timed-out descendant did not exit'
	writer_lock.release()
}

// Reuse the test executable so these cases also run without a shell on Windows.
// Every descendant is finite, even when testing an implementation that ignores
// the deadline after its leader exits.
fn testsuite_begin() {
	if os.args.len < 2 || os.args[1] != timeout_pipe_probe {
		return
	}
	if os.args.len != 5 {
		exit(2)
	}
	mode := os.args[2]
	if mode in ['leader-success', 'leader-failure'] {
		mut writer := os.new_process(os.executable())
		writer.set_args([timeout_pipe_probe, 'writer', os.args[3], os.args[4]])
		// Inherit both output pipes and the leader's process group.
		writer.run()
		exit(if mode == 'leader-failure' { 7 } else { 0 })
	}
	if mode == 'writer' {
		marker := os.args[3]
		if marker != '' {
			mut writer_lock := filelock.new(marker + '.lock')
			writer_lock.acquire() or { exit(3) }
			// The OS releases the lock when the writer exits.
			println(timeout_descendant_ready)
			os.flush()
		}
		time.sleep(os.args[4].int() * time.millisecond)
		if marker != '' {
			os.write_file(marker, 'descendant survived') or { exit(3) }
		}
		println('late descendant stdout')
		eprintln('late descendant stderr')
		exit(0)
	}
	if mode == 'fast' {
		println('fast child stdout')
		eprintln('fast child stderr')
		exit(os.args[4].int())
	}
	if mode == 'quiet' {
		time.sleep(2500 * time.millisecond)
		exit(0)
	}
	if mode == 'chatty' {
		unbuffer_stdout()
		sw := time.new_stopwatch()
		// A regression that ignores busy pipes must still let the probe finish.
		for sw.elapsed() < 2 * time.second {
			println('v-cmdexec-timeout-stream')
		}
		exit(0)
	}
	exit(2)
}

fn test_run_with_timeout_drains_pipes_after_an_early_leader_exit() {
	for merge_output in [false, true] {
		for mode in ['leader-success', 'leader-failure'] {
			result := run_in_mode(os.executable(), [timeout_pipe_probe, mode, '', '50'], '',
				merge_output, 5000)
			expected_code := if mode == 'leader-failure' { 7 } else { 0 }
			assert result.exit_code == expected_code, result.output
			assert result.output.contains('late descendant stdout'), result.output
			assert result.output.contains('late descendant stderr'), result.output
			assert_no_unreaped_child('an early-exiting leader with completed output')
		}
	}
}

fn test_run_with_timeout_keeps_deadline_after_an_early_leader_exit() {
	for merge_output in [false, true] {
		marker := os.join_path(os.vtmp_dir(), 'v_cmdexec_early_exit_${os.getpid()}_${merge_output}.marker')
		os.rm(marker) or {}
		defer {
			os.rm(marker) or {}
			os.rm(marker + '.lock') or {}
		}
		sw := time.new_stopwatch()
		result := run_in_mode(os.executable(), [timeout_pipe_probe, 'leader-success', marker, '2500'], '', merge_output, 500)
		elapsed := sw.elapsed().milliseconds()
		wait_for_timeout_descendant(marker, result.output)
		assert result.exit_code != 0, result.output
		assert elapsed < 2000, 'inherited pipes bypassed the 500ms deadline: ${elapsed}ms'
		assert_no_unreaped_child('a timed out early-exiting leader')
		$if !windows {
			assert !os.exists(marker), 'the early leader exit prevented process-group cleanup'
		}
	}
}

fn test_run_with_timeout_preserves_fast_child_status_and_both_streams() {
	for code in [0, 23] {
		result := run_with_timeout(os.executable(), [timeout_pipe_probe, 'fast', '', code.str()],
			5000)
		assert result.exit_code == code, result.output
		assert result.output.contains('fast child stdout'), result.output
		assert result.output.contains('fast child stderr'), result.output
		assert_no_unreaped_child('a fast native child')
	}
}

fn test_run_with_timeout_does_not_confuse_empty_open_pipes_with_eof() {
	sw := time.new_stopwatch()
	result := run_with_timeout(os.executable(), [timeout_pipe_probe, 'quiet', '', ''],
		timeout_direct_child_ms)
	assert result.exit_code != 0, result.output
	assert sw.elapsed().milliseconds() >= timeout_direct_child_ms
	assert_no_unreaped_child('a quiet native child')
}
