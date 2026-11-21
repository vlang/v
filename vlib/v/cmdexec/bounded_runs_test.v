// vtest build: !windows
module cmdexec

import os
import time

// process_group_is_gone is true when no process is left in the group led by `pid`.
fn process_group_is_gone(pid int) bool {
	result := os.exec(['ps', '-e', '-o', 'pgid=,stat='])
	assert result.exit_code == 0, result.output
	for line in result.output.split_into_lines() {
		fields := line.fields()
		if fields.len == 2 && fields[0] == pid.str() && !fields[1].starts_with('Z') {
			return false
		}
	}
	return true
}

fn run_a_long_bounded_command(marker string) os.Result {
	// the shell starts a second process and waits for it: a group of two
	result, _ := run_merged_timed('/bin/sh', ['-c', 'echo \$\$ > ${marker}; sleep 600 & wait'],
		600_000, false)
	return result
}

// A tool that is cancelled ends its bounded runs: each is in a process group of its own, which
// the tool's own signal does not reach.
fn test_kill_bounded_runs_ends_a_run_in_progress_and_what_it_started() {
	marker := os.join_path(os.vtmp_dir(), 'cmdexec_bounded_${os.getpid()}.pid')
	os.rm(marker) or {}
	defer {
		os.rm(marker) or {}
	}
	assert bounded_runs_in_progress() == 0
	run := spawn run_a_long_bounded_command(marker)
	defer {
		// a failed assertion must not leave the command running
		kill_bounded_runs()
	}
	mut leader := 0
	for _ in 0 .. 100 {
		leader = (os.read_file(marker) or { '' }).trim_space().int()
		if leader > 0 {
			break
		}
		time.sleep(50 * time.millisecond)
	}
	assert leader > 0, 'the command did not start'
	assert bounded_runs_in_progress() == 1
	assert !process_group_is_gone(leader)
	assert kill_bounded_runs() == 1
	// Asked before the run is waited for: a run whose group outlived it holds its pipes, and
	// the wait would not return.
	mut gone := false
	for _ in 0 .. 40 {
		if process_group_is_gone(leader) {
			gone = true
			break
		}
		time.sleep(50 * time.millisecond)
	}
	assert gone, 'the group of ${leader} is still there'
	result := run.wait()
	assert result.exit_code != 0, result.output
	assert bounded_runs_in_progress() == 0
	// with nothing in progress it kills nothing
	assert kill_bounded_runs() == 0
}

// A run without a limit shares this process's group and is reached by its signals: it is not
// tracked, and not killed.
fn test_an_unbounded_run_is_not_tracked() {
	result := run_in_merged('/bin/sh', ['-c', 'echo done'], '')
	assert result.exit_code == 0 && result.output.trim_space() == 'done'
	assert bounded_runs_in_progress() == 0
}
