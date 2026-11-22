// vtest build: !windows
module cmdexec

import os
import sync.stdatomic
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

// wait_until_group_is_gone asks for up to two seconds.
fn wait_until_group_is_gone(pid int) bool {
	for _ in 0 .. 40 {
		if process_group_is_gone(pid) {
			return true
		}
		time.sleep(50 * time.millisecond)
	}
	return false
}

fn run_a_long_bounded_command(marker string) os.Result {
	// the shell starts a second process and waits for it: a group of two
	result, _ := run_merged_timed('/bin/sh', ['-c', 'echo \$\$ > ${marker}; sleep 600 & wait'],
		600_000, false)
	return result
}

// start_a_long_bounded_command returns the thread of the run and the pid of its leader.
fn start_a_long_bounded_command(marker string) (thread os.Result, int) {
	os.rm(marker) or {}
	run := spawn run_a_long_bounded_command(marker)
	mut leader := 0
	for _ in 0 .. 100 {
		leader = (os.read_file(marker) or { '' }).trim_space().int()
		if leader > 0 {
			break
		}
		time.sleep(50 * time.millisecond)
	}
	return run, leader
}

// forget_the_cancellation puts the module back as a fresh process has it.
fn forget_the_cancellation() {
	stdatomic.store_u64(&cmdexec_bounded_runs_cancelled, 0)
}

fn waits_for_the_end(done chan bool) {
	done <- end_cancelled_bounded_runs(20 * time.second)
}

// A tool that is cancelled ends its bounded runs: each is in a process group of its own, which
// the tool's own signal does not reach.
fn test_kill_bounded_runs_ends_a_run_in_progress_and_what_it_started() {
	marker := os.join_path(os.vtmp_dir(), 'cmdexec_bounded_${os.getpid()}.pid')
	defer {
		os.rm(marker) or {}
	}
	assert bounded_runs_in_progress() == 0
	run, leader := start_a_long_bounded_command(marker)
	defer {
		// a failed assertion must not leave the command running
		kill_bounded_runs()
	}
	assert leader > 0, 'the command did not start'
	assert bounded_runs_in_progress() == 1
	assert !process_group_is_gone(leader)
	assert kill_bounded_runs() == 1
	// Asked before the run is waited for: a run whose group outlived it holds its pipes, and
	// the wait would not return.
	assert wait_until_group_is_gone(leader), 'the group of ${leader} is still there'
	result := run.wait()
	assert result.exit_code != 0, result.output
	assert bounded_runs_in_progress() == 0
	// with nothing in progress it kills nothing
	assert kill_bounded_runs() == 0
}

// The cancellation itself is a flag (a signal handler sets it): the thread that owns a run
// sees it and ends the run, and a run is not started any more.
fn test_a_cancelled_run_is_ended_by_its_own_thread_and_none_is_started_after() {
	marker := os.join_path(os.vtmp_dir(), 'cmdexec_cancelled_${os.getpid()}.pid')
	defer {
		os.rm(marker) or {}
		forget_the_cancellation()
	}
	assert bounded_runs_cancelled() == 0
	run, leader := start_a_long_bounded_command(marker)
	defer {
		kill_bounded_runs()
	}
	assert leader > 0, 'the command did not start'
	cancel_bounded_runs(int(os.Signal.int))
	assert bounded_runs_cancelled() == int(os.Signal.int)
	// nothing but the flag was touched here: the run's own thread ends it
	assert wait_until_group_is_gone(leader), 'the group of ${leader} is still there'
	result := run.wait()
	assert result.exit_code != 0, result.output
	assert bounded_runs_in_progress() == 0
	assert end_cancelled_bounded_runs(time.second)
	// a run asked for now is refused, and starts nothing
	os.rm(marker) or {}
	refused, timed_out := run_merged_timed('/bin/sh', ['-c', 'echo started > ${marker}'], 600_000,
		false)
	assert refused.exit_code != 0 && !timed_out
	assert refused.output.contains('was not started'), refused.output
	assert !os.exists(marker)
	// a run without a limit is no part of this
	unbounded := run_in_merged('/bin/sh', ['-c', 'echo done'], '')
	assert unbounded.exit_code == 0 && unbounded.output.trim_space() == 'done'
}

// A run can be cancelled between its fork and the moment its thread has put it in the table.
// The thread counts as starting a run from before the fork, and the tool does not go while
// one is being started: here the count is held as that thread would hold it.
fn test_the_end_of_a_cancellation_waits_for_a_run_that_is_being_started() {
	defer {
		forget_the_cancellation()
	}
	begin_bounded_start()
	cancel_bounded_runs(int(os.Signal.term))
	done := chan bool{cap: 1}
	spawn waits_for_the_end(done)
	time.sleep(300 * time.millisecond)
	mut returned_early := false
	select {
		_ := <-done {
			returned_early = true
		}
		else {
		}
	}
	assert !returned_early, 'the tool was let go while a run was being started'
	// the run is in the table now (here: it is over), and the thread says so
	end_bounded_start()
	mut finished := false
	select {
		ok := <-done {
			finished = ok
		}
		2 * time.second {
		}
	}
	assert finished, 'the tool was not let go when nothing was being started any more'
}

// The wait has a limit: a run that is never noted does not hold the tool for ever.
fn test_the_end_of_a_cancellation_gives_up_after_its_limit() {
	defer {
		end_bounded_start()
		forget_the_cancellation()
	}
	begin_bounded_start()
	cancel_bounded_runs(1)
	waited := time.new_stopwatch()
	assert !end_cancelled_bounded_runs(200 * time.millisecond)
	assert waited.elapsed() < 5 * time.second
}

// A run without a limit shares this process's group and is reached by its signals: it is not
// tracked, and not killed.
fn test_an_unbounded_run_is_not_tracked() {
	result := run_in_merged('/bin/sh', ['-c', 'echo done'], '')
	assert result.exit_code == 0 && result.output.trim_space() == 'done'
	assert bounded_runs_in_progress() == 0
}
