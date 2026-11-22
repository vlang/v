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
	begin_cancellable_work()
}

fn waits_for_the_end(done chan bool) {
	done <- end_cancelled_bounded_runs(20 * time.second)
}

fn finishes_the_work(done chan int) {
	done <- finish_bounded_runs()
}

fn runs_a_short_bounded_command(milliseconds int) os.Result {
	result, _ := run_merged_timed('/bin/sleep', ['0.${milliseconds:03}'], 600_000, false)
	return result
}

// The cancellation is a flag (a signal handler sets it): the thread that owns a run sees it
// and ends the run with everything it started, and a run is not started any more. Nothing
// but the flag is touched from outside that thread.
fn test_a_cancelled_run_is_ended_by_its_own_thread_and_none_is_started_after() {
	marker := os.join_path(os.vtmp_dir(), 'cmdexec_cancelled_${os.getpid()}.pid')
	defer {
		os.rm(marker) or {}
		forget_the_cancellation()
	}
	assert bounded_runs_cancelled() == 0
	assert bounded_runs_in_progress() == 0
	run, leader := start_a_long_bounded_command(marker)
	defer {
		// a failed assertion must not leave the command running
		cancel_bounded_runs(1)
	}
	assert leader > 0, 'the command did not start'
	assert bounded_runs_in_progress() == 1
	assert !process_group_is_gone(leader)
	cancel_bounded_runs(int(os.Signal.int))
	assert bounded_runs_cancelled() == int(os.Signal.int)
	// Asked before the run is waited for: a run whose group outlived it holds its pipes, and
	// the wait would not return.
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
	assert bounded_runs_in_progress() == 0
	// a run without a limit is no part of this
	unbounded := run_in_merged('/bin/sh', ['-c', 'echo done'], '')
	assert unbounded.exit_code == 0 && unbounded.output.trim_space() == 'done'
}

// A run can be cancelled at any moment between before its fork and after its close. The
// thread counts as owning a run for all of that, and the tool does not go while a run has an
// owner: here the count is held as a thread between its fork and its first read holds it.
fn test_the_end_of_a_cancellation_waits_for_a_run_that_has_an_owner() {
	defer {
		forget_the_cancellation()
	}
	begin_bounded_run()
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
	assert !returned_early, 'the tool was let go while a run had an owner'
	// the owner has ended the run, collected it and closed it
	end_bounded_run()
	mut finished := false
	select {
		ok := <-done {
			finished = ok
		}
		2 * time.second {
		}
	}
	assert finished, 'the tool was not let go when no run had an owner any more'
}

// The wait has a limit: a run whose owner never lets go does not hold the tool for ever.
fn test_the_end_of_a_cancellation_gives_up_after_its_limit() {
	defer {
		end_bounded_run()
		forget_the_cancellation()
	}
	begin_bounded_run()
	cancel_bounded_runs(1)
	waited := time.new_stopwatch()
	assert !end_cancelled_bounded_runs(200 * time.millisecond)
	assert waited.elapsed() < 5 * time.second
}

// How the tool ends is decided once. Finished first: a cancellation that was accepted before
// is handed to the thread that finished, and one that comes after ends the process at once.
fn test_work_that_is_finished_takes_an_earlier_cancellation_and_ends_at_a_later_one() {
	defer {
		forget_the_cancellation()
	}
	begin_cancellable_work()
	// accepted before the work was finished: not the moment to end the process
	assert !note_cancellation(int(os.Signal.int))
	assert finish_bounded_runs() == int(os.Signal.int)
	// the other thread stands back
	assert !begin_cancelled_exit()
	// accepted after: the process is to end here (cancel_bounded_runs does)
	assert note_cancellation(int(os.Signal.term))
	// finished without a cancellation
	forget_the_cancellation()
	assert finish_bounded_runs() == 0
	assert !begin_cancelled_exit()
}

// Cancelled first: the thread that ends the tool has it, and the thread that finishes the
// work at that moment does not go on to an ordinary end.
fn test_a_tool_that_is_being_cancelled_is_not_finished_by_another_thread() {
	defer {
		forget_the_cancellation()
	}
	begin_cancellable_work()
	assert !note_cancellation(int(os.Signal.int))
	assert begin_cancelled_exit()
	assert !begin_cancelled_exit()
	done := chan int{cap: 1}
	spawn finishes_the_work(done)
	mut returned := false
	select {
		_ := <-done {
			returned = true
		}
		300 * time.millisecond {
		}
	}
	assert !returned, 'the work was finished in the ordinary way while the tool was being cancelled'
	// (the tool exits here; this test lets the waiting thread go instead)
	begin_cancellable_work()
	_ := <-done
}

// Runs end by themselves while the cancellation arrives: each is collected by its own thread
// and by nothing else, whichever came first, and none is left with an owner.
fn test_a_cancellation_while_runs_end_by_themselves_leaves_none() {
	defer {
		forget_the_cancellation()
	}
	for round in 0 .. 8 {
		forget_the_cancellation()
		mut runs := []thread os.Result{}
		for i in 0 .. 16 {
			runs << spawn runs_a_short_bounded_command(5 + ((i * 7 + round * 3) % 60))
		}
		time.sleep((10 + round * 6) * time.millisecond)
		cancel_bounded_runs(int(os.Signal.int))
		assert end_cancelled_bounded_runs(10 * time.second), 'round ${round}: ${bounded_runs_in_progress()} runs still have an owner'
		results := runs.wait()
		assert results.len == 16
		assert bounded_runs_in_progress() == 0
	}
}

// A run without a limit shares this process's group and is reached by its signals: it is not
// counted.
fn test_an_unbounded_run_is_not_counted() {
	result := run_in_merged('/bin/sh', ['-c', 'echo done'], '')
	assert result.exit_code == 0 && result.output.trim_space() == 'done'
	assert bounded_runs_in_progress() == 0
}

// cancels_then_tells waits until the shell has said who it and its descendant are, records
// the cancellation, and only then tells the shell to go: the leader ends AFTER the
// cancellation was recorded. The FIFO is opened for both ends, so this never waits for a
// reader, and what it writes is kept until the shell reads it.
fn cancels_then_tells(pid_file string, ack string) {
	for _ in 0 .. 2000 {
		if (os.read_file(pid_file) or { '' }).fields().len == 2 {
			break
		}
		time.sleep(time.millisecond)
	}
	fd := C.open(&char(ack.str), C.O_RDWR)
	cancel_bounded_runs(2)
	if fd >= 0 {
		os.fd_write(fd, 'go\n')
		// long enough for the shell to take the line; the shell is ended by its owner if not
		time.sleep(20 * time.millisecond)
		os.fd_close(fd)
	}
}

// A leader that ends by itself after the cancellation was recorded, with a descendant in its
// group that holds neither pipe: the output is at its end and the leader has ended, which is
// exactly how a run that was not cancelled ends. The owner must still take the cancellation:
// the leader is looked at without being reaped, the cancellation is read after that, and the
// group is ended while its leader's pid is still its own. (Before, the owner reaped the
// leader on that path and left the loop without looking at the cancellation: the descendant
// lived on, with nobody left to end it.) The owner looks every millisecond and the shell goes
// within a fraction of one, so one attempt can pass by luck without the rule; twenty do not.
fn test_a_cancellation_recorded_before_the_leader_ends_reaches_a_descendant_that_holds_no_pipe() {
	dir := os.join_path(os.vtmp_dir(), 'bounded_eof_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mut left := []string{}
	for attempt in 0 .. 20 {
		forget_the_cancellation()
		pid_file := os.join_path(dir, 'pids_${attempt}')
		ack := os.join_path(dir, 'ack_${attempt}')
		assert os.exec(['mkfifo', ack]).exit_code == 0
		teller := spawn cancels_then_tells(pid_file, ack)
		result, timed_out := run_merged_timed('/bin/sh', ['-c',
			'sleep 600 > /dev/null 2>&1 & echo \$\$ \$! > ${pid_file}; read line < ${ack}'],
			600_000, false)
		teller.wait()
		pids := (os.read_file(pid_file) or { '' }).fields()
		assert pids.len == 2, 'attempt ${attempt}: the shell said who it is'
		assert !timed_out
		assert bounded_runs_in_progress() == 0
		if !wait_until_group_is_gone(pids[0].int()) {
			left << 'attempt ${attempt}: the descendant ${pids[1]} lives on (the run returned ${result.exit_code})'
			C.kill(pids[1].int(), C.SIGKILL)
		} else {
			assert result.exit_code != 0, 'attempt ${attempt}: a run that was cancelled does not report success'
		}
	}
	forget_the_cancellation()
	assert left.len == 0, left.join('\n')
}

// The decision above rests on one thing: a leader can be seen to have ended without being
// reaped. Asked twice, an ended leader is still there to be asked, and to be waited for with
// its own exit code afterwards; a running one is said to be running.
fn test_a_leader_that_ended_is_seen_without_being_reaped() {
	mut ended := os.new_process('/bin/sh')
	ended.set_args(['-c', 'exit 7'])
	ended.run()
	mut seen := 0
	for _ in 0 .. 2000 {
		seen = leader_has_ended(mut ended)
		if seen != 0 {
			break
		}
		time.sleep(time.millisecond)
	}
	$if linux || macos {
		assert seen == 1, 'waitid with WNOWAIT is there on this system'
	}
	if seen == -1 {
		eprintln('> leader_has_ended cannot tell on this system: a bounded run falls back to reading the cancellation before it asks (and reaps) the leader')
		ended.wait()
		ended.close()
		return
	}
	assert seen == 1
	assert leader_has_ended(mut ended) == 1, 'asked again, it is still there: the first answer did not reap it'
	// and it is still there to be waited for, with its own exit code (a leader that had been
	// reaped has none left to give: wait() would report -1)
	ended.wait()
	assert ended.code == 7
	ended.close()

	mut running := os.new_process('/bin/sleep')
	running.set_args(['600'])
	running.run()
	assert leader_has_ended(mut running) == 0
	running.signal_kill()
	running.wait()
	running.close()
}
