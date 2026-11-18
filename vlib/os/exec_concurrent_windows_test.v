import os
import time

// Children started at the same moment by different threads must not wait for each
// other. Before the creation of a child was serialized, a child inherited the write
// end of the pipe that another thread had just made for a child of its own, and that
// thread's read saw no end of file until both children had exited: a call for a
// command that prints a word returned only when the slow command beside it had
// finished (#32395).

// A fast call takes tens of milliseconds; one held by the slow child takes four
// seconds. The limit sits between them, with room for a loaded CI runner.
const fast_limit_ms = 2000
const fast_calls = 6

// slow_args is a command that runs for about four seconds.
fn slow_args() []string {
	return ['ping', '-n', '5', '127.0.0.1']
}

// fast_args is a command that prints a word and exits.
fn fast_args() []string {
	return ['cmd', '/d', '/c', 'echo', 'hi']
}

fn wait_for(go_at time.Time) {
	for time.now() < go_at {
	}
}

fn timed_exec(args []string, go_at time.Time) i64 {
	wait_for(go_at)
	sw := time.new_stopwatch()
	res := os.exec(args)
	assert res.exit_code == 0, res.output
	return sw.elapsed().milliseconds()
}

fn timed_process(args []string, go_at time.Time) i64 {
	wait_for(go_at)
	sw := time.new_stopwatch()
	mut p := os.new_process(os.find_abs_path_of_executable(args[0]) or { panic(err) })
	p.set_args(args[1..])
	p.set_redirect_stdio()
	p.run()
	output := p.stdout_slurp()
	p.wait()
	assert p.code == 0, output
	p.close()
	return sw.elapsed().milliseconds()
}

fn test_exec_calls_started_together_do_not_wait_for_each_other() {
	go_at := time.now().add(200 * time.millisecond)
	slow := spawn timed_exec(slow_args(), go_at)
	mut fast := []thread i64{}
	for _ in 0 .. fast_calls {
		fast << spawn timed_exec(fast_args(), go_at)
	}
	took := fast.wait()
	slow_took := slow.wait()
	assert slow_took > fast_limit_ms, 'the slow command is not slow enough to tell: ${slow_took} ms'
	for ms in took {
		assert ms < fast_limit_ms, 'an os.exec call waited for the child of another thread: ${took} ms'
	}
}

fn test_process_started_beside_exec_does_not_wait_for_it() {
	go_at := time.now().add(200 * time.millisecond)
	slow := spawn timed_exec(slow_args(), go_at)
	mut fast := []thread i64{}
	for _ in 0 .. fast_calls {
		fast << spawn timed_process(fast_args(), go_at)
	}
	took := fast.wait()
	slow_took := slow.wait()
	assert slow_took > fast_limit_ms, 'the slow command is not slow enough to tell: ${slow_took} ms'
	for ms in took {
		assert ms < fast_limit_ms, 'an os.Process with redirected output waited for the child of another thread: ${took} ms'
	}
}
