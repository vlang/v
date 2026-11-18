import os
import time

// A process that cannot be started ends the program, and ending it runs the at_exit
// callbacks. A callback that starts a command of its own must not wait for the lock
// that serializes the creation of children: the failed start has to release it before
// the exit. With the lock still held, the helper below never ends.

// The helper normally ends in well under a second. The limit only has to tell that
// from a deadlock, with room for a loaded CI runner.
const helper_limit_ms = 30000

const helper_source = "import os

fn cleanup() {
	res := os.exec(['cmd', '/d', '/c', 'echo', 'cleanup-ran'])
	println(res.output.trim_space())
}

fn main() {
	at_exit(cleanup) or { panic(err) }
	mut p := os.new_process('no-such-program-for-the-spawn-lock-test.exe')
	if os.args.len > 1 && os.args[1] == 'redirect' {
		p.set_redirect_stdio()
	}
	p.run()
	println('not reached')
}
"

fn build_helper() string {
	dir := os.join_path(os.vtmp_dir(), 'spawn_failure_at_exit_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	source := os.join_path(dir, 'helper.v')
	exe := os.join_path(dir, 'helper.exe')
	os.write_file(source, helper_source) or { panic(err) }
	res := os.exec([@VEXE, '-o', exe, source])
	assert res.exit_code == 0, res.output
	return exe
}

// run_helper starts the helper and waits for it, but never longer than the limit:
// a helper that is still alive then is killed, and reported as a deadlock.
fn run_helper(exe string, mode string) (int, string) {
	mut p := os.new_process(exe)
	if mode != '' {
		p.set_args([mode])
	}
	p.set_redirect_stdio()
	p.run()
	sw := time.new_stopwatch()
	for p.is_alive() {
		if sw.elapsed().milliseconds() > helper_limit_ms {
			p.signal_kill()
			p.wait()
			p.close()
			assert false, 'the helper (${mode}) was still running after ${helper_limit_ms} ms: its at_exit callback waits for the lock its failed start still holds'
		}
		time.sleep(20 * time.millisecond)
	}
	output := p.stdout_slurp()
	p.wait()
	code := p.code
	p.close()
	return code, output
}

fn test_a_failed_start_releases_the_creation_lock_before_at_exit_callbacks_run() {
	exe := build_helper()
	defer {
		os.rmdir_all(os.dir(exe)) or {}
	}
	// without redirection the start fails at CreateProcess; with it, after the pipes
	// for the child have been made, which the failure path has to close as well
	for mode in ['', 'redirect'] {
		code, output := run_helper(exe, mode)
		assert code == 1, 'mode `${mode}`: exit code ${code}, output: ${output}'
		assert output.contains('cleanup-ran'), 'mode `${mode}`: the at_exit callback did not report its command: ${output}'
		assert !output.contains('not reached'), output
	}
}
