// Regression test for the hang in issue #28896. On Windows, Boehm's default
// fatal-error handler shows a modal message box and waits for someone to click
// it, so a console program or a test run stops instead of failing. V installs
// its own handler, which reports the error on stderr like other platforms do.
//
// Each child is run with a deadline, so a modal dialog shows up as a timeout
// failure rather than a hung test.
import os
import time

const vexe = @VEXE

const child_deadline = 60 * time.second

const abort_message = 'Invalid pointer passed to free()'

fn test_boehm_abort_is_reported_on_stderr_without_blocking() {
	dir := os.join_path(os.vtmp_dir(), 'v_gc_abort_stderr_${os.getpid()}')
	defer {
		os.rmdir_all(dir) or {}
	}
	exe := build_gc_abort_child(dir) or { return }

	mut child := os.new_process(exe)
	child.set_redirect_stdio()
	child.run()
	started := time.now()
	for child.is_alive() {
		if time.since(started) > child_deadline {
			child.signal_kill()
			child.wait()
			assert false, 'the Boehm abort blocked for more than ${child_deadline} (a modal dialog?)'
		}
		time.sleep(50 * time.millisecond)
	}
	child.wait()
	stdout := child.stdout_slurp()
	stderr := child.stderr_slurp()
	child.close()
	assert child.code != 0, 'the child exited cleanly; stdout: ${stdout}'
	assert !stdout.contains('not reached')
	assert stderr.contains(abort_message), stderr
}

// With GC_LOOP_ON_ABORT set, Boehm's handler spins forever after a fatal error,
// so that a debugger can be attached. Like Boehm's own handler, V's must print
// the message before that, not after. The spinning child is killed once the
// message shows up, or at the deadline.
fn test_boehm_abort_is_reported_before_gc_loop_on_abort() {
	dir := os.join_path(os.vtmp_dir(), 'v_gc_abort_loop_${os.getpid()}')
	defer {
		os.rmdir_all(dir) or {}
	}
	exe := build_gc_abort_child(dir) or { return }

	mut env := os.environ()
	env['GC_LOOP_ON_ABORT'] = '1'
	mut child := os.new_process(exe)
	child.set_redirect_stdio()
	child.set_environment(env)
	child.run()
	started := time.now()
	mut stderr := ''
	for child.is_alive() && !has_abort_line(stderr) && time.since(started) < child_deadline {
		stderr += child.stderr_read()
		time.sleep(50 * time.millisecond)
	}
	if child.is_alive() {
		child.signal_kill()
	}
	child.wait()
	stderr += child.stderr_slurp()
	child.close()
	// Boehm itself may log a similar line with the pointer appended first; the
	// handler's line is the bare message.
	assert has_abort_line(stderr), 'no `${abort_message}` line within ${child_deadline / time.second} s with GC_LOOP_ON_ABORT set; stderr: ${stderr}'
}

fn has_abort_line(stderr string) bool {
	return stderr.split_into_lines().any(it.trim_space() == abort_message)
}

// build_gc_abort_child builds, in `dir`, a program that makes Boehm abort
// deterministically: under `-gc boehm_leak`, GC_FREE of a pointer Boehm does
// not own aborts with "Invalid pointer passed to free()". It returns none when
// no Boehm library is available.
fn build_gc_abort_child(dir string) ?string {
	os.mkdir_all(dir) or { panic(err) }
	source := os.join_path(dir, 'gc_abort_child.v')
	os.write_file(source, [
		'fn C.GC_FREE(voidptr)',
		'',
		'fn main() {',
		'\tmut not_heap := 0',
		'\tunsafe { C.GC_FREE(voidptr(&not_heap)) }',
		"\tprintln('not reached')",
		'}',
	].join('\n')) or { panic(err) }
	mut exe := os.join_path(dir, 'gc_abort_child')
	$if windows {
		exe += '.exe'
	}
	build := os.execute('${os.quoted_path(vexe)} -gc boehm_leak -o ${os.quoted_path(exe)} ${os.quoted_path(source)}')
	if build.exit_code != 0 && build.output.contains('libgc') {
		eprintln('skipping: no Boehm GC library available\n${build.output}')
		return none
	}
	assert build.exit_code == 0, build.output
	return exe
}
