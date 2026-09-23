// Regression test for the hang in issue #28896. On Windows, Boehm's default
// fatal-error handler shows a modal message box and waits for someone to click
// it, so a console program or a test run stops instead of failing. V installs
// its own handler, which reports the error on stderr like other platforms do.
//
// The child program triggers a deterministic Boehm abort: under
// `-gc boehm_leak`, GC_FREE of a pointer Boehm does not own aborts with
// "Invalid pointer passed to free()". The child is run with a deadline, so a
// modal dialog shows up as a timeout failure rather than a hung test.
import os
import time

const vexe = @VEXE

const child_deadline = 60 * time.second

fn test_boehm_abort_is_reported_on_stderr_without_blocking() {
	dir := os.join_path(os.vtmp_dir(), 'v_gc_abort_stderr_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
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
		return
	}
	assert build.exit_code == 0, build.output

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
	assert stderr.contains('Invalid pointer passed to free()'), stderr
}
