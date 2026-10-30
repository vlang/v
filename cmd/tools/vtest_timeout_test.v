import os
import time

const vexe = @VEXE
const timeout_root = os.join_path(os.vtmp_dir(), 'vtest_timeout_${os.getpid()}')

// The test program: counts its runs in `runs.txt`, starts a child that outlives it, writes the
// child's pid to `child.pid`, and then hangs.
const hanging_test_source = "import os
import time

fn test_hangs_with_a_child_process() {
	mut runs := os.open_append(os.join_path(os.dir(@FILE), 'runs.txt'))!
	runs.write_string('x')!
	runs.close()
	mut child := os.new_process('/bin/sleep')
	child.set_args(['600'])
	child.run()
	os.write_file(os.join_path(os.dir(@FILE), 'child.pid'), child.pid.str())!
	time.sleep(600 * time.second)
}
"

fn testsuite_end() {
	os.rmdir_all(timeout_root) or {}
}

fn new_hanging_test(name string) string {
	dir := os.join_path(timeout_root, name)
	os.mkdir_all(dir) or { panic(err) }
	file := os.join_path(dir, 'hang_test.v')
	os.write_file(file, hanging_test_source) or { panic(err) }
	return file
}

// process_is_alive is false for a process that is gone, and for one that is dead and waits to be
// reaped by its new parent (state Z), which a killed process tree leaves until init collects it.
fn process_is_alive(pid int) bool {
	result := os.exec(['ps', '-o', 'stat=', '-p', pid.str()])
	state := result.output.trim_space()
	return result.exit_code == 0 && state != '' && !state.starts_with('Z')
}

fn run_v_test(args []string, env map[string]string) (os.Result, time.Duration) {
	mut cmd_args := [vexe, '-new-compiler', '-silent', 'test']
	cmd_args << args
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VJOBS', '1', true)
	os.unsetenv('VTEST_TIMEOUT')
	os.unsetenv('VTEST_RETRY_TIMEOUT')
	for key, value in env {
		os.setenv(key, value, true)
	}
	defer {
		os.unsetenv('VTEST_TIMEOUT')
	}
	sw := time.new_stopwatch()
	result := os.exec(cmd_args)
	return result, sw.elapsed()
}

fn test_a_hanging_test_fails_by_name_within_the_limit() {
	$if windows {
		return
	}
	file := new_hanging_test('limit_option')
	result, elapsed := run_v_test(['-timeout', '3', file], {})
	assert result.exit_code == 1, result.output
	assert result.output.contains('FAIL'), result.output
	assert result.output.contains('hang_test.v'), result.output
	assert result.output.contains('timed out after 3 s'), result.output
	// The compile takes a few seconds; the hang itself would take ten minutes.
	assert elapsed < 120 * time.second, '${elapsed}'
	// The run was not repeated: a test that printed nothing used to be retried.
	runs := os.read_file(os.join_path(os.dir(file), 'runs.txt'))!
	assert runs == 'x', 'the test ran ${runs.len} times'
	assert !result.output.contains('retrying'), result.output
	// Nothing of the process tree is left behind.
	child_pid := os.read_file(os.join_path(os.dir(file), 'child.pid'))!.int()
	assert child_pid > 0
	assert !process_is_alive(child_pid), 'the child ${child_pid} of the test is still running'
}

fn test_the_limit_can_come_from_the_environment() {
	$if windows {
		return
	}
	file := new_hanging_test('limit_env')
	result, _ := run_v_test([file], {
		'VTEST_TIMEOUT': '2'
	})
	assert result.exit_code == 1, result.output
	assert result.output.contains('timed out after 2 s'), result.output
}

fn test_a_timeout_is_retried_only_when_asked_for() {
	$if windows {
		return
	}
	file := new_hanging_test('retry_timeouts')
	result, _ := run_v_test(['-timeout', '2', '-retry-timeouts', file], {})
	assert result.exit_code == 1, result.output
	assert result.output.contains('timed out after 2 s'), result.output
	runs := os.read_file(os.join_path(os.dir(file), 'runs.txt'))!
	assert runs.len > 1, 'with -retry-timeouts the test ran ${runs.len} time'
}
