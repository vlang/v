import os
import time

const vexe = @VEXE
const timeout_root = os.join_path(os.vtmp_dir(), 'vtest_timeout_${os.getpid()}')

// The program the test starts that outlives it: `/bin/sleep 600`, and on Windows `PING.EXE -n 600
// 127.0.0.1`, which waits as long.
const child_program = $if windows { r'C:\Windows\System32\PING.EXE' } $else { '/bin/sleep' }
const child_args = $if windows { "['-n', '600', '127.0.0.1']" } $else { "['600']" }

// The test program: counts its runs in `runs.txt`, starts a child that outlives it, writes the
// child's pid to `child.pid`, and then hangs.
fn hanging_test_source() string {
	return "import os
import time

fn test_hangs_with_a_child_process() {
	mut runs := os.open_append(os.join_path(os.dir(@FILE), 'runs.txt'))!
	runs.write_string('x')!
	runs.close()
	mut child := os.new_process(r'${child_program}')
	child.set_args(${child_args})
	child.run()
	os.write_file(os.join_path(os.dir(@FILE), 'child.pid'), child.pid.str())!
	time.sleep(600 * time.second)
}
"
}

// A test that would pass if it were left alone for forty seconds: with a limit shorter than that
// it is a timeout, however often it is run again.
const slow_test_source = "import os
import time

fn test_passes_after_forty_seconds() {
	mut runs := os.open_append(os.join_path(os.dir(@FILE), 'runs.txt'))!
	runs.write_string('x')!
	runs.close()
	time.sleep(40 * time.second)
}
"

fn testsuite_end() {
	os.rmdir_all(timeout_root) or {}
}

fn new_hanging_test(name string) string {
	dir := os.join_path(timeout_root, name)
	os.mkdir_all(dir) or { panic(err) }
	file := os.join_path(dir, 'hang_test.v')
	os.write_file(file, hanging_test_source()) or { panic(err) }
	return file
}

// process_is_alive is false for a process that is gone, and (not on Windows) for one that is dead
// and waits to be reaped by its new parent (state Z), which a killed process tree leaves until
// init collects it. On Windows `tasklist` lists the process by pid or says that there is none.
fn process_is_alive(pid int) bool {
	$if windows {
		result := os.exec(['tasklist', '/fi', 'PID eq ${pid}', '/fo', 'csv', '/nh'])
		return result.output.contains('"${pid}"')
	} $else {
		result := os.exec(['ps', '-o', 'stat=', '-p', pid.str()])
		state := result.output.trim_space()
		return result.exit_code == 0 && state != '' && !state.starts_with('Z')
	}
}

fn run_v_test(args []string, env map[string]string) (os.Result, time.Duration) {
	mut cmd_args := [vexe, '-silent', 'test']
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
	file := new_hanging_test('limit_env')
	result, _ := run_v_test([file], {
		'VTEST_TIMEOUT': '2'
	})
	assert result.exit_code == 1, result.output
	assert result.output.contains('timed out after 2 s'), result.output
}

fn test_a_timeout_is_retried_only_when_asked_for() {
	file := new_hanging_test('retry_timeouts')
	result, _ := run_v_test(['-timeout', '2', '-retry-timeouts', file], {})
	assert result.exit_code == 1, result.output
	assert result.output.contains('timed out after 2 s'), result.output
	runs := os.read_file(os.join_path(os.dir(file), 'runs.txt'))!
	assert runs.len > 1, 'with -retry-timeouts the test ran ${runs.len} time'
}

// With `-stats` the test is compiled and run by one command, and its retries are that command
// again. They are under the limit as the first run is: a retry left to finish reported a test
// that timed out as passed (exit code 0). Both ways of asking for the retry are held to it.
fn test_a_stats_retry_is_under_the_limit_too() {
	$if windows {
		return
	}
	routes := {
		'flags':       ['-timeout', '10', '-retry-timeouts']
		'environment': []string{}
	}
	for route, flags in routes {
		dir := os.join_path(timeout_root, 'stats_retry_${route}')
		os.mkdir_all(dir)!
		file := os.join_path(dir, 'slow_test.v')
		os.write_file(file, slow_test_source)!
		os.setenv('VCOLORS', 'never', true)
		os.setenv('VJOBS', '1', true)
		// the inner run is its own: not this file's function filter, not an outer limit
		os.unsetenv('VTEST_ONLY_FN')
		os.unsetenv('VTEST_TIMEOUT')
		os.unsetenv('VTEST_RETRY_TIMEOUT')
		if route == 'environment' {
			os.setenv('VTEST_TIMEOUT', '10', true)
			os.setenv('VTEST_RETRY_TIMEOUT', '1', true)
		}
		sw := time.new_stopwatch()
		mut cmd_args := [vexe, '-stats', '-silent', 'test']
		cmd_args << flags
		cmd_args << file
		result := os.exec(cmd_args)
		seconds := sw.elapsed().seconds()
		os.unsetenv('VTEST_TIMEOUT')
		os.unsetenv('VTEST_RETRY_TIMEOUT')
		assert result.exit_code == 1, '${route}, after ${seconds:.0f} s:\n${result.output}'
		assert result.output.contains('timed out after 10 s'), '${route}:\n${result.output}'
	}
}

// A test with a module cache of its own is told where it is through the environment of its
// commands (`V3CACHE`, in the folder its executable is built in). The run under a limit is one
// of those commands, with `-stats` and without.
const own_cache_test_source = "import os

fn test_the_cache_is_this_tests_own() {
	cache := os.getenv('V3CACHE')
	assert os.file_name(cache) == 'v3cache', cache
	assert os.real_path(os.dir(cache)) == os.dir(os.executable()), cache + ' / ' + os.executable()
}
"

fn test_a_limited_run_gets_the_per_test_environment() {
	$if windows {
		return
	}
	for mode in ['', '-stats'] {
		dir := os.join_path(timeout_root, 'own_env${mode}')
		os.mkdir_all(dir)!
		file := os.join_path(dir, 'own_cache_test.v')
		os.write_file(file, own_cache_test_source)!
		os.setenv('VCOLORS', 'never', true)
		os.setenv('VJOBS', '1', true)
		os.unsetenv('VTEST_ONLY_FN')
		os.unsetenv('VTEST_TIMEOUT')
		mut cmd_args := [vexe]
		if mode != '' {
			cmd_args << mode
		}
		cmd_args << ['-silent', 'test', '-timeout', '120', file]
		result := os.exec(cmd_args)
		assert result.exit_code == 0, 'mode `${mode}`:
${result.output}'
	}
}

// A test that may be retried twice: it fails an assertion, then hangs, then would pass. Each run
// appends its number to `runs.txt`.
const fail_hang_pass_test_source = "// vtest retry: 2
import os
import time

fn test_fails_then_hangs_then_passes() {
	file := os.join_path(os.dir(@FILE), 'runs.txt')
	attempt := (os.read_file(file) or { '' }).len
	mut runs := os.open_append(file)!
	runs.write_string(attempt.str())!
	runs.close()
	if attempt == 0 {
		assert false, 'an ordinary failure'
	}
	if attempt == 1 {
		time.sleep(120 * time.second)
	}
}
"

// A timeout is a timeout on whichever run it happens: when a retry of an ordinary failure runs
// past the limit, the test fails by name there, and the retries left do not get to turn it into
// a pass. With `-retry-timeouts` they do.
fn test_a_retry_that_times_out_is_not_retried_either() {
	for mode in ['', '-stats'] {
		// with `-stats` the limit covers the compilation too
		limit := if mode == '' { '3' } else { '15' }
		for opt_in in [false, true] {
			dir := os.join_path(timeout_root, 'fail_hang_pass${mode}_${opt_in}')
			os.mkdir_all(dir)!
			file := os.join_path(dir, 'fail_hang_pass_test.v')
			os.write_file(file, fail_hang_pass_test_source)!
			os.setenv('VCOLORS', 'never', true)
			os.setenv('VJOBS', '1', true)
			os.unsetenv('VTEST_ONLY_FN')
			os.unsetenv('VTEST_TIMEOUT')
			os.unsetenv('VTEST_RETRY_TIMEOUT')
			mut cmd_args := [vexe]
			if mode != '' {
				cmd_args << mode
			}
			cmd_args << ['-silent', 'test', '-timeout', limit]
			if opt_in {
				cmd_args << '-retry-timeouts'
			}
			cmd_args << file
			result := os.exec(cmd_args)
			runs := os.read_file(os.join_path(dir, 'runs.txt'))!
			what := 'mode `${mode}`, -retry-timeouts ${opt_in}, runs `${runs}`:\n${result.output}'
			if opt_in {
				assert result.exit_code == 0, what
				assert runs == '012', what
			} else {
				assert result.exit_code == 1, what
				assert result.output.contains('timed out after ${limit} s'), what
				assert runs == '01', what
				assert !result.output.contains('retrying 2/2'), what
			}
		}
	}
}
