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
		mut cmd_args := [vexe, '-new-compiler', '-stats', '-silent', 'test']
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
	assert os.dir(cache) == os.dir(os.executable()), cache + ' / ' + os.executable()
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
		mut cmd_args := [vexe, '-new-compiler']
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
	$if windows {
		return
	}
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
			mut cmd_args := [vexe, '-new-compiler']
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

// A test that writes its own pid and the pid of a child that outlives it, and then hangs.
const interrupted_test_source = "import os
import time

fn test_waits_to_be_interrupted() {
	dir := os.dir(@FILE)
	os.write_file(os.join_path(dir, 'test.pid'), os.getpid().str())!
	mut child := os.new_process('/bin/sleep')
	child.set_args(['600'])
	child.run()
	os.write_file(os.join_path(dir, 'child.pid'), child.pid.str())!
	time.sleep(600 * time.second)
}
"

// wait_for_file returns the text of `path` once it is there and not empty, or '' after `seconds`.
fn wait_for_file(path string, seconds int) string {
	for _ in 0 .. seconds * 10 {
		text := os.read_file(path) or { '' }
		if text.len > 0 {
			return text
		}
		time.sleep(100 * time.millisecond)
	}
	return ''
}

// A test under a limit runs in a process group of its own, so the Ctrl-C that ends `v test`
// does not reach it: the runner has to end what it started. The runner here is in a group of its
// own, as a command in a terminal is, and the interrupt goes to that group, as Ctrl-C does.
fn test_interrupting_the_runner_ends_the_tests_it_started() {
	$if windows {
		return
	}
	dir := os.join_path(timeout_root, 'interrupted')
	os.mkdir_all(dir)!
	file := os.join_path(dir, 'interrupted_test.v')
	os.write_file(file, interrupted_test_source)!
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VJOBS', '1', true)
	os.unsetenv('VTEST_ONLY_FN')
	os.unsetenv('VTEST_TIMEOUT')
	os.unsetenv('VTEST_RETRY_TIMEOUT')
	mut runner := os.new_process(vexe)
	runner.set_args(['-new-compiler', '-silent', 'test', file])
	runner.use_pgroup = true
	runner.set_redirect_stdio()
	runner.run()
	child_pid := wait_for_file(os.join_path(dir, 'child.pid'), 180).int()
	test_pid := wait_for_file(os.join_path(dir, 'test.pid'), 5).int()
	defer {
		// whatever a failing run left behind
		for pid in [test_pid, child_pid] {
			if pid > 0 {
				os.exec(['/bin/kill', '-KILL', pid.str()])
			}
		}
		runner.signal_pgkill()
		runner.close()
	}
	assert child_pid > 0 && test_pid > 0, 'the test did not start: ${runner.stdout_slurp()}'
	assert process_is_alive(test_pid) && process_is_alive(child_pid)
	interrupt := os.exec(['/bin/kill', '-INT', '--', '-${runner.pid}'])
	assert interrupt.exit_code == 0, interrupt.output
	mut runner_gone := false
	for _ in 0 .. 100 {
		if !runner.is_alive() {
			runner_gone = true
			break
		}
		time.sleep(100 * time.millisecond)
	}
	assert runner_gone, 'the runner was still there ten seconds after its interrupt'
	// the runner is gone: nothing is left to enforce the limit, so nothing may be left running
	mut left := [test_pid, child_pid]
	for _ in 0 .. 30 {
		left = left.filter(process_is_alive(it))
		if left.len == 0 {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	assert left.len == 0, 'still running after the runner was interrupted: ${left} (the test is ${test_pid}, its child ${child_pid})'
}

// A fork() that holds the runner right after it has started the test: once the child of the
// fork has become the test program and the test has started its own child, it says so and
// waits to be released. The runner is then interrupted when it has a test running that it has
// not yet noted anywhere. Other forks (the compiler's) pass through. It changes no result.
const holding_fork_source = '#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

static void short_pause(void) {
	struct timespec pause = {0, 10000000};
	nanosleep(&pause, 0);
}

/* the path of the program that `pid` runs: the first string of its command line */
static int program_of(pid_t pid, char *program, size_t size) {
	char path[64];
	snprintf(path, sizeof(path), "/proc/%d/cmdline", (int)pid);
	int fd = open(path, O_RDONLY);
	if (fd < 0) {
		return 0;
	}
	ssize_t n = read(fd, program, size - 1);
	close(fd);
	if (n <= 0) {
		return 0;
	}
	program[n] = 0;
	return 1;
}

pid_t fork(void) {
	pid_t (*real_fork)(void) = dlsym(RTLD_NEXT, "fork");
	pid_t pid = real_fork();
	const char *hold = getenv("VTEST_SPAWN_HOLD");
	const char *name = getenv("VTEST_SPAWN_HOLD_NAME");
	const char *ready = getenv("VTEST_SPAWN_HOLD_READY");
	if (pid <= 0 || !hold || !name || !ready) {
		return pid;
	}
	char own[4096], other[4096];
	if (!program_of(getpid(), own, sizeof(own))) {
		return pid;
	}
	/* until the child has become another program, it has the command line of this one */
	int is_the_test = 0;
	for (int i = 0; i < 500; i++) {
		if (!program_of(pid, other, sizeof(other))) {
			return pid;
		}
		if (strcmp(own, other) != 0) {
			is_the_test = strstr(other, name) != 0;
			break;
		}
		short_pause();
	}
	if (!is_the_test) {
		return pid;
	}
	for (int i = 0; i < 6000 && access(ready, F_OK) != 0; i++) {
		short_pause();
	}
	char path[4096];
	snprintf(path, sizeof(path), "%s.started", hold);
	FILE *started = fopen(path, "w");
	if (started) {
		fputs("held", started);
		fclose(started);
	}
	snprintf(path, sizeof(path), "%s.release", hold);
	for (int i = 0; i < 60000 && access(path, F_OK) != 0; i++) {
		short_pause();
	}
	return pid;
}
'

// The runner can be interrupted when it has just started a test and not yet noted it: the
// test is running in its own process group already. It must be ended like any other.
fn test_interrupting_the_runner_while_it_starts_a_test_ends_that_test_too() {
	$if !linux {
		return
	}
	gcc := os.find_abs_path_of_executable('gcc') or {
		eprintln('skipping: no gcc')
		return
	}
	dir := os.join_path(timeout_root, 'spawn_held')
	os.mkdir_all(dir)!
	file := os.join_path(dir, 'spawn_held_test.v')
	os.write_file(file, interrupted_test_source)!
	shim_source := os.join_path(dir, 'holding_fork.c')
	shim := os.join_path(dir, 'holding_fork.so')
	os.write_file(shim_source, holding_fork_source)!
	shim_build := os.exec([gcc, '-shared', '-fPIC', '-o', shim, shim_source, '-ldl'])
	assert shim_build.exit_code == 0, shim_build.output
	hold := os.join_path(dir, 'hold')
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VJOBS', '1', true)
	os.unsetenv('VTEST_ONLY_FN')
	os.unsetenv('VTEST_TIMEOUT')
	os.unsetenv('VTEST_RETRY_TIMEOUT')
	os.setenv('VTEST_SPAWN_HOLD', hold, true)
	os.setenv('VTEST_SPAWN_HOLD_NAME', 'spawn_held_test', true)
	os.setenv('VTEST_SPAWN_HOLD_READY', os.join_path(dir, 'child.pid'), true)
	os.setenv('LD_PRELOAD', shim, true)
	mut runner := os.new_process(vexe)
	runner.set_args(['-new-compiler', '-silent', 'test', file])
	runner.use_pgroup = true
	runner.set_redirect_stdio()
	runner.run()
	os.unsetenv('LD_PRELOAD')
	os.unsetenv('VTEST_SPAWN_HOLD')
	os.unsetenv('VTEST_SPAWN_HOLD_NAME')
	os.unsetenv('VTEST_SPAWN_HOLD_READY')
	held := wait_for_file(hold + '.started', 240) == 'held'
	child_pid := wait_for_file(os.join_path(dir, 'child.pid'), 5).int()
	test_pid := wait_for_file(os.join_path(dir, 'test.pid'), 5).int()
	defer {
		os.write_file(hold + '.release', 'go') or {}
		for pid in [test_pid, child_pid] {
			if pid > 0 {
				os.exec(['/bin/kill', '-KILL', pid.str()])
			}
		}
		runner.signal_pgkill()
		runner.close()
	}
	assert held, 'the runner was not held after starting the test: ${runner.stdout_slurp()}'
	assert child_pid > 0 && test_pid > 0
	assert process_is_alive(test_pid) && process_is_alive(child_pid)
	interrupt := os.exec(['/bin/kill', '-INT', '--', '-${runner.pid}'])
	assert interrupt.exit_code == 0, interrupt.output
	// the interrupt has arrived while the runner is held; now the runner goes on
	time.sleep(300 * time.millisecond)
	os.write_file(hold + '.release', 'go')!
	mut runner_gone := false
	for _ in 0 .. 100 {
		if !runner.is_alive() {
			runner_gone = true
			break
		}
		time.sleep(100 * time.millisecond)
	}
	assert runner_gone, 'the runner was still there ten seconds after its interrupt'
	mut left := [test_pid, child_pid]
	for _ in 0 .. 30 {
		left = left.filter(process_is_alive(it))
		if left.len == 0 {
			break
		}
		time.sleep(100 * time.millisecond)
	}
	assert left.len == 0, 'still running after the runner was interrupted: ${left} (the test is ${test_pid}, its child ${child_pid})'
}
