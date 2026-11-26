import os
import time

// What interrupting `v test` itself does to the tests it runs, and to its exit status. A test
// under a limit is in a process group of its own, which the runner's signal does not reach
// (the limit itself is in vtest_timeout_test.v).
const vexe = @VEXE
const interrupt_root = os.join_path(os.vtmp_dir(), 'vtest_interrupt_${os.getpid()}')

fn testsuite_end() {
	os.rmdir_all(interrupt_root) or {}
}

// process_is_alive is false for a process that is gone, and for one that is dead and waits to be
// reaped by its new parent (state Z), which a killed process tree leaves until init collects it.
fn process_is_alive(pid int) bool {
	result := os.exec(['ps', '-o', 'stat=', '-p', pid.str()])
	state := result.output.trim_space()
	return result.exit_code == 0 && state != '' && !state.starts_with('Z')
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
	dir := os.join_path(interrupt_root, 'interrupted')
	os.mkdir_all(dir)!
	file := os.join_path(dir, 'interrupted_test.v')
	os.write_file(file, interrupted_test_source)!
	os.setenv('VCOLORS', 'never', true)
	os.setenv('VJOBS', '1', true)
	os.unsetenv('VTEST_ONLY_FN')
	os.unsetenv('VTEST_TIMEOUT')
	os.unsetenv('VTEST_RETRY_TIMEOUT')
	mut runner := os.new_process(vexe)
	runner.set_args(['-silent', 'test', file])
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
	dir := os.join_path(interrupt_root, 'spawn_held')
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
	runner.set_args(['-silent', 'test', file])
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

// A test that interrupts the runner that started it, and passes.
const interrupts_its_runner_source = "import os

fn test_interrupts_its_runner_and_passes() {
	result := os.exec(['/bin/kill', '-INT', os.getppid().str()])
	assert result.exit_code == 0
}
"

// An interrupt that arrives as the last test ends is still an interrupt: the runner was told
// to stop, and its exit status says so, whichever of its threads sees it first and however
// little was left to do. The test passes, so a runner that got to its ordinary end exits 0.
fn test_an_interrupt_as_the_last_test_ends_decides_the_exit_status() {
	$if windows {
		return
	}
	dir := os.join_path(interrupt_root, 'interrupt_at_the_end')
	os.mkdir_all(dir)!
	file := os.join_path(dir, 'interrupts_its_runner_test.v')
	os.write_file(file, interrupts_its_runner_source)!
	for attempt in 0 .. 6 {
		result, _ := run_v_test([file], {})
		assert result.exit_code == 128 + int(os.Signal.int), 'attempt ${attempt}: the interrupted runner exited ${result.exit_code}:\n${result.output}'
	}
}
