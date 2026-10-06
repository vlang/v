import os

const child_source = r'
module main

import os

struct Item {
	value int
}

fn recurse(depth int) int {
	mut buf := [64]int{}
	buf[depth % 64] = depth
	return recurse(depth + 1) + buf[(depth + 1) % 64]
}

fn main() {
	mode := if os.args.len > 1 { os.args[1] } else { "main" }
	match mode {
		"thread" {
			t := spawn recurse(0)
			println(t.wait())
		}
		"nil" {
			p := unsafe { &Item(nil) }
			println(p.value)
		}
		else {
			println(recurse(0))
		}
	}
}
'

// The child of test_previous_handler_keeps_its_signal_mask installs a SIGSEGV/SIGBUS
// handler before V installs its own, from a C constructor.
const previous_handler_header = r'
#include <signal.h>
#include <string.h>
#include <unistd.h>

// It stands for a handler that relies on its sa_mask, like the write fault handler of
// the Boehm GC, that blocks the GC suspend signal.
static void v_test_previous_handler(int sig) {
	(void)sig;
	sigset_t blocked;
	sigemptyset(&blocked);
	sigprocmask(SIG_BLOCK, NULL, &blocked);
	const char* msg = sigismember(&blocked, SIGUSR1) == 1 ? "previous handler: SIGUSR1 blocked\n"
		: "previous handler: SIGUSR1 not blocked\n";
	write(2, msg, strlen(msg));
	_exit(0);
}

__attribute__((constructor)) static void v_test_install_previous_handler(void) {
	struct sigaction sa;
	memset(&sa, 0, sizeof(sa));
	sa.sa_handler = v_test_previous_handler;
	sigemptyset(&sa.sa_mask);
	sigaddset(&sa.sa_mask, SIGUSR1);
	sigaction(SIGSEGV, &sa, NULL);
	sigaction(SIGBUS, &sa, NULL);
}

// The TCC `-bt` runtime installs its own handlers from a constructor, that runs after
// the one above, so V then chains to those instead.
static int v_test_previous_handler_replaced(void) {
#if defined(__TINYC__) && !defined(CUSTOM_DEFINE_no_backtrace)
	return 1;
#else
	return 0;
#endif
}
'

const previous_handler_child_source = r'
module main

#include "@DIR/previous_handler.h"

fn C.v_test_previous_handler_replaced() int

struct Item {
	value int
}

fn main() {
	if C.v_test_previous_handler_replaced() != 0 {
		println("previous handler replaced")
		return
	}
	p := unsafe { &Item(nil) }
	println(p.value)
}
'

fn run_child(binary string, mode string) os.Result {
	// The messages go to stderr, so merge it into the captured output.
	return os.exec(['/bin/sh', '-c', '${os.quoted_path(binary)} ${mode} 2>&1'])
}

fn test_stack_overflow_prints_a_message() {
	$if windows {
		return
	}
	work_dir := os.join_path(os.vtmp_dir(), 'stack_overflow_test_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	source := os.join_path(work_dir, 'child.v')
	os.write_file(source, child_source)!
	for build_mode in ['normal', 'parallel'] {
		binary := os.join_path(work_dir, 'child_${build_mode}')
		mut flags := [@VEXE]
		if build_mode == 'parallel' {
			flags << ['-parallel-cc', '-cc', 'cc', '-showcc', '-nocache']
		} else {
			$if macos {
				// Darwin leaves TCC's preinstalled backtrace handlers in control. Test the
				// overflow reporter with default signal dispositions instead.
				flags << ['-cc', 'clang']
			}
		}
		flags << ['-o', binary, source]
		compile := os.exec(flags)
		assert compile.exit_code == 0, '${build_mode}: ${compile.output}'
		if build_mode == 'parallel' {
			assert compile.output.contains('unit_0.c'), compile.output
			assert compile.output.contains('unit_1.c'), compile.output
		}
		for mode in ['main', 'thread'] {
			res := run_child(binary, mode)
			assert res.exit_code != 0, '${build_mode}/${mode}: ${res.output}'
			assert res.output.contains('V panic: stack overflow'), '${build_mode}/${mode}: ${res.output}'
		}
		// Other faults keep their message: V's segmentation fault message, or the one of
		// the TCC `-bt` runtime, that handled them before.
		res := run_child(binary, 'nil')
		assert res.exit_code != 0, '${build_mode}/nil: ${res.output}'
		assert res.output.contains('segmentation fault')
			|| res.output.contains('invalid memory access'), '${build_mode}/nil: ${res.output}'
		assert !res.output.contains('stack overflow'), '${build_mode}/nil: ${res.output}'
	}
}

// V's handler calls the handler that was installed before it for any fault that is not
// a stack overflow, so it has to block the signals that the previous action blocks.
fn test_previous_handler_keeps_its_signal_mask() {
	$if windows {
		return
	}
	work_dir := os.join_path(os.vtmp_dir(), 'stack_overflow_previous_handler_test_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	source := os.join_path(work_dir, 'child.v')
	binary := os.join_path(work_dir, 'child')
	os.write_file(os.join_path(work_dir, 'previous_handler.h'), previous_handler_header)!
	os.write_file(source, previous_handler_child_source)!
	compile := os.exec([@VEXE, '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	res := run_child(binary, '')
	if res.output.contains('previous handler replaced') {
		return
	}
	assert res.output.contains('previous handler: SIGUSR1 blocked'), res.output
}

fn test_previous_one_shot_handler_is_consumed() ! {
	$if windows || vinix || freestanding {
		return
	}
	work_dir := os.join_path(os.vtmp_dir(), 'stack_overflow_one_shot_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	header := r'
#include <signal.h>
#include <unistd.h>
static void v_test_one_shot(int sig) {
	(void)sig;
	write(2, "one shot handler\n", 17);
}

__attribute__((constructor)) static void v_test_install_one_shot(void) {
	struct sigaction sa = {0};
	sa.sa_handler = v_test_one_shot;
	sa.sa_flags = SA_RESETHAND;
	sigemptyset(&sa.sa_mask);
	sigaction(SIGBUS, &sa, NULL);
}
static void v_test_raise_bus(void) { raise(SIGBUS); }
'
	os.write_file(os.join_path(work_dir, 'one_shot.h'), header)!
	source := os.join_path(work_dir, 'child.c.v')
	binary := os.join_path(work_dir, 'child')
	os.write_file(source, r'
module main
#include "@DIR/one_shot.h"
fn C.v_test_raise_bus()
fn main() {
	C.v_test_raise_bus()
	C.v_test_raise_bus()
	println("unexpected survival")
}
')!
	// Clang leaves the constructor installed; TCC backtrace mode replaces it.
	compile := os.exec([@VEXE, '-cc', 'clang', '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	result := run_child(binary, '')
	assert result.exit_code != 0, result.output
	assert result.output.count('one shot handler') == 1, result.output
	assert !result.output.contains('unexpected survival'), result.output
}

fn test_previous_persistent_handler_can_return_twice() ! {
	$if windows || vinix || freestanding {
		return
	}
	work_dir := os.join_path(os.vtmp_dir(), 'stack_overflow_persistent_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	header := r'
#include <signal.h>
#include <unistd.h>
static void v_test_persistent(int sig) {
	(void)sig;
	write(2, "persistent handler\n", 19);
}
__attribute__((constructor)) static void v_test_install_persistent(void) {
	struct sigaction sa = {0};
	sa.sa_handler = v_test_persistent;
	sigemptyset(&sa.sa_mask);
	sigaction(SIGBUS, &sa, NULL);
}
static void v_test_raise_bus(void) { raise(SIGBUS); }
static int v_test_persistent_has_precedence(void) {
#if defined(__APPLE__)
	struct sigaction sa;
	return sigaction(SIGBUS, NULL, &sa) == 0 && sa.sa_handler == v_test_persistent;
#else
	return 1;
#endif
}
'
	os.write_file(os.join_path(work_dir, 'persistent.h'), header)!
	source := os.join_path(work_dir, 'child.c.v')
	binary := os.join_path(work_dir, 'child')
	os.write_file(source, r'
module main
#include "@DIR/persistent.h"
fn C.v_test_raise_bus()
fn C.v_test_persistent_has_precedence() int
fn main() {
	assert C.v_test_persistent_has_precedence() == 1
	C.v_test_raise_bus()
	C.v_test_raise_bus()
	println("survived")
}
')!
	compile := os.exec([@VEXE, '-cc', 'clang', '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	result := run_child(binary, '')
	assert result.exit_code == 0, result.output
	assert result.output.count('persistent handler') == 2, result.output
	assert result.output.contains('survived'), result.output
}
