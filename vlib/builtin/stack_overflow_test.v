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
	binary := os.join_path(work_dir, 'child')
	os.write_file(source, child_source)!
	compile := os.exec([@VEXE, '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	for mode in ['main', 'thread'] {
		res := run_child(binary, mode)
		assert res.exit_code != 0, '${mode}: ${res.output}'
		assert res.output.contains('V panic: stack overflow'), '${mode}: ${res.output}'
	}
	// Other faults keep their message: V's segmentation fault message, or the one of
	// the TCC `-bt` runtime, that handled them before.
	res := run_child(binary, 'nil')
	assert res.exit_code != 0, res.output
	assert res.output.contains('segmentation fault')
		|| res.output.contains('invalid memory access'), res.output
	assert !res.output.contains('stack overflow'), res.output
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
