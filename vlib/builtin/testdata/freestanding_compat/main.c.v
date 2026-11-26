module main

import os

#include "@DIR/probe.h"
#flag @DIR/probe.c
#flag @DIR/stdio_probe.c
#flag @DIR/mutex_probe.c
#flag @DIR/dns_fuzz.c
#flag @DIR/fontstack_probe.c

fn C.v_freestanding_compat_check(directory &char) i32
fn C.v_freestanding_compat_increment()
fn C.v_freestanding_compat_total() i32
fn C.v_freestanding_stdio_check(directory &char) i32
fn C.v_freestanding_stdio_exit_output(explicit_exit i32)
fn C.v_freestanding_mutex_check() i32
fn C.v_freestanding_dns_parser_fuzz() i32

fn C.v_freestanding_fontstack_check(&char) i32
fn C.v_freestanding_fontstack_environment() i32
fn C.v_freestanding_condition_worker()
fn C.v_freestanding_condition_release() i32

fn condition_worker() { C.v_freestanding_condition_worker() }

fn increment() {
	os.signal_ignore(.pipe)
	C.v_freestanding_compat_increment()
}

// Keep native ABI checks active in production builds.
fn require_compat(condition bool, detail string) {
	if !condition {
		eprintln(detail)
		exit(92)
	}
}

fn main() {
	if os.args.len == 3 {
		C.v_freestanding_stdio_exit_output(if os.args[2] == 'exit' { i32(1) } else { i32(0) })
		return
	}
	require_compat(os.args.len == 2, 'compatibility fixture check at line 45')
	checked := C.v_freestanding_compat_check(&char(os.args[1].str))
	if checked != 0 {
		eprintln('C compatibility probe failed at line ${checked}')
	}
	require_compat(checked == 0, checked.str())
	stdio := C.v_freestanding_stdio_check(&char(os.args[1].str))
	if stdio != 0 {
		eprintln('stdio probe failed at C line ${stdio}')
	}
	require_compat(stdio == 0, 'compatibility fixture check at line 55')
	mutex := C.v_freestanding_mutex_check()
	if mutex != 0 {
		eprintln('mutex probe failed at C line ${mutex}')
	}
	require_compat(mutex == 0, 'compatibility fixture check at line 60')
	fuzz := C.v_freestanding_dns_parser_fuzz()
	if fuzz != 0 {
		eprintln('DNS parser guard-page fuzz failed at C line ${fuzz}')
	}
	require_compat(fuzz == 0, 'compatibility fixture check at line 65')
	mut workers := []thread{}
	for _ in 0 .. 8 {
		workers << spawn increment()
	}
	workers.wait()
	require_compat(C.v_freestanding_compat_total() == 80000, 'compatibility fixture check at line 71')
	fontstack := C.v_freestanding_fontstack_check(&char(os.args[1].str))
	require_compat(fontstack == 0, 'font stack probe failed at C line ${fontstack}')
	require_compat(os.getenv('V_NATIVE_ENV_INTEROP') == 'C-side', 'compatibility fixture check at line 74')
	os.setenv('V_NATIVE_ENV_INTEROP', 'V-side', true)
	require_compat(C.v_freestanding_fontstack_environment() == 1, 'compatibility fixture check at line 76')
	workers.clear()
	for _ in 0 .. 8 { workers << spawn condition_worker() }
	condition := C.v_freestanding_condition_release()
	require_compat(condition == 0, 'condition probe failed at C line ${condition}')
	workers.wait()
	mut process := os.new_process('/bin/sh')
	process.set_args(['-c', 'printf child-output; printf child-error >&2'])
	process.set_redirect_stdio()
	process.run()
	process.wait()
	require_compat(process.is_pending(.stdout), 'compatibility fixture check at line 87')
	require_compat(process.stdout_read() == 'child-output', 'compatibility fixture check at line 88')
	require_compat(process.is_pending(.stderr), 'compatibility fixture check at line 89')
	require_compat(process.stderr_read() == 'child-error', 'compatibility fixture check at line 90')
	process.close()
	println('freestanding C compatibility ok')
}
