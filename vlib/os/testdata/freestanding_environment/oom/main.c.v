module main

import os

#flag @DIR/limits.c
#include "@DIR/limits.h"

fn C.environment_fail_allocations() i32
fn C.environment_restore_allocations() i32

fn main() {
	// This payload remains live, so replacing the small existing entry needs
	// fresh backing storage after the kernel prohibits anonymous heap growth.
	payload := 'x'.repeat(32 * 1024 * 1024)
	initial := os.setenv('FENV_OOM', 'retained', true)
	if initial != 0 { panic('allocation failure setup failed') }
	if C.environment_fail_allocations() != 0 { panic('could not set child process limit') }
	status := os.setenv('FENV_OOM', payload, true)
	restored := C.environment_restore_allocations()
	if restored != 0 { panic('could not restore child process limit') }
	if status != -1 { panic('environment allocation failure was not reported') }
	if os.getenv('FENV_OOM') != 'retained' { panic('failed replacement changed the old entry') }
	if os.setenv('FENV_OOM', 'reused', true) != 0 || os.getenv('FENV_OOM') != 'reused' {
		panic('environment mutex or state did not recover after allocation failure')
	}
	println('environment allocation failure passed')
}
