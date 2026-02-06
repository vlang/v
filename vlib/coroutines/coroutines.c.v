// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module coroutines

import v.util
import time

#flag -I @VEXEROOT/thirdparty/photon
#flag @VEXEROOT/thirdparty/photon/photonwrapper.so
#include "photonwrapper.h"

$if windows {
	#include "processthreadsapi.h"
} $else {
	#include <pthread.h>
}
#flag -I @VEXEROOT/vlib/coroutines
#include "sp_corrector.c"

fn C.set_photon_thread_stack_allocator(fn (voidptr, int) voidptr, fn (voidptr, voidptr, int))

// fn C.default_photon_thread_stack_alloc(voidptr, int) voidptr
// fn C.default_photon_thread_stack_dealloc(voidptr, voidptr, int)
fn C.new_photon_work_pool(int) voidptr
fn C.delete_photon_work_pool()
fn C.init_photon_work_pool(int)
fn C.photon_set_log_output_stdout()
fn C.photon_set_log_output_stderr()
fn C.photon_set_log_output_null()
fn C.photon_join_current_thread_into_workpool() int
fn C.photon_thread_create_and_migrate_to_work_pool(f voidptr, arg voidptr)
fn C.photon_thread_create(f voidptr, arg voidptr, stack_size u64)
fn C.photon_thread_migrate()

// fn C.photon_thread_migrate(work_pool voidptr)
fn C.photon_init_default() int
fn C.photon_sleep_s(n int)
fn C.photon_sleep_ms(n int)

fn C.sp_corrector(voidptr, voidptr)

// sleep is coroutine-safe version of time.sleep()
pub fn sleep(duration time.Duration) {
	C.photon_sleep_ms(duration.milliseconds())
}

fn alloc(_ voidptr, stack_size int) voidptr {
	unsafe {
		stack_ptr := malloc(stack_size)

		$if gcboehm ? {
			// TODO: this wont work if a coroutine gets moved to a different
			// thread, so we are using `C.GC_set_sp_corrector` with our own
			// corrector function which seems to be the best solution for now.
			// It would probably be more performant if we could hook into photon's context
			// switching code (currently not possible) or we write our own implementation.
			// TODO: update - I'm not sure if what I wrote above is correct. test
			// C.GC_set_stackbottom(0, stack_ptr)

			C.GC_add_roots(stack_ptr, charptr(stack_ptr) + stack_size)
		}

		return stack_ptr
	}
}

fn dealloc(_ voidptr, stack_ptr voidptr, stack_size int) {
	unsafe {
		$if gcboehm ? {
			C.GC_remove_roots(stack_ptr, charptr(stack_ptr) + stack_size)
		}
		free(stack_ptr)
	}
}

fn init_photon_vcpu() {
	C.photon_init_default()
	$if gcboehm ? {
		mut sb := C.GC_stack_base{}
		C.GC_get_stack_base(&sb)
		C.GC_register_my_thread(&sb)
	}
	C.photon_join_current_thread_into_workpool()
	$if gcboehm ? {
		C.GC_unregister_my_thread()
	}
}

fn init() {
	$if gcboehm ? {
		// NOTE `sp_corrector` only works for platforms with the stack growing down
		// MacOs, Win32 and Linux always have stack growing down.
		// A proper solution is planned (hopefully) for boehm v8.4.0.
		C.GC_set_sp_corrector(C.sp_corrector)
		if C.GC_get_sp_corrector() == unsafe { nil } {
			panic('stack pointer correction unsupported')
		}
	}
	C.photon_set_log_output_null()
	C.set_photon_thread_stack_allocator(alloc, dealloc)
	ret := C.photon_init_default()

	if util.nr_jobs >= 1 {
		// automatic
		// C.init_photon_work_pool(util.nr_jobs)
		// manual - pass 0 because we will start our own
		C.init_photon_work_pool(0)
		// start our own vcpu's manually
		for _ in 1 .. util.nr_jobs {
			spawn init_photon_vcpu()
		}
	}

	if ret < 0 {
		panic_n('failed to initialize coroutines via photon ret:', ret)
	}
}
