// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module coroutines

import v.util
import time

#flag -I @VEXEROOT/thirdparty/photon
#flag @VEXEROOT/thirdparty/photon/photonwrapper.so

#include "photonwrapper.h"

// struct C.WorkPool {}
// fn C.new_photon_work_pool) C.WorkPool
fn C.init_photon_work_pool(int)

// fn C.photon_thread_create_and_migrate_to_work_pool(f voidptr, arg voidptr)
fn C.photon_thread_create(f voidptr, arg voidptr)
fn C.photon_init_default() int
fn C.photon_sleep_s(n int)
fn C.photon_sleep_ms(n int)
fn C.set_photon_thread_stack_allocator(fn (voidptr, int) voidptr, fn (voidptr, voidptr, int))

// sleep is coroutine-safe version of time.sleep()
pub fn sleep(duration time.Duration) {
	C.photon_sleep_ms(duration.milliseconds())
}

fn init() {
	alloc := fn (_ voidptr, stack_size int) voidptr {
		unsafe {
			stack_ptr := malloc(stack_size)
			$if gcboehm ? {
				C.GC_add_roots(stack_ptr, charptr(stack_ptr) + stack_size)
			}
			return stack_ptr
		}
	}
	dealloc := fn (_ voidptr, stack_ptr voidptr, stack_size int) {
		unsafe {
			$if gcboehm ? {
				C.GC_remove_roots(stack_ptr, charptr(stack_ptr) + stack_size)
			}
			free(stack_ptr)
		}
	}
	C.set_photon_thread_stack_allocator(alloc, dealloc)
	ret := C.photon_init_default()
	if util.nr_jobs > 0 {
		C.init_photon_work_pool(util.nr_jobs)
	}
	if ret < 0 {
		panic('failed to initialize coroutines via photon (ret=${ret})')
	}
}
