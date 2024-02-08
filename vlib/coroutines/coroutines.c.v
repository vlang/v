// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module coroutines

import v.util
import time

#flag -I @VEXEROOT/thirdparty/photon
#flag @VEXEROOT/thirdparty/photon/photonwrapper.so

#include "photonwrapper.h"

fn C.set_photon_thread_stack_allocator(fn (voidptr, int) voidptr, fn (voidptr, voidptr, int))
fn C.default_photon_thread_stack_alloc(voidptr, int) voidptr
fn C.default_photon_thread_stack_dealloc(voidptr, voidptr, int)
fn C.new_photon_work_pool(int) voidptr
fn C.delete_photon_work_pool()
fn C.init_photon_work_pool(int)
fn C.init_photon_manual(int, fn ())
fn C.init_photon_manual2(fn (), fn ())
fn C.photon_thread_create_and_migrate_to_work_pool(f voidptr, arg voidptr)
fn C.photon_thread_create(f voidptr, arg voidptr)
fn C.photon_thread_migrate()

// fn C.photon_thread_migrate(work_pool voidptr)
fn C.photon_init_default() int

fn C.photon_sleep_s(n int)
fn C.photon_sleep_ms(n int)

// sleep is coroutine-safe version of time.sleep()
pub fn sleep(duration time.Duration) {
	C.photon_sleep_ms(duration.milliseconds())
}

fn alloc(_ voidptr, stack_size int) voidptr {
	// println('## alloc called')
	unsafe {
		// thread_id := C.pthread_self()
		// println('## Thread ID: $thread_id')

		// $if gcboehm ? {
		// 	mut sb := C.GC_stack_base{}
		// 	C.GC_get_stack_base(&sb)
		// 	C.GC_register_my_thread(&sb)
		// 	// res = C.GC_register_my_thread(&sb)
		// 	// println('## RES: $res')
		// }

		// NOTE: when using malloc (GC_MALLOC) we get a segfault
		// when migrating from a new thread to a work pool thread
		// stack_ptr := malloc(stack_size)
		// stack_ptr := C.malloc(stack_size)
		stack_ptr := C.default_photon_thread_stack_alloc(nil, stack_size)

		$if gcboehm ? {
			C.GC_add_roots(stack_ptr, charptr(stack_ptr) + stack_size)
		}

		return stack_ptr
	}
}

fn dealloc(_ voidptr, stack_ptr voidptr, stack_size int) {
	// println('## dealloc called')
	unsafe {
		$if gcboehm ? {
			// C.GC_unregister_my_thread()
			C.GC_remove_roots(stack_ptr, charptr(stack_ptr) + stack_size)
		}
		// free(stack_ptr)
		C.default_photon_thread_stack_dealloc(nil, stack_ptr, stack_size)
	}
}

fn init() {
	C.set_photon_thread_stack_allocator(alloc, dealloc)
	ret := C.photon_init_default()

	if util.nr_jobs >= 1 {
		C.init_photon_work_pool(util.nr_jobs)
	}

	if ret < 0 {
		panic('failed to initialize coroutines via photon (ret=${ret})')
	}
}
