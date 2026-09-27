module sync

#insert "@VEXEROOT/vlib/sync/thread_helper.h"

fn C.v_sync_thread_create_detached(voidptr, voidptr) int

fn waitgroup_thread_entry(args_ptr voidptr) voidptr {
	args := unsafe { &WaitGroupThreadArgs(args_ptr) }
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer {
			unsafe {
				prealloc_scope_end(scope)
				prealloc_thread_cleanup()
			}
		}
	}
	args.f()
	mut wg := args.wg
	wg.done()
	free_waitgroup_thread_args(args)
	return unsafe { nil }
}

fn start_waitgroup_thread(mut wg WaitGroup, f fn ()) {
	args := new_waitgroup_thread_args(&wg, f)
	result := C.v_sync_thread_create_detached(voidptr(waitgroup_thread_entry), voidptr(args))
	if result != 0 {
		wg.done()
		free_waitgroup_thread_args(args)
		panic('could not start waitgroup task: error ${result}')
	}
}
