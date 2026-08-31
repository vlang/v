module time

#insert "@VEXEROOT/vlib/sync/thread_helper.h"

fn C.v_sync_thread_create_detached(voidptr, voidptr) int

fn timer_thread_entry(args_ptr voidptr) voidptr {
	args := unsafe { &TimerThreadArgs(args_ptr) }
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer {
			unsafe {
				prealloc_scope_end(scope)
				prealloc_thread_cleanup()
			}
		}
	}
	run_timer(args.duration, args.output, args.stop, args.done)
	free_timer_thread_args(args)
	return unsafe { nil }
}

fn start_timer(duration Duration, output chan Time, stop chan chan bool, done chan bool) {
	args := new_timer_thread_args(duration, output, stop, done)
	result := C.v_sync_thread_create_detached(voidptr(timer_thread_entry), voidptr(args))
	if result != 0 {
		free_timer_thread_args(args)
		panic('could not start timer: error ${result}')
	}
}
