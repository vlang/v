module main

$if freestanding {
	$compile_error('-vlibc must preserve the regular V runtime')
}

$if gcboehm ? {
	#include <gc.h>
	fn C.GC_thread_is_registered() i32
}

fn require(condition bool) {
	if !condition { exit(94) }
}

$if vlibc ? {
	@[heap]
	struct State {
	mut:
		ready   u32
		release u32
		done    u32
		message string
	}

	fn worker(argument u64) {
		// The parent retains this borrowed state until join completes.
		mut state := unsafe { &State(argument) }
		$if gcboehm ? {
			require(C.GC_thread_is_registered() != 0)
		}
		text := 'worker allocation'.repeat(1024)
		vlibc_store_u32(&state.ready, 1)
		for vlibc_load_u32(&state.release) == 0 { vlibc_sleep_ns(1_000_000) }
		require(text.starts_with('worker allocation') && text.ends_with('worker allocation'))
		require(text.len == 17 * 1024 && state.message == 'borrowed parent state')
		unsafe { text.free() }
		vlibc_store_u32(&state.done, 1)
	}

	fn check_thread() {
		mut state := &State{ message: 'borrowed parent state'.clone() }
		mut thread, code := vlibc_thread_spawn(worker, u64(state))
		require(code == .enoerror)
		for vlibc_load_u32(&state.ready) == 0 { vlibc_sleep_ns(1_000_000) }
		$if gcboehm ? {
			// The worker's stack must remain registered during collection, not just
			// while it creates its first allocation.
			for _ in 0 .. 8 { gc_collect() }
		}
		vlibc_store_u32(&state.release, 1)
		require(vlibc_thread_join(mut thread) == .enoerror)
		require(vlibc_load_u32(&state.done) == 1)
		unsafe {
			state.message.free()
			free(state)
		}
	}
}

fn main() {
	$if gcboehm ? {
		println('gc=boehm')
	} $else $if vgc ? {
		println('gc=vgc')
	} $else {
		println('gc=none')
	}
	$if vlibc ? {
		require(vlibc_sys_getpid() > 0)
		check_thread()
		println('vlibc regular-runtime threads passed')
	}
}
