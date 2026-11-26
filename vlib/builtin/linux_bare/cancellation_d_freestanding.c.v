@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_cancel_abi.h"
	fn C.v_cancel_implicit() uint
	fn C.v_cancel_implicit_capacity() uint
	fn C.v_cancel_asm(&u32, i64, uint, uint, uint, uint, uint, uint) i64
	fn C.v_cancel_window(uint)
	fn C.v_cancel_context_pc(uint, uint) uint
	fn C.v_cancel_handler_address() uint
	fn C.v_cancel_jump_sp(uint) uint
	fn C.v_cancel_buffer_get(uint, u32) uint
	fn C.v_cancel_buffer_set(uint, u32, uint)
	fn C.v_cancel_unwinder_available() i32
	fn C.v_cancel_force_unwind(uint)
	fn C.v_cancel_condition_fields(uint, uint)
	fn C.v_cancel_mutex_lock(uint) i32
	fn C.v_cancel_mutex_unlock(uint) i32

	// Atomic cancellation word: disabled=1, pending=2, asynchronous=4, exiting=8.
	// The syscall window's native mask ignores the asynchronous bit.
	struct BareCancelCleanup {
	mut:
		previous &BareCancelCleanup = unsafe { nil }
		function uint
		argument uint
	}
	__global bare_cancel_handler_state u32

	fn bare_cancel_unblock() {
		mask := u64(1) << 31
		sys_call4(u64(C.SYS_rt_sigprocmask), u64(C.SIG_UNBLOCK), u64(&mask), 0, 8)
	}
	fn bare_cancel_current() &BarePthread {
		mut thread := bare_pthread_current_get()
		if thread != unsafe { nil } { return thread }
		// Native TLS provides aligned storage without allocating inside pthread_self.
		// Check the carrier capacity before interpreting it as a V control block.
		if sizeof(BarePthread) > C.v_cancel_implicit_capacity() { bare_thread_fail() }
		thread = unsafe { &BarePthread(C.v_cancel_implicit()) }
		mut stack_pointer := uint(0)
		$if amd64 {
			asm volatile amd64 { mov stack_pointer, rsp
				; =r (stack_pointer) }
		} $else {
			asm volatile arm64 { mov stack_pointer, sp
				; =r (stack_pointer) }
		}
		thread.stack_sample = stack_pointer
		thread.tid = u32(sys_call0(u64(C.SYS_gettid)))
		thread.implicit = if thread.tid == u32(sys_call0(u64(C.SYS_getpid))) {
			u32(1)
		} else {
			u32(2)
		}
		thread.state = if thread.implicit == 1 { u32(1) } else { u32(2) }
		thread.ready = 1
		C.v_pthread_tls_store(0, uint(thread))
		bare_cancel_unblock()
		return thread
	}
	fn bare_cancel_word(mut thread BarePthread, set u32, clear u32) u32 {
		mut previous := bare_load_u32(&thread.cancel_word)
		for !bare_cas_u32(&thread.cancel_word, previous, (previous & ~clear) | set) {
			previous = bare_load_u32(&thread.cancel_word)
		}
		return previous
	}
	fn bare_cancel_mark_exiting(mut thread BarePthread) {
		bare_cancel_word(mut thread, 9, 0)
	}
	fn bare_cancel_enabled(word u32) bool { return word & 11 == 2 }
	fn bare_cancel_install() i32 {
		// pthread_cancel is async-cancel-safe: its one-time installation must not
		// leave a lock held if its caller already has an asynchronous request.
		mask := u64(1) << 31
		mut previous_mask := u64(0)
		masked := i64(sys_call4(u64(C.SYS_rt_sigprocmask), u64(C.SIG_BLOCK), u64(&mask), u64(&previous_mask), 8))
		if masked < 0 { return i32(-masked) }
		defer { sys_call4(u64(C.SYS_rt_sigprocmask), u64(C.SIG_SETMASK), u64(&previous_mask), 0, 8) }
		for {
			if bare_load_u32(&bare_cancel_handler_state) == 2 { return 0 }
			if bare_cas_u32(&bare_cancel_handler_state, 0, 1) {
				mut action := BareNativeSignalAction{ handler: C.v_cancel_handler_address(), flags: uint(C.SA_SIGINFO) }
				$if amd64 {
					action.flags |= 0x04000000
					action.restorer = C.v_signal_restorer()
				}
				result := i64(sys_call4(u64(C.SYS_rt_sigaction), 32, u64(&action), 0, 8))
				bare_store_u32(&bare_cancel_handler_state, if result == 0 { u32(2) } else { u32(0) })
				bf_futex_wake(&bare_cancel_handler_state, 0x7fffffff)
				return if result < 0 { i32(-result) } else { i32(0) }
			}
			bf_futex_wait(&bare_cancel_handler_state, 1, -1)
		}
		return 0
	}
	@[export: 'v_pthread_cancel'; markused]
	fn bare_pthread_cancel(handle uint) i32 {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		mut thread := unsafe { &BarePthread(handle) }
		// V-created workers retain V's result/mapping ownership. Their opaque
		// identity is not a joinable native pthread control block.
		if thread.implicit == 2 { return i32(C.ENOTSUP) }
		installed := bare_cancel_install()
		if installed != 0 { return installed }
		before := bare_cancel_word(mut thread, 2, 0)
		if before & 8 != 0 { return 0 }
		if handle == uint(bare_cancel_current()) {
			if before & 4 != 0 { bare_pthread_testcancel() }
			return 0
		}
		tid := bare_load_u32(&thread.tid)
		if tid == 0 { return 0 }
		result := i64(sys_call3(u64(C.SYS_tgkill), sys_call0(u64(C.SYS_getpid)), u64(tid), 32))
		return if result < 0 && result != -i64(C.ESRCH) { i32(-result) } else { i32(0) }
	}
	@[export: 'v_pthread_setcancelstate'; markused]
	fn bare_pthread_setcancelstate(state i32, output uint) i32 {
		if state != i32(C.PTHREAD_CANCEL_DISABLE) && state != i32(C.PTHREAD_CANCEL_ENABLE) {
			return i32(C.EINVAL)
		}
		mut thread := bare_cancel_current()
		before := bare_cancel_word(mut thread, if state == i32(C.PTHREAD_CANCEL_DISABLE) {
			u32(1)
		} else {
			u32(0)
		}, 1)
		if output != 0 {
			unsafe {
				*(&i32(output)) = if before & 1 != 0 {
					i32(C.PTHREAD_CANCEL_DISABLE)
				} else {
					i32(C.PTHREAD_CANCEL_ENABLE)
				}
			}
		}
		if state == i32(C.PTHREAD_CANCEL_ENABLE) && before & 4 != 0 { bare_pthread_testcancel() }
		return 0
	}
	@[export: 'v_pthread_setcanceltype'; markused]
	fn bare_pthread_setcanceltype(kind i32, output uint) i32 {
		if kind != i32(C.PTHREAD_CANCEL_DEFERRED) && kind != i32(C.PTHREAD_CANCEL_ASYNCHRONOUS) {
			return i32(C.EINVAL)
		}
		mut thread := bare_cancel_current()
		before := bare_cancel_word(mut thread, if kind == i32(C.PTHREAD_CANCEL_ASYNCHRONOUS) {
			u32(4)
		} else {
			u32(0)
		}, 4)
		if output != 0 {
			unsafe {
				*(&i32(output)) = if before & 4 != 0 {
					i32(C.PTHREAD_CANCEL_ASYNCHRONOUS)
				} else {
					i32(C.PTHREAD_CANCEL_DEFERRED)
				}
			}
		}
		if kind == i32(C.PTHREAD_CANCEL_ASYNCHRONOUS) { bare_pthread_testcancel() }
		return 0
	}
	@[export: 'v_pthread_testcancel'; markused]
	fn bare_pthread_testcancel() {
		mut thread := bare_cancel_current()
		if bare_cancel_enabled(bare_load_u32(&thread.cancel_word)) && thread.cancel_depth == 0 {
			bare_pthread_exit(~uint(0))
		}
	}
	@[export: 'v_cancel_is_pending'; markused]
	fn bare_cancel_is_pending() i32 {
		thread := bare_cancel_current()
		return i32(bare_cancel_enabled(bare_load_u32(&thread.cancel_word)))
	}
	@[export: 'v_cancel_signal'; markused]
	fn bare_cancel_signal(_ i32, _ uint, context uint) {
		saved := bf_native_errno_cell
		defer { bf_native_errno_cell = saved }
		thread := bare_pthread_current_get()
		if thread == unsafe { nil } { return }
		word := bare_load_u32(&thread.cancel_word)
		if !bare_cancel_enabled(word) { return }
		if word & 4 != 0 { bare_pthread_exit(~uint(0)) }
		mut window := [3]uint{}
		C.v_cancel_window(uint(&window[0]))
		pc := C.v_cancel_context_pc(context, 0)
		if pc >= window[0] && pc < window[1] { C.v_cancel_context_pc(context, window[2]) }
	}
	@[export: 'v_cancel_syscall'; markused]
	fn bare_cancel_syscall(number i64, a uint, b uint, c uint, d uint, e uint, f uint) i64 {
		mut thread := bare_cancel_current()
		result := C.v_cancel_asm(&thread.cancel_word, number, a, b, c, d, e, f)
		if result == -125 || result == -i64(C.EINTR) {
			if thread.cancel_depth == 0 { bare_pthread_testcancel() }
			return if result == -125 { -i64(C.EINTR) } else { result }
		}
		return result
	}
	@[export: 'v_cancel_shield_enter'; markused]
	fn bare_cancel_shield_enter() u32 {
		mut previous := i32(0)
		bare_pthread_setcancelstate(i32(C.PTHREAD_CANCEL_DISABLE), uint(&previous))
		return u32(previous)
	}
	@[export: 'v_cancel_shield_leave'; markused]
	fn bare_cancel_shield_leave(previous u32) {
		bare_pthread_setcancelstate(i32(previous), 0)
	}

	fn bare_cancel_join_release(argument uint) {
		mut thread := unsafe { &BarePthread(argument) }
		bare_store_u32(&thread.joiner, 0)
	}
	struct BareCancelCondition {
		waiters uint
		mutex   uint
	}
	fn bare_cancel_counter_add(address uint, change i32) {
		word := unsafe { &u32(address) }
		mut before := bare_load_u32(word)
		for !bare_cas_u32(word, before, u32(i64(before) + change)) { before = bare_load_u32(word) }
	}
	fn bare_cancel_condition_cleanup(argument uint) {
		state := unsafe { &BareCancelCondition(argument) }
		bare_cancel_counter_add(state.waiters, -1)
		C.v_cancel_mutex_lock(state.mutex)
	}
	@[export: 'v_cancel_condition_wait'; markused]
	fn bare_cancel_condition_wait(condition uint, mutex uint, clock i32, deadline uint) i32 {
		mut fields := [4]uint{}
		C.v_cancel_condition_fields(condition, uint(&fields[0]))
		if fields[3] != 0 || (clock != 0 && clock != 1) { return i32(C.EINVAL) }
		mut expired := false
		if deadline != 0 {
			time := unsafe { &i64(deadline) }
			if unsafe { time[1] < 0 || time[1] >= 1000000000 } { return i32(C.EINVAL) }
			expired = unsafe { time[0] < 0 }
		}
		generation := bare_load_u32(unsafe { &u32(fields[0]) })
		bare_cancel_counter_add(fields[1], 1)
		unlocked := C.v_cancel_mutex_unlock(mutex)
		if unlocked != 0 {
			bare_cancel_counter_add(fields[1], -1)
			return unlocked
		}
		state := BareCancelCondition{ waiters: fields[1], mutex: mutex }
		mut cleanup := BareCancelCleanup{}
		bare_cancel_cleanup_push(mut cleanup, uint(bare_cancel_condition_cleanup), uint(&state))
		mut operation := uint(9) | if fields[2] & 1 != 0 { uint(0) } else { uint(128) }
		if deadline != 0 && clock == 0 { operation |= 256 }
		waited := if expired {
			-i64(C.ETIMEDOUT)
		} else {
			bare_cancel_syscall(i64(C.SYS_futex), fields[0], operation, generation, deadline, 0, uint(~u32(0)))
		}
		bare_cancel_cleanup_pop(&cleanup)
		bare_cancel_counter_add(fields[1], -1)
		locked := C.v_cancel_mutex_lock(mutex)
		if locked != 0 { return locked }
		return if waited == -i64(C.ETIMEDOUT) {
			i32(C.ETIMEDOUT)
		} else if waited < 0 && waited != -i64(C.EAGAIN) && waited != -i64(C.EINTR) {
			i32(-waited)
		} else {
			i32(0)
		}
	}
	fn bare_cancel_cleanup_push(mut entry BareCancelCleanup, function uint, argument uint) {
		mut thread := bare_cancel_current()
		entry.previous = thread.cancel_cleanups
		entry.function = function
		entry.argument = argument
		thread.cancel_cleanups = &entry
	}
	fn bare_cancel_cleanup_pop(entry &BareCancelCleanup) {
		mut thread := bare_cancel_current()
		thread.cancel_cleanups = entry.previous
	}
	fn bare_cancel_clean_to(mut thread BarePthread, frame uint) {
		for thread.cancel_cleanups != unsafe { nil } && uint(thread.cancel_cleanups) < frame {
			entry := thread.cancel_cleanups
			thread.cancel_cleanups = entry.previous
			C.v_pthread_destructor_call(entry.function, entry.argument)
		}
		for bare_pthread_once_cleanup_get() != unsafe { nil } && uint(bare_pthread_once_cleanup_get()) < frame {
			entry := bare_pthread_once_cleanup_get()
			C.v_pthread_tls_store(4, uint(entry.previous))
			bare_store_u32(entry.control, 0)
			bf_futex_wake(entry.control, 0x7fffffff)
		}
	}
	@[noreturn]
	fn bare_cancel_finish_implicit(mut thread BarePthread) {
		bare_cancel_clean_to(mut thread, ~uint(0))
		bare_pthread_cleanup()
		bare_credential_thread_leave()
		bare_pthread_metadata_leave(mut thread)
		bare_store_u32(&thread.state, 3)
		bf_futex_wake(&thread.state, 0x7fffffff)
		sys_call1(u64(C.SYS_exit), 0)
		for {}
	}
	@[noreturn]
	fn bare_cancel_jump(mut thread BarePthread) {
		target := if thread.cancel_buffers != 0 {
			thread.cancel_buffers
		} else {
			uint(thread.exit_target)
		}
		if target == 0 { bare_cancel_finish_implicit(mut thread) }
		bare_cancel_clean_to(mut thread, C.v_cancel_jump_sp(target))
		C.longjmp(voidptr(target), 1)
		for {}
	}
	@[export: 'v_cancel_unwind_stop'; markused]
	fn bare_cancel_unwind_stop(actions i32, frame uint, argument uint) i32 {
		mut thread := unsafe { &BarePthread(argument) }
		bare_cancel_clean_to(mut thread, frame)
		target := if thread.cancel_buffers != 0 {
			thread.cancel_buffers
		} else {
			uint(thread.exit_target)
		}
		if actions & 16 != 0 || (target != 0 && frame >= C.v_cancel_jump_sp(target)) {
			bare_cancel_jump(mut thread)
		}
		return 0
	}
	@[export: 'v_cancel_unwind_discard'; markused; noreturn]
	fn bare_cancel_unwind_discard() {
		eprint('V: thread cancellation exception was not rethrown\n')
		C.abort()
		for {}
	}
	@[noreturn]
	fn bare_cancel_unwind(mut thread BarePthread) {
		if C.v_cancel_unwinder_available() != 0 {
			C.v_cancel_force_unwind(uint(&thread))
			C.abort()
		}
		// Ordinary C cleanup macros carry explicit jump buffers. A C++ or
		// exception-enabled dependency supplies its native GCC unwinder instead.
		bare_cancel_jump(mut thread)
	}
	@[export: 'v_cancel_register'; markused]
	fn bare_cancel_register(buffer uint, deferred i32) {
		mut thread := bare_cancel_current()
		C.v_cancel_buffer_set(buffer, 0, thread.cancel_buffers)
		thread.cancel_buffers = buffer
		if deferred != 0 {
			before := bare_cancel_word(mut thread, 0, 4)
			C.v_cancel_buffer_set(buffer, 1, uint(before & 4))
		}
	}
	@[export: 'v_cancel_unregister'; markused]
	fn bare_cancel_unregister(buffer uint, restore i32) {
		mut thread := bare_cancel_current()
		thread.cancel_buffers = C.v_cancel_buffer_get(buffer, 0)
		if restore != 0 {
			before := bare_cancel_word(mut thread, u32(C.v_cancel_buffer_get(buffer, 1)), 4)
			if before & 1 == 0 && C.v_cancel_buffer_get(buffer, 1) != 0 {
				bare_pthread_testcancel()
			}
		}
	}
	@[export: 'v_cancel_unwind_next'; markused; noreturn]
	fn bare_cancel_unwind_next(buffer uint) {
		mut thread := bare_cancel_current()
		thread.cancel_buffers = C.v_cancel_buffer_get(buffer, 0)
		bare_cancel_unwind(mut thread)
	}
}
