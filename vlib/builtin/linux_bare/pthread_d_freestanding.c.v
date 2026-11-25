@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_pthread_abi.h"
	#include <errno.h>

	fn C.v_pthread_attributes_size() uint
	fn C.v_pthread_jump_size() uint
	fn C.v_pthread_call(uint, uint) uint
	fn C.v_pthread_destructor_call(uint, uint)
	fn C.v_pthread_once_call(uint)
	fn C.v_pthread_tls_load(u32) uint
	fn C.v_pthread_tls_store(u32, uint)
	fn C.v_pthread_tls_identity() uint
	fn C._setjmp(voidptr) i32
	fn C.longjmp(voidptr, i32)

	struct BarePthreadAttributes {
	mut:
		stack_size uint
		stack_base uint
		detached   i32
	}
	@[heap]
	struct BarePthread {
	mut:
		backend         voidptr
		start           uint
		argument        uint
		result          uint
		exit_target     voidptr
		ready           u32
		state           u32
		joiner          u32
		cancel_word     u32
		tid             u32
		stack_sample    uint
		implicit        u32
		cancel_depth    u32
		metadata_lock   u32
		cancel_buffers  uint
		cancel_cleanups &BareCancelCleanup = unsafe { nil }
	}
	struct BarePthreadKey {
	mut:
		destructor uint
		generation u64
		active     bool
	}
	struct BarePthreadKeyValue {
	mut:
		value      uint
		generation u64
	}
	@[heap]
	struct BarePthreadDestructor {
	mut:
		function uint
		argument uint
		next     &BarePthreadDestructor = unsafe { nil }
	}
	struct BarePthreadOnceCleanup {
	mut:
		control  &u32                    = unsafe { nil }
		previous &BarePthreadOnceCleanup = unsafe { nil }
	}

	__global bare_pthread_keys [1024]BarePthreadKey
	__global bare_pthread_keys_lock u32

	fn bare_pthread_current_get() &BarePthread {
		return unsafe { &BarePthread(C.v_pthread_tls_load(0)) }
	}
	fn bare_pthread_values_get() &BarePthreadKeyValue {
		return unsafe { &BarePthreadKeyValue(C.v_pthread_tls_load(1)) }
	}
	fn bare_pthread_capacity_get() uint { return C.v_pthread_tls_load(2) }
	fn bare_pthread_destructors_get() &BarePthreadDestructor {
		return unsafe { &BarePthreadDestructor(C.v_pthread_tls_load(3)) }
	}
	fn bare_pthread_once_cleanup_get() &BarePthreadOnceCleanup {
		return unsafe { &BarePthreadOnceCleanup(C.v_pthread_tls_load(4)) }
	}

	fn bare_compat_exchange(word &u32, value u32) u32 {
		mut previous := bare_load_u32(word)
		for !bare_cas_u32(word, previous, value) { previous = bare_load_u32(word) }
		return previous
	}
	@[inline]
	fn bare_compat_load_uint(address &uint) uint {
		mut value := uint(0)
		$if amd64 {
			asm volatile amd64 {
				mov value, [address]
				; =r (value)
				; r (address)
				; memory
			}
		} $else {
			asm volatile arm64 {
				ldar value, [address]
				; =r (value)
				; r (address)
				; memory
			}
		}
		return value
	}
	@[inline]
	fn bare_compat_store_uint(address &uint, value uint) {
		$if amd64 {
			asm volatile amd64 {
				mov [address], value
				; ; r (address)
				    r (value)
				; memory
			}
		} $else {
			asm volatile arm64 {
				stlr value, [address]
				; ; r (address)
				    r (value)
				; memory
			}
		}
	}
	fn bare_compat_lock(word &u32) {
		if bare_cas_u32(word, 0, 1) { return }
		for bare_compat_exchange(word, 2) != 0 { bf_futex_wait(word, 2, -1) }
	}
	fn bare_compat_unlock(word &u32) {
		if bare_compat_exchange(word, 0) == 2 { bf_futex_wake(word, 1) }
	}

	@[export: 'v_pthread_attr_init'; markused]
	fn bare_pthread_attr_init(address uint) i32 {
		unsafe { vmemset(voidptr(address), 0, int(C.v_pthread_attributes_size())) }
		return 0
	}
	@[export: 'v_pthread_attr_destroy'; markused]
	fn bare_pthread_attr_destroy(_ uint) i32 { return 0 }
	@[export: 'v_pthread_attr_setstack'; markused]
	fn bare_pthread_attr_setstack(address uint, size uint) i32 {
		if size < 16384 || size > uint(max_int) - 4095 { return i32(C.EINVAL) }
		mut attributes := unsafe { &BarePthreadAttributes(address) }
		attributes.stack_size = size
		return 0
	}
	@[export: 'v_pthread_attr_getstack'; markused]
	fn bare_pthread_attr_getstack(address uint, output uint) i32 {
		attributes := unsafe { &BarePthreadAttributes(address) }
		size := if attributes.stack_size == 0 { uint(1 << 20) } else { attributes.stack_size }
		unsafe { *(&uint(output)) = size }
		return 0
	}
	@[export: 'v_pthread_attr_setdetach'; markused]
	fn bare_pthread_attr_setdetach(address uint, state i32) i32 {
		if state != i32(C.PTHREAD_CREATE_JOINABLE) && state != i32(C.PTHREAD_CREATE_DETACHED) {
			return i32(C.EINVAL)
		}
		mut attributes := unsafe { &BarePthreadAttributes(address) }
		attributes.detached = state
		return 0
	}
	@[export: 'v_pthread_attr_getdetach'; markused]
	fn bare_pthread_attr_getdetach(address uint, output uint) i32 {
		attributes := unsafe { &BarePthreadAttributes(address) }
		unsafe { *(&i32(output)) = attributes.detached }
		return 0
	}
	@[export: 'v_pthread_self'; markused]
	fn bare_pthread_self() uint {
		return uint(bare_cancel_current())
	}

	@[export: 'v_pthread_key_create'; markused]
	fn bare_pthread_key_create(output uint, destructor uint) i32 {
		bare_compat_lock(&bare_pthread_keys_lock)
		defer { bare_compat_unlock(&bare_pthread_keys_lock) }
		for i in 0 .. bare_pthread_keys.len {
			if bare_pthread_keys[i].active { continue }
			bare_pthread_keys[i].generation++
			bare_pthread_keys[i].destructor = destructor
			bare_pthread_keys[i].active = true
			unsafe { *(&u32(output)) = u32(i) }
			return 0
		}
		return i32(C.EAGAIN)
	}
	@[export: 'v_pthread_key_delete'; markused]
	fn bare_pthread_key_delete(key u32) i32 {
		if key >= 1024 { return i32(C.EINVAL) }
		bare_compat_lock(&bare_pthread_keys_lock)
		defer { bare_compat_unlock(&bare_pthread_keys_lock) }
		result := if bare_pthread_keys[key].active { i32(0) } else { i32(C.EINVAL) }
		bare_pthread_keys[key].active = false
		bare_pthread_keys[key].destructor = 0
		return result
	}
	@[export: 'v_pthread_getspecific'; markused]
	fn bare_pthread_getspecific(key u32) uint {
		if uint(key) >= bare_pthread_capacity_get() { return 0 }
		bare_compat_lock(&bare_pthread_keys_lock)
		defer { bare_compat_unlock(&bare_pthread_keys_lock) }
		entry := unsafe { bare_pthread_values_get()[key] }
		if !bare_pthread_keys[key].active || entry.generation != bare_pthread_keys[key].generation {
			return 0
		}
		return entry.value
	}
	@[export: 'v_pthread_setspecific'; markused]
	fn bare_pthread_setspecific(key u32, value uint) i32 {
		if key >= 1024 { return i32(C.EINVAL) }
		bare_compat_lock(&bare_pthread_keys_lock)
		generation := if bare_pthread_keys[key].active {
			bare_pthread_keys[key].generation
		} else {
			u64(0)
		}
		bare_compat_unlock(&bare_pthread_keys_lock)
		if generation == 0 { return i32(C.EINVAL) }
		if uint(key) >= bare_pthread_capacity_get() {
			if value == 0 { return 0 }
			mut capacity := if bare_pthread_capacity_get() == 0 {
				uint(16)
			} else {
				bare_pthread_capacity_get()
			}
			for capacity <= uint(key) { capacity *= 2 }
			saved_errno := bf_native_errno_cell
			mut values := unsafe { &BarePthreadKeyValue(__calloc(capacity, sizeof(BarePthreadKeyValue))) }
			bf_native_errno_cell = saved_errno
			if values == unsafe { nil } { return i32(C.ENOMEM) }
			for i in 0 .. int(bare_pthread_capacity_get()) {
				unsafe { values[i] = bare_pthread_values_get()[i] }
			}
			unsafe { __free(bare_pthread_values_get()) }
			C.v_pthread_tls_store(1, uint(values))
			C.v_pthread_tls_store(2, capacity)
		}
		unsafe { bare_pthread_values_get()[key] = BarePthreadKeyValue{ value: value, generation: generation } }
		return 0
	}
	@[export: 'v_pthread_tls_destructor'; markused]
	fn bare_pthread_tls_destructor(function uint, argument uint, _ uint) i32 {
		mut entry := unsafe { &BarePthreadDestructor(__calloc(1, sizeof(BarePthreadDestructor))) }
		if entry == unsafe { nil } { return -1 }
		entry.function = function
		entry.argument = argument
		entry.next = bare_pthread_destructors_get()
		C.v_pthread_tls_store(3, uint(entry))
		return 0
	}

	@[export: 'v_freestanding_thread_cleanup'; markused]
	fn bare_pthread_cleanup() {
		for bare_pthread_destructors_get() != unsafe { nil } {
			entry := bare_pthread_destructors_get()
			C.v_pthread_tls_store(3, uint(entry.next))
			function := entry.function
			argument := entry.argument
			unsafe { __free(entry) }
			C.v_pthread_destructor_call(function, argument)
		}
		for _ in 0 .. 4 {
			mut invoked := false
			mut i := 0
			for uint(i) < bare_pthread_capacity_get() {
				entry := unsafe { bare_pthread_values_get()[i] }
				if entry.value != 0 {
					bare_compat_lock(&bare_pthread_keys_lock)
					key := bare_pthread_keys[i]
					function := if key.active && key.generation == entry.generation {
						key.destructor
					} else {
						uint(0)
					}
					bare_compat_unlock(&bare_pthread_keys_lock)
					unsafe { bare_pthread_values_get()[i].value = 0 }
					if function != 0 {
						invoked = true
						C.v_pthread_destructor_call(function, entry.value)
					}
				}
				i++
			}
			if !invoked { break }
		}
		unsafe { __free(bare_pthread_values_get()) }
		C.v_pthread_tls_store(1, 0)
		C.v_pthread_tls_store(2, 0)
	}

	@[export: 'v_pthread_once'; markused]
	fn bare_pthread_once(address uint, initialize uint) i32 {
		control := unsafe { &u32(address) }
		for {
			if bare_load_u32(control) == 2 { return 0 }
			if bare_cas_u32(control, 0, 1) {
				mut cleanup := BarePthreadOnceCleanup{ control: control, previous: bare_pthread_once_cleanup_get() }
				C.v_pthread_tls_store(4, uint(&cleanup))
				C.v_pthread_once_call(initialize)
				C.v_pthread_tls_store(4, uint(cleanup.previous))
				bare_store_u32(control, 2)
				bf_futex_wake(control, 0x7fffffff)
				return 0
			}
			bf_futex_wait(control, 1, -1)
		}
		return 0
	}

	fn bare_pthread_entry(argument voidptr) voidptr {
		mut thread := unsafe { &BarePthread(argument) }
		C.v_pthread_tls_store(0, uint(thread))
		bare_store_u32(&thread.tid, u32(sys_call0(u64(C.SYS_gettid))))
		bare_cancel_unblock()
		for bare_load_u32(&thread.ready) == 0 {
		}
		if C._setjmp(thread.exit_target) == 0 {
			thread.result = C.v_pthread_call(thread.start, thread.argument)
		}
		bare_cancel_mark_exiting(mut thread)
		bare_pthread_cleanup()
		bare_pthread_metadata_leave(mut thread)
		C.v_pthread_tls_store(0, 0)
		unsafe { __free(thread.exit_target) }
		if !bare_cas_u32(&thread.state, 1, 3) { unsafe { __free(thread) } }
		// V owns its result packet; C's opaque return value stays with its joiner.
		return unsafe { nil }
	}
	@[export: 'v_pthread_create'; markused]
	fn bare_pthread_create(output uint, attribute_address uint, start uint, argument uint) i32 {
		saved_errno := bf_native_errno_cell
		defer { bf_native_errno_cell = saved_errno }
		settings := if attribute_address != 0 {
			unsafe { *(&BarePthreadAttributes(attribute_address)) }
		} else {
			BarePthreadAttributes{}
		}
		mut thread := unsafe { &BarePthread(__calloc(1, sizeof(BarePthread))) }
		if thread == unsafe { nil } { return i32(C.ENOMEM) }
		thread.exit_target = unsafe { __calloc(1, C.v_pthread_jump_size()) }
		if thread.exit_target == unsafe { nil } {
			unsafe { __free(thread) }
			return i32(C.ENOMEM)
		}
		thread.start = start
		thread.argument = argument
		thread.state = 1
		mut code := i32(0)
		backend := bare_thread_create(bare_pthread_entry, thread, settings.stack_size, 0, &code)
		if backend == unsafe { nil } || code != 0 {
			unsafe {
				__free(thread.exit_target)
				__free(thread)
			}
			return if code != 0 { code } else { i32(C.EAGAIN) }
		}
		thread.backend = backend
		bare_store_u32(&thread.tid, u32(unsafe { (&BareThreadContext(backend)).mapping.tid }))
		unsafe { *(&uint(output)) = uint(thread) }
		if settings.detached == i32(C.PTHREAD_CREATE_DETACHED) {
			thread.state = 2
			bare_thread_detach(backend)
		}
		bare_store_u32(&thread.ready, 1)
		return 0
	}
	@[export: 'v_pthread_join'; markused]
	fn bare_pthread_join(handle uint, output uint) i32 {
		bare_pthread_testcancel()
		if handle == bare_pthread_self() { return i32(C.EDEADLK) }
		mut thread := unsafe { &BarePthread(handle) }
		if bare_load_u32(&thread.state) == 2 || !bare_cas_u32(&thread.joiner, 0, 1) {
			return i32(C.EINVAL)
		}
		mut cleanup := BareCancelCleanup{}
		bare_cancel_cleanup_push(mut cleanup, uint(bare_cancel_join_release), handle)
		if thread.implicit == 1 {
			for bare_load_u32(&thread.state) != 3 {
				state := bare_load_u32(&thread.state)
				if state == 3 { break }
				bare_cancel_syscall(i64(C.SYS_futex), uint(&thread.state), 128, state, 0, 0, 0)
			}
		} else {
			context := unsafe { &BareThreadContext(thread.backend) }
			for {
				tid := bare_load_u32(context.mapping.tid_addr)
				if tid == 0 { break }
				bare_cancel_syscall(i64(C.SYS_futex), uint(context.mapping.tid_addr), 0, tid, 0, 0, 0)
			}
			bare_thread_join(thread.backend)
		}
		bare_cancel_cleanup_pop(&cleanup)
		if output != 0 { unsafe { *(&uint(output)) = thread.result } }
		if thread.implicit == 0 { unsafe { __free(thread) } }
		return 0
	}
	@[export: 'v_pthread_detach'; markused]
	fn bare_pthread_detach(handle uint) i32 {
		mut thread := unsafe { &BarePthread(handle) }
		if bare_load_u32(&thread.joiner) != 0 { return i32(C.EINVAL) }
		backend := thread.backend
		mut state := bare_load_u32(&thread.state)
		for {
			if state == 2 { return i32(C.EINVAL) }
			if bare_cas_u32(&thread.state, state, 2) { break }
			state = bare_load_u32(&thread.state)
		}
		if thread.implicit == 0 {
			bare_thread_detach(backend)
			if state == 3 { unsafe { __free(thread) } }
		}
		return 0
	}
	@[export: 'v_pthread_exit'; markused; noreturn]
	fn bare_pthread_exit(value uint) {
		mut thread := bare_cancel_current()
		thread.result = value
		bare_cancel_mark_exiting(mut thread)
		bare_cancel_unwind(mut thread)
	}
}
