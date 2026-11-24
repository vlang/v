module builtin

// The compiler and sync helpers use one scalar V-owned thread ABI. The
// ordinary heap holds controls/boxes; worker TLS never inherits Arena state.
$if freestanding && !native && amd64 && !v_bare_no_startup ? {
	#include <mach/i386/thread_status.h>

	@[typedef]
	struct C.x86_thread_state64_t {
	mut:
		__rip    u64
		__rdi    u64
		__rsp    u64
		__rflags u64
	}

	type BareThreadStart = fn (voidptr) voidptr

	struct BareThreadContext {
	mut:
		start         BareThreadStart
		argument      voidptr
		result        voidptr
		mapping       u64
		mapping_size  u64
		tls           voidptr
		start_gate    u32
		completion    u32
		thread_right  u32
		original_mask u32
		// 0 constructing, 1 joinable, 2 detached, 3 completed joinable.
		state u32
	}

	// bare_cas_u32 compares and exchanges a word with acquire/release ordering.
	@[inline; no_profile; no_trace]
	fn bare_cas_u32(ptr &u32, old u32, new u32) bool {
		mut swapped := u8(0)
		$if arm64 {
			// Named operands use X registers; copy word values into explicit W aliases.
			old_word := u64(old)
			new_word := u64(new)
			asm volatile arm64 {
				mov x6, old_word
				mov x7, new_word
				1:
				ldaxr w4, [ptr]
				cmp w4, w6
				b.ne '2f'
				stlxr w5, w7, [ptr]
				cbnz w5, '1b'
				mov swapped, #1
				b '3f'
				2:
				clrex
				mov swapped, #0
				3:
				; =r (swapped)
				; r (ptr)
				  r (old_word)
				  r (new_word)
				; x4
				  x5
				  x6
				  x7
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov eax, old
				lock cmpxchg [ptr], new
				sete swapped
				; =r (swapped)
				; r (old)
				  r (ptr)
				  r (new)
				; eax
				  cc
				  memory
			}
		}
		return swapped != 0
	}

	// bare_load_u32 loads a word with acquire ordering.
	@[inline; no_profile; no_trace]
	fn bare_load_u32(ptr &u32) u32 {
		mut value := u32(0)
		$if arm64 {
			mut loaded_word := u64(0)
			asm volatile arm64 {
				ldar w4, [ptr]
				mov loaded_word, x4
				; =r (loaded_word)
				; r (ptr)
				; x4
				  memory
			}
			value = u32(loaded_word)
		} $else {
			asm volatile amd64 {
				mov value, [ptr]
				; =r (value)
				; r (ptr)
				; memory
			}
		}
		return value
	}

	// bare_store_u32 stores a word with release ordering.
	@[inline; no_profile; no_trace]
	fn bare_store_u32(ptr &u32, val u32) {
		$if arm64 {
			stored_word := u64(val)
			asm volatile arm64 {
				mov x4, stored_word
				stlr w4, [ptr]
				; ; r (ptr)
				    r (stored_word)
				; x4
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov [ptr], val
				; ; r (ptr)
				    r (val)
				; memory
			}
		}
	}

	@[export: 'v_bare_thread_fail'; markused; noreturn; no_profile; no_trace]
	fn bare_thread_fail() {
		message := 'V: freestanding thread operation failed\n'
		mac_kernel_bsd6(4, 2, u64(message.str), u64(message.len), 0, 0, 0)
		mac_kernel_bsd6(1, 1, 0, 0, 0, 0, 0)
		for {}
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_drop_semaphore(semaphore u32) {
		if semaphore == 0 { return }
		if mac_kernel_semaphore_destroy(semaphore) != 0
			|| mac_kernel_port_deallocate(semaphore) != 0 {
			bare_thread_fail()
		}
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_wait(semaphore u32) {
		mut code := mac_kernel_semaphore_wait(semaphore)
		for code == i32(C.KERN_ABORTED) { code = mac_kernel_semaphore_wait(semaphore) }
		if code != 0 { bare_thread_fail() }
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_mask(how i32, mask &u32, old &u32) i32 {
		// BSD48 changes every thread; BSD329 modifies only this uthread and
		// does not require process-wide pthread registration.
		return mac_kernel_bsd6(329, u64(u32(how)), u64(mask), u64(old), 0, 0, 0)
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_entry(argument u64) {
		mut context := unsafe { &BareThreadContext(argument) }
		if bare_macos_tls_activate(context.tls) != 0 { bare_thread_fail() }
		gate := context.start_gate
		bare_macos_thread_wait(gate)
		bare_macos_thread_drop_semaphore(gate)
		initial := bare_load_u32(&context.state)
		if initial != 1 && initial != 2 { bare_thread_fail() }
		original_mask := context.original_mask
		if bare_macos_thread_mask(3, &original_mask, unsafe { nil }) != 0 { bare_thread_fail() }
		context.result = context.start(context.argument)
		blocked := ~u32(0)
		if bare_macos_thread_mask(1, &blocked, unsafe { nil }) != 0 { bare_thread_fail() }
		mapping := context.mapping
		mapping_size := context.mapping_size
		right := context.thread_right
		mut completion := context.completion
		if !bare_cas_u32(&context.state, 1, 3) {
			if bare_load_u32(&context.state) != 2 { bare_thread_fail() }
			bare_macos_thread_drop_semaphore(completion)
			completion = 0
			if context.result != unsafe { nil } { unsafe { __free(context.result) } }
			unsafe { __free(context) }
		}
		// BSD361 unmaps stack+TLS, posts completion, and consumes the owned
		// thread reference. No user instruction returns to the old mapping.
		mac_kernel_thread_exit_mapped(mapping, mapping_size, right, completion)
		bare_thread_fail()
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_error(code i32) i32 {
		return match code {
			i32(C.KERN_INVALID_ARGUMENT) { 22 }
			i32(C.KERN_INVALID_ADDRESS) { 14 }
			i32(C.KERN_PROTECTION_FAILURE) { 13 }
			i32(C.KERN_NO_SPACE), i32(C.KERN_RESOURCE_SHORTAGE) { 12 }
			else { 5 }
		}
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_discard(context &BareThreadContext) {
		bare_macos_thread_drop_semaphore(context.start_gate)
		bare_macos_thread_drop_semaphore(context.completion)
		if context.mapping != 0 && mac_kernel_vm_deallocate(context.mapping, context.mapping_size) != 0 {
			bare_thread_fail()
		}
		unsafe { __free(context) }
	}

	@[export: 'v_bare_thread_create'; markused; no_profile; no_trace]
	fn bare_thread_create(start BareThreadStart, argument voidptr, requested_stack uint, detached i32, error &i32) voidptr {
		unsafe { *error = 0 }
		stack_size := if requested_stack == 0 { u64(1 << 20) } else { u64(requested_stack) }
		if start == unsafe { nil } || stack_size < 16384 {
			unsafe { *error = 22 }
			return unsafe { nil }
		}
		mut page := u64(0)
		page_error := mac_kernel_page_size(&page)
		if page_error != 0 {
			unsafe { *error = page_error }
			return unsafe { nil }
		}
		mut tls_size := u64(bare_macos_tls_storage_size())
		if tls_size == 0 || tls_size > ~u64(0) - (page - 1) || stack_size > ~u64(0) - (page - 1) {
			unsafe { *error = 22 }
			return unsafe { nil }
		}
		tls_size = (tls_size + page - 1) & ~(page - 1)
		stack := (stack_size + page - 1) & ~(page - 1)
		if tls_size > ~u64(0) - page || stack > ~u64(0) - tls_size - page {
			unsafe { *error = 22 }
			return unsafe { nil }
		}
		mut context := unsafe { &BareThreadContext(__calloc(1, sizeof(BareThreadContext))) }
		if context == unsafe { nil } {
			unsafe { *error = 12 }
			return unsafe { nil }
		}
		context.start = start
		context.argument = argument
		context.mapping_size = tls_size + page + stack
		mut code := mac_kernel_vm_allocate(&context.mapping, context.mapping_size, i32(C.VM_FLAGS_ANYWHERE))
		if code != 0 {
			unsafe {
				__free(context)
				*error = bare_macos_thread_error(code)
			}
			return unsafe { nil }
		}
		context.tls = unsafe { voidptr(context.mapping) }
		task := mac_kernel_task_self()
		if task == 0 {
			bare_macos_thread_discard(context)
			unsafe { *error = 12 }
			return unsafe { nil }
		}
		code = i32(mac_kernel_mach_trap6(14, task, context.mapping + tls_size, page, 0, u64(C.VM_PROT_NONE), 0))
		if mac_kernel_port_release(task, task) != 0 { bare_thread_fail() }
		if code != 0 || bare_macos_tls_init_storage(context.tls, uint(tls_size)) != 0 {
			bare_macos_thread_discard(context)
			unsafe { *error = 22 }
			return unsafe { nil }
		}
		code = mac_kernel_semaphore_create(i32(C.SYNC_POLICY_FIFO), 0, &context.start_gate)
		if code == 0 && detached == 0 {
			code = mac_kernel_semaphore_create(i32(C.SYNC_POLICY_FIFO), 0, &context.completion)
		}
		if code != 0 {
			bare_macos_thread_discard(context)
			unsafe { *error = bare_macos_thread_error(code) }
			return unsafe { nil }
		}
		blocked := ~u32(0)
		mask_error := bare_macos_thread_mask(1, &blocked, &context.original_mask)
		if mask_error != 0 {
			bare_macos_thread_discard(context)
			unsafe { *error = mask_error }
			return unsafe { nil }
		}
		original_mask := context.original_mask
		state := C.x86_thread_state64_t{
			__rip:    u64(bare_macos_thread_entry)
			__rdi:    u64(context)
			__rsp:    context.mapping + context.mapping_size - 8
			__rflags: 0x202
		}
		mut right := u32(0)
		code = mac_kernel_thread_create_running(i32(C.x86_THREAD_STATE64), unsafe { &u32(&state) }, u32(C.x86_THREAD_STATE64_COUNT), &right)
		if bare_macos_thread_mask(3, &original_mask, unsafe { nil }) != 0 { bare_thread_fail() }
		if code != 0 {
			// An accepted create with a lost/invalid reply could leave a gated
			// worker alive. Never unmap storage it may still own.
			if code < 0 || u32(code) >= u32(0x10000000) { bare_thread_fail() }
			bare_macos_thread_discard(context)
			unsafe { *error = bare_macos_thread_error(code) }
			return unsafe { nil }
		}
		context.thread_right = right
		gate := context.start_gate
		bare_store_u32(&context.state, if detached != 0 { u32(2) } else { u32(1) })
		if mac_kernel_semaphore_signal(gate) != 0 { bare_thread_fail() }
		// An initially detached worker can already have freed context.
		return if detached != 0 { unsafe { nil } } else { context }
	}

	@[no_profile; no_trace]
	fn bare_macos_thread_collect(context &BareThreadContext) voidptr {
		if context.completion == 0 { bare_thread_fail() }
		bare_macos_thread_wait(context.completion)
		if bare_load_u32(&context.state) != 3 { bare_thread_fail() }
		result := context.result
		bare_macos_thread_drop_semaphore(context.completion)
		unsafe { __free(context) }
		return result
	}

	@[export: 'v_bare_thread_join'; markused; no_profile; no_trace]
	fn bare_thread_join(handle voidptr) voidptr {
		if handle == unsafe { nil } { bare_thread_fail() }
		return bare_macos_thread_collect(unsafe { &BareThreadContext(handle) })
	}

	@[export: 'v_bare_thread_detach'; markused; no_profile; no_trace]
	fn bare_thread_detach(handle voidptr) {
		if handle == unsafe { nil } { return }
		context := unsafe { &BareThreadContext(handle) }
		if bare_cas_u32(&context.state, 1, 2) { return }
		if bare_load_u32(&context.state) != 3 { bare_thread_fail() }
		result := bare_macos_thread_collect(context)
		if result != unsafe { nil } { unsafe { __free(result) } }
	}
}
