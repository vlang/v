@[has_globals]
module builtin

// Compiler-owned thread arguments/results use the ordinary heap directly. The
// creation gate publishes the complete mapping before a detached worker can
// reclaim it; a worker and a detacher transfer exclusive ownership with CAS.
$if !native && (amd64 || arm64) {
	type BareThreadStart = fn (voidptr) voidptr

	struct BareThreadContext {
	mut:
		start   BareThreadStart
		arg     voidptr
		result  voidptr
		mapping VlibcThread
		// 0 constructing, 1 joinable, 2 detached, 3 completed joinable.
		state u32
	}

	fn bare_thread_entry(argument u64) {
		mut context := unsafe { &BareThreadContext(argument) }
		context.result = context.start(context.arg)
		for bare_load_u32(&context.state) == 0 {
		}
		if bare_cas_u32(&context.state, 1, 3) { return }
		// A successful detach transferred all reclamation to this worker.
		// A signal handler must never run after TLS or its signal stack disappears.
		bare_thread_block_signals()
		mapping := context.mapping
		if context.result != unsafe { nil } { unsafe { __free(context.result) } }
		unsafe { __free(context) }
		bare_thread_unmap_self(mapping.stack, mapping.stack_size, mapping.tls_mapping, mapping.tls_mapping_size)
	}

	fn bare_thread_block_signals() {
		// Linux's kernel signal mask has 64 bits on both supported architectures.
		// SIGKILL/SIGSTOP are ignored by the kernel and cannot invoke a handler.
		mask := ~u64(0)
		mut result := i64(0)
		$if amd64 {
			result = i64(sys_call4(14, 0, u64(&mask), 0, 8))
		} $else {
			result = i64(sys_call4(135, 0, u64(&mask), 0, 8))
		}
		if result != 0 { bare_thread_fail() }
	}

	// bare_thread_unmap_self never accesses its old stack/TLS after unmapping.
	// Clear the kernel's child-TID pointer first: a detached thread has no joiner.
	// The final raw syscalls run entirely in registers and cannot return to C.
	@[noreturn]
	fn bare_thread_unmap_self(stack voidptr, stack_size u64, tls voidptr, tls_size u64) {
		$if amd64 {
			asm volatile amd64 {
				mov r12, stack
				mov r13, stack_size
				mov r14, tls
				mov r15, tls_size
				xor rdi, rdi
				mov rax, 218
				syscall
				test r14, r14
				jz '1f'
				mov rdi, r14
				mov rsi, r15
				mov rax, 11
				syscall
				1:
				mov rdi, r12
				mov rsi, r13
				mov rax, 11
				syscall
				xor rdi, rdi
				mov rax, 60
				syscall
				ud2
				; ; r (stack)
				    r (stack_size)
				    r (tls)
				    r (tls_size)
				; rax
				  rdi
				  rsi
				  r12
				  r13
				  r14
				  r15
				  rcx
				  r11
				  cc
				  memory
			}
		} $else {
			asm volatile arm64 {
				mov x12, stack
				mov x13, stack_size
				mov x14, tls
				mov x15, tls_size
				mov x0, #0
				mov x8, #96
				svc #0
				cbz x14, '1f'
				mov x0, x14
				mov x1, x15
				mov x8, #215
				svc #0
				1:
				mov x0, x12
				mov x1, x13
				mov x8, #215
				svc #0
				mov x0, #0
				mov x8, #93
				svc #0
				brk #0
				; ; r (stack)
				    r (stack_size)
				    r (tls)
				    r (tls_size)
				; x0
				  x1
				  x8
				  x12
				  x13
				  x14
				  x15
				  memory
			}
		}
		for {}
	}

	// The compiler and sync helpers share this V-owned ABI, rather than exposing
	// a partial pthread implementation. A library host creates its own threads.
	@[export: 'v_bare_thread_create'; markused]
	fn bare_thread_create(start BareThreadStart, arg voidptr, requested_stack uint, detached i32, error_code &i32) voidptr {
		unsafe { *error_code = 0 }
		stack_size := if requested_stack == 0 { u64(1 << 20) } else { u64(requested_stack) }
		// Two header words and the kernel TID cell must fit before clone runs.
		if stack_size < 16384 || stack_size > u64(max_int) - 4095 {
			unsafe { *error_code = i32(Errno.einval) }
			return unsafe { nil }
		}
		mut context := unsafe { &BareThreadContext(__calloc(1, sizeof(BareThreadContext))) }
		if context == unsafe { nil } {
			unsafe { *error_code = i32(Errno.enomem) }
			return unsafe { nil }
		}
		context.start = start
		context.arg = arg
		mapping, code := bare_thread_mapping_spawn(bare_thread_entry, u64(context), (stack_size + 4095) & ~u64(4095))
		if code != .enoerror {
			unsafe { __free(context) }
			unsafe { *error_code = i32(code) }
			return unsafe { nil }
		}
		context.mapping = mapping
		bare_store_u32(&context.state, if detached != 0 { u32(2) } else { u32(1) })
		// Once detached is published the worker may immediately free context.
		return if detached != 0 { unsafe { nil } } else { context }
	}

	@[export: 'v_bare_thread_join'; markused]
	fn bare_thread_join(handle voidptr) voidptr {
		if handle == unsafe { nil } { bare_thread_fail() }
		mut context := unsafe { &BareThreadContext(handle) }
		bare_thread_mapping_join(mut context.mapping)
		result := context.result
		unsafe { __free(context) }
		return result
	}

	@[export: 'v_bare_thread_detach'; markused]
	fn bare_thread_detach(handle voidptr) {
		if handle == unsafe { nil } { return }
		mut context := unsafe { &BareThreadContext(handle) }
		if bare_cas_u32(&context.state, 1, 2) { return }
		// The worker committed to ordinary kernel exit before detach won.
		// Wait for that exit before reclaiming the mapping from this thread.
		bare_thread_mapping_join(mut context.mapping)
		if context.result != unsafe { nil } { unsafe { __free(context.result) } }
		unsafe { __free(context) }
	}

	@[export: 'v_bare_thread_fail'; markused; noreturn]
	fn bare_thread_fail() {
		eprint('V: freestanding thread operation failed\n')
		exit(1)
	}
}
