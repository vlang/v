module builtin

// Shared by hosted vlibc and freestanding builds. Only the latter owns ELF TLS.
$if !i386 {
	// bare_cas_u32 compares and exchanges a word with acquire/release ordering.
	@[inline]
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
	@[inline]
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
	@[inline]
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

	// VlibcThreadFn is the entry point of a freestanding thread.
	pub type VlibcThreadFn = fn (u64)

	// VlibcThread owns a thread stack and any TLS image until join observes kernel exit.
	pub struct VlibcThread {
	pub:
		fn_ptr           VlibcThreadFn
		arg              u64
		stack            voidptr
		stack_size       u64
		done             u32
		tid              i64
		tid_addr         &u32 = unsafe { nil }
		tls_mapping      voidptr
		tls_mapping_size u64
	}

	// The child never returns through the parent's frame. Its entry function and
	// argument live at its own stack top, and the kernel clears tid_addr on exit.
	fn bare_clone_raw(newsp voidptr, tid_addr &u32, tls_pointer voidptr) i64 {
		mut result := i64(0)
		flags := if tls_pointer == unsafe { nil } { u64(0x00350f00) } else { u64(0x003d0f00) }
		$if arm64 {
			asm volatile arm64 {
				mov x1, newsp
				mov x2, tid_addr
				mov x4, tid_addr
				mov x3, tls_pointer
				mov x0, flags
				mov x8, #220
				svc #0
				cbnz x0, '1f'
				ldr x9, [sp]
				ldr x0, [sp, #8]
				blr x9
				mov x0, #0
				mov x8, #93
				svc #0
				1:
				mov result, x0
				; =r (result)
				; r (newsp)
				  r (tid_addr)
				  r (tls_pointer)
				  r (flags)
				; x0
				  x1
				  x2
				  x3
				  x4
				  x8
				  x9
				  x30
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov rsi, newsp
				mov rdx, tid_addr
				mov r10, tid_addr
				mov r8, tls_pointer
				mov rdi, flags
				mov rax, 56
				syscall
				test rax, rax
				jnz '1f'
				mov rax, [rsp]
				mov rdi, [rsp + 8]
				call rax
				mov rax, 60
				xor rdi, rdi
				syscall
				1:
				; =a (result)
				; r (newsp)
				  r (tid_addr)
				  r (tls_pointer)
				  r (flags)
				; rdi
				  rsi
				  rdx
				  r10
				  r8
				  rcx
				  r11
				  cc
				  memory
			}
		}
		return result
	}

	// bare_thread_mapping_spawn starts fn_ptr(arg) with per-thread state and the requested stack.
	// Freestanding C builds initialize an independent ELF TLS image for the child.
	fn bare_thread_mapping_spawn(fn_ptr VlibcThreadFn, arg u64, stack_size u64) (VlibcThread, Errno) {
		mut t := VlibcThread{ fn_ptr: fn_ptr, arg: arg, stack_size: stack_size }
		mem, e := sys_mmap(unsafe { nil }, t.stack_size, unsafe { MemProt(3) }, unsafe { MapFlags(0x22) }, -1, 0)
		if e != .enoerror {
			return t, e
		}
		t.stack = mem
		t.tid_addr = unsafe { &u32(mem) }
		mut tls_pointer := voidptr(unsafe { nil })
		$if freestanding && !native {
			tls, tls_error := bare_tls_allocate()
			if tls_error != .enoerror {
				sys_munmap(t.stack, t.stack_size)
				t.stack = unsafe { nil }
				t.tid_addr = unsafe { nil }
				return t, unsafe { Errno(tls_error) }
			}
			t.tls_mapping = tls.mapping
			t.tls_mapping_size = u64(tls.mapping_size)
			tls_pointer = tls.pointer
		}
		newsp := unsafe { &u64(u64(mem) + t.stack_size - 16) }
		// The stack mapping owns all data used by the child; spawning again cannot
		// overwrite it. No heap allocator is needed on either clone return path.
		unsafe {
			newsp[0] = u64(fn_ptr)
			newsp[1] = arg
		}
		t.tid = bare_clone_raw(newsp, t.tid_addr, tls_pointer)
		if t.tid < 0 {
			sys_munmap(t.stack, t.stack_size)
			if t.tls_mapping != unsafe { nil } {
				sys_munmap(t.tls_mapping, t.tls_mapping_size)
				t.tls_mapping = unsafe { nil }
				t.tls_mapping_size = 0
			}
			t.stack = unsafe { nil }
			t.tid_addr = unsafe { nil }
			return t, unsafe { Errno(-t.tid) }
		}
		return t, Errno.enoerror
	}

	// bare_thread_mapping_join waits for kernel-confirmed exit before releasing stack and TLS storage.
	fn bare_thread_mapping_join(mut t VlibcThread) Errno {
		if t.stack == unsafe { nil } {
			return .enoerror
		}
		for {
			tid := bare_load_u32(t.tid_addr)
			if tid == 0 {
				break
			}
			// CLONE_CHILD_CLEARTID wakes shared FUTEX_WAIT, not FUTEX_WAIT_PRIVATE.
			bf_futex_raw(t.tid_addr, 0, tid, unsafe { nil }, unsafe { nil }, 0)
		}
		sys_munmap(t.stack, t.stack_size)
		if t.tls_mapping != unsafe { nil } {
			sys_munmap(t.tls_mapping, t.tls_mapping_size)
			t.tls_mapping = unsafe { nil }
			t.tls_mapping_size = 0
		}
		t.stack = unsafe { nil }
		t.tid_addr = unsafe { nil }
		t.done = 1
		return .enoerror
	}
}
