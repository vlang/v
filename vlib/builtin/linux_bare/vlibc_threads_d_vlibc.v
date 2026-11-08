module builtin

$if !i386 {
	// vlibc_cas_u32 compares and exchanges a word with acquire/release ordering.
	pub fn vlibc_cas_u32(ptr &u32, old u32, new u32) bool {
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

	// vlibc_load_u32 loads a word with acquire ordering.
	pub fn vlibc_load_u32(ptr &u32) u32 {
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

	// vlibc_store_u32 stores a word with release ordering.
	pub fn vlibc_store_u32(ptr &u32, val u32) {
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

	// Incrementing the condition sequence must not lose concurrent signals.
	fn vlibc_increment_u32(ptr &u32) {
		$if arm64 {
			asm volatile arm64 {
				1:
				ldaxr w4, [ptr]
				add w4, w4, #1
				stlxr w5, w4, [ptr]
				cbnz w5, '1b'
				; ; r (ptr)
				; x4
				  x5
				  memory
			}
		} $else {
			one := u32(1)
			asm volatile amd64 {
				lock add [ptr], one
				; ; r (ptr)
				    r (one)
				; cc
				  memory
			}
		}
	}

	// VlibcThreadFn is the entry point of a freestanding thread.
	pub type VlibcThreadFn = fn (u64)

	// VlibcThread owns a thread stack until vlibc_thread_join observes kernel exit.
	pub struct VlibcThread {
	pub:
		fn_ptr     VlibcThreadFn
		arg        u64
		stack      voidptr
		stack_size u64
		done       u32
		tid        i64
		tid_addr   &u32 = unsafe { nil }
	}

	// The child never returns through the parent's frame. Its entry function and
	// argument live at its own stack top, and the kernel clears tid_addr on exit.
	fn vlibc_clone_raw(newsp voidptr, tid_addr &u32) i64 {
		mut result := i64(0)
		$if arm64 {
			asm volatile arm64 {
				mov x1, newsp
				mov x2, tid_addr
				mov x4, tid_addr
				mov x3, #0
				mov x0, #0x0f00
				movk x0, #0x35, lsl #16
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
				xor r8, r8
				mov rdi, 0x00350f00
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

	// vlibc_thread_spawn starts fn_ptr(arg) with per-thread state and a 1 MiB stack.
	@[unsafe]
	pub fn vlibc_thread_spawn(fn_ptr VlibcThreadFn, arg u64) (VlibcThread, VlibcErrno) {
		mut t := VlibcThread{ fn_ptr: fn_ptr, arg: arg, stack_size: 1 << 20 }
		mem, e := vlibc_sys_mmap(unsafe { nil }, t.stack_size, 3, 0x22, -1, 0)
		if e != .enoerror {
			return t, e
		}
		t.stack = mem
		t.tid_addr = unsafe { &u32(mem) }
		newsp := unsafe { &u64(u64(mem) + t.stack_size - 16) }
		// The stack mapping owns all data used by the child; spawning again cannot
		// overwrite it. No heap allocator is needed on either clone return path.
		unsafe {
			newsp[0] = u64(fn_ptr)
			newsp[1] = arg
		}
		t.tid = vlibc_clone_raw(newsp, t.tid_addr)
		if t.tid < 0 {
			vlibc_sys_munmap(t.stack, t.stack_size)
			t.stack = unsafe { nil }
			t.tid_addr = unsafe { nil }
			return t, unsafe { VlibcErrno(-t.tid) }
		}
		return t, VlibcErrno.enoerror
	}

	// vlibc_thread_join waits for kernel-confirmed exit before releasing the stack.
	@[unsafe]
	pub fn vlibc_thread_join(mut t VlibcThread) VlibcErrno {
		if t.stack == unsafe { nil } {
			return .enoerror
		}
		for {
			tid := vlibc_load_u32(t.tid_addr)
			if tid == 0 {
				break
			}
			// CLONE_CHILD_CLEARTID wakes shared FUTEX_WAIT, not FUTEX_WAIT_PRIVATE.
			vlibc_sys_futex(t.tid_addr, 0, tid, unsafe { nil }, unsafe { nil }, 0)
		}
		vlibc_sys_munmap(t.stack, t.stack_size)
		t.stack = unsafe { nil }
		t.tid_addr = unsafe { nil }
		t.done = 1
		return .enoerror
	}

	// ---- mutex ----
	pub struct VlibcMutex {
	pub:
		state u32
	}

	// vlibc_mutex_init creates an unlocked mutex.
	pub fn vlibc_mutex_init() VlibcMutex {
		return VlibcMutex{ state: 0 }
	}

	// vlibc_mutex_lock waits until the mutex is acquired.
	@[unsafe]
	pub fn vlibc_mutex_lock(mut m VlibcMutex) {
		for {
			if vlibc_cas_u32(&m.state, 0, 1) {
				return
			}
			// wait
			vlibc_sys_futex(&m.state, 0, 1, unsafe { nil }, unsafe { nil }, 0)
		}
	}

	// vlibc_mutex_unlock releases the mutex.
	@[unsafe]
	pub fn vlibc_mutex_unlock(mut m VlibcMutex) {
		vlibc_store_u32(&m.state, 0)
		vlibc_sys_futex(&m.state, 1, 1, unsafe { nil }, unsafe { nil }, 0) // FUTEX_WAKE
	}

	// ---- condvar ----
	pub struct VlibcCond {
	pub:
		seq u32
	}

	// vlibc_cond_init creates an initialized condition variable.
	pub fn vlibc_cond_init() VlibcCond {
		return VlibcCond{ seq: 0 }
	}

	// vlibc_cond_wait releases the mutex while waiting, then locks it again before returning.
	@[unsafe]
	pub fn vlibc_cond_wait(mut c VlibcCond, mut m VlibcMutex) {
		v := vlibc_load_u32(&c.seq)
		vlibc_mutex_unlock(mut m)
		vlibc_sys_futex(&c.seq, 0, v, unsafe { nil }, unsafe { nil }, 0)
		vlibc_mutex_lock(mut m)
	}

	// vlibc_cond_signal increments the sequence atomically and wakes one waiter.
	@[unsafe]
	pub fn vlibc_cond_signal(mut c VlibcCond) {
		vlibc_increment_u32(&c.seq)
		vlibc_sys_futex(&c.seq, 1, 1, unsafe { nil }, unsafe { nil }, 0)
	}

	// vlibc_cond_broadcast increments the sequence atomically and wakes all waiters.
	@[unsafe]
	pub fn vlibc_cond_broadcast(mut c VlibcCond) {
		vlibc_increment_u32(&c.seq)
		vlibc_sys_futex(&c.seq, 1, 0x7fffffff, unsafe { nil }, unsafe { nil }, 0)
	}
}
