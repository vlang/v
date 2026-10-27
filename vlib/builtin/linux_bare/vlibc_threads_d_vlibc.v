module builtin

$if !i386 {
	__global __global_vlibc_thread_fn VlibcThreadFn
	__global __global_vlibc_thread_arg u64
	__global __global_vlibc_thread_done &u32
	__global __global_clone_result i64
	__global __global_child_done u32

	// ---- atomic helpers ----
	pub fn vlibc_cas_u32(ptr &u32, old u32, new u32) bool {
		mut swapped := u8(0)
		$if arm64 {
			asm volatile arm64 {
				retry_cas:
				ldaxr w4, [ptr]
				cmp w4, old
				b.ne cas_fail
				stlxr w5, new, [ptr]
				cbnz w5, retry_cas
				mov swapped, #1
				b cas_done
				cas_fail:
				mov swapped, #0
				cas_done:
				; =r (swapped)
				; r (ptr)
				  r (old)
				  r (new)
				; memory
			}
		} $else {
			asm volatile amd64 {
				mov eax, old
				lock cmpxchg [ptr], new
				sete swapped
				; =r (swapped)
				; a (old)
				  r (ptr)
				  r (new)
				; memory
			}
		}
		return swapped != 0
	}

	pub fn vlibc_load_u32(ptr &u32) u32 {
		return unsafe { *ptr }
	}

	pub fn vlibc_store_u32(ptr &u32, val u32) {
		unsafe { *ptr = val }
	}

	// ---- threads ----
	pub type VlibcThreadFn = fn (u64)

	pub struct VlibcThread {
	pub:
		fn_ptr     VlibcThreadFn
		arg        u64
		stack      voidptr
		stack_size u64
		done       u32
		tid        i64
	}

	fn vlibc_clone_raw(newsp voidptr, fn_ptr voidptr, arg u64, done_ptr voidptr) i64 {
		__global_clone_result = 0
		$if arm64 {
			asm volatile arm64 {
				// save inputs in callee-saved regs
				mov x19, newsp
				mov x20, fn_ptr
				mov x21, arg
				mov x22, done_ptr
				// clone flags
				mov x0, #0x00000100
				orr x0, x0, #0x00010000
				orr x0, x0, #0x00000400
				orr x0, x0, #0x00000200
				orr x0, x0, #0x00000800
				orr x0, x0, #0x00040000
				orr x0, x0, #0x00000011
				mov x1, x19
				mov x2, #0
				mov x3, #0
				mov x4, #0
				mov x8, #220
				svc #0
				cmp x0, #0
				beq arm_child
				// parent
				mov __global_clone_result, x0
				b arm_done
				arm_child:
				// child: call fn(arg)
				mov x0, x21
				blr x20
				// set done = 1
				mov w9, #1
				str w9, [x22]
				// sys_exit(0)
				mov x0, #0
				mov x8, #93
				svc #0
				arm_done:
				; 
				; r (newsp)
				  r (fn_ptr)
				  r (arg)
				  r (done_ptr)
				; memory
			}
		} $else {
			asm volatile amd64 {
				push r12
				push r13
				push r14
				push r15
				// save inputs in callee-saved regs (preserved across syscall)
				mov r12, newsp
				mov r13, fn_ptr
				mov r14, arg
				mov r15, done_ptr
				// clone syscall
				mov rax, 56
				mov rdi, 0x00000100
				or rdi, 0x00010000
				or rdi, 0x00000400
				or rdi, 0x00000200
				or rdi, 0x00000800
				or rdi, 0x00040000
				or rdi, 0x00000011
				mov rsi, r12
				xor rdx, rdx
				xor r10, r10
				xor r8, r8
				syscall
				cmp rax, 0
				je amd_child
				// parent
				mov __global_clone_result, rax
				jmp amd_done
				amd_child:
				// child: call fn(arg) using globals
				mov rax, __global_vlibc_thread_fn
				mov rdi, __global_vlibc_thread_arg
				call rax
				// set done = 1
				mov __global_child_done, 1
				// sys_exit(0)
				mov rax, 60
				xor rdi, rdi
				syscall
				amd_done:
				pop r15
				pop r14
				pop r13
				pop r12
				; 
				; r (newsp)
				  r (fn_ptr)
				  r (arg)
				  r (done_ptr)
				; memory
			}
		}
		return __global_clone_result
	}

	pub fn vlibc_thread_spawn(fn_ptr VlibcThreadFn, arg u64) (VlibcThread, VlibcErrno) {
		mut t := VlibcThread{}
		t.fn_ptr = fn_ptr
		t.arg = arg
		t.stack_size = 1 << 20 // 1MB
		mem, e := vlibc_sys_mmap(unsafe { nil }, t.stack_size, 3, 0x22, -1, 0) // RW + PRIVATE|ANON
		if e != .enoerror {
			return t, e
		}
		t.stack = mem
		t.done = 0
		__global_vlibc_thread_fn = fn_ptr
		__global_vlibc_thread_arg = arg
		__global_child_done = 0
		// newsp: leave 64 bytes redzone below top
		newsp := unsafe { voidptr(u64(t.stack) + t.stack_size - 64) }
		tid := vlibc_clone_raw(newsp, fn_ptr, arg, &t.done)
		t.tid = tid
		if tid < 0 {
			vlibc_sys_munmap(t.stack, t.stack_size)
			return t, unsafe { VlibcErrno(-t.tid) }
		}
		// child path is handled inside vlibc_clone_raw (never returns here).
		return t, VlibcErrno.enoerror
	}

	pub fn vlibc_thread_join(mut t VlibcThread) VlibcErrno {
		for {
			if __global_child_done != 0 {
				break
			}
			vlibc_sleep_ns(1_000_000)
		}
		vlibc_sys_munmap(t.stack, t.stack_size)
		t.stack = unsafe { nil }
		return VlibcErrno.enoerror
	}

	// ---- mutex ----
	pub struct VlibcMutex {
	pub:
		state u32
	}

	pub fn vlibc_mutex_init() VlibcMutex {
		return VlibcMutex{ state: 0 }
	}

	pub fn vlibc_mutex_lock(mut m VlibcMutex) {
		for {
			if vlibc_cas_u32(&m.state, 0, 1) {
				return
			}
			// wait
			vlibc_sys_futex(&m.state, 0, 1, unsafe { nil }, unsafe { nil }, 0)
		}
	}

	pub fn vlibc_mutex_unlock(mut m VlibcMutex) {
		vlibc_store_u32(&m.state, 0)
		vlibc_sys_futex(&m.state, 1, 1, unsafe { nil }, unsafe { nil }, 0) // FUTEX_WAKE
	}

	// ---- condvar ----
	pub struct VlibcCond {
	pub:
		seq u32
	}

	pub fn vlibc_cond_init() VlibcCond {
		return VlibcCond{ seq: 0 }
	}

	pub fn vlibc_cond_wait(mut c VlibcCond, mut m VlibcMutex) {
		v := vlibc_load_u32(&c.seq)
		vlibc_mutex_unlock(mut m)
		vlibc_sys_futex(&c.seq, 0, v, unsafe { nil }, unsafe { nil }, 0)
		vlibc_mutex_lock(mut m)
	}

	pub fn vlibc_cond_signal(mut c VlibcCond) {
		vlibc_store_u32(&c.seq, vlibc_load_u32(&c.seq) + 1)
		vlibc_sys_futex(&c.seq, 1, 1, unsafe { nil }, unsafe { nil }, 0)
	}

	pub fn vlibc_cond_broadcast(mut c VlibcCond) {
		vlibc_store_u32(&c.seq, vlibc_load_u32(&c.seq) + 1)
		vlibc_sys_futex(&c.seq, 1, 0x7fffffff, unsafe { nil }, unsafe { nil }, 0)
	}
}
