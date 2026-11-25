module builtin

$if !i386 {
	// vlibc_cas_u32 compares and exchanges a word with acquire/release ordering.
	@[inline]
	pub fn vlibc_cas_u32(ptr &u32, old u32, new u32) bool {
		return bare_cas_u32(ptr, old, new)
	}

	// vlibc_load_u32 loads a word with acquire ordering.
	@[inline]
	pub fn vlibc_load_u32(ptr &u32) u32 { return bare_load_u32(ptr) }

	// vlibc_store_u32 stores a word with release ordering.
	@[inline]
	pub fn vlibc_store_u32(ptr &u32, val u32) { bare_store_u32(ptr, val) }

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

	$if freestanding && !native && !v_bare_no_startup ? {
		// The child owns this heap payload independently of the spawning arena.
		@[heap]
		struct BarePublicThreadStart {
			callback VlibcThreadFn
			argument u64
		}
		fn bare_public_thread_entry(argument u64) {
			start := unsafe { &BarePublicThreadStart(argument) }
			callback := start.callback
			value := start.argument
			unsafe { __free(start) }
			callback(value)
			C.v_freestanding_thread_cleanup()
			bare_credential_thread_leave()
		}
	}

	// vlibc_thread_spawn starts fn_ptr(arg) with independent TLS. Libc-enabled builds
	// use a native pthread; standalone builds map a 1 MiB stack and ELF TLS image.
	pub fn vlibc_thread_spawn(fn_ptr VlibcThreadFn, arg u64) (VlibcThread, VlibcErrno) {
		$if !freestanding || v_bare_libc ? {
			return vlibc_hosted_thread_spawn(fn_ptr, arg)
		}
		$if freestanding && !native && !v_bare_no_startup ? {
			bare_compat_lock(&bare_credential_mutex)
			defer { bare_compat_unlock(&bare_credential_mutex) }
			start := unsafe { &BarePublicThreadStart(__malloc(sizeof(BarePublicThreadStart))) }
			if start == unsafe { nil } { return VlibcThread{}, unsafe { VlibcErrno(12) } }
			unsafe { *start = BarePublicThreadStart{ callback: fn_ptr, argument: arg } }
			bare_credential_threads++
			t, code := bare_thread_mapping_spawn(bare_public_thread_entry, u64(start), 1 << 20)
			if code != .enoerror {
				bare_credential_threads--
				unsafe { __free(start) }
			}
			return t, unsafe { VlibcErrno(code) }
		}
		t, code := bare_thread_mapping_spawn(fn_ptr, arg, 1 << 20)
		return t, unsafe { VlibcErrno(code) }
	}

	// vlibc_thread_join waits for thread exit and releases the owned thread resources.
	pub fn vlibc_thread_join(mut t VlibcThread) VlibcErrno {
		$if !freestanding || v_bare_libc ? {
			return vlibc_hosted_thread_join(mut t)
		}
		return unsafe { VlibcErrno(bare_thread_mapping_join(mut t)) }
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
	pub fn vlibc_cond_wait(mut c VlibcCond, mut m VlibcMutex) {
		v := vlibc_load_u32(&c.seq)
		vlibc_mutex_unlock(mut m)
		vlibc_sys_futex(&c.seq, 0, v, unsafe { nil }, unsafe { nil }, 0)
		vlibc_mutex_lock(mut m)
	}

	// vlibc_cond_signal increments the sequence atomically and wakes one waiter.
	pub fn vlibc_cond_signal(mut c VlibcCond) {
		vlibc_increment_u32(&c.seq)
		vlibc_sys_futex(&c.seq, 1, 1, unsafe { nil }, unsafe { nil }, 0)
	}

	// vlibc_cond_broadcast increments the sequence atomically and wakes all waiters.
	pub fn vlibc_cond_broadcast(mut c VlibcCond) {
		vlibc_increment_u32(&c.seq)
		vlibc_sys_futex(&c.seq, 1, 0x7fffffff, unsafe { nil }, unsafe { nil }, 0)
	}
}
