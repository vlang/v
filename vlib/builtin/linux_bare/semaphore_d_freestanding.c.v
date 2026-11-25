module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	struct BareSemaphore {
	mut:
		count        u32
		private_flag u32
	}

	@[export: 'v_sem_init'; markused]
	fn bare_sem_init(address uint, shared i32, value u32) i32 {
		if value > 0x7fffffff { return i32(bare_native_result(u64(-i64(C.EINVAL)))) }
		mut sem := unsafe { &BareSemaphore(address) }
		sem.private_flag = if shared == 0 { u32(128) } else { u32(0) }
		bare_store_u32(&sem.count, value)
		return 0
	}

	@[export: 'v_sem_post'; markused]
	fn bare_sem_post(address uint) i32 {
		sem := unsafe { &BareSemaphore(address) }
		operation := uint(1 | sem.private_flag)
		mut value := bare_load_u32(&sem.count)
		for {
			if value == 0x7fffffff { return i32(bare_native_result(u64(-i64(C.EOVERFLOW)))) }
			if bare_cas_u32(&sem.count, value, value + 1) { break }
			value = bare_load_u32(&sem.count)
		}
		// After publishing the token the last waiter may destroy the semaphore.
		// Wake using the captured address and operation without reading it again.
		sys_call3(u64(C.SYS_futex), u64(address), u64(operation), 1)
		return 0
	}

	@[export: 'v_sem_value'; markused]
	fn bare_sem_value(address uint, output uint) i32 {
		sem := unsafe { &BareSemaphore(address) }
		unsafe { *(&i32(output)) = i32(bare_load_u32(&sem.count)) }
		return 0
	}

	@[export: 'v_sem_wait'; markused]
	fn bare_sem_wait(address uint, clock i32, deadline uint, try_only i32) i32 {
		if clock != 0 && clock != 1 { return i32(bare_native_result(u64(-i64(C.EINVAL)))) }
		if try_only == 0 { bare_pthread_testcancel() }
		sem := unsafe { &BareSemaphore(address) }
		mut operation := uint(9 | sem.private_flag)
		if clock == 0 && deadline != 0 { operation |= 256 }
		for {
			value := bare_load_u32(&sem.count)
			if value != 0 {
				if bare_cas_u32(&sem.count, value, value - 1) { return 0 }
				continue
			}
			if try_only != 0 { return i32(bare_native_result(u64(-i64(C.EAGAIN)))) }
			if deadline != 0 {
				t := unsafe { &i64(deadline) }
				if unsafe { t[1] < 0 || t[1] >= 1_000_000_000 } {
					return i32(bare_native_result(u64(-i64(C.EINVAL))))
				}
				if unsafe { t[0] < 0 } { return i32(bare_native_result(u64(-i64(C.ETIMEDOUT)))) }
			}
			status := bare_cancel_syscall(i64(C.SYS_futex), address, operation, 0, deadline, 0, uint(~u32(0)))
			if status != 0 && status != -i64(C.EAGAIN) {
				return i32(bare_native_result(u64(status)))
			}
		}
		return 0
	}
}
