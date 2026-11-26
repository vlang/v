@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	__global bare_credential_mutex u32
	// Count the other live threads. The initial thread and the sole survivor of
	// fork start with zero. Any thread, including the initial one, may leave first.
	__global bare_credential_threads uint

	fn bare_credential_thread_leave() {
		bare_compat_lock(&bare_credential_mutex)
		if bare_credential_threads == 0 {
			bare_compat_unlock(&bare_credential_mutex)
			__exit(0)
		}
		bare_credential_threads--
		bare_compat_unlock(&bare_credential_mutex)
	}

	// Kernel credential setters affect one task. Reject a multithreaded change
	// until a process-wide rendezvous is available; never leave mixed credentials.
	@[export: 'v_native_credentials'; markused]
	fn bare_native_credentials(operation i32, first u32, second u32, third u32, groups uint, count uint) i32 {
		bare_compat_lock(&bare_credential_mutex)
		defer { bare_compat_unlock(&bare_credential_mutex) }
		if bare_credential_threads != 0 {
			bf_native_errno_cell = i32(C.ENOTSUP)
			return -1
		}
		result := match operation {
			0 { sys_call1(u64(C.SYS_setuid), u64(first)) }
			1 { sys_call1(u64(C.SYS_setgid), u64(first)) }
			2 { sys_call2(u64(C.SYS_setreuid), u64(first), u64(second)) }
			3 { sys_call2(u64(C.SYS_setregid), u64(first), u64(second)) }
			4 { sys_call3(u64(C.SYS_setresuid), u64(first), u64(second), u64(third)) }
			5 { sys_call3(u64(C.SYS_setresgid), u64(first), u64(second), u64(third)) }
			6 {
				if count > 65536 {
					bf_native_errno_cell = i32(C.EINVAL)
					return -1
				}
				sys_call2(u64(C.SYS_setgroups), u64(count), u64(groups))
			}
			7 {
				if first == ~u32(0) {
					bf_native_errno_cell = i32(C.EINVAL)
					return -1
				}
				sys_call3(u64(C.SYS_setresuid), u64(~u32(0)), u64(first), u64(~u32(0)))
			}
			8 {
				if first == ~u32(0) {
					bf_native_errno_cell = i32(C.EINVAL)
					return -1
				}
				sys_call3(u64(C.SYS_setresgid), u64(~u32(0)), u64(first), u64(~u32(0)))
			}
			else {
				bf_native_errno_cell = i32(C.EINVAL)
				return -1
			}
		}
		return i32(bare_native_result(result))
	}
}
