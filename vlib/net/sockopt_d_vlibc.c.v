module net

// Hosted `-d vlibc` sockopt/fcntl bridge (Linux-only syscalls).
//
// The helpers below route SO_ERROR reads and the non-blocking fcntl setup
// through the vlibc syscalls (vlibc_sys_getsockopt/vlibc_sys_fcntl from
// linux_bare) instead of libc. They are guarded by `$if linux`, so other
// vlibc targets keep the libc path at the call sites: only linux_bare
// provides vlibc_sys_setsockopt/getsockopt. `set_option`/`set_read_timeout`
// call the vlibc syscalls directly at their call sites. Windows is
// untouched (it never defines `linux`). getpeername stays on libc
// everywhere: no vlibc_sys_getpeername wrapper exists yet, and in hosted
// mode libc is still linked, so it works on vlibc-created fds.

$if linux {
	// vlibc_get_so_error reads SO_ERROR, mirroring the C.getsockopt call it
	// replaces: xyz is 0 on success (err holds the pending socket error),
	// -1 when the syscall itself failed.
	fn vlibc_get_so_error(handle int) (int, i32) {
		v, e := vlibc_getsockopt_int(i64(handle), C.SOL_SOCKET, C.SO_ERROR)
		if e != .enoerror {
			return -1, i32(0)
		}
		return 0, i32(v)
	}

	// vlibc_set_blocking toggles O_NONBLOCK via the vlibc fcntl syscall.
	fn vlibc_set_blocking(handle int, state bool) ! {
		fl, e := vlibc_sys_fcntl(i64(handle), C.F_GETFL, 0)
		vlibc_wrap_err(e)!
		mut flags := int(fl)
		nonblock := int(C.O_NONBLOCK)
		if state {
			flags &= ~nonblock
		} else {
			flags |= nonblock
		}
		_, e2 := vlibc_sys_fcntl(i64(handle), C.F_SETFL, i64(flags))
		vlibc_wrap_err(e2)!
	}
}
