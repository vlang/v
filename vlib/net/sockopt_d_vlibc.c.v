module net

// Hosted `-d vlibc` sockopt/fcntl bridge.
//
// vlibc_get_so_error routes SO_ERROR reads through the vlibc getsockopt
// syscall, so it is available on every vlibc target: linux_bare and the
// BSD/macOS/Solaris bare targets all provide vlibc_sys_setsockopt/
// getsockopt. `set_option`/`set_read_timeout` call the vlibc syscalls
// directly at their call sites. Windows is untouched (it never defines
// `linux`). getpeername stays on libc everywhere: in hosted mode libc is
// still linked, so it works on vlibc-created fds too.

$if vlibc ? {
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
}

$if linux {
	// vlibc_set_blocking toggles O_NONBLOCK via the vlibc fcntl syscall.
	// Linux-only: macOS vlibc_sys_fcntl takes a voidptr arg, so this shared
	// helper does not typecheck there; other vlibc targets keep the libc
	// fcntl path at the call site in socket.c.v.
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
