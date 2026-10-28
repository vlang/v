module net

// Hosted `-d vlibc` sockopt/fcntl bridge.
//
// SO_ERROR reads go through the vlibc getsockopt syscall on Linux only
// (linux_bare is the only target providing vlibc_sys_getsockopt); other
// vlibc targets keep the libc call at the call sites in common.c.v and
// tcp.c.v. `set_option`/`set_read_timeout` call the vlibc_sys_setsockopt
// syscall directly at their call sites on Linux. Windows is untouched
// (it never defines `linux`). getpeername stays on libc everywhere:
// no vlibc_sys_getpeername wrapper exists yet, and in hosted mode libc
// is still linked, so it works on vlibc-created fds.
//
// The non-blocking fcntl setup below goes through vlibc_sys_fcntl, which
// exists on all six bare targets, so it applies to every `-d vlibc`
// build. Only the arg passing differs: macOS takes the fcntl value as a
// pointer-sized voidptr, the other targets as i64.

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
}

$if !windows {
	// vlibc_set_blocking toggles O_NONBLOCK via the vlibc fcntl syscall.
	fn vlibc_set_blocking(handle int, state bool) ! {
		nonblock := int(C.O_NONBLOCK)
		$if macos {
			fl, e := vlibc_sys_fcntl(i64(handle), C.F_GETFL, unsafe { nil })
			vlibc_wrap_err(e)!
			mut flags := int(fl)
			if state {
				flags &= ~nonblock
			} else {
				flags |= nonblock
			}
			_, e2 := vlibc_sys_fcntl(i64(handle), C.F_SETFL, unsafe { voidptr(u64(flags)) })
			vlibc_wrap_err(e2)!
		} $else {
			fl, e := vlibc_sys_fcntl(i64(handle), C.F_GETFL, 0)
			vlibc_wrap_err(e)!
			mut flags := int(fl)
			if state {
				flags &= ~nonblock
			} else {
				flags |= nonblock
			}
			_, e2 := vlibc_sys_fcntl(i64(handle), C.F_SETFL, i64(flags))
			vlibc_wrap_err(e2)!
		}
	}
}
