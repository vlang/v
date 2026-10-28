module builtin

$if !i386 {
	pub fn vlibc_fork() i64 {
		$if arm64 {
			return vlibc_sys_clone(0x00000011, unsafe { nil }, unsafe { nil }, unsafe { nil }, unsafe { nil })
		} $else {
			rc := sys_call0(vlibc_sys_nr_fork)
			return i64(rc)
		}
	}

	pub fn vlibc_execve(path string, argv []string, envp []string) int {
		// convert []string to []&u8 for execve
		mut a := []&u8{cap: argv.len + 1}
		for s in argv {
			a << s.str
		}
		a << unsafe { nil }
		mut e := []&u8{cap: envp.len + 1}
		for s in envp {
			e << s.str
		}
		e << unsafe { nil }
		return vlibc_sys_execve(path.str, a, e)
	}

	pub fn vlibc_waitpid(pid i64) (i64, VlibcErrno) {
		mut status := 0
		e := vlibc_sys_waitid(.p_pid, int(pid), &status, 0x00000004 | 0x00000002 | 0x00000001, unsafe { nil })
		if e != .enoerror {
			return -1, e
		}
		return i64(pid), VlibcErrno.enoerror
	}

	pub fn vlibc_pipe() ([2]int, VlibcErrno) {
		mut fds := [0, 0]
		e := vlibc_sys_pipe2(&fds[0], 0)
		if e != .enoerror {
			return fds, e
		}
		// The kernel writes two 32-bit C ints, which land packed in the
		// first 64-bit slot (all vlibc targets are little-endian); split
		// them so callers get one fd per element.
		mut res := [2]int{}
		res[0] = fds[0] & 0xffff_ffff
		res[1] = (fds[0] >> 32) & 0xffff_ffff
		return res, VlibcErrno.enoerror
	}

	pub fn vlibc_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		return vlibc_sys_dup2(oldfd, newfd)
	}

	pub fn vlibc_signal(sig int, handler voidptr) VlibcErrno {
		// rt_sigaction with empty mask
		mut sa := [32]u8{} // opaque; kernel expects struct sigaction (size ~152 on x86_64)
		// We cannot easily build full sigaction without struct; provide minimal via kernel's
		// expected layout is complex. Instead use the legacy signal() syscall? not available.
		// For now, return ENOSYS to avoid unsafe partial structs.
		_ = sa
		_ = sig
		_ = handler
		return VlibcErrno.enotsys
	}

	pub fn vlibc_kill(pid i64, sig int) VlibcErrno {
		return vlibc_sys_kill(pid, sig)
	}
}
