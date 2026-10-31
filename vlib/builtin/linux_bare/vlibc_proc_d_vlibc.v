module builtin

$if !i386 {
	// vlibc_fork creates a child process, returning its PID to the parent and zero to the child.
	pub fn vlibc_fork() i64 {
		$if arm64 {
			return vlibc_sys_clone(0x00000011, unsafe { nil }, unsafe { nil }, unsafe { nil }, unsafe { nil })
		} $else {
			rc := sys_call0(vlibc_sys_nr_fork)
			return i64(rc)
		}
	}

	// vlibc_execve replaces the process image using string argument and environment arrays.
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

	// vlibc_waitpid waits for pid to exit and returns the reaped process id.
	pub fn vlibc_waitpid(pid i64) (i64, VlibcErrno) {
		mut status := i32(0)
		for {
			waited, e := vlibc_sys_wait4(pid, &status, 0, unsafe { nil })
			if e == .eintr { continue }
			return waited, e
		}
	}

	// vlibc_pipe creates a pipe using the kernel's 32-bit descriptor layout.
	pub fn vlibc_pipe() ([2]int, VlibcErrno) {
		mut fds := [2]i32{}
		e := vlibc_sys_pipe2(&fds[0], 0)
		return [int(fds[0]), int(fds[1])]!, e
	}

	// vlibc_dup2 duplicates oldfd onto newfd, preserving POSIX same-descriptor behavior.
	pub fn vlibc_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		return vlibc_sys_dup2(oldfd, newfd)
	}

	// vlibc_signal returns ENOSYS because installing signal handlers is not implemented.
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

	// vlibc_kill sends a signal to a process or process group.
	pub fn vlibc_kill(pid i64, sig int) VlibcErrno {
		return vlibc_sys_kill(pid, sig)
	}
}
