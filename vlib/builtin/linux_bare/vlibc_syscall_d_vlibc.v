module builtin

// vlibc: experimental syscall layer for freestanding V.
// Gated behind -d vlibc (file suffix). Provides kernel-ABI syscalls for both
// glibc and musl hosts (kernel ABI is identical, so no libc-specific code).

$if !i386 {
	// ---- errno ----
	// VlibcErrno is defined in vlibc_errno_d_vlibc.v.

	// ---- syscall numbers ----
	$if arm64 {
		const vlibc_sys_nr_openat = u64(56)
		const vlibc_sys_nr_close = u64(57)
		const vlibc_sys_nr_lseek = u64(62)
		const vlibc_sys_nr_statx = u64(291)
		const vlibc_sys_nr_getcwd = u64(17)
		const vlibc_sys_nr_chdir = u64(49)
		const vlibc_sys_nr_unlinkat = u64(35)
		const vlibc_sys_nr_mkdirat = u64(34)
		const vlibc_sys_nr_renameat = u64(38)
		const vlibc_sys_nr_getdents64 = u64(61)
		const vlibc_sys_nr_clock_gettime = u64(113)
		const vlibc_sys_nr_nanosleep = u64(101)
		const vlibc_sys_nr_uname = u64(160)
		const vlibc_sys_nr_getpid = u64(172)
		const vlibc_sys_nr_getppid = u64(173)
		const vlibc_sys_nr_gettid = u64(178)
		const vlibc_sys_nr_getuid = u64(174)
		const vlibc_sys_nr_geteuid = u64(175)
		const vlibc_sys_nr_kill = u64(129)
		const vlibc_sys_nr_tgkill = u64(131)
		const vlibc_sys_nr_rt_sigaction = u64(134)
		const vlibc_sys_nr_rt_sigprocmask = u64(135)
		const vlibc_sys_nr_sigaltstack = u64(132)
		const vlibc_sys_nr_futex = u64(98)
		const vlibc_sys_nr_set_tid_address = u64(96)
		const vlibc_sys_nr_clone = u64(220)
		const vlibc_sys_nr_exit = u64(93)
		const vlibc_sys_nr_exit_group = u64(94)
		const vlibc_sys_nr_execve = u64(221)
		const vlibc_sys_nr_waitid = u64(95)
		const vlibc_sys_nr_wait4 = u64(260)
		const vlibc_sys_nr_fcntl = u64(25)
		const vlibc_sys_nr_ioctl = u64(29)
		const vlibc_sys_nr_pipe2 = u64(59)
		const vlibc_sys_nr_dup3 = u64(24)
		const vlibc_sys_nr_mprotect = u64(226)
		const vlibc_sys_nr_mmap = u64(222)
		const vlibc_sys_nr_munmap = u64(215)
		const vlibc_sys_nr_mremap = u64(216)
		const vlibc_sys_nr_madvise = u64(233)
		const vlibc_sys_nr_socket = u64(198)
		const vlibc_sys_nr_socketpair = u64(199)
		const vlibc_sys_nr_bind = u64(200)
		const vlibc_sys_nr_listen = u64(201)
		const vlibc_sys_nr_accept4 = u64(242)
		const vlibc_sys_nr_connect = u64(203)
		const vlibc_sys_nr_sendto = u64(206)
		const vlibc_sys_nr_recvfrom = u64(207)
		const vlibc_sys_nr_shutdown = u64(210)
		const vlibc_sys_nr_getsockname = u64(204)
		const vlibc_sys_nr_setsockopt = u64(208)
		const vlibc_sys_nr_getsockopt = u64(209)
		const vlibc_sys_nr_sendmsg = u64(211)
		const vlibc_sys_nr_recvmsg = u64(212)
		const vlibc_sys_nr_fork = u64(0) // use clone
	} $else {
		const vlibc_sys_nr_openat = u64(257)
		const vlibc_sys_nr_close = u64(3)
		const vlibc_sys_nr_lseek = u64(8)
		const vlibc_sys_nr_statx = u64(332)
		const vlibc_sys_nr_getcwd = u64(79)
		const vlibc_sys_nr_chdir = u64(80)
		const vlibc_sys_nr_unlinkat = u64(263)
		const vlibc_sys_nr_mkdirat = u64(258)
		const vlibc_sys_nr_renameat = u64(264)
		const vlibc_sys_nr_getdents64 = u64(217)
		const vlibc_sys_nr_clock_gettime = u64(228)
		const vlibc_sys_nr_nanosleep = u64(35)
		const vlibc_sys_nr_uname = u64(63)
		const vlibc_sys_nr_getpid = u64(39)
		const vlibc_sys_nr_getppid = u64(110)
		const vlibc_sys_nr_gettid = u64(186)
		const vlibc_sys_nr_getuid = u64(102)
		const vlibc_sys_nr_geteuid = u64(107)
		const vlibc_sys_nr_kill = u64(62)
		const vlibc_sys_nr_tgkill = u64(234)
		const vlibc_sys_nr_rt_sigaction = u64(13)
		const vlibc_sys_nr_rt_sigprocmask = u64(14)
		const vlibc_sys_nr_sigaltstack = u64(131)
		const vlibc_sys_nr_futex = u64(202)
		const vlibc_sys_nr_set_tid_address = u64(218)
		const vlibc_sys_nr_clone = u64(56)
		const vlibc_sys_nr_exit = u64(60)
		const vlibc_sys_nr_exit_group = u64(231)
		const vlibc_sys_nr_execve = u64(59)
		const vlibc_sys_nr_waitid = u64(247)
		const vlibc_sys_nr_wait4 = u64(61)
		const vlibc_sys_nr_fcntl = u64(72)
		const vlibc_sys_nr_ioctl = u64(16)
		const vlibc_sys_nr_pipe2 = u64(293)
		const vlibc_sys_nr_dup2 = u64(33)
		const vlibc_sys_nr_dup3 = u64(292)
		const vlibc_sys_nr_mprotect = u64(10)
		const vlibc_sys_nr_mmap = u64(9)
		const vlibc_sys_nr_munmap = u64(11)
		const vlibc_sys_nr_mremap = u64(25)
		const vlibc_sys_nr_madvise = u64(28)
		const vlibc_sys_nr_socket = u64(41)
		const vlibc_sys_nr_socketpair = u64(53)
		const vlibc_sys_nr_bind = u64(49)
		const vlibc_sys_nr_listen = u64(50)
		const vlibc_sys_nr_accept4 = u64(288)
		const vlibc_sys_nr_connect = u64(42)
		const vlibc_sys_nr_sendto = u64(44)
		const vlibc_sys_nr_recvfrom = u64(45)
		const vlibc_sys_nr_shutdown = u64(48)
		const vlibc_sys_nr_getsockname = u64(51)
		const vlibc_sys_nr_setsockopt = u64(54)
		const vlibc_sys_nr_getsockopt = u64(55)
		const vlibc_sys_nr_sendmsg = u64(46)
		const vlibc_sys_nr_recvmsg = u64(47)
		const vlibc_sys_nr_fork = u64(57)
	}

	// ---- wrappers (use sys_callN directly) ----
	fn vlibc_split(rc_in u64) (i64, VlibcErrno) {
		rc := i64(rc_in)
		if rc < 0 {
			return i64(-1), unsafe { VlibcErrno(-rc) }
		}
		return rc, VlibcErrno.enoerror
	}

	// vlibc_sys_openat opens a path relative to a directory descriptor with native flags and mode.
	pub fn vlibc_sys_openat(dirfd i64, path &u8, flags u64, mode u64) (i64, VlibcErrno) {
		$if arm64 {
			return vlibc_split(sys_call4(vlibc_sys_nr_openat, u64(dirfd), u64(path), flags, mode))
		} $else {
			return vlibc_split(sys_call4(vlibc_sys_nr_openat, u64(dirfd), u64(path), flags, mode))
		}
	}

	// vlibc_sys_close closes a descriptor and returns the kernel error.
	pub fn vlibc_sys_close(fd i64) VlibcErrno {
		rc := sys_call1(vlibc_sys_nr_close, u64(fd))
		if rc == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_read reads at most count bytes using the architecture-specific read syscall.
	pub fn vlibc_sys_read(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(sys_nr_read, u64(fd), u64(buf), count))
	}

	// vlibc_sys_write writes at most count bytes using the architecture-specific write syscall.
	pub fn vlibc_sys_write(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(sys_nr_write, u64(fd), u64(buf), count))
	}

	// vlibc_sys_lseek changes a descriptor's byte offset relative to whence.
	pub fn vlibc_sys_lseek(fd i64, off i64, whence int) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(vlibc_sys_nr_lseek, u64(fd), u64(off), u64(whence)))
	}

	// vlibc_sys_statx requests extended metadata for a path relative to a directory descriptor.
	pub fn vlibc_sys_statx(dirfd i64, path &u8, flags u32, mask u32, buf &VlibcStatx) VlibcErrno {
		rc := sys_call5(vlibc_sys_nr_statx, u64(dirfd), u64(path), u64(flags), u64(mask), u64(buf))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_getcwd writes the working directory into a byte buffer.
	pub fn vlibc_sys_getcwd(buf &u8, size u64) (i64, VlibcErrno) {
		return vlibc_split(sys_call2(vlibc_sys_nr_getcwd, u64(buf), size))
	}

	// vlibc_sys_chdir changes the working directory using a NUL-terminated path.
	pub fn vlibc_sys_chdir(path &u8) VlibcErrno {
		rc := sys_call1(vlibc_sys_nr_chdir, u64(path))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_unlinkat removes a path relative to a directory descriptor.
	pub fn vlibc_sys_unlinkat(dirfd i64, path &u8, flags int) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_unlinkat, u64(dirfd), u64(path), u64(flags))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_mkdirat creates a directory relative to a directory descriptor.
	pub fn vlibc_sys_mkdirat(dirfd i64, path &u8, mode u32) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_mkdirat, u64(dirfd), u64(path), u64(mode))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_renameat renames a path relative to the supplied directory descriptors.
	pub fn vlibc_sys_renameat(olddir i64, oldpath &u8, newdir i64, newpath &u8) VlibcErrno {
		rc := sys_call4(vlibc_sys_nr_renameat, u64(olddir), u64(oldpath), u64(newdir), u64(newpath))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_getdents64 reads native 64-bit directory records into a byte buffer.
	pub fn vlibc_sys_getdents64(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(vlibc_sys_nr_getdents64, u64(fd), u64(buf), count))
	}

	// vlibc_sys_clock_gettime writes the selected clock value into a native timespec.
	pub fn vlibc_sys_clock_gettime(clk int, tp &VlibcTimespec) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_clock_gettime, u64(clk), u64(tp))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_nanosleep requests a sleep and optionally reports the remaining duration.
	pub fn vlibc_sys_nanosleep(req &VlibcTimespec, rem &VlibcTimespec) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_nanosleep, u64(req), u64(rem))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_uname writes the kernel's system identification into buf.
	pub fn vlibc_sys_uname(buf &VlibcUtsname) VlibcErrno {
		rc := sys_call1(vlibc_sys_nr_uname, u64(buf))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_getpid returns the current process ID.
	pub fn vlibc_sys_getpid() i64 {
		return i64(sys_call0(vlibc_sys_nr_getpid))
	}

	// vlibc_sys_getppid returns the parent process ID.
	pub fn vlibc_sys_getppid() i64 {
		return i64(sys_call0(vlibc_sys_nr_getppid))
	}

	// vlibc_sys_gettid returns the current kernel thread ID.
	pub fn vlibc_sys_gettid() i64 {
		return i64(sys_call0(vlibc_sys_nr_gettid))
	}

	// vlibc_sys_getuid returns the real user ID.
	pub fn vlibc_sys_getuid() i64 {
		return i64(sys_call0(vlibc_sys_nr_getuid))
	}

	// vlibc_sys_geteuid returns the effective user ID.
	pub fn vlibc_sys_geteuid() i64 {
		return i64(sys_call0(vlibc_sys_nr_geteuid))
	}

	// vlibc_sys_kill sends a signal to a process or process group.
	pub fn vlibc_sys_kill(pid i64, sig int) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_kill, u64(pid), u64(sig))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_tgkill sends a signal to a specific thread within a process.
	pub fn vlibc_sys_tgkill(tgid i64, tid i64, sig int) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_tgkill, u64(tgid), u64(tid), u64(sig))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_futex performs a native futex operation and returns the raw result.
	pub fn vlibc_sys_futex(uaddr voidptr, op int, val u32, timeout voidptr, uaddr2 voidptr, val3 u32) i64 {
		return i64(sys_call6(vlibc_sys_nr_futex, u64(uaddr), u64(op), u64(val), u64(timeout), u64(uaddr2), u64(val3)))
	}

	// vlibc_sys_set_tid_address sets the address the kernel clears when the calling thread exits.
	pub fn vlibc_sys_set_tid_address(tidptr voidptr) i64 {
		return i64(sys_call1(vlibc_sys_nr_set_tid_address, u64(tidptr)))
	}

	// vlibc_sys_clone invokes clone with the architecture-specific argument order.
	pub fn vlibc_sys_clone(flags u64, newsp voidptr, parent_tid voidptr, child_tid voidptr, tls voidptr) i64 {
		$if arm64 {
			// arm64 clone arg order: flags, newsp, parent_tid, tls, child_tid
			return i64(sys_call5(vlibc_sys_nr_clone, flags, u64(newsp), u64(parent_tid), u64(tls), u64(child_tid)))
		} $else {
			// x86_64: flags, newsp, parent_tid, child_tid, tls
			return i64(sys_call5(vlibc_sys_nr_clone, flags, u64(newsp), u64(parent_tid), u64(child_tid), u64(tls)))
		}
	}

	// vlibc_sys_exit terminates the calling process with the given exit code.
	pub fn vlibc_sys_exit(code int) {
		sys_call1(vlibc_sys_nr_exit, u64(code))
		for {}
	}

	// vlibc_sys_exit_group terminates all threads in the process with the given exit code.
	pub fn vlibc_sys_exit_group(code int) {
		sys_call1(vlibc_sys_nr_exit_group, u64(code))
		for {}
	}

	// vlibc_sys_execve replaces the process image using NUL-terminated pointer arrays.
	pub fn vlibc_sys_execve(filename &u8, argv []&u8, envp []&u8) int {
		return int(sys_call3(vlibc_sys_nr_execve, u64(filename), u64(argv.data), u64(envp.data)))
	}

	// vlibc_sys_wait4 waits for a child and writes the raw POSIX status to an i32.
	pub fn vlibc_sys_wait4(pid i64, status &i32, options int, ru voidptr) (i64, VlibcErrno) {
		return vlibc_split(sys_call4(vlibc_sys_nr_wait4, u64(pid), u64(status), u64(options), u64(ru)))
	}

	// vlibc_sys_waitid waits for a child state change, writing a full kernel siginfo_t to infop.
	pub fn vlibc_sys_waitid(which WiWhich, pid int, infop voidptr, options int, ru voidptr) VlibcErrno {
		rc := sys_call5(vlibc_sys_nr_waitid, u64(which), u64(pid), u64(infop), u64(options), u64(ru))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_fcntl performs a native descriptor-control operation.
	pub fn vlibc_sys_fcntl(fd i64, cmd int, arg i64) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(vlibc_sys_nr_fcntl, u64(fd), u64(cmd), u64(arg)))
	}

	// vlibc_sys_ioctl performs a native device-control operation.
	pub fn vlibc_sys_ioctl(fd i64, req u64, arg voidptr) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(vlibc_sys_nr_ioctl, u64(fd), req, u64(arg)))
	}

	// vlibc_sys_pipe2 writes two kernel i32 descriptors to filedes.
	pub fn vlibc_sys_pipe2(filedes &i32, flags int) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_pipe2, u64(filedes), u64(flags))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_dup2 duplicates oldfd onto newfd, including the same-fd validity check.
	pub fn vlibc_sys_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		$if arm64 {
			if oldfd == newfd {
				_, e := vlibc_sys_fcntl(i64(oldfd), 1, 0) // F_GETFD checks oldfd.
				if e != .enoerror { return -1, e }
				return i64(newfd), .enoerror
			}
			return vlibc_sys_dup3(oldfd, newfd, 0)
		} $else {
			return vlibc_split(sys_call2(vlibc_sys_nr_dup2, u64(oldfd), u64(newfd)))
		}
	}

	// vlibc_sys_dup3 duplicates a descriptor with native duplication flags.
	pub fn vlibc_sys_dup3(oldfd int, newfd int, flags int) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(vlibc_sys_nr_dup3, u64(oldfd), u64(newfd), u64(flags)))
	}

	// vlibc_sys_mprotect changes the protection of a mapped memory range.
	pub fn vlibc_sys_mprotect(addr voidptr, len u64, prot int) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_mprotect, u64(addr), len, u64(prot))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_mmap maps memory using native protection and mapping flags.
	pub fn vlibc_sys_mmap(addr voidptr, len u64, prot int, flags int, fd i64, off u64) (voidptr, VlibcErrno) {
		rc := sys_call6(vlibc_sys_nr_mmap, u64(addr), len, u64(prot), u64(flags), u64(fd), off)
		if i64(rc) < 0 {
			return unsafe { nil }, unsafe { VlibcErrno(-i64(rc)) }
		}
		return unsafe { voidptr(rc) }, VlibcErrno.enoerror
	}

	// vlibc_sys_munmap unmaps a memory range.
	pub fn vlibc_sys_munmap(addr voidptr, len u64) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_munmap, u64(addr), len)
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_madvise advises the kernel about a mapped memory range.
	pub fn vlibc_sys_madvise(addr voidptr, len u64, advice int) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_madvise, u64(addr), len, u64(advice))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_socket creates a socket with native domain, type, and protocol values.
	pub fn vlibc_sys_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		return vlibc_split(sys_call3(vlibc_sys_nr_socket, u64(domain), u64(typ), u64(protocol)))
	}

	// vlibc_sys_socketpair writes two kernel i32 socket descriptors to sv.
	pub fn vlibc_sys_socketpair(domain int, typ int, protocol int, sv &i32) VlibcErrno {
		rc := sys_call4(vlibc_sys_nr_socketpair, u64(domain), u64(typ), u64(protocol), u64(sv))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_bind binds a socket using a native address buffer.
	pub fn vlibc_sys_bind(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_bind, u64(fd), u64(addr), u64(addrlen))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_listen marks a socket as accepting connections with the given backlog.
	pub fn vlibc_sys_listen(fd i64, backlog int) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_listen, u64(fd), u64(backlog))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_accept4 accepts a connection with native descriptor flags and address buffers.
	pub fn vlibc_sys_accept4(fd i64, addr voidptr, addrlen &u32, flags int) (i64, VlibcErrno) {
		return vlibc_split(sys_call4(vlibc_sys_nr_accept4, u64(fd), u64(addr), u64(addrlen), u64(flags)))
	}

	// vlibc_sys_connect connects a socket using a native address buffer.
	pub fn vlibc_sys_connect(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_connect, u64(fd), u64(addr), u64(addrlen))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_sendto sends bytes to an optional native destination address.
	pub fn vlibc_sys_sendto(fd i64, buf voidptr, len u64, flags int, dest voidptr, addrlen u32) (i64, VlibcErrno) {
		return vlibc_split(sys_call6(vlibc_sys_nr_sendto, u64(fd), u64(buf), len, u64(flags), u64(dest), u64(addrlen)))
	}

	// vlibc_sys_recvfrom receives bytes and optionally writes the peer's native socket address.
	pub fn vlibc_sys_recvfrom(fd i64, buf voidptr, len u64, flags int, src voidptr, addrlen &u32) (i64, VlibcErrno) {
		return vlibc_split(sys_call6(vlibc_sys_nr_recvfrom, u64(fd), u64(buf), len, u64(flags), u64(src), u64(addrlen)))
	}

	// vlibc_sys_shutdown disables the requested socket directions.
	pub fn vlibc_sys_shutdown(fd i64, how int) VlibcErrno {
		rc := sys_call2(vlibc_sys_nr_shutdown, u64(fd), u64(how))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_getsockname writes the local socket address into native buffers.
	pub fn vlibc_sys_getsockname(fd i64, addr voidptr, addrlen &u32) VlibcErrno {
		rc := sys_call3(vlibc_sys_nr_getsockname, u64(fd), u64(addr), u64(addrlen))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_setsockopt sets a socket option from a native value buffer.
	pub fn vlibc_sys_setsockopt(fd i64, level int, optname int, optval voidptr, optlen u32) VlibcErrno {
		rc := sys_call5(vlibc_sys_nr_setsockopt, u64(fd), u64(level), u64(optname), u64(optval), u64(optlen))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}

	// vlibc_sys_getsockopt reads a socket option into native buffers.
	pub fn vlibc_sys_getsockopt(fd i64, level int, optname int, optval voidptr, optlen &u32) VlibcErrno {
		rc := sys_call5(vlibc_sys_nr_getsockopt, u64(fd), u64(level), u64(optname), u64(optval), u64(optlen))
		if i64(rc) == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(-i64(rc)) }
	}
}
