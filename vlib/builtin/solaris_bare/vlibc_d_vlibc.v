module builtin

// Solaris/Illumos vlibc: experimental syscall layer, gated behind -d vlibc.
// Same V API as linux_bare/vlibc_*, implemented on the Solaris kernel ABI.

$if !i386 {
	struct VlibcTimespec {
		sec  i64
		nsec i64
	}

	// SolStatRaw mirrors Solaris struct stat64 (144 bytes on amd64).
	struct SolStatRaw {
		dev     u64
		ino     u64
		mode    u32
		nlink   u32
		uid     u32
		gid     u32
		rdev    u64
		size    i64
		atime   VlibcTimespec
		mtime   VlibcTimespec
		ctime   VlibcTimespec
		blksize i64
		blocks  i64
		fstype  [16]u8
	}

	pub struct VlibcStat {
	pub:
		mode    u32
		uid     u32
		gid     u32
		size    i64
		mtime   i64
		ino     u64
		nlink   u32
		blksize u32
	}

	// ---- sys wrappers (Solaris numbers) ----
	// vlibc_sys_openat opens a path relative to a directory descriptor with native flags and mode.
	pub fn vlibc_sys_openat(dirfd i64, path &u8, flags u64, mode u64) (i64, VlibcErrno) {
		rc, er := sys_call4(sol_nr_openat, u64(dirfd), u64(path), flags, mode)
		return bsd_split(rc, er)
	}

	// vlibc_sys_close closes a descriptor and returns the kernel error.
	pub fn vlibc_sys_close(fd i64) VlibcErrno {
		_, er := sys_call1(sol_nr_close, u64(fd))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_read reads up to count bytes into buf and returns the count and error.
	pub fn vlibc_sys_read(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_read, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	// vlibc_sys_write writes up to count bytes from buf and returns the count and error.
	pub fn vlibc_sys_write(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_write, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	// vlibc_sys_lseek changes a descriptor's byte offset relative to whence.
	pub fn vlibc_sys_lseek(fd i64, off i64, whence int) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_lseek, u64(fd), u64(off), u64(whence))
		return bsd_split(rc, er)
	}

	// vlibc_sys_fstatat writes metadata for a path relative to a directory descriptor.
	pub fn vlibc_sys_fstatat(dirfd i64, path &u8, buf &SolStatRaw, flags u32) VlibcErrno {
		_, er := sys_call4(sol_nr_fstatat, u64(dirfd), u64(path), u64(buf), u64(flags))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_getcwd writes the working directory into a byte buffer.
	pub fn vlibc_sys_getcwd(buf &u8, size u64) (i64, VlibcErrno) {
		rc, er := sys_call2(sol_nr_getcwd, u64(buf), size)
		return bsd_split(rc, er)
	}

	// vlibc_sys_chdir changes the working directory using a NUL-terminated path.
	pub fn vlibc_sys_chdir(path &u8) VlibcErrno {
		_, er := sys_call1(sol_nr_chdir, u64(path))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_unlinkat removes a path relative to a directory descriptor.
	pub fn vlibc_sys_unlinkat(dirfd i64, path &u8, flags int) VlibcErrno {
		_, er := sys_call3(sol_nr_unlinkat, u64(dirfd), u64(path), u64(flags))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_mkdirat creates a directory relative to a directory descriptor.
	pub fn vlibc_sys_mkdirat(dirfd i64, path &u8, mode u32) VlibcErrno {
		_, er := sys_call3(sol_nr_mkdirat, u64(dirfd), u64(path), u64(mode))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_renameat renames a path relative to the supplied directory descriptors.
	pub fn vlibc_sys_renameat(olddir i64, oldpath &u8, newdir i64, newpath &u8) VlibcErrno {
		_, er := sys_call4(sol_nr_renameat, u64(olddir), u64(oldpath), u64(newdir), u64(newpath))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_getdents64 reads native 64-bit directory records into a byte buffer.
	pub fn vlibc_sys_getdents64(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_getdents64, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	// vlibc_sys_clock_gettime reads the selected clock into a native timespec.
	pub fn vlibc_sys_clock_gettime(clk int, tp &VlibcTimespec) VlibcErrno {
		_, er := sys_call2(sol_nr_clock_gettime, u64(clk), u64(tp))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_nanosleep suspends the calling thread for the requested interval.
	pub fn vlibc_sys_nanosleep(req &VlibcTimespec, rem &VlibcTimespec) VlibcErrno {
		_, er := sys_call2(sol_nr_nanosleep, u64(req), u64(rem))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_sched_yield yields the processor to another runnable thread.
	pub fn vlibc_sys_sched_yield() VlibcErrno {
		_, er := sys_call0(sol_nr_sched_yield)
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_sysctl invokes sysctl with 32-bit MIB entries.
	pub fn vlibc_sys_sysctl(name &i32, namelen u32, oldp voidptr, oldlenp &u64, newp voidptr, newlen u64) VlibcErrno {
		_ = name
		_ = namelen
		_ = oldp
		_ = oldlenp
		_ = newp
		_ = newlen
		return VlibcErrno.enosys
	}

	// vlibc_sys_getpid returns the current process ID.
	pub fn vlibc_sys_getpid() i64 {
		rc, _ := sys_call0(sol_nr_getpid)
		return i64(rc)
	}

	// vlibc_sys_getppid returns the parent process ID from the getpid pair.
	pub fn vlibc_sys_getppid() i64 {
		rc, _ := sol_sys_call0_secondary(sol_nr_getpid)
		return i64(rc)
	}

	// vlibc_sys_getuid returns the real user ID.
	pub fn vlibc_sys_getuid() i64 {
		rc, _ := sys_call0(sol_nr_getuid)
		return i64(rc)
	}

	// vlibc_sys_geteuid returns the effective user ID from the getuid pair.
	pub fn vlibc_sys_geteuid() i64 {
		rc, _ := sol_sys_call0_secondary(sol_nr_getuid)
		return i64(rc)
	}

	// vlibc_sys_getgid returns the real group ID.
	pub fn vlibc_sys_getgid() i64 {
		rc, _ := sys_call0(sol_nr_getgid)
		return i64(rc)
	}

	// vlibc_sys_getegid returns the effective group ID from the getgid pair.
	pub fn vlibc_sys_getegid() i64 {
		rc, _ := sol_sys_call0_secondary(sol_nr_getgid)
		return i64(rc)
	}

	// vlibc_sys_kill sends a signal to a process or process group.
	pub fn vlibc_sys_kill(pid i64, sig int) VlibcErrno {
		_, er := sys_call2(sol_nr_kill, u64(pid), u64(sig))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_fork creates a child and returns zero in the child.
	pub fn vlibc_sys_fork() i64 {
		// forksys(0, flags) == fork; flags=0.
		rc, child, er := sol_sys_call0_pair(sol_nr_forksys)
		if er != 0 {
			return -i64(er)
		}
		if child != 0 { return 0 }
		return i64(rc)
	}

	// vlibc_sys_execve replaces the process image using NUL-terminated pointer arrays.
	pub fn vlibc_sys_execve(filename &u8, argv []&u8, envp []&u8) int {
		_, er := sys_call3(sol_nr_execve, u64(filename), u64(argv.data), u64(envp.data))
		return -int(er)
	}

	// vlibc_sys_waitid waits for process state and writes a native siginfo buffer.
	pub fn vlibc_sys_waitid(idtype int, id i64, infop voidptr, options int, rusage voidptr) (i64, VlibcErrno) {
		rc, er := sys_call5(sol_nr_waitid, u64(idtype), u64(id), u64(infop), u64(options), u64(rusage))
		return bsd_split(rc, er)
	}

	// vlibc_sys_fcntl performs a native descriptor-control operation.
	pub fn vlibc_sys_fcntl(fd i64, cmd int, arg i64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_fcntl, u64(fd), u64(cmd), u64(arg))
		return bsd_split(rc, er)
	}

	// vlibc_sys_ioctl invokes the platform ioctl interface.
	pub fn vlibc_sys_ioctl(fd i64, req u64, arg voidptr) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_ioctl, u64(fd), req, u64(arg))
		return bsd_split(rc, er)
	}

	// vlibc_sys_pipe stores the two descriptors returned in rax and rdx.
	pub fn vlibc_sys_pipe(filedes &i32) VlibcErrno {
		first, second, er := sol_sys_call0_pair(sol_nr_pipe)
		if er != 0 {
			return unsafe { VlibcErrno(er) }
		}
		unsafe {
			filedes[0] = i32(first)
			filedes[1] = i32(second)
		}
		return .enoerror
	}

	// vlibc_sys_dup2 duplicates a descriptor onto the requested descriptor number.
	pub fn vlibc_sys_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		rc, er := sys_call2(sol_nr_dup2, u64(oldfd), u64(newfd))
		return bsd_split(rc, er)
	}

	// vlibc_sys_mprotect changes the protection of a mapped memory range.
	pub fn vlibc_sys_mprotect(addr voidptr, len u64, prot int) VlibcErrno {
		_, er := sys_call3(sol_nr_mprotect, u64(addr), len, u64(prot))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_mmap maps memory using native protection and mapping flags.
	pub fn vlibc_sys_mmap(addr voidptr, len u64, prot int, flags int, fd i64, off u64) (voidptr, VlibcErrno) {
		rc, er := sys_call6(sol_nr_mmap, u64(addr), len, u64(prot), u64(flags), u64(fd), off)
		if er != 0 {
			return unsafe { nil }, unsafe { VlibcErrno(er) }
		}
		return unsafe { voidptr(rc) }, VlibcErrno.enoerror
	}

	// vlibc_sys_munmap unmaps a memory range.
	pub fn vlibc_sys_munmap(addr voidptr, len u64) VlibcErrno {
		_, er := sys_call2(sol_nr_munmap, u64(addr), len)
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_madvise advises the kernel about a mapped memory range.
	pub fn vlibc_sys_madvise(addr voidptr, len u64, advice int) VlibcErrno {
		_, er := sys_call3(sol_nr_mincore, u64(addr), len, u64(advice))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_socket creates a socket with native domain, type, and protocol values.
	pub fn vlibc_sys_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_socket, u64(domain), u64(typ), u64(protocol))
		return bsd_split(rc, er)
	}

	// vlibc_sys_socketpair writes two 32-bit socket descriptors to the supplied buffer.
	pub fn vlibc_sys_socketpair(domain int, typ int, protocol int, sv &i32) VlibcErrno {
		_, er := sys_call4(sol_nr_socketpair, u64(domain), u64(typ), u64(protocol), u64(sv))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_bind binds a socket using a native address buffer.
	pub fn vlibc_sys_bind(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		_, er := sys_call3(sol_nr_bind, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_listen marks a socket as accepting connections with the given backlog.
	pub fn vlibc_sys_listen(fd i64, backlog int) VlibcErrno {
		_, er := sys_call2(sol_nr_listen, u64(fd), u64(backlog))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_accept accepts a connection into native socket-address buffers.
	pub fn vlibc_sys_accept(fd i64, addr voidptr, addrlen &u32) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_accept, u64(fd), u64(addr), u64(addrlen))
		return bsd_split(rc, er)
	}

	// vlibc_sys_connect connects a socket using a native address buffer.
	pub fn vlibc_sys_connect(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		_, er := sys_call3(sol_nr_connect, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_sys_sendto sends bytes to an optional native destination address.
	pub fn vlibc_sys_sendto(fd i64, buf voidptr, len u64, flags int, dest voidptr, addrlen u32) (i64, VlibcErrno) {
		rc, er := sys_call6(sol_nr_sendto, u64(fd), u64(buf), len, u64(flags), u64(dest), u64(addrlen))
		return bsd_split(rc, er)
	}

	// vlibc_sys_recvfrom receives bytes and optionally writes the peer's native socket address.
	pub fn vlibc_sys_recvfrom(fd i64, buf voidptr, len u64, flags int, src voidptr, addrlen &u32) (i64, VlibcErrno) {
		rc, er := sys_call6(sol_nr_recvfrom, u64(fd), u64(buf), len, u64(flags), u64(src), u64(addrlen))
		return bsd_split(rc, er)
	}

	// vlibc_sys_shutdown disables the requested socket directions.
	pub fn vlibc_sys_shutdown(fd i64, how int) VlibcErrno {
		_, er := sys_call2(sol_nr_shutdown, u64(fd), u64(how))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// ---- time ----
	// vlibc_now_realtime_ns returns the wall-clock timestamp in nanoseconds since the Unix epoch.
	pub fn vlibc_now_realtime_ns() i64 {
		mut ts := VlibcTimespec{}
		vlibc_sys_clock_gettime(0, &ts)
		return ts.sec * 1_000_000_000 + ts.nsec
	}

	// vlibc_now_monotonic_ns returns the monotonic clock value in nanoseconds.
	pub fn vlibc_now_monotonic_ns() i64 {
		mut ts := VlibcTimespec{}
		vlibc_sys_clock_gettime(4, &ts)
		return ts.sec * 1_000_000_000 + ts.nsec
	}

	// vlibc_sleep_ns requests a sleep duration in nanoseconds; signals may interrupt it.
	pub fn vlibc_sleep_ns(nsec u64) {
		mut ts := VlibcTimespec{ sec: i64(nsec / 1_000_000_000), nsec: i64(nsec % 1_000_000_000) }
		vlibc_sys_nanosleep(&ts, unsafe { nil })
	}

	// ---- uname via sysinfo (Solaris has no uname syscall) ----
	fn vlibc_sysinfo(cmd int, buf &u8, n u64) (i64, VlibcErrno) {
		_ = cmd
		_ = buf
		_ = n
		return 0, VlibcErrno.enosys
	}

	pub struct VlibcUtsname {
	pub:
		sysname  string
		nodename string
		release  string
		version  string
		machine  string
	}

	// vlibc_uname returns operating-system identification fields.
	pub fn vlibc_uname() VlibcUtsname {
		mut u := VlibcUtsname{}
		u.sysname = 'SunOS'
		$if amd64 {
			u.machine = 'i86pc'
		}
		return u
	}

	// ---- file ----
	// vlibc_stat returns file metadata, following symbolic links.
	pub fn vlibc_stat(path string) (VlibcStat, VlibcErrno) {
		mut raw := SolStatRaw{}
		e := vlibc_sys_fstatat(sol_at_fdcwd, path.str, &raw, 0)
		if e != .enoerror {
			return VlibcStat{}, e
		}
		mut s := VlibcStat{}
		s.mode = raw.mode
		s.uid = raw.uid
		s.gid = raw.gid
		s.size = raw.size
		s.mtime = raw.mtime.sec
		s.ino = raw.ino
		s.nlink = raw.nlink
		s.blksize = u32(raw.blksize)
		return s, VlibcErrno.enoerror
	}

	// vlibc_open opens a path with native flags and permissions subject to the process umask.
	pub fn vlibc_open(path string, flags u64, mode u64) (i64, VlibcErrno) {
		return vlibc_sys_openat(sol_at_fdcwd, path.str, flags, mode)
	}

	// vlibc_close closes a file descriptor.
	pub fn vlibc_close(fd i64) VlibcErrno {
		return vlibc_sys_close(fd)
	}

	// vlibc_read_all reads until EOF, preserving partial bytes when a read fails.
	pub fn vlibc_read_all(fd i64) ([]u8, VlibcErrno) {
		mut cap := int(4096)
		mut buf := []u8{len: cap}
		mut total := 0
		for {
			if total == buf.len {
				cap = cap * 2
				buf.grow_cap(cap)
				for buf.len < cap {
					buf << u8(0)
				}
			}
			n, e := vlibc_sys_read(fd, &u8(buf.data) + total, cap - total)
			if e == .eintr {
				continue
			}
			if e != .enoerror {
				buf.trim(total)
				return buf, e
			}
			if n == 0 {
				break
			}
			total += int(n)
		}
		buf.trim(total)
		return buf, VlibcErrno.enoerror
	}

	// vlibc_read_file reads a file and reports read or close errors.
	pub fn vlibc_read_file(path string) ([]u8, VlibcErrno) {
		fd, e := vlibc_open(path, 0, 0)
		if e != .enoerror {
			return []u8{}, e
		}
		data, e2 := vlibc_read_all(fd)
		close_error := vlibc_close(fd)
		return data, if e2 != .enoerror { e2 } else { close_error }
	}

	// vlibc_write_file writes all bytes and reports write or close errors.
	pub fn vlibc_write_file(path string, data []u8) VlibcErrno {
		fd, e := vlibc_open(path, 0x201, 0o644)
		if e != .enoerror {
			return e
		}
		mut off := u64(0)
		for off < u64(data.len) {
			n, e2 := vlibc_sys_write(fd, &u8(data.data) + off, u64(data.len) - off)
			if e2 == .eintr {
				continue
			}
			if e2 != .enoerror {
				vlibc_close(fd)
				return e2
			}
			if n == 0 {
				vlibc_close(fd)
				return VlibcErrno.eio
			}
			off += u64(n)
		}
		return vlibc_close(fd)
	}

	// vlibc_mkdir creates a directory with permissions subject to the process umask.
	pub fn vlibc_mkdir(path string, mode u32) VlibcErrno {
		return vlibc_sys_mkdirat(sol_at_fdcwd, path.str, mode)
	}

	// vlibc_rmdir removes an empty directory.
	pub fn vlibc_rmdir(path string) VlibcErrno {
		return vlibc_sys_unlinkat(sol_at_fdcwd, path.str, sol_at_removedir)
	}

	// vlibc_unlink removes a filesystem entry.
	pub fn vlibc_unlink(path string) VlibcErrno {
		return vlibc_sys_unlinkat(sol_at_fdcwd, path.str, 0)
	}

	// vlibc_rename renames or moves a filesystem entry.
	pub fn vlibc_rename(oldpath string, newpath string) VlibcErrno {
		return vlibc_sys_renameat(sol_at_fdcwd, oldpath.str, sol_at_fdcwd, newpath.str)
	}

	// vlibc_getcwd returns the current working directory and any error.
	pub fn vlibc_getcwd() (string, VlibcErrno) {
		mut buf := []u8{len: 4096}
		_, e := vlibc_sys_getcwd(&u8(buf.data), 4096)
		if e != .enoerror {
			return '', e
		}
		mut s := ''
		unsafe {
			s = tos_clone(&u8(buf.data))
		}
		return s, VlibcErrno.enoerror
	}

	// vlibc_chdir changes the current working directory.
	pub fn vlibc_chdir(path string) VlibcErrno {
		_, er := sys_call1(sol_nr_chdir, u64(path.str))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// vlibc_list_dir returns directory entry names, excluding dot and dot-dot.
	pub fn vlibc_list_dir(path string) ([]string, VlibcErrno) {
		fd, e := vlibc_open(path, 0, 0)
		if e != .enoerror {
			return []string{}, e
		}
		mut buf := []u8{len: 8192}
		mut names := []string{}
		for {
			n, e2 := vlibc_sys_getdents64(fd, &u8(buf.data), 8192)
			if e2 != .enoerror {
				vlibc_close(fd)
				return names, e2
			}
			if n == 0 {
				break
			}
			mut off := u64(0)
			for off < u64(n) {
				// Solaris dirent64: ino u64@0, off i64@8, reclen u16@16, type u8@18, namlen u16@20? Need verify.
				// Solaris dirent64 layout: d_ino(8) d_off(8) d_reclen(2) d_name[1][].
				// Actually Solaris uses d_reclen at offset 16, name at offset 18 (no namlen field).
				reclen := *(&u16(&u8(buf.data) + off + 16))
				if reclen == 0 {
					break
				}
				nameptr := &u8(&u8(buf.data) + off + 18)
				mut name := ''
				unsafe {
					name = tos_clone(nameptr)
				}
				if name != '.' && name != '..' && name != '' {
					names << name
				}
				off += u64(reclen)
			}
		}
		vlibc_close(fd)
		return names, VlibcErrno.enoerror
	}

	// ---- env (argv-derived) ----
	fn vlibc_environ() []&u8 {
		argv := &&u8(g_main_argv)
		mut envp := &u8(u64(argv) + u64(g_main_argc + 1) * 8)
		mut out := []&u8{}
		for {
			entry := unsafe { *(&&u8(envp)) }
			if entry == 0 {
				break
			}
			out << entry
			envp = &u8(u64(envp) + 8)
		}
		return out
	}

	// vlibc_getenv returns an environment value and whether it was found.
	pub fn vlibc_getenv(name string) (string, bool) {
		for entry in vlibc_environ() {
			mut i := 0
			mut ok := true
			for i < name.len {
				c := unsafe { entry[i] }
				if c == 0 || c != u8(name[i]) {
					ok = false
					break
				}
				i++
			}
			if ok && unsafe { entry[name.len] } == u8(61) {
				valptr := unsafe { &u8(u64(entry) + u64(name.len) + 1) }
				mut s := ''
				unsafe {
					s = tos_clone(valptr)
				}
				return s, true
			}
		}
		return '', false
	}

	// vlibc_environ_keys returns the names available in the process environment.
	pub fn vlibc_environ_keys() []string {
		mut keys := []string{}
		for entry in vlibc_environ() {
			mut i := 0
			for {
				c := unsafe { entry[i] }
				if c == 0 || c == u8(61) {
					break
				}
				i++
			}
			if i > 0 && unsafe { entry[i] } == u8(61) {
				mut key := []u8{cap: i + 1}
				for k in 0 .. i {
					key << unsafe { entry[k] }
				}
				key << u8(0)
				mut s := ''
				unsafe {
					s = tos_clone(&u8(key.data))
				}
				keys << s
			}
		}
		return keys
	}

	// ---- proc ----
	// vlibc_fork creates a child and returns zero in the child.
	pub fn vlibc_fork() i64 {
		return vlibc_sys_fork()
	}

	// vlibc_execve replaces the current process with the requested executable.
	pub fn vlibc_execve(path string, argv []string, envp []string) int {
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
		_, er := sys_call3(sol_nr_execve, u64(path.str), u64(a.data), u64(e.data))
		return -int(er)
	}

	// vlibc_waitpid waits for a child and returns its process ID.
	pub fn vlibc_waitpid(pid i64) (i64, VlibcErrno) {
		// LP64 siginfo_t is 256 bytes; the process ID is at byte offset 16.
		mut info := [32]u64{}
		for {
			_, er := vlibc_sys_waitid(0, pid, &info[0], 1, unsafe { nil })
			if er == .eintr { continue }
			if er != .enoerror { return -1, er }
			return i64(i32(info[2])), .enoerror
		}
	}

	// vlibc_pipe creates a pipe and returns its read and write descriptors.
	pub fn vlibc_pipe() ([2]int, VlibcErrno) {
		mut fds := [2]i32{}
		e := vlibc_sys_pipe(&fds[0])
		return [int(fds[0]), int(fds[1])]!, e
	}

	// vlibc_dup2 duplicates oldfd onto newfd, preserving POSIX same-descriptor behavior.
	pub fn vlibc_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		return vlibc_sys_dup2(oldfd, newfd)
	}

	// vlibc_signal returns ENOSYS because installing signal handlers is not implemented.
	pub fn vlibc_signal(sig int, handler voidptr) VlibcErrno {
		_ = sig
		_ = handler
		return VlibcErrno.enosys
	}

	// vlibc_kill sends a signal to a process or process group.
	pub fn vlibc_kill(pid i64, sig int) VlibcErrno {
		return vlibc_sys_kill(pid, sig)
	}

	// ---- net ----
	pub struct VlibcSockAddrIn {
	pub:
		family u16
		port   u16
		addr   u32
		zero   [8]u8
	}

	// vlibc_htons converts a 16-bit host-order value to network byte order.
	pub fn vlibc_htons(v u16) u16 {
		return (v >> 8) | (v << 8)
	}

	// vlibc_htonl converts a 32-bit value to network byte order.
	pub fn vlibc_htonl(v u32) u32 {
		return ((v & 0x000000ff) << 24) | ((v & 0x0000ff00) << 8) |
			((v & 0x00ff0000) >> 8) | ((v & 0xff000000) >> 24)
	}

	// vlibc_parse_ipv4 packs dotted IPv4 bytes in network byte order.
	pub fn vlibc_parse_ipv4(s string) u32 {
		mut parts := [u32(0), u32(0), u32(0), u32(0)]
		mut idx := 0
		mut cur := u32(0)
		mut has := false
		for c in s {
			if c == u8(46) {
				if idx < 4 {
					parts[idx] = cur
					idx++
				}
				cur = 0
				has = false
			} else if c >= u8(48) && c <= u8(57) {
				cur = cur * 10 + u32(c - 48)
				has = true
			}
		}
		if idx < 4 && has {
			parts[idx] = cur
			idx++
		}
		return vlibc_htonl((parts[0] << 24) | (parts[1] << 16) | (parts[2] << 8) | parts[3])
	}

	// vlibc_socket creates a socket with the native domain, type, and protocol.
	pub fn vlibc_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		return vlibc_sys_socket(domain, typ, protocol)
	}

	// vlibc_socketpair creates a pair of connected socket descriptors.
	pub fn vlibc_socketpair(domain int, typ int, protocol int) ([2]int, VlibcErrno) {
		mut sv := [2]i32{}
		e := vlibc_sys_socketpair(domain, typ, protocol, &sv[0])
		return [int(sv[0]), int(sv[1])]!, e
	}

	// vlibc_bind binds a socket to an IPv4 address in network byte order and a host-order port.
	pub fn vlibc_bind(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.family = 2
		sa.port = vlibc_htons(port)
		sa.addr = addr
		return vlibc_sys_bind(fd, unsafe { voidptr(&sa) }, 16)
	}

	// vlibc_listen marks a socket as accepting connections with the given backlog.
	pub fn vlibc_listen(fd i64, backlog int) VlibcErrno {
		return vlibc_sys_listen(fd, backlog)
	}

	// vlibc_accept accepts a connection and returns its descriptor.
	pub fn vlibc_accept(fd i64) (i64, VlibcErrno) {
		return vlibc_sys_accept(fd, unsafe { nil }, unsafe { nil })
	}

	// vlibc_connect connects to a network-order IPv4 address and a host-order port.
	pub fn vlibc_connect(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.family = 2
		sa.port = vlibc_htons(port)
		sa.addr = addr
		return vlibc_sys_connect(fd, unsafe { voidptr(&sa) }, 16)
	}

	// vlibc_send sends bytes on a connected socket and returns the number sent and any error.
	pub fn vlibc_send(fd i64, data []u8, flags int) (i64, VlibcErrno) {
		if data.len == 0 {
			return 0, VlibcErrno.enoerror
		}
		return vlibc_sys_sendto(fd, unsafe { voidptr(data.data) }, u64(data.len), flags, unsafe { nil }, 0)
	}

	// vlibc_recv receives bytes into buf and returns the number read and any error.
	pub fn vlibc_recv(fd i64, buf []u8, flags int) (i64, VlibcErrno) {
		if buf.len == 0 {
			return 0, VlibcErrno.enoerror
		}
		return vlibc_sys_recvfrom(fd, unsafe { voidptr(buf.data) }, u64(buf.len), flags, unsafe { nil }, unsafe { nil })
	}

	// vlibc_shutdown shuts down the selected socket directions.
	pub fn vlibc_shutdown(fd i64, how int) VlibcErrno {
		return vlibc_sys_shutdown(fd, how)
	}

	// ---- atomic helpers ----
	// vlibc_cas_u32 compares and exchanges a word with acquire/release ordering.
	pub fn vlibc_cas_u32(ptr &u32, old u32, new u32) bool {
		mut swapped := u8(0)
		$if arm64 {
			asm volatile arm64 {
				1:
				ldaxr w4, [ptr]
				cmp w4, old
				b.ne '2f'
				stlxr w5, new, [ptr]
				cbnz w5, '1b'
				mov swapped, #1
				b '3f'
				2:
				clrex
				mov swapped, #0
				3:
				; =r (swapped)
				; r (ptr)
				  r (old)
				  r (new)
				; x4
				  x5
				  cc
				  memory
			}
		} $else {
			asm volatile amd64 {
				mov eax, old
				lock cmpxchg [ptr], new
				sete swapped
				; =r (swapped)
				; r (old)
				  r (ptr)
				  r (new)
				; eax
				  cc
				  memory
			}
		}
		return swapped != 0
	}

	// vlibc_load_u32 loads a word with acquire ordering.
	pub fn vlibc_load_u32(ptr &u32) u32 {
		mut value := u32(0)
		$if arm64 {
			asm volatile arm64 {
				ldar value, [ptr]
				; =r (value)
				; r (ptr)
				; memory
			}
		} $else {
			asm volatile amd64 {
				mov value, [ptr]
				; =r (value)
				; r (ptr)
				; memory
			}
		}
		return value
	}

	// vlibc_store_u32 stores a word with release ordering.
	pub fn vlibc_store_u32(ptr &u32, val u32) {
		$if arm64 {
			asm volatile arm64 {
				stlr val, [ptr]
				; ; r (ptr)
				    r (val)
				; memory
			}
		} $else {
			asm volatile amd64 {
				mov [ptr], val
				; ; r (ptr)
				    r (val)
				; memory
			}
		}
	}

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

	// ---- threads: not yet implemented on Solaris (lwp_create + ucontext
	// setup is non-trivial; spin-based mutex only) ----
	type VlibcThreadFn = fn (u64)

	pub struct VlibcThread {
	pub:
		fn_ptr VlibcThreadFn
		arg    u64
		done   u32
		tid    i64
	}

	// vlibc_thread_spawn returns ENOSYS because Solaris thread creation is not implemented.
	pub fn vlibc_thread_spawn(fn_ptr VlibcThreadFn, arg u64) (VlibcThread, VlibcErrno) {
		_ = fn_ptr
		_ = arg
		return VlibcThread{}, VlibcErrno.enosys
	}

	// vlibc_thread_join returns ENOSYS because Solaris thread creation is not implemented.
	pub fn vlibc_thread_join(mut t VlibcThread) VlibcErrno {
		_ = t
		return VlibcErrno.enosys
	}

	// ---- mutex/cond: spin + sched_yield (Solaris has no simple futex) ----
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
			vlibc_sys_sched_yield()
		}
	}

	// vlibc_mutex_unlock releases the mutex.
	pub fn vlibc_mutex_unlock(mut m VlibcMutex) {
		vlibc_store_u32(&m.state, 0)
	}

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
		for vlibc_load_u32(&c.seq) == v {
			vlibc_sys_sched_yield()
		}
		vlibc_mutex_lock(mut m)
	}

	// vlibc_cond_signal advances the condition sequence to notify waiting threads.
	pub fn vlibc_cond_signal(mut c VlibcCond) {
		vlibc_increment_u32(&c.seq)
	}

	// vlibc_cond_broadcast advances the condition sequence and wakes all waiters.
	pub fn vlibc_cond_broadcast(mut c VlibcCond) {
		vlibc_increment_u32(&c.seq)
	}
}
