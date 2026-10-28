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

	// ---- syscalls (defined in syscalls.v) ----
	// bsd_split converts (rc, er) to (i64, VlibcErrno).
	fn bsd_split(rc_in u64, er_in u64) (i64, VlibcErrno) {
		if er_in != 0 {
			return i64(-1), unsafe { VlibcErrno(er_in) }
		}
		return i64(rc_in), VlibcErrno.enoerror
	}

	// ---- sys wrappers (Solaris numbers) ----
	pub fn vlibc_sys_openat(dirfd i64, path &u8, flags u64, mode u64) (i64, VlibcErrno) {
		rc, er := sys_call4(sol_nr_openat, u64(dirfd), u64(path), flags, mode)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_close(fd i64) VlibcErrno {
		_, er := sys_call1(sol_nr_close, u64(fd))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_read(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_read, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_write(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_write, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_lseek(fd i64, off i64, whence int) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_lseek, u64(fd), u64(off), u64(whence))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_fstatat(dirfd i64, path &u8, buf &SolStatRaw, flags u32) VlibcErrno {
		_, er := sys_call4(sol_nr_fstatat, u64(dirfd), u64(path), u64(buf), u64(flags))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getcwd(buf &u8, size u64) (i64, VlibcErrno) {
		rc, er := sys_call2(sol_nr_getcwd, u64(buf), size)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_chdir(path &u8) VlibcErrno {
		_, er := sys_call1(sol_nr_chdir, u64(path))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_unlinkat(dirfd i64, path &u8, flags int) VlibcErrno {
		_, er := sys_call3(sol_nr_unlinkat, u64(dirfd), u64(path), u64(flags))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_mkdirat(dirfd i64, path &u8, mode u32) VlibcErrno {
		_, er := sys_call3(sol_nr_mkdirat, u64(dirfd), u64(path), u64(mode))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_renameat(olddir i64, oldpath &u8, newdir i64, newpath &u8) VlibcErrno {
		_, er := sys_call4(sol_nr_renameat, u64(olddir), u64(oldpath), u64(newdir), u64(newpath))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getdents64(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_getdents64, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_clock_gettime(clk int, tp &VlibcTimespec) VlibcErrno {
		_, er := sys_call2(sol_nr_clock_gettime, u64(clk), u64(tp))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_nanosleep(req &VlibcTimespec, rem &VlibcTimespec) VlibcErrno {
		_, er := sys_call2(sol_nr_nanosleep, u64(req), u64(rem))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_sysctl(name &int, namelen u32, oldp voidptr, oldlenp &u64, newp voidptr, newlen u64) VlibcErrno {
		_ = name
		_ = namelen
		_ = oldp
		_ = oldlenp
		_ = newp
		_ = newlen
		return VlibcErrno.enosys
	}

	pub fn vlibc_sys_getpid() i64 {
		rc, _ := sys_call0(sol_nr_getpid)
		return i64(rc)
	}

	pub fn vlibc_sys_getppid() i64 {
		rc, _ := sys_call0(sol_nr_getppid)
		return i64(rc)
	}

	pub fn vlibc_sys_getuid() i64 {
		rc, _ := sys_call0(sol_nr_getuid)
		return i64(rc)
	}

	pub fn vlibc_sys_geteuid() i64 {
		rc, _ := sys_call0(sol_nr_geteuid)
		return i64(rc)
	}

	pub fn vlibc_sys_getgid() i64 {
		rc, _ := sys_call0(sol_nr_getgid)
		return i64(rc)
	}

	pub fn vlibc_sys_getegid() i64 {
		rc, _ := sys_call0(sol_nr_getegid)
		return i64(rc)
	}

	pub fn vlibc_sys_kill(pid i64, sig int) VlibcErrno {
		_, er := sys_call2(sol_nr_kill, u64(pid), u64(sig))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_fork() i64 {
		// forksys(0, flags) == fork; flags=0.
		rc, er := sys_call2(sol_nr_forksys, 0, 0)
		if er != 0 {
			return -i64(er)
		}
		return i64(rc)
	}

	pub fn vlibc_sys_execve(filename &u8, argv []&u8, envp []&u8) int {
		_, er := sys_call3(sol_nr_execve, u64(filename), u64(argv.data), u64(envp.data))
		return -int(er)
	}

	pub fn vlibc_sys_waitid(idtype int, id i64, infop &int, options int, rusage voidptr) (i64, VlibcErrno) {
		rc, er := sys_call5(sol_nr_waitid, u64(idtype), u64(id), u64(infop), u64(options), u64(rusage))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_fcntl(fd i64, cmd int, arg i64) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_fcntl, u64(fd), u64(cmd), u64(arg))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_ioctl(fd i64, req u64, arg voidptr) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_ioctl, u64(fd), req, u64(arg))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_pipe(filedes &int) VlibcErrno {
		_, er := sys_call1(sol_nr_pipe, u64(filedes))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		rc, er := sys_call2(sol_nr_dup2, u64(oldfd), u64(newfd))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_mprotect(addr voidptr, len u64, prot int) VlibcErrno {
		_, er := sys_call3(sol_nr_mprotect, u64(addr), len, u64(prot))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_mmap(addr voidptr, len u64, prot int, flags int, fd i64, off u64) (voidptr, VlibcErrno) {
		rc, er := sys_call6(sol_nr_mmap, u64(addr), len, u64(prot), u64(flags), u64(fd), off)
		if er != 0 {
			return unsafe { nil }, unsafe { VlibcErrno(er) }
		}
		return unsafe { voidptr(rc) }, VlibcErrno.enoerror
	}

	pub fn vlibc_sys_munmap(addr voidptr, len u64) VlibcErrno {
		_, er := sys_call2(sol_nr_munmap, u64(addr), len)
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_madvise(addr voidptr, len u64, advice int) VlibcErrno {
		_, er := sys_call3(sol_nr_mincore, u64(addr), len, u64(advice))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_socket, u64(domain), u64(typ), u64(protocol))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_socketpair(domain int, typ int, protocol int, sv &int) VlibcErrno {
		_, er := sys_call4(sol_nr_socketpair, u64(domain), u64(typ), u64(protocol), u64(sv))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_bind(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		_, er := sys_call3(sol_nr_bind, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_listen(fd i64, backlog int) VlibcErrno {
		_, er := sys_call2(sol_nr_listen, u64(fd), u64(backlog))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_accept(fd i64, addr voidptr, addrlen &u32) (i64, VlibcErrno) {
		rc, er := sys_call3(sol_nr_accept, u64(fd), u64(addr), u64(addrlen))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_connect(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		_, er := sys_call3(sol_nr_connect, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_sendto(fd i64, buf voidptr, len u64, flags int, dest voidptr, addrlen u32) (i64, VlibcErrno) {
		rc, er := sys_call6(sol_nr_sendto, u64(fd), u64(buf), len, u64(flags), u64(dest), u64(addrlen))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_recvfrom(fd i64, buf voidptr, len u64, flags int, src voidptr, addrlen &u32) (i64, VlibcErrno) {
		rc, er := sys_call6(sol_nr_recvfrom, u64(fd), u64(buf), len, u64(flags), u64(src), u64(addrlen))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_shutdown(fd i64, how int) VlibcErrno {
		_, er := sys_call2(sol_nr_shutdown, u64(fd), u64(how))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getsockname(fd i64, addr voidptr, addrlen &u32) VlibcErrno {
		_, er := sys_call3(sol_nr_getsockname, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getpeername(fd i64, addr voidptr, addrlen &u32) VlibcErrno {
		_, er := sys_call3(sol_nr_getpeername, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_setsockopt(fd i64, level int, optname int, optval voidptr, optlen u32) VlibcErrno {
		_, er := sys_call5(sol_nr_setsockopt, u64(fd), u64(level), u64(optname), u64(optval), u64(optlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getsockopt(fd i64, level int, optname int, optval voidptr, optlen &u32) VlibcErrno {
		_, er := sys_call5(sol_nr_getsockopt, u64(fd), u64(level), u64(optname), u64(optval), u64(optlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// ---- time ----
	pub fn vlibc_now_realtime_ns() i64 {
		mut ts := VlibcTimespec{}
		vlibc_sys_clock_gettime(0, &ts)
		return ts.sec * 1_000_000_000 + ts.nsec
	}

	pub fn vlibc_now_monotonic_ns() i64 {
		mut ts := VlibcTimespec{}
		vlibc_sys_clock_gettime(4, &ts)
		return ts.sec * 1_000_000_000 + ts.nsec
	}

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

	pub fn vlibc_uname() VlibcUtsname {
		mut u := VlibcUtsname{}
		u.sysname = 'SunOS'
		$if amd64 {
			u.machine = 'i86pc'
		}
		return u
	}

	// ---- file ----
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

	pub fn vlibc_open(path string, flags u64, mode u64) (i64, VlibcErrno) {
		return vlibc_sys_openat(sol_at_fdcwd, path.str, flags, mode)
	}

	pub fn vlibc_close(fd i64) VlibcErrno {
		return vlibc_sys_close(fd)
	}

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
			if e != .enoerror {
				if total == 0 {
					return []u8{}, e
				}
				break
			}
			if n == 0 {
				break
			}
			total += int(n)
		}
		buf.trim(total)
		return buf, VlibcErrno.enoerror
	}

	pub fn vlibc_read_file(path string) ([]u8, VlibcErrno) {
		fd, e := vlibc_open(path, 0, 0)
		if e != .enoerror {
			return []u8{}, e
		}
		data, e2 := vlibc_read_all(fd)
		vlibc_close(fd)
		return data, e2
	}

	pub fn vlibc_write_file(path string, data []u8) VlibcErrno {
		fd, e := vlibc_open(path, 0x201, 0o644)
		if e != .enoerror {
			return e
		}
		mut off := u64(0)
		for off < u64(data.len) {
			n, e2 := vlibc_sys_write(fd, &u8(data.data) + off, u64(data.len) - off)
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
		vlibc_close(fd)
		return VlibcErrno.enoerror
	}

	pub fn vlibc_mkdir(path string, mode u32) VlibcErrno {
		return vlibc_sys_mkdirat(sol_at_fdcwd, path.str, mode)
	}

	pub fn vlibc_rmdir(path string) VlibcErrno {
		return vlibc_sys_unlinkat(sol_at_fdcwd, path.str, sol_at_removedir)
	}

	pub fn vlibc_unlink(path string) VlibcErrno {
		return vlibc_sys_unlinkat(sol_at_fdcwd, path.str, 0)
	}

	pub fn vlibc_rename(oldpath string, newpath string) VlibcErrno {
		return vlibc_sys_renameat(sol_at_fdcwd, oldpath.str, sol_at_fdcwd, newpath.str)
	}

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

	pub fn vlibc_chdir(path string) VlibcErrno {
		_, er := sys_call1(sol_nr_chdir, u64(path.str))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

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
	pub fn vlibc_fork() i64 {
		rc, er := sys_call2(sol_nr_forksys, 0, 0)
		if er != 0 {
			return -i64(er)
		}
		return i64(rc)
	}

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

	pub fn vlibc_waitpid(pid i64) (i64, VlibcErrno) {
		mut status := 0
		return vlibc_sys_waitid(1, pid, &status, 0, unsafe { nil })
	}

	pub fn vlibc_pipe() ([2]int, VlibcErrno) {
		mut fds := [0, 0]
		e := vlibc_sys_pipe(&fds[0])
		return fds, e
	}

	pub fn vlibc_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		return vlibc_sys_dup2(oldfd, newfd)
	}

	pub fn vlibc_signal(sig int, handler voidptr) VlibcErrno {
		_ = sig
		_ = handler
		return VlibcErrno.enosys
	}

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

	pub fn vlibc_htons(v u16) u16 {
		return (v >> 8) | (v << 8)
	}

	pub fn vlibc_htonl(v u32) u32 {
		return ((v & 0x000000ff) << 24) | ((v & 0x0000ff00) << 8) | ((v & 0x00ff0000) >> 8) | ((v & 0xff000000) >> 24)
	}

	pub fn vlibc_ntohs(v u16) u16 {
		return vlibc_htons(v)
	}

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
		return (parts[0] << 24) | (parts[1] << 16) | (parts[2] << 8) | parts[3]
	}

	pub fn vlibc_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		return vlibc_sys_socket(domain, typ, protocol)
	}

	pub fn vlibc_socketpair(domain int, typ int, protocol int) ([2]int, VlibcErrno) {
		mut sv := [0, 0]
		e := vlibc_sys_socketpair(domain, typ, protocol, &sv[0])
		return sv, e
	}

	pub fn vlibc_bind(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.family = 2
		sa.port = vlibc_htons(port)
		sa.addr = addr
		return vlibc_sys_bind(fd, unsafe { voidptr(&sa) }, 16)
	}

	pub fn vlibc_listen(fd i64, backlog int) VlibcErrno {
		return vlibc_sys_listen(fd, backlog)
	}

	pub fn vlibc_accept(fd i64) (i64, VlibcErrno) {
		return vlibc_sys_accept(fd, unsafe { nil }, unsafe { nil })
	}

	pub fn vlibc_connect(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.family = 2
		sa.port = vlibc_htons(port)
		sa.addr = addr
		return vlibc_sys_connect(fd, unsafe { voidptr(&sa) }, 16)
	}

	pub fn vlibc_send(fd i64, data []u8, flags int) (i64, VlibcErrno) {
		if data.len == 0 {
			return 0, VlibcErrno.enoerror
		}
		return vlibc_sys_sendto(fd, unsafe { voidptr(data.data) }, u64(data.len), flags, unsafe { nil }, 0)
	}

	pub fn vlibc_recv(fd i64, buf []u8, flags int) (i64, VlibcErrno) {
		if buf.len == 0 {
			return 0, VlibcErrno.enoerror
		}
		return vlibc_sys_recvfrom(fd, unsafe { voidptr(buf.data) }, u64(buf.len), flags, unsafe { nil }, unsafe { nil })
	}

	pub fn vlibc_shutdown(fd i64, how int) VlibcErrno {
		return vlibc_sys_shutdown(fd, how)
	}

	// vlibc_setsockopt_int sets an int-valued socket option via the
	// setsockopt syscall (used for SO_REUSEADDR, timeouts, buffer sizes...).
	pub fn vlibc_setsockopt_int(fd i64, level int, optname int, val int) VlibcErrno {
		v := i32(val) // C socket options are 4-byte `int`
		return vlibc_sys_setsockopt(fd, level, optname, voidptr(&v), u32(sizeof(v)))
	}

	// vlibc_getsockopt_int reads an int-valued socket option via the
	// getsockopt syscall (used for SO_ERROR reads).
	pub fn vlibc_getsockopt_int(fd i64, level int, optname int) (int, VlibcErrno) {
		mut v := i32(0)
		mut olen := u32(sizeof(v))
		e := vlibc_sys_getsockopt(fd, level, optname, voidptr(&v), &olen)
		return int(v), e
	}

	// ---- atomic helpers ----
	pub fn vlibc_cas_u32(ptr &u32, old u32, new u32) bool {
		mut swapped := u8(0)
		asm volatile amd64 {
			mov eax, old
			lock cmpxchg [ptr], new
			sete swapped
			; =r (swapped)
			; a (old)
			  r (ptr)
			  r (new)
			; memory
		}
		return swapped != 0
	}

	pub fn vlibc_load_u32(ptr &u32) u32 {
		return unsafe { *ptr }
	}

	pub fn vlibc_store_u32(ptr &u32, val u32) {
		unsafe { *ptr = val }
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

	pub fn vlibc_thread_spawn(fn_ptr VlibcThreadFn, arg u64) (VlibcThread, VlibcErrno) {
		_ = fn_ptr
		_ = arg
		return VlibcThread{}, VlibcErrno.enosys
	}

	pub fn vlibc_thread_join(mut t VlibcThread) VlibcErrno {
		_ = t
		return VlibcErrno.enosys
	}

	// ---- mutex/cond: spin + sched_yield (Solaris has no simple futex) ----
	pub struct VlibcMutex {
	pub:
		state u32
	}

	pub fn vlibc_mutex_init() VlibcMutex {
		return VlibcMutex{ state: 0 }
	}

	pub fn vlibc_mutex_lock(mut m VlibcMutex) {
		for {
			if vlibc_cas_u32(&m.state, 0, 1) {
				return
			}
			vlibc_sys_sched_yield()
		}
	}

	pub fn vlibc_mutex_unlock(mut m VlibcMutex) {
		vlibc_store_u32(&m.state, 0)
	}

	pub struct VlibcCond {
	pub:
		seq u32
	}

	pub fn vlibc_cond_init() VlibcCond {
		return VlibcCond{ seq: 0 }
	}

	pub fn vlibc_cond_wait(mut c VlibcCond, mut m VlibcMutex) {
		v := vlibc_load_u32(&c.seq)
		vlibc_mutex_unlock(mut m)
		for vlibc_load_u32(&c.seq) == v {
			vlibc_sys_sched_yield()
		}
		vlibc_mutex_lock(mut m)
	}

	pub fn vlibc_cond_signal(mut c VlibcCond) {
		vlibc_store_u32(&c.seq, vlibc_load_u32(&c.seq) + 1)
	}

	pub fn vlibc_cond_broadcast(mut c VlibcCond) {
		vlibc_store_u32(&c.seq, vlibc_load_u32(&c.seq) + 1)
	}
}
