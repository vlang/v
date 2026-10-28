module builtin

// macOS vlibc: experimental syscall layer, gated behind -d vlibc.
// Same V API as linux_bare/vlibc_* where the kernel provides the primitive.
//
// macOS differences from the BSDs (all verified against Go's generated
// zsysnum/ztypes files, themselves generated from the macOS SDK headers):
// - No openat, clock_gettime, nanosleep, getcwd or uname syscalls: open(5),
//   stat64(338)/fstat64(339), gettimeofday(116), select(93), sysctl(202) and
//   fcntl F_GETPATH(50) are used instead.
// - Threads and their sync use libSystem pthread (like Go's runtime does):
//   a macOS process always starts via dyld, which links libSystem anyway,
//   so there is no such thing as a libSystem-free macOS binary in practice.
//   Everything else stays raw-syscall pure.

$if !i386 {
	struct VlibcTimespec {
		sec  i64
		nsec i64
	}

	// macOS struct timeval: sec i64, usec i32 + pad (Go syscall.Timeval).
	struct MacTimeval {
		sec  i64
		usec i32
		pad  i32
	}

	// MacStat64 mirrors the 64-bit struct stat (Go syscall.Stat_t, 152 bytes).
	struct MacStat64 {
		dev     i32
		mode    u16
		nlink   u16
		ino     u64
		uid     u32
		gid     u32
		rdev    i32
		pad     i32
		atime   VlibcTimespec
		mtime   VlibcTimespec
		ctime   VlibcTimespec
		btime   VlibcTimespec
		size    i64
		blocks  i64
		blksize i32
		flags   u32
		gen     u32
		lspare  i32
		qspare0 i64
		qspare1 i64
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

	fn C.pthread_create(thread &voidptr, attr voidptr, start voidptr, arg voidptr) int
	fn C.pthread_join(thread voidptr, retval &voidptr) int
	fn C.pthread_mutex_init(mutex voidptr, attr voidptr) int
	fn C.pthread_mutex_lock(mutex voidptr) int
	fn C.pthread_mutex_unlock(mutex voidptr) int
	fn C.pthread_cond_init(cond voidptr, attr voidptr) int
	fn C.pthread_cond_wait(cond voidptr, mutex voidptr) int
	fn C.pthread_cond_signal(cond voidptr) int
	fn C.pthread_cond_broadcast(cond voidptr) int

	pub fn vlibc_sys_open(path &u8, flags u64, mode u64) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_open, u64(path), flags, mode)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_close(fd i64) VlibcErrno {
		_, er := sys_call1(mac_nr_close, u64(fd))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_read(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_read, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_write(fd i64, buf &u8, count u64) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_write, u64(fd), u64(buf), count)
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_lseek(fd i64, off i64, whence int) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_lseek, u64(fd), u64(off), u64(whence))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_stat64(path &u8, buf &MacStat64) VlibcErrno {
		_, er := sys_call2(mac_nr_stat64, u64(path), u64(buf))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_fstat64(fd i64, buf &MacStat64) VlibcErrno {
		_, er := sys_call2(mac_nr_fstat64, u64(fd), u64(buf))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getdirentries64(fd i64, buf &u8, count u64, basep &u64) (i64, VlibcErrno) {
		rc, er := sys_call4(mac_nr_getdirentries64, u64(fd), u64(buf), count, u64(basep))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_gettimeofday(tv &MacTimeval, tz voidptr) VlibcErrno {
		_, er := sys_call2(mac_nr_gettimeofday, u64(tv), u64(tz))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_select(nfds int, rfds voidptr, wfds voidptr, efds voidptr, tv &MacTimeval) (i64, VlibcErrno) {
		rc, er := sys_call5(mac_nr_select, u64(nfds), u64(rfds), u64(wfds), u64(efds), u64(tv))
		return bsd_split(rc, er)
	}

	// vlibc_sys_sysctl invokes sysctl with 32-bit MIB entries.
	pub fn vlibc_sys_sysctl(name &i32, namelen u32, oldp voidptr, oldlenp &u64, newp voidptr, newlen u64) VlibcErrno {
		_, er := sys_call6(mac_nr_sysctl, u64(name), u64(namelen), u64(oldp), u64(oldlenp), u64(newp), newlen)
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_getpid() i64 {
		rc, _ := sys_call0(mac_nr_getpid)
		return i64(rc)
	}

	pub fn vlibc_sys_getppid() i64 {
		rc, _ := sys_call0(mac_nr_getppid)
		return i64(rc)
	}

	pub fn vlibc_sys_getuid() i64 {
		rc, _ := sys_call0(mac_nr_getuid)
		return i64(rc)
	}

	pub fn vlibc_sys_geteuid() i64 {
		rc, _ := sys_call0(mac_nr_geteuid)
		return i64(rc)
	}

	pub fn vlibc_sys_kill(pid i64, sig int) VlibcErrno {
		_, er := sys_call2(mac_nr_kill, u64(pid), u64(sig))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_exit(code int) {
		sys_call1(mac_nr_exit, u64(code))
		for {}
	}

	// vlibc_sys_fork creates a child and returns zero in the child.
	pub fn vlibc_sys_fork() i64 {
		rc, child, er := mac_sys_call0_pair(mac_nr_fork)
		if er != 0 {
			return -i64(er)
		}
		if child != 0 { return 0 }
		return i64(rc)
	}

	pub fn vlibc_sys_execve(filename &u8, argv []&u8, envp []&u8) int {
		_, er := sys_call3(mac_nr_execve, u64(filename), u64(argv.data), u64(envp.data))
		return -int(er)
	}

	// vlibc_sys_wait4 waits for a child and optionally writes its 32-bit POSIX status.
	pub fn vlibc_sys_wait4(pid i64, status &i32, options int, rusage voidptr) (i64, VlibcErrno) {
		rc, er := sys_call4(mac_nr_wait4, u64(pid), u64(status), u64(options), u64(rusage))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_fcntl(fd i64, cmd int, arg voidptr) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_fcntl, u64(fd), u64(cmd), u64(arg))
		return bsd_split(rc, er)
	}

	// vlibc_sys_ioctl invokes the platform ioctl interface.
	pub fn vlibc_sys_ioctl(fd i64, req u64, arg voidptr) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_ioctl, u64(fd), req, u64(arg))
		return bsd_split(rc, er)
	}

	// vlibc_sys_pipe stores the two descriptors returned in the syscall result registers.
	pub fn vlibc_sys_pipe(filedes &i32) VlibcErrno {
		first, second, err := mac_sys_call0_pair(mac_nr_pipe)
		if err != 0 { return unsafe { VlibcErrno(err) } }
		unsafe {
			filedes[0] = i32(first)
			filedes[1] = i32(second)
		}
		return .enoerror
	}

	pub fn vlibc_sys_dup2(oldfd int, newfd int) (i64, VlibcErrno) {
		rc, er := sys_call2(mac_nr_dup2, u64(oldfd), u64(newfd))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_mprotect(addr voidptr, len u64, prot int) VlibcErrno {
		_, er := sys_call3(mac_nr_mprotect, u64(addr), len, u64(prot))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_mmap(addr voidptr, len u64, prot int, flags int, fd i64, off u64) (voidptr, VlibcErrno) {
		rc, er := sys_call6(mac_nr_mmap, u64(addr), len, u64(prot), u64(flags), u64(fd), off)
		if er != 0 {
			return unsafe { nil }, unsafe { VlibcErrno(er) }
		}
		return unsafe { voidptr(rc) }, VlibcErrno.enoerror
	}

	pub fn vlibc_sys_munmap(addr voidptr, len u64) VlibcErrno {
		_, er := sys_call2(mac_nr_munmap, u64(addr), len)
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_madvise(addr voidptr, len u64, advice int) VlibcErrno {
		_, er := sys_call3(mac_nr_madvise, u64(addr), len, u64(advice))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_socket, u64(domain), u64(typ), u64(protocol))
		return bsd_split(rc, er)
	}

	// vlibc_sys_socketpair writes two 32-bit socket descriptors to the supplied buffer.
	pub fn vlibc_sys_socketpair(domain int, typ int, protocol int, sv &i32) VlibcErrno {
		_, er := sys_call4(mac_nr_socketpair, u64(domain), u64(typ), u64(protocol), u64(sv))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_bind(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		_, er := sys_call3(mac_nr_bind, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_listen(fd i64, backlog int) VlibcErrno {
		_, er := sys_call2(mac_nr_listen, u64(fd), u64(backlog))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_accept(fd i64, addr voidptr, addrlen &u32) (i64, VlibcErrno) {
		rc, er := sys_call3(mac_nr_accept, u64(fd), u64(addr), u64(addrlen))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_connect(fd i64, addr voidptr, addrlen u32) VlibcErrno {
		_, er := sys_call3(mac_nr_connect, u64(fd), u64(addr), u64(addrlen))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_sys_sendto(fd i64, buf voidptr, len u64, flags int, dest voidptr, addrlen u32) (i64, VlibcErrno) {
		rc, er := sys_call6(mac_nr_sendto, u64(fd), u64(buf), len, u64(flags), u64(dest), u64(addrlen))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_recvfrom(fd i64, buf voidptr, len u64, flags int, src voidptr, addrlen &u32) (i64, VlibcErrno) {
		rc, er := sys_call6(mac_nr_recvfrom, u64(fd), u64(buf), len, u64(flags), u64(src), u64(addrlen))
		return bsd_split(rc, er)
	}

	pub fn vlibc_sys_shutdown(fd i64, how int) VlibcErrno {
		_, er := sys_call2(mac_nr_shutdown, u64(fd), u64(how))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	// ---- time (gettimeofday + select; macOS has no clock_gettime/nanosleep syscalls) ----
	pub fn vlibc_now_realtime_ns() i64 {
		mut tv := MacTimeval{}
		vlibc_sys_gettimeofday(&tv, unsafe { nil })
		return tv.sec * 1_000_000_000 + i64(tv.usec) * 1000
	}

	pub fn vlibc_now_monotonic_ns() i64 {
		// No monotonic clock syscall on macOS; gettimeofday is used instead.
		// This jumps with wall-clock changes; documented limitation.
		return vlibc_now_realtime_ns()
	}

	pub fn vlibc_sleep_ns(nsec u64) {
		mut tv := MacTimeval{
			sec:  i64(nsec / 1_000_000_000)
			usec: i32((nsec % 1_000_000_000) / 1000)
		}
		vlibc_sys_select(0, unsafe { nil }, unsafe { nil }, unsafe { nil }, &tv)
	}

	fn vlibc_sysctl_string(mib0 int, mib1 int) string {
		mut mib := [2]i32{}
		mib[0] = i32(mib0)
		mib[1] = i32(mib1)
		mut buf := []u8{len: 256}
		mut buflen := u64(256)
		e := vlibc_sys_sysctl(&mib[0], 2, &u8(buf.data), &buflen, unsafe { nil }, 0)
		if e != .enoerror || buflen == 0 {
			return ''
		}
		if buflen > 256 {
			buflen = 256
		}
		buf[buflen - 1] = 0
		mut s := ''
		unsafe {
			s = tos_clone(&u8(buf.data))
		}
		return s
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
		$if macos {
			u.sysname = 'Darwin'
		}
		u.nodename = vlibc_sysctl_string(1, 10)
		u.release = vlibc_sysctl_string(1, 2)
		u.version = vlibc_sysctl_string(1, 4)
		$if amd64 {
			u.machine = 'x86_64'
		} $else $if arm64 {
			u.machine = 'arm64'
		}
		return u
	}

	// ---- file ----
	pub fn vlibc_stat(path string) (VlibcStat, VlibcErrno) {
		mut raw := MacStat64{}
		e := vlibc_sys_stat64(path.str, &raw)
		if e != .enoerror {
			return VlibcStat{}, e
		}
		mut s := VlibcStat{}
		s.mode = u32(raw.mode)
		s.uid = raw.uid
		s.gid = raw.gid
		s.size = raw.size
		s.mtime = raw.mtime.sec
		s.ino = raw.ino
		s.nlink = u32(raw.nlink)
		s.blksize = u32(raw.blksize)
		return s, VlibcErrno.enoerror
	}

	pub fn vlibc_open(path string, flags u64, mode u64) (i64, VlibcErrno) {
		return vlibc_sys_open(path.str, flags, mode)
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
		// O_WRONLY|O_CREAT|O_TRUNC = 0x601 on macOS.
		fd, e := vlibc_open(path, 0x601, 0o644)
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

	pub fn vlibc_mkdir(path string, mode u32) VlibcErrno {
		_, er := sys_call2(mac_nr_mkdir, u64(path.str), u64(mode))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_rmdir(path string) VlibcErrno {
		_, er := sys_call1(mac_nr_rmdir, u64(path.str))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_unlink(path string) VlibcErrno {
		_, er := sys_call1(mac_nr_unlink, u64(path.str))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_rename(oldpath string, newpath string) VlibcErrno {
		_, er := sys_call2(mac_nr_rename, u64(oldpath.str), u64(newpath.str))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_getcwd() (string, VlibcErrno) {
		// macOS has no getcwd syscall; fcntl F_GETPATH on "." is the way.
		fd, e := vlibc_open('.', 0, 0)
		if e != .enoerror {
			return '', e
		}
		mut buf := []u8{len: 1024}
		_, e2 := vlibc_sys_fcntl(fd, 50, &u8(buf.data))
		vlibc_close(fd)
		if e2 != .enoerror {
			return '', e2
		}
		mut s := ''
		unsafe {
			s = tos_clone(&u8(buf.data))
		}
		return s, VlibcErrno.enoerror
	}

	pub fn vlibc_chdir(path string) VlibcErrno {
		_, er := sys_call1(mac_nr_chdir, u64(path.str))
		if er == 0 {
			return VlibcErrno.enoerror
		}
		return unsafe { VlibcErrno(er) }
	}

	pub fn vlibc_list_dir(path string) ([]string, VlibcErrno) {
		fd, e := vlibc_open(path, 0x100000, 0)
		if e != .enoerror {
			return []string{}, e
		}
		mut buf := []u8{len: 8192}
		mut base := u64(0)
		mut names := []string{}
		for {
			n, e2 := vlibc_sys_getdirentries64(fd, &u8(buf.data), 8192, &base)
			if e2 != .enoerror {
				vlibc_close(fd)
				return names, e2
			}
			if n == 0 {
				break
			}
			mut off := u64(0)
			for off < u64(n) {
				// getdirentries64 dirent: ino u64@0, seekoff u64@8, reclen u16@16, namlen u16@18, type u8@20, name@21
				reclen := *(&u16(&u8(buf.data) + off + 16))
				if reclen == 0 {
					break
				}
				nameptr := &u8(&u8(buf.data) + off + 21)
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

	// vlibc_environ_keys returns environment keys from the process environment.
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
	// vlibc_fork creates a child process and returns zero in the child.
	pub fn vlibc_fork() i64 {
		return vlibc_sys_fork()
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
		_, er := sys_call3(mac_nr_execve, u64(path.str), u64(a.data), u64(e.data))
		return -int(er)
	}

	// vlibc_waitpid blocks until the child exits and returns the reaped process ID.
	pub fn vlibc_waitpid(pid i64) (i64, VlibcErrno) {
		mut status := i32(0)
		return vlibc_sys_wait4(pid, &status, 0, unsafe { nil })
	}

	// vlibc_pipe creates a pipe and returns its read and write descriptors.
	pub fn vlibc_pipe() ([2]int, VlibcErrno) {
		mut fds := [2]i32{}
		e := vlibc_sys_pipe(&fds[0])
		return [int(fds[0]), int(fds[1])]!, e
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
		len    u8
		family u8
		port   u16
		addr   u32
		zero   [8]u8
	}

	pub fn vlibc_htons(v u16) u16 {
		return (v >> 8) | (v << 8)
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

	pub fn vlibc_socket(domain int, typ int, protocol int) (i64, VlibcErrno) {
		return vlibc_sys_socket(domain, typ, protocol)
	}

	// vlibc_socketpair creates a pair of connected socket descriptors.
	pub fn vlibc_socketpair(domain int, typ int, protocol int) ([2]int, VlibcErrno) {
		mut sv := [2]i32{}
		e := vlibc_sys_socketpair(domain, typ, protocol, &sv[0])
		return [int(sv[0]), int(sv[1])]!, e
	}

	pub fn vlibc_bind(fd i64, port u16, addr u32) VlibcErrno {
		mut sa := VlibcSockAddrIn{}
		sa.len = 16
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
		sa.len = 16
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

	// ---- threads via libSystem pthread ----
	type VlibcThreadFn = fn (u64)

	struct MacThreadStart {
		fn_ptr VlibcThreadFn
		arg    u64
		done   u32
	}

	fn macos_thread_entry(raw voidptr) voidptr {
		s := &MacThreadStart(raw)
		s.fn_ptr(s.arg)
		vlibc_store_u32(&s.done, 1)
		return unsafe { nil }
	}

	pub struct VlibcThread {
	pub:
		fn_ptr VlibcThreadFn
		arg    u64
		start  &MacThreadStart
		thread voidptr
		tid    i64
		done   u32
	}

	// vlibc_thread_spawn starts a worker with its own argument and stack state.
	pub fn vlibc_thread_spawn(fn_ptr VlibcThreadFn, arg u64) (VlibcThread, VlibcErrno) {
		mut t := VlibcThread{}
		t.fn_ptr = fn_ptr
		t.arg = arg
		ctx := &MacThreadStart(malloc(sizeof(MacThreadStart)))
		ctx.fn_ptr = fn_ptr
		ctx.arg = arg
		ctx.done = 0
		t.start = ctx
		t.done = 0
		t.thread = unsafe { nil }
		rc := C.pthread_create(&t.thread, unsafe { nil }, voidptr(macos_thread_entry), voidptr(ctx))
		if rc != 0 {
			free(ctx)
			return t, unsafe { VlibcErrno(rc) }
		}
		// No pthread_threadid call here: the pthread handle identifies the thread.
		t.tid = i64(t.thread)
		return t, VlibcErrno.enoerror
	}

	// vlibc_thread_join waits for kernel thread exit before releasing its stack.
	pub fn vlibc_thread_join(mut t VlibcThread) VlibcErrno {
		rc := C.pthread_join(t.thread, unsafe { nil })
		if rc != 0 { return unsafe { VlibcErrno(rc) } }
		free(t.start)
		t.start = unsafe { nil }
		return VlibcErrno.enoerror
	}

	// ---- mutex/cond via libSystem pthread ----
	pub struct VlibcMutex {
	pub:
		opaque [64]u8
	}

	pub fn vlibc_mutex_init() VlibcMutex {
		mut m := VlibcMutex{}
		C.pthread_mutex_init(&m.opaque[0], unsafe { nil })
		return m
	}

	pub fn vlibc_mutex_lock(mut m VlibcMutex) {
		C.pthread_mutex_lock(&m.opaque[0])
	}

	pub fn vlibc_mutex_unlock(mut m VlibcMutex) {
		C.pthread_mutex_unlock(&m.opaque[0])
	}

	pub struct VlibcCond {
	pub:
		opaque [48]u8
	}

	pub fn vlibc_cond_init() VlibcCond {
		mut c := VlibcCond{}
		C.pthread_cond_init(&c.opaque[0], unsafe { nil })
		return c
	}

	pub fn vlibc_cond_wait(mut c VlibcCond, mut m VlibcMutex) {
		C.pthread_cond_wait(&c.opaque[0], &m.opaque[0])
	}

	pub fn vlibc_cond_signal(mut c VlibcCond) {
		C.pthread_cond_signal(&c.opaque[0])
	}

	pub fn vlibc_cond_broadcast(mut c VlibcCond) {
		C.pthread_cond_broadcast(&c.opaque[0])
	}
}
