module builtin

// bf.v is the DragonFly BSD member of the uniform freestanding os-syscall
// surface (`bf_*`). DragonFly rides this directory's carry-flag trampolines;
// the file is amd64-only, matching syscalls.v's scope. Per this directory's
// table (df_nr_*) and DragonFly sys/fcntl.h: AT_FDCWD=-328243 and the *at
// flags are one-bit codes (SYMLINK_NOFOLLOW=1, REMOVEDIR=2), O_* follow the
// BSD bit family with O_DIRECTORY at bit 21 of the packed struct (0x1000000
// shifted per kernel; in-tree uses 0o400000000 in flags).

$if (freestanding || syscalls) && amd64 {
	const bf_nr_read = u64(3)
	const bf_nr_write = u64(4)
	const bf_nr_close = u64(6)
	const bf_nr_lseek = u64(199)
	const bf_nr_openat = u64(504)
	const bf_nr_getdirentries = u64(479)
	const bf_nr_fstatat = u64(505)
	const bf_nr_getcwd = u64(326)
	const bf_nr_chdir = u64(12)
	const bf_nr_nanosleep = u64(240)
	const bf_nr_mkdirat = u64(524)
	const bf_nr_unlinkat = u64(508)
	const bf_nr_renameat = u64(523)
	const bf_nr_readlinkat = u64(527)
	const bf_nr_ftruncate = u64(201)
	const bf_nr_fchmodat = u64(506)
	const bf_nr_ioctl = u64(54)
	const bf_nr_lwp_gettid = u64(496)

	const bf_at_fdcwd = i64(-328243)
	const bf_at_symlink_nofollow = u64(1)
	const bf_at_removedir = u64(2)

	const bf_o_directory = u64(0x8000000) // bit 27

	// bf_last_errno_cell stores the last error the bf wrappers normalized (Linux
	// numbers), the freestanding reading for os.last_error.
	__global bf_last_errno_cell = int(0)

	pub fn bf_last_errno() int {
		return bf_last_errno_cell
	}

	fn bf_norm_errno(e VlibcErrno) int {
		// values normalized to the Linux numbers for the os message tables; an
		// errno with no Linux counterpart reads as EIO, never as success
		n := match e {
			.enoerror { 0 }
			.eperm { 1 }
			.enoent { 2 }
			.esrch { 3 }
			.eintr { 4 }
			.eio { 5 }
			.enxio { 6 }
			.e2big { 7 }
			.enoexec { 8 }
			.ebadf { 9 }
			.echild { 10 }
			.edeadlk { 35 }
			.enomem { 12 }
			.eacces { 13 }
			.efault { 14 }
			.enotblk { 15 }
			.ebusy { 16 }
			.eexist { 17 }
			.exdev { 18 }
			.enodev { 19 }
			.enotdir { 20 }
			.eisdir { 21 }
			.einval { 22 }
			.enfile { 23 }
			.emfile { 24 }
			.enotty { 25 }
			.etxtbsy { 26 }
			.efbig { 27 }
			.enospc { 28 }
			.espipe { 29 }
			.erofs { 30 }
			.emlink { 31 }
			.epipe { 32 }
			.edom { 33 }
			.erange { 34 }
			.eagain { 11 }
			.einprogress { 115 }
			.ealready { 114 }
			.enotsock { 88 }
			.edestaddrreq { 89 }
			.emsgsize { 90 }
			.eprototype { 91 }
			.enoprotoopt { 92 }
			.eprotonosupport { 93 }
			.esocktnosupport { 94 }
			.eopnotsupp { 95 }
			.epfnosupport { 96 }
			.eafnosupport { 97 }
			.eaddrinuse { 98 }
			.eaddrnotavail { 99 }
			.enetdown { 100 }
			.enetunreach { 101 }
			.enetreset { 102 }
			.econnaborted { 103 }
			.econnreset { 104 }
			.enobufs { 105 }
			.eisconn { 106 }
			.enotconn { 107 }
			.eshutdown { 108 }
			.etoomanyrefs { 109 }
			.etimedout { 110 }
			.econnrefused { 111 }
			.eloop { 40 }
			.enametoolong { 36 }
			.ehostdown { 112 }
			.ehostunreach { 113 }
			.enotempty { 39 }
			.eusers { 87 }
			.edquot { 122 }
			.estale { 116 }
			.eremote { 66 }
			.enolck { 37 }
			.enosys { 38 }
			.eidrm { 43 }
			.enomsg { 42 }
			.eoverflow { 75 }
			.ecanceled { 125 }
			.eilseq { 84 }
			.ebadmsg { 74 }
			.emultihop { 72 }
			.enolink { 67 }
			.eproto { 71 }
			.enotrecoverable { 131 }
			.eownerdead { 130 }
			else { 5 }
		}
		bf_last_errno_cell = n
		return n
	}

	fn bf_err_of(rc u64, er u64) int {
		if er == 0 {
			return 0
		}
		return bf_norm_errno(unsafe { VlibcErrno(er) })
	}

	const bf_nr_getppid = u64(39)
	const bf_nr_sysctl = u64(202)

	pub fn bf_getppid() int {
		rc, er := sys_call0(bf_nr_getppid)
		if er != 0 {
			bf_err_of(rc, er)
			return -1
		}
		return int(rc)
	}

	const bf_nr_fork = u64(2)
	const bf_nr_execve = u64(59)
	const bf_nr_wait4 = u64(7)

	// bf_fork forks the process: the child's pid in the parent, 0 in the child,
	// -1 with the errno cell set on failure. The kernel marks the child with 1
	// in the second result register; the first one is not 0 there.
	pub fn bf_fork() int {
		first, second, er := bsd_sys_call0_pair(bf_nr_fork)
		if er != 0 {
			bf_err_of(first, er)
			return -1
		}
		return if second != 0 { 0 } else { int(first) }
	}

	// bf_execve replaces the process image; it only returns on failure (-1).
	pub fn bf_execve(path &char, argv voidptr, envp voidptr) int {
		rc, er := sys_call3(bf_nr_execve, u64(path), u64(argv), u64(envp))
		bf_err_of(rc, er)
		return -1
	}

	// bf_wait4 waits for child `pid`, storing its wait status and, when `rusage`
	// is not nil, its resource usage. `options` takes this kernel's own W* values,
	// passed through unchanged as the libc wait4 of a hosted build does.
	pub fn bf_wait4(pid int, status &int, options int, rusage voidptr) int {
		rc, er := sys_call4(bf_nr_wait4, u64(i64(pid)), u64(status), u64(i64(options)),
			u64(rusage))
		if er != 0 {
			bf_err_of(rc, er)
			return -1
		}
		return int(rc)
	}

	// bf_uname_field copies the sysctl string at [top, leaf] into one 65-byte
	// field of the Linux utsname layout. Like uname(3), it turns the newlines and
	// tabs of kern.version into spaces and trims the trailing ones.
	fn bf_uname_field(top i32, leaf i32, dst &u8) bool {
		mut mib := [2]i32{}
		mib[0] = top
		mib[1] = leaf
		mut tmp := [512]u8{}
		mut size := u64(tmp.len)
		rc, er := sys_call6(bf_nr_sysctl, u64(voidptr(&mib[0])), u64(2), u64(voidptr(&tmp[0])),
			u64(voidptr(&size)), u64(0), u64(0))
		if er != 0 {
			bf_err_of(rc, er)
			return false
		}
		mut n := 0
		for n < 64 && n < int(size) && tmp[n] != 0 {
			c := tmp[n]
			unsafe {
				dst[n] = if c == `\n` || c == `\t` { ` ` } else { c }
			}
			n++
		}
		for n > 0 && unsafe { dst[n - 1] } == ` ` {
			n--
		}
		unsafe {
			dst[n] = 0
		}
		return true
	}

	// bf_uname fills the Linux utsname layout (six 65-byte fields: sysname,
	// nodename, release, version, machine, domainname) from the kern.ostype,
	// kern.hostname, kern.osrelease, kern.version and hw.machine sysctls, the
	// same sources uname(3) reads. domainname stays empty.
	pub fn bf_uname(buf &u8) int {
		for i in 0 .. 6 * 65 {
			unsafe {
				buf[i] = 0
			}
		}
		// CTL_KERN = 1: KERN_OSTYPE = 1, KERN_HOSTNAME = 10, KERN_OSRELEASE = 2,
		// KERN_VERSION = 4; CTL_HW = 6: HW_MACHINE = 1
		if !bf_uname_field(1, 1, buf) || !bf_uname_field(1, 10, unsafe { buf + 65 })
			|| !bf_uname_field(1, 2, unsafe { buf + 130 })
			|| !bf_uname_field(1, 4, unsafe { buf + 195 })
			|| !bf_uname_field(6, 1, unsafe { buf + 260 }) {
			return -1
		}
		return 0
	}

	// bf_flags transposes Linux o_* numbers into DragonFly values (the shared
	// BSD bit family: CREAT bit9, TRUNC bit10, EXCL bit11, APPEND bit3,
	// NONBLOCK bit2, SYNC bit7; NOCTTY bit15).
	pub fn bf_flags(linux_flags u64) u64 {
		mut out := linux_flags & u64(0x3)
		if linux_flags & u64(0o100) != 0 {
			out |= u64(0x200)
		}
		if linux_flags & u64(0x80) != 0 {
			out |= u64(0x800)
		}
		if linux_flags & u64(0o400) != 0 {
			out |= u64(0x8000)
		}
		if linux_flags & u64(0o1000) != 0 {
			out |= u64(0x400)
		}
		if linux_flags & u64(0o2000) != 0 {
			out |= u64(0x8)
		}
		if linux_flags & u64(0o4000) != 0 {
			out |= u64(0x4)
		}
		if linux_flags & u64(0o4010000) != 0 {
			out |= u64(0x80)
		}
		return out
	}

	fn bf_split_int(rc u64, er u64) (int, int) {
		if er != 0 {
			return -1, bf_err_of(rc, er)
		}
		return int(rc), 0
	}

	pub fn bf_openat(path &u8, flags_raw u64, mode u64) (int, int) {
		flags := bf_flags(flags_raw)

		rc, er := sys_call4(bf_nr_openat, u64(bf_at_fdcwd) & 0xffffffff, u64(path), flags, mode)
		return bf_split_int(rc, er)
	}

	// O_DIRECTORY on DragonFly carries DIRECTORY at packed bit 21 (Zig's c.zig
	// view is 21 in the packed struct starting from ACCMODE). The usable constant
	// from in-tree is 0o2000000-shifted, verified against the vlibc layer.
	pub fn bf_openat_dir(path &u8) (int, int) {
		return bf_openat(path, u64(0) | bf_o_directory, u64(0))
	}

	pub fn bf_read(fd int, buf &u8, count int) (int, int) {
		for {
			nr, er := sys_call3(bf_nr_read, u64(fd), u64(buf), u64(count))
			if er == 0 {
				return int(nr), 0
			}
			ee := bf_err_of(nr, er)
			if ee == 4 { // EINTR
				continue
			}
			return -1, ee
		}
	}

	pub fn bf_write(fd int, buf &u8, count int) (int, int) {
		for {
			nw, er := sys_call3(bf_nr_write, u64(fd), u64(buf), u64(count))
			if er == 0 {
				return int(nw), 0
			}
			ee := bf_err_of(nw, er)
			if ee == 4 { // EINTR
				continue
			}
			return -1, ee
		}
	}

	pub fn bf_close(fd int) int {
		rc, er := sys_call1(bf_nr_close, u64(fd))
		return bf_err_of(rc, er)
	}

	pub fn bf_lseek(fd int, offset i64, whence i64) (i64, int) {
		// lseek(int fd, int PAD, off_t offset, int whence)
		rc, er := sys_call4(bf_nr_lseek, u64(fd), 0, u64(offset), u64(whence))
		if er != 0 {
			return i64(-1), bf_err_of(rc, er)
		}
		return i64(rc), 0
	}

	pub fn bf_getcwd(buf &u8, size int) (int, int) {
		rc, er := sys_call2(bf_nr_getcwd, u64(buf), u64(size))
		return bf_split_int(rc, er)
	}

	pub fn bf_chdir(path string) int {
		rc, er := sys_call1(bf_nr_chdir, u64(path.str))
		return bf_err_of(rc, er)
	}

	pub fn bf_mkdir(path string, mode u64) int {
		rc, er := sys_call3(bf_nr_mkdirat, u64(bf_at_fdcwd) & 0xffffffff, u64(path.str), mode)
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_unlink(path string) int {
		rc, er := sys_call3(bf_nr_unlinkat, u64(bf_at_fdcwd) & 0xffffffff, u64(path.str), u64(0))
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_rmdir(path string) int {
		rc, er := sys_call3(bf_nr_unlinkat, u64(bf_at_fdcwd) & 0xffffffff, u64(path.str),
			bf_at_removedir)
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_rename(src string, dst string) int {
		rc, er := sys_call4(bf_nr_renameat, u64(bf_at_fdcwd) & 0xffffffff, u64(src.str),
			u64(bf_at_fdcwd) & 0xffffffff, u64(dst.str))
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_readlink(path string, buf &u8, size int) (int, int) {
		rc, er := sys_call4(bf_nr_readlinkat, u64(bf_at_fdcwd) & 0xffffffff, u64(path.str),
			u64(buf), u64(size))
		return bf_split_int(rc, er)
	}

	pub fn bf_fchmodat(path string, mode u64) int {
		rc, er := sys_call3(bf_nr_fchmodat, u64(bf_at_fdcwd) & 0xffffffff, u64(path.str), mode)
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_truncate(path string, len u64) int {
		fd, e := bf_openat(path.str, u64(1), u64(0)) // O_WRONLY
		if e != 0 {
			return e
		}
		// ftruncate(int fd, int PAD, off_t length): the length sits after the padding slot.
		rc, er := sys_call3(bf_nr_ftruncate, u64(fd), u64(0), len)
		bf_close(fd)
		return bf_err_of(rc, er)
	}

	// FSysTimespec matches BSD timespec (i64 seconds and nanoseconds).
	struct FSysTimespec {
		tv_sec  i64
		tv_nsec i64
	}

	pub fn bf_nanosleep(ms i64) int {
		mut req := FSysTimespec{ms / 1000, 1_000_000 * (ms % 1000)}
		mut rem := FSysTimespec{}
		for {
			rc, er := sys_call2(bf_nr_nanosleep, u64(voidptr(&req)), u64(voidptr(&rem)))
			if er == 0 {
				return 0
			}
			ee := bf_err_of(rc, er)
			if ee == 4 { // EINTR
				req.tv_sec = rem.tv_sec
				req.tv_nsec = rem.tv_nsec
				continue
			}
			return ee
		}
	}

	pub fn bf_getpid() int {
		rc, er := sys_call0(u64(20))
		_ = er
		return int(rc)
	}

	pub fn bf_gettid() u64 {
		rc, er := sys_call0(bf_nr_lwp_gettid)
		_ = er
		return rc
	}

	// FSysStatRaw per DragonFly sys/stat.h (LP64, Zig c.zig layout): ino u64,
	// nlink u32, dev u32, mode u16, pad u16, uid/gid u32, rdev u32, three
	// timespecs, size u64, blocks i64, blksize/flags/gen u32, lspare i32.
	struct FSysStatRaw {
		ino     u64
		nlink   u32
		dev     u32
		mode    u16
		pad0    u16
		uid     u32
		gid     u32
		rdev    u32
		atim    FSysTimespec
		mtim    FSysTimespec
		ctim    FSysTimespec
		size    u64
		blocks  i64
		blksize u32
		flags   u32
		gen     u32
		lspare  i32
		qspare1 u64
		qspare2 u64
	}

	pub fn bf_stat(path string, follow bool) (FSysStat, int) {
		flags := if follow { u64(0) } else { bf_at_symlink_nofollow }
		mut st := FSysStatRaw{}
		rc, er := sys_call4(bf_nr_fstatat, u64(bf_at_fdcwd) & 0xffffffff, u64(path.str),
			u64(voidptr(&st)), flags)
		if er != 0 {
			return FSysStat{}, bf_err_of(rc, er)
		}
		return FSysStat{
			dev:   u64(st.dev)
			ino:   st.ino
			nlink: u64(st.nlink)
			mode:  u32(st.mode)
			uid:   st.uid
			gid:   st.gid
			rdev:  u64(st.rdev)
			size:  st.size
			atime: st.atim.tv_sec
			mtime: st.mtim.tv_sec
			ctime: st.ctim.tv_sec
		}, 0
	}

	// FSysDirentRaw per DragonFly: fileno u64, namlen u16, dtype u8, unused1 u8,
	// unused2 u32, name at offset 16; no reclen field - records are computed
	// (align to 8) (Zig: c.dirent.reclen).
	struct FSysDirentRaw {
		ino     u64
		namlen  u16
		dtype   u8
		unused1 u8
		unused2 u32
	}

	const bf_dirent_name_offset = int(16)

	fn df_reclen(namlen int) int {
		return (bf_dirent_name_offset + namlen + 1 + 7) & ~7
	}

	pub fn bf_readdir(fd int) ([]string, int) {
		entries := []string{cap: 50}
		mut buf := []u8{len: 2048}
		mut seek_cookie := i64(0)
		for {
			rc, er := sys_call4(bf_nr_getdirentries, u64(fd), u64(voidptr(&buf[0])), u64(buf.len),
				u64(voidptr(&seek_cookie)))
			if er != 0 {
				return entries, bf_err_of(rc, er)
			}
			if rc == 0 {
				break
			}
			unsafe {
				mut i := 0
				for i < int(rc) {
					ent := &FSysDirentRaw(&buf[i])
					if ent.ino != 0 {
						bptr := &u8(voidptr(uint(&buf[i]) + uint(bf_dirent_name_offset)))
						nm := bptr.vstring_with_len(int(ent.namlen))
						if nm != '.' && nm != '..' {
							entries << nm.clone()
						}
					}
					i += df_reclen(int(ent.namlen))
				}
			}
		}
		return entries, 0
	}

	// bf_ioctl issues `request` (FIONREAD) on `fd` with an integer arg pointer.
	pub fn bf_ioctl(fd int, request u64, arg voidptr) int {
		rc, er := sys_call3(bf_nr_ioctl, u64(fd), request, u64(arg))
		_ = rc
		return if er == 0 { 0 } else { bf_norm_errno(unsafe { VlibcErrno(er) }) }
	}
	// bf_ioctl issues `request` (FIONREAD) on `fd` with an integer arg pointer.
	pub fn bf_ioctl(fd int, request u64, arg voidptr) int {
		rc, er := sys_call3(bf_nr_ioctl, u64(fd), request, u64(arg))
		_ = rc
		return if er == 0 { 0 } else { bf_norm_errno(unsafe { VlibcErrno(er) }) }
	}
	pub fn bf_environ() map[string]string {
		return env_walk_to_map()
	}

	pub fn bf_getenv(key string) (string, bool) {
		return env_walk_find(key)
	}

	// bf_executable reads /proc/curproc/file like os.c.v's hosted DragonFly route.
	pub fn bf_executable() string {
		mut buf := [4096]u8{}
		nr, e := bf_readlink('/proc/curproc/file', &buf[0], buf.len)
		if e != 0 {
			return ''
		}
		unsafe {
			return (&buf[0]).vstring_with_len(nr).clone()
		}
	}
}
