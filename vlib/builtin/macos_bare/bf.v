module builtin

// bf.v is the macOS/darwin member of the uniform freestanding os-syscall
// surface (`bf_*`). macOS rides this directory's trampolines: on arm64 the
// syscall number goes through x16 with svc 0x80 and the 0x2000000 BSD class
// stripped; on amd64 through plain syscall (both already in syscalls.v).
// macOS has no openat/mkdirat/fstatat: the surface uses open(0x2000005),
// mkdir(0x2000088), unlink(0x200000a), rmdir(0x2000088-unset ... 0x2000088 is
// mkdir; rmdir=0x2000089 per Apple tables), rename(0x2000080), stat64
// (0x2000152) and fstat64 (0x2000153). Startup stays libSystem (the tree's
// model), so freestanding macOS programs still link -lSystem.

$if (freestanding || syscalls) && !i386 {
	const bf_nr_read = u64(0x2000003)
	const bf_nr_write = u64(0x2000004)
	const bf_nr_close = u64(0x2000006)
	const bf_nr_open = u64(0x2000005)
	const bf_nr_lseek = u64(0x20000c7)
	const bf_nr_ftruncate = u64(0x20000c9)
	const bf_nr_stat64 = u64(0x2000152)
	const bf_nr_lstat64 = u64(0x2000154) // lstat64 is syscall 340 in XNU
	const bf_nr_getdirentries64 = u64(0x2000158)
	const bf_nr_chdir = u64(0x200000c)
	const bf_nr_nanosleep = u64(0x20000f0)
	const bf_nr_ioctl = u64(0x2000036)
	const bf_nr_mkdir = u64(0x2000088)
	const bf_nr_unlink = u64(0x200000a)
	const bf_nr_rmdir = u64(0x2000089)
	const bf_nr_rename = u64(0x2000080)
	const bf_nr_readlink = u64(0x200003a)
	const bf_nr_chmod = u64(0x200000f)
	const bf_nr_fcntl = u64(0x200005c)
	const bf_nr_ftruncate64 = u64(0x20000c9)

	const bf_o_directory = u64(0x100000) // bit 20 of the O packed struct
	const bf_fcntl_getpath = u64(50) // F_GETPATH

	fn bf_norm_errno(e VlibcErrno) int {
		// values normalized to the Linux numbers for the os message tables; an
		// errno with no Linux counterpart reads as EIO, never as success
		return match e {
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
			.einprogress { 116 }
			.ealready { 115 }
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
			.enetdown { 101 }
			.enetunreach { 102 }
			.enetreset { 103 }
			.econnaborted { 104 }
			.econnreset { 105 }
			.enobufs { 106 }
			.eisconn { 107 }
			.enotconn { 108 }
			.eshutdown { 109 }
			.etoomanyrefs { 110 }
			.etimedout { 111 }
			.econnrefused { 112 }
			.eloop { 40 }
			.enametoolong { 36 }
			.ehostdown { 113 }
			.ehostunreach { 114 }
			.enotempty { 39 }
			.eusers { 87 }
			.edquot { 123 }
			.estale { 117 }
			.eremote { 66 }
			.enolck { 37 }
			.enosys { 38 }
			else { 5 }
		}
	}

	fn bf_err_of(rc u64, er u64) int {
		if er == 0 {
			return 0
		}
		return bf_norm_errno(unsafe { VlibcErrno(er) })
	}

	// bf_flags transposes Linux o_* numbers (what the os consts carry under
	// freestanding) into darwin values:
	// RW bits 0-1 pass through; CREAT linux 0o100->0x200; TRUNC 0o1000->0x400;
	// EXCL 0x80->0x800; APPEND 0o2000->0x8; NONBLOCK 0o4000->0x4; SYNC
	// 0o4010000->0x80 (0x401000 carries sync); NOCTTY 0o400 not provided by
	// darwin (ignored); O_DIRECTORY is only used by bf_openat_dir.
	pub fn bf_flags(linux_flags u64) u64 {
		mut out := linux_flags & u64(0x3)
		if linux_flags & u64(0o100) != 0 {
			out |= u64(0x200)
		}
		if linux_flags & u64(0x80) != 0 {
			out |= u64(0x800)
		}
		if linux_flags & u64(0o400) != 0 {
			out |= u64(0o200000) // O_NOCTTY == bit 17 on darwin
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

	// bf_openat has no *at syscall on darwin; the path given is used directly
	// with O_* flags (relative paths resolve against the cwd, like open/1).
	pub fn bf_openat(path &u8, flags_raw u64, mode u64) (int, int) {
		flags := bf_flags(flags_raw)
		rc, er := sys_call3(bf_nr_open, u64(path), flags, mode)
		return bf_split_int(rc, er)
	}

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
		rc, er := sys_call3(bf_nr_lseek, u64(fd), u64(offset), u64(whence))
		if er != 0 {
			return i64(-1), bf_err_of(rc, er)
		}
		return i64(rc), 0
	}

	// bf_getcwd uses fcntl F_GETPATH on the cwd fd 0 via `.`-opening; Zig's
	// Io/Threaded.zig resolves darwin realPath through F.GETPATH (50).
	pub fn bf_getcwd(buf &u8, size int) (int, int) {
		fd, e := bf_openat(c'.', u64(0) | bf_o_directory, u64(0))
		if e != 0 {
			return -1, e
		}
		defer {
			bf_close(fd)
		}
		rc, er := sys_call3(bf_nr_fcntl, u64(fd), bf_fcntl_getpath, u64(buf))
		return bf_split_int(rc, er)
	}

	pub fn bf_chdir(path string) int {
		rc, er := sys_call1(bf_nr_chdir, u64(path.str))
		return bf_err_of(rc, er)
	}

	pub fn bf_mkdir(path string, mode u64) int {
		rc, er := sys_call2(bf_nr_mkdir, u64(path.str), mode)
		return bf_err_of(rc, er)
	}

	pub fn bf_unlink(path string) int {
		rc, er := sys_call1(bf_nr_unlink, u64(path.str))
		return bf_err_of(rc, er)
	}

	pub fn bf_rmdir(path string) int {
		rc, er := sys_call1(bf_nr_rmdir, u64(path.str))
		return bf_err_of(rc, er)
	}

	pub fn bf_rename(src string, dst string) int {
		rc, er := sys_call2(bf_nr_rename, u64(src.str), u64(dst.str))
		return bf_err_of(rc, er)
	}

	pub fn bf_readlink(path string, buf &u8, size int) (int, int) {
		rc, er := sys_call3(bf_nr_readlink, u64(path.str), u64(buf), u64(size))
		return bf_split_int(rc, er)
	}

	pub fn bf_fchmodat(path string, mode u64) int {
		rc, er := sys_call2(bf_nr_chmod, u64(path.str), mode)
		return bf_err_of(rc, er)
	}

	pub fn bf_truncate(path string, len u64) int {
		fd, e := bf_openat(path.str, u64(1), u64(0)) // O_WRONLY
		if e != 0 {
			return e
		}
		rc, er := sys_call2(bf_nr_ftruncate, u64(fd), len)
		bf_close(fd)
		return bf_err_of(rc, er)
	}

	// FSysTimespec matches darwin's struct timespec (i64 seconds and nanoseconds).
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
		rc, er := sys_call0(u64(0x2000014))
		_ = er
		return int(rc)
	}

	pub fn bf_gettid() u64 {
		// darwin exposes the TID through the libSystem surface; keep the process
		// identity for the signal globals until pthread_threadid_np gets ported.
		return u64(bf_getpid())
	}

	// FSysStatRaw matches darwin `struct stat` (one layout for amd64 and arm64,
	// Zig c.zig: dev i32, mode u16, nlink u16, ino u64, uid/gid u32, rdev i32,
	// four timespecs, size i64, blocks i64, blksize i32, flags/gen u32,
	// lspare i32, qspare [2]i64).
	struct FSysStatRaw {
		dev     i32
		mode    u16
		nlink   u16
		ino     u64
		uid     u32
		gid     u32
		rdev    i32
		atim    FSysTimespec
		mtim    FSysTimespec
		ctim    FSysTimespec
		btim    FSysTimespec
		size    i64
		blocks  i64
		blksize i32
		flags   u32
		gen     u32
		lspare  i32
		qspare  [2]i64
	}

	// bf_stat decodes through stat64 (follow) or lstat64 (nofollow); darwin has
	// no fstatat.
	pub fn bf_stat(path string, follow bool) (FSysStat, int) {
		mut st := FSysStatRaw{}
		nr := if follow { bf_nr_stat64 } else { bf_nr_lstat64 }
		rc, er := sys_call2(nr, u64(path.str), u64(voidptr(&st)))
		if er != 0 {
			return FSysStat{}, bf_err_of(rc, er)
		}
		return FSysStat{
			dev:   u64(u32(st.dev))
			ino:   st.ino
			nlink: u64(st.nlink)
			mode:  u32(st.mode)
			uid:   st.uid
			gid:   st.gid
			rdev:  u64(u32(st.rdev))
			size:  u64(st.size)
			atime: st.atim.tv_sec
			mtime: st.mtim.tv_sec
			ctime: st.ctim.tv_sec
		}, 0
	}

	// FSysDirentRaw per darwin: ino u64, seekoff u64, reclen u16, namlen u16,
	// dtype u8 - name starts at offset 21.
	struct FSysDirentRaw {
		ino    u64
		off    u64
		reclen u16
		namlen u16
		dtype  u8
	}

	const bf_dirent_name_offset = int(21)

	// bf_readdir walks __getdirentries64 batches; the kernel writes one u64
	// cookie ("seek base" basep pointer in Zig's Io, i64 there) when lev positioned
	// after the buffer. Deleted entries carry fileno zero.
	pub fn bf_readdir(fd int) ([]string, int) {
		entries := []string{cap: 50}
		mut buf := []u8{len: 2048}
		mut seek_cookie := u64(0)
		for {
			rc, er := sys_call4(bf_nr_getdirentries64, u64(fd), u64(voidptr(&buf[0])), u64(buf.len),
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
					i += int(ent.reclen)
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
	pub fn bf_environ() map[string]string {
		return env_walk_to_map()
	}

	pub fn bf_getenv(key string) (string, bool) {
		return env_walk_find(key)
	}

	// bf_executable resolves _dyld_get_image_name(0), which libSystem provides on
	// darwin freestanding builds (same symbol os.c.v's hosted route uses).
	fn C._dyld_get_image_name(image u32) &char

	pub fn bf_executable() string {
		unsafe {
			self_path := &u8(C._dyld_get_image_name(u32(0)))
			if self_path == unsafe { nil } {
				return ''
			}
			mut s := 0
			for self_path[s] != 0 {
				s++
			}
			return self_path.vstring_with_len(s).clone()
		}
	}
}
