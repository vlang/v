module builtin

// bf.v is the FreeBSD member of the uniform freestanding os-syscall surface
// (`bf_*`): the os module calls these on every freestanding target, and each
// *_bare directory shapes its own kernel ABI into the same signatures with
// errno values normalized to the Linux numbers. FreeBSD rides this directory's
// carry-flag syscall trampolines (`sys_callN`), decoded by bsd_split: on error
// the carry flag is set and the kernel returns the positive errno in rax/x0.
// Numbers verified against FreeBSD sys/sys/syscall.h (amd64/arm64 unified).

$if (freestanding || syscalls) && !i386 {
	const bf_nr_read = u64(3)
	const bf_nr_write = u64(4)
	const bf_nr_close = u64(6)
	const bf_nr_lseek = u64(478)
	const bf_nr_openat = u64(499)
	const bf_nr_getdirentries = u64(554)
	const bf_nr_fstatat = u64(552)
	const bf_nr_getcwd = u64(326)
	const bf_nr_chdir = u64(12)
	const bf_nr_nanosleep = u64(240)
	const bf_nr_mkdir = u64(136)
	const bf_nr_unlinkat = u64(503)
	const bf_nr_rmdir = u64(137)
	const bf_nr_renameat = u64(501)
	const bf_nr_readlinkat = u64(500)
	const bf_nr_ftruncate = u64(480)
	const bf_nr_fchmodat = u64(490)
	const bf_nr_thr_self = u64(432)
	const bf_nr_ioctl = u64(54)
	u64(432)
	const bf_nr_sysctl = u64(202)

	const bf_at_fdcwd = i64(-100)
	const bf_at_symlink_nofollow = u64(0x200)
	const bf_at_removedir = u64(0x800)

	fn bf_norm_errno(e VlibcErrno) int {
		// values normalized to the Linux numbers for the os message tables
		return match e {
			.eperm { 1 }
			.enoent { 2 }
			.esrch { 3 }
			.eintr { 4 }
			.eio { 5 }
			.enxio { 6 }
			.ebadf { 9 }
			.edeadlk { 45 }
			.enomem { 12 }
			.eacces { 13 }
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
			.eloop { 40 }
			.enametoolong { 36 }
			.enotempty { 39 }
			.enosys { 38 }
			else { 0 }
		}
	}

	fn bf_err_of(rc u64, er u64) int {
		if er == 0 {
			return 0
		}
		return bf_norm_errno(unsafe { VlibcErrno(er) })
	}

	// bf_flags transposes Linux o_* numbers (what the os consts carry under
	// freestanding) into freebsd_bare values; bit families differ per kernel.
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

	// The os open layers pass Linux o_* numbers through their consts; the bf
	// surface keeps FreeBSD's semantics directly: O_APPEND=0x8 O_EXCL=0x800
	// O_NOCTTY=0x8000 O_TRUNC=0x400 O_CREAT=0x200 (sys/fcntl.h). bf_flags keeps
	// the pieces; read/write/close/lseek need no conversion.
	pub fn bf_openat(path &u8, flags_raw u64, mode u64) (int, int) {
		flags := bf_flags(flags_raw)

		rc, er := sys_call4(bf_nr_openat, u64(u32(bf_at_fdcwd)), u64(path), flags, mode)
		return bf_split_int(rc, er)
	}

	// O_DIRECTORY=0x20000 on FreeBSD (bit 17).
	const bf_o_directory = u64(0o20000)
	const bf_o_rdonly = u64(0)

	pub fn bf_openat_dir(path &u8) (int, int) {
		return bf_openat(path, bf_o_rdonly | bf_o_directory, u64(0))
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
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_lseek(fd int, offset i64, whence i64) (i64, int) {
		rc, er := sys_call3(bf_nr_lseek, u64(fd), u64(offset), u64(whence))
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
		rc, er := sys_call2(bf_nr_mkdir, u64(path.str), mode)
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
		rc, er := sys_call2(bf_nr_ftruncate, u64(fd), len)
		_ = rc
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
		rc, er := sys_call0(pid_nr())
		_ = er
		return int(rc)
	}

	// the getpid number sits in the generated table as fb_nr_getpid; keep the
	// read through that const (20 on freebsd).
	@[inline]
	fn pid_nr() u64 {
		return 20
	}

	pub fn bf_gettid() u64 {
		rc, er := sys_call0(bf_nr_thr_self)
		_ = er
		return rc
	}

	// FSysStatRaw matches FreeBSD `struct stat` from sys/stat.h (sysroot-verified
	// field order and widths, LP64): the kernel fills 232 bytes through fstatat.
	struct FSysStatRaw {
		dev      u64
		ino      u64
		nlink    u64
		mode     u16
		bsdflags i16
		uid      u32
		gid      u32
		pad1     i32
		rdev     u64
		atim     FSysTimespec
		mtim     FSysTimespec
		ctim     FSysTimespec
		btim     FSysTimespec
		size     i64
		blocks   u64
		blksize  i32
		flags    u32
		gen      u64
		filerev  u64
		spare    [9]u64
	}

	pub fn bf_stat(path string, follow bool) (FSysStat, int) {
		flags := if follow { u64(0) } else { bf_at_symlink_nofollow }
		mut st := FSysStatRaw{}
		rc, er := sys_call4(bf_nr_fstatat, u64(u32(bf_at_fdcwd)), u64(path.str), u64(voidptr(&st)),
			flags)
		if er != 0 {
			return FSysStat{}, bf_err_of(rc, er)
		}
		return FSysStat{
			dev:   st.dev
			ino:   st.ino
			nlink: st.nlink
			mode:  u32(st.mode)
			uid:   st.uid
			gid:   st.gid
			rdev:  st.rdev
			size:  u64(st.size)
			atime: st.atim.tv_sec
			mtime: st.mtim.tv_sec
			ctime: st.ctim.tv_sec
		}, 0
	}

	// FSysDirentRaw matches FreeBSD `struct dirent` (sysroot-verified): the name
	// starts at offset 24, records walk by reclen, deleted entries carry fileno
	// zero and must be skipped the way Zig's dirReadBsd does.
	struct FSysDirentRaw {
		ino    u64
		off    i64
		reclen u16
		dtype  u8
		pad0   u8
		namlen u16
		pad1   u16
	}

	const bf_dirent_name_offset = int(24)

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

	// bf_executable: FreeBSD exposes the executable path through
	// SYS_sysctl(KERN_PROC_PATHNAME); the mib is [CTL.KERN (1, 14), 12, -1],
	// matching os.c.v's hosted C.sysctl route.
	const bf_sysctl_ctl_kern = i32(1)
	const bf_sysctl_kern_proc = i32(14)
	const bf_sysctl_kern_proc_pathname = i32(12)

	pub fn bf_executable() string {
		mut buf := [4096]u8{}
		mib := [bf_sysctl_ctl_kern, bf_sysctl_kern_proc, bf_sysctl_kern_proc_pathname, -1]!
		bufsize := u64(buf.len)
		rc, er := sys_call6(bf_nr_sysctl, u64(voidptr(&mib[0])), u64(mib.len), u64(voidptr(&buf[0])),
			u64(voidptr(&bufsize)), u64(0), u64(0))
		_ = rc
		if er != 0 {
			return ''
		}
		unsafe {
			return tos_clone(&buf[0])
		}
	}
}

// env_walk_to_map / env_walk_find walk the argv-block environment (see
// linux_bare/bf.v for the layout rationale). Shared between the bf files of
// the ELF targets, so it lives outside the per-OS $if and only the callers do.
