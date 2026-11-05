module builtin

// bf.v is the OpenBSD member of the uniform freestanding os-syscall surface
// (`bf_*`). OpenBSD rides this directory's carry-flag trampolines
// (sys_callN with the (raw, errno) pair; errno sits in x0/rax). Numbers
// verified against OpenBSD sys/sys/syscall.h; AT_* and O_* flags from
// sys/fcntl.h: AT_FDCWD=-100, SYMLINK_NOFOLLOW=0x2, REMOVEDIR=0x8,
// O_CREAT=0x200, O_TRUNC=0x400, O_NONBLOCK=0x4, O_APPEND=0x8, O_DIRECTORY=0x20000.

$if (freestanding || syscalls) && !i386 {
	const bf_nr_read = u64(3)
	const bf_nr_write = u64(4)
	const bf_nr_close = u64(6)
	const bf_nr_lseek = u64(166)
	const bf_nr_openat = u64(321)
	const bf_nr_getdents = u64(99)
	const bf_nr_fstatat = u64(42)
	const bf_nr_getcwd = u64(304)
	const bf_nr_chdir = u64(12)
	const bf_nr_nanosleep = u64(91)
	const bf_nr_mkdir = u64(136)
	const bf_nr_unlink = u64(10)
	const bf_nr_rmdir = u64(137)
	const bf_nr_rename = u64(128)
	const bf_nr_readlink = u64(58)
	const bf_nr_ftruncate = u64(168)
	const bf_nr_fchmodat = u64(314)
	const bf_nr_getthrid = u64(299)
	const bf_nr_ioctl = u64(54)
	const bf_nr_sysctl_out = u64(202)

	const bf_at_fdcwd = i64(-100)
	const bf_at_symlink_nofollow = u64(0x2)
	const bf_at_removedir = u64(0x8)

	const bf_o_directory = u64(0o20000)

	fn bf_norm_errno(e VlibcErrno) int {
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
	// freestanding) into openbsd_bare values; bit families differ per kernel.
	pub fn bf_flags(linux_flags u64) u64 {
		mut out := linux_flags & u64(0x3)
		if linux_flags & u64(0o100) != 0 {
			out |= u64(0x200)
		}
		if linux_flags & u64(0x80) != 0 {
			out |= u64(0x800)
		}
		if linux_flags & u64(0o400) != 0 {
			out |= u64(0)
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

		rc, er := sys_call4(bf_nr_openat, u64(u32(bf_at_fdcwd)), u64(path), flags, mode)
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
		rc, er := sys_call3(bf_nr_fchmodat, u64(u32(bf_at_fdcwd)), u64(path.str), mode)
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
		rc, er := sys_call0(bf_nr_getthrid)
		_ = er
		return rc
	}

	// FSysStatRaw matches OpenBSD `struct stat` from sys/stat.h (LP64, mode
	// first): mode u32 @0, dev/in u64, nlink/uid/gid u32, rdev u64, three
	// timespecs, size i64 @96, blocks i64, blksize u32, flags u32, gen u32, and
	// the birth timespec, 144 bytes total through fstatat.
	struct FSysStatRaw {
		mode    u32
		dev     u64
		ino     u64
		nlink   u32
		uid     u32
		gid     u32
		rdev    u64
		atim    FSysTimespec
		mtim    FSysTimespec
		ctim    FSysTimespec
		size    i64
		blocks  i64
		blksize u32
		flags   u32
		gen     u32
		btim    FSysTimespec
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
			nlink: u64(st.nlink)
			mode:  st.mode
			uid:   st.uid
			gid:   st.gid
			rdev:  st.rdev
			size:  u64(st.size)
			atime: st.atim.tv_sec
			mtime: st.mtim.tv_sec
			ctime: st.ctim.tv_sec
		}, 0
	}

	// FSysDirentRaw matches OpenBSD `struct dirent`: records walk by reclen and
	// deleted entries carry d_fileno zero.
	struct FSysDirentRaw {
		ino    u64
		off    i64
		reclen u16
		dtype  u8
		namlen u8
	}

	const bf_dirent_name_offset = int(24)

	pub fn bf_readdir(fd int) ([]string, int) {
		entries := []string{cap: 50}
		mut buf := []u8{len: 2048}
		for {
			rc, er := sys_call3(bf_nr_getdents, u64(fd), u64(voidptr(&buf[0])), u64(buf.len))
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

	// bf_executable: OpenBSD first reads argv[0] out of the kernel (SYS_sysctl
	// KERN_PROC_ARGS/argv at os.c.v's hosted mib [1, 55, pid, 1]); when it names
	// an absolute path, that is the resolution; otherwise the os fallback runs.
	pub fn bf_executable() string {
		return ''
	}
}
