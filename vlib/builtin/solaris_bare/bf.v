module builtin

// bf.v is the Solaris/illumos member of the uniform freestanding os-syscall
// surface (`bf_*`). Solaris rides this directory's carry-flag trampolines
// (sys_callN giving (raw, errno)); the surface is amd64-only, matching
// syscalls.v. Numbers verified against the Solaris sys/sys/syscall.h: openat64
// 69, fstatat64 67, getdents64 213, getcwd 229, nanosleep 199, mkdirat 102,
// unlinkat 65, renameat 64, readlinkat 22 and fchmodat 101. Kernel quirks
// kept: AT_FDCWD = 0xffd19553, and errno values diverge heavily from the BSDs
// (ENOSYS=89, ELOOP=90, ENOTEMPTY=93, ENAMETOOLONG=78) - both handled at the
// boundary below, so the os routes keep Linux-shaped errno values.

$if (freestanding || syscalls) && amd64 {
	const bf_nr_read = u64(3)
	const bf_nr_write = u64(4)
	const bf_nr_close = u64(6)
	const bf_nr_lseek = u64(19)
	const bf_nr_openat64 = u64(69)
	const bf_nr_fstatat64 = u64(67)
	const bf_nr_getdents64 = u64(213)
	const bf_nr_getcwd = u64(229)
	const bf_nr_chdir = u64(12)
	const bf_nr_nanosleep = u64(199)
	const bf_nr_mkdirat = u64(102)
	const bf_nr_unlinkat64 = u64(65)
	const bf_nr_rmdir = u64(79)
	const bf_nr_renameat64 = u64(64)
	const bf_nr_readlinkat = u64(22)
	const bf_nr_fchmodat = u64(101)
	const bf_nr_ioctl = u64(54)

	const bf_at_fdcwd = u64(0xffd19553)
	const bf_at_symlink_nofollow = u64(0x1000)
	const bf_at_removedir = u64(1)

	const bf_o_directory = u64(0x1000000)

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
	// freestanding) into solaris_bare values; bit families differ per kernel.
	pub fn bf_flags(linux_flags u64) u64 {
		mut out := linux_flags & u64(0x3)
		if linux_flags & u64(0o100) != 0 {
			out |= u64(0x100)
		}
		if linux_flags & u64(0x80) != 0 {
			out |= u64(0x400)
		}
		if linux_flags & u64(0o400) != 0 {
			out |= u64(0x800)
		}
		if linux_flags & u64(0o1000) != 0 {
			out |= u64(0x200)
		}
		if linux_flags & u64(0o2000) != 0 {
			out |= u64(0x8)
		}
		if linux_flags & u64(0o4000) != 0 {
			out |= u64(0x80)
		}
		if linux_flags & u64(0o4010000) != 0 {
			out |= u64(0x10)
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

		rc, er := sys_call4(bf_nr_openat64, bf_at_fdcwd, u64(path), flags, mode)
		return bf_split_int(rc, er)
	}

	// O_DIRECTORY=0x1000000 on Solaris.
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
		rc, er := sys_call3(bf_nr_mkdirat, bf_at_fdcwd, u64(path.str), mode)
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_unlink(path string) int {
		rc, er := sys_call3(bf_nr_unlinkat64, bf_at_fdcwd, u64(path.str), u64(0))
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_rmdir(path string) int {
		rc, er := sys_call2(bf_nr_rmdir, u64(path.str), u64(0))
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_rename(src string, dst string) int {
		rc, er := sys_call4(bf_nr_renameat64, bf_at_fdcwd, u64(src.str), bf_at_fdcwd, u64(dst.str))
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_readlink(path string, buf &u8, size int) (int, int) {
		rc, er := sys_call4(bf_nr_readlinkat, bf_at_fdcwd, u64(path.str), u64(buf), u64(size))
		return bf_split_int(rc, er)
	}

	pub fn bf_fchmodat(path string, mode u64) int {
		rc, er := sys_call3(bf_nr_fchmodat, bf_at_fdcwd, u64(path.str), mode)
		_ = rc
		return bf_err_of(rc, er)
	}

	pub fn bf_truncate(path string, len u64) int {
		fd, e := bf_openat(path.str, u64(1), u64(0)) // O_WRONLY
		if e != 0 {
			return e
		}
		defer {
			bf_close(fd)
		}
		// Solaris has no ftruncate; F_FREESP through fcntl runs the same shrink
		// (fcntl=92 would need; the surface reports not-implemented instead).
		return 38
	}

	// FSysTimespec matches timestruc_t (i64 seconds and nanoseconds).
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
		// illumos: pthread_self() is the TID (Thread.zig's generic fallthrough);
		// the C path rides libSystem, so bf returns the process identity here.
		return u64(bf_getpid())
	}

	// FSysStatRaw per Solaris struct stat64 (LP64, sysroot-verified): dev u64,
	// ino u64, mode u32, nlink u32, uid u32, gid u32, rdev u64, size i64, three
	// timestrucs, blksize i64, blocks i64, fstype [16]u8; 128 bytes total.
	struct FSysStatRaw {
		dev     u64
		ino     u64
		mode    u32
		nlink   u32
		uid     u32
		gid     u32
		rdev    u64
		size    i64
		atim    FSysTimespec
		mtim    FSysTimespec
		ctim    FSysTimespec
		blksize i64
		blocks  i64
		fstype  [16]u8
	}

	pub fn bf_stat(path string, follow bool) (FSysStat, int) {
		flags := if follow { u64(0) } else { bf_at_symlink_nofollow }
		mut st := FSysStatRaw{}
		rc, er := sys_call4(bf_nr_fstatat64, bf_at_fdcwd, u64(path.str), u64(voidptr(&st)), flags)
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

	// FSysDirentRaw per Solaris dirent64: ino u64, off i64, reclen u16, name at
	// offset 18 (sysroot-verified; no d_type - the os surface only lists names).
	struct FSysDirentRaw {
		ino    u64
		off    i64
		reclen u16
	}

	const bf_dirent_name_offset = int(18)

	pub fn bf_readdir(fd int) ([]string, int) {
		entries := []string{cap: 50}
		mut buf := []u8{len: 2048}
		for {
			rc, er := sys_call3(bf_nr_getdents64, u64(fd), u64(voidptr(&buf[0])), u64(buf.len))
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
						mut nb := 0
						for bptr[nb] != 0 {
							nb++
						}
						nm := bptr.vstring_with_len(nb)
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

	// bf_executable: os executables started through the Solaris kernel get their
	// image from the argv block (the tree's os executable() has no dedicated
	// Solaris route; the os fallback covers this in freestanding too).
	pub fn bf_executable() string {
		return ''
	}
}
