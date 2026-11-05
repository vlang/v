module builtin

// bf.v is the Linux member of the uniform freestanding os-syscall surface
// (`bf_*`): the os module calls these on every freestanding target, and each
// *_bare directory shapes its own kernel ABI into the same signatures with
// errno values normalized to the Linux numbers. Definitions are visible only
// in freestanding builds, because the driver adds *_bare directories just for
// those (and for the non-C backends, builtin/freestanding.c.v mirrors the same
// surface natively). Every wrapper sits on linux_bare's private sys_* entry
// points, reuses its errno decode, and retries nothing: interrupted syscalls
// restart inside bb_read/bf_write/bf_nanosleep where the callers of zstd
// expect it, so the checker sees plain errno values.

$if (freestanding || syscalls) && !i386 {

	// Linux syscall numbers (amd64 table; arm64 openat/lseek/getdents64/statx/mem
	// match this file because it only reaches its *at/64-bit entries).
	const bf_nr_read = u64(0)
	const bf_nr_write = u64(1)
	const bf_nr_close = u64(3)
	const bf_nr_lseek = u64(8)
	const bf_nr_openat = u64(257)
	const bf_nr_getdents64 = u64(217)
	const bf_nr_statx = u64(332)
	const bf_nr_getcwd = u64(79)
	const bf_nr_chdir = u64(80)
	const bf_nr_nanosleep = u64(35)
	const bf_nr_mkdir = u64(83)
	const bf_nr_unlink = u64(87)
	const bf_nr_rmdir = u64(84)
	const bf_nr_rename = u64(82)
	const bf_nr_readlink = u64(89)
	const bf_nr_ftruncate = u64(77)
	const bf_nr_fchmodat = u64(268)
	const bf_nr_ioctl = u64(16)
	const bf_nr_thr_self = u64(186)
	const bf_nr_getppid = u64(110)
	const bf_nr_uname = u64(63)
	const bf_nr_statfs = u64(137)

	const bf_at_fdcwd = u32(4294967196) // -100 as u32, seen by the kernel register

	// The idea here is to ride linux_bare: sys_callN are this build's asm
	// trampolines. Their errno returns in kernel form; bf_ semantics accept both
	// so that the os routes never open the errno question again.
	fn bf_split_int(rc u64) (int, int) {
		v := i64(rc)
		if v < 0 {
			return -1, int(-v)
		}
		return int(v), 0
	}

	// bf_last_errno_cell stores the last error the bf wrappers normalized,
	// the freestanding reading for os.last_error and the SSA errno access.
	__global bf_last_errno_cell = int(0)

	// The C-backend float formatter reaches for libc's fabs/floor/pow; carry
	// pure forms so a bare build resolves them (same shapes the fallbacks in
	// freestanding_math.c.v use).
	fn bf_ln(x f64) f64 {
		// atanh series on (0,2]
		if x == 0 {
			return f64(-1) / 0
		}
		if x < 0 {
			return f64(0) / 0
		}
		mut k := x
		mut scale := 0
		for k >= 2 {
			k = k / 2
			scale++
		}
		for k < 0.5 {
			k = k * 2
			scale--
		}
		m := (k - 1) / (k + 1)
		m2 := m * m
		mut acc := m
		mut term := m
		for n in 1 .. 24 {
			term *= m2
			acc += term / f64(2 * n + 1)
		}
		return 2 * acc + f64(scale) * 0.6931471805599453
	}

	fn bf_exp(x f64) f64 {
		if x < -745 {
			return 0
		}
		k := int(x / 0.6931471805599453)
		mut r := x - f64(k) * 0.6931471805599453
		mut term := 1.0
		mut acc := 1.0
		for n in 1 .. 18 {
			term *= r / f64(n)
			acc += term
		}
		mut power := 1.0
		mut kk := k
		for kk > 0 {
			power *= 2
			kk--
		}
		for kk < 0 {
			power = power / 2
			kk++
		}
		return acc * power
	}

	pub fn bf_last_errno() int {
		return bf_last_errno_cell
	}
	fn bf_errno_of(rc u64) int {
		v := i64(rc)
		e := if v < 0 { int(-v) } else { 0 }
		bf_last_errno_cell = e
		return e
	}

	pub fn bf_openat(path &u8, flags u64, mode u64) (int, int) {
		rc := sys_call4(bf_nr_openat, u64(bf_at_fdcwd), u64(path), flags, mode)
		return bf_split_int(rc)
	}

	// bf_read keeps the read going through EINTR (signal handlers, futex wakeups),
	// which bare zstd expects.
	// O_DIRECTORY per kernel: linux 0x100000 (0o200000).
	const bf_o_directory = u64(0o200000)

	// bf_openat_dir opens `path` as a directory (O_DIRECTORY) for bf_readdir.
	pub fn bf_openat_dir(path &u8) (int, int) {
		return bf_openat(path, u64(o_rdonly) | bf_o_directory, u64(0))
	}

	pub fn bf_read(fd int, buf &u8, count int) (int, int) {
		for {
			nr, e := bf_split_int(sys_call3(bf_nr_read, u64(fd), u64(buf), u64(count)))
			if e == 4 { // EINTR
				continue
			}
			return nr, e
		}
	}

	pub fn bf_write(fd int, buf &u8, count int) (int, int) {
		for {
			nw, e := bf_split_int(sys_call3(bf_nr_write, u64(fd), u64(buf), u64(count)))
			if e == 4 { // EINTR
				continue
			}
			return nw, e
		}
	}

	pub fn bf_close(fd int) int {
		return bf_errno_of(sys_call1(bf_nr_close, u64(fd)))
	}

	pub fn bf_lseek(fd int, offset i64, whence i64) (i64, int) {
		rc := sys_call3(bf_nr_lseek, u64(fd), u64(offset), u64(whence))
		v := i64(rc)
		if v < 0 {
			return i64(-1), int(-v)
		}
		return v, 0
	}

	pub fn bf_getcwd(buf &u8, size int) (int, int) {
		rc := sys_call2(bf_nr_getcwd, u64(buf), u64(size))
		return bf_split_int(rc)
	}

	pub fn bf_chdir(path string) int {
		return bf_errno_of(sys_call1(bf_nr_chdir, u64(path.str)))
	}

	pub fn bf_mkdir(path string, mode u64) int {
		return bf_errno_of(sys_call2(bf_nr_mkdir, u64(path.str), mode))
	}

	pub fn bf_unlink(path string) int {
		return bf_errno_of(sys_call1(bf_nr_unlink, u64(path.str)))
	}

	pub fn bf_rmdir(path string) int {
		return bf_errno_of(sys_call1(bf_nr_rmdir, u64(path.str)))
	}

	pub fn bf_rename(src string, dst string) int {
		return bf_errno_of(sys_call2(bf_nr_rename, u64(src.str), u64(dst.str)))
	}

	pub fn bf_readlink(path string, buf &u8, size int) (int, int) {
		rc := sys_call3(bf_nr_readlink, u64(path.str), u64(buf), u64(size))
		return bf_split_int(rc)
	}

	pub fn bf_fchmodat(path string, mode u64) int {
		return bf_errno_of(sys_call3(bf_nr_fchmodat, u64(bf_at_fdcwd), u64(path.str), mode))
	}

	pub fn bf_truncate(path string, len u64) int {
		fd, e := bf_openat(path.str, u64(1), u64(0)) // O_WRONLY
		if e != 0 {
			return e
		}
		rc := bf_errno_of(sys_call2(bf_nr_ftruncate, u64(fd), len))
		bf_close(fd)
		return rc
	}

	// FSysTimespec matches libc timespec, identical shape on amd64 and arm64.
	struct FSysTimespec {
		tv_sec  i64
		tv_nsec i64
	}

	// bf_nanosleep sleeps `ms` milliseconds, restarting on EINTR with the kernel
	// remainder (Zig's handling; an interrupted sleep never reaches callers).
	pub fn bf_nanosleep(ms i64) int {
		mut req := FSysTimespec{ms / 1000, 1_000_000 * (ms % 1000)}
		mut rem := FSysTimespec{}
		for {
			v := i64(sys_call2(bf_nr_nanosleep, u64(voidptr(&req)), u64(voidptr(&rem))))
			if v < 0 {
				e := int(-v)
				if e == 4 { // EINTR
					req.tv_sec = rem.tv_sec
					req.tv_nsec = rem.tv_nsec
					continue
				}
				return e
			}
			return 0
		}
	}

	pub fn bf_getpid() int {
		return sys_getpid()
	}

	pub fn bf_gettid() u64 {
		return sys_call0(bf_nr_thr_self)
	}

	pub fn bf_getppid() int {
		rc := sys_call1(bf_nr_getppid, 0)
		if i64(rc) < 0 {
			bf_last_errno_cell = int(-i64(rc))
			return -1
		}
		return int(rc)
	}

	// bf_uname fills the linux utsname layout: six NUL-gapped char fields of
	// 65 bytes each (sysname, nodename, release, version, machine, domainname).
	pub fn bf_uname(buf &u8) int {
		rc := sys_call1(bf_nr_uname, u64(buf))
		if i64(rc) < 0 {
			bf_last_errno_cell = int(-i64(rc))
			return -1
		}
		return int(rc)
	}

	// bf_statfs fills the linux `struct statfs` for `path`.
	pub fn bf_statfs(path string, buf &u8) int {
		rc := sys_call2(bf_nr_statfs, u64(path.str), u64(buf))
		if i64(rc) < 0 {
			bf_last_errno_cell = int(-i64(rc))
			return -1
		}
		return int(rc)
	}

	// FSysStatx matches `struct statx` from linux/stat.h, 0x100 bytes on amd64 and
	// arm64; os decodes FSysStat out of it below.
	struct FSysStatx {
		mask            u32
		blksize         u32
		attributes      u64
		nlink           u32
		uid             u32
		gid             u32
		mode            u16
		spare0          u8
		ino             u64
		size            u64
		blocks          u64
		attributes_mask u64
		atime_sec       i64
		atime_nsec      u32
		atime_pad       u32
		btime_sec       i64
		btime_nsec      u32
		btime_pad       u32
		ctime_sec       i64
		ctime_nsec      u32
		ctime_pad       u32
		mtime_sec       i64
		mtime_nsec      u32
		mtime_pad       u32
		rdev_major      u32
		rdev_minor      u32
		dev_major       u32
		dev_minor       u32
		spare2          [14]u64
	}

	// bf_stat fetches file metadata for `path`; linux keeps statx as its one
	// portable stat interface, with lstat through AT_SYMLINK_NOFOLLOW.
	pub fn bf_stat(path string, follow bool) (FSysStat, int) {
		flags := if follow { u64(0) } else { u64(0x100) }
		mut stx := FSysStatx{}
		rc := sys_call5(bf_nr_statx, u64(bf_at_fdcwd), u64(path.str), flags, u64(0x7ff),
			u64(voidptr(&stx)))
		if i64(rc) < 0 {
			return FSysStat{}, int(-i64(rc))
		}
		dev := (u64(stx.dev_major) << 8) | u64(stx.dev_minor)
		rdev := (u64(stx.rdev_major) << 8) | u64(stx.rdev_minor)
		return FSysStat{
			dev:   dev
			ino:   stx.ino
			nlink: u64(stx.nlink)
			mode:  u32(stx.mode)
			uid:   stx.uid
			gid:   stx.gid
			rdev:  rdev
			size:  stx.size
			atime: stx.atime_sec
			mtime: stx.mtime_sec
			ctime: stx.ctime_sec
		}, 0
	}

	// FSysDirent64 matches linux/dirent64: the fixed 19-byte header (ino, off,
	// reclen, dtype) is followed by the NUL-terminated name, and reclen carries
	// each 8-byte aligned record's length.
	struct FSysDirent64 {
		ino    u64
		off    u64
		reclen u16
		dtype  u8
	}

	// bf_getdents64 reads one directory-record batch for `fd`.
	// The kernel returns ENOENT exactly at the drained end of the stream; the os
	// route treats both conditions uniformly through bf_readdir below.
	pub fn bf_getdents64(fd int, buf &u8, count int) (int, int) {
		rc := sys_call3(bf_nr_getdents64, u64(fd), u64(buf), u64(count))
		return bf_split_int(rc)
	}

	pub const bf_dirent_name_offset = int(19)

	// bf_readdir paths the remaining records of the directory at `fd` into plain
	// strings, skipping . and .., the way the libc os.ls backend does. ENOENT at
	// the drained end counts as success, and buffer batches mirror Zig's 2048
	// (reader_buffer_len).
	pub fn bf_readdir(fd int) ([]string, int) {
		entries := []string{cap: 50}
		mut buf := []u8{len: 2048}
		for {
			nr, re := bf_getdents64(fd, buf.data, buf.len)
			if re != 0 && re != 2 { // ENOENT means the end of the directory stream
				return entries, re
			}
			if nr == 0 {
				break
			}
			mut i := 0
			for i < nr {
				unsafe {
					ent := &FSysDirent64(&buf[i])
					bptr := &u8(voidptr(uint(&buf[i]) + uint(bf_dirent_name_offset)))
					mut nb := 0
					for bptr[nb] != 0 {
						nb++
					}
					nm := bptr.vstring_with_len(nb)
					if nm != '.' && nm != '..' {
						entries << nm.clone()
					}
					i += int(ent.reclen)
				}
			}
		}
		return entries, 0
	}

	// bf_getenv walks the userland environment block: on every ELF kernel the
	// argv[] array is followed by envp[] at argv + (argc + 1) slots, which the
	// freestanding `_start` receives and builtin's generated main keeps in
	// g_main_argv/g_main_argc. That is portable across linux/bsd/solaris/macos,
	// matches Zig's `_start` envp walk, and avoids /proc (which macOS and some
	// BSDs do not expose).
	pub fn bf_getenv(key string) (string, bool) {
		if g_main_argv == unsafe { nil } {
			return '', false
		}
		// envp entries start after argv's nil terminator; the walk mirrors
		// builtin's arguments() so the C and native backends lower it identically.
		argv := &&u8(g_main_argv)
		mut i := 0
		for {
			entry := unsafe { argv[g_main_argc + 1 + i] }
			if entry == unsafe { nil } {
				return '', false
			}
			// the key part runs from entry[0] to the `=` (61); entries
			// without `=` are skipped (Zig: process/Environ.getPosix)
			mut n := 0
			for {
				c := unsafe { entry[n] }
				if c == 0 { break }
				n++
			}
			mut eq := -1
			for k in 0 .. n {
				if unsafe { entry[k] } == u8(61) {
					eq = k
					break
				}
			}
			if eq == key.len && eq >= 0 {
				mut ok := true
				for k in 0 .. key.len {
					if unsafe { entry[k] } != u8(key[k]) {
						ok = false
						break
					}
				}
				if ok {
					val_len := n - (key.len + 1)
					if val_len == 0 {
						return '', true
					}
					val := unsafe { tos(entry + key.len + 1, val_len) }
					return val.clone(), true
				}
			}
			i++
		}
	}

	// bf_executable returns the path of the running executable; linux resolves
	// /proc/self/exe with the readlink syscall.
	// bf_environ returns the process environment as a map (the argv block
	// walk of bf_getenv, collecting every KEY=VALUE pair).
	// bf_ioctl issues `request` (FIONREAD) on `fd` with an integer arg pointer.
	pub fn bf_ioctl(fd int, request u64, arg voidptr) int {
		rc, er := sys_call3(bf_nr_ioctl, u64(fd), request, u64(arg))
		_ = rc
		return if er == 0 { 0 } else { bf_norm_errno(unsafe { VlibcErrno(er) }) }
	}
	pub fn bf_environ() map[string]string {
		mut res := map[string]string{}
		if g_main_argv == unsafe { nil } {
			return res
		}
		argv := &&u8(g_main_argv)
		mut i := 0
		for {
			entry := unsafe { argv[g_main_argc + 1 + i] }
			if entry == unsafe { nil } {
				return res
			}
			mut n := 0
			for {
				c := unsafe { entry[n] }
				if c == 0 { break }
				n++
			}
			mut eq := -1
			for k in 0 .. n {
				if unsafe { entry[k] } == u8(61) {
					eq = k
					break
				}
			}
			if eq > 0 && (n - (eq + 1)) > 0 {
				key_part := unsafe { tos(entry, eq) }
				val := unsafe { tos(entry + eq + 1, n - (eq + 1)) }
				res[key_part.clone()] = val.clone()
			}
			i++
		}
		return res
	}

	pub fn bf_executable() string {
		mut buf := [4096]u8{}
		nr, e := bf_readlink('/proc/self/exe', &buf[0], buf.len)
		if e != 0 {
			return ''
		}
		return unsafe { (&buf[0]).vstring_with_len(nr).clone() }
	}
}
