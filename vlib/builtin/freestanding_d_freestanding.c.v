module builtin

// freestanding.c.v provides builtin's own raw syscall runtime for freestanding
// builds that run on a non-C backend (`-b x64` / `-b arm64`, the SSA pipeline).
// The driver includes the per-platform bare builtin directory (and with it the
// reference implementations of the malloc/realloc/memcpy surface and the
// bare_* hooks in linux_bare) only when `backend == 'c'`, so on other backends
// builtin's own freestanding branches (printing, string.free, panicing,
// allocation) would reference undefined functions. Everything here is gated to
// `$if freestanding && native`, so these definitions are mutually exclusive
// with the linux_bare ones, and to `$if linux`, because the inline asm
// implements the Linux syscall ABI.
$if freestanding && native && linux {
	$if amd64 {
		const bare_sys_nr_read = u64(0)
		const bare_sys_nr_write = u64(1)
		const bare_sys_nr_mmap = u64(9)
		const bare_sys_nr_munmap = u64(11)
		const bare_sys_nr_mremap = u64(25)
		const bare_sys_nr_exit_group = u64(231)
		const bare_sys_nr_close = u64(3)
		const bare_sys_nr_lseek = u64(8)
		const bare_sys_nr_openat = u64(257)
		const bare_sys_nr_getdents64 = u64(217)
		const bare_sys_nr_statx = u64(332)
		const bare_sys_nr_getcwd = u64(79)
		const bare_sys_nr_chdir = u64(80)
		const bare_sys_nr_nanosleep = u64(35)
		const bare_sys_nr_mkdir = u64(83)
		const bare_sys_nr_unlink = u64(87)
		const bare_sys_nr_rmdir = u64(84)
		const bare_sys_nr_rename = u64(82)
		const bare_sys_nr_readlink = u64(89)
		const bare_sys_nr_ftruncate = u64(77)
		const bare_sys_nr_fchmodat = u64(268)
		const bare_sys_nr_thr_self = u64(186)
		const bare_sys_nr_getppid = u64(110)
		const bare_sys_nr_uname = u64(63)
		const bare_sys_nr_statfs = u64(137)
	} $else $if arm64 {
		const bare_sys_nr_read = u64(63)
		const bare_sys_nr_write = u64(64)
		const bare_sys_nr_mmap = u64(222)
		const bare_sys_nr_munmap = u64(215)
		const bare_sys_nr_mremap = u64(216)
		const bare_sys_nr_exit_group = u64(94)
		const bare_sys_nr_close = u64(3)
		const bare_sys_nr_lseek = u64(62)
		const bare_sys_nr_openat = u64(56)
		const bare_sys_nr_getdents64 = u64(61)
		const bare_sys_nr_statx = u64(291)
		const bare_sys_nr_getcwd = u64(17)
		const bare_sys_nr_chdir = u64(49)
		const bare_sys_nr_nanosleep = u64(101)
		const bare_sys_nr_mkdir = u64(34)
		const bare_sys_nr_unlink = u64(35)
		const bare_sys_nr_rmdir = u64(35)
		const bare_sys_nr_rename = u64(38)
		const bare_sys_nr_readlink = u64(78)
		const bare_sys_nr_ftruncate = u64(46)
		const bare_sys_nr_fchmodat = u64(53)
		const bare_sys_nr_thr_self = u64(178)
		const bare_sys_nr_getppid = u64(173)
		const bare_sys_nr_uname = u64(160)
		const bare_sys_nr_statfs = u64(43)
	}

	// The kernel clobbers rcx and r11 on amd64; the arm64 syscall number travels
	// in x8. The shapes follow builtin/linux_bare/sys_call* and Zig's
	// std/os/linux syscall0..syscall6 entries.
	fn bsys_call1(scn u64, arg1 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
			syscall
			; =a (res)
			; 0 (scn)
			  D (arg1)
			; rcx
			  r11
			  memory
		}
		} $else $if arm64 {
			asm volatile arm64 {
			mov x8, scn
			mov x0, arg1
			svc 0
			mov res, x0
			; =r (res)
			; r (scn)
			  r (arg1)
			; x0
			  x1
			  x8
			  memory
		}
		}
		return res
	}

	fn bsys_call2(scn u64, arg1 u64, arg2 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
			syscall
			; =a (res)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			; rcx
			  r11
			  memory
		}
		} $else $if arm64 {
			asm volatile arm64 {
			mov x8, scn
			mov x0, arg1
			mov x1, arg2
			svc 0
			mov res, x0
			; =r (res)
			; r (scn)
			  r (arg1)
			  r (arg2)
			; x0
			  x1
			  x2
			  x8
			  memory
		}
		}
		return res
	}

	fn bsys_call3(scn u64, arg1 u64, arg2 u64, arg3 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
			syscall
			; =a (res)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			; rcx
			  r11
			  memory
		}
		} $else $if arm64 {
			asm volatile arm64 {
			mov x8, scn
			mov x0, arg1
			mov x1, arg2
			mov x2, arg3
			svc 0
			mov res, x0
			; =r (res)
			; r (scn)
			  r (arg1)
			  r (arg2)
			  r (arg3)
			; x0
			  x1
			  x2
			  x8
			  memory
		}
		}
		return res
	}

	fn bsys_call4(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
			mov r10, arg4
			syscall
			; =a (res)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			  g (arg4)
			; rcx
			  r10
			  r11
			  memory
		}
		} $else $if arm64 {
			asm volatile arm64 {
			mov x8, scn
			mov x0, arg1
			mov x1, arg2
			mov x2, arg3
			mov x3, arg4
			svc 0
			mov res, x0
			; =r (res)
			; r (scn)
			  r (arg1)
			  r (arg2)
			  r (arg3)
			  r (arg4)
			; x0
			  x1
			  x2
			  x3
			  x8
			  memory
		}
		}
		return res
	}

	fn bsys_call5(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
			mov r10, arg4
			mov r8, arg5
			syscall
			; =a (res)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			  g (arg4)
			  g (arg5)
			; rcx
			  r10
			  r11
			  r8
			  memory
		}
		} $else $if arm64 {
			asm volatile arm64 {
			mov x8, scn
			mov x0, arg1
			mov x1, arg2
			mov x2, arg3
			mov x3, arg4
			mov x4, arg5
			svc 0
			mov res, x0
			; =r (res)
			; r (scn)
			  r (arg1)
			  r (arg2)
			  r (arg3)
			  r (arg4)
			  r (arg5)
			; x0
			  x1
			  x2
			  x3
			  x4
			  x8
			  memory
		}
		}
		return res
	}

	fn bsys_call6(scn u64, arg1 u64, arg2 u64, arg3 u64, arg4 u64, arg5 u64, arg6 u64) u64 {
		mut res := u64(-i64(38)) // -ENOSYS, overwritten by the syscall on amd64 and arm64
		$if amd64 {
			asm volatile amd64 {
			mov r10, arg4
			mov r8, arg5
			mov r9, arg6
			syscall
			; =a (res)
			; 0 (scn)
			  D (arg1)
			  S (arg2)
			  d (arg3)
			  g (arg4)
			  g (arg5)
			  g (arg6)
			; rcx
			  r10
			  r11
			  r8
			  r9
			  memory
		}
		} $else $if arm64 {
			asm volatile arm64 {
			mov x8, scn
			mov x0, arg1
			mov x1, arg2
			mov x2, arg3
			mov x3, arg4
			mov x4, arg5
			mov x5, arg6
			svc 0
			mov res, x0
			; =r (res)
			; r (scn)
			  r (arg1)
			  r (arg2)
			  r (arg3)
			  r (arg4)
			  r (arg5)
			  r (arg6)
			; x0
			  x1
			  x2
			  x3
			  x4
			  x5
			  x8
			  memory
		}
		}
		return res
	}

	// bsys_write writes `len` bytes of `buf` to descriptor `fd`; a failed write
	// returns the kernel's errno as a small negative number.
	fn bsys_write(fd i64, buf &u8, len u64) i64 {
		return i64(bsys_call3(bare_sys_nr_write, u64(fd), u64(buf), len))
	}

	// bsys_mmap maps private anonymous zeroed memory; success returns its address.
	fn bsys_mmap(addr voidptr, len u64) (&u8, i64) {
		// the -1 anonymous fd travels through the register as the max u64 value
		anon_fd := u64(18446744073709551615)
		rc := bsys_call6(bare_sys_nr_mmap, u64(addr), len, u64(3), u64(0x22), anon_fd, u64(0))
		v := i64(rc)
		if v < 0 {
			return unsafe { nil }, v
		}
		return unsafe { &u8(v) }, 0
	}

	fn bsys_munmap(addr voidptr, len u64) i64 {
		rc := bsys_call3(bare_sys_nr_munmap, u64(addr), len, u64(0))
		v := i64(rc)
		return if v < 0 { v } else { 0 }
	}

	fn bsys_mremap(old_addr voidptr, old_len u64, new_len u64) voidptr {
		maymove := u64(1) // MREMAP_MAYMOVE
		rc := bsys_call6(bare_sys_nr_mremap, u64(old_addr), old_len, new_len, maymove, u64(0), u64(0))
		v := i64(rc)
		if v < 0 {
			return unsafe { nil }
		}
		return unsafe { voidptr(v) }
	}

	// The allocator: every block occupies one anonymous mapping. A size header
	// precedes the returned pointer, like linux_bare's mm_alloc, so free/realloc
	// know the mapping size and freed blocks are returned to the kernel. This is
	// less compact than the dlmalloc arrangement of the C backend, but needs no
	// module imports, which keeps it usable from builtin itself.
	const bare_block_header_size = u64(16)

	// bmem_alloc returns the user pointer of a fresh mapping for `n` bytes.
	fn bmem_alloc(n u64) voidptr {
		a, _ := bsys_mmap(unsafe { nil }, n + bare_block_header_size)
		if a == unsafe { nil } {
			return unsafe { nil }
		}
		unsafe {
			*&u64(voidptr(a)) = n
			return voidptr(a + bare_block_header_size)
		}
	}

	// bmem_block_start reads the mapping start for a user pointer.
	fn bmem_block_start(ptr voidptr) &u8 {
		unsafe {
			return &u8(voidptr(uint(ptr) - uint(bare_block_header_size)))
		}
	}

	fn bmem_block_size(ptr voidptr) u64 {
		unsafe {
			return *&u64(voidptr(bmem_block_start(ptr)))
		}
	}

	// The bare output/input hooks that builtin's freestanding branches call.
	fn bare_print(buf &u8, len u64) {
		bsys_write(1, buf, len)
	}

	fn bare_eprint(buf &u8, len u64) {
		bsys_write(2, buf, len)
	}

	@[noreturn]
	fn bare_panic(msg string) {
		bare_eprint(msg.str, u64(msg.len))
		bare_eprint(c'\n', 1)
		bsys_call1(bare_sys_nr_exit_group, 1)
		for {}
	}

	// bare_backtrace is not available without libc (or a debug information
	// reader), matching the linux_bare implementation.
	fn bare_backtrace() string {
		return 'backtraces are not available with `-freestanding`'
	}

	// ---- Uniform bf_* os surface (the same signatures linux_bare/bf.v
	// carries, so the os module routes resolve identically under SSA) ----

	const bf_at_fdcwd = u64(18446744073709551516) // -100 in the 64-bit register view

	fn bf_split_int(rc u64) (int, int) {
		v := i64(rc)
		if v < 0 {
			return -1, int(-v)
		}
		return int(v), 0
	}

	fn bf_errno_of(rc u64) int {
		v := i64(rc)
		e := if v < 0 { int(-v) } else { 0 }
		bf_last_errno_cell = e
		return e
	}

	pub fn bf_last_errno() int {
		return bf_last_errno_cell
	}

	pub fn bf_openat(path &u8, flags u64, mode u64) (int, int) {
		rc := bsys_call4(bare_sys_nr_openat, bf_at_fdcwd, u64(path), flags, mode)
		return bf_split_int(rc)
	}

	const bf_o_directory = u64(0o200000)

	pub fn bf_openat_dir(path &u8) (int, int) {
		return bf_openat(path, u64(0) | bf_o_directory, u64(0))
	}

	pub fn bf_read(fd int, buf &u8, count int) (int, int) {
		for {
			nr, re := bf_split_int(bsys_call3(bare_sys_nr_read, u64(fd), u64(buf), u64(count)))
			if re == 4 { // EINTR
				continue
			}
			return nr, re
		}
	}

	pub fn bf_write(fd int, buf &u8, count int) (int, int) {
		for {
			nw, we := bf_split_int(bsys_call3(bare_sys_nr_write, u64(fd), u64(buf), u64(count)))
			if we == 4 { // EINTR
				continue
			}
			return nw, we
		}
	}

	pub fn bf_close(fd int) int {
		return bf_errno_of(bsys_call1(bare_sys_nr_close, u64(fd)))
	}

	pub fn bf_lseek(fd int, offset i64, whence i64) (i64, int) {
		rc := bsys_call3(bare_sys_nr_lseek, u64(fd), u64(offset), u64(whence))
		v := i64(rc)
		if v < 0 {
			return i64(-1), int(-v)
		}
		return v, 0
	}

	pub fn bf_getcwd(buf &u8, size int) (int, int) {
		rc := bsys_call2(bare_sys_nr_getcwd, u64(buf), u64(size))
		return bf_split_int(rc)
	}

	pub fn bf_chdir(path string) int {
		return bf_errno_of(bsys_call1(bare_sys_nr_chdir, u64(path.str)))
	}

	pub fn bf_mkdir(path string, mode u64) int {
		$if arm64 {
			return bf_errno_of(bsys_call3(bare_sys_nr_mkdir, bf_at_fdcwd, u64(path.str), mode))
		}
		return bf_errno_of(bsys_call2(bare_sys_nr_mkdir, u64(path.str), mode))
	}

	pub fn bf_unlink(path string) int {
		$if arm64 {
			return bf_errno_of(bsys_call3(bare_sys_nr_unlink, bf_at_fdcwd, u64(path.str), u64(0)))
		}
		return bf_errno_of(bsys_call1(bare_sys_nr_unlink, u64(path.str)))
	}

	pub fn bf_rmdir(path string) int {
		$if arm64 {
			return bf_errno_of(bsys_call3(bare_sys_nr_unlink, bf_at_fdcwd, u64(path.str),
				u64(0x200))) // AT_REMOVEDIR
		}
		return bf_errno_of(bsys_call1(bare_sys_nr_rmdir, u64(path.str)))
	}

	pub fn bf_rename(src string, dst string) int {
		$if arm64 {
			rc := bsys_call4(bare_sys_nr_rename, bf_at_fdcwd, u64(src.str), bf_at_fdcwd, u64(dst.str))
			e := bf_errno_of(rc)
			_ = rc
			return e
		}
		return bf_errno_of(bsys_call2(bare_sys_nr_rename, u64(src.str), u64(dst.str)))
	}

	pub fn bf_readlink(path string, buf &u8, size int) (int, int) {
		$if arm64 {
			rc := bsys_call4(bare_sys_nr_readlink, bf_at_fdcwd, u64(path.str), u64(buf), u64(size))
			return bf_split_int(rc)
		}
		rc := bsys_call3(bare_sys_nr_readlink, u64(path.str), u64(buf), u64(size))
		return bf_split_int(rc)
	}

	pub fn bf_fchmodat(path string, mode u64) int {
		return bf_errno_of(bsys_call3(bare_sys_nr_fchmodat, bf_at_fdcwd, u64(path.str), mode))
	}

	pub fn bf_truncate(path string, len u64) int {
		fd, e := bf_openat(path.str, u64(1), u64(0)) // O_WRONLY
		if e != 0 {
			return e
		}
		rc := bf_errno_of(bsys_call2(bare_sys_nr_ftruncate, u64(fd), len))
		bf_close(fd)
		return rc
	}

	pub fn bf_getpid() int {
		return int(bsys_call1(u64(39), 0))
	}

	pub fn bf_gettid() u64 {
		return bsys_call1(bare_sys_nr_thr_self, 0)
	}

	pub fn bf_getppid() int {
		rc := bsys_call1(bare_sys_nr_getppid, 0)
		if i64(rc) < 0 {
			bf_last_errno_cell = int(-i64(rc))
			return -1
		}
		return int(rc)
	}

	pub fn bf_nanosleep(ms i64) int {
		mut req := FSysTimespec{ms / 1000, 1_000_000 * (ms % 1000)}
		mut rem := FSysTimespec{}
		for {
			v := i64(bsys_call2(bare_sys_nr_nanosleep, u64(voidptr(&req)), u64(voidptr(&rem))))
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

	// FSysTimespec matches libc timespec on amd64/arm64.
	struct FSysTimespec {
		tv_sec  i64
		tv_nsec i64
	}

	// FSysStatx matches linux/stat.h statx: 0x100 bytes on amd64 and arm64.
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

	pub fn bf_stat(path string, follow bool) (FSysStat, int) {
		flags := if follow { u64(0) } else { u64(0x100) }
		mut stx := FSysStatx{}
		rc := bsys_call5(bare_sys_nr_statx, bf_at_fdcwd, u64(path.str), flags, u64(0x7ff),
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

	pub fn bf_uname(buf &u8) int {
		rc := bsys_call1(bare_sys_nr_uname, u64(buf))
		if i64(rc) < 0 {
			bf_last_errno_cell = int(-i64(rc))
			return -1
		}
		return int(rc)
	}

	pub fn bf_statfs(path string, buf &u8) int {
		rc := bsys_call2(bare_sys_nr_statfs, u64(path.str), u64(buf))
		if i64(rc) < 0 {
			bf_last_errno_cell = int(-i64(rc))
			return -1
		}
		return int(rc)
	}

	// bf_getenv / bf_environ walk the argv-block environment (layout note in
	// linux_bare/bf.v) - identical mechanics on every ELF kernel.
	pub fn bf_getenv(key string) (string, bool) {
		if g_main_argv == unsafe { nil } {
			return '', false
		}
		// envp starts at argv[argc + 1] on every ELF kernel; the walk mirrors
		// builtin's arguments() so the C and native backends lower it identically.
		argv := &&u8(g_main_argv)
		mut i := 0
		for {
			entry := unsafe { argv[g_main_argc + 1 + i] }
			if entry == unsafe { nil } {
				return '', false
			}
			// scan to '=': key = entry[0..eq], value = entry[eq + 1..n]
			mut n := 0
			for {
				b := unsafe { entry[n] }
				if b == 0 { break }
				n++
			}
			if n > key.len && unsafe { entry[key.len] } == u8(61) {
				mut same := true
				for k in 0 .. key.len {
					if unsafe { entry[k] } != u8(key[k]) {
						same = false
						break
					}
				}
				if same {
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
				b := unsafe { entry[n] }
				if b == 0 { break }
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

	// FSysDirent64 matches linux/dirent64; name at byte offset 19.
	struct FSysDirent64 {
		ino    u64
		off    u64
		reclen u16
		dtype  u8
	}

	pub fn bf_getdents64(fd int, buf &u8, count int) (int, int) {
		rc := bsys_call3(bare_sys_nr_getdents64, u64(fd), u64(buf), u64(count))
		return bf_split_int(rc)
	}

	pub fn bf_readdir(fd int) ([]string, int) {
		entries := []string{cap: 50}
		mut buf := []u8{len: 2048}
		for {
			nr, re := bf_getdents64(fd, buf.data, buf.len)
			if re != 0 && re != 2 { // ENOENT: end of the directory stream
				return entries, re
			}
			if nr == 0 {
				break
			}
			mut i := 0
			for i < nr {
				unsafe {
					ent := &FSysDirent64(&buf[i])
					bptr := &u8(voidptr(uint(&buf[i]) + uint(19)))
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

	pub fn bf_executable() string {
		mut buf := [4096]u8{}
		nr, e := bf_readlink('/proc/self/exe', &buf[0], buf.len)
		if e != 0 {
			return ''
		}
		unsafe {
			return (&buf[0]).vstring_with_len(nr).clone()
		}
	}

	pub fn bf_ioctl(fd int, request u64, arg voidptr) int {
		return bf_errno_of(bsys_call3(16, u64(fd), request, u64(arg)))
	}

	// The libc-shaped surface builtin and generated code expect. Keep the
	// linux_bare export names, so C shims built against either implementation
	// keep working.

	// __malloc carves a fresh block from a private anonymous mapping.
	@[export: 'malloc']
	@[markused; unsafe]
	fn __malloc(n uint) voidptr {
		if n == 0 {
			return unsafe { nil }
		}
		return bmem_alloc(u64(n))
	}

	@[export: 'free']
	@[markused; unsafe]
	fn __free(ptr voidptr) {
		if ptr == 0 {
			return
		}
		unsafe {
			bsys_munmap(bmem_block_start(ptr), bmem_block_size(ptr) + bare_block_header_size)
		}
	}

	@[export: 'realloc']
	@[markused; unsafe]
	fn realloc(old_area voidptr, new_size uint) voidptr {
		if old_area == 0 {
			return unsafe { __malloc(new_size) }
		}
		if new_size == uint(0) {
			unsafe { __free(old_area) }
			return unsafe { nil }
		}
		unsafe {
			old_size := bmem_block_size(old_area)
			new_addr := bmem_alloc(u64(new_size))
			if new_addr == unsafe { nil } {
				return unsafe { nil }
			}
			min_size := if old_size < u64(new_size) { old_size } else { u64(new_size) }
			memcpy(new_addr, old_area, uint(min_size))
			bsys_munmap(bmem_block_start(old_area), old_size + bare_block_header_size)
			return new_addr
		}
	}

	@[export: 'calloc']
	@[markused; unsafe]
	fn __calloc(nmemb uint, size uint) voidptr {
		new_area := unsafe { __malloc(nmemb * size) }
		if new_area != 0 {
			unsafe { memset(new_area, 0, nmemb * size) }
		}
		return new_area
	}

	@[export: 'memcpy']
	@[markused; unsafe]
	fn memcpy(dest voidptr, const_src voidptr, n uint) voidptr {
		dest_ := unsafe { &u8(dest) }
		src_ := unsafe { &u8(const_src) }
		unsafe {
			for i in 0 .. int(n) {
				dest_[i] = src_[i]
			}
		}
		return unsafe { dest }
	}

	@[export: 'memmove']
	@[markused; unsafe]
	fn memmove(dest voidptr, const_src voidptr, n uint) voidptr {
		dest_ := unsafe { &u8(dest) }
		src_ := unsafe { &u8(const_src) }
		if uint(dest_) <= uint(src_) || uint(dest_) >= uint(src_) + n {
			unsafe {
				for i in 0 .. int(n) {
					dest_[i] = src_[i]
				}
			}
		} else {
			unsafe {
				for i := int(n) - 1; i >= 0; i-- {
					dest_[i] = src_[i]
				}
			}
		}
		return unsafe { dest }
	}

	@[export: 'memset']
	@[markused; unsafe]
	fn memset(s voidptr, c int, n uint) voidptr {
		s_ := unsafe { &char(s) }
		unsafe {
			for i in 0 .. int(n) {
				s_[i] = char(c)
			}
		}
		return unsafe { s }
	}

	@[export: 'strlen']
	@[markused; unsafe]
	fn strlen(const_s voidptr) uint {
		s := unsafe { &u8(const_s) }
		mut i := 0
		for ; unsafe { s[i] } != 0; i++ {
		}
		return uint(i)
	}

	@[export: 'memcmp']
	@[markused]
	fn memcmp(const_a voidptr, const_b voidptr, n uint) int {
		a_ := unsafe { &u8(const_a) }
		b_ := unsafe { &u8(const_b) }
		unsafe {
			for i in 0 .. int(n) {
				if a_[i] != b_[i] {
					return int(a_[i]) - int(b_[i])
				}
			}
		}
		return 0
	}

	@[export: 'getchar']
	@[markused]
	fn getchar() int {
		mut x := u8(0)
		rc := bsys_call3(bare_sys_nr_read, u64(0), u64(&x), u64(1))
		if i64(rc) <= 0 {
			return -1
		}
		return int(x)
	}

	@[export: 'exit']
	@[markused; noreturn]
	fn __exit(code int) {
		bsys_call1(bare_sys_nr_exit_group, u64(code))
		for {}
	}

	@[export: 'qsort']
	@[markused]
	fn __qsort(base voidptr, nmemb uint, size uint, sort_cb voidptr) {
		panic('qsort() is not yet implemented in `-freestanding`')
	}

	// vsprintf and vsnprintf back string interpolation; linux_bare rejects it
	// the same way, since a freestanding runtime has no printf machinery.
	fn vsprintf(str_ &char, format &char, ap &u8) int {
		panic('vsprintf(): string interpolation is not supported in `-freestanding`')
	}

	fn vsnprintf(str_ &char, size uint, ap &u8) int {
		panic('vsnprintf(): string interpolation is not supported in `-freestanding`')
	}
}
