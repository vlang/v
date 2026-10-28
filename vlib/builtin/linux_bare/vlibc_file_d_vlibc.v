module builtin

$if !i386 {
	pub struct VlibcStatxTime {
	pub:
		sec      i64
		nsec     u32
		reserved i32
	}

	pub struct VlibcStatx {
	pub:
		mask                      u32
		blksize                   u32
		attributes                u64
		nlink                     u32
		uid                       u32
		gid                       u32
		mode                      u16
		spare0                    [1]u16
		ino                       u64
		size                      u64
		blocks                    u64
		attributes_mask           u64
		atime                     VlibcStatxTime
		btime                     VlibcStatxTime
		ctime                     VlibcStatxTime
		mtime                     VlibcStatxTime
		rdev_major                u32
		rdev_minor                u32
		dev_major                 u32
		dev_minor                 u32
		mnt_id                    u64
		dio_mem_align             u32
		dio_offset_align          u32
		subvol                    u64
		atomic_write_unit_min     u32
		atomic_write_unit_max     u32
		atomic_write_segments_max u32
		dio_read_offset_align     u32
		atomic_write_unit_max_opt u32
		spare2                    u32
		spare3                    [8]u64
	}

	// VlibcStat wraps the most useful statx fields.
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

	pub fn vlibc_stat(path string) (VlibcStat, VlibcErrno) {
		// AT_SYMLINK_NOFOLLOW: lstat semantics, the link itself is reported.
		return vlibc_statx_flags(path, 0x100)
	}

	pub fn vlibc_stat_follow(path string) (VlibcStat, VlibcErrno) {
		// flags=0: symlinks are followed, stat semantics.
		return vlibc_statx_flags(path, 0)
	}

	fn vlibc_statx_flags(path string, flags u32) (VlibcStat, VlibcErrno) {
		mut stx := VlibcStatx{}
		e := vlibc_sys_statx(-100, path.str, flags, 0x7ff, &stx)
		if e != .enoerror {
			return VlibcStat{}, e
		}
		mut s := VlibcStat{}
		s.mode = u32(stx.mode)
		s.uid = stx.uid
		s.gid = stx.gid
		s.size = i64(stx.size)
		s.mtime = stx.mtime.sec
		s.ino = stx.ino
		s.nlink = stx.nlink
		s.blksize = stx.blksize
		return s, VlibcErrno.enoerror
	}

	pub fn vlibc_open(path string, flags u64, mode u64) (i64, VlibcErrno) {
		return vlibc_sys_openat(-100, path.str, flags, mode)
	}

	pub fn vlibc_ftruncate(fd i64, len u64) VlibcErrno {
		return vlibc_sys_ftruncate(fd, len)
	}

	pub fn vlibc_truncate(path string, len u64) VlibcErrno {
		// No truncate syscall on arm64, so open + ftruncate like libc does.
		// O_WRONLY|O_TRUNC without O_CREAT, mirroring os.truncate:
		// a missing path fails, matching truncate(2).
		fd, e := vlibc_open(path, 0x201, 0)
		if e != .enoerror {
			return e
		}
		e2 := vlibc_sys_ftruncate(fd, len)
		vlibc_close(fd)
		return e2
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
				// extend logical length to capacity
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
		fd, e := vlibc_open(path, 0x241, 0o644) // O_WRONLY|O_CREAT|O_TRUNC
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
		return vlibc_sys_mkdirat(-100, path.str, mode)
	}

	pub fn vlibc_rmdir(path string) VlibcErrno {
		return vlibc_sys_unlinkat(-100, path.str, 0x200) // AT_REMOVEDIR
	}

	pub fn vlibc_unlink(path string) VlibcErrno {
		return vlibc_sys_unlinkat(-100, path.str, 0)
	}

	pub fn vlibc_rename(oldpath string, newpath string) VlibcErrno {
		return vlibc_sys_renameat(-100, oldpath.str, -100, newpath.str)
	}

	// ---- metadata/link/time ----
	pub fn vlibc_fchmodat(dirfd i64, path string, mode u32, flags int) VlibcErrno {
		return vlibc_sys_fchmodat(dirfd, path.str, mode, flags)
	}

	pub fn vlibc_chmod(path string, mode u32) VlibcErrno {
		return vlibc_sys_fchmodat(-100, path.str, mode, 0)
	}

	pub fn vlibc_faccessat(dirfd i64, path string, mode int, flags int) VlibcErrno {
		return vlibc_sys_faccessat(dirfd, path.str, mode, flags)
	}

	// vlibc_access mirrors access(2): real-uid check (flags=0), symlinks followed.
	pub fn vlibc_access(path string, mode int) VlibcErrno {
		return vlibc_sys_faccessat(-100, path.str, mode, 0)
	}

	pub fn vlibc_fchownat(dirfd i64, path string, owner u32, group u32, flags int) VlibcErrno {
		return vlibc_sys_fchownat(dirfd, path.str, owner, group, flags)
	}

	pub fn vlibc_chown(path string, owner int, group int) VlibcErrno {
		return vlibc_sys_fchownat(-100, path.str, u32(owner), u32(group), 0)
	}

	pub fn vlibc_utimensat(dirfd i64, path string, times &VlibcTimespec, flags int) VlibcErrno {
		return vlibc_sys_utimensat(dirfd, path.str, times, flags)
	}

	// vlibc_utime mirrors os.utime: second-resolution atime/mtime, symlinks followed.
	pub fn vlibc_utime(path string, actime i64, modtime i64) VlibcErrno {
		mut times := [2]VlibcTimespec{}
		times[0] = VlibcTimespec{ sec: actime, nsec: 0 }
		times[1] = VlibcTimespec{ sec: modtime, nsec: 0 }
		return vlibc_sys_utimensat(-100, path.str, &times[0], 0)
	}

	pub fn vlibc_symlinkat(target string, dirfd i64, linkpath string) VlibcErrno {
		return vlibc_sys_symlinkat(target.str, dirfd, linkpath.str)
	}

	pub fn vlibc_symlink(target string, linkpath string) VlibcErrno {
		return vlibc_sys_symlinkat(target.str, -100, linkpath.str)
	}

	pub fn vlibc_readlinkat(dirfd i64, path string, buf &u8, size u64) (i64, VlibcErrno) {
		return vlibc_sys_readlinkat(dirfd, path.str, buf, size)
	}

	// vlibc_readlink mirrors os.readlink: full target, heap string, no libc.
	pub fn vlibc_readlink(path string) (string, VlibcErrno) {
		mut buf := []u8{len: 4096}
		n, e := vlibc_sys_readlinkat(-100, path.str, &u8(buf.data), 4096)
		if e != .enoerror {
			return '', e
		}
		if n < 4096 {
			buf[int(n)] = 0
			mut s := ''
			unsafe {
				s = tos_clone(&u8(buf.data))
			}
			return s, VlibcErrno.enoerror
		}
		mut size := u64(8192)
		for size <= u64(1) << 20 {
			mut big := []u8{len: int(size)}
			m, e2 := vlibc_sys_readlinkat(-100, path.str, &u8(big.data), size)
			if e2 != .enoerror {
				return '', e2
			}
			if u64(m) < size {
				big[int(m)] = 0
				mut s := ''
				unsafe {
					s = tos_clone(&u8(big.data))
				}
				return s, VlibcErrno.enoerror
			}
			size *= 2
		}
		// Target did not fit in 1 MiB; report it as too long.
		return '', VlibcErrno.enametoolong
	}

	pub fn vlibc_linkat(olddir i64, oldpath string, newdir i64, newpath string, flags int) VlibcErrno {
		return vlibc_sys_linkat(olddir, oldpath.str, newdir, newpath.str, flags)
	}

	pub fn vlibc_link(oldpath string, newpath string) VlibcErrno {
		return vlibc_sys_linkat(-100, oldpath.str, -100, newpath.str, 0)
	}

	pub fn vlibc_mknodat(dirfd i64, path string, mode u32, dev u64) VlibcErrno {
		return vlibc_sys_mknodat(dirfd, path.str, mode, dev)
	}

	// vlibc_mkfifo mirrors mkfifo(path, mode): S_IFIFO via mknodat (no SYS_mkfifo on Linux).
	pub fn vlibc_mkfifo(path string, mode u32) VlibcErrno {
		return vlibc_sys_mknodat(-100, path.str, u32(0o10000) | mode, 0)
	}

	pub fn vlibc_getcwd() (string, VlibcErrno) {
		mut buf := []u8{len: 4096}
		n, e := vlibc_sys_getcwd(&u8(buf.data), 4096)
		if e != .enoerror {
			return '', e
		}
		mut s := string{}
		unsafe {
			s = tos_clone(&u8(buf.data))
		}
		return s, VlibcErrno.enoerror
	}

	pub fn vlibc_chdir(path string) VlibcErrno {
		return vlibc_sys_chdir(path.str)
	}

	// VlibcDirent is a minimal view of getdents64 entry.
	pub struct VlibcDirent {
	pub:
		ino    u64
		off    i64
		reclen u16
		typ    u8
		name   string
	}

	pub fn vlibc_list_dir(path string) ([]string, VlibcErrno) {
		fd, e := vlibc_open(path, 0x10000 | 0, 0) // O_DIRECTORY
		if e != .enoerror {
			// retry without O_DIRECTORY (some kernels accept plain open)
			fd2, e2 := vlibc_open(path, 0, 0)
			if e2 != .enoerror {
				return []string{}, e2
			}
			fd = fd2
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
				// linux_dirent64: u64 d_ino; i64 d_off; u16 d_reclen; u8 d_type; char d_name[]
				ino := *(&u64(&u8(buf.data) + off))
				_ = ino
				reclen := *(&u16(&u8(buf.data) + off + 16))
				typ := *(&u8(&u8(buf.data) + off + 18))
				_ = typ
				nameptr := &u8(&u8(buf.data) + off + 19)
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
}
