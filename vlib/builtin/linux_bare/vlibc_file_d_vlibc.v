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

	// vlibc_stat returns file metadata, following symbolic links.
	pub fn vlibc_stat(path string) (VlibcStat, VlibcErrno) {
		mut stx := VlibcStatx{}
		e := vlibc_sys_statx(-100, path.str, 0, 0x7ff, &stx)
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

	// vlibc_open opens a path with native flags and permissions subject to the process umask.
	pub fn vlibc_open(path string, flags u64, mode u64) (i64, VlibcErrno) {
		return vlibc_sys_openat(-100, path.str, flags, mode)
	}

	// vlibc_close closes a file descriptor.
	pub fn vlibc_close(fd i64) VlibcErrno {
		return vlibc_sys_close(fd)
	}

	// vlibc_read_all reads to EOF, retrying interrupted reads and retaining data on error.
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
				if e == .eintr {
					continue
				}
				if total == 0 {
					unsafe { buf.free() }
					return []u8{}, e
				}
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

	// vlibc_read_file reads a file and reports read errors before any close error.
	pub fn vlibc_read_file(path string) ([]u8, VlibcErrno) {
		fd, e := vlibc_open(path, 0, 0)
		if e != .enoerror {
			return []u8{}, e
		}
		data, e2 := vlibc_read_all(fd)
		close_error := vlibc_close(fd)
		return data, if e2 != .enoerror { e2 } else { close_error }
	}

	// vlibc_write_file writes all bytes, retries interruptions, and reports close failures.
	pub fn vlibc_write_file(path string, data []u8) VlibcErrno {
		fd, e := vlibc_open(path, 0x241, 0o644) // O_WRONLY|O_CREAT|O_TRUNC
		if e != .enoerror {
			return e
		}
		mut off := u64(0)
		for off < u64(data.len) {
			n, e2 := vlibc_sys_write(fd, &u8(data.data) + off, u64(data.len) - off)
			if e2 != .enoerror {
				if e2 == .eintr {
					continue
				}
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

	// vlibc_mkdir creates a directory with permissions subject to the process umask.
	pub fn vlibc_mkdir(path string, mode u32) VlibcErrno {
		return vlibc_sys_mkdirat(-100, path.str, mode)
	}

	// vlibc_rmdir removes an empty directory.
	pub fn vlibc_rmdir(path string) VlibcErrno {
		return vlibc_sys_unlinkat(-100, path.str, 0x200) // AT_REMOVEDIR
	}

	// vlibc_unlink removes a filesystem entry.
	pub fn vlibc_unlink(path string) VlibcErrno {
		return vlibc_sys_unlinkat(-100, path.str, 0)
	}

	// vlibc_rename renames or moves a filesystem entry.
	pub fn vlibc_rename(oldpath string, newpath string) VlibcErrno {
		return vlibc_sys_renameat(-100, oldpath.str, -100, newpath.str)
	}

	// vlibc_getcwd returns the current working directory and any error.
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

	// vlibc_chdir changes the current working directory.
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

	// vlibc_list_dir lists directory entries using the architecture-specific O_DIRECTORY flag.
	pub fn vlibc_list_dir(path string) ([]string, VlibcErrno) {
		directory_flag := $if arm64 { 0x4000 } $else { 0x10000 }
		fd, e := vlibc_open(path, directory_flag, 0) // O_DIRECTORY
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
