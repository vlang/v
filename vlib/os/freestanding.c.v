module os

// freestanding.c.v supplies the os module internals for `-freestanding` and `-syscalls` builds.
// It no longer carries syscall tables of its own: builtin's uniform
// freestanding surface (`bf_*`, shaping every supported platform's ABI into one
// set of signatures, with errno values normalized to the Linux numbers) is
// what the routes below call. The shapes follow Zig's std/os handling:
// per-OS syscall layers behind a single portable surface, interrupted syscalls
// retried in the wrappers, and sizesless /proc and /sys files read by a loop
// instead of a size-based read.

// fsys_read_file reads the whole file at `path` with an openat + read loop, so
// it also handles sizeless /proc and /sys files that a size-based read would
// truncate.
$if freestanding || syscalls {
	fn fsys_read_file(path string) ![]u8 {
		fd, e := bf_openat(path.str, u64(o_rdonly), u64(0))
		if e != 0 {
			return error_with_code('read_file failed: errno ${e}', e)
		}
		defer {
			bf_close(fd)
		}
		mut data := []u8{cap: 4096}
		mut chunk := []u8{len: 4096}
		for {
			nr, re := bf_read(fd, chunk.data, chunk.len)
			if re == 4 { // EINTR
				continue
			}
			if re != 0 {
				return error_with_code('read_file failed: errno ${re}', re)
			}
			if nr == 0 {
				break
			}
			data << chunk[0..nr]
		}
		return data
	}

	// fsys_write_file creates or overwrites the file at `path` with `data`, using
	// partial write and EINTR retries (an interrupted write never reaches callers
	// as an error).
	fn fsys_write_file(path string, data []u8) ! {
		fd, e := bf_openat(path.str, u64(o_wronly | o_create | o_trunc), u64(0o644))
		if e != 0 {
			return error_with_code('write_file failed: errno ${e}', e)
		}
		defer {
			bf_close(fd)
		}
		mut remaining := data.len
		mut offset := 0
		for remaining > 0 {
			nw, we := bf_write(fd, &u8(voidptr(uint(data.data) + uint(offset))), remaining)
			if we == 4 { // EINTR
				continue
			}
			if we != 0 {
				return error_with_code('write_file failed: errno ${we}', we)
			}
			if nw == 0 {
				return error_with_code('write_file failed: 0 bytes written', 0)
			}
			remaining -= nw
			offset += nw
		}
	}

	// fsys_ls lists the entries of `path` with the directory-read syscall of the
	// platform, skipping `.` and `..` the way the libc backend of os.ls does,
	// unsorted.
	fn fsys_ls(path string) ![]string {
		fd, e := bf_openat_dir(path.str)
		if e != 0 {
			return error_with_code('ls() couldnt open dir "${path}": errno ${e}', e)
		}
		defer {
			bf_close(fd)
		}
		bf_entries, e2 := bf_readdir(fd)
		if e2 != 0 {
			return error_with_code('ls() failed: errno ${e2}', e2)
		}
		return bf_entries
	}

	// fs_sleep_ms sleeps for `ms` milliseconds; an interrupted sleep is never
	// leaked to callers.
	fn fs_sleep_ms(ms i64) int {
		return bf_nanosleep(ms)
	}

	// fs_truncate shrinks or extends the file at `path` to `len` bytes.
	fn fs_truncate(path string, len u64) int {
		return bf_truncate(path, len)
	}

	// fsys_getenv returns the value and whether the variable named `key` is
	// present in the process environment. Linux rides the kernel's
	// /proc/self/environ snapshot (a NUL-separated `KEY=VALUE` block) through
	// the plain file-read route, which the C and native backends lower with
	// the same syscalls — no pointer-chain walk of the envp block, which the
	// native backend lowers differently than the C backend. The other
	// platforms keep the bf walk.
	fn fsys_getenv(key string) (string, bool) {
		$if linux {
			return fsys_proc_env_find(key)
		}
		return bf_getenv(key)
	}

	// fsys_proc_env_find scans the `/proc/self/environ` block for `key`.
	fn fsys_proc_env_find(key string) (string, bool) {
		content := fsys_read_file('/proc/self/environ') or { return '', false }
		return fscan_env_block(content.bytestr(), key)
	}

	// fscan_env_block parses a NUL-separated `KEY=VALUE` block for `key`.
	fn fscan_env_block(content string, key string) (string, bool) {
		mut i := 0
		for i < content.len {
			mut eq := -1
			mut n := i
			for n < content.len && content[n] != 0 {
				if eq < 0 && content[n] == `=` {
					eq = n
				}
				n++
			}
			if eq >= 0 && (eq - i) == key.len && n > (eq + 1) {
				mut same := true
				for k in 0 .. key.len {
					if content[i + k] != key[k] {
						same = false
						break
					}
				}
				if same {
					val := content[eq + 1..n].clone()
					return val, true
				}
			}
			i = n + 1
		}
		return '', false
	}

	// fsys_proc_environ_map returns every `KEY=VALUE` entry of
	// /proc/self/environ as a map.
	fn fsys_proc_environ_map() map[string]string {
		mut res := map[string]string{}
		content := fsys_read_file('/proc/self/environ') or { return res }
		text := content.bytestr()
		mut i := 0
		for i < text.len {
			mut eq := -1
			mut n := i
			for n < text.len && text[n] != 0 {
				if eq < 0 && text[n] == `=` {
					eq = n
				}
				n++
			}
			if eq >= 0 && eq > i && n > (eq + 1) {
				key := text[i..eq].clone()
				val := text[eq + 1..n].clone()
				res[key] = val
			}
			i = n + 1
		}
		return res
	}

	// fsys_errno_message returns a short human readable description for errno
	// `code`, normalized to the Linux numbers by the bf layer (there is no
	// strerror(3) in a libc-less runtime; Zig carries the same descriptions as doc
	// comments on its `E` enum variants).
	fn fsys_errno_message(code int) string {
		match code {
			1 { return 'Operation not permitted' }
			2 { return 'No such file or directory' }
			3 { return 'No such process' }
			4 { return 'Interrupted system call' }
			5 { return 'Input/output error' }
			6 { return 'No such device or address' }
			9 { return 'Bad file descriptor' }
			10 { return 'No child processes' }
			11 { return 'Resource temporarily unavailable' }
			12 { return 'Cannot allocate memory' }
			13 { return 'Permission denied' }
			14 { return 'Bad address' }
			16 { return 'Device or resource busy' }
			17 { return 'File exists' }
			18 { return 'Invalid cross-device link' }
			19 { return 'No such device' }
			20 { return 'Not a directory' }
			21 { return 'Is a directory' }
			22 { return 'Invalid argument' }
			23 { return 'Too many open files in system' }
			24 { return 'Too many open files' }
			26 { return 'Text file busy' }
			27 { return 'File too large' }
			28 { return 'No space left on device' }
			29 { return 'Illegal seek' }
			30 { return 'Read-only file system' }
			31 { return 'Too many links' }
			32 { return 'Broken pipe' }
			36 { return 'File name too long' }
			38 { return 'Function not implemented' }
			39 { return 'Directory not empty' }
			40 { return 'Too many levels of symbolic links' }
			61 { return 'No data available' }
			75 { return 'Value too large for defined data type' }
			104 { return 'Connection reset by peer' }
			110 { return 'Connection timed out' }
			111 { return 'Connection refused' }
			122 { return 'Disk quota exceeded' }
			else { return 'Unknown error ${code}' }
		}
	}

	// fsys_stat_from_norm converts builtin's normalized freestanding stat (bf_*)
	// into the platform agnostic os.Stat. Device ids arrive as (major << 8) |
	// minor encodings from the bf layer.
	fn fsys_stat_from_norm(stx FSysStat) Stat {
		return Stat{
			dev:   stx.dev
			inode: stx.ino
			nlink: stx.nlink
			mode:  stx.mode
			uid:   stx.uid
			gid:   stx.gid
			rdev:  stx.rdev
			size:  stx.size
			atime: stx.atime
			mtime: stx.mtime
			ctime: stx.ctime
		}
	}

	// fsys_real_path makes `fpath` absolute and resolves `.` and `..` lexically.
	// It does not chase symlinks: there is no realpath syscall, and Zig's
	// O_PATH-open + /proc/self/fd read depends on procfs, which is not portable.
	fn fsys_real_path(fpath string) string {
		if fpath.len == 0 {
			return '.'
		}
		mut comps := []string{cap: 24}
		mut absolute := false
		for p in fpath.split('/') {
			if p == '' || p == '.' {
				// a leading empty element marks an absolute path
				if p == '' && comps.len == 0 {
					absolute = true
				}
				continue
			}
			if p == '..' {
				if comps.len > 0 {
					comps.pop()
				}
				continue
			}
			comps << p
		}
		if absolute {
			if comps.len == 0 {
				return '/'
			}
			return '/' + comps.join('/')
		}
		wd := getwd()
		if comps.len == 0 {
			return wd
		}
		return wd + '/' + comps.join('/')
	}

	// fsys_ioctl issues `request` on `fd` (FIONREAD) through the bf surface.
	fn fsys_ioctl(fd int, request u64, arg voidptr) int {
		return bf_ioctl(fd, request, arg)
	}
}
