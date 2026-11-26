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
	const fsys_darwin_path_max = 1024

	@[manualfree]
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
		defer { unsafe { chunk.free() } }
		for {
			nr, re := bf_read(fd, chunk.data, chunk.len)
			if re == 4 { // EINTR
				continue
			}
			if re != 0 {
				unsafe { data.free() }
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
		// As with create(), the kernel applies the process umask to new files.
		fd, e := bf_openat(path.str, u64(o_wronly | o_create | o_trunc), u64(0o666))
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

	// fsys_getenv reads the live V environment on the supported freestanding C
	// targets. Other Linux backends retain their /proc startup snapshot because
	// the experimental native backend lowers argv pointer walks differently.
	fn fsys_getenv(key string) (string, bool) {
		$if freestanding && (linux || (macos && amd64)) && !native && !i386 {
			return fsys_environment_get(key)
		}
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
			if eq > i && (eq - i) == key.len {
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
	@[manualfree]
	fn fsys_proc_environ_map() map[string]string {
		mut res := map[string]string{}
		content := fsys_read_file('/proc/self/environ') or { return res }
		text := content.bytestr()
		defer {
			unsafe {
				text.free()
				content.free()
			}
		}
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
			if eq > i {
				key := text[i..eq].clone()
				val := text[eq + 1..n].clone()
				res[key] = val
				unsafe { key.free() }
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

	// fsys_real_path preserves the public real_path failure contract: the caller
	// owns a copy of the original spelling when any component cannot be resolved.
	@[manualfree]
	fn fsys_real_path(fpath string) string {
		return fsys_resolve_path(fpath) or { fpath.clone() }
	}

	// Resolve components in filesystem order. In particular, a symlink's target
	// must be read before processing a following `..`; abs_path is only lexical.
	// Each kernel path is NUL terminated in `path`. `pending` owns the unresolved
	// suffix, so replacing it during symlink expansion cannot borrow freed data.
	@[manualfree]
	fn fsys_resolve_path(fpath string) ?string {
		// Match the C-string API used by hosted real_path, including embedded NUL.
		mut input_len := fpath.len
		for i in 0 .. fpath.len {
			if fpath[i] == 0 {
				input_len = i
				break
			}
		}
		if input_len == 0 {
			return none
		}
		$if macos {
			// Darwin's realpath bounds the unresolved spelling, before folding dots.
			initial_separator := if fpath[0] == `/` { 1 } else { 0 }
			if input_len - initial_separator >= fsys_darwin_path_max { return none }
		}
		mut path := [max_path_buffer_size]u8{}
		path[0] = `/`
		mut length := 1
		if fpath[0] != `/` {
			cwd := getwd()
			defer { unsafe { cwd.free() } }
			if cwd.len == 0 || cwd[0] != `/` || cwd.len >= path.len {
				return none
			}
			// The checked length fits the fixed buffer, including its terminator.
			unsafe { vmemcpy(&path[0], cwd.str, cwd.len) }
			length = cwd.len
		}
		mut pending := []u8{len: input_len}
		unsafe { vmemcpy(pending.data, fpath.str, input_len) }
		defer { unsafe { pending.free() } }
		mut target := [max_path_buffer_size]u8{}
		mut cursor := 0
		mut links := 0
		// Hosted resolution permits 40 traversals on Linux and 33 on Darwin.
		// A limit also bounds cycles whose spellings keep growing (link -> dir/link).
		link_limit := $if macos { 33 } $else { 40 }
		for cursor < pending.len {
			if pending[cursor] == `/` {
				cursor++
				continue
			}
			start := cursor
			for cursor < pending.len && pending[cursor] != `/` {
				cursor++
			}
			component_len := cursor - start
			parent_len := length
			is_dot := pending[start] == `.` && (component_len == 1
				|| (component_len == 2 && pending[start + 1] == `.`))
			$if macos {
				// Darwin validates named entries, then folds dots even after a file.
				if is_dot {
					if component_len == 2 {
						for length > 1 && path[length - 1] != `/` { length-- }
						if length > 1 { length-- }
					}
					continue
				}
			}
			separator := if length == 1 { 0 } else { 1 }
			if component_len >= path.len - length - separator {
				return none
			}
			if separator != 0 {
				path[length] = `/`
				length++
			}
			// Both source and destination ranges are bounded above. A view of this
			// buffer is borrowed only for the synchronous stat/readlink syscalls.
			unsafe { vmemcpy(&path[length], &u8(pending.data) + start, component_len) }
			length += component_len
			path[length] = 0
			candidate := unsafe { tos(&path[0], length) }
			mut st := FSysStat{}
			for {
				current, code := bf_stat(candidate, false)
				if code == 4 { continue } // EINTR
				if code != 0 { return none }
				st = current
				break
			}
			if st.mode & s_ifmt == s_iflnk {
				links++
				if links > link_limit { return none }
				mut count := 0
				for {
					n, code := bf_readlink(candidate, &target[0], target.len)
					if code == 4 { continue }
					if code != 0 || n <= 0 || n >= target.len { return none }
					count = n
					break
				}
				remaining := pending.len - cursor
				if count > max_int - remaining { return none }
				$if macos {
					if count + remaining >= fsys_darwin_path_max { return none }
				}
				mut next := []u8{len: count + remaining}
				unsafe {
					vmemcpy(next.data, &target[0], count)
					vmemcpy(&u8(next.data) + count, &u8(pending.data) + cursor, remaining)
					pending.free()
					// Transfer ownership; next is not retained or freed separately.
					pending = next
				}
				cursor = 0
				length = if target[0] == `/` { 1 } else { parent_len }
				continue
			}
			$if !macos {
				if cursor < pending.len && st.mode & s_ifmt != s_ifdir {
					return none
				}
				// Stat dots before removing them, so a missing, non-directory or
				// unsearchable predecessor cannot disappear through normalization.
				if is_dot {
					length = parent_len
					if component_len == 2 {
						for length > 1 && path[length - 1] != `/` { length-- }
						if length > 1 { length-- }
					}
				}
			}
		}
		// This is the sole escaping allocation. Neither stack storage nor the
		// pending buffer is retained by the returned string.
		return unsafe { tos(&path[0], length).clone() }
	}

	// fsys_ioctl issues `request` on `fd` (FIONREAD) through the bf surface.
	fn fsys_ioctl(fd int, request u64, arg voidptr) int {
		return bf_ioctl(fd, request, arg)
	}
}
