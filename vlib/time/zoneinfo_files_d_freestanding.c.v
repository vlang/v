@[has_globals]
module time

$if (linux || (macos && amd64)) && !native && !i386 {
	import sync

	const zoneinfo_vroot_zip = @VEXEROOT + '/vlib/time/tzdata/zoneinfo.zip'

	__global zoneinfo_loaders_mutex sync.Mutex

	fn zoneinfo_loaders_lock() {
		zoneinfo_loaders_mutex.lock()
	}

	fn zoneinfo_loaders_unlock() {
		zoneinfo_loaders_mutex.unlock()
	}

	fn zoneinfo_getenv(name string) ?string {
		value, found := bf_getenv(name)
		if !found {
			return none
		}
		return value
	}

	fn zoneinfo_is_abs_path(path string) bool {
		return path.len > 0 && path[0] == `/`
	}

	fn zoneinfo_join(dir string, name string) string {
		return dir.trim_right('/') + '/' + name
	}

	fn zoneinfo_exists(path string) bool {
		_, code := bf_stat(path, true)
		return code == 0
	}

	fn zoneinfo_is_dir(path string) bool {
		info, code := bf_stat(path + '/.', true)
		return code == 0 && info.mode & 0o170000 == 0o040000
	}

	fn zoneinfo_is_file(path string) bool {
		info, code := bf_stat(path, true)
		return code == 0 && info.mode & 0o170000 == 0o100000
	}

	fn zoneinfo_readlink(path string) ?string {
		mut buffer := []u8{len: 256}
		for {
			// The synchronous syscall finishes before this buffer can be resized.
			count, code := bf_readlink(path, unsafe { &buffer[0] }, buffer.len)
			if code != 0 || count <= 0 {
				return none
			}
			if count < buffer.len {
				return buffer[..count].bytestr()
			}
			buffer = []u8{len: buffer.len * 2}
		}
	}

	fn zoneinfo_read_file(path string) ![]u8 {
		if !zoneinfo_is_file(path) {
			return error('time: "${path}" is not a regular file')
		}
		// BF uses Linux-normalized flags on both platforms: O_CLOEXEC | O_NONBLOCK.
		// Nonblocking open also prevents a concurrent replacement with a FIFO
		// from hanging a timezone lookup. Regular-file reads remain synchronous.
		fd, opened := bf_openat(path.str, u64(0o2004000), 0)
		if opened != 0 {
			return error('time: failed to open "${path}" (${opened})')
		}
		defer { bf_close(fd) }
		mut data := []u8{}
		mut buffer := [4096]u8{}
		for {
			count, code := bf_read(fd, &buffer[0], buffer.len)
			if code != 0 {
				return error('time: failed to read "${path}" (${code})')
			}
			if count == 0 {
				break
			}
			unsafe { data.push_many(&buffer[0], count) }
		}
		return data
	}
}
