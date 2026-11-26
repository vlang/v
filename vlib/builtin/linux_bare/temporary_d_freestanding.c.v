module builtin

// C callers own the writable NUL-terminated template storage accessed below.

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	@[export: 'v_native_temporary'; markused]
	fn bare_native_temporary(pattern uint, suffix i32, flags i32, directory i32) i32 {
		if pattern == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		length := bare_string_chr_nul(pattern, 0) - pattern
		if suffix < 0 || length < uint(suffix) + 6 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		start := pattern + length - uint(suffix) - 6
		for i in 0 .. 6 {
			if unsafe { *(&u8(start + uint(i))) } != `X` {
				bf_native_errno_cell = i32(C.EINVAL)
				return -1
			}
		}
		alphabet := c'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
		for _ in 0 .. 238328 {
			mut written := 0
			for written < 6 {
				mut random := [16]u8{}
				count := i64(sys_call3(u64(C.SYS_getrandom), u64(&random[0]), 16, 0))
				if count == -i64(C.EINTR) { continue }
				if count <= 0 {
					if count == 0 {
						bf_native_errno_cell = i32(C.EIO)
					} else {
						bare_native_result(u64(count))
					}
					return -1
				}
				for i in 0 .. int(count) {
					// Reject the biased tail rather than reducing every random byte modulo 62.
					if random[i] >= 248 { continue }
					unsafe { *(&u8(start + uint(written))) = u8(alphabet[int(random[i] % 62)]) }
					written++
					if written == 6 { break }
				}
			}
			result := if directory != 0 {
				bare_native_mkdirat(-100, pattern, 0o700)
			} else {
				bare_native_openat(-100, pattern, (flags & ~(i32(C.O_ACCMODE) | i32(C.O_CREAT) | i32(C.O_EXCL))) | i32(C.O_RDWR) | i32(C.O_CREAT) | i32(C.O_EXCL), 0o600)
			}
			if result >= 0 || bf_native_errno_cell != i32(C.EEXIST) { return result }
		}
		bf_native_errno_cell = i32(C.EEXIST)
		return -1
	}
	@[export: 'v_native_mkdtemp'; markused]
	fn bare_native_mkdtemp(pattern uint) uint {
		return if bare_native_temporary(pattern, 0, 0, 1) == 0 { pattern } else { uint(0) }
	}
}
