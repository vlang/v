module builtin

$if (freestanding || syscalls) && !i386 && !windows && !openbsd {
	// bf_set_last_errno records a normalized syscall error for os.last_error().
	@[inline]
	pub fn bf_set_last_errno(code int) {
		bf_last_errno_cell = code
	}

	// bf_read_ready reads available bytes without waiting for input. It returns
	// (count, errno), with zero for EOF and normalized errno 11 for unavailable
	// input. The caller supplies count writable bytes and exclusively controls
	// the descriptor and its shared open-file description for this call. Original
	// status flags are restored before returning, including after read errors.
	// Interrupted operations retry; restoration failure takes precedence.
	pub fn bf_read_ready(fd int, buf &u8, count int) (int, int) {
		if count < 0 {
			return -1, 22
		}
		flags, flags_error := bf_descriptor_control_retry(fd, 3, 0) // F_GETFL
		if flags_error != 0 {
			return -1, flags_error
		}
		nonblock := $if linux { 0x800 } $else $if solaris { 0x80 } $else { 0x4 }
		changed := (flags & nonblock) == 0
		if changed {
			_, set_error := bf_descriptor_control_retry(fd, 4, flags | nonblock) // F_SETFL
			if set_error != 0 {
				return -1, set_error
			}
		}
		bytes, read_error := bf_read(fd, buf, count)
		if changed {
			_, restore_error := bf_descriptor_control_retry(fd, 4, flags)
			if restore_error != 0 {
				return -1, restore_error
			}
		}
		return bytes, read_error
	}

	@[no_profile; no_trace]
	fn bf_descriptor_control_retry(fd int, command int, value int) (int, int) {
		for {
			result, err := bf_descriptor_control(fd, command, value)
			if err != 4 { // EINTR
				return result, err
			}
		}
		return -1, 4
	}

	// bf_tai_now returns the kernel's TAI clock as seconds and nanoseconds since
	// the Unix epoch, followed by a normalized errno. It never substitutes a UTC
	// clock. Platforms without this clock return errno 38 (ENOSYS); callers may
	// implement their own documented UTC-to-TAI conversion policy.
	pub fn bf_tai_now() (i64, i64, int) {
		$if linux {
			mut stamp := FSysTaiTimespec{}
			err := bf_clock_tai(&stamp)
			return stamp.seconds, stamp.nanoseconds, err
		} $else {
			return 0, 0, 38
		}
	}

	// The supported 64-bit Linux ABIs use two signed 64-bit timespec fields.
	struct FSysTaiTimespec {
	mut:
		seconds     i64
		nanoseconds i64
	}
}
