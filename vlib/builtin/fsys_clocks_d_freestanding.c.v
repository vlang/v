module builtin

$if (linux || (macos && amd64)) && !native && !i386 {
	struct FSysClockStamp {
	mut:
		seconds     i64
		nanoseconds i64
	}

	// bf_realtime_now returns Unix seconds, nanoseconds, and a positive BF error.
	// It reads the kernel calendar clock without changing either errno cell.
	@[no_profile; no_trace]
	pub fn bf_realtime_now() (i64, i64, int) {
		$if macos {
			seconds, nanos, code := mac_clock_realtime_native()
			return seconds, nanos, bf_errno_code(unsafe { VlibcErrno(code) })
		} $else {
			mut stamp := FSysClockStamp{}
			code := i64(sys_call2(bf_nr_clock_gettime, 0, u64(&stamp)))
			return stamp.seconds, stamp.nanoseconds, if code < 0 { int(-code) } else { 0 }
		}
	}

	// bf_monotonic_now returns nanoseconds on an awake-time monotonic clock and
	// a positive BF error. Its unspecified origin is suitable for elapsed times;
	// suspended time is excluded. Neither errno cell is changed.
	@[no_profile; no_trace]
	pub fn bf_monotonic_now() (u64, int) {
		$if macos {
			nanos, code := mac_clock_uptime_native()
			return nanos, bf_errno_code(unsafe { VlibcErrno(code) })
		} $else {
			mut stamp := FSysClockStamp{}
			code := i64(sys_call2(bf_nr_clock_gettime, 1, u64(&stamp)))
			if code < 0 { return 0, int(-code) }
			return u64(stamp.seconds) * 1_000_000_000 + u64(stamp.nanoseconds), 0
		}
	}

	// bf_sleep_ns sleeps for a nonnegative relative duration in nanoseconds,
	// retaining the remaining interval across signals. It returns a positive BF
	// error; zero duration succeeds immediately and neither errno cell is changed.
	@[no_profile; no_trace]
	pub fn bf_sleep_ns(duration i64) int {
		if duration < 0 { return 22 }
		if duration == 0 { return 0 }
		return bf_sleep_stamp(FSysClockStamp{
			seconds:     duration / 1_000_000_000
			nanoseconds: duration % 1_000_000_000
		})
	}

	// Keep the full timespec range for inputs measured in larger units, without
	// narrowing them through an i64 nanosecond duration.
	@[no_profile; no_trace]
	fn bf_sleep_stamp(initial FSysClockStamp) int {
		mut request := initial
		mut remaining := FSysClockStamp{}
		for {
			$if macos {
				code := mac_clock_sleep_native(request, mut remaining)
				if code == 4 {
					request = remaining
					continue
				}
				return bf_errno_code(unsafe { VlibcErrno(code) })
			} $else {
				code := i64(sys_call2(bf_nr_nanosleep, u64(&request), u64(&remaining)))
				if code == -4 {
					request = remaining
					continue
				}
				return if code < 0 { int(-code) } else { 0 }
			}
		}
		return 0
	}
}
