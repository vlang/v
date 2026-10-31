module builtin

$if !i386 {
	pub struct VlibcTimespec {
	pub:
		sec  i64
		nsec i64
	}

	pub struct VlibcUtsname {
	pub:
		sysname    [65]u8
		nodename   [65]u8
		release    [65]u8
		version    [65]u8
		machine    [65]u8
		domainname [65]u8
	}

	// vlibc_now_realtime_ns returns the wall-clock timestamp in nanoseconds since the Unix epoch.
	pub fn vlibc_now_realtime_ns() i64 {
		mut ts := VlibcTimespec{}
		vlibc_sys_clock_gettime(0, &ts)
		return ts.sec * 1_000_000_000 + ts.nsec
	}

	// vlibc_now_monotonic_ns returns the monotonic clock value in nanoseconds.
	pub fn vlibc_now_monotonic_ns() i64 {
		mut ts := VlibcTimespec{}
		vlibc_sys_clock_gettime(1, &ts)
		return ts.sec * 1_000_000_000 + ts.nsec
	}

	// vlibc_sleep_ns requests a sleep duration in nanoseconds; signals may interrupt it.
	@[unsafe]
	pub fn vlibc_sleep_ns(nsec u64) {
		mut ts := VlibcTimespec{ sec: i64(nsec / 1_000_000_000), nsec: i64(nsec % 1_000_000_000) }
		vlibc_sys_nanosleep(&ts, unsafe { nil })
	}

	// vlibc_uname returns operating-system identification fields.
	pub fn vlibc_uname() VlibcUtsname {
		mut u := VlibcUtsname{}
		vlibc_sys_uname(&u)
		return u
	}
}
