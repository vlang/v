module time

// The functions below exist so that the `$if macos`/`$if solaris` branches in the
// shared time code still link on Linux, where those branches are dropped at compile
// time. The portable `-os cross` snapshot (vc/v.c) is the one build where that is not
// true: it is generated on Linux, so it bakes this file, and then it is compiled on
// macOS to bootstrap v1, where the C preprocessor *does* take the `__APPLE__` branches.
// Returning a zero time there left the compiler with a clock that never advances, so
// answer with the portable POSIX implementation instead of a dummy value.

// sys_mono_now_darwin returns a monotonically increasing time, in nanoseconds.
fn sys_mono_now_darwin() u64 {
	ts := C.timespec{}
	C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
	return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
}

// darwin_now returns the current local time.
fn darwin_now() Time {
	return linux_now()
}

// solaris_now returns the current local time.
fn solaris_now() Time {
	return linux_now()
}

// darwin_utc returns the current UTC time.
fn darwin_utc() Time {
	return linux_utc()
}

// solaris_utc returns the current UTC time.
fn solaris_utc() Time {
	return linux_utc()
}
