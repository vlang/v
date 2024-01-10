module time

// solaris_now returns the local time with high precision for most os:es
// this should be implemented properly with support for leap seconds.
// It uses the realtime clock to get and converts it to local time
fn solaris_now() Time {
	// get the high precision time as UTC realtime clock
	// and use the nanoseconds part
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	loc_tm := C.tm{}
	C.localtime_r(voidptr(&ts.tv_sec), &loc_tm)
	return convert_ctime(loc_tm, int(ts.tv_nsec))
}

fn solaris_utc() Time {
	// get the high precision time as UTC realtime clock
	// and use the nanoseconds part
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	return unix_nanosecond(i64(ts.tv_sec), int(ts.tv_nsec))
}

// dummy to compile with all compilers
fn darwin_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn darwin_utc() Time {
	return Time{}
}
