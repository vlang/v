module time

fn sys_mono_now_darwin() u64 {
	return 0
}

// dummy to compile with all compilers
pub fn darwin_now() Time {
	return Time{}
}

// linux_now returns the local time with high precision for most os:es
// this should be implemented with native system calls eventually
// but for now a bit tweaky. It uses the realtime clock to get
// the nano seconds part and normal local time to get correct local time
// if the time has shifted on a second level between calls it uses
// zero as microsecond. Not perfect but better that unix time only
fn linux_now() Time {

	// get the high precision time as UTC realtime clock
	// and use the nanoseconds part
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)

	t := C.time(0)
	tm := C.localtime(&t)

	// if the second part (very rare) is different
	// microseconds is set to zero since it passed the second
	// also avoid divide by zero if nsec is zero
	if int(t) != ts.tv_sec || ts.tv_nsec == 0 {
		return convert_ctime(tm, 0)
	}

	return convert_ctime(tm, int(ts.tv_nsec/1000))
}