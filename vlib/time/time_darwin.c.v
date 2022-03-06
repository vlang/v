module time

#include <mach/mach_time.h>

const (
	// start_time is needed on Darwin and Windows because of potential overflows
	start_time = C.mach_absolute_time()
	time_base  = init_time_base()
)

[typedef]
struct C.mach_timebase_info_data_t {
	numer u32
	denom u32
}

fn C.mach_absolute_time() u64

fn C.mach_timebase_info(&C.mach_timebase_info_data_t)

fn C.clock_gettime_nsec_np(int) u64

struct InternalTimeBase {
	numer u32 = 1
	denom u32 = 1
}

pub struct C.timeval {
	tv_sec  u64
	tv_usec u64
}

fn init_time_base() C.mach_timebase_info_data_t {
	tb := C.mach_timebase_info_data_t{}
	C.mach_timebase_info(&tb)
	return C.mach_timebase_info_data_t{
		numer: tb.numer
		denom: tb.denom
	}
}

fn sys_mono_now_darwin() u64 {
	tm := C.mach_absolute_time()
	if time.time_base.denom == 0 {
		C.mach_timebase_info(&time.time_base)
	}
	return (tm - time.start_time) * time.time_base.numer / time.time_base.denom
}

// Note: vpc_now_darwin is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
[inline]
fn vpc_now_darwin() u64 {
	tm := C.mach_absolute_time()
	if time.time_base.denom == 0 {
		C.mach_timebase_info(&time.time_base)
	}
	return (tm - time.start_time) * time.time_base.numer / time.time_base.denom
}

// darwin_now returns a better precision current time for Darwin based operating system
// this should be implemented with native system calls eventually
// but for now a bit tweaky. It uses the deprecated  gettimeofday clock to get
// the microseconds seconds part and converts to local time
fn darwin_now() Time {
	// get the high precision time as UTC clock
	tv := C.timeval{}
	C.gettimeofday(&tv, 0)
	loc_tm := C.tm{}
	asec := voidptr(&tv.tv_sec)
	C.localtime_r(asec, &loc_tm)
	return convert_ctime(loc_tm, int(tv.tv_usec))
}

// darwin_utc returns a better precision current time for Darwin based operating system
// this should be implemented with native system calls eventually
// but for now a bit tweaky. It uses the deprecated  gettimeofday clock to get
// the microseconds seconds part and normal local time to get correct local time
fn darwin_utc() Time {
	// get the high precision time as UTC clock
	tv := C.timeval{}
	C.gettimeofday(&tv, 0)
	return unix2(i64(tv.tv_sec), int(tv.tv_usec))
}

// dummy to compile with all compilers
pub fn solaris_now() Time {
	return Time{}
}

// dummy to compile with all compilers
pub fn solaris_utc() Time {
	return Time{}
}
