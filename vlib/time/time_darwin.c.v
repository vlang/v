module time

#include <mach/mach_time.h>

// start_time is needed on Darwin and Windows because of potential overflows
const start_time = C.mach_absolute_time()

const time_base = init_time_base()

@[typedef]
pub struct C.mach_timebase_info_data_t {
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
	if time_base.denom == 0 {
		unsafe {
			C.mach_timebase_info(&time_base)
		}
	}
	return (tm - start_time) * time_base.numer / time_base.denom
}

// Note: vpc_now_darwin is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
@[inline]
fn vpc_now_darwin() u64 {
	tm := C.mach_absolute_time()
	if time_base.denom == 0 {
		unsafe {
			C.mach_timebase_info(&time_base)
		}
	}
	return (tm - start_time) * time_base.numer / time_base.denom
}

// darwin_now returns a better precision current time for macos
fn darwin_now() Time {
	// get the high precision time as UTC realtime clock, and use the nanoseconds part
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	loc_tm := C.tm{}
	C.localtime_r(voidptr(&ts.tv_sec), &loc_tm)
	return convert_ctime(loc_tm, int(ts.tv_nsec))
}

// darwin_utc returns a better precision current time for macos
fn darwin_utc() Time {
	// get the high precision time as UTC clock
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	return unix_nanosecond(i64(ts.tv_sec), int(ts.tv_nsec))
}

// dummy to compile with all compilers
fn solaris_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn solaris_utc() Time {
	return Time{}
}
