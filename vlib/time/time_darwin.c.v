module time

#include <mach/mach_time.h>

const (
	// start_time is needed on Darwin and Windows because of potential overflows
	start_time = C.mach_absolute_time()
	time_base = init_time_base()
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

fn init_time_base() InternalTimeBase {
	tb := C.mach_timebase_info_data_t{}
	C.mach_timebase_info(&tb)
	return InternalTimeBase{numer:tb.numer, denom:tb.denom}
}

fn sys_mono_now_darwin() u64 {
	tm := C.mach_absolute_time()
	if time_base.denom == 0 {
		C.mach_timebase_info(&time_base)
	}
	return (tm - start_time) * time_base.numer / time_base.denom
}

// NB: vpc_now_darwin is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
[inline]
fn vpc_now_darwin() u64 {
	tm := C.mach_absolute_time()
	if time_base.denom == 0 {
		C.mach_timebase_info(&time_base)
	}
	return (tm - start_time) * time_base.numer / time_base.denom
}

// darwin_now returns a better precision current time for Darwin based operating system
// this should be implemented with native system calls eventually
// but for now a bit tweaky. It uses the deprecated  gettimeofday clock to get
// the microseconds seconds part and normal local time to get correct local time
// if the time has shifted on a second level between calls it uses
// zero as microsecond. Not perfect but better that unix time only us a second
fn darwin_now() Time {

	// get the high precision time as UTC clock
	// and use the nanoseconds part
	tv := C.timeval{}
	C.gettimeofday(&tv, 0)

	t := C.time(0)
	C.localtime(&t)

	// if the second part (very rare) is different
	// microseconds is set to zero since it passed the second
	// also avoid divide by zero if nsec is zero
	if int(t) != tv.tv_sec {
		return unix2(int(t), 0)
	}

	return unix2(int(t), int(tv.tv_usec))
}
