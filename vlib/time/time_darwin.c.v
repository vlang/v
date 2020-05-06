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
