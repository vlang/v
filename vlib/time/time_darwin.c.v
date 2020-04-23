module time

#include <mach/mach_time.h>

const (
	// start_time is needed on Darwin and Windows because of potential overflows
	start_time = C.mach_absolute_time()
	time_base = init_time_base()
)

fn C.mach_absolute_time() u64
fn C.mach_timebase_info(&C.mach_timebase_info_data_t)

struct C.mach_timebase_info_data_t {
	numer u64
	denom u64
}

fn init_time_base() C.mach_timebase_info_data_t {
	mut tb := C.mach_timebase_info_data_t
	C.mach_timebase_info(&tb)
	return tb
}

fn sys_mono_now_darwin() u64 {
	tm := C.mach_absolute_time()
	return mul_div(tm - start_time, time_base.numer, time_base.denom)
}

fn mul_div(val, numer, denom u64) u64 {
	q := val / denom
	r := val % denom
	return q * numer + r * numer / denom
}
