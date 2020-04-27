// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

struct C.tm {
	tm_year int
	tm_mon  int
	tm_mday int
	tm_hour int
	tm_min  int
	tm_sec  int
}

fn C.timegm(&tm) time_t

fn make_unix_time(t C.tm) int {
	return int(C.timegm(&t))
}

type time_t voidptr

// in most systems, these are __quad_t, which is an i64
struct C.timespec {
	tv_sec  i64
	tv_nsec i64
}

// the first arg is defined in include/bits/types.h as `__S32_TYPE`, which is `int`
fn C.clock_gettime(int, &C.timespec)

fn sys_mono_now() u64 {
	$if macos {
		return sys_mono_now_darwin()
	} $else {
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	}
}

// NB: vpc_now is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
[inline]
fn vpc_now() u64 {
	$if macos {
		tm := C.mach_absolute_time()
		if time_base.denom == 0 {
			C.mach_timebase_info(&time_base)
		}
		return (tm - start_time) * time_base.numer / time_base.denom
	} $else {    
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	}
}
