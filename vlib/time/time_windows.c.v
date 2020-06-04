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


const (
	// start_time is needed on Darwin and Windows because of potential overflows
	start_time = init_win_time_start()
	freq_time  = init_win_time_freq()
)

// in most systems, these are __quad_t, which is an i64
struct C.timespec {
	tv_sec  i64
	tv_nsec i64
}


fn C._mkgmtime(&C.tm) time_t

fn C.QueryPerformanceCounter(&u64) C.BOOL

fn C.QueryPerformanceFrequency(&u64) C.BOOL

fn make_unix_time(t C.tm) int {
	return int(C._mkgmtime(&t))
}

fn init_win_time_freq() u64 {
	f := u64(0)
	C.QueryPerformanceFrequency(&f)
	return f
}

fn init_win_time_start() u64 {
	s := u64(0)
	C.QueryPerformanceCounter(&s)
	return s
}

pub fn sys_mono_now() u64 {
	tm := u64(0)
	C.QueryPerformanceCounter(&tm) // XP or later never fail
	return (tm - start_time) * 1_000_000_000 / freq_time
}

// NB: vpc_now is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
[inline]
fn vpc_now() u64 {
	tm := u64(0)
	C.QueryPerformanceCounter(&tm)
	return tm
}
// the first arg is defined in include/bits/types.h as `__S32_TYPE`, which is `int`
//fn C.clock_gettime(int, &C.timespec)

fn win_now() Time {
	mono := sys_mono_now()

	tv_sec 	:= mono / freq_time
	tv_nsec := ((mono %  freq_time) * 1_000_000_000) / freq_time

	mut t := unix(int(tv_sec))
	t.nanosecond = int(tv_nsec)
	return t
}