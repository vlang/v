// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#include <time.h>

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
	start_time 		 = init_win_time_start()
	freq_time  		 = init_win_time_freq()
	start_local_time = local_as_unix_time()
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

// local_as_unix_time returns the current local time as unix time
fn local_as_unix_time() int {
	t := C.time(0)
	tm := C.localtime(&t)

	return make_unix_time(tm)
}

// win_now calculates current time using performance counters to get higher resolution on windows
// Since clock_gettime is not available in standard c performance counters are used
// to calculate the relative time from the program started in micro seconds and added
// the local time from program start
fn win_now() Time {

	tm := vpc_now()

	// get the relative time in micro seconds
	relative_time_us := (tm - start_time) / (freq_time / 1000000 )

	// total seconds as unix time
	total_seconds := start_local_time + int(relative_time_us / 1000000)
	remainder_us := relative_time_us % 1000000

	mut t := unix2(total_seconds, int(remainder_us))

	return t
}

// dummy to compile with all compilers
pub fn darwin_now() Time {
	return Time{}
}

// dummy to compile with all compilers
pub struct C.timeval {
	tv_sec  u64
	tv_usec u64
}
