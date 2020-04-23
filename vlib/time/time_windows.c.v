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

struct C.LARGE_INTEGER {
	QuadPart i64
}

const (
	// start_time is needed on Darwin and Windows because of potential overflows
	start_time = init_win_time_start()
	freq_time  = init_win_time_freq()
)

fn C._mkgmtime(&C.tm) time_t

fn C.QueryPerformanceCounter(*C.LARGE_INTEGER) C.BOOL

fn C.QueryPerformanceFrequency(*C.LARGE_INTEGER) C.BOOL

fn make_unix_time(t C.tm) int {
	return int(C._mkgmtime(&t))
}

fn init_win_time_freq() C.LARGE_INTEGER {
	mut f := C.LARGE_INTEGER{}
	_ := C.QueryPerformanceFrequency(&f)
	return f
}

fn init_win_time_start() C.LARGE_INTEGER {
	mut s := C.LARGE_INTEGER{}
	_ := C.QueryPerformanceCounter(&s)
	return s
}

fn sys_mono_now() u64 {
	mut tm := C.LARGE_INTEGER{}
	_ := C.QueryPerformanceCounter(&tm) // XP or later never fail
	return mul_div(tm.QuadPart - start_time.QuadPart, 1_000_000_000, freq_time.QuadPart)
}

fn mul_div(val, numer, denom u64) u64 {
	q := val / denom
	r := val % denom
	return q * numer + r * numer / denom
}
