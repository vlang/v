// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#include <time.h>
// #include <sysinfoapi.h>

pub struct C.tm {
pub mut:
	tm_year int
	tm_mon  int
	tm_mday int
	tm_hour int
	tm_min  int
	tm_sec  int
}

pub struct C._FILETIME {
pub mut:
	dwLowDateTime  u32
	dwHighDateTime u32
}

struct SystemTime {
	year        u16
	month       u16
	day_of_week u16
	day         u16
	hour        u16
	minute      u16
	second      u16
	millisecond u16
}

fn C.GetSystemTimeAsFileTime(lpSystemTimeAsFileTime &C._FILETIME)

fn C.FileTimeToSystemTime(lpFileTime &C._FILETIME, lpSystemTime &SystemTime)

fn C.SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation &C.TIME_ZONE_INFORMATION, lpUniversalTime &SystemTime,
	lpLocalTime &SystemTime)

fn C.localtime_s(t &C.time_t, tm &C.tm)

fn C.timespec_get(t &C.timespec, base int) int

// start_time is needed on Darwin and Windows because of potential overflows
const start_time = init_win_time_start()
const freq_time = init_win_time_freq()
const start_local_time = local_as_unix_time()

// in most systems, these are __quad_t, which is an i64
pub struct C.timespec {
pub:
	tv_sec  i64
	tv_nsec i64
}

fn C.QueryPerformanceCounter(&u64) C.BOOL

fn C.QueryPerformanceFrequency(&u64) C.BOOL

fn make_unix_time(t C.tm) i64 {
	return portable_timegm(&t)
}

fn init_win_time_freq() u64 {
	f := u64(0)
	C.QueryPerformanceFrequency(voidptr(&f))
	return f
}

fn init_win_time_start() u64 {
	s := u64(0)
	C.QueryPerformanceCounter(voidptr(&s))
	return s
}

// sys_mono_now returns a *monotonically increasing time*, NOT a time adjusted for daylight savings, location etc.
pub fn sys_mono_now() u64 {
	tm := u64(0)
	C.QueryPerformanceCounter(voidptr(&tm)) // XP or later never fail
	return (tm - start_time) * 1000000000 / freq_time
}

// Note: vpc_now is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
@[inline]
fn vpc_now() u64 {
	tm := u64(0)
	C.QueryPerformanceCounter(voidptr(&tm))
	return tm
}

// local_as_unix_time returns the current local time as unix time
fn local_as_unix_time() i64 {
	t := C.time(0)
	tm := C.localtime(&t)
	return make_unix_time(*tm)
}

// local - return the time `t`, converted to the currently active local timezone.
pub fn (t Time) local() Time {
	if t.is_local {
		return t
	}
	st_utc := SystemTime{
		year:        u16(t.year)
		month:       u16(t.month)
		day:         u16(t.day)
		hour:        u16(t.hour)
		minute:      u16(t.minute)
		second:      u16(t.second)
		millisecond: u16(t.nanosecond / 1_000_000)
	}
	st_local := SystemTime{}
	C.SystemTimeToTzSpecificLocalTime(unsafe { nil }, voidptr(&st_utc), voidptr(&st_local))
	t_local := Time{
		year:       st_local.year
		month:      st_local.month
		day:        st_local.day
		hour:       st_local.hour
		minute:     st_local.minute
		second:     st_local.second // These are the same
		nanosecond: int(st_local.millisecond) * 1_000_000
		unix:       st_local.unix()
	}
	return t_local
}

// win_now calculates current time using winapi to get higher resolution on windows
// GetSystemTimeAsFileTime is used and converted to local time. It can resolve time
// down to millisecond. Other more precise methods can be implemented in the future
fn win_now() Time {
	ft_utc := C._FILETIME{}
	C.GetSystemTimeAsFileTime(&ft_utc)
	st_utc := SystemTime{}
	C.FileTimeToSystemTime(&ft_utc, voidptr(&st_utc))
	st_local := SystemTime{}
	C.SystemTimeToTzSpecificLocalTime(unsafe { nil }, voidptr(&st_utc), voidptr(&st_local))
	t := Time{
		year:       st_local.year
		month:      st_local.month
		day:        st_local.day
		hour:       st_local.hour
		minute:     st_local.minute
		second:     st_local.second
		nanosecond: int(st_local.millisecond) * 1_000_000
		unix:       st_local.unix()
		is_local:   true
	}
	return t
}

// win_utc calculates current time using winapi to get higher resolution on windows
// GetSystemTimeAsFileTime is used. It can resolve time down to millisecond
// other more precise methods can be implemented in the future
fn win_utc() Time {
	ft_utc := C._FILETIME{}
	C.GetSystemTimeAsFileTime(&ft_utc)
	st_utc := SystemTime{}
	C.FileTimeToSystemTime(&ft_utc, voidptr(&st_utc))
	t := Time{
		year:       st_utc.year
		month:      st_utc.month
		day:        st_utc.day
		hour:       st_utc.hour
		minute:     st_utc.minute
		second:     st_utc.second
		nanosecond: int(st_utc.millisecond) * 1_000_000
		unix:       st_utc.unix()
		is_local:   false
	}
	return t
}

// unix returns Unix time.
fn (st SystemTime) unix() i64 {
	tt := C.tm{
		tm_sec:  st.second
		tm_min:  st.minute
		tm_hour: st.hour
		tm_mday: st.day
		tm_mon:  st.month - 1
		tm_year: st.year - 1900
	}
	return make_unix_time(tt)
}

// dummy to compile with all compilers
fn darwin_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn linux_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn solaris_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn darwin_utc() Time {
	return Time{}
}

// dummy to compile with all compilers
fn linux_utc() Time {
	return Time{}
}

// dummy to compile with all compilers
fn solaris_utc() Time {
	return Time{}
}

// sleep makes the calling thread sleep for a given duration (in nanoseconds).
pub fn sleep(duration Duration) {
	C.Sleep(int(duration / millisecond))
}
