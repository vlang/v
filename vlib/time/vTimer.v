@[has_globals]
module time

import sync.stdatomic
import math

// Windows specific
$if windows {
	#flag windows -lKernel32
	#include <windows.h>

	fn C.QueryPerformanceCounter(lpPerformanceCount voidptr) C.BOOL
	fn C.QueryPerformanceFrequency(lpFrequency voidptr) C.BOOL
}

// Unix/Linux/MacOS specific
$if !windows {
	$if macos {
		#include <mach/mach_time.h>

		struct C.mach_timebase_info_data_t {
			numer u32
			denom u32
		}

		fn C.mach_absolute_time() u64
		fn C.mach_timebase_info(&C.mach_timebase_info_data_t) int
	} $else {
		#include <time.h>

		struct C.timespec {
			tv_sec  i64
			tv_nsec i64
		}

		fn C.clock_gettime(clk_id int, tp &C.timespec) int
	}
}

// Global Definitions
__global qpc_freq_global i64
__global qpc_init_state i64
// 0 = uninitialized, 1 = init in progress, 2 = done
__global coeff_global Time_Coefficients
__global coeff_init_state i64
$if macos {
	__global mach_timebase_numer u32
	__global mach_timebase_denom u32
	__global mach_timebase_initialized i64
}

const thousandth = i64(1000)
const millionth = i64(1000000)
const billionth = i64(1000000000)
const minuteth = i64(60000000000)
const hourth = i64(3600000000000)
const dayth = i64(86400000000000)

// CLOCK_MONOTONIC for Unix systems
const clock_monotonic = 1

struct Time_Coefficients {
	multipler u64
	shift     u32
}

fn get_global_coeff() Time_Coefficients {
	if stdatomic.load_i64(&coeff_init_state) == 2 {
		return coeff_global
	}

	if stdatomic.load_i64(&coeff_init_state) == 1 {
		sleep(nanosecond.times(100))
		return coeff_global
	}
	stdatomic.store_i64(&coeff_init_state, 1)

	freq := init_freq_once()
	coeff := compute_coefficients(freq)
	unsafe {
		coeff_global = coeff
	}
	stdatomic.store_i64(&coeff_init_state, 2)
	return coeff
}

fn compute_coefficients(freq i64) Time_Coefficients {
	mut shift := u32(0)
	mut numerator := u64(billionth)

	// Scale numerator upward until division produces a non-zero multipler
	for numerator / u64(freq) == 0 {
		numerator <<= 1
		shift++
	}

	// Now compute multipler safely
	return Time_Coefficients{
		multipler: numerator / u64(freq)
		shift:     shift
	}
}

fn ticks_to_ns(ticks i64, coeff Time_Coefficients) i64 {
	return i64((u64(ticks) * coeff.multipler) >> coeff.shift)
}

fn get_monotonic_time() i64 {
	$if windows {
		mut now := i64(0)
		C.QueryPerformanceCounter(voidptr(&now))
		return now
	} $else $if macos {
		ticks := C.mach_absolute_time()
		// Initialize timebase if needed
		if stdatomic.load_i64(&mach_timebase_initialized) == 0 {
			mut info := C.mach_timebase_info_data_t{}
			C.mach_timebase_info(&info)
			unsafe {
				mach_timebase_numer = info.numer
				mach_timebase_denom = info.denom
			}
			stdatomic.store_i64(&mach_timebase_initialized, 1)
		}
		// Convert to nanoseconds: ns = ticks * numer / denom
		numer := unsafe { mach_timebase_numer }
		denom := unsafe { mach_timebase_denom }
		return i64(ticks * u64(numer) / u64(denom))
	} $else {
		ts := C.timespec{}
		C.clock_gettime(clock_monotonic, &ts)
		return ts.tv_sec * billionth + ts.tv_nsec
	}
}

// Timer - start, stop, new_timer, format_time, now_ns, ns
// atomic and precise
pub struct Timer {
	freq  i64
	coeff Time_Coefficients
pub mut:
	start_t i64 // accessed via atomics only
	end_t   i64 // accessed via atomics only
	running bool
}

// start - Atomically set start_t to current counter
pub fn (mut t Timer) start() {
	now := get_monotonic_time()
	stdatomic.store_i64(&t.start_t, now)
	stdatomic.store_i64(&t.end_t, 0)
	t.running = true
}

pub fn (mut t Timer) stop() {
	now := get_monotonic_time()
	stdatomic.store_i64(&t.end_t, now)
	t.running = false
}

// new_timer - initialize a new WinTimer
pub fn new_timer() Timer {
	freq := init_freq_once()
	return Timer{
		freq:    freq
		coeff:   compute_coefficients(freq)
		start_t: 0
		end_t:   0
		running: false
	}
}

fn (mut t Timer) elapsed() i64 {
	now := if t.running {
		get_monotonic_time()
	} else {
		stdatomic.load_i64(&t.end_t)
	}

	start := stdatomic.load_i64(&t.start_t)
	return now - start
}

// ns - Return elapsed nanoseconds
pub fn (mut t Timer) ns() i64 {
	$if windows {
		return ticks_to_ns(t.elapsed(), t.coeff)
	} $else {
		return t.elapsed()
	}
}

// ns_to_us - Return elapsed microseconds
pub fn (mut t Timer) ns_to_us() f64 {
	return f64(t.ns()) / thousandth
}

// ns_to_ms - Return elapsed milliseconds
pub fn (mut t Timer) ns_to_ms() f64 {
	return f64(t.ns()) / millionth
}

// ns_to_s - Return elapsed seconds
pub fn (mut t Timer) ns_to_secs() f64 {
	return f64(t.ns()) / billionth
}

// ns_to_mins - Return elapsed minutes
pub fn (mut t Timer) ns_to_mins() f64 {
	return f64(t.ns()) / minuteth
}

// ns_to_hrs - Return elapsed hours
pub fn (mut t Timer) ns_to_hrs() f64 {
	return f64(t.ns()) / hourth
}

// ns_to_days - Return elapsed days
pub fn (mut t Timer) ns_to_days() f64 {
	return f64(t.ns()) / dayth
}

pub fn now_ns() i64 {
	$if windows {
		coeff := get_global_coeff()
		mut ticks := i64(0)
		C.QueryPerformanceCounter(voidptr(&ticks))
		return ticks_to_ns(ticks, coeff)
	} $else $if macos {
		ticks := C.mach_absolute_time()
		if stdatomic.load_i64(&mach_timebase_initialized) == 0 {
			mut info := C.mach_timebase_info_data_t{}
			C.mach_timebase_info(&info)
			unsafe {
				mach_timebase_numer = info.numer
				mach_timebase_denom = info.denom
			}
			stdatomic.store_i64(&mach_timebase_initialized, 1)
		}
		// Convert to nanoseconds: ns = ticks * numer / denom
		numer := unsafe { mach_timebase_numer }
		denom := unsafe { mach_timebase_denom }
		return i64(ticks * u64(numer) / u64(denom))
	} $else {
		ts := C.timespec{}
		C.clock_gettime(clock_monotonic, &ts)
		return ts.tv_sec * billionth + ts.tv_nsec
	}
}

fn init_freq_once() i64 {
	if stdatomic.load_i64(&qpc_init_state) == 2 {
		return stdatomic.load_i64(&qpc_freq_global)
	}
	if stdatomic.load_i64(&qpc_init_state) == 1 {
		sleep(nanosecond.times(10))
		return stdatomic.load_i64(&qpc_freq_global)
	}

	stdatomic.store_i64(&qpc_init_state, 1)
	mut freq := i64(0)
	$if windows {
		C.QueryPerformanceFrequency(voidptr(&freq))
	} $else {
		// On Unix systems - clock_gettime returns nanoseconds directly
		// so our frequency is 1 billion (nanoseconds per second)
		freq = billionth
	}

	stdatomic.store_i64(&qpc_freq_global, freq)

	// Publish completion
	stdatomic.store_i64(&qpc_init_state, 2)
	return freq
}

pub fn format_time[T](ns T) string {
	mut remaining := f64(ns)
	mut secs := i64(remaining / billionth)
	remaining = math.fmod(remaining, billionth)

	mut mins := secs / 60
	secs %= 60

	mut hrs := mins / 60
	mins %= 60

	mut days := hrs / 24
	hrs %= 24

	mut parts := []string{}

	if days > 0 {
		parts << '${days} day' + if days > 1 { 's' } else { '' }
	}
	if hrs > 0 {
		parts << '${hrs} hr' + if hrs > 1 { 's' } else { '' }
	}
	if mins > 0 {
		parts << '${mins} min' + if mins > 1 { 's' } else { '' }
	}
	if secs > 0 {
		parts << '${secs} sec' + if secs > 1 { 's' } else { '' }
	}

	// fallback to sub-second duration
	if parts.len == 0 {
		ns_val := f64(ns)
		if ns_val >= millionth {
			return '${ns_val / millionth:.3f} ms'
		} else if ns >= thousandth {
			return '${ns_val / thousandth:.3f} ㎲'
		} else {
			return '${ns_val} ns'
		}
	}
	return parts.join(' ')
}
