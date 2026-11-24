module time

// A portable snapshot can be generated on Darwin and compiled on another OS.
// Its clocks use the POSIX API instead of retaining Darwin-only initializers.
$if !(freestanding && amd64 && !native) && !cross ? {
	// start_time is needed on Darwin and Windows because of potential overflows
	const start_time = C.mach_absolute_time()
	const time_base = init_time_base()

	@[typedef]
	pub struct C.mach_timebase_info_data_t {
		numer u32
		denom u32
	}

	// Clock initialization needs these declarations even in cached module headers.
	@[c_extern; markused]
	fn C.mach_absolute_time() u64
	@[c_extern; markused]
	fn C.mach_timebase_info(&C.mach_timebase_info_data_t) i32
	fn C.clock_gettime_nsec_np(i32) u64

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
}

fn sys_mono_now_darwin() u64 {
	$if freestanding && amd64 && !native {
		nanos, code := bf_monotonic_now()
		if code != 0 { panic('time: monotonic clock failed (${code})') }
		return nanos
	} $else $if cross ? {
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	} $else {
		tm := C.mach_absolute_time()
		if time_base.denom == 0 {
			unsafe {
				C.mach_timebase_info(&time_base)
			}
		}
		return (tm - start_time) * time_base.numer / time_base.denom
	}
}

// Note: vpc_now_darwin is used by `v -profile` .
// Every V helper it calls must also be excluded from profiling.
@[inline; no_profile; no_trace]
fn vpc_now_darwin() u64 {
	$if freestanding && amd64 && !native {
		nanos, _ := bf_monotonic_now()
		return nanos
	} $else $if cross ? {
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	} $else {
		tm := C.mach_absolute_time()
		if time_base.denom == 0 {
			unsafe {
				C.mach_timebase_info(&time_base)
			}
		}
		return (tm - start_time) * time_base.numer / time_base.denom
	}
}

// darwin_now returns a better precision current time for macos
fn darwin_now() Time {
	$if freestanding && amd64 && !native {
		seconds, nanos := bare_realtime_parts()
		return bare_local_from_unix(seconds, nanos)
	} $else $if cross ? {
		return linux_now()
	} $else {
		// Use clock_gettime_nsec_np to avoid field-address lowering on C.timespec.
		epoch_ns := i64(C.clock_gettime_nsec_np(C.CLOCK_REALTIME))
		sec := epoch_ns / i64(second)
		nsec := int(epoch_ns % i64(second))
		loc_tm := C.tm{}
		C.localtime_r(voidptr(&sec), &loc_tm)
		return convert_ctime_with_unix(loc_tm, nsec, sec + i64(loc_tm.tm_gmtoff))
	}
}

// darwin_utc returns a better precision current time for macos
fn darwin_utc() Time {
	$if freestanding && amd64 && !native {
		seconds, nanos := bare_realtime_parts()
		return unix_nanosecond(seconds, nanos)
	} $else $if cross ? {
		return linux_utc()
	} $else {
		epoch_ns := i64(C.clock_gettime_nsec_np(C.CLOCK_REALTIME))
		sec := epoch_ns / i64(second)
		nsec := int(epoch_ns % i64(second))
		return unix_nanosecond(sec, nsec)
	}
}

// dummy to compile with all compilers
fn solaris_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn solaris_utc() Time {
	return Time{}
}
