module builtin

$if amd64 && !native {
	#include "@DIR/clock_abi.h"

	// Keep SDK scalar typedefs opaque here so export wrappers preserve their
	// native spelling rather than replacing time_t/clockid_t with V integers.
	@[typedef]
	struct C.VMacClockId {}

	@[typedef]
	struct C.VMacClockTime {}

	pub struct C.timespec {
	pub mut:
		tv_sec  i64
		tv_nsec i64
	}

	struct C.timeval {}

	@[typedef]
	struct C.VMacClockConstTimespec {
		tv_sec  i64
		tv_nsec i64
	}

	// The compiler's export wrappers use these SDK types, including the const
	// nanosleep input; the implementation and errno policy remain entirely V.
	fn C.clock_gettime(C.VMacClockId, &C.timespec) i32
	fn C.gettimeofday(&C.timeval, voidptr) i32
	fn C.time(&C.VMacClockTime) C.VMacClockTime
	fn C.nanosleep(const_request &C.timespec, remaining &C.timespec) i32

	@[no_profile; no_trace]
	fn mac_clock_status(code int) i32 {
		if code != 0 {
			C.errno = code
			return -1
		}
		return 0
	}

	@[export: 'clock_gettime'; markused; no_profile; no_trace]
	fn mac_posix_clock_gettime(clock u32, stamp &C.timespec) i32 {
		if stamp == unsafe { nil } { return mac_clock_status(14) }
		mut value := FSysClockStamp{}
		code := mac_clock_parts(i32(clock), mut value)
		if code == 0 {
			unsafe {
				stamp.tv_sec = value.seconds
				stamp.tv_nsec = value.nanoseconds
			}
		}
		return mac_clock_status(code)
	}

	@[export: 'gettimeofday'; markused; no_profile; no_trace]
	fn mac_posix_gettimeofday(stamp &C.timeval, timezone voidptr) i32 {
		code := mac_kernel_bsd_call6(116, u64(stamp), u64(timezone), 0, 0, 0, 0, unsafe { nil })
		return mac_clock_status(int(code))
	}

	@[export: 'time'; markused; no_profile; no_trace]
	fn mac_posix_time(output voidptr) i64 {
		mut stamp := FSysClockStamp{}
		code := mac_clock_realtime_stamp(mut stamp)
		if code != 0 {
			mac_clock_status(code)
			return -1
		}
		if output != unsafe { nil } {
			unsafe { vmemcpy(output, &stamp.seconds, int(sizeof(i64))) }
		}
		return stamp.seconds
	}

	@[export: 'nanosleep'; markused; no_profile; no_trace]
	fn mac_posix_nanosleep(request &C.VMacClockConstTimespec, remaining &C.timespec) i32 {
		if request == unsafe { nil } { return mac_clock_status(14) }
		value := FSysClockStamp{ seconds: request.tv_sec, nanoseconds: request.tv_nsec }
		mut rest := FSysClockStamp{}
		code := mac_clock_sleep_native(value, mut rest)
		if code == 4 && remaining != unsafe { nil } {
			unsafe {
				remaining.tv_sec = rest.seconds
				remaining.tv_nsec = rest.nanoseconds
			}
		}
		return mac_clock_status(code)
	}
}
