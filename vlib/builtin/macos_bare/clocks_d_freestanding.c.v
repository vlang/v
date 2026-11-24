module builtin

$if amd64 && !native {
	// Use the existing scalar atomics, rather than a profiled V wrapper, on
	// the profiling clock's dependency chain. No heap or TLS is needed here.
	#insert "@VEXEROOT/thirdparty/stdatomic/nix/atomic.h"

	fn C.atomic_load_u64(voidptr) u64
	fn C.atomic_store_u64(voidptr, u64)
	fn C.atomic_load_byte(voidptr) u8
	fn C.atomic_thread_fence(i32)

	__global mac_clock_ratio u64

	struct MacClockTimebase {
		numerator   u32
		denominator u32
	}

	// XNU ntptimeval64: nanotime, maxerror, esterror, tai, state, padding.
	struct MacClockNtpTime {
	mut:
		time     FSysClockStamp
		maxerror i64
		esterror i64
		tai      i64
		state    i32 = -1
		padding  i32
	}

	struct MacClockTimeval {
		seconds      i64
		microseconds i32
		padding      i32
	}

	struct MacClockUsage {
		user   MacClockTimeval
		system MacClockTimeval
		other  [14]i64
	}

	// These are the published 64-bit Intel commpage offsets in XNU's
	// osfmk/i386/cpu_capabilities.h. Only kernel-maintained data is read.
	const mac_clock_commpage = u64(0x00007fffffe00000)

	@[no_profile; no_trace]
	fn mac_clock_word(offset u64) u64 {
		return C.atomic_load_u64(voidptr(mac_clock_commpage + offset))
	}

	@[no_profile; no_trace]
	fn mac_clock_fence() {
		C.atomic_thread_fence(5) // memory_order_seq_cst
	}

	@[no_profile; no_trace]
	fn mac_clock_realtime_stamp(mut stamp FSysClockStamp) int {
		mut value := MacClockNtpTime{}
		code := mac_kernel_bsd_call6(528, u64(&value), 0, 0, 0, 0, 0, unsafe { nil })
		// XNU copies the timestamp, then returns NTP state 0..5 through the
		// error channel. Unsynchronized time is still readable. Require the
		// independent output to agree, so untouched storage is never accepted.
		if code > 5 || value.state != code || value.time.nanoseconds < 0
			|| value.time.nanoseconds >= 1_000_000_000 {
			return if code != 0 { int(code) } else { 5 }
		}
		stamp = value.time
		return 0
	}

	@[no_profile; no_trace]
	fn mac_clock_realtime_native() (i64, i64, int) {
		mut stamp := FSysClockStamp{}
		code := mac_clock_realtime_stamp(mut stamp)
		return stamp.seconds, stamp.nanoseconds, code
	}

	@[no_profile; no_trace]
	fn mac_clock_ticks(approximate bool) (u64, int) {
		if approximate && C.atomic_load_byte(voidptr(mac_clock_commpage + 0x088)) != 0 {
			return mac_clock_word(0x080), 0
		}
		mut ticks := u64(0)
		// gettimeofday's third output is mach_absolute_time; the first two
		// outputs may be nil. This is the raw kernel route, without libSystem.
		code := mac_kernel_bsd_call6(116, 0, 0, u64(&ticks), 0, 0, 0, unsafe { nil })
		return ticks, int(code)
	}

	@[no_profile; no_trace]
	fn mac_clock_timebase() (u64, int) {
		mut ratio := C.atomic_load_u64(&mac_clock_ratio)
		if ratio == 0 {
			value := MacClockTimebase{}
			code := mac_kernel_mach_trap6(89, u64(&value), 0, 0, 0, 0, 0)
			if code != 0 || value.numerator == 0 || value.denominator == 0 {
				return 0, 5
			}
			ratio = u64(value.numerator) | (u64(value.denominator) << 32)
			// The ratio is immutable; duplicate concurrent cold queries agree.
			C.atomic_store_u64(&mac_clock_ratio, ratio)
		}
		return ratio, 0
	}

	@[no_profile; no_trace]
	fn mac_clock_scale(ticks u64, ratio u64) (u64, int) {
		numerator := u64(u32(ratio))
		denominator := ratio >> 32
		whole := ticks / denominator
		fraction := (ticks % denominator) * numerator / denominator
		if whole > (~u64(0) - fraction) / numerator {
			return 0, 84 // native Darwin EOVERFLOW
		}
		return whole * numerator + fraction, 0
	}

	@[no_profile; no_trace]
	fn mac_clock_uptime(approximate bool) (u64, int) {
		ticks, code := mac_clock_ticks(approximate)
		if code != 0 { return 0, code }
		ratio, ratio_code := mac_clock_timebase()
		if ratio_code != 0 { return 0, ratio_code }
		return mac_clock_scale(ticks, ratio)
	}

	@[no_profile; no_trace]
	fn mac_clock_uptime_native() (u64, int) {
		return mac_clock_uptime(false)
	}

	@[no_profile; no_trace]
	fn mac_clock_continuous(approximate bool) (u64, int) {
		for {
			before := mac_clock_word(0x0c0)
			mac_clock_fence()
			ticks, code := mac_clock_ticks(approximate)
			if code != 0 { return 0, code }
			mac_clock_fence()
			if before != mac_clock_word(0x0c0) { continue }
			if ticks > ~u64(0) - before { return 0, 84 }
			ratio, ratio_code := mac_clock_timebase()
			if ratio_code != 0 { return 0, ratio_code }
			return mac_clock_scale(ticks + before, ratio)
		}
		return 0, 5
	}

	@[ignore_overflow; no_profile; no_trace]
	fn mac_clock_boottime() (u64, int) {
		microseconds := mac_clock_word(0x0c8)
		if microseconds != 0 { return microseconds, 0 }
		name := [i32(1), 21]! // CTL_KERN, KERN_BOOTTIME
		stamp := MacClockTimeval{}
		mut size := u64(sizeof(MacClockTimeval))
		code := mac_kernel_bsd_call6(202, u64(&name[0]), 2, u64(&stamp), u64(&size), 0, 0, unsafe { nil })
		if code != 0 { return 0, int(code) }
		if size < sizeof(MacClockTimeval) || stamp.microseconds < 0
			|| stamp.microseconds >= 1_000_000 {
			return 0, 5
		}
		return u64(stamp.seconds) * 1_000_000 + u64(stamp.microseconds), 0
	}

	@[no_profile; no_trace]
	fn mac_clock_adjusted(mut stamp FSysClockStamp) int {
		for {
			before, before_code := mac_clock_boottime()
			if before_code != 0 { return before_code }
			mac_clock_fence()
			code := mac_clock_realtime_stamp(mut stamp)
			if code != 0 { return code }
			mac_clock_fence()
			after, after_code := mac_clock_boottime()
			if after_code != 0 { return after_code }
			if before != after { continue }
			stamp = mac_clock_since_boot(stamp, before)
			return 0
		}
		return 5
	}

	// XNU stores boot calendar microseconds modulo 2^64. Subtract in that
	// domain so a valid pre-epoch calendar does not break monotonic time.
	@[ignore_overflow; no_profile; no_trace]
	fn mac_clock_since_boot(calendar FSysClockStamp, boot_microseconds u64) FSysClockStamp {
		calendar_microseconds := u64(calendar.seconds) * 1_000_000 + u64(calendar.nanoseconds) / 1000
		elapsed := calendar_microseconds - boot_microseconds
		return FSysClockStamp{
			seconds:     i64(elapsed / 1_000_000)
			nanoseconds: i64(elapsed % 1_000_000) * 1000 + calendar.nanoseconds % 1000
		}
	}

	@[no_profile; no_trace]
	fn mac_clock_parts(clock i32, mut stamp FSysClockStamp) int {
		if clock == 0 { return mac_clock_realtime_stamp(mut stamp) }
		if clock == 6 { return mac_clock_adjusted(mut stamp) }
		if clock == 12 {
			usage := MacClockUsage{}
			code := mac_kernel_bsd_call6(117, 0, u64(&usage), 0, 0, 0, 0, unsafe { nil })
			if code != 0 { return int(code) }
			microseconds := i64(usage.user.microseconds) + i64(usage.system.microseconds)
			stamp.seconds = usage.user.seconds + usage.system.seconds + microseconds / 1_000_000
			stamp.nanoseconds = (microseconds % 1_000_000) * 1000
			return 0
		}
		mut nanos := u64(0)
		mut code := 0
		match clock {
			4, 5 { nanos, code = mac_clock_continuous(clock == 5) }
			8, 9 { nanos, code = mac_clock_uptime(clock == 9) }
			16 {
				mut ticks := u64(0)
				code = int(mac_kernel_bsd_call6(482, 0, 0, 0, 0, 0, 0, &ticks))
				if code == 0 {
					ratio, ratio_code := mac_clock_timebase()
					if ratio_code != 0 { return ratio_code }
					nanos, code = mac_clock_scale(ticks, ratio)
				}
			}
			else { return 22 }
		}
		if code != 0 { return code }
		stamp.seconds = i64(nanos / 1_000_000_000)
		stamp.nanoseconds = i64(nanos % 1_000_000_000)
		return 0
	}

	// ceil((seconds*1e9 + nanos)*denominator/numerator), saturated at limit.
	// Splitting seconds by numerator bounds every product by 64 bits, even for
	// the largest valid POSIX timespec. No compiler-rt wide division is needed.
	@[no_profile; no_trace]
	fn mac_clock_duration_ticks(seconds u64, nanos u64, ratio u64, limit u64) u64 {
		numerator := u64(u32(ratio))
		denominator := ratio >> 32
		unit := u64(1_000_000_000) * denominator
		whole := seconds / numerator
		if whole > limit / unit { return limit }
		mut result := whole * unit
		partial := (seconds % numerator) * 1_000_000_000 + nanos
		term := (partial / numerator) * denominator
		if term > limit - result { return limit }
		result += term
		fraction := ((partial % numerator) * denominator + numerator - 1) / numerator
		if fraction > limit - result { return limit }
		return result + fraction
	}

	@[no_profile; no_trace]
	fn mac_clock_sleep_native(request FSysClockStamp, mut remaining FSysClockStamp) int {
		if request.seconds < 0 || request.nanoseconds < 0 || request.nanoseconds >= 1_000_000_000 {
			return 22
		}
		if request.seconds == 0 && request.nanoseconds == 0 { return 0 }
		start, start_code := mac_clock_ticks(false)
		if start_code != 0 { return start_code }
		ratio, ratio_code := mac_clock_timebase()
		if ratio_code != 0 { return ratio_code }
		ticks := mac_clock_duration_ticks(u64(request.seconds), u64(request.nanoseconds), ratio, ~u64(0) - start)
		result := mac_kernel_mach_trap6(90, start + ticks, 0, 0, 0, 0, 0)
		if result == 0 { return 0 }
		if result != 14 { return 5 } // KERN_ABORTED -> EINTR
		now, now_code := mac_clock_ticks(false)
		if now_code != 0 { return now_code }
		elapsed, elapsed_code := mac_clock_scale(now - start, ratio)
		if elapsed_code != 0 { return elapsed_code }
		mut seconds := request.seconds - i64(elapsed / 1_000_000_000)
		mut nanos := request.nanoseconds - i64(elapsed % 1_000_000_000)
		if nanos < 0 {
			nanos += 1_000_000_000
			seconds--
		}
		remaining.seconds = if seconds < 0 { 0 } else { seconds }
		remaining.nanoseconds = if seconds < 0 { 0 } else { nanos }
		return 4
	}
}
