module main

#flag @DIR/abi_consumer.c
#include "@DIR/abi_consumer.h"

fn C.v_test_clock_abi() i32
fn C.v_test_clock_interrupted() i32

@[export: 'v_test_clock_errno'; no_profile; no_trace]
fn mock_errno_addr() &i32 {
	$if macos {
		return C.__error()
	} $else {
		return C.__errno_location()
	}
}

// The test harness compiles the unchanged V clock policy with raw dispatcher
// doubles on the host. Actual Darwin ABI/kernel probes are a separate test.
struct FSysClockStamp {
mut:
	seconds     i64
	nanoseconds i64
}

__global (
	bf_native_errno_cell i32
	mock_ntp_state       i32
	mock_ntp_error       i32
	mock_ntp_nanos       = i64(123456789)
	mock_ntp_seconds     = i64(123)
	mock_untouched       bool
	mock_ticks           = u64(100)
	mock_timebase        = u64(2) << 32 | u64(3)
	mock_wait_result     u64
	mock_deadline        u64
)

@[no_profile; no_trace]
fn mac_kernel_bsd_call6(number u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64, value &u64) i32 {
	if number == 528 {
		if !mock_untouched {
			unsafe {
				mut stamp := &MacClockNtpTime(a1)
				stamp.time.seconds = mock_ntp_seconds
				stamp.time.nanoseconds = mock_ntp_nanos
				stamp.state = mock_ntp_state
			}
		}
		return mock_ntp_error
	}
	if number == 116 {
		if a1 != 0 {
			unsafe { *&MacClockTimeval(a1) = MacClockTimeval{ seconds: 123, microseconds: 456789 } }
		}
		if a3 != 0 { unsafe { *&u64(a3) = mock_ticks } }
		return 0
	}
	return 22
}

@[no_profile; no_trace]
fn mac_kernel_mach_trap6(index u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64) u64 {
	if index == 89 {
		unsafe { *&MacClockTimebase(a1) = MacClockTimebase{ numerator: u32(mock_timebase), denominator: u32(mock_timebase >> 32) } }
		return 0
	}
	if index == 90 {
		mock_deadline = a1
		mock_ticks += 2
		return mock_wait_result
	}
	return 5
}

fn main() {
	assert sizeof(MacClockTimebase) == 8
	assert sizeof(MacClockNtpTime) == 48
	assert __offsetof(MacClockNtpTime, state) == 40
	assert sizeof(MacClockTimeval) == 16
	assert sizeof(MacClockUsage) == 144
	for state in 0 .. 6 {
		mock_ntp_state = i32(state)
		mock_ntp_error = i32(state)
		seconds, nanos, code := mac_clock_realtime_native()
		assert code == 0 && seconds == 123 && nanos == 123456789
	}
	for epoch in [i64(-1), 0, -9223372036854775807] {
		mock_ntp_state = 0
		mock_ntp_error = 0
		mock_ntp_seconds = epoch
		seconds, _, code := mac_clock_realtime_native()
		assert code == 0 && seconds == epoch
	}
	mock_ntp_seconds = 123
	negative := mac_clock_since_boot(FSysClockStamp{ seconds: -1, nanoseconds: 123456789 }, u64(i64(-5000000)))
	assert negative.seconds == 4 && negative.nanoseconds == 123456789
	cross_epoch := mac_clock_since_boot(FSysClockStamp{ seconds: 1, nanoseconds: 123456789 }, u64(i64(-1000000)))
	assert cross_epoch.seconds == 2 && cross_epoch.nanoseconds == 123456789
	positive := mac_clock_since_boot(FSysClockStamp{ seconds: 10, nanoseconds: 999999999 }, 1999999)
	assert positive.seconds == 9 && positive.nanoseconds == 999
	mock_untouched = true
	_, _, untouched := mac_clock_realtime_native()
	assert untouched == 5
	mock_ntp_error = 14
	_, _, fault := mac_clock_realtime_native()
	assert fault == 14
	mock_untouched = false
	mock_ntp_state = 0
	mock_ntp_error = 0
	mock_ntp_nanos = 1_000_000_000
	_, _, invalid := mac_clock_realtime_native()
	assert invalid == 5
	mock_ntp_nanos = 123456789
	check_clock_vectors()
	mut remaining := FSysClockStamp{ seconds: 77, nanoseconds: 88 }
	assert mac_clock_sleep_native(FSysClockStamp{ seconds: -1 }, mut remaining) == 22
	assert mac_clock_sleep_native(FSysClockStamp{ nanoseconds: 1_000_000_000 }, mut remaining) == 22
	assert mac_clock_sleep_native(FSysClockStamp{}, mut remaining) == 0
	assert remaining.seconds == 77 && remaining.nanoseconds == 88
	mock_wait_result = 0
	mock_ticks = 100
	assert mac_clock_sleep_native(FSysClockStamp{ nanoseconds: 4 }, mut remaining) == 0
	assert mock_deadline == 103 // ceil(4*2/3)
	mock_wait_result = 14
	mock_ticks = 100
	assert mac_clock_sleep_native(FSysClockStamp{ seconds: 1, nanoseconds: 4 }, mut remaining) == 4
	assert remaining.seconds == 1 && remaining.nanoseconds == 1 // two ticks = 3ns
	mock_ticks = 100
	assert mac_clock_sleep_native(FSysClockStamp{ nanoseconds: 2 }, mut remaining) == 4
	assert remaining.seconds == 0 && remaining.nanoseconds == 0
	mock_wait_result = 0
	mock_ticks = 100
	assert mac_clock_sleep_native(FSysClockStamp{ seconds: 9223372036854775807 }, mut remaining) == 0
	assert mock_deadline == ~u64(0)
	mock_wait_result = 1
	assert mac_clock_sleep_native(FSysClockStamp{ nanoseconds: 1 }, mut remaining) == 5
	// The C consumer exercises SDK prototypes, including the const input.
	bf_native_errno_cell = 55
	mock_wait_result = 0
	assert C.v_test_clock_abi() == 0
	mock_wait_result = 14
	assert C.v_test_clock_interrupted() == 0
	assert bf_native_errno_cell == 55 // host errno must not use a private cell
	println('V clock policy and ABI ok')
}
