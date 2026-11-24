module main

import math

#flag -I @VEXEROOT/vlib/builtin/macos_bare
#flag @DIR/probe.c
#include "@DIR/probe.h"

fn C.v_macos_clock_abi_probe() i32
fn C.v_macos_clock_probe_errno() i32
fn C.fabs(f64) f64

fn require(ok bool, detail string) {
	if !ok { panic('macOS clock: ' + detail) }
}

fn main() {
	code := C.v_macos_clock_abi_probe()
	require(code == 0, 'C ABI probe ${code}')
	println('raw macOS clock ABI ok')
	require(bf_close(-1) == 9 && bf_last_errno() == 9, 'BF error seed')
	seconds, nanos, realtime_error := bf_realtime_now()
	require(realtime_error == 0 && seconds > 1700000000 && nanos >= 0 && nanos < 1_000_000_000, 'BF realtime')
	start, monotonic_error := bf_monotonic_now()
	require(monotonic_error == 0 && start > 0, 'BF monotonic')
	require(bf_sleep_ns(-1) == 22 && bf_sleep_ns(0) == 0, 'BF sleep boundaries')
	require(bf_sleep_ns(2_000_000) == 0, 'BF sleep')
	after, after_error := bf_monotonic_now()
	require(after_error == 0 && after >= start + 2_000_000, 'BF elapsed sleep')
	require(bf_last_errno() == 9 && C.v_macos_clock_probe_errno() == 77, 'clock helpers preserve both errno cells')
	require(bf_nanosleep(1) == 0 && bf_last_errno() == 9, 'legacy milliseconds use raw clock')
	require(bf_nanosleep(-1) == 22 && bf_last_errno() == 22, 'legacy invalid timeout error policy')
	require(C.v_macos_clock_probe_errno() == 77, 'legacy sleep preserves native errno')
	value := f64(seconds % 4 + 1)
	require(math.sqrt(value * value) == value, 'portable sqrt')
	require(math.pow(value, 2) == value * value, 'portable power')
	require(math.floor(-value - 0.25) == -value - 1, 'portable floor')
	require(math.ceil(value + 0.25) == value + 1, 'portable ceil')
	require(math.exp(0) == 1 && math.log(1) == 0 && math.sin(0) == 0, 'portable transcendentals')
	negative_nan := math.f64_from_bits(u64(0xfff8000000000001))
	require(math.f64_bits(C.fabs(negative_nan)) == u64(0x7ff8000000000001), 'fabs clears NaN sign')
	require(bf_close(2) == 0, 'close trace stream')
	errno_canary()
	println('raw macOS BF clock ok')
}

fn errno_canary() {
	require(bf_last_errno() == 22 && C.v_macos_clock_probe_errno() == 77,
		'tracing with a failed stream preserves native and BF errors')
}
