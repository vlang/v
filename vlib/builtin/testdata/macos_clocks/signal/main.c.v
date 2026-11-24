module main

import builtin.testdata.macos_raw_signal as raw_signal
import sync.stdatomic

pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

fn C.nanosleep(const_request &C.timespec, remaining &C.timespec) i32
fn C.__error() &i32
fn C.atomic_fetch_add_u64(voidptr, u64) u64

__global (
	sleep_target  u64
	sleep_done    u64
	sleep_signals u64
)

@[no_profile; no_trace]
fn on_usr1(_ i32, _ voidptr, _ voidptr) {
	C.atomic_fetch_add_u64(&sleep_signals, 1)
}

fn require_sleep(ok bool) {
	if !ok { exit(92) }
}

fn interrupted_sleep() [4]i64 {
	port := bf_mach_thread_self()
	require_sleep(port != 0)
	old_mask, mask_error := raw_signal.mask()
	require_sleep(mask_error == 0)
	require_sleep(raw_signal.set_mask(old_mask & ~(u32(1) << 29)) == 0)
	require_sleep(bf_close(-1) == 9 && bf_last_errno() == 9)
	C.errno = 77
	request := C.timespec{ tv_sec: 2, tv_nsec: 0 }
	mut remaining := C.timespec{ tv_sec: -1, tv_nsec: -1 }
	stdatomic.store_u64(&sleep_target, u64(port))
	code := C.nanosleep(&request, &remaining)
	native_error := unsafe { *C.__error() }
	require_sleep(bf_last_errno() == 9)
	require_sleep(raw_signal.set_mask(old_mask) == 0)
	stdatomic.store_u64(&sleep_done, 1)
	return [i64(code), i64(native_error), remaining.tv_sec, remaining.tv_nsec]!
}

fn main() {
	require_sleep(raw_signal.install_usr1(on_usr1, false) == 0)
	require_sleep(bf_close(-1) == 9)
	C.errno = 71
	worker := spawn interrupted_sleep()
	mut port := u64(0)
	for port == 0 {
		port = stdatomic.load_u64(&sleep_target)
		require_sleep(bf_sleep_ns(100_000) == 0)
	}
	start, clock_error := bf_monotonic_now()
	require_sleep(clock_error == 0)
	// Publication alone does not prove the worker has entered its wait. Keep
	// signaling until completion, so a signal delivered just before nanosleep
	// cannot make this test accidentally wait out the entire request.
	for stdatomic.load_u64(&sleep_done) == 0 {
		signal_error := raw_signal.send_usr1(u32(port))
		// The worker may finish after the loop's completion read. A dead
		// target is expected only when completion has already been published.
		require_sleep(signal_error == 0
			|| (signal_error == 3 && stdatomic.load_u64(&sleep_done) != 0))
		require_sleep(bf_sleep_ns(1_000_000) == 0)
		now, now_error := bf_monotonic_now()
		require_sleep(now_error == 0 && now - start < 3_000_000_000)
	}
	result := worker.wait()
	require_sleep(bf_mach_port_deallocate(u32(port)) == 0)
	require_sleep(stdatomic.load_u64(&sleep_signals) > 0)
	require_sleep(result[0] == -1 && result[1] == 4)
	require_sleep(result[2] >= 0 && result[2] <= 2 && result[3] >= 0 && result[3] < 1_000_000_000)
	require_sleep(result[2] < 2 || result[3] == 0)
	require_sleep(bf_last_errno() == 9 && unsafe { *C.__error() } == 71)
	println('raw macOS nanosleep interruption ok')
}
