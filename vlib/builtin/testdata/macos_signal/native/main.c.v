module main

import os

#flag @DIR/../policy.c

fn C.v_macos_signal_policy_probe() i32
fn C.v_macos_signal_delivery_probe() i32
fn C.v_macos_signal_install_worker(fn (i32)) i32
fn C.v_macos_signal_restore_worker() i32
fn C.v_macos_signal_expect_worker(u64)
fn C.v_macos_signal_note_worker(i32, u64, i32, i32)
fn C.v_macos_signal_worker_seen() i32
fn C.v_macos_kernel_bsd_call6(u32, u64, u64, u64, u64, u64, u64, &u64) i32
fn C.v_macos_kernel_thread_self() u32
fn C.v_macos_kernel_port_deallocate(u32) i32

fn require_signal(ok bool, detail string) {
	if !ok {
		eprintln(detail)
		exit(93)
	}
}

@[no_profile; no_trace]
fn on_worker_signal(number i32) {
	C.v_macos_signal_note_worker(number, bf_gettid(), bf_last_errno(), C.errno)
}

fn restart_reader(fd i32, ready chan u32) i32 {
	require_signal(bf_close(-1) == 9, 'seed worker BF errno')
	C.errno = 77
	C.v_macos_signal_expect_worker(bf_gettid())
	port := C.v_macos_kernel_thread_self()
	require_signal(port != 0, 'acquire worker port')
	ready <- port
	mut byte := u8(0)
	mut count := u64(0)
	// One raw read: an EINTR retry here would hide a missing SA_RESTART.
	code := C.v_macos_kernel_bsd_call6(3, u64(fd), u64(&byte), 1, 0, 0, 0, &count)
	require_signal(C.v_macos_kernel_port_deallocate(port) == 0, 'release worker port')
	if code != 0 { return code }
	require_signal(count == 1 && byte == 42, 'restarted read result')
	require_signal(C.errno == 77 && bf_last_errno() == 9, 'worker errno after read')
	return 0
}

fn require_restarted_read_on_worker() {
	mut pair := [2]i32{}
	require_signal(C.v_macos_kernel_bsd_call6(135, 1, 1, 0, u64(&pair[0]), 0, 0, unsafe { nil }) == 0,
		'create stream pair')
	defer {
		require_signal(bf_close(pair[0]) == 0, 'close reader')
		require_signal(bf_close(pair[1]) == 0, 'close writer')
	}
	require_signal(C.v_macos_signal_install_worker(on_worker_signal) == 0, 'install V handler')
	defer { require_signal(C.v_macos_signal_restore_worker() == 0, 'restore worker handler') }
	ready := chan u32{cap: 1}
	task := spawn restart_reader(pair[0], ready)
	port := <-ready
	// Let the receiver block on the empty stream before interrupting its read.
	require_signal(bf_sleep_ns(10_000_000) == 0, 'wait for blocked reader')
	require_signal(C.v_macos_kernel_bsd_call6(328, port, 30, 0, 0, 0, 0, unsafe { nil }) == 0,
		'signal reader')
	for C.v_macos_signal_worker_seen() == 0 {
		require_signal(bf_sleep_ns(1_000_000) == 0, 'wait for V handler')
	}
	require_signal(C.v_macos_signal_worker_seen() == 1, 'handler thread and errno')
	mut byte := u8(42)
	mut written := u64(0)
	require_signal(C.v_macos_kernel_bsd_call6(4, u64(pair[1]), u64(&byte), 1, 0, 0, 0, &written) == 0,
		'write reader payload')
	require_signal(written == 1, 'writer byte count')
	code := task.wait()
	require_signal(code == 0, 'restart read: ${code}')
}

fn ignore_on_worker() {
	require_signal(!os.is_main_thread(), 'signal_ignore worker identity')
	mut original := u32(0)
	require_signal(C.v_macos_kernel_bsd_call6(329, 3, 0, u64(&original), 0, 0, 0, unsafe { nil }) == 0,
		'read original worker mask')
	os.signal_ignore(.pipe, .urg)
	mut current := u32(0)
	require_signal(C.v_macos_kernel_bsd_call6(329, 3, 0, u64(&current), 0, 0, 0, unsafe { nil }) == 0,
		'read ignored worker mask')
	expected := original | (u32(1) << (u32(os.Signal.pipe) - 1)) | (u32(1) << (u32(os.Signal.urg) - 1))
	require_signal(current == expected, 'signal_ignore did not block worker signals')
}

fn require_public_signal_ignore() {
	require_signal(os.is_main_thread(), 'signal_ignore main identity')
	os.signal_ignore(.pipe)
	require_signal(C.v_macos_kernel_bsd_call6(37, u64(os.getpid()), u64(os.Signal.pipe), 0,
		0, 0, 0, unsafe { nil }) == 0, 'deliver ignored SIGPIPE to main')
	mut original := u32(0)
	require_signal(C.v_macos_kernel_bsd_call6(329, 3, 0, u64(&original), 0, 0, 0, unsafe { nil }) == 0,
		'read main mask')
	task := spawn ignore_on_worker()
	task.wait()
	mut current := u32(0)
	require_signal(C.v_macos_kernel_bsd_call6(329, 3, 0, u64(&current), 0, 0, 0, unsafe { nil }) == 0,
		'read main mask after worker')
	require_signal(current == original, 'signal_ignore changed the main thread mask')
}

fn main() {
	require_signal(bf_close(-1) == 9, 'seed BF errno')
	policy := C.v_macos_signal_policy_probe()
	require_signal(policy == 0, 'signal policy: ${policy}')
	require_signal(bf_last_errno() == 9, 'BF errno after policy')
	delivery := C.v_macos_signal_delivery_probe()
	require_signal(delivery == 0, 'signal delivery: ${delivery}')
	require_signal(bf_last_errno() == 9, 'BF errno after delivery')
	require_restarted_read_on_worker()
	require_public_signal_ignore()
	println('V Darwin signal delivery and C ABI ok')
}
