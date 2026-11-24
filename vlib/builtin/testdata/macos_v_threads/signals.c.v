module main

import arena
import builtin.testdata.macos_raw_signal as raw_signal
import sync.stdatomic

fn C.atomic_load_u32(voidptr) u32
fn C.atomic_store_u32(voidptr, u32)

fn C.v_bare_thread_create(fn (voidptr) voidptr, voidptr, uint, i32, &i32) voidptr

struct Record {
mut:
	ready    u32
	received u32
	port     u32
}

__global thread_signal_hits u64
__global thread_signal_failures u64

fn require_signal(condition bool) {
	if !condition { exit(102) }
}

@[no_profile; no_trace]
fn thread_signal_handler(number i32, _info voidptr, _context voidptr) {
	// These are native TLS reads. The creator keeps its own Arena active and
	// workers start with a fresh Arena slot and seed their own BF errno with EBADF.
	if number != 30 || arena.active() || bf_last_errno() != 9 {
		stdatomic.add_u64(&thread_signal_failures, 1)
	}
	stdatomic.add_u64(&thread_signal_hits, 1)
}

fn signal_worker(argument voidptr) voidptr {
	mut record := unsafe { &Record(argument) }
	if arena.active() || bf_close(-1) != 9 { stdatomic.add_u64(&thread_signal_failures, 1) }
	record.port = bf_mach_thread_self()
	C.atomic_store_u32(&record.ready, 1)
	bf_wake_u32(&record.ready, true)
	for C.atomic_load_u32(&record.received) == 0 { bf_wait_u32(&record.received, 0, 1000000) }
	return unsafe { nil }
}

fn main() {
	mut region := arena.new()
	region.push()
	require_signal(region.is_current())
	require_signal(raw_signal.install_usr1(thread_signal_handler, false) == 0)
	old_mask, mask_error := raw_signal.mask()
	require_signal(mask_error == 0)
	new_mask := old_mask & ~(u32(1) << 29)
	require_signal(raw_signal.set_mask(new_mask) == 0)
	mut records := [32]Record{}
	for i in 0 .. records.len {
		mut record := &records[i]
		mut error_code := i32(-1)
		require_signal(C.v_bare_thread_create(signal_worker, record, 0, 1, &error_code) == unsafe { nil } && error_code == 0)
		for C.atomic_load_u32(&record.ready) == 0 { bf_wait_u32(&record.ready, 0, 1000000) }
		before := stdatomic.load_u64(&thread_signal_hits)
		for stdatomic.load_u64(&thread_signal_hits) == before {
			require_signal(raw_signal.send_usr1(record.port) == 0)
		}
		C.atomic_store_u32(&record.received, 1)
		bf_wake_u32(&record.received, true)
		for _ in 0 .. 128 {
			code := raw_signal.send_usr1(record.port)
			if code == 3 { break }
			require_signal(code == 0)
		}
		for {
			kind, status := bf_mach_port_type(record.port)
			require_signal(status == 0)
			if kind & u32(0x100000) != 0 { break }
		}
		require_signal(bf_mach_port_deallocate(record.port) == 0)
		require_signal(region.is_current())
	}
	after_mask, after_error := raw_signal.mask()
	require_signal(after_error == 0 && after_mask == new_mask)
	require_signal(raw_signal.set_mask(old_mask) == 0)
	require_signal(stdatomic.load_u64(&thread_signal_hits) >= 32)
	require_signal(stdatomic.load_u64(&thread_signal_failures) == 0)
	region.pop()
	region.free()
	println('macOS V thread signal teardown passed')
}
