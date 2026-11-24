@[has_globals]
module main

import time

struct Probe {
mut:
	ready u32
	start u32
	done  u32
}

__global calls int

fn require(ok bool) { if !ok { panic('concurrent local time check failed') } }

fn loader(name string) ![]u8 {
	if name != 'Custom/Persistent' { return error('not this loader') }
	calls++
	return $embed_file('../test.tzif').to_bytes()
}

fn worker(address u64) {
	probe := unsafe { &Probe(voidptr(address)) }
	vlibc_store_u32(&probe.ready, 1)
	for vlibc_load_u32(&probe.start) == 0 {
	}
	for _ in 0 .. 1000 {
		local := time.unix_nanosecond(1_730_615_400, 456).local()
		require(local.hour == 1 && local.unix() == 1_730_615_400 && local.nanosecond == 456)
	}
	vlibc_store_u32(&probe.done, 1)
}

fn main() {
	time.register_zoneinfo_loader(loader)
	require(time.utc().unix() > 0 && calls == 0)
	mut probes := [4]Probe{}
	mut threads := []VlibcThread{cap: 4}
	for i in 0 .. 4 {
		thread, code := vlibc_thread_spawn(worker, u64(voidptr(&probes[i])))
		require(code == .enoerror)
		threads << thread
	}
	for i in 0 .. 4 {
		for vlibc_load_u32(&probes[i].ready) == 0 {
		}
	}
	for i in 0 .. 4 { vlibc_store_u32(&probes[i].start, 1) }
	for i in 0 .. 4 {
		require(vlibc_thread_join(mut threads[i]) == .enoerror)
		require(vlibc_load_u32(&probes[i].done) == 1)
	}
	require(calls == 1)
	println('concurrent local time ok')
}
