module main

import arena

struct Probe {
mut:
	ready   u32
	release u32
	value   u64
}

struct SelfProbe {
mut:
	handle  VlibcThread
	release u32
	done    u32
}

fn require(ok bool) {
	if !ok { exit(91) }
}

fn worker(argument u64) {
	mut probe := unsafe { &Probe(voidptr(argument)) }
	require(!arena.active())
	vlibc_store_u32(&probe.ready, 1)
	for vlibc_load_u32(&probe.release) == 0 {
	}
	mut local := arena.new(chunk_size: 128)
	local.push()
	pointer := unsafe { malloc(32) }
	require(local.owns(pointer))
	unsafe { pointer[31] = 93 }
	probe.value += u64(unsafe { pointer[31] })
	local.pop()
	local.free()
	require(!arena.active())
}

fn self_worker(argument u64) {
	mut probe := unsafe { &SelfProbe(voidptr(argument)) }
	for vlibc_load_u32(&probe.release) == 0 {
	}
	require(vlibc_thread_join(mut probe.handle) == .edeadlk)
	vlibc_store_u32(&probe.done, 1)
}

fn main() {
	mut parent := arena.new(chunk_size: 128)
	mut first := Probe{ value: 100 }
	mut second := Probe{ value: 200 }
	parent.push()
	before := parent.used()
	mut one, first_error := vlibc_thread_spawn(worker, u64(voidptr(&first)))
	mut two, second_error := vlibc_thread_spawn(worker, u64(voidptr(&second)))
	require(first_error == .enoerror && second_error == .enoerror)
	require(parent.used() == before && parent.is_current())
	parent.pop()
	parent.free()
	for vlibc_load_u32(&first.ready) == 0 || vlibc_load_u32(&second.ready) == 0 {
	}
	vlibc_store_u32(&first.release, 1)
	vlibc_store_u32(&second.release, 1)
	require(vlibc_thread_join(mut one) == .enoerror)
	require(vlibc_thread_join(mut two) == .enoerror)
	require(first.value == 193 && second.value == 293)
	$if macos {
		require(isnil(one.thread) && isnil(one.start) && one.done == 1)
		require(vlibc_thread_join(mut one) == .esrch)
		mut empty := VlibcThread{}
		require(vlibc_thread_join(mut empty) == .esrch)
		mut failed, failure := vlibc_thread_spawn(unsafe { VlibcThreadFn(nil) }, 0)
		require(failure == .einval && isnil(failed.thread) && isnil(failed.start))
		require(vlibc_thread_join(mut failed) == .esrch)
		mut self_probe := SelfProbe{}
		mut self, self_error := vlibc_thread_spawn(self_worker, u64(voidptr(&self_probe)))
		require(self_error == .enoerror)
		self_probe.handle = self
		vlibc_store_u32(&self_probe.release, 1)
		for vlibc_load_u32(&self_probe.done) == 0 {
		}
		require(vlibc_thread_join(mut self) == .enoerror)
	}
	for _ in 0 .. 32 {
		mut probe := Probe{ release: 1 }
		mut task, code := vlibc_thread_spawn(worker, u64(voidptr(&probe)))
		require(code == .enoerror)
		require(vlibc_thread_join(mut task) == .enoerror && probe.value == 93)
	}
	println('vlibc thread ownership passed')
}
