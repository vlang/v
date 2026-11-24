module main

import arena
import os

$if gcboehm ? {
	#flag -I @VEXEROOT/thirdparty/libgc/include
	#flag -I @VMODROOT/vlib/builtin/testdata/gc_weak_reference
	#flag @VMODROOT/vlib/builtin/testdata/gc_weak_reference/probe.o
	#include "probe.h"
	fn C.weak_test_initialize()
	fn C.weak_test_track(voidptr) i32
	fn C.weak_test_keep_strong(voidptr)
	fn C.weak_test_clear_strong()
	fn C.weak_test_collect() i32
	fn C.weak_test_non_gc() voidptr
}

struct Payload {
	marker  u64
	padding [7]u64
}

struct Handles {
	survivor GCWeakReference
	expired  GCWeakReference
	copies   []GCWeakReference
}

fn require(ok bool, message string) {
	if !ok { panic(message) }
}

fn target(marker u64) voidptr {
	value := voidptr(unsafe { vcalloc_heap(int(sizeof(Payload))) })
	unsafe { *(&Payload(value)) = Payload{ marker: marker } }
	return value
}

fn prepare() Handles {
	$if gcboehm ? {
		kept := target(173)
		dropped := target(241)
		require(C.weak_test_track(kept) == 0 && C.weak_test_track(dropped) == 0, 'targets must be GC bases')
		C.weak_test_keep_strong(kept)
		// Both the target and weak cell must bypass this ephemeral allocator.
		mut scratch := arena.new()
		scratch.push()
		survivor := unsafe { gc_weak_reference(kept) } or { panic('weak construction failed') }
		expired := unsafe { gc_weak_reference(dropped) } or { panic('weak construction failed') }
		independent := unsafe { gc_weak_reference(kept) } or { panic('second weak construction failed') }
		scratch.pop()
		scratch.free()
		mut copies := []GCWeakReference{cap: 1}
		copies << survivor
		for _ in 0 .. 128 { copies << survivor }
		copies << independent
		retained := copies.clone()
		return Handles{ survivor: survivor, expired: expired, copies: retained }
	} $else {
		panic('Boehm required')
	}
}

fn check_live(handles &Handles) int {
	mut count := 0
	for weak in handles.copies {
		pointer := weak.get()
		require(pointer != unsafe { nil }, 'strongly rooted target expired')
		require(unsafe { (&Payload(pointer)).marker } == 173, 'weak pointer changed identity/value')
		count++
	}
	return count
}

fn hold_acquired(weak GCWeakReference, ready chan bool, release chan bool) bool {
	pointer := weak.get()
	require(pointer != unsafe { nil }, 'rooted target missing before acquisition')
	ready <- true
	_ := <-release
	require(unsafe { (&Payload(pointer)).marker } == 173, 'acquired strong pointer did not survive collection')
	return true
}

fn lifetime_case() {
	$if gcboehm ?&& !gcboehm_leak ? {
		C.weak_test_initialize()
		task := spawn prepare()
		handles := task.wait()
		require(C.weak_test_collect() == 1, 'unrooted target must finalize without retaining weak cell')
		require(handles.expired.get() == unsafe { nil }, 'expired target must disappear')
		checked := spawn check_live(&handles)
		require(checked.wait() == 130, 'moved/copied/independent handles must all resolve')
		ready := chan bool{cap: 1}
		release := chan bool{cap: 1}
		holder := spawn hold_acquired(handles.survivor, ready, release)
		require(<-ready, 'strong acquisition worker did not start')
		C.weak_test_clear_strong()
		require(C.weak_test_collect() == 1, 'acquired strong pointer was reclaimed while held by a worker')
		release <- true
		require(holder.wait(), 'strong acquisition worker failed')
		require(C.weak_test_collect() == 2, 'weak handles retained target after the strong root was dropped')
		require(handles.survivor.get() == unsafe { nil }, 'survivor must now expire')
		for weak in handles.copies {
			require(weak.get() == unsafe { nil }, 'copied weak handle did not expire')
		}
		println('lifetime: two finalized, 130 copied/moved/independent handles expired')
	}
}

fn rejected(value voidptr) bool {
	unsafe {
		if _ := gc_weak_reference(value) { return false }
	}
	return true
}

fn rejection_case() {
	$if gcboehm ?&& !gcboehm_leak ? {
		require(rejected(unsafe { nil }), 'accepted nil')
		require(rejected(C.weak_test_non_gc()), 'accepted static non-GC address')
		allocation := unsafe { vcalloc_heap(64) }
		require(rejected(unsafe { allocation + 1 }), 'accepted interior pointer')
		mut scratch := arena.new()
		scratch.push()
		item := unsafe { vcalloc(64) }
		require(rejected(item), 'accepted arena payload')
		scratch.pop()
		scratch.free()
		println('rejection: nil/static/interior/arena')
	}
}

fn unsupported_case() {
	value := target(173)
	defer { unsafe { vfree_heap(value) } }
	require(rejected(value), 'unsupported mode accepted weak reference')
	require(GCWeakReference{}.get() == unsafe { nil }, 'unsupported zero handle not empty')
	println('unsupported: no weak reference, zero handle empty')
}

fn find_leak_case() {
	$if gcboehm ? {
		C.GC_set_find_leak(1)
		unsupported_case()
	}
}

fn main() {
	require(GCWeakReference{}.get() == unsafe { nil }, 'zero handle not empty')
	match os.args[1] {
		'lifetime' { lifetime_case() }
		'rejection' { rejection_case() }
		'unsupported' { unsupported_case() }
		'find-leak' { find_leak_case() }
		else { panic('unknown weak-reference case') }
	}
}
