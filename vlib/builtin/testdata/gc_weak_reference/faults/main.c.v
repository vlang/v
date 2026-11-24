module main

import os

#flag -I @VEXEROOT/thirdparty/libgc/include
#flag -I @VMODROOT/vlib/builtin/testdata/gc_weak_reference
#flag -I @VMODROOT/vlib/builtin/testdata/gc_weak_reference/faults
#flag @VMODROOT/vlib/builtin/testdata/gc_weak_reference/probe.o
#flag @VMODROOT/vlib/builtin/testdata/gc_weak_reference/faults/wrap.o
#flag -Wl,--wrap=GC_malloc_atomic,--wrap=GC_general_register_disappearing_link,--wrap=GC_free
#include "probe.h"
#include "wrap.h"

fn C.weak_test_initialize()
fn C.weak_test_track(voidptr) i32
fn C.weak_test_collect() i32
fn C.weak_fault_begin(i32)
fn C.weak_fault_end()
fn C.weak_fault_atomic_calls() i32
fn C.weak_fault_registration_calls() i32
fn C.weak_fault_free_calls() i32

fn require(ok bool, message string) {
	if !ok { panic(message) }
}

fn attempt(kind i32) bool {
	target := voidptr(unsafe { vcalloc_heap(64) })
	require(C.weak_test_track(target) == 0, 'target must be a GC object base')
	C.weak_fault_begin(kind)
	unsafe {
		if _ := gc_weak_reference(target) { panic('faulted weak construction succeeded') }
	}
	C.weak_fault_end()
	require(C.weak_fault_atomic_calls() == 1, 'atomic allocation interception did not run exactly once')
	expected := if kind == 1 { i32(0) } else { i32(1) }
	require(C.weak_fault_registration_calls() == expected, 'registration interception count incorrect')
	require(C.weak_fault_free_calls() == 0, 'weak constructor explicitly freed a link container')
	return true
}

fn main() {
	C.weak_test_initialize()
	kind := i32(os.args[1].int())
	require(kind >= 1 && kind <= 3, 'unknown fault case')
	worker := spawn attempt(kind)
	require(worker.wait(), 'fault worker failed')
	require(C.weak_test_collect() == 1, 'failed weak construction kept target alive')
	println('fault ${kind}: none, no explicit cell free, target finalized')
}
