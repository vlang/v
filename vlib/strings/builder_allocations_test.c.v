// vtest vflags: -gc none -autofree -d track_heap -nocache
import strings

#include "@VEXEROOT/vlib/strings/testdata/builder_heap_tracking.h"

fn C.builder_heap_start()
fn C.builder_heap_stop() int
fn C.builder_heap_allocation_count() int

fn test_swap_and_empty_transfer_do_not_allocate_or_leak() {
	C.builder_heap_start()
	mut source := strings.new_builder(16)
	mut target := strings.new_builder(4)
	source.write_string('abcdef')
	target.write_string('xy')
	source_data := source.data
	target_data := target.data
	before := C.builder_heap_allocation_count()
	source.swap(mut target)
	assert source.data == target_data
	assert target.data == source_data
	source.clear()
	target.transfer_to(mut source)
	assert source.data == source_data
	assert target.data == target_data
	assert source.len == 6
	assert target.len == 0
	after := C.builder_heap_allocation_count()
	// Both exchanged allocations are still independently owned.
	unsafe {
		source.free()
		target.free()
	}
	live := C.builder_heap_stop()
	assert before > 0
	assert after == before
	assert live == 0
}

// Release the intentionally aliased storage explicitly after detaching it.
@[manualfree]
fn check_aliased_transfer_cleanup(capacity int) {
	C.builder_heap_start()
	mut target := strings.new_builder(capacity)
	target.write_string('abcdef')
	mut source := strings.new_builder(0)
	// Make a real shared header even under autofree, which clones array assignments.
	unsafe { vmemcpy(&source, &target, int(sizeof(strings.Builder))) }
	source.drop(1)
	source.transfer_to(mut target)
	// Capacity 6 grows during transfer; capacity 64 grows only after transfer.
	for _ in 0 .. 100 {
		target.write_string('more')
	}
	assert target.len == 411
	assert target[0] == `a`
	assert target[6] == `b`
	assert target[10] == `f`
	source.write_string('new')
	// All allocations in this measured scope belong to these two builders.
	unsafe {
		source.free()
		target.free()
	}
	live := C.builder_heap_stop()
	assert live == 0
}

fn test_aliased_transfer_releases_buffers_during_and_after_growth() {
	check_aliased_transfer_cleanup(6)
	check_aliased_transfer_cleanup(64)
}
