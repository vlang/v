// vtest vflags: -gc vgc
// vtest build: !race?
@[has_globals]
module builtin

#include "@VEXEROOT/vlib/builtin/gc_stack_roots_vgc_test.h"

fn C.vgc_test_visit_stack_root(object voidptr, visit fn (uint, uint) int) int

__global vgc_test_stack_root = unsafe { nil }

fn vgc_test_check_spill_bounds(_ voidptr, regs voidptr) {
	mut lo := uint(0)
	mut hi := uint(0)
	if C.vgc_get_stack_bounds(&lo, &hi) == 0 {
		return
	}
	idx := C.vgc_get_cache_idx()
	vgc_refresh_stack_range_for_sp(idx, uint(regs))
	assert vgc_heap.caches[idx].stack_lo >= lo
	assert vgc_heap.caches[idx].stack_hi <= hi
}

fn test_register_spills_keep_physical_stack_bounds() {
	$if vgc ? {
		C.vgc_call_with_registers_spilled(vgc_test_check_spill_bounds, unsafe { nil })
	}
}

fn vgc_test_mark_stack_window(lo uint, hi uint) int {
	mut stack_lo := uint(0)
	mut stack_hi := uint(0)
	if C.vgc_get_stack_bounds(&stack_lo, &stack_hi) == 0 {
		return 1
	}
	vgc_clear_mark_bits()
	// Only the supplied window is a root: do not scan the globals or other stack
	// words that might contain another copy of the object's address.
	vgc_scan_stack_range(lo, hi, stack_lo, stack_hi, C.vgc_current_fake_stack())
	vgc_drain_mark_work()
	span := vgc_find_span(vgc_test_stack_root)
	assert span != unsafe { nil }
	index := u32((uint(vgc_test_stack_root) - span.base) / uint(span.elem_size))
	return C.vgc_bitmap_get(span.mark_bits, index)
}

fn test_live_local_in_asan_fake_frame_is_a_root() {
	$if vgc ? {
		vgc_test_stack_root = vgc_malloc_noscan(200)
		assert C.vgc_test_visit_stack_root(vgc_test_stack_root, vgc_test_mark_stack_window) == 1
		vgc_test_stack_root = unsafe { nil }
	}
}
