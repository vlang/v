// vtest vflags: -gc vgc
// vtest build: !race?
@[has_globals]
module builtin

@[aligned: 512]
struct VgcGlobalAlignedCell {
	value string
}

__global vgc_global_aligned_cells = &[1]VgcGlobalAlignedCell{init: VgcGlobalAlignedCell{
	value: 'managed field'.repeat(16)
}}

fn test_global_aligned_array_is_scanned_by_vgc() {
	$if vgc ? {
		assert usize(voidptr(vgc_global_aligned_cells)) % 512 == 0
		span := vgc_find_span(voidptr(vgc_global_aligned_cells))
		assert span != unsafe { nil }
		assert !span.noscan
		vgc_gc_start()
		assert unsafe { vgc_global_aligned_cells[0].value } == 'managed field'.repeat(16)
	}
}

fn test_global_aligned_array_is_rooted_without_stack_references() {
	$if vgc ? {
		// Mark only global roots so cached stack values cannot retain the array.
		registered_threads := vgc_heap.ncaches
		vgc_heap.ncaches = 0
		vgc_clear_mark_bits()
		vgc_mark_roots()
		vgc_heap.ncaches = registered_threads
		vgc_drain_mark_work()
		assert vgc_test_object_is_marked(voidptr(vgc_global_aligned_cells))
		assert vgc_test_object_is_marked(unsafe { vgc_global_aligned_cells[0].value.str })
	}
}

fn vgc_test_object_is_marked(value voidptr) bool {
	$if vgc ? {
		span := vgc_find_span(value)
		if span == unsafe { nil } || span.mark_bits == unsafe { nil } {
			return false
		}
		index := (usize(value) - span.base) / usize(span.elem_size)
		return unsafe { span.mark_bits[index / 8] } & u8(1 << (index % 8)) != 0
	}
	return false
}
