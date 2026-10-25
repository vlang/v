// vtest vflags: -gc vgc
// vtest build: !race?
// vgc takes every word in the writable data of the executable for a possible pointer into its
// heap. Its own globals are in there too, but none of their words is a pointer that the program
// holds: they are the bounds of the stacks and of the arenas, and the spans of the caches. A stack
// bound kept an arena that the system had put right above the main stack alive.
@[has_globals]
module builtin

__global vgc_test_root = unsafe { nil }

// vgc_test_marked_from_data marks from the data segments and the global arrays alone, without
// the stacks, and reports whether that marks the object at p.
fn vgc_test_marked_from_data(p voidptr) bool {
	vgc_clear_mark_bits()
	vgc_mark_data_segments()
	C.v3_vgc_mark_global_arrays()
	vgc_drain_mark_work()
	return vgc_test_is_marked(p)
}

fn vgc_test_is_marked(p voidptr) bool {
	span := vgc_find_span(p)
	if span == unsafe { nil } || span.mark_bits == unsafe { nil } {
		return false
	}
	index := u32((uint(p) - span.base) / uint(span.elem_size))
	return C.vgc_bitmap_get(span.mark_bits, index) != 0
}

fn test_a_global_of_the_program_is_a_root() {
	$if vgc ? {
		p := vgc_malloc_noscan(200)
		vgc_test_root = p
		assert vgc_test_marked_from_data(p)
		vgc_test_root = unsafe { nil }
		assert !vgc_test_marked_from_data(p)
	}
}

fn test_the_globals_of_vgc_are_not_roots() {
	$if vgc ? {
		p := vgc_malloc_noscan(200)
		// The cache slot of a thread that does not exist.
		slot := vgc_max_threads - 1
		assert !vgc_heap.caches[slot].registered
		vgc_heap.caches[slot].stack_base = uint(p)
		vgc_heap.caches[slot].stack_hi = uint(p)
		marked := vgc_test_marked_from_data(p)
		vgc_heap.caches[slot].stack_base = 0
		vgc_heap.caches[slot].stack_hi = 0
		assert !marked
	}
}

fn test_the_tiny_block_of_a_thread_is_a_root() {
	$if vgc ? {
		// A pointer-free object of less than 16 bytes goes into the thread's tiny block, which the
		// thread keeps filling with the next ones.
		p := vgc_malloc_noscan(4)
		idx := C.vgc_get_cache_idx()
		tiny := vgc_heap.caches[idx].tiny
		assert tiny != 0
		assert uint(p) >= tiny && uint(p) < tiny + vgc_tiny_size
		// Mark from all the roots, without this thread's stack.
		lo := vgc_heap.caches[idx].stack_lo
		hi := vgc_heap.caches[idx].stack_hi
		vgc_heap.caches[idx].stack_lo = 0
		vgc_heap.caches[idx].stack_hi = 0
		vgc_clear_mark_bits()
		vgc_mark_roots()
		vgc_heap.caches[idx].stack_lo = lo
		vgc_heap.caches[idx].stack_hi = hi
		vgc_drain_mark_work()
		assert vgc_test_is_marked(voidptr(tiny))
	}
}

fn test_the_heap_is_far_from_small_numbers() {
	$if vgc ? {
		$if windows && x64 {
			// Windows hands out the lowest free addresses first: with TinyCC, whose executables
			// start at 4 MB, the heap started at about 9 MB, where words in the data segment
			// that hold numbers like 0x2000537 point. The arenas come from the top now.
			assert vgc_arena_lo > uint(1) << 32
		}
	}
}
