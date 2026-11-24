// vtest vflags: -gc vgc
// vtest build: !race?
// A pointer at word 64 or later cannot be named in the 64-bit pointer map:
// shifting by its offset is undefined behavior and the child would be missed.
// Such structs must fall back to conservative scanning so both children stay
// marked. The padding is omitted from the literal so the precise path is
// attempted.
module builtin

struct VGCWideLeaf {
	v int
}

struct VGCWide {
	first &VGCWideLeaf
	pad   [64]u64
	last  &VGCWideLeaf
}

fn test_vgc_wide_struct_children_are_marked() {
	$if vgc ? {
		gc_disable()
		w := &VGCWide{
			first: &VGCWideLeaf{ v: 1 }
			last:  &VGCWideLeaf{ v: 2 }
		}
		vgc_clear_mark_bits()
		vgc_shade(uint(w))
		vgc_drain_mark_work()
		for child in [w.first, w.last] {
			child_span := vgc_find_span(voidptr(child))
			child_idx := u32((uint(child) - child_span.base) / uint(child_span.elem_size))
			assert C.vgc_bitmap_get(child_span.mark_bits, child_idx) != 0
		}
		gc_enable()
	}
}
