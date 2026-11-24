// vtest vflags: -gc vgc
// vtest build: !race?
// Equal-sized structs with different pointer offsets share a size-class span.
// The span must not keep the first map; it must fall back to conservative
// scanning so every live child stays marked.
module builtin

struct VGCMixedLeaf {
	v int
}

struct VGCMixedFront {
	child &VGCMixedLeaf
	pad   [31]u64
}

struct VGCMixedBack {
	pad   [31]u64
	child &VGCMixedLeaf
}

fn test_vgc_mixed_sizeclass_child_is_marked() {
	$if vgc ? {
		gc_disable()
		front := &VGCMixedFront{
			child: &VGCMixedLeaf{ v: 11 }
		}
		back := &VGCMixedBack{
			child: &VGCMixedLeaf{ v: 22 }
		}
		assert vgc_find_span(voidptr(front)) == vgc_find_span(voidptr(back))
		vgc_clear_mark_bits()
		vgc_shade(uint(back))
		vgc_drain_mark_work()
		child_span := vgc_find_span(voidptr(back.child))
		child_idx := u32((uint(back.child) - child_span.base) / uint(child_span.elem_size))
		assert C.vgc_bitmap_get(child_span.mark_bits, child_idx) != 0
		gc_enable()
	}
}
