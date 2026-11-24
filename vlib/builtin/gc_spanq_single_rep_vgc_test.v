// vtest vflags: -gc vgc -d vgc_greentea
// vtest build: !race?
// Draining a single-object span whose scan shades a same-span child must not
// lose the child. The representative is marked scanned before its references
// are shaded, so a re-enqueued child keeps its own queue entry and the leaf
// stays marked.
module builtin

struct VGCSpanqNode {
mut:
	left  &VGCSpanqNode = unsafe { nil }
	right &VGCSpanqNode = unsafe { nil }
}

fn test_vgc_spanq_same_span_child_is_scanned() {
	$if vgc ? {
		gc_disable()
		leaf := &VGCSpanqNode{}
		mid := &VGCSpanqNode{
			left: leaf
		}
		root := &VGCSpanqNode{
			left:  mid
			right: &VGCSpanqNode{}
		}
		assert vgc_find_span(voidptr(root)) == vgc_find_span(voidptr(mid))
		assert vgc_find_span(voidptr(mid)) == vgc_find_span(voidptr(leaf))
		vgc_clear_mark_bits()
		vgc_shade(uint(root))
		vgc_drain_mark_work()
		leaf_span := vgc_find_span(voidptr(leaf))
		leaf_idx := u32((uint(leaf) - leaf_span.base) / uint(leaf_span.elem_size))
		assert C.vgc_bitmap_get(leaf_span.mark_bits, leaf_idx) != 0
		gc_enable()
	}
}
