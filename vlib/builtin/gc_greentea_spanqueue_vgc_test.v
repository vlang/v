// vtest vflags: -gc vgc -d vgc_greentea
// vtest build: !race?
// GreenTea P1: span queue must batch several objects in one span pass on a
// high-fanout tree. With the old object queue, spans_visited ~ objs_scanned.
// With the span queue, multi_spans > 0 and objs_scanned > span_dequeues.

struct GSNode {
mut:
	left  &GSNode = unsafe { nil }
	right &GSNode = unsafe { nil }
	val   int
}

@[noinline]
fn build_gsnode(depth int) &GSNode {
	if depth == 0 {
		return &GSNode{ val: 1 }
	}
	return &GSNode{
		left:  build_gsnode(depth - 1)
		right: build_gsnode(depth - 1)
		val:   depth
	}
}

@[noinline]
fn count_gsnode(n &GSNode) int {
	if isnil(n) {
		return 0
	}
	return 1 + count_gsnode(n.left) + count_gsnode(n.right)
}

fn test_vgc_greentea_span_batching() {
	gc_collect()
	before := vgc_mark_stats()
	root := build_gsnode(13)
	assert count_gsnode(root) == 16383
	gc_collect()
	after := vgc_mark_stats()
	assert after.gc_cycles == before.gc_cycles + 1
	assert after.span_enqueues > before.span_enqueues
	assert after.span_dequeues > before.span_dequeues
	assert after.multi_spans > before.multi_spans
	assert after.objs_scanned > before.objs_scanned
	// Density: more than one object per span dequeue on this tree.
	assert after.objs_scanned - before.objs_scanned > after.span_dequeues - before.span_dequeues
	assert count_gsnode(root) == 16383
}
