// vtest vflags: -gc vgc
// GreenTea P0 baseline: object-queue density proxy counters move on a
// pointer-heavy heap and stay consistent (puts >= gets, scanned > 0).

struct GTree {
mut:
	left  &GTree = unsafe { nil }
	right &GTree = unsafe { nil }
	val   int
}

@[noinline]
fn build_gtree(depth int) &GTree {
	if depth == 0 {
		return &GTree{ val: 1 }
	}
	return &GTree{
		left:  build_gtree(depth - 1)
		right: build_gtree(depth - 1)
		val:   depth
	}
}

@[noinline]
fn live_gtree(n &GTree) int {
	if isnil(n) {
		return 0
	}
	return 1 + live_gtree(n.left) + live_gtree(n.right)
}

fn test_vgc_mark_stats_moves_with_live_heap() {
	gc_collect()
	before := vgc_mark_stats()
	assert before.gc_cycles >= 0
	// Keep a live tree so the next collection must scan it.
	root := build_gtree(12)
	assert live_gtree(root) == 8191
	gc_collect()
	after := vgc_mark_stats()
	assert after.gc_cycles == before.gc_cycles + 1
	assert after.work_puts > before.work_puts
	assert after.work_gets > before.work_gets
	assert after.objs_scanned > before.objs_scanned
	assert after.spans_visited > before.spans_visited
	assert after.work_puts >= after.work_gets
	assert live_gtree(root) == 8191
}
