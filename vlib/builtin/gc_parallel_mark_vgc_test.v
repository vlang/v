// vtest vflags: -gc vgc -d vgc_parallel_mark
// vtest build: !race?
// Parallel mark workers must not lose marks for objects whose mark bits
// share a byte.
module builtin

struct PMNode {
mut:
	left  &PMNode = unsafe { nil }
	right &PMNode = unsafe { nil }
	v     int
}

@[noinline]
fn build_pmnode(depth int) &PMNode {
	if depth == 0 {
		return &PMNode{ v: 1 }
	}
	return &PMNode{
		left:  build_pmnode(depth - 1)
		right: build_pmnode(depth - 1)
		v:     depth
	}
}

@[noinline]
fn count_pmnode(n &PMNode) int {
	if isnil(n) {
		return 0
	}
	return 1 + count_pmnode(n.left) + count_pmnode(n.right)
}

fn test_vgc_parallel_mark_keeps_live_tree() {
	$if vgc ? {
		root := build_pmnode(12)
		assert count_pmnode(root) == 8191
		gc_collect()
		assert vgc_heap.gc_nworkers > 1
		assert count_pmnode(root) == 8191
	}
}
