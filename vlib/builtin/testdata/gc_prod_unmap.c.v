module main

fn C.GC_expand_hp(usize) int
fn C.GC_set_force_unmap_on_gcollect(int)

fn main() {
	// Grow free heap space without retaining an allocated object's address on the stack.
	if C.GC_expand_hp(32 * 1024 * 1024) == 0 {
		panic('could not expand GC heap')
	}
	C.GC_set_force_unmap_on_gcollect(1)
	for _ in 0 .. 3 {
		gc_collect()
	}
	if gc_heap_usage().unmapped_bytes == 0 {
		panic('production bundled GC did not unmap free heap pages')
	}
}
