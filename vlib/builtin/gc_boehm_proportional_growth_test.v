// vtest vflags: -gc boehm -d gc_proportional_growth
// vtest build: !race?
// A live heap that only grows used to get one full collection per 8 MiB of
// growth (#32272). With -d gc_proportional_growth, the Boehm helper grows the
// heap in proportion to its size when free space runs low.

@[noinline]
fn grow_live_heap_for_gc_test() u64 {
	mut blocks := [][]u8{}
	for _ in 0 .. 64 {
		blocks << []u8{len: 1024 * 1024, init: u8(1)}
	}
	mut sum := u64(0)
	for b in blocks {
		sum += u64(b.len) + u64(b[0])
	}
	return sum
}

fn test_proportional_growth_expands_the_heap() {
	before := v_gc_expansion_count_report()
	assert grow_live_heap_for_gc_test() > 0
	after := v_gc_expansion_count_report()
	assert after > before
}
