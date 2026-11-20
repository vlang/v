// vtest vflags: -gc vgc
// vtest build: !race?
// Shared mark-phase counters are updated by several threads at once, so they
// must use atomic adds. Four threads adding one million each must total four
// million; a plain volatile read-modify-write loses updates.
@[has_globals]
module builtin

__global vgc_counter_test_total = u64(0)

fn vgc_counter_test_add(n int) int {
	for _ in 0 .. n {
		C.vgc_counter_add_atomic(&vgc_counter_test_total, u64(1))
	}
	return n
}

fn test_vgc_shared_counter_add_is_atomic() {
	$if vgc ? {
		vgc_counter_test_total = 0
		mut threads := []thread int{}
		for _ in 0 .. 4 {
			threads << spawn vgc_counter_test_add(1000000)
		}
		totals := threads.wait()
		assert totals == [1000000, 1000000, 1000000, 1000000]
		assert C.vgc_counter_load(&vgc_counter_test_total) == 4000000
	}
}
