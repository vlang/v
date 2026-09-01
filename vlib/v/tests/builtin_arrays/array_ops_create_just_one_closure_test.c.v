@[has_globals]
module main

struct C.builtin__closure__Closure {
	closure_cap int
}

__global C.g_closure voidptr

fn setup(fname string) (int, int, []int) {
	println(fname)
	return unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap }, 42, []int{len: 5, init: index * 5}
}

fn assert_at_most_one_new_closure(start_closure_cap int) {
	closure_cap := unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap }
	// V3 reclaims array callbacks at function exit, so later tests can reuse the
	// first test's slot without consuming another slot from the current page.
	assert start_closure_cap - closure_cap in [0, 1]
}

fn test_array_filter() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.filter(fn [x] (i int) bool {
		println('x: ${x} | i: ${i}')
		return i < 20
	}))
	assert_at_most_one_new_closure(start_closure_cap)
}

fn test_array_map() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.map(fn [x] (i int) int {
		println('x: ${x} | i: ${i}')
		return x + i
	}))
	assert_at_most_one_new_closure(start_closure_cap)
}

fn test_array_any() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.any(fn [x] (i int) bool {
		println('x: ${x} | i: ${i}')
		return i < x
	}))
	assert_at_most_one_new_closure(start_closure_cap)
}

fn test_array_all() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.all(fn [x] (i int) bool {
		println('x: ${x} | i: ${i}')
		return i < x
	}))
	assert_at_most_one_new_closure(start_closure_cap)
}
