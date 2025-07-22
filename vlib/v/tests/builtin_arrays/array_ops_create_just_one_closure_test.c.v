struct C.builtin__closure__Closure {
	closure_cap int
}

fn setup(fname string) (int, int, []int) {
	println(fname)
	return unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap }, 42, []int{len: 5, init: index * 5}
}

fn test_array_filter() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.filter(fn [x] (i int) bool {
		println('x: ${x} | i: ${i}')
		return i < 20
	}))
	assert start_closure_cap - unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap } == 1
}

fn test_array_map() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.map(fn [x] (i int) int {
		println('x: ${x} | i: ${i}')
		return x + i
	}))
	assert start_closure_cap - unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap } == 1
}

fn test_array_any() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.any(fn [x] (i int) bool {
		println('x: ${x} | i: ${i}')
		return i < x
	}))
	assert start_closure_cap - unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap } == 1
}

fn test_array_all() {
	start_closure_cap, x, a := setup(@LOCATION)
	println(a.all(fn [x] (i int) bool {
		println('x: ${x} | i: ${i}')
		return i < x
	}))
	assert start_closure_cap - unsafe { &C.builtin__closure__Closure(voidptr(&C.g_closure)).closure_cap } == 1
}
