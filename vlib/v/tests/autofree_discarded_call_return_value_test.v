// vtest vflags: -autofree
@[has_globals]
module main

__global (
	freed_sum int
)

struct Test {
	a int
}

fn (t &Test) free() {
	unsafe {
		freed_sum += t.a
	}
}

fn make_test(a int) Test {
	return Test{a}
}

fn run_autofree_discarded_return_case() {
	a := make_test(1)
	make_test(2)
	_ = a
}

fn test_autofree_frees_discarded_call_return_value() {
	unsafe {
		freed_sum = 0
	}
	run_autofree_discarded_return_case()
	assert unsafe { freed_sum } == 3
}
