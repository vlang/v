fn test_for_c_init_closure_survives_body_tail_cleanup() {
	base := 40
	mut i := 0
	mut values := []int{}
	for h := fn [base] (x int) int {
		return base + x}; i < 3; i++ {
		values << h(i)
	}
	assert values == [40, 41, 42]
}

fn test_for_c_init_closure_survives_continue_cleanup() {
	base := 50
	mut i := 0
	mut values := []int{}
	for h := fn [base] (x int) int {
		return base + x}; i < 3; i++ {
		if i == 0 {
			continue
		}
		values << h(i)
	}
	assert values == [51, 52]
}

fn test_multi_for_c_init_closure_survives_body_tail_cleanup() {
	base := 60
	mut values := []int{}
	for h, i := fn [base] (x int) int {
		return base + x}, 0; i < 3; i++ {
		values << h(i)
	}
	assert values == [60, 61, 62]
}

fn test_nested_for_c_init_closure_continue_outer() {
	outer_base := 70
	mut i := 0
	mut values := []int{}
	continue_outer: for outer_h := fn [outer_base] (x int) int {
		return outer_base + x}; i < 2; i++ {
		inner_base := 80
		mut j := 0
		for inner_h := fn [inner_base] (x int) int {
			return inner_base + x}; j < 1; j++ {
			values << outer_h(i)
			values << inner_h(j)
			continue continue_outer
		}
	}
	assert values == [70, 80, 71, 80]
}

fn test_nested_for_c_init_closure_break_outer() {
	outer_base := 90
	mut i := 0
	mut values := []int{}
	break_outer: for outer_h := fn [outer_base] (x int) int {
		return outer_base + x}; i < 2; i++ {
		inner_base := 100
		mut j := 0
		for inner_h := fn [inner_base] (x int) int {
			return inner_base + x}; j < 1; j++ {
			values << outer_h(i)
			values << inner_h(j)
			break break_outer
		}
	}
	assert values == [90, 100]
}
