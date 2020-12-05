struct Counter {
mut:
	val int
}

// if this is called more than once, the test'll fail
fn (mut c Counter) new_arr(msg string) []int {
	if c.val > 0 { panic(msg) }
	c.val++
	return [1, 3, 2]
}

fn test_array_eval_count() {
	// `new_arr()` should only be evaluated once, not on every iteration
	mut a1 := Counter{}
	assert a1.new_arr('map() failed').map(it * 2) == [2, 6, 4]

	mut a2 := Counter{}
	assert a2.new_arr('filter() failed').filter(it < 3) == [1, 2]
}
