module main

import samename

// The export belongs to `main.sum` only. The checker used to look up
// `samename.sum` by its bare name as well, take it for a second export of
// `my_sum`, and reject the program.
@[export: 'my_sum']
fn sum(a int, b int) int {
	return a + b
}

fn test_module_fn_does_not_take_the_export_of_a_same_named_main_fn() {
	assert sum(1, 2) == 3
	assert samename.sum(1, 2) == 103
}
