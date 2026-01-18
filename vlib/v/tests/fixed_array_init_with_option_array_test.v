module main

fn test_fixed_array_init_with_option_array() {
	z := [2][]?int{init: []?int{len: 3}}
	assert z.str() == '[[Option(none), Option(none), Option(none)], [Option(none), Option(none), Option(none)]]'
}
