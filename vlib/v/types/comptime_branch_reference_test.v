module types

fn test_reading_or_writing_a_name_is_a_reference() {
	assert code_references_ident('println(x)', 'x')
	assert code_references_ident('x = 1', 'x')
	assert code_references_ident('x++', 'x')
	assert code_references_ident('y := x + 1', 'x')
	assert code_references_ident('for x < 3 {', 'x')
	assert code_references_ident(r'${x}', 'x')
}

fn test_a_member_or_a_field_label_is_not_a_reference() {
	assert !code_references_ident('cfg.x', 'x')
	assert !code_references_ident('a.b.x', 'x')
	assert !code_references_ident('match e { .x {} }', 'x')
	assert !code_references_ident('_ = Config{ x: 1 }', 'x')
	assert !code_references_ident('x: for {}', 'x')
}

fn test_a_new_binding_is_not_a_reference() {
	assert !code_references_ident('x := 1', 'x')
	assert !code_references_ident('mut x := 1', 'x')
	assert !code_references_ident('a, x := pair()', 'x')
	assert !code_references_ident('x, b := pair()', 'x')
	assert !code_references_ident('mut a, mut x := pair()', 'x')
	assert !code_references_ident('for x in list {', 'x')
	assert !code_references_ident('for i, x in list {', 'x')
	assert !code_references_ident('for mut x in list {', 'x')
}

fn test_a_longer_name_that_merely_contains_the_searched_one() {
	assert !code_references_ident('println(x_ray)', 'x')
	assert !code_references_ident('println(prefix)', 'fix')
	assert !code_references_ident('', 'x')
	assert !code_references_ident('println(x)', '')
}

fn test_a_number_does_not_turn_the_next_name_into_a_member() {
	assert code_references_ident('println(1.5 + x)', 'x')
	assert code_references_ident('for i in 0 .. x {', 'x')
}
