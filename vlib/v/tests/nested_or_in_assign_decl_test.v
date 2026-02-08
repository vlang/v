module main

fn fx1() ?int {
	return none
}

fn fx2() ?int {
	return none
}

fn fx3() ?int {
	return none
}

fn test_nested_or_in_assign_decl() {
	x1 := fx1()
	x2 := fx2()
	x3 := fx3()
	def := 123

	y := x1 or { x2 or { x3 or { def } } }
	assert y == 123
}
