import arrays

struct MembershipCalculator {
	rows [][]string = [['+', '-'], ['*', '÷']]
}

fn (app &MembershipCalculator) has_operator(op string) bool {
	return op in arrays.flatten(app.rows)
}

fn test_generic_array_call_membership() {
	app := MembershipCalculator{}
	assert app.has_operator('+')
	assert !app.has_operator('x')
	assert '÷' in arrays.flatten(app.rows)
	assert 'x' !in arrays.flatten(app.rows)
	assert arrays.flatten(app.rows).contains('*')
	assert !arrays.flatten(app.rows).contains('x')
	assert arrays.flatten(app.rows).index('÷') == 3
	assert arrays.flatten(app.rows).index('x') == -1
}
