import arrays

struct MembershipCalculator {
	rows [][]string = [['+', '-'], ['*', '÷']]
}

enum MembershipSign {
	plus
	minus
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

fn membership_operator(mut calls []string) string {
	calls << 'needle'
	return '+'
}

fn membership_rows[T](mut calls []string, rows [][]T) [][]T {
	calls << 'rows'
	return rows
}

fn test_generic_array_membership_stages_concrete_needles_in_source_order() {
	app := MembershipCalculator{}
	mut calls := []string{}
	assert membership_operator(mut calls) in arrays.flatten(app.rows)
	assert calls == ['needle']
	calls.clear()
	assert membership_operator(mut calls) in arrays.flatten(membership_rows(mut calls, app.rows))
	assert calls == ['needle', 'rows']
	calls.clear()
	assert membership_operator(mut calls) !in arrays.flatten(membership_rows(mut calls, [['x']]))
	assert calls == ['needle', 'rows']
	calls.clear()
	assert arrays.flatten(membership_rows(mut calls, app.rows)).contains(membership_operator(mut calls))
	assert calls == ['rows', 'needle']
}

fn test_generic_enum_array_membership_uses_concrete_element_type() {
	rows := [[MembershipSign.plus], [MembershipSign.minus, MembershipSign.plus]]
	assert arrays.flatten(rows).contains(.minus)
	assert arrays.flatten(rows).index(.plus) == 0
	assert arrays.flatten(rows).last_index(.plus) == 2
}
