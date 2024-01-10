fn cross_assign_anon_fn_one(a int, b bool) string {
	return 'one'
}

fn cross_assign_anon_fn_two(a int, b bool) string {
	return 'two'
}

fn cross_assign_anon_fn_three() (string, string) {
	return 'three', 'three'
}

fn cross_assign_anon_fn_four() (string, string) {
	return 'four', 'four'
}

fn cross_assign_anon_fn_five(a ...int) string {
	return 'five'
}

fn cross_assign_anon_fn_six(a ...int) string {
	return 'six'
}

fn cross_assign_anon_fn_seven(a int, b bool) string {
	return 'seven'
}

fn cross_assign_anon_fn_eight(a int, b bool) string {
	return 'eight'
}

fn test_cross_assign_anon_fn() {
	mut one := cross_assign_anon_fn_one
	mut two := cross_assign_anon_fn_two
	one, two = two, one
	foo := two(0, true) + one(0, true)
	assert foo == 'onetwo'

	mut three := cross_assign_anon_fn_three
	mut four := cross_assign_anon_fn_four
	three, four = four, three
	mut foo2, mut foo3 := four()
	foo4, foo5 := three()
	foo2 += foo4
	foo3 += foo5
	assert foo2 == 'threefour'
	assert foo3 == 'threefour'

	mut five := cross_assign_anon_fn_five
	mut six := cross_assign_anon_fn_six
	five, six = six, five
	foo6 := six(1, 2, 3) + five(1, 2, 3)
	assert foo6 == 'fivesix'

	one, two, three, four, five, six = two, one, four, three, six, five
	mut foo7, _ := three()
	foo8, _ := four()
	foo7 += foo8
	foo9 := one(0, true) + two(0, true) + foo7 + five(1, 2, 3) + six(1, 2, 3)
	assert foo9 == 'onetwothreefourfivesix'

	mut seven := cross_assign_anon_fn_seven
	mut eight := cross_assign_anon_fn_eight
	one, two, seven, eight = two, seven, eight, one
	foo10 := one(0, true) + two(0, true) + seven(0, true) + eight(0, true)
	assert foo10 == 'twoseveneightone'
}
