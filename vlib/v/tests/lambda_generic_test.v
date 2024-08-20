import arrays

fn abc[T](arg T, call fn (a T) string) string {
	return call(arg)
}

fn test_main() {
	a := [1, 2, 3, 4]
	b := arrays.join_to_string(a, '-', |x| x.str())
	assert b == '1-2-3-4'
	assert abc(123, |x| x.str()) == '123'
}
