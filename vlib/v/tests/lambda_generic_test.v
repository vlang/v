import arrays

fn ddd[T](call fn (arg1 T, arg2 T) string, arg1 T, arg2 T) string {
	return call(arg1, arg2)
}

fn abc2[T](arg1 T, arg2 T) string {
	return ddd(|x, y| (x + y).str(), arg1, arg2)
}

fn abc[T](arg T, call fn (a T) string) string {
	return call(arg)
}

fn test_main() {
	a := [1, 2, 3, 4]
	b := arrays.join_to_string(a, '-', |x| x.str())
	assert b == '1-2-3-4'
	assert abc(123, |x| x.str()) == '123'
}

fn test_generic() {
	assert dump(abc2('3', '3')) == '33'
	assert dump(abc2(3, 3)) == '6'
	assert dump(abc2(3.1, 3.1)) == '6.2'
}
