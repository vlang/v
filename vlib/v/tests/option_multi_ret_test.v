struct Test {
mut:
	a ?int
	b ?string
	c ?[]int
}

fn mr_1() (?int, string, []int) {
	return 0, 'foo', [1]
}

fn mr_2() (?int, string, []int) {
	return none, 'foo', [1]
}

fn mr_3() ([]?int, ?int) {
	a := ?int(0)
	return [a], none
}

fn test_mr_first_option() {
	mut t := Test{}
	t.a, t.b, t.c = mr_1()

	a := dump(t.a)
	b := dump(t.b)
	c := dump(t.c)
	assert a? == t.a?
	assert b? == t.b?
	assert c? == t.c?
}

fn test_mr_first_option_none() {
	mut t := Test{}
	t.a, t.b, t.c = mr_2()

	a := dump(t.a)
	b := dump(t.b)
	c := dump(t.c)
	assert a == none
	assert b? == t.b?
	assert c? == t.c?
}

fn test_mr_first_option_array() {
	a, b := mr_3()

	t := a[0]
	assert t? == 0
	assert b == none
}
