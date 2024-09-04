struct Test {
mut:
	a ?int
	b ?string
	c ?[]int
}

fn mr_1() (?int, string, []int) {
	return 456, 'foo', [1]
}

fn mr_2() (?int, string, []int) {
	return none, 'zoo', [99]
}

fn mr_3() ([]?int, ?int) {
	return [?int(456)], none
}

fn test_mr_first_option() {
	mut t := Test{}
	t.a, t.b, t.c = mr_1()
	assert t.a? == 456
	assert t.b? == 'foo'
	assert t.c? == [1]
}

fn test_mr_first_option_none() {
	mut t := Test{}
	t.a, t.b, t.c = mr_2()
	assert t.a == none
	assert t.b? == 'zoo'
	assert t.c? == [99]
}

fn test_mr_first_option_array() {
	a, b := mr_3()
	t := a[0]
	assert t? == 456
	assert b == none
}
