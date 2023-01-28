struct Foo {
mut:
	name string
}

fn (mut f Foo) test(arr ?[]int) ?string {
	return arr?.len.str()
}

fn (mut f Foo) test2() ?string {
	return none
}

fn test_main() {
	mut m := Foo{}
	v := m?.test([1, 2, 3]) or { '4' }
	m?.name = 'foo'
	assert m?.name == 'foo'
	assert v == '3'
	var := m?.test2() or { '' }
	assert var == ''
}
