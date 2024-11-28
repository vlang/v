struct Foo {
mut:
	name string
}

struct Test {
pub mut:
	a string
	b ?string
}

fn (mut f Foo) test(arr ?[]int) ?string {
	return arr?.len.str()
}

fn (mut f Foo) test2() ?string {
	return none
}

fn fn_a(s ?Test) ? {
	println(s?.a)
	assert false
}

fn fn_b(s ?Test) ? {
	println(s?.b?)
	assert false
}

fn test_main() {
	mut m := ?Foo{}
	assert m == none
	m = Foo{
		name: 'foo'
	}
	assert m != none
	v := m?.test([1, 2, 3]) or { '4' }
	m?.name = 'foo'
	assert m?.name == 'foo'
	assert v == '3'
	var := m?.test2() or { '' }
	assert var == ''
}

fn test_opt_call() {
	mut t := ?Test{}
	fn_a(none) // returns none
	fn_b(t) // returns none
}
