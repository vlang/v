struct Test {
	a ?string
}

fn foo(a ?string) ? {
	println(a?)
	if a == none {
		assert false
	}
}

fn test_main() {
	v := Test{}
	$for f in Test.fields {
		mut t := v.$(f.name)
		assert t == none
		z := t or { '' }
		assert z == ''
		foo(t)
		t = 'foo'
		assert t != none
		foo(t)
		assert t? == 'foo'
	}
}
