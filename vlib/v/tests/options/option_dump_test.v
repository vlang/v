struct Test {
	a ?string = 'foo'
	b ?int    = 3
	c ?f64    = 1.1
	d ?[]int  = [1]
	e []?int  = [?int(2)]
}

fn test_dump_opt1() ? {
	a := ?int(0)
	b := dump(a)
	assert b? == 0
}

fn test_dump_opt2() ? {
	c := ?int(none)
	d := dump(c)
	assert d == none
}

fn test_comptime() ? {
	v := Test{}
	$for f in Test.fields {
		a := dump(v.$(f.name))
		b := v.$(f.name)
		dump(a)
		dump(b)
	}
}
