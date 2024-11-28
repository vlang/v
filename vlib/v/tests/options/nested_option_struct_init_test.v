struct Data {
	a ?int
	b ?int = 1
}

struct Data2 {
	d Data
}

fn test_nested_option_struct_init() {
	d2 := Data2{}
	println(d2)
	assert d2.d.a == none
	assert d2.d.b != none
	assert d2.d.b? == 1
}

struct AA {
	bb ?BB // defaults to none
}

struct BB {
	x string @[required]
}

fn test_nested_option_struct_with_attr_init() {
	aa := AA{}
	println(aa)
	assert aa.bb == none
}
