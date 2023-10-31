struct Abc {
	a string
	b string
	c string
	d string
	//
	e string
	f string
	g string
	h string
	//
	x int // number of fields must be > 8
}

fn (a Abc) str() string {
	return 'abc'
}

fn (a Abc) some_method() string {
	println(a)
	return '${a}'
}

fn test_custom_str_on_struct_with_too_many_fields() {
	abc := Abc{}
	ret := abc.some_method()
	println(ret)
	assert ret == 'abc'
}
