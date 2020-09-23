struct Abc {
	foo int
	bar bool
	str string
}

type ST = int | string | bool | Abc

fn test_int_st_str() {
	a := ST(0)
	assert '$a' == 'ST(0)'
}

fn test_string_st_str() {
	a := ST('test')
	assert '$a' == 'ST(\'test\')'
}

fn test_struct_st_str() {
	a := ST(Abc{})
	assert '$a' == 'ST(Abc {\n    foo: 0\n    bar: false\n    str: \'\'\n})'
}

fn test_bool_st_str() {
	a := ST(false)
	assert '$a' == 'ST(false)'
}

fn test_str() {
	a := ST(false)
	assert a.str() == 'ST(false)'
}
