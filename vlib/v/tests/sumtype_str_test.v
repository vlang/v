struct Abc {
	foo int
	bar bool
	str string
}

type ST = Abc | bool | int | string

fn test_int_st_str() {
	a := ST(0)
	assert '${a}' == 'ST(0)'
	assert a.str() == 'ST(0)'
}

fn test_string_st_str() {
	a := ST('test')
	assert '${a}' == "ST('test')"
	assert a.str() == "ST('test')"
}

fn test_struct_st_str() {
	a := ST(Abc{})
	assert '${a}' == "ST(Abc{\n    foo: 0\n    bar: false\n    str: ''\n})"
	assert a.str() == "ST(Abc{\n    foo: 0\n    bar: false\n    str: ''\n})"
}

fn test_bool_st_str() {
	a := ST(false)
	assert '${a}' == 'ST(false)'
	assert a.str() == 'ST(false)'
}

struct Container {
	st ST
}

fn test_in_struct() {
	c := Container{ST(0)}
	assert '${c}' == 'Container{\n    st: ST(0)\n}'
	assert c.str() == 'Container{\n    st: ST(0)\n}'
}

fn test_unknown_value() {
	c := Container{}
	assert '${c}' == 'Container{\n    st: unknown sum type value\n}'
	assert c.str() == 'Container{\n    st: unknown sum type value\n}'
}

fn test_nested_in_struct() {
	abc := Abc{}
	c := Container{ST(abc)}
	assert '${c}' == "Container{\n    st: ST(Abc{\n        foo: 0\n        bar: false\n        str: ''\n    })\n}"
	assert c.str() == "Container{\n    st: ST(Abc{\n        foo: 0\n        bar: false\n        str: ''\n    })\n}"
}

fn test_pointer() {
	st := ST(0)
	assert '${&st}' == '&ST(0)'
}

struct Xyz {}

type Hola = Abc | Xyz

fn (h Hola) str() string {
	return 'Hola'
}

struct HolaContainer {
	h Hola
}

fn test_custom_str_method() {
	h := HolaContainer{}
	assert h.str() == 'HolaContainer{\n    h: Hola\n}'
	assert '${h}' == 'HolaContainer{\n    h: Hola\n}'
}
