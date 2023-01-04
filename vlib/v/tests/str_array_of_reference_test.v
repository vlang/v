fn test_creating_an_array_of_string_reference() {
	names := ['John', 'Paul', 'George', 'Ringo']
	a := unsafe { [&names[0], &names[1]] }
	println(a[0])
	println(a)
	assert '${a}' == "[&'John', &'Paul']"
	assert typeof(a[0]).name == '&string'
}

fn test_pushing_to_an_array_of_string_references() {
	mut a := []&string{}
	v1 := 'abc'
	v2 := 'def'
	a << &v1
	a << &v2
	assert *(a[0]) == 'abc'
	assert *(a[1]) == 'def'
}
