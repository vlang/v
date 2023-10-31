type Abc = int | string

fn test_array_of_sumtype_with_default() {
	a1 := []Abc{len: 1, init: 22}
	println(a1)
	assert '${a1}' == '[Abc(22)]'

	a2 := []Abc{len: 1, init: 'hello'}
	println(a2)
	assert '${a2}' == "[Abc('hello')]"
}
