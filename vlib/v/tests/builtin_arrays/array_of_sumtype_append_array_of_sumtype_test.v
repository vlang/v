type IntStr = int | string

fn test_array_of_sumtype_append_array_of_sumtype() {
	mut a := [IntStr(1), 2, 'a']
	a << [3, 4, 'dsafa']
	println(a)
	assert '${a}' == "[IntStr(1), IntStr(2), IntStr('a'), IntStr(3), IntStr(4), IntStr('dsafa')]"
}
