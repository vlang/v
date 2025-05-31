type ALchar = char

fn test_alias_char_type_reference() {
	c1 := &char(unsafe { nil })
	c2 := &ALchar(unsafe { nil })
	println('${typeof(c1).name} ${c1}')
	assert '${c1}' == '0'
	println('${typeof(c2).name} ${c2}')
	assert '${c2}' == '0'
}
