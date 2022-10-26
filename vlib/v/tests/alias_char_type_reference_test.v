module main

type ALchar = char

fn test_alias_char_type_reference() {
	c1 := &char(0)
	c2 := &ALchar(0)
	println('${typeof(c1).name} ${c1}')
	assert '${c1}' == '0'
	println('${typeof(c2).name} ${c2}')
	assert '${c2}' == '0'
}
