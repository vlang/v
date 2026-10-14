module strings

fn test_indent() {
	assert indent('a\nb', '> ', false) == '> a\n> b'
	assert indent('a\nb', '> ', true) == 'a\n> b'
	assert indent('', '--', false) == '--'
	assert indent('x', '--', true) == 'x'
}

fn test_indent_code_block() {
	src := 'fn main() {\nprintln(1)\n}'
	assert indent(src, '\t', true) == 'fn main() {\n\tprintln(1)\n\t}'
}
