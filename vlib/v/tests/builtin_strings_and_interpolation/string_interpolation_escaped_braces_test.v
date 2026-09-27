// vtest vflags: -no-retry-compilation

fn test_escaped_braced_interpolation_stays_literal_next_to_interpolation() {
	value := 123
	assert '${value}\n\${missing}' == '123\n' + r'${missing}'
	assert '\${missing}-${value}-\${other}' == r'${missing}' + '-123-' + r'${other}'
}

fn test_escaped_nested_if_text_stays_literal() {
	cond := true
	a := 'A'
	b := 'B'
	assert '\${if cond { ${a} } else { ${b} }}' == r'${if cond { ' + a + r' } else { ' + b + r' }}'
}
