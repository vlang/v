import strings
 
fn test_sb() {
	mut sb := strings.Builder{}
	sb.write('hi')
	sb.write('!')
	sb.write('hello')
	assert sb.str() == 'hi!hello'
	sb = strings.new_builder(10)
	sb.write('a')
	sb.write('b')
	println(sb.str())
	assert sb.str() == 'ab'
}

