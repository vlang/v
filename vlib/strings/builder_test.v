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

const (
	n = 100000
)

fn test_big_sb() {
	mut sb := strings.new_builder(100)
	for i in 0..n {
		sb.writeln(i.str())
	}	
	s := sb.str()
	lines := s.split_into_lines()
	assert lines.len == n
	assert lines[0] == '0'
	assert lines[1] == '1'
	assert lines[777] == '777'
	assert lines[98765] == '98765'
	
}	

