import strings

fn test_sb() {
	mut sb := strings.Builder{}
	sb.write('hi')
	sb.write('!')
	sb.write('hello')
	assert sb.len == 8
	assert sb.str() == 'hi!hello'
	assert sb.len == 0
	sb = strings.new_builder(10)
	sb.write('a')
	sb.write('b')
	assert sb.len == 2
	assert sb.str() == 'ab'
}

const (
	n = 100000
)

fn test_big_sb() {
	mut sb := strings.new_builder(100)
	mut sb2 := strings.new_builder(10000)
	for i in 0..n {
		sb.writeln(i.str())
		sb2.write('+')
	}
	s := sb.str()
	lines := s.split_into_lines()
	assert lines.len == n
	assert lines[0] == '0'
	assert lines[1] == '1'
	assert lines[777] == '777'
	assert lines[98765] == '98765'
	println(sb2.len)
	assert sb2.len == n

}

fn test_byte_write() {
	mut sb := strings.new_builder(100)
	temp_str := "byte testing"
	mut count := 0
	for word in temp_str {
		sb.write_b(word)
		count++
		assert count == sb.len
	}
	assert sb.str() == temp_str
}
