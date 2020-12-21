import strings

type MyInt = int

fn test_sb() {
	mut sb := strings.Builder{}
	sb.write('hi')
	sb.write('!')
	sb.write('hello')
	assert sb.len == 8
	sb_end := sb.str()
	assert sb_end == 'hi!hello'
	assert sb.len == 0
	///
	sb = strings.new_builder(10)
	sb.write('a')
	sb.write('b')
	assert sb.len == 2
	assert sb.str() == 'ab'
	// Test interpolation optimization
	sb = strings.new_builder(10)
	x := 10
	y := MyInt(20)
	sb.writeln('x = $x y = $y')
	res := sb.str()
	assert res[res.len - 1] == `\n`
	println('"$res"')
	assert res.trim_space() == 'x = 10 y = 20'
	//
	sb = strings.new_builder(10)
	sb.write('x = $x y = $y')
	assert sb.str() == 'x = 10 y = 20'
	$if !windows {
		// TODO msvc bug
		sb = strings.new_builder(10)
		sb.write('123456')
		last_2 := sb.cut_last(2)
		assert last_2 == '56'
		final_sb := sb.str()
		assert final_sb == '1234'
	}
}

const (
	maxn = 100000
)

fn test_big_sb() {
	mut sb := strings.new_builder(100)
	mut sb2 := strings.new_builder(10000)
	for i in 0 .. maxn {
		sb.writeln(i.str())
		sb2.write('+')
	}
	s := sb.str()
	lines := s.split_into_lines()
	assert lines.len == maxn
	assert lines[0] == '0'
	assert lines[1] == '1'
	assert lines[777] == '777'
	assert lines[98765] == '98765'
	println(sb2.len)
	assert sb2.len == maxn
}

fn test_byte_write() {
	mut sb := strings.new_builder(100)
	temp_str := 'byte testing'
	mut count := 0
	for word in temp_str {
		sb.write_b(word)
		count++
		assert count == sb.len
	}
	sb_final := sb.str()
	assert sb_final == temp_str
}
