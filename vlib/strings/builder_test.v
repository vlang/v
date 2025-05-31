import strings

type MyInt = int

const maxn = 100000

fn test_sb() {
	mut sb := strings.new_builder(100)
	sb.write_string('hi')
	sb.write_string('!')
	sb.write_string('hello')
	assert sb.len == 8
	sb_end := sb.str()
	assert sb_end == 'hi!hello'
	assert sb.len == 0
	///
	sb = strings.new_builder(10)
	sb.write_string('a')
	sb.write_string('b')
	assert sb.len == 2
	assert sb.str() == 'ab'
	// Test interpolation optimization
	sb = strings.new_builder(10)
	x := 10
	y := MyInt(20)
	sb.writeln('x = ${x} y = ${y}')
	res := sb.str()
	assert res[res.len - 1] == `\n`
	println('"${res}"')
	assert res.trim_space() == 'x = 10 y = 20'

	sb = strings.new_builder(10)
	sb.write_string('x = ${x} y = ${y}')
	assert sb.str() == 'x = 10 y = 20'
	//$if !windows {
	sb = strings.new_builder(10)
	sb.write_string('123456')
	last_2 := sb.cut_last(2)
	assert last_2 == '56'
	final_sb := sb.str()
	assert final_sb == '1234'
	//}
	sb.clear()
	assert sb.str() == ''
}

fn test_big_sb() {
	mut sb := strings.new_builder(100)
	mut sb2 := strings.new_builder(10000)
	for i in 0 .. maxn {
		sb.writeln(i.str())
		sb2.write_string('+')
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
		sb.write_u8(word)
		count++
		assert count == sb.len
	}
	sb_final := sb.str()
	assert sb_final == temp_str
}

fn test_strings_builder_reuse() {
	mut sb := strings.new_builder(256)
	sb.write_string('world')
	assert sb.str() == 'world'
	sb.write_string('hello')
	assert sb.str() == 'hello'
}

fn test_cut_to() {
	mut sb := strings.new_builder(16)
	sb.write_string('hello')
	assert sb.cut_to(3) == 'lo'
	assert sb.len == 3
	assert sb.cut_to(3) == ''
	assert sb.len == 3
	assert sb.cut_to(0) == 'hel'
	assert sb.cut_to(32) == ''
	assert sb.len == 0
}

fn test_write_rune() {
	mut sb := strings.new_builder(10)
	sb.write_rune(`h`)
	sb.write_rune(`e`)
	sb.write_rune(`l`)
	sb.write_rune(`l`)
	sb.write_rune(`o`)
	x := sb.str()
	assert x == 'hello'
}

fn test_write_runes() {
	mut sb := strings.new_builder(20)
	sb.write_runes([`h`, `e`, `l`, `l`, `o`])
	sb.write_rune(` `)
	sb.write_runes([`w`, `o`, `r`, `l`, `d`])
	x := sb.str()
	assert x == 'hello world'
}

fn test_ensure_cap() {
	mut sb := strings.new_builder(0)
	assert sb.cap == 0
	sb.ensure_cap(10)
	assert sb.cap == 10
	sb.ensure_cap(10)
	assert sb.cap == 10
	sb.ensure_cap(15)
	assert sb.cap == 15
	sb.ensure_cap(10)
	assert sb.cap == 15
	sb.ensure_cap(-1)
	assert sb.cap == 15
}

fn test_drain_builder() {
	mut sb := strings.new_builder(0)
	mut target_sb := strings.new_builder(0)
	assert sb.cap == 0
	assert target_sb.cap == 0

	sb.write_string('abc')
	assert sb.len == 3

	target_sb.drain_builder(mut sb, 0)
	assert sb.len == 0
	assert target_sb.len == 3
	assert target_sb.str() == 'abc'
}

@[manualfree]
fn sb_i64_str(n i64) string {
	mut sb := strings.new_builder(24)
	defer {
		unsafe { sb.free() }
	}
	sb.write_decimal(n)
	return sb.str()
}

fn test_write_decimal() {
	assert sb_i64_str(0) == '0'
	assert sb_i64_str(1) == '1'
	assert sb_i64_str(-1) == '-1'
	assert sb_i64_str(1001) == '1001'
	assert sb_i64_str(-1001) == '-1001'
	assert sb_i64_str(1234567890) == '1234567890'
	assert sb_i64_str(-1234567890) == '-1234567890'
	assert sb_i64_str(9223372036854775807) == '9223372036854775807'
	assert sb_i64_str(-9223372036854775807) == '-9223372036854775807'
}

fn test_grow_len() {
	mut sb := strings.new_builder(10)
	assert sb.len == 0
	assert sb.cap == 10

	sb.write_string('0123456789')
	assert sb.len == 10

	unsafe { sb.grow_len(-5) }
	assert sb.len == 10
	assert sb.cap == 10

	unsafe { sb.grow_len(10) }
	assert sb.len == 20
	assert sb.cap == 20

	unsafe { sb.ensure_cap(35) }
	assert sb.len == 20
	assert sb.cap == 35

	unsafe { sb.grow_len(5) }
	assert sb.len == 25
	assert sb.cap == 35
}
