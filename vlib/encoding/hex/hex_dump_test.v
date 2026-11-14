module hex

fn test_hexdump() {
	d := hexdump('Hello'.bytes())
	assert d.starts_with('00000000')
	assert d.contains('48 65 6c 6c 6f')
	assert d.contains('|Hello|')
	assert d.ends_with('\n')
}

fn test_hexdump_empty() {
	assert hexdump([]u8{}) == ''
}

fn test_hexdump_multiline() {
	mut data := []u8{len: 17}
	for i in 0 .. 17 {
		data[i] = u8(i)
	}
	d := hexdump(data)
	assert d.starts_with('00000000')
	assert d.contains('00000010')
}
