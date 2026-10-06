module main

enum ParsedDecimal {
	ten             = 010
	eight           = 08
	prefixed_octal  = 0o20
	prefixed_hex    = 0x20
	prefixed_binary = 0b10
}

fn returned_values(mut observed []int) (int, int, int, int, int, int, int, int, int, int, int) {
	defer {
		observed << $res(010)
		observed << $res(0o10)
	}
	return 0, 11, 22, 33, 44, 55, 66, 77, 88, 99, 110
}

fn test_decimal_literal_readers_and_explicit_prefixes() {
	assert int(ParsedDecimal.ten) == 10
	assert int(ParsedDecimal.eight) == 8
	assert int(ParsedDecimal.prefixed_octal) == 16
	assert int(ParsedDecimal.prefixed_hex) == 32
	assert int(ParsedDecimal.prefixed_binary) == 2
	values := [010]int{}
	assert values.len == 10
	mut observed := []int{}
	_, _, _, _, _, _, _, _, _, _, last := returned_values(mut observed)
	assert last == 110
	assert observed == [110, 88]
}
