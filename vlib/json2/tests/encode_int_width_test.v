import json2

type EncodedInt = int

struct EncodedIntegerWidths {
	value int
	alias EncodedInt
	small i32
}

fn test_encode_int_preserves_its_width() {
	values := if sizeof(int) == 8 {
		[int(i64(2147483648)), int(i64(-2147483649)), int(max_i64), int(min_i64)]
	} else {
		[int(max_i32), int(min_i32)]
	}
	for value in values {
		expected := i64(value).str()
		assert json2.encode(value) == expected
		assert json2.encode(EncodedInt(value)) == expected
		assert json2.encode([value]) == '[${expected}]'
		assert json2.encode(map[string]int{
			'k': value
		}) == '{"k":${expected}}'
		assert json2.encode(EncodedIntegerWidths{value, EncodedInt(value), -3}) ==
			'{"value":${expected},"alias":${expected},"small":-3}'
		mut destination := 'prefix:'.bytes()
		json2.encode_append(value, mut destination)
		assert destination.bytestr() == 'prefix:${expected}'
	}
}

fn test_encode_i32_retains_its_limits() {
	assert json2.encode(max_i32) == '2147483647'
	assert json2.encode(min_i32) == '-2147483648'
}
