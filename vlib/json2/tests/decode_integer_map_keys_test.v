import json2

struct IntegerKeyFields {
	names    map[int]string
	numbers  map[int]int
	nested   map[int]map[int]string
	optional map[int]?int
}

fn test_decode_integer_map_keys() {
	names := json2.decode[map[int]string]('{"1":"a","-2":"b"}')!
	assert names.len == 2
	assert names[1] == 'a'
	assert names[-2] == 'b'
	numbers := json2.decode[map[int]int]('{"7":3}')!
	assert numbers[7] == 3
	fields := json2.decode[IntegerKeyFields]('{"names":{"1":"a"},"numbers":{"7":3},"nested":{"2":{"3":"c"}},"optional":{"4":null,"5":6}}')!
	assert fields.names[1] == 'a'
	assert fields.numbers[7] == 3
	assert fields.nested[2][3] == 'c'
	assert fields.optional.len == 2
	assert 4 in fields.optional
	assert fields.optional[4] == none
	present := fields.optional[5]
	assert present? == 6
}

fn test_decode_wide_integer_map_keys() {
	signed := json2.decode[map[i64]string]('{"-9223372036854775808":"min","9223372036854775807":"max"}')!
	assert signed[i64(-9223372036854775807) - 1] == 'min'
	assert signed[i64(9223372036854775807)] == 'max'
	unsigned := json2.decode[map[u64]string]('{"18446744073709551615":"max"}')!
	assert unsigned[u64(18446744073709551615)] == 'max'
	middle := json2.decode[map[u32]int]('{"4294967295":8}')!
	assert middle[u32(4294967295)] == 8
	small := json2.decode[map[u8]int]('{"255":7}')!
	assert small[u8(255)] == 7
}

fn test_decode_string_and_rune_map_keys() {
	strings := json2.decode[map[string]int]('{"a":1}')!
	assert strings['a'] == 1
	runes := json2.decode[map[rune]int]('{"97":2}')!
	assert runes[rune(97)] == 2
}
