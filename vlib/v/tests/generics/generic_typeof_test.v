module main

fn decode_array[T](x []T) string {
	return 'decode_array: x.typename = ${typeof(x).name}'
}

fn decode[T]() string {
	x := T{0}
	mut s := 'decode: x.typename = ${typeof(x).name}. '
	s += decode_array(x)
	return s
}

fn second[K, V](x map[K]V) string {
	return 'second: x.typename = ${typeof(x).name}'
}

fn first[T]() string {
	x := T{}
	mut s := 'first: x.typename = ${typeof(x).name}. '
	s += second(x)
	return s
}

fn test_map() {
	assert first[map[string]string]() == 'first: x.typename = map[string]string. second: x.typename = map[string]string'
	assert first[map[string]int]() == 'first: x.typename = map[string]int. second: x.typename = map[string]int'
	assert first[map[string]u8]() == 'first: x.typename = map[string]u8. second: x.typename = map[string]u8'
	assert first[map[string]rune]() == 'first: x.typename = map[string]rune. second: x.typename = map[string]rune'
}

fn test_array() {
	assert decode[[]u8]() == 'decode: x.typename = []u8. decode_array: x.typename = []u8'
	assert decode[[]u16]() == 'decode: x.typename = []u16. decode_array: x.typename = []u16'
	assert decode[[]int]() == 'decode: x.typename = []int. decode_array: x.typename = []int'
	assert decode[[]rune]() == 'decode: x.typename = []rune. decode_array: x.typename = []rune'
}
