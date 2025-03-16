module main

fn second[K, V](x map[K]V) string {
	return 'second: x.typename = ${typeof(x).name}'
}

fn first[T]() string {
	x := T{}
	mut s := 'first: x.typename = ${typeof(x).name}. '
	s += second(x)
	return s
}

fn test_main() {
	assert first[map[string]string]() == 'first: x.typename = map[string]string. second: x.typename = map[string]string'
	assert first[map[string]int]() == 'first: x.typename = map[string]int. second: x.typename = map[string]int'
	assert first[map[string]u8]() == 'first: x.typename = map[string]u8. second: x.typename = map[string]u8'
	assert first[map[string]rune]() == 'first: x.typename = map[string]rune. second: x.typename = map[string]rune'
}
