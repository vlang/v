pub fn mymap[T, R](input []T, f fn (T) R) []R {
	mut results := []R{cap: input.len}
	for x in input {
		results << f(x)
	}
	return results
}

fn test_main() {
	assert dump(mymap([1, 2, 3, 4, 5], fn (i int) int {
		return i * i
	})) == [1, 4, 9, 16, 25]
	assert dump(mymap([1, 2, 3, 4, 5], |x| x * x)) == [1, 4, 9, 16, 25]
	assert dump(mymap([1, 2, 3, 4, 5], |x| u16(x * x))) == [u16(1), 4, 9, 16, 25]
}
