pub fn mymap[T, R](input []T, f fn (T) R) []R {
	mut results := []R{cap: input.len}
	for x in input {
		results << f(x)
	}
	return results
}

type StringConvertFn[T] = fn (s string) !T

fn parse_string[T](s string, sep string, conv StringConvertFn[T]) ![]T {
	mut result := []T{}
	for part in s.split(sep) {
		result << conv[T](part)!
	}
	return result
}

fn test_main() {
	assert dump(mymap([1, 2, 3, 4, 5], fn (i int) int {
		return i * i
	})) == [1, 4, 9, 16, 25]
	assert dump(mymap([1, 2, 3, 4, 5], |x| x * x)) == [1, 4, 9, 16, 25]
	assert dump(mymap([1, 2, 3, 4, 5], |x| u16(x * x))) == [u16(1), 4, 9, 16, 25]
	assert parse_string[int]('1..5', '..', fn [T](s string) !int {
		return s.int()
	})! == [1, 5]
	assert parse_string[int]('1..5', '..', |s| s.int())! == [1, 5]
}
