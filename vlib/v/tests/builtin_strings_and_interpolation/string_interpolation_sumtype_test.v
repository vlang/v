type Ints = []int
type Strings = []string
type Result = Ints | Strings

fn test_string_interpolation_sumtype() {
	res := Result(Ints([1, 2, 3]))
	println(res)
	assert '${res}' == 'Result(Ints([1, 2, 3]))'
}
