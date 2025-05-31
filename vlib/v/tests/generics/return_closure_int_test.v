fn vectorize(op fn (int) int) fn ([]int) []int {
	return fn [op] (values []int) []int {
		mut result := []int{len: values.len}
		for i in 0 .. values.len {
			result[i] = op(values[i])
		}
		return result
	}
}

fn add_one(x int) int {
	return x + 1
}

fn test_return_generic_closure_int() {
	vadd1 := vectorize(add_one)
	v1 := [1, 2, 3, 4]
	println(vadd1(v1))
	assert vadd1(v1) == [2, 3, 4, 5]
}
