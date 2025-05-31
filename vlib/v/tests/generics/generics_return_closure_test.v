fn vectorize[T](op fn (T) T) fn ([]T) []T {
	return fn [op] [T](values []T) []T {
		mut result := []T{len: values.len}
		for i in 0 .. values.len {
			result[i] = op(values[i])
		}
		return result
	}
}

fn add_one1(x f64) f64 {
	return x + 1
}

fn add_one2(x int) int {
	return x + 1
}

fn test_generic_return_generic_closure() {
	vadd1 := vectorize[f64](add_one1)
	v1 := [1.0, 2, 3, 4]
	println(vadd1(v1))
	assert vadd1(v1) == [2.0, 3, 4, 5]

	vadd2 := vectorize[int](add_one2)
	v2 := [1, 2, 3, 4]
	println(vadd2(v2))
	assert vadd2(v2) == [2, 3, 4, 5]
}
