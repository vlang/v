fn vectorize[T](op fn (T) T) fn ([]T) []T {
	return fn [op] [T](values []T) []T {
		mut result := []T{len: values.len}
		for i in 0 .. values.len {
			result[i] = op(values[i])
		}
		return result
	}
}

fn add_one(x f64) f64 {
	return x + 1
}

fn test_return_generic_closure() {
	vadd := vectorize[f64](add_one)
	v := [1.0, 2, 3, 4]
	assert vadd(v) == [2.0, 3, 4, 5]
}
