fn vectorize(op fn (f64) f64) fn ([]f64) []f64 {
	return fn [op] (values []f64) []f64 {
		mut result := []f64{len: values.len}
		for i in 0 .. values.len {
			result[i] = op(values[i])
		}
		return result
	}
}

fn add_one(x f64) f64 {
	return x + 1
}

fn test_return_generic_closure_f64() {
	vadd1 := vectorize(add_one)
	v1 := [1.0, 2, 3, 4]
	println(vadd1(v1))
	assert vadd1(v1) == [2.0, 3, 4, 5]
}
