fn f64_fun(x []f64) f64 {
	return x[0] + x[1]
}

fn t_fun[T](x []T) T {
	$if T is f64 {
		return f64_fun(x)
	} $else {
		return T(0)
	}
}

fn test_main() {
	assert t_fun([1.0, 2.0]) == 3.0
}
