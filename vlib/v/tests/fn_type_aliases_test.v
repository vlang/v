type FdfFn = fn (x f64, params []f64) (f64, f64)

fn abc(x f64, params []f64) (f64, f64) {
	return x + params[0], x + params[1]
}

fn printer(x f64, f FdfFn) string {
	a, b := f(x, [10.0, 20.0])
	return 'a: ${a:5.2f} | b: ${b:5.2f}'
}

fn test_function_with_multiple_return_values_can_be_type_aliased() {
	res := printer(12.2, abc)
	assert res == 'a: 22.20 | b: 32.20'
}
