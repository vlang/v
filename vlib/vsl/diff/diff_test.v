// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
import diff
import vsl
import vsl.math

fn f1(x f64, _ []f64) f64 {
	return math.exp(x)
}

fn df1(x f64, _ []f64) f64 {
	return math.exp(x)
}

fn f2(x f64, _ []f64) f64 {
	if x >= 0.00 {
		return x * math.sqrt(x)
	}
	else {
		return 0.00
	}
}

fn df2(x f64, _ []f64) f64 {
	if x >= 0.00 {
		return 1.50 * math.sqrt(x)
	}
	else {
		return 0.00
	}
}

fn f3(x f64, _ []f64) f64 {
	if x != 0.00 {
		return math.sin(1.00 / x)
	}
	else {
		return 0.00
	}
}

fn df3(x f64, _ []f64) f64 {
	if x != 0.00 {
		return -math.cos(1.00 / x) / (x * x)
	}
	else {
		return 0.00
	}
}

fn f4(x f64, _ []f64) f64 {
	return math.exp(-x * x)
}

fn df4(x f64, _ []f64) f64 {
	return -2.00 * x * math.exp(-x * x)
}

fn f5(x f64, _ []f64) f64 {
	return x * x
}

fn df5(x f64, _ []f64) f64 {
	return 2.00 * x
}

fn f6(x f64, _ []f64) f64 {
	return 1.00 / x
}

fn df6(x f64, _ []f64) f64 {
	return -1.00 / (x * x)
}

fn test_diff() {
	_f1 := vsl.Function{
		function: f1
	}
	_df1 := vsl.Function{
		function: df1
	}
	_f2 := vsl.Function{
		function: f2
	}
	_df2 := vsl.Function{
		function: df2
	}
	_f3 := vsl.Function{
		function: f3
	}
	_df3 := vsl.Function{
		function: df3
	}
	_f4 := vsl.Function{
		function: f4
	}
	_df4 := vsl.Function{
		function: df4
	}
	_f5 := vsl.Function{
		function: f5
	}
	_df5 := vsl.Function{
		function: df5
	}
	_f6 := vsl.Function{
		function: f6
	}
	_df6 := vsl.Function{
		function: df6
	}
	assert diff_test('central', _f1, _df1, f64(1.0))
	assert diff_test('forward', _f1, _df1, f64(1.0))
	assert diff_test('backward', _f1, _df1, f64(1.0))
	assert diff_test('central', _f2, _df2, f64(0.1))
	assert diff_test('forward', _f2, _df2, f64(0.1))
	assert diff_test('backward', _f2, _df2, f64(0.1))
	assert diff_test('central', _f3, _df3, f64(0.45))
	assert diff_test('forward', _f3, _df3, f64(0.45))
	assert diff_test('backward', _f3, _df3, f64(0.45))
	assert diff_test('central', _f4, _df4, f64(0.5))
	assert diff_test('forward', _f4, _df4, f64(0.5))
	assert diff_test('backward', _f4, _df4, f64(0.5))
	assert diff_test('central', _f5, _df5, f64(0))
	assert diff_test('forward', _f5, _df5, f64(0))
	assert diff_test('backward', _f5, _df5, f64(0))
	assert diff_test('central', _f6, _df6, f64(10.0))
	assert diff_test('forward', _f6, _df6, f64(10.0))
	assert diff_test('backward', _f6, _df6, f64(10.0))
}

fn diff_test(diff_method string, f, df vsl.Function, x f64) bool {
	expected := df.eval(x)
	result,_ := if diff_method == 'backward' { diff.backward(f, x) } else if diff_method == 'forward' { diff.forward(f, x) } else { diff.central(f, x) }
	return compare(result, expected)
}

fn diff_near_test(diff_method string, f, df vsl.Function, x, tolerance f64) bool {
	expected := df.eval(x)
	result,_ := if diff_method == 'backward' { diff.backward(f, x) } else if diff_method == 'forward' { diff.forward(f, x) } else { diff.central(f, x) }
	return compare_near(result, expected, tolerance)
}

// Helper methods for comparing floats
[inline]
fn compare(x, y f64) bool {
	return compare_near(x, y, 1e-5)
}

fn compare_near(x, y, tolerance f64) bool {
	// Special case for zeroes
	if x < tolerance && x > (-1.0 * tolerance) && y < tolerance && y > (-1.0 * tolerance) {
		return true
	}
	diff := math.abs(x - y)
	mean := math.abs(x + y) / 2.0
	return if math.is_nan(diff / mean) { true } else { ((diff / mean) < tolerance) }
}
