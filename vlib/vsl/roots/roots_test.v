// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
import roots
import vsl
import vsl.math

const (
	epsabs = 0.0001
	epsrel = 0.00001
	n_max = 100
)

fn f_cos(x f64, _ []f64) f64 {
	return math.cos(x)
}

fn fdf_cos(x f64, _ []f64) (f64,f64) {
	return math.cos(x),-math.sin(x)
}

fn test_root_bisection() {
	x1 := f64(0)
	x2 := f64(3)
	func := vsl.Function{
		function: f_cos
	}
	result := roots.bisection(func, x1, x2, epsrel, epsabs, n_max) or {
		panic(err)
	}
	assert compare(result, math.pi / 2.00)
}

fn test_root_newton() {
	x0 := f64(0.5)
	func := vsl.FunctionFdf{
		fdf: fdf_cos
	}
	result := roots.newton(func, x0, epsrel, epsabs, n_max) or {
		panic(err)
	}
	assert compare(result, math.pi / 2.00)
}

// Helper method for comparing floats
fn compare(x, y f64) bool {
	tolerance := epsabs
	// Special case for zeroes
	if x < tolerance && x > (-1.0 * tolerance) && y < tolerance && y > (-1.0 * tolerance) {
		return true
	}
	diff := math.abs(x - y)
	mean := math.abs(x + y) / 2.0
	return if math.is_nan(diff / mean) { true } else { ((diff / mean) < tolerance) }
}
