// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
import fit
import vsl.math

fn test_linear_fit01() {
	// data
	x := [f64(1), 2, 3, 4]
	y := [f64(1), 2, 3, 4]
	a,b,sigma_a,sigma_b,chi_2 := fit.linear_sigma(x, y)
	assert compare(a, 0.0)
	assert compare(b, 1.0)
	assert compare(sigma_a, 0.0)
	assert compare(sigma_b, 0.0)
	assert compare(chi_2, 0.0)
}

fn test_linear_fit02() {
	// data
	x := [f64(1), 2, 3, 4]
	y := [f64(6), 5, 7, 10]
	a,b := fit.linear(x, y)
	assert compare(a, 3.5)
	assert compare(b, 1.4)
}

// Helper method for comparing floats
fn compare(x, y f64) bool {
	tolerance := 0.00001
	// Special case for zeroes
	if x < tolerance && x > (-1.0 * tolerance) && y < tolerance && y > (-1.0 * tolerance) {
		return true
	}
	diff := math.abs(x - y)
	mean := math.abs(x + y) / 2.0
	return if math.is_nan(diff / mean) { true } else { ((diff / mean) < tolerance) }
}
