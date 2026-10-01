@[translated]
module main

#include <math.h>

fn C.exp(f64) f64

// Translated files may assign to locals declared without `mut`, also when a
// function has the same name (C translated by c2v declares `exp := 0` in SQLite's
// decimal extension, next to `fn C.exp(f64) f64`).
fn digits(n int) int {
	exp := 0
	exp = n - 1
	return exp
}

fn test_a_local_shadows_a_c_function_of_the_same_name() {
	assert digits(5) == 4
	assert C.exp(0) == 1.0
}
