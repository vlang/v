module edwards25519

fn check_aliasing_onearg(f fn (mut v Scalar, x Scalar) Scalar, mut v Scalar, x Scalar) bool {
	x1, mut v1 := x, x

	// Calculate a reference f(x) without aliasing.
	mut out := f(mut v, x)
	if out != v || !is_reduced(out) {
		return false
	}

	// Test aliasing the argument and the receiver.
	out2 := f(mut v1, v1)
	if out2 != v1 || v1 != v || !is_reduced(out2) {
		return false
	}

	// Ensure the arguments was not modified.
	return x == x1
}

fn negate_aliasing(mut v Scalar, x Scalar) Scalar {
	// mut t := v
	return v.negate(x)
}

fn test_check_aliasing_oneargs() {
	x := generate_notzero_scalar(10)!
	mut v := generate_notzero_scalar(10)!
	out := check_aliasing_onearg(negate_aliasing, mut v, x)
	assert out == true
}

fn multiply_aliasing(mut v Scalar, x Scalar, y Scalar) Scalar {
	return v.multiply(x, y)
}

fn add_aliasing(mut v Scalar, x Scalar, y Scalar) Scalar {
	return v.add(x, y)
}

fn subtract_aliasing(mut v Scalar, x Scalar, y Scalar) Scalar {
	return v.subtract(x, y)
}

fn test_check_aliasing_twoargs() {
	fn_with_twoargs := [add_aliasing, multiply_aliasing, subtract_aliasing]
	for f in fn_with_twoargs {
		mut v := generate_notzero_scalar(10)!
		x := generate_notzero_scalar(10)!
		y := generate_notzero_scalar(10)!
		out := check_aliasing_twoargs(f, mut v, x, y)
		assert out == true
	}
}

fn check_aliasing_twoargs(f fn (mut v Scalar, x Scalar, y Scalar) Scalar, mut v Scalar, x Scalar, y Scalar) bool {
	x1, y1, mut v1 := x, y, Scalar{}

	// Calculate a reference f(x, y) without aliasing.
	mut out := f(mut v, x, y)
	if out != v || !is_reduced(out) {
		return false
	}

	// Test aliasing the first argument and the receiver.
	v1 = x
	out2 := f(mut v1, v1, y)
	if out2 != v1 || v1 != v || !is_reduced(out2) {
		return false
	}
	// Test aliasing the second argument and the receiver.
	v1 = y
	out3 := f(mut v1, x, v1)
	if out3 != v1 || v1 != v || !is_reduced(out3) {
		return false
	}

	// Calculate a reference f(x, x) without aliasing.
	out4 := f(mut v, x, x)
	if out4 != v || !is_reduced(out4) {
		return false
	}

	// Test aliasing the first argument and the receiver.
	v1 = x
	out5 := f(mut v1, v1, x)
	if out5 != v1 || v1 != v || !is_reduced(out5) {
		return false
	}
	// Test aliasing the second argument and the receiver.
	v1 = x
	out6 := f(mut v1, x, v1)
	if out6 != v1 || v1 != v || !is_reduced(out6) {
		return false
	}
	// Test aliasing both arguments and the receiver.
	v1 = x
	out7 := f(mut v1, v1, v1)
	if out7 != v1 || v1 != v || !is_reduced(out7) {
		return false
	}

	// Ensure the arguments were not modified.
	return x == x1 && y == y1
}
