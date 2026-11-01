// Module cmp provides comparison helpers for ordered values, similar to Go's
// `cmp` package and Rust's `std::cmp`.
module cmp

// compare returns -1 if a < b, 0 if a == b, and 1 if a > b.
// For floating point types, NaN sorts before all non-NaN values and compares
// equal to NaN. Negative and positive zero compare equal.
pub fn compare[T](a T, b T) int {
	$if T is $float {
		if a != a {
			return if b != b { 0 } else { -1 }
		}
		if b != b {
			return 1
		}
	}
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// less reports whether a sorts before b, using the same ordering as compare.
pub fn less[T](a T, b T) bool {
	$if T is $float {
		if a != a {
			return b == b
		}
	}
	return a < b
}

// min returns the smaller of a and b.
// For floating point types, either NaN argument produces NaN, as in math.min.
// Equal non-NaN values return b, including negative and positive zero.
pub fn min[T](a T, b T) T {
	$if T is $float {
		if a != a {
			return a
		}
		if b != b {
			return b
		}
	}
	if a < b {
		return a
	}
	return b
}

// max returns the larger of a and b.
// For floating point types, either NaN argument produces NaN, as in math.max.
// Equal non-NaN values return b, including negative and positive zero.
pub fn max[T](a T, b T) T {
	$if T is $float {
		if a != a {
			return a
		}
		if b != b {
			return b
		}
	}
	if a > b {
		return a
	}
	return b
}

// clamp restricts x to the range [lo, hi].
// The caller must supply lo <= hi, with neither bound NaN.
// A NaN x is returned unchanged, as in math.clamp.
pub fn clamp[T](x T, lo T, hi T) T {
	if x < lo {
		return lo
	}
	if x > hi {
		return hi
	}
	return x
}

// coalesce returns the first of vals that differs from the default value T{},
// or T{} when all values are default or no values are supplied.
// Struct defaults apply. NaN is non-default; either floating point zero is default.
pub fn coalesce[T](vals ...T) T {
	for v in vals {
		if v != T{} {
			return v
		}
	}
	return T{}
}
