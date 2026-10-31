// Module cmp provides comparison helpers for ordered values, similar to Go's
// `cmp` package and Rust's `std::cmp`.
module cmp

// compare returns -1 if a < b, 0 if a == b, and 1 if a > b.
pub fn compare[T](a T, b T) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// less reports whether a < b.
pub fn less[T](a T, b T) bool {
	return a < b
}

// min returns the smaller of a and b.
pub fn min[T](a T, b T) T {
	if a < b {
		return a
	}
	return b
}

// max returns the larger of a and b.
pub fn max[T](a T, b T) T {
	if a > b {
		return a
	}
	return b
}

// clamp restricts x to the range [lo, hi].
pub fn clamp[T](x T, lo T, hi T) T {
	if x < lo {
		return lo
	}
	if x > hi {
		return hi
	}
	return x
}

// coalesce returns the first of vals that is not the zero value, or the zero
// value if all are zero. Mirrors Go's cmp.Or.
pub fn coalesce[T](vals ...T) T {
	for v in vals {
		if v != T{} {
			return v
		}
	}
	if vals.len > 0 {
		return vals[vals.len - 1]
	}
	return T{}
}
