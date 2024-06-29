module math

// DivResult[T] represents the result of an integer division (both quotient and remainder)
// See also https://en.wikipedia.org/wiki/Modulo
pub struct DivResult[T] {
pub mut:
	quot T
	rem  T
}

// divide_truncated returns the truncated version of the result of dividing numer to denom
pub fn divide_truncated[T](numer T, denom T) DivResult[T] {
	return DivResult[T]{
		quot: numer / denom
		rem: numer % denom
	}
}

// divide_euclid returns the Euclidean version of the result of dividing numer to denom
pub fn divide_euclid[T](numer T, denom T) DivResult[T] {
	mut q := numer / denom
	mut r := numer % denom
	if r < 0 {
		if denom > 0 {
			q = q - 1
			r = r + denom
		} else {
			q = q + 1
			r = r - denom
		}
	}
	return DivResult[T]{
		quot: q
		rem: r
	}
}

// divide_floored returns the floored version of the result of dividing numer to denom
pub fn divide_floored[T](numer T, denom T) DivResult[T] {
	mut q := numer / denom
	mut r := numer % denom
	if (r > 0 && denom < 0) || (r < 0 && denom > 0) {
		q = q - 1
		r = r + denom
	}
	return DivResult[T]{
		quot: q
		rem: r
	}
}

// modulo_truncated returns the truncated remainder of dividing numer to denom
pub fn modulo_truncated[T](numer T, denom T) T {
	return numer % denom
}

// modulo_euclid returns the Euclidean remainder of dividing numer to denom
pub fn modulo_euclid[T](numer T, denom T) T {
	return divide_euclid(numer, denom).rem
}

// modulo_floored returns the floored remainder of dividing numer to denom
pub fn modulo_floored[T](numer T, denom T) T {
	return divide_floored(numer, denom).rem
}
