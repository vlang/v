module math

// Saturating[T] is an integer wrapper with saturating arithmetic: results
// that would overflow clamp to the type's minimum or maximum instead of
// wrapping or panicking, in every build mode. It is the V-idiomatic
// equivalent of Rust's `std::num::Saturating`.
// Supported for the 8 fixed-width integer types (u8..u64, i8..i64).
pub struct Saturating[T] {
pub:
	value T
}

// saturating creates a Saturating[T] holding value.
pub fn saturating[T](value T) Saturating[T] {
	return Saturating[T]{value}
}

// str returns the decimal representation of the saturated value.
pub fn (s Saturating[T]) str() string {
	return s.value.str()
}

// + returns the sum clamped to the type's range.
pub fn (a Saturating[T]) + (b Saturating[T]) Saturating[T] {
	return saturating(sat_add(a.value, b.value))
}

// - returns the difference clamped to the type's range.
pub fn (a Saturating[T]) - (b Saturating[T]) Saturating[T] {
	return saturating(sat_sub(a.value, b.value))
}

// * returns the product clamped to the type's range.
pub fn (a Saturating[T]) * (b Saturating[T]) Saturating[T] {
	return saturating(sat_mul(a.value, b.value))
}

// / returns the quotient, mapping min/-1 to max. Division by zero still
// panics, like plain division.
pub fn (a Saturating[T]) / (b Saturating[T]) Saturating[T] {
	return saturating(sat_div(a.value, b.value))
}

// % returns the remainder. Like plain remainder it panics on a zero divisor.
pub fn (a Saturating[T]) % (b Saturating[T]) Saturating[T] {
	return saturating(sat_mod(a.value, b.value))
}

// neg returns the negation clamped to the type's range (negating min yields
// max; negating an unsigned zero stays zero).
pub fn (s Saturating[T]) neg() Saturating[T] {
	return saturating(sat_sub(T(0), s.value))
}

// sat_add returns a + b clamped to [min, max]. Every operation in the guards
// is provably in-range, so no intermediate step can overflow in any mode.
fn sat_add[T](a T, b T) T {
	mx := maxof[T]()
	mn := minof[T]()
	z := T(0)
	if b >= z && a > mx - b {
		return mx
	}
	if b < z && a < mn - b {
		return mn
	}
	return a + b
}

// sat_sub returns a - b clamped to [min, max], with provably in-range guards.
fn sat_sub[T](a T, b T) T {
	mx := maxof[T]()
	mn := minof[T]()
	z := T(0)
	if b >= z && a < mn + b {
		return mn
	}
	if b < z && a > mx + b {
		return mx
	}
	return a - b
}

// sat_mul returns a * b clamped to [min, max]. All divisions in the guards
// are zero-safe by construction, and the final multiply is provably in-range.
fn sat_mul[T](a T, b T) T {
	mx := maxof[T]()
	mn := minof[T]()
	z := T(0)
	if a > z {
		if b > z {
			if a > mx / b {
				return mx
			}
		} else {
			if b < mn / a {
				return mn
			}
		}
	} else if a < z {
		if b > z {
			if a < mn / b {
				return mn
			}
		} else {
			if b < mx / a {
				return mx
			}
		}
	} else {
		return z
	}
	return a * b
}

// sat_div returns a / b, mapping min/-1 to max. Division by zero still
// panics, like plain division.
fn sat_div[T](a T, b T) T {
	mx := maxof[T]()
	mn := minof[T]()
	z := T(0)
	if a == mn && mn != z && b == z - T(1) {
		return mx
	}
	return a / b
}

// sat_mod returns a % b, handling min % -1 before the backend's division
// operation can overflow. Division by zero still panics.
fn sat_mod[T](a T, b T) T {
	mn := minof[T]()
	z := T(0)
	if a == mn && mn != z && b == z - T(1) {
		return z
	}
	return a % b
}
