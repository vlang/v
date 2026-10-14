module math

import math.bits

// Wrapping[T] is an integer wrapper with wraparound (modular) arithmetic:
// results that would overflow wrap around instead of panicking or depending
// on backend-specific behavior, in every build mode. It is the V-idiomatic
// equivalent of Rust's `std::num::Wrapping`.
// Supported for the 8 fixed-width integer types (u8..u64, i8..i64).
pub struct Wrapping[T] {
pub:
	value T
}

// wrapping creates a Wrapping[T] holding value.
pub fn wrapping[T](value T) Wrapping[T] {
	return Wrapping[T]{value}
}

// str returns the decimal representation of the wrapped value.
pub fn (w Wrapping[T]) str() string {
	return w.value.str()
}

// + returns the wrapped sum.
pub fn (a Wrapping[T]) + (b Wrapping[T]) Wrapping[T] {
	return wrapping(wrap_add(a.value, b.value))
}

// - returns the wrapped difference.
pub fn (a Wrapping[T]) - (b Wrapping[T]) Wrapping[T] {
	return wrapping(wrap_sub(a.value, b.value))
}

// * returns the wrapped product.
pub fn (a Wrapping[T]) * (b Wrapping[T]) Wrapping[T] {
	return wrapping(wrap_mul(a.value, b.value))
}

// / returns the wrapped quotient. Like plain division it panics on a zero
// divisor; min/-1 yields min (which would otherwise overflow).
pub fn (a Wrapping[T]) / (b Wrapping[T]) Wrapping[T] {
	return wrapping(wrap_div(a.value, b.value))
}

// % returns the remainder. Like plain remainder it panics on a zero divisor.
pub fn (a Wrapping[T]) % (b Wrapping[T]) Wrapping[T] {
	return wrapping(wrap_mod(a.value, b.value))
}

// neg returns the wrapped negation.
pub fn (w Wrapping[T]) neg() Wrapping[T] {
	return wrapping(wrap_sub(T(0), w.value))
}

// wrap_add returns (a + b) modulo 2^N via the u64 domain, so no intermediate
// operation can overflow regardless of build mode.
fn wrap_add[T](a T, b T) T {
	// The mask covers all value bits: for unsigned types maxof already is the
	// mask; for signed types one more bit is needed (max | (max + 1), where
	// the +1 is exact in the u64 domain, or wraps to 0 for u64 itself).
	m := u64(maxof[T]())
	mask := m | (m + 1)
	sum, _ := bits.add_64(u64(a) & mask, u64(b) & mask, 0)
	return T(sum & mask)
}

// wrap_sub returns (a - b) modulo 2^N via the u64 domain.
fn wrap_sub[T](a T, b T) T {
	// The mask covers all value bits: for unsigned types maxof already is the
	// mask; for signed types one more bit is needed (max | (max + 1), where
	// the +1 is exact in the u64 domain, or wraps to 0 for u64 itself).
	m := u64(maxof[T]())
	mask := m | (m + 1)
	diff, _ := bits.sub_64(u64(a) & mask, u64(b) & mask, 0)
	return T(diff & mask)
}

// wrap_mul returns (a * b) modulo 2^N via the u64 domain.
fn wrap_mul[T](a T, b T) T {
	// The mask covers all value bits: for unsigned types maxof already is the
	// mask; for signed types one more bit is needed (max | (max + 1), where
	// the +1 is exact in the u64 domain, or wraps to 0 for u64 itself).
	m := u64(maxof[T]())
	mask := m | (m + 1)
	_, lo := bits.mul_64(u64(a) & mask, u64(b) & mask)
	return T(lo & mask)
}

// wrap_div returns a / b, mapping the single overflowing case (min / -1) to
// min. Division by zero still panics, like plain division.
fn wrap_div[T](a T, b T) T {
	mn := minof[T]()
	z := T(0)
	if a == mn && mn != z && b == z - T(1) {
		return mn
	}
	return a / b
}

// wrap_mod returns a % b, handling min % -1 before the backend's division
// operation can overflow. Division by zero still panics.
fn wrap_mod[T](a T, b T) T {
	mn := minof[T]()
	z := T(0)
	if a == mn && mn != z && b == z - T(1) {
		return z
	}
	return a % b
}
