module time

// operator `==` returns true if provided time is equal to time
@[inline]
pub fn (t1 Time) == (t2 Time) bool {
	return t1.unix() == t2.unix() && t1.nanosecond == t2.nanosecond
}

// operator `<` returns true if provided time is less than time
@[inline]
pub fn (t1 Time) < (t2 Time) bool {
	return t1.unix() < t2.unix() || (t1.unix() == t2.unix() && t1.nanosecond < t2.nanosecond)
}

// Time subtract using operator overloading.
@[inline]
pub fn (lhs Time) - (rhs Time) Duration {
	// lhs.unix * 1_000_000_000 + i64(lhs.nanosecond) will overflow i64, for years > 3000 .
	// Doing the diff first, and *then* multiplying by `second`, is less likely to overflow,
	// since lhs and rhs will be likely close to each other.
	unixs := i64(lhs.unix() - rhs.unix()) * second
	nanos := lhs.nanosecond - rhs.nanosecond
	return unixs + nanos
}
