module time

// operator `==` returns true if provided time is equal to time
[inline]
pub fn (t1 Time) == (t2 Time) bool {
	return t1.unix == t2.unix && t1.microsecond == t2.microsecond
}

// operator `<` returns true if provided time is less than time
[inline]
pub fn (t1 Time) < (t2 Time) bool {
	return t1.unix < t2.unix || (t1.unix == t2.unix && t1.microsecond < t2.microsecond)
}

// Time subtract using operator overloading.
[inline]
pub fn (lhs Time) - (rhs Time) Duration {
	lhs_micro := lhs.unix * 1_000_000 + lhs.microsecond
	rhs_micro := rhs.unix * 1_000_000 + rhs.microsecond
	return (lhs_micro - rhs_micro) * microsecond
}
