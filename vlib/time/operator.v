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
	lhs_micro := lhs.unix * 1000 * 1000 + u64(lhs.microsecond)
	rhs_micro := rhs.unix * 1000 * 1000 + u64(rhs.microsecond)
	return (i64(lhs_micro) - i64(rhs_micro)) * microsecond
}
