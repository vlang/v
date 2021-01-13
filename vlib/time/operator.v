module time

// operator `==` returns true if provided time is equal to time
[inline]
pub fn (t1 Time) == (t2 Time) bool {
	if t1.unix == t2.unix && t1.microsecond == t2.microsecond {
		return true
	}
	return false
}

// operator `!=` returns true if provided time is not equal to time
[inline]
pub fn (t1 Time) != (t2 Time) bool {
	return !(t1 == t2)
}

// operator `<` returns true if provided time is less than time
[inline]
pub fn (t1 Time) < (t2 Time) bool {
	if t1.unix < t2.unix || (t1.unix == t2.unix && t1.microsecond < t2.microsecond) {
		return true
	}
	return false
}

// operator `<=` returns true if provided time is less or equal to time
[inline]
pub fn (t1 Time) <= (t2 Time) bool {
	return t1 < t2 || t1 == t2
}

// operator `>` returns true if provided time is greater than time
[inline]
pub fn (t1 Time) > (t2 Time) bool {
	if t1.unix > t2.unix || (t1.unix == t2.unix && t1.microsecond > t2.microsecond) {
		return true
	}
	return false
}

// operator `>=` returns true if provided time is greater or equal to time
[inline]
pub fn (t1 Time) >= (t2 Time) bool {
	return t1 > t2 || t1 == t2
}

// Time subtract using operator overloading.
[inline]
pub fn (lhs Time) - (rhs Time) Duration {
	lhs_micro := lhs.unix * 1000 * 1000 + u64(lhs.microsecond)
	rhs_micro := rhs.unix * 1000 * 1000 + u64(rhs.microsecond)
	return (i64(lhs_micro) - i64(rhs_micro)) * microsecond
}
