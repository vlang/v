module time

// eq returns true if provided time is equal to time
[inline]
pub fn (t1 Time) eq(t2 Time) bool {
	if t1.unix == t2.unix && t1.microsecond == t2.microsecond {
		return true
	}
	return false
}

// ne returns true if provided time is not equal to time
pub fn (t1 Time) ne(t2 Time) bool {
	return !t1.eq(t2)
}

// lt returns true if provided time is less than time
pub fn (t1 Time) lt(t2 Time) bool {
	if t1.unix < t2.unix || (t1.unix == t2.unix && t1.microsecond < t2.microsecond) {
		return true
	}
	return false
}

// le returns true if provided time is less or equal to time
pub fn (t1 Time) le(t2 Time) bool {
	return t1.lt(t2) || t1.eq(t2)
}

// gt returns true if provided time is greater than time
pub fn (t1 Time) gt(t2 Time) bool {
	if t1.unix > t2.unix || (t1.unix == t2.unix && t1.microsecond > t2.microsecond) {
		return true
	}
	return false
}

// ge returns true if provided time is greater or equal to time
pub fn (t1 Time) ge(t2 Time) bool {
	return t1.gt(t2) || t1.eq(t2)
}