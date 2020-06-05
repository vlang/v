module time

// ==
fn (t1 Time) eq(t2 Time) bool {
	if t1.unix == t2.unix && t1.microsecond == t2.microsecond {
		return true
	}
	return false
}

// !=
fn (t1 Time) ne(t2 Time) bool {
	return !t1.eq(t2)
}
