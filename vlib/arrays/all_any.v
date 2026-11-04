module arrays

// all reports whether all elements of `array` satisfy `predicate`.
pub fn all[T](array []T, predicate fn (T) bool) bool {
	for x in array {
		if !predicate(x) {
			return false
		}
	}
	return true
}

// some reports whether any element of `array` satisfies `predicate`.
pub fn some[T](array []T, predicate fn (T) bool) bool {
	for x in array {
		if predicate(x) {
			return true
		}
	}
	return false
}
