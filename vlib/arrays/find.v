module arrays

// find returns the first element satisfying predicate, or none.
pub fn find[T](array []T, predicate fn (T) bool) ?T {
	for x in array {
		if predicate(x) {
			return x
		}
	}
	return none
}
