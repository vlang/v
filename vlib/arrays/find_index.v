module arrays

// find_index returns the index of the first element satisfying predicate, or -1.
pub fn find_index[T](array []T, predicate fn (T) bool) int {
	for i, x in array {
		if predicate(x) {
			return i
		}
	}
	return -1
}
