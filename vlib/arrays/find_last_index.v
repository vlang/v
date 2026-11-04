module arrays

// find_last_index returns the last index of an element satisfying predicate, or -1.
pub fn find_last_index[T](array []T, predicate fn (T) bool) int {
	for i := array.len - 1; i >= 0; i-- {
		if predicate(array[i]) {
			return i
		}
	}
	return -1
}
