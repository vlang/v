module arrays

// index_of_first returns the index of the first element of `array`, for which the predicate fn returns true.
// If predicate does not return true for any of the elements, then index_of_first will return -1.
// Example: arrays.index_of_first([4,5,0,7,0,9], fn(idx int, x int) bool { return x == 0 }) == 2
pub fn index_of_first[T](array []T, predicate fn (idx int, elem T) bool) int {
	for i, e in array {
		if predicate(i, e) {
			return i
		}
	}
	return -1
}

// index_of_last returns the index of the last element of `array`, for which the predicate fn returns true.
// If predicate does not return true for any of the elements, then index_of_last will return -1.
// Example: arrays.index_of_last([4,5,0,7,0,9], fn(idx int, x int) bool { return x == 0 }) == 4
pub fn index_of_last[T](array []T, predicate fn (idx int, elem T) bool) int {
	for i := array.len - 1; i >= 0; i-- {
		e := array[i]
		if predicate(i, e) {
			return i
		}
	}
	return -1
}
