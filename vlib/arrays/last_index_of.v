module arrays

// last_index_of returns the last index of `value` in `array`, or -1.
pub fn last_index_of[T](array []T, value T) int {
	for i := array.len - 1; i >= 0; i-- {
		if array[i] == value {
			return i
		}
	}
	return -1
}
