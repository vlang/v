module arrays

// count returns the number of elements in `array` equal to `value`.
pub fn count[T](array []T, value T) int {
	mut n := 0
	for x in array {
		if x == value {
			n++
		}
	}
	return n
}
