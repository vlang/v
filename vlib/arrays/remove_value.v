module arrays

// remove_value returns a new slice with all occurrences of value removed.
pub fn remove_value[T](array []T, value T) []T {
	mut out := []T{}
	for x in array {
		if x != value {
			out << x
		}
	}
	return out
}
