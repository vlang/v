module arrays

// insert returns a new slice with value inserted at index.
pub fn insert[T](array []T, index int, value T) []T {
	mut out := []T{cap: array.len + 1}
	if index <= 0 {
		out << value
		out << array
		return out
	}
	if index >= array.len {
		out << array
		out << value
		return out
	}
	out << array[..index]
	out << value
	out << array[index..]
	return out
}
