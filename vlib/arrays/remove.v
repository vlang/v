module arrays

// remove returns a new slice with the element at `index` removed.
pub fn remove[T](array []T, index int) []T {
	if index < 0 || index >= array.len {
		return array.clone()
	}
	mut out := []T{cap: array.len - 1}
	out << array[..index]
	out << array[index + 1..]
	return out
}
