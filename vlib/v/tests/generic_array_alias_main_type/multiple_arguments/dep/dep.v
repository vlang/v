module dep

// first returns the first argument without changing its caller-owned type.
pub fn first[T, U](value T, unused U) T {
	return value
}
