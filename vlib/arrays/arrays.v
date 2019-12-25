module arrays

pub fn range<T>(start, end T) []T {
	mut res := []T
	for i := start; i < end; i++ {
		res << i
	}
	return res
}
