module arrays

fn range<T>(start, end T) []T {
	mut res := [T(0)]
	for i := start; i < end; i++ {
		res << i
	}	
	return res
}	

