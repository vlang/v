module arrays

fn range<T>(start, end T) []T {
	mut res := [start]
	for i := start + 1; i < end; i++ {
		res << i
	}	
	return res
}	

