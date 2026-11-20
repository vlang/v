module math

// partition returns the number of integer partitions of n.
pub fn partition(n int) i64 {
	if n < 0 {
		return 0
	}
	if n == 0 {
		return 1
	}
	mut p := []i64{len: n + 1}
	p[0] = 1
	for i in 1 .. n + 1 {
		for j in i .. n + 1 {
			p[j] += p[j - i]
		}
	}
	return p[n]
}
