module math

// divisors returns all positive divisors of n.
pub fn divisors(n i64) []i64 {
	mut out := []i64{}
	if n == 0 {
		return out
	}
	mut x := n
	if x < 0 {
		x = -x
	}
	for i := i64(1); i * i <= x; i++ {
		if x % i == 0 {
			out << i
			if i != x / i {
				out << x / i
			}
		}
	}
	return out
}
