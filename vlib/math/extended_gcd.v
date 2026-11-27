module math

// extended_gcd returns g, x, y such that a*x + b*y = g = gcd(a,b).
pub fn extended_gcd(a i64, b i64) (i64, i64, i64) {
	if b == 0 {
		return a, 1, 0
	}
	g, x1, y1 := extended_gcd(b, a % b)
	return g, y1, x1 - (a / b) * y1
}
