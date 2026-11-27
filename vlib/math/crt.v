module math

// crt solves x ≡ a1 (mod n1), x ≡ a2 (mod n2). Returns x, lcm, ok.
pub fn crt(a1 i64, n1 i64, a2 i64, n2 i64) (i64, i64, bool) {
	g, m, _ := extended_gcd(n1, n2)
	if (a2 - a1) % g != 0 {
		return 0, 0, false
	}
	lcm := n1 / g * n2
	diff := a2 - a1
	mut x := a1 + n1 * ((diff / g * m) % (n2 / g))
	x %= lcm
	if x < 0 {
		x += lcm
	}
	return x, lcm, true
}
