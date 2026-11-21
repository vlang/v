module structs

pub struct Pair {
pub:
	a int
	b f64
	c bool
}

pub fn sum(p Pair) f64 {
	return p.a + p.b
}

pub fn make(n int) Pair {
	return Pair{ a: n, b: 1.5, c: true }
}
