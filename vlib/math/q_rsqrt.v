module math

// q_sqrt computes an approximation of the inverse square root (1 / √x) using a fast
// inverse square root algorithm. This method is often used in applications
// where performance is crucial, such as in computer graphics or physics
// simulations.
// (This algorithm is inspired by the famous "fast inverse square root" code
// used in the Quake III Arena game engine.)
@[inline]
pub fn q_rsqrt(x f64) f64 {
	x_half := 0.5 * x
	mut i := i64(f64_bits(x))
	i = 0x5fe6eb50c7b537a9 - (i >> 1)
	mut j := f64_from_bits(u64(i))
	j *= (1.5 - x_half * j * j)
	j *= (1.5 - x_half * j * j)
	return j
}
