module math

// scalbn scales x by FLT_RADIX raised to the power of n, returning the same as:
// scalbn(x,n) = x * FLT_RADIX ** n
pub fn scalbn(x f64, n_ int) f64 {
	mut n := n_
	x1p1023 := f64_from_bits(u64(0x7fe0000000000000))
	x1p53 := f64_from_bits(u64(0x4340000000000000))
	x1p_1022 := f64_from_bits(u64(0x0010000000000000))

	mut y := x
	if n > 1023 {
		y *= x1p1023
		n -= 1023
		if n > 1023 {
			y *= x1p1023
			n -= 1023
			if n > 1023 {
				n = 1023
			}
		}
	} else if n < -1022 {
		/*
		make sure final n < -53 to avoid double
        rounding in the subnormal range
		*/
		y *= x1p_1022 * x1p53
		n += 1022 - 53
		if n < -1022 {
			y *= x1p_1022 * x1p53
			n += 1022 - 53
			if n < -1022 {
				n = -1022
			}
		}
	}
	return y * f64_from_bits(u64((0x3ff + n)) << 52)
}
