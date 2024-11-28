module math

const tan_p = [
	-1.30936939181383777646e+4,
	1.15351664838587416140e+6,
	-1.79565251976484877988e+7,
]
const tan_q = [
	1.00000000000000000000e+0,
	1.36812963470692954678e+4,
	-1.32089234440210967447e+6,
	2.50083801823357915839e+7,
	-5.38695755929454629881e+7,
]
const tan_dp1 = 7.853981554508209228515625e-1
const tan_dp2 = 7.94662735614792836714e-9
const tan_dp3 = 3.06161699786838294307e-17
const tan_lossth = 1.073741824e+9

// tan calculates tangent of a number
pub fn tan(a f64) f64 {
	mut x := a
	if x == 0.0 || is_nan(x) {
		return x
	}
	if is_inf(x, 0) {
		return nan()
	}
	mut sign := 1 // make argument positive but save the sign
	if x < 0 {
		x = -x
		sign = -1
	}
	if x > tan_lossth {
		return 0.0
	}
	// compute x mod pi_4
	mut y := floor(x * 4.0 / pi) // strip high bits of integer part
	mut z := ldexp(y, -3)
	z = floor(z) // integer part of y/8
	z = y - ldexp(z, 3) // y - 16 * (y/16) // integer and fractional part modulo one octant
	mut octant := int(z) // map zeros and singularities to origin
	if (octant & 1) == 1 {
		octant++
		y += 1.0
	}
	z = ((x - y * tan_dp1) - y * tan_dp2) - y * tan_dp3
	zz := z * z
	if zz > 1.0e-14 {
		y = z + z * (zz * (((tan_p[0] * zz) + tan_p[1]) * zz + tan_p[2]) / ((((zz + tan_q[1]) * zz +
			tan_q[2]) * zz + tan_q[3]) * zz + tan_q[4]))
	} else {
		y = z
	}
	if (octant & 2) == 2 {
		y = -1.0 / y
	}
	if sign < 0 {
		y = -y
	}
	return y
}

// tanf calculates tangent. (float32)
@[inline]
pub fn tanf(a f32) f32 {
	return f32(tan(a))
}

// tan calculates cotangent of a number
pub fn cot(a f64) f64 {
	mut x := a
	if x == 0.0 {
		return inf(1)
	}
	mut sign := 1 // make argument positive but save the sign
	if x < 0 {
		x = -x
		sign = -1
	}
	if x > tan_lossth {
		return 0.0
	}
	// compute x mod pi_4
	mut y := floor(x * 4.0 / pi) // strip high bits of integer part
	mut z := ldexp(y, -3)
	z = floor(z) // integer part of y/8
	z = y - ldexp(z, 3) // y - 16 * (y/16) // integer and fractional part modulo one octant
	mut octant := int(z) // map zeros and singularities to origin
	if (octant & 1) == 1 {
		octant++
		y += 1.0
	}
	z = ((x - y * tan_dp1) - y * tan_dp2) - y * tan_dp3
	zz := z * z
	if zz > 1.0e-14 {
		y = z + z * (zz * (((tan_p[0] * zz) + tan_p[1]) * zz + tan_p[2]) / ((((zz + tan_q[1]) * zz +
			tan_q[2]) * zz + tan_q[3]) * zz + tan_q[4]))
	} else {
		y = z
	}
	if (octant & 2) == 2 {
		y = -y
	} else {
		y = 1.0 / y
	}
	if sign < 0 {
		y = -y
	}
	return y
}
