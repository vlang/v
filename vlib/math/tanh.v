module math

const tanh_p = [
	-9.64399179425052238628e-1,
	-9.92877231001918586564e+1,
	-1.61468768441708447952e+3,
]
const tanh_q = [
	1.12811678491632931402e+2,
	2.23548839060100448583e+3,
	4.84406305325125486048e+3,
]

// tanh returns the hyperbolic tangent of x.
//
// special cases are:
// tanh(±0) = ±0
// tanh(±inf) = ±1
// tanh(nan) = nan
pub fn tanh(x f64) f64 {
	maxlog := 8.8029691931113054295988e+01 // log(2**127)
	mut z := abs(x)
	if z > 0.5 * maxlog {
		if x < 0 {
			return f64(-1)
		}
		return 1.0
	} else if z >= 0.625 {
		s := exp(2.0 * z)
		z = 1.0 - 2.0 / (s + 1.0)
		if x < 0 {
			z = -z
		}
	} else {
		if x == 0 {
			return x
		}
		s := x * x
		z = x + x * s * ((tanh_p[0] * s + tanh_p[1]) * s + tanh_p[2]) / (((s + tanh_q[0]) * s +
			tanh_q[1]) * s + tanh_q[2])
	}
	return z
}
