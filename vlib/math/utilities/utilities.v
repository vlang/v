module utilities

import math
// copysign returns a value with the magnitude of x and the sign of y
pub fn copysign(x, y f64) f64 {
	sign := u64(1)<<63
	return math.f64_from_bits((math.f64_bits(x) & ~sign) | (math.f64_bits(y) & sign))
}
