module utilities
import math

pub fn copysign(x f64, y f64) f64 {

	if(y > 0. || (y == 0. && math.atan2(y, -1.) > 0.)) {
		return math.abs(x)
        } else {
            return -math.abs(x)
        }
}
