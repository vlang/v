module math

const (
	modf_maxpowtwo = 4.503599627370496000e+15
)

// modf returns integer and fractional floating-point numbers
// that sum to f. Both values have the same sign as f.
//
// special cases are:
// modf(±inf) = ±inf, nan
// modf(nan) = nan, nan
pub fn modf(f f64) (f64, f64) {
	abs_f := abs(f)
	mut i := 0.0
	if abs_f >= math.modf_maxpowtwo {
		i = f // it must be an integer
	} else {
		i = abs_f + math.modf_maxpowtwo // shift fraction off right
		i -= math.modf_maxpowtwo // shift back without fraction
		for i > abs_f { // above arithmetic might round
			i -= 1.0 // test again just to be sure
		}
		if f < 0.0 {
			i = -i
		}
	}
	return i, f - i // signed fractional part
}
