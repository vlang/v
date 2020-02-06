// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl

module math

const (
        _maxpowtwo = 4.503599627370496000e+15
)

// modf returns integer and fractional floating-point numbers
// that sum to f. Both values have the same sign as f.
//
// special cases are:
//	modf(±inf) = ±inf, nan
//	modf(nan) = nan, nan
pub fn modf(f f64) (f64,f64) {
        abs_f := abs(f)
        mut i := f64(0)
	if abs_f >= _maxpowtwo {
		i = f /* it must be an integer */
	}
        else {
		i = abs_f + _maxpowtwo /* shift fraction off right */
		i -= _maxpowtwo /* shift back without fraction */
		for i > abs_f { /* above arithmetic might round */
			i -= 1.0 /* test again just to be sure */
                }
                if f < 0.0 {
			i = -i
                }
	}
	return i, f - i /* signed fractional part */
}
