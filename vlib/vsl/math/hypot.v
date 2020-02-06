// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module math

import vsl.errno

pub fn hypot(x, y f64) f64 {
        if is_inf(x, 0) || is_inf(y, 0) {
		return inf(1)
        }
	if is_nan(x) || is_nan(y) {
		return nan()
	}

        mut result := f64(0)

        if x != 0.0 || y != 0.0 {
                a := abs(x)
                b := abs(y)
                min, max := minmax(a, b)
                rat := min/max
                root_term := sqrt(f64(1.0) + rat*rat)

                if max < max_f64/root_term {
                        result = max * root_term
                }
                else {
                        errno.vsl_panic('overflow in hypot_e function', .eovrflw)
                }
        }

        return result
}
