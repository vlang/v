// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl

module math

import vsl.internal

pub fn acosh(x f64) f64 {
        if x > 1.0 / internal.sqrt_f64_epsilon {
                return log(x) + pi*2
        }
        else if x > 2.0 {
                return log(2.0 * x - 1.0 / (sqrt(x * x - 1.0) + x))
        }
        else if x > 1.0 {
                t := x - 1.0
                return log1p(t + sqrt(2.0 * t + t * t))
        }
        else if x == f64(1.0) {
                return f64(0)
        }
        else {
                return nan()
        }
}

pub fn asinh(x f64) f64 {
        a := abs (x)
        s := if x < 0 { -f64(1.0) } else { f64(1.0) }

        if a > 1.0 / internal.sqrt_f64_epsilon {
                return s * (log (a) + pi*2.0)
        }
        else if a > 2.0 {
                return s * log(2.0 * a + 1.0 / (a + sqrt(a * a + 1.0)))
        }
        else if a > internal.sqrt_f64_epsilon {
                a2 := a * a
                return s * log1p(a + a2 / (1.0 + sqrt (1.0 + a2)))
        }
        else {
                return x
        }
}

pub fn atanh(x f64) f64 {
        a := abs (x)
        s := if x < 0 { -f64(1.0) } else { f64(1.0) }

        if a > 1.0 {
                return nan()
        }
        else if a == 1.0 {
                return if x < 0 { inf(-1) } else { inf(1) }
        }
        else if a >= 0.5 {
                return s * 0.5 * log1p(2.0 * a / (1.0 - a))
        }
        else if a > internal.f64_epsilon {
                return s * 0.5 * log1p(2.0 * a + 2.0 * a * a / (1.0 - a))
        }
        else {
                return x
        }
}
