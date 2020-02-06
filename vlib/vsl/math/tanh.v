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
        TANH_P = [
                f64(-9.64399179425052238628e-1),
                -9.92877231001918586564e+1,
                -1.61468768441708447952e+3,
        ]
        TANH_Q = [
                f64(1.12811678491632931402e+2),
                2.23548839060100448583e+3,
                4.84406305325125486048e+3,
        ]
)

// tanh returns the hyperbolic tangent of x.
//
// special cases are:
//	tanh(±0) = ±0
//	tanh(±inf) = ±1
//	tanh(nan) = nan
pub fn tanh(x f64) f64 {
	maxlog := 8.8029691931113054295988e+01 // log(2**127)
	mut z := abs(x)
	if z > 0.5*maxlog {
		if x < 0 {
			return f64(-1)
		}
		return f64(1)
        }
	else if z >= 0.625 {
		s := exp(f64(2) * z)
		z = f64(1) - f64(2)/(s+f64(1))
		if x < 0 {
			z = -z
		}
        }
	else {
		if x == 0 {
			return x
		}
		s := x * x
		z = x + x*s*((TANH_P[0]*s+TANH_P[1])*s+TANH_P[2])/(((s+TANH_Q[0])*s+TANH_Q[1])*s+TANH_Q[2])
	}
	return z
}