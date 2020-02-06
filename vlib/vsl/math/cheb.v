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

/* data for a Chebyshev series over a given interval */

pub struct ChebSeries {
pub:
        c        []f64      // coefficients               
        order    int        // order of expansion         
        a        f64        // lower interval point       
        b        f64        // upper interval point       
}

fn (cs ChebSeries) eval_e(x f64) (f64, f64) {
        mut d := f64(0)
        mut dd := f64(0)

        y  := (2.0*x - cs.a - cs.b) / (cs.b - cs.a)
        y2 := 2.0 * y

        mut e := f64(0)
        mut temp := f64(0)

        for j := cs.order; j >= 1; j-- {
                temp = d
                d = y2*d - dd + cs.c[j]
                e += abs(y2*temp) + abs(dd) + abs(cs.c[j])
                dd = temp
        }

        temp = d
        d = y*d - dd + 0.5 * cs.c[0]
        e += abs(y*temp) + abs(dd) + 0.5 * abs(cs.c[0])

        return d, f64(internal.f64_epsilon) * e + abs(cs.c[cs.order])
}
