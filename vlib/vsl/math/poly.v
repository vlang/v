// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl

module math

[inline]
fn poly_n_eval(c []f64, n int, x f64) f64 {
	if c.len == 0 {
                panic('coeficients can not be empty')
        }

        len := int(min(c.len, n))
        mut ans := c[len-1]

        for e in c[..len-1] {
                ans = e + x * ans
        }

        return ans
}

[inline]
fn poly_n_1_eval(c []f64, n int, x f64) f64 {
        if c.len == 0 {
                panic('coeficients can not be empty')
        }

        len := int(min(c.len, n)) - 1
        mut ans := c[len-1]

        for e in c[..len-1] {
                ans = e + x * ans
        }

        return ans
}

[inline]
fn poly_eval(c []f64, x f64) f64 {
        return poly_n_eval(c, c.len, x)
}

[inline]
fn poly_1_eval(c []f64, x f64) f64 {
        return poly_n_1_eval(c, c.len, x)
}
