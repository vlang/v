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
        TAN_P = [
        f64(-1.30936939181383777646e+4),
        1.15351664838587416140e+6,
        -1.79565251976484877988e+7
        ]
        TAN_Q = [
        f64(1.00000000000000000000e+0),
        1.36812963470692954678e+4,
        -1.32089234440210967447e+6,
        2.50083801823357915839e+7,
        -5.38695755929454629881e+7
        ]
        TAN_DP1 = 7.853981554508209228515625e-1
        TAN_DP2 = 7.94662735614792836714e-9
        TAN_DP3 = 3.06161699786838294307e-17
        tan_lossth = 1.073741824e+9
)

pub fn tan(x_ f64) f64 {
        mut x := x_

        if x == 0.0 || is_nan(x) {
                return x
        }
        if is_inf(x, 0) {
                return nan()
        }

        mut sign := 1

        /* make argument positive but save the sign */
        if x < 0 {
                x = -x
                sign = -1
        }

        if x > tan_lossth {
                return f64(0)
        }

        /* compute x mod pi_4 */
        mut y := floor(x * 4.0 / pi)

        /* strip high bits of integer part */
        mut z := ldexp(y, -3)
        z = floor(z)		/* integer part of y/8 */
        z = y - ldexp(z, 3)  /* y - 16 * (y/16) */

        /* integer and fractional part modulo one octant */
        mut octant := int(z)

        /* map zeros and singularities to origin */
        if (octant & 1) == 1 {
                octant++
                y += 1.0
        }

        z = ((x - y * TAN_DP1) - y * TAN_DP2) - y * TAN_DP3

        zz := z * z

        if zz > 1.0e-14 {
                y = z + z*(zz*(((TAN_P[0]*zz)+TAN_P[1])*zz+TAN_P[2])/((((zz+TAN_Q[1])*zz+TAN_Q[2])*zz+TAN_Q[3])*zz+TAN_Q[4]))
        }
        else {
                y = z
        }

        if (octant & 2) == 2 {
                y = -1.0/y
        }

        if sign < 0 {
                y = -y
        }

        return y
}

pub fn cot(x_ f64) f64 {
        mut x := x_

        if x == 0.0 {
                return inf(1)
        }

        mut sign := 1

        /* make argument positive but save the sign */
        if x < 0 {
                x = -x
                sign = -1
        }

        if x > tan_lossth {
                return f64(0)
        }

        /* compute x mod pi_4 */
        mut y := floor(x * 4.0 / pi)

        /* strip high bits of integer part */
        mut z := ldexp(y, -3)
        z = floor(z)		/* integer part of y/8 */
        z = y - ldexp(z, 3)  /* y - 16 * (y/16) */

        /* integer and fractional part modulo one octant */
        mut octant := int(z)

        /* map zeros and singularities to origin */
        if (octant & 1) == 1 {
                octant++
                y += 1.0
        }

        z = ((x - y * TAN_DP1) - y * TAN_DP2) - y * TAN_DP3

        zz := z * z

        if zz > 1.0e-14 {
                y = z + z*(zz*(((TAN_P[0]*zz)+TAN_P[1])*zz+TAN_P[2])/((((zz+TAN_Q[1])*zz+TAN_Q[2])*zz+TAN_Q[3])*zz+TAN_Q[4]))
        }
        else {
                y = z
        }

        if (octant & 2) == 2 {
                y = -y
        } else {
                y = 1.0/y
        }

        if sign < 0 {
                y = -y
        }

        return y
}