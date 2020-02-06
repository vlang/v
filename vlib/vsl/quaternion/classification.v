// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl

module quaternion

import vsl.math

pub fn (q Quaternion) is_nan() bool {
        return math.is_nan(q.w) || math.is_nan(q.x) || math.is_nan(q.y) || math.is_nan(q.z)
}

pub fn (q Quaternion) is_zero() bool {
        return if q.is_nan() {
                true
        }
        else {
                q.w == 0.0 &&
                q.x == 0.0 &&
                q.y == 0.0 &&
                q.z == 0.0
        }
}

pub fn (q Quaternion) is_inf() bool {
        return math.is_inf(q.w, 0) || math.is_inf(q.x, 0) || math.is_inf(q.y, 0) || math.is_inf(q.z, 0)
}

fn is_finite(a f64) bool {
        return !math.is_nan(a) && !math.is_inf(a, 0)
}

pub fn (q Quaternion) is_finite() bool {
        return is_finite(q.w) && is_finite(q.x) && is_finite(q.y) && is_finite(q.z)
}


pub fn (q1 Quaternion) equal(q2 Quaternion) bool {
        return
                !q1.is_nan() &&
                !q2.is_nan() &&
                q1.w == q2.w &&
                q1.x == q2.x &&
                q1.y == q2.y &&
                q1.z == q2.z
}

fn choose(c, a, b bool) bool {
	return if c { a } else { b }
}

pub fn (q1 Quaternion) is_less(q2 Quaternion) bool {
        return 
                (!q1.is_nan() && !q2.is_nan()) &&
                choose(q1.w != q2.w, q1.w < q2.w,
                        choose(q1.x != q2.x, q1.x < q2.x,
                                choose(q1.y != q2.y, q1.y < q2.y,
                                        choose(q1.z != q2.z, q1.z < q2.z, false)
                                )
                        )
                )
}

pub fn (q1 Quaternion) is_greater(q2 Quaternion) bool {
        return 
                (!q1.is_nan() && !q2.is_nan()) &&
                choose(q1.w != q2.w, q1.w > q2.w,
                        choose(q1.x != q2.x, q1.x > q2.x,
                                choose(q1.y != q2.y, q1.y > q2.y,
                                        choose(q1.z != q2.z, q1.z > q2.z, false)
                                )
                        )
                )
}

pub fn (q1 Quaternion) is_lessequal(q2 Quaternion) bool {
        return 
                (!q1.is_nan() && !q2.is_nan()) &&
                choose(q1.w != q2.w, q1.w < q2.w,
                        choose(q1.x != q2.x, q1.x < q2.x,
                                choose(q1.y != q2.y, q1.y < q2.y,
                                        choose(q1.z != q2.z, q1.z < q2.z, true)
                                )
                        )
                )

        /* Note that the final possibility __is 1, whereas in
         * `is_less` it was 0.  This distinction correctly
         * accounts for equality.
         */
}

pub fn (q1 Quaternion) is_greaterequal(q2 Quaternion) bool {
        return 
                (!q1.is_nan() && !q2.is_nan()) &&
                choose(q1.w != q2.w, q1.w > q2.w,
                        choose(q1.x != q2.x, q1.x > q2.x,
                                choose(q1.y != q2.y, q1.y > q2.y,
                                        choose(q1.z != q2.z, q1.z > q2.z, true)
                                )
                        )
                )
        /* Note that the final possibility __is 1, whereas in
         * `is_greater` it was 0.  This distinction correctly
         * accounts for equality.
         */
}
