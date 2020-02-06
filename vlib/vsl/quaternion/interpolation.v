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

pub fn (start Quaternion) lerp(end Quaternion, tau f64) Quaternion {
        // if tau is 0, return start
        if tau == 0.0 {
                return start.copy()
        }

        // if tau is 1 return end
        if tau == 1.0 {
                return end.copy()
        }

        f1 := 1.0 - tau
        f2 := tau

        // this expanded form avoids calling multiply and add
        return quaternion(
                f1 * start.w + f2 * end.w,
                f1 * start.x + f2 * end.x,
                f1 * start.y + f2 * end.y,
                f1 * start.z + f2 * end.z
        )
}


pub fn (start Quaternion) nlerp(end Quaternion, tau f64) Quaternion {
        return start.lerp(end, tau).normalized()
}


pub fn (start Quaternion) slerp(end Quaternion, tau f64) Quaternion {
        return if start.rotor_chordal_distance(end) <= math.sqrt2 {
                end
                .divide(start)
                .scalar_pow(tau)
                .multiply(start)
        }
        else {
                end
                .opposite()
                .divide(start)
                .scalar_pow(tau)
                .multiply(start)
        }
}

pub fn (qi Quaternion) squad(taui f64, ai, bip1, qip1 Quaternion) Quaternion {
        return qi
                .slerp(qip1, taui)
                .slerp(
                        ai.slerp(bip1, taui),
                        2.0 * taui * (1.0 - taui)
                )
}
