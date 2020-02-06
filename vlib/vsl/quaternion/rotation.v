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

pub fn (q1 Quaternion) rotor_intrinsic_distance(q2 Quaternion) f64 {
        return 2.0 * q1.divide(q2).log().abs()
}

pub fn (q1 Quaternion) rotor_chordal_distance(q2 Quaternion) f64 {
        return q1.subtract(q2).abs()
}

pub fn (q1 Quaternion) rotation_intrinsic_distance(q2 Quaternion) f64 {
        return if q1.rotor_chordal_distance(q2) <= math.sqrt2 {
                2.0 * q1.divide(q2).log().abs()
        }
        else {
                2.0 * q1.divide(q2.opposite()).log().abs()
        }
}

pub fn (q1 Quaternion) rotation_chordal_distance(q2 Quaternion) f64 {
        return if q1.rotor_chordal_distance(q2) <= math.sqrt2 {
                q1.subtract(q2).abs()
        }
        else {
                q1.add(q2).abs()
        }
}
