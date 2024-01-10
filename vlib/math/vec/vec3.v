// Copyright(C) 2020-2024 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package
module vec

import math

// Vec3[T] is a generic struct representing a vector in 3D space.
pub struct Vec3[T] {
pub mut:
	x T
	y T
	z T
}

// vec3[T] returns a `Vec3` of type `T`, with `x`,`y` and `z` fields set.
pub fn vec3[T](x T, y T, z T) Vec3[T] {
	return Vec3[T]{
		x: x
		y: y
		z: z
	}
}

// zero sets the `x`,`y` and `z` fields to 0.
pub fn (mut v Vec3[T]) zero() {
	v.x = 0
	v.y = 0
	v.z = 0
}

// one sets the `x`,`y` and `z` fields to 1.
pub fn (mut v Vec3[T]) one() {
	v.x = 1
	v.y = 1
	v.z = 1
}

// copy returns a copy of this vector.
pub fn (mut v Vec3[T]) copy() Vec3[T] {
	return Vec3[T]{v.x, v.y, v.z}
}

// from sets the `x`,`y` and `z` fields from `u`.
pub fn (mut v Vec3[T]) from(u Vec3[T]) {
	v.x = u.x
	v.y = u.y
	v.z = u.z
}

// from_vec2 sets the `x` and `y` fields from `u`.
pub fn (mut v Vec3[T]) from_vec2[U](u Vec2[U]) {
	v.x = T(u.x)
	v.y = T(u.y)
}

// as_vec2 returns a Vec2 with `x` and `y` fields set from `v`.
pub fn (mut v Vec3[T]) as_vec2[T]() Vec2[T] {
	return Vec2[T]{v.x, v.y}
}

// from_vec4 sets the `x`,`y` and `z` fields from `u`.
pub fn (mut v Vec3[T]) from_vec4[U](u Vec4[U]) {
	v.x = T(u.x)
	v.y = T(u.y)
	v.z = T(u.z)
}

// as_vec4 returns a Vec4 with `x`,`y` and `z` fields set from `v`, `w` is set to 0.
pub fn (mut v Vec3[T]) as_vec4[T]() Vec4[T] {
	return Vec4[T]{v.x, v.y, v.z, 0}
}

//
// Addition
//

// + returns the resulting vector of the addition of `v` and `u`.
@[inline]
pub fn (v Vec3[T]) + (u Vec3[T]) Vec3[T] {
	return Vec3[T]{v.x + u.x, v.y + u.y, v.z + u.z}
}

// add returns the resulting vector of the addition of `v` + `u`.
pub fn (v Vec3[T]) add(u Vec3[T]) Vec3[T] {
	return v + u
}

// add_vec2 returns the resulting vector of the addition of the
// `x` and `y` fields of `u`, `z` is left untouched.
pub fn (v Vec3[T]) add_vec2[U](u Vec2[U]) Vec3[T] {
	return Vec3[T]{v.x + T(u.x), v.y + T(u.y), v.z}
}

// add_scalar returns the resulting vector of the addition of the `scalar` value.
pub fn (v Vec3[T]) add_scalar[U](scalar U) Vec3[T] {
	return Vec3[T]{v.x + T(scalar), v.y + T(scalar), v.z + T(scalar)}
}

// plus adds vector `u` to the vector.
pub fn (mut v Vec3[T]) plus(u Vec3[T]) {
	v.x += u.x
	v.y += u.y
	v.z += u.z
}

// plus_vec2 adds `x` and `y` fields of vector `u` to the vector, `z` is left untouched.
pub fn (mut v Vec3[T]) plus_vec2[U](u Vec2[U]) {
	v.x += T(u.x)
	v.y += T(u.y)
}

// plus_scalar adds the scalar `scalar` to the vector.
pub fn (mut v Vec3[T]) plus_scalar[U](scalar U) {
	v.x += T(scalar)
	v.y += T(scalar)
	v.z += T(scalar)
}

//
// Subtraction
//

// - returns the resulting vector of the subtraction of `v` and `u`.
@[inline]
pub fn (v Vec3[T]) - (u Vec3[T]) Vec3[T] {
	return Vec3[T]{v.x - u.x, v.y - u.y, v.z - u.z}
}

// sub returns the resulting vector of the subtraction of `v` - `u`.
pub fn (v Vec3[T]) sub(u Vec3[T]) Vec3[T] {
	return v - u
}

// sub_scalar returns the resulting vector of the subtraction of the `scalar` value.
pub fn (v Vec3[T]) sub_scalar[U](scalar U) Vec3[T] {
	return Vec3[T]{v.x - T(scalar), v.y - T(scalar), v.z - T(scalar)}
}

// subtract subtracts vector `u` from the vector.
pub fn (mut v Vec3[T]) subtract(u Vec3[T]) {
	v.x -= u.x
	v.y -= u.y
	v.z -= u.z
}

// subtract_scalar subtracts the scalar `scalar` from the vector.
pub fn (mut v Vec3[T]) subtract_scalar[U](scalar U) {
	v.x -= T(scalar)
	v.y -= T(scalar)
	v.z -= T(scalar)
}

//
// Multiplication
//

// * returns the resulting vector of the multiplication of `v` and `u`.
@[inline]
pub fn (v Vec3[T]) * (u Vec3[T]) Vec3[T] {
	return Vec3[T]{v.x * u.x, v.y * u.y, v.z * u.z}
}

// mul returns the resulting vector of the multiplication of `v` * `u`.
pub fn (v Vec3[T]) mul(u Vec3[T]) Vec3[T] {
	return v * u
}

// mul_scalar returns the resulting vector of the multiplication of the `scalar` value.
pub fn (v Vec3[T]) mul_scalar[U](scalar U) Vec3[T] {
	return Vec3[T]{v.x * T(scalar), v.y * T(scalar), v.z * T(scalar)}
}

// multiply multiplies the vector with `u`.
pub fn (mut v Vec3[T]) multiply(u Vec3[T]) {
	v.x *= u.x
	v.y *= u.y
	v.z *= u.z
}

// multiply_scalar multiplies the vector with `scalar`.
pub fn (mut v Vec3[T]) multiply_scalar[U](scalar U) {
	v.x *= T(scalar)
	v.y *= T(scalar)
	v.z *= T(scalar)
}

//
// Division
//

// / returns the resulting vector of the division of `v` and `u`.
@[inline]
pub fn (v Vec3[T]) / (u Vec3[T]) Vec3[T] {
	return Vec3[T]{v.x / u.x, v.y / u.y, v.z / u.z}
}

// div returns the resulting vector of the division of `v` / `u`.
pub fn (v Vec3[T]) div(u Vec3[T]) Vec3[T] {
	return v / u
}

// div_scalar returns the resulting vector of the division by the `scalar` value.
pub fn (v Vec3[T]) div_scalar[U](scalar U) Vec3[T] {
	return Vec3[T]{v.x / T(scalar), v.y / T(scalar), v.z / T(scalar)}
}

// divide divides the vector by `u`.
pub fn (mut v Vec3[T]) divide(u Vec3[T]) {
	v.x /= u.x
	v.y /= u.y
	v.z /= u.z
}

// divide_scalar divides the vector by `scalar`.
pub fn (mut v Vec3[T]) divide_scalar[U](scalar U) {
	v.x /= T(scalar)
	v.y /= T(scalar)
	v.z /= T(scalar)
}

//
// Utility
//

// magnitude returns the magnitude, also known as the length, of the vector.
pub fn (v Vec3[T]) magnitude() T {
	if v.x == 0 && v.y == 0 && v.z == 0 {
		return T(0)
	}
	return T(math.sqrt((v.x * v.x) + (v.y * v.y) + (v.z * v.z)))
}

// dot returns the dot product of `v` and `u`.
pub fn (v Vec3[T]) dot(u Vec3[T]) T {
	return T((v.x * u.x) + (v.y * u.y) + (v.z * u.z))
}

// cross returns the cross product of `v` and `u`.
pub fn (v Vec3[T]) cross(u Vec3[T]) Vec3[T] {
	return Vec3[T]{
		x: (v.y * u.z) - (v.z * u.y)
		y: (v.z * u.x) - (v.x * u.z)
		z: (v.x * u.y) - (v.y * u.x)
	}
}

// unit returns the unit vector.
// unit vectors always have a magnitude, or length, of exactly 1.
pub fn (v Vec3[T]) unit() Vec3[T] {
	m := v.magnitude()
	return Vec3[T]{v.x / m, v.y / m, v.z / m}
}

// perpendicular returns the `u` projected perpendicular vector to this vector.
pub fn (v Vec3[T]) perpendicular(u Vec3[T]) Vec3[T] {
	return v - v.project(u)
}

// project returns the projected vector.
pub fn (v Vec3[T]) project(u Vec3[T]) Vec3[T] {
	percent := v.dot(u) / u.dot(v)
	return u.mul_scalar(percent)
}

// eq returns a bool indicating if the two vectors are equal.
@[inline]
pub fn (v Vec3[T]) eq(u Vec3[T]) bool {
	return v.x == u.x && v.y == u.y && v.z == u.z
}

// eq_epsilon returns a bool indicating if the two vectors are equal within the module `vec_epsilon` const.
pub fn (v Vec3[T]) eq_epsilon(u Vec3[T]) bool {
	return v.eq_approx[T, f32](u, vec_epsilon)
}

// eq_approx returns whether these vectors are approximately equal within `tolerance`.
pub fn (v Vec3[T]) eq_approx[T, U](u Vec3[T], tolerance U) bool {
	diff_x := math.abs(v.x - u.x)
	diff_y := math.abs(v.y - u.y)
	diff_z := math.abs(v.z - u.z)
	if diff_x <= tolerance && diff_y <= tolerance && diff_z <= tolerance {
		return true
	}

	max_x := math.max(math.abs(v.x), math.abs(u.x))
	max_y := math.max(math.abs(v.y), math.abs(u.y))
	max_z := math.max(math.abs(v.z), math.abs(u.z))
	if diff_x < max_x * tolerance && diff_y < max_y * tolerance && diff_z < max_z * tolerance {
		return true
	}
	return false
}

// is_approx_zero returns whether this vector is equal to zero within `tolerance`.
pub fn (v Vec3[T]) is_approx_zero(tolerance f64) bool {
	if math.abs(v.x) <= tolerance && math.abs(v.y) <= tolerance && math.abs(v.z) <= tolerance {
		return true
	}
	return false
}

// eq_scalar returns a bool indicating if the `x`,`y` and `z` fields all equals `scalar`.
pub fn (v Vec3[T]) eq_scalar[U](scalar U) bool {
	return v.x == T(scalar) && v.y == T(scalar) && v.z == T(scalar)
}

// distance returns the distance to the vector `u`.
pub fn (v Vec3[T]) distance(u Vec3[T]) f64 {
	return math.sqrt((v.x - u.x) * (v.x - u.x) + (v.y - u.y) * (v.y - u.y) +
		(v.z - u.z) * (v.z - u.z))
}

// manhattan_distance returns the Manhattan distance to the vector `u`.
pub fn (v Vec3[T]) manhattan_distance(u Vec3[T]) f64 {
	return math.abs(v.x - u.x) + math.abs(v.y - u.y) + math.abs(v.z - u.z)
}

// angle_between returns the angle in radians to the vector `u`.
pub fn (v Vec3[T]) angle_between(u Vec3[T]) T {
	$if T is f64 {
		return math.acos(((v.x * u.x) + (v.y * u.y) + (v.z * u.z)) / math.sqrt((v.x * v.x) +
			(v.y * v.y) + (v.z * v.z)) * math.sqrt((u.x * u.x) + (u.y * u.y) + (u.z * u.z)))
	} $else {
		return T(math.acos(f64((v.x * u.x) + (v.y * u.y) + (v.z * u.z)) / math.sqrt(
			f64(v.x * v.x) + (v.y * v.y) + (v.z * v.z)) * math.sqrt(f64(u.x * u.x) + (u.y * u.y) +
			(u.z * u.z))))
	}
}

// abs sets `x`, `y` and `z` field values to their absolute values.
pub fn (mut v Vec3[T]) abs() {
	if v.x < 0 {
		v.x = math.abs(v.x)
	}
	if v.y < 0 {
		v.y = math.abs(v.y)
	}
	if v.z < 0 {
		v.z = math.abs(v.z)
	}
}

// clean returns a vector with all fields of this vector set to zero (0) if they fall within `tolerance`.
pub fn (v Vec3[T]) clean[U](tolerance U) Vec3[T] {
	mut r := v.copy()
	if math.abs(v.x) < tolerance {
		r.x = 0
	}
	if math.abs(v.y) < tolerance {
		r.y = 0
	}
	if math.abs(v.z) < tolerance {
		r.z = 0
	}
	return r
}

// clean_tolerance sets all fields to zero (0) if they fall within `tolerance`.
pub fn (mut v Vec3[T]) clean_tolerance[U](tolerance U) {
	if math.abs(v.x) < tolerance {
		v.x = 0
	}
	if math.abs(v.y) < tolerance {
		v.y = 0
	}
	if math.abs(v.z) < tolerance {
		v.z = 0
	}
}

// inv returns the inverse, or reciprocal, of the vector.
pub fn (v Vec3[T]) inv() Vec3[T] {
	return Vec3[T]{
		x: if v.x != 0 { T(1) / v.x } else { 0 }
		y: if v.y != 0 { T(1) / v.y } else { 0 }
		z: if v.z != 0 { T(1) / v.z } else { 0 }
	}
}

// normalize normalizes the vector.
pub fn (v Vec3[T]) normalize() Vec3[T] {
	m := v.magnitude()
	if m == 0 {
		return vec3[T](0, 0, 0)
	}
	return Vec3[T]{
		x: v.x * (1 / m)
		y: v.y * (1 / m)
		z: v.z * (1 / m)
	}
}

// sum returns a sum of all the fields.
pub fn (v Vec3[T]) sum() T {
	return v.x + v.y + v.z
}
