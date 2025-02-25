// Copyright(C) 2020-2024 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package
module vec

import math

pub const vec_epsilon = f32(10e-7)

// Vec2[T] is a generic struct representing a vector in 2D space.
pub struct Vec2[T] {
pub mut:
	x T
	y T
}

// vec2[T] returns a `Vec2` of type `T`, with `x` and `y` fields set.
pub fn vec2[T](x T, y T) Vec2[T] {
	return Vec2[T]{
		x: x
		y: y
	}
}

// zero sets the `x` and `y` fields to 0.
pub fn (mut v Vec2[T]) zero() {
	v.x = 0
	v.y = 0
}

// one sets the `x` and `y` fields to 1.
pub fn (mut v Vec2[T]) one() {
	v.x = 1
	v.y = 1
}

// copy returns a copy of this vector.
pub fn (v Vec2[T]) copy() Vec2[T] {
	return Vec2[T]{v.x, v.y}
}

// from sets the `x` and `y` fields from `u`.
pub fn (mut v Vec2[T]) from(u Vec2[T]) {
	v.x = u.x
	v.y = u.y
}

// from_vec3 sets the `x` and `y` fields from `u`.
pub fn (mut v Vec2[T]) from_vec3[U](u Vec3[U]) {
	v.x = T(u.x)
	v.y = T(u.y)
}

// as_vec3 returns a Vec3 with `x` and `y` fields set from `v`, `z` is set to 0.
pub fn (v Vec2[T]) as_vec3[T]() Vec3[T] {
	return Vec3[T]{v.x, v.y, 0}
}

// from_vec4 sets the `x` and `y` fields from `u`.
pub fn (mut v Vec2[T]) from_vec4[U](u Vec4[U]) {
	v.x = T(u.x)
	v.y = T(u.y)
}

// as_vec4 returns a Vec4 with `x` and `y` fields set from `v`, `z` and `w` is set to 0.
pub fn (v Vec2[T]) as_vec4[T]() Vec4[T] {
	return Vec4[T]{v.x, v.y, 0, 0}
}

//
// Addition
//

// + returns the resulting vector of the addition of `v` and `u`.
@[inline]
pub fn (v Vec2[T]) + (u Vec2[T]) Vec2[T] {
	return Vec2[T]{v.x + u.x, v.y + u.y}
}

// add returns the resulting vector of the addition of `v` + `u`.
pub fn (v Vec2[T]) add(u Vec2[T]) Vec2[T] {
	return v + u
}

// add_scalar returns the resulting vector of the addition of the `scalar` value.
pub fn (v Vec2[T]) add_scalar[U](scalar U) Vec2[T] {
	return Vec2[T]{v.x + T(scalar), v.y + T(scalar)}
}

// plus adds vector `u` to the vector.
pub fn (mut v Vec2[T]) plus(u Vec2[T]) {
	v.x += u.x
	v.y += u.y
}

// plus_scalar adds the scalar `scalar` to the vector.
pub fn (mut v Vec2[T]) plus_scalar[U](scalar U) {
	v.x += T(scalar)
	v.y += T(scalar)
}

//
// Subtraction
//

// - returns the resulting vector of the subtraction of `v` and `u`.
@[inline]
pub fn (v Vec2[T]) - (u Vec2[T]) Vec2[T] {
	return Vec2[T]{v.x - u.x, v.y - u.y}
}

// sub returns the resulting vector of the subtraction of `v` - `u`.
pub fn (v Vec2[T]) sub(u Vec2[T]) Vec2[T] {
	return v - u
}

// sub_scalar returns the resulting vector of the subtraction of the `scalar` value.
pub fn (v Vec2[T]) sub_scalar[U](scalar U) Vec2[T] {
	return Vec2[T]{v.x - T(scalar), v.y - T(scalar)}
}

// subtract subtracts vector `u` from the vector.
pub fn (mut v Vec2[T]) subtract(u Vec2[T]) {
	v.x -= u.x
	v.y -= u.y
}

// subtract_scalar subtracts the scalar `scalar` from the vector.
pub fn (mut v Vec2[T]) subtract_scalar[U](scalar U) {
	v.x -= T(scalar)
	v.y -= T(scalar)
}

//
// Multiplication
//

// * returns the resulting vector of the multiplication of `v` and `u`.
@[inline]
pub fn (v Vec2[T]) * (u Vec2[T]) Vec2[T] {
	return Vec2[T]{v.x * u.x, v.y * u.y}
}

// mul returns the resulting vector of the multiplication of `v` * `u`.
pub fn (v Vec2[T]) mul(u Vec2[T]) Vec2[T] {
	return v * u
}

// mul_scalar returns the resulting vector of the multiplication of the `scalar` value.
pub fn (v Vec2[T]) mul_scalar[U](scalar U) Vec2[T] {
	return Vec2[T]{v.x * T(scalar), v.y * T(scalar)}
}

// multiply multiplies the vector with `u`.
pub fn (mut v Vec2[T]) multiply(u Vec2[T]) {
	v.x *= u.x
	v.y *= u.y
}

// multiply_scalar multiplies the vector with `scalar`.
pub fn (mut v Vec2[T]) multiply_scalar[U](scalar U) {
	v.x *= T(scalar)
	v.y *= T(scalar)
}

//
// Division
//

// / returns the resulting vector of the division of `v` and `u`.
@[inline]
pub fn (v Vec2[T]) / (u Vec2[T]) Vec2[T] {
	return Vec2[T]{v.x / u.x, v.y / u.y}
}

// div returns the resulting vector of the division of `v` / `u`.
pub fn (v Vec2[T]) div(u Vec2[T]) Vec2[T] {
	return v / u
}

// div_scalar returns the resulting vector of the division by the `scalar` value.
pub fn (v Vec2[T]) div_scalar[U](scalar U) Vec2[T] {
	return Vec2[T]{v.x / T(scalar), v.y / T(scalar)}
}

// divide divides the vector by `u`.
pub fn (mut v Vec2[T]) divide(u Vec2[T]) {
	v.x /= u.x
	v.y /= u.y
}

// divide_scalar divides the vector by `scalar`.
pub fn (mut v Vec2[T]) divide_scalar[U](scalar U) {
	v.x /= T(scalar)
	v.y /= T(scalar)
}

//
// Utility
//

// magnitude returns the magnitude, also known as the length, of the vector.
pub fn (v Vec2[T]) magnitude() T {
	if v.x == 0 && v.y == 0 {
		return T(0)
	}
	$if T is f64 {
		return math.sqrt((v.x * v.x) + (v.y * v.y))
	} $else {
		return T(math.sqrt(f64(v.x * v.x) + f64(v.y * v.y)))
	}
}

// magnitude_x returns the magnitude, also known as the length, of the 1D vector field x, y is ignored.
pub fn (v Vec2[T]) magnitude_x() T {
	return T(math.sqrt(v.x * v.x))
}

// magnitude_x returns the magnitude, also known as the length, of the 1D vector field y, x is ignored.
pub fn (v Vec2[T]) magnitude_y() T {
	return T(math.sqrt(v.y * v.y))
}

// dot returns the dot product of `v` and `u`.
pub fn (v Vec2[T]) dot(u Vec2[T]) T {
	return (v.x * u.x) + (v.y * u.y)
}

// cross returns the cross product of `v` and `u`.
pub fn (v Vec2[T]) cross(u Vec2[T]) T {
	return (v.x * u.y) - (v.y * u.x)
}

// unit returns the unit vector.
// unit vectors always have a magnitude, or length, of exactly 1.
pub fn (v Vec2[T]) unit() Vec2[T] {
	m := v.magnitude()
	return Vec2[T]{v.x / m, v.y / m}
}

// perp_cw returns the clockwise, or "left-hand", perpendicular vector of this vector.
pub fn (v Vec2[T]) perp_cw() Vec2[T] {
	return Vec2[T]{v.y, -v.x}
}

// perp_ccw returns the counter-clockwise, or "right-hand", perpendicular vector of this vector.
pub fn (v Vec2[T]) perp_ccw() Vec2[T] {
	return Vec2[T]{-v.y, v.x}
}

// perpendicular returns the `u` projected perpendicular vector to this vector.
pub fn (v Vec2[T]) perpendicular(u Vec2[T]) Vec2[T] {
	return v - v.project(u)
}

// project returns the projected vector.
pub fn (v Vec2[T]) project(u Vec2[T]) Vec2[T] {
	percent := v.dot(u) / u.dot(v)
	return u.mul_scalar(percent)
}

// rotate_around_cw returns the vector `v` rotated *clockwise* `radians` around an origin vector `o` in Cartesian space.
pub fn (v Vec2[T]) rotate_around_cw(o Vec2[T], radians f64) Vec2[T] {
	return v.rotate_around_ccw(o, -radians)
}

// rotate_around_ccw returns the vector `v` rotated *counter-clockwise* `radians` around an origin vector `o` in Cartesian space.
pub fn (v Vec2[T]) rotate_around_ccw(o Vec2[T], radians f64) Vec2[T] {
	s := math.sin(radians)
	c := math.cos(radians)
	dx := v.x - o.x
	dy := v.y - o.y
	return Vec2[T]{
		x: T(c * dx - s * dy + o.x)
		y: T(s * dx + c * dy + o.y)
	}
}

// eq returns a bool indicating if the two vectors are equal.
@[inline]
pub fn (v Vec2[T]) eq(u Vec2[T]) bool {
	return v.x == u.x && v.y == u.y
}

// eq_epsilon returns a bool indicating if the two vectors are equal within the module `vec_epsilon` const.
pub fn (v Vec2[T]) eq_epsilon(u Vec2[T]) bool {
	return v.eq_approx[T, f32](u, vec_epsilon)
}

// eq_approx returns whether these vectors are approximately equal within `tolerance`.
pub fn (v Vec2[T]) eq_approx[T, U](u Vec2[T], tolerance U) bool {
	diff_x := math.abs(v.x - u.x)
	diff_y := math.abs(v.y - u.y)
	if diff_x <= tolerance && diff_y <= tolerance {
		return true
	}

	max_x := math.max(math.abs(v.x), math.abs(u.x))
	max_y := math.max(math.abs(v.y), math.abs(u.y))
	if diff_x < max_x * tolerance && diff_y < max_y * tolerance {
		return true
	}

	return false
}

// is_approx_zero returns whether this vector is equal to zero within `tolerance`.
pub fn (v Vec2[T]) is_approx_zero(tolerance T) bool {
	if math.abs(v.x) <= tolerance && math.abs(v.y) <= tolerance {
		return true
	}
	return false
}

// eq_scalar returns a bool indicating if the `x` and `y` fields both equals `scalar`.
pub fn (v Vec2[T]) eq_scalar[U](scalar U) bool {
	return v.x == T(scalar) && v.y == scalar
}

// distance returns the distance to the vector `u`.
pub fn (v Vec2[T]) distance(u Vec2[T]) T {
	$if T is f64 {
		return math.sqrt((v.x - u.x) * (v.x - u.x) + (v.y - u.y) * (v.y - u.y))
	} $else {
		return T(math.sqrt(f64(v.x - u.x) * f64(v.x - u.x) + f64(v.y - u.y) * f64(v.y - u.y)))
	}
}

// manhattan_distance returns the Manhattan Distance to the vector `u`.
pub fn (v Vec2[T]) manhattan_distance(u Vec2[T]) T {
	return math.abs(v.x - u.x) + math.abs(v.y - u.y)
}

// angle_between returns the angle in radians to the vector `u`.
pub fn (v Vec2[T]) angle_between(u Vec2[T]) T {
	$if T is f64 {
		return math.atan2(v.cross(u), v.dot(u))
	} $else {
		return T(math.atan2(f64(v.cross(u)), f64(v.dot(u))))
	}
}

// angle_towards returns the angle in radians between the horizontal axis,
// and a line passing through the first and second point, as if the first point
// was at the center of the coordinate system.
pub fn (p1 Vec2[T]) angle_towards(p2 Vec2[T]) T {
	$if T is f64 {
		return math.atan2(p2.y - p1.y, p2.x - p1.x)
	} $else {
		return T(math.atan2(f64(p2.y) - f64(p1.y), f64(p2.x) - f64(p1.x)))
	}
}

// angle returns the angle in radians of the vector.
pub fn (v Vec2[T]) angle() T {
	$if T is f64 {
		return math.atan2(v.y, v.x)
	} $else {
		return T(math.atan2(f64(v.y), f64(v.x)))
	}
}

// abs sets `x` and `y` field values to their absolute values.
pub fn (mut v Vec2[T]) abs() {
	if v.x < 0 {
		v.x = math.abs(v.x)
	}
	if v.y < 0 {
		v.y = math.abs(v.y)
	}
}

// clean returns a vector with all fields of this vector set to zero (0) if they fall within `tolerance`.
pub fn (v Vec2[T]) clean[U](tolerance U) Vec2[T] {
	mut r := v.copy()
	if math.abs(v.x) < tolerance {
		r.x = 0
	}
	if math.abs(v.y) < tolerance {
		r.y = 0
	}
	return r
}

// clean_tolerance sets all fields to zero (0) if they fall within `tolerance`.
pub fn (mut v Vec2[T]) clean_tolerance[U](tolerance U) {
	if math.abs(v.x) < tolerance {
		v.x = 0
	}
	if math.abs(v.y) < tolerance {
		v.y = 0
	}
}

// inv returns the inverse, or reciprocal, of the vector.
pub fn (v Vec2[T]) inv() Vec2[T] {
	return Vec2[T]{
		x: if v.x != 0 { T(1) / v.x } else { 0 }
		y: if v.y != 0 { T(1) / v.y } else { 0 }
	}
}

// normalize normalizes the vector.
pub fn (v Vec2[T]) normalize() Vec2[T] {
	m := v.magnitude()
	if m == 0 {
		return vec2[T](0, 0)
	}
	return Vec2[T]{
		x: v.x * (1 / m)
		y: v.y * (1 / m)
	}
}

// sum returns a sum of all the fields.
pub fn (v Vec2[T]) sum() T {
	return v.x + v.y
}
