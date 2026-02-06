/**********************************************************************
* Simple vector/matrix utility
* Copyright (c) 2024 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
**********************************************************************/
module m4

import math

pub struct Vec4 {
pub mut:
	e [4]f32
}

// str returns a `string` representation of `Vec4`.
pub fn (x Vec4) str() string {
	return '|${x.e[0]:-6.3},${x.e[1]:-6.3},${x.e[2]:-6.3},${x.e[3]:-6.3}|'
}

// vec3 creates a Vec4 value, passing x,y,z as parameters. The w element is set to 1
@[inline]
pub fn vec3(x f32, y f32, z f32) Vec4 {
	return Vec4{
		e: [x, y, z, 1]!
	}
}

// vec4 creates a Vec4 value, based on the x,y,z,w parameters
@[inline]
pub fn vec4(x f32, y f32, z f32, w f32) Vec4 {
	return Vec4{
		e: [x, y, z, w]!
	}
}

// is_equal checks if two vector are equal using the module precision (10e-7)
@[direct_array_access]
pub fn (x Vec4) is_equal(y Vec4) bool {
	unsafe {
		for c, value in x.e {
			if f32_abs(value - y.e[c]) > precision {
				return false
			}
		}
		return true
	}
}

// clean returns a new vector, based on `x`, but with all the values < precision, set to 0
@[direct_array_access]
pub fn (x Vec4) clean() Vec4 {
	mut n := x
	for c, value in x.e {
		if f32_abs(value) < precision {
			n.e[c] = 0
		}
	}
	return n
}

// copy sets all elements of `x` to `value`
pub fn (mut x Vec4) copy(value f32) {
	x.e = [value, value, value, value]!
}

// mul_scalar returns the result of multiplying the vector `x`, by the scalar `value`
@[inline]
pub fn (x Vec4) mul_scalar(value f32) Vec4 {
	return Vec4{
		e: [x.e[0] * value, x.e[1] * value, x.e[2] * value, x.e[3] * value]!
	}
}

// inv returns the reciprocal of the vector `x`
pub fn (x Vec4) inv() Vec4 {
	return Vec4{
		e: [
			if x.e[0] != 0 { 1.0 / x.e[0] } else { 0 },
			if x.e[1] != 0 { 1.0 / x.e[1] } else { 0 },
			if x.e[2] != 0 { 1.0 / x.e[2] } else { 0 },
			if x.e[3] != 0 { 1.0 / x.e[3] } else { 0 },
		]!
	}
}

// normalize returns a normalized form of the vector `x`
pub fn (x Vec4) normalize() Vec4 {
	m := x.mod()
	if m == 0 {
		return zero_v4()
	}
	return Vec4{
		e: [
			x.e[0] * (1 / m),
			x.e[1] * (1 / m),
			x.e[2] * (1 / m),
			x.e[3] * (1 / m),
		]!
	}
}

// normalize3 returns a normalized form of the vector `x`, where the w element is set to 0
pub fn (x Vec4) normalize3() Vec4 {
	m := x.mod3()
	if m == 0 {
		return zero_v4()
	}
	return Vec4{
		e: [
			x.e[0] * (1 / m),
			x.e[1] * (1 / m),
			x.e[2] * (1 / m),
			0,
		]!
	}
}

// mod returns a module of the vector `x`
@[inline]
pub fn (x Vec4) mod() f32 {
	return math.sqrtf(x.e[0] * x.e[0] + x.e[1] * x.e[1] + x.e[2] * x.e[2] + x.e[3] * x.e[3])
}

// mod3 returns a module of the 3d vector `x`, ignoring the value of its w element
@[inline]
pub fn (x Vec4) mod3() f32 {
	return math.sqrtf(x.e[0] * x.e[0] + x.e[1] * x.e[1] + x.e[2] * x.e[2])
}

/*********************************************************************
* Math
*********************************************************************/
// zero_v4 returns a zero vector (all elements set to 0)
@[inline]
pub fn zero_v4() Vec4 {
	return Vec4{
		e: [
			f32(0),
			0,
			0,
			0,
		]!
	}
}

// one_v4 returns a vector, where all elements are set to 1
@[inline]
pub fn one_v4() Vec4 {
	return Vec4{
		e: [
			f32(1),
			1,
			1,
			1,
		]!
	}
}

// blank_v4 returns a vector, where all elements are set to 0, except `w`, which is set to 1
pub fn blank_v4() Vec4 {
	return Vec4{
		e: [
			f32(0),
			0,
			0,
			1,
		]!
	}
}

// set_v4 returns a vector, where all elements are set to `value`
@[inline]
pub fn set_v4(value f32) Vec4 {
	return Vec4{
		e: [
			value,
			value,
			value,
			value,
		]!
	}
}

// sum returns a sum of all the elements
@[inline]
pub fn (x Vec4) sum() f32 {
	return x.e[0] + x.e[1] + x.e[2] + x.e[3]
}

/*********************************************************************
* Operators
*********************************************************************/
// + returns `a` + `b` (corresponding elements are added)
@[inline]
pub fn (a Vec4) + (b Vec4) Vec4 {
	return Vec4{
		e: [
			a.e[0] + b.e[0],
			a.e[1] + b.e[1],
			a.e[2] + b.e[2],
			a.e[3] + b.e[3],
		]!
	}
}

// - returns `a` + `b` (corresponding elements are subtracted)
@[inline]
pub fn (a Vec4) - (b Vec4) Vec4 {
	return Vec4{
		e: [
			a.e[0] - b.e[0],
			a.e[1] - b.e[1],
			a.e[2] - b.e[2],
			a.e[3] - b.e[3],
		]!
	}
}

// * returns `a` * `b` (corresponding elements are multiplied, then summed), i.e. a dot product
@[inline]
pub fn (a Vec4) * (b Vec4) f32 {
	return a.e[0] * b.e[0] + a.e[1] * b.e[1] + a.e[2] * b.e[2] + a.e[3] * b.e[3]
}

// % returns a cross product of the vectors `a` and `b`
@[inline]
pub fn (a Vec4) % (b Vec4) Vec4 {
	return Vec4{
		e: [
			(a.e[1] * b.e[2]) - (a.e[2] * b.e[1]),
			(a.e[2] * b.e[0]) - (a.e[0] * b.e[2]),
			(a.e[0] * b.e[1]) - (a.e[1] * b.e[0]),
			0,
		]!
	}
}

// mul_vec4 returns a vector, where the corresponding `x` and `y` elements are multiplied
@[inline]
pub fn (x Vec4) mul_vec4(y Vec4) Vec4 {
	return Vec4{
		e: [
			x.e[0] * y.e[0],
			x.e[1] * y.e[1],
			x.e[2] * y.e[2],
			x.e[3] * y.e[3],
		]!
	}
}
