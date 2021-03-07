/**********************************************************************
*
* Simply vector/matrix utility
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
module m4

import math

pub struct Vec4 {
pub mut:
	e [4]f32
}

/*********************************************************************
*
* Utility
*
*********************************************************************/
pub fn (x Vec4) str() string {
	return '|${x.e[0]:-6.3},${x.e[1]:-6.3},${x.e[2]:-6.3},${x.e[3]:-6.3}|'
}

// create a Vec4 function passing x,y,z as parameteres. w is set to 1
pub fn vec3(x f32, y f32, z f32) Vec4 {
	return m4.Vec4{e:[x, y, z, 1]!}
}

// Remove all the raw zeros
[direct_array_access]
pub fn (a Vec4) clean() Vec4 {
	mut x := Vec4{}
	for c, value in a.e {
		if abs(value) < precision {
			x.e[c] = 0
		} else {
			x.e[c] = value
		}
	}
	return x
}

// Set all elements to value
pub fn (mut x Vec4) copy(value f32) {
	x.e = [ value, value, value, value,	]!
}

// Scale the vector using a scalar
pub fn (x Vec4) mul_scalar(value f32) Vec4 {
	return Vec4{ e: [ x.e[0] * value, x.e[1] * value,  x.e[2] * value, x.e[3] * value, ]! }
}

// Reciprocal of the vector
pub fn (x Vec4) inv() Vec4 {
	return Vec4{
		e: [
			if x.e[0] != 0 { 1.0 / x.e[0] } else { f32(0) },
			if x.e[1] != 0 { 1.0 / x.e[1] } else { f32(0) },
			if x.e[2] != 0 { 1.0 / x.e[2] } else { f32(0) },
			if x.e[3] != 0 { 1.0 / x.e[3] } else { f32(0) },
		]!
	}
}

// Normalize the vector
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

// Normalize only xyz, w set to 0
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

// Module of the vector xyzw
pub fn (x Vec4) mod() f32 {
	return f32(math.sqrt(x.e[0] * x.e[0] + x.e[1] * x.e[1] + x.e[2] * x.e[2] + x.e[3] * x.e[3]))
}

// Module for 3d vector xyz, w ignored
pub fn (x Vec4) mod3() f32 {
	return f32(math.sqrt(x.e[0] * x.e[0] + x.e[1] * x.e[1] + x.e[2] * x.e[2]))
}

/*********************************************************************
*
* Math
*
*********************************************************************/
// Return a zero vector
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

// Return all one vector
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

// Return a blank vector
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

// Set all elements to value
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

// Sum of all the elements
pub fn (x Vec4) sum() f32 {
	return x.e[0] + x.e[1] + x.e[2] + x.e[3]
}

/*********************************************************************
*
* Operators
*
*********************************************************************/
// Addition
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

// Subtraction
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

// Dot product
pub fn (a Vec4) * (b Vec4) f32 {
	return a.e[0] * b.e[0] + a.e[1] * b.e[1] + a.e[2] * b.e[2] + a.e[3] * b.e[3]
}

// Cross product
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

// Components multiplication
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
