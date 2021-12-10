// Copyright(C) 2019 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license file distributed with this software package
module vec2

pub struct Vec2 {
pub mut:
	x f64
	y f64
}

pub fn (mut v Vec2) zero() {
	v.x = 0.0
	v.y = 0.0
}

pub fn (mut v Vec2) from(src Vec2) {
	v.x = src.x
	v.y = src.y
}

// * Addition
// + operator overload. Adds two vectors
pub fn (v1 Vec2) + (v2 Vec2) Vec2 {
	return Vec2{v1.x + v2.x, v1.y + v2.y}
}

pub fn (v Vec2) add(vector Vec2) Vec2 {
	return Vec2{v.x + vector.x, v.y + vector.y}
}

pub fn (v Vec2) add_f64(scalar f64) Vec2 {
	return Vec2{v.x + scalar, v.y + scalar}
}

pub fn (mut v Vec2) plus(vector Vec2) {
	v.x += vector.x
	v.y += vector.y
}

pub fn (mut v Vec2) plus_f64(scalar f64) {
	v.x += scalar
	v.y += scalar
}

// * Subtraction
pub fn (v1 Vec2) - (v2 Vec2) Vec2 {
	return Vec2{v1.x - v2.x, v1.y - v2.y}
}

pub fn (v Vec2) sub(vector Vec2) Vec2 {
	return Vec2{v.x - vector.x, v.y - vector.y}
}

pub fn (v Vec2) sub_f64(scalar f64) Vec2 {
	return Vec2{v.x - scalar, v.y - scalar}
}

pub fn (mut v Vec2) subtract(vector Vec2) {
	v.x -= vector.x
	v.y -= vector.y
}

pub fn (mut v Vec2) subtract_f64(scalar f64) {
	v.x -= scalar
	v.y -= scalar
}

// * Multiplication
pub fn (v1 Vec2) * (v2 Vec2) Vec2 {
	return Vec2{v1.x * v2.x, v1.y * v2.y}
}

pub fn (v Vec2) mul(vector Vec2) Vec2 {
	return Vec2{v.x * vector.x, v.y * vector.y}
}

pub fn (v Vec2) mul_f64(scalar f64) Vec2 {
	return Vec2{v.x * scalar, v.y * scalar}
}

pub fn (mut v Vec2) multiply(vector Vec2) {
	v.x *= vector.x
	v.y *= vector.y
}

pub fn (mut v Vec2) multiply_f64(scalar f64) {
	v.x *= scalar
	v.y *= scalar
}
