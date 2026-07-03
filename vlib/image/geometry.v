// Copyright (c) 2026 The V Authors. All rights reserved.
// Portions derived from Go's image package.
// Copyright 2010 The Go Authors. All rights reserved.
// Use of the original Go source is governed by Go's BSD-style license.
module image

import image.color

// Point is an x, y coordinate pair. Axes increase right and down.
pub struct Point {
pub mut:
	x int
	y int
}

// str returns a string representation like `(3,4)`.
pub fn (p Point) str() string {
	return '(${p.x},${p.y})'
}

// add returns the vector p + q.
pub fn (p Point) add(q Point) Point {
	return Point{
		x: p.x + q.x
		y: p.y + q.y
	}
}

// sub returns the vector p - q.
pub fn (p Point) sub(q Point) Point {
	return Point{
		x: p.x - q.x
		y: p.y - q.y
	}
}

// mul returns the vector p * k.
pub fn (p Point) mul(k int) Point {
	return Point{
		x: p.x * k
		y: p.y * k
	}
}

// div returns the vector p / k.
pub fn (p Point) div(k int) Point {
	return Point{
		x: p.x / k
		y: p.y / k
	}
}

// in_rect reports whether p is inside r.
pub fn (p Point) in_rect(r Rectangle) bool {
	return r.min.x <= p.x && p.x < r.max.x && r.min.y <= p.y && p.y < r.max.y
}

// mod returns the point q in r where p - q is a multiple of r's size.
pub fn (p Point) mod(r Rectangle) Point {
	w := r.dx()
	h := r.dy()
	mut q := p.sub(r.min)
	q.x %= w
	if q.x < 0 {
		q.x += w
	}
	q.y %= h
	if q.y < 0 {
		q.y += h
	}
	return q.add(r.min)
}

// eq reports whether p and q are equal.
pub fn (p Point) eq(q Point) bool {
	return p == q
}

// pt returns a Point from x and y.
pub fn pt(x int, y int) Point {
	return Point{
		x: x
		y: y
	}
}

// Rectangle contains points where min.x <= x < max.x and min.y <= y < max.y.
pub struct Rectangle {
pub mut:
	min Point
	max Point
}

// str returns a string representation like `(3,4)-(6,5)`.
pub fn (r Rectangle) str() string {
	return '${r.min}-${r.max}'
}

// dx returns r's width.
pub fn (r Rectangle) dx() int {
	return r.max.x - r.min.x
}

// dy returns r's height.
pub fn (r Rectangle) dy() int {
	return r.max.y - r.min.y
}

// size returns r's width and height as a Point.
pub fn (r Rectangle) size() Point {
	return Point{
		x: r.dx()
		y: r.dy()
	}
}

// add returns r translated by p.
pub fn (r Rectangle) add(p Point) Rectangle {
	return Rectangle{
		min: r.min.add(p)
		max: r.max.add(p)
	}
}

// sub returns r translated by -p.
pub fn (r Rectangle) sub(p Point) Rectangle {
	return Rectangle{
		min: r.min.sub(p)
		max: r.max.sub(p)
	}
}

// inset returns r inset by n. A negative n expands the rectangle.
pub fn (r Rectangle) inset(n int) Rectangle {
	mut out := r
	if out.dx() < 2 * n {
		out.min.x = (out.min.x + out.max.x) / 2
		out.max.x = out.min.x
	} else {
		out.min.x += n
		out.max.x -= n
	}
	if out.dy() < 2 * n {
		out.min.y = (out.min.y + out.max.y) / 2
		out.max.y = out.min.y
	} else {
		out.min.y += n
		out.max.y -= n
	}
	return out
}

// intersect returns the largest rectangle contained by r and s.
pub fn (r Rectangle) intersect(s Rectangle) Rectangle {
	mut out := r
	if out.min.x < s.min.x {
		out.min.x = s.min.x
	}
	if out.min.y < s.min.y {
		out.min.y = s.min.y
	}
	if out.max.x > s.max.x {
		out.max.x = s.max.x
	}
	if out.max.y > s.max.y {
		out.max.y = s.max.y
	}
	if out.empty() {
		return Rectangle{}
	}
	return out
}

// union returns the smallest rectangle containing r and s.
pub fn (r Rectangle) union(s Rectangle) Rectangle {
	if r.empty() {
		return s
	}
	if s.empty() {
		return r
	}
	mut out := r
	if out.min.x > s.min.x {
		out.min.x = s.min.x
	}
	if out.min.y > s.min.y {
		out.min.y = s.min.y
	}
	if out.max.x < s.max.x {
		out.max.x = s.max.x
	}
	if out.max.y < s.max.y {
		out.max.y = s.max.y
	}
	return out
}

// empty reports whether r contains no points.
pub fn (r Rectangle) empty() bool {
	return r.min.x >= r.max.x || r.min.y >= r.max.y
}

// eq reports whether r and s contain the same set of points.
pub fn (r Rectangle) eq(s Rectangle) bool {
	return r == s || (r.empty() && s.empty())
}

// overlaps reports whether r and s have a non-empty intersection.
pub fn (r Rectangle) overlaps(s Rectangle) bool {
	return !r.empty() && !s.empty() && r.min.x < s.max.x && s.min.x < r.max.x && r.min.y < s.max.y
		&& s.min.y < r.max.y
}

// inside reports whether every point in r is inside s.
pub fn (r Rectangle) inside(s Rectangle) bool {
	if r.empty() {
		return true
	}
	return s.min.x <= r.min.x && r.max.x <= s.max.x && s.min.y <= r.min.y && r.max.y <= s.max.y
}

// canon returns r with min and max swapped if needed.
pub fn (r Rectangle) canon() Rectangle {
	mut out := r
	if out.max.x < out.min.x {
		out.min.x, out.max.x = out.max.x, out.min.x
	}
	if out.max.y < out.min.y {
		out.min.y, out.max.y = out.max.y, out.min.y
	}
	return out
}

// at returns opaque white for points inside r and transparent otherwise.
pub fn (r Rectangle) at(x int, y int) color.Color {
	if pt(x, y).in_rect(r) {
		return color.opaque
	}
	return color.transparent
}

// rgba64_at returns opaque white for points inside r and transparent otherwise.
pub fn (r Rectangle) rgba64_at(x int, y int) color.RGBA64 {
	if pt(x, y).in_rect(r) {
		return color.RGBA64{0xffff, 0xffff, 0xffff, 0xffff}
	}
	return color.RGBA64{}
}

// bounds returns r.
pub fn (r Rectangle) bounds() Rectangle {
	return r
}

// color_model returns the color model for rectangles.
pub fn (r Rectangle) color_model() color.Model {
	return color.alpha16_model
}

// rect returns a well-formed rectangle with the given coordinates.
pub fn rect(x0 int, y0 int, x1 int, y1 int) Rectangle {
	mut min_x := x0
	mut max_x := x1
	if min_x > max_x {
		min_x, max_x = max_x, min_x
	}
	mut min_y := y0
	mut max_y := y1
	if min_y > max_y {
		min_y, max_y = max_y, min_y
	}
	return Rectangle{
		min: pt(min_x, min_y)
		max: pt(max_x, max_y)
	}
}

fn pixel_buffer_length(bytes_per_pixel int, r Rectangle, image_type_name string) int {
	total_length := mul3_non_neg(bytes_per_pixel, r.dx(), r.dy())
	if total_length < 0 {
		panic('image: new_${image_type_name} rectangle has huge or negative dimensions')
	}
	return total_length
}

fn mul3_non_neg(x int, y int, z int) int {
	if x < 0 || y < 0 || z < 0 {
		return -1
	}
	if y != 0 && x > max_int / y {
		return -1
	}
	xy := x * y
	if z != 0 && xy > max_int / z {
		return -1
	}
	return xy * z
}

fn add2_non_neg(x int, y int) int {
	if x < 0 || y < 0 {
		return -1
	}
	if x > max_int - y {
		return -1
	}
	return x + y
}
