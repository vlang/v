module canvas

import math

// Rect is an axis-aligned rectangle. Like Skia Graphite's geometry Rect it
// stores `[left, top, -right, -bottom]`, so that the derived values (width,
// center, intersections, ...) are computed with the same floating-point
// operations and therefore round identically.
pub struct Rect {
pub:
	vals [4]f32
}

// Rect.ltrb creates a rectangle from its edges.
pub fn Rect.ltrb(left f32, top f32, right f32, bottom f32) Rect {
	return Rect{
		vals: [left, top, -right, -bottom]!
	}
}

// Rect.xywh creates a rectangle from its origin and size.
pub fn Rect.xywh(x f32, y f32, width f32, height f32) Rect {
	return Rect.ltrb(x, y, x + width, y + height)
}

// Rect.infinite_inverted returns `[inf, inf, -inf, -inf]`, the identity for
// `join`.
pub fn Rect.infinite_inverted() Rect {
	inf := f32(math.inf(1))
	return Rect{
		vals: [inf, inf, inf, inf]!
	}
}

@[inline]
pub fn (r Rect) left() f32 {
	return r.vals[0]
}

@[inline]
pub fn (r Rect) top() f32 {
	return r.vals[1]
}

@[inline]
pub fn (r Rect) right() f32 {
	return -r.vals[2]
}

@[inline]
pub fn (r Rect) bottom() f32 {
	return -r.vals[3]
}

// width returns `-(left + -right)`, which equals Skia's `right - left`.
@[inline]
pub fn (r Rect) width() f32 {
	return -(r.vals[0] + r.vals[2])
}

@[inline]
pub fn (r Rect) height() f32 {
	return -(r.vals[1] + r.vals[3])
}

// center returns `(left * 0.5 + right * 0.5, top * 0.5 + bottom * 0.5)`.
pub fn (r Rect) center() Point {
	// Vector multiply, then vector add (never fused, like skvx).
	l := r.vals[0] * f32(0.5)
	t := r.vals[1] * f32(0.5)
	rr := r.vals[2] * f32(-0.5)
	b := r.vals[3] * f32(-0.5)
	return Point{l + rr, t + b}
}

pub fn (r Rect) area() f32 {
	w := r.vals[0] + r.vals[2]
	h := r.vals[1] + r.vals[3]
	return w * h
}

// is_empty_negative_or_nan reports a non-positive width or height, or NaN.
pub fn (r Rect) is_empty_negative_or_nan() bool {
	return !(r.vals[0] + r.vals[2] < 0 && r.vals[1] + r.vals[3] < 0)
}

pub fn (r Rect) inset(d f32) Rect {
	return Rect{
		vals: [r.vals[0] + d, r.vals[1] + d, r.vals[2] + d, r.vals[3] + d]!
	}
}

pub fn (r Rect) outset(d f32) Rect {
	return Rect{
		vals: [r.vals[0] - d, r.vals[1] - d, r.vals[2] - d, r.vals[3] - d]!
	}
}

pub fn (r Rect) intersect(o Rect) Rect {
	return Rect{
		vals: [max_f32(r.vals[0], o.vals[0]), max_f32(r.vals[1], o.vals[1]), max_f32(r.vals[2],
			o.vals[2]), max_f32(r.vals[3], o.vals[3])]!
	}
}

pub fn (r Rect) join(o Rect) Rect {
	return Rect{
		vals: [min_f32(r.vals[0], o.vals[0]), min_f32(r.vals[1], o.vals[1]), min_f32(r.vals[2],
			o.vals[2]), min_f32(r.vals[3], o.vals[3])]!
	}
}

// round rounds each edge with `floor(x + 0.5)`, like SkRect::round().
pub fn (r Rect) round() Rect {
	return Rect.ltrb(floor_f32(r.left() + f32(0.5)), floor_f32(r.top() + f32(0.5)), floor_f32(r.right() +
		f32(0.5)), floor_f32(r.bottom() + f32(0.5)))
}

// round_out returns the smallest integer rectangle containing `r`.
pub fn (r Rect) round_out() Rect {
	return Rect{
		vals: [floor_f32(r.vals[0]), floor_f32(r.vals[1]), floor_f32(r.vals[2]), floor_f32(r.vals[3])]!
	}
}

// sorted swaps inverted edges.
pub fn (r Rect) sorted() Rect {
	return Rect{
		vals: [min_f32(r.vals[0], -r.vals[2]), min_f32(r.vals[1], -r.vals[3]), min_f32(r.vals[2],
			-r.vals[0]), min_f32(r.vals[3], -r.vals[1])]!
	}
}

pub fn (r Rect) nearly_equals(o Rect, epsilon f32) bool {
	for i in 0 .. 4 {
		if !(abs_f32(r.vals[i] - o.vals[i]) <= epsilon) {
			return false
		}
	}
	return true
}

pub fn (r Rect) contains(o Rect) bool {
	return r.vals[0] <= o.vals[0] && r.vals[1] <= o.vals[1] && r.vals[2] <= o.vals[2]
		&& r.vals[3] <= o.vals[3]
}

pub fn (r Rect) is_finite() bool {
	for v in r.vals {
		if !is_finite_f32(v) {
			return false
		}
	}
	return true
}

// Point is a 2D point or vector.
pub struct Point {
pub mut:
	x f32
	y f32
}

@[inline]
fn min_f32(a f32, b f32) f32 {
	// Matches skvx::min / std::min: returns `a` unless `b < a`.
	return if b < a { b } else { a }
}

@[inline]
fn max_f32(a f32, b f32) f32 {
	return if a < b { b } else { a }
}

@[inline]
fn abs_f32(a f32) f32 {
	return f32(math.abs(a))
}

@[inline]
fn floor_f32(a f32) f32 {
	return C.floorf(a)
}

@[inline]
fn is_finite_f32(a f32) bool {
	// NaN and infinities fail `a * 0 == 0`.
	return a * f32(0) == f32(0)
}

fn C.floorf(x f32) f32
fn C.roundf(x f32) f32
fn C.sqrtf(x f32) f32
fn C.nextafterf(x f32, y f32) f32
