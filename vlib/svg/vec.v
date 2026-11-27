module svg

import math

// Vec2 is a 2D point/vector using f32 coordinates (SVG user units).
pub struct Vec2 {
pub mut:
	x f32 = 0
	y f32 = 0
}

// vec2 creates a point.
pub fn vec2(x f32, y f32) Vec2 {
	return Vec2{ x: x, y: y }
}

// add returns the vector sum.
pub fn (p Vec2) add(q Vec2) Vec2 {
	return Vec2{ x: p.x + q.x, y: p.y + q.y }
}

// sub returns the vector difference.
pub fn (p Vec2) sub(q Vec2) Vec2 {
	return Vec2{ x: p.x - q.x, y: p.y - q.y }
}

// scale returns the vector multiplied by k.
pub fn (p Vec2) scale(k f32) Vec2 {
	return Vec2{ x: p.x * k, y: p.y * k }
}

// length returns the Euclidean length.
pub fn (p Vec2) length() f32 {
	return math.sqrtf(p.x * p.x + p.y * p.y)
}

// distance returns the distance to q.
pub fn (p Vec2) distance(q Vec2) f32 {
	return p.sub(q).length()
}

// lerp interpolates between p and q by t (0..1).
pub fn (p Vec2) lerp(q Vec2, t f32) Vec2 {
	return Vec2{
		x: p.x + (q.x - p.x) * t
		y: p.y + (q.y - p.y) * t
	}
}

// str returns a compact string representation.
pub fn (p Vec2) str() string {
	return '${fmt_f32(p.x)},${fmt_f32(p.y)}'
}

// Rect is an axis-aligned rectangle in user units.
pub struct Rect {
pub:
	x f32 = 0
	y f32 = 0
	w f32 = 0
	h f32 = 0
}

// rect creates a rectangle from position and size.
pub fn rect(x f32, y f32, w f32, h f32) Rect {
	return Rect{ x: x, y: y, w: w, h: h }
}

// rect_pts creates a rectangle from two corners.
pub fn rect_pts(x0 f32, y0 f32, x1 f32, y1 f32) Rect {
	return Rect{
		x: math.min(x0, x1)
		y: math.min(y0, y1)
		w: math.abs(x1 - x0)
		h: math.abs(y1 - y0)
	}
}

// min_x returns the left edge.
pub fn (r Rect) min_x() f32 {
	return r.x
}

// min_y returns the top edge.
pub fn (r Rect) min_y() f32 {
	return r.y
}

// max_x returns the right edge.
pub fn (r Rect) max_x() f32 {
	return r.x + r.w
}

// max_y returns the bottom edge.
pub fn (r Rect) max_y() f32 {
	return r.y + r.h
}

// is_empty reports whether the rectangle has no area.
pub fn (r Rect) is_empty() bool {
	return r.w <= 0 || r.h <= 0
}

// contains reports whether p lies inside the rectangle.
pub fn (r Rect) contains(p Vec2) bool {
	return p.x >= r.x && p.x <= r.x + r.w && p.y >= r.y && p.y <= r.y + r.h
}

// union returns the smallest rectangle containing both r and s.
pub fn (r Rect) union(s Rect) Rect {
	if r.is_empty() {
		return s
	}
	if s.is_empty() {
		return r
	}
	x0 := math.min(r.x, s.x)
	y0 := math.min(r.y, s.y)
	x1 := math.max(r.max_x(), s.max_x())
	y1 := math.max(r.max_y(), s.max_y())
	return Rect{ x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
}

// intersect returns the overlap of r and s (possibly empty).
pub fn (r Rect) intersect(s Rect) Rect {
	x0 := math.max(r.x, s.x)
	y0 := math.max(r.y, s.y)
	x1 := math.min(r.max_x(), s.max_x())
	y1 := math.min(r.max_y(), s.max_y())
	if x1 < x0 || y1 < y0 {
		return Rect{}
	}
	return Rect{ x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
}

// inflate grows the rectangle by dx/dy on each side.
pub fn (r Rect) inflate(dx f32, dy f32) Rect {
	return Rect{
		x: r.x - dx
		y: r.y - dy
		w: r.w + 2 * dx
		h: r.h + 2 * dy
	}
}

// center returns the rectangle's center point.
pub fn (r Rect) center() Vec2 {
	return Vec2{ x: r.x + r.w / 2, y: r.y + r.h / 2 }
}
