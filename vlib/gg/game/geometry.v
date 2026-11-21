// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module game

import math
import gg

fn geometry_cross(a gg.Vec2, b gg.Vec2) f32 {
	return a.x * b.y - a.y * b.x
}

// closest_point_on_segment returns the point on segment ab nearest to p.
pub fn closest_point_on_segment(p gg.Vec2, a gg.Vec2, b gg.Vec2) gg.Vec2 {
	ab := b - a
	length2 := ab.x * ab.x + ab.y * ab.y
	if length2 == 0 {
		return a
	}
	ap := p - a
	t := math.clip((ap.x * ab.x + ap.y * ab.y) / length2, 0, 1)
	return a + ab.scale(t)
}

// dist_point_segment returns the shortest distance from p to segment ab.
pub fn dist_point_segment(p gg.Vec2, a gg.Vec2, b gg.Vec2) f32 {
	q := closest_point_on_segment(p, a, b)
	d := p - q
	return f32(math.sqrt(f64(d.x * d.x + d.y * d.y)))
}

// segments_intersect returns the intersection point of two segments, or none
// when they are parallel or do not overlap.
pub fn segments_intersect(a1 gg.Vec2, a2 gg.Vec2, b1 gg.Vec2, b2 gg.Vec2) ?gg.Vec2 {
	d1 := a2 - a1
	d2 := b2 - b1
	den := geometry_cross(d1, d2)
	if den == 0 {
		return none
	}
	b1a1 := b1 - a1
	t := geometry_cross(b1a1, d2) / den
	u := geometry_cross(b1a1, d1) / den
	if t < 0 || t > 1 || u < 0 || u > 1 {
		return none
	}
	return a1 + d1.scale(t)
}

// polygon_area returns the absolute area of a simple polygon.
pub fn polygon_area(points []gg.Vec2) f32 {
	if points.len < 3 {
		return 0
	}
	mut sum := f32(0)
	for i in 0 .. points.len {
		a := points[i]
		b := points[(i + 1) % points.len]
		sum += a.x * b.y - b.x * a.y
	}
	return sum / 2
}

// polygon_centroid returns the area-weighted center of a simple polygon.
// Degenerate polygons return the average of their vertices.
pub fn polygon_centroid(points []gg.Vec2) gg.Vec2 {
	if points.len == 0 {
		return gg.Vec2{}
	}
	mut area2 := f32(0)
	mut cx := f32(0)
	mut cy := f32(0)
	for i in 0 .. points.len {
		a := points[i]
		b := points[(i + 1) % points.len]
		cross := geometry_cross(a, b)
		area2 += cross
		cx += (a.x + b.x) * cross
		cy += (a.y + b.y) * cross
	}
	if area2 == 0 {
		mut sx := f32(0)
		mut sy := f32(0)
		for p in points {
			sx += p.x
			sy += p.y
		}
		return gg.Vec2{sx / f32(points.len), sy / f32(points.len)}
	}
	return gg.Vec2{cx / (3 * area2), cy / (3 * area2)}
}

// convex_hull returns the counter-clockwise convex hull of points using the
// monotone chain algorithm. Fewer than three points are returned unchanged.
pub fn convex_hull(points []gg.Vec2) []gg.Vec2 {
	if points.len < 3 {
		return points.clone()
	}
	mut pts := points.clone()
	for i := 1; i < pts.len; i++ {
		key := pts[i]
		mut j := i - 1
		for j >= 0 && (pts[j].x > key.x || (pts[j].x == key.x && pts[j].y > key.y)) {
			pts[j + 1] = pts[j]
			j--
		}
		pts[j + 1] = key
	}

	mut lower := []gg.Vec2{}
	for p in pts {
		for lower.len >= 2
			&& geometry_cross(lower[lower.len - 1] - lower[lower.len - 2], p - lower[lower.len - 1]) <= 0 {
			lower = lower[..lower.len - 1]
		}
		lower << p
	}
	mut upper := []gg.Vec2{}
	for i := points.len - 1; i >= 0; i-- {
		p := pts[i]
		for upper.len >= 2
			&& geometry_cross(upper[upper.len - 1] - upper[upper.len - 2], p - upper[upper.len - 1]) <= 0 {
			upper = upper[..upper.len - 1]
		}
		upper << p
	}
	if lower.len > 0 {
		lower = lower[..lower.len - 1]
	}
	if upper.len > 0 {
		upper = upper[..upper.len - 1]
	}
	mut out := []gg.Vec2{len: lower.len + upper.len}
	for i, p in lower {
		out[i] = p
	}
	for i, p in upper {
		out[lower.len + i] = p
	}
	return out
}
