// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// catmull_rom returns the point on a Catmull-Rom spline segment between p1 and
// p2, using p0 and p3 to shape the tangents. t is in [0, 1].
pub fn catmull_rom(p0 Vec2, p1 Vec2, p2 Vec2, p3 Vec2, t f32) Vec2 {
	t2 := t * t
	t3 := t2 * t
	return Vec2{
		x: 0.5 * (2 * p1.x + (-p0.x + p2.x) * t + (2 * p0.x - 5 * p1.x + 4 * p2.x - p3.x) * t2 +
			(-p0.x + 3 * p1.x - 3 * p2.x + p3.x) * t3)
		y: 0.5 * (2 * p1.y + (-p0.y + p2.y) * t + (2 * p0.y - 5 * p1.y + 4 * p2.y - p3.y) * t2 +
			(-p0.y + 3 * p1.y - 3 * p2.y + p3.y) * t3)
	}
}

// sample_catmull samples a Catmull-Rom curve through points at normalized
// parameter t. Endpoints clamp and neighboring points are duplicated so the
// curve reaches the first and last control point.
pub fn sample_catmull(points []Vec2, t f32) Vec2 {
	if points.len == 0 {
		return Vec2{}
	}
	if points.len == 1 || t <= 0 {
		return points[0]
	}
	if t >= 1 {
		return points[points.len - 1]
	}
	scaled := t * f32(points.len - 1)
	i := int(scaled)
	local := scaled - f32(i)
	before := if i > 0 { i - 1 } else { 0 }
	after := if i + 1 < points.len { i + 1 } else { points.len - 1 }
	last := if i + 2 < points.len { i + 2 } else { points.len - 1 }
	return catmull_rom(points[before], points[i], points[after], points[last], local)
}

// bezier2 returns the point on a cubic Bezier curve at t in [0, 1].
pub fn bezier2(p0 Vec2, p1 Vec2, p2 Vec2, p3 Vec2, t f32) Vec2 {
	u := 1 - t
	a := u * u * u
	b := 3 * u * u * t
	c := 3 * u * t * t
	d := t * t * t
	return Vec2{
		x: p0.x * a + p1.x * b + p2.x * c + p3.x * d
		y: p0.y * a + p1.y * b + p2.y * c + p3.y * d
	}
}

// catmull_tangent returns a unit tangent of the Catmull-Rom curve at t.
// Zero-length tangents fall back to the positive x axis.
pub fn catmull_tangent(points []Vec2, t f32) Vec2 {
	eps := f32(0.001)
	a := sample_catmull(points, math.clip(t - eps, 0.0, 1.0))
	b := sample_catmull(points, math.clip(t + eps, 0.0, 1.0))
	d := b - a
	len := f32(math.sqrt(f64(d.x * d.x + d.y * d.y)))
	if len == 0 {
		return Vec2{1, 0}
	}
	return Vec2{d.x / len, d.y / len}
}

// resample_polyline returns count points spaced evenly by arc length along
// the given polyline. Degenerate input returns an empty slice.
pub fn resample_polyline(points []Vec2, count int) []Vec2 {
	if points.len < 2 || count < 2 {
		return []Vec2{}
	}
	mut seg := []f32{len: points.len - 1}
	mut total := f32(0)
	for i in 0 .. points.len - 1 {
		d := points[i + 1] - points[i]
		seg[i] = f32(math.sqrt(f64(d.x * d.x + d.y * d.y)))
		total += seg[i]
	}
	if total <= 0 {
		return []Vec2{}
	}
	mut out := []Vec2{len: count}
	step := total / f32(count - 1)
	mut seg_i := 0
	mut acc := f32(0)
	for k in 0 .. count {
		target := step * f32(k)
		for seg_i < seg.len - 1 && acc + seg[seg_i] < target {
			acc += seg[seg_i]
			seg_i++
		}
		seg_len := seg[seg_i]
		if seg_len <= 0 {
			out[k] = points[seg_i]
			continue
		}
		t := math.clip((target - acc) / seg_len, 0.0, 1.0)
		out[k] = points[seg_i] + (points[seg_i + 1] - points[seg_i]).scale(t)
	}
	return out
}
