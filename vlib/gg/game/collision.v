// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module game

import math
import gg

// Circle is a 2D circle described by its center and radius.
pub struct Circle {
pub mut:
	x      f32
	y      f32
	radius f32
}

// AABB is an axis-aligned 3D box described by its minimum and maximum corners.
pub struct AABB {
pub mut:
	min gg.Vec3
	max gg.Vec3
}

// point_in_rect reports whether p is inside r.
pub fn point_in_rect(p gg.Vec2, r gg.Rect) bool {
	return p.x >= r.x && p.x <= r.x + r.width && p.y >= r.y && p.y <= r.y + r.height
}

// closest_point_on_rect returns the point of r closest to p.
pub fn closest_point_on_rect(p gg.Vec2, r gg.Rect) gg.Vec2 {
	return gg.Vec2{
		x: math.clip(p.x, r.x, r.x + r.width)
		y: math.clip(p.y, r.y, r.y + r.height)
	}
}

// point_in_circle reports whether p is inside c, including its boundary.
pub fn point_in_circle(p gg.Vec2, c Circle) bool {
	dx := p.x - c.x
	dy := p.y - c.y
	return dx * dx + dy * dy <= c.radius * c.radius
}

// rects_overlap reports whether two rectangles share any area.
pub fn rects_overlap(a gg.Rect, b gg.Rect) bool {
	return a.x < b.x + b.width && a.x + a.width > b.x && a.y < b.y + b.height
		&& a.y + a.height > b.y
}

// circles_overlap reports whether two circles overlap or touch.
pub fn circles_overlap(a Circle, b Circle) bool {
	dx := a.x - b.x
	dy := a.y - b.y
	r := a.radius + b.radius
	return dx * dx + dy * dy <= r * r
}

// circle_rect_overlap reports whether a circle and rectangle overlap.
pub fn circle_rect_overlap(c Circle, r gg.Rect) bool {
	q := closest_point_on_rect(gg.Vec2{c.x, c.y}, r)
	dx := q.x - c.x
	dy := q.y - c.y
	return dx * dx + dy * dy <= c.radius * c.radius
}

// point_in_polygon reports whether p is inside the polygon using an even-odd
// ray cast. Points on an edge may be classified either way.
pub fn point_in_polygon(p gg.Vec2, points []gg.Vec2) bool {
	if points.len < 3 {
		return false
	}
	mut inside := false
	mut j := points.len - 1
	for i in 0 .. points.len {
		a := points[i]
		b := points[j]
		if (a.y > p.y) != (b.y > p.y) {
			cross_x := (b.x - a.x) * (p.y - a.y) / (b.y - a.y) + a.x
			if p.x < cross_x {
				inside = !inside
			}
		}
		j = i
	}
	return inside
}

// rect_mtv returns the minimum translation vector that separates rectangle a
// from b. The zero vector is returned when they do not overlap.
pub fn rect_mtv(a gg.Rect, b gg.Rect) gg.Vec2 {
	if !rects_overlap(a, b) {
		return gg.Vec2{}
	}
	overlap_x := f32(math.min(f64(a.x + a.width), f64(b.x + b.width)) -
		f64(math.max(f64(a.x), f64(b.x))))
	overlap_y := f32(math.min(f64(a.y + a.height), f64(b.y + b.height)) -
		f64(math.max(f64(a.y), f64(b.y))))
	acx := a.x + a.width / 2
	acy := a.y + a.height / 2
	bcx := b.x + b.width / 2
	bcy := b.y + b.height / 2
	if overlap_x < overlap_y {
		return gg.Vec2{if acx < bcx { -overlap_x } else { overlap_x }, 0}
	}
	return gg.Vec2{0, if acy < bcy { -overlap_y } else { overlap_y }}
}

// aabb_from_center builds an AABB from a center point and full extents.
pub fn aabb_from_center(center gg.Vec3, size gg.Vec3) AABB {
	half := size.scale(0.5)
	return AABB{ min: center - half, max: center + half }
}

// aabb_overlap reports whether two axis-aligned boxes overlap or touch.
pub fn aabb_overlap(a AABB, b AABB) bool {
	return a.min.x <= b.max.x && a.max.x >= b.min.x && a.min.y <= b.max.y
		&& a.max.y >= b.min.y && a.min.z <= b.max.z && a.max.z >= b.min.z
}

// sphere_overlap reports whether two spheres overlap or touch.
pub fn sphere_overlap(a gg.Vec3, ra f32, b gg.Vec3, rb f32) bool {
	dx := a.x - b.x
	dy := a.y - b.y
	dz := a.z - b.z
	r := ra + rb
	return dx * dx + dy * dy + dz * dz <= r * r
}

// sphere_aabb_overlap reports whether a sphere overlaps an axis-aligned box.
pub fn sphere_aabb_overlap(center gg.Vec3, radius f32, box AABB) bool {
	qx := math.clip(center.x, box.min.x, box.max.x)
	qy := math.clip(center.y, box.min.y, box.max.y)
	qz := math.clip(center.z, box.min.z, box.max.z)
	dx := qx - center.x
	dy := qy - center.y
	dz := qz - center.z
	return dx * dx + dy * dy + dz * dz <= radius * radius
}

fn ray_slab(o f32, d f32, lo f32, hi f32) ?gg.Vec2 {
	if math.abs(d) < 1e-6 {
		if o < lo || o > hi {
			return none
		}
		return gg.Vec2{-f32(math.inf(1)), f32(math.inf(1))}
	}
	t1 := (lo - o) / d
	t2 := (hi - o) / d
	return gg.Vec2{if t1 < t2 { t1 } else { t2 }, if t1 < t2 { t2 } else { t1 }}
}

// ray_aabb returns the nearest intersection of ray with an axis-aligned box.
// This is the AABB form of ray_box and uses the same gg.RayHit result.
pub fn ray_aabb(ray gg.Ray, box AABB) gg.RayHit {
	sx := ray_slab(ray.origin.x, ray.direction.x, box.min.x, box.max.x) or {
		return gg.RayHit{}
	}
	sy := ray_slab(ray.origin.y, ray.direction.y, box.min.y, box.max.y) or {
		return gg.RayHit{}
	}
	sz := ray_slab(ray.origin.z, ray.direction.z, box.min.z, box.max.z) or {
		return gg.RayHit{}
	}
	mut t_min := f32(-math.inf(1))
	mut t_max := f32(math.inf(1))
	for s in [sx, sy, sz] {
		if s.x > t_min {
			t_min = s.x
		}
		if s.y < t_max {
			t_max = s.y
		}
		if t_min > t_max {
			return gg.RayHit{}
		}
	}
	if t_max < 0 {
		return gg.RayHit{}
	}
	t := if t_min >= 0 { t_min } else { t_max }
	return gg.RayHit{
		hit:      true
		distance: t
		point:    ray.origin + ray.direction.scale(t)
	}
}
