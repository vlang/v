// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// Ray is a half-line in world space used for picking.
pub struct Ray {
pub mut:
	origin    Vec3
	direction Vec3 // always normalized
}

// RayHit describes the nearest intersection of a ray with a shape.
pub struct RayHit {
pub mut:
	hit      bool
	distance f32
	point    Vec3
}

// screen_to_world_ray maps a screen pixel to a world-space ray for cam.
// The coordinates are in logical pixels with the origin at the top left.
pub fn screen_to_world_ray(x f32, y f32, cam Camera3D, width f32, height f32) Ray {
	if width <= 0 || height <= 0 {
		return Ray{ origin: cam.position, direction: Vec3{0, 0, -1} }
	}
	forward := (cam.target - cam.position).normalized()
	right := forward.cross(cam.up).normalized()
	cam_up := right.cross(forward)
	ndc_x := 2.0 * x / width - 1.0
	ndc_y := 1.0 - 2.0 * y / height
	aspect := width / height
	if cam.projection == .orthographic {
		origin := cam.position + right.scale(ndc_x * cam.ortho_size * aspect) +
			cam_up.scale(ndc_y * cam.ortho_size)
		return Ray{ origin: origin, direction: forward }
	}
	tan_half := f32(math.tan(f64(cam.fovy * math.pi / 360.0)))
	dir := forward + right.scale(ndc_x * aspect * tan_half) + cam_up.scale(ndc_y * tan_half)
	return Ray{ origin: cam.position, direction: dir.normalized() }
}

// ray_sphere returns the nearest intersection of ray with a sphere.
pub fn ray_sphere(ray Ray, center Vec3, radius f32) RayHit {
	oc := ray.origin - center
	b := oc.dot(ray.direction)
	c := oc.dot(oc) - radius * radius
	disc := b * b - c
	if disc < 0 {
		return RayHit{}
	}
	root := f32(math.sqrt(f64(disc)))
	mut t := -b - root
	if t < 0 {
		t = -b + root
	}
	if t < 0 {
		return RayHit{}
	}
	return RayHit{
		hit:      true
		distance: t
		point:    ray.origin + ray.direction.scale(t)
	}
}

// ray_box returns the nearest intersection of ray with an axis-aligned box.
pub fn ray_box(ray Ray, center Vec3, size Vec3) RayHit {
	min := Vec3{center.x - size.x / 2, center.y - size.y / 2, center.z - size.z / 2}
	max := Vec3{center.x + size.x / 2, center.y + size.y / 2, center.z + size.z / 2}
	mut tmin := f32(-math.inf(1))
	mut tmax := f32(math.inf(1))
	origins := [ray.origin.x, ray.origin.y, ray.origin.z]
	dirs := [ray.direction.x, ray.direction.y, ray.direction.z]
	mins := [min.x, min.y, min.z]
	maxs := [max.x, max.y, max.z]
	for i in 0 .. 3 {
		d := dirs[i]
		if math.abs(d) < 1e-6 {
			if origins[i] < mins[i] || origins[i] > maxs[i] {
				return RayHit{}
			}
			continue
		}
		mut t1 := (mins[i] - origins[i]) / d
		mut t2 := (maxs[i] - origins[i]) / d
		if t1 > t2 {
			tmp := t1
			t1 = t2
			t2 = tmp
		}
		if t1 > tmin {
			tmin = t1
		}
		if t2 < tmax {
			tmax = t2
		}
		if tmin > tmax {
			return RayHit{}
		}
	}
	mut t := tmin
	if t < 0 {
		t = tmax
	}
	if t < 0 {
		return RayHit{}
	}
	return RayHit{
		hit:      true
		distance: t
		point:    ray.origin + ray.direction.scale(t)
	}
}

// ray_triangle returns the nearest intersection of ray with a triangle.
// Both sides of the triangle are hit; use your own normal test for culling.
pub fn ray_triangle(ray Ray, a Vec3, b Vec3, c Vec3) RayHit {
	e1 := b - a
	e2 := c - a
	p := ray.direction.cross(e2)
	det := e1.dot(p)
	if math.abs(det) < 1e-6 {
		return RayHit{}
	}
	inv_det := 1.0 / det
	t := ray.origin - a
	u := t.dot(p) * inv_det
	if u < 0 || u > 1 {
		return RayHit{}
	}
	q := t.cross(e1)
	v := ray.direction.dot(q) * inv_det
	if v < 0 || u + v > 1 {
		return RayHit{}
	}
	dist := e2.dot(q) * inv_det
	if dist < 0 {
		return RayHit{}
	}
	return RayHit{
		hit:      true
		distance: dist
		point:    ray.origin + ray.direction.scale(dist)
	}
}

// ColliderKind selects the shape used by a Collider in pick_nearest().
pub enum ColliderKind {
	sphere
	box
}

// Collider is a simple pickable volume with a user-defined id.
pub struct Collider {
pub mut:
	id     int
	kind   ColliderKind = .box
	center Vec3
	size   Vec3 = Vec3{1, 1, 1}
	radius f32  = 1
}

// ray_mesh returns the nearest hit against every triangle of a transformed mesh.
pub fn ray_mesh(ray Ray, mesh &Mesh3D, pos Vec3, rotation Vec3, size f32) RayHit {
	if !mesh.valid() {
		return RayHit{}
	}
	basis := rotation_basis(rotation)
	mut best := RayHit{}
	for t in 0 .. mesh.indices.len / 3 {
		mut p := [Vec3{}, Vec3{}, Vec3{}]!
		for k in 0 .. 3 {
			v := apply_basis(mesh.vertices[mesh.indices[t * 3 + k]], basis).scale(size)
			p[k] = Vec3{pos.x + v.x, pos.y + v.y, pos.z + v.z}
		}
		hit := ray_triangle(ray, p[0], p[1], p[2])
		if hit.hit && (!best.hit || hit.distance < best.distance) {
			best = hit
		}
	}
	return best
}

// pick_nearest returns the id of the closest collider hit by ray.
pub fn pick_nearest(ray Ray, colliders []Collider) ?int {
	mut best_dist := f32(0)
	mut best_id := -1
	for c in colliders {
		hit := if c.kind == .sphere {
			ray_sphere(ray, c.center, c.radius)
		} else {
			ray_box(ray, c.center, c.size)
		}
		if hit.hit && (best_id < 0 || hit.distance < best_dist) {
			best_dist = hit.distance
			best_id = c.id
		}
	}
	if best_id < 0 {
		return none
	}
	return best_id
}
