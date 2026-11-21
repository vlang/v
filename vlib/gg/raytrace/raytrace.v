// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module raytrace

import gg
import gg.hdr
import math

// infinity is used as the default maximum ray distance.
pub const infinity = 1e30

// epsilon offsets secondary ray origins off the surface they just hit.
pub const epsilon = 0.001

// Ray is a half-line with a normalized direction.
pub struct Ray {
pub mut:
	origin    gg.Vec3
	direction gg.Vec3 = gg.Vec3{0, 0, 1}
}

// at returns the point t units along the ray.
pub fn (r &Ray) at(t f32) gg.Vec3 {
	return r.origin + r.direction.scale(t)
}

// PrimKind identifies the primitive stored in a Hit.
pub enum PrimKind {
	none
	sphere
	triangle
}

// Hit describes the nearest intersection of a ray with the scene.
pub struct Hit {
pub mut:
	hit    bool
	t      f32
	point  gg.Vec3
	normal gg.Vec3
	uv     gg.Vec2
	front  bool = true
	kind   PrimKind
	id     int = -1
}

// MaterialKind selects the surface response used by the path tracer.
pub enum MaterialKind {
	diffuse
	metal
	dielectric
	emissive
}

// Material holds linear-light surface parameters.
pub struct Material {
pub mut:
	kind      MaterialKind
	albedo    hdr.ColorHDR
	emission  hdr.ColorHDR
	roughness f32
	ior       f32 = 1.5
}

// diffuse_material builds a matte material.
pub fn diffuse_material(color hdr.ColorHDR) Material {
	return Material{ kind: .diffuse, albedo: color }
}

// metal_material builds a reflective material; roughness blurs the reflection.
pub fn metal_material(color hdr.ColorHDR, roughness f32) Material {
	return Material{ kind: .metal, albedo: color, roughness: math.clip(roughness, 0, 1) }
}

// dielectric_material builds a refractive material such as glass or water.
pub fn dielectric_material(ior f32) Material {
	return Material{ kind: .dielectric, ior: if ior > 1 { ior } else { 1.5 } }
}

// emissive_material builds a light source with the given radiance.
pub fn emissive_material(radiance hdr.ColorHDR) Material {
	return Material{ kind: .emissive, emission: radiance }
}

// Sphere is a sphere primitive referencing a material by index.
pub struct Sphere {
pub mut:
	center   gg.Vec3
	radius   f32 = 1
	material int
}

// Triangle is a triangle primitive referencing a material by index.
pub struct Triangle {
pub mut:
	a        gg.Vec3
	b        gg.Vec3
	c        gg.Vec3
	material int
}

// Bounds is an axis-aligned bounding box used by the BVH.
pub struct Bounds {
pub mut:
	min gg.Vec3
	max gg.Vec3
}

fn point_in_bounds(mut b Bounds, p gg.Vec3) {
	if p.x < b.min.x {
		b.min.x = p.x
	}
	if p.y < b.min.y {
		b.min.y = p.y
	}
	if p.z < b.min.z {
		b.min.z = p.z
	}
	if p.x > b.max.x {
		b.max.x = p.x
	}
	if p.y > b.max.y {
		b.max.y = p.y
	}
	if p.z > b.max.z {
		b.max.z = p.z
	}
}

fn bounds_union(a Bounds, b Bounds) Bounds {
	return Bounds{gg.Vec3{math.min(f64(a.min.x), f64(b.min.x)), math.min(f64(a.min.y), f64(b.min.y)), math.min(f64(a.min.z), f64(b.min.z))}, gg.Vec3{math.max(f64(a.max.x), f64(b.max.x)), math.max(f64(a.max.y), f64(b.max.y)), math.max(f64(a.max.z), f64(b.max.z))}}
}

fn bounds_center(b Bounds) gg.Vec3 {
	return (b.min + b.max).scale(0.5)
}

// bounds_hit reports whether a ray reaches the box before tmax.
pub fn bounds_hit(b Bounds, ray Ray, tmax f32) bool {
	mut t0 := f32(0)
	mut t1 := tmax
	for axis in 0 .. 3 {
		origin := if axis == 0 {
			ray.origin.x
		} else if axis == 1 {
			ray.origin.y
		} else {
			ray.origin.z
		}
		dir := if axis == 0 {
			ray.direction.x
		} else if axis == 1 {
			ray.direction.y
		} else {
			ray.direction.z
		}
		lo := if axis == 0 {
			b.min.x
		} else if axis == 1 {
			b.min.y
		} else {
			b.min.z
		}
		hi := if axis == 0 {
			b.max.x
		} else if axis == 1 {
			b.max.y
		} else {
			b.max.z
		}
		if dir == 0 {
			if origin < lo || origin > hi {
				return false
			}
			continue
		}
		inv := 1 / dir
		mut near := (lo - origin) * inv
		mut far := (hi - origin) * inv
		if near > far {
			tmp := near
			near = far
			far = tmp
		}
		if near > t0 {
			t0 = near
		}
		if far < t1 {
			t1 = far
		}
		if t0 > t1 {
			return false
		}
	}
	return true
}

struct Item {
	kind   PrimKind
	id     int
	bounds Bounds
}

struct BVHNode {
	bounds Bounds
	left   int = -1
	right  int = -1
	start  int
	count  int
}

// Scene owns primitives, materials and an optional BVH acceleration structure.
@[heap]
pub struct Scene {
pub mut:
	spheres   []Sphere
	triangles []Triangle
	materials []Material
	items     []Item
	nodes     []BVHNode
	built     bool
	// environment is returned when a ray misses everything.
	environment hdr.ColorHDR
}

pub fn new_scene() &Scene {
	return &Scene{}
}

// add_material appends a material and returns its index.
pub fn (mut s Scene) add_material(m Material) int {
	s.materials << m
	return s.materials.len - 1
}

// add_sphere appends a sphere using an existing material index.
pub fn (mut s Scene) add_sphere(center gg.Vec3, radius f32, material int) int {
	s.spheres << Sphere{center, radius, material}
	s.built = false
	return s.spheres.len - 1
}

// add_triangle appends a triangle using an existing material index.
pub fn (mut s Scene) add_triangle(a gg.Vec3, b gg.Vec3, c gg.Vec3, material int) int {
	s.triangles << Triangle{a, b, c, material}
	s.built = false
	return s.triangles.len - 1
}

// add_mesh appends every triangle of a gg.Mesh3D to the scene.
pub fn (mut s Scene) add_mesh(mesh &gg.Mesh3D, material int) {
	if mesh.indices.len % 3 != 0 {
		return
	}
	for i in 0 .. mesh.indices.len / 3 {
		a := mesh.vertices[mesh.indices[i * 3]]
		b := mesh.vertices[mesh.indices[i * 3 + 1]]
		c := mesh.vertices[mesh.indices[i * 3 + 2]]
		s.add_triangle(a, b, c, material)
	}
}

// build constructs the BVH. Call it after adding all primitives and before
// rendering; it is called automatically by the renderer when needed.
pub fn (mut s Scene) build() {
	s.items = []
	for i, sphere in s.spheres {
		lo := sphere.center - gg.Vec3{sphere.radius, sphere.radius, sphere.radius}
		hi := sphere.center + gg.Vec3{sphere.radius, sphere.radius, sphere.radius}
		s.items << Item{.sphere, i, Bounds{lo, hi}}
	}
	for i, tri in s.triangles {
		mut b := Bounds{tri.a, tri.a}
		point_in_bounds(mut b, tri.b)
		point_in_bounds(mut b, tri.c)
		s.items << Item{.triangle, i, b}
	}
	s.nodes = []
	if s.items.len == 0 {
		s.built = true
		return
	}
	mut all := Bounds{s.items[0].bounds.min, s.items[0].bounds.max}
	for item in s.items[1..] {
		all = bounds_union(all, item.bounds)
	}
	s.nodes << BVHNode{all, -1, -1, 0, s.items.len}
	mut stack := [0]
	for stack.len > 0 {
		idx := stack[stack.len - 1]
		stack = stack[..stack.len - 1]
		node := s.nodes[idx]
		if node.count <= 4 {
			continue
		}
		mut centroid_bounds := Bounds{bounds_center(s.items[node.start].bounds), bounds_center(s.items[node.start].bounds)}
		for i in node.start + 1 .. node.start + node.count {
			point_in_bounds(mut centroid_bounds, bounds_center(s.items[i].bounds))
		}
		extent := centroid_bounds.max - centroid_bounds.min
		axis := if extent.x >= extent.y && extent.x >= extent.z {
			0
		} else if extent.y >= extent.z {
			1
		} else {
			2
		}
		mut ids := []int{len: node.count}
		for i in 0 .. node.count {
			ids[i] = i
		}
		// Insertion sort by centroid on the chosen axis.
		for i := 1; i < ids.len; i++ {
			key := ids[i]
			key_axis := s.centroid_axis(s.items[node.start + key].bounds, axis)
			mut j := i - 1
			for j >= 0 && s.centroid_axis(s.items[node.start + ids[j]].bounds, axis) > key_axis {
				ids[j + 1] = ids[j]
				j--
			}
			ids[j + 1] = key
		}
		old := s.items[node.start..node.start + node.count].clone()
		for i, source in ids {
			s.items[node.start + i] = old[source]
		}
		mid := node.count / 2
		left_bounds := s.range_bounds(node.start, mid)
		right_bounds := s.range_bounds(node.start + mid, node.count - mid)
		left_idx := s.nodes.len
		right_idx := left_idx + 1
		s.nodes << BVHNode{left_bounds, -1, -1, node.start, mid}
		s.nodes << BVHNode{right_bounds, -1, -1, node.start + mid, node.count - mid}
		s.nodes[idx] = BVHNode{node.bounds, left_idx, right_idx, node.start, node.count}
		stack << left_idx
		stack << right_idx
	}
	s.built = true
}

fn (s &Scene) centroid_axis(b Bounds, axis int) f32 {
	c := bounds_center(b)
	return if axis == 0 {
		c.x
	} else if axis == 1 {
		c.y
	} else {
		c.z
	}
}

fn (s &Scene) range_bounds(start int, count int) Bounds {
	mut b := Bounds{s.items[start].bounds.min, s.items[start].bounds.max}
	for i in start + 1 .. start + count {
		b = bounds_union(b, s.items[i].bounds)
	}
	return b
}

fn sphere_hit(ray Ray, sphere Sphere, tmax f32) ?Hit {
	oc := ray.origin - sphere.center
	a := ray.direction.dot(ray.direction)
	b := 2 * oc.dot(ray.direction)
	c := oc.dot(oc) - sphere.radius * sphere.radius
	disc := b * b - 4 * a * c
	if disc < 0 {
		return none
	}
	root := f32(math.sqrt(f64(disc)))
	mut t := (-b - root) / (2 * a)
	if t <= epsilon {
		t = (-b + root) / (2 * a)
	}
	if t <= epsilon || t >= tmax {
		return none
	}
	point := ray.at(t)
	normal := (point - sphere.center).normalized()
	u := 0.5 + f32(math.atan2(f64(normal.z), f64(normal.x)) / (2 * math.pi))
	v := 0.5 - f32(math.asin(f64(math.clip(normal.y, -1, 1))) / math.pi)
	return Hit{
		hit:    true
		t:      t
		point:  point
		normal: normal
		uv:     gg.Vec2{u, v}
		front:  ray.direction.dot(normal) < 0
		kind:   .sphere
	}
}

fn triangle_hit(ray Ray, tri Triangle, tmax f32) ?Hit {
	edge1 := tri.b - tri.a
	edge2 := tri.c - tri.a
	pvec := ray.direction.cross(edge2)
	det := edge1.dot(pvec)
	if det > -1e-8 && det < 1e-8 {
		return none
	}
	inv_det := 1 / det
	tvec := ray.origin - tri.a
	u := inv_det * tvec.dot(pvec)
	if u < 0 || u > 1 {
		return none
	}
	qvec := tvec.cross(edge1)
	v := inv_det * ray.direction.dot(qvec)
	if v < 0 || u + v > 1 {
		return none
	}
	t := inv_det * edge2.dot(qvec)
	if t <= epsilon || t >= tmax {
		return none
	}
	n := edge1.cross(edge2).normalized()
	return Hit{
		hit:    true
		t:      t
		point:  ray.at(t)
		normal: n
		uv:     gg.Vec2{u, v}
		front:  ray.direction.dot(n) < 0
		kind:   .triangle
	}
}

fn hit_item(s &Scene, item Item, ray Ray, best Hit, tmax f32) Hit {
	match item.kind {
		.sphere {
			if item.id >= s.spheres.len {
				return best
			}
			mut h := sphere_hit(ray, s.spheres[item.id], tmax) or { return best }
			if !best.hit || h.t < best.t {
				h.id = item.id
				return h
			}
		}
		.triangle {
			if item.id >= s.triangles.len {
				return best
			}
			mut h := triangle_hit(ray, s.triangles[item.id], tmax) or { return best }
			if !best.hit || h.t < best.t {
				h.id = item.id
				return h
			}
		}
		.none {}
	}
	return best
}

fn hit_node(s &Scene, index int, ray Ray, best Hit, tmax f32) Hit {
	if index < 0 || index >= s.nodes.len {
		return best
	}
	node := s.nodes[index]
	limit := if best.hit { best.t } else { tmax }
	if !bounds_hit(node.bounds, ray, limit) {
		return best
	}
	mut result := best
	if node.left < 0 {
		for i in node.start .. node.start + node.count {
			result = hit_item(s, s.items[i], ray, result, limit)
		}
		return result
	}
	result = hit_node(s, node.left, ray, result, limit)
	result = hit_node(s, node.right, ray, result, limit)
	return result
}

// intersect returns the nearest hit, or none when the ray misses the scene.
pub fn (s &Scene) intersect(ray Ray) ?Hit {
	if !s.built {
		return none
	}
	if s.nodes.len == 0 {
		return none
	}
	h := hit_node(s, 0, ray, Hit{}, infinity)
	if h.hit {
		return h
	}
	return none
}

// material_for returns the material of a hit, or a safe default.
pub fn (s &Scene) material_for(h Hit) Material {
	if h.id < 0 || s.materials.len == 0 {
		return diffuse_material(hdr.white())
	}
	mut mat_idx := -1
	if h.kind == .sphere && h.id < s.spheres.len {
		mat_idx = s.spheres[h.id].material
	} else if h.kind == .triangle && h.id < s.triangles.len {
		mat_idx = s.triangles[h.id].material
	}
	if mat_idx < 0 || mat_idx >= s.materials.len {
		return diffuse_material(hdr.white())
	}
	return s.materials[mat_idx]
}
