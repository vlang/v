module game

import gg
import math

// Sphere is a 3D ball used for broad-phase tests and frustum culling.
pub struct Sphere {
pub mut:
	center gg.Vec3
	radius f32
}

// sphere_contains reports whether p is inside or on s.
pub fn sphere_contains(s &Sphere, p gg.Vec3) bool {
	d := p - s.center
	return d.dot(d) <= s.radius * s.radius
}

// sphere_intersects reports whether two spheres overlap or touch.
pub fn sphere_intersects(a &Sphere, b &Sphere) bool {
	return sphere_overlap(a.center, a.radius, b.center, b.radius)
}

// sphere_closest_point returns the point on s nearest to p.
pub fn sphere_closest_point(s &Sphere, p gg.Vec3) gg.Vec3 {
	d := p - s.center
	len := f32(math.sqrt(f64(d.dot(d))))
	if len == 0 {
		return s.center + gg.Vec3{s.radius, 0, 0}
	}
	return s.center + d.scale(s.radius / len)
}

// Plane is a half-space defined by normal.dot(p) + d = 0.
pub struct Plane {
pub mut:
	normal gg.Vec3
	d      f32
}

// plane_from_point_normal builds a plane through point with the given normal.
pub fn plane_from_point_normal(point gg.Vec3, normal gg.Vec3) Plane {
	n := normal.normalized()
	return Plane{ normal: n, d: -n.dot(point) }
}

// plane_from_points builds a plane through three points, or none when they
// are collinear.
pub fn plane_from_points(a gg.Vec3, b gg.Vec3, c gg.Vec3) ?Plane {
	n := (b - a).cross(c - a).normalized()
	if n.x == 0 && n.y == 0 && n.z == 0 {
		return none
	}
	return plane_from_point_normal(a, n)
}

// distance returns the signed distance from p to the plane.
pub fn (p &Plane) distance(point gg.Vec3) f32 {
	return p.normal.dot(point) + p.d
}

// contains reports whether p lies on the positive side of the plane.
pub fn (p &Plane) contains(point gg.Vec3) bool {
	return p.distance(point) >= 0
}

// Triangle is three world-space points.
pub struct Triangle {
pub mut:
	a gg.Vec3
	b gg.Vec3
	c gg.Vec3
}

// normal returns a unit surface normal.
pub fn (t &Triangle) normal() gg.Vec3 {
	return (t.b - t.a).cross(t.c - t.a).normalized()
}

// area returns the triangle's surface area.
pub fn (t &Triangle) area() f32 {
	cross := (t.b - t.a).cross(t.c - t.a)
	len := f32(math.sqrt(f64(cross.dot(cross))))
	return len / 2
}

// centroid returns the average of the three vertices.
pub fn (t &Triangle) centroid() gg.Vec3 {
	return (t.a + t.b + t.c).scale(1.0 / 3)
}

// OBB is an oriented box with three orthonormal half-axes.
pub struct OBB {
pub mut:
	center    gg.Vec3
	half_size gg.Vec3
	axes      [3]gg.Vec3 = [gg.Vec3{1, 0, 0}, gg.Vec3{0, 1, 0}, gg.Vec3{0, 0, 1}]!
}

// obb_from_aabb converts an axis-aligned box to an OBB.
pub fn obb_from_aabb(box AABB) OBB {
	return OBB{
		center:    (box.min + box.max).scale(0.5)
		half_size: (box.max - box.min).scale(0.5)
	}
}

fn obb_project_radius(o &OBB, axis gg.Vec3) f32 {
	return o.half_size.x * f32(math.abs(f64(axis.dot(o.axes[0])))) +
		o.half_size.y * f32(math.abs(f64(axis.dot(o.axes[1])))) +
		o.half_size.z * f32(math.abs(f64(axis.dot(o.axes[2]))))
}

// obb_overlaps tests two oriented boxes with the separating axis theorem.
pub fn obb_overlaps(a &OBB, b &OBB) bool {
	delta := b.center - a.center
	mut axes := [gg.Vec3{1, 0, 0}, gg.Vec3{0, 1, 0}, gg.Vec3{0, 0, 1}, gg.Vec3{}, gg.Vec3{}, gg.Vec3{},
		gg.Vec3{}, gg.Vec3{}, gg.Vec3{}, gg.Vec3{}, gg.Vec3{}, gg.Vec3{}, gg.Vec3{}, gg.Vec3{},
		gg.Vec3{}]!
	mut count := 0
	for i in 0 .. 3 {
		axes[count] = a.axes[i]
		count++
		axes[count] = b.axes[i]
		count++
	}
	for i in 0 .. 3 {
		for j in 0 .. 3 {
			cross := a.axes[i].cross(b.axes[j])
			if cross.dot(cross) > 0.000001 {
				axes[count] = cross.normalized()
				count++
			}
		}
	}
	for i in 0 .. count {
		axis := axes[i]
		dist := f32(math.abs(f64(delta.dot(axis))))
		reach := obb_project_radius(a, axis) + obb_project_radius(b, axis)
		if dist > reach {
			return false
		}
	}
	return true
}

// Frustum holds the six clip planes of a perspective camera.
pub struct Frustum {
pub mut:
	planes [6]Plane
}

// frustum_from_camera builds view frustum planes from a perspective camera
// and a viewport size. Orthographic cameras are not supported.
pub fn frustum_from_camera(cam gg.Camera3D, width f32, height f32) Frustum {
	mut f := Frustum{ planes: [6]Plane{}! }
	if width <= 0 || height <= 0 || cam.projection == .orthographic {
		return f
	}
	mut near_pts := []gg.Vec3{len: 4}
	mut far_pts := []gg.Vec3{len: 4}
	xs := [f32(0), width, 0, width]
	ys := [height, height, f32(0), f32(0)]
	for i in 0 .. 4 {
		ray := gg.screen_to_world_ray(xs[i], ys[i], cam, width, height)
		near_pts[i] = ray.origin + ray.direction.scale(cam.near)
		far_pts[i] = ray.origin + ray.direction.scale(cam.far)
	}
	forward := (cam.target - cam.position).normalized()
	inside := cam.position + forward.scale((cam.near + cam.far) / 2)
	f.planes[0] = oriented_plane(near_pts[0], near_pts[1], near_pts[2], inside)
	f.planes[1] = oriented_plane(far_pts[1], far_pts[0], far_pts[2], inside)
	f.planes[2] = oriented_plane(near_pts[0], near_pts[2], far_pts[0], inside)
	f.planes[3] = oriented_plane(near_pts[1], far_pts[1], near_pts[3], inside)
	f.planes[4] = oriented_plane(near_pts[2], near_pts[3], far_pts[2], inside)
	f.planes[5] = oriented_plane(near_pts[0], far_pts[0], near_pts[1], inside)
	return f
}

fn oriented_plane(a gg.Vec3, b gg.Vec3, c gg.Vec3, inside gg.Vec3) Plane {
	p := plane_from_points(a, b, c) or { return Plane{} }
	if p.distance(inside) < 0 {
		return Plane{ normal: p.normal.scale(-1), d: -p.d }
	}
	return p
}

// contains_point reports whether p is inside all six planes.
pub fn (f &Frustum) contains_point(p gg.Vec3) bool {
	for plane in f.planes {
		if plane.distance(p) < 0 {
			return false
		}
	}
	return true
}

// intersects_sphere reports whether a sphere touches the frustum.
pub fn (f &Frustum) intersects_sphere(s &Sphere) bool {
	for plane in f.planes {
		if plane.distance(s.center) < -s.radius {
			return false
		}
	}
	return true
}

// intersects_aabb reports whether an axis-aligned box touches the frustum.
pub fn (f &Frustum) intersects_aabb(box AABB) bool {
	for plane in f.planes {
		mut positive := gg.Vec3{}
		mut negative := gg.Vec3{}
		if plane.normal.x >= 0 {
			positive.x = box.max.x
			negative.x = box.min.x
		} else {
			positive.x = box.min.x
			negative.x = box.max.x
		}
		if plane.normal.y >= 0 {
			positive.y = box.max.y
			negative.y = box.min.y
		} else {
			positive.y = box.min.y
			negative.y = box.max.y
		}
		if plane.normal.z >= 0 {
			positive.z = box.max.z
			negative.z = box.min.z
		} else {
			positive.z = box.min.z
			negative.z = box.max.z
		}
		if plane.distance(positive) < 0 {
			return false
		}
		_ = negative
	}
	return true
}
