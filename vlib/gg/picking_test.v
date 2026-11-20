module gg

import math

fn test_ray_hits_centred_sphere() {
	ray := Ray{ origin: gg.Vec3{0, 0, 5}, direction: gg.Vec3{0, 0, -1} }
	hit := gg.ray_sphere(ray, gg.Vec3{0, 0, 0}, 1.0)
	assert hit.hit
	assert math.abs(hit.distance - 4.0) < 1e-4
	assert math.abs(hit.point.z - 1.0) < 1e-4
}

fn test_ray_misses_sphere_outside_cone() {
	ray := Ray{ origin: gg.Vec3{3, 0, 5}, direction: gg.Vec3{0, 0, -1} }
	hit := gg.ray_sphere(ray, gg.Vec3{0, 0, 0}, 1.0)
	assert !hit.hit
}

fn test_ray_hits_box_from_outside() {
	ray := Ray{ origin: gg.Vec3{0, 0, 5}, direction: gg.Vec3{0, 0, -1} }
	hit := gg.ray_box(ray, gg.Vec3{0, 0, 0}, gg.Vec3{2, 2, 2})
	assert hit.hit
	assert math.abs(hit.distance - 4.0) < 1e-4
}

fn test_ray_from_inside_box_exits() {
	ray := Ray{ origin: gg.Vec3{0, 0, 0}, direction: gg.Vec3{1, 0, 0} }
	hit := gg.ray_box(ray, gg.Vec3{0, 0, 0}, gg.Vec3{2, 2, 2})
	assert hit.hit
	assert math.abs(hit.distance - 1.0) < 1e-4
}

fn test_ray_hits_triangle() {
	ray := Ray{ origin: gg.Vec3{0, 0, 5}, direction: gg.Vec3{0, 0, -1} }
	hit := gg.ray_triangle(ray, gg.Vec3{-1, -1, 1}, gg.Vec3{1, -1, 1}, gg.Vec3{0, 1, 1})
	assert hit.hit
	assert math.abs(hit.distance - 4.0) < 1e-4
	assert math.abs(hit.point.z - 1.0) < 1e-4
}

fn test_ray_misses_triangle_outside_bounds() {
	ray := Ray{ origin: gg.Vec3{5, 5, 5}, direction: gg.Vec3{0, 0, -1} }
	hit := gg.ray_triangle(ray, gg.Vec3{-1, -1, 1}, gg.Vec3{1, -1, 1}, gg.Vec3{0, 1, 1})
	assert !hit.hit
}

fn test_screen_ray_points_at_screen_centre() {
	cam := gg.Camera3D{
		position: gg.Vec3{0, 0, 5}
		target:   gg.Vec3{0, 0, 0}
		fovy:     90
	}
	ray := gg.screen_to_world_ray(400, 300, cam, 800, 600)
	assert math.abs(ray.direction.z + 1.0) < 1e-4
	assert math.abs(ray.direction.x) < 1e-4
	hit := gg.ray_sphere(ray, gg.Vec3{0, 0, 0}, 1.0)
	assert hit.hit
}

fn test_screen_to_world_ray_with_vertical_camera() {
	// First fallback branch: forward along -Y, parallel to default up.
	cam := gg.Camera3D{ position: gg.Vec3{0, 5, 0}, target: gg.Vec3{0, 0, 0} }
	left := gg.screen_to_world_ray(10, 50, cam, 100, 100)
	right := gg.screen_to_world_ray(90, 50, cam, 100, 100)
	assert math.abs(left.direction.z - right.direction.z) > 1e-3
	// Second fallback branch: forward parallel to X (abs(fwd.x) > 0.9).
	side_cam := gg.Camera3D{ position: gg.Vec3{5, 0, 0}, target: gg.Vec3{0, 0, 0}, up: gg.Vec3{1, 0, 0} }
	a := gg.screen_to_world_ray(10, 50, side_cam, 100, 100)
	b := gg.screen_to_world_ray(90, 50, side_cam, 100, 100)
	assert math.abs(a.direction.y - b.direction.y) > 1e-3
}
