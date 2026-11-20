module gg

import math

fn test_pick_nearest_returns_closest_id() {
	ray := gg.Ray{ origin: gg.Vec3{0, 0, 10}, direction: gg.Vec3{0, 0, -1} }
	colliders := [
		gg.Collider{ id: 7, kind: .box, center: gg.Vec3{0, 0, 2}, size: gg.Vec3{1, 1, 1} }
		gg.Collider{ id: 3, kind: .sphere, center: gg.Vec3{0, 0, 5}, radius: 1 },
	]
	id := gg.pick_nearest(ray, colliders) or {
		assert false
		return
	}
	assert id == 3
}

fn test_pick_nearest_miss() {
	ray := gg.Ray{ origin: gg.Vec3{20, 20, 10}, direction: gg.Vec3{0, 0, -1} }
	colliders := [gg.Collider{ id: 1, center: gg.Vec3{}, size: gg.Vec3{1, 1, 1} }]
	assert gg.pick_nearest(ray, colliders) == none
}

fn test_ray_mesh_hits_cube() {
	ray := gg.Ray{ origin: gg.Vec3{0, 0, 5}, direction: gg.Vec3{0, 0, -1} }
	mesh := gg.gen_mesh_cube(gg.Vec3{2, 2, 2})
	hit := gg.ray_mesh(ray, &mesh, gg.Vec3{}, gg.Vec3{}, 1.0)
	assert hit.hit
	assert math.abs(hit.point.z - 1.0) < 0.01
}

fn test_world_to_screen_centre() {
	cam := gg.Camera3D{ position: gg.Vec3{0, 0, 5}, target: gg.Vec3{0, 0, 0} }
	sp := gg.world_to_screen(gg.Vec3{}, cam, 800, 600)
	assert math.abs(sp.x - 400) < 1
	assert math.abs(sp.y - 300) < 1
}
