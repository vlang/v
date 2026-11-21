module game

import gg

fn test_sphere() {
	s := Sphere{gg.Vec3{0, 0, 0}, 2}
	assert sphere_contains(s, gg.Vec3{1, 0, 0})
	assert !sphere_contains(s, gg.Vec3{3, 0, 0})
	assert sphere_intersects(s, Sphere{gg.Vec3{3, 0, 0}, 2})
	p := sphere_closest_point(s, gg.Vec3{5, 0, 0})
	assert p.x == 2
}

fn test_plane() {
	p := plane_from_point_normal(gg.Vec3{0, 0, 0}, gg.Vec3{0, 1, 0})
	assert p.distance(gg.Vec3{3, 5, 0}) == 5
	assert p.contains(gg.Vec3{0, 1, 0})
	q := plane_from_points(gg.Vec3{0, 0, 0}, gg.Vec3{1, 0, 0}, gg.Vec3{0, 1, 0})
	assert q != none
	assert plane_from_points(gg.Vec3{}, gg.Vec3{1, 0, 0}, gg.Vec3{2, 0, 0}) == none
}

fn test_triangle() {
	t := Triangle{gg.Vec3{0, 0, 0}, gg.Vec3{2, 0, 0}, gg.Vec3{0, 2, 0}}
	assert (t.area() - 2) < 0.001
	assert t.normal().z > 0.99
	assert t.centroid().x > 0.6 && t.centroid().x < 0.7
}

fn test_obb() {
	box := AABB{ min: gg.Vec3{-1, -1, -1}, max: gg.Vec3{1, 1, 1} }
	a := obb_from_aabb(box)
	assert obb_overlaps(a, obb_from_aabb(box))
	far := obb_from_aabb(AABB{ min: gg.Vec3{5, 0, 0}, max: gg.Vec3{6, 1, 1} })
	assert !obb_overlaps(a, far)
}

fn test_frustum() {
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 5}
	cam.target = gg.Vec3{0, 0, 0}
	cam.fovy = 60
	cam.near = 0.1
	cam.far = 100
	f := frustum_from_camera(cam, 800, 600)
	assert f.contains_point(gg.Vec3{0, 0, 0})
	assert !f.contains_point(gg.Vec3{100, 0, 0})
	assert f.intersects_sphere(Sphere{gg.Vec3{0, 0, 0}, 1})
	assert !f.intersects_sphere(Sphere{gg.Vec3{100, 0, 0}, 1})
	assert f.intersects_aabb(AABB{ min: gg.Vec3{-1, -1, -1}, max: gg.Vec3{1, 1, 1} })
}

fn test_octree() {
	root := AABB{ min: gg.Vec3{-10, -10, -10}, max: gg.Vec3{10, 10, 10} }
	mut tree := new_octree(root, 2)
	for i in 0 .. 20 {
		x := f32(i % 5) - 2
		z := f32(i / 5) - 2
		tree.insert(i, AABB{ min: gg.Vec3{x, 0, z}, max: gg.Vec3{x + 0.5, 1, z + 0.5} })
	}
	mut hits := []int{}
	tree.query(AABB{ min: gg.Vec3{-0.2, -1, -0.2}, max: gg.Vec3{0.7, 2, 0.7} }, mut hits)
	assert hits.len > 0
	assert !tree.insert(99, AABB{ min: gg.Vec3{50, 0, 0}, max: gg.Vec3{51, 1, 1} })
}
