module game

import gg

fn test_lathe() {
	profile := [gg.Vec2{1, 0}, gg.Vec2{1, 2}]
	m := gen_mesh_lathe(profile, 8)
	assert m.valid()
	assert m.vertices.len == 16
	assert m.indices.len == 48
	assert gen_mesh_lathe(profile, 2).vertices.len == 0
}

fn test_extrude() {
	shape := [gg.Vec2{0, 0}, gg.Vec2{2, 0}, gg.Vec2{0, 2}]
	m := gen_mesh_extrude(shape, 1)
	assert m.valid()
	assert m.vertices.len == 6
	assert m.indices.len > 0
	assert gen_mesh_extrude([gg.Vec2{0, 0}], 1).vertices.len == 0
}

fn test_tube() {
	pts := [gg.Vec3{0, 0, 0}, gg.Vec3{0, 0, 4}]
	m := gen_mesh_tube(pts, 0.5, 6)
	assert m.valid()
	assert m.vertices.len == 12
	assert gen_mesh_tube(pts, 0, 6).vertices.len == 0
}
