module splat

import gg

fn test_project() {
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 5}
	cam.target = gg.Vec3{0, 0, 0}
	cam.fovy = 60
	cam.near = 0.1
	cam.far = 100
	g := Gaussian{gg.Vec3{0, 0, 0}, gg.Vec3{0.2, 0.2, 0.2}, gg.red, 1}
	p := project(g, cam, 100, 100) or { panic('not projected') }
	assert p.x > 45 && p.x < 55
	assert p.y > 45 && p.y < 55
	assert p.depth > 4.9
	behind := Gaussian{gg.Vec3{0, 0, 20}, gg.Vec3{1, 1, 1}, gg.red, 1}
	assert project(behind, cam, 100, 100) == none
}

fn test_rasterize() {
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 5}
	cam.target = gg.Vec3{0, 0, 0}
	cam.fovy = 60
	cam.near = 0.1
	cam.far = 100
	mut cloud := new_cloud()
	cloud.points << Gaussian{gg.Vec3{0, 0, 0}, gg.Vec3{1, 1, 1}, gg.red, 1}
	mut pixels := []u8{len: 32 * 32 * 4}
	rasterize(cloud, cam, 32, 32, mut pixels)
	center := (16 * 32 + 16) * 4
	assert pixels[center] > 0
	assert pixels[center + 3] == 0
}
