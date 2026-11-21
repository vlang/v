module xr

import gg

fn test_stereo_offset() {
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 0}
	cam.target = gg.Vec3{0, 0, -1}
	cam.up = gg.Vec3{0, 1, 0}
	rig := stereo_from_camera(cam, 0.06)
	assert rig.ipd == 0.06
	assert rig.left.position.x < 0
	assert rig.right.position.x > 0
	diff := rig.right.position - rig.left.position
	assert (diff.x - 0.06) < 0.001
}

fn test_viewports() {
	left, right := viewports(800, 600)
	assert left.x == 0 && left.width == 400
	assert right.x == 400 && right.width == 400
	assert left.height == 600
}

fn test_convergence() {
	mut cam := gg.Camera3D{}
	cam.position = gg.Vec3{0, 0, 0}
	cam.target = gg.Vec3{0, 0, -1}
	cam.up = gg.Vec3{0, 1, 0}
	rig := stereo_from_camera(cam, 0.06)
	p := convergence_point(rig, 5)
	assert p.z < -4.9
}
