// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module xr

import gg

// StereoRig holds the left and right eye cameras derived from one head camera.
// It is a rendering-side helper: headset tracking and compositing are left to
// your platform's XR runtime.
pub struct StereoRig {
pub mut:
	ipd   f32 = 0.064
	left  gg.Camera3D
	right gg.Camera3D
}

// stereo_from_camera builds a rig by offsetting the head camera along its
// right vector by half the interpupillary distance.
pub fn stereo_from_camera(cam gg.Camera3D, ipd f32) StereoRig {
	forward := (cam.target - cam.position).normalized()
	mut right := forward.cross(cam.up).normalized()
	if right.x == 0 && right.y == 0 && right.z == 0 {
		right = gg.Vec3{1, 0, 0}
	}
	half := ipd / 2
	mut left_cam := cam
	mut right_cam := cam
	left_cam.position = cam.position - right.scale(half)
	right_cam.position = cam.position + right.scale(half)
	return StereoRig{ ipd: ipd, left: left_cam, right: right_cam }
}

// viewports returns side-by-side rectangles for a stereo framebuffer.
pub fn viewports(width int, height int) (gg.Rect, gg.Rect) {
	half := f32(width) / 2
	return gg.Rect{0, 0, half, f32(height)}, gg.Rect{half, 0, half, f32(height)}
}

// convergence_point returns a target between the eyes at distance, useful for
// setting a fixed vergence on a 3D cursor.
pub fn convergence_point(rig &StereoRig, distance f32) gg.Vec3 {
	center := (rig.left.position + rig.right.position).scale(0.5)
	forward := (rig.left.target - rig.left.position).normalized()
	return center + forward.scale(distance)
}
