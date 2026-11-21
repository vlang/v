// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module splat

import gg
import math

// Gaussian is one anisotropic 3D Gaussian with a color and opacity.
pub struct Gaussian {
pub mut:
	position gg.Vec3
	scale    gg.Vec3 = gg.Vec3{1, 1, 1}
	color    gg.Color
	opacity  f32 = 1
}

// SplatCloud is a collection of Gaussians, typically loaded from a .ply/.splat
// file by your own asset pipeline.
pub struct SplatCloud {
pub mut:
	points []Gaussian
}

// new_cloud returns an empty cloud.
pub fn new_cloud() SplatCloud {
	return SplatCloud{}
}

// ProjectedSplat is a screen-space ellipse ready for blending.
pub struct ProjectedSplat {
pub mut:
	x       f32
	y       f32
	rx      f32
	ry      f32
	color   gg.Color
	opacity f32
	depth   f32
}

// project maps one Gaussian to screen space using a perspective camera.
// Splats behind the camera or outside the depth range are skipped.
pub fn project(g &Gaussian, cam gg.Camera3D, width f32, height f32) ?ProjectedSplat {
	if width <= 0 || height <= 0 || cam.projection == .orthographic {
		return none
	}
	forward := (cam.target - cam.position).normalized()
	mut right := forward.cross(cam.up).normalized()
	if right.x == 0 && right.y == 0 && right.z == 0 {
		right = gg.Vec3{1, 0, 0}
	}
	up := right.cross(forward).normalized()
	rel := g.position - cam.position
	depth := rel.dot(forward)
	if depth <= cam.near || depth >= cam.far {
		return none
	}
	tan_half := f32(math.tan(f64(cam.fovy * math.pi / 360)))
	aspect := width / height
	ndc_x := rel.dot(right) / (depth * tan_half * aspect)
	ndc_y := rel.dot(up) / (depth * tan_half)
	sx := (ndc_x * 0.5 + 0.5) * width
	sy := (0.5 - ndc_y * 0.5) * height
	rx := g.scale.x / (depth * tan_half) * width / 2
	ry := g.scale.y / (depth * tan_half) * height / 2
	if rx <= 0 || ry <= 0 {
		return none
	}
	return ProjectedSplat{
		x:       sx
		y:       sy
		rx:      rx
		ry:      ry
		color:   g.color
		opacity: math.clip(g.opacity, 0, 1)
		depth:   depth
	}
}

fn blend(mut pixels []u8, width int, height int, s ProjectedSplat) {
	min_x := int(s.x - s.rx * 2)
	max_x := int(s.x + s.rx * 2)
	min_y := int(s.y - s.ry * 2)
	max_y := int(s.y + s.ry * 2)
	for y in min_y .. max_y + 1 {
		for x in min_x .. max_x + 1 {
			if x < 0 || y < 0 || x >= width || y >= height {
				continue
			}
			dx := (f32(x) - s.x) / s.rx
			dy := (f32(y) - s.y) / s.ry
			d2 := dx * dx + dy * dy
			if d2 > 4 {
				continue
			}
			alpha := s.opacity * f32(math.exp(f64(-2 * d2)))
			at := (y * width + x) * 4
			if at + 3 >= pixels.len {
				continue
			}
			mut channels := [s.color.r, s.color.g, s.color.b]
			for c in 0 .. 3 {
				pixels[at + c] = u8(f32(pixels[at + c]) * (1 - alpha) + f32(channels[c]) * alpha)
			}
		}
	}
}

// rasterize blends all visible splats into an interleaved RGBA8 buffer in
// back-to-front order. It is pure V but intended for small previews; real-time
// clouds need a GPU renderer.
pub fn rasterize(cloud &SplatCloud, cam gg.Camera3D, width int, height int, mut pixels []u8) {
	if width <= 0 || height <= 0 || pixels.len < width * height * 4 {
		return
	}
	mut projected := []ProjectedSplat{}
	for g in cloud.points {
		if p := project(g, cam, f32(width), f32(height)) {
			projected << p
		}
	}
	for i := 1; i < projected.len; i++ {
		key := projected[i]
		mut j := i - 1
		for j >= 0 && projected[j].depth < key.depth {
			projected[j + 1] = projected[j]
			j--
		}
		projected[j + 1] = key
	}
	for p in projected {
		blend(mut pixels, width, height, p)
	}
}
