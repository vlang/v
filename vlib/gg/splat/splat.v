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

// SplatBasis holds the camera frame and projection constants that are shared
// by every splat in one rasterization pass.
struct SplatBasis {
	cam      gg.Camera3D
	forward  gg.Vec3
	right    gg.Vec3
	up       gg.Vec3
	tan_half f32
	aspect   f32
	width    f32
	height   f32
}

// make_splat_basis computes the per-frame camera frame once. Splitting it out
// of project() avoids three vector normalizations and a tan() per splat.
fn make_splat_basis(cam gg.Camera3D, width f32, height f32) ?SplatBasis {
	if width <= 0 || height <= 0 || cam.projection == .orthographic {
		return none
	}
	forward := (cam.target - cam.position).normalized()
	mut right := forward.cross(cam.up).normalized()
	if right.x == 0 && right.y == 0 && right.z == 0 {
		right = gg.Vec3{1, 0, 0}
	}
	return SplatBasis{
		cam:      cam
		forward:  forward
		right:    right
		up:       right.cross(forward).normalized()
		tan_half: f32(math.tan(f64(cam.fovy * math.pi / 360)))
		aspect:   width / height
		width:    width
		height:   height
	}
}

fn project_with_basis(g &Gaussian, b SplatBasis) ?ProjectedSplat {
	rel := g.position - b.cam.position
	depth := rel.dot(b.forward)
	if depth <= b.cam.near || depth >= b.cam.far {
		return none
	}
	ndc_x := rel.dot(b.right) / (depth * b.tan_half * b.aspect)
	ndc_y := rel.dot(b.up) / (depth * b.tan_half)
	sx := (ndc_x * 0.5 + 0.5) * b.width
	sy := (0.5 - ndc_y * 0.5) * b.height
	rx := g.scale.x / (depth * b.tan_half) * b.width / 2
	ry := g.scale.y / (depth * b.tan_half) * b.height / 2
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

// project maps one Gaussian to screen space using a perspective camera.
// Splats behind the camera or outside the depth range are skipped.
pub fn project(g &Gaussian, cam gg.Camera3D, width f32, height f32) ?ProjectedSplat {
	if basis := make_splat_basis(cam, width, height) {
		return project_with_basis(g, basis)
	}
	return none
}

fn splat_heap_push(mut heap []ProjectedSplat, item ProjectedSplat) {
	heap << item
	mut i := heap.len - 1
	for i > 0 {
		parent := (i - 1) / 2
		if heap[parent].depth >= heap[i].depth {
			break
		}
		swap := heap[parent]
		heap[parent] = heap[i]
		heap[i] = swap
		i = parent
	}
}

fn splat_heap_pop(mut heap []ProjectedSplat) ProjectedSplat {
	top := heap[0]
	last := heap[heap.len - 1]
	heap = heap[..heap.len - 1]
	if heap.len == 0 {
		return top
	}
	heap[0] = last
	mut i := 0
	for {
		left := 2 * i + 1
		right := left + 1
		mut largest := i
		if left < heap.len && heap[left].depth > heap[largest].depth {
			largest = left
		}
		if right < heap.len && heap[right].depth > heap[largest].depth {
			largest = right
		}
		if largest == i {
			break
		}
		swap := heap[i]
		heap[i] = heap[largest]
		heap[largest] = swap
		i = largest
	}
	return top
}

fn blend(mut pixels []u8, width int, height int, s ProjectedSplat) {
	// Clip once instead of bounds-checking every pixel in the ellipse.
	mut min_x := int(s.x - s.rx * 2)
	mut max_x := int(s.x + s.rx * 2)
	mut min_y := int(s.y - s.ry * 2)
	mut max_y := int(s.y + s.ry * 2)
	if min_x < 0 {
		min_x = 0
	}
	if min_y < 0 {
		min_y = 0
	}
	if max_x >= width {
		max_x = width - 1
	}
	if max_y >= height {
		max_y = height - 1
	}
	if min_x > max_x || min_y > max_y {
		return
	}
	// Hoist the two divisions and the color components out of the pixel loop.
	inv_rx := 1 / s.rx
	inv_ry := 1 / s.ry
	cr := f32(s.color.r)
	cg := f32(s.color.g)
	cb := f32(s.color.b)
	for y in min_y .. max_y + 1 {
		dy := (f32(y) - s.y) * inv_ry
		dy2 := dy * dy
		row := y * width
		for x in min_x .. max_x + 1 {
			dx := (f32(x) - s.x) * inv_rx
			d2 := dx * dx + dy2
			if d2 > 4 {
				continue
			}
			alpha := s.opacity * f32(math.exp(f64(-2 * d2)))
			at := (row + x) * 4
			if at + 3 >= pixels.len {
				continue
			}
			pixels[at] = u8(f32(pixels[at]) * (1 - alpha) + cr * alpha)
			pixels[at + 1] = u8(f32(pixels[at + 1]) * (1 - alpha) + cg * alpha)
			pixels[at + 2] = u8(f32(pixels[at + 2]) * (1 - alpha) + cb * alpha)
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
	basis := make_splat_basis(cam, f32(width), f32(height)) or { return }
	mut heap := []ProjectedSplat{}
	for g in cloud.points {
		if p := project_with_basis(g, basis) {
			splat_heap_push(mut heap, p)
		}
	}
	for heap.len > 0 {
		blend(mut pixels, width, height, splat_heap_pop(mut heap))
	}
}
