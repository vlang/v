// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gg.m4
import gg.render

// mat4_from_m4 converts m4.Mat4 into the matrix type used by gg/render.
// m4 applies matrices to row vectors with mul_vec(), while gg/render applies
// them to column vectors; the columns of the render matrix are therefore the
// images of the m4 basis vectors.
pub fn mat4_from_m4(src m4.Mat4) render.Mat4 {
	mut m := render.mat4_identity()
	for col in 0 .. 4 {
		mut basis := m4.Vec4{}
		basis.e[col] = 1
		out := m4.mul_vec(src, basis)
		m.v[col][0] = out.e[0]
		m.v[col][1] = out.e[1]
		m.v[col][2] = out.e[2]
		m.v[col][3] = out.e[3]
	}
	return m
}

// mat4_to_m4 converts a gg/render matrix back into m4.Mat4.
pub fn mat4_to_m4(src render.Mat4) m4.Mat4 {
	mut dst := m4.Mat4{}
	values := src.to_array()
	unsafe {
		for row in 0 .. 4 {
			for col in 0 .. 4 {
				dst.e[row * 4 + col] = values[col * 4 + row]
			}
		}
	}
	return dst
}
