module gg

import gg.m4
import gg.render
import math

fn close(a f64, b f64) bool {
	return math.abs(a - b) < 1e-3
}

fn apply_render(m &render.Mat4, v m4.Vec4) [4]f32 {
	a := m.to_array()
	mut out := [4]f32{}
	for row in 0 .. 4 {
		out[row] = a[row] * v.e[0] + a[4 + row] * v.e[1] + a[8 + row] * v.e[2] +
			a[12 + row] * v.e[3]
	}
	return out
}

fn test_bridge_preserves_transformation() {
	matrices := [m4.perspective(60, 1.5, 0.1, 100), m4.unit_m4(), m4.scale(m4.vec4(2, 3, 4, 1))]
	point := m4.vec3(1, 2, -5)
	for src in matrices {
		want := m4.mul_vec(src, point)
		converted := gg.mat4_from_m4(src)
		got := apply_render(&converted, point)
		for i in 0 .. 4 {
			assert close(got[i], want.e[i])
		}
	}
}

fn test_bridge_round_trip() {
	src := m4.perspective(60, 1.5, 0.1, 100)
	converted := gg.mat4_from_m4(src)
	back := gg.mat4_to_m4(converted)
	point := m4.vec3(1, 2, -5)
	want := m4.mul_vec(src, point)
	got := m4.mul_vec(back, point)
	for i in 0 .. 4 {
		assert close(got.e[i], want.e[i])
	}
}
