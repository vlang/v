module render

import math

pub struct Mat4 {
pub mut:
	v [4][4]f32
}

pub fn mat4_identity() Mat4 {
	mut m := Mat4{}
	for i in 0 .. 4 {
		m.v[i][i] = 1.0
	}
	return m
}

@[direct_array_access]
pub fn mat4_mul(a Mat4, b Mat4) Mat4 {
	mut p := Mat4{}
	for r in 0 .. 4 {
		ai0 := a.v[0][r]
		ai1 := a.v[1][r]
		ai2 := a.v[2][r]
		ai3 := a.v[3][r]
		p.v[0][r] = ai0 * b.v[0][0] + ai1 * b.v[0][1] + ai2 * b.v[0][2] + ai3 * b.v[0][3]
		p.v[1][r] = ai0 * b.v[1][0] + ai1 * b.v[1][1] + ai2 * b.v[1][2] + ai3 * b.v[1][3]
		p.v[2][r] = ai0 * b.v[2][0] + ai1 * b.v[2][1] + ai2 * b.v[2][2] + ai3 * b.v[2][3]
		p.v[3][r] = ai0 * b.v[3][0] + ai1 * b.v[3][1] + ai2 * b.v[3][2] + ai3 * b.v[3][3]
	}
	return p
}

@[direct_array_access]
pub fn (mut m Mat4) translate(x f32, y f32, z f32) {
	for r in 0 .. 4 {
		m.v[3][r] = m.v[0][r] * x + m.v[1][r] * y + m.v[2][r] * z + m.v[3][r]
	}
}

@[direct_array_access]
pub fn (mut m Mat4) scale(x f32, y f32, z f32) {
	for r in 0 .. 4 {
		m.v[0][r] *= x
		m.v[1][r] *= y
		m.v[2][r] *= z
	}
}

@[direct_array_access]
pub fn (mut m Mat4) rotate(angle f32, x0 f32, y0 f32, z0 f32) {
	mut x := x0
	mut y := y0
	mut z := z0
	mag := math.sqrt(x * x + y * y + z * z)
	if mag < 1.0e-4 { return }
	x /= mag
	y /= mag
	z /= mag
	s := math.sinf(angle)
	c := math.cosf(angle)
	one_c := 1.0 - c
	xx := x * x
	yy := y * y
	zz := z * z
	xy := x * y
	yz := y * z
	zx := z * x
	xs := x * s
	ys := y * s
	zs := z * s
	mut rr := Mat4{}
	rr.v[0][0] = (one_c * xx) + c
	rr.v[1][0] = (one_c * xy) - zs
	rr.v[2][0] = (one_c * zx) + ys
	rr.v[3][0] = 0.0
	rr.v[0][1] = (one_c * xy) + zs
	rr.v[1][1] = (one_c * yy) + c
	rr.v[2][1] = (one_c * yz) - xs
	rr.v[3][1] = 0.0
	rr.v[0][2] = (one_c * zx) - ys
	rr.v[1][2] = (one_c * yz) + xs
	rr.v[2][2] = (one_c * zz) + c
	rr.v[3][2] = 0.0
	rr.v[0][3] = 0.0
	rr.v[1][3] = 0.0
	rr.v[2][3] = 0.0
	rr.v[3][3] = 1.0
	tmp := mat4_mul(m, rr)
	m.v = tmp.v
}

@[direct_array_access]
pub fn (mut m Mat4) ortho(l f32, r f32, b f32, t f32, n f32, f f32) {
	mut mm := Mat4{}
	mm.v[0][0] = 2.0 / (r - l)
	mm.v[1][0] = 0.0
	mm.v[2][0] = 0.0
	mm.v[3][0] = -(r + l) / (r - l)
	mm.v[0][1] = 0.0
	mm.v[1][1] = 2.0 / (t - b)
	mm.v[2][1] = 0.0
	mm.v[3][1] = -(t + b) / (t - b)
	mm.v[0][2] = 0.0
	mm.v[1][2] = 0.0
	mm.v[2][2] = -2.0 / (f - n)
	mm.v[3][2] = -(f + n) / (f - n)
	mm.v[0][3] = 0.0
	mm.v[1][3] = 0.0
	mm.v[2][3] = 0.0
	mm.v[3][3] = 1.0
	tmp := mat4_mul(m, mm)
	m.v = tmp.v
}

@[direct_array_access]
pub fn (mut m Mat4) frustum(l f32, r f32, b f32, t f32, n f32, f f32) {
	x := (2.0 * n) / (r - l)
	y := (2.0 * n) / (t - b)
	a := (r + l) / (r - l)
	bb := (t + b) / (t - b)
	c := -(f + n) / (f - n)
	d := -(2.0 * f * n) / (f - n)
	mut mm := Mat4{}
	mm.v[0][0] = x
	mm.v[1][0] = 0.0
	mm.v[2][0] = a
	mm.v[3][0] = 0.0
	mm.v[0][1] = 0.0
	mm.v[1][1] = y
	mm.v[2][1] = bb
	mm.v[3][1] = 0.0
	mm.v[0][2] = 0.0
	mm.v[1][2] = 0.0
	mm.v[2][2] = c
	mm.v[3][2] = d
	mm.v[0][3] = 0.0
	mm.v[1][3] = 0.0
	mm.v[2][3] = -1.0
	mm.v[3][3] = 0.0
	tmp := mat4_mul(m, mm)
	m.v = tmp.v
}

@[direct_array_access]
pub fn (mut m Mat4) perspective(fovy f32, aspect f32, znear f32, zfar f32) {
	sine := math.sinf(fovy / 2.0)
	delta_z := zfar - znear
	if delta_z == 0.0 || sine == 0.0 || aspect == 0.0 { return }
	cotan := math.cosf(fovy / 2.0) / sine
	mut mm := Mat4{}
	mm.v[0][0] = cotan / aspect
	mm.v[1][1] = cotan
	mm.v[2][2] = -(zfar + znear) / delta_z
	mm.v[2][3] = -1.0
	mm.v[3][2] = -2.0 * znear * zfar / delta_z
	mm.v[3][3] = 0.0
	tmp := mat4_mul(m, mm)
	m.v = tmp.v
}

@[direct_array_access]
pub fn (mut m Mat4) lookat(eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32, up_z f32) {
	mut fwd := [3]f32{init: 0.0}
	fwd[0] = center_x - eye_x
	fwd[1] = center_y - eye_y
	fwd[2] = center_z - eye_z
	mut up := [3]f32{init: 0.0}
	up[0] = up_x
	up[1] = up_y
	up[2] = up_z
	mag := math.sqrt(fwd[0] * fwd[0] + fwd[1] * fwd[1] + fwd[2] * fwd[2])
	if mag != 0.0 {
		fwd[0] /= mag
		fwd[1] /= mag
		fwd[2] /= mag
	}
	mut side := [3]f32{init: 0.0}
	side[0] = fwd[1] * up[2] - fwd[2] * up[1]
	side[1] = fwd[2] * up[0] - fwd[0] * up[2]
	side[2] = fwd[0] * up[1] - fwd[1] * up[0]
	mag2 := math.sqrt(side[0] * side[0] + side[1] * side[1] + side[2] * side[2])
	if mag2 != 0.0 {
		side[0] /= mag2
		side[1] /= mag2
		side[2] /= mag2
	}
	up[0] = side[1] * fwd[2] - side[2] * fwd[1]
	up[1] = side[2] * fwd[0] - side[0] * fwd[2]
	up[2] = side[0] * fwd[1] - side[1] * fwd[0]
	mut mm := Mat4{}
	mm.v[0][0] = side[0]
	mm.v[1][0] = side[1]
	mm.v[2][0] = side[2]
	mm.v[0][1] = up[0]
	mm.v[1][1] = up[1]
	mm.v[2][1] = up[2]
	mm.v[0][2] = -fwd[0]
	mm.v[1][2] = -fwd[1]
	mm.v[2][2] = -fwd[2]
	mm.v[3][3] = 1.0
	tmp := mat4_mul(m, mm)
	m.v = tmp.v
	m.translate(-eye_x, -eye_y, -eye_z)
}

@[direct_array_access]
pub fn mat4_from_array(arr []f32) Mat4 {
	mut m := Mat4{}
	for i in 0 .. 16 {
		m.v[i / 4][i % 4] = arr[i]
	}
	return m
}

@[direct_array_access]
pub fn (m &Mat4) to_array() [16]f32 {
	mut out := [16]f32{}
	for i in 0 .. 16 {
		out[i] = m.v[i / 4][i % 4]
	}
	return out
}
