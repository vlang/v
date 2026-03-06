module sgl

import math

@[direct_array_access]
fn mat4_identity(mut m Matrix) {
	for c in 0 .. 4 {
		for r in 0 .. 4 {
			m.v[c][r] = if r == c { f32(1.0) } else { f32(0.0) }
		}
	}
}

@[direct_array_access]
fn mat4_transpose(mut dst Matrix, m &Matrix) {
	for c in 0 .. 4 {
		for r in 0 .. 4 {
			dst.v[r][c] = m.v[c][r]
		}
	}
}

// mat4_mul multiplies two 4x4 matrices: p = a * b
@[direct_array_access]
fn mat4_mul(mut p Matrix, a &Matrix, b &Matrix) {
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
}

// mat4_mul_inplace multiplies dst by m in place: dst = dst * m
fn mat4_mul_inplace(mut dst Matrix, m &Matrix) {
	mut tmp := Matrix{}
	mat4_mul(mut tmp, dst, m)
	unsafe {
		*dst = tmp
	}
}

@[direct_array_access]
fn mat4_rotate(mut dst Matrix, a f32, x_ f32, y_ f32, z_ f32) {
	mut x := x_
	mut y := y_
	mut z := z_
	s := math.sinf(a)
	c := math.cosf(a)

	mag := math.sqrtf(x * x + y * y + z * z)
	if mag < 1.0e-4 {
		return
	}
	x /= mag
	y /= mag
	z /= mag
	xx := x * x
	yy := y * y
	zz := z * z
	xy := x * y
	yz := y * z
	zx := z * x
	xs := x * s
	ys := y * s
	zs := z * s
	one_c := f32(1.0) - c

	mut m := Matrix{}
	m.v[0][0] = (one_c * xx) + c
	m.v[1][0] = (one_c * xy) - zs
	m.v[2][0] = (one_c * zx) + ys
	m.v[3][0] = 0.0
	m.v[0][1] = (one_c * xy) + zs
	m.v[1][1] = (one_c * yy) + c
	m.v[2][1] = (one_c * yz) - xs
	m.v[3][1] = 0.0
	m.v[0][2] = (one_c * zx) - ys
	m.v[1][2] = (one_c * yz) + xs
	m.v[2][2] = (one_c * zz) + c
	m.v[3][2] = 0.0
	m.v[0][3] = 0.0
	m.v[1][3] = 0.0
	m.v[2][3] = 0.0
	m.v[3][3] = 1.0
	mat4_mul_inplace(mut dst, &m)
}

@[direct_array_access]
fn mat4_scale(mut dst Matrix, x f32, y f32, z f32) {
	for r in 0 .. 4 {
		dst.v[0][r] *= x
		dst.v[1][r] *= y
		dst.v[2][r] *= z
	}
}

@[direct_array_access]
fn mat4_translate(mut dst Matrix, x f32, y f32, z f32) {
	for r in 0 .. 4 {
		dst.v[3][r] = dst.v[0][r] * x + dst.v[1][r] * y + dst.v[2][r] * z + dst.v[3][r]
	}
}

@[direct_array_access]
fn mat4_frustum(mut dst Matrix, left f32, right f32, bottom f32, top f32, znear f32, zfar f32) {
	x := (2.0 * znear) / (right - left)
	y := (2.0 * znear) / (top - bottom)
	a := (right + left) / (right - left)
	b := (top + bottom) / (top - bottom)
	c := -(zfar + znear) / (zfar - znear)
	d := -(2.0 * zfar * znear) / (zfar - znear)
	mut m := Matrix{}
	m.v[0][0] = x
	m.v[0][1] = 0.0
	m.v[0][2] = 0.0
	m.v[0][3] = 0.0
	m.v[1][0] = 0.0
	m.v[1][1] = y
	m.v[1][2] = 0.0
	m.v[1][3] = 0.0
	m.v[2][0] = a
	m.v[2][1] = b
	m.v[2][2] = c
	m.v[2][3] = -1.0
	m.v[3][0] = 0.0
	m.v[3][1] = 0.0
	m.v[3][2] = d
	m.v[3][3] = 0.0
	mat4_mul_inplace(mut dst, &m)
}

@[direct_array_access]
fn mat4_ortho(mut dst Matrix, left f32, right f32, bottom f32, top f32, znear f32, zfar f32) {
	mut m := Matrix{}
	m.v[0][0] = 2.0 / (right - left)
	m.v[1][0] = 0.0
	m.v[2][0] = 0.0
	m.v[3][0] = -(right + left) / (right - left)
	m.v[0][1] = 0.0
	m.v[1][1] = 2.0 / (top - bottom)
	m.v[2][1] = 0.0
	m.v[3][1] = -(top + bottom) / (top - bottom)
	m.v[0][2] = 0.0
	m.v[1][2] = 0.0
	m.v[2][2] = -2.0 / (zfar - znear)
	m.v[3][2] = -(zfar + znear) / (zfar - znear)
	m.v[0][3] = 0.0
	m.v[1][3] = 0.0
	m.v[2][3] = 0.0
	m.v[3][3] = 1.0
	mat4_mul_inplace(mut dst, &m)
}

@[direct_array_access]
fn mat4_perspective(mut dst Matrix, fovy f32, aspect f32, znear f32, zfar f32) {
	sine := math.sinf(fovy / 2.0)
	delta_z := zfar - znear
	if delta_z == 0.0 || sine == 0.0 || aspect == 0.0 {
		return
	}
	cotan := math.cosf(fovy / 2.0) / sine
	mut m := Matrix{}
	mat4_identity(mut m)
	m.v[0][0] = cotan / aspect
	m.v[1][1] = cotan
	m.v[2][2] = -(zfar + znear) / delta_z
	m.v[2][3] = -1.0
	m.v[3][2] = -2.0 * znear * zfar / delta_z
	m.v[3][3] = 0.0
	mat4_mul_inplace(mut dst, &m)
}

@[direct_array_access]
fn vec3_normalize(mut v [3]f32) {
	r := math.sqrtf(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])
	if r == 0.0 {
		return
	}
	v[0] /= r
	v[1] /= r
	v[2] /= r
}

@[direct_array_access]
fn vec3_cross(v1 [3]f32, v2 [3]f32) [3]f32 {
	return [
		v1[1] * v2[2] - v1[2] * v2[1],
		v1[2] * v2[0] - v1[0] * v2[2],
		v1[0] * v2[1] - v1[1] * v2[0],
	]!
}

@[direct_array_access]
fn mat4_lookat(mut dst Matrix, eye_x f32, eye_y f32, eye_z f32, center_x f32, center_y f32, center_z f32, up_x f32, up_y f32, up_z f32) {
	mut fwd := [center_x - eye_x, center_y - eye_y, center_z - eye_z]!
	mut up := [up_x, up_y, up_z]!
	vec3_normalize(mut fwd)
	mut side := vec3_cross(fwd, up)
	vec3_normalize(mut side)
	up = vec3_cross(side, fwd)

	mut m := Matrix{}
	mat4_identity(mut m)
	m.v[0][0] = side[0]
	m.v[1][0] = side[1]
	m.v[2][0] = side[2]
	m.v[0][1] = up[0]
	m.v[1][1] = up[1]
	m.v[2][1] = up[2]
	m.v[0][2] = -fwd[0]
	m.v[1][2] = -fwd[1]
	m.v[2][2] = -fwd[2]
	mat4_mul_inplace(mut dst, &m)
	mat4_translate(mut dst, -eye_x, -eye_y, -eye_z)
}

// Matrix stack access helpers for ContextInternal

fn (ctx &ContextInternal) matrix_projection() &Matrix {
	return &ctx.matrix_stack[int(MatrixMode.projection)][ctx.matrix_tos[int(MatrixMode.projection)]]
}

fn (ctx &ContextInternal) matrix_modelview() &Matrix {
	return &ctx.matrix_stack[int(MatrixMode.modelview)][ctx.matrix_tos[int(MatrixMode.modelview)]]
}

fn (ctx &ContextInternal) matrix_texture() &Matrix {
	return &ctx.matrix_stack[int(MatrixMode.texture)][ctx.matrix_tos[int(MatrixMode.texture)]]
}

fn (ctx &ContextInternal) current_matrix() &Matrix {
	return &ctx.matrix_stack[int(ctx.cur_matrix_mode)][ctx.matrix_tos[int(ctx.cur_matrix_mode)]]
}

fn (mut ctx ContextInternal) current_matrix_mut() &Matrix {
	return &ctx.matrix_stack[int(ctx.cur_matrix_mode)][ctx.matrix_tos[int(ctx.cur_matrix_mode)]]
}
