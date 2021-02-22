// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module glm

import math

/*
#flag -lmyglm
# f32* myglm_ortho(f32, f32, f32, f32);
# f32* myglm_translate(f32, f32, f32);
*/
// # f32* myglm_rotate(f32 *m, f32 angle, f32, f32, f32);
// # f32* myglm_perspective(f32, f32, f32, f32);
// # f32* myglm_look_at(glm__Vec3, glm__Vec3, glm__Vec3);
// # glm__Vec3 myglm_mult(glm__Vec3, glm__Vec3);
// # glm__Vec3 myglm_cross(glm__Vec3, glm__Vec3);
// # glm__Vec3 myglm_normalize(glm__Vec3);
pub struct Mat4 {
pub:
	data &f32
}

struct Vec2 {
	x f32
	y f32
}

struct Vec3 {
	x f32
	y f32
	z f32
}

pub fn vec3(x f32, y f32, z f32) Vec3 {
	res := Vec3{
		x: x
		y: y
		z: z
	}
	return res
}

fn mat4(f &f32) Mat4 {
	res := Mat4{
		data: f
	}
	return res
}

pub fn (v Vec3) str() string {
	return 'Vec3{ $v.x, $v.y, $v.z }'
}

pub fn (v Vec2) str() string {
	return 'Vec3{ $v.x, $v.y }'
}

pub fn (m Mat4) str() string {
	mut s := '[ '
	for i in 0 .. 4 {
		if i != 0 {
			s += '  '
		}
		for j in 0 .. 4 {
			val := unsafe {m.data[i * 4 + j]}
			s += '${val:5.2f} '
		}
		if i != 3 {
			s += '\n'
		}
	}
	s += ']'
	return s
}

fn vec2(x int, y int) Vec2 {
	res := Vec2{
		x: f32(x)
		y: f32(y)
	}
	return res
}

fn (a Vec3) add(b Vec3) Vec3 {
	res := Vec3{
		x: a.x + b.x
		y: a.y + b.y
		z: a.z + b.z
	}
	return res
}

fn (a Vec3) sub(b Vec3) Vec3 {
	res := Vec3{
		x: a.x - b.x
		y: a.y - b.y
		z: a.z - b.z
	}
	return res
}

// fn (a Vec3) mult(b Vec3) Vec3 {
// # return myglm_mult(a,b);
// }
fn (a Vec3) mult_scalar(b f32) Vec3 {
	res := Vec3{
		x: a.x * b
		y: a.y * b
		z: a.z * b
	}
	return res
}

fn (a Vec3) print() {
	x := a.x
	y := a.y
	z := a.z
	C.printf('vec3{%f,%f,%f}\n', x, y, z)
	// println('vec3{$x,$y,$z}')
}

/*
fn rotate(m Mat4, angle f32, vec Vec3) Mat4 {
	// # t_mat4 m;
	// println('rotate done')
	# return glm__mat4( myglm_rotate(m.data, angle, vec.x,vec.y,vec.z) );
	return Mat4{}
}
*/
fn f32_calloc(n int) &f32 {
	return voidptr(vcalloc(n * int(sizeof(f32))))
}

// fn translate(vec Vec3) *f32 {
pub fn translate(m Mat4, v Vec3) Mat4 {
	// # return glm__mat4(myglm_translate(vec.x,vec.y,vec.z)  );
	a := m.data
	mut out := f32_calloc(16)
	x := v.x
	y := v.y
	z := v.z
	unsafe {
		a00 := a[0]
		a01 := a[1]
		a02 := a[2]
		a03 := a[3]
		a10 := a[4]
		a11 := a[5]
		a12 := a[6]
		a13 := a[7]
		a20 := a[8]
		a21 := a[9]
		a22 := a[10]
		a23 := a[11]
		out[0] = a00
		out[1] = a01
		out[2] = a02
		out[3] = a03
		out[4] = a10
		out[5] = a11
		out[6] = a12
		out[7] = a13
		out[8] = a20
		out[9] = a21
		out[10] = a22
		out[11] = a23
		out[12] = a00 * x + a10 * y + a20 * z + a[12]
		out[13] = a01 * x + a11 * y + a21 * z + a[13]
		out[14] = a02 * x + a12 * y + a22 * z + a[14]
		out[15] = a03 * x + a13 * y + a23 * z + a[15]
	}
	return mat4(out)
}

/*
fn normalize(vec Vec3) Vec3 {
	# return myglm_normalize(vec);
	return Vec3{}
}
*/
// https://github.com/g-truc/glm/blob/0ceb2b755fb155d593854aefe3e45d416ce153a4/glm/ext/matrix_clip_space.inl
pub fn ortho(left f32, right f32, bottom f32, top f32) Mat4 {
	// println('glm ortho($left, $right, $bottom, $top)')
	// mat<4, 4, T, defaultp> Result(static_cast<T>(1));
	n := 16
	mut res := f32_calloc(n)
	unsafe {
		res[0] = 2.0 / (right - left)
		res[5] = 2.0 / (top - bottom)
		res[10] = 1.0
		res[12] = -(right + left) / (right - left)
		res[13] = -(top + bottom) / (top - bottom)
		res[15] = 1.0
	}
	return mat4(res)
}

// https://github.com/g-truc/glm/blob/0ceb2b755fb155d593854aefe3e45d416ce153a4/glm/ext/matrix_clip_space.inl
pub fn ortho_zo(left f32, right f32, bottom f32, top f32, zNear f32, zFar f32) Mat4 {
	// println('glm ortho($left, $right, $bottom, $top)')
	// mat<4, 4, T, defaultp> Result(static_cast<T>(1));
	n := 16
	mut res := f32_calloc(n)
	unsafe {
		res[0] = 2.0 / (right - left)
		res[5] = 2.0 / (top - bottom)
		res[10] = 1.0
		res[12] = -(right + left) / (right - left)
		res[13] = -(top + bottom) / (top - bottom)
		res[14] = -zNear / (zFar - zNear)
		res[15] = 1.0
	}
	return mat4(res)
}

// fn scale(a *f32, v Vec3) *f32 {
pub fn scale(m Mat4, v Vec3) Mat4 {
	a := m.data
	mut out := f32_calloc(16)
	x := v.x
	y := v.y
	z := v.z
	unsafe {
		out[0] = a[0] * v.x
		out[1] = a[1] * x
		out[2] = a[2] * x
		out[3] = a[3] * x
		out[4] = a[4] * y
		out[5] = a[5] * y
		out[6] = a[6] * y
		out[7] = a[7] * y
		out[8] = a[8] * z
		out[9] = a[9] * z
		out[10] = a[10] * z
		out[11] = a[11] * z
		out[12] = a[12]
		out[13] = a[13]
		out[14] = a[14]
		out[15] = a[15]
	}
	return mat4(out)
}

// multiplies two matrices
pub fn mult(a Mat4, b Mat4) Mat4 {
	mut out := f32_calloc(16)
	for i in 0 .. 4 {
		for r in 0 .. 4 {
			mut prod := f32(0)
			for c in 0 .. 4 {
				prod += unsafe {a.data[c * 4 + r] * b.data[i * 4 + c]}
			}
			unsafe {
				out[i * 4 + r] = prod
			}
		}
	}
	return mat4(out)
}

pub fn rotate(angle f32, axis Vec3, src Mat4) Mat4 {
	c := f32(math.cos(angle))
	s := f32(math.sin(angle))
	oneminusc := f32(1.0) - c
	xy := axis.x * axis.y
	yz := axis.y * axis.z
	xz := axis.x * axis.z
	xs := axis.x * s
	ys := axis.y * s
	zs := axis.z * s
	f00 := axis.x * axis.x * oneminusc + c
	f01 := xy * oneminusc + zs
	f02 := xz * oneminusc - ys
	f10 := xy * oneminusc - zs
	f11 := axis.y * axis.y * oneminusc + c
	f12 := yz * oneminusc + xs
	f20 := xz * oneminusc + ys
	f21 := yz * oneminusc - xs
	f22 := axis.z * axis.z * oneminusc + c
	data := src.data
	unsafe {
		t00 := data[0] * f00 + data[4] * f01 + data[8] * f02
		t01 := data[1] * f00 + data[5] * f01 + data[9] * f02
		t02 := data[2] * f00 + data[6] * f01 + data[10] * f02
		t03 := data[3] * f00 + data[7] * f01 + data[11] * f02
		t10 := data[0] * f10 + data[4] * f11 + data[8] * f12
		t11 := data[1] * f10 + data[5] * f11 + data[9] * f12
		t12 := data[2] * f10 + data[6] * f11 + data[10] * f12
		t13 := data[3] * f10 + data[7] * f11 + data[11] * f12
		mut dest := src.data
		dest[8] = data[0] * f20 + data[4] * f21 + data[8] * f22
		dest[9] = data[1] * f20 + data[5] * f21 + data[9] * f22
		dest[10] = data[2] * f20 + data[6] * f21 + data[10] * f22
		dest[11] = data[3] * f20 + data[7] * f21 + data[11] * f22
		dest[0] = t00
		dest[1] = t01
		dest[2] = t02
		dest[3] = t03
		dest[4] = t10
		dest[5] = t11
		dest[6] = t12
		dest[7] = t13
		return mat4(dest)
	}
}

// fn rotate_z(a *f32, rad f32) *f32 {
pub fn rotate_z(m Mat4, rad f32) Mat4 {
	a := m.data
	mut out := f32_calloc(16)
	s := f32(math.sin(rad))
	c := f32(math.cos(rad))
	unsafe {
		a00 := a[0]
		a01 := a[1]
		a02 := a[2]
		a03 := a[3]
		a10 := a[4]
		a11 := a[5]
		a12 := a[6]
		a13 := a[7]
		out[8] = a[8]
		out[9] = a[9]
		out[10] = a[10]
		out[11] = a[11]
		out[12] = a[12]
		out[13] = a[13]
		out[14] = a[14]
		out[15] = a[15]
		// Perform axis-specific matrix multiplication
		out[0] = a00 * c + a10 * s
		out[1] = a01 * c + a11 * s
		out[2] = a02 * c + a12 * s
		out[3] = a03 * c + a13 * s
		out[4] = a10 * c - a00 * s
		out[5] = a11 * c - a01 * s
		out[6] = a12 * c - a02 * s
		out[7] = a13 * c - a03 * s
	}
	return mat4(out)
}

pub fn identity() Mat4 {
	// 1 0 0 0
	// 0 1 0 0
	// 0 0 1 0
	// 0 0 0 1
	n := 16
	mut res := f32_calloc(int(sizeof(f32)) * n)
	unsafe {
		res[0] = 1
		res[5] = 1
		res[10] = 1
		res[15] = 1
	}
	return mat4(res)
}

// returns *f32 without allocation
pub fn identity2(mut res &f32) {
	res[0] = 1
	res[5] = 1
	res[10] = 1
	res[15] = 1
	// # f32 f[16]={0};// for (int i =0;i<16;i++)
	// # printf("!!%d\n", f[0]);
	// # glm__identity2(&f);
	// # gl__Shader_set_mat4(shader, tos2("projection"), f) ;
}

pub fn identity3() []f32 {
	res := [f32(1.0), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]
	return res
}

// https://github.com/toji/gl-matrix/blob/1549cf21dfa14a2bc845993485343d519cf064fe/src/gl-matrix/mat4.js
fn ortho_js(left f32, right f32, bottom f32, top f32) &f32 {
	// mynear := 1
	// myfar := 1
	lr := 1.0 / (left - right)
	bt := 1.0 / (bottom - top)
	nf := f32(1.0) / 1.0 // (mynear -myfar)
	unsafe {
		mut out := &f32(malloc(int(sizeof(f32) * 16)))
		out[0] = -2.0 * lr
		out[1] = 0
		out[2] = 0
		out[3] = 0
		out[4] = 0
		out[5] = -2.0 * bt
		out[6] = 0
		out[7] = 0
		out[8] = 0
		out[9] = 0
		out[10] = 2.0 * nf
		out[11] = 0
		out[12] = (left + right) * lr
		out[13] = (top + bottom) * bt
		out[14] = 1.0 * nf // (far + near) * nf;
		out[15] = 1
		return out
	}
	// f := 0.0
	// return &f
}

// fn ortho_old(a, b, c, d f32) *f32 {
// # return myglm_ortho(a,b,c,d);
// }
fn cross(a Vec3, b Vec3) Vec3 {
	// # return myglm_cross(a,b);
	return Vec3{}
}

/*
fn perspective(degrees f32, ratio f32, a, b f32) Mat4 {
	// println('lang per degrees=$degrees ratio=$ratio a=$a b=$b')
	// # printf("lang pers degrees=%f ratio=%f a=%f b=%f\n", degrees, ratio, a,b);
	# return glm__mat4( myglm_perspective(degrees, ratio, a,b)  ) ;
	return Mat4{}
}

fn look_at(eye, center, up Vec3) Mat4 {
	# return glm__mat4(  myglm_look_at(eye, center, up)  ) ;
	return Mat4{}
}
*/
