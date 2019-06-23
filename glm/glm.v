// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module glm

import math

/* 
#flag -lmyglm
# float* myglm_ortho(float, float, float, float);
# float* myglm_translate(float, float, float);
*/
// # float* myglm_rotate(float *m, float angle, float, float, float);
// # float* myglm_perspective(float, float, float, float);
// # float* myglm_look_at(glm__Vec3, glm__Vec3, glm__Vec3);
// # glm__Vec3 myglm_mult(glm__Vec3, glm__Vec3);
// # glm__Vec3 myglm_cross(glm__Vec3, glm__Vec3);
// # glm__Vec3 myglm_normalize(glm__Vec3);
struct Mat4 {
pub:
	data *float
}

struct Vec2 {
	x float
	y float
}

struct Vec3 {
	x float
	y float
	z float
}

fn vec3(x, y, z float) Vec3 {
	res := Vec3 {
		x: x,
		y: y,
		z: z,
	}
	return res
}

fn mat4(f *float) Mat4 {
	res := Mat4 {
		data: f
	}
	return res
}

fn (v Vec3) str() string {
	return 'Vec3{ $v.x, $v.y, $v.z }'
}

fn (v Vec2) str() string {
	return 'Vec3{ $v.x, $v.y }'
}

fn (m Mat4) str() string {
	mut s := '[ '
	for i := 0; i < 4; i++ {
		if i != 0 {
			s += '  '
		}
		for j := 0; j < 4; j++ {
			val := m.data[i * 4 + j]
			s += '${val:.2f} '
		}
		if i != 3 {
			s += '\n'
		}
	}
	s += ']'
	return s
}

fn vec2(x, y int) Vec2 {
	res := Vec2 {
		x: x,
		y: y,
	}
	return res
}

fn (a Vec3) add(b Vec3) Vec3 {
	res := Vec3 {
		x: a.x + b.x,
		y: a.y + b.y,
		z: a.z + b.z,
	}
	return res
}

fn (a Vec3) sub(b Vec3) Vec3 {
	res := Vec3 {
		x: a.x - b.x,
		y: a.y - b.y,
		z: a.z - b.z,
	}
	return res
}

// fn (a Vec3) mult(b Vec3) Vec3 {
// # return myglm_mult(a,b);
// }
fn (a Vec3) mult_scalar(b float) Vec3 {
	res := Vec3 {
		x: a.x * b,
		y: a.y * b,
		z: a.z * b,
	}
	return res
}

fn (a Vec3) print() {
	x := a.x
	y := a.y
	z := a.z
	# printf("vec3{%f,%f,%f}\n",x,y,z);
	// println('vec3{$x,$y,$z}')
}

/* 
fn rotate(m Mat4, angle float, vec Vec3) Mat4 {
	// # t_mat4 m;
	// println('rotate done')
	# return glm__mat4( myglm_rotate(m.data, angle, vec.x,vec.y,vec.z) );
	return Mat4{}
}
*/

fn float_calloc(n int) *float {
	return *float(calloc(n * sizeof(float)))
}
// fn translate(vec Vec3) *float {
fn translate(m Mat4, v Vec3) Mat4 {
	// # return glm__mat4(myglm_translate(vec.x,vec.y,vec.z)  );
	a := m.data
	mut out := float_calloc(16)
	x := v.x
	y := v.y
	z := v.z
	a00 := a[0]a01 := a[1]a02 := a[2]a03 := a[3]
	a10 := a[4]a11 := a[5]a12 := a[6]a13 := a[7]
	a20 := a[8]a21 := a[9]a22 := a[10]a23 := a[11]
	out[0] = a00 out[1] = a01 out[2] = a02 out[3] = a03
	out[4] = a10 out[5] = a11 out[6] = a12 out[7] = a13
	out[8] = a20 out[9] = a21 out[10] = a22 out[11] = a23
	out[12] = a00 * x + a10 * y + a20 * z + a[12]
	out[13] = a01 * x + a11 * y + a21 * z + a[13]
	out[14] = a02 * x + a12 * y + a22 * z + a[14]
	out[15] = a03 * x + a13 * y + a23 * z + a[15]
	return mat4(out)
}

/* 
fn normalize(vec Vec3) Vec3 {
	# return myglm_normalize(vec);
	return Vec3{}
}
*/
// https://github.com/g-truc/glm/blob/0ceb2b755fb155d593854aefe3e45d416ce153a4/glm/ext/matrix_clip_space.inl
fn ortho(left, right, bottom, top float) Mat4 {
	println('glm ortho($left, $right, $bottom, $top)')
	// mat<4, 4, T, defaultp> Result(static_cast<T>(1));
	n := 16
	mut res := float_calloc(n)
	# res[0] = 2 / (right - left) ;
	# res[5] = 2.0 / (top - bottom);
	# res[10] =  (1);
	# res[12] = - (right + left) / (right - left);
	# res[13] = - (top + bottom) / (top - bottom);
	res[15] = 1
	return mat4(res)
}

// fn scale(a *float, v Vec3) *float {
fn scale(m Mat4, v Vec3) Mat4 {
	a := m.data
	mut out := float_calloc(16)
	x := v.x
	y := v.y
	z := v.z
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
	return mat4(out)
}

// fn rotate_z(a *float, rad float) *float {
fn rotate_z(m Mat4, rad float) Mat4 {
	a := m.data
	mut out := float_calloc(16)
	s := math.sin(rad)
	c := math.cos(rad)
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
	return mat4(out)
}

fn identity() Mat4 {
	// 1 0 0 0
	// 0 1 0 0
	// 0 0 1 0
	// 0 0 0 1
	n := 16
	mut res := float_calloc(sizeof(float) * n)
	res[0] = 1
	res[5] = 1
	res[10] = 1
	res[15] = 1
	return mat4(res)
}

// returns *float without allocation
fn identity2(res *float) {
	res[0] = 1
	res[5] = 1
	res[10] = 1
	res[15] = 1
	// # float f[16]={0};// for (int i =0;i<16;i++)
	// # printf("!!%d\n", f[0]);
	// # glm__identity2(&f);
	// # gl__Shader_set_mat4(shader, tos2("projection"), f) ;
}

fn identity3() []float {
	res := [1.0, 0, 0, 0,
	0, 1, 0, 0,
	0, 0, 1, 0,
	0, 0, 0, 1,
	] !
	return res
}

// https://github.com/toji/gl-matrix/blob/1549cf21dfa14a2bc845993485343d519cf064fe/src/gl-matrix/mat4.js
fn ortho_js(left, right, bottom, top float) *float {
	mynear := 1
	myfar := 1
	lr := 1.0 / (left - right)
	bt := 1.0 / (bottom - top)
	nf := 1.0 / 1.0// (mynear -myfar)
	# float* out = malloc (sizeof(float) * 16);
	# out[0] = -2 * lr;
	# out[1] = 0;
	# out[2] = 0;
	# out[3] = 0;
	# out[4] = 0;
	# out[5] = -2 * bt;
	# out[6] = 0;
	# out[7] = 0;
	# out[8] = 0;
	# out[9] = 0;
	# out[10] = 2 * nf;
	# out[11] = 0;
	# out[12] = (left + right) * lr;
	# out[13] = (top + bottom) * bt;
	# out[14] = 1 * nf;//(far + near) * nf;
	# out[15] = 1;
	# return out;
	f := 0.0
	return &f
}

// fn ortho_old(a, b, c, d float) *float {
// # return myglm_ortho(a,b,c,d);
// }
fn cross(a, b Vec3) Vec3 {
	// # return myglm_cross(a,b);
	return Vec3{}
}

/* 
fn perspective(degrees float, ratio float, a, b float) Mat4 {
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
