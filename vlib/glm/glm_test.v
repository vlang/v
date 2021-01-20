// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// might need special case for this
// import gl
import glm

fn cmp(a f32, b f32) bool {
	return int(a * 1000) == int(b * 1000)
}

fn test_ortho() {
	projection := glm.ortho(0, 200, 400, 0)
	$if debug {
		println(unsafe {projection.data[0]})
	}
	unsafe {
		assert cmp(projection.data[0], 0.01)
		assert cmp(projection.data[1], 0.000000)
		assert cmp(projection.data[2], 0.000000)
		assert cmp(projection.data[3], 0.000000)
		assert cmp(projection.data[4], 0.000000)
		assert cmp(projection.data[5], -0.005000)
		assert cmp(projection.data[6], 0.000000)
		assert cmp(projection.data[7], 0.000000)
		assert cmp(projection.data[8], 0.000000)
		assert cmp(projection.data[9], 0.000000)
		assert cmp(projection.data[10], 1.000000)
		assert cmp(projection.data[11], 0.000000)
		assert cmp(projection.data[12], -1.000000)
		assert cmp(projection.data[13], 1.000000)
		assert cmp(projection.data[14], 0.000000)
		assert cmp(projection.data[15], 1.000000)
	}
	// f := gg.ortho(1,2,3,4)
	/*
	// for debugging broken tetris in gg.o
		# projection.data[0]=0.010000;
		# projection.data[1]=0.000000;
		# projection.data[2]=0.000000;
		# projection.data[3]=0.000000;
		# projection.data[4]=0.000000;
		# projection.data[5]=-0.005000;
		# projection.data[6]=0.000000;
		# projection.data[7]=0.000000;
		# projection.data[8]=0.000000;
		# projection.data[9]=0.000000;
		# projection.data[10]=1.000000;
		# projection.data[11]=0.000000;
		# projection.data[12]=-1.000000;
		# projection.data[13]=1.000000;
		# projection.data[14]=0.000000;
		# projection.data[15]=1.000000;
	*/
}

fn test_rotate() {
	$if debug {
		println('rotate')
	}
	mut m := glm.identity()
	m = glm.scale(m, glm.vec3(2, 2, 2))
	$if debug {
		println(m)
	}
	m = glm.rotate_z(m, 1)
	$if debug {
		println(m)
	}
	mut m1 := glm.identity()
	mut m2 := glm.identity()
	m1 = glm.rotate(1, glm.vec3(1, 0, 0), m1)
	m2 = glm.rotate(1, glm.vec3(0, 1, 0), m2)
	mut same := true
	for i in 0 .. 15 {
		if unsafe {m1.data[i]} != unsafe {m2.data[i]} {
			same = false
		}
	}
	assert !same
}

fn test_translate() {
	mut m := glm.identity()
	m = glm.translate(m, glm.vec3(0, 0, -0.5))
	$if debug {
		println(m)
	}
	unsafe {
		assert m.data[0] == 1.0
		assert m.data[1] == 0.0
		assert m.data[2] == 0.0
		assert m.data[3] == 0.0
		//
		assert m.data[4] == 0.0
		assert m.data[5] == 1.0
		assert m.data[6] == 0.0
		assert m.data[7] == 0.0
		assert m.data[8] == 0.0
		assert m.data[9] == 0.0
		assert m.data[10] == 1.0
		assert m.data[11] == 0.0
		//
		assert m.data[12] == 0.0
		assert m.data[13] == 0.0
		assert m.data[14] == -0.5
		assert m.data[15] == 1.0
	}
}

fn f32_calloc(n int) &f32 {
	return voidptr(vcalloc(n * int(sizeof(f32))))
}

fn test_mult1() {
	mut adata := f32_calloc(16)
	unsafe {
		adata[1 * 4 + 1] = 6
		adata[2 * 4 + 3] = 2
		adata[0 * 4 + 2] = 3
		adata[2 * 4 + 1] = 1
	}
	mut bdata := f32_calloc(16)
	unsafe {
		bdata[1 * 4 + 1] = -2
		bdata[2 * 4 + 3] = 1
		bdata[0 * 4 + 2] = 6
		bdata[2 * 4 + 1] = -3
	}
	mut expected := f32_calloc(16)
	unsafe {
		expected[0 * 4 + 0] = 0 // 0*0+0*0+0*6+0*0
		expected[0 * 4 + 1] = 6 // 0*0+0*6+1*6+0*0
		expected[0 * 4 + 2] = 0 // 3*0+0*0+0*6+0*0
		expected[0 * 4 + 3] = 12 // 0*0+0*0+2*6+0*0
		expected[1 * 4 + 0] = 0 // 0*0+0*-2+0*0+0*0
		expected[1 * 4 + 1] = -12 // 0*0­+6*-2+1*0­+0*0
		expected[1 * 4 + 2] = 0 // 3*0­+0*-2­+0*0­+0*0
		expected[1 * 4 + 3] = 0 // 0*0­+0*-2­+2*0­+0*0
		expected[2 * 4 + 0] = 0 // 0*0­+0*-3­+0*0­+0*1
		expected[2 * 4 + 1] = -18 // 0*0­+6*-3­+1*0­+0*1
		expected[2 * 4 + 2] = 0 // 3*0­+0*-3+0*0­+0*1
		expected[2 * 4 + 3] = 0 // 0*0­+0*-3­+2*0­+0*1
		expected[3 * 4 + 0] = 0 // 0*0­+0*0­+0*0­+0*0
		expected[3 * 4 + 1] = 0 // 0*0­+6*0­+1*0­+0*0
		expected[3 * 4 + 2] = 0 // 3*0­+0*0­+0*0­+0*0
		expected[3 * 4 + 3] = 0 // 0*0­+0*0­+2*0­+0*0
	}
	mut a := glm.Mat4{adata}
	b := glm.Mat4{bdata}
	a = glm.mult(a, b)
	for i in 0 .. 15 {
		assert unsafe {a.data[i]} == unsafe {expected[i]}
	}
}
