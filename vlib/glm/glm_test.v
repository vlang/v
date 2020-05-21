// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// might need special case for this
// import gl
import glm

fn cmp(a, b f32) bool {
	return int(a * 1000) == int(b * 1000)
}

fn test_ortho() {
	projection := glm.ortho(0, 200, 400, 0)
	$if debug {
		println(projection.data[0])
	}
	assert cmp(projection.data[0], 0.01)
	assert cmp(projection.data[1], 0.000000)
	assert cmp(projection.data[2], 0.000000)
	assert cmp(projection.data[3], 0.000000)
	assert cmp(projection.data[4], 0.000000)
	assert cmp(projection.data[5], - 0.005000)
	assert cmp(projection.data[6], 0.000000)
	assert cmp(projection.data[7], 0.000000)
	assert cmp(projection.data[8], 0.000000)
	assert cmp(projection.data[9], 0.000000)
	assert cmp(projection.data[10], 1.000000)
	assert cmp(projection.data[11], 0.000000)
	assert cmp(projection.data[12], - 1.000000)
	assert cmp(projection.data[13], 1.000000)
	assert cmp(projection.data[14], 0.000000)
	assert cmp(projection.data[15], 1.000000)
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
	for i in 0..15 {
		if m1.data[i] != m2.data[i] {
			same = false
		}
	}
	assert !same
}

fn test_translate() {
	mut m := glm.identity()
	m = glm.translate(m, glm.vec3(0, 0, - 0.5))
	$if debug {
		println(m)
	}
    assert m.data[0]  == 1.0 
    assert m.data[1]  == 0.0
    assert m.data[2]  == 0.0
    assert m.data[3]  == 0.0
    //
    assert m.data[4]  == 0.0
    assert m.data[5]  == 1.0
    assert m.data[6]  == 0.0
    assert m.data[7]  == 0.0
    
    assert m.data[8]  == 0.0
    assert m.data[9]  == 0.0
    assert m.data[10] == 1.0
    assert m.data[11] == 0.0
    //
    assert m.data[12] == 0.0
    assert m.data[13] == 0.0
    assert m.data[14] == -0.5
    assert m.data[15] == 1.0
}

fn test_mult() {
	//TODO improve test
	mut a := glm.identity()
	mut b := glm.identity()
	mut c := glm.identity()
	a = glm.mult(a, b)
	for i in 0..15 {
		assert a.data[i] == c.data[i]
	}
}
