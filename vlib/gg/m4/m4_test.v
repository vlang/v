import gg.m4

pub fn test_m4() {
	unsafe {
		// Test Mat4
		mut a := m4.Mat4{ e: [
			f32(0),	1,	2,	3,
				4,	5,	6,	7,
				8,	9,	10,	11,
				12,	13,	14,	15,
				]!
		}
		mut b := m4.Mat4{}
		mut c := m4.Mat4{}

		// equal test
		assert a.e == [
			f32(0),	1,	2,	3,
				4,	5,	6,	7,
				8,	9,	10,	11,
				12,	13,	14,	15,
				]!

		// copy test
		b.copy(a)
		assert a.e == b.e

		// test: transpose, scale
		assert b.transpose().mul_scalar(2.0).mul_scalar(0.5).transpose().e == a.e
		assert b.sum_all() == 120.0

		// test rows/columns set/get
		for i in 0 .. 4 {
			b = m4.zero_m4()
			b.set_row(i, m4.Vec4{ e: [f32(1.0), 2, 3, 4]! })
			assert b.get_f(0, i) == 1.0
			assert b.get_f(1, i) == 2.0
			assert b.get_f(2, i) == 3.0
			assert b.get_f(3, i) == 4.0
			// println(b)
			c = m4.zero_m4()
			c.set_col(i, m4.Vec4{ e: [f32(1.0), 2, 3, 4]! })
			assert c.get_f(i, 0) == 1.0
			assert c.get_f(i, 1) == 2.0
			assert c.get_f(i, 2) == 3.0
			assert c.get_f(i, 3) == 4.0
			// println(c)
		}
	}
}

fn test_swap_col_row() {
	unsafe {
		// swap_col / swap_row
		b := m4.Mat4{ e: [
			f32(1),	2,	3,	4,
				5,	6,	7,	8,
				9,	10,	11,	12,
				13,	14,	15,	16,
				]!
		}
		b.swap_col(0, 2)
		assert b.e == [
			f32(3),	2,	1,	4,
				7,	6,	5,	8,
				11,	10,	9,	12,
				15,	14,	13,	16,
				]!
		b = m4.Mat4{ e: [
			f32(1),	2,	3,	4,
				5,	6,	7,	8,
				9,	10,	11,	12,
				13,	14,	15,	16,
				]!
		}
		b.swap_row(0, 2)
		assert b.e == [
			f32(9),	10,	11,	12,
				5,	6,	7,	8,
				1,	2,	3,	4,
				13,	14,	15,	16,
				]!
	}
}

fn test_sum_sub() {
	unsafe {
		// test sum/sub
		b := m4.unit_m4()
		c := m4.unit_m4()
		assert m4.sub(m4.add(b, c), b).e == m4.unit_m4().e
		assert (b + c - b).e == m4.unit_m4().e
	}
}

fn test_transpose() {
	unsafe {
		b := m4.Mat4{ e: [
			f32(0),	1,	2,	3,
				4,	5,	6,	7,
				8,	9,	10,	11,
				12,	13,	14,	15,
				]!
		}
		assert b.transpose().transpose().e == b.e
	}
}

fn test_multiplication() {
	unsafe {
		b := m4.Mat4{ e: [
			f32(1),	0,	0,	0,
				0,	2,	0,	0,
				0,	0,	3,	0,
				0,	0,	0,	4,
				]!
		}
		c := m4.Mat4{ e: [
			f32(1),	2,	3,	4,
				5,	6,	7,	8,
				9,	10,	11,	12,
				13,	14,	15,	16,
				]!
		}

		assert (c * c).e == [
			f32(90),100,110,120,
				202,228,254,280,
				314,356,398,440,
				426,484,542,600,
				]!

		assert m4.mul(c, c).e == [
			f32(90),100,110,120,
				202,228,254,280,
				314,356,398,440,
				426,484,542,600,
		]!

		assert m4.mul(b, c).e == [
			f32(1),	2,	3,	4,
				10,	12,	14,	16,
				27,	30,	33,	36,
				52,	56,	60,	64,
				]!

		assert (b * c).e == [
			f32(1),	2,	3,	4,
				10,	12,	14,	16,
				27,	30,	33,	36,
				52,	56,	60,	64,
				]!

		assert m4.det(b) == 24
	}
}

fn test_det() {
	unsafe {
		b := m4.Mat4{ e: [
			f32(5),	6,	6,	8,
				2,	2,	2,	8,
				6,	6,	2,	8,
				2,	3,	6,	7,
				]!
		}
		assert m4.det(b) == -8

		c := m4.Mat4{ e: [
			f32(1),	8,	2,	3,
				8,	2,	3,	1,
				2,	3,	3,	2,
				3,	1,	2,	4,
				]!
		}
		// println("*** INVERSE ****")
		// println(m4.mul(b.inverse(),b))
		// println(m4.clean_small(m4.mul(c.inverse(),c)))
		// println("****************")
		assert m4.mul(b.inverse(), b).e == m4.unit_m4().e
		assert m4.mul(c.inverse(), c).is_equal(m4.unit_m4())
	}
}

fn test_vec4() {
	// Test Vec4
	// println("*** Vector4 ****")
	assert m4.vec3(1,2,3) == m4.Vec4{[f32(1), 2, 3, 1]!}
	mut v := m4.Vec4{[f32(1), 2, 3, 4]!}
	assert v * v.inv() == 4
	assert v.mul_scalar(1.0 / v.mod()).mod() == 1
	assert v + m4.Vec4{ e: [f32(5), 6, 7, 8]! } == m4.Vec4{ e: [f32(6), 8, 10, 12]! }
	assert v - m4.Vec4{ e: [f32(1), 2, 3, 4]! } == m4.Vec4{ e: [f32(0), 0, 0, 0]! }
	assert v.mul_vec4(m4.Vec4{ e: [f32(2), 2, 2, 2]! }) == m4.Vec4{	e: [f32(2), 4, 6, 8]! }
	assert m4.abs(v.normalize().mod() - 1) < m4.precision
	v = m4.Vec4{[f32(1), 2, 3, 0]!}
	assert m4.abs(v.normalize3().mod3() - 1) < m4.precision
	assert m4.abs(v.normalize3().mod() - 1) < m4.precision
	// cross product
	// x y z
	// 1 2 3 ==> -3 6 -3 0
	// 4 5 6
	// println(m4.Vec4{[f32(1),2,3,2]!} % m4.Vec4{[f32(4),5,6,2]!})
	assert m4.Vec4{[f32(1), 2, 3, 0]!} % m4.Vec4{[f32(4), 5, 6, 0]!} == m4.Vec4{[ f32(-3),	6,	-3,	0, ]!}
	assert m4.Vec4{[f32(1), 2, 3, 13]!} % m4.Vec4{[f32(4), 5, 6, 11]!} == m4.Vec4{[	f32(-3), 6,	-3,	0, ]!}
	// matrix * vector
	a := m4.Mat4{ e: [
		f32(1),2,3,4
		5,6,7,8
		9,10,11,12
		13,14,15,16
		]!
	}
	assert m4.mul_vec(a, m4.Vec4{[f32(1), 2, 3, 4]!}) == m4.Vec4{[ f32(30),	70,	110,150, ]!}
	// Rotation
	// println("*** Rotation ****")
	rotx := m4.rotate(m4.rad(-90), m4.Vec4{	e: [f32(1.0), 0, 0, 0]!	}).clean()
	roty := m4.rotate(m4.rad(-90), m4.Vec4{	e: [f32(0), 1.0, 0, 0]! }).clean()
	rotz := m4.rotate(m4.rad(-90), m4.Vec4{	e: [f32(0), 0, 1, 0]!	}).clean()
	// println( rotx )
	// println( roty )
	// println( rotz )
	// println( m4.mul_vec(rotx, m4.Vec4{e:[f32(0),0,1,0]!}).clean())
	assert m4.mul_vec(roty, m4.Vec4{ e: [f32(1.0), 0.0, 0, 0]! }).clean() == m4.Vec4{ e: [f32(0), 0.0, -1, 0]! }
	assert m4.mul_vec(rotz, m4.Vec4{ e: [f32(1.0), 0.0, 0, 0]! }).clean() == m4.Vec4{ e: [f32(0), 1, 0, 0]! }
	assert m4.mul_vec(rotx, m4.Vec4{ e: [f32(0), 0, 1, 0]! }).clean() == m4.Vec4{ e: [f32(0), -1, 0, 0]! }
	// println("****************")
}

fn test_proj() {
	ort := m4.ortho(0,300,0,200,0,0)
	assert m4.mul_vec(ort, m4.Vec4{[ f32(150),	100,	0, 1]!}) == m4.Vec4{[ f32(0),	0,	0, 1]!}
	assert m4.mul_vec(ort, m4.Vec4{[ f32(0),	0,	0, 1]!}) == m4.Vec4{[ f32(-1),	-1,	0, 1]!}
	assert m4.mul_vec(ort, m4.Vec4{[ f32(300),	200,	0, 1]!}) == m4.Vec4{[ f32(1),	1,	0, 1]!}
}