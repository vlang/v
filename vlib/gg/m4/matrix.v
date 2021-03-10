/**********************************************************************
*
* Simply vector/matrix utility
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
module m4

import math

pub union Mat4 {
pub mut:
	e [16]f32
	f [4][4]f32
}

pub const precision = f32(10e-7)

// default precision for the module

/*********************************************************************
*
* Utility
*
*********************************************************************/
// String representation of the matrix
pub fn (x Mat4) str() string {
	unsafe {
		return '|${x.e[0]:-6.3},${x.e[1]:-6.3},${x.e[2]:-6.3},${x.e[3]:-6.3}|\n' +
			'|${x.e[4]:-6.3},${x.e[5]:-6.3},${x.e[6]:-6.3},${x.e[7]:-6.3}|\n' +
			'|${x.e[8]:-6.3},${x.e[9]:-6.3},${x.e[10]:-6.3},${x.e[11]:-6.3}|\n' +
			'|${x.e[12]:-6.3},${x.e[13]:-6.3},${x.e[14]:-6.3},${x.e[15]:-6.3}|'
	}
}

// Remove all the raw zeros
[direct_array_access]
pub fn (a Mat4) clean() Mat4 {
	unsafe {
		x := Mat4{}
		for c, value in a.e {
			if f32_abs(value) < m4.precision {
				x.e[c] = 0
			} else {
				x.e[c] = value
			}
		}
		return x
	}
}

// Sum all the elements of the matrix
pub fn (x Mat4) sum_all() f32 {
	mut res := f32(0)
	for v in unsafe { x.e } {
		res += v
	}
	return res
}

// Check if two matrix are equal using module precision
[direct_array_access]
pub fn (x Mat4) is_equal(y Mat4) bool {
	unsafe {
		for c, value in x.e {
			if f32_abs(value - y.e[c]) > m4.precision {
				return false
			}
		}
		return true
	}
}

//-------------------------------------
// Set/Get values
//-------------------------------------
// Get an element of the matrix using [0..15] indexes, one dimension
pub fn (x Mat4) get_e(elem_index int) f32 {
	unsafe {
		return x.e[elem_index]
	}
}

// Get an element of the matrix using [0..3][0..3] indexes, two dimension
pub fn (x Mat4) get_f(index_col int, index_row int) f32 {
	unsafe {
		return x.e[(index_row << 2) + index_col]
	}
}

// Set an element of the matrix using [0..15] indexes, one dimension
pub fn (mut x Mat4) set_e(index int, value f32) {
	unsafe {
		x.e[index] = value
	}
}

// Set an element of the matrix using [0..3][0..3] indexes, two dimension
pub fn (mut x Mat4) set_f(index_col int, index_row int, value f32) {
	unsafe {
		x.e[(index_row << 2) + index_col] = value
	}
}

// Copy a matrix elements from another matrix
pub fn (mut x Mat4) copy(y Mat4) {
	unsafe {
		x.e = [
			y.e[0 ], y.e[1 ], y.e[2 ], y.e[3 ],
			y.e[4 ], y.e[5 ], y.e[6 ], y.e[7 ],
			y.e[8 ], y.e[9 ], y.e[10], y.e[11],
			y.e[12], y.e[13], y.e[14], y.e[15],
		]!
	}
}

// Set the trace of the matrix using a vec4
pub fn (mut x Mat4) set_trace(v3 Vec4) {
	unsafe {
		x.e[0 ] = v3.e[0]
		x.e[5 ] = v3.e[1]
		x.e[10] = v3.e[2]
		x.e[15] = v3.e[3]
	}
}

// Get the trace of the matrix
pub fn (x Mat4) get_trace() Vec4 {
	unsafe {
		return Vec4{ e: [ x.e[0], x.e[5], x.e[10], x.e[15],	]! }
	}
}

// Set all the matrix elements to value
pub fn (mut x Mat4) set_f32(value f32) {
	unsafe {
		x.e = [
			value, value, value, value,
			value, value, value, value,
			value, value, value, value,
			value, value, value, value,
		]!
	}
}

//-------------------------------------
// Rows/Column access
//-------------------------------------
// Set the row as the input vec4
[direct_array_access]
[unsafe]
pub fn (mut x Mat4) set_row(row int, v3 Vec4) {
	unsafe {
		x.e[row * 4 + 0] = v3.e[0]
		x.e[row * 4 + 1] = v3.e[1]
		x.e[row * 4 + 2] = v3.e[2]
		x.e[row * 4 + 3] = v3.e[3]
	}
}

// Get a row from a matrix
[direct_array_access]
[unsafe]
pub fn (x Mat4) get_row(row int) Vec4 {
	unsafe {
		return Vec4{
			e: [
				x.e[row * 4 + 0],
				x.e[row * 4 + 1],
				x.e[row * 4 + 2],
				x.e[row * 4 + 3],
			]!
		}
	}
}

// Set the column as the input vec4
[direct_array_access]
[unsafe]
pub fn (mut x Mat4) set_col(col int, v3 Vec4) {
	unsafe {
		x.e[col] = v3.e[0]
		x.e[col + 4 ] = v3.e[1]
		x.e[col + 8 ] = v3.e[2]
		x.e[col + 12] = v3.e[3]
	}
}

// Get a column from a matrix
[direct_array_access]
[unsafe]
pub fn (x Mat4) get_col(col int) Vec4 {
	unsafe {
		return Vec4{
			e: [
				x.e[col],
				x.e[col + 4 ],
				x.e[col + 8 ],
				x.e[col + 12],
			]!
		}
	}
}

// Swap two columns in the matrix
[direct_array_access]
[unsafe]
pub fn (mut x Mat4) swap_col(col1 int, col2 int) {
	unsafe {
		v0 := x.e[col1]
		v1 := x.e[col1 + 4 ]
		v2 := x.e[col1 + 8 ]
		v3 := x.e[col1 + 12]

		x.e[col1] = x.e[col2]
		x.e[col1 + 4 ] = x.e[col2 + 4 ]
		x.e[col1 + 8 ] = x.e[col2 + 8 ]
		x.e[col1 + 12] = x.e[col2 + 12]

		x.e[col2] = v0
		x.e[col2 + 4 ] = v1
		x.e[col2 + 8 ] = v2
		x.e[col2 + 12] = v3
	}
}

// Swap two rows in the matrix
[direct_array_access]
[unsafe]
pub fn (mut x Mat4) swap_row(row1 int, row2 int) {
	unsafe {
		v0 := x.e[row1 * 4 + 0]
		v1 := x.e[row1 * 4 + 1]
		v2 := x.e[row1 * 4 + 2]
		v3 := x.e[row1 * 4 + 3]

		x.e[row1 * 4 + 0] = x.e[row2 * 4 + 0]
		x.e[row1 * 4 + 1] = x.e[row2 * 4 + 1]
		x.e[row1 * 4 + 2] = x.e[row2 * 4 + 2]
		x.e[row1 * 4 + 3] = x.e[row2 * 4 + 3]

		x.e[row2 * 4 + 0] = v0
		x.e[row2 * 4 + 1] = v1
		x.e[row2 * 4 + 2] = v2
		x.e[row2 * 4 + 3] = v3
	}
}

//-------------------------------------
// Modify data
//-------------------------------------
// Transpose the matrix
pub fn (x Mat4) transpose() Mat4 {
	unsafe {
		return Mat4{ e: [
				x.e[0 ], x.e[4 ], x.e[8 ], x.e[12],
				x.e[1 ], x.e[5 ], x.e[9 ], x.e[13],
				x.e[2 ], x.e[6 ], x.e[10], x.e[14],
				x.e[3 ], x.e[7 ], x.e[11], x.e[15],
			]!
		}
	}
}

// Multiply the all the elements of the matrix by a scalar
pub fn (x Mat4) mul_scalar(s f32) Mat4 {
	unsafe {
		return Mat4{ e: [
				x.e[0 ] * s, x.e[1 ] * s, x.e[2 ] * s, x.e[3 ] * s,
				x.e[4 ] * s, x.e[5 ] * s, x.e[6 ] * s, x.e[7 ] * s,
				x.e[8 ] * s, x.e[9 ] * s, x.e[10] * s, x.e[11] * s,
				x.e[12] * s, x.e[13] * s, x.e[14] * s, x.e[15] * s,
			]!
		}
	}
}

/*********************************************************************
*
* Init/set
*
*********************************************************************/
// Return a zero matrix
pub fn zero_m4() Mat4 {
	return Mat4{ e: [
		f32(0),	0,	0,	0,
			0,	0,	0,	0,
			0,	0,	0,	0,
			0,	0,	0,	0,
			]!
	}
}

// Return a unity matrix
pub fn unit_m4() Mat4 {
	return Mat4{ e: [
			f32(1),	0,	0,	0,
				0,	1,	0,	0,
				0,	0,	1,	0,
				0,	0,	0,	1,
				]!
	}
}

// Return a matrix initialized with value
pub fn set_m4(value f32) Mat4 {
	return Mat4{ e: [
			value, value, value, value,
			value, value, value, value,
			value, value, value, value,
			value, value, value, value,
			]!
	}
}

/*********************************************************************
*
* Math
*
*********************************************************************/

// Sum of matrix, operator +
pub fn (a Mat4) + (b Mat4) Mat4 {
	unsafe {
		return Mat4{ e: [
				a.e[0 ] + b.e[0 ], 	a.e[1 ] + b.e[1 ], 	a.e[2 ] + b.e[2 ],	a.e[3 ] + b.e[3 ],
				a.e[4 ] + b.e[4 ],	a.e[5 ] + b.e[5 ],	a.e[6 ] + b.e[6 ],	a.e[7 ] + b.e[7 ],
				a.e[8 ] + b.e[8 ],	a.e[9 ] + b.e[9 ],	a.e[10] + b.e[10],	a.e[11] + b.e[11],
				a.e[12] + b.e[12],	a.e[13] + b.e[13],	a.e[14] + b.e[14],	a.e[15] + b.e[15],
			]!
		}
	}
}

// Subtraction of matrix, operator -
pub fn (a Mat4) - (b Mat4) Mat4 {
	unsafe {
		return Mat4{ e: [
				a.e[0 ] - b.e[0 ], 	a.e[1 ] - b.e[1 ],	a.e[2 ] - b.e[2 ],	a.e[3 ] - b.e[3 ],
				a.e[4 ] - b.e[4 ],	a.e[5 ] - b.e[5 ],	a.e[6 ] - b.e[6 ],	a.e[7 ] - b.e[7 ],
				a.e[8 ] - b.e[8 ],	a.e[9 ] - b.e[9 ],	a.e[10] - b.e[10],	a.e[11] - b.e[11],
				a.e[12] - b.e[12],	a.e[13] - b.e[13],	a.e[14] - b.e[14],	a.e[15] - b.e[15],
			]!
		}
	}
}

// Multiplication of matrix, operator *
pub fn (a Mat4) * (b Mat4) Mat4 {
	unsafe {
		return Mat4{
			e: [
				/* [0][0] */  a.f[0][0] * b.f[0][0] + a.f[0][1] * b.f[1][0] + a.f[0][2] * b.f[2][0] + a.f[0][3] * b.f[3][0]
				/* [0][1] */, a.f[0][0] * b.f[0][1] + a.f[0][1] * b.f[1][1] + a.f[0][2] * b.f[2][1] + a.f[0][3] * b.f[3][1]
				/* [0][2] */, a.f[0][0] * b.f[0][2] + a.f[0][1] * b.f[1][2] + a.f[0][2] * b.f[2][2] + a.f[0][3] * b.f[3][2]
				/* [0][3] */, a.f[0][0] * b.f[0][3] + a.f[0][1] * b.f[1][3] + a.f[0][2] * b.f[2][3] + a.f[0][3] * b.f[3][3]

				/* [1][0] */, a.f[1][0] * b.f[0][0] + a.f[1][1] * b.f[1][0] + a.f[1][2] * b.f[2][0] + a.f[1][3] * b.f[3][0]
				/* [1][1] */, a.f[1][0] * b.f[0][1] + a.f[1][1] * b.f[1][1] + a.f[1][2] * b.f[2][1] + a.f[1][3] * b.f[3][1]
				/* [1][2] */, a.f[1][0] * b.f[0][2] + a.f[1][1] * b.f[1][2] + a.f[1][2] * b.f[2][2] + a.f[1][3] * b.f[3][2]
				/* [1][3] */, a.f[1][0] * b.f[0][3] + a.f[1][1] * b.f[1][3] + a.f[1][2] * b.f[2][3] + a.f[1][3] * b.f[3][3]

				/* [2][0] */, a.f[2][0] * b.f[0][0] + a.f[2][1] * b.f[1][0] + a.f[2][2] * b.f[2][0] + a.f[2][3] * b.f[3][0]
				/* [2][1] */, a.f[2][0] * b.f[0][1] + a.f[2][1] * b.f[1][1] + a.f[2][2] * b.f[2][1] + a.f[2][3] * b.f[3][1]
				/* [2][2] */, a.f[2][0] * b.f[0][2] + a.f[2][1] * b.f[1][2] + a.f[2][2] * b.f[2][2] + a.f[2][3] * b.f[3][2]
				/* [2][3] */, a.f[2][0] * b.f[0][3] + a.f[2][1] * b.f[1][3] + a.f[2][2] * b.f[2][3] + a.f[2][3] * b.f[3][3]

				/* [3][0] */, a.f[3][0] * b.f[0][0] + a.f[3][1] * b.f[1][0] + a.f[3][2] * b.f[2][0] + a.f[3][3] * b.f[3][0]
				/* [3][1] */, a.f[3][0] * b.f[0][1] + a.f[3][1] * b.f[1][1] + a.f[3][2] * b.f[2][1] + a.f[3][3] * b.f[3][1]
				/* [3][2] */, a.f[3][0] * b.f[0][2] + a.f[3][1] * b.f[1][2] + a.f[3][2] * b.f[2][2] + a.f[3][3] * b.f[3][2]
				/* [3][3] */, a.f[3][0] * b.f[0][3] + a.f[3][1] * b.f[1][3] + a.f[3][2] * b.f[2][3] + a.f[3][3] * b.f[3][3],
			]!
		}
	}
}

// Sum of matrix function
pub fn add(a Mat4, b Mat4) Mat4 {
	unsafe {
		return a + b
	}
}

// Subtraction of matrix function
pub fn sub(a Mat4, b Mat4) Mat4 {
	unsafe {
		return a - b
	}
}

// Multiplication of matrix function
pub fn mul(a Mat4, b Mat4) Mat4 {
	unsafe {
		return a * b
	}
}

// Multiply a Matrix by a vector
pub fn mul_vec(a Mat4, v Vec4) Vec4 {
	unsafe {
		return Vec4{ e: [
				a.e[0 ] * v.e[0] + a.e[1 ] * v.e[1] + a.e[2 ] * v.e[2] + a.e[3 ] * v.e[3],
				a.e[4 ] * v.e[0] + a.e[5 ] * v.e[1] + a.e[6 ] * v.e[2] + a.e[7 ] * v.e[3],
				a.e[8 ] * v.e[0] + a.e[9 ] * v.e[1] + a.e[10] * v.e[2] + a.e[11] * v.e[3],
				a.e[12] * v.e[0] + a.e[13] * v.e[1] + a.e[14] * v.e[2] + a.e[15] * v.e[3],
				]!
		}
	}
}

// Calculate the determinant of the Matrix
pub fn det(x Mat4) f32 {
	unsafe {
		mut t := [6]f32{}
		x00 := x.f[0][0]
		x10 := x.f[1][0]
		x20 := x.f[2][0]
		x30 := x.f[3][0]
		x01 := x.f[0][1]
		x11 := x.f[1][1]
		x21 := x.f[2][1]
		x31 := x.f[3][1]
		x02 := x.f[0][2]
		x12 := x.f[1][2]
		x22 := x.f[2][2]
		x32 := x.f[3][2]
		x03 := x.f[0][3]
		x13 := x.f[1][3]
		x23 := x.f[2][3]
		x33 := x.f[3][3]

		t[0] = x22 * x33 - x23 * x32
		t[1] = x12 * x33 - x13 * x32
		t[2] = x12 * x23 - x13 * x22
		t[3] = x02 * x33 - x03 * x32
		t[4] = x02 * x23 - x03 * x22
		t[5] = x02 * x13 - x03 * x12

		return 0.0 +
			x00 * (x11 * t[0] - x21 * t[1] + x31 * t[2]) -
			x10 * (x01 * t[0] - x21 * t[3] + x31 * t[4]) +
			x20 * (x01 * t[1] - x11 * t[3] + x31 * t[5]) -
			x30 * (x01 * t[2] - x11 * t[4] + x21 * t[5])
	}
}

// Calculate the inverse of the Matrix
pub fn (x Mat4) inverse() Mat4 {
	unsafe {
		mut t := [6]f32{}
		mut det := f32(0)

		a := x.f[0][0]
		b := x.f[1][0]
		c := x.f[2][0]
		d := x.f[3][0]
		e := x.f[0][1]
		f := x.f[1][1]
		g := x.f[2][1]
		h := x.f[3][1]
		i := x.f[0][2]
		j := x.f[1][2]
		k := x.f[2][2]
		l := x.f[3][2]
		m := x.f[0][3]
		n := x.f[1][3]
		o := x.f[2][3]
		p := x.f[3][3]

		t[0] = k * p - o * l
		t[1] = j * p - n * l
		t[2] = j * o - n * k
		t[3] = i * p - m * l
		t[4] = i * o - m * k
		t[5] = i * n - m * j

		mut dest := Mat4{}
		dest.f[0][0] = f * t[0] - g * t[1] + h * t[2]
		dest.f[0][1] = -(e * t[0] - g * t[3] + h * t[4])
		dest.f[0][2] = e * t[1] - f * t[3] + h * t[5]
		dest.f[0][3] = -(e * t[2] - f * t[4] + g * t[5])

		dest.f[1][0] = -(b * t[0] - c * t[1] + d * t[2])
		dest.f[1][1] = a * t[0] - c * t[3] + d * t[4]
		dest.f[1][2] = -(a * t[1] - b * t[3] + d * t[5])
		dest.f[1][3] = a * t[2] - b * t[4] + c * t[5]

		t[0] = g * p - o * h
		t[1] = f * p - n * h
		t[2] = f * o - n * g
		t[3] = e * p - m * h
		t[4] = e * o - m * g
		t[5] = e * n - m * f

		dest.f[2][0] = b * t[0] - c * t[1] + d * t[2]
		dest.f[2][1] = -(a * t[0] - c * t[3] + d * t[4])
		dest.f[2][2] = a * t[1] - b * t[3] + d * t[5]
		dest.f[2][3] = -(a * t[2] - b * t[4] + c * t[5])

		t[0] = g * l - k * h
		t[1] = f * l - j * h
		t[2] = f * k - j * g
		t[3] = e * l - i * h
		t[4] = e * k - i * g
		t[5] = e * j - i * f

		dest.f[3][0] = -(b * t[0] - c * t[1] + d * t[2])
		dest.f[3][1] = a * t[0] - c * t[3] + d * t[4]
		dest.f[3][2] = -(a * t[1] - b * t[3] + d * t[5])
		dest.f[3][3] = a * t[2] - b * t[4] + c * t[5]

		tmp := (a * dest.f[0][0] + b * dest.f[0][1] + c * dest.f[0][2] + d * dest.f[0][3])
		if tmp != 0 {
			det = f32(1.0) / tmp
		}
		return dest.mul_scalar(det)
	}
}

/*********************************************************************
*
* Transformations
*
*********************************************************************/

// Get a rotation matrix using w as rotation axis vector, the angle is in radians
pub fn rotate(angle f32, w Vec4) Mat4 {
	cs := f32(math.cos(angle))
	sn := f32(math.sin(angle))
	cv := f32(1.0) - cs
	axis := w.normalize3()
	unsafe {
		ax := axis.e[0]
		ay := axis.e[1]
		az := axis.e[2]

		return Mat4{ e: [
				/* [0][0] */  (ax * ax * cv) + cs
				/* [0][1] */, (ax * ay * cv) + az * sn
				/* [0][2] */, (ax * az * cv) - ay * sn
				/* [0][3] */, 0

				/* [1][0] */, (ay * ax * cv) - az * sn
				/* [1][1] */, (ay * ay * cv) + cs
				/* [1][2] */, (ay * az * cv) + ax * sn
				/* [1][3] */, 0

				/* [2][0] */, (az * ax * cv) + ay * sn
				/* [2][1] */, (az * ay * cv) - ax * sn
				/* [2][2] */, (az * az * cv) + cs
				/* [2][3] */, 0

				/* [3][0] */, 0
				/* [3][1] */, 0
				/* [3][2] */, 0
				/* [3][3] */, 1,
			]!
		}
	}
}

/*********************************************************************
*
* Graphic
*
*********************************************************************/
// Get a matrix translated by a vector w
pub fn (x Mat4) translate(w Vec4) Mat4 {
	unsafe {
		return Mat4{ e: [
				x.e[0],	x.e[1], x.e[2 ], 	x.e[3 ] ,
				x.e[4], x.e[5],	x.e[6 ], 	x.e[7 ] ,
				x.e[8], x.e[9], x.e[10], 	x.e[11] ,
				x.e[12] + w.e[0], 	x.e[13] + w.e[1], x.e[14] + w.e[2], x.e[15],
				]!
		}
	}
}

// Get a scale matrix, the scale vector is w, only xyz are evaluated.
pub fn scale(w Vec4) Mat4 {
	unsafe {
		return Mat4{ e: [
				w.e[0], 	0,			0,			0,
				0,			w.e[1],		0,			0,
				0,			0,			w.e[2],		0,
				0,			0,			0,			1,
				]!
		}
	}
}
