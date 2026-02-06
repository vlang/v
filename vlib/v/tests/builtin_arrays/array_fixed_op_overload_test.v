import math
import math.vec

type Vector3 = vec.Vec3[f32]

fn vector3(x f32, y f32, z f32) Vector3 {
	return Vector3{x, y, z}
}

pub type Matrix4 = [16]f32

pub fn matrix4(x0 f32, x1 f32, x2 f32, x3 f32, x4 f32, x5 f32, x6 f32, x7 f32, x8 f32, x9 f32, x10 f32, x11 f32, x12 f32, x13 f32, x14 f32, x15 f32) Matrix4 {
	return [
		x0,
		x1,
		x2,
		x3,
		x4,
		x5,
		x6,
		x7,
		x8,
		x9,
		x10,
		x11,
		x12,
		x13,
		x14,
		x15,
	]!
}

pub fn (x Matrix4) str() string {
	return '|${x[0]:-6.3},${x[1]:-6.3},${x[2]:-6.3},${x[3]:-6.3}|\n' +
		'|${x[4]:-6.3},${x[5]:-6.3},${x[6]:-6.3},${x[7]:-6.3}|\n' +
		'|${x[8]:-6.3},${x[9]:-6.3},${x[10]:-6.3},${x[11]:-6.3}|\n' +
		'|${x[12]:-6.3},${x[13]:-6.3},${x[14]:-6.3},${x[15]:-6.3}|'
}

pub fn matrix4_unit() Matrix4 {
	return matrix4(f32(1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
}

pub fn (a Matrix4) * (b Matrix4) Matrix4 {
	mut res := Matrix4{}
	for i := 0; i < 4; i++ {
		for j := 0; j < 4; j++ {
			res[j * 4 + i] = 0
			for k := 0; k < 4; k++ {
				res[j * 4 + i] += a[k * 4 + i] * b[j * 4 + k]
			}
		}
	}
	return res
}

pub fn rotate(angle f32, w Vector3) Matrix4 {
	cs := f32(math.cos(angle))
	sn := f32(math.sin(angle))
	cv := f32(1.0) - cs
	axis := w.normalize()

	ax := axis.x
	ay := axis.y
	az := axis.z

	return matrix4((ax * ax * cv) + cs, (ax * ay * cv) + az * sn, (ax * az * cv) - ay * sn,
		0, (ay * ax * cv) - az * sn, (ay * ay * cv) + cs, (ay * az * cv) + ax * sn, 0,
		(az * ax * cv) + ay * sn, (az * ay * cv) - ax * sn, (az * az * cv) + cs, 0, 0,
		0, 0, 1)
}

fn test_array_fixed_op_overload() {
	mut rot := matrix4_unit()
	rot *= rotate(0.5, vector3(0.5, 0.5, 0.5))
	println(rot)
	assert true
}
