module main

import math.vec

type Vec4 = vec.Vec4[f32]

fn (v Vec4) to_array() [4]f32 {
	return [v.x, v.y, v.z, v.w]!
}

fn test_main() {
	v := Vec4{1, 2, 3, 4}
	_ := [
		v.to_array(),
	]!
	u := [
		v.to_array(),
	]!
	assert u[0][0] == f32(1)
	assert u[0][1] == f32(2)
	assert u[0][2] == f32(3)
	assert u[0][3] == f32(4)
}
