module main

import math.vec

type Vec4 = vec.Vec4[f32]
type Vec4x = vec.Vec4[f32]

fn test_main() {
	mut v := Vec4{0, 0, 0, 1}
	v.one()
	assert v.x == 1
	assert v.y == 1
	assert v.z == 1
	assert v.w == 1
	v.from(Vec4x{3, 3, 3, 3})
	assert v.x == 3
	assert v.y == 3
	assert v.z == 3
	assert v.w == 3
}
