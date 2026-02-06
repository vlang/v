module main

import math.vec

type Vector3 = vec.Vec3[f32]

pub fn calc(a Vector3, b Vector3) Vector3 {
	f := a.normalize()
	return f * a + a * f
}

fn test_main() {
	a := Vector3{
		x: 1
		y: 2
		z: 3
	}
	t := calc(a, a)
	assert int(t.x) == 0
	assert int(t.y) == 2
	assert int(t.z) == 4
}
