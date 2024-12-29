import math.vec

type UnusedType = vec.Vec3[f32]

fn (n UnusedType) unused_function() f32 {
	return n.mul_scalar(2).magnitude()
}

fn test_main() {
	assert vec.Vec3[f32]{0.5, 0.5, 0.5}.magnitude() == f32(0.8660254)
	assert vec.Vec3[f32]{1.5, 1.5, 1.5}.magnitude() == f32(2.598076)
}
