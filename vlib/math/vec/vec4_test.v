import math.vec

fn test_vec4_int() {
	mut v1 := vec.vec4(0, 0, 0, 0)
	mut v2 := vec.vec4(0, 0, 0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1.z == 1
	assert v1.w == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec4[int]'
	assert v3.x == 2
	assert v3.y == 2
	assert v3.z == 2
	assert v3.w == 2
}

fn test_vec4_f32() {
	mut v1 := vec.vec4(f32(0), 0, 0, 0)
	mut v2 := vec.vec4(f32(0), 0, 0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1.z == 1
	assert v1.w == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec4[f32]'
	assert v3.x == 2
	assert v3.y == 2
	assert v3.z == 2
	assert v3.w == 2
}

fn test_vec4_f64() {
	mut v1 := vec.vec4(0.0, 0, 0, 0)
	mut v2 := vec.vec4(0.0, 0, 0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1.z == 1
	assert v1.w == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec4[f64]'
	assert v3.x == 2
	assert v3.y == 2
	assert v3.z == 2
	assert v3.w == 2
}

fn test_vec4_f64_utils_1() {
	mut v1 := vec.vec4(2.0, 3.0, 1.5, 3.0)
	mut v2 := vec.vec4(1.0, 4.0, 1.5, 3.0)

	mut zv := vec.vec4(5.0, 5.0, 5.0, 5.0)
	zv.zero()

	v3 := v1 + v2
	assert v3.x == 3
	assert v3.y == 7
	assert v3.z == 3
	assert v3.w == 6

	assert v3.unit().magnitude() == 1

	mut ctv1 := vec.vec4(0.000001, 0.000001, 0.000001, 0.000001)
	ctv1.clean_tolerance(0.00001)
	assert ctv1 == zv
}

fn test_vec4_f64_utils_2() {
	mut v1 := vec.vec4(4.0, 4.0, 8.0, 2.0)
	assert v1.unit().magnitude() == 1

	v2 := v1.mul_scalar(0.5)
	assert v2.x == 2
	assert v2.y == 2
	assert v2.z == 4
	assert v2.w == 1
	assert v2.unit().magnitude() == 1

	invv2 := v2.inv()
	assert invv2.x == 0.5
	assert invv2.y == 0.5
	assert invv2.z == 0.25
	assert invv2.w == 1.0
}
