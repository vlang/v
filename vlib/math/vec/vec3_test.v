import math { veryclose }
import math.vec

fn test_vec3_int() {
	mut v1 := vec.vec3(0, 0, 0)
	mut v2 := vec.vec3(0, 0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1.z == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec3[int]'
	assert v3.x == 2
	assert v3.y == 2
	assert v3.z == 2
}

fn test_vec3_f32() {
	mut v1 := vec.vec3(f32(0), 0, 0)
	mut v2 := vec.vec3(f32(0), 0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1.z == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec3[f32]'
	assert v3.x == 2
	assert v3.y == 2
	assert v3.z == 2
}

fn test_vec3_f64() {
	mut v1 := vec.vec3(0.0, 0, 0)
	mut v2 := vec.vec3(0.0, 0, 0)
	assert v1 == v2
	v1.one()
	v2.one()
	assert v1.x == 1
	assert v1.y == 1
	assert v1.z == 1
	assert v1 == v2

	v3 := v1 + v2
	assert typeof(v3).name == 'vec.Vec3[f64]'
	assert v3.x == 2
	assert v3.y == 2
	assert v3.z == 2
}

fn test_vec3_f64_utils_1() {
	mut v1 := vec.vec3(2.0, 3.0, 1.5)
	mut v2 := vec.vec3(1.0, 4.0, 1.5)

	mut zv := vec.vec3(5.0, 5.0, 5.0)
	zv.zero()

	v3 := v1 + v2
	assert v3.x == 3
	assert v3.y == 7
	assert v3.z == 3

	v1l := vec.vec3(6.0, 2.0, -3.0)
	assert v1l.magnitude() == 7

	mut ctv1 := vec.vec3(0.000001, 0.000001, 0.000001)
	ctv1.clean_tolerance(0.00001)
	assert ctv1 == zv
}

fn test_vec3_f64_utils_2() {
	mut v1 := vec.vec3(4.0, 4.0, 8.0)
	assert v1.unit().magnitude() == 1
	v2 := v1.mul_scalar(0.5)
	assert v2.x == 2
	assert v2.y == 2
	assert v2.z == 4
	assert v2.unit().magnitude() == 1

	invv2 := v2.inv()
	assert invv2.x == 0.5
	assert invv2.y == 0.5
	assert invv2.z == 0.25
}

// sample tests for vec3 projection
fn test_vec3_project_onto_basic() {
	v := vec.vec3(5.0, 6.0, 0.0) // magnitude ~7.81 vector
	u := vec.vec3(3.0, 4.0, 0.0) // magnitude 5 vector
	// hand-computed:
	// v·u = 5*3 + 6*4 + 0*0 = 39
	// |u|^2 = 3^2 + 4^2 +0^2 = 25
	// scale = 39/25 = 1.56
	// proj = scale * u = (1.56*3, 1.56*4, 1.56*0) = (4.68, 6.24, 0)
	proj := v.project(u)
	assert veryclose(proj.x, 4.68)
	assert veryclose(proj.y, 6.24)
	assert veryclose(proj.z, 0.0)
}

// Test for Vec3 projection onto zero vector
//
fn test_vec3_project_onto_zero() {
	v := vec.vec3(0.0, 0.0, 0.0)
	u := vec.vec3(3.0, 4.0, 0.0)
	proj := v.project(u)
	assert proj.x == 0.0
	assert proj.y == 0.0
	assert proj.z == 0.0
}

// Test for vec3 projection at an angle
//
fn test_vec3_project_onto_angle() {
	v := vec.vec3(1.0, 1.0, 0.0) // magnitude sqrt(2) vector
	u := vec.vec3(1.0, 0.0, 0.0) // magnitude 1 vector
	// hand-computed:
	// v·u = 1*1 + 1*0 + 0*0 = 1
	// |u|^2 = 1^2 + 0^2 +0^2 = 1
	// scale = 1/1 = 1
	// proj = scale * u = (1*1, 1*0, 1*0) = (1, 0, 0)
	proj := v.project(u)
	assert veryclose(proj.x, 1.0)
	assert veryclose(proj.y, 0.0)
	assert veryclose(proj.z, 0.0)
}

// Test for perpendicularity
// 'u' and 'v' are already perpendicular so it must return v
fn test_vec3_perpendicularity_angle() {
	u := vec.vec3(1.0, 0.0, 0.0)
	v := vec.vec3(0.0, 3.0, 2.0)
	
	per := u.perpendicular(v)
	assert tolerance(per.x, v.x, vec.vec_epsilon)
	assert tolerance(per.y, v.y, vec.vec_epsilon)
	assert tolerance(per.z, v.z, vec.vec_epsilon)
}

// 'u' and 'v' are orthogonal so it must return the null vector
fn test_vec3_orthogonal_angle() {
	u := vec.vec3(1.0, 0.0, 0.0)
	v := vec.vec3(0.0, 3.0, 2.0)
	
	per := u.perpendicular(v)
	assert tolerance(per.x, 0.0, vec.vec_epsilon)
	assert tolerance(per.y, 0.0, vec.vec_epsilon)
	assert tolerance(per.z, 0.0, vec.vec_epsilon)
}
