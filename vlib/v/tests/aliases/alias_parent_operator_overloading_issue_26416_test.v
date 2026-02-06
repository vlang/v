pub struct Vec3 {
pub:
	x f64
	y f64
	z f64
}

pub fn (u Vec3) + (v Vec3) Vec3 {
	return Vec3{u.x + v.x, u.y + v.y, u.z + v.z}
}

pub type Color3 = Vec3

pub fn (u Color3) + (v Color3) Color3 {
	return Color3{100 + u.x + v.x, 200 + u.y + v.y, 300 + u.z + v.z}
}

fn color(x int) Color3 {
	return Color3{1, 2, 3}
}

fn test_main() {
	mut c := Color3{1, 2, 3}
	for _ in 0 .. 3 {
		c += color(123)
	}
	println(c)
	assert c.x == 304.0 // make sure that the overloaded + for Color3 was called, and NOT the overloaded + for Vec3.
	assert c.y == 608.0
	assert c.z == 912.0
}
