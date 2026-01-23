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
	return Color3{u.x + v.x, u.y + v.y, u.z + v.z}
}

pub const black = Color3{1, 2, 3}

fn color(x int) Color3 {
	return black
}

fn test_main() {
	mut c := black
	for _ in 0 .. 3 {
		c += color(123)
	}
	println(c)
}
