@[has_globals]
module main

type Mat4 = [16]f32

pub fn mat4(x0 f32, x1 f32, x2 f32, x3 f32, x4 f32, x5 f32, x6 f32, x7 f32, x8 f32, x9 f32, x10 f32, x11 f32, x12 f32, x13 f32, x14 f32, x15 f32) Mat4 {
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

pub fn unit_m4() Mat4 {
	return mat4(f32(1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)
}

struct GameObject {
mut:
	rot       Mat4 = unit_m4()
	transform Mat4 = [f32(1), 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]!
}

__global (
	p GameObject
)

fn test_main() {
	println(p)
	assert p.rot[0] == f32(1)
	assert p.rot[15] == f32(1)

	assert p.transform[0] == f32(1)
	assert p.transform[15] == f32(1)
}
