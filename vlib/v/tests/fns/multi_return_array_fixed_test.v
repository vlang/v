module main

type Mat4 = [2]f32

struct GameObject {
mut:
	rot       Mat4 = Mat4([f32(1.1), 2.2]!)
	transform Mat4 = Mat4([f32(1.1), 2.2]!)
}

fn (g GameObject) calc() (Mat4, Mat4) {
	return g.rot, g.transform
}

fn test_main() {
	x, y := GameObject{}.calc()
	assert x == Mat4([f32(1.1), 2.2]!)
	assert y == Mat4([f32(1.1), 2.2]!)
}
