module main

type Mat22 = [2][2]f32
type Mat4 = [4]f32

@[heap]
struct Game {
mut:
	object_id u32
}

@[heap]
struct GameObject {
mut:
	transform  Mat22 = Mat22([[f32(1), 2]!, [f32(3), 4]!]!)
	transform2 Mat4  = Mat4([f32(1), 2, 3, 4]!)
}

fn (mut gameobject GameObject) instance() &GameObject {
	return &GameObject{
		...gameobject
	}
}

fn test_main() {
	mut v := GameObject{}
	mut v2 := v.instance()
	dump(v)
	assert v2.transform == Mat22([[f32(1), 2]!, [f32(3), 4]!]!)
	assert v2.transform2 == Mat4([f32(1), 2, 3, 4]!)
}
