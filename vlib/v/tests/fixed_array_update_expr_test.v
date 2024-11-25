module main

type Mat4 = [3]f32

@[heap]
struct Game {
mut:
	object_id u32
}

@[heap]
struct GameObject {
mut:
	transform Mat4 = Mat4([f32(1), 2, 3]!)
}

fn (mut gameobject GameObject) instance() &GameObject {
	return &GameObject{
		...gameobject
	}
}

fn test_main() {
	mut v := GameObject{}
	mut v2 := v.instance()
	assert v2.transform == Mat4([f32(1), 2, 3]!)
}
