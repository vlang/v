module main

pub type Mat4 = [16]f32

interface IGameObject {
	world_transform() Mat4
}

struct Foo implements IGameObject {
}

fn (f Foo) world_transform() Mat4 {
	return Mat4{}
}

fn test_main() {
	t := Foo{}
	a := t.world_transform()
	b := Mat4{}
	assert a == b
}
