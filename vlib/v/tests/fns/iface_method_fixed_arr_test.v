module main

pub type Mat4 = [16]f32

pub fn (a Mat4) * (b Mat4) Mat4 {
	mut res := Mat4{}
	for i := 0; i < 4; i++ {
		for j := 0; j < 4; j++ {
			res[j * 4 + i] = 0
			for k := 0; k < 4; k++ {
				res[j * 4 + i] += a[k * 4 + i] * b[j * 4 + k]
			}
		}
	}
	return res
}

interface IGameObject {
	parent    ?&IGameObject
	transform Mat4
	world_transform() Mat4
}

struct GameObject implements IGameObject {
mut:
	transform Mat4
	parent    ?&IGameObject
}

fn (gameobject GameObject) world_transform() Mat4 {
	if mut p := gameobject.parent {
		return p.world_transform() * gameobject.transform
	}
	return gameobject.transform
}

fn test_main() {
	t := GameObject{}
	a := dump(t.world_transform())
	assert a.len == 16
}
