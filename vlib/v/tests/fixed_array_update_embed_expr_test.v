module main

type Mat4 = [14]f32

struct GameObject {
mut:
	transform Mat4
}

struct Ship {
	GameObject
}

fn (mut ship Ship) instance() &Ship {
	return &Ship{
		...ship
	}
}

fn test_fixed_array_update_embed_expr() {
	mut v := Ship{}
	mut v2 := v.instance()

	assert v2.transform.len == 14
	assert v2.transform[0] == 0
	assert v2.transform[13] == 0
	assert v2.transform.all(it == 0)

	dump('V=${v2}')
}
