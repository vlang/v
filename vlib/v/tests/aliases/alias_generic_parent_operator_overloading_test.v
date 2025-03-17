import math.vec

type V2 = vec.Vec2[f32]

fn test_alias_generic_parent_operator_overloading() {
	a := V2{1, 1}
	b := V2{10, 20}

	mut c := a + b
	assert c.x == 11.0
	assert c.y == 21.0
	dump(c)

	c += a
	assert c.x == 12.0
	assert c.y == 22.0
	dump(c)
}
