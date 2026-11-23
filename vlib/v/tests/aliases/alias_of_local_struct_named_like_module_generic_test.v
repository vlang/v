import math.vec

// math.vec declares a generic `Vec3[T]`; this program's own `Vec3` is not generic.
struct Vec3 {
	x f64
}

type Color3 = Vec3

fn test_alias_of_a_local_struct_named_like_an_imported_generic_struct() {
	assert vec.Vec2[f32]{1, 2}.x == 1
	c := Color3{1}
	assert c.x == 1.0
	assert Vec3{2}.x == 2.0
}
