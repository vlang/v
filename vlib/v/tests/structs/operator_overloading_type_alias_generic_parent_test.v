import math.vec

type Vec3 = vec.Vec3[f64]

fn test_operator_overloading_type_alias_generic_parent() {
	a := Vec3{1.0, 2.0, 3.0}
	b := Vec3{0.5, 1.5, 2.5}

	assert a + b == Vec3{1.5, 3.5, 5.5}
	assert a - b == Vec3{0.5, 0.5, 0.5}
	assert a * b == Vec3{0.5, 3.0, 7.5}
	assert a / b == Vec3{2.0, 1.3333333333333333, 1.2}
}
