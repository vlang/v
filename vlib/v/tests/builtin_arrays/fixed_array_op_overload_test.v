type Vec3 = [3]int

fn (v Vec3) add(u Vec3) Vec3 {
	return Vec3([v[0] + u[0], v[1] + u[1], v[2] + u[2]]!)
}

fn (v Vec3) + (u Vec3) Vec3 {
	return Vec3([v[0] + u[0], v[1] + u[1], v[2] + u[2]]!)
}

fn test_main() {
	vec := Vec3([1, 2, 3]!)
	a := vec.add(vec)
	b := vec + vec
	assert a == b
}
