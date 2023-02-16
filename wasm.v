struct Vector {
	x int
	y int
}

fn add(a Vector, b Vector) Vector {
	return Vector{a.x + b.x, a.y + b.y}
}

pub fn test(a int, b int) (int, int) {
	vec := Vector{a, b}

	ret := add(vec, Vector{10, 5})

	return ret.x, ret.y
}