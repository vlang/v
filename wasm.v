/* struct Vector {
mut:
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
} */

/* fn (mut a Vector) add(b Vector) {
	a.x += b.x
	a.y += b.y
}

pub fn test() (int, int) {
	mut val := Vector{10, 15}

	val.add(Vector{50, 20})

	return val.x, val.y
} */

/* struct Test {
	x int
}

fn (t Test) get_x() int {
	return t.x
}

pub fn test() int {
	a := Test{x: -1}
	return a.get_x()
} */