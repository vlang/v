struct Vec {
mut:
	x f64
	y f64
	z f64
}

struct Plane {
	position Vec
	normal   Vec
}

struct Sphere {
	position Vec
	radius   f64
}

interface Object {
	position Vec
}

fn test_append_struct_to_interface_array() {
	mut scene := []Object{}

	scene << Plane{
		position: Vec{0, -10, 0}
		normal: Vec{0, -1, 0}
	}
	scene << Sphere{
		position: Vec{0, 0, -20}
		radius: 7
	}

	println(scene)

	assert scene.len == 2
	assert scene[0].position.x == 0
	assert scene[0].position.y == -10
	assert scene[0].position.z == 0
	assert scene[1].position.x == 0
	assert scene[1].position.y == 0
	assert scene[1].position.z == -20
}
