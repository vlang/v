module main

type Mat4 = [16]f32
type Mat14 = [1][16]f32

@[heap]
struct GameObject {
mut:
	transform  Mat4
	transform2 Mat14
}

@[heap]
struct Ship {
	GameObject
}

fn Ship.new() &Ship {
	mut ship := &Ship{}
	return ship
}

fn (mut ship Ship) clone() &Ship {
	return &Ship{
		...ship
	}
}

fn test_main() {
	mut v1 := Ship.new()
	v1.transform[0] = 1.0
	v1.transform[15] = 2.0
	v1.transform2[0][0] = 1.0
	v1.transform2[0][15] = 2.0
	mut v2 := v1.clone()
	eprintln('v1=${v1.transform}\nv2=${v2.transform}')
	assert v1.transform == v2.transform
	assert v1.transform2 == v2.transform2
	assert v1.transform[0] == 1.0
	assert v2.transform[0] == 1.0
	assert v1.transform[15] == 2.0
	assert v2.transform[15] == 2.0

	assert v1.transform2[0][0] == 1.0
	assert v2.transform2[0][0] == 1.0
	assert v1.transform2[0][15] == 2.0
	assert v2.transform2[0][15] == 2.0
}
