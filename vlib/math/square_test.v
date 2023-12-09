import math

fn test_square() {
	assert math.square(0) == 0
	assert math.square(1) == 1
	assert math.square(-1) == 1
	assert math.square(5) == 25
	assert math.square(u8(3)) == 9
	assert math.square(u32(10)) == 100
	assert math.square(-7) == 49
}

fn test_cube() {
	assert math.cube(0) == 0
	assert math.cube(1) == 1
	assert math.cube(-1) == -1
	assert math.cube(5) == 125
	assert math.cube(u8(3)) == 27
	assert math.cube(u32(10)) == 1000
	assert math.cube(-7) == -343
}
