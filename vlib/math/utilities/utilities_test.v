module utilities

fn test_copysign() {
	assert copysign(5, -7) == -5.0
	assert copysign(-5, 7) == 5.0
	assert copysign(-5, -7) == -5.0
	assert copysign(10, 0) == 10.0
	assert copysign(10, 10) == 10.0
}
