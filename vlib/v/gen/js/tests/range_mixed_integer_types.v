fn main() {
	mut iterations := 0
	for _ in int(-1) .. u32(1) {
		iterations++
	}
	assert iterations == 2

	mut arithmetic_range_entered := false
	for _ in u8(200) .. u8(255) + u8(101) {
		arithmetic_range_entered = true
		break
	}
	assert arithmetic_range_entered
}
