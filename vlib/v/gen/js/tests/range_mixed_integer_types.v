const cast_low = 1.1
const cast_high = 1.9

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

	mut cast_range_entered := false
	for _ in u8(cast_low) .. u8(cast_high) {
		cast_range_entered = true
		break
	}
	assert cast_range_entered
}
