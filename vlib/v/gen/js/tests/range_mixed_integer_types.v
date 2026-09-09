fn main() {
	mut iterations := 0
	for _ in int(-1) .. u32(1) {
		iterations++
	}
	assert iterations == 2
}
