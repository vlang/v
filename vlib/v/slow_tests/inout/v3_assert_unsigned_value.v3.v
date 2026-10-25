fn main() {
	left := u64(9223372036854775808)
	right := uint(9223372036854775809)
	assert left == right
}
