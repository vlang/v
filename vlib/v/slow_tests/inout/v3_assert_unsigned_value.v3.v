fn main() {
	left := u64(9223372036854775808)
	right := usize(9223372036854775809)
	assert left == right
}
