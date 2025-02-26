const f = [u8(0), 1, 2]!

const ff = [[u8(1), 2, 3]!, [u8(5), 4, 3]!, f]!

fn test_main() {
	assert ff == [[u8(1), 2, 3]!, [u8(5), 4, 3]!, [u8(0), 1, 2]!]!
}
