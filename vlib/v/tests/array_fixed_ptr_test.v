fn test_main() {
	a := &[2]u8{}
	dump(a)
	assert a.len == 2
}

fn test_empty() {
	_ := &[2]u8{}

	assert true
}
