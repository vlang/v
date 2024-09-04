struct Another {
	a [3]int
	b [4]u8
	c [2]u32
}

fn test_main() {
	$for f in Another.fields {
	}
	assert true
}
