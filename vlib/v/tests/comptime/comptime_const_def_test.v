const buf_size = $if true {
	2
} $else {
	1
}

fn test_main() {
	mut buf := [buf_size]u8{}
	assert buf.len == 2
}
