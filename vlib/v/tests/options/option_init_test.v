struct Cell {}

fn test_main() {
	mut var := []?Cell{len: 1}
	assert var.len == 1
}
