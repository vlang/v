import os

fn test_for_in_optional() {
	for d in os.read_lines(@FILE) or { panic('not found') } {
		println(d)
	}
	assert true
}
