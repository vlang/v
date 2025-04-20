module main

fn bang(a int, b int) int {
	if a in 0..9 && b in 0..9 {
		return a + b
	} else {
		return 0
	}
}

fn test_main() {
	assert bang(1, 2) == 3
	assert bang(10, 1) == 0
}
