module main

struct Example {}

struct Components {
	example map[string]Example
}

fn test_main() {
	a := Components{}
	b := Components{}
	if a == b {
		assert true
	} else {
		assert false
	}
}
