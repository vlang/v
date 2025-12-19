module main

fn test_main() {
	v1 := unsafe { bool(1) }
	v10 := unsafe { bool(10) }

	assert v1 == true
	assert v10 == true

	v2 := false
	assert int(v2) == 0
	v3 := true
	assert int(v3) == 1
}
