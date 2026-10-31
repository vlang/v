struct Vec {
	x int
}

fn mk(x int) Vec {
	return Vec{x}
}

fn value(v &Vec) int {
	return v.x
}

// A value passed to a reference parameter is referenced implicitly, also
// when the value is that of an `unsafe` block.
fn test_unsafe_block_value_as_reference_argument() {
	cond := true
	assert value(mk(1)) == 1
	assert value(unsafe { mk(2) }) == 2
	assert value(unsafe {
		if cond {
			mk(3)
		} else {
			mk(4)
		}
	}) == 3
	assert value(if cond { unsafe { mk(5) } } else { mk(6) }) == 5
}
