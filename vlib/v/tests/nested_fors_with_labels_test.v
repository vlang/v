const good = [
	'> x: 0 | y: 0 | z: 0',
	'> x: 0 | y: 0 | z: 1',
	'> x: 0 | y: 1 | z: 0',
	'> x: 0 | y: 1 | z: 1',
	'> x: 1 | y: 0 | z: 0',
	'> x: 1 | y: 0 | z: 1',
	'> x: 1 | y: 1 | z: 0',
	'> x: 1 | y: 1 | z: 1',
]

fn test_labeled_nested_loops_for_in() {
	mut values := []string{}
	abc: for x in 0 .. 2 {
		def: for y in 0 .. 5 {
			if y > 1 {
				continue abc
			}
			xyz: for z in 0 .. 10 {
				if z > 1 {
					continue def
				}
				values << '> x: ${x} | y: ${y} | z: ${z}'
			}
		}
	}
	assert values == good
}

fn test_labeled_nested_loops_for_c_style() {
	mut values := []string{}
	abc: for x := 0; x < 2; x++ {
		def: for y := 0; y < 5; y++ {
			if y > 1 {
				continue abc
			}
			xyz: for z := 0; z < 10; z++ {
				if z > 1 {
					continue def
				}
				values << '> x: ${x} | y: ${y} | z: ${z}'
			}
		}
	}
	assert values == good
}

fn test_labeled_nested_loops_for_in_array() {
	mut values := []string{}
	x_array := [0, 1]
	y_array := [0, 1, 2, 3, 4]
	z_array := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	abc: for x in x_array {
		def: for y in y_array {
			if y > 1 {
				continue abc
			}
			xyz: for z in y_array {
				if z > 1 {
					continue def
				}
				values << '> x: ${x} | y: ${y} | z: ${z}'
			}
		}
	}
	assert values == good
}

fn test_labeled_nested_loops_for_condition() {
	mut values := []string{}
	mut x := -1
	abc: for x < 1 {
		x++
		mut y := -1
		def: for y < 5 {
			y++
			if y > 1 {
				continue abc
			}
			mut z := -1
			xyz: for z < 10 {
				z++
				if z > 1 {
					continue def
				}
				values << '> x: ${x} | y: ${y} | z: ${z}'
			}
		}
	}
	assert values == good
}
