const t = [30, 38, 36, 31, 39, 37, 24, 32, 25, 33, 18, 26, 11, 18, 12, 4, 10, 17, 16, 11, 3, 9,
	2, 23, 30, 24, 16, 22, 23, 17, 9, 15, 10, 2, 8, 3, 1, 0, 7, 14, 29, 21]

fn test_multiple_iterations_of_calling_clear_should_be_always_equivalent_to_assigning_a_new_map() {
	println('array t len: ${t.len}, t: ${t}')
	mut clr := map[int]int{}
	for i in 0 .. 0xFFFF {
		mut new := map[int]int{}
		clr.clear()
		for e in t {
			new[e] = e
			clr[e] = e
			assert new.len == clr.len // , 'mismatch found after setting element: $e, on iteration ${i}'
		}
		if i & 0x3FFF == 0 {
			println('index ${i}')
			println('>> map     new: ${new.len} | ${new.keys().sorted()}')
			println('>> map cleared: ${clr.len} | ${clr.keys().sorted()}')
		}
		assert new == clr // , 'mismatch found for iteration: ${i}'
	}
}
