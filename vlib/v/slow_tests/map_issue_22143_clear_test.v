fn add(mut m map[int]int, k int, v int) {
	m[k] = v
	dump('${m.len} ${m}')
	// for x, y in m { println('      > m key: ${x} | value: ${y}') }
}

fn test_map_clear_there_should_not_be_doble_entries_after_clear() {
	mut ints := map[int]int{}
	add(mut ints, 20, 120)
	add(mut ints, 34, 134)
	add(mut ints, 20, 150) // no double entry!
	assert ints.len == 2
	assert ints[34] == 134
	assert ints[20] == 150
	for i in 0 .. 3 {
		println('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ints.clear(), i: ${i}')
		ints.clear()
		assert ints.len == 0
		add(mut ints, 20, 2000 + i)
		assert ints[20] == 2000 + i
		add(mut ints, 34, 34)
		assert ints[34] == 34
		add(mut ints, 50, 50)
		assert ints[50] == 50
		assert ints.len == 3
		add(mut ints, 20, 100) // try setting the first key again
		assert ints.len == 3
		assert ints[20] == 100
		add(mut ints, 34, 55) // try setting the second key again
		assert ints.len == 3
		assert ints[34] == 55
		add(mut ints, 20, 200 + i) // try setting the third key again
		assert ints.len == 3
		assert ints[20] == 200 + i
	}
	assert ints.len == 3
}
