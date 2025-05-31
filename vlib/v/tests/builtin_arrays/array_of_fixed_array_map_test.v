fn test_array_of_fixed_array_map_filter_any_all() {
	mut db := [][2]string{}

	db << ['aaa', '111']!
	db << ['bbb', '222']!
	db << ['ccc', '333']!

	keys := db.map(it[0])

	println(keys)
	assert keys == ['aaa', 'bbb', 'ccc']

	assert db.map(it[0] == 'aaa') == [true, false, false]
	assert db.filter(it[0] == 'bbb') == [['bbb', '222']!]
	assert db.any(it[0] == 'aaa') == true
	assert db.all(it[0] == 'aaa') == false
}
