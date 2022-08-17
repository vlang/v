fn test_array_of_fixed_array_map() {
	mut db := [][2]string{}

	db << ['aaa', '111']!
	db << ['bbb', '222']!
	db << ['ccc', '333']!

	keys := db.map(it[0])

	println(keys)
	assert keys == ['aaa', 'bbb', 'ccc']
}
