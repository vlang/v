fn test_dump_fixed_array_on_array_append() {
	mut myarr := [][3]u8{}
	myarr << [u8(1), 2, 3]!
	myarr << [u8(4), 5, 6]!
	myarr << [u8(7), 8, 9]!
	println(myarr)
	assert myarr[0] == [u8(1), 2, 3]!
	assert myarr[1] == [u8(4), 5, 6]!
	assert myarr[2] == [u8(7), 8, 9]!

	mut myarr2 := [][3]u8{}
	myarr2 << dump([u8(1), 2, 3]!)
	myarr2 << dump([u8(4), 5, 6]!)
	myarr2 << dump([u8(7), 8, 9]!)
	println(myarr2)
	assert myarr2[0] == [u8(1), 2, 3]!
	assert myarr2[1] == [u8(4), 5, 6]!
	assert myarr2[2] == [u8(7), 8, 9]!
}
