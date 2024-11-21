pub type Addr = [4]u8

fn test_alias_fixed_array_append_to_array() {
	mut my_array := []Addr{}
	for i := 0; i <= 3; i++ {
		my_array << Addr([u8(10), 0, 0, u8(i)]!)
	}
	println(my_array)
	assert my_array[0] == Addr([u8(10), 0, 0, 0]!)
	assert my_array[1] == Addr([u8(10), 0, 0, 1]!)
	assert my_array[2] == Addr([u8(10), 0, 0, 2]!)
	assert my_array[3] == Addr([u8(10), 0, 0, 3]!)
}
