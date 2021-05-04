fn test_clash_var_name_of_array() {
	array_f32 := 1
	array_f64 := 2
	array_bool := 3
	array_i8 := 4
	array_byte := 5
	array_i16 := 6
	array_u16 := 7
	array_int := 8
	array_u32 := 9
	array_string := 10
	array_rune := 11

	a_f32 := []f32{}
	a_f64 := []f64{}
	a_bool := []bool{}
	a_i8 := []i8{}
	a_byte := []byte{}
	a_i16 := []i16{}
	a_u16 := []u16{}
	a_int := []int{}
	a_u32 := []u32{}
	a_string := []string{}
	a_rune := []rune{}

	println(a_f32)
	assert '$a_f32' == '[]'
	println(a_f64)
	assert '$a_f64' == '[]'
	println(a_bool)
	assert '$a_bool' == '[]'
	println(a_i8)
	assert '$a_i8' == '[]'
	println(a_byte)
	assert '$a_byte' == '[]'
	println(a_i16)
	assert '$a_i16' == '[]'
	println(a_u16)
	assert '$a_u16' == '[]'
	println(a_int)
	assert '$a_int' == '[]'
	println(a_u32)
	assert '$a_u32' == '[]'
	println(a_string)
	assert '$a_string' == '[]'
	println(a_rune)
	assert '$a_rune' == '[]'

	println(array_f32)
	assert array_f32 == 1
	println(array_f64)
	assert array_f64 == 2
	println(array_bool)
	assert array_bool == 3
	println(array_i8)
	assert array_i8 == 4
	println(array_byte)
	assert array_byte == 5
	println(array_i16)
	assert array_i16 == 6
	println(array_u16)
	assert array_u16 == 7
	println(array_int)
	assert array_int == 8
	println(array_u32)
	assert array_u32 == 9
	println(array_string)
	assert array_string == 10
	println(array_rune)
	assert array_rune == 11
}

fn test_clash_var_name_of_map() {
	map_string_int := 1
	a := map[string]int{}
	assert '$a' == '{}'
	assert map_string_int == 1
}
