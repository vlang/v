const sbuffer_size = 10

fn test_hardcoded_static_arr() {
	myints := [10]int{}
	size := sizeof(myints)
	assert size == 40
}

fn test_const_based_static_arr() {
	myints := [sbuffer_size]int{}
	size := sizeof(myints)
	assert size == 40
}

fn test_const_based_static_arr_of_f64() {
	myf64 := [sbuffer_size]f64{}
	size := sizeof(myf64)
	assert size == 80
}

fn test_const_based_static_arr_of_f32() {
	myf32 := [sbuffer_size]f32{}
	size := sizeof(myf32)
	assert size == 40
}

fn test_const_based_static_arr_of_i8() {
	myi8 := [sbuffer_size]i8{}
	size := sizeof(myi8)
	assert size == 10
}

fn test_const_based_static_arr_of_i16() {
	myi16 := [sbuffer_size]i16{}
	size := sizeof(myi16)
	assert size == 20
}
