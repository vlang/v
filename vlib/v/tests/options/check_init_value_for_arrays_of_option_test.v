fn check_init_value_for_arrays_of_option[T]() {
	a := []?T{len: 2}
	assert a.len == 2
	for value in a {
		assert value == none
		println(value)
	}
}

enum MyEnum {
	zero
	one
	two
}

enum MyEnum32 as u32 {
	zero
	one
	two
}

struct MyStruct {
	x    int    = 123
	name string = 'abc'
}

fn test_primitives() {
	check_init_value_for_arrays_of_option[i8]()
	check_init_value_for_arrays_of_option[i16]()
	check_init_value_for_arrays_of_option[i32]()
	check_init_value_for_arrays_of_option[int]()
	check_init_value_for_arrays_of_option[i64]()

	check_init_value_for_arrays_of_option[u8]()
	check_init_value_for_arrays_of_option[u16]()
	check_init_value_for_arrays_of_option[u32]()
	check_init_value_for_arrays_of_option[u64]()

	check_init_value_for_arrays_of_option[f32]()
	check_init_value_for_arrays_of_option[f64]()

	check_init_value_for_arrays_of_option[string]()
	check_init_value_for_arrays_of_option[rune]()
}

fn test_arrays() {
	check_init_value_for_arrays_of_option[[]int]()
	check_init_value_for_arrays_of_option[[]f32]()
}

fn test_enums() {
	check_init_value_for_arrays_of_option[MyEnum]()
	check_init_value_for_arrays_of_option[MyEnum32]()
}

fn test_structs() {
	check_init_value_for_arrays_of_option[MyStruct]()
}
