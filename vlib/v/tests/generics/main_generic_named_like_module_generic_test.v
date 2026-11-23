import json2

// `main.decode[[]int]` and `json2.decode[[]int]` are both specialized as
// `decode_T_Array_int`. The module's specialization used to register that bare
// name too, so `main`'s took its signature (a call with json2's default arguments)
// or was not generated at all.
fn decode[T]() string {
	return typeof[T]().name
}

fn test_main_and_module_generic_functions_of_one_name_keep_their_own_instances() {
	assert decode[[]int]() == '[]int'
	values := json2.decode[[]int]('[1, 2]') or { panic(err) }
	assert values == [1, 2]
	assert decode[[]u8]() == '[]u8'
}
