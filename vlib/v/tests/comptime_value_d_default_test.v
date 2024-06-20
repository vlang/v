const my_f64 = $d('my_f64', 1.0)
const my_i64 = $d('my_i64', 2)
const my_string = $d('my_string', 'three')
const my_bool = $d('my_bool', false)
const my_char = $d('my_char', `f`)

fn test_default_compile_values() {
	assert my_f64 == 1.0
	assert my_i64 == 2
	assert my_string == 'three'
	assert my_bool == false
	assert my_char == `f`
	my_fixed_size_array := [$d('my_size', 4)]int{}
	assert my_fixed_size_array.len == 4
}
