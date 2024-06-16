const my_f64 = $compile_value('my_f64', 1.0)
const my_int = $compile_value('my_int', 2)
const my_string = $compile_value('my_string', 'three')
const my_bool = $compile_value('my_bool', false)
const my_char = $compile_value('my_char', `f`)

fn test_default_compile_values() {
	assert my_f64 == 1.0
	assert my_int == 2
	assert my_string == 'three'
	assert my_bool == false
	assert my_char == `f`
}
