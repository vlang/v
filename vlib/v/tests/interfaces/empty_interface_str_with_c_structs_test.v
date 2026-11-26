import os

// os declares `C.dirent` and `C.stat` with the fields its V code uses; on Linux
// they are not all fields of the real headers. The printer of an empty interface
// value (here in the assert message) used to print the C structs that the
// program references field by field, which did not compile.
interface Value {}

fn spread_values[T](values []T) []Value {
	mut params := []Value{}
	params << ...values
	return params
}

fn test_empty_interface_printer_in_a_program_that_uses_c_structs() {
	params := spread_values([7, 8])
	assert params[1] == 8
	assert sizeof(int) >= 4
	assert os.getenv('V_EMPTY_INTERFACE_C_STRUCTS_UNSET') == ''
}
