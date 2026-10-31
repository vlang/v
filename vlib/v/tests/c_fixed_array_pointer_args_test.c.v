#insert "@VEXEROOT/vlib/v/tests/c_fixed_array_pointer_args_test.c"

type CIntStorage = i32

fn C.fixed_array_pointer_sum(values [2]int) int

fn C.fixed_array_pointer_rows(values [2]&int) int

fn C.fixed_array_pointer_callbacks(values [2]fn (int) int) int

fn fixed_array_narrow_callback(value i32) i32 {
	return value + 1
}

fn test_c_fixed_array_pointer_arguments_use_c_integer_storage() {
	values := [i32(10), 20]
	aliases := [CIntStorage(10), 20]
	assert C.fixed_array_pointer_sum([int(10), 20]!) == 30
	assert C.fixed_array_pointer_sum(&values[0]) == 30
	assert C.fixed_array_pointer_sum(&aliases[0]) == 30
	assert C.fixed_array_pointer_sum(unsafe { nil }) == 0
	assert C.fixed_array_pointer_sum(unsafe { voidptr(&values[0]) }) == 30
	rows := [&values[0], &values[1]]
	assert C.fixed_array_pointer_rows(&rows[0]) == 30
	callbacks := [fixed_array_narrow_callback, fixed_array_narrow_callback]
	assert C.fixed_array_pointer_callbacks(&callbacks[0]) == 32
}
