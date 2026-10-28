#include "@DIR/generic_fn_value_fixed_array_length.h"

const C.N int

fn pick_with_fixed_array[T](value T, values [C.N]int) T {
	assert values[0] == 1
	return value
}

fn test_generic_fn_value_ignores_fixed_array_length_as_type_parameter() {
	pick_int := pick_with_fixed_array[int]
	pick_string := pick_with_fixed_array[string]
	assert pick_int(42, [1, 2]!) == 42
	assert pick_string('value', [1, 2]!) == 'value'
}
