fn test_assign() {
	_ = 123
}

fn fn_with_blank_param(_ int) {
	_ = 456
}

fn test_fn_with_blank_param() {
	fn_with_blank_param(321)
}

fn fn_with_multiple_blank_param(_ int, _ f32) {
	_ = 'not an int nor a float'
}

interface Foo {
	fn_with_multiple_blank_param(int, f32)
}

struct Abc {}

fn (_ Abc) fn_with_multiple_blank_param(_ int, _ f32) {}

fn test_fn_with_multiple_blank_param() {
	fn_with_multiple_blank_param(1, 1.1)
	a := Abc{}
	a.fn_with_multiple_blank_param(1, 1.1)
}

fn call_fn_with_multiple_blank_param(foo Foo) {
	foo.fn_with_multiple_blank_param(1, 1.1)
}

fn test_interface_fn_with_multiple_blank_param() {
	call_fn_with_multiple_blank_param(Abc{})
}

fn test_for_in_range() {
	for _ in 1 .. 10 {
		assert true
	}
}

fn test_nested_range() {
	for _ in 1 .. 10 {
		for _ in 1 .. 10 {
			assert true
		}
	}
}

fn test_for_in_array_simple() {
	for _ in [1, 2, 3] {
		assert true
	}
}

fn test_for_in_array_key() {
	mut i := 1
	for _, v in [1, 2, 3] {
		assert v == i
		i++
	}
}

fn test_for_in_array_val() {
	mut j := 0
	for i, _ in [3, 4, 5] {
		assert i == j
		j++
	}
}

fn test_for_in_array_both() {
	for _, _ in [1, 2, 3] {
		assert true
	}
}

fn test_nested_for_in_array_simple() {
	for _ in [1, 2, 3] {
		for _ in [1, 2, 3] {
			assert true
		}
	}
}

fn test_nested_for_in_array_key() {
	for _, v in [1, 2, 3] {
		assert v > 0
		for _, w in [1, 2, 3] {
			assert w > 0
			assert true
		}
	}
}

fn test_nested_for_in_array_val() {
	for i, _ in [1, 2, 3] {
		assert i > -1
		for j, _ in [1, 2, 3] {
			assert j > -1
			assert true
		}
	}
}

fn test_nested_for_in_array_both() {
	for _, _ in [1, 2, 3] {
		for _, _ in [1, 2, 3] {
			assert true
		}
	}
}

const m = {
	'key': 'value'
}

fn test_for_in_map_key() {
	for _, v in m {
		assert v == 'value'
	}
}

fn test_for_in_map_val() {
	for i, _ in m {
		assert i == 'key'
	}
}

fn test_for_in_map_both() {
	mut i := 0
	for _, _ in m {
		i++
	}
	assert i == 1
}

fn test_nested_for_in_map_key() {
	for _, v in m {
		assert v == 'value'
		for _, w in m {
			assert w == 'value'
		}
	}
}

fn test_nested_for_in_map_val() {
	for i, _ in m {
		assert i == 'key'
		for j, _ in m {
			assert j == 'key'
		}
	}
}

fn test_nested_for_in_map_both() {
	mut i := 0
	for _, _ in m {
		for _, _ in m {
			i++
		}
	}
	assert i == 1
}

fn fn_for_in_variadic_args_simple(arr ...string) {
	for _ in arr {
		assert true
	}
}

fn fn_for_in_variadic_args_key(arr ...string) {
	for _, v in arr {
		assert v > 'A'
		assert true
	}
}

fn fn_for_in_variadic_args_val(arr ...string) {
	for i, _ in arr {
		assert i > -1
		assert true
	}
}

fn fn_for_in_variadic_args_both(arr ...string) {
	for _, _ in arr {
		assert true
	}
}

fn fn_nested_for_in_variadic_args(arr ...string) {
	for _ in arr {
		for _ in arr {
			assert true
		}
	}
}

fn fn_nested_for_in_variadic_args_key(arr ...string) {
	for _, v in arr {
		assert v > 'A'
		for _, w in arr {
			assert w > 'A'
			assert true
		}
	}
}

fn fn_nested_for_in_variadic_args_val(arr ...string) {
	for i, _ in arr {
		assert i > -1
		for j, _ in arr {
			assert j > -1
			assert true
		}
	}
}

fn fn_nested_for_in_variadic_args_both(arr ...string) {
	for _, _ in arr {
		for _, _ in arr {
			assert true
		}
	}
}

fn test_for_in_variadic_args() {
	fn_for_in_variadic_args_simple('a', 'b', 'c')
	fn_for_in_variadic_args_key('a', 'b', 'c')
	fn_for_in_variadic_args_val('a', 'b', 'c')
	fn_for_in_variadic_args_both('a', 'b', 'c')
	fn_nested_for_in_variadic_args('a', 'b', 'c')
	fn_nested_for_in_variadic_args_key('a', 'b', 'c')
	fn_nested_for_in_variadic_args_val('a', 'b', 'c')
	fn_nested_for_in_variadic_args_both('a', 'b', 'c')
}

fn test_for_in_string_simple() {
	for _ in 'abcd' {
		assert true
	}
}

fn test_for_in_string_key() {
	for _, v in 'a' {
		assert v == `a`
	}
}

fn test_for_in_string_val() {
	for i, _ in 'a' {
		assert i == 0
	}
}

fn test_for_in_string_both() {
	for _, _ in 'abcd' {
		assert true
	}
}

fn test_nested_for_in_string_simple() {
	for _ in 'abcd' {
		for _ in 'abcd' {
			assert true
		}
	}
}

fn test_nested_for_in_string_key() {
	for _, v in 'a' {
		assert v == `a`
		for _, w in 'a' {
			assert w == `a`
		}
	}
}

fn test_nested_for_in_string_val() {
	for i, _ in 'a' {
		assert i == 0
		for j, _ in 'a' {
			assert j == 0
		}
	}
}

fn test_nested_for_in_string_both() {
	for _, _ in 'abcd' {
		for _, _ in 'abcd' {
			assert true
		}
	}
}

fn multi_return() (int, int, string) {
	return 1, 2, '3'
}

fn test_blank_multi_return() {
	_, a, b := multi_return()
	c, _, d := multi_return()
	e, f, _ := multi_return()
	_, _, g := multi_return()
	_, h, _ := multi_return()
	i, _, _ := multi_return()
	_, _, _ := multi_return()
	assert c == 1
	assert e == 1
	assert i == 1
	assert a == 2
	assert f == 2
	assert h == 2
	assert b == '3'
	assert d == '3'
	assert g == '3'
}

fn test_blank_in_for_c_init_stmt() {
	a := []int{len: 2}
	for _ := a[1]; a[1] != 0; {
	}
	assert true
}
