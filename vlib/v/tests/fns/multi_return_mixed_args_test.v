fn mixed_pair(n int) (int, int) {
	return n, n + 1
}

fn mixed_two(a int, b int) int {
	return a * 10 + b
}

fn mixed_three(a int, b int, c int) int {
	return a * 100 + b * 10 + c
}

fn mixed_four(a int, b int, c int, d int) int {
	return a * 1000 + b * 100 + c * 10 + d
}

fn test_multi_return_arguments_with_ordinary_arguments() {
	assert mixed_two(mixed_pair(2)) == 23
	assert mixed_three(mixed_pair(2), 4) == 234
	assert mixed_three(1, mixed_pair(2)) == 123
	assert mixed_four(1, mixed_pair(2), 4) == 1234
	assert mixed_four(mixed_pair(1), mixed_pair(3)) == 1234
}

struct MixedReceiver {
	prefix int
}

fn (r MixedReceiver) three(a int, b int, c int) int {
	return r.prefix * 1000 + a * 100 + b * 10 + c
}

fn (r MixedReceiver) four(a int, b int, c int, d int) int {
	return r.prefix * 10000 + a * 1000 + b * 100 + c * 10 + d
}

fn test_multi_return_arguments_with_ordinary_method_arguments() {
	r := MixedReceiver{ prefix: 9 }
	assert r.three(mixed_pair(2), 4) == 9234
	assert r.three(1, mixed_pair(2)) == 9123
	assert r.four(1, mixed_pair(2), 4) == 91234
	assert r.four(mixed_pair(1), mixed_pair(3)) == 91234
}

struct MixedCallCount {
mut:
	value int
}

fn counted_mixed_pair(mut calls MixedCallCount, n int) (int, int) {
	calls.value++
	return n, n + 1
}

fn test_mixed_multi_return_arguments_are_evaluated_once() {
	mut calls := MixedCallCount{}
	assert mixed_three(counted_mixed_pair(mut calls, 2), 4) == 234
	assert calls.value == 1
	assert mixed_four(counted_mixed_pair(mut calls, 1), counted_mixed_pair(mut calls, 3)) == 1234
	assert calls.value == 3
	r := MixedReceiver{ prefix: 9 }
	assert r.four(1, counted_mixed_pair(mut calls, 2), 4) == 91234
	assert calls.value == 4
}

fn heterogeneous_mixed_pair() (int, string) {
	return 42, 'answer'
}

fn mixed_heterogeneous(a int, b string, c bool) string {
	return '${a}:${b}:${c}'
}

fn (r MixedReceiver) heterogeneous(a int, b string, c bool) string {
	return '${r.prefix}:${a}:${b}:${c}'
}

fn test_mixed_multi_return_arguments_keep_component_types() {
	assert mixed_heterogeneous(heterogeneous_mixed_pair(), true) == '42:answer:true'
	r := MixedReceiver{ prefix: 9 }
	assert r.heterogeneous(heterogeneous_mixed_pair(), false) == '9:42:answer:false'
}

fn change_mixed_count(mut value MixedCallCount) int {
	value.value = 9
	return 0
}

fn mixed_read_count(a int, b int, value &MixedCallCount, marker int) int {
	return a + b + value.value + marker
}

fn (r MixedReceiver) read_count(a int, b int, value &MixedCallCount, marker int) int {
	return r.prefix + a + b + value.value + marker
}

fn mixed_read_count_after_pairs(a int, b int, c int, d int, value &MixedCallCount, marker int) int {
	return a + b + c + d + value.value + marker
}

fn test_mixed_multi_return_arguments_preserve_implicit_references() {
	mut direct := MixedCallCount{ value: 5 }
	assert mixed_read_count(1, 2, direct, change_mixed_count(mut direct)) == 12
	mut expanded := MixedCallCount{ value: 5 }
	assert mixed_read_count(mixed_pair(1), expanded, change_mixed_count(mut expanded)) == 12
	reader := mixed_read_count
	mut indirect := MixedCallCount{ value: 5 }
	assert reader(mixed_pair(1), indirect, change_mixed_count(mut indirect)) == 12
	r := MixedReceiver{ prefix: 100 }
	mut method_value := MixedCallCount{ value: 5 }
	assert r.read_count(mixed_pair(1), method_value, change_mixed_count(mut method_value)) == 112
	mut calls := MixedCallCount{}
	mut cumulative := MixedCallCount{ value: 5 }
	assert mixed_read_count_after_pairs(counted_mixed_pair(mut calls, 1),
		counted_mixed_pair(mut calls, 3), cumulative, change_mixed_count(mut cumulative)) == 19
	assert calls.value == 2
}

interface MixedShape {
	area() int
}

struct MixedSquare {
	side int
}

fn (s &MixedSquare) area() int {
	return s.side * s.side
}

struct MixedCircle {
	radius int
}

fn (c &MixedCircle) area() int {
	return 3 * c.radius * c.radius
}

fn mixed_shape_area(a int, b int, shape &MixedShape) int {
	return a + b + shape.area()
}

fn (r MixedReceiver) shape_area(a int, b int, shape &MixedShape) int {
	return r.prefix + a + b + shape.area()
}

fn mixed_generic_shape_match[T](kind string, size T) int {
	return mixed_shape_area(mixed_pair(1), match kind {
		'square' { &MixedSquare{int(size)} }
		else { &MixedCircle{int(size)} }
	})
}

fn mixed_generic_shape_if[T](square bool, size T) int {
	return mixed_shape_area(mixed_pair(1), if square {
		&MixedSquare{int(size)}
	} else {
		&MixedCircle{int(size)}
	})
}

fn mixed_method_shape_match(kind string, size int) int {
	r := MixedReceiver{ prefix: 100 }
	return r.shape_area(mixed_pair(1), match kind {
		'square' { &MixedSquare{int(size)} }
		else { &MixedCircle{int(size)} }
	})
}

fn mixed_method_shape_if(square bool, size int) int {
	r := MixedReceiver{ prefix: 100 }
	return r.shape_area(mixed_pair(1), if square {
		&MixedSquare{int(size)}
	} else {
		&MixedCircle{int(size)}
	})
}

fn test_mixed_multi_return_arguments_keep_generic_interface_branch_targets() {
	assert mixed_generic_shape_match('square', 2) == 7
	assert mixed_generic_shape_match('circle', 2) == 15
	assert mixed_generic_shape_if(true, 2) == 7
	assert mixed_generic_shape_if(false, 2) == 15
	assert mixed_method_shape_match('square', 2) == 107
	assert mixed_method_shape_match('circle', 2) == 115
	assert mixed_method_shape_if(true, 2) == 107
	assert mixed_method_shape_if(false, 2) == 115
}

fn change_mixed_fixed_array(mut values [2]int) int {
	values[0] = 9
	return 0
}

fn mixed_read_fixed_array(a int, b int, values &[2]int, marker int) int {
	return a + b + values[0] + marker
}

fn mixed_read_array_reference(a int, b int, values &[]int, marker int) int {
	return a + b + values[0] + marker
}

fn test_mixed_multi_return_arguments_borrow_original_fixed_array_storage() {
	mut fixed := [5, 6]!
	assert mixed_read_fixed_array(mixed_pair(1), fixed, change_mixed_fixed_array(mut fixed)) == 12
	mut dynamic_view := [5, 6]!
	assert mixed_read_array_reference(mixed_pair(1), dynamic_view,
		change_mixed_fixed_array(mut dynamic_view)) == 12
}

fn mixed_retarget_count(a int, b int, mut value &MixedCallCount, marker int) {
	value = &MixedCallCount{ value: a + b + marker }
}

fn test_mixed_multi_return_arguments_keep_mutable_pointer_slots() {
	mut later := MixedCallCount{}
	mut direct := &MixedCallCount{ value: 5 }
	mixed_retarget_count(mixed_pair(1), mut direct, change_mixed_count(mut later))
	assert direct.value == 3
	retarget := mixed_retarget_count
	mut pointers := [&MixedCallCount{ value: 5 }]
	for mut pointer in pointers {
		retarget(mixed_pair(1), mut pointer, change_mixed_count(mut later))
	}
	assert pointers[0].value == 3
}
