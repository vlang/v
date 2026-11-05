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
