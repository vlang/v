// A fixed array in a multi-return slot is copied into a new dynamic array where one
// with the same element type is expected: in a forwarded `return f()` and in a
// `a, b = f()` assignment, as in a single return or assignment. The fixed arrays of
// an array appended to an array of dynamic arrays are copied the same way.
type Ints = []int

type Pair = [2]int

struct Holder {
mut:
	values []int
}

fn make_pair() (int, [2]int) {
	return 7, [1, 2]!
}

fn make_pair_result() !(int, [2]int) {
	return 7, [3, 4]!
}

fn make_pair_option() ?(int, [2]int) {
	return 7, [5, 6]!
}

fn make_alias_pair() (int, Pair) {
	return 7, Pair([7, 8]!)
}

fn make_words() (int, [2]string) {
	return 7, ['a', 'b']!
}

fn make_optional_pair() ?[2]int {
	return [9, 10]!
}

fn forward_pair() (int, []int) {
	return make_pair()
}

fn forward_pair_result() !(int, []int) {
	return make_pair_result()
}

fn forward_pair_option() ?(int, []int) {
	return make_pair_option()
}

fn forward_alias_pair() (int, Ints) {
	return make_alias_pair()
}

fn forward_words() (int, []string) {
	return make_words()
}

fn test_forwarded_fixed_array_slots_are_copied() {
	n, mut values := forward_pair()
	assert n == 7
	values << 0
	assert values == [1, 2, 0]
	_, from_result := forward_pair_result() or { panic(err) }
	assert from_result == [3, 4]
	_, from_option := forward_pair_option() or { panic('none') }
	assert from_option == [5, 6]
	_, from_alias := forward_alias_pair()
	assert from_alias.len == 2
	assert from_alias[1] == 8
	_, words := forward_words()
	assert words == ['a', 'b']
}

fn assign_to_mut_param(mut values []int) int {
	mut n := 0
	n, values = make_pair()
	return n
}

fn test_multi_return_assignment_fixed_array_slots_are_copied() {
	mut n := 0
	mut values := []int{}
	n, values = make_pair()
	assert n == 7
	values << 0
	assert values == [1, 2, 0]
	n, values = make_pair_result() or { panic(err) }
	assert values == [3, 4]
	n, values = make_pair_option() or { panic('none') }
	assert values == [5, 6]
	mut words := []string{}
	n, words = make_words()
	assert words == ['a', 'b']
	mut holder := Holder{}
	n, holder.values = make_pair()
	assert holder.values == [1, 2]
	mut param_target := []int{}
	assert assign_to_mut_param(mut param_target) == 7
	assert param_target == [1, 2]
}

fn test_optional_fixed_array_payload_assignment_is_copied() {
	mut values := ?[]int(none)
	values = make_optional_pair()
	payload := values or { panic('none') }
	assert payload == [9, 10]
}

fn test_appended_fixed_array_elements_are_copied() {
	fixed_rows := [[1, 2]!, [3, 4]!]
	mut rows := [][]int{}
	rows << fixed_rows
	rows[0][0] = 9
	assert rows == [[9, 2], [3, 4]]
	assert fixed_rows[0][0] == 1
}
