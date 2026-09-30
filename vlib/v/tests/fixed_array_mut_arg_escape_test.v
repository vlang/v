@[has_globals]
module main

__global external_fixed = [1, 2, 3]!

struct Keeper {
mut:
	data []int
}

fn (mut k Keeper) keep(mut a []int) {
	k.data = a
}

fn pass_through(mut a []int) []int {
	return a
}

fn returned_whole() []int {
	mut x := [1, 2, 3]!
	return pass_through(mut x)
}

fn returned_range() []int {
	mut x := [1, 2, 3]!
	return pass_through(mut x[1..])
}

fn stored_whole(mut k Keeper) {
	mut x := [4, 5, 6]!
	k.keep(mut x)
}

// overwrite_stack reuses the stack that a view into a dead fixed array would point to.
fn overwrite_stack() int {
	b := [7, 7, 7, 7, 7, 7, 7, 7]!
	return b[3]
}

fn fill(mut a []int, value int) {
	for i in 0 .. a.len {
		a[i] = value
	}
}

fn append_then_write(mut a []int) {
	a[0] = 1
	a << 5
	a[1] = 2
}

fn reassign(mut a []int) {
	a[0] = 4
	a = [7, 7, 7]
	a[1] = 8
}

fn write_then_fail(mut a []int) !int {
	a[0] = 5
	return error('failed')
}

fn forward_failure(mut out [2]int) !int {
	return write_then_fail(mut out)!
}

fn test_returned_mut_fixed_array_argument_outlives_the_array() {
	whole := returned_whole()
	assert overwrite_stack() == 7
	assert whole == [1, 2, 3]
	ranged := returned_range()
	assert overwrite_stack() == 7
	assert ranged == [2, 3]
}

fn test_stored_mut_fixed_array_argument_outlives_the_array() {
	mut k := Keeper{}
	stored_whole(mut k)
	assert overwrite_stack() == 7
	assert k.data == [4, 5, 6]
}

fn test_mut_fixed_array_argument_still_writes_in_place() {
	mut a := [0, 0, 0]!
	fill(mut a, 9)
	assert a == [9, 9, 9]!
	fill(mut a[1..], 3)
	assert a == [9, 3, 3]!
	mut b := [0, 0, 0]!
	append_then_write(mut b)
	assert b == [1, 0, 0]!
	mut ranged_append := [0, 0, 0]!
	append_then_write(mut ranged_append[1..])
	assert ranged_append == [0, 1, 0]!
	mut c := [0, 0, 0]!
	reassign(mut c)
	assert c == [4, 0, 0]!
	mut ranged_reassign := [0, 0, 0]!
	reassign(mut ranged_reassign[1..])
	assert ranged_reassign == [0, 4, 0]!
	mut d := [0, 0]!
	forward_failure(mut d) or { assert err.msg() == 'failed' }
	assert d == [5, 0]!
}

fn write_overlap(mut left []int, mut right []int) int {
	left[1] = 5
	seen := right[0]
	right[1] = 8
	return seen
}

fn write_whole_and_range(mut whole []int, mut part []int) int {
	whole[1] = 6
	seen := part[0]
	part[1] = 9
	return seen
}

fn range_start(mut values []int) int {
	return values[0]
}

fn adjust_range_start(mut values []int) int {
	values[0] = 6
	values[1] = 7
	return 1
}

fn read_overlap(mut left []int, mut right []int) int {
	return left[0] + left[1] + right[0]
}

fn change_external_fixed() int {
	external_fixed[0] = 8
	return 2
}

fn read_after_argument(before int, mut values []int, change int) int {
	return before + values[0] + change
}

fn test_mut_fixed_array_views_see_later_argument_writes() {
	mut x := [1, 2, 3]!
	assert read_overlap(mut x[0..2], mut x[adjust_range_start(mut x)..3]) == 20
	assert x == [6, 7, 3]!
	external_fixed = [1, 2, 3]!
	assert read_after_argument(external_fixed[0], mut external_fixed, change_external_fixed()) == 11
	assert external_fixed == [8, 2, 3]!
}

struct KeptViews {
mut:
	left  []int
	right []int
}

fn retain_overlap(mut views KeptViews, mut left []int, mut right []int) {
	left[1] = 4
	views.left = left
	views.right = right
}

fn stored_overlapping_ranges() KeptViews {
	mut x := [1, 2, 3]!
	mut views := KeptViews{}
	retain_overlap(mut views, mut x[0..2], mut x[1..3])
	assert x == [1, 4, 3]!
	return views
}

fn return_overlap(mut left []int, mut right []int) KeptViews {
	left[1] = 7
	return KeptViews{left, right}
}

fn returned_overlapping_ranges() KeptViews {
	mut x := [1, 2, 3]!
	views := return_overlap(mut x[0..2], mut x[1..3])
	assert x == [1, 7, 3]!
	return views
}

fn test_overlapping_mut_fixed_array_arguments_share_writes() {
	mut x := [1, 2, 3]!
	assert write_overlap(mut x[0..2], mut x[1..3]) == 5
	assert x == [1, 5, 8]!
	mut y := [1, 2, 3]!
	assert write_whole_and_range(mut y, mut y[1..]) == 6
	assert y == [1, 6, 9]!
	mut nested := [1, 2, 3]!
	assert write_overlap(mut nested[0..2], mut nested[range_start(mut nested)..3]) == 5
	assert nested == [1, 5, 8]!
	mut separate := [10, 20, 30]!
	assert write_overlap(mut x[0..2], mut separate[1..3]) == 20
	assert separate == [10, 20, 8]!
}

fn test_escaped_overlapping_mut_fixed_array_views_keep_shared_storage() {
	mut stored := stored_overlapping_ranges()
	assert overwrite_stack() == 7
	assert stored.left == [1, 4]
	assert stored.right == [4, 3]
	stored.left[1] = 11
	assert stored.right[0] == 11
	mut returned := returned_overlapping_ranges()
	assert overwrite_stack() == 7
	assert returned.left == [1, 7]
	assert returned.right == [7, 3]
	returned.right[0] = 12
	assert returned.left[1] == 12
}

fn write_range_and_unpassed_element(mut values []int) {
	values[0] = 7
	external_fixed[2] = 9
}

fn test_mut_fixed_array_range_copies_back_only_its_elements() {
	external_fixed = [1, 2, 3]!
	write_range_and_unpassed_element(mut external_fixed[0..1])
	assert external_fixed == [7, 2, 9]!
}

fn keep_nested(mut values [][]int) [][]int {
	values[0][0] = 5
	return values
}

fn keep_maps(mut values []map[string]int) []map[string]int {
	values[0]['value'] = 6
	return values
}

struct NestedItem {
mut:
	values []int
}

fn keep_items(mut values []NestedItem) []NestedItem {
	values[0].values[0] = 7
	return values
}

fn test_escaped_mut_fixed_array_elements_have_independent_storage() {
	mut nested := [[1, 2], [3, 4]]!
	mut kept_nested := keep_nested(mut nested)
	assert nested[0] == [5, 2]
	kept_nested[0][0] = 9
	assert nested[0] == [5, 2]
	mut maps := [{
		'value': 1
	}, {
		'value': 2
	}]!
	mut kept_maps := keep_maps(mut maps)
	assert maps[0]['value'] == 6
	kept_maps[0]['value'] = 9
	assert maps[0]['value'] == 6
	mut items := [NestedItem{[1]}, NestedItem{[2]}]!
	mut kept_items := keep_items(mut items[0..1])
	assert items[0].values == [7]
	kept_items[0].values[0] = 9
	assert items[0].values == [7]
	assert items[1].values == [2]
}
