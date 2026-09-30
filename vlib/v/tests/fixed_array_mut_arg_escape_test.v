@[has_globals]
module main

__global external_fixed = [1, 2, 3]!
__global aliased_fixed = [0, 0]!

fn view_then_fixed(mut values []int) {
	values[0] = 1
	assert aliased_fixed[0] == 1
	aliased_fixed[0] = 2
	assert values[0] == 2
}

fn fixed_then_view(mut values []int) {
	aliased_fixed[0] = 3
	assert values[0] == 3
	values[0] = 4
	assert aliased_fixed[0] == 4
}

fn view_and_fixed_alias(mut values []int, mut original [2]int) {
	values[0] = 5
	assert original[0] == 5
	original[0] = 6
	assert values[0] == 6
}

fn test_mut_fixed_array_views_share_original_storage_during_the_call() {
	view_then_fixed(mut aliased_fixed)
	assert aliased_fixed[0] == 2
	fixed_then_view(mut aliased_fixed[0..1])
	assert aliased_fixed[0] == 4
	mut local := [0, 0]!
	view_and_fixed_alias(mut local[0..1], mut local)
	assert local[0] == 6
}

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

fn keep_array_reference(mut values []int) &[]int {
	return &values
}

fn returned_array_reference() &[]int {
	mut values := [2, 3, 4]!
	return keep_array_reference(mut values[1..])
}

struct FixedHolder {
mut:
	values [2]int
}

fn forward_holder(mut holder FixedHolder) []int {
	return pass_through(mut holder.values)
}

fn returned_holder_alias() []int {
	mut holder := FixedHolder{[4, 5]!}
	mut pointer := &holder
	mut alias := pointer
	return forward_holder(mut alias)
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
	reference := returned_array_reference()
	assert overwrite_stack() == 7
	assert reference.len == 2
	assert *reference == [3, 4]
	holder := returned_holder_alias()
	assert overwrite_stack() == 7
	assert holder == [4, 5]
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

fn test_mut_fixed_array_range_preserves_unpassed_elements() {
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

fn test_escaped_mut_fixed_array_views_keep_sharing_original_elements() {
	mut nested := [[1, 2], [3, 4]]!
	mut kept_nested := keep_nested(mut nested)
	assert nested[0] == [5, 2]
	kept_nested[0][0] = 9
	assert nested[0] == [9, 2]
	mut maps := [{
		'value': 1
	}, {
		'value': 2
	}]!
	mut kept_maps := keep_maps(mut maps)
	assert maps[0]['value'] == 6
	kept_maps[0]['value'] = 9
	assert maps[0]['value'] == 9
	mut items := [NestedItem{[1]}, NestedItem{[2]}]!
	mut kept_items := keep_items(mut items[0..1])
	assert items[0].values == [7]
	kept_items[0].values[0] = 9
	assert items[0].values == [9]
	assert items[1].values == [2]
}

fn retain_immutable_array_reference(values &[]int) &[]int {
	return values
}

fn reference_from_fixed_value(values [2]int) &[]int {
	unsafe {
		return retain_immutable_array_reference(&values)
	}
}

fn reference_from_holder_value(value FixedHolder) &[]int {
	unsafe {
		return retain_immutable_array_reference(&value.values)
	}
}

fn fixed_temporary() [2]int { return [31, 32]! }

fn reference_from_fixed_temporary(literal bool) &[]int {
	if literal { return retain_immutable_array_reference([41, 42]!) }
	return retain_immutable_array_reference(fixed_temporary())
}

fn reference_from_addressed_fixed_temporary() &[]int {
	unsafe {
		return retain_immutable_array_reference(&[51, 52]!)
	}
}

fn test_immutable_fixed_array_references_keep_value_params_and_temporaries_alive() {
	first := reference_from_fixed_value([11, 12]!)
	second := reference_from_holder_value(FixedHolder{[21, 22]!})
	third := reference_from_fixed_temporary(false)
	fourth := reference_from_fixed_temporary(true)
	fifth := reference_from_addressed_fixed_temporary()
	assert overwrite_stack() == 7
	unsafe {
		assert *first == [11, 12]
		assert *second == [21, 22]
		assert *third == [31, 32]
		assert *fourth == [41, 42]
		assert *fifth == [51, 52]
	}
}

__global fixed_source_order = []string{}

fn preceding_fixed_scalar() int {
	fixed_source_order << 'scalar'
	return 7
}

fn fixed_source_getter() &[2]int {
	fixed_source_order << 'source'
	return &aliased_fixed
}

fn ordered_fixed_source(before int, values &[]int) int {
	fixed_source_order << 'callee'
	return before + values[0]
}

fn test_fixed_array_source_getter_follows_earlier_scalar_argument() {
	fixed_source_order = []string{}
	aliased_fixed = [5, 6]!
	assert ordered_fixed_source(preceding_fixed_scalar(), fixed_source_getter()) == 12
	assert fixed_source_order == ['scalar', 'source', 'callee']
}

@[aligned: 64]
struct AlignedFixedItem {
	value int
}

struct AlignedFixedHolder {
	values [2]AlignedFixedItem
}

fn observe_aligned_fixed(values &[]AlignedFixedItem) {
	unsafe {
		assert usize(&values[0]) % 64 == 0
		assert usize(&values[1]) % 64 == 0
	}
}

fn test_fixed_array_reference_storage_preserves_element_and_container_alignment() {
	for i in 0 .. 32 {
		fixed := [AlignedFixedItem{i}, AlignedFixedItem{i + 1}]!
		observe_aligned_fixed(fixed)
		holder := AlignedFixedHolder{[AlignedFixedItem{i}, AlignedFixedItem{i + 1}]!}
		observe_aligned_fixed(holder.values)
	}
}

fn fixed_array_second_return() (int, [2]int) {
	return 7, [61, 62]!
}

fn fixed_array_three_returns() ([2]int, [2]int, [2]int) {
	return [71, 72]!, [81, 82]!, [91, 92]!
}

fn reference_from_second_fixed_return() &[]int {
	_, values := fixed_array_second_return()
	return retain_immutable_array_reference(&values)
}

fn reference_from_middle_fixed_return() &[]int {
	first, middle, last := fixed_array_three_returns()
	assert first[0] == 71
	assert last[0] == 91
	return retain_immutable_array_reference(&middle)
}

fn test_fixed_array_reference_keeps_second_multi_return_local_alive() {
	kept := reference_from_second_fixed_return()
	middle := reference_from_middle_fixed_return()
	assert overwrite_stack() == 7
	unsafe {
		assert *kept == [61, 62]
		assert *middle == [81, 82]
	}
}
