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

fn retain_variadic_array_reference(index int, values ...&[]int) &[]int {
	return values[index]
}

fn retain_generic_variadic_array_reference[T](index int, values ...&[]T) &[]T {
	return values[index]
}

struct VariadicFixedKeeper {}

fn (keeper VariadicFixedKeeper) retain(index int, values ...&[]int) &[]int {
	return values[index]
}

@[noinline]
fn reference_from_variadic_fixed_locals(index int, kind int) &[]int {
	first := [101, 102]!
	middle := [111, 112]!
	last := [121, 122]!
	if kind == 0 {
		return retain_variadic_array_reference(index, first, middle, last)
	}
	if kind == 1 {
		return retain_generic_variadic_array_reference[int](index, first, middle, last)
	}
	if kind == 2 {
		retain_fn := retain_variadic_array_reference
		return retain_fn(index, first, middle, last)
	}
	keeper := VariadicFixedKeeper{}
	return keeper.retain(index, first, middle, last)
}

fn test_fixed_array_variadic_references_keep_every_tail_position_alive() {
	for kind in 0 .. 4 {
		for index, expected in [[101, 102], [111, 112], [121, 122]] {
			values := reference_from_variadic_fixed_locals(index, kind)
			assert overwrite_stack() == 7
			unsafe {
				assert *values == expected
			}
		}
	}
}

@[noinline]
fn reference_from_single_variadic_fn_value() &[]int {
	values := [131, 132]!
	retain_fn := retain_variadic_array_reference
	return retain_fn(0, values)
}

fn test_fixed_array_single_variadic_fn_value_reference_remains_valid() {
	values := reference_from_single_variadic_fn_value()
	assert overwrite_stack() == 7
	unsafe {
		assert *values == [131, 132]
	}
}

type FixedIntSlice = []int
type FixedIntSliceRef = &[]int

fn retain_aliased_fixed_slice(values &FixedIntSlice) &FixedIntSlice {
	return values
}

fn retain_aliased_fixed_slice_reference(values FixedIntSliceRef) FixedIntSliceRef {
	return values
}

fn retain_variadic_aliased_fixed_slice(index int, values ...&FixedIntSlice) &FixedIntSlice {
	return values[index]
}

@[noinline]
fn reference_from_aliased_fixed_slice(index int, kind int) &[]int {
	first := [141, 142]!
	last := [151, 152]!
	if kind == 0 {
		return retain_aliased_fixed_slice(first)
	}
	if kind == 1 {
		return retain_aliased_fixed_slice_reference(first)
	}
	return retain_variadic_aliased_fixed_slice(index, first, last)
}

fn test_aliased_fixed_array_references_keep_declared_array_header_type() {
	for kind in 0 .. 3 {
		values := reference_from_aliased_fixed_slice(0, kind)
		assert overwrite_stack() == 7
		unsafe {
			assert *values == [141, 142]
		}
	}
	last := reference_from_aliased_fixed_slice(1, 2)
	assert overwrite_stack() == 7
	unsafe {
		assert *last == [151, 152]
	}
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

struct OrderedVariadicFixedKeeper {}

fn ordered_variadic_fixed_keeper() OrderedVariadicFixedKeeper {
	fixed_source_order << 'factory'
	return OrderedVariadicFixedKeeper{}
}

fn ordered_variadic_fixed_source(tag string, value int) [2]int {
	fixed_source_order << tag
	return [value, value + 1]!
}

fn (keeper OrderedVariadicFixedKeeper) consume(before int, values ...&[]int) int {
	fixed_source_order << 'callee'
	first := unsafe { *values[0] }
	last := unsafe { *values[1] }
	return before + first[0] + last[0]
}

fn test_variadic_fixed_array_sources_follow_computed_callee_and_scalar_argument() {
	fixed_source_order = []string{}
	assert ordered_variadic_fixed_keeper().consume(preceding_fixed_scalar(),
		ordered_variadic_fixed_source('first', 10), ordered_variadic_fixed_source('last', 20)) == 37
	assert fixed_source_order == ['factory', 'scalar', 'first', 'last', 'callee']
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

struct EmptyFixedPayload {}

type FixedPayload = EmptyFixedPayload | FixedHolder
type FixedPayloadAlias = FixedPayload

struct GenericFixedSumHolder[T] {
mut:
	values [2]T
}

type GenericFixedPayload[T] = EmptyFixedPayload | GenericFixedSumHolder[T]

fn keep_fixed_sum_reference(payload &FixedPayload) &[]int {
	if payload is FixedHolder {
		return retain_immutable_array_reference(&payload.values)
	}
	panic('unexpected fixed array payload')
}

fn keep_mut_fixed_sum_reference(mut payload FixedPayload) &[]int {
	if payload is FixedHolder {
		payload.values[0] = 41
		return retain_immutable_array_reference(&payload.values)
	}
	panic('unexpected fixed array payload')
}

fn keep_generic_array_reference[T](values &[]T) &[]T {
	return values
}

fn keep_generic_fixed_sum_reference[T](payload &GenericFixedPayload[T]) &[]T {
	if payload is GenericFixedSumHolder[T] {
		return keep_generic_array_reference[T](&payload.values)
	}
	panic('unexpected fixed array payload')
}

@[noinline]
fn fixed_sum_reference_from_local(kind int) &[]int {
	if kind == 0 {
		payload := FixedPayload(FixedHolder{[11, 12]!})
		return keep_fixed_sum_reference(payload)
	}
	if kind == 1 {
		mut payload := FixedPayload(FixedHolder{[21, 22]!})
		return keep_mut_fixed_sum_reference(mut payload)
	}
	if kind == 2 {
		payload := FixedPayloadAlias(FixedHolder{[31, 32]!})
		return keep_fixed_sum_reference(payload)
	}
	payload := GenericFixedPayload[int](GenericFixedSumHolder[int]{[51, 52]!})
	return keep_generic_fixed_sum_reference[int](payload)
}

fn test_fixed_array_references_into_boxed_sum_variants_remain_valid() {
	for kind, expected in [[11, 12], [41, 22], [31, 32], [51, 52]] {
		values := fixed_sum_reference_from_local(kind)
		assert overwrite_stack() == 7
		unsafe {
			assert *values == expected
		}
	}
}

fn keep_sibling_fixed_holder(holder &FixedHolder) &[]int {
	return retain_immutable_array_reference(&holder.values)
}

@[noinline]
fn fixed_array_reference_from_sibling_branch(kind int) &[]int {
	match kind {
		0 {
			holder := FixedHolder{[71, 72]!}
			return keep_sibling_fixed_holder(holder)
		}
		else {
			holder := FixedHolder{[81, 82]!}
			return keep_sibling_fixed_holder(holder)
		}
	}
}

fn test_fixed_array_reference_promotes_same_named_sibling_bindings() {
	for kind, expected in [[71, 72], [81, 82]] {
		values := fixed_array_reference_from_sibling_branch(kind)
		assert overwrite_stack() == 7
		unsafe {
			assert *values == expected
		}
	}
}

fn retain_optional_fixed_reference(values ?&[]int) ?&[]int {
	return values
}

fn retain_optional_aliased_fixed_reference(values ?FixedIntSliceRef) ?FixedIntSliceRef {
	return values
}

fn retain_variadic_optional_fixed_reference(index int, values ...?&[]int) ?&[]int {
	return values[index]
}

@[noinline]
fn optional_reference_from_local_fixed(present bool) ?&[]int {
	if !present {
		return retain_optional_fixed_reference(none)
	}
	values := [91, 92]!
	return retain_optional_fixed_reference(values)
}

@[noinline]
fn optional_aliased_reference_from_local_fixed(present bool) ?FixedIntSliceRef {
	if !present {
		return retain_optional_aliased_fixed_reference(none)
	}
	values := [101, 102]!
	return retain_optional_aliased_fixed_reference(?FixedIntSliceRef(retain_aliased_fixed_slice_reference(values)))
}

@[noinline]
fn variadic_optional_reference_from_local_fixed(index int) ?&[]int {
	first := [111, 112]!
	last := [121, 122]!
	return retain_variadic_optional_fixed_reference(index, none, ?&[]int(retain_immutable_array_reference(first)), ?&[]int(retain_immutable_array_reference(last)))
}

fn read_retained_optional_fixed_reference(values &[]int) []int {
	return unsafe { *values }
}

fn test_optional_fixed_array_reference_keeps_present_payload_alive_and_preserves_none() {
	assert optional_reference_from_local_fixed(false) == none
	assert optional_aliased_reference_from_local_fixed(false) == none
	assert variadic_optional_reference_from_local_fixed(0) == none
	present := optional_reference_from_local_fixed(true) or { panic('missing fixed reference') }
	aliased := optional_aliased_reference_from_local_fixed(true) or {
		panic('missing aliased fixed reference')
	}
	first := variadic_optional_reference_from_local_fixed(1) or { panic('missing first reference') }
	last := variadic_optional_reference_from_local_fixed(2) or { panic('missing last reference') }
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(present) == [91, 92]
	assert read_retained_optional_fixed_reference(aliased) == [101, 102]
	assert read_retained_optional_fixed_reference(first) == [111, 112]
	assert read_retained_optional_fixed_reference(last) == [121, 122]
}

fn retain_result_fixed_reference(values &[]int) !&[]int {
	return values
}

@[noinline]
fn optional_range_reference_from_local_fixed(present bool) ?&[]int {
	if !present {
		return retain_optional_fixed_reference(none)
	}
	values := [131, 132, 133]!
	return retain_optional_fixed_reference(?&[]int(retain_immutable_array_reference(values[1..])))
}

@[noinline]
fn result_range_reference_from_local_fixed(present bool) !&[]int {
	if !present {
		return error('missing range')
	}
	values := [141, 142, 143]!
	return retain_result_fixed_reference(values[1..])
}

@[noinline]
fn direct_range_reference_from_local_fixed() &[]int {
	values := [151, 152, 153]!
	return retain_immutable_array_reference(values[1..])
}

fn test_wrapped_fixed_array_ranges_keep_their_reference_headers_alive() {
	assert optional_range_reference_from_local_fixed(false) == none
	if unexpected := result_range_reference_from_local_fixed(false) {
		_ = unexpected
		assert false, 'expected the failed range result'
	} else {
		assert err.msg() == 'missing range'
	}
	optional := optional_range_reference_from_local_fixed(true) or { panic('missing optional range') }
	result := result_range_reference_from_local_fixed(true) or { panic(err) }
	direct := direct_range_reference_from_local_fixed()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(optional) == [132, 133]
	assert read_retained_optional_fixed_reference(result) == [142, 143]
	assert read_retained_optional_fixed_reference(direct) == [152, 153]
}

@[noinline]
fn reference_from_direct_fixed_address(use_range bool) &[]int {
	values := [161, 162, 163]!
	unsafe {
		if use_range {
			return retain_immutable_array_reference((*&values)[1..])
		}
		return retain_immutable_array_reference(*(&values))
	}
}

@[noinline]
fn reference_from_direct_holder_address() &[]int {
	holder := FixedHolder{[171, 172]!}
	return unsafe { retain_immutable_array_reference((*(&holder)).values[1..]) }
}

fn test_direct_address_dereferences_keep_original_fixed_roots_alive() {
	whole := reference_from_direct_fixed_address(false)
	range := reference_from_direct_fixed_address(true)
	field := reference_from_direct_holder_address()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(whole) == [161, 162, 163]
	assert read_retained_optional_fixed_reference(range) == [162, 163]
	assert read_retained_optional_fixed_reference(field) == [172]
}

type FixedRootPtr = &[3]int
type FixedRootPtrAlias = FixedRootPtr

@[noinline]
fn reference_from_cast_fixed_address(chained bool) &[]int {
	values := [181, 182, 183]!
	unsafe {
		if chained {
			return retain_immutable_array_reference((*FixedRootPtrAlias(&values))[1..])
		}
		return retain_immutable_array_reference((*FixedRootPtr(&values))[1..])
	}
}

fn test_pointer_alias_cast_addresses_keep_original_fixed_roots_alive() {
	for chained in [false, true] {
		kept := reference_from_cast_fixed_address(chained)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(kept) == [182, 183]
	}
}

fn read_immutable_fixed_pointer(values &[2]int) int {
	return values[1]
}

@[noinline]
fn reference_from_fixed_and_pointer_sibling_bindings(kind int) &[]int {
	match kind {
		0 {
			values := [201, 202]!
			return retain_immutable_array_reference(values)
		}
		else {
			source := [211, 212]!
			values := unsafe { &source }
			assert read_immutable_fixed_pointer(values) == 212
			return retain_immutable_array_reference(values)
		}
	}
}

fn test_sibling_pointer_bindings_preserve_their_checked_reference_type() {
	for kind, expected in [[201, 202], [211, 212]] {
		kept := reference_from_fixed_and_pointer_sibling_bindings(kind)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(kept) == expected
	}
}

@[noinline]
fn reference_after_copying_fixed_root_in_multi_declaration() &[]int {
	values := [221, 222]!
	kept := retain_immutable_array_reference(values)
	zero, copied := 0, values
	assert zero == 0
	assert copied == [221, 222]!
	return kept
}

fn test_multi_declaration_rhs_reads_promoted_fixed_storage_as_a_value() {
	kept := reference_after_copying_fixed_root_in_multi_declaration()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(kept) == [221, 222]
}

fn optional_fixed_guard_payload() ?[2]int {
	return [241, 242]!
}

fn result_fixed_guard_payload() ![2]int {
	return [321, 322]!
}

@[noinline]
fn reference_from_generated_fixed_guard_binding(kind int) &[]int {
	match kind {
		0 {
			if values := optional_fixed_guard_payload() {
				return retain_immutable_array_reference(values)
			}
		}
		1 {
			payloads := {
				'hit': [251, 252]!
			}
			if values := payloads['hit'] {
				return retain_immutable_array_reference(values)
			}
		}
		2 {
			payloads := [[261, 262]!]
			if values := payloads[0] {
				return retain_immutable_array_reference(values)
			}
		}
		3 {
			payloads := {
				'hit': [271, 272]!
			}
			return if values := payloads['hit'] {
				retain_immutable_array_reference(values)
			} else {
				panic('expected a guard payload')
			}
		}
		4 {
			return if values := optional_fixed_guard_payload() {
				retain_immutable_array_reference(values)
			} else {
				panic('expected an optional payload')
			}
		}
		5 {
			payloads := [[331, 332]!]
			return if values := payloads[0] {
				retain_immutable_array_reference(values)
			} else {
				panic('expected an array payload')
			}
		}
		6 {
			return if values := result_fixed_guard_payload() {
				retain_immutable_array_reference(values)
			} else {
				panic(err)
			}
		}
		else {
			channel := chan [2]int{cap: 1}
			channel <- [341, 342]!
			return if values := <-channel {
				retain_immutable_array_reference(values)
			} else {
				panic('expected a channel payload')
			}
		}
	}
	panic('expected a guard payload')
}

fn test_generated_fixed_guard_bindings_keep_retained_views_alive() {
	for kind, expected in [[241, 242], [251, 252], [261, 262], [271, 272], [241, 242], [331, 332],
		[321, 322], [341, 342]] {
		kept := reference_from_generated_fixed_guard_binding(kind)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(kept) == expected
	}
}

@[noinline]
fn retained_fixed_reference_after_lowering_literal_bindings() &[]int {
	values := [281, 282]!
	kept := retain_immutable_array_reference(values)
	read_parameter := fn (values int) int {
		p := &values
		return *p
	}
	read_capture := fn [values] () int {
		p := unsafe { &values }
		return (*p)[0]
	}
	assert read_parameter(283) == 283
	assert read_capture() == 281
	return kept
}

fn test_lifted_parameter_and_capture_bindings_isolate_outer_storage_markers() {
	kept := retained_fixed_reference_after_lowering_literal_bindings()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(kept) == [281, 282]
}

@[noinline]
fn fixed_reference_after_lowering_heap_struct_capture() &[]int {
	mut holder := FixedHolder{ values: [301, 302]! }
	kept := retain_immutable_array_reference(holder.values)
	read_capture := fn [holder] () int {
		p := &holder
		return (*p).values[0]
	}
	assert read_capture() == 301
	holder.values[0] = 303
	assert read_capture() == 301
	return kept
}

fn test_lifted_heap_struct_captures_recreate_their_own_storage_markers() {
	values := fixed_reference_after_lowering_heap_struct_capture()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(values) == [303, 302]
}

@[noinline]
fn reference_from_fixed_array_select_receive(channel chan [2]int) &[]int {
	select {
		values := <-channel {
			return retain_immutable_array_reference(values)
		}
	}
	panic('expected a receive payload')
}

fn test_select_receive_fixed_bindings_keep_retained_views_alive() {
	channel := chan [2]int{cap: 1}
	channel <- [311, 312]!
	kept := reference_from_fixed_array_select_receive(channel)
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(kept) == [311, 312]
}

@[noinline]
fn reference_from_plain_fixed_multi_declaration() &[]int {
	values, unused := [351, 352]!, 0
	assert unused == 0
	return retain_immutable_array_reference(values)
}

@[noinline]
fn reference_from_tuple_fixed_multi_declaration(use_match bool) &[]int {
	if use_match {
		values, unused := match true {
			true { [361, 362]!, 0 }
			false { [0, 0]!, 1 }
		}
		assert unused == 0
		return retain_immutable_array_reference(values)
	}
	values, unused := if true { [371, 372]!, 0 } else { [0, 0]!, 1 }
	assert unused == 0
	return retain_immutable_array_reference(values)
}

fn test_generated_multi_declaration_fixed_bindings_keep_retained_views_alive() {
	plain := reference_from_plain_fixed_multi_declaration()
	matched := reference_from_tuple_fixed_multi_declaration(true)
	conditional := reference_from_tuple_fixed_multi_declaration(false)
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(plain) == [351, 352]
	assert read_retained_optional_fixed_reference(matched) == [361, 362]
	assert read_retained_optional_fixed_reference(conditional) == [371, 372]
}

struct RetainedFixedRowIterator {
mut:
	index int
}

fn (mut iter RetainedFixedRowIterator) next() ?[2]int {
	if iter.index == 2 {
		return none
	}
	iter.index++
	return [381 + iter.index, 391 + iter.index]!
}

@[noinline]
fn references_from_indexed_fixed_row_bindings() []&[]int {
	rows := [[401, 402]!, [411, 412]!]
	mut kept := []&[]int{}
	for row in rows {
		kept << retain_immutable_array_reference(row)
	}
	return kept
}

@[noinline]
fn references_from_iterator_fixed_row_bindings() []&[]int {
	mut kept := []&[]int{}
	for row in RetainedFixedRowIterator{} {
		kept << retain_immutable_array_reference(row)
	}
	return kept
}

fn test_by_value_fixed_row_loop_bindings_keep_retained_views_alive() {
	indexed := references_from_indexed_fixed_row_bindings()
	iterator := references_from_iterator_fixed_row_bindings()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(indexed[0]) == [401, 402]
	assert read_retained_optional_fixed_reference(indexed[1]) == [411, 412]
	assert read_retained_optional_fixed_reference(iterator[0]) == [382, 392]
	assert read_retained_optional_fixed_reference(iterator[1]) == [383, 393]
}

fn retain_fixed_reference_pair(left &[]int, right &[]int) (&[]int, &[]int) {
	return left, right
}

fn temporary_fixed_array_pair() ([2]int, [2]int) {
	return [421, 422]!, [431, 432]!
}

@[noinline]
fn references_from_expanded_temporary_fixed_pair() (&[]int, &[]int) {
	return retain_fixed_reference_pair(temporary_fixed_array_pair())
}

fn test_expanded_multi_return_fixed_arguments_keep_their_backing_alive() {
	left, right := references_from_expanded_temporary_fixed_pair()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(left) == [421, 422]
	assert read_retained_optional_fixed_reference(right) == [431, 432]
}

@[noinline]
fn references_from_map_fixed_row_bindings() []&[]int {
	rows := {
		1: [441, 442]!
		2: [451, 452]!
	}
	mut kept := []&[]int{}
	for _, row in rows {
		kept << retain_immutable_array_reference(row)
	}
	return kept
}

fn test_map_fixed_row_loop_bindings_keep_retained_views_alive() {
	kept := references_from_map_fixed_row_bindings()
	assert overwrite_stack() == 7
	assert kept.len == 2
	assert read_retained_optional_fixed_reference(kept[0]) == [441, 442]
	assert read_retained_optional_fixed_reference(kept[1]) == [451, 452]
}

@[noinline]
fn reference_after_shared_mutable_fixed_capture() &[]int {
	mut values := [461, 462]!
	kept := retain_immutable_array_reference(values)
	mut change_capture := fn [mut values] () int {
		values[0]++
		return values[0]
	}
	ptr := unsafe { &values }
	read_pointer_capture := fn [ptr] () int {
		return (*ptr)[0]
	}
	values[0] = 471
	assert change_capture() == 472
	assert change_capture() == 473
	assert values[0] == 473
	assert read_pointer_capture() == 473
	return kept
}

fn test_mutable_fixed_value_captures_share_original_storage() {
	kept := reference_after_shared_mutable_fixed_capture()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(kept) == [473, 462]
}

fn temporary_fixed_rows() [2][2]int {
	return [[501, 502]!, [511, 512]!]!
}

@[noinline]
fn references_from_mutable_fixed_backing(use_temporary bool) []&[]int {
	mut kept := []&[]int{}
	if use_temporary {
		for mut row in temporary_fixed_rows() {
			kept << retain_immutable_array_reference(row)
			row[0]++
		}
	} else {
		mut rows := [[481, 482]!, [491, 492]!]!
		for mut row in rows {
			kept << retain_immutable_array_reference(row)
			row[0]++
		}
		assert rows[0][0] == 482
		assert rows[1][0] == 492
	}
	return kept
}

fn test_mutable_fixed_loop_views_keep_original_or_temporary_backing_alive() {
	for use_temporary, expected in [[482, 482], [502, 502]] {
		kept := references_from_mutable_fixed_backing(use_temporary == 1)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(kept[0]) == expected
		assert read_retained_optional_fixed_reference(kept[1]) == [expected[0] + 10, expected[1] + 10]
	}
}

@[noinline]
fn references_from_dynamic_fixed_elements(slice_alias bool) []&[]int {
	mut rows := [[601, 602]!, [611, 612]!]
	mut kept := []&[]int{}
	if slice_alias {
		mut shifted := rows[1..]
		kept << retain_immutable_array_reference(shifted[0])
		shifted[0][0]++
		assert rows[1][0] == 612
	} else {
		kept << retain_immutable_array_reference(rows[0])
		kept << retain_immutable_array_reference(rows[0])
		rows[0][0]++
		assert read_retained_optional_fixed_reference(kept[0]) == [602, 602]
		assert read_retained_optional_fixed_reference(kept[1]) == [602, 602]
	}
	return kept
}

fn test_dynamic_fixed_element_references_share_retained_owner_buffer() {
	for slice_alias in [false, true] {
		kept := references_from_dynamic_fixed_elements(slice_alias)
		assert overwrite_stack() == 7
		for reference in kept {
			assert read_retained_optional_fixed_reference(reference) == if slice_alias {
				[612, 612]
			} else {
				[602, 602]
			}
		}
	}
}

struct FixedRowProvider {
	seed int
}

@[noinline]
fn (provider FixedRowProvider) [] (index int) [2]int {
	return [provider.seed + index, provider.seed + index + 1]!
}

@[noinline]
fn reference_from_overloaded_fixed_result() &[]int {
	provider := FixedRowProvider{701}
	return retain_immutable_array_reference(provider[2])
}

fn test_overloaded_fixed_index_result_has_owning_backing() {
	kept := reference_from_overloaded_fixed_result()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(kept) == [703, 704]
}

struct FixedRowReferenceProvider {
	row &[2]int
}

fn (provider FixedRowReferenceProvider) [] (index int) &[2]int {
	assert index == 0
	return provider.row
}

fn test_overloaded_fixed_index_pointer_result_preserves_storage_identity() {
	mut row := [711, 712]!
	provider := FixedRowReferenceProvider{unsafe { &row }}
	kept := retain_immutable_array_reference(provider[0])
	row[0] = 713
	assert read_retained_optional_fixed_reference(kept) == [713, 712]
}

struct FixedPointerFieldHolder {
	p &[3]int
}

struct FixedPointerFieldStore {
mut:
	data FixedIntSliceRef = unsafe { nil }
}

fn (mut store FixedPointerFieldStore) retain(values &[]int) {
	store.data = values
}

@[noinline]
fn store_from_fixed_pointer_aggregate(mut store FixedPointerFieldStore, kind int) {
	mut values := [721, 722, 723]!
	holder := FixedPointerFieldHolder{ p: unsafe { &values } }
	unsafe {
		if kind == 0 {
			store.retain((*holder.p)[1..])
		} else if kind == 1 {
			holders := [holder]
			store.retain((*holders[0].p)[1..])
		} else {
			pointers := [&values]
			store.retain((*pointers[0])[1..])
		}
	}
	values[1] = 724
	assert read_retained_optional_fixed_reference(store.data) == [724, 723]
}

fn test_stored_fixed_views_follow_aggregate_pointer_sources() {
	for kind in 0 .. 3 {
		mut store := FixedPointerFieldStore{}
		store_from_fixed_pointer_aggregate(mut store, kind)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(store.data) == [724, 723]
	}
}

fn temporary_dynamic_fixed_rows() [][2]int {
	return [[731, 732]!, [741, 742]!]
}

@[noinline]
fn references_from_mutable_dynamic_fixed_rows(kind int) []&[]int {
	mut kept := []&[]int{}
	if kind == 0 {
		for mut row in temporary_dynamic_fixed_rows() {
			kept << retain_immutable_array_reference(row)
			row[0]++
		}
	} else if kind == 3 {
		mut rows := [FixedHolder{[731, 732]!}, FixedHolder{[741, 742]!}]
		for mut row in rows {
			kept << retain_immutable_array_reference(row.values)
			row.values[0]++
		}
		assert rows[0].values[0] == 732
		assert rows[1].values[0] == 742
	} else {
		mut rows := temporary_dynamic_fixed_rows()
		mut view := if kind == 1 { rows[..] } else { rows }
		for mut row in view {
			kept << retain_immutable_array_reference(row)
			row[0]++
		}
		assert rows[0][0] == 732
		assert rows[1][0] == 742
	}
	return kept
}

fn test_mutable_dynamic_fixed_rows_retain_their_original_buffer() {
	for kind in 0 .. 4 {
		kept := references_from_mutable_dynamic_fixed_rows(kind)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(kept[0]) == [732, 732]
		assert read_retained_optional_fixed_reference(kept[1]) == [742, 742]
	}
}

struct FixedProjectionTrace {
mut:
	calls int
}

fn fixed_map_key(mut trace FixedProjectionTrace, missing bool) string {
	trace.calls++
	return if missing { 'missing' } else { 'row' }
}

@[noinline]
fn reference_from_map_fixed_value(missing bool, field bool) &[]int {
	mut trace := FixedProjectionTrace{}
	if field {
		mut rows := map[string]FixedHolder{}
		rows['row'] = FixedHolder{[751, 752]!}
		kept := retain_immutable_array_reference(rows[fixed_map_key(mut trace, missing)].values)
		assert trace.calls == 1
		rows['row'].values[0] = 753
		unsafe { rows.free() }
		return kept
	}
	mut rows := map[string][2]int{}
	rows['row'] = [751, 752]!
	kept := retain_immutable_array_reference(rows[fixed_map_key(mut trace, missing)])
	assert trace.calls == 1
	rows['row'] = [753, 754]!
	unsafe { rows.free() }
	return kept
}

fn test_map_fixed_values_and_inline_fields_have_independent_backing() {
	for field in [false, true] {
		for missing in [false, true] {
			kept := reference_from_map_fixed_value(missing, field)
			assert overwrite_stack() == 7
			assert read_retained_optional_fixed_reference(kept) == if missing {
				[0, 0]
			} else {
				[751, 752]
			}
		}
	}
}

fn test_map_fixed_pointer_values_keep_the_original_storage_identity() {
	mut row := [761, 762]!
	mut rows := map[string]&[2]int{}
	rows['row'] = unsafe { &row }
	kept := retain_immutable_array_reference(rows['row'])
	row[0] = 763
	unsafe { rows.free() }
	assert read_retained_optional_fixed_reference(kept) == [763, 762]
}

@[noinline]
fn references_from_first_last_fixed_rows(slice_alias bool) []&[]int {
	mut rows := [[771, 772]!, [781, 782]!, [791, 792]!]
	mut view := if slice_alias { rows[1..] } else { rows }
	kept := [retain_immutable_array_reference(view.first()),
		retain_immutable_array_reference(view.last())]
	rows[if slice_alias { 1 } else { 0 }][0]++
	rows[2][0]++
	unsafe { rows.free() }
	return kept
}

fn test_first_last_fixed_rows_share_the_retained_receiver_buffer() {
	for slice_alias in [false, true] {
		kept := references_from_first_last_fixed_rows(slice_alias)
		assert overwrite_stack() == 7
		assert read_retained_optional_fixed_reference(kept[0]) == if slice_alias {
			[782, 782]
		} else {
			[772, 772]
		}
		assert read_retained_optional_fixed_reference(kept[1]) == [792, 792]
	}
}

fn fixed_accessor_receiver_index(mut trace FixedProjectionTrace) int {
	trace.calls++
	return 0
}

@[noinline]
fn references_from_nested_fixed_accessors() []&[]int {
	mut trace := FixedProjectionTrace{}
	mut groups := [[FixedHolder{[801, 802]!}, FixedHolder{[811, 812]!}]]
	kept := [
		retain_immutable_array_reference(groups[fixed_accessor_receiver_index(mut trace)].first().values),
		retain_immutable_array_reference(groups[fixed_accessor_receiver_index(mut trace)].last().values),
	]
	assert trace.calls == 2
	groups[0][0].values[0]++
	groups[0][1].values[0]++
	unsafe {
		groups[0].free()
		groups.free()
	}
	return kept
}

fn test_nested_fixed_accessor_fields_evaluate_and_retain_the_receiver_once() {
	kept := references_from_nested_fixed_accessors()
	assert overwrite_stack() == 7
	assert read_retained_optional_fixed_reference(kept[0]) == [802, 802]
	assert read_retained_optional_fixed_reference(kept[1]) == [812, 812]
}

struct FixedAccessorReferences {
	fixed &FixedHolder
mut:
	rows [][2]int
}

fn test_fixed_accessor_pointer_and_dynamic_fields_keep_storage_identity() {
	mut fixed := FixedHolder{[821, 822]!}
	mut holders := [FixedAccessorReferences{&fixed, [[831, 832]!]}]
	pointer_view := retain_immutable_array_reference(holders.first().fixed.values)
	dynamic_view := retain_immutable_array_reference(holders.last().rows[0])
	fixed.values[0]++
	holders[0].rows[0][0]++
	unsafe {
		holders[0].rows.free()
		holders.free()
	}
	assert read_retained_optional_fixed_reference(pointer_view) == [822, 822]
	assert read_retained_optional_fixed_reference(dynamic_view) == [832, 832]
}
