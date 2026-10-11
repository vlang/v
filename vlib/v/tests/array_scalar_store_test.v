type ScalarStoreNumber = i64

enum ScalarStoreEnum {
	first
	second
}

struct ScalarStoreCalls {
mut:
	base  int
	index int
	value int
}

fn scalar_store_base(mut values []i64, mut calls ScalarStoreCalls) &[]i64 {
	calls.base++
	return &values
}

fn scalar_store_index(mut calls ScalarStoreCalls) int {
	calls.index++
	return 0
}

fn scalar_store_grow(mut values []i64, mut calls ScalarStoreCalls) i64 {
	calls.value++
	for i in 0 .. 10000 {
		values << i64(i)
	}
	return 42
}

fn test_scalar_store_evaluates_once_and_resolves_data_after_rhs_growth() {
	mut values := []i64{cap: 1}
	values << 1
	mut calls := ScalarStoreCalls{}
	(*scalar_store_base(mut values, mut calls))[scalar_store_index(mut calls)] = scalar_store_grow(mut values,
		mut calls)
	assert calls == ScalarStoreCalls{1, 1, 1}
	assert values.len == 10001
	assert values[0] == 42
}

fn test_scalar_store_checks_bounds_after_rhs_changes_length() {
	mut values := []i64{}
	mut calls := ScalarStoreCalls{}
	values[scalar_store_index(mut calls)] = scalar_store_grow(mut values, mut calls)
	assert calls.index == 1
	assert calls.value == 1
	assert values.len == 10000
	assert values[0] == 42
}

fn test_scalar_append_evaluates_once_after_rhs_growth() {
	mut values := []i64{cap: 1}
	values << 1
	mut calls := ScalarStoreCalls{}
	(*scalar_store_base(mut values, mut calls)) << scalar_store_grow(mut values, mut calls)
	assert calls.base == 1
	assert calls.value == 1
	assert values.len == 10002
	assert values.last() == 42
}

fn test_scalar_append_handles_empty_growth_and_spare_capacity() {
	mut values := []i64{}
	for i in 0 .. 10000 {
		values << i64(i)
	}
	assert values.len == 10000
	for i, value in values {
		assert value == i64(i)
	}
}

fn test_scalar_append_detaches_slice_with_spare_capacity() {
	mut original := [i64(10), 20, 30, 40]
	mut view := unsafe { original[1..4] }
	// Shorten the slice header to exercise append while it has spare capacity.
	unsafe {
		view.len = 1
	}
	assert view.cap > view.len
	assert view.flags.has(.is_slice)
	view << 99
	assert view == [i64(20), 99]
	assert original == [i64(10), 20, 30, 40]
	assert !view.flags.has(.is_slice)
}

fn test_scalar_append_with_non_slice_flags_preserves_existing_views() {
	mut original := []i64{len: 2, cap: 4}
	original[0] = 10
	original[1] = 20
	mut view := unsafe { original[..] }
	unsafe {
		original.flags.set(.noslices | .nogrow)
	}
	original << 30
	assert original == [i64(10), 20, 30]
	assert view == [i64(10), 20]
	view[0] = 42
	assert original[0] == 42
}

fn test_scalar_store_and_append_cover_numeric_alias_bool_float_and_enum() {
	mut aliases := []ScalarStoreNumber{cap: 4}
	aliases << ScalarStoreNumber(3)
	aliases[0] = ScalarStoreNumber(42)
	assert aliases[0] == ScalarStoreNumber(42)
	mut booleans := []bool{cap: 4}
	booleans << false
	booleans[0] = true
	assert booleans[0]
	mut floats := []f64{cap: 4}
	floats << 1.25
	floats[0] = 2.5
	assert floats[0] == 2.5
	mut enums := []ScalarStoreEnum{cap: 4}
	enums << .first
	enums[0] = .second
	assert enums[0] == .second
	mut wide := []u128{cap: 4}
	wide << u128(3)
	wide[0] = u128(42)
	assert wide[0] == u128(42)
}

fn test_shared_scalar_array_keeps_its_locked_operations() {
	shared values := []i64{len: 1, cap: 4}
	lock values {
		values[0] = 42
		values << 43
	}
	rlock values {
		assert values == [i64(42), 43]
	}
}
