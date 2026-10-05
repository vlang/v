struct StoreCalls {
mut:
	total int
}

fn grow_array_for_store(mut a []int, mut calls StoreCalls) int {
	calls.total++
	for i in 0 .. 10000 {
		a << i
	}
	return 3
}

fn store_index(mut calls StoreCalls) int {
	calls.total++
	return 0
}

@[direct_array_access]
fn direct_store_after_growing(mode int) {
	mut a := []int{cap: 1}
	a << 10
	mut calls := StoreCalls{}
	match mode {
		0 { a[store_index(mut calls)] = grow_array_for_store(mut a, mut calls) }
		1 { a[store_index(mut calls)] += grow_array_for_store(mut a, mut calls) }
		2 { a[store_index(mut calls)] **= grow_array_for_store(mut a, mut calls) }
		3 { a[store_index(mut calls)] <<= grow_array_for_store(mut a, mut calls) }
		else { a[store_index(mut calls)] ^= grow_array_for_store(mut a, mut calls) }
	}
	assert calls.total == 2
	assert a.len == 10001
	assert a[0] == [3, 13, 1000, 80, 9][mode]
}

fn unsafe_store_after_growing(mode int) {
	mut a := []int{cap: 1}
	a << 10
	mut calls := StoreCalls{}
	// Exercise the same unchecked store lowering as the attribute above.
	unsafe {
		match mode {
			0 { a[store_index(mut calls)] = grow_array_for_store(mut a, mut calls) }
			1 { a[store_index(mut calls)] += grow_array_for_store(mut a, mut calls) }
			2 { a[store_index(mut calls)] **= grow_array_for_store(mut a, mut calls) }
			3 { a[store_index(mut calls)] <<= grow_array_for_store(mut a, mut calls) }
			else { a[store_index(mut calls)] ^= grow_array_for_store(mut a, mut calls) }
		}
	}
	assert calls.total == 2
	assert a.len == 10001
	assert a[0] == [3, 13, 1000, 80, 9][mode]
}

fn grow_wide_array_for_store(mut a []u128) u128 {
	for i in 0 .. 10000 {
		a << u128(i)
	}
	return u128(3)
}

@[direct_array_access]
fn direct_wide_store_after_growing() {
	mut a := []u128{cap: 1}
	a << u128(10)
	a[0] += grow_wide_array_for_store(mut a)
	assert a[0] == u128(13)
}

fn test_direct_stores_resolve_the_element_after_rhs_reallocation() {
	for mode in 0 .. 5 {
		direct_store_after_growing(mode)
		unsafe_store_after_growing(mode)
	}
	direct_wide_store_after_growing()
}

@[direct_array_access]
fn direct_store_with_source_names(mut a []int, _a0 int, _i0 int, _v0 int, __v3_internal_symbol_array_store_value_0 int) {
	a[0] = _a0 + _i0 + _v0 + __v3_internal_symbol_array_store_value_0
}

fn test_direct_store_temporaries_do_not_shadow_source_names() {
	mut a := [1]
	direct_store_with_source_names(mut a, 1, 2, 3, 36)
	assert a[0] == 42
}
