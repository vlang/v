fn nested_map_get[K, V](m &map[K]V, k K, zero V) V {
	return (*m)[k] or { zero }
}

fn nested_map_nil_ref[T]() &T {
	return unsafe { nil }
}

fn nested_map_set[K, V](m &map[K]V, k K, v V) {
	unsafe { (*m)[k] = v }
}

fn test_nested_generic_map_reference_result_with_nil_default() {
	outer := &{
		'x': &{
			'y': 5
		}
	}
	nested_map_set(nested_map_get(outer, 'x', nested_map_nil_ref[map[string]int]()), 'z', 6)
	inner := (*outer)['x'] or { panic('missing inner map') }
	assert inner.len == 2
	assert (*inner)['z'] == 6
}

fn test_nested_generic_map_reference_result_with_non_nil_default() {
	outer := &{
		'x': &{
			'y': 5
		}
	}
	fallback := &map[string]int{}
	nested_map_set(nested_map_get(outer, 'x', fallback), 'z', 7)
	inner := (*outer)['x'] or { panic('missing inner map') }
	assert inner.len == 2
	assert (*inner)['z'] == 7
	assert fallback.len == 0
}

fn test_nested_generic_map_reference_result_with_missing_key() {
	outer := &map[string]&map[string]int{}
	fallback := &map[string]int{}
	nested_map_set(nested_map_get(outer, 'missing', fallback), 'z', 8)
	assert (*fallback)['z'] == 8
	assert outer.len == 0
}

fn test_nested_generic_map_reference_with_explicit_type_arguments() {
	outer := &{
		'x': &{
			'y': 5
		}
	}
	nested_map_set(nested_map_get[string, &map[string]int](outer, 'x',
		nested_map_nil_ref[map[string]int]()), 'z', 9)
	inner := (*outer)['x'] or { panic('missing inner map') }
	assert inner.len == 2
	assert (*inner)['z'] == 9
}

fn test_parenthesized_nested_generic_map_reference_result() {
	outer := &{
		'x': &{
			'y': 5
		}
	}
	nested_map_set((nested_map_get(outer, 'x', nested_map_nil_ref[map[string]int]())), 'z', 10)
	inner := (*outer)['x'] or { panic('missing inner map') }
	assert inner.len == 2
	assert (*inner)['z'] == 10
}

fn nested_map_identity[T](value T) T {
	return value
}

fn nested_map_borrow[T](value &T) T {
	return *value
}

fn test_nested_generic_value_result_still_borrows() {
	assert nested_map_borrow(nested_map_identity(42)) == 42
}
