// Iterating array constants from generic bodies used to crash the compiler:
// scoped monomorphization workers memoized the constants' storage class in map
// buckets shared with their parent, using keys owned by their scratch arenas.
// The nested specializations below run in a later batch and read those keys.
// Non-generic code here must not iterate the fixed-storage constants: that
// would classify them on the parent before any batch worker runs.

// Only iterated, indexed or measured: these use fixed-array storage.
const generic_iter_names = ['alpha', 'beta']
const generic_iter_weights = [3, 5, 7]
// Also used through dynamic array methods: this keeps dynamic storage.
const generic_iter_dynamic = [10, 20, 30]

struct GenericIterPair {
	first  int
	second u8
}

struct GenericIterWide {
	small  i8
	medium u16
	large  i64
}

fn generic_iter_collect[T](value T) int {
	mut total := 0
	for name in generic_iter_names {
		total += name.len
	}
	for i, weight in generic_iter_weights {
		total += i * weight
	}
	return total + int(value) + generic_iter_weights[generic_iter_weights.len - 1]
}

fn generic_iter_fields[T](value T) int {
	mut total := 0
	$for field in T.fields {
		for name in generic_iter_names {
			total += name.len
		}
		total += generic_iter_collect(value.$(field.name))
	}
	return total
}

fn generic_iter_snapshot[T](prefix T) []string {
	mut out := []string{cap: generic_iter_names.len}
	for name in generic_iter_names {
		out << '${prefix}${name}'
	}
	return out
}

fn generic_iter_weight_sum[T](scale T) T {
	mut total := T(0)
	for weight in generic_iter_weights {
		total += T(weight) * scale
	}
	return total
}

fn generic_iter_dynamic_sum[T](scale T) T {
	mut total := T(0)
	for value in generic_iter_dynamic {
		total += T(value) * scale
	}
	return total
}

fn generic_iter_dynamic_doubled[T](scale T) []int {
	return generic_iter_dynamic.map(it * 2 * int(scale))
}

fn test_generic_specializations_iterate_fixed_const_arrays() {
	// 9 name bytes + (0*3 + 1*5 + 2*7) + last weight 7 = 35, plus the value.
	assert generic_iter_fields(GenericIterPair{1, 2}) == (9 + 36) + (9 + 37)
	assert generic_iter_fields(GenericIterWide{-1, 4, 100}) == (9 + 34) + (9 + 39) + (9 + 135)
	// Repeating a specialization reuses the emitted body.
	assert generic_iter_fields(GenericIterPair{3, 4}) == (9 + 38) + (9 + 39)
}

fn test_generic_specializations_keep_dynamic_const_arrays() {
	assert generic_iter_dynamic_sum(2) == 120
	assert generic_iter_dynamic_sum(f64(0.5)) == 30.0
	assert generic_iter_dynamic_sum(u8(1)) == u8(60)
	assert generic_iter_dynamic_doubled(1) == [20, 40, 60]
}

fn test_const_arrays_keep_their_values() {
	assert generic_iter_snapshot('') == ['alpha', 'beta']
	assert generic_iter_snapshot(1) == ['1alpha', '1beta']
	assert generic_iter_weight_sum(1) == 15
	assert generic_iter_weight_sum(f32(0.5)) == f32(7.5)
}
