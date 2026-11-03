module types

import v.flat

const combination_id_param_t = flat.source_type_id('T')
const combination_id_param_u = flat.source_type_id('U')

fn test_generic_body_combination_ids_preserve_parameter_order() {
	names := [combination_id_param_t, combination_id_param_u]
	first := {
		combination_id_param_t: flat.source_type_int
		combination_id_param_u: flat.source_type_string
	}
	reordered := {
		combination_id_param_u: flat.source_type_string
		combination_id_param_t: flat.source_type_int
	}
	swapped := {
		combination_id_param_t: flat.source_type_string
		combination_id_param_u: flat.source_type_int
	}
	assert type_param_combination_key(names, first) == type_param_combination_key(names, reordered)
	assert type_param_combination_seen(names, reordered, [first], [0])
	assert !type_param_combination_seen(names, swapped, [first], [0])
}

fn test_generic_body_combination_buckets_verify_all_spelling_ids() {
	names := [combination_id_param_t, combination_id_param_u]
	first := {
		combination_id_param_t: flat.source_type_int
		combination_id_param_u: flat.source_type_string
	}
	second := {
		combination_id_param_t: flat.source_type_int
		combination_id_param_u: flat.source_type_bool
	}
	absent := {
		combination_id_param_t: flat.source_type_f64
		combination_id_param_u: flat.source_type_bool
	}
	combinations := [first, second]
	// Force several candidates into one bucket, independent of their fingerprints.
	assert type_param_combination_seen(names, first, combinations, [0, 1])
	assert type_param_combination_seen(names, second, combinations, [0, 1])
	assert !type_param_combination_seen(names, absent, combinations, [0, 1])
	assert !type_param_combination_seen(names, first, combinations, []int{})
}
