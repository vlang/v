module types

import v.flat

fn test_generic_body_combination_ids_preserve_parameter_order() {
	names := ['T', 'U']
	first := {
		'T': flat.source_type_int
		'U': flat.source_type_string
	}
	reordered := {
		'U': flat.source_type_string
		'T': flat.source_type_int
	}
	swapped := {
		'T': flat.source_type_string
		'U': flat.source_type_int
	}
	assert type_param_combination_key(names, first) == type_param_combination_key(names, reordered)
	assert type_param_combination_seen(names, reordered, [first], [0])
	assert !type_param_combination_seen(names, swapped, [first], [0])
}

fn test_generic_body_combination_buckets_verify_all_spelling_ids() {
	names := ['T', 'U']
	first := {
		'T': flat.source_type_int
		'U': flat.source_type_string
	}
	second := {
		'T': flat.source_type_int
		'U': flat.source_type_bool
	}
	absent := {
		'T': flat.source_type_f64
		'U': flat.source_type_bool
	}
	combinations := [first, second]
	// Force several candidates into one bucket, independent of their fingerprints.
	assert type_param_combination_seen(names, first, combinations, [0, 1])
	assert type_param_combination_seen(names, second, combinations, [0, 1])
	assert !type_param_combination_seen(names, absent, combinations, [0, 1])
	assert !type_param_combination_seen(names, first, combinations, []int{})
}
