module fastc

fn test_embedded_payloads_do_not_crowd_function_bodies_into_one_worker() {
	payload := 'x'.repeat(80000)
	mut pieces := [payload]
	for _ in 0 .. 16 {
		pieces << 'f'.repeat(1000)
	}
	units := FastcUnitLayout{
		head_end:       1
		solo_end:       1
		unit_starts:    []int{len: 17, init: index + 1}
		extern_indexes: [0]
		define_texts:   [payload]
		extern_texts:   ['extern const u8 __vf_embed_resource[];\n']
	}
	plan := fastc_c_unit_plan('program', pieces, units, 4)
	assert plan.paths.len == 4
	assert plan.first_units[3] < 12
	// Rendering still allocates for the complete payload, regardless of its weight.
	assert plan.sizes[0] >= payload.len
	for i in 0 .. plan.paths.len {
		end := if i + 1 < plan.paths.len { plan.first_units[i + 1] } else { 16 }
		assert end > plan.first_units[i]
	}
}

fn test_constant_macro_dependencies_have_prototypes_in_every_unit() {
	pieces := ['#define VALUE helper()\n', 'int helper(void);\n', 'int first(void) { return VALUE; }\n',
		'int second(void) { return VALUE; }\n']
	units := FastcUnitLayout{
		head_end:             2
		solo_end:             2
		unit_starts:          [2, 3, 4]
		prototype_start:      1
		prototype_end:        2
		prototype_texts:      ['int helper(void);\n']
		unit_ref_starts:      [0, 0, 0]
		shared_prototype_ids: [0]
	}
	assert fastc_render_c_unit(pieces, &units, 1, 1, 2).contains('int helper(void);')
}
