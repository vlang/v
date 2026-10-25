module transform

import v.flat

fn test_generic_fn_spec_map_checks_full_identity_on_hash_collision() {
	first := generic_fn_spec_key('main.pick', [flat.source_type_int, flat.source_type_string])
	second := GenericFnSpecKey{ decl_key: 'main.other', args: first.args, hash: first.hash }
	third := GenericFnSpecKey{
		decl_key: first.decl_key
		args:     [
			flat.source_type_string,
			flat.source_type_int,
		]
		hash:     first.hash
	}
	mut entries := GenericFnSpecMap[int]{}
	entries.set(first, 1)
	entries.set(second, 2)
	entries.set(third, 3)
	assert entries.len == 3
	assert entries.get(first) or { 0 } == 1
	assert entries.get(second) or { 0 } == 2
	assert entries.get(third) or { 0 } == 3
	entries.set(second, 4)
	assert entries.len == 3
	assert entries.get(second) or { 0 } == 4
	entries.delete(first)
	assert !entries.contains(first)
	assert entries.contains(second)
	assert entries.contains(third)
	assert entries.len == 2
	entries.clear()
	assert entries.len == 0
	assert !entries.contains(second)
}

fn test_generic_fn_spec_identity_preserves_order_arity_and_raw_spelling() {
	mut args := [flat.source_type_int]
	key := generic_fn_spec_key('main.pick', args)
	mut entries := GenericFnSpecMap[int]{}
	entries.set(key, 7)
	args[0] = flat.source_type_string
	assert entries.get(generic_fn_spec_key('main.pick', [flat.source_type_int])) or { 0 } == 7
	assert !entries.contains(generic_fn_spec_key('main.pick', [flat.source_type_string]))
	assert !entries.contains(generic_fn_spec_key('main.pick', []u32{}))
	assert !entries.contains(generic_fn_spec_key('main.pick', [flat.source_type_id(' int ')]))
	assert !entries.contains(generic_fn_spec_key('main.pick', [flat.source_type_int,
		flat.source_type_int]))
	assert generic_fn_spec_key('main.pick', [flat.source_type_int]).legacy_text() == 'main.pick[int]'
	assert generic_fn_spec_key('main.pick', [flat.source_type_int, flat.source_type_string]).legacy_text() == 'main.pick[int, string]'
}

fn test_generic_fn_spec_cache_retains_legacy_rendered_alias_last_write() {
	joined := generic_fn_spec_key('main.pick', [flat.source_type_id('int, string')])
	separate := generic_fn_spec_key('main.pick', [flat.source_type_int, flat.source_type_string])
	assert joined.legacy_text() == separate.legacy_text()
	assert !joined.matches(separate)
	mut t := Transformer{}
	t.record_monomorph_cache_spec(joined, 'main.pick', 'first', joined.args)
	t.record_monomorph_cache_spec(separate, 'main.pick', 'second', separate.args)
	assert t.monomorph_cache_specs.len == 2
	mut specs := t.sorted_monomorph_cache_specs()
	assert specs.len == 1
	assert specs[0].module == 'second'
	t.record_monomorph_cache_spec(joined, 'main.pick', 'last', joined.args)
	specs = t.sorted_monomorph_cache_specs()
	assert specs.len == 1
	assert specs[0].module == 'last'
}

fn test_generic_fn_spec_cache_exports_legacy_lexical_order() {
	mut t := Transformer{}
	for name in ['z.Type', 'a.Type', 'm.Type'] {
		args := [flat.source_type_id(name)]
		t.record_monomorph_cache_spec(generic_fn_spec_key('main.pick', args), 'main.pick', name, args)
	}
	specs := t.sorted_monomorph_cache_specs()
	assert specs.map(it.module) == ['a.Type', 'm.Type', 'z.Type']
}
