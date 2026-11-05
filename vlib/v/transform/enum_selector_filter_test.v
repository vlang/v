module transform

import v.flat
import v.types

fn enum_filter_selector(mut a flat.FlatAst, base flat.NodeId, name string) flat.Node {
	start := a.children.len
	a.children << base
	return flat.Node{ kind: .selector, value: name, children_start: start, children_count: 1 }
}

fn test_qualified_enum_selector_probe_does_not_intern_missing_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	missing := '__numeric_registry_missing_selector_boundary__.NoSuchEnum'
	assert flat.source_type_existing_id(missing) == none
	assert t.qualified_enum_type_selector_name_from_selector_name('__numeric_registry_missing_selector_boundary__',
		'NoSuchEnum') == none
	assert flat.source_type_existing_id(missing) == none
	known_id := flat.source_type_id('enum_probe_module.Known')
	t.enum_types[known_id] = ['ready']
	resolved_id := t.qualified_enum_type_selector_name_from_selector_name('enum_probe_module', 'Known') or {
		panic('registered enum selector was not resolved')
	}
	assert resolved_id == known_id
}

fn test_enum_selector_filter_keeps_alias_module_generated_and_regular_value_resolution() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('Alias'), flat.source_type_id('model.Kind'))
	tc.type_aliases.set_id(flat.source_type_id('NextAlias'), flat.source_type_id('Alias'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'model'
	t.enum_types[flat.source_type_id('model.Kind')] = ['ready', 'waiting']
	t.enum_types[flat.source_type_id('Kind')] = ['ready', 'waiting']
	t.enum_types[flat.source_type_id('model.generated')] = ['module', '@escaped', 'éclair']
	t.enum_types[flat.source_type_id('generated')] = ['module', '@escaped', 'éclair']
	mut nodes := []flat.Node{}
	for base in ['Kind', 'Alias', 'NextAlias', 'model.Kind', 'generated'] {
		base_id := a.add_val(.ident, base)
		for field in ['ready', 'waiting', 'module', '@escaped', 'éclair', 'mXdule', 'missing'] {
			nodes << enum_filter_selector(mut a, base_id, field)
		}
	}
	module_id := a.add_val(.ident, 'model')
	nodes << enum_filter_selector(mut a, module_id, 'Kind')
	nodes << enum_filter_selector(mut a, module_id, 'generated')
	mut before := []u32{}
	for node in nodes { before << t.resolve_selector_type_uncached(node) }
	assert before[0] == flat.source_type_id('Kind')
	assert before[7] == flat.source_type_id('model.Kind')
	assert before[before.len - 2] == flat.source_type_id('model.Kind')
	assert before.last() == flat.source_type_id('model.generated')
	t.prepare_enum_selector_filter()
	for i, node in nodes {
		assert t.resolve_selector_type_uncached(node) == before[i]
	}
	assert file_import_alias_bit('module') == file_import_alias_bit('mXdule')
	assert t.enum_selector_may_match('mXdule')
	assert !t.enum_selector_may_match('absent_selector_field')
}

fn test_enum_selector_filter_retains_non_enum_selector_results() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	tc.const_types['Config.limit'] = types.Type(types.int_)
	t.structs[flat.source_type_id('Record')] = StructInfo{ fields: [FieldInfo{ name: 'amount', typ: flat.source_type_string }] }
	t.set_var_type('record', flat.source_type_id('Record'))
	t.set_var_type('items', flat.source_type_id('[]int'))
	base := a.add_val(.ident, 'items')
	len_node := enum_filter_selector(mut a, base, 'len')
	assert t.resolve_selector_type_uncached(len_node) == flat.source_type_int
	record_id := a.add_val(.ident, 'record')
	field_node := enum_filter_selector(mut a, record_id, 'amount')
	config_id := a.add_val(.ident, 'Config')
	const_node := enum_filter_selector(mut a, config_id, 'limit')
	assert t.resolve_selector_type_uncached(field_node) == flat.source_type_string
	assert t.resolve_selector_type_uncached(const_node) == flat.source_type_int
	t.prepare_enum_selector_filter()
	assert !t.enum_selector_may_match('len')
	assert t.resolve_selector_type_uncached(len_node) == flat.source_type_int
	assert t.resolve_selector_type_uncached(field_node) == flat.source_type_string
	assert t.resolve_selector_type_uncached(const_node) == flat.source_type_int
}

fn test_enum_selector_filter_rechecks_mutable_query_bytes_and_shares_only_frozen_bits() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.enum_types[flat.source_type_id('Kind')] = ['open', 'shut']
	t.prepare_enum_selector_filter()
	worker := t.fork_program_view(&a, &tc, map[string]bool{}, false)
	scan := t.fork_scan_worker(&tc)
	assert voidptr(worker.enum_selector_filter) == voidptr(t.enum_selector_filter)
	assert voidptr(scan.enum_selector_filter) == voidptr(t.enum_selector_filter)
	mut bytes := 'open'.bytes()
	// Queries may refer to a reused scratch buffer. The filter records only bits.
	name := unsafe { tos(bytes.data, bytes.len) }
	for instance in [&t, &worker, scan] {
		assert instance.enum_selector_may_match(name)
	}
	for i, ch in 'shut'.bytes() { bytes[i] = ch }
	for instance in [&t, &worker, scan] {
		assert instance.enum_selector_may_match(name)
	}
	for i, ch in 'zzzz'.bytes() { bytes[i] = ch }
	for instance in [&t, &worker, scan] {
		assert !instance.enum_selector_may_match(name)
	}
}

fn test_enum_selector_filter_falls_back_for_new_enums_and_rebuilds_without_stale_bits() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.enum_selector_may_match('later')
	t.prepare_enum_selector_filter()
	assert !t.enum_selector_may_match('later')
	t.enum_types[flat.source_type_id('Added')] = ['later']
	assert t.enum_selector_may_match('later')
	base := a.add_val(.ident, 'Added')
	node := enum_filter_selector(mut a, base, 'later')
	assert t.resolve_selector_type_uncached(node) == flat.source_type_id('Added')
	t.prepare_enum_selector_filter()
	assert t.enum_selector_may_match('later')
	t.enum_types = map[u32][]string{}
	t.prepare_enum_selector_filter()
	assert !t.enum_selector_may_match('later')
	assert !t.enum_selector_may_match('Added')
}

fn test_enum_selector_filter_preserves_dotted_selector_values() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.enum_types[flat.source_type_id('model.nested.Kind')] = ['ready', 'state.value']
	module_id := a.add_val(.ident, 'model')
	enum_id := a.add_val(.ident, 'model.nested.Kind')
	nodes := [enum_filter_selector(mut a, module_id, 'nested.Kind'),
		enum_filter_selector(mut a, enum_id, 'state.value')]
	for node in nodes {
		assert t.resolve_selector_type_uncached(node) == flat.source_type_id('model.nested.Kind')
	}
	t.prepare_enum_selector_filter()
	for node in nodes {
		assert t.resolve_selector_type_uncached(node) == flat.source_type_id('model.nested.Kind')
	}
}

fn test_enum_selector_base_filter_skips_only_bases_that_cannot_name_an_enum() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('Shade'), flat.source_type_id('paint.Tone'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'paint'
	t.enum_types[flat.source_type_id('paint.Tone')] = ['dark', 'light']
	mut nodes := []flat.Node{}
	for base in ['Tone', 'paint.Tone', 'Shade', '__eq_lhs_basefilter_1', 'tone_value'] {
		base_id := a.add_val(.ident, base)
		nodes << enum_filter_selector(mut a, base_id, 'dark')
	}
	mut before := []u32{}
	for node in nodes { before << t.resolve_selector_type_uncached(node) }
	assert before[0] == flat.source_type_id('paint.Tone')
	assert before[1] == flat.source_type_id('paint.Tone')
	assert before[2] == flat.source_type_id('paint.Tone')
	t.prepare_enum_selector_filter()
	assert t.enum_selector_base_may_match('Tone')
	assert t.enum_selector_base_may_match('other.Shade')
	assert !t.enum_selector_base_may_match('__eq_lhs_basefilter_2')
	// Spellings outside plain dotted identifiers keep the full resolution.
	assert t.enum_selector_base_may_match('Box[int]')
	missing := '__eq_lhs_basefilter_3'
	assert t.enum_type_name_from_selector_name(missing) == none
	assert flat.source_type_existing_id(missing) == none
	for i, node in nodes {
		assert t.resolve_selector_type_uncached(node) == before[i]
	}
	// A type alias registered after the filter was built disables it.
	tc.type_aliases.set_id(flat.source_type_id('Hue'), flat.source_type_id('paint.Tone'))
	assert t.enum_selector_base_may_match('Hue')
	assert t.enum_type_name_from_selector_name('Hue') or { 0 } == flat.source_type_id('paint.Tone')
}
