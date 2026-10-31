module parser

import v.flat
import v.pref

fn test_anonymous_struct_shape_cache_checks_numeric_types_and_names_after_hash_collision() {
	mut cache := AnonymousStructShapeCache{}
	first := flat.source_type_id('AnonCacheFirst')
	second := flat.source_type_id('AnonCacheSecond')
	third := flat.source_type_id('AnonCacheThird')
	cache.append_typed(['item'], [flat.source_type_int], first)
	cache.append_typed(['item'], [flat.source_type_string], second)
	cache.append_typed(['other'], [flat.source_type_int], third)
	// Put distinct shapes ahead of the matching entry in the same bucket.
	hashed := anonymous_struct_typed_shape_hash(['item'], [flat.source_type_int])
	cache.typed_buckets[hashed] = [1, 2, 0]
	assert cache.typed_candidates(['item'], [flat.source_type_int]) or { []u32{} } == [first]
	assert cache.typed_candidates(['item'], [flat.source_type_string]) or { []u32{} } == [second]
	missing_hash := anonymous_struct_typed_shape_hash(['item'], [flat.source_type_u8])
	cache.typed_buckets[missing_hash] = [0, 1, 2]
	if _ := cache.typed_candidates(['item'], [flat.source_type_u8]) {
		assert false, 'a hash collision must not match a different numeric type'
	}
	if _ := cache.typed_candidates(['missing'], [flat.source_type_int]) {
		assert false, 'a missing shape must stay absent'
	}
	cache.append_typed(['item'], [flat.source_type_int], second)
	cache.append_typed(['item'], [flat.source_type_int], first)
	assert cache.typed_candidates(['item'], [flat.source_type_int]) or { []u32{} } == [
		first,
		second,
		first,
	]
	assert cache.typed_shapes.len == 3
}

fn test_anonymous_struct_name_shape_cache_checks_order_and_preserves_candidates() {
	mut cache := AnonymousStructShapeCache{}
	first := flat.source_type_id('AnonCacheNamesFirst')
	second := flat.source_type_id('AnonCacheNamesSecond')
	cache.append_declared(['a', 'b'], first)
	cache.append_declared(['b', 'a'], second)
	hashed := anonymous_struct_name_shape_hash(['a', 'b'])
	cache.name_buckets[hashed] = [1, 0]
	assert cache.name_candidates(['a', 'b']) or { []u32{} } == [first]
	assert cache.name_candidates(['b', 'a']) or { []u32{} } == [second]
	missing_hash := anonymous_struct_name_shape_hash(['a'])
	cache.name_buckets[missing_hash] = [0, 1]
	if _ := cache.name_candidates(['a']) {
		assert false, 'a name hash collision must not match different ordered fields'
	}
	cache.append_declared(['a', 'b'], second)
	cache.append_declared(['a', 'b'], first)
	assert cache.name_candidates(['a', 'b']) or { []u32{} } == [first, second, first]
	assert cache.name_shapes.len == 2
}

fn test_anonymous_struct_shape_cache_owns_input_arrays_and_handles_empty_shapes() {
	mut cache := AnonymousStructShapeCache{}
	candidate := flat.source_type_id('AnonCacheOwned')
	mut names := ['item']
	mut types := [flat.source_type_int]
	cache.append_typed(names, types, candidate)
	cache.append_declared(names, candidate)
	names[0] = 'changed'
	types[0] = flat.source_type_string
	assert cache.typed_candidates(['item'], [flat.source_type_int]) or { []u32{} } == [candidate]
	assert cache.name_candidates(['item']) or { []u32{} } == [candidate]
	cache.append_typed([]string{}, []u32{}, candidate)
	cache.append_declared([]string{}, candidate)
	assert cache.typed_candidates([]string{}, []u32{}) or { []u32{} } == [candidate]
	assert cache.name_candidates([]string{}) or { []u32{} } == [candidate]
	cache.append_typed(['unknown'], [u32(0)], candidate)
	assert cache.typed_candidates(['unknown'], [u32(0)]) or { []u32{} } == [candidate]
	if _ := cache.name_candidates(['unknown']) {
		assert false, 'an inferred-only typed shape must not publish a declared shape'
	}
}

fn test_anonymous_struct_typed_shape_cache_preserves_missing_and_excess_type_semantics() {
	mut cache := AnonymousStructShapeCache{}
	first := flat.source_type_id('AnonCachePadded')
	second := flat.source_type_id('AnonCachePaddedAgain')
	third := flat.source_type_id('AnonCachePaddedExtra')
	fields := ['a', 'b']
	short := [flat.source_type_int]
	padded := [flat.source_type_int, u32(0)]
	extra := [flat.source_type_int, u32(0), flat.source_type_string]
	assert anonymous_struct_typed_shape_hash(fields, short) == anonymous_struct_typed_shape_hash(fields,
		padded)
	assert anonymous_struct_typed_shape_hash(fields, short) == anonymous_struct_typed_shape_hash(fields,
		extra)
	cache.append_typed(fields, short, first)
	assert cache.typed_candidates(fields, padded) or { []u32{} } == [first]
	assert cache.typed_candidates(fields, extra) or { []u32{} } == [first]
	cache.append_typed(fields, padded, second)
	cache.append_typed(fields, extra, third)
	assert cache.typed_shapes.len == 1
	assert cache.typed_shapes[0].field_types == padded
	assert cache.typed_candidates(fields, short) or { []u32{} } == [first, second, third]
	if _ := cache.typed_candidates(fields, [flat.source_type_int, flat.source_type_string]) {
		assert false, 'a missing type must keep the empty spelling identity'
	}
	cache.append_typed([]string{}, [flat.source_type_int], first)
	assert cache.typed_candidates([]string{}, []u32{}) or { []u32{} } == [first]
	assert cache.typed_shapes[1].field_types.len == 0
}

fn anonymous_cache_integer_literal(mut p Parser) flat.Node {
	value := p.a.add_node(flat.Node{
		kind:  .int_literal
		value: '1'
	})
	field := p.a.add_node(flat.Node{
		kind:           .field_init
		value:          'item'
		children_start: p.add_child(value)
		children_count: 1
	})
	init := p.a.add_node(flat.Node{
		kind:           .struct_init
		source_value:   flat.source_type_id('struct')
		children_start: p.add_child(field)
		children_count: 1
	})
	return p.a.nodes[int(init)]
}

fn test_anonymous_struct_cache_keeps_inferred_shapes_out_of_declared_index() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('inferred.v', 'fn main() {}\n')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	init := anonymous_cache_integer_literal(mut p)
	candidate := p.anonymous_struct_type_for_literal(init) or { panic('missing inferred type') }
	assert p.anonymous_struct_types.typed_candidates(['item'], [flat.source_type_int]) or {
		[]u32{}
	} == [candidate]
	if _ := p.anonymous_struct_types.name_candidates(['item']) {
		assert false, 'an inferred literal type must not become a declared contextual candidate'
	}
	assert p.a.contextual_anon_struct_types[candidate]
	assert p.anonymous_struct_count == 1
	assert p.anonymous_struct_type_for_literal(init) or { u32(0) } == candidate
	assert p.anonymous_struct_count == 1
}

fn test_anonymous_struct_cache_reuses_single_typed_declaration() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('single.v', 'fn consume(value struct { item int }) {}
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	declared := p.anonymous_struct_types.name_candidates(['item']) or { panic('missing declaration') }
	assert declared.len == 1
	init := anonymous_cache_integer_literal(mut p)
	assert p.anonymous_struct_type_for_literal(init) or { u32(0) } == declared[0]
	assert p.anonymous_struct_count == 1
}

fn test_anonymous_struct_cache_keeps_contextual_ambiguity() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('ambiguous.v', 'fn first(value struct { item int }) {}
fn second(value struct { item i64 }) {}
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	declared := p.anonymous_struct_types.name_candidates(['item']) or { panic('missing declarations') }
	assert declared.len == 2
	int_candidates := p.anonymous_struct_types.typed_candidates(['item'], [flat.source_type_int]) or {
		panic('missing int shape')
	}
	assert int_candidates.len == 1
	init := anonymous_cache_integer_literal(mut p)
	if _ := p.anonymous_struct_type_for_literal(init) {
		assert false, 'the literal must keep both declared contextual candidates'
	}
	assert p.anonymous_struct_literal_has_contextual_candidate(init, ['item'])
	if _ := p.create_anonymous_struct_type_for_literal(init) {
		assert false, 'contextual ambiguity must not synthesize another type'
	}
	assert p.anonymous_struct_count == 2
}

fn test_anonymous_struct_cache_resets_between_files() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('first.v', 'fn first(value struct { item int }) {}
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.anonymous_struct_types.typed_shapes.len == 1
	assert p.anonymous_struct_types.name_shapes.len == 1
	p.parse_text('second.v', 'fn second() {}
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.anonymous_struct_types.typed_shapes.len == 0
	assert p.anonymous_struct_types.typed_buckets.len == 0
	assert p.anonymous_struct_types.name_shapes.len == 0
	assert p.anonymous_struct_types.name_buckets.len == 0
	assert p.anonymous_struct_count == 0
}
