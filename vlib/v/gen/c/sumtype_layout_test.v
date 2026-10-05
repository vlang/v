module c

import v.flat
import v.types

fn test_sumtype_layout_contains_inline_values() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.sum_types['Value'] = ['int', 'string', 'voidptr']
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.emit_sum_type('Value')
	expected := [
		'struct Value {',
		'\tint typ;',
		'\tunion {',
		'\t\ti64 _int;',
		'\t\tstring _string;',
		'\t\tvoid* voidptr;',
		'\t};',
		'};',
		'',
	]
	assert g.sb.str().split_into_lines() == expected
}

fn test_type_preseed_distinguishes_function_return_types() {
	mut seen := PreseedTypeSeen{}
	integer := &types.Type(types.int_)
	text := &types.Type(types.string_)
	first := types.Type(types.FnType{ return_type: integer })
	second := types.Type(types.FnType{ return_type: text })
	assert seen.insert(first)
	assert seen.insert(second)
	assert !seen.insert(first)
	assert !seen.insert(second)
}

fn test_type_preseed_reuses_storage_after_hash_collisions() {
	mut seen := PreseedTypeSeen{}
	mut slots := map[u64]int{}
	mut first := types.Type(types.void_)
	mut second := types.Type(types.void_)
	for i in 0 .. seen.indexes.len + 1 {
		candidate := types.Type(types.Struct{ name: 'Type${i}' })
		slot := types.semantic_type_hash(candidate) & u64(seen.indexes.len - 1)
		if previous := slots[slot] {
			first = types.Type(types.Struct{ name: 'Type${previous}' })
			second = candidate
			break
		}
		slots[slot] = i
	}
	assert first is types.Struct
	assert second is types.Struct
	assert seen.values.len == 0
	assert seen.insert(first)
	assert !seen.insert(first)
	assert seen.insert(second)
	assert !seen.insert(second)
	assert seen.insert(first)
	assert seen.values.len == 1
}

fn test_expected_sum_storage_resolves_nested_aliases() {
	value := &types.Type(types.SumType{ name: 'Value' })
	first := &types.Type(types.Alias{ name: 'First', base_type: value })
	second := types.Type(types.Alias{ name: 'Second', base_type: first })
	g := FlatGen.new()
	resolved := g.sum_type_for_expected_value(second) or {
		panic('nested aliases must resolve to the sum storage')
	}
	assert resolved.name == 'Value'
}
