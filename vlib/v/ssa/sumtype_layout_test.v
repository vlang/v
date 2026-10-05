module ssa

import os
import v.flat
import v.parser
import v.pref

fn sumtype_layout_builder(source string) !Builder {
	path := os.join_path(os.vtmp_dir(), 'inline_union_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0
	mut m := Module.new()
	mut b := Builder{
		a:        p.a
		m:        m
		i32_type: m.type_store.get_int(32)
		i64_type: m.type_store.get_int(64)
	}
	b.register_types()
	return b
}

fn test_sumtype_storage_overlaps_variants() {
	source := 'module main\nstruct Pair { x i64\n y i64 }\ntype Choice = int | Pair | [3]i64\n'
	b := sumtype_layout_builder(source)!
	m := b.m
	choice := b.struct_types['Choice']
	layout := m.type_store.types[choice]
	assert layout.field_names == ['typ', '_payload']
	payload := layout.fields[1]
	assert m.type_store.types[payload].is_union
	assert m.type_size(payload) == 24
	assert m.type_size(choice) == 32
	assert m.struct_field_offset(payload, 0) == 0
	assert m.struct_field_offset(payload, 1) == 0
	assert m.struct_field_offset(payload, 2) == 0
	assert m.struct_field_offset(choice, 1) == 8
}

fn test_sumtype_pointer_fields_match_lowered_storage() {
	source := 'module main
struct Pair { x i64
 y i64 }
type Choice = Pair | &Pair | &&Pair
'
	mut b := sumtype_layout_builder(source)!
	choice := b.struct_types['Choice']
	payload := b.m.type_store.types[choice].fields[1]
	expected := ['Pair', '_ptr_Pair', '_ptr__ptr_Pair']
	assert b.m.type_store.types[payload].field_names == expected
	function := b.m.add_function('projection', b.i64_type)
	b.cur_block = b.m.add_block(function, 'entry')
	slot := b.emit0(.alloca, b.m.type_store.get_ptr(choice))
	b.a.nodes << flat.Node{ kind: .ident }
	node := flat.NodeId(b.a.nodes.len - 1)
	variants := ['Pair', '&Pair', '&&Pair']
	for pointers, variant in variants {
		b.a.nodes[node].typ = variant
		start := b.m.instrs.len
		field := b.smartcast_sum_selector_addr(slot, node, 'y') or {
			panic('variant must expose its fields')
		}
		assert b.deref_type(field) == b.i64_type
		loads := b.m.instrs[start..].filter(it.op == .load)
		assert loads.len == pointers
		instruction := b.m.instrs[b.m.values[field].index]
		assert b.m.values[instruction.operands[1]].name == '8'
	}
}

fn test_sumtype_variant_selection_preserves_qualification() {
	b := Builder{
		sum_type_variants: {
			'Choice': ['left.Item', 'right.Item', '&right.Item']
		}
	}
	assert b.find_sum_variant('Choice', 'right.Item')? == 'right.Item'
	assert b.find_sum_variant('Choice', '&right.Item')? == '&right.Item'
	assert b.sum_variant_index('Choice', 'right.Item') == 2
	assert b.sum_variant_index('Choice', '&right.Item') == 3
	assert b.sum_variant_index('Choice', 'Item') == 0
	assert b.find_sum_variant('Choice', 'Item') == none
	assert b.find_sum_variant('Choice', 'missing.Item') == none
}
