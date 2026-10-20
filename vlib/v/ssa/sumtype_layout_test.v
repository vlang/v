module ssa

import os
import v.flat
import v.parser
import v.pref

fn sumtype_layout_builder(source string) !Builder {
	return sumtype_layout_builder_for_target(source, 8)
}

fn sumtype_layout_builder_for_target(source string, pointer_size int) !Builder {
	path := os.join_path(os.vtmp_dir(), 'inline_union_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0
	mut m := Module.new()
	m.target = TargetData{ ptr_size: pointer_size }
	mut b := Builder{
		a:        p.a
		m:        m
		i32_type: m.type_store.get_int(32)
		u32_type: m.type_store.get_uint(32)
		i8_type:  m.type_store.get_int(8)
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
	payload := b.m.type_store.types[choice].fields.last()
	expected := ['Pair', '_ptr_Pair', '_ptr__ptr_Pair']
	assert b.m.type_store.types[payload].field_names == expected
	function := b.m.add_function('projection', b.i64_type)
	b.cur_block = b.m.add_block(function, 'entry')
	slot := b.emit0(.alloca, b.m.type_store.get_ptr(choice))
	b.a.nodes << flat.Node{ kind: .ident }
	node := flat.NodeId(b.a.nodes.len - 1)
	variants := ['Pair', '&Pair', '&&Pair']
	for pointers, variant in variants {
		b.a.nodes[node].typ = flat.source_type_id(variant)
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

fn test_sumtype_synthetic_tag_reads_the_header_through_references() {
	for pointer_size in [4, 8] {
		mut b := sumtype_layout_builder_for_target('module main\nstruct Pair { x i64\n y i64 }\ntype Choice = i32 | Pair\n', pointer_size)!
		function := b.m.add_function('tag', b.i32_type)
		b.cur_block = b.m.add_block(function, 'entry')
		mut binding_type := b.struct_types['Choice']
		for depth, spelling in ['Choice', '&Choice', '&&Choice'] {
			if depth > 0 {
				binding_type = b.m.type_store.get_ptr(binding_type)
			}
			slot := b.emit0(.alloca, b.m.type_store.get_ptr(binding_type))
			b.vars['value'] = slot
			base := b.a.add_node(flat.Node{ kind: .ident, value: 'value', typ: flat.source_type_id(spelling) })
			children := b.a.children.len
			b.a.children << base
			selector := flat.Node{ kind: .selector, value: '__v_sum_type_tag__', children_start: children, children_count: 1 }
			start := b.m.instrs.len
			tag := b.build_selector(selector)
			assert b.value_type(tag) == b.i32_type
			load := b.m.instrs[b.m.values[tag].index]
			assert load.op == .load
			address := b.m.instrs[b.m.values[load.operands[0]].index]
			assert address.op == .get_element_ptr
			assert b.m.values[address.operands[1]].name == '0'
			loads := b.m.instrs[start..].filter(it.op == .load)
			assert loads.len == depth + 1
		}
	}
}

fn test_sumtype_reference_guards_project_fields_and_expire_on_reassignment() {
	mut b := sumtype_layout_builder('module main\nstruct Pair { x i64\n y i64 }\ntype Choice = Pair | &Pair | &&Pair\n')!
	function := b.m.add_function('guarded_projection', b.i64_type)
	b.cur_block = b.m.add_block(function, 'entry')
	mut binding_type := b.struct_types['Choice']
	for depth, spelling in ['Choice', '&Choice', '&&Choice'] {
		if depth > 0 {
			binding_type = b.m.type_store.get_ptr(binding_type)
		}
		slot := b.emit0(.alloca, b.m.type_store.get_ptr(binding_type))
		replacement := b.emit0(.alloca, b.m.type_store.get_ptr(binding_type))
		b.vars['value'] = slot
		b.vars['replacement'] = replacement
		base := b.a.add_node(flat.Node{ kind: .ident, value: 'value', typ: flat.source_type_id(spelling) })
		rhs := b.a.add_node(flat.Node{ kind: .ident, value: 'replacement', typ: flat.source_type_id(spelling) })
		for variant_depth, variant in ['Pair', '&Pair', '&&Pair'] {
			b.preserve_sum_variant(base, u32(0), 0)
			assert slot !in b.sum_guard_variants
			b.preserve_sum_variant(base, u32(0), variant_depth + 1)
			assert b.sum_guard_variants[slot] == flat.source_type_id(variant)
			start := b.m.instrs.len
			field := b.smartcast_sum_selector_addr(slot, base, 'y') or {
				panic('a borrowed sum must expose the guarded variant field')
			}
			assert b.deref_type(field) == b.i64_type
			address := b.m.instrs[b.m.values[field].index]
			assert b.m.values[address.operands[1]].name == '8'
			loads := b.m.instrs[start..].filter(it.op == .load)
			assert loads.len == depth + variant_depth
			assert b.sum_guard_variants.len == 1
			if depth > 0 {
				// Reading the borrowed binding still passes the whole sum reference.
				assert b.value_type(b.build_expr(base)) == binding_type
			}
			children := b.a.children.len
			b.a.children << [base, rhs]
			b.build_assign(flat.Node{ kind: .assign, op: .assign, children_start: children, children_count: 2 })
			assert slot !in b.sum_guard_variants
			assert b.smartcast_sum_selector_addr(slot, base, 'y') == none
		}
	}
}

fn test_sumtype_variant_selection_preserves_qualification() {
	mut b := Builder{}
	b.sum_type_variants.set('Choice', ['left.Item', 'right.Item', '&right.Item'])
	assert b.find_sum_variant(flat.source_type_id('Choice'), flat.source_type_id('right.Item'))? == flat.source_type_id('right.Item')
	assert b.find_sum_variant(flat.source_type_id('Choice'), flat.source_type_id('&right.Item'))? == flat.source_type_id('&right.Item')
	assert b.sum_variant_index(flat.source_type_id('Choice'), flat.source_type_id('right.Item')) == 2
	assert b.sum_variant_index(flat.source_type_id('Choice'), flat.source_type_id('&right.Item')) == 3
	assert b.sum_variant_index(flat.source_type_id('Choice'), flat.source_type_id('Item')) == 0
	assert b.find_sum_variant(flat.source_type_id('Choice'), flat.source_type_id('Item')) == none
	assert b.find_sum_variant(flat.source_type_id('Choice'), flat.source_type_id('missing.Item')) == none
}

fn test_sumtype_pointer_owner_slot_matches_32_and_64_bit_c_layout() {
	for pointer_size in [4, 8] {
		b := sumtype_layout_builder_for_target('module main\ntype PointerChoice = i32 | &i32\ntype ValueChoice = i32 | u32\n', pointer_size)!
		pointer_choice := b.struct_types['PointerChoice']
		pointer_layout := b.m.type_store.types[pointer_choice]
		assert pointer_layout.field_names == ['typ', '_pointer_variant_is_owned', '_payload']
		assert b.m.struct_field_offset(pointer_choice, 1) == 4
		assert b.m.struct_field_offset(pointer_choice, 2) == 8
		assert b.m.type_size(pointer_choice) == 8 + pointer_size
		value_choice := b.struct_types['ValueChoice']
		assert b.m.type_store.types[value_choice].field_names == ['typ', '_payload']
		assert b.m.struct_field_offset(value_choice, 1) == 4
		assert b.m.type_size(value_choice) == 8
	}
}

fn test_sumtype_cast_initializes_pointer_owner_state() {
	mut b := sumtype_layout_builder('module main\ntype PointerChoice = i32 | &i32\n')!
	function := b.m.add_function('cast', b.i32_type)
	b.cur_block = b.m.add_block(function, 'entry')
	zero := b.a.add_val(.int_literal, '0')
	start := b.a.children.len
	b.a.children << zero
	pointer := b.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          '&i32'
		typ:            flat.source_type_id('&i32')
		children_start: start
		children_count: 1
	})
	before := b.m.instrs.len
	value := b.build_sum_type_cast(flat.source_type_id('PointerChoice'), pointer) or { panic('sum cast failed') }
	assert b.value_type(value) == b.struct_types['PointerChoice']
	mut initialized := false
	for instruction in b.m.instrs[before..] {
		if instruction.op != .store { continue }
		destination := b.m.values[instruction.operands[1]]
		if destination.kind != .instruction { continue }
		address := b.m.instrs[destination.index]
		if address.op == .get_element_ptr && b.m.values[address.operands[1]].name == '4' {
			assert b.m.values[instruction.operands[0]].name == '0'
			initialized = true
		}
	}
	assert initialized
}

fn test_heap_sum_initializer_keeps_the_selected_payload() {
	mut b := sumtype_layout_builder('module main\ntype Choice = i32 | u32\n')!
	function := b.m.add_function('heap', b.i32_type)
	b.cur_block = b.m.add_block(function, 'entry')
	tag := b.a.add_val(.int_literal, '1')
	data := b.a.add_val(.int_literal, '9')
	start := b.a.children.len
	b.a.children << tag
	tag_field := b.a.add_node(flat.Node{ kind: .field_init, value: 'typ', children_start: start, children_count: 1 })
	b.a.children << data
	value_field := b.a.add_node(flat.Node{ kind: .field_init, value: '_i32', typ: flat.source_type_i32, children_start: start + 1, children_count: 1 })
	fields := b.a.children.len
	b.a.children << [tag_field, value_field]
	node := flat.Node{ kind: .struct_init, value: 'Choice', typ: flat.source_type_id('Choice'), children_start: fields, children_count: 2 }
	before := b.m.instrs.len
	result := b.build_heap_struct_init(node)
	assert b.deref_type(result) == b.struct_types['Choice']
	mut payload_stores := 0
	mut tag_stores := 0
	for instruction in b.m.instrs[before..] {
		if instruction.op != .store { continue }
		mut base := instruction.operands[1]
		mut offset := 0
		mut has_field_address := false
		for b.m.values[base].kind == .instruction {
			address := b.m.instrs[b.m.values[base].index]
			if address.op == .get_element_ptr {
				step := b.m.values[address.operands[1]]
				assert step.kind == .constant
				offset += step.name.int()
				base = address.operands[0]
				has_field_address = true
			} else if address.op == .bitcast {
				base = address.operands[0]
			} else {
				break
			}
		}
		if !has_field_address { continue }
		assert b.m.values[base].kind == .instruction
		assert b.m.instrs[b.m.values[base].index].op == .alloca
		assert b.deref_type(base) == b.struct_types['Choice']
		mut stored := instruction.operands[0]
		for b.m.values[stored].kind == .instruction {
			conversion := b.m.instrs[b.m.values[stored].index]
			assert conversion.op in [.trunc, .sext, .zext, .bitcast]
			stored = conversion.operands[0]
		}
		if offset == 4 {
			assert b.m.values[stored].name == '9'
			payload_stores++
		} else if offset == 0 {
			assert b.m.values[stored].name == '1'
			tag_stores++
		}
	}
	assert payload_stores == 1
	assert tag_stores == 1
}
