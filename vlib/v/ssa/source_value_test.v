module ssa

import v.flat

fn test_cast_source_operands_keep_constant_and_checked_lowering_priorities() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	mut b := Builder{
		a:        &a
		m:        m
		i64_type: m.type_store.get_int(64)
		u8_type:  m.type_store.get_uint(8)
		u16_type: m.type_store.get_uint(16)
	}
	literal := a.add_val(.int_literal, '257')
	start := a.children.len
	a.children << literal
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		source_value:   flat.source_type_u8
		typ:            flat.source_type_u16
		children_start: start
		children_count: 1
	})
	assert a.node(cast).value == ''
	// Constant evaluation follows source syntax; checked lowering keeps its
	// existing preference for the refined annotation.
	assert b.global_constant_type(cast) == b.u8_type
	assert b.global_constant_int(cast, 0) == 1
	function := m.new_function('checked_cast', b.u16_type)
	b.cur_block = m.add_block(function, 'entry')
	value := b.build_expr(cast)
	assert m.values[value].typ == b.u16_type
	assert m.instrs[m.values[value].index].op == .trunc
}

fn test_sizeof_source_operands_keep_expression_and_binding_precedence() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	mut b := Builder{
		a:        &a
		m:        m
		i64_type: m.type_store.get_int(64)
		u8_type:  m.type_store.get_uint(8)
		u16_type: m.type_store.get_uint(16)
	}
	b.var_type_names.set_id('SizeAlias', flat.source_type_u16)
	alias_size := a.add_node(flat.Node{
		kind:         .sizeof_expr
		source_value: flat.source_type_id('SizeAlias')
		typ:          flat.source_type_usize
	})
	alias_value := b.build_expr(alias_size)
	assert m.values[alias_value].name == '2'
	literal := a.add_node(flat.Node{
		kind:  .int_literal
		value: '7'
		typ:   flat.source_type_i64
	})
	start := a.children.len
	a.children << literal
	expression_size := a.add_node(flat.Node{
		kind:           .sizeof_expr
		source_value:   flat.source_type_u8
		typ:            flat.source_type_usize
		children_start: start
		children_count: 1
	})
	expression_value := b.build_expr(expression_size)
	assert m.values[expression_value].name == '8'
	assert m.instrs.len == 0
}

fn test_type_only_identifier_text_stays_available_to_qualified_name_helpers() {
	mut a := flat.FlatAst.new()
	b := Builder{ a: &a }
	base := a.add_node(flat.Node{
		kind:         .ident
		source_value: flat.source_type_id('pkg.Record[int]')
	})
	start := a.children.len
	a.children << base
	selector := a.add_node(flat.Node{
		kind:           .selector
		value:          'method'
		children_start: start
		children_count: 1
	})
	assert b.ident_name(base) == 'pkg.Record[int]'
	assert b.qualified_expr_name(selector) == 'pkg.Record[int].method'
	assert b.selector_qualified_name(*a.node(selector)) == 'pkg.Record[int].method'
}

fn test_sum_declaration_variants_use_source_only_identifier_operands() {
	mut a := flat.FlatAst.new()
	b := Builder{ a: &a }
	variant_id := flat.source_type_id('Record')
	qualified_id := flat.source_type_id('pkg.Record[int]')
	first := a.add_node(flat.Node{ kind: .ident, source_value: variant_id })
	second := a.add_node(flat.Node{ kind: .ident, source_value: qualified_id })
	start := a.children.len
	a.children << first
	a.children << second
	decl := flat.Node{
		kind:           .type_decl
		value:          'Sum'
		children_start: start
		children_count: 2
	}
	assert b.sum_type_variants_for_decl(decl, 'main') == [variant_id, qualified_id]
	assert b.sum_type_variants_for_decl(decl, 'other') == [
		flat.source_type_id('other.Record'),
		qualified_id,
	]
}

fn test_native_type_layout_uses_interned_source_operands() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	i8_type := m.type_store.get_int(8)
	i64_type := m.type_store.get_int(64)
	record_type := m.type_store.register(Type{
		kind:        .struct_t
		fields:      [i8_type, i64_type]
		field_names: ['tag', 'payload']
	})
	mut b := Builder{
		a:            &a
		m:            m
		i8_type:      i8_type
		i64_type:     i64_type
		struct_types: {
			'Record': record_type
		}
	}
	size := a.add_node(flat.Node{ kind: .sizeof_expr, source_value: flat.source_type_id('Record') })
	assert a.node(size).value == ''
	assert b.global_constant_int(size, 0) == 16
	offset := a.add_node(flat.Node{
		kind:         .offsetof_expr
		source_value: flat.source_type_id('Record')
		typ:          flat.source_type_id('payload')
	})
	assert a.node(offset).value == ''
	result := b.build_expr(offset)
	assert m.values[result].name == '8'
}
