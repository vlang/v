module ssa

import v.flat

fn test_byte_is_not_an_ssa_primitive_type() {
	mut b := Builder{}
	assert b.primitive_type_id('byte') == none
	assert !type_ref_is_builtin('byte')
	assert normalize_primitive_type_name('models.byte') == 'models.byte'
}

fn test_sizeof_byte_expression_uses_operand_type_without_evaluating_it() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	mut b := Builder{
		a:        &a
		m:        m
		i64_type: m.type_store.get_int(64)
		f64_type: m.type_store.get_float(64)
		u8_type:  m.type_store.get_uint(8)
	}
	for typ, expected in {
		'f64':      '8'
		'u8':       '1'
		'[3]u8':    '3'
		'[2][3]u8': '6'
	} {
		operand := a.add_node(flat.Node{
			kind:  .ident
			value: 'byte'
			typ:   typ
		})
		start := a.children.len
		a.children << operand
		sizeof_id := a.add_node(flat.Node{
			kind:           .sizeof_expr
			value:          'byte'
			children_start: start
			children_count: 1
		})
		result := b.build_expr(sizeof_id)
		assert m.values[result].kind == .constant
		assert m.values[result].name == expected
		assert m.instrs.len == 0
	}
	call := a.add_node(flat.Node{
		kind: .call
		typ:  'f64'
	})
	start := a.children.len
	a.children << call
	sizeof_call := a.add_node(flat.Node{
		kind:           .sizeof_expr
		children_start: start
		children_count: 1
	})
	result := b.build_expr(sizeof_call)
	assert m.values[result].name == '8'
	assert m.instrs.len == 0
}
