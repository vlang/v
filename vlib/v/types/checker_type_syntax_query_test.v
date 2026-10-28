module types

import v.flat
import v.token

fn test_numeric_generic_placeholder_queries_keep_recursive_spelling_rules() {
	a := &flat.FlatAst{}
	tc := TypeChecker.new(a)
	for typ in [' C ', 'os.T', 'map[T]', 'map[]T', 'Box[T', '[3] T ', 'fn(value T)', 'fn(mut value T)',
		'fn (value []T) int', 'fn(T) [', 'Box[&T]'] {
		assert tc.type_text_has_generic_placeholder(flat.source_type_id(typ)), typ
	}
	for typ in ['os. T', 'map[T', 'fn(T', 'Box[T + 1]', 'Box[16]'] {
		assert !tc.type_text_has_generic_placeholder(flat.source_type_id(typ)), typ
	}
	assert !tc.type_text_has_unbound_generic_placeholder_ids(flat.source_type_id('fn (T) int'), [])
	assert tc.type_text_has_unbound_generic_placeholder_ids(flat.source_type_id('fn (Box[T]) int'), [])
	assert !tc.type_text_has_unbound_generic_placeholder_ids(flat.source_type_id('map[os.T]V'), [
		flat.source_type_id('T'),
		flat.source_type_id('V'),
	])
	assert tc.type_text_has_unbound_generic_placeholder_ids(flat.source_type_id('map[os.T]V'), [
		flat.source_type_id('V'),
	])
	assert tc.type_text_has_unbound_generic_placeholder_ids(flat.source_type_id(' T '), [
		flat.source_type_id(' T '),
	])
}

fn test_numeric_reflection_type_queries_keep_function_wrapper_order() {
	a := &flat.FlatAst{}
	tc := TypeChecker.new(a)
	for typ in ['fn', 'fn(', 'fn (', '[]fn (', '&?[]int', '[3]map[string]int', 'map[string][]int'] {
		assert tc.comptime_reflection_condition_type_known(flat.source_type_id(typ)), typ
	}
	for typ in ['&fn', '&fn (int)', '[3]', 'map[string]', 'map[]int', 'map[string]Missing'] {
		assert !tc.comptime_reflection_condition_type_known(flat.source_type_id(typ)), typ
	}
}

fn test_numeric_type_qualification_keeps_raw_empty_parts_and_suffixes() {
	a := &flat.FlatAst{}
	mut tc := TypeChecker.new(a)
	tc.cur_module = 'owner'
	cases := {
		' T ':                           'owner.T'
		'mut &T':                        '&owner.T'
		'chan mut T':                    'chan mut owner.T'
		'chan  mut T':                   'chan &owner.T'
		'thread T':                      'thread owner.T'
		'map[ ]T':                       'map[ ]owner.T'
		'[ 3 ]T':                        '[ 3 ]owner.T'
		'(T, , int)':                    '(owner.T,  , int)'
		'Box[T, , int] tail':            'owner.Box[owner.T,  , int] tail'
		'Box[3] tail':                   'owner.Box[3] tail'
		'int[T] tail':                   'int[T] tail'
		'fn (mut value T, other []T) T': 'fn(mut owner.T, []owner.T) owner.T'
		'fn(, T,)':                      'fn(, owner.T)'
		'typeof(__expr_17)':             'typeof(__expr_17)'
		'   ':                           '   '
	}
	for input, expected in cases {
		assert tc.qualify_type_text(flat.source_type_id(input)) == flat.source_type_id(expected), input
	}
	assert tc.qualify_type_text_with_generic_params(flat.source_type_id('Box[T]'), generic_param_ids(['T'])) == flat.source_type_id('owner.Box[T]')
}

fn test_written_nested_option_probe_keeps_source_storage_and_operand_roles() {
	nested_id := flat.source_type_id('Box[??int]')
	for indexed in [false, true] {
		node := flat.Node{
			kind:         .ident
			value:        if indexed { '' } else { 'Box[??int]' }
			source_value: if indexed { nested_id } else { u32(0) }
		}
		assert has_written_nested_option_result_type(&node)
	}
	// Raw source keeps precedence when a legacy node carries both fields.
	for node in [
		flat.Node{ kind: .ident, value: 'ordinary', source_value: nested_id },
		flat.Node{ kind: .param, value: '??name', typ: flat.source_type_int },
		flat.Node{ kind: .fn_decl, value: '??name', typ: flat.source_type_int },
		flat.Node{ kind: .c_fn_decl, value: '??name', typ: flat.source_type_int },
		flat.Node{ kind: .ident, value: 'ordinary', typ: nested_id },
		flat.Node{ kind: .struct_init, value: 'Ordinary', typ: nested_id },
		flat.Node{ kind: .sizeof_expr, source_value: nested_id, children_count: 1 },
		flat.Node{ kind: .typeof_expr, source_value: nested_id, children_count: 1 },
		flat.Node{ kind: .interface_field, op: .dot, value: 'Box[??int]' },
		flat.Node{ kind: .interface_field, value: 'Box[??int]', typ: flat.source_type_int },
	] {
		assert !has_written_nested_option_result_type(&node), node.kind.str()
	}
	for node in [
		flat.Node{ kind: .ident, value: 'Box[?!int', source_value: flat.source_type_int },
		flat.Node{ kind: .param, value: 'value', typ: nested_id },
		flat.Node{ kind: .c_fn_decl, value: 'function', typ: nested_id },
		flat.Node{ kind: .sizeof_expr, source_value: nested_id },
		flat.Node{ kind: .typeof_expr, source_value: nested_id },
		flat.Node{ kind: .interface_field, value: 'Box[??int]' },
	] {
		assert has_written_nested_option_result_type(&node), node.kind.str()
	}
	assert nested_option_result_marker('Box[?!int') == 4
	assert nested_option_result_marker('? ?int') == -1
	assert nested_option_result_marker('int?') == -1
}

fn test_written_generic_source_names_keep_only_type_operands() {
	generic_id := flat.source_type_id('Box[int]')
	for raw in ['ordinary', 'T', '?Name', 'name\x00suffix'] {
		mut node := flat.Node{ kind: .ident, value: raw, source_value: generic_id }
		node.pos = token.new_span(0, 0, 1)
		assert written_type_ids(node) == []u32{}
		node.kind = .interface_field
		assert written_type_ids(node) == []u32{}
		node.typ = flat.source_type_int
		assert written_type_ids(node) == [flat.source_type_int]
	}
	for indexed in [false, true] {
		mut node := flat.Node{
			kind:         .ident
			value:        if indexed { '' } else { 'Box[int]' }
			source_value: if indexed { generic_id } else { u32(0) }
		}
		node.pos = token.new_span(0, 0, 1)
		assert written_type_ids(node) == [generic_id]
		node.pos = token.new_span(0, 0, 0)
		assert written_type_ids(node) == []u32{}
	}
	mut malformed := flat.Node{ kind: .ident, value: 'Box[' }
	malformed.pos = token.new_span(0, 0, 1)
	assert written_type_ids(malformed) == [flat.source_type_id('Box[')]
	mut embedded := flat.Node{ kind: .interface_field, value: 'Box[int]' }
	assert written_type_ids(embedded) == [generic_id]
	embedded.op = .dot
	assert written_type_ids(embedded) == []u32{}
	for kind in [flat.NodeKind.sizeof_expr, .typeof_expr] {
		mut node := flat.Node{ kind: kind, source_value: generic_id }
		assert written_type_ids(node) == [generic_id]
		node.children_count = 1
		assert written_type_ids(node) == []u32{}
	}
}
