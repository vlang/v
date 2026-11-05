module types

import v.flat

fn test_inferred_type_witness_resolves_before_expression_annotation() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for kind, text in {
		flat.NodeKind.bool_literal:   'false'
		flat.NodeKind.int_literal:    '12'
		flat.NodeKind.float_literal:  '1.25'
		flat.NodeKind.string_literal: 'hello'
	} {
		value := a.add_node(flat.Node{ kind: kind, value: text })
		if _ := tc.expr_type(value) {
			assert false, 'the detached witness must not have been checked'
		}
		annotation := 'typeof(__expr_${int(value)})'
		expected := match kind {
			.bool_literal { 'bool' }
			.int_literal { 'int' }
			.float_literal { 'f64' }
			else { 'string' }
		}
		assert tc.parse_type(flat.source_type_id(annotation)).name() == expected
		assert tc.parse_type(flat.source_type_id('!${annotation}')).name() == '!${expected}'
		assert flat.source_type_name(tc.resolve_inferred_type_id(flat.source_type_id('&Signal[${annotation}]'))) == '&Signal[${expected}]'
	}
}

fn test_inferred_type_witness_uses_current_lexical_scope() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	annotation := 'typeof(__expr_${int(value)})'
	for payload in ['bool', 'string', 'int', 'f64'] {
		tc.cur_scope = new_scope(tc.file_scope)
		tc.cur_scope.insert('value', *tc.parse_type(flat.source_type_id(payload)))
		assert tc.parse_type(flat.source_type_id(annotation)).name() == payload
	}
}
