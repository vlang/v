from pathlib import Path

OLD = '''\tif clean_expected.is_float()
\t\t&& (tc.is_untyped_float_literal_expr(id) || node.kind == .int_literal) {
\t\ttc.register_synth_type(id, expected_raw)
\t\treturn expected_raw
\t}
'''
NEW = '''\tif clean_expected.is_float() {
\t\t// Unary signs do not turn an untyped numeric constant into a typed integer.
\t\tknown, _ := tc.untyped_numeric_literal_expr_info(id, 0)
\t\tif known {
\t\t\ttc.register_synth_type(id, expected_raw)
\t\t\treturn expected_raw
\t\t}
\t}
'''

UNIT = '''module types

import v.flat

fn float_context_test_wrap(mut a flat.FlatAst, kind flat.NodeKind, op flat.Op, child flat.NodeId) flat.NodeId {
	children_start := a.begin_children()
	a.add_child(child)
	return a.add_node(flat.Node{
		kind:           kind
		op:             op
		children_start: children_start
		children_count: 1
	})
}

fn test_signed_integer_literals_adopt_float_context() {
	for expected in [Type(f32_), Type(f64_)] {
		mut a := flat.FlatAst.new()
		one := a.add_val(.int_literal, '1')
		negative := float_context_test_wrap(mut a, .prefix, .minus, one)
		positive := float_context_test_wrap(mut a, .prefix, .plus, one)
		paren := float_context_test_wrap(mut a, .paren, .none, one)
		negative_paren := float_context_test_wrap(mut a, .prefix, .minus, paren)
		paren_negative := float_context_test_wrap(mut a, .paren, .none, negative)
		double_negative := float_context_test_wrap(mut a, .prefix, .minus, paren_negative)
		mut tc := TypeChecker.new(&a)
		for expr in [one, negative, positive, negative_paren, paren_negative, double_negative] {
			assert tc.resolve_expr(expr, expected).name() == expected.name()
		}
	}
}

fn test_float_context_preserves_typed_integer_expressions() {
	mut a := flat.FlatAst.new()
	one := a.add_val(.int_literal, '1')
	cast_children := a.begin_children()
	a.add_child(one)
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		value:          'int'
		children_start: cast_children
		children_count: 1
	})
	n := a.add_val(.ident, 'n')
	negative_n := float_context_test_wrap(mut a, .prefix, .minus, n)
	positive_n := float_context_test_wrap(mut a, .prefix, .plus, n)
	paren_n := float_context_test_wrap(mut a, .paren, .none, negative_n)
	negative_cast := float_context_test_wrap(mut a, .prefix, .minus, cast)
	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	tc.cur_scope.insert('n', Type(int_))
	for expr in [n, negative_n, positive_n, paren_n, cast, negative_cast] {
		assert tc.resolve_expr(expr, Type(f32_)).name() == 'int'
	}
}

fn test_signed_integer_literal_does_not_adopt_unsigned_context() {
	mut a := flat.FlatAst.new()
	one := a.add_val(.int_literal, '1')
	negative := float_context_test_wrap(mut a, .prefix, .minus, one)
	mut tc := TypeChecker.new(&a)
	assert tc.resolve_expr(negative, Type(u32_)).name() == 'int'
}
'''

RUNTIME = '''const signed_float_integer_constant = -3

fn test_assign_signed_integer_literals_to_f32() {
	mut a := f32(0)
	a = 1
	assert a == f32(1)
	a = -1
	assert a == f32(-1)
	a = +1
	assert a == f32(1)
	a = -(1)
	assert a == f32(-1)
	a = (-1)
	assert a == f32(-1)
	a = -(-1)
	assert a == f32(1)
	a = -0
	assert a == f32(0)
	a = -0x20
	assert a == f32(-32)
	a = -0b10
	assert a == f32(-2)
	a = -1_024
	assert a == f32(-1024)
}

fn test_assign_signed_integer_literals_to_f64() {
	mut a := f64(0)
	a = 1
	assert a == f64(1)
	a = -1
	assert a == f64(-1)
	a = +1
	assert a == f64(1)
	a = -(1)
	assert a == f64(-1)
	a = (-1)
	assert a == f64(-1)
	a = -(-1)
	assert a == f64(1)
}

fn test_assign_untyped_integer_constant_expressions_to_float() {
	mut a := f32(0)
	a = signed_float_integer_constant
	assert a == f32(-3)
	a = -signed_float_integer_constant
	assert a == f32(3)
	a = -(1 + 2)
	assert a == f32(-3)
	a = 1 - 2
	assert a == f32(-1)
	// Integer constant division must still happen before conversion to float.
	a = -7 / 2
	assert a == f32(-3)
}

fn signed_integer_literal_f32_return() f32 {
	return -1
}

fn signed_integer_literal_f32_argument(value f32) f32 {
	return value
}

struct SignedIntegerFloatFields {
	value32 f32
	value64 f64
}

fn test_signed_integer_literals_in_other_float_contexts() {
	assert signed_integer_literal_f32_return() == f32(-1)
	assert signed_integer_literal_f32_argument(-1) == f32(-1)
	values := SignedIntegerFloatFields{
		value32: -1
		value64: -1
	}
	assert values.value32 == f32(-1)
	assert values.value64 == f64(-1)
}
'''

if __name__ == '__main__':
    import hashlib
    path = Path('vlib/v/types/checker_tail_stmt.v')
    data = path.read_bytes()
    sha = hashlib.sha1(f'blob {len(data)}\0'.encode() + data).hexdigest()
    if sha != '339cf15a6c71e1c366cd870b76b43a05cc57a47a':
        raise SystemExit('Source differs from the reviewed base; refusing to patch.')
    text = data.decode()
    if text.count(OLD) != 1:
        raise SystemExit('Expected exactly one contextual float literal check.')
    for name in ['vlib/v/types/negative_integer_float_test.v',
                 'vlib/v/tests/assign/negative_integer_float_test.v']:
        if Path(name).exists():
            raise SystemExit(f'Refusing to overwrite existing test: {name}')
    path.write_text(text.replace(OLD, NEW, 1))
    Path('vlib/v/types/negative_integer_float_test.v').write_text(UNIT)
    Path('vlib/v/tests/assign/negative_integer_float_test.v').write_text(RUNTIME)
