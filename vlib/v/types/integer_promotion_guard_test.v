module types

import v.flat

struct IntegerPromotionBoundaryCase {
	value    string
	other    Type
	expected string
}

fn test_integer_promotion_guard_preserves_storage_and_small_integer_boundaries() {
	cases := [
		IntegerPromotionBoundaryCase{'127', Primitive{ props: .integer, size: 8 }, 'i8'},
		IntegerPromotionBoundaryCase{'128', Primitive{ props: .integer, size: 8 }, 'int'},
		IntegerPromotionBoundaryCase{'-128', Primitive{ props: .integer, size: 8 }, 'i8'},
		IntegerPromotionBoundaryCase{'-129', Primitive{ props: .integer, size: 8 }, 'int'},
		IntegerPromotionBoundaryCase{'255', Primitive{ props: .integer | .unsigned, size: 8 }, 'u8'},
		IntegerPromotionBoundaryCase{'256', Primitive{ props: .integer | .unsigned, size: 8 }, 'int'},
		IntegerPromotionBoundaryCase{'-1', Primitive{ props: .integer | .unsigned, size: 8 }, 'int'},
		IntegerPromotionBoundaryCase{'32767', Primitive{ props: .integer, size: 16 }, 'i16'},
		IntegerPromotionBoundaryCase{'32768', Primitive{ props: .integer, size: 16 }, 'int'},
		IntegerPromotionBoundaryCase{'-32768', Primitive{ props: .integer, size: 16 }, 'i16'},
		IntegerPromotionBoundaryCase{'-32769', Primitive{ props: .integer, size: 16 }, 'int'},
		IntegerPromotionBoundaryCase{'65535', Primitive{ props: .integer | .unsigned, size: 16 }, 'u16'},
		IntegerPromotionBoundaryCase{'65536', Primitive{ props: .integer | .unsigned, size: 16 }, 'int'},
		IntegerPromotionBoundaryCase{'-1', Primitive{ props: .integer | .unsigned, size: 16 }, 'int'},
		IntegerPromotionBoundaryCase{'42', Primitive{ props: .integer, size: 32 }, 'i32'},
		IntegerPromotionBoundaryCase{'42', Primitive{ props: .integer, size: 64 }, 'i64'},
		IntegerPromotionBoundaryCase{'42', Primitive{ props: .integer | .unsigned, size: 32 }, 'u32'},
		IntegerPromotionBoundaryCase{'42', Primitive{ props: .integer | .unsigned, size: 64 }, 'u64'},
		IntegerPromotionBoundaryCase{'42', Rune{}, 'rune'},
		IntegerPromotionBoundaryCase{'42', ISize{}, 'isize'},
		IntegerPromotionBoundaryCase{'42', USize{}, 'uint'},
		IntegerPromotionBoundaryCase{'42', Alias{ name_id: flat.source_type_id('Count'), base_type: &Type(i64_) }, 'i64'},
	]
	for c in cases {
		mut a := flat.FlatAst.new()
		mut literal := a.add_val(.int_literal, c.value.trim_left('-'))
		if c.value.starts_with('-') {
			start := a.children.len
			a.children << literal
			literal = a.add_node(flat.Node{ kind: .prefix, op: .minus, children_start: start, children_count: 1 })
		}
		other := a.add_val(.ident, 'operand')
		tc := TypeChecker.new(&a)
		actual := tc.int_literal_promoted_infix_type(literal, other, builtin_int_type, c.other) or {
			panic('missing ${c.value} promotion to ${c.other.name()}')
		}
		assert actual.name() == c.expected, '${c.value}, ${c.other.name()}'
	}
}

fn test_integer_promotion_guard_rejects_ineligible_operands_and_preserves_constant_expressions() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.int_literal, '20')
	right := a.add_val(.int_literal, '22')
	a.children << left
	a.children << right
	expr := a.add_node(flat.Node{ kind: .infix, op: .plus, children_start: 0, children_count: 2 })
	constant := a.add_val(.ident, 'limit')
	unknown := a.add_val(.ident, 'missing')
	other := a.add_val(.ident, 'operand')
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['limit'] = expr
	for id in [expr, constant] {
		actual := tc.int_literal_promoted_infix_type(id, other, builtin_int_type, builtin_i64_type) or {
			panic('missing constant expression promotion')
		}
		assert actual.name() == 'i64'
		for typ in [Type(int_), String{}, Primitive{ props: .float, size: 64 },
			Pointer{ base_type: &Type(int_) }, Array{ elem_type: &Type(int_) },
			OptionType{ base_type: &Type(int_) }, Struct{ name_id: flat.source_type_id('Item') },
			Alias{ name_id: flat.source_type_id('Plain'), base_type: &Type(int_) }] {
			assert tc.int_literal_promoted_infix_type(id, other, builtin_int_type, typ) == none
		}
	}
	assert tc.int_literal_promoted_infix_type(left, right, builtin_int_type, builtin_i64_type) == none
	assert tc.int_literal_promoted_infix_type(unknown, other, builtin_int_type, builtin_i64_type) == none
	assert tc.int_literal_promoted_infix_type(left, other, builtin_i64_type, builtin_i64_type) == none
}
