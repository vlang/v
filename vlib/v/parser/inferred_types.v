module parser

import v.flat

// shift_inferred_type_id is `shift_inferred_type` for an interned node
// annotation; other spellings keep their ID without reading their bytes.
@[inline]
fn shift_inferred_type_id(typ u32, shift int) u32 {
	if !flat.source_type_query(typ).has(.inferred_witness) {
		return typ
	}
	syntax := flat.source_type_syntax(typ)
	if syntax.inferred_annotations.len == 0 {
		return typ
	}
	mut parts := []u32{cap: syntax.inferred_parts.len + syntax.inferred_annotations.len}
	for i, expression_bits in syntax.inferred_expr_ids {
		parts << syntax.inferred_parts[i]
		parts << flat.source_type_composition_one(.inferred_expression, u32(int(expression_bits) + shift))
	}
	parts << syntax.inferred_parts[syntax.inferred_parts.len - 1]
	return flat.source_type_composition(.concat, parts)
}

// shift_inferred_value relocates `typeof(expr)` links held in a node's value.
// Besides identifiers, array initializers (`[]typeof(x){}`), casts, struct
// initializers and other type-bearing nodes keep their type spelling there.
// Literal text is user data and never holds a link.
@[inline]
fn shift_inferred_value(kind flat.NodeKind, value string, shift int) string {
	if kind in [.string_literal, .char_literal, .string_interp] {
		return value
	}
	if !value.contains('typeof(__expr_') {
		return value
	}
	return shift_inferred_type(value, shift)
}

// Parser workers merge local ASTs with shifted node ids. `typeof(expr)` type
// annotations hold expression links and relocate with the ordinary child links.
fn shift_inferred_type(typ string, shift int) string {
	return flat.source_type_name(shift_inferred_type_id(flat.source_type_id(typ), shift))
}
