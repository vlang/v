module transform

import v.flat

const inferred_type_marker = 'typeof(__expr_'

// inferred_type_id reports whether an interned spelling holds a `typeof(expr)`
// type annotation. The shape check keeps ordinary spellings byte-free.
@[inline]
fn inferred_type_id(typ_id u32) bool {
	return flat.source_type_shape(typ_id).has(.typeof_expr)
		&& flat.source_type_name(typ_id).contains(inferred_type_marker)
}

// resolve_inferred_type_id is `resolve_inferred_type_text` for an
// interned spelling; other spellings are returned unchanged.
fn (t &Transformer) resolve_inferred_type_id(typ_id u32) u32 {
	if !inferred_type_id(typ_id) {
		return typ_id
	}
	typ := flat.source_type_name(typ_id)
	resolved := t.resolve_inferred_type_text(typ)
	return if resolved == typ { typ_id } else { flat.source_type_id(resolved) }
}

// `typeof(expr)` type annotations retain their ordinary V expression.
// Resolve them while transform owns the same lexical locals as that expression,
// before recording a concrete callback signature or generic specialization.
fn (t &Transformer) resolve_inferred_type_text(typ string) string {
	mut result := typ
	mut offset := 0
	marker := inferred_type_marker
	for offset < result.len {
		start := result[offset..].index(marker) or { break } + offset
		end := result[start..].index(')') or { break } + start + 1
		expression_id := flat.NodeId(result[start + marker.len..end - 1].int())
		if int(expression_id) < 0 || int(expression_id) >= t.a.nodes.len { break }
		value_id := t.resolve_expr_type(expression_id)
		value := flat.source_type_name(value_id)
		if value.len == 0 || value == 'void' || value.contains(marker) { break }
		// Source witnesses precede transform's generated-node range. Their AST
		// text is promoted with the enclosing source scope, while a synthesized
		// checker Type would retain pointers into that disposable scope.
		mut writer := unsafe { &Transformer(voidptr(t)) }
		writer.set_node_typ(int(expression_id), value_id)
		result = result[..start] + value + result[end..]
		offset = start + value.len
	}
	return result
}
