module transform

import v.flat
import v.types

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
		value_id := t.inferred_witness_type(expression_id)
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

// resolve_fn_literal_inferred_return_type resolves the `typeof(expr)` parts of a
// function literal's return type with its parameters bound, since they shadow
// enclosing locals of the same name. Other return types are returned unchanged.
fn (t &Transformer) resolve_fn_literal_inferred_return_type(ret_type u32, params []flat.Node) u32 {
	if !inferred_type_id(ret_type) {
		return ret_type
	}
	mut writer := unsafe { &Transformer(voidptr(t)) }
	mut shadowed := []string{}
	mut saved_types := []u32{}
	mut saved_raw_types := []u32{}
	for param in params {
		if param.kind != .param || param.value.len == 0 || param.value == '_' || param.typ == 0
			|| param.value in shadowed {
			continue
		}
		shadowed << param.value
		saved_types << t.var_type(param.value)
		saved_raw_types << t.raw_var_type(param.value)
		writer.set_var_type(param.value, param.typ)
	}
	resolved := t.resolve_inferred_type_id(ret_type)
	for i, name in shadowed {
		if saved_types[i] == 0 {
			writer.unset_var_type(name)
		} else {
			writer.set_var_type_with_raw(name, saved_types[i], saved_raw_types[i])
		}
	}
	return resolved
}

// inferred_witness_type is the type of a `typeof(expr)` expression, resolved
// with transform's current locals. Transform's own resolution of a generic call
// such as `typeof(identity(value))` names the callee's open `T`; outside generic
// code the checker has already checked the expression in its lexical scope and
// inferred those arguments, so its type is used instead.
fn (t &Transformer) inferred_witness_type(expression_id flat.NodeId) u32 {
	resolved := t.resolve_expr_type(expression_id)
	if resolved != 0 && flat.source_type_name(resolved) !in ['unknown', 'void']
		&& !t.generic_arg_is_unresolved(resolved) {
		return resolved
	}
	if !isnil(t.tc) && t.active_specialization_args.ids.len == 0 && t.active_generic_params.len == 0 {
		if checked := t.tc.expr_type(expression_id) {
			if checked !is types.Unknown && checked !is types.Void {
				return t.semantic_type_name(checked)
			}
		}
		// Resolving a witness records its type on the expression node and drops
		// the checker's cached entry for it; later lookups read that record.
		recorded := t.a.nodes[int(expression_id)].typ
		if recorded != 0 && flat.source_type_name(recorded) !in ['unknown', 'void', 'array', 'map']
			&& !t.generic_arg_is_unresolved(recorded) {
			return recorded
		}
	}
	return resolved
}
