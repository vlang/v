module transform

import v.flat
import v.types

// inferred_type_id reports whether an interned spelling holds a `typeof(expr)`
// type annotation without inspecting its spelling bytes.
@[inline]
fn inferred_type_id(typ_id u32) bool {
	return flat.source_type_query(typ_id).has(.inferred_witness)
}

// resolve_inferred_type_id resolves source witness nodes from cached numeric
// annotation components; other spellings are returned unchanged.
fn (t &Transformer) resolve_inferred_type_id(typ_id u32) u32 {
	if !inferred_type_id(typ_id) {
		return typ_id
	}
	syntax := flat.source_type_syntax(typ_id)
	mut result := syntax.inferred_parts()[0]
	mut stopped := false
	mut changed := false
	for i, annotation in syntax.inferred_annotations() {
		mut replacement := annotation
		if !stopped {
			expression_id := flat.NodeId(syntax.inferred_expr_ids()[i])
			if int(expression_id) < 0 || int(expression_id) >= t.a.nodes.len {
				stopped = true
			} else {
				value_id := t.inferred_witness_type(expression_id)
				if value_id in [u32(0), flat.source_type_void] || inferred_type_id(value_id) {
					stopped = true
				} else {
					// Source witnesses precede transform's generated-node range. Their
					// concrete spelling IDs remain valid after their source scope expires.
					mut writer := unsafe { &Transformer(voidptr(t)) }
					writer.set_node_typ(int(expression_id), value_id)
					replacement = value_id
					changed = true
				}
			}
		}
		result = flat.source_type_composition_two(.concat, result, replacement)
		result = flat.source_type_composition_two(.concat, result, syntax.inferred_parts()[i + 1])
	}
	return if changed { result } else { typ_id }
}

// `typeof(expr)` type annotations retain their ordinary V expression.
// Resolve them while transform owns the same lexical locals as that expression,
// before recording a concrete callback signature or generic specialization.
fn (t &Transformer) resolve_inferred_type_text(typ string) string {
	return flat.source_type_name(t.resolve_inferred_type_id(flat.source_type_id(typ)))
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
	if resolved !in [u32(0), flat.source_type_unknown, flat.source_type_void]
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
		if recorded !in [u32(0), flat.source_type_unknown, flat.source_type_void,
			flat.source_type_id('array'), flat.source_type_id('map')]
			&& !t.generic_arg_is_unresolved(recorded) {
			return recorded
		}
	}
	return resolved
}
