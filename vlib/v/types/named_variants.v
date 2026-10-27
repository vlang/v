module types

import v.flat

// named_variant_pattern_binding returns the payload binding of a `match` branch
// pattern such as `Expr.Count(n)`. The parser stores it as a `.param` child of
// the pattern node.
pub fn named_variant_pattern_binding(a &flat.FlatAst, cond &flat.Node) ?flat.NodeId {
	if cond.kind !in [.ident, .selector] {
		return none
	}
	for i in 0 .. cond.children_count {
		child_id := a.child(cond, i)
		if a.node(child_id).kind == .param {
			return child_id
		}
	}
	return none
}

// named_variant_payload_type returns the payload type of a concrete hidden
// variant struct, or none for a payload-less variant.
pub fn (tc &TypeChecker) named_variant_payload_type(variant_type_id u32) ?Type {
	if !flat.source_type_syntax(variant_type_id).named_variant() {
		return none
	}
	return tc.struct_field_type(variant_type_id, flat.named_variant_payload_field)
}

// declare_named_variant_binding declares the payload binding of a single
// pattern `match` branch, `Expr.Count(n) { ... }`, in the branch scope. The
// binding is an immutable copy of the payload, also in `match mut` branches.
fn (mut tc TypeChecker) declare_named_variant_binding(subject_type Type, branch &flat.Node, n_conds int) {
	if n_conds != 1 {
		return
	}
	cond_id := tc.a.child(branch, 0)
	cond := tc.a.node(cond_id)
	binding_id := named_variant_pattern_binding(tc.a, cond) or { return }
	binding := tc.a.node(binding_id)
	// An earlier `is` check can already narrow the subject to a variant struct.
	// Resolve patterns against its owning sum so payload bindings still work.
	sum_name := if subject_type is SumType {
		subject_type.source_type_id()
	} else if subject_type is Struct {
		variant_syntax := flat.source_type_syntax(subject_type.source_type_id())
		if !variant_syntax.named_variant_valid() {
			return
		}
		variant_syntax.named_variant_owner()
	} else {
		return
	}
	pattern_source_id := tc.match_type_pattern(cond) or { return }

	variant_type_source_id := tc.sum_variant_type_for_pattern(sum_name, pattern_source_id) or { return }
	payload_type := tc.named_variant_payload_type(variant_type_source_id) or {
		display := flat.named_variant_display_name(flat.source_type_name(variant_type_source_id))
		tc.record_error_at(.condition_mismatch, '`${display}` has no payload to bind to `${binding.value}`',
			binding_id, binding.pos)
		tc.cur_scope.insert(binding.value, Type(Unknown{}))
		return
	}
	if binding.is_mut {
		display := flat.named_variant_display_name(flat.source_type_name(variant_type_source_id))
		tc.record_error_at(.assignment_mismatch, 'the payload binding `${binding.value}` cannot be mutable; it is a copy of the payload, assign a new variant value to change it',
			binding_id, binding.pos)
	}
	tc.check_local_binding_global_shadowing(binding_id)
	owner := tc.insert_decl_lhs(binding_id, payload_type, false)
	tc.initialize_unknown_pointer_binding(owner, payload_type)
	tc.remember_expr_type(binding_id, payload_type)
}

const named_variant_payload_field_prefix = 'cannot assign to field `${flat.named_variant_payload_field}`: '

// named_variant_diagnostic rewords a payload field diagnostic of a lowered
// variant constructor, `Expr.Count('x')`, in terms of the variant.
fn (tc &TypeChecker) named_variant_diagnostic(msg string, node flat.NodeId) string {
	if !msg.starts_with(named_variant_payload_field_prefix) || !tc.valid_node_id(node) {
		return msg
	}
	parent_id := tc.direct_parent_id(node)
	if !tc.valid_node_id(parent_id) {
		return msg
	}
	parent := tc.a.node(parent_id)
	if parent.kind != .struct_init
		|| !flat.source_type_syntax(parent.source_value_id()).named_variant() {
		return msg
	}
	variant := flat.named_variant_display_name(flat.source_type_name(flat.source_type_syntax(parent.source_value_id()).unqualified()))
	return 'invalid payload for `${variant}`: ${msg[named_variant_payload_field_prefix.len..]}'
}

// named_sum_variant_names returns the variant names of a sum type declared with
// named variants, or an empty list for any other type.
fn (tc &TypeChecker) named_sum_variant_names(sum_name_id u32) []string {
	variants := tc.sum_types.type_ids(flat.source_type_name(sum_name_id)) or { return []string{} }
	mut names := []string{cap: variants.len}
	for variant in variants {
		syntax := flat.source_type_syntax(variant)
		if !syntax.named_variant_valid() {
			return []string{}
		}
		names << flat.source_type_name(syntax.named_variant_name())
	}
	return names
}

fn named_variant_init_display(variant_id u32) string {
	syntax := flat.source_type_syntax(variant_id)
	owner_name := flat.source_type_syntax(syntax.named_variant_owner()).unqualified()
	return '${flat.source_type_name(owner_name)}.${flat.source_type_name(syntax.named_variant_name())}'
}

// check_named_variant_init checks a lowered variant constructor,
// `Expr(Expr@variant@Count{payload: x})` for `Expr.Count(x)`, before the regular
// struct literal checks. It returns false when the literal was fully handled.
fn (mut tc TypeChecker) check_named_variant_init(id flat.NodeId, node flat.Node) bool {
	variant_id := node.source_value_id()
	variant_syntax := flat.source_type_syntax(variant_id)
	if !variant_syntax.named_variant_valid() {
		return true
	}
	variant_type := tc.parse_type(variant_id)
	base_id := generic_base_type_id(variant_id)
	if variant_type !is Struct
		|| (flat.source_type_name(base_id) !in tc.structs
			&& flat.source_type_name(tc.qualify_type_name(base_id)) !in tc.structs) {
		sum_type := tc.parse_type(variant_syntax.named_variant_owner())
		names := if sum_type is SumType && sum_type.source_type_id() != 0 {
			tc.named_sum_variant_names(sum_type.source_type_id())
		} else {
			[]string{}
		}
		sum_text := flat.source_type_name(variant_syntax.named_variant_owner())
		variant := flat.source_type_name(variant_syntax.named_variant_name())
		display := named_variant_init_display(variant_id)
		message := if names.len > 0 {
			'`${sum_text}` has no variant `${variant}`; its variants are: ${names.map('`${it}`').join(', ')}'
		} else if sum_type is SumType || sum_type is Struct || sum_type is Enum
			|| sum_type is Interface || sum_type is Alias {
			'`${sum_text}` has no named variant `${variant}`; only sum types declared with named variants, like `type ${sum_text} = ${variant}(int) | Other`, have them'
		} else {
			'unknown type `${sum_text}`, used in `${display}`'
		}
		tc.record_error_at(.unknown_type, message, id, node.pos)
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.register_synth_type(id, Type(Unknown{}))
		return false
	}
	if payload_type := tc.named_variant_payload_type(variant_id) {
		if node.children_count == 0 {
			display := named_variant_init_display(variant_id)
			tc.record_error_at(.assignment_mismatch, '`${display}` needs a payload of type `${flat.source_type_name(tc.type_name(payload_type))}`, e.g. `${display}(value)`',
				id, node.pos)
			tc.register_synth_type(id, variant_type)
			return false
		}
	} else if node.children_count > 0 {
		display := named_variant_init_display(variant_id)
		tc.record_error_at(.assignment_mismatch, '`${display}` has no payload; write it without a value: `${display}`',
			id, node.pos)
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.register_synth_type(id, variant_type)
		return false
	}
	return true
}

// named_variant_init_was_reported reports whether `id` is a lowered variant
// constructor whose error was already reported, so the surrounding cast to the
// sum type does not report it again.
fn (tc &TypeChecker) named_variant_init_was_reported(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	return node.kind == .struct_init && flat.source_type_syntax(node.source_value_id()).named_variant()
		&& tc.errors.any(it.node == id)
}
