module types

import v.flat

// warn_unhandled_result_calls warns about the calls that return a Result whose
// error nothing handles, where the checker reports nothing else: a call made as
// a statement, the argument of a function that takes any type (as `println`), a
// part of a string interpolation, an element of an array literal, or a value
// assigned to `_`. The error is lost there: the program runs on, or prints the
// Result as if it were the value. `!` at the end passes it on, and an `or {}`
// block handles it, as does `if x := call() {`.
pub fn (mut tc TypeChecker) warn_unhandled_result_calls() {
	saved_file := tc.cur_file
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.kind != .call {
			continue
		}
		id := flat.NodeId(idx)
		typ := tc.expr_type(id) or { continue }
		if unalias_type(typ) !is ResultType {
			continue
		}
		unhandled, deferred := tc.unhandled_result_spot(id, node)
		if !unhandled || tc.declaration_contains_error(node) {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		tc.cur_file = file.name
		hint := if deferred {
			// `!` cannot pass an error out of a `defer`
			'so it should have an `or {}` block at the end'
		} else {
			'so it should have either an `or {}` block, or `!` at the end'
		}
		tc.record_warning_at(.call_arg_mismatch, '${tc.call_display_name(node)}() returns `${typ.name()}`, ${hint}',
			id, tc.wrapped_operand_diagnostic_pos(id))
	}
	tc.cur_file = saved_file
}

// unhandled_result_spot tells whether the call `id`, which returns a Result, is
// one whose error nothing handles and that the checker says nothing else about
// (see warn_unhandled_result_calls), and whether it is inside a `defer`.
fn (tc &TypeChecker) unhandled_result_spot(id flat.NodeId, call flat.Node) (bool, bool) {
	if tc.call_has_postfix_propagation(call) {
		return false, false
	}
	mut child := id
	mut parent_id := tc.direct_parent_id(child)
	for tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .paren {
		child = parent_id
		parent_id = tc.direct_parent_id(child)
	}
	if !tc.valid_node_id(parent_id) {
		return false, false
	}
	parent := tc.a.node(parent_id)
	match parent.kind {
		.expr_stmt {
			return true, tc.is_in_defer(parent_id)
		}
		.string_interp, .array_literal, .dump_expr {
			return true, false
		}
		.call {
			// An argument, not the function that is called. A parameter of a type
			// makes that an error, reported on the call or on the argument.
			return parent.children_count > 0 && tc.a.child(parent, 0) != child
				&& !tc.declaration_contains_error(parent), false
		}
		.assign {
			// `_ = call()`
			for i := 0; i + 1 < parent.children_count; i += 2 {
				if tc.a.child(parent, i + 1) == child {
					lhs := tc.a.child_node(parent, i)
					return lhs.kind == .ident && lhs.value == '_', false
				}
			}
			return false, false
		}
		else {
			return false, false
		}
	}
}

// is_in_defer reports whether the node `id` is in the body of a `defer` of the
// function around it.
fn (tc &TypeChecker) is_in_defer(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 256 {
		current = tc.direct_parent_id(current)
		if !tc.valid_node_id(current) {
			return false
		}
		kind := tc.a.node(current).kind
		if kind == .defer_stmt {
			return true
		}
		if kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
	}
	return false
}
