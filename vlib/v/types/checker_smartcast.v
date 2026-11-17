module types

import v.flat

// smartcast_type_name returns the source spelling ID of the type `id` was narrowed to by an enclosing
// `is` (or `!= none`) check, or zero when the expression is not smartcast.
// The transform lowers comparisons after checking, and has to see the same narrowed
// type the checker validated the expression with: an index or selector node keeps
// its declared type, so without this a smartcast sum type operand would still be
// compared with the sum type's equality helper.
pub fn (tc &TypeChecker) smartcast_type_name(id flat.NodeId) u32 {
	typ := tc.smartcast_type(id) or { return u32(0) }
	return tc.type_name(typ)
}

// smartcast_call_operand_type_name returns the source spelling ID of the type that
// a smartcast narrowed the receiver or argument `id` of a generic call to while
// the call was checked, or zero when it recorded none (see
// remember_smartcast_call_operands).
pub fn (tc &TypeChecker) smartcast_call_operand_type_name(id flat.NodeId) u32 {
	return tc.smartcast_call_operands[int(id)] or { u32(0) }
}

// remember_smartcast_call_operands records what the active smartcasts narrowed
// the receiver and the arguments of the generic call `node` to. A check
// monomorphizes the tree as it was parsed, where they still have their declared
// types, and once checking is over the enclosing blocks no longer tell every
// narrowing: not the one after an `!is` guard that returns, nor the one of a value
// that a closure captures.
fn (mut tc TypeChecker) remember_smartcast_call_operands(node flat.Node, info CallInfo) {
	if tc.smartcasts.len == 0 || node.children_count == 0 || !isnil(tc.storage_query_probe)
		|| !isnil(tc.fork_overlay) || tc.comptime_static_depth > 0
		|| !tc.call_may_ask_for_an_instance(info) {
		return
	}
	if info.has_receiver {
		if receiver_id := tc.generic_method_call_receiver_id(node) {
			tc.remember_smartcast_call_operand(receiver_id)
		}
	}
	for i in 1 .. node.children_count {
		tc.remember_smartcast_call_operand(tc.call_arg_value(tc.a.child(&node, i)))
	}
}

// call_may_ask_for_an_instance reports whether the call resolved to `info` is of a
// generic function, or of a method of a generic type (`Box[T].label`), whose
// receiver's type parameters it has.
fn (tc &TypeChecker) call_may_ask_for_an_instance(info CallInfo) bool {
	if tc.call_generic_params(info.name).len > 0 {
		return true
	}
	return info.has_receiver && info.name.all_before_last('.').contains('[')
}

fn (mut tc TypeChecker) remember_smartcast_call_operand(id flat.NodeId) {
	mut operand := id
	for tc.valid_node_id(operand) {
		node := tc.a.node(operand)
		if node.kind != .paren || node.children_count == 0 {
			break
		}
		operand = tc.a.child(node, 0)
	}
	if !tc.valid_node_id(operand) || tc.a.node(operand).kind !in [.ident, .selector, .index] {
		return
	}
	key := tc.expr_key(operand)
	if key.len == 0 || !valid_string_data(key) {
		return
	}
	typ := tc.smartcasts[key] or { return }
	name := tc.type_name(typ)
	if name != 0 && name != flat.source_type_void {
		tc.smartcast_call_operands[int(operand)] = name
	}
}
