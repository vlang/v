module flat

// free_method_receiver_calls finds calls spelled free(receiver) in a free method.
// Callers must resolve the callee to distinguish builtin free from selectively imported functions.
// Calls inside nested functions do not release the receiver when the method itself is called.
pub fn (a &FlatAst) free_method_receiver_calls(id NodeId) []NodeId {
	mut calls := []NodeId{}
	if int(id) < 0 || int(id) >= a.nodes.len {
		return calls
	}
	node := a.node(id)
	if node.kind != .fn_decl || !node.value.ends_with('.free') || node.children_count == 0 {
		return calls
	}
	receiver := a.child_node(node, 0)
	if receiver.kind != .param || receiver.op != .dot || receiver.value.len == 0 {
		return calls
	}
	for i in 1 .. node.children_count {
		a.collect_free_method_receiver_calls(a.child(node, i), receiver.value, mut calls)
	}
	return calls
}

fn (a &FlatAst) collect_free_method_receiver_calls(id NodeId, receiver_name string, mut calls []NodeId) {
	node := a.node(id)
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return
	}
	if node.kind == .call && node.children_count == 2 {
		callee := a.child_node(node, 0)
		if callee.kind == .ident && callee.value in ['free', 'builtin.free']
			&& a.free_method_matches_receiver(a.child(node, 1), receiver_name) {
			calls << id
		}
	}
	for i in 0 .. node.children_count {
		a.collect_free_method_receiver_calls(a.child(node, i), receiver_name, mut calls)
	}
}

fn (a &FlatAst) free_method_matches_receiver(id NodeId, receiver_name string) bool {
	node := a.node(id)
	return match node.kind {
		.ident { node.value == receiver_name }
		.cast_expr, .paren {
			node.children_count == 1 && a.free_method_matches_receiver(a.child(node, 0), receiver_name)
		}
		else { false }
	}
}
