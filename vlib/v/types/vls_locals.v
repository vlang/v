module types

import v.flat

// vls_local_declaration finds the node that declares the local name the
// identifier `id` uses: the identifier on the left of a `:=`, a variable of a
// `for ... in`, of an `if x := ...` guard or of a closure's capture list, or a
// parameter. It walks out from the use, through the statements before it in
// each enclosing block, as the scopes of the checker did while checking.
fn (tc &TypeChecker) vls_local_declaration(id flat.NodeId) ?flat.NodeId {
	name := tc.a.nodes[int(id)].value
	use_offset := int(tc.a.nodes[int(id)].pos.offset)
	mut child := id
	mut parent := tc.direct_parent_id(child)
	for _ in 0 .. 4096 {
		if !tc.valid_node_id(parent) {
			return none
		}
		p := tc.a.node(parent)
		match p.kind {
			.block {
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return found
				}
			}
			.for_in_stmt {
				// The loop variables are the identifiers before the container.
				for i in 0 .. p.children_count {
					c := tc.a.child(p, i)
					if c == child || i >= 2 {
						break
					}
					if !tc.valid_node_id(c) {
						continue
					}
					cn := tc.a.node(c)
					if cn.kind == .ident && cn.value == name {
						return c
					}
				}
			}
			.for_stmt {
				// The variable a C-style loop declares, `for i := 0; ...`, exists
				// in its condition, its step and its body.
				if p.value == 'c_style' && p.children_count > 0 && tc.a.child(p, 0) != child {
					init := tc.a.child(p, 0)
					if tc.valid_node_id(init) && tc.a.node(init).kind == .decl_assign {
						if found := tc.vls_assigned_name(init, name) {
							return found
						}
					}
				}
			}
			.if_expr {
				// The variables of an `if x := ...` guard exist in its first branch.
				if p.children_count > 1 && tc.a.child(p, 1) == child {
					cond := tc.a.child_node(p, 0)
					if cond.kind == .decl_assign {
						if found := tc.vls_assigned_name(tc.a.child(p, 0), name) {
							return found
						}
					}
				}
			}
			.fn_decl, .fn_literal, .lambda_expr {
				// A function's statements are its own children, after its
				// parameters.
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return found
				}
				for i in 0 .. p.children_count {
					c := tc.a.child(p, i)
					cn := tc.a.node(c)
					if cn.kind == .param && cn.value == name {
						return c
					}
				}
				// A closure's capture list names a variable of the enclosing
				// function: the search goes on there, as V1's did.
				if p.kind == .fn_decl {
					return none
				}
			}
			.file {
				return none
			}
			else {
				// Statements directly under a function body live in its block.
				if p.kind in [.or_expr, .match_branch, .comptime_if] {
					if found := tc.vls_declared_before(p, child, name, use_offset) {
						return found
					}
				}
			}
		}
		child = parent
		parent = tc.direct_parent_id(child)
	}
	return none
}

// vls_declared_before looks through the statements of `block` that come before
// its child `child` for the latest `:=` that declares `name`.
fn (tc &TypeChecker) vls_declared_before(block &flat.Node, child flat.NodeId, name string, use_offset int) ?flat.NodeId {
	mut found := ?flat.NodeId(none)
	for i in 0 .. block.children_count {
		c := tc.a.child(block, i)
		if c == child {
			break
		}
		cn := tc.a.node(c)
		if int(cn.pos.offset) > use_offset {
			break
		}
		if cn.kind == .decl_assign {
			if decl := tc.vls_assigned_name(c, name) {
				found = decl
			}
		}
	}
	return found
}

// vls_declared_here is where the identifier `id` is declared when it is itself
// the name a local declaration introduces: the left of a `:=`, or a variable
// of a `for ... in`. VLS asks for the declaration of every occurrence of a name
// it renames, the declaring one too.
fn (tc &TypeChecker) vls_declared_here(id flat.NodeId) ?VlsPos {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return none
	}
	parent := tc.a.node(parent_id)
	node := tc.a.node(id)
	mut declares := false
	if parent.kind == .decl_assign {
		if decl := tc.vls_assigned_name(parent_id, node.value) {
			declares = decl == id
		}
	} else if parent.kind == .for_in_stmt && parent.children_count > 2 {
		// The key and the value come before the container.
		declares = tc.a.child(parent, 0) == id || tc.a.child(parent, 1) == id
	}
	if !declares {
		return none
	}
	return VlsPos{int(node.pos.id), int(node.pos.offset)}
}

// vls_assigned_name returns the identifier that the `:=` node `id` declares
// with the name `name`: one on its left, not a use of `name` on its right.
fn (tc &TypeChecker) vls_assigned_name(id flat.NodeId, name string) ?flat.NodeId {
	for c in tc.multi_assign_lhs_ids(tc.a.node(id)) {
		cn := tc.a.node(c)
		if cn.kind == .ident && cn.value == name {
			return c
		}
	}
	return none
}
