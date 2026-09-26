module types

import v.flat

// VlsBinding is what a local name stands for where it is used: the node that
// declares it, or, for a variable the language declares, where that comes from
// (see vls_implicit_binding).
struct VlsBinding {
	decl_id  flat.NodeId
	implicit bool
	at       VlsPos
}

// vls_local_declaration finds the node that declares the local name the
// identifier `id` uses, if the code declares it (see vls_local_binding).
fn (tc &TypeChecker) vls_local_declaration(id flat.NodeId) ?flat.NodeId {
	binding := tc.vls_local_binding(id)?
	return if binding.implicit { none } else { binding.decl_id }
}

// vls_local_binding finds what the local name the identifier `id` uses stands
// for: the identifier on the left of a `:=`, a variable of a `for ... in`, of
// an `if x := ...` guard or of a closure's capture list, a parameter, or a
// variable the language declares, `err`, `it`, `a` or `b`. It walks out from
// the use, through the statements before it in each enclosing block, as the
// scopes of the checker did while checking: the nearest one is the one.
fn (tc &TypeChecker) vls_local_binding(id flat.NodeId) ?VlsBinding {
	name := tc.a.nodes[int(id)].value
	use_offset := int(tc.a.nodes[int(id)].pos.offset)
	mut child := id
	mut parent := tc.vls_parent_id(child)
	for _ in 0 .. 4096 {
		if !tc.valid_node_id(parent) {
			return none
		}
		p := tc.a.node(parent)
		match p.kind {
			.block {
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return VlsBinding{
						decl_id: found
					}
				}
			}
			.for_in_stmt {
				// The statements of the body are children of the loop, after the
				// variables, the container and the end of a range, as many as
				// its value counts. A local of the body before the use comes
				// first, then the loop variables, which exist in the body only.
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return VlsBinding{
						decl_id: found
					}
				}
				header := p.value.int()
				mut in_body := false
				for i in header .. p.children_count {
					if tc.a.child(p, i) == child {
						in_body = true
						break
					}
				}
				if in_body {
					for i in 0 .. 2 {
						c := tc.a.child(p, i)
						if tc.valid_node_id(c) && tc.a.node(c).kind == .ident
							&& tc.a.node(c).value == name {
							return VlsBinding{
								decl_id: c
							}
						}
					}
				}
			}
			.for_stmt {
				// The statements of the body are children of the loop, and so is
				// the variable a C-style loop declares, `for i := 0; ...`, which
				// exists in its condition, its step and its body.
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return VlsBinding{
						decl_id: found
					}
				}
			}
			.if_expr {
				// The variables of an `if x := ...` guard exist in its first branch.
				if p.children_count > 1 && tc.a.child(p, 1) == child {
					cond := tc.a.child_node(p, 0)
					if cond.kind == .decl_assign {
						if found := tc.vls_assigned_name(tc.a.child(p, 0), name) {
							return VlsBinding{
								decl_id: found
							}
						}
					}
				}
			}
			.fn_decl, .fn_literal, .lambda_expr {
				// A function's statements are its own children, after its
				// parameters.
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return VlsBinding{
						decl_id: found
					}
				}
				for i in 0 .. p.children_count {
					c := tc.a.child(p, i)
					cn := tc.a.node(c)
					if cn.kind == .param && cn.value == name {
						return VlsBinding{
							decl_id: c
						}
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
				// Statements that are children of their node, not of a block: those
				// of `or {}`, of a `match` branch, of `$if`, `defer` or a `select`
				// branch. The node of an expression has no `:=` among them.
				if found := tc.vls_declared_before(p, child, name, use_offset) {
					return VlsBinding{
						decl_id: found
					}
				}
			}
		}
		if at := tc.vls_implicit_binding(parent, p, child, name) {
			return VlsBinding{
				implicit: true
				at:       at
			}
		}
		child = parent
		parent = tc.vls_parent_id(child)
	}
	return none
}

// vls_implicit_binding is where a variable the language declares comes from,
// when the node `p`, the parent of `child`, declares it there with the name
// `name`: `err` in an `or {}` block and in the `else` of an `if x := f()`, from
// the `{` of the block, and `it`, or `a` and `b`, in the argument of `.map()`,
// `.filter()`, `.any()`, `.all()` and `.count()`, or of `.sort()` and
// `.sorted()`, from the name of the method, as V1 answered.
fn (tc &TypeChecker) vls_implicit_binding(p_id flat.NodeId, p &flat.Node, child flat.NodeId, name string) ?VlsPos {
	if name == 'err' && p.kind == .block {
		owner_id := tc.vls_parent_id(p_id)
		if !tc.valid_node_id(owner_id) {
			return none
		}
		owner := tc.a.node(owner_id)
		from_or := owner.kind == .or_expr && owner.children_count > 1
			&& tc.a.child(owner, 1) == p_id
		from_else := owner.kind == .if_expr && owner.children_count > 2
			&& tc.a.child(owner, 2) == p_id && tc.a.child_node(owner, 0).kind == .decl_assign
		if from_or || from_else {
			return VlsPos{int(p.pos.id), int(p.pos.offset)}
		}
		return none
	}
	if name !in ['it', 'a', 'b'] || p.kind != .call || p.children_count < 2
		|| tc.a.child(p, 0) == child {
		return none
	}
	methods := if name == 'it' {
		['filter', 'map', 'any', 'all', 'count']
	} else {
		['sort', 'sorted']
	}
	callee := tc.a.child_node(p, 0)
	if callee.kind == .selector && callee.value in methods {
		return VlsPos{int(callee.pos.id), int(callee.pos.end) - callee.value.len}
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
		// The placeholder of a loop without an init or a step, which the
		// parser may add after the body.
		if !tc.valid_node_id(c) || tc.a.node(c).kind == .empty {
			continue
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

// vls_parent_id is the node that holds `id`. The bounds of a slice, `s[a..b]`,
// are children of the slice and of a `.range` node that the parser builds
// first and leaves out of the tree: the index of parents may name that range,
// and a walk up from a bound would stop there. The slice is the parent then.
fn (tc &TypeChecker) vls_parent_id(id flat.NodeId) flat.NodeId {
	parent := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent) || tc.a.node(parent).kind != .range
		|| tc.valid_node_id(tc.direct_parent_id(parent)) {
		return parent
	}
	for idx in tc.a.user_code_start .. tc.a.nodes.len {
		node := tc.a.nodes[idx]
		if node.kind != .index || node.value != 'range' {
			continue
		}
		for i in 1 .. node.children_count {
			if tc.a.child(&node, i) == id {
				return flat.NodeId(idx)
			}
		}
	}
	return parent
}

// vls_declared_here is where the identifier `id` is declared when it is itself
// the name a local declaration introduces: the left of a `:=`, or a variable
// of a `for ... in`. VLS asks for the declaration of every occurrence of a name
// it renames, the declaring one too.
fn (tc &TypeChecker) vls_declared_here(id flat.NodeId) ?VlsPos {
	parent_id := tc.vls_parent_id(id)
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
