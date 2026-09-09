module transform

import v3.flat

// match_has_sum_type_subject checks if a match_stmt's subject expression
// resolves to a sum type. This determines whether the match needs to be
// lowered with is_expr conditions (sum type dispatch) or plain equality
// checks (value matching).
fn (mut t Transformer) match_has_sum_type_subject(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	subject_id := t.a.child(&node, 0)
	subject_type := t.resolve_expr_type(subject_id)
	if subject_type.len == 0 {
		return false
	}
	return subject_type in t.sum_types
}

// resolve_match_subject_type determines the type of a match subject expression.
// Uses resolve_expr_type on child 0 (the match expression).
fn (mut t Transformer) resolve_match_subject_type(node flat.Node) string {
	if node.children_count == 0 {
		return ''
	}
	subject_id := t.a.child(&node, 0)
	return t.resolve_expr_type(subject_id)
}

// transform_match_branch_body transforms a match branch body with smartcast
// awareness. When the branch matches a sum type variant, the subject variable
// should be narrowed to that variant's type within the branch body.
//
// For now, this performs basic body transformation without smartcast injection.
// When smartcast support is fully wired up, this will:
//   1. Push a smartcast context for the subject variable narrowed to the variant
//   2. Transform the body statements under that context
//   3. Pop the smartcast context
//   4. Rebuild the block with the transformed statements
fn (mut t Transformer) transform_match_branch_body(branch_id flat.NodeId, subject_name string, variant_name string) flat.NodeId {
	if int(branch_id) < 0 {
		return branch_id
	}
	branch := t.a.nodes[int(branch_id)]
	if branch.children_count == 0 {
		return branch_id
	}
	// Collect the body statement ids from the branch
	mut body_ids := []flat.NodeId{}
	// Determine where the body starts (after the condition values)
	n_conds := t.count_conds(branch)
	for i in n_conds .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	if body_ids.len == 0 {
		return branch_id
	}
	// Push smartcast if we have a sum type variant match
	mut has_sc := subject_name.len > 0 && variant_name.len > 0
	if has_sc {
		sum_type_name := t.find_sum_type_for_variant(variant_name)
		if sum_type_name.len > 0 {
			t.push_smartcast(subject_name, variant_name, sum_type_name)
		} else {
			has_sc = false
		}
	}
	// Transform the body statements under smartcast context
	new_body := t.transform_stmts(body_ids)
	if has_sc {
		t.pop_smartcast()
	}
	return t.make_block(new_body)
}
