module ssa

import v.flat

fn (mut b Builder) intersect_sum_guards(other map[ValueID]string) {
	mut discard := []ValueID{}
	for addr, variant in b.sum_guard_variants {
		if other[addr] or { '' } != variant {
			discard << addr
		}
	}
	for addr in discard {
		b.sum_guard_variants.delete(addr)
	}
}

// preserve_sum_guard records the variant proved on a conditional control-flow edge.
// The transformer can leave fields in a negative guard typed as the original sum.
fn (mut b Builder) preserve_sum_guard(id flat.NodeId, truth bool) {
	if !b.valid_node_id(id) {
		return
	}
	node := b.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		b.preserve_sum_guard(b.a.child(&node, 0), truth)
		return
	}
	if node.kind == .prefix && node.op == .not && node.children_count > 0 {
		b.preserve_sum_guard(b.a.child(&node, 0), !truth)
		return
	}
	if node.kind == .is_expr && truth && node.children_count > 0 {
		b.preserve_sum_variant(b.a.child(&node, 0), node.value, 0)
		return
	}
	if node.kind != .infix || node.children_count < 2 {
		return
	}
	lhs := b.a.child(&node, 0)
	rhs := b.a.child(&node, 1)
	if (node.op == .logical_and && truth) || (node.op == .logical_or && !truth) {
		b.preserve_sum_guard(lhs, truth)
		b.preserve_sum_guard(rhs, truth)
		return
	}
	if !((node.op == .eq && truth) || (node.op == .ne && !truth)) {
		return
	}
	b.preserve_sum_tag_comparison(lhs, rhs)
	b.preserve_sum_tag_comparison(rhs, lhs)
}

fn (mut b Builder) preserve_sum_tag_comparison(selector_id flat.NodeId, tag_id flat.NodeId) {
	selector := b.a.nodes[int(selector_id)]
	tag := b.a.nodes[int(tag_id)]
	if selector.kind != .selector || selector.value != '__v_sum_type_tag__'
		|| selector.children_count == 0 || tag.kind != .int_literal {
		return
	}
	b.preserve_sum_variant(b.a.child(&selector, 0), '', tag.value.int())
}

fn (mut b Builder) preserve_sum_variant(expr_id flat.NodeId, variant_name string, tag int) {
	expr := b.a.nodes[int(expr_id)]
	if expr.kind == .paren && expr.children_count > 0 {
		b.preserve_sum_variant(b.a.child(&expr, 0), variant_name, tag)
		return
	}
	if expr.kind != .ident {
		return
	}
	addr := b.vars[expr.value] or { return }
	// Further checks cannot change a proved variant. A conflicting edge is
	// unreachable; retaining the fact keeps diagnostic tag scans from erasing it.
	if addr in b.sum_guard_variants {
		return
	}
	sum_name := b.sum_name_for_type_id(b.deref_type(addr)) or { return }
	if variant_name.len > 0 {
		if variant := b.find_sum_variant(sum_name, variant_name) {
			b.sum_guard_variants[addr] = variant
		}
		return
	}
	variants := b.sum_type_variants[sum_name] or { return }
	if tag > 0 && tag <= variants.len {
		b.sum_guard_variants[addr] = variants[tag - 1]
	}
}
