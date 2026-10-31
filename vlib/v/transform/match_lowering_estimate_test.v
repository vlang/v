module transform

import v.flat
import v.types

// lowering_estimate_test_match appends a `match` with `branches` branches and
// returns its node.
fn lowering_estimate_test_match(mut a flat.FlatAst, branches int) flat.NodeId {
	mut children := []flat.NodeId{cap: branches + 1}
	children << a.add_node(flat.Node{
		kind:  .ident
		value: 'subject'
	})
	for i in 0 .. branches {
		children << a.add_node(flat.Node{
			kind:  .int_literal
			value: i.str()
		})
	}
	start := a.children.len
	for child in children {
		a.children << child
	}
	return a.add_node(flat.Node{
		kind:           .match_stmt
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn test_match_lowering_estimate_counts_the_branches_of_every_match_in_the_span() {
	mut a := flat.FlatAst.new()
	small := lowering_estimate_test_match(mut a, 3)
	lowering_estimate_test_match(mut a, 5000)
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})
	hi := a.nodes.len
	small_estimate := 4 * match_branch_lowering_estimate
	assert t.fn_span_match_lowering_estimate(0, int(small) + 1) == small_estimate
	assert t.fn_span_match_lowering_estimate(0, hi) == small_estimate +
		5001 * match_branch_lowering_estimate
	// A span without a `match`, an empty span, and bounds outside the AST.
	assert t.fn_span_match_lowering_estimate(0, int(small)) == 0
	assert t.fn_span_match_lowering_estimate(hi, hi) == 0
	assert t.fn_span_match_lowering_estimate(-8, hi + 8) == t.fn_span_match_lowering_estimate(0,
		hi)
}

// A worker region is a share of the append pool by cost. The pool has about 5/3
// of the child links of the AST, and lowering a branch of a dense `return match`
// (4 nodes) appends 7 child links, so the cost has to count more than the nodes.
fn test_match_lowering_estimate_covers_the_growth_of_a_dense_return_match() {
	nodes_per_branch := 4
	child_links_appended_per_branch := 7
	cost_per_branch := nodes_per_branch + match_branch_lowering_estimate
	assert cost_per_branch * 5 >= child_links_appended_per_branch * 3
	assert nodes_per_branch * 5 < child_links_appended_per_branch * 3
}
