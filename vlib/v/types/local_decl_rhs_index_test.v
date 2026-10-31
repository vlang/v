module types

import os
import v.flat
import v.parser
import v.pref
import v.token

fn local_rhs_test_node(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.begin_children()
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		children_start: start
		children_count: i32(children.len)
	})
}

fn local_rhs_test_decl(mut a flat.FlatAst, name string, offset int, value string) (flat.NodeId, flat.NodeId) {
	lhs := a.add_node(flat.Node{
		kind:  .ident
		value: name
		pos:   token.new_pos(0, offset)
	})
	rhs := a.add_val(.int_literal, value)
	return local_rhs_test_node(mut a, .decl_assign, [lhs, rhs]), rhs
}

fn local_rhs_test_use(mut a flat.FlatAst, name string, file int, offset int) flat.NodeId {
	return a.add_node(flat.Node{
		kind:  .ident
		value: name
		pos:   token.new_pos(file, offset)
	})
}

fn test_local_decl_rhs_index_matches_fallback_lookup() {
	mut a := flat.FlatAst.new()
	before := local_rhs_test_use(mut a, 'value', 0, 10)
	first_decl, first_rhs := local_rhs_test_decl(mut a, 'value', 20, '1')
	first_use := local_rhs_test_use(mut a, 'value', 0, 30)
	shadow_decl, shadow_rhs := local_rhs_test_decl(mut a, 'value', 40, '2')
	shadow_use := local_rhs_test_use(mut a, 'value', 0, 50)
	block := local_rhs_test_node(mut a, .block, [shadow_decl, shadow_use])
	closure_decl, _ := local_rhs_test_decl(mut a, 'value', 60, '3')
	hidden_decl, _ := local_rhs_test_decl(mut a, 'hidden', 70, '4')
	closure := local_rhs_test_node(mut a, .fn_literal, [closure_decl, hidden_decl])
	lambda_decl, _ := local_rhs_test_decl(mut a, 'value', 80, '5')
	lambda := local_rhs_test_node(mut a, .lambda_expr, [lambda_decl])
	after := local_rhs_test_use(mut a, 'value', 0, 90)
	hidden_use := local_rhs_test_use(mut a, 'hidden', 0, 90)
	other_file := local_rhs_test_use(mut a, 'value', 1, 90)
	same_offset := local_rhs_test_use(mut a, 'value', 0, 40)
	fn_id := local_rhs_test_node(mut a, .fn_decl, [before, first_decl, first_use, block, closure,
		lambda, after, hidden_use, other_file, same_offset])
	uses := [before, first_use, shadow_use, after, hidden_use, other_file, same_offset]
	expected := [flat.empty_node, first_rhs, shadow_rhs, shadow_rhs, flat.empty_node, flat.empty_node,
		first_rhs]
	// Exercise the fallback, the tree index, and the contiguous work-item index.
	for mode in 0 .. 3 {
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		tc.fn_context.node_id = int(fn_id)
		if mode == 2 {
			tc.check_range_lo = 0
			tc.check_range_hi = int(fn_id)
		}
		if mode > 0 {
			tc.index_local_decl_rhs(fn_id)
		}
		for i, use_id in uses {
			name := a.node(use_id).value
			assert tc.local_decl_rhs_before(name, use_id) or { flat.empty_node } == expected[i]
		}
		assert tc.local_decl_rhs_before('missing', after) == none
		assert tc.local_decl_rhs_before('', after) == none
		assert tc.local_decl_rhs_before('value', flat.empty_node) == none
	}
}

fn test_self_host_full_resolution_checks_dynamic_method_local() {
	path := os.join_path(os.vtmp_dir(), 'v3_local_rhs_full_resolution_${os.getpid()}.v')
	os.write_file(path, 'struct Target {}
fn (t Target) run() {}
fn main() {
	method := 42
	Target{}.\$method()
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	for building_v in [false, true] {
		mut tc := TypeChecker.new(a)
		tc.building_v_fast = building_v
		tc.valid_resolution_fast = false
		tc.collect(a)
		tc.check_semantics_opt(false)
		assert tc.errors.any(it.msg == 'invalid string method call: expected `string`, not `int`'), tc.errors.str()
		assert !tc.errors.any(it.msg == 'unknown identifier `method`'), tc.errors.str()
	}
}
