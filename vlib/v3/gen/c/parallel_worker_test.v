module c

import v3.flat
import v3.types

fn parallel_worker_test_gen(scoped bool) (&FlatGen, &types.TypeChecker) {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.scope_parallel_workers = scoped
	return &g, &tc
}

fn test_parallel_dispatch_worker_owns_checker_outside_scoped_batching() {
	g, tc := parallel_worker_test_gen(false)
	w := g.new_parallel_dispatch_worker(1)
	assert w.tc != tc
}

fn test_parallel_dispatch_worker_shares_checker_as_scoped_accumulator() {
	g, tc := parallel_worker_test_gen(true)
	w := g.new_parallel_dispatch_worker(1)
	assert w.tc == tc
}

fn test_parallel_checker_clone_preserves_sparse_transform_caches() {
	g, mut tc := parallel_worker_test_gen(false)
	tc.a.nodes = [flat.Node{
		kind: .ident
	}, flat.Node{
		kind: .ident
	}]
	tc.resolved_call_names = ['source_call']
	tc.resolved_call_set = [true]
	tc.expr_type_values = [types.Type(types.int_)]
	tc.expr_type_set = [true]
	tc.begin_sparse_transform_node_caches(1)
	tc.sparse_resolved_call_names[1] = 'transformed_call'
	tc.sparse_expr_type_values[1] = types.Type(types.String{})

	w := g.clone_parallel_type_checker()
	assert w.parallel_check_sparse
	assert w.check_range_lo == 0
	assert w.check_range_hi == 0
	assert w.resolved_call_name(flat.NodeId(0)) or { '' } == 'source_call'
	assert w.resolved_call_name(flat.NodeId(1)) or { '' } == 'transformed_call'
	assert w.expr_type(flat.NodeId(0)) or { types.Type(types.void_) } is types.Primitive
	assert w.expr_type(flat.NodeId(1)) or { types.Type(types.void_) } is types.String
	assert g.parallel_cached_expr_type(flat.NodeId(0), tc.a.nodes[0]) or { types.Type(types.void_) } is types.Primitive
	assert g.parallel_cached_expr_type(flat.NodeId(1), tc.a.nodes[1]) or { types.Type(types.void_) } is types.String
}

fn test_scoped_cgen_batch_preserves_worker_interned_literals() {
	mut g, _ := parallel_worker_test_gen(true)
	assert g.intern_string('source') == 0
	for generated in ['generated_a', 'generated_b'] {
		mut batch := g.new_parallel_worker(0)
		generated_id := batch.intern_string(generated)
		assert generated_id == g.str_lits.len
		g.absorb_scoped_cgen_batch(batch, false)
		assert g.str_lits[generated_id] == generated
		assert g.str_lit_ids[generated] == generated_id
	}
	assert g.str_lits == ['source', 'generated_a', 'generated_b']
}
