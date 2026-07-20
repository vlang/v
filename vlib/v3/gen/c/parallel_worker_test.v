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

fn test_scoped_parallel_dispatch_worker_owns_string_snapshot() {
	mut g, _ := parallel_worker_test_gen(true)
	assert g.intern_string('source') == 0
	mut w := g.new_parallel_dispatch_worker(1)
	assert !w.str_lits_shared
	assert w.intern_string('worker generated') == 1
	assert g.str_lits == ['source']
	assert g.intern_string('master generated') == 1
	assert w.str_lits == ['source', 'worker generated']
	assert g.str_lits == ['source', 'master generated']
}

fn test_scoped_parallel_worker_reuses_preselected_functions_and_c_extern_refs() {
	mut g, _ := parallel_worker_test_gen(true)
	g.fn_gen_items = [FlatFnGenItem{
		c_name: 'main__run'
	}]
	g.c_extern_refs['puts'] = true
	g.c_extern_refs_ready = true

	w := g.new_parallel_worker(1)
	assert w.fn_gen_items.len == 1
	assert w.fn_gen_items[0].c_name == 'main__run'
	assert w.c_extern_refs == {
		'puts': true
	}
	assert w.c_extern_refs_ready
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

fn test_parallel_checker_clone_keeps_checked_file_scope_identity() {
	g, mut tc := parallel_worker_test_gen(true)
	tc.file_scope.insert('file_value', types.Type(types.int_))
	w := g.clone_parallel_type_checker()
	assert w.file_scope == tc.file_scope
	assert w.cur_scope != w.file_scope
	owner := w.cur_scope.lookup_owner('file_value') or { panic('missing file binding') }
	assert owner.belongs_to_scope(w.file_scope)
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

fn test_scoped_cgen_worker_merge_publishes_generated_literals() {
	mut g, _ := parallel_worker_test_gen(true)
	assert g.intern_string('source') == 0

	mut helper := g.new_parallel_dispatch_worker(1)
	mut helper_batch := helper.new_parallel_worker(0)
	assert helper_batch.intern_string('helper generated') == 1
	helper_batch.sb.write_string('helper(_str_1); "_str_1"; /* _str_1 */')
	helper_batch.add_spawn_wrapper_def('spawn_helper(_str_1);')
	helper.absorb_scoped_cgen_batch(helper_batch, false)

	mut master_batch := g.new_parallel_worker(0)
	assert master_batch.intern_string('master generated') == 1
	master_batch.sb.write_string('master(_str_1);')
	g.absorb_scoped_cgen_batch(master_batch, false)
	g.merge_parallel_worker(helper)

	assert g.str_lits == ['source', 'master generated', 'helper generated']
	assert g.str_lit_ids['helper generated'] == 2
	assert g.fn_segs == ['master(_str_1);', 'helper(_str_2); "_str_1"; /* _str_1 */']
	assert g.spawn_wrapper_defs == ['spawn_helper(_str_2);']
}

fn test_scoped_cgen_string_remap_preserves_user_c_identifiers() {
	mut g, _ := parallel_worker_test_gen(true)
	g.c_extern_refs['_str_999'] = true
	g.c_extern_refs_ready = true
	user_c_symbols := g.cache_user_c_string_symbols()
	remap := {
		1:   2
		999: 1000
	}
	source := 'helper(_str_1); _str_999(); "_str_1"; /* _str_999 */'
	assert remap_scoped_worker_string_symbols(source, remap, user_c_symbols) == 'helper(_str_2); _str_999(); "_str_1"; /* _str_999 */'
}

fn test_fused_parallel_prep_interns_body_string_literals() {
	mut g, _ := parallel_worker_test_gen(false)
	g.a.nodes = [
		flat.Node{
			kind:           .fn_decl
			children_start: 0
			children_count: 1
		},
		flat.Node{
			kind:  .string_literal
			value: 'worker literal'
		},
	]
	g.a.children = [flat.NodeId(1)]
	mut stack := []flat.NodeId{}
	mut type_text_cache := map[string]bool{}
	g.fn_item_cost_and_prep(0, mut stack, mut type_text_cache)
	assert g.str_lits == ['worker literal']
	assert g.str_lit_ids['worker literal'] == 0
}
