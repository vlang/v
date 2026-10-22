module c

import v.flat
import v.types

fn test_detached_spawn_cleanup_reaches_deep_thread_results() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	mut name := 'int'
	for _ in 0 .. 18 {
		name = 'thread ${name}'
	}
	cleanup := g.detached_spawn_result_cleanup(tc.parse_type(flat.source_type_id(name)), 'result', 0)
	assert cleanup.count('__v_thread_join') == 18, cleanup
}

fn test_detached_spawn_cleanup_uses_sum_type_and_variant_drops() {
	for sum_name in ['Box[[2]string]', 'model.Box[[2]string]'] {
		mut ast := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&ast)
		tc.sum_types.set(sum_name, ['[2]string', 'int'])
		tc.cur_module = 'builtin'
		tc.cur_file = 'builtin.v'
		mut g := FlatGen.new()
		g.a = &ast
		g.tc = &tc
		g.detached_spawn_drop = true
		sum_id := flat.source_type_id(sum_name)
		cleanup := g.detached_spawn_aggregate_cleanup(tc.parse_canonical_type(sum_id), 'result')
		generated := g.spawn_wrapper_defs.join('\n')
		helper_name := g.ownership_recursive_drop_helper_name(sum_id)
		sum_c_type := g.cname(sum_name)
		assert sum_c_type != g.struct_cname(sum_name)
		assert generated.count('(${sum_c_type}* _value)') == 2, generated
		assert generated.contains('switch ((*_value).typ)'), generated
		assert generated.contains('string__free('), generated
		// The helper expands its own sum once; the call site uses that helper.
		assert !generated.contains('${helper_name}(&(*_value))'), generated
		assert cleanup.contains('${helper_name}(&(result))'), cleanup
		assert tc.cur_module == 'builtin'
		assert tc.cur_file == 'builtin.v'
	}
}
