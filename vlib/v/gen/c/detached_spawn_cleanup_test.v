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
	cleanup := g.detached_spawn_result_cleanup(tc.parse_type(name), 'result', 0)
	assert cleanup.count('__v_thread_join') == 18, cleanup
}
