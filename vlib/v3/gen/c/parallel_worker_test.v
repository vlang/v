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
