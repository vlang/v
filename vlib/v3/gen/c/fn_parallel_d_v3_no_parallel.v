module c

import v3.flat

// gen_fns_dispatch emits all functions serially when v3 is built with the
// internal `v3_no_parallel` define.
fn (mut g FlatGen) gen_fns_dispatch(_ bool) {
	g.gen_fns()
	g.gen_synthetic_main_after_fns()
}

// run_pre_dispatch_parallel is serial-only in `v3_no_parallel` builds.
fn (mut g FlatGen) run_pre_dispatch_parallel(_ bool) bool {
	return false
}

// fn_item_cost_and_prep never pre-seeds in `v3_no_parallel` builds; it is only
// reachable with want_parallel_prep set, which nothing sets here.
fn (mut g FlatGen) fn_item_cost_and_prep(node_id flat.NodeId, mut _stack []flat.NodeId, mut _type_text_cache map[string]bool) int {
	return flat_fn_gen_item_cost(g.a, node_id)
}
