module c

import v3.flat
import v3.types

// clone_cgen_string_list keeps stage-scoped results alive in serial-only builds.
fn clone_cgen_string_list(values []string) []string {
	mut cloned := []string{cap: values.len}
	for value in values {
		cloned << value.clone()
	}
	return cloned
}

// clone_parallel_type_checker returns the serial checker when worker codegen is
// compiled out. The call sites still use their scoped serial batching path, but
// no concurrent mutation requires a private checker view.
fn (g &FlatGen) clone_parallel_type_checker() &types.TypeChecker {
	return g.tc
}

// gen_fns_dispatch emits all functions serially when v3 is built with the
// internal `v3_no_parallel` define.
fn (mut g FlatGen) gen_fns_dispatch(_ bool) {
	g.gen_fns()
	g.gen_synthetic_main_after_fns()
}

// prepare_serial_fn_tables is unnecessary when parallel cgen is compiled out.
fn (mut g FlatGen) prepare_serial_fn_tables() {}

// run_pre_dispatch_parallel is serial-only in `v3_no_parallel` builds.
fn (mut g FlatGen) run_pre_dispatch_parallel(_ bool) bool {
	return false
}

// fn_item_cost_and_prep never pre-seeds in `v3_no_parallel` builds; it is only
// reachable with want_parallel_prep set, which nothing sets here.
fn (mut g FlatGen) fn_item_cost_and_prep(node_id flat.NodeId, mut _stack []flat.NodeId, mut _type_text_cache map[string]bool) int {
	return flat_fn_gen_item_cost(g.a, node_id)
}
