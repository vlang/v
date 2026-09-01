module c

import v3.flat
import time

fn (mut g FlatGen) refine_fn_item_costs(_ bool, _ bool) {}

fn par_cgen_prep_enabled() bool {
	return false
}

fn (mut g FlatGen) scan_collect_gen_info(_ bool) CollectGenInfoScanCounts {
	return g.scan_collect_gen_info_serial()
}

fn (mut g FlatGen) apply_fn_signature_registrations(registrations []FnSignatureRegistration) {
	for group in 0 .. 4 {
		for registration in registrations {
			g.apply_fn_signature_registration_group(registration, group)
		}
	}
}

fn (mut g FlatGen) prepare_shared_sum_and_fixed_array_ret_wrappers(_ bool) bool {
	mut sw := time.new_stopwatch()
	g.collect_shared_type_names()
	g.precompute_sum_name_lookup()
	if !g.skip_generics {
		g.precompute_generic_method_candidate_index()
	}
	g.timing_profile('  [ttime]     wr shared+sum  ${f64(sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	sw.restart()
	g.populate_fixed_array_ret_wrappers()
	g.timing_profile('  [ttime]     wr fixed ret   ${f64(sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	return false
}

fn (mut g FlatGen) collect_gen_info_fn_preps(_ []int, _ bool) []CollectGenFnPrep {
	return []CollectGenFnPrep{}
}

fn (mut g FlatGen) collect_fn_gen_candidates_parallel(direct_array_access_fns DirectArrayAccessFns, ignore_overflow_fns DirectArrayAccessFns, program_modules map[string]bool) []FlatFnGenCandidate {
	nodes := g.top_level_nodes()
	return g.collect_fn_gen_candidates_range(nodes, 0, nodes.len, '', '', direct_array_access_fns,
		ignore_overflow_fns, program_modules)
}

// gen_fns_dispatch emits all functions serially when v3 is built with the
// internal `v3_no_parallel` define.
fn (mut g FlatGen) gen_fns_dispatch(_ bool) {
	g.gen_test_failure_global()
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

fn (mut g FlatGen) fn_item_cost_and_c_extern_prep(node_id flat.NodeId, mut _stack []flat.NodeId) int {
	return flat_fn_gen_item_cost(g.a, node_id)
}
