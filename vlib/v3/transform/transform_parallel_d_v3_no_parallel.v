module transform

import v3.flat

fn scan_literal_decl_flags_parallel(_ &flat.FlatAst, _ int, mut _ []u8) bool {
	return false
}

fn scan_top_level_kind_flags_parallel(_ &flat.FlatAst, _ int, mut _ []u8) bool {
	return false
}

// collect_interface_boxed_types_parallel keeps the interface scan serial when
// v3 is built with the internal `v3_no_parallel` define.
fn (mut t Transformer) collect_interface_boxed_types_parallel() bool {
	return false
}

// run_parallel_transform falls back to the serial transform when v3 is built
// with the internal `v3_no_parallel` define.
fn (mut t Transformer) run_parallel_transform(items []FnWorkItem, _ int, _ int) bool {
	t.transform_pure_items_serial(items)
	return false
}

// scan_late_call_names_dispatch falls back to the serial late-name scan when
// v3 is built with the internal `v3_no_parallel` define.
fn (mut t Transformer) scan_late_call_names_dispatch(cands []LateFnCandidate, used map[string]bool) []string {
	return t.scan_late_call_names_range(cands, used, 0, cands.len)
}
