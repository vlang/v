module transform

import v3.flat
import v3.types

fn scan_literal_decl_flags_parallel(_ &Transformer, _ int, mut _ []u8, mut _ []u8) bool {
	return false
}

fn scan_top_level_kind_flags_parallel(_ &flat.FlatAst, _ int, mut _ []u8, _ bool) bool {
	return false
}

fn (mut _ Transformer) prepare_parallel_monomorph_scan(_ int, _ int) bool {
	return false
}

fn (mut _ Transformer) run_parallel_monomorphize_specs(_ []PendingGenericFnSpec, _ map[string]GenericStructDecl, _ map[string]GenericSumDecl, mut _ map[string]bool, mut _ []string) bool {
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
fn (mut t Transformer) scan_late_call_names_dispatch(cands []LateFnCandidate, used &map[string]bool, candidate_names &map[string]bool) []string {
	return t.scan_late_call_names_range(cands, used, candidate_names, 0, cands.len)
}

pub fn promote_scoped_texts_parallel(mut _ flat.FlatAst, _ voidptr) bool {
	return false
}

pub fn promote_scoped_checker_node_caches_parallel(mut _ types.TypeChecker, _ &flat.FlatAst, _ voidptr, _ int) bool {
	return false
}

pub fn scan_scoped_text_flags_parallel(_ &flat.FlatAst, _ voidptr, mut _ []u8) bool {
	return false
}

fn free_worker_scopes_parallel(_ &flat.FlatAst, _ []voidptr) bool {
	return false
}

// prepare_with_pre_scans keeps preparation serial when v3 is built with the
// internal `v3_no_parallel` define.
fn (mut t Transformer) prepare_with_pre_scans() {
	t.prepare()
}
