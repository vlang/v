module transform

import v3.flat
import v3.types

// arr1 supports arr1 handling for transform.
fn arr1(a flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	return r
}

// arr2 supports arr2 handling for transform.
fn arr2(a flat.NodeId, b flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	return r
}

// arr3 supports arr3 handling for transform.
fn arr3(a flat.NodeId, b flat.NodeId, c flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	r << c
	return r
}

// arr4 supports arr4 handling for transform.
fn arr4(a flat.NodeId, b flat.NodeId, c flat.NodeId, d flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	r << c
	r << d
	return r
}

// node_kind_id supports node kind id handling for transform.
fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

// option_unwrap_marker tags a SmartcastContext produced by an `x != none`
// condition: variant_name holds the option's base type and the access is
// lowered to the option's `.value` field instead of a sum union field.
pub const option_unwrap_marker = '?opt'

// SumEqRequest records where a sum type's equality helper was first requested,
// so the helper body is built under that module/file resolution context. The
// helper module can differ for program-specific generic specializations, whose
// generated functions and helpers must stay in the main cache segment.
pub struct SumEqRequest {
pub:
	module        string
	file          string
	helper_module string
}

// SmartcastContext stores smartcast context state used by transform.
pub struct SmartcastContext {
pub:
	expr_name     string // the expression being smartcast (e.g. "node")
	variant_name  string // the variant type name (e.g. "Ident")
	sum_type_name string // the parent sum type name (e.g. "Expr")
}

// Transformer represents transformer data used by transform.
pub struct Transformer {
mut:
	a                            &flat.FlatAst      = unsafe { nil }
	tc                           &types.TypeChecker = unsafe { nil }
	structs                      map[string]StructInfo
	unique_fields                map[string]string
	alias_methods                map[string]string
	globals                      map[string]string
	sum_types                    map[string][]string
	sum_variant_parents          map[string][]string
	sum_variant_names            map[string]bool
	sum_variant_fields           map[string]string
	qualified_types              map[string]string
	fn_ret_types                 map[string]string
	receiver_method_suffix_index map[string]string
	const_suffixes               map[string]string
	enum_types                   map[string][]string
	enum_backing_types           map[string]string
	cur_file                     string
	cur_module                   string
	cur_fn_name                  string
	cur_fn_ret_type              string
	cur_fn_is_generic            bool
	skip_generics                bool
	var_types                    []VarTypeBinding
	mut_param_values             map[string]bool
	mut_value_ident_nodes        map[int]bool
	pointer_value_lvalues        map[string]bool
	pointer_value_rvalues        map[string]bool
	temp_counter                 int
	pending_stmts                []flat.NodeId
	smartcast_stack              []SmartcastContext
	invalidated_smartcasts       map[string]bool
	in_call_callee               bool
	in_monomorphize_scan         bool
	validating_generic_spec      bool
	monomorph_errors             []string
	monomorph_error_seen         map[string]bool
	in_spawn_expr                bool
	has_spawn_expr               bool
	in_const_init                bool
	in_return_expr               bool
	expected_expr_node           int = -1
	expected_expr_type           string
	in_selector_base             bool
	autolock_depth               int
	alias_cache                  &AliasCache             = unsafe { nil }
	sum_cache                    &AliasCache             = unsafe { nil }
	generic_unresolved_cache     &GenericUnresolvedCache = unsafe { nil }
	struct_field_type_cache      &LookupCache            = unsafe { nil }
	variant_short_name_cache     &LookupCache            = unsafe { nil }
	call_param_types_decl_cache  map[string][]types.Type
	call_param_types_decl_misses map[string]bool
	call_param_types_decl_index  map[string]FnParamDeclRef
	call_param_types_index_ready bool
	used_fns                     map[string]bool
	comptime_reflected_params    map[string][]ParamMeta
	// sum_eq_types records sum types whose deep-equality helper fn
	// (__v3_sum_eq_<name>) is called somewhere, keyed by sum name with the
	// module/file context of the requesting call site (type resolution inside
	// the helper body needs that context). The helpers are synthesized
	// serially after the (possibly parallel) transform completes.
	sum_eq_types                 map[string]SumEqRequest
	sum_eq_synthesized           map[string]bool
	sum_eq_helper_module         string
	interface_boxed_types        map[string]bool
	interface_boxed_types_done   bool
	interface_var_concrete_types map[string]string
	// used_struct_operator_fns holds the callee names of direct calls seen during
	// monomorphize. Infix operators on generic instances are lowered to direct calls
	// (`Vec_int__plus(a, b)`) before this pass, so an operator overload is specialized for
	// an instantiated generic struct only when its mangled name appears here — an instance
	// whose type argument never has the operator applied is not emitted with a body that
	// would fail C compilation.
	used_struct_operator_fns map[string]bool
	// active_generic_params holds the generic parameter names of the decl currently
	// being specialized/rewritten, in the same order as the inferred type `args`.
	// It lets type-text substitution map placeholders by name (so non-canonical
	// params like `D`/`F` resolve to the right arg) instead of by the positional
	// `generic_param_index` heuristic (which collapses anything outside the T/U/C
	// sequences to index 0). Empty for struct-generic specialization, which keeps
	// the legacy positional behaviour.
	active_generic_params []string
	// cloning_comptime_for_depth > 0 while a generic clone descends into a `$for` body: nested
	// generic calls there must not be specialized (the loop var members are not resolved yet).
	cloning_comptime_for_depth int
	cloning_comptime_for_vars  []string
	// ignored_comptime_for_nodes marks source `$for` subtrees replaced by concrete
	// unrolled nodes. Keeping the marker outside the shared flat AST lets parallel
	// workers record their own discarded nodes without racing.
	ignored_comptime_for_nodes []bool
	// cloning_generic_fn_depth > 0 while a generic specialization is cloned with a live,
	// seeded parameter scope. Ident inference should use that scope rather than scan annotations.
	cloning_generic_fn_depth int
	// escaping_amp_ptrs holds the names of pointer locals `p` declared as `p := &v`
	// (v a value local) whose pointer escapes the function (is returned). V semantics
	// auto-heap such a `v`; v3 otherwise takes the address of a stack local that dies
	// on return. Recomputed per function (structural pre-pass in transform_fn_body),
	// consumed when the `p := &v` decl is transformed (RHS rewritten to a heap copy).
	escaping_amp_ptrs map[string]bool
	// escaping_amp_sources holds the source locals `v` of such `p := &v` escapes — the
	// values whose address leaves the frame. The local itself is moved to the heap at its
	// declaration (its type becomes `&T`) so a mutation between `p := &v` and `return p`
	// is observed by the caller; copying eagerly at the alias would return stale data.
	escaping_amp_sources map[string]bool
	// heaped_amp_locals records which of those sources were actually moved to the heap, so
	// the `p := &v` alias emits `p = v` (the heap pointer) instead of a fresh memdup copy.
	heaped_amp_locals                map[string]bool
	generic_specialization_args      map[string][]string
	generic_fn_specs_in_progress     map[string]bool
	generic_fn_decls_cache           map[string]GenericFnDecl
	generic_receiver_methods_by_name map[string][]string
	generic_fn_decls_ready           bool
	generic_call_spec_cache          map[int]GenericCallSpec
	generic_call_spec_misses         map[int]bool
	stringify_stack                  []string
	node_module_map_cache            []string
	node_module_map_nodes            int = -1
	// used_fns_log records names newly inserted into used_fns while the
	// late-used-fn-bodies pass runs, so that pass can tell "was this name
	// already used before the current body's transform" without cloning the
	// whole used_fns map per function (those clones dominated the pass's time
	// and, under -gc none, were never freed).
	used_fns_log        []string
	used_fns_log_active bool
	// transformed_fns[i] is set when the fn_decl at node id i has had its body
	// transformed (main pass, any thread — worker chunks are marked at merge).
	// The late-used-fn-bodies pass excludes these candidates: lowered bodies
	// surface sanitized call spellings (`seed__time_seed_array`) that the
	// used-set (holding `seed.time_seed_array`) cannot filter, which used to
	// re-transform hundreds of already-transformed bodies every build.
	transformed_fns []bool
	// Shared-base (clone-free) parallel transform: all threads operate on views
	// of the master arrays, appending into pre-partitioned capacity regions.
	// While base_write_intercept is set, in-place writes to base-range node
	// slots outside the current item's subtree range [item_range_lo,
	// item_range_hi] are dropped (workers — matching the old clone path, where
	// such writes stayed in the discarded clone) or deferred until after join
	// (master, defer_oor_writes — matching the old path where the master's
	// writes landed on the shared AST).
	base_write_intercept bool
	defer_oor_writes     bool
	shared_base_nodes    int = -1
	item_range_lo        int = -1
	item_range_hi        int = -1
	deferred_base_writes []DeferredBaseWrite
	// Prealloc self-host builds put helper-thread scratch allocations in
	// disposable arenas. The worker's surviving AST strings are cloned by the
	// master before that arena is released.
	scope_parallel_workers bool
	worker_scope           voidptr
}

// AliasCache memoizes normalize_type_alias results. It lives on the heap so the
// many `&Transformer` (read-only) query methods can populate it through the
// pointer. normalize_type_alias is a pure function of (cur_module, typ) plus the
// collected type maps (which never change during transform), so the cache is
// keyed by typ and cleared whenever cur_module changes.
struct AliasCache {
mut:
	module  string
	entries map[string]string
}

struct LookupCache {
mut:
	entries map[string]string
	misses  map[string]bool
}

// GenericUnresolvedCache memoizes generic_arg_is_unresolved results. Lives on
// the heap so `&Transformer` query methods can populate it; keyed by type text
// and cleared on module switch (resolution consults module-qualified names).
struct GenericUnresolvedCache {
mut:
	module  string
	entries map[string]i8 // 1 = unresolved, -1 = resolved
}

// StructInfo stores struct info metadata used by transform.
pub struct StructInfo {
pub:
	name      string
	module    string
	is_params bool
	fields    []FieldInfo
}

// FieldInfo stores field info metadata used by transform.
pub struct FieldInfo {
pub:
	name         string
	typ          string
	raw_typ      string
	default_expr flat.NodeId
	is_embedded  bool
}

// TupleBlockParts represents tuple block parts data used by transform.
struct TupleBlockParts {
	prefix []flat.NodeId
	values []flat.NodeId
}

// StructFieldLookup represents struct field lookup data used by transform.
struct StructFieldLookup {
	info       StructInfo
	owner_type string
}

// VarTypeBinding represents var type binding data used by transform.
struct VarTypeBinding {
	name    string
	typ     string
	raw_typ string
}

struct GenericFnDecl {
	id     flat.NodeId
	node   flat.Node
	file   string
	module string
	key    string
}

struct GenericCallSpec {
	decl_key string
	args     []string
}

struct FnParamDeclRef {
	idx    int
	file   string
	module string
}

// --- entry point ---

// transform supports transform handling for transform.
pub fn transform(mut a flat.FlatAst, tc &types.TypeChecker) {
	transform_with_used(mut a, tc, map[string]bool{})
}

// transform_with_used transforms transform with used data for transform.
pub fn transform_with_used(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	augmented, _ := transform_with_used_opt(mut a, tc, used_fns, false)
	return augmented
}

// transform_with_used_opt is transform_with_used with an opt-in for parallel
// function-body transform. It returns the augmented used-fn set and whether the
// function bodies were actually transformed across threads (false when parallel
// was not requested, the build lacks thread support, or there was too little work).
pub fn transform_with_used_opt(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool) (map[string]bool, bool) {
	return transform_with_used_opt_config(mut a, tc, used_fns, want_parallel, false)
}

// transform_with_used_opt_config is transform_with_used_opt with extra pipeline
// switches for self-host builds.
pub fn transform_with_used_opt_config(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool) (map[string]bool, bool) {
	return transform_with_used_opt_config_scoped_workers(mut a, tc, used_fns, want_parallel,
		skip_generics, false)
}

// transform_with_used_opt_config_scoped_workers optionally gives parallel
// helpers disposable prealloc arenas. It is used by prealloc self-host builds
// to retain parallel latency without retaining every helper's scratch memory.
pub fn transform_with_used_opt_config_scoped_workers(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool, scope_parallel_workers bool) (map[string]bool, bool) {
	augmented, was_parallel, _ := transform_with_used_opt_config_scoped_workers_checked(mut a, tc,
		used_fns, want_parallel, skip_generics, scope_parallel_workers)
	return augmented, was_parallel
}

// transform_with_used_opt_config_scoped_workers_checked also returns diagnostics selected while
// normal comptime reflection loops are unrolled.
pub fn transform_with_used_opt_config_scoped_workers_checked(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool, scope_parallel_workers bool) (map[string]bool, bool, []string) {
	mut t := new_transformer(mut a, tc, used_fns)
	t.skip_generics = skip_generics
	t.scope_parallel_workers = scope_parallel_workers
	t.prepare()
	t.cache_comptime_param_reflection_metadata()
	if want_parallel {
		// Transform roughly grows the node/children arrays by ~75%. Reserve that capacity
		// up front so they don't double past it (the parsed AST already overshoots to the
		// next power of two, wasting ~40MB, and each doubling briefly holds both the old and
		// new arrays — the dominant peak-RSS contributor under -gc none). The serial path is
		// latency-sensitive and does not clone worker ASTs, so let it grow naturally.
		// Nodes grow a bit less than children on large compiler inputs. Keeping their
		// reserve below the next power-of-two cliff avoids retaining a mostly-empty
		// doubled nodes array through annotate/CGen.
		// The shared-base parallel path partitions this headroom into per-worker
		// append regions, so it reserves ~2x the expected growth: untouched
		// capacity pages are never written, so RSS does not grow with the
		// extra reserve.
		nodes_factor_num, nodes_factor_den := if skip_generics { 7, 3 } else { 5, 3 }
		children_factor_num, children_factor_den := if skip_generics { 5, 2 } else { 7, 4 }
		reserve_nodes := a.nodes.len * nodes_factor_num / nodes_factor_den - a.nodes.cap
		if reserve_nodes > 0 {
			unsafe { a.nodes.grow_cap(reserve_nodes) }
		}
		reserve_children := a.children.len * children_factor_num / children_factor_den - a.children.cap
		if reserve_children > 0 {
			unsafe { a.children.grow_cap(reserve_children) }
		}
	}
	base_node_count := t.a.nodes.len
	t.transformed_fns = []bool{len: t.a.nodes.len}
	was_parallel := t.transform_all_dispatch(want_parallel)
	t.apply_ignored_comptime_for_nodes()
	// The late-name scan backfills call names that markused's raw-AST resolution
	// missed and generic-specialization names. When building the V compiler
	// itself (skip_generics: no generics, fully resolvable calls) it provably
	// contributes nothing — the emitted function set is identical with and
	// without it — so skip the full-AST rescan there. A genuine future gap would
	// fail loudly (missing C symbol), not silently.
	mut late_names := []string{}
	if !skip_generics {
		late_names = t.new_call_names_from_used_fn_bodies(used_fns, t.a.nodes.len)
	}
	late_names << newly_used_fn_names(used_fns, t.used_fns)
	t.transform_late_used_fn_bodies(late_names, base_node_count)
	t.run_sum_eq_synthesis_rounds(base_node_count)
	t.apply_ignored_comptime_for_nodes()
	return t.used_fns, was_parallel, t.monomorph_errors
}

// run_sum_eq_synthesis_rounds alternates sum-eq helper synthesis with the
// late-used-fn transform until neither produces new work: building a helper
// body can mark a payload struct's overloaded `==` as used (which then needs
// its body transformed), and transforming that body can request equality
// helpers for further sum types.
fn (mut t Transformer) run_sum_eq_synthesis_rounds(node_limit int) {
	for _ in 0 .. 16 {
		new_names := t.synthesize_sum_eq_helpers()
		if new_names.len == 0 {
			return
		}
		t.transform_late_used_fn_bodies(new_names, node_limit)
	}
}

fn (mut t Transformer) new_call_names_from_used_fn_bodies(used map[string]bool, node_limit int) []string {
	if used.len == 0 || node_limit <= 0 {
		return []string{}
	}
	limit := if node_limit < t.a.nodes.len { node_limit } else { t.a.nodes.len }
	cands := t.collect_late_scan_candidates(limit)
	return t.scan_late_call_names_dispatch(cands, used)
}

// collect_late_scan_candidates lists every fn_decl below `limit` with its
// file/module context. The per-candidate filtering (generic templates,
// late-used matching) runs in scan_late_call_names_range, so it can be spread
// across worker threads.
fn (t &Transformer) collect_late_scan_candidates(limit int) []LateFnCandidate {
	mut cands := []LateFnCandidate{cap: 8192}
	mut cur_module := ''
	mut cur_file := ''
	for i in 0 .. limit {
		node := t.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			cur_module = ''
		} else if kind_id == 73 {
			cur_module = node.value
		} else if kind_id == 61 {
			cands << LateFnCandidate{
				idx:    i
				file:   cur_file
				module: cur_module
			}
		}
	}
	return cands
}

// scan_late_call_names_range performs the late-name scan for a contiguous
// candidate range: the used/generated fn bodies are walked for call names that
// are not in the used set yet. Reads shared state only (plus this
// transformer's private per-function context and checker caches), so disjoint
// ranges can run on worker threads; concatenating the per-range results in
// range order and deduplicating reproduces the serial scan exactly.
fn (mut t Transformer) scan_late_call_names_range(cands []LateFnCandidate, used map[string]bool, start int, end int) []string {
	mut names := []string{}
	mut seen := map[string]bool{}
	old_module := t.cur_module
	old_file := t.cur_file
	for ci in start .. end {
		cand := cands[ci]
		node := t.a.nodes[cand.idx]
		if t.fn_decl_has_unresolved_generics(node, cand.module) {
			continue
		}
		if !transform_is_generated_fn_after_markused(node.value)
			&& !late_used_fn_matches(used, node, cand.module) {
			continue
		}
		t.cur_file = cand.file
		t.cur_module = cand.module
		for call_name in t.generated_fn_body_call_names(flat.NodeId(cand.idx)) {
			if call_name.len == 0 || seen[call_name] {
				continue
			}
			if used[call_name] || used[c_name(call_name)]
				|| late_used_fn_contains_in_module(used, call_name, cand.module) {
				continue
			}
			seen[call_name] = true
			names << call_name
		}
	}
	t.cur_module = old_module
	t.cur_file = old_file
	return names
}

fn newly_used_fn_names(before map[string]bool, after map[string]bool) []string {
	mut names := []string{}
	for name, used in after {
		if !used || name.len == 0 {
			continue
		}
		if before[name] || before[c_name(name)] {
			continue
		}
		if name.contains('.') && before[name.all_after_last('.')] {
			continue
		}
		names << name
	}
	return names
}

pub fn monomorphize_with_used(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	result, _ := monomorphize_with_used_checked(mut a, tc, used_fns)
	return result
}

// monomorphize_with_used_checked also reports semantic errors that can only be
// resolved after generic parameters have concrete types.
pub fn monomorphize_with_used_checked(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) (map[string]bool, []string) {
	mut augmented_used_fns := used_fns.clone()
	mut t := new_transformer(mut a, tc, augmented_used_fns)
	t.prepare()
	base_node_count := t.a.nodes.len
	generated_names := t.monomorphize_pass()
	mut late_names := []string{}
	for name in generated_names {
		was_used := used_fns[name] || used_fns[c_name(name)]
		augmented_used_fns[name] = true
		augmented_used_fns[c_name(name)] = true
		t.used_fns[name] = true
		t.used_fns[c_name(name)] = true
		if !was_used {
			late_names << name
		}
	}
	// Monomorphization adds concrete generic functions after markused. Scan the
	// augmented set so calls introduced by their transformed/comptime-unrolled
	// bodies also root their non-generic helpers.
	late_names << t.new_call_names_from_used_fn_bodies(t.used_fns, t.a.nodes.len)
	late_names << newly_used_fn_names(used_fns, t.used_fns)
	t.transform_late_used_fn_bodies(late_names, base_node_count)
	used_before_remaining_matches := t.used_fns.clone()
	t.lower_remaining_matches_in_used_fns()
	remaining_match_names := newly_used_fn_names(used_before_remaining_matches, t.used_fns)
	t.transform_late_used_fn_bodies(remaining_match_names, base_node_count)
	t.materialize_generic_structs()
	t.run_sum_eq_synthesis_rounds(base_node_count)
	t.apply_ignored_comptime_for_nodes()
	return t.used_fns, t.monomorph_errors
}

fn new_transformer(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) Transformer {
	return Transformer{
		a:                                a
		tc:                               unsafe { tc }
		has_spawn_expr:                   tc.threads_condition_value()
		mut_param_values:                 map[string]bool{}
		mut_value_ident_nodes:            map[int]bool{}
		pointer_value_lvalues:            map[string]bool{}
		pointer_value_rvalues:            map[string]bool{}
		invalidated_smartcasts:           map[string]bool{}
		escaping_amp_ptrs:                map[string]bool{}
		escaping_amp_sources:             map[string]bool{}
		heaped_amp_locals:                map[string]bool{}
		generic_specialization_args:      map[string][]string{}
		generic_fn_specs_in_progress:     map[string]bool{}
		generic_receiver_methods_by_name: map[string][]string{}
		generic_call_spec_cache:          map[int]GenericCallSpec{}
		generic_call_spec_misses:         map[int]bool{}
		monomorph_error_seen:             map[string]bool{}
		call_param_types_decl_cache:      map[string][]types.Type{}
		call_param_types_decl_misses:     map[string]bool{}
		call_param_types_decl_index:      map[string]FnParamDeclRef{}
		enum_backing_types:               map[string]string{}
		sum_variant_names:                map[string]bool{}
		used_fns:                         used_fns.clone()
		comptime_reflected_params:        map[string][]ParamMeta{}
		interface_boxed_types:            map[string]bool{}
		interface_var_concrete_types:     map[string]string{}
	}
}

fn (mut t Transformer) mark_fn_used_name(name string) {
	if name.len == 0 {
		return
	}
	t.mark_used_fn_key(name)
	t.mark_used_fn_key(c_name(name))
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		needs_module_prefix := !name.contains('.') || local_method_fn_name_needs_module_prefix(name)
		if needs_module_prefix && !name.starts_with('${t.cur_module}.') {
			qname := '${t.cur_module}.${name}'
			t.mark_used_fn_key(qname)
			t.mark_used_fn_key(c_name(qname))
		}
	}
}

// mark_used_fn_key inserts one spelling into used_fns, recording first-time
// insertions in used_fns_log while the late-used-fn-bodies pass is running.
fn (mut t Transformer) mark_used_fn_key(key string) {
	if t.used_fns_log_active && !t.used_fns[key] {
		t.used_fns_log << key
	}
	t.used_fns[key] = true
}

fn local_method_fn_name_needs_module_prefix(name string) bool {
	if !name.contains('.') {
		return false
	}
	receiver_name := name.all_before('.')
	if receiver_name.len == 0 {
		return false
	}
	first := receiver_name[0]
	return first >= `A` && first <= `Z`
}

fn (mut t Transformer) prepare() {
	t.collect_types()
	t.collect_const_suffixes()
	t.collect_alias_methods()
	t.rebuild_receiver_method_suffix_index()
	// Enable the alias cache only now that the type maps are fully populated.
	// During collection those maps are incomplete, so caching there would poison
	// entries with results computed against a partial view.
	t.alias_cache = &AliasCache{}
	t.sum_cache = &AliasCache{}
	t.generic_unresolved_cache = &GenericUnresolvedCache{}
	t.struct_field_type_cache = &LookupCache{
		entries: map[string]string{}
		misses:  map[string]bool{}
	}
	t.variant_short_name_cache = &LookupCache{
		entries: map[string]string{}
		misses:  map[string]bool{}
	}
}

// base_write_allowed reports whether an in-place write to node slot `idx` is
// safe right now: always outside the shared-base parallel region; inside it,
// only appended nodes (>= shared_base_nodes) and slots of the fn subtree
// currently being transformed (this thread is that range's only owner).
@[inline]
fn (t &Transformer) base_write_allowed(idx int) bool {
	return !t.base_write_intercept || idx >= t.shared_base_nodes
		|| (idx >= t.item_range_lo && idx <= t.item_range_hi)
}

@[inline]
fn (mut t Transformer) set_node_typ(idx int, typ string) {
	if t.base_write_allowed(idx) {
		t.a.nodes[idx].typ = typ
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx:  idx
			kind: 0
			str:  typ
		}
	}
}

@[inline]
fn (mut t Transformer) set_node_value(idx int, value string) {
	if t.base_write_allowed(idx) {
		t.a.nodes[idx].value = value
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx:  idx
			kind: 1
			str:  value
		}
	}
}

@[inline]
fn (mut t Transformer) set_node(idx int, node flat.Node) {
	if t.base_write_allowed(idx) {
		t.a.nodes[idx] = node
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx:  idx
			kind: 2
			node: node
		}
	}
}

fn (mut t Transformer) ignore_comptime_for_subtree(id flat.NodeId) {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len {
		return
	}
	if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
		t.ignored_comptime_for_nodes << []bool{len: t.a.nodes.len - t.ignored_comptime_for_nodes.len}
	}
	if t.ignored_comptime_for_nodes[idx] {
		return
	}
	t.ignored_comptime_for_nodes[idx] = true
	node := t.a.nodes[idx]
	for i in 0 .. node.children_count {
		t.ignore_comptime_for_subtree(t.a.child(&node, i))
	}
}

fn (mut t Transformer) apply_ignored_comptime_for_nodes() {
	for idx, ignored in t.ignored_comptime_for_nodes {
		if !ignored || idx >= t.a.nodes.len {
			continue
		}
		old := t.a.nodes[idx]
		t.a.nodes[idx] = flat.Node{
			kind: .empty
			pos:  old.pos
		}
		t.clear_typechecker_node_cache(idx)
	}
	t.ignored_comptime_for_nodes = []bool{}
}

@[inline]
fn (mut t Transformer) set_node_generic_params(idx int, gparams []string) {
	if t.base_write_allowed(idx) {
		t.a.nodes[idx].generic_params = gparams
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx:     idx
			kind:    3
			gparams: gparams
		}
	}
}

// flush_deferred_base_writes applies the master's parked out-of-range writes
// in original program order once every worker has been joined.
fn (mut t Transformer) flush_deferred_base_writes() {
	for w in t.deferred_base_writes {
		match w.kind {
			0 { t.a.nodes[w.idx].typ = w.str }
			1 { t.a.nodes[w.idx].value = w.str }
			2 { t.a.nodes[w.idx] = w.node }
			else { t.a.nodes[w.idx].generic_params = w.gparams }
		}
	}
	t.deferred_base_writes = []DeferredBaseWrite{}
}

fn (mut t Transformer) clear_struct_field_type_cache() {
	if isnil(t.struct_field_type_cache) {
		return
	}
	mut cache := t.struct_field_type_cache
	cache.entries.clear()
	cache.misses.clear()
}

const receiver_method_suffix_ambiguous = '__v_receiver_method_suffix_ambiguous__'
const sum_type_tag_selector_field = '__v_sum_type_tag__'
const pending_loop_label_marker = '__v_pending_loop_label:'

fn (mut t Transformer) rebuild_receiver_method_suffix_index() {
	t.receiver_method_suffix_index.clear()
	for name, _ in t.fn_ret_types {
		t.add_receiver_method_suffix_index(name)
	}
	if isnil(t.tc) {
		return
	}
	for name, _ in t.tc.fn_ret_types {
		t.add_receiver_method_suffix_index(name)
	}
}

fn (mut t Transformer) add_receiver_method_suffix_index(name string) {
	if name.len == 0 {
		return
	}
	t.set_receiver_method_suffix_index(name, name)
	for i in 0 .. name.len {
		if name[i] == `.` && i + 1 < name.len {
			t.set_receiver_method_suffix_index(name[i + 1..], name)
		}
	}
}

fn (mut t Transformer) set_receiver_method_suffix_index(key string, name string) {
	if key.len == 0 {
		return
	}
	if existing := t.receiver_method_suffix_index[key] {
		if existing != name {
			t.receiver_method_suffix_index[key] = receiver_method_suffix_ambiguous
		}
		return
	}
	t.receiver_method_suffix_index[key] = name
}

// reset_var_types updates reset var types state for transform.
fn (mut t Transformer) reset_var_types() {
	t.var_types.clear()
	t.mut_param_values.clear()
	t.interface_var_concrete_types.clear()
}

// set_var_type updates set var type state for transform.
fn (mut t Transformer) set_var_type(name string, typ string) {
	t.set_var_type_with_raw(name, typ, typ)
}

fn (mut t Transformer) set_var_type_with_raw(name string, typ string, raw_typ string) {
	if name.len == 0 {
		return
	}
	raw := if raw_typ.len > 0 { raw_typ } else { typ }
	for i, binding in t.var_types {
		if binding.name == name {
			t.var_types[i] = VarTypeBinding{
				name:    name
				typ:     typ
				raw_typ: raw
			}
			return
		}
	}
	t.var_types << VarTypeBinding{
		name:    name
		typ:     typ
		raw_typ: raw
	}
}

// unset_var_type supports unset var type handling for Transformer.
fn (mut t Transformer) unset_var_type(name string) {
	for i, binding in t.var_types {
		if binding.name == name {
			t.var_types.delete(i)
			t.interface_var_concrete_types.delete(name)
			return
		}
	}
}

// var_type supports var type handling for Transformer.
fn (t &Transformer) var_type(name string) string {
	for binding in t.var_types {
		if binding.name == name {
			return binding.typ
		}
	}
	return ''
}

fn (t &Transformer) raw_var_type(name string) string {
	for binding in t.var_types {
		if binding.name == name {
			if binding.raw_typ.len > 0 {
				return binding.raw_typ
			}
			return binding.typ
		}
	}
	return ''
}

fn (mut t Transformer) restore_var_types(saved []VarTypeBinding) {
	t.var_types = saved
}

// --- type collection ---

// collect_types updates collect types state for transform.
fn (mut t Transformer) collect_types() {
	// All matched kinds are top-level declarations; when the checker's
	// top-level index covers the AST, iterate it instead of streaming every
	// node (same nodes, same order, ~100x fewer).
	use_idx := !isnil(t.tc) && t.tc.top_level_idx.len > 0
		&& t.tc.top_level_idx_nodes_len == t.a.nodes.len
	count := if use_idx { t.tc.top_level_idx.len } else { t.a.nodes.len }
	mut cur_mod := ''
	for ii in 0 .. count {
		node := if use_idx { t.a.nodes[t.tc.top_level_idx[ii]] } else { t.a.nodes[ii] }
		match node.kind {
			.file {
				cur_mod = ''
			}
			.module_decl {
				cur_mod = node.value
			}
			.struct_decl {
				owner_type := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					'${cur_mod}.${node.value}'
				} else {
					node.value
				}
				mut fields := []FieldInfo{}
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					if f.kind != .field_decl {
						continue
					}
					default_expr := if f.children_count > 0 {
						t.a.child(f, 0)
					} else {
						flat.empty_node
					}
					field_typ := t.normalize_field_type(f.typ, owner_type)
					fields << FieldInfo{
						name:         f.value
						typ:          field_typ
						raw_typ:      f.typ
						default_expr: default_expr
						is_embedded:  field_decl_is_embedded(f.value, field_typ)
					}
				}
				info := StructInfo{
					name:      node.value
					module:    cur_mod
					is_params: 'params' in node.typ.split(',')
					fields:    fields
				}
				if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					qname := '${cur_mod}.${node.value}'
					t.structs[qname] = info
					if node.value !in t.structs {
						t.structs[node.value] = info
					}
					if node.value !in t.qualified_types {
						t.qualified_types[node.value] = qname
					}
				} else {
					t.structs[node.value] = info
				}
				for f in fields {
					t.add_unique_field_type(f.name, f.typ)
				}
			}
			.type_decl {
				if node.children_count > 0 {
					mut variants := []string{}
					for i in 0 .. node.children_count {
						v := t.a.child_node(&node, i)
						variants << t.normalize_sum_variant_type(v.value, cur_mod,
							node.generic_params)
					}
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						qname := '${cur_mod}.${node.value}'
						t.sum_types[qname] = variants
						if node.value !in t.sum_types {
							t.sum_types[node.value] = variants
							for variant in variants {
								t.add_sum_variant_parent(variant, node.value)
							}
						}
						if node.value !in t.qualified_types {
							t.qualified_types[node.value] = qname
						}
						for variant in variants {
							t.add_sum_variant_parent(variant, qname)
						}
					} else {
						t.sum_types[node.value] = variants
						for variant in variants {
							t.add_sum_variant_parent(variant, node.value)
						}
					}
				}
			}
			.enum_decl {
				mut field_names := []string{}
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					if f.kind == .enum_field {
						field_names << f.value
					}
				}
				backing_storage_type := if node.generic_params.len > 0
					&& node.generic_params[0].len > 0 {
					t.normalize_type_in_module(node.generic_params[0], cur_mod)
				} else {
					''
				}
				if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					qname := '${cur_mod}.${node.value}'
					t.enum_types[qname] = field_names
					if node.value !in t.enum_types {
						t.enum_types[node.value] = field_names
						if backing_storage_type.len > 0 {
							t.enum_backing_types[node.value] = backing_storage_type
						}
					}
					if backing_storage_type.len > 0 {
						t.enum_backing_types[qname] = backing_storage_type
					}
				} else {
					t.enum_types[node.value] = field_names
					if backing_storage_type.len > 0 {
						t.enum_backing_types[node.value] = backing_storage_type
					}
				}
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					mut typ := t.normalize_type_in_module(f.typ, cur_mod)
					if typ.len == 0 && f.children_count > 0 {
						typ = t.normalize_type_in_module(t.node_type(t.a.child(f, 0)), cur_mod)
					}
					t.globals[f.value] = typ
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						t.globals['${cur_mod}.${f.value}'] = typ
					}
				}
			}
			.fn_decl {
				if node.typ.len > 0 {
					ret_typ := t.normalize_type_in_module(node.typ, cur_mod)
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						qname := '${cur_mod}.${node.value}'
						t.fn_ret_types[qname] = ret_typ
						qlowered := c_name(qname)
						if qlowered != qname {
							t.fn_ret_types[qlowered] = ret_typ
						}
					} else {
						t.fn_ret_types[node.value] = ret_typ
						lowered := c_name(node.value)
						if lowered != node.value {
							t.fn_ret_types[lowered] = ret_typ
						}
					}
				}
			}
			.c_fn_decl {
				if node.typ.len > 0 {
					ret_typ := t.normalize_type_in_module(node.typ, cur_mod)
					t.fn_ret_types[node.value] = ret_typ
					if node.value.starts_with('C.') {
						t.fn_ret_types[node.value[2..]] = ret_typ
					} else {
						t.fn_ret_types['C.${node.value}'] = ret_typ
					}
				}
			}
			else {}
		}
	}
}

// add_sum_variant_parent updates add sum variant parent state for Transformer.
fn (mut t Transformer) add_sum_variant_parent(variant string, sum_name string) {
	if variant.len == 0 || sum_name.len == 0 {
		return
	}
	field_name := t.sum_field_name(variant)
	if field_name.contains('__') && field_name !in t.sum_variant_fields {
		t.sum_variant_fields[field_name] = variant
	}
	t.sum_variant_names[variant] = true
	t.sum_variant_names[t.variant_short_name(variant)] = true
	t.add_sum_variant_parent_key(variant, sum_name)
	if variant.contains('.') {
		t.add_sum_variant_parent_key(variant.all_after_last('.'), sum_name)
	}
}

// add_sum_variant_parent_key updates add sum variant parent key state for Transformer.
fn (mut t Transformer) add_sum_variant_parent_key(key string, sum_name string) {
	mut parents := t.sum_variant_parents[key] or { []string{} }
	if sum_name !in parents {
		parents << sum_name
		t.sum_variant_parents[key] = parents
	}
}

// add_unique_field_type updates add unique field type state for Transformer.
fn (mut t Transformer) add_unique_field_type(name string, typ string) {
	if name.len == 0 || typ.len == 0 {
		return
	}
	if existing := t.unique_fields[name] {
		if existing != typ {
			t.unique_fields[name] = ''
		}
	} else {
		t.unique_fields[name] = typ
	}
}

// collect_const_suffixes updates collect const suffixes state for transform.
fn (mut t Transformer) collect_const_suffixes() {
	if isnil(t.tc) {
		return
	}
	// Register every dot-delimited suffix of each const key so that both
	// unqualified (`foo`) and partially-qualified (`mod.foo`) lookups resolve
	// in O(1) via const_type_key, instead of scanning all consts per ident.
	for key, _ in t.tc.const_types {
		if !key.contains('.') {
			t.add_const_suffix(key, key)
			continue
		}
		mut i := 0
		for i < key.len {
			if key[i] == `.` {
				t.add_const_suffix(key[i + 1..], key)
			}
			i++
		}
	}
}

// add_const_suffix updates add const suffix state for Transformer.
fn (mut t Transformer) add_const_suffix(suffix string, key string) {
	if existing := t.const_suffixes[suffix] {
		if existing != key {
			t.const_suffixes[suffix] = ''
		}
	} else {
		t.const_suffixes[suffix] = key
	}
}

// collect_alias_methods converts collect alias methods data for transform.
fn (mut t Transformer) collect_alias_methods() {
	if isnil(t.tc) {
		return
	}
	for name, params in t.tc.fn_param_types {
		if params.len == 0 || !name.contains('.') {
			continue
		}
		receiver_name := name.all_before_last('.')
		if receiver_name.len == 0 || receiver_name !in t.tc.type_aliases {
			continue
		}
		method := name.all_after_last('.')
		param_name := params[0].name()
		clean_alias := if param_name.starts_with('&') { param_name[1..] } else { param_name }
		alias_target := t.normalize_type_alias(clean_alias)
		if alias_target.len == 0 {
			continue
		}
		key := '${alias_target}.${method}'
		if key !in t.alias_methods {
			t.alias_methods[key] = name
		}
	}
}

// normalize_sum_variant_type transforms normalize sum variant type data for transform.
fn (t &Transformer) normalize_sum_variant_type(typ string, mod string, generic_params []string) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean in generic_params {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.normalize_sum_variant_type(clean[1..], mod, generic_params)
	}
	if clean.starts_with('mut ') {
		return '&' + t.normalize_sum_variant_type(clean[4..], mod, generic_params)
	}
	if clean.starts_with('?') {
		return '?' + t.normalize_sum_variant_type(clean[1..], mod, generic_params)
	}
	if clean.starts_with('!') {
		return '!' + t.normalize_sum_variant_type(clean[1..], mod, generic_params)
	}
	if clean.starts_with('...') {
		return '...' + t.normalize_sum_variant_type(clean[3..], mod, generic_params)
	}
	if clean.starts_with('[]') {
		return '[]' + t.normalize_sum_variant_type(clean[2..], mod, generic_params)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := t.normalize_sum_variant_type(clean[4..bracket_end], mod, generic_params)
			value := t.normalize_sum_variant_type(clean[bracket_end + 1..], mod, generic_params)
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + t.normalize_sum_variant_type(clean[bracket_end +
				1..], mod, generic_params)
		}
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := generic_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut args := []string{}
			for arg in split_generic_args(clean[bracket + 1..bracket_end]) {
				args << t.normalize_sum_variant_type(arg, mod, generic_params)
			}
			base := clean[..bracket]
			qbase := t.normalize_sum_variant_type(base, mod, generic_params)
			return qbase + '[' + args.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	if clean.contains('.') || mod.len == 0 || mod == 'main' || mod == 'builtin'
		|| types.is_builtin_type_name(clean) {
		return clean
	}
	return '${mod}.${clean}'
}

// --- main transform pass ---

// transform_all transforms transform all data for transform.
fn (mut t Transformer) transform_all() {
	mut has_entry_main := false
	mut entry_module := ''
	node_count := t.a.nodes.len
	for i in 0 .. node_count {
		node := t.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			t.cur_file = node.value
			entry_module = ''
		}
		if kind_id == 73 {
			t.cur_module = node.value
			entry_module = node.value
		}
		if kind_id == 61 {
			if node.value == 'main' && (entry_module.len == 0 || entry_module == 'main') {
				has_entry_main = true
			}
			if t.fn_decl_has_unresolved_generics(node, t.cur_module) {
				continue
			}
			if !t.should_transform_fn(node) {
				continue
			}
			t.transform_fn_body(i)
		} else if kind_id == 65 {
			t.transform_const_decl(node)
		} else if kind_id == 64 {
			t.transform_global_decl(node)
		}
	}
	if !has_entry_main {
		t.transform_top_level_user_stmts()
	}
}

fn (t &Transformer) has_entry_main() bool {
	mut cur_module := ''
	for node in t.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_module = ''
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			continue
		}
		if kind_id == 61 && node.value == 'main' && (cur_module.len == 0 || cur_module == 'main') {
			return true
		}
	}
	return false
}

fn (mut t Transformer) transform_top_level_user_stmts() {
	node_count := t.a.nodes.len
	for file_idx in 0 .. node_count {
		file_node := t.a.nodes[file_idx]
		if !t.should_transform_top_level_file(file_idx, file_node) {
			continue
		}
		t.transform_top_level_file(file_idx, file_node)
	}
}

fn (t &Transformer) should_transform_top_level_file(file_idx int, file_node flat.Node) bool {
	if file_idx < t.a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	module_name := t.file_module_name(file_node)
	return module_name.len == 0 || module_name == 'main'
}

fn (t &Transformer) file_module_name(file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := t.a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn transform_is_top_level_stmt(node flat.Node) bool {
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt,
		.for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt, .block {
			true
		}
		else {
			false
		}
	}
}

fn (mut t Transformer) transform_top_level_file(file_idx int, file_node flat.Node) {
	old_file := t.cur_file
	old_module := t.cur_module
	old_fn_name := t.cur_fn_name
	old_fn_ret_type := t.cur_fn_ret_type
	old_var_types := t.var_types.clone()
	old_smartcast_stack := t.smartcast_stack.clone()
	old_pending_stmts := t.pending_stmts.clone()
	module_name := t.file_module_name(file_node)
	t.cur_file = file_node.value
	t.cur_module = if module_name.len == 0 { 'main' } else { module_name }
	t.cur_fn_name = 'main'
	t.cur_fn_ret_type = 'void'
	t.reset_var_types()
	t.smartcast_stack.clear()
	t.pending_stmts.clear()
	mut new_children := []flat.NodeId{cap: int(file_node.children_count)}
	mut pending_stmts := []flat.NodeId{}
	for i in 0 .. file_node.children_count {
		child_id := t.a.child(&file_node, i)
		if int(child_id) >= t.a.user_code_start {
			child := t.a.nodes[int(child_id)]
			if transform_is_top_level_stmt(child) {
				pending_stmts << child_id
				continue
			}
		}
		t.append_transformed_top_level_stmts(mut new_children, mut pending_stmts)
		new_children << child_id
	}
	t.append_transformed_top_level_stmts(mut new_children, mut pending_stmts)
	start := t.a.children.len
	for child_id in new_children {
		t.a.children << child_id
	}
	t.set_node(file_idx, flat.Node{
		kind:           .file
		kind_id:        file_node.kind_id
		op:             file_node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            file_node.pos
		value:          file_node.value
		typ:            file_node.typ
		generic_params: file_node.generic_params
	})
	t.cur_file = old_file
	t.cur_module = old_module
	t.cur_fn_name = old_fn_name
	t.cur_fn_ret_type = old_fn_ret_type
	t.restore_var_types(old_var_types)
	t.smartcast_stack = old_smartcast_stack
	t.pending_stmts = old_pending_stmts
}

fn (mut t Transformer) append_transformed_top_level_stmts(mut out []flat.NodeId, mut pending []flat.NodeId) {
	if pending.len == 0 {
		return
	}
	transformed := t.transform_stmts(pending)
	out << transformed
	pending.clear()
}

// FnWorkItem identifies one top-level function body to transform, together with
// the file/module context active at its declaration and a rough cost estimate
// (subtree node count) used to balance work across parallel workers.
struct FnWorkItem {
	fn_idx   int
	range_lo int // first node id of this fn's subtree (fn subtree = [range_lo, fn_idx])
	file     string
	module   string
	cost     int
	rank     i64
}

// DeferredBaseWrite is an in-place base-node write recorded by the master
// during the shared-base parallel transform instead of being applied live
// (worker threads read those slots concurrently); flushed after all joins.
struct DeferredBaseWrite {
	idx     int
	kind    u8 // 0 = typ, 1 = value, 2 = whole node, 3 = generic_params
	str     string
	node    flat.Node
	gparams []string
}

// transform_all_dispatch runs the main transform pass either serially (the
// original single-threaded walk) or, when `want_parallel` is set and there is
// enough work, with closure-free function bodies transformed across threads.
// Returns whether function bodies were actually transformed in parallel.
fn (mut t Transformer) transform_all_dispatch(want_parallel bool) bool {
	// Collect source-level interface conversions before any worker rewrites its
	// private AST. Equality and automatic string lowering can then generate the
	// same bounded tag dispatch independently of worker scheduling.
	t.collect_interface_boxed_types()
	if !want_parallel {
		t.transform_all()
		return false
	}
	has_entry_main := t.has_entry_main()
	// Serial phase: transform consts/globals and every function whose body
	// contains a function literal (the only construct that lifts new top-level
	// declarations and mutates the shared TypeChecker). Collect the remaining,
	// closure-free functions as parallelizable work items.
	has_fn_literals := t.has_fn_literal_nodes()
	pure_items := t.transform_serial_then_collect_pure(has_fn_literals)
	base_nodes := t.a.nodes.len
	base_children := t.a.children.len
	was_parallel := t.run_parallel_transform(pure_items, base_nodes, base_children)
	if !has_entry_main {
		t.transform_top_level_user_stmts()
	}
	return was_parallel
}

fn (t &Transformer) has_fn_literal_nodes() bool {
	for node in t.a.nodes {
		if node.kind == .fn_literal || node.kind == .lambda_expr {
			return true
		}
	}
	return false
}

// transform_serial_then_collect_pure walks the top level once: it transforms
// const/global declarations and closure-bearing functions in place (serially),
// and returns work items for the closure-free functions left to transform.
fn (mut t Transformer) transform_serial_then_collect_pure(scan_fn_literals bool) []FnWorkItem {
	mut pure := []FnWorkItem{}
	original_len := t.a.nodes.len
	literal_decls := if scan_fn_literals {
		t.collect_literal_fn_decls(original_len)
	} else {
		[]bool{}
	}
	// The checker's top-level index gives the exact subtree range of each fn
	// ((previous top-level decl of ANY kind, fn_idx]); the shared-base parallel
	// transform relies on those ranges being disjoint per item.
	tl := if !isnil(t.tc) { t.tc.top_level_idx } else { []int{} }
	mut ti := 0
	mut prev_tl_any := -1
	mut prev_decl_end := 0
	for i in 0 .. original_len {
		for ti < tl.len && tl[ti] < i {
			prev_tl_any = tl[ti]
			ti++
		}
		node := t.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			t.cur_file = node.value
		} else if kind_id == 73 {
			t.cur_module = node.value
		} else if kind_id == 61 {
			// The parser builds nodes bottom-up, so a declaration's subtree
			// precedes its node: the span since the previous top-level
			// declaration approximates this function's subtree size.
			span_cost := i - prev_decl_end
			prev_decl_end = i
			if t.fn_decl_has_unresolved_generics(node, t.cur_module) {
				continue
			}
			if !t.should_transform_fn(node) {
				continue
			}
			mut has_literal := false
			mut cost := int(node.children_count) + 1
			if scan_fn_literals {
				has_literal = literal_decls[i]
				cost = if span_cost > 0 { span_cost } else { 1 }
			}
			if has_literal {
				t.transform_fn_body(i)
			} else {
				pure << FnWorkItem{
					fn_idx:   i
					range_lo: prev_tl_any + 1
					file:     t.cur_file
					module:   t.cur_module
					cost:     cost
					rank:     i64(cost) * 1_000_000_000 - i64(i)
				}
			}
		} else if kind_id == 65 {
			prev_decl_end = i
			t.transform_const_decl(node)
		} else if kind_id == 64 {
			prev_decl_end = i
			t.transform_global_decl(node)
		}
	}
	return pure
}

// collect_literal_fn_decls returns, per node id below `limit`, whether that
// fn_decl's subtree contains a function literal or lambda. The parser builds
// nodes bottom-up, so a literal always precedes its enclosing fn_decl node:
// one linear pass attributing "a literal appeared since the last consumed
// declaration" to the next fn_decl replaces the old per-function subtree
// walks. Literals inside const/global initializers reset at their decl; any
// other stray attribution can only route an extra function to the serial
// transform path, which is always safe.
fn (t &Transformer) collect_literal_fn_decls(limit int) []bool {
	mut result := []bool{len: limit}
	mut literal_pending := false
	for i in 0 .. limit {
		node := t.a.nodes[i]
		kid := node_kind_id(node)
		// 21 = fn_literal, 32 = lambda_expr (see fn_subtree_scan's old check).
		if kid == 21 || kid == 32 {
			literal_pending = true
		} else if kid == 61 {
			result[i] = literal_pending
			literal_pending = false
		} else if kid == 64 || kid == 65 {
			literal_pending = false
		}
	}
	return result
}

// transform_pure_items_serial transforms a list of closure-free function bodies
// on this Transformer, in order. Used both as the serial fallback and as the
// per-worker body in the parallel path.
fn (mut t Transformer) transform_pure_items_serial(items []FnWorkItem) {
	for it in items {
		t.cur_file = it.file
		t.cur_module = it.module
		t.item_range_lo = it.range_lo
		t.item_range_hi = it.fn_idx
		t.transform_fn_body(it.fn_idx)
	}
	t.item_range_lo = -1
	t.item_range_hi = -1
}

// clone_ast_base produces a private FlatAst holding an independent copy of the
// first base_nodes nodes / base_children children, so a worker can append its own
// transformed nodes without racing the master or other workers. Read-only metadata
// (disabled_fns) is shared. The copies carry headroom for the worker's own appends:
// an exact-size clone (cap == len) would double on the first push, transiently
// copying the whole array again and keeping the doubled capacity resident for the
// rest of the build (worker memory is never freed under -gc none). Transform grows
// the AST by well under 25% per worker, so a fixed base/4 margin avoids the cliff.
fn (t &Transformer) clone_ast_base(base_nodes int, base_children int) &flat.FlatAst {
	mut nodes := []flat.Node{cap: base_nodes + base_nodes / 4}
	nodes << t.a.nodes[0..base_nodes]
	mut children := []flat.NodeId{cap: base_children + base_children / 4}
	children << t.a.children[0..base_children]
	return &flat.FlatAst{
		nodes:                nodes
		children:             children
		user_code_start:      t.a.user_code_start
		disabled_fns:         t.a.disabled_fns
		noreturn_fns:         t.a.noreturn_fns
		specialized_fn_nodes: t.a.specialized_fn_nodes
	}
}

// fork_worker builds a worker Transformer that shares this transformer's
// read-only collected maps (structs, sum types, fn return types, …) and
// operates on its own cloned AST `ast` and forked TypeChecker `wtc`. All
// per-function mutable state, helper-root tracking, used-fn additions, and
// memoization caches are reset/private so the worker can run on its own thread.
fn (t &Transformer) fork_worker(ast &flat.FlatAst, wtc &types.TypeChecker) &Transformer {
	mut w := *t
	w.a = ast
	w.tc = wtc
	w.used_fns = t.used_fns.clone()
	w.comptime_reflected_params = t.comptime_reflected_params.clone()
	w.alias_cache = &AliasCache{}
	w.sum_cache = &AliasCache{}
	w.generic_unresolved_cache = &GenericUnresolvedCache{}
	w.struct_field_type_cache = &LookupCache{
		entries: map[string]string{}
		misses:  map[string]bool{}
	}
	w.variant_short_name_cache = &LookupCache{
		entries: map[string]string{}
		misses:  map[string]bool{}
	}
	w.generic_fn_decls_cache = map[string]GenericFnDecl{}
	w.generic_fn_decls_ready = false
	w.generic_call_spec_cache = map[int]GenericCallSpec{}
	w.generic_call_spec_misses = map[int]bool{}
	// run_parallel_transform snapshots declaration signatures before workers
	// start. Keep the shared read-only index and signature cache; rebuilding
	// either would read fn_decl nodes while shared-base workers rewrite them.
	// Misses stay private because unknown call names can still be queried.
	w.call_param_types_decl_misses = t.call_param_types_decl_misses.clone()
	w.node_module_map_cache = []string{}
	w.node_module_map_nodes = -1
	w.var_types = []VarTypeBinding{}
	w.mut_param_values = map[string]bool{}
	w.smartcast_stack = []SmartcastContext{}
	w.invalidated_smartcasts = map[string]bool{}
	w.pending_stmts = []flat.NodeId{}
	w.pointer_value_lvalues = map[string]bool{}
	w.pointer_value_rvalues = map[string]bool{}
	w.escaping_amp_ptrs = map[string]bool{}
	w.escaping_amp_sources = map[string]bool{}
	w.heaped_amp_locals = map[string]bool{}
	w.generic_specialization_args = t.generic_specialization_args.clone()
	w.generic_fn_specs_in_progress = map[string]bool{}
	w.monomorph_errors = []string{}
	w.monomorph_error_seen = map[string]bool{}
	w.used_fns = t.used_fns.clone()
	// Fields added after the fork/merge machinery was first written. They are
	// mutated during body transforms (or lazily built), so each worker needs
	// private backing storage — a plain struct copy would share the master's.
	w.stringify_stack = []string{}
	w.interface_var_concrete_types = t.interface_var_concrete_types.clone()
	w.interface_boxed_types = t.interface_boxed_types.clone()
	w.interface_boxed_types_done = true
	w.sum_eq_types = t.sum_eq_types.clone()
	w.sum_eq_synthesized = t.sum_eq_synthesized.clone()
	w.sum_eq_helper_module = ''
	w.generic_receiver_methods_by_name = map[string][]string{}
	w.used_fns_log = []string{}
	w.used_fns_log_active = false
	w.deferred_base_writes = []DeferredBaseWrite{}
	w.ignored_comptime_for_nodes = []bool{}
	w.worker_scope = unsafe { nil }
	// Workers do not record transformed fns (that would write the master's
	// shared backing array); the master marks each worker's chunk items when it
	// merges the worker.
	w.transformed_fns = []bool{}
	w.temp_counter = 0
	w.cur_file = ''
	w.cur_module = ''
	w.cur_fn_name = ''
	w.cur_fn_ret_type = ''
	w.in_call_callee = false
	w.in_const_init = false
	w.in_return_expr = false
	w.expected_expr_node = -1
	w.expected_expr_type = ''
	return &w
}

// fork_scan_worker builds a read-only worker for the parallel late-name scan.
// Unlike fork_worker it shares the big lookup maps by reference (the scan never
// writes them) and does not clone used_fns — the scan filters against the
// caller's `used` snapshot and never marks names — so forking costs almost
// nothing even with one fork per scan thread.
fn (t &Transformer) fork_scan_worker(wtc &types.TypeChecker) &Transformer {
	mut w := *t
	w.tc = wtc
	w.used_fns = map[string]bool{}
	w.alias_cache = &AliasCache{}
	w.sum_cache = &AliasCache{}
	w.generic_unresolved_cache = &GenericUnresolvedCache{}
	w.struct_field_type_cache = &LookupCache{
		entries: map[string]string{}
		misses:  map[string]bool{}
	}
	w.variant_short_name_cache = &LookupCache{
		entries: map[string]string{}
		misses:  map[string]bool{}
	}
	w.generic_fn_decls_cache = map[string]GenericFnDecl{}
	w.generic_fn_decls_ready = false
	w.generic_call_spec_cache = map[int]GenericCallSpec{}
	w.generic_call_spec_misses = map[int]bool{}
	w.node_module_map_cache = []string{}
	w.node_module_map_nodes = -1
	w.var_types = []VarTypeBinding{}
	w.mut_param_values = map[string]bool{}
	w.smartcast_stack = []SmartcastContext{}
	w.invalidated_smartcasts = map[string]bool{}
	w.pending_stmts = []flat.NodeId{}
	w.pointer_value_lvalues = map[string]bool{}
	w.pointer_value_rvalues = map[string]bool{}
	w.escaping_amp_ptrs = map[string]bool{}
	w.escaping_amp_sources = map[string]bool{}
	w.heaped_amp_locals = map[string]bool{}
	w.generic_fn_specs_in_progress = map[string]bool{}
	w.stringify_stack = []string{}
	w.interface_boxed_types = map[string]bool{}
	w.interface_boxed_types_done = false
	w.generic_receiver_methods_by_name = map[string][]string{}
	w.used_fns_log = []string{}
	w.used_fns_log_active = false
	w.worker_scope = unsafe { nil }
	w.transformed_fns = []bool{}
	w.temp_counter = 0
	w.cur_file = ''
	w.cur_module = ''
	w.cur_fn_name = ''
	w.cur_fn_ret_type = ''
	w.in_call_callee = false
	w.in_const_init = false
	w.in_return_expr = false
	w.expected_expr_node = -1
	w.expected_expr_type = ''
	return &w
}

fn (mut t Transformer) merge_worker_used_fns(w &Transformer) {
	scoped := w.worker_scope != unsafe { nil }
	for name, used in w.used_fns {
		if used {
			owned_name := if scoped { name.clone() } else { name }
			t.used_fns[owned_name] = true
		}
	}
	for name, req in w.sum_eq_types {
		if name !in t.sum_eq_types {
			if scoped {
				t.sum_eq_types[name.clone()] = SumEqRequest{
					module:        req.module.clone()
					file:          req.file.clone()
					helper_module: req.helper_module.clone()
				}
			} else {
				t.sum_eq_types[name] = req
			}
		}
	}
}

// clone_scoped_worker_node moves a node's owned fields from a helper arena to
// the master's arena before the helper arena is released.
fn (mut t Transformer) clone_scoped_worker_node(idx int) {
	if idx < 0 || idx >= t.a.nodes.len {
		return
	}
	mut node := unsafe { &t.a.nodes[idx] }
	if node.value.len > 0 {
		node.value = node.value.clone()
	}
	if node.typ.len > 0 {
		node.typ = node.typ.clone()
	}
	if node.generic_params.len > 0 {
		mut params := []string{cap: node.generic_params.len}
		for param in node.generic_params {
			params << param.clone()
		}
		node.generic_params = params
	}
}

// merge_worker folds a finished worker's transformed output back into the master
// AST. The worker created its new nodes/children at indices base_nodes/base_children
// (matching the master at fork time); here they are appended to the master and every
// reference to a worker-local new node or new children block is shifted by the
// distance the block moved. `items` lists the function indices this worker owned, so
// their rewritten top-level nodes can be copied into place.
fn (mut t Transformer) merge_worker(w &Transformer, items []FnWorkItem, base_nodes int, base_children int) {
	node_shift := i32(t.a.nodes.len - base_nodes)
	child_shift := i32(t.a.children.len - base_children)
	// New children: bulk-copy the worker block, then relocate references to
	// worker-local new nodes in place (a per-element push paid a capacity check
	// and branch per child id).
	new_children := w.a.children.len - base_children
	if new_children > 0 {
		old_len := t.a.children.len
		unsafe {
			t.a.children.grow_len(new_children)
			// vmemmove: under the shared-base path the source region lives in
			// the same array; compaction copies leftward, which can touch the
			// source block's tail when regions are exactly packed.
			vmemmove(&t.a.children[old_len], &w.a.children[base_children],
				new_children * int(sizeof(flat.NodeId)))
		}
		for k in old_len .. t.a.children.len {
			cid := t.a.children[k]
			if int(cid) >= base_nodes {
				t.a.children[k] = flat.NodeId(int(cid) + int(node_shift))
			}
		}
	}
	// New nodes: bulk-copy the worker block, then relocate children_start that
	// points into the new children block in place (a per-element push paid a
	// capacity check, a branch and a struct copy per node).
	new_nodes := w.a.nodes.len - base_nodes
	if new_nodes > 0 {
		nodes_old_len := t.a.nodes.len
		unsafe {
			t.a.nodes.grow_len(new_nodes)
			vmemmove(&t.a.nodes[nodes_old_len], &w.a.nodes[base_nodes],
				new_nodes * int(sizeof(flat.Node)))
		}
		for k in nodes_old_len .. t.a.nodes.len {
			t.clear_typechecker_node_cache(k)
			if t.a.nodes[k].children_start >= base_children {
				t.a.nodes[k] = t.a.nodes[k].with_shifted_children(child_shift)
			}
			if w.worker_scope != unsafe { nil } {
				t.clone_scoped_worker_node(k)
			}
		}
	}
	// Rewritten top-level function nodes keep their original index in the master.
	for it in items {
		n := w.a.nodes[it.fn_idx]
		if n.children_start >= base_children {
			t.set_node(it.fn_idx, n.with_shifted_children(child_shift))
		} else {
			t.set_node(it.fn_idx, n)
		}
		if it.fn_idx >= 0 && it.fn_idx < t.transformed_fns.len {
			t.transformed_fns[it.fn_idx] = true
		}
		if w.worker_scope != unsafe { nil } {
			for idx in it.range_lo .. it.fn_idx + 1 {
				t.clone_scoped_worker_node(idx)
			}
		}
	}
	// Replay the call/fn-value resolutions the worker recorded for its
	// transform-created nodes (Transformer.copy_cloned_resolution writes into the
	// fork's private overlay; see fork_for_parallel_transform) into the master
	// under the shifted node ids, so or/return lowering in the late pass and
	// cgen see them exactly as after a serial transform.
	if !isnil(w.tc.fork_overlay) {
		for idx, name in w.tc.fork_overlay.resolved_call_names {
			shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
			owned_name := if w.worker_scope != unsafe { nil } { name.clone() } else { name }
			t.set_resolved_call_entry(shifted, owned_name)
		}
		for idx, name in w.tc.fork_overlay.resolved_fn_values {
			shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
			owned_name := if w.worker_scope != unsafe { nil } { name.clone() } else { name }
			t.set_resolved_fn_value_entry(shifted, owned_name)
		}
	}
	for name, used in w.used_fns {
		if used {
			owned_name := if w.worker_scope != unsafe { nil } { name.clone() } else { name }
			t.used_fns[owned_name] = true
		}
	}
	for message in w.monomorph_errors {
		owned_message := if w.worker_scope != unsafe { nil } { message.clone() } else { message }
		t.record_monomorph_error(owned_message)
	}
	if w.ignored_comptime_for_nodes.len > 0 {
		if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
			t.ignored_comptime_for_nodes << []bool{len: t.a.nodes.len - t.ignored_comptime_for_nodes.len}
		}
		for idx, ignored in w.ignored_comptime_for_nodes {
			if !ignored {
				continue
			}
			shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
			if shifted >= 0 && shifted < t.ignored_comptime_for_nodes.len {
				t.ignored_comptime_for_nodes[shifted] = true
			}
		}
	}
}

fn (mut t Transformer) set_resolved_call_entry(idx int, name string) {
	for t.tc.resolved_call_names.len <= idx {
		t.tc.resolved_call_names << ''
		t.tc.resolved_call_set << false
	}
	t.tc.resolved_call_names[idx] = name
	t.tc.resolved_call_set[idx] = true
}

fn (mut t Transformer) set_resolved_fn_value_entry(idx int, name string) {
	for t.tc.resolved_fn_value_names.len <= idx {
		t.tc.resolved_fn_value_names << ''
		t.tc.resolved_fn_value_set << false
	}
	t.tc.resolved_fn_value_names[idx] = name
	t.tc.resolved_fn_value_set[idx] = true
}

fn (mut t Transformer) clear_typechecker_node_cache(idx int) {
	if isnil(t.tc) || idx < 0 {
		return
	}
	if idx < t.tc.resolved_call_set.len {
		t.tc.resolved_call_names[idx] = ''
		t.tc.resolved_call_set[idx] = false
	}
	if idx < t.tc.resolved_fn_value_set.len {
		t.tc.resolved_fn_value_names[idx] = ''
		t.tc.resolved_fn_value_set[idx] = false
	}
	if idx < t.tc.expr_type_set.len {
		t.tc.expr_type_set[idx] = false
	}
	if idx < t.tc.statement_nodes.len {
		t.tc.statement_nodes[idx] = false
	}
	// The sparse maps are only populated while the checker itself runs in
	// sparse mode; after the parallel-check merge they are empty on the master.
	// Deleting from an empty map still hashes the key — and this runs once per
	// merged node — so guard on emptiness.
	if t.tc.sparse_resolved_call_names.len > 0 {
		t.tc.sparse_resolved_call_names.delete(idx)
	}
	if t.tc.sparse_resolved_fn_values.len > 0 {
		t.tc.sparse_resolved_fn_values.delete(idx)
	}
	if t.tc.sparse_expr_type_values.len > 0 {
		t.tc.sparse_expr_type_values.delete(idx)
	}
	if t.tc.sparse_statement_nodes.len > 0 {
		t.tc.sparse_statement_nodes.delete(idx)
	}
}

// split_work_items distributes items across `n` buckets using greedy
// least-loaded-by-cost assignment, so heavy functions are spread evenly. The
// assignment is deterministic for a given input (required for reproducible builds).
// Buckets are seeded with a virtual load matching their thread's start delay in
// the spawn chain (see run_parallel_transform): bucket 0 (the master) starts
// after one AST clone, worker k starts after k+1 clones plus its own
// clone-for-successor, and the last worker clones nothing. One `unit`
// approximates one clone-time in cost terms, so all threads finish together.
fn split_work_items(items []FnWorkItem, n int) [][]FnWorkItem {
	return split_work_items_ex(items, n, true)
}

// split_work_items_ex is split_work_items with the spawn-chain stagger bias
// made optional: the shared-base path spawns every worker up front, so only
// the master keeps a lighter share (it pays for the merges afterwards).
fn split_work_items_ex(items []FnWorkItem, n int, chain_stagger bool) [][]FnWorkItem {
	mut buckets := [][]FnWorkItem{len: n, init: []FnWorkItem{}}
	mut loads := []i64{len: n}
	if n > 1 {
		mut total := i64(0)
		for it in items {
			total += i64(it.cost) + 1
		}
		unit := total / i64(n * 16)
		if chain_stagger {
			// The master (bucket 0) also pays for the serial pre-phase warmup, the
			// first worker clone, and the interleaved merges, and measures slower
			// per cost unit than the helpers; give it a markedly lighter share.
			loads[0] = unit * 5
			for b in 1 .. n {
				if b == n - 1 {
					loads[b] = unit * i64(b)
				} else {
					loads[b] = unit * i64(b + 1)
				}
			}
		} else {
			// All workers start together. Stagger the worker shares slightly so
			// the workers merged first finish first — the master then folds each
			// one in while the later ones are still running — and keep the
			// master's own share a touch lighter than even to cover the final
			// merge tail.
			loads[0] = unit * 3
			for b in 1 .. n {
				loads[b] = unit * i64(n - b) / 2
			}
		}
	}
	mut sorted := items.clone()
	sorted.sort(a.rank > b.rank)
	for it in sorted {
		mut best := 0
		for b in 1 .. n {
			if loads[b] < loads[best] {
				best = b
			}
		}
		buckets[best] << it
		loads[best] += i64(it.cost) + 1
	}
	// Each bucket is processed sequentially by one thread: order its items by
	// AST position, which groups functions of the same module together (the
	// per-module alias/type-normalization caches are cleared on every module
	// switch, and rank order interleaves modules almost per item) and walks the
	// node arrays roughly sequentially.
	for bi in 0 .. n {
		buckets[bi].sort(a.fn_idx < b.fn_idx)
	}
	return buckets
}

// should_transform_fn reports whether should transform fn applies in transform.
fn (t &Transformer) should_transform_fn(node flat.Node) bool {
	if !t.has_used_fn_filter() {
		return true
	}
	if transform_is_generated_fn_after_markused(node.value) {
		return true
	}
	if t.cur_module == 'builtin' && node.value == 'exit' {
		return true
	}
	if node.value.contains('[') {
		base_value := generic_fn_decl_base_value(node.value)
		if base_value != node.value && t.used_fn_contains_in_module(base_value, t.cur_module) {
			return true
		}
	}
	return t.used_fn_contains_in_module(node.value, t.cur_module)
}

fn transform_is_generated_fn_after_markused(name string) bool {
	return name.starts_with('__anon_fn_') || name.contains('.__anon_fn_')
}

fn (mut t Transformer) transform_late_used_fn_bodies(names []string, node_limit int) {
	if names.len == 0 || node_limit <= 0 {
		return
	}
	mut late := map[string]bool{}
	mut queued := map[string]bool{}
	mut pending := []string{}
	old_module := t.cur_module
	old_file := t.cur_file
	t.cur_module = ''
	t.cur_file = ''
	for name in names {
		t.mark_fn_used_name(name)
		add_late_used_fn_name(name, mut late, mut pending, mut queued)
	}
	t.cur_module = old_module
	t.cur_file = old_file
	limit := if node_limit < t.a.nodes.len { node_limit } else { t.a.nodes.len }
	// Build the fn_decl candidate list once. The scan range [0, limit) is fixed:
	// transform_fn_body rewrites the current fn_decl in place and appends new
	// nodes beyond `limit` (those generated nodes are handled inline below); it
	// never inserts or moves existing top-level nodes below `limit`. So the set
	// of candidate fn_decls, their (file, module) context, and their generic-ness
	// never change across rounds. Re-deriving all of this every round previously
	// made this pass O(pending * nodes): a full node walk (millions of
	// node_kind_id calls) plus a repeated fn_decl_has_unresolved_generics check
	// on every non-matching fn_decl.
	mut candidates := []LateFnCandidate{}
	mut scan_file := ''
	mut scan_module := ''
	for i in 0 .. limit {
		node := t.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			scan_file = node.value
			scan_module = ''
		} else if kind_id == 73 {
			scan_module = node.value
		} else if kind_id == 61 && !(i < t.transformed_fns.len && t.transformed_fns[i])
			&& !t.fn_decl_has_unresolved_generics(node, scan_module) {
			candidates << LateFnCandidate{
				idx:    i
				file:   scan_file
				module: scan_module
			}
		}
	}
	// Index the candidates by every used-set spelling under which
	// late_used_fn_matches can match them, so each pending name maps straight to
	// its fn_decls. Rescanning the whole candidate list per pending name (as this
	// pass used to) is O(pending * candidates), with two c_name allocations per
	// probe — the dominant transform cost on compiler-sized inputs.
	mut candidate_index := map[string][]int{}
	for ci, cand in candidates {
		for key in late_candidate_match_keys(t.a.nodes[cand.idx].value, cand.module) {
			candidate_index[key] << ci
		}
	}
	was_log_active := t.used_fns_log_active
	t.used_fns_log_active = true
	for pending.len > 0 {
		name := pending.pop()
		t.transform_late_candidates_for(name, candidate_index, mut candidates, mut late, mut
			pending, mut queued)
	}
	t.used_fns_log_active = was_log_active
}

// late_candidate_match_keys returns every used-set spelling under which the
// fn_decl `value` declared in `module_name` is matched by late_used_fn_matches,
// including the generic base-value spellings.
fn late_candidate_match_keys(value string, module_name string) []string {
	mut keys := late_name_spellings(value, module_name)
	if value.contains('[') {
		base_value := generic_fn_decl_base_value(value)
		if base_value != value {
			keys << late_name_spellings(base_value, module_name)
		}
	}
	return keys
}

// late_name_spellings returns the used-set keys probed by
// late_used_fn_contains_in_module(used, name, module_name).
fn late_name_spellings(name string, module_name string) []string {
	if name.len == 0 {
		return []string{}
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		if name.starts_with('${module_name}.') {
			return [name, c_name(name)]
		}
		dfn := '${module_name}.${name}'
		return [dfn, c_name(dfn)]
	}
	return [name, c_name(name)]
}

// transform_late_candidates_for transforms the bodies of the not-yet-processed
// candidates matched by the late name `name`. add_late_used_fn_name records both
// the plain and the c_name spelling in `late`, so both are looked up here.
fn (mut t Transformer) transform_late_candidates_for(name string, candidate_index map[string][]int, mut candidates []LateFnCandidate, mut late map[string]bool, mut pending []string, mut queued map[string]bool) {
	spellings := [name, c_name(name)]
	for key in spellings {
		for ci in candidate_index[key] {
			if candidates[ci].processed {
				continue
			}
			node := t.a.nodes[candidates[ci].idx]
			if !late_used_fn_matches(late, node, candidates[ci].module) {
				continue
			}
			t.transform_late_candidate(ci, mut candidates, mut late, mut pending, mut queued)
		}
	}
}

// transform_late_candidate transforms one matched candidate's body and enqueues
// the call names that became used during that transform.
fn (mut t Transformer) transform_late_candidate(ci int, mut candidates []LateFnCandidate, mut late map[string]bool, mut pending []string, mut queued map[string]bool) {
	candidates[ci].processed = true
	idx := candidates[ci].idx
	t.cur_file = candidates[ci].file
	t.cur_module = candidates[ci].module
	log_start := t.used_fns_log.len
	node_count_before := t.a.nodes.len
	t.transform_fn_body(idx)
	for call_name in t.generated_fn_body_call_names(flat.NodeId(idx)) {
		t.enqueue_late_used_call_name(call_name, log_start, mut late, mut pending, mut queued)
	}
	for j in node_count_before .. t.a.nodes.len {
		generated := t.a.nodes[j]
		if generated.kind != .fn_decl || !transform_is_generated_fn_after_markused(generated.value) {
			continue
		}
		for call_name in t.generated_fn_body_call_names(flat.NodeId(j)) {
			t.enqueue_late_used_call_name(call_name, log_start, mut late, mut pending, mut queued)
		}
	}
}

// LateFnCandidate is a non-generic fn_decl reachable by the late-used-fn-bodies
// pass, together with the file/module context resolved for it during the single
// structural scan in transform_late_used_fn_bodies.
struct LateFnCandidate {
	idx    int
	file   string
	module string
mut:
	processed bool
}

fn (mut t Transformer) enqueue_late_used_call_name(name string, log_start int, mut late map[string]bool, mut pending []string, mut queued map[string]bool) {
	if name.len == 0 {
		return
	}
	was_used := t.late_name_was_used_before(name, log_start)
	t.mark_fn_used_name(name)
	if was_used {
		return
	}
	add_late_used_fn_name(name, mut late, mut pending, mut queued)
}

// late_name_was_used_before reports whether `name` (under the current module)
// was already in used_fns before position `log_start` of the insertion log —
// i.e. before the current candidate body's transform started. It probes the
// same spellings the old used_fns-snapshot check did.
fn (t &Transformer) late_name_was_used_before(name string, log_start int) bool {
	if t.late_spelling_was_used_before(name, log_start) {
		return true
	}
	if t.late_spelling_was_used_before(c_name(name), log_start) {
		return true
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
		&& !name.starts_with('${t.cur_module}.') {
		dfn := '${t.cur_module}.${name}'
		return t.late_spelling_was_used_before(dfn, log_start)
			|| t.late_spelling_was_used_before(c_name(dfn), log_start)
	}
	return false
}

fn (t &Transformer) late_spelling_was_used_before(spelling string, log_start int) bool {
	if !t.used_fns[spelling] {
		return false
	}
	for i in log_start .. t.used_fns_log.len {
		if t.used_fns_log[i] == spelling {
			return false
		}
	}
	return true
}

fn add_late_used_fn_name(name string, mut late map[string]bool, mut pending []string, mut queued map[string]bool) {
	if name.len == 0 {
		return
	}
	late[name] = true
	late[c_name(name)] = true
	if !queued[name] {
		queued[name] = true
		pending << name
	}
}

fn late_used_fn_matches(used map[string]bool, node flat.Node, module_name string) bool {
	if late_used_fn_contains_in_module(used, node.value, module_name) {
		return true
	}
	if node.value.contains('[') {
		base_value := generic_fn_decl_base_value(node.value)
		return base_value != node.value
			&& late_used_fn_contains_in_module(used, base_value, module_name)
	}
	return false
}

fn late_used_fn_contains_in_module(used map[string]bool, name string, module_name string) bool {
	if name.len == 0 {
		return false
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& name.starts_with('${module_name}.') {
		return used[name] || used[c_name(name)]
	}
	dfn := transform_dotted_fn_name(module_name, name)
	qfn := c_name(dfn)
	if used[dfn] || used[qfn] {
		return true
	}
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		cfn := c_name(name)
		return used[name] || used[cfn]
	}
	return false
}

fn (t &Transformer) has_used_fn_filter() bool {
	return t.used_fns.len > 0 && t.used_fns['main']
}

fn (t &Transformer) used_fn_contains_name(name string) bool {
	return name.len > 0 && t.used_fns[name]
}

fn (t &Transformer) used_fn_contains_in_module(name string, module_name string) bool {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& name.starts_with('${module_name}.') {
		if t.used_fn_contains_name(name) {
			return true
		}
		cfn := c_name(name)
		if cfn != name && t.used_fn_contains_name(cfn) {
			return true
		}
	}
	dfn := transform_dotted_fn_name(module_name, name)
	qfn := c_name(dfn)
	if t.used_fn_contains_name(dfn) || t.used_fn_contains_name(qfn) {
		return true
	}
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		cfn := c_name(name)
		return t.used_fn_contains_name(name) || t.used_fn_contains_name(cfn)
	}
	return false
}

fn transform_dotted_fn_name(mod string, name string) string {
	if mod.len > 0 && mod != 'main' && mod != 'builtin' {
		return '${mod}.${name}'
	}
	return name
}

// transform_qualified_fn_name transforms transform qualified fn name data for transform.
fn transform_qualified_fn_name(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

// transform_const_decl transforms the initializer expression of each const field
// so that const-level lowering (e.g. string concatenation in the prelude's
// embedded data tables) happens in the transformer rather than the backend.
fn (mut t Transformer) transform_const_decl(node flat.Node) {
	old_in_const_init := t.in_const_init
	t.in_const_init = true
	for ci in 0 .. node.children_count {
		cf_id := t.a.child(&node, ci)
		if int(cf_id) < 0 {
			continue
		}
		cf := t.a.nodes[int(cf_id)]
		if cf.kind == .const_field && cf.children_count >= 1 && cf.children_start >= 0 {
			val_id := t.a.child(&cf, 0)
			if int(val_id) < 0 {
				continue
			}
			val := t.a.nodes[int(val_id)]
			if block_val := t.const_block_value(val) {
				new_val := t.transform_const_value(block_val)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .string_interp {
				new_val := t.transform_const_string_interp(val_id, val)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .or_expr {
				const_typ := t.const_field_type_name(cf)
				new_val := t.transform_const_or_expr(val_id, val, const_typ)
				t.a.children[cf.children_start] = new_val
			} else if val.kind in [.struct_init, .cast_expr, .call, .array_literal, .array_init,
				.fn_literal, .lambda_expr] {
				new_val := t.transform_const_expr_no_pending(val_id)
				t.a.children[cf.children_start] = new_val
			} else if val.kind == .infix && val.children_count >= 2 {
				new_val := t.transform_const_expr_no_pending(val_id)
				// Overwrite the field's value slot in place (each const_field owns
				// its own single-element child range, so this is safe).
				t.a.children[cf.children_start] = new_val
			}
		}
	}
	t.in_const_init = old_in_const_init
}

// const_field_type_name supports const field type name handling for Transformer.
fn (t &Transformer) const_field_type_name(field flat.Node) string {
	if field.value.len > 0 {
		if t.cur_module.len > 0 {
			if typ := t.const_type_name('${t.cur_module}.${field.value}') {
				return typ
			}
		}
		if typ := t.const_type_name(field.value) {
			return typ
		}
	}
	return field.typ
}

fn (mut t Transformer) transform_const_expr_no_pending(id flat.NodeId) flat.NodeId {
	old_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	transformed := t.transform_expr(id)
	pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	t.pending_stmts = old_pending
	if pending.len == 0 {
		return transformed
	}
	// A lowering left prerequisite statements (`.map()` chain temps); the
	// expression is unusable without them, so pack them into a block value
	// the const emitter renders as a braced runtime init.
	start := t.a.children.len
	for stmt in pending {
		t.a.children << stmt
	}
	t.a.children << transformed
	return t.a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: flat.child_count(pending.len + 1)
		typ:            t.node_type(transformed)
	})
}

// transform_const_or_expr transforms transform const or expr data for transform.
fn (mut t Transformer) transform_const_or_expr(_id flat.NodeId, node flat.Node, const_typ string) flat.NodeId {
	if node.children_count < 2 {
		return _id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 0 {
			children << t.transform_const_expr_no_pending(child_id)
		} else {
			children << child_id
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           .or_expr
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            if const_typ.len > 0 { const_typ } else { node.typ }
	})
}

// transform_global_decl transforms transform global decl data for transform.
fn (mut t Transformer) transform_global_decl(node flat.Node) {
	for ci in 0 .. node.children_count {
		gf_id := t.a.child(&node, ci)
		if int(gf_id) < 0 {
			continue
		}
		gf := t.a.nodes[int(gf_id)]
		if gf.kind == .field_decl && gf.children_count >= 1 && gf.children_start >= 0 {
			val_id := t.a.child(&gf, 0)
			if int(val_id) < 0 {
				continue
			}
			val := t.a.nodes[int(val_id)]
			if preserved := t.transform_global_amp_initializer(val_id, val) {
				t.a.children[gf.children_start] = preserved
				continue
			}
			old_pending := t.pending_stmts.clone()
			t.pending_stmts.clear()
			new_val := t.transform_expr(val_id)
			has_pending := t.pending_stmts.len > 0
			t.pending_stmts.clear()
			t.pending_stmts = old_pending
			if !has_pending {
				t.a.children[gf.children_start] = new_val
			}
		}
	}
}

// transform_global_amp_initializer transforms transform global amp initializer data for transform.
fn (mut t Transformer) transform_global_amp_initializer(val_id flat.NodeId, val flat.Node) ?flat.NodeId {
	if val.kind != .prefix || val.op != .amp || val.children_count != 1 {
		return none
	}
	child_id := t.a.child(&val, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .assoc {
		return val_id
	}
	old_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut result := flat.empty_node
	if child.kind == .struct_init {
		if preserved := t.transform_amp_struct_init_for_type(val_id, val, val.typ) {
			result = preserved
		}
	} else if child.kind == .cast_expr {
		if preserved := t.transform_global_amp_interface_cast(val, val.typ) {
			result = preserved
		}
	}
	has_pending := t.pending_stmts.len > 0
	t.pending_stmts.clear()
	t.pending_stmts = old_pending
	if has_pending || int(result) < 0 {
		return none
	}
	return result
}

// transform_const_value transforms transform const value data for transform.
fn (mut t Transformer) transform_const_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if block_val := t.const_block_value(node) {
		return t.transform_const_value(block_val)
	}
	if node.kind == .map_init {
		return id
	}
	return t.transform_expr(id)
}

// const_block_value supports const block value handling for Transformer.
fn (t &Transformer) const_block_value(node flat.Node) ?flat.NodeId {
	if node.kind != .block || node.children_count == 0 {
		return none
	}
	for i := int(node.children_count) - 1; i >= 0; i-- {
		stmt_id := t.a.child(&node, i)
		stmt := t.a.nodes[int(stmt_id)]
		if stmt.kind == .empty {
			continue
		}
		if stmt.kind == .expr_stmt && stmt.children_count == 1 {
			return t.a.child(&stmt, 0)
		}
		break
	}
	return none
}

// transform_const_string_interp transforms transform const string interp data for transform.
fn (mut t Transformer) transform_const_string_interp(_id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_string_literal('')
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut parts := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		parts << t.transform_string_interp_part(child_id)
	}
	mut expr := parts[0]
	for i in 1 .. parts.len {
		expr = t.make_call_typed('string__plus', arr2(expr, parts[i]), 'string')
	}
	mut stmts := []flat.NodeId{}
	t.drain_pending(mut stmts)
	t.pending_stmts = outer_pending
	if stmts.len == 0 {
		return expr
	}
	stmts << t.make_expr_stmt(expr)
	return t.make_block(stmts)
}

fn (mut t Transformer) transform_string_interp_part(child_id flat.NodeId) flat.NodeId {
	mut expr_id := child_id
	mut format := ''
	child := t.a.nodes[int(child_id)]
	if child.kind == .directive && child.value == 'string_interp_format' && child.children_count > 0 {
		expr_id = t.a.child(&child, 0)
		format = child.typ
	}
	t.mark_string_interp_call_part_used(expr_id)
	mut transformed := t.transform_expr(expr_id)
	mut typ := t.raw_alias_type_for_expr(expr_id)
	if typ.len == 0 {
		typ = t.node_type(transformed)
	}
	if typ.len == 0 {
		typ = t.reliable_stringify_type(transformed)
	}
	if typ.len == 0 {
		typ = t.reliable_stringify_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.node_type(expr_id)
	}
	expr_node := t.a.nodes[int(expr_id)]
	if expr_node.kind == .ident && t.string_interp_needs_value_read(expr_node.value, typ) {
		transformed = t.make_prefix(.mul, transformed)
		typ = typ[1..]
	}
	if typ.len == 0 {
		typ = 'string'
	}
	return t.wrap_formatted_string_conversion(transformed, typ, format)
}

fn (mut t Transformer) mark_string_interp_call_part_used(expr_id flat.NodeId) {
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(expr_id)]
	if node.kind != .call {
		return
	}
	call_name := t.call_name_for_node(expr_id, node)
	if call_name.len > 0 {
		t.mark_fn_used_name(call_name)
		return
	}
	if node.children_count == 0 {
		return
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 || int(fn_id) >= t.a.nodes.len {
		return
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .ident && fn_node.value.len > 0 {
		t.mark_fn_used_name(fn_node.value)
		return
	}
	if fn_node.kind == .selector && fn_node.value.len > 0 && fn_node.children_count > 0 {
		base_id := t.a.child(&fn_node, 0)
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 {
			t.mark_fn_used_name(method_name)
		}
	}
}

fn (t &Transformer) string_interp_needs_value_read(name string, typ string) bool {
	if !typ.starts_with('&') {
		return false
	}
	if t.mut_param_values[name] {
		return true
	}
	source_type := t.var_type(name)
	if source_type.len == 0 || source_type.starts_with('&') {
		return false
	}
	return t.normalize_type_alias(source_type) == t.normalize_type_alias(typ[1..])
}

// transform_fn_body transforms transform fn body data for transform.
// try_heap_escaping_amp reports whether a `p := &v` decl is an escaping address of
// a value local that must be heap-copied: `p` is in escaping_amp_ptrs (returned)
// and `v` resolves to a non-reference value type.
fn (t &Transformer) try_heap_escaping_amp(node flat.Node, rhs_id flat.NodeId) bool {
	lhs := t.a.nodes[int(t.a.child(&node, 0))]
	if lhs.kind != .ident || lhs.value !in t.escaping_amp_ptrs {
		return false
	}
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind != .prefix || rhs.op != .amp || rhs.children_count == 0 {
		return false
	}
	amp_child := t.a.child(&rhs, 0)
	amp_node := t.a.nodes[int(amp_child)]
	if amp_node.kind != .ident {
		return false
	}
	// The source local was moved to the heap at its declaration: the alias is now just that
	// `&T` pointer (handled below), regardless of its rewritten pointer type.
	if amp_node.value in t.heaped_amp_locals {
		return true
	}
	local_type := t.node_type(amp_child)
	return local_type.len > 0 && !local_type.starts_with('&') && !local_type.starts_with('[]')
		&& !local_type.starts_with('map[') && !local_type.starts_with('?')
		&& !local_type.starts_with('!')
}

// heap_escaping_amp_rhs rewrites `&v` into `(&T)memdup(&v, sizeof(T))`, a heap copy
// of the value local `v` so the escaping pointer outlives the stack frame. When `v` was
// itself moved to the heap at its declaration, the alias is simply that pointer — copying
// would resurrect the stale-mutation bug the move avoids.
fn (mut t Transformer) heap_escaping_amp_rhs(rhs_id flat.NodeId) flat.NodeId {
	rhs := t.a.nodes[int(rhs_id)]
	amp_child := t.a.child(&rhs, 0)
	amp_node := t.a.nodes[int(amp_child)]
	if amp_node.kind == .ident && amp_node.value in t.heaped_amp_locals {
		// The source local was itself moved to the heap at its declaration, so it *is* the
		// `&T` heap pointer; the alias is simply that pointer. Suppress the pointer-value
		// rvalue auto-deref while lowering it, otherwise `transform_ident_expr` would turn
		// `v` into `*v` (a stale stack value) and initialize `p`'s `&T` decl from a `T`,
		// reviving the very stale-mutation bug the heap move exists to avoid.
		had_rvalue := amp_node.value in t.pointer_value_rvalues
		if had_rvalue {
			t.pointer_value_rvalues.delete(amp_node.value)
		}
		transformed := t.transform_expr(amp_child)
		if had_rvalue {
			t.pointer_value_rvalues[amp_node.value] = true
		}
		return transformed
	}
	local_type := t.node_type(amp_child)
	addr := t.make_prefix(.amp, t.transform_expr(amp_child))
	size := t.make_sizeof_type(local_type)
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	return t.make_cast('&${local_type}', dup, '&${local_type}')
}

// heapable_value_type reports whether a local of this declared type can be moved to the heap
// as a `&T` — a plain value type, not an already-reference / container / optional type (those
// either carry their own indirection or are not addressable as a single `T`).
fn (t &Transformer) heapable_value_type(typ string) bool {
	return typ.len > 0 && !typ.starts_with('&') && !typ.starts_with('[]')
		&& !typ.starts_with('map[') && !typ.starts_with('?') && !typ.starts_with('!')
		&& !typ.starts_with('[') && typ != 'unknown' && typ != 'void'
}

// heap_escaping_source_decl rewrites `mut v := <init>` (where `&v` escapes) into a heap
// allocation so `v` is a `&T` to a heap object. A struct literal becomes `&T{..}` (the cgen
// memdup's it); any other initializer is copied into a stack temp and memdup'd. Subsequent
// `v.field = ..` writes then mutate the heap object the returned pointer alias also sees.
fn (mut t Transformer) heap_escaping_source_decl(node flat.Node, var_name string, elem_typ string) []flat.NodeId {
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	ptr_typ := '&${elem_typ}'
	mut stmts := []flat.NodeId{}
	transformed_init := t.transform_expr(rhs_id)
	// Statements lifted out while transforming the initializer must precede the heap decl.
	t.drain_pending(mut stmts)
	mut heap_rhs := flat.NodeId(0)
	if rhs.kind == .struct_init {
		heap_rhs = t.make_prefix(.amp, transformed_init)
	} else {
		tmp := t.new_temp('esc')
		stmts << t.make_decl_assign_typed(tmp, transformed_init, elem_typ)
		addr := t.make_prefix(.amp, t.make_ident(tmp))
		size := t.make_sizeof_type(elem_typ)
		dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
		heap_rhs = t.make_cast(ptr_typ, dup, ptr_typ)
	}
	t.heaped_amp_locals[var_name] = true
	// The local is now a `&T`, so its compound/postfix mutations (`v += 1`, `v++`) must store
	// through the pointer (`*v += 1`); mark it as a pointer-value lvalue so that lowering fires.
	t.pointer_value_lvalues[var_name] = true
	t.pointer_value_rvalues[var_name] = true
	stmts << t.make_decl_assign_typed(var_name, heap_rhs, ptr_typ)
	return stmts
}

// mark_escaping_amp_ptrs runs a structural pre-pass over a function body to find
// `p := &v` declarations whose pointer `p` is later returned. Such a `v` is a local
// value whose address escapes, so it must be heap-copied (V auto-heaps it); the
// names are recorded in `escaping_amp_ptrs` and consumed by the decl-assign
// transform. Purely structural (no type info needed here): the type check happens
// at rewrite time when `v`'s type is known.
fn (mut t Transformer) mark_escaping_amp_ptrs(body_ids []flat.NodeId) {
	t.reset_escaping_amp_state()
	mut amp_ptrs := map[string]bool{}
	mut amp_sources := map[string]string{} // pointer `p` -> source local `v`
	mut ptr_aliases := map[string]string{} // copy `q := p` -> aliased pointer `p`
	mut returned := map[string]bool{}
	for id in body_ids {
		t.scan_escape_pass(id, mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut returned)
	}
	// A pointer may be returned through a copy (`p := &v; q := p; return q`): `q` is collected
	// as returned but `p` is not, so propagate "returned" backward along the `q := p` aliases
	// until a fixpoint. Then `p` (and its source `v`) is recognised as escaping below.
	for _ in 0 .. ptr_aliases.len {
		mut changed := false
		for q, p in ptr_aliases {
			if q in returned && p !in returned {
				returned[p] = true
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	for name, _ in amp_ptrs {
		if name in returned {
			t.escaping_amp_ptrs[name] = true
			if src := amp_sources[name] {
				t.escaping_amp_sources[src] = true
			}
		}
	}
}

fn (mut t Transformer) reset_escaping_amp_state() {
	t.escaping_amp_ptrs.clear()
	t.escaping_amp_sources.clear()
	t.heaped_amp_locals.clear()
	// Cleared per function: heaped locals add their names below (in heap_escaping_source_decl);
	// for-loop element vars set and restore their own entries within the loop body.
	t.pointer_value_lvalues.clear()
	t.pointer_value_rvalues.clear()
}

// scan_escape_pass recursively collects, in a function-body subtree, (a) the LHS
// names of `p := &ident` declarations into `amp_ptrs` (and the source `ident` into
// `amp_sources[p]`), (b) plain pointer copies `q := p` into `ptr_aliases[q] = p`, and
// (c) every ident name appearing inside a return statement into `returned`.
fn (mut t Transformer) scan_escape_pass(id flat.NodeId, mut amp_ptrs map[string]bool, mut amp_sources map[string]string, mut ptr_aliases map[string]string, mut returned map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .decl_assign && node.children_count == 2 {
		lhs := t.a.nodes[int(t.a.child(&node, 0))]
		rhs := t.a.nodes[int(t.a.child(&node, 1))]
		if lhs.kind == .ident && lhs.value.len > 0 && rhs.kind == .prefix && rhs.op == .amp
			&& rhs.children_count > 0 {
			amp_child := t.a.nodes[int(t.a.child(&rhs, 0))]
			if amp_child.kind == .ident {
				amp_ptrs[lhs.value] = true
				amp_sources[lhs.value] = amp_child.value
			}
		} else if lhs.kind == .ident && lhs.value.len > 0 && rhs.kind == .ident && rhs.value.len > 0 {
			// `q := p` aliases an existing pointer; recorded so a returned alias still marks the
			// underlying `p := &v` as escaping.
			ptr_aliases[lhs.value] = rhs.value
		}
	}
	if node.kind == .return_stmt {
		for i in 0 .. node.children_count {
			t.collect_return_escape_idents(t.a.child(&node, i), mut returned)
		}
	}
	for i in 0 .. node.children_count {
		t.scan_escape_pass(t.a.child(&node, i), mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut
			returned)
	}
}

// collect_return_escape_idents gathers the idents in a return-expression subtree that occupy an
// actual escape position — the returned value itself, or a member of a returned aggregate
// (struct/array/map literal, multi-return). It deliberately stops at operators that consume their
// operands into a fresh value: infix (`==`, `&&`, arithmetic, …), postfix, `is`/`in`, and any
// non-`&` prefix (deref `*p`, `!x`, `-x`). That way a pointer that is merely compared or
// dereferenced in the return expression — e.g. `return p == p && v == 1` — is not mistaken for a
// pointer that escapes, so its source local is not needlessly heap-moved (which would also make
// later non-pointer uses of that local read through an `int*`).
fn (mut t Transformer) collect_return_escape_idents(id flat.NodeId, mut names map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if node.value.len > 0 {
				names[node.value] = true
			}
			return
		}
		.infix, .postfix, .is_expr, .in_expr {
			// These yield a new scalar/bool; their operands do not escape through the return.
			return
		}
		.prefix {
			// `&x` propagates an address (which may escape); any other prefix (`*x`, `!x`, `-x`)
			// produces a fresh value, so its operand does not escape.
			if node.op != .amp {
				return
			}
		}
		else {}
	}

	for i in 0 .. node.children_count {
		t.collect_return_escape_idents(t.a.child(&node, i), mut names)
	}
}

fn (mut t Transformer) transform_fn_body(fn_idx int) {
	if fn_idx >= 0 && fn_idx < t.transformed_fns.len {
		t.transformed_fns[fn_idx] = true
	}
	fn_node := t.a.nodes[fn_idx]
	t.cur_fn_name = fn_node.value
	old_is_generic := t.cur_fn_is_generic
	t.cur_fn_is_generic = if t.skip_generics {
		false
	} else {
		t.fn_decl_has_unresolved_generics(fn_node, t.cur_module)
	}
	param_count := t.fn_body_param_count(fn_node)
	param_types := t.fn_body_param_types(fn_node, param_count)
	t.cur_fn_ret_type = t.fn_body_return_type(fn_node)
	t.reset_var_types()
	t.smartcast_stack.clear()
	t.invalidated_smartcasts.clear()
	// Collect param types
	mut param_idx := 0
	mut source_mut_params := []string{}
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) == 75 && child.value.len > 0 {
			raw_source_typ := if child.typ.starts_with('...') {
				'[]' + child.typ[3..]
			} else {
				child.typ
			}
			raw_typ := if child.typ.len > 0 {
				if child.typ.starts_with('...') {
					'[]' + t.normalize_type_alias(child.typ[3..])
				} else {
					t.normalize_type_alias(child.typ)
				}
			} else {
				''
			}
			mut typ := if raw_typ.len > 0 {
				raw_typ
			} else if param_idx < param_types.len {
				t.normalize_type_alias(param_types[param_idx].name())
			} else if param_idx == 0 {
				t.fn_body_receiver_type(fn_node.value)
			} else {
				''
			}
			if typ.starts_with('&') && raw_typ.len > 0 && !raw_typ.starts_with('&')
				&& t.normalize_type_alias(typ[1..]) == raw_typ {
				typ = raw_typ
			}
			if typ.len > 0 {
				t.set_var_type_with_raw(child.value, typ, raw_source_typ)
			}
			if child.is_mut || child.op == .amp || child.typ.starts_with('mut ') {
				t.mut_param_values[child.value] = true
				source_mut_params << child.value
			}
			param_idx++
		}
	}
	mut body_ids := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) != 75 {
			body_ids << child_id
		}
	}
	if t.cur_fn_ret_type == 'void' {
		t.reset_escaping_amp_state()
	} else {
		t.mark_escaping_amp_ptrs(body_ids)
	}
	for name in source_mut_params {
		t.pointer_value_lvalues[name] = true
	}
	new_body := t.transform_stmts(body_ids)
	// Rebuild function children: params then new body
	start := t.a.children.len
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if node_kind_id(child) == 75 {
			t.a.children << child_id
		}
	}
	for id in new_body {
		t.a.children << id
	}
	count := t.a.children.len - start
	t.set_node(fn_idx, flat.Node{
		kind:           .fn_decl
		kind_id:        61
		op:             fn_node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            fn_node.pos
		value:          fn_node.value
		typ:            fn_node.typ
		generic_params: fn_node.generic_params
	})
	t.smartcast_stack.clear()
	t.invalidated_smartcasts.clear()
	t.cur_fn_is_generic = old_is_generic
}

// fn_body_param_types supports fn body param types handling for Transformer.
fn (t &Transformer) fn_body_param_types(fn_node flat.Node, expected int) []types.Type {
	if isnil(t.tc) {
		return []types.Type{}
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${fn_node.value}'
		if params := t.fn_param_types_for_name(qname, expected) {
			return params
		}
		cqname := c_name(qname)
		if cqname != qname {
			if params := t.fn_param_types_for_name(cqname, expected) {
				return params
			}
		}
	}
	if params := t.fn_param_types_for_name(fn_node.value, expected) {
		return params
	}
	cname := c_name(fn_node.value)
	if cname != fn_node.value {
		if params := t.fn_param_types_for_name(cname, expected) {
			return params
		}
	}
	return []types.Type{}
}

fn (t &Transformer) fn_body_receiver_type(fn_name string) string {
	if !fn_name.contains('.') {
		return ''
	}
	receiver := fn_name.all_before_last('.')
	if receiver.len == 0 {
		return ''
	}
	typ := t.normalize_type_in_module(receiver, t.cur_module)
	if typ.len == 0 || typ == fn_name {
		return ''
	}
	return typ
}

// fn_param_types_for_name supports fn param types for name handling for Transformer.
fn (t &Transformer) fn_param_types_for_name(name string, expected int) ?[]types.Type {
	params := t.tc.fn_param_types[name] or { return none }
	if expected != 0 && params.len != expected {
		return none
	}
	return params
}

// fn_body_param_count supports fn body param count handling for Transformer.
fn (t &Transformer) fn_body_param_count(fn_node flat.Node) int {
	mut n := 0
	for i in 0 .. fn_node.children_count {
		child := t.a.child_node(&fn_node, i)
		if child.kind == .param {
			n++
		}
	}
	return n
}

// fn_body_return_type supports fn body return type handling for Transformer.
fn (t &Transformer) fn_body_return_type(fn_node flat.Node) string {
	if !isnil(t.tc) {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${fn_node.value}'
			if ret := t.fn_return_type_for_name(qname) {
				return ret
			}
			cqname := c_name(qname)
			if cqname != qname {
				if ret := t.fn_return_type_for_name(cqname) {
					return ret
				}
			}
		}
		if ret := t.fn_return_type_for_name(fn_node.value) {
			return ret
		}
		cname := c_name(fn_node.value)
		if cname != fn_node.value {
			if ret := t.fn_return_type_for_name(cname) {
				return ret
			}
		}
	}
	return t.normalize_type_alias(fn_node.typ)
}

// fn_return_type_for_name supports fn return type for name handling for Transformer.
fn (t &Transformer) fn_return_type_for_name(name string) ?string {
	ret := t.tc.fn_ret_types[name] or { return none }
	return t.normalize_type_alias(ret.name())
}

// --- statement list driver ---

// transform_stmts transforms transform stmts data for transform.
pub fn (mut t Transformer) transform_stmts(ids []flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{cap: ids.len}
	base_smartcasts := t.smartcast_stack.clone()
	defer {
		t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
	}
	mut i := 0
	for i < ids.len {
		id := ids[i]
		if int(id) >= 0 && i + 1 < ids.len {
			node := t.a.nodes[int(id)]
			if node_kind_id(node) == 44 && node.children_count == 0 && t.cur_fn_ret_type.len > 0
				&& t.cur_fn_ret_type != 'void' {
				next_id := ids[i + 1]
				next_node := t.a.nodes[int(next_id)]
				if node_kind_id(next_node) == 39 && next_node.children_count > 0 {
					expr_id := t.a.child(&next_node, 0)
					start := t.a.children.len
					t.a.children << expr_id
					merged_return := t.a.add_node(flat.Node{
						kind:           .return_stmt
						children_start: start
						children_count: 1
						typ:            node.typ
					})
					expanded := t.transform_stmt(merged_return)
					t.drain_pending(mut result)
					for eid in expanded {
						result << eid
					}
					i += 2
					continue
				}
			}
			if node.kind == .label_stmt {
				next_id := ids[i + 1]
				next_node := t.a.nodes[int(next_id)]
				if next_node.kind in [.for_stmt, .for_in_stmt] {
					expanded := t.transform_labeled_loop(node.value, next_id, next_node)
					t.drain_pending(mut result)
					for eid in expanded {
						result << eid
					}
					i += 2
					continue
				}
				if t.is_multi_init_for_block(next_node) {
					expanded := t.transform_labeled_multi_init_loop(node.value, next_id, next_node)
					t.drain_pending(mut result)
					for eid in expanded {
						result << eid
					}
					i += 2
					continue
				}
			}
		}
		expanded := t.transform_stmt(id)
		t.drain_pending(mut result)
		for eid in expanded {
			result << eid
		}
		for info in t.post_if_exit_smartcasts(id) {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		i++
	}
	t.drain_pending(mut result)
	return result
}

fn (t &Transformer) non_invalidated_smartcasts(contexts []SmartcastContext) []SmartcastContext {
	mut keep := []SmartcastContext{cap: contexts.len}
	for sc in contexts {
		if !t.smartcast_context_invalidated(sc.expr_name) {
			keep << sc
		}
	}
	return keep
}

fn (t &Transformer) smartcast_context_invalidated(expr_name string) bool {
	if expr_name.len == 0 || t.invalidated_smartcasts.len == 0 {
		return false
	}
	for key, _ in t.invalidated_smartcasts {
		if expr_name == key || expr_name.starts_with('${key}.') {
			return true
		}
	}
	return false
}

fn (t &Transformer) is_multi_init_for_block(node flat.Node) bool {
	if node.kind != .block || node.children_count != 2 {
		return false
	}
	if node.value != 'for_c_style_multi' {
		return false
	}
	init_id := t.a.child(&node, 0)
	loop_id := t.a.child(&node, 1)
	if int(init_id) < 0 || int(loop_id) < 0 {
		return false
	}
	init_node := t.a.nodes[int(init_id)]
	loop_node := t.a.nodes[int(loop_id)]
	if init_node.kind !in [.assign, .decl_assign] || init_node.children_count < 3 {
		return false
	}
	if loop_node.kind != .for_stmt || loop_node.children_count < 3 {
		return false
	}
	loop_init_id := t.a.child(&loop_node, 0)
	if int(loop_init_id) < 0 {
		return false
	}
	return t.a.nodes[int(loop_init_id)].kind == .empty
}

// transform_labeled_loop transforms transform labeled loop data for transform.
fn (mut t Transformer) transform_labeled_loop(label string, loop_id flat.NodeId, loop_node flat.Node) []flat.NodeId {
	if label.len == 0 {
		return t.transform_stmt(loop_id)
	}
	continue_label := '${label}_continue'
	break_label := '${label}_break'
	body_start := if loop_node.kind == .for_in_stmt { loop_node.value.int() } else { 3 }
	mut children := []flat.NodeId{cap: int(loop_node.children_count) + 1}
	for i in 0 .. loop_node.children_count {
		children << t.a.child(&loop_node, i)
	}
	if body_start <= children.len {
		children << t.a.add_val(.label_stmt, continue_label)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	new_loop := t.a.add_node(flat.Node{
		kind:           loop_node.kind
		op:             loop_node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            loop_node.pos
		value:          loop_node.value
		typ:            loop_node.typ
	})
	mut result := []flat.NodeId{}
	result << t.a.add_val(.label_stmt, label)
	transformed_loop := t.transform_stmt(new_loop)
	mut marked_loop := false
	for item_id in transformed_loop {
		if !marked_loop && t.a.nodes[int(item_id)].kind in [.for_stmt, .for_in_stmt] {
			result << t.a.add_val(.label_stmt, pending_loop_label_marker + label)
			marked_loop = true
		}
		result << item_id
	}
	result << t.a.add_val(.label_stmt, break_label)
	return result
}

fn (mut t Transformer) transform_labeled_multi_init_loop(label string, block_id flat.NodeId, block_node flat.Node) []flat.NodeId {
	if label.len == 0 {
		return t.transform_stmt(block_id)
	}
	init_id := t.a.child(&block_node, 0)
	loop_id := t.a.child(&block_node, 1)
	loop_node := t.a.nodes[int(loop_id)]
	mut block_children := []flat.NodeId{}
	init_expanded := t.transform_stmt(init_id)
	t.drain_pending(mut block_children)
	for eid in init_expanded {
		block_children << eid
	}
	labeled_loop := t.transform_labeled_loop(label, loop_id, loop_node)
	t.drain_pending(mut block_children)
	for i in 0 .. labeled_loop.len - 1 {
		block_children << labeled_loop[i]
	}
	mut result := []flat.NodeId{}
	result << t.make_block(block_children)
	result << labeled_loop.last()
	return result
}

// transform_stmt transforms transform stmt data for transform.
pub fn (mut t Transformer) transform_stmt(id flat.NodeId) []flat.NodeId {
	if int(id) < 0 {
		return arr1(id)
	}
	node := t.a.nodes[int(id)]
	kind_id := node_kind_id(node)
	if kind_id == 44 {
		return t.transform_return_stmt(id, node)
	}
	if kind_id == 40 || kind_id == 42 || kind_id == 43 {
		return t.transform_assign_stmt(id, node)
	}
	if kind_id == 41 {
		return t.transform_decl_assign_stmt(id, node)
	}
	if kind_id == 39 {
		return t.transform_expr_stmt(id, node)
	}
	if kind_id == 46 {
		return t.transform_for_stmt(id, node)
	}
	if kind_id == 47 {
		return t.transform_for_in_stmt(id, node)
	}
	if kind_id == 45 {
		return t.transform_block_stmt(id, node)
	}
	if kind_id == 15 {
		return t.transform_if_stmt(id, node)
	}
	if kind_id == 50 {
		return arr1(t.lower_one_match(node))
	}
	if kind_id == 52 {
		return t.transform_defer_stmt(id, node)
	}
	if kind_id == 53 {
		return t.transform_children_stmt(id, node)
	}
	if kind_id == 56 {
		return t.transform_select_stmt(node)
	}
	match node.kind {
		.return_stmt {
			return t.transform_return_stmt(id, node)
		}
		.assign, .selector_assign, .index_assign {
			return t.transform_assign_stmt(id, node)
		}
		.decl_assign {
			return t.transform_decl_assign_stmt(id, node)
		}
		.expr_stmt {
			return t.transform_expr_stmt(id, node)
		}
		.for_stmt {
			return t.transform_for_stmt(id, node)
		}
		.for_in_stmt {
			return t.transform_for_in_stmt(id, node)
		}
		.block {
			return t.transform_block_stmt(id, node)
		}
		.comptime_if {
			return t.transform_comptime_if_stmt(id, node)
		}
		.comptime_for {
			return t.expand_comptime_for(id, node)
		}
		.if_expr {
			return t.transform_if_stmt(id, node)
		}
		.match_stmt {
			return arr1(t.lower_one_match(node))
		}
		.defer_stmt {
			return t.transform_defer_stmt(id, node)
		}
		.assert_stmt {
			return t.transform_children_stmt(id, node)
		}
		.select_stmt {
			return t.transform_select_stmt(node)
		}
		else {
			return arr1(id)
		}
	}
}

// transform_expr transforms transform expr data for transform.
pub fn (mut t Transformer) transform_expr(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	kind_id := node_kind_id(node)
	if kind_id == 8 {
		return t.transform_infix_expr(id, node)
	}
	if kind_id == 12 {
		return t.transform_call_expr(id, node)
	}
	if kind_id == 15 {
		return t.transform_if_expr(id, node)
	}
	if kind_id == 16 {
		return t.transform_struct_init(id, node)
	}
	if kind_id == 17 {
		return t.transform_field_init_expr(id, node)
	}
	if kind_id == 14 {
		return t.transform_index_expr(id, node)
	}
	if kind_id == 6 {
		return t.transform_string_interp(id, node)
	}
	if kind_id == 13 {
		return t.transform_selector_expr(id, node)
	}
	if kind_id == 22 {
		return t.transform_or_expr(id, node)
	}
	if kind_id == 24 {
		return t.transform_as_expr(id, node)
	}
	if kind_id == 9 {
		return t.transform_prefix_expr(id, node)
	}
	if kind_id == 11 {
		return t.transform_paren_expr(id, node)
	}
	if kind_id == 10 {
		return t.transform_postfix_expr(id, node)
	}
	if kind_id == 23 {
		return t.transform_cast_expr(id, node)
	}
	if kind_id == 18 {
		return t.transform_array_literal(id, node)
	}
	if kind_id == 19 {
		return t.transform_array_init_expr(id, node)
	}
	if kind_id == 20 {
		return t.transform_map_init(id, node)
	}
	if kind_id == 38 {
		return t.transform_in_expr(id, node)
	}
	if kind_id == 37 {
		return t.transform_is_expr(id, node)
	}
	if kind_id == 50 {
		return t.lower_one_match(node)
	}
	if kind_id == 45 {
		return t.transform_block_expr(id, node)
	}
	if kind_id == 31 {
		return t.transform_lock_expr(id, node)
	}
	if kind_id == 34 {
		return t.transform_typeof_expr(id, node)
	}
	if kind_id == 7 {
		return t.transform_ident_expr(id, node)
	}
	if kind_id == 26 {
		return t.transform_assoc_expr(id, node)
	}
	if kind_id == 21 {
		return t.lift_fn_literal(id, node)
	}
	if kind_id == 56 {
		return t.transform_select_expr(node)
	}
	if kind_id == 30 || kind_id == 35 || kind_id == 27 || kind_id == 57 {
		return t.transform_children_expr(id, node)
	}
	if kind_id == 1 || kind_id == 2 || kind_id == 3 || kind_id == 4 || kind_id == 5 || kind_id == 28
		|| kind_id == 29 || kind_id == 25 || kind_id == 33 || kind_id == 36 {
		return id
	}
	match node.kind {
		.infix {
			return t.transform_infix_expr(id, node)
		}
		.call {
			return t.transform_call_expr(id, node)
		}
		.if_expr {
			return t.transform_if_expr(id, node)
		}
		.struct_init {
			return t.transform_struct_init(id, node)
		}
		.field_init {
			return t.transform_field_init_expr(id, node)
		}
		.index {
			return t.transform_index_expr(id, node)
		}
		.string_interp {
			return t.transform_string_interp(id, node)
		}
		.selector {
			return t.transform_selector_expr(id, node)
		}
		.or_expr {
			return t.transform_or_expr(id, node)
		}
		.as_expr {
			return t.transform_as_expr(id, node)
		}
		.prefix {
			return t.transform_prefix_expr(id, node)
		}
		.paren {
			return t.transform_paren_expr(id, node)
		}
		.postfix {
			return t.transform_postfix_expr(id, node)
		}
		.cast_expr {
			return t.transform_cast_expr(id, node)
		}
		.array_literal {
			return t.transform_array_literal(id, node)
		}
		.array_init {
			return t.transform_array_init_expr(id, node)
		}
		.map_init {
			return t.transform_map_init(id, node)
		}
		.sql_expr {
			return t.transform_sql_expr(id, node)
		}
		.in_expr {
			return t.transform_in_expr(id, node)
		}
		.is_expr {
			return t.transform_is_expr(id, node)
		}
		.match_stmt {
			return t.lower_one_match(node)
		}
		.block {
			return t.transform_block_expr(id, node)
		}
		.comptime_if {
			return t.transform_comptime_if_expr(id, node)
		}
		.lock_expr {
			return t.transform_lock_expr(id, node)
		}
		.typeof_expr {
			return t.transform_typeof_expr(id, node)
		}
		.ident {
			return t.transform_ident_expr(id, node)
		}
		.assoc {
			return t.transform_assoc_expr(id, node)
		}
		.fn_literal {
			return t.lift_fn_literal(id, node)
		}
		.spawn_expr {
			return t.transform_spawn_expr(id, node)
		}
		.select_stmt {
			return t.transform_select_expr(node)
		}
		.lambda_expr, .dump_expr, .range, .select_branch {
			return t.transform_children_expr(id, node)
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .nil_literal,
		.none_expr, .enum_val, .sizeof_expr, .offsetof_expr {
			// leaf/simple nodes - pass through unchanged
			return id
		}
		else {
			return id
		}
	}
}

fn (mut t Transformer) transform_spawn_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	old_in_spawn_expr := t.in_spawn_expr
	t.in_spawn_expr = true
	result := t.transform_children_expr(id, node)
	t.in_spawn_expr = old_in_spawn_expr
	return result
}

// transform_lvalue transforms transform lvalue data for transform.
pub fn (mut t Transformer) transform_lvalue(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return id
		}
		.selector {
			if node.children_count == 0 {
				return id
			}
			if t.selector_chain_has_sum_shared_field(id) {
				value := t.transform_selector_expr(id, node)
				mut value_type := t.node_type(id)
				if value_type.len == 0 {
					value_type = t.node_type(value)
				}
				return t.stable_transformed_expr_for_reuse(value, value_type, 'lvalue')
			}
			full_key := t.expr_key(id)
			if t.has_smartcast(full_key) {
				return t.transform_selector_expr(id, node)
			}
			base_id := t.a.child(&node, 0)
			base_key := t.expr_key(base_id)
			if t.has_smartcast(base_key) {
				return t.transform_selector_expr(id, node)
			}
			base := t.transform_lvalue(t.a.child(&node, 0))
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << base
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           .selector
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		.index {
			if node.children_count == 0 {
				return id
			}
			if lowered := t.lower_gated_scalar_index(node) {
				return lowered
			}
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << t.transform_expr(t.a.child(&node, 0))
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           .index
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				child := t.transform_expr(t.a.child(&node, 0))
				start := t.a.children.len
				t.a.children << child
				return t.a.add_node(flat.Node{
					kind:           .prefix
					op:             node.op
					children_start: start
					children_count: 1
					pos:            node.pos
					value:          node.value
					typ:            node.typ
				})
			}
			return t.transform_expr(id)
		}
		.paren {
			if node.children_count == 0 {
				return id
			}
			child := t.transform_lvalue(t.a.child(&node, 0))
			start := t.a.children.len
			t.a.children << child
			return t.a.add_node(flat.Node{
				kind:           .paren
				op:             node.op
				children_start: start
				children_count: 1
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		else {
			return t.transform_expr(id)
		}
	}
}

// --- stmt handlers (skeleton - identity transforms with child recursion) ---

// transform_return_stmt transforms transform return stmt data for transform.
fn (mut t Transformer) transform_return_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	source_return_id := t.return_drop_source_id(id, node)
	if expanded := t.try_expand_return_if(source_return_id, node) {
		return expanded
	}
	if expanded := t.try_expand_return_match(source_return_id, node) {
		return expanded
	}
	if direct := t.try_return_direct_optional_expr(node) {
		return direct
	}
	if expanded := t.try_expand_return_optional_expr(source_return_id, node) {
		return expanded
	}
	if expanded := t.try_expand_forwarded_multi_return(source_return_id, node) {
		return expanded
	}
	if node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		if t.is_optional_type_name(t.cur_fn_ret_type) && t.return_expr_is_err(child_id) {
			err_expr := t.transform_expr(child_id)
			return t.with_pending_before(t.make_none_return_stmt_with_err_expr(err_expr))
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_return_child(child_id, i, int(node.children_count))
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .return_stmt
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) return_values_with_extra(first_id flat.NodeId, extra_ids []flat.NodeId) []flat.NodeId {
	total := extra_ids.len + 1
	mut ids := []flat.NodeId{cap: total}
	ids << first_id
	for extra_id in extra_ids {
		ids << extra_id
	}
	return t.return_values_from_ids(ids)
}

fn (mut t Transformer) transform_return_child(child_id flat.NodeId, child_index int, total_children int) flat.NodeId {
	old_in_return_expr := t.in_return_expr
	t.in_return_expr = true
	defer {
		t.in_return_expr = old_in_return_expr
	}
	if converted := t.fixed_array_return_value(child_id) {
		return converted
	}
	if copied := t.heap_copy_local_address_return(child_id) {
		return copied
	}
	if int(child_id) >= 0 && int(child_id) < t.a.nodes.len {
		child := t.a.nodes[int(child_id)]
		if child.kind == .or_expr {
			return t.transform_expr(child_id)
		}
	}
	target_type := t.return_child_target_type(child_index, total_children)
	if target_type.len > 0 && t.is_optional_type_name(target_type) {
		child := t.a.nodes[int(child_id)]
		if child.kind == .or_expr {
			return t.transform_expr_for_type(child_id, target_type)
		}
		payload_type := t.optional_base_type(t.qualify_optional_type(target_type))
		resolved_payload_type := t.resolve_sum_name(payload_type)
		if resolved_payload_type in t.sum_types {
			return t.wrap_sum_value(child_id, resolved_payload_type)
		}
		return t.transform_expr_for_type(child_id, payload_type)
	}
	if target_type.len > 0 && target_type !in t.sum_types && !t.is_optional_type_name(target_type) {
		return t.transform_expr_for_type(child_id, target_type)
	}
	return t.wrap_sum_return_expr(child_id)
}

fn (t &Transformer) return_child_target_type(child_index int, total_children int) string {
	if total_children > 1 && !isnil(t.tc) && t.cur_fn_ret_type.len > 0 {
		if items := multi_return_types_from_type(t.tc.parse_type(t.cur_fn_ret_type), total_children) {
			if child_index >= 0 && child_index < items.len {
				return items[child_index].name()
			}
		}
	}
	return t.cur_fn_ret_type
}

// heap_copy_local_address_return supports heap copy local address return handling for Transformer.
fn (mut t Transformer) heap_copy_local_address_return(child_id flat.NodeId) ?flat.NodeId {
	if !t.cur_fn_ret_type.starts_with('&') || int(child_id) < 0 {
		return none
	}
	node := t.a.nodes[int(child_id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	inner_id := t.a.child(&node, 0)
	inner := t.a.nodes[int(inner_id)]
	if inner.kind != .ident || inner.value.len == 0 {
		return none
	}
	local_type := t.var_type(inner.value)
	if local_type.len == 0 {
		return none
	}
	ret_base_type := t.cur_fn_ret_type[1..]
	if ret_base_type.len == 0 {
		return none
	}
	clean_local_type := t.normalize_type_alias(local_type)
	clean_ret_type := t.normalize_type_alias(ret_base_type)
	if clean_local_type != clean_ret_type && local_type != ret_base_type {
		return none
	}
	addr := t.make_prefix(.amp, t.make_ident(inner.value))
	size := t.make_sizeof_type(ret_base_type)
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	return t.make_cast(t.cur_fn_ret_type, dup, t.cur_fn_ret_type)
}

// fixed_array_return_value supports fixed array return value handling for Transformer.
fn (mut t Transformer) fixed_array_return_value(child_id flat.NodeId) ?flat.NodeId {
	mut ret_type := t.cur_fn_ret_type
	if t.is_optional_type_name(ret_type) {
		ret_type = t.optional_base_type(ret_type)
	}
	// A function whose declared return type is itself a fixed array keeps
	// fixed-array (by-value) semantics; the C backend returns it via a wrapper
	// struct. Only a *dynamic* array return needs a fixed→dynamic conversion of a
	// fixed-array return value.
	if t.is_fixed_array_type(ret_type) {
		return none
	}
	return t.fixed_array_value_to_dynamic(child_id, ret_type)
}

// fixed_array_value_to_dynamic converts a fixed-array *value* (e.g. a fixed-array
// const or variable, not a literal — those have their own lowering) to a dynamic
// array when `target_type` is `[]T` with a matching element type. Returns none
// when no conversion is needed/possible.
fn (mut t Transformer) fixed_array_value_to_dynamic(value_id flat.NodeId, target_type string) ?flat.NodeId {
	array_type := target_type
	if !array_type.starts_with('[]') {
		return none
	}
	mut child_type := t.node_type(value_id)
	mut const_storage := false
	if !t.is_fixed_array_type(child_type) {
		if const_storage_type := t.const_array_literal_storage_type_name_for_expr(value_id) {
			child_type = const_storage_type
			const_storage = true
		}
	}
	if !t.is_fixed_array_type(child_type) || fixed_array_elem_type(child_type) != array_type[2..] {
		return none
	}
	if const_storage {
		mut data := t.make_prefix(.amp, t.transform_expr(value_id))
		t.set_node_typ(int(data), '&${child_type}')
		return t.fixed_array_data_to_array(data, child_type, array_type)
	}
	return t.fixed_array_value_to_array(value_id, child_type, array_type)
}

fn (t &Transformer) const_array_literal_storage_type_name_for_expr(id flat.NodeId) ?string {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	name := t.expr_key(id)
	if name.len == 0 {
		return none
	}
	key := t.const_type_key_in_context(name, t.cur_module, t.cur_file) or { return none }
	expr_id := t.tc.const_exprs[key] or { return none }
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
		return none
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind != .array_literal || expr.children_count == 0 {
		return none
	}
	raw_type := t.tc.const_types[key] or { return none }
	clean_type := t.normalize_type_alias(raw_type.name())
	if t.is_fixed_array_type(clean_type) {
		return clean_type
	}
	if !t.const_array_literal_requires_fixed_storage(key) {
		return none
	}
	if t.const_array_literal_storage_elem_excluded(raw_type, expr) {
		return none
	}
	if clean_type.starts_with('[]') && clean_type.len > 2 {
		elem_type := t.normalize_type_alias(clean_type[2..])
		return '${elem_type}[${expr.children_count}]'
	}
	return none
}

fn (t &Transformer) const_array_literal_storage_elem_excluded(raw_type types.Type, expr flat.Node) bool {
	mut elem_type := types.Type(types.void_)
	if raw_type is types.Array {
		elem_type = raw_type.elem_type
	} else if expr.children_count > 0 {
		elem_type = t.tc.resolve_type(t.a.child(&expr, 0))
	}
	return elem_type is types.Array || elem_type is types.Map || elem_type is types.Void
		|| elem_type is types.Unknown
}

fn (t &Transformer) const_array_literal_requires_fixed_storage(key string) bool {
	mut fixed_safe_refs := map[int]bool{}
	for node in t.a.nodes {
		if node.kind == .index && node.children_count > 0 {
			t.mark_const_ref_descendants(mut fixed_safe_refs, t.a.child(&node, 0))
		}
		if node.kind == .selector && node.value == 'len' && node.children_count > 0 {
			t.mark_const_ref_descendants(mut fixed_safe_refs, t.a.child(&node, 0))
		}
	}
	mut cur_module := 'main'
	mut cur_file := ''
	mut fixed_candidate := false
	mut dynamic_use := false
	for idx, node in t.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			cur_module = 'main'
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			continue
		}
		if node.kind == .call && node.children_count > 0 {
			fn_node := t.a.child_node(&node, 0)
			if fn_node.kind == .selector && fn_node.children_count > 0 {
				base_id := t.a.child(fn_node, 0)
				if base_key := t.const_ref_key_in_context(base_id, cur_module, cur_file) {
					if base_key == key {
						dynamic_use = true
					}
				}
			}
		}
		if !(fixed_safe_refs[idx] or { false }) {
			if ref_key := t.const_ref_key_in_context(flat.NodeId(idx), cur_module, cur_file) {
				if ref_key == key {
					dynamic_use = true
				}
			}
		}
		if node.kind == .index && node.children_count > 0 {
			base_id := t.a.child(&node, 0)
			if base_key := t.const_ref_key_in_context(base_id, cur_module, cur_file) {
				if base_key == key {
					fixed_candidate = true
				}
			}
		}
	}
	return fixed_candidate && !dynamic_use
}

fn (t &Transformer) mark_const_ref_descendants(mut ids map[int]bool, id flat.NodeId) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	ids[int(id)] = true
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		t.mark_const_ref_descendants(mut ids, t.a.child(&node, 0))
	}
}

fn (t &Transformer) const_ref_key_in_context(id flat.NodeId, module_name string, file string) ?string {
	name := t.expr_key(id)
	if name.len == 0 {
		return none
	}
	return t.const_type_key_in_context(name, module_name, file)
}

// transform_assign_stmt transforms transform assign stmt data for transform.
fn (mut t Transformer) transform_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	if expanded := t.try_expand_multi_return_assign(node) {
		return expanded
	}
	if expanded := t.try_expand_plain_multi_assign(node) {
		return expanded
	}
	if lowered := t.try_lower_sum_shared_field_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_optional_selector_lvalue_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_pointer_value_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_nested_map_index_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_map_index_selector_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_map_index_assign(node) {
		return lowered
	}
	// string `s += x` on a plain ident -> `s = string__plus(s, x)` (only when detectable as string)
	if expanded := t.try_lower_string_compound_assign(id, node) {
		return expanded
	}
	if expanded := t.try_lower_struct_compound_assign(node) {
		return expanded
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i % 2 == 0 {
			preserves_smartcast := node.op == .assign && i + 1 < node.children_count
				&& t.assignment_preserves_smartcast(child_id, t.a.child(&node, i + 1))
			new_children << if node.op == .assign && !preserves_smartcast {
				t.transform_lvalue_without_smartcast(child_id)
			} else {
				t.transform_lvalue(child_id)
			}
		} else {
			lhs_id := t.a.child(&node, i - 1)
			lhs := t.a.nodes[int(lhs_id)]
			mut lhs_type := if lhs.kind in [.selector, .index] {
				t.lvalue_type(lhs_id)
			} else {
				t.original_expr_type(lhs_id)
			}
			if lhs_type.len == 0 {
				lhs_type = t.lvalue_type(lhs_id)
			}
			// A value local moved to the heap (its type became `&T`) is assigned by storing a
			// value through the pointer (cgen emits `*v = ...`), so coerce the RHS to the value
			// type `T`, not `&T`. Otherwise a heaped-local RHS (`v = w`, both `&T`) is copied as a
			// pointer — aliasing `w`'s object — instead of dereferenced to its value.
			if lhs.kind == .ident && lhs.value in t.heaped_amp_locals && lhs_type.starts_with('&') {
				lhs_type = lhs_type[1..]
			}
			// A `mut val T` value param resolves to `&T`; cgen writes assignments
			// through the pointer (`*val = ...`), so coerce the RHS to `T`, not `&T`.
			if lhs.kind == .ident && lhs_type.starts_with('&') && t.mut_param_values[lhs.value]
				&& !lhs_type.starts_with('&&') {
				lhs_type = lhs_type[1..]
			}
			sum_target := t.assignment_sum_target(lhs_id, child_id, lhs_type)
			if node.op == .assign && sum_target.len > 0 {
				new_children << t.wrap_sum_value(child_id, sum_target)
			} else {
				new_children << t.transform_expr_for_type(child_id, lhs_type)
			}
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	if node.kind == .assign && node.op == .left_shift_assign {
		t.annotate_left_shift_assign(new_id)
	}
	if node.op == .assign {
		for i := 0; i < node.children_count; i += 2 {
			lhs_id := t.a.child(&node, i)
			if i + 1 < node.children_count
				&& t.assignment_preserves_smartcast(lhs_id, t.a.child(&node, i + 1)) {
				continue
			}
			t.invalidate_smartcast_for_lvalue(lhs_id)
		}
	}
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) transform_lvalue_without_smartcast(id flat.NodeId) flat.NodeId {
	key := t.expr_key(id)
	if key.len == 0 || t.smartcast_stack.len == 0 {
		return t.transform_lvalue(id)
	}
	base_smartcasts := t.smartcast_stack.clone()
	prefix := '${key}.'
	mut keep := []SmartcastContext{cap: base_smartcasts.len}
	for sc in base_smartcasts {
		if sc.expr_name == key || sc.expr_name.starts_with(prefix) {
			continue
		}
		keep << sc
	}
	t.smartcast_stack = keep
	transformed := t.transform_lvalue(id)
	t.smartcast_stack = base_smartcasts
	return transformed
}

fn (t &Transformer) assignment_preserves_smartcast(lhs_id flat.NodeId, rhs_id flat.NodeId) bool {
	key := t.expr_key(lhs_id)
	if key.len == 0 {
		return false
	}
	sc := t.find_smartcast(key) or { return false }
	if int(rhs_id) < 0 {
		return false
	}
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind != .cast_expr || rhs.children_count == 0 {
		return false
	}
	target_sum := t.resolve_sum_name(sc.sum_type_name)
	cast_sum := t.resolve_sum_name(t.qualify_type(rhs.value))
	if target_sum.len == 0 || cast_sum != target_sum {
		return false
	}
	payload_type := t.node_type(t.a.child(&rhs, 0))
	target_variant := t.smartcast_target_type(sc)
	return t.variant_names_match(payload_type, target_variant)
}

fn (mut t Transformer) invalidate_smartcast_for_lvalue(id flat.NodeId) {
	key := t.expr_key(id)
	if key.len == 0 || t.smartcast_stack.len == 0 {
		return
	}
	t.invalidated_smartcasts[key] = true
	prefix := '${key}.'
	mut keep := []SmartcastContext{cap: t.smartcast_stack.len}
	for sc in t.smartcast_stack {
		if sc.expr_name == key || sc.expr_name.starts_with(prefix) {
			continue
		}
		keep << sc
	}
	t.smartcast_stack = keep
}

fn (mut t Transformer) try_lower_optional_selector_lvalue_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .selector_assign || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return none
	}
	lowered_lhs, guard_source, guard_body, guard_mode := t.lower_optional_selector_lvalue(lhs_id) or {
		return none
	}
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	not_ok := t.make_prefix(.not, t.make_selector(guard_source, 'ok', 'bool'))
	guard_stmts := t.optional_selector_lvalue_guard_stmts(guard_body, guard_mode, guard_source)
	result << t.make_if(not_ok, t.make_block(guard_stmts), t.make_empty())
	lhs_type := t.lvalue_type(lhs_id)
	sum_target := t.assignment_sum_target(lhs_id, rhs_id, lhs_type)
	rhs := if node.op == .assign && sum_target.len > 0 {
		t.wrap_sum_value(rhs_id, sum_target)
	} else {
		t.transform_expr_for_type(rhs_id, lhs_type)
	}
	t.drain_pending(mut result)
	result << t.make_assign_op(lowered_lhs, rhs, node.op)
	return result
}

fn (mut t Transformer) optional_selector_lvalue_guard_stmts(body_id flat.NodeId, mode string, guard_source flat.NodeId) []flat.NodeId {
	err_expr := t.make_selector(guard_source, 'err', 'IError')
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return arr1(t.make_return(t.make_optional_none_with_err(t.cur_fn_ret_type, err_expr),
				t.cur_fn_ret_type))
		}
		return arr1(t.make_panic_stmt('option/result propagation failed'))
	}
	return t.lower_or_body_to_stmts_with_err_expr(body_id, '', '', mode, err_expr)
}

fn (mut t Transformer) lower_optional_selector_lvalue(id flat.NodeId) ?(flat.NodeId, flat.NodeId, flat.NodeId, string) {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 || node.value.len == 0 {
		return none
	}
	base_id := t.a.child(&node, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind == .or_expr && base.children_count >= 2 {
		return t.lower_optional_selector_lvalue_from_or(id, node, base)
	}
	if base.kind == .paren && base.children_count > 0 {
		inner_id := t.a.child(&base, 0)
		inner := t.a.nodes[int(inner_id)]
		if inner.kind == .or_expr && inner.children_count >= 2 {
			return t.lower_optional_selector_lvalue_from_or(id, node, inner)
		}
	}
	lowered_base, guard_source, guard_body, guard_mode := t.lower_optional_selector_lvalue(base_id) or {
		return none
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << lowered_base
	for i in 1 .. node.children_count {
		new_children << t.transform_expr(t.a.child(&node, i))
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	lowered := t.a.add_node(flat.Node{
		kind:           .selector
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return lowered, guard_source, guard_body, guard_mode
}

fn (mut t Transformer) lower_optional_selector_lvalue_from_or(id flat.NodeId, node flat.Node, base flat.Node) ?(flat.NodeId, flat.NodeId, flat.NodeId, string) {
	source_id := t.a.child(&base, 0)
	if !t.optional_selector_lvalue_source(source_id) {
		return none
	}
	expr_type, value_type := t.or_expr_types(source_id, base.typ)
	if !t.is_optional_type_name(expr_type) || value_type.len == 0 || value_type == 'void' {
		return none
	}
	source := t.transform_lvalue(source_id)
	value_base := t.make_selector(source, 'value', value_type)
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << value_base
	for i in 1 .. node.children_count {
		new_children << t.transform_expr(t.a.child(&node, i))
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	lhs_type := t.lvalue_type(id)
	lowered := t.a.add_node(flat.Node{
		kind:           .selector
		op:             if value_type.starts_with('&') { flat.Op.arrow } else { node.op }
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            if lhs_type.len > 0 { lhs_type } else { node.typ }
	})
	return lowered, source, t.a.child(&base, 1), base.value
}

fn (t &Transformer) optional_selector_lvalue_source(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value.len > 0
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return t.optional_selector_lvalue_source(t.a.child(&node, 0))
		}
		.selector {
			if node.children_count == 0 || node.value.len == 0 {
				return false
			}
			return t.optional_selector_lvalue_source(t.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (mut t Transformer) try_lower_struct_compound_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count != 2 {
		return none
	}
	op_name := compound_assign_struct_operator_symbol(node.op) or { return none }
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	mut lhs_type := t.var_type(lhs.value)
	if lhs_type.len == 0 {
		lhs_type = t.original_expr_type(lhs_id)
	}
	if lhs_type.starts_with('&') {
		return none
	}
	mut operator_type := t.struct_lookup_name(lhs_type)
	if operator_type.len == 0 {
		if _ := t.struct_operator_fn_name(lhs_type, op_name) {
			operator_type = lhs_type
		}
	}
	if operator_type.len == 0 {
		return none
	}
	method_name := t.struct_operator_fn_name(operator_type, op_name) or { return none }
	rhs := t.transform_expr_for_type(rhs_id, lhs_type)
	t.mark_fn_used_name(method_name)
	call := t.make_call_typed(method_name, arr2(t.make_ident(lhs.value), rhs), lhs_type)
	return arr1(t.make_assign(t.make_ident(lhs.value), call))
}

fn compound_assign_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus_assign { return '+' }
		.minus_assign { return '-' }
		.mul_assign { return '*' }
		.div_assign { return '/' }
		.mod_assign { return '%' }
		else {}
	}

	return none
}

// try_lower_sum_shared_field_assign
// supports helper handling in transform.
fn (mut t Transformer) try_lower_sum_shared_field_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind !in [.assign, .selector_assign] || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return none
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .selector || lhs.children_count == 0 || lhs.value.len == 0 {
		return none
	}
	base_id := t.a.child(&lhs, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	field_type := t.sum_shared_field_type_name(base_type, lhs.value) or { return none }
	mut base := t.transform_lvalue(base_id)
	mut sum_type := base_type
	if !t.is_stable_expr_for_reuse(base) {
		clean_sum := t.trim_pointer_type(sum_type)
		ptr_type := if sum_type.starts_with('&') { sum_type } else { '&${clean_sum}' }
		addr := if sum_type.starts_with('&') {
			base
		} else {
			mut addr_expr := t.make_prefix(.amp, base)
			t.set_node_typ(int(addr_expr), ptr_type)
			addr_expr
		}
		tmp_name := t.new_temp('sum_lhs')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, addr, ptr_type)
		base = t.make_ident(tmp_name)
		sum_type = ptr_type
	}
	mut rhs := if node.op == .assign {
		t.transform_expr_for_type(rhs_id, field_type)
	} else {
		t.transform_expr(rhs_id)
	}
	mut rhs_type := t.node_type(rhs)
	if rhs_type.len == 0 {
		rhs_type = t.node_type(rhs_id)
	}
	if rhs_type.len == 0 {
		rhs_type = field_type
	}
	rhs = t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'sum_assign')
	resolved_sum := t.resolve_sum_name(t.trim_pointer_type(sum_type))
	variants := t.sum_types[resolved_sum] or { return none }
	stmt := t.build_sum_shared_field_assign_chain(base, sum_type, resolved_sum, variants,
		lhs.value, field_type, rhs, node.op, 0)
	return t.with_pending_before(stmt)
}

// build_sum_shared_field_assign_chain supports build_sum_shared_field_assign_chain handling.
fn (mut t Transformer) build_sum_shared_field_assign_chain(base flat.NodeId, sum_type string, resolved_sum string, variants []string, field string, field_type string, rhs flat.NodeId, op flat.Op, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_empty()
	}
	variant := variants[idx]
	tag := t.make_sum_tag_selector(base, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(resolved_sum, variant)))
	qv := t.resolve_variant(resolved_sum, variant)
	sum_field := t.sum_field_name(qv)
	use_ptr := t.variant_references_sum(qv, resolved_sum)
	variant_base := t.make_selector_op(base, sum_field, if use_ptr { '&${qv}' } else { qv }, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	mut then_stmt := t.make_empty()
	if nested_field_type := t.sum_shared_field_type_name(qv, field) {
		nested_sum := t.resolve_sum_name(qv)
		if nested_variants := t.sum_types[nested_sum] {
			then_stmt = t.build_sum_shared_field_assign_chain(variant_base, qv, nested_sum,
				nested_variants, field, nested_field_type, rhs, op, 0)
		}
	} else {
		field_lhs := t.make_selector_op(variant_base, field, field_type, if use_ptr {
			.arrow
		} else {
			.dot
		})
		then_stmt = t.make_assign_op(field_lhs, rhs, op)
	}
	then_block := t.make_block(arr1(then_stmt))
	else_stmt := t.build_sum_shared_field_assign_chain(base, sum_type, resolved_sum, variants,
		field, field_type, rhs, op, idx + 1)
	return t.make_if(cond, then_block, else_stmt)
}

// assignment_sum_target supports assignment sum target handling for Transformer.
fn (t &Transformer) assignment_sum_target(lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type string) string {
	if lhs_type.starts_with('&') {
		return ''
	}
	if lhs_type.starts_with('[]') || t.is_fixed_array_type(lhs_type) {
		return ''
	}
	if t.is_sum_type_name(lhs_type) {
		return lhs_type
	}
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return ''
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .selector || lhs.value.len == 0 {
		return ''
	}
	if lhs.value == 'obj' {
		sum_name := t.resolve_sum_name('ScopeObject')
		if t.is_sum_type_name(sum_name) {
			return sum_name
		}
	}
	rhs := t.a.nodes[int(rhs_id)]
	if inferred_sum := t.sum_type_for_field_variant(lhs.value, rhs_id, rhs) {
		return inferred_sum
	}
	if lhs.value == 'info' {
		if type_info_sum := t.type_info_sum_name() {
			return type_info_sum
		}
	}
	return ''
}

// type_info_sum_name returns type info sum name data for Transformer.
fn (t &Transformer) type_info_sum_name() ?string {
	for sum_name, _ in t.sum_types {
		if sum_name == 'TypeInfo' || sum_name.ends_with('.TypeInfo') {
			return sum_name
		}
	}
	return none
}

// try_lower_pointer_value_assign supports try lower pointer value assign handling for Transformer.
fn (mut t Transformer) try_lower_pointer_value_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	mut lhs_type := t.var_type(lhs.value)
	if lhs_type.len == 0 {
		lhs_type = t.node_type(lhs_id)
	}
	if !lhs_type.starts_with('&') {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.node_type(rhs_id)
	lhs_value_type_raw := lhs_type[1..]
	lhs_value_type := t.normalize_type_alias(lhs_value_type_raw)
	if node.op != .assign {
		if !t.pointer_value_lvalues[lhs.value] {
			return none
		}
		new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
		return arr1(t.make_assign_op(new_lhs, t.transform_expr(rhs_id), node.op))
	}
	if !t.pointer_value_assign_rhs_matches(lhs_value_type_raw, lhs_value_type, rhs_type) {
		return none
	}
	new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
	return arr1(t.make_assign(new_lhs, t.transform_expr_for_type(rhs_id, lhs_value_type_raw)))
}

fn (t &Transformer) pointer_value_assign_rhs_matches(lhs_value_type_raw string, lhs_value_type string, rhs_type string) bool {
	if rhs_type.len == 0 {
		return false
	}
	if rhs_type == lhs_value_type || t.type_alias_targets_type(lhs_value_type_raw, rhs_type) {
		return true
	}
	if t.is_optional_type_name(lhs_value_type) {
		base := t.optional_base_type(t.qualify_optional_type(lhs_value_type))
		return rhs_type == base || t.type_alias_targets_type(base, rhs_type)
	}
	return false
}

// transform_expr_for_type transforms transform expr for type data for transform.
fn (mut t Transformer) transform_expr_for_type(id flat.NodeId, target_type string) flat.NodeId {
	old_expected_node := t.expected_expr_node
	old_expected_type := t.expected_expr_type
	if int(id) >= 0 && target_type.len > 0 {
		t.expected_expr_node = int(id)
		t.expected_expr_type = target_type
	}
	defer {
		t.expected_expr_node = old_expected_node
		t.expected_expr_type = old_expected_type
	}
	if int(id) >= 0 && target_type.len > 0 {
		node := t.a.nodes[int(id)]
		if node.kind == .none_expr && t.is_optional_type_name(target_type) {
			return t.make_optional_none(t.qualify_optional_type(target_type))
		}
		if node.kind == .none_expr && t.is_ierror_type(target_type) {
			return t.make_ierror_none()
		}
		if t.is_optional_type_name(target_type) && node.kind in [.array_init, .array_literal] {
			optional_target := t.qualify_optional_type(target_type)
			payload_type := t.optional_base_type(optional_target)
			if payload_type.starts_with('[]') {
				value := t.transform_expr_for_type(id, payload_type)
				return t.make_optional_some(value, optional_target)
			}
		}
		if target_type.starts_with('&') && node.kind == .ident
			&& t.pointer_global_arg_matches_param(node.value, target_type) {
			return t.transform_expr(id)
		}
		if target_type.starts_with('&') {
			if expr := t.transform_amp_struct_init_for_type(id, node, target_type) {
				return expr
			}
		}
		if node.kind == .field_init {
			if expr := t.transform_field_init_for_struct_type(id, target_type) {
				return expr
			}
		}
		if expr := t.transform_interface_value_for_type(id, target_type, false) {
			return expr
		}
		if node.kind == .block {
			if lowered := t.transform_block_expr_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .if_expr {
			if lowered := t.try_expand_if_expr_value_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .match_stmt {
			if lowered := t.transform_match_expr_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .or_expr && target_type.len > 0 {
			old_typ := node.typ
			t.set_node_typ(int(id), target_type)
			expr := t.transform_expr(id)
			t.set_node_typ(int(id), old_typ)
			return t.coerce_transformed_expr_to_type(expr, id, target_type)
		}
		if node.kind == .array_literal {
			if lowered := t.transform_fixed_array_literal_for_type(id, node, target_type) {
				return lowered
			}
			if lowered := t.transform_array_literal_for_type(id, node, target_type) {
				return lowered
			}
		}
		if node.kind == .postfix && node.op == .not && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .array_literal {
				if lowered := t.transform_fixed_array_literal_for_type(child_id, child, target_type) {
					return lowered
				}
			}
		}
		if node.kind == .array_init {
			if lowered := t.transform_empty_array_init_for_type(node, target_type) {
				return lowered
			}
		}
		if node.kind == .map_init {
			clean_target := t.clean_map_type(target_type)
			if clean_target.starts_with('map[') {
				mut map_node := node
				map_node.value = clean_target
				map_node.typ = clean_target
				return t.lower_map_init_to_runtime(id, map_node)
			}
		}
		if target_type in ['f32', 'f64'] && node.kind == .infix
			&& node.op in [.plus, .minus, .mul, .div] && node.children_count >= 2 {
			lhs_id := t.a.child(&node, 0)
			mut lhs_type := t.node_type(lhs_id)
			if lhs_type.len == 0 {
				lhs_type = t.resolve_expr_type(lhs_id)
			}
			if t.infix_struct_operator_result_type(node, lhs_type).len == 0 {
				lhs := t.transform_expr_for_type(lhs_id, target_type)
				rhs := t.transform_expr_for_type(t.a.child(&node, 1), target_type)
				start := t.a.children.len
				t.a.children << lhs
				t.a.children << rhs
				return t.a.add_node(flat.Node{
					kind:           .infix
					op:             node.op
					children_start: start
					children_count: 2
					pos:            node.pos
					value:          node.value
					typ:            target_type
				})
			}
		}
	}
	expr := t.transform_expr(id)
	return t.coerce_transformed_expr_to_type(expr, id, target_type)
}

fn (mut t Transformer) transform_field_init_for_struct_type(id flat.NodeId, target_type string) ?flat.NodeId {
	clean_target := t.normalize_type_alias(target_type)
	if clean_target.len == 0 || clean_target.starts_with('&') || clean_target.starts_with('[]')
		|| clean_target.starts_with('map[') || t.is_optional_type_name(clean_target) {
		return none
	}
	if _ := t.lookup_struct_info(clean_target) {
		start := t.a.children.len
		t.a.children << id
		struct_id := t.a.add_node(flat.Node{
			kind:           .struct_init
			children_start: start
			children_count: 1
			value:          clean_target
			typ:            clean_target
		})
		return t.transform_struct_init(struct_id, t.a.nodes[int(struct_id)])
	}
	return none
}

// transform_block_expr_for_type transforms transform block expr for type data for transform.
fn (mut t Transformer) transform_block_expr_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .block || node.children_count == 0 || target_type.len == 0 {
		return none
	}
	last_id := t.a.child(&node, node.children_count - 1)
	last := t.a.nodes[int(last_id)]
	tail_expr_id := if last.kind == .expr_stmt && last.children_count > 0 {
		t.a.child(&last, 0)
	} else if last.kind == .block && t.stmt_value_type(last_id).len > 0 {
		last_id
	} else if !t.is_stmt_kind(last.kind) {
		last_id
	} else {
		return none
	}
	mut prefix := []flat.NodeId{cap: int(node.children_count - 1)}
	for i in 0 .. node.children_count - 1 {
		prefix << t.a.child(&node, i)
	}
	mut new_children := t.transform_stmts(prefix)
	tail_expr := t.transform_expr_for_type(tail_expr_id, target_type)
	tail_stmt := t.make_expr_stmt(tail_expr)
	for stmt in t.with_pending_before(tail_stmt) {
		new_children << stmt
	}
	new_block := t.make_block(new_children)
	block_typ := t.stmt_value_type(new_block)
	t.set_node_typ(int(new_block), if block_typ.len > 0 { block_typ } else { node.typ })
	return new_block
}

// transform_match_expr_for_type transforms transform match expr for type data for transform.
fn (mut t Transformer) transform_match_expr_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if target_type.len == 0 || node.kind != .match_stmt {
		return none
	}
	mut actual_result_type := t.match_expr_type(node)
	if actual_result_type.len == 0 || actual_result_type == 'void' {
		actual_result_type = target_type
	}
	if t.sum_target_accepts_variant_type(target_type, actual_result_type) {
		actual_result_type = target_type
	}
	tmp_name := t.new_temp('match_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	prelude << t.make_decl_assign_typed(tmp_name, t.zero_value_for_type(actual_result_type),
		actual_result_type)
	for stmt in t.build_match_value_stmts(node, tmp_name, actual_result_type) {
		prelude << stmt
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	tmp := t.make_ident(tmp_name)
	t.set_node_typ(int(tmp), actual_result_type)
	return tmp
}

// transform_amp_struct_init_for_type supports transform_amp_struct_init_for_type handling.
fn (mut t Transformer) transform_amp_struct_init_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind != .struct_init {
		return none
	}
	new_child := t.transform_struct_init(child_id, child)
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            if target_type.len > 0 { target_type } else { node.typ }
	})
}

// coerce_transformed_expr_to_type converts coerce transformed expr to type data for transform.
fn (mut t Transformer) coerce_transformed_expr_to_type(expr flat.NodeId, source_id flat.NodeId, target_type string) flat.NodeId {
	mut target := t.normalize_type_alias(target_type)
	if target.len == 0 || int(expr) < 0 {
		return expr
	}
	mut expr_type := t.node_type(expr)
	if expr_type.len == 0 {
		expr_type = t.node_type(source_id)
	}
	if expr_type.len == 0 {
		expr_type = t.resolve_expr_type(source_id)
	}
	expr_type = t.normalize_type_alias(expr_type)
	mut optional_target := if t.is_optional_type_name(target_type) {
		t.qualify_optional_type(target_type)
	} else {
		target
	}
	optional_target = t.infer_typed_optional_target(optional_target, expr_type)
	if optional_target.starts_with('!') && t.is_ierror_type(expr_type) {
		return t.make_optional_none_with_err(optional_target, expr)
	}
	if t.is_optional_type_name(optional_target) && !t.is_optional_type_name(expr_type) {
		source := if int(source_id) >= 0 { t.a.nodes[int(source_id)] } else { flat.Node{} }
		if source.kind != .none_expr {
			return t.make_optional_some(expr, optional_target)
		}
	}
	if expr_type.len == 0 || expr_type == target {
		return expr
	}
	if target == 'map*' {
		clean_expr_type := t.clean_map_type(expr_type)
		if clean_expr_type.starts_with('map[') {
			if expr_type.starts_with('&') {
				return expr
			}
			if t.expr_can_take_address(expr) {
				addr := t.make_prefix(.amp, expr)
				t.set_node_typ(int(addr), target)
				return addr
			}
			tmp_name := t.new_temp('addr')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, clean_expr_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.set_node_typ(int(addr), target)
			return addr
		}
	}
	if target in ['f32', 'f64'] && t.is_integer_type_name(expr_type) {
		return t.make_cast(target, expr, target)
	}
	if target.starts_with('&') {
		if t.expr_is_nil_like(source_id) {
			t.set_node_typ(int(expr), target)
			return expr
		}
		target_value_type := t.normalize_type_alias(target[1..])
		expr_value_type := if expr_type.starts_with('&') {
			t.normalize_type_alias(expr_type[1..])
		} else {
			expr_type
		}
		if t.is_sum_type_name(target_value_type)
			&& t.find_sum_type_for_variant(t.trim_pointer_type(expr_type)).len > 0 {
			if t.resolve_sum_name(t.trim_pointer_type(expr_type)) == t.resolve_sum_name(target_value_type) {
				if expr_type.starts_with('&') {
					return expr
				}
				if t.expr_can_take_address(expr) {
					addr := t.make_prefix(.amp, expr)
					t.set_node_typ(int(addr), target)
					return addr
				}
				tmp_name := t.new_temp('sum_ref')
				t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, target_value_type)
				addr := t.make_prefix(.amp, t.make_ident(tmp_name))
				t.set_node_typ(int(addr), target)
				return addr
			}
			source := t.a.nodes[int(source_id)]
			wrap_source_id := if source.kind == .prefix && source.op == .amp
				&& source.children_count > 0 {
				t.a.child(&source, 0)
			} else {
				source_id
			}
			wrapped := t.wrap_sum_value(wrap_source_id, target_value_type)
			tmp_name := t.new_temp('sum_ref')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, target_value_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.set_node_typ(int(addr), target)
			return addr
		}
		if expr_value_type == target_value_type
			|| t.type_alias_targets_type(target[1..], expr_value_type) {
			if expr_type.starts_with('&') {
				return expr
			}
			if t.expr_can_take_address(expr) {
				addr := t.make_prefix(.amp, expr)
				t.set_node_typ(int(addr), target)
				return addr
			}
			tmp_name := t.new_temp('addr')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, expr_value_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.set_node_typ(int(addr), target)
			return addr
		}
		return expr
	}
	if expr_type.starts_with('&') {
		expr_value_type := t.normalize_type_alias(expr_type[1..])
		if expr_value_type == target || t.type_alias_targets_type(expr_type[1..], target) {
			deref := t.make_prefix(.mul, expr)
			t.set_node_typ(int(deref), target)
			return deref
		}
	}
	return expr
}

// is_ierror_type reports whether is ierror type applies in transform.
fn (t &Transformer) is_ierror_type(name string) bool {
	clean := t.trim_pointer_type(t.normalize_type_alias(name))
	return clean == 'IError' || clean == 'builtin.IError'
}

// expr_is_nil_like supports expr is nil like handling for Transformer.
fn (t &Transformer) expr_is_nil_like(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .nil_literal {
		return true
	}
	if node.kind != .block || node.children_count == 0 {
		return false
	}
	last_id := t.a.child(&node, node.children_count - 1)
	last := t.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count > 0 {
		return t.expr_is_nil_like(t.a.child(&last, 0))
	}
	return t.expr_is_nil_like(last_id)
}

// infer_typed_optional_target resolves infer typed optional target information for transform.
fn (t &Transformer) infer_typed_optional_target(optional_target string, expr_type string) string {
	if expr_type.len == 0 {
		return optional_target
	}
	mut value_type := expr_type
	if !value_type.contains('.') {
		qualified := t.qualify_type(value_type)
		if qualified != value_type {
			value_type = qualified
		}
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(value_type)
		parsed_name := parsed.name()
		if parsed_name.len > 0 && parsed_name != 'unknown' {
			value_type = parsed_name
		}
	}
	if t.is_optional_type_name(optional_target) {
		base := t.optional_base_type(optional_target)
		if value_type.contains('.') && base == value_type.all_after_last('.') {
			return '?${value_type}'
		}
		if value_type.contains('.') && base.contains('.')
			&& base.all_after_last('.') == value_type.all_after_last('.')
			&& !t.is_known_type_name(base) && t.is_known_type_name(value_type) {
			return '?${value_type}'
		}
		return optional_target
	}
	if optional_target != 'Optional' || isnil(t.tc) {
		return optional_target
	}
	typ := t.tc.parse_type(value_type)
	if typ is types.Primitive || typ is types.Enum || typ is types.Void {
		return optional_target
	}
	return '?${value_type}'
}

// make_optional_some builds make optional some data for transform.
fn (mut t Transformer) make_optional_some(value flat.NodeId, optional_type string) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(true), 'bool')
	base_type := t.optional_base_type(optional_type)
	mut fields := []flat.NodeId{cap: 2}
	fields << ok_field
	if base_type.len > 0 && base_type != 'void' {
		fields << t.make_sum_literal_field('value', value, base_type)
	}
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(fields.len)
		value:          optional_type
		typ:            optional_type
	})
}

// make_optional_none builds make optional none data for transform.
fn (mut t Transformer) make_optional_none(optional_type string) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(false), 'bool')
	start := t.a.children.len
	t.a.children << ok_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 1
		value:          optional_type
		typ:            optional_type
	})
}

fn (mut t Transformer) make_ierror_none() flat.NodeId {
	none_value := t.make_struct_init('None__')
	addr := t.make_prefix(.amp, none_value)
	size := t.make_sizeof_type('None__')
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	object := t.make_cast('&None__', dup, '&None__')
	type_id := t.interface_impl_type_id('IError', 'None__') or { 0 }
	fields := [
		t.make_sum_literal_field('_typ', t.make_int_literal(type_id), 'int'),
		t.make_sum_literal_field('_object', object, '&None__'),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(fields.len)
		value:          'IError'
		typ:            'IError'
	})
}

// make_optional_none_with_err builds make optional none with err data for transform.
fn (mut t Transformer) make_optional_none_with_err(optional_type string, err_expr flat.NodeId) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(false), 'bool')
	err_field := t.make_sum_literal_field('err', err_expr, 'IError')
	start := t.a.children.len
	t.a.children << ok_field
	t.a.children << err_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          optional_type
		typ:            optional_type
	})
}

// expr_can_take_address supports expr can take address handling for Transformer.
fn (t &Transformer) expr_can_take_address(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return true
		}
		.index {
			// `a[lo..hi]` (an index node tagged `range`) yields a fresh array value, not an
			// addressable element, so its address can't be taken in place — runtime_addr
			// must materialize it to a temp first. Plain element indexing is addressable only
			// when the indexed storage is addressable too.
			if node.value == 'range' {
				return false
			}
			if node.children_count == 0 {
				return false
			}
			return t.expr_can_take_address(t.a.child(&node, 0))
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			if t.selector_chain_has_sum_variant_field(id) {
				return false
			}
			return t.expr_can_take_address(t.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return t.expr_can_take_address(t.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

// type_alias_targets_type returns type alias targets type data for Transformer.
fn (t &Transformer) type_alias_targets_type(alias_name string, target_type string) bool {
	if alias_name.len == 0 || target_type.len == 0 || isnil(t.tc) {
		return false
	}
	for name, target in t.tc.type_aliases {
		if name == alias_name || name.all_after_last('.') == alias_name {
			if t.normalize_type_alias(target) == target_type {
				return true
			}
		}
	}
	return false
}

// try_lower_string_compound_assign
// supports helper handling in transform.
fn (mut t Transformer) try_lower_string_compound_assign(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.op != .plus_assign || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	is_string := t.resolve_expr_type(lhs_id) == 'string' || rhs.kind == .string_literal
		|| rhs.kind == .string_interp || t.resolve_expr_type(rhs_id) == 'string'
	if !is_string {
		return none
	}
	new_rhs := t.transform_expr(rhs_id)
	lhs_copy := t.make_ident(lhs.value)
	concat := t.make_call('string__plus', arr2(lhs_copy, new_rhs))
	new_lhs := t.make_ident(lhs.value)
	return arr1(t.make_assign(new_lhs, concat))
}

// transform_decl_assign_stmt transforms transform decl assign stmt data for transform.
fn (mut t Transformer) transform_decl_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	mut has_empty_child := false
	for i in 0 .. node.children_count {
		if int(t.a.child(&node, i)) < 0 {
			has_empty_child = true
		}
	}
	if has_empty_child {
		mut parts := []string{}
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			if int(child_id) < 0 {
				parts << '${i}:empty'
			} else {
				child := t.a.nodes[int(child_id)]
				parts << '${i}:${child.kind}:${child.value}:${child.typ}'
			}
		}
		panic('internal error: empty decl_assign child in ${t.cur_fn_name}: count=${node.children_count} typ=${node.typ} value=${node.value} children=${parts.join('|')}')
	}
	mut inferred_typ := ''
	if node.children_count > 2 && !isnil(t.tc) {
		rhs_id := t.a.child(&node, 1)
		if rhs_types := t.multi_return_types_for_expr(rhs_id, node.children_count - 1) {
			for j, field_type in rhs_types {
				lhs_idx := if j == 0 { 0 } else { j + 1 }
				if lhs_idx >= node.children_count {
					continue
				}
				lhs := t.a.child_node(&node, lhs_idx)
				if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
					t.set_var_type(lhs.value, t.normalize_type_alias(field_type.name()))
				}
			}
		}
	}
	if expanded := t.try_expand_multi_return_decl(node) {
		return expanded
	}
	if expanded := t.try_expand_plain_multi_decl(node) {
		return expanded
	}
	// Track the variable type for the common 2-child case.
	if node.children_count == 2 {
		lhs := t.a.child_node(&node, 0)
		if lhs.kind == .ident && lhs.value.len > 0 {
			mut typ := t.infer_decl_type(node)
			rhs_id := t.a.child(&node, 1)
			rhs := t.a.nodes[int(rhs_id)]
			if rhs.kind == .call {
				if call_typ := t.checker_resolved_non_builtin_return_type(rhs_id, rhs) {
					if decl_type_is_usable(call_typ) || !decl_type_is_usable(typ) {
						typ = call_typ
					}
				} else if !decl_type_is_usable(typ) {
					call_typ := t.node_type(rhs_id)
					if decl_type_is_usable(call_typ) {
						typ = call_typ
					} else {
						raw_call_typ := t.get_call_return_type(rhs_id, rhs)
						if raw_call_typ.len > 0 {
							typ = raw_call_typ
						}
					}
				}
				generic_typ := t.concrete_generic_call_return_type(rhs_id, rhs)
				if generic_typ.len > 0 {
					typ = generic_typ
				}
			}
			if rhs.kind == .call && t.is_strings_builder_new_call(rhs_id, rhs) {
				typ = 'strings.Builder'
			} else if rhs.kind == .if_expr {
				if_typ := t.if_expr_result_type(rhs_id, rhs)
				if if_typ.len > 0 {
					typ = if_typ
				}
			} else if rhs.kind == .match_stmt {
				match_typ := t.match_expr_type(rhs)
				if match_typ.len > 0 {
					typ = match_typ
				}
			} else if rhs.kind == .block {
				block_typ := t.stmt_value_type(rhs_id)
				if block_typ.len > 0 {
					typ = block_typ
				}
			} else if rhs.kind == .or_expr && rhs.children_count > 0 {
				or_source_id := t.a.child(&rhs, 0)
				if info := t.map_index_info(or_source_id) {
					typ = info.value_type
					if t.map_value_type_is_optional(info.value_type) {
						or_body_id := if rhs.children_count > 1 {
							t.a.child(&rhs, 1)
						} else {
							flat.empty_node
						}
						body_type := t.stmt_value_type(or_body_id)
						if !t.or_body_is_none(or_body_id)
							&& !t.map_value_type_is_optional(body_type) {
							typ = t.map_optional_value_base_type(info.value_type)
						}
					}
				} else if info := t.array_index_info(or_source_id) {
					typ = info.value_type
					if t.is_optional_type_name(info.value_type) {
						or_body_id := if rhs.children_count > 1 {
							t.a.child(&rhs, 1)
						} else {
							flat.empty_node
						}
						body_type := t.stmt_value_type(or_body_id)
						if !t.or_body_is_none(or_body_id) && !t.is_optional_type_name(body_type) {
							typ = t.optional_base_type(t.qualify_optional_type(info.value_type))
						}
					}
				} else {
					or_body_id := if rhs.children_count > 1 {
						t.a.child(&rhs, 1)
					} else {
						flat.empty_node
					}
					fallback_type := if typ.len > 0 { typ } else { t.stmt_value_type(or_body_id) }
					expr_type, value_type := t.or_expr_types(or_source_id, fallback_type)
					if t.is_optional_type_name(expr_type) && value_type.len > 0
						&& value_type != 'void' {
						typ = value_type
					}
				}
			}
			if node.typ.len == 0 {
				if rhs.kind == .array_literal && t.is_fixed_array_type(typ) {
					typ = '[]${fixed_array_elem_type(typ)}'
					t.set_node_typ(int(rhs_id), typ)
				}
			}
			if amp_vt := t.mut_param_amp_decl_type(rhs_id) {
				// `mut t := &table` where table is a `mut` value param: the RHS
				// IS the caller's pointer, so the local is single-`&`, not `&&`.
				typ = amp_vt
			}
			if typ.len > 0 {
				mut raw_typ := ''
				for candidate in [t.a.child_node(&node, 0).typ, node.typ] {
					if candidate.len > 0
						&& (t.generic_type_text_contains_alias(candidate, t.cur_module)
						|| t.enum_type_name_for_expected(candidate, t.cur_module).len > 0) {
						raw_typ = candidate
						break
					}
				}
				if raw_typ.len == 0 {
					raw_typ = t.raw_decl_type_for_rhs(rhs, typ)
				}
				if rhs.kind == .call {
					generic_raw_typ := t.raw_generic_call_return_type(rhs_id, rhs)
					if generic_raw_typ.len > 0 {
						raw_typ = generic_raw_typ
					}
				}
				t.set_var_type_with_raw(lhs.value, typ, raw_typ)
				inferred_typ = typ
			}
		}
	}
	// A value local whose address escapes (`p := &v` with `p` returned) is moved to the heap
	// at its own declaration so writes after the alias are visible to the caller. Must run
	// before the `p := &v` alias is transformed (the source is declared first).
	if node.children_count == 2 {
		src := t.a.child_node(&node, 0)
		if src.kind == .ident && src.value in t.escaping_amp_sources
			&& src.value !in t.heaped_amp_locals && t.heapable_value_type(inferred_typ) {
			return t.heap_escaping_source_decl(node, src.value, inferred_typ)
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 0 || (node.children_count > 2 && i > 1) {
			new_children << t.transform_lvalue(child_id)
		} else if node.children_count == 2 && t.try_heap_escaping_amp(node, child_id) {
			new_children << t.heap_escaping_amp_rhs(child_id)
			// When `v` was heap-moved it is already a `&T`, so `p := &v` is really `p := v`
			// (a `&T`), not `&&T` as the literal `&v` would infer. Adopt the source's pointer
			// type for `p` so its declaration and later uses are consistent.
			amp := t.a.nodes[int(child_id)]
			if amp.children_count > 0 {
				amp_src := t.a.nodes[int(t.a.child(&amp, 0))]
				if amp_src.kind == .ident && amp_src.value in t.heaped_amp_locals {
					inferred_typ = t.var_type(amp_src.value)
					t.set_var_type(t.a.nodes[int(t.a.child(&node, 0))].value, inferred_typ)
				}
			}
		} else {
			lhs_id := t.a.child(&node, 0)
			lhs_type := if inferred_typ.len > 0 {
				inferred_typ
			} else if decl_type_is_usable(node.typ) {
				node.typ
			} else {
				t.lvalue_type(lhs_id)
			}
			sum_target := t.assignment_sum_target(lhs_id, child_id, lhs_type)
			if sum_target.len > 0 && !t.expr_has_smartcast(child_id) {
				new_children << t.wrap_sum_value(child_id, sum_target)
			} else {
				new_children << t.transform_expr_for_type(child_id, lhs_type)
			}
		}
	}
	if node.children_count == 2 && !decl_type_is_usable(node.typ) {
		lhs := t.a.nodes[int(new_children[0])]
		if lhs.kind == .ident && lhs.value.len > 0 {
			rhs_typ := t.node_type(new_children[1])
			if decl_type_is_usable(rhs_typ) && (inferred_typ.len == 0
				|| inferred_typ in ['array', 'map', 'unknown']
				|| t.generic_arg_is_unresolved(inferred_typ)) {
				t.set_var_type(lhs.value, rhs_typ)
				inferred_typ = rhs_typ
			}
		}
	}
	if node.children_count == 2 && new_children.len == 2 {
		lhs := t.a.nodes[int(new_children[0])]
		if lhs.kind == .ident && lhs.value.len > 0 {
			if concrete := t.interface_box_concrete_type(new_children[1]) {
				t.interface_var_concrete_types[lhs.value] = concrete
			}
		}
	}
	if inferred_typ.len > 0 && new_children.len > 0 {
		lhs := t.a.nodes[int(new_children[0])]
		if lhs.kind == .ident && (lhs.typ.len == 0 || t.generic_arg_is_unresolved(lhs.typ)) {
			t.set_node_typ(int(new_children[0]), inferred_typ)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .decl_assign
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            if inferred_typ.len > 0 { inferred_typ } else { node.typ }
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) try_expand_plain_multi_decl(node flat.Node) ?[]flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 4 {
		return none
	}
	lhs_count := t.multi_assign_lhs_count(node)
	rhs_count := t.multi_assign_rhs_count(node)
	if lhs_count != rhs_count || rhs_count <= 1 {
		return none
	}
	mut result := []flat.NodeId{}
	for i in 0 .. lhs_count {
		lhs_id := t.multi_assign_lhs_id(node, i)
		rhs_id := t.multi_assign_rhs_id(node, i)
		lhs := t.a.nodes[int(lhs_id)]
		rhs_node := t.a.nodes[int(rhs_id)]
		generic_rhs_typ := if rhs_node.kind == .call {
			t.concrete_generic_call_return_type(rhs_id, rhs_node)
		} else {
			''
		}
		rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		if lhs.kind != .ident || lhs.value == '_' {
			continue
		}
		rhs_authority := t.decl_rhs_type(rhs_id)
		mut typ := if t.is_fn_pointer_type_name(rhs_authority) { rhs_authority } else { '' }
		if typ.len == 0 {
			typ = generic_rhs_typ
		}
		if typ.len == 0 {
			typ = t.node_type(rhs)
		}
		if typ.len == 0 {
			typ = t.node_type(rhs_id)
		}
		if typ.len == 0 {
			typ = rhs_authority
		}
		if typ.len == 0 && lhs.typ.len > 0 {
			typ = lhs.typ
		}
		if typ.len > 0 {
			typ = t.normalize_type_alias(typ)
			t.set_var_type(lhs.value, typ)
			result << t.make_decl_assign_typed(lhs.value, rhs, typ)
		} else {
			result << t.make_decl_assign(lhs.value, rhs)
		}
	}
	return result
}

// expr_has_smartcast converts expr has smartcast data for transform.
fn (t &Transformer) expr_has_smartcast(id flat.NodeId) bool {
	key := t.expr_key(id)
	return t.has_smartcast(key)
}

fn (t &Transformer) expr_has_option_unwrap_smartcast(id flat.NodeId) bool {
	key := t.expr_key(id)
	if key.len == 0 {
		return false
	}
	for sc in t.smartcasts_for(key) {
		if sc.sum_type_name == option_unwrap_marker {
			return true
		}
	}
	return false
}

// try_expand_multi_return_decl supports try expand multi return decl handling for Transformer.
fn (mut t Transformer) try_expand_multi_return_decl(node flat.Node) ?[]flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 3 || isnil(t.tc) {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	lhs_ids := t.multi_assign_lhs_ids(node)
	if t.multi_assign_rhs_count(node) != 1 {
		return none
	}
	if rhs.kind == .if_expr {
		if expanded := t.expand_multi_return_if_decl(rhs_id, rhs, lhs_ids) {
			return expanded
		}
	}
	if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
		tmp_name := t.new_temp('multi_ret')
		mut result := []flat.NodeId{}
		new_rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		result << t.make_decl_assign_typed(tmp_name, new_rhs, t.multi_return_type_name(rhs_types))
		for j, field_type in rhs_types {
			if j >= lhs_ids.len {
				continue
			}
			lhs_id := lhs_ids[j]
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind != .ident || lhs.value == '_' {
				continue
			}
			field_name := 'arg${j}'
			field_type_name := field_type.name()
			field := t.make_selector(t.make_ident(tmp_name), field_name, field_type_name)
			t.set_var_type(lhs.value, t.normalize_type_alias(field_type_name))
			result << t.make_decl_assign_typed(lhs.value, field, field_type_name)
		}
		return result
	}
	return none
}

// try_expand_multi_return_assign supports try expand multi return assign handling for Transformer.
fn (mut t Transformer) try_expand_multi_return_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count < 3 || isnil(t.tc) {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	lhs_ids := t.multi_assign_lhs_ids(node)
	if t.multi_assign_rhs_count(node) != 1 {
		return none
	}
	if rhs.kind == .if_expr {
		if expanded := t.expand_multi_return_if_assign(rhs_id, rhs, lhs_ids) {
			return expanded
		}
	}
	if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
		tmp_name := t.new_temp('multi_ret')
		mut result := []flat.NodeId{}
		new_rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		result << t.make_decl_assign_typed(tmp_name, new_rhs, t.multi_return_type_name(rhs_types))
		for j, field_type in rhs_types {
			if j >= lhs_ids.len {
				continue
			}
			lhs_id := lhs_ids[j]
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value == '_' {
				continue
			}
			field_name := 'arg${j}'
			field_type_name := field_type.name()
			field := t.make_selector(t.make_ident(tmp_name), field_name, field_type_name)
			result << t.make_assign(t.transform_lvalue(lhs_id), field)
		}
		return result
	}
	return none
}

// try_expand_plain_multi_assign supports try expand plain multi assign handling for Transformer.
fn (mut t Transformer) try_expand_plain_multi_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.op != .assign || node.children_count < 4 {
		return none
	}
	lhs_count := t.multi_assign_lhs_count(node)
	rhs_count := t.multi_assign_rhs_count(node)
	if lhs_count != rhs_count || rhs_count <= 1 {
		return none
	}
	mut result := []flat.NodeId{}
	mut lhs_ids := []flat.NodeId{}
	mut tmp_names := []string{}
	for i in 0 .. lhs_count {
		lhs_id := t.multi_assign_lhs_id(node, i)
		rhs_id := t.multi_assign_rhs_id(node, i)
		lhs_ids << lhs_id
		lhs := t.a.nodes[int(lhs_id)]
		mut lhs_type := if lhs.kind in [.selector, .index] {
			t.lvalue_type(lhs_id)
		} else {
			t.original_expr_type(lhs_id)
		}
		if lhs_type.len == 0 {
			lhs_type = t.lvalue_type(lhs_id)
		}
		rhs := if lhs_type.len > 0 {
			t.transform_expr_for_type(rhs_id, lhs_type)
		} else {
			t.transform_expr(rhs_id)
		}
		t.drain_pending(mut result)
		tmp_name := t.new_temp('assign')
		tmp_type := if lhs_type.len > 0 { lhs_type } else { t.node_type(rhs_id) }
		result << t.make_decl_assign_typed(tmp_name, rhs, tmp_type)
		tmp_names << tmp_name
	}
	for i, lhs_id in lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		result << t.make_assign(t.transform_lvalue_without_smartcast(lhs_id),
			t.make_ident(tmp_names[i]))
	}
	for lhs_id in lhs_ids {
		t.invalidate_smartcast_for_lvalue(lhs_id)
	}
	return result
}

// multi_assign_lhs_ids supports multi assign lhs ids handling for Transformer.
fn (t &Transformer) multi_assign_lhs_ids(node flat.Node) []flat.NodeId {
	lhs_count := t.multi_assign_lhs_count(node)
	mut lhs_ids := []flat.NodeId{cap: lhs_count}
	for i in 0 .. lhs_count {
		lhs_ids << t.multi_assign_lhs_id(node, i)
	}
	return lhs_ids
}

fn (t &Transformer) multi_assign_lhs_count(node flat.Node) int {
	if node.value.is_int() {
		count := node.value.int()
		if count > 0 && count <= int(node.children_count) {
			return count
		}
	}
	if node.children_count <= 2 {
		return if node.children_count > 0 { 1 } else { 0 }
	}
	return int(node.children_count) - 1
}

fn (t &Transformer) multi_assign_rhs_count(node flat.Node) int {
	lhs_count := t.multi_assign_lhs_count(node)
	rhs_count := int(node.children_count) - lhs_count
	return if rhs_count > 0 { rhs_count } else { 0 }
}

fn (t &Transformer) multi_assign_lhs_id(node flat.Node, index int) flat.NodeId {
	rhs_count := t.multi_assign_rhs_count(node)
	child_index := if index < rhs_count { index * 2 } else { rhs_count + index }
	return t.a.child(&node, child_index)
}

fn (t &Transformer) multi_assign_rhs_id(node flat.Node, index int) flat.NodeId {
	return t.a.child(&node, index * 2 + 1)
}

// multi_return_types_for_expr supports multi return types for expr handling for Transformer.
fn (t &Transformer) multi_return_types_for_expr(id flat.NodeId, expected_count int) ?[]types.Type {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	if typ := t.tc.expr_type(id) {
		if items := multi_return_types_from_type(typ, expected_count) {
			return items
		}
	}
	node := t.a.nodes[int(id)]
	if node.kind == .or_expr && node.children_count > 0 {
		return t.multi_return_types_for_expr(t.a.child(&node, 0), expected_count)
	}
	if node.kind == .match_stmt {
		return t.match_multi_return_types(node, expected_count)
	}
	if node.kind == .expr_stmt {
		return t.expr_stmt_multi_return_types(node, expected_count)
	}
	if node.kind == .block {
		return t.block_multi_return_types(node, expected_count)
	}
	if node.kind == .lock_expr && node.children_count > 0 {
		return t.multi_return_types_for_expr(t.a.child(&node, node.children_count - 1),
			expected_count)
	}
	mut typ_name := node.typ
	if node.kind == .call {
		ret := t.get_call_return_type(id, node)
		if ret.len > 0 {
			typ_name = ret
		}
	} else if typ_name.len == 0 {
		typ_name = t.resolve_expr_type(id)
	}
	if typ_name.len == 0 {
		if node.kind == .call {
			return t.find_multi_return_call_types(node, expected_count)
		}
		return none
	}
	typ := t.tc.parse_type(typ_name)
	if items := multi_return_types_from_type(typ, expected_count) {
		return items
	}
	if node.kind == .call {
		return t.find_multi_return_call_types(node, expected_count)
	}
	return none
}

fn (t &Transformer) match_multi_return_types(node flat.Node, expected_count int) ?[]types.Type {
	if node.children_count < 2 {
		return none
	}
	for i in 1 .. node.children_count {
		branch := t.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(*branch) }
		if branch.children_count <= body_start {
			continue
		}
		tail_id := t.a.child(branch, branch.children_count - 1)
		if items := t.multi_return_types_for_expr(tail_id, expected_count) {
			return items
		}
	}
	return none
}

fn (t &Transformer) expr_stmt_multi_return_types(node flat.Node, expected_count int) ?[]types.Type {
	if expected_count <= 0 || node.children_count != expected_count {
		return none
	}
	mut result := []types.Type{cap: expected_count}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut typ_name := t.node_type(child_id)
		if typ_name.len == 0 {
			typ_name = t.resolve_expr_type(child_id)
		}
		if typ_name.len == 0 {
			return none
		}
		result << t.tc.parse_type(typ_name)
	}
	return result
}

fn (t &Transformer) block_multi_return_types(node flat.Node, expected_count int) ?[]types.Type {
	if expected_count <= 0 || node.children_count == 0 {
		return none
	}
	last_id := t.a.child(&node, node.children_count - 1)
	if int(last_id) >= 0 {
		last := t.a.nodes[int(last_id)]
		if last.kind in [.block, .if_expr, .match_stmt, .expr_stmt] {
			if types := t.multi_return_types_for_expr(last_id, expected_count) {
				return types
			}
		}
	}
	mut values := []flat.NodeId{}
	for i := int(node.children_count) - 1; i >= 0; i-- {
		stmt_id := t.a.child(&node, i)
		if int(stmt_id) < 0 {
			break
		}
		stmt := t.a.nodes[int(stmt_id)]
		if stmt.kind != .expr_stmt || stmt.children_count == 0 {
			break
		}
		for j := int(stmt.children_count) - 1; j >= 0; j-- {
			values.prepend(t.a.child(&stmt, j))
			if values.len == expected_count {
				break
			}
		}
		if values.len == expected_count {
			break
		}
	}
	if values.len != expected_count {
		return none
	}
	mut result := []types.Type{cap: expected_count}
	for child_id in values {
		mut typ_name := t.node_type(child_id)
		if typ_name.len == 0 {
			typ_name = t.resolve_expr_type(child_id)
		}
		if typ_name.len == 0 {
			return none
		}
		result << t.tc.parse_type(typ_name)
	}
	return result
}

// multi_return_types_from_type converts multi return types from type data for transform.
fn multi_return_types_from_type(typ types.Type, expected_count int) ?[]types.Type {
	if typ is types.MultiReturn {
		if expected_count <= 0 || typ.types.len == expected_count {
			items := typ.types.clone()
			return items
		}
		return none
	}
	if typ is types.OptionType {
		return multi_return_types_from_type(typ.base_type, expected_count)
	}
	if typ is types.ResultType {
		return multi_return_types_from_type(typ.base_type, expected_count)
	}
	return none
}

// find_multi_return_call_types resolves find multi return call types information for transform.
fn (t &Transformer) find_multi_return_call_types(node flat.Node, expected_count int) ?[]types.Type {
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	mut candidates := []string{}
	if fn_node.kind == .ident {
		candidates << fn_node.value
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			candidates << '${t.cur_module}.${fn_node.value}'
		}
	} else if fn_node.kind == .selector {
		if fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			mut base_type := t.resolve_expr_type(base_id)
			if base_type.starts_with('&') {
				base_type = base_type[1..]
			}
			if base_type.len > 0 {
				candidates << '${base_type}.${fn_node.value}'
				if base_type.contains('.') {
					candidates << '${base_type.all_after_last('.')}.${fn_node.value}'
				}
			}
		}
		candidates << '.${fn_node.value}'
	}
	for candidate in candidates {
		if ret := t.tc.fn_ret_types[candidate] {
			if items := multi_return_types_from_type(ret, expected_count) {
				return items
			}
		}
	}
	mut has_ambiguous_candidate := false
	for candidate in candidates {
		index_key := if candidate.starts_with('.') { candidate[1..] } else { candidate }
		if indexed := t.receiver_method_suffix_index[index_key] {
			if indexed == receiver_method_suffix_ambiguous {
				has_ambiguous_candidate = true
				continue
			}
			if candidate.starts_with('.') && !indexed.ends_with(candidate) {
				continue
			}
			if ret := t.tc.fn_ret_types[indexed] {
				if items := multi_return_types_from_type(ret, expected_count) {
					return items
				}
			}
		}
	}
	if !has_ambiguous_candidate {
		return none
	}
	for candidate in candidates {
		for key, ret in t.tc.fn_ret_types {
			matches := if candidate.starts_with('.') {
				key.ends_with(candidate)
			} else {
				key == candidate || key.ends_with('.${candidate}')
			}
			if matches {
				if items := multi_return_types_from_type(ret, expected_count) {
					return items
				}
			}
		}
	}
	return none
}

// multi_return_type_name supports multi return type name handling for Transformer.
fn (t &Transformer) multi_return_type_name(items []types.Type) string {
	mut names := []string{cap: items.len}
	for item in items {
		names << item.name()
	}
	return '(${names.join(', ')})'
}

// expand_multi_return_if_decl builds expand multi return if decl data for transform.
fn (mut t Transformer) expand_multi_return_if_decl(rhs_id flat.NodeId, rhs flat.Node, lhs_ids []flat.NodeId) ?[]flat.NodeId {
	if lhs_ids.len == 0 {
		return none
	}
	if !t.if_expr_has_tuple_tail_values(rhs_id, lhs_ids.len) {
		return none
	}
	value_types := t.promoted_multi_if_value_types(rhs_id, rhs, lhs_ids.len)
	mut result := []flat.NodeId{}
	for i, lhs_id in lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value == '_' {
			continue
		}
		typ := if i < value_types.len { value_types[i] } else { 'int' }
		result << t.make_decl_assign_typed(lhs.value, t.zero_value_for_type(typ), typ)
	}
	if_stmts := t.expand_multi_return_if_assign(rhs_id, rhs, lhs_ids) or { return none }
	for stmt in if_stmts {
		result << stmt
	}
	return result
}

fn (t &Transformer) promoted_multi_if_value_types(rhs_id flat.NodeId, rhs flat.Node, count int) []string {
	if !isnil(t.tc) {
		if item_types := t.tc.multi_expr_tail_types_for_transform(rhs_id, count) {
			mut result := []string{cap: item_types.len}
			for item_type in item_types {
				result << item_type.name()
			}
			return result
		}
	}
	return t.infer_multi_if_value_types(rhs, count)
}

// expand_multi_return_if_assign builds expand multi return if assign data for transform.
fn (mut t Transformer) expand_multi_return_if_assign(rhs_id flat.NodeId, rhs flat.Node, lhs_ids []flat.NodeId) ?[]flat.NodeId {
	if rhs.kind != .if_expr || lhs_ids.len == 0 {
		return none
	}
	if !t.if_expr_has_tuple_tail_values(rhs_id, lhs_ids.len) {
		return none
	}
	return t.lower_multi_if_assign(rhs, lhs_ids)
}

fn (t &Transformer) if_expr_has_tuple_tail_values(expr_id flat.NodeId, count int) bool {
	if int(expr_id) < 0 || count <= 0 {
		return false
	}
	node := t.a.nodes[int(expr_id)]
	if node.kind != .if_expr {
		return t.branch_has_tuple_tail_values(expr_id, count)
	}
	if node.children_count > 1 && t.branch_has_tuple_tail_values(t.a.child(&node, 1), count) {
		return true
	}
	return node.children_count > 2 && t.if_expr_has_tuple_tail_values(t.a.child(&node, 2), count)
}

fn (t &Transformer) branch_has_tuple_tail_values(branch_id flat.NodeId, count int) bool {
	if int(branch_id) < 0 || count <= 0 {
		return false
	}
	if parts := t.tuple_block_parts(branch_id, count) {
		return parts.values.len > 0
	}
	branch := t.a.nodes[int(branch_id)]
	match branch.kind {
		.if_expr {
			return t.if_expr_has_tuple_tail_values(branch_id, count)
		}
		.expr_stmt {
			return branch.children_count > 0
				&& t.branch_has_tuple_tail_values(t.a.child(&branch, 0), count)
		}
		.block {
			if branch.children_count == 0 {
				return false
			}
			return t.branch_has_tuple_tail_values(t.a.child(&branch, branch.children_count - 1),
				count)
		}
		else {
			return false
		}
	}
}

// lower_multi_if_assign builds lower multi if assign data for transform.
fn (mut t Transformer) lower_multi_if_assign(node flat.Node, lhs_ids []flat.NodeId) []flat.NodeId {
	if node.children_count < 2 {
		return []
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	mut result := []flat.NodeId{}
	new_cond := t.transform_expr_for_type(cond_id, 'bool')
	t.drain_pending(mut result)
	then_block := t.multi_if_assign_block(then_id, lhs_ids)
	mut else_block := t.make_empty()
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			else_stmts := t.lower_multi_if_assign(else_node, lhs_ids)
			else_block = t.make_block(else_stmts)
		} else {
			else_block = t.multi_if_assign_block(else_id, lhs_ids)
		}
	}
	result << t.make_if(new_cond, then_block, else_block)
	return result
}

// multi_if_assign_block supports multi if assign block handling for Transformer.
fn (mut t Transformer) multi_if_assign_block(block_id flat.NodeId, lhs_ids []flat.NodeId) flat.NodeId {
	parts := t.tuple_block_parts(block_id, lhs_ids.len) or {
		if nested := t.nested_multi_tail_assign_block(block_id, lhs_ids) {
			return nested
		}
		return t.transform_expr(block_id)
	}
	stmts := t.multi_if_assign_stmts(parts, lhs_ids)
	return t.make_block(stmts)
}

fn (mut t Transformer) multi_if_assign_stmts(parts TupleBlockParts, lhs_ids []flat.NodeId) []flat.NodeId {
	mut stmts := t.transform_stmts(parts.prefix)
	mut tmp_names := []string{cap: parts.values.len}
	for i, value_id in parts.values {
		target_type := if i < lhs_ids.len { t.lvalue_type(lhs_ids[i]) } else { '' }
		value := if target_type.len > 0 {
			t.transform_expr_for_type(value_id, target_type)
		} else {
			t.transform_expr(value_id)
		}
		t.drain_pending(mut stmts)
		tmp_name := t.new_temp('multi_if')
		tmp_type := if target_type.len > 0 { target_type } else { t.node_type(value_id) }
		stmts << t.make_decl_assign_typed(tmp_name, value, tmp_type)
		tmp_names << tmp_name
	}
	for i, tmp_name in tmp_names {
		if i >= lhs_ids.len {
			continue
		}
		lhs_id := lhs_ids[i]
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		stmts << t.make_assign(t.transform_lvalue(lhs_id), t.make_ident(tmp_name))
	}
	return stmts
}

fn (mut t Transformer) nested_multi_tail_assign_block(block_id flat.NodeId, lhs_ids []flat.NodeId) ?flat.NodeId {
	if int(block_id) < 0 || lhs_ids.len == 0 {
		return none
	}
	block := t.a.nodes[int(block_id)]
	if block.kind != .block || block.children_count == 0 {
		return none
	}
	children := t.a.children_of(&block).clone()
	if children.len == 0 {
		return none
	}
	last_id := children[children.len - 1]
	last := t.a.nodes[int(last_id)]
	mut stmts := t.transform_stmts(children[..children.len - 1])
	if last.kind == .if_expr {
		nested_stmts := t.lower_multi_if_assign(last, lhs_ids)
		for stmt in nested_stmts {
			stmts << stmt
		}
		return t.make_block(stmts)
	}
	if last.kind == .block {
		nested_parts := t.tuple_block_parts(last_id, lhs_ids.len) or { return none }
		stmts << t.make_block(t.multi_if_assign_stmts(nested_parts, lhs_ids))
		return t.make_block(stmts)
	}
	return none
}

// tuple_block_parts supports tuple block parts handling for Transformer.
fn (t &Transformer) tuple_block_parts(block_id flat.NodeId, count int) ?TupleBlockParts {
	if int(block_id) < 0 || count <= 0 {
		return none
	}
	block := t.a.nodes[int(block_id)]
	if block.kind != .block {
		return none
	}
	children := t.a.children_of(&block).clone()
	if children.len == 0 {
		return none
	}
	last_id := children[children.len - 1]
	last := t.a.nodes[int(last_id)]
	if last.kind == .block {
		if nested := t.tuple_block_parts(last_id, count) {
			if nested.prefix.len == 0 && nested.values.len == count {
				return TupleBlockParts{
					prefix: children[..children.len - 1].clone()
					values: nested.values.clone()
				}
			}
		}
	}
	mut values := []flat.NodeId{}
	mut prefix_end := children.len
	for i := children.len - 1; i >= 0; i-- {
		child_id := children[i]
		child := t.a.nodes[int(child_id)]
		if child.kind != .expr_stmt || child.children_count == 0 {
			break
		}
		for j := int(child.children_count) - 1; j >= 0; j-- {
			values.prepend(t.a.child(&child, j))
			if values.len == count {
				break
			}
		}
		prefix_end = i
		if values.len == count {
			return TupleBlockParts{
				prefix: children[..prefix_end].clone()
				values: values.clone()
			}
		}
	}
	return none
}

fn (t &Transformer) multi_if_branch_value_ids(block_id flat.NodeId, count int) ?[]flat.NodeId {
	if parts := t.tuple_block_parts(block_id, count) {
		return parts.values.clone()
	}
	if int(block_id) < 0 || count <= 0 {
		return none
	}
	block := t.a.nodes[int(block_id)]
	if block.kind != .block || block.children_count == 0 {
		return none
	}
	children := t.a.children_of(&block)
	if children.len == 0 {
		return none
	}
	last_id := children[children.len - 1]
	last := t.a.nodes[int(last_id)]
	if last.kind != .if_expr || last.children_count < 2 {
		return none
	}
	return t.multi_if_branch_value_ids(t.a.child(&last, 1), count)
}

// infer_multi_if_value_types resolves infer multi if value types information for transform.
fn (t &Transformer) infer_multi_if_value_types(node flat.Node, count int) []string {
	mut result := []string{cap: count}
	if node.kind != .if_expr || node.children_count < 2 {
		return result
	}
	then_id := t.a.child(&node, 1)
	if values := t.multi_if_branch_value_ids(then_id, count) {
		for value_id in values {
			mut typ := t.tuple_value_type(value_id)
			if typ.len == 0 {
				typ = 'int'
			}
			result << typ
		}
	}
	for result.len < count {
		result << 'int'
	}
	return result
}

// tuple_value_type supports tuple value type handling for Transformer.
fn (t &Transformer) tuple_value_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.cast_expr {
			return node.value
		}
		.prefix {
			if node.children_count > 0 {
				inner := t.tuple_value_type(t.a.child(&node, 0))
				if node.op == .amp && inner.len > 0 {
					return '&${inner}'
				}
				if node.op == .mul && inner.starts_with('&') {
					return inner[1..]
				}
			}
			return ''
		}
		.paren {
			if node.children_count > 0 {
				return t.tuple_value_type(t.a.child(&node, 0))
			}
			return ''
		}
		else {
			mut typ := t.resolve_expr_type(id)
			if typ.len == 0 {
				typ = t.node_type(id)
			}
			return typ
		}
	}
}

// transform_expr_stmt transforms transform expr stmt data for transform.
fn (mut t Transformer) transform_expr_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	child_id := t.a.children[node.children_start]
	child := t.a.nodes[int(child_id)]
	if child.kind == .call && t.is_disabled_fn_call(child_id, child) {
		return []flat.NodeId{}
	}
	if t.autolock_depth == 0 {
		if lock_id := t.shared_postfix_autolock_target(child_id) {
			body := t.make_block([id])
			start := t.a.children.len
			t.a.children << lock_id
			t.a.children << body
			auto_lock := t.a.add_node(flat.Node{
				kind:           .lock_expr
				value:          'lock'
				children_start: start
				children_count: 2
			})
			return t.transform_lock_stmt(auto_lock, t.a.nodes[int(auto_lock)])
		}
	}
	if child.kind == .or_expr && !t.is_map_index_or_expr(child) && !t.is_array_index_or_expr(child)
		&& !t.is_string_slice_or_expr(child) && !t.is_channel_receive_or_expr(child) {
		if lowered := t.transform_match_trailing_or_expr(child_id, child) {
			return t.with_pending_before(lowered)
		}
		t.lower_or_expr_to_stmt(child)
		mut result := []flat.NodeId{}
		t.drain_pending(mut result)
		return result
	}
	if child.kind == .lock_expr {
		return t.transform_lock_stmt(child_id, child)
	}
	if lowered := t.try_lower_map_index_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_map_index_postfix_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_array_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_flag_enum_stmt(child_id) {
		return arr1(lowered)
	}
	new_child := t.transform_expr(child_id)
	start := t.a.children.len
	t.a.children << new_child
	new_id := t.a.add_node(flat.Node{
		kind:           .expr_stmt
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.with_pending_before(new_id)
}

fn (t &Transformer) shared_postfix_autolock_target(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .postfix || node.op !in [.inc, .dec] || node.children_count == 0 {
		return none
	}
	index := t.a.child_node(&node, 0)
	if index.kind != .index || index.children_count == 0 {
		return none
	}
	base_id := t.a.child(index, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind != .ident {
		return none
	}
	raw_type := t.raw_var_type(base.value)
	typ := if raw_type.len > 0 { raw_type } else { t.var_type(base.value) }
	if typ.trim_space().starts_with('shared ') || t.local_decl_is_shared_before(base.value, id) {
		return base_id
	}
	return none
}

fn (t &Transformer) local_decl_is_shared_before(name string, before flat.NodeId) bool {
	if name.len == 0 || int(before) < 0 || int(before) >= t.a.nodes.len {
		return false
	}
	// Follow the mutation's ancestor path and inspect only declarations preceding that
	// path in each enclosing scope; bindings inside sibling blocks must not leak out.
	mut parents := map[int]int{}
	for parent_id, node in t.a.nodes {
		for i in 0 .. node.children_count {
			child_id := int(t.a.child(&node, i))
			if child_id >= 0 && child_id != parent_id {
				parents[child_id] = parent_id
			}
		}
	}
	mut path := [int(before)]
	mut cursor := int(before)
	mut found_fn_scope := false
	for _ in 0 .. t.a.nodes.len {
		parent_id := parents[cursor] or { break }
		path << parent_id
		parent := t.a.nodes[parent_id]
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			found_fn_scope = true
			break
		}
		cursor = parent_id
	}
	if !found_fn_scope {
		return false
	}
	mut found := false
	mut is_shared := false
	for path_idx := path.len - 1; path_idx > 0; path_idx-- {
		parent := t.a.nodes[path[path_idx]]
		next_id := path[path_idx - 1]
		for i in 0 .. parent.children_count {
			child_id := int(t.a.child(&parent, i))
			if child_id == next_id {
				break
			}
			if child_id < 0 || child_id >= t.a.nodes.len {
				continue
			}
			child := t.a.nodes[child_id]
			if child.kind == .param && child.value == name {
				found = true
				is_shared = false
			} else if child.kind == .decl_assign {
				if binding_shared := t.local_decl_shared_binding(child, name) {
					found = true
					is_shared = binding_shared
				}
			}
		}
	}
	return found && is_shared
}

fn (t &Transformer) local_decl_shared_binding(node flat.Node, name string) ?bool {
	if node.kind != .decl_assign || node.children_count == 0 {
		return none
	}
	for i := 0; i < node.children_count; i += 2 {
		lhs_id := t.a.child(&node, i)
		if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
			continue
		}
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == name {
			return node.value == 'shared' || node.value.starts_with('shared:')
		}
	}
	return none
}

// transform_lock_stmt transforms transform lock stmt data for transform.
fn (mut t Transformer) transform_lock_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.with_pending_before(t.transform_lock_node(id, node))
}

// transform_for_stmt transforms transform for stmt data for transform.
fn (mut t Transformer) transform_for_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.transform_for_body(id, node)
}

// transform_for_in_stmt transforms transform for in stmt data for transform.
fn (mut t Transformer) transform_for_in_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.transform_for_in_body(id, node)
}

// transform_block_stmt transforms transform block stmt data for transform.
fn (mut t Transformer) transform_block_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	new_block := t.make_block(new_children)
	return arr1(new_block)
}

fn (mut t Transformer) transform_comptime_if_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	take_then := t.comptime_type_condition_value(node.value) or { return arr1(_id) }
	branch_index := if take_then { 0 } else { 1 }
	for i in 0 .. node.children_count {
		if i != branch_index {
			t.ignore_comptime_for_subtree(t.a.child(&node, i))
		}
	}
	if branch_index >= node.children_count {
		return []flat.NodeId{}
	}
	branch_id := t.a.child(&node, branch_index)
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .block {
		return t.transform_stmts(t.a.children_of(&branch))
	}
	return t.transform_stmt(branch_id)
}

fn (mut t Transformer) transform_comptime_if_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	take_then := t.comptime_type_condition_value(node.value) or { return id }
	branch_index := if take_then { 0 } else { 1 }
	for i in 0 .. node.children_count {
		if i != branch_index {
			t.ignore_comptime_for_subtree(t.a.child(&node, i))
		}
	}
	if branch_index >= node.children_count {
		return t.make_empty()
	}
	return t.transform_expr(t.a.child(&node, branch_index))
}

fn comptime_condition_matching_paren(s string, start int) int {
	mut paren_depth := 0
	mut bracket_depth := 0
	for i in start .. s.len {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
				if paren_depth == 0 && bracket_depth == 0 {
					return i
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			else {}
		}
	}
	return s.len
}

fn comptime_condition_strip_outer_parens(cond string) string {
	mut clean := cond.trim_space()
	for clean.len >= 2 && clean.starts_with('(') {
		end := comptime_condition_matching_paren(clean, 0)
		if end != clean.len - 1 {
			break
		}
		clean = clean[1..clean.len - 1].trim_space()
	}
	return clean
}

fn comptime_condition_top_level_index(s string, needle string) int {
	if needle.len == 0 || s.len < needle.len {
		return -1
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	for i := 0; i <= s.len - needle.len; i++ {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				if paren_depth > 0 {
					paren_depth--
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			else {}
		}

		if paren_depth == 0 && bracket_depth == 0 && s[i..i + needle.len] == needle {
			return i
		}
	}
	return -1
}

fn (mut t Transformer) comptime_type_condition_value(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond)
	if clean == 'threads' {
		return t.has_spawn_expr
	}
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := t.comptime_type_condition_value(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return t.comptime_type_condition_value(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := t.comptime_type_condition_value(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return t.comptime_type_condition_value(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			matches := t.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	for op in [' !in', ' in'] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			after := op_idx + op.len
			if after >= clean.len || (clean[after] != `[` && clean[after] != ` `) {
				continue
			}
			actual := clean[..op_idx].trim_space()
			list := clean[after..].trim_space()
			if !list.starts_with('[') || !list.ends_with(']') {
				return none
			}
			mut found := false
			for expected in split_generic_args(list[1..list.len - 1]) {
				if t.comptime_type_matches(actual, expected) or { false } {
					found = true
					break
				}
			}
			return if op == ' in' { found } else { !found }
		}
	}
	for op in [' != ', ' == ', ' <= ', ' >= ', ' < ', ' > '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx < 0 {
			continue
		}
		left := clean[..op_idx].trim_space()
		right := clean[op_idx + op.len..].trim_space()
		if !comptime_is_int(left) || !comptime_is_int(right) {
			continue
		}
		l := left.int()
		r := right.int()
		return match op {
			' != ' { l != r }
			' == ' { l == r }
			' <= ' { l <= r }
			' >= ' { l >= r }
			' < ' { l < r }
			else { l > r }
		}
	}
	if clean.starts_with('!') {
		value := t.comptime_type_condition_value(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn (mut t Transformer) comptime_type_matches(actual string, expected string) ?bool {
	mut clean_actual := t.comptime_condition_actual_type(actual)
	clean_expected := expected.trim_space()
	if clean_actual.len == 0 || clean_expected.len == 0 {
		return none
	}
	if comptime_condition_is_unresolved_value_ident(clean_actual) {
		return none
	}
	if is_generic_fn_placeholder_name(clean_actual) {
		// A real generic parameter remains undecidable until specialization. An
		// otherwise undeclared capitalized name in a comptime type test is simply
		// not the requested type (the reference compiler's behaviour).
		return if clean_actual in t.active_generic_params { none } else { false }
	}
	// `X.typ` / `X.unaliased_typ` metadata selectors compare the underlying type
	// itself; strip the suffix so a substituted `SumtypeTimeValue.unaliased_typ`
	// resolves to the concrete type instead of never matching anything.
	// `.typ` preserves alias identity (only `.unaliased_typ` compares the
	// alias target), so `MyAlias.typ is string` stays false for
	// `type MyAlias = string`.
	mut keep_alias := false
	for suffix in ['.unaliased_typ', '.typ'] {
		if clean_actual.ends_with(suffix) {
			base := clean_actual[..clean_actual.len - suffix.len].trim_space()
			if base.len == 0 || is_generic_fn_placeholder_name(base) {
				return none
			}
			clean_actual = base
			keep_alias = suffix == '.typ'
			break
		}
	}
	// `$if some_var is $int` compares the VARIABLE's type; resolve the ident
	// through the current scope before treating the text as a type name. Only
	// a `mut` param drops its internal `&` (its language-level type is `T`);
	// a real pointer variable keeps it so `$if p is $pointer` stays true and
	// `$if p is $int` stays false for `p &int`.
	mut local_name := clean_actual
	if clean_actual.contains('.') && clean_actual.all_before_last('.') == t.cur_module {
		short_name := clean_actual.all_after_last('.')
		if t.var_type(short_name).len > 0 {
			local_name = short_name
		}
	}
	if !local_name.contains('.') {
		var_typ := t.var_type(local_name)
		if var_typ.len > 0 {
			clean_actual = if t.mut_param_values[local_name] {
				var_typ.trim_string_left('&')
			} else {
				var_typ
			}
		}
	}
	is_alias_actual := keep_alias && t.generic_arg_is_alias_name(clean_actual, t.cur_module)
	normalized := if is_alias_actual {
		clean_actual
	} else {
		t.normalize_type_alias(clean_actual)
	}
	if is_alias_actual && (clean_actual == clean_expected
		|| t.qualify_type(clean_actual) == t.qualify_type(clean_expected)) {
		return true
	}
	match clean_expected {
		'$array' {
			return normalized.starts_with('[]') || transform_type_text_is_fixed_array(normalized)
		}
		'$array_dynamic' {
			return normalized.starts_with('[]')
		}
		'$array_fixed' {
			return transform_type_text_is_fixed_array(normalized)
		}
		'$map' {
			return normalized.starts_with('map[')
		}
		'$function' {
			return normalized.starts_with('fn(') || normalized.starts_with('fn (')
		}
		'$option' {
			return normalized.starts_with('?')
		}
		'$shared' {
			return normalized.starts_with('shared ')
		}
		'$pointer' {
			return normalized.starts_with('&')
		}
		'$voidptr' {
			return normalized == 'voidptr'
		}
		'$int' {
			if typ := types.builtin_type(normalized) {
				return typ.is_integer()
			}
			return false
		}
		'$float' {
			if typ := types.builtin_type(normalized) {
				return typ.is_float()
			}
			return false
		}
		'$string' {
			return normalized == 'string'
		}
		'$struct' {
			if _ := types.builtin_type(normalized) {
				return false
			}
			return t.comptime_struct_type_known(normalized)
		}
		'$enum' {
			return !isnil(t.tc) && normalized in t.tc.enum_names
		}
		'$alias' {
			if isnil(t.tc) {
				return false
			}
			if clean_actual in t.tc.type_aliases {
				return true
			}
			if !clean_actual.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
				&& t.cur_module != 'builtin' {
				return '${t.cur_module}.${clean_actual}' in t.tc.type_aliases
			}
			return false
		}
		'$sumtype' {
			return !isnil(t.tc) && normalized in t.tc.sum_types
		}
		'$interface' {
			return !isnil(t.tc) && normalized in t.tc.interface_names
		}
		else {}
	}

	expected_normalized := t.normalize_type_alias(clean_expected)
	if !isnil(t.tc) && expected_normalized in t.tc.interface_names {
		if t.tc.type_text_implements_interface(clean_actual, expected_normalized)
			|| (normalized != clean_actual
			&& t.tc.type_text_implements_interface(normalized, expected_normalized)) {
			return true
		}
	}
	return normalized == expected_normalized
}

fn comptime_condition_is_unresolved_value_ident(name string) bool {
	if name.len == 0 || name.contains('.') {
		return false
	}
	if _ := types.builtin_type(name) {
		return false
	}
	if name[0] < `a` || name[0] > `z` {
		return false
	}
	for ch in name {
		if !(ch >= `a` && ch <= `z`) && !(ch >= `A` && ch <= `Z`) && !(ch >= `0` && ch <= `9`)
			&& ch != `_` {
			return false
		}
	}
	return true
}

fn (t &Transformer) comptime_struct_type_known(raw string) bool {
	if isnil(t.tc) {
		return false
	}
	clean := raw.trim_space()
	if clean.starts_with('map[') || clean.starts_with('[]')
		|| transform_type_text_is_fixed_array(clean) {
		return false
	}
	base := comptime_generic_type_base(clean)
	for candidate in [clean, base] {
		if candidate.len == 0 {
			continue
		}
		if candidate in t.tc.structs {
			return true
		}
		short := candidate.all_after_last('.')
		for name, _ in t.tc.structs {
			name_base := comptime_generic_type_base(name)
			if name.all_after_last('.') == short
				|| (name_base.len > 0 && name_base.all_after_last('.') == short) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) comptime_condition_actual_type(raw string) string {
	mut clean := raw.trim_space()
	mut force_unaliased := false
	if clean.ends_with('.unaliased_typ') {
		clean = clean[..clean.len - '.unaliased_typ'.len].trim_space()
		force_unaliased = true
	}
	contexts := t.smartcasts_for(clean)
	if contexts.len > 0 {
		clean = contexts.last().variant_name
	}
	raw_var_type := t.raw_var_type(clean)
	if raw_var_type.len > 0 {
		clean = if t.mut_param_values[clean] {
			raw_var_type.trim_string_left('&')
		} else {
			raw_var_type
		}
	} else if typ := t.comptime_for_var_source_type(clean) {
		clean = typ
	}
	clean = t.comptime_resolve_selective_import_type(clean)
	if force_unaliased {
		return t.comptime_normalize_type_alias_chain(clean)
	}
	return clean
}

fn transform_type_text_is_fixed_array(typ string) bool {
	if typ.starts_with('[]') || typ.starts_with('[?') {
		return false
	}
	if typ.starts_with('[') {
		end := typ.index_u8(`]`)
		return end > 1
	}
	return typ.contains('[') && typ.ends_with(']') && is_decimal_text(fixed_array_len_text(typ))
}

// transform_block_expr transforms transform block expr data for transform.
fn (mut t Transformer) transform_block_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	new_block := t.make_block(new_children)
	block_typ := t.stmt_value_type(new_block)
	t.set_node_typ(int(new_block), if block_typ.len > 0 { block_typ } else { node.typ })
	return new_block
}

// transform_lock_expr transforms transform lock expr data for transform.
fn (mut t Transformer) transform_lock_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	return t.transform_lock_node(id, node)
}

fn (mut t Transformer) transform_lock_node(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count - 1 {
		lock_id := t.a.child(&node, i)
		if int(lock_id) < 0 {
			continue
		}
		children << t.transform_expr(lock_id)
	}
	body_id := t.a.child(&node, node.children_count - 1)
	if int(body_id) < 0 {
		return id
	}
	body := t.a.nodes[int(body_id)]
	t.autolock_depth++
	new_body := if body.kind == .block {
		mut new_block := if node.typ.len > 0 && node.typ != 'void' {
			t.transform_block_expr_for_type(body_id, body, node.typ) or {
				t.transform_block_expr(body_id, body)
			}
		} else {
			t.transform_block_expr(body_id, body)
		}
		block_typ := t.stmt_value_type(new_block)
		if node.typ == 'void' || block_typ == 'void' {
			mut block_children := t.a.children_of(&t.a.nodes[int(new_block)]).clone()
			block_children << t.make_expr_stmt(t.make_int_literal(0))
			new_block = t.make_block(block_children)
			t.set_node_typ(int(new_block), 'int')
		} else {
			t.set_node_typ(int(new_block), node.typ)
		}
		new_block
	} else if t.is_stmt_kind_id(node_kind_id(body)) {
		t.make_block(t.transform_stmt(body_id))
	} else {
		t.transform_expr(body_id)
	}
	t.autolock_depth--
	children << new_body
	lock_typ := if node.typ.len > 0 {
		node.typ
	} else {
		body_typ := t.node_type(new_body)
		if body_typ.len > 0 {
			body_typ
		} else {
			t.stmt_value_type(new_body)
		}
	}
	start := t.a.children.len
	t.a.children << children
	return t.a.add_node(flat.Node{
		kind:           .lock_expr
		value:          node.value
		typ:            lock_typ
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
	})
}

// transform_if_stmt transforms transform if stmt data for transform.
fn (mut t Transformer) transform_if_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if expanded := t.try_expand_if_guard(id, node) {
		return expanded
	}
	// A condition made entirely of literal string comparisons is decided now
	// (`if T.name in ['x.json2.Any', ...]` after `T.name` became a literal in
	// a specialization); the dead branch may not even typecheck for this
	// instantiation.
	if node.children_count >= 2 {
		if take_then := t.generic_const_string_cond(t.a.child(&node, 0), []) {
			branch_index := if take_then { 1 } else { 2 }
			if branch_index >= int(node.children_count) {
				return []flat.NodeId{}
			}
			branch_id := t.a.child(&node, branch_index)
			branch := t.a.nodes[int(branch_id)]
			if branch.kind == .block {
				return arr1(t.transform_block_expr(branch_id, branch))
			}
			return t.transform_stmt(branch_id)
		}
	}
	new_id := t.transform_if_branches_with_smartcast(id, node)
	return t.with_pending_before(new_id)
}

// transform_defer_stmt transforms transform defer stmt data for transform.
fn (mut t Transformer) transform_defer_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	body_id := t.a.child(&node, 0)
	if int(body_id) < 0 {
		return arr1(id)
	}
	body := t.a.nodes[int(body_id)]
	new_body := if body.kind == .block {
		t.transform_block_expr(body_id, body)
	} else if t.is_stmt_kind_id(node_kind_id(body)) {
		t.make_block(t.transform_stmt(body_id))
	} else {
		t.make_block(arr1(t.transform_expr(body_id)))
	}
	start := t.a.children.len
	t.a.children << new_body
	new_id := t.a.add_node(flat.Node{
		kind:           .defer_stmt
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return arr1(new_id)
}

fn (mut t Transformer) transform_select_stmt(node flat.Node) []flat.NodeId {
	return arr1(t.transform_select_expr(node))
}

fn (mut t Transformer) transform_select_expr(node flat.Node) flat.NodeId {
	mut branches := []flat.NodeId{cap: int(node.children_count)}
	if t.smartcast_stack.len == 0 {
		for i in 0 .. node.children_count {
			branches << t.transform_select_branch(t.a.child(&node, i))
		}
	} else {
		base_smartcasts := t.smartcast_stack.clone()
		base_invalidated := t.invalidated_smartcasts.clone()
		mut merged_invalidated := base_invalidated.clone()
		for i in 0 .. node.children_count {
			t.smartcast_stack = base_smartcasts.clone()
			t.invalidated_smartcasts = base_invalidated.clone()
			branches << t.transform_select_branch(t.a.child(&node, i))
			for key, invalidated in t.invalidated_smartcasts {
				if invalidated {
					merged_invalidated[key] = true
				}
			}
		}
		t.invalidated_smartcasts = merged_invalidated.move()
		t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
	}
	start := t.a.children.len
	for branch in branches {
		t.a.children << branch
	}
	return t.a.add_node(flat.Node{
		kind:           .select_stmt
		children_start: start
		children_count: flat.child_count(branches.len)
		pos:            node.pos
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_select_branch(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	branch := t.a.nodes[int(id)]
	if branch.kind != .select_branch {
		return t.transform_expr(id)
	}
	mut body_start := if branch.value == 'else' { 0 } else { 1 }
	if branch.children_count >= 2 {
		second := t.a.child_node(&branch, 1)
		if second.kind == .prefix && second.op == .arrow {
			body_start = 2
		}
	}
	mut bound_name := ''
	mut saved_var_types := []VarTypeBinding{}
	mut saved_smartcasts := []SmartcastContext{}
	mut saved_invalidated := map[string]bool{}
	if branch.value == 'recv' && body_start == 2 {
		lhs_id := t.a.child(&branch, 0)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
			bound_name = lhs.value
			saved_var_types = t.var_types.clone()
			if t.smartcast_stack.len > 0 {
				remaining_smartcasts := smartcasts_without_binding(t.smartcast_stack, bound_name)
				if remaining_smartcasts.len < t.smartcast_stack.len {
					saved_smartcasts = t.smartcast_stack.clone()
					saved_invalidated = t.invalidated_smartcasts.clone()
					t.smartcast_stack = remaining_smartcasts
				}
			}
		}
	}
	mut children := []flat.NodeId{cap: int(branch.children_count)}
	for i in 0 .. body_start {
		child_id := t.a.child(&branch, i)
		children << if branch.value == 'recv_assign' && body_start == 2 && i == 0 {
			t.transform_lvalue_without_smartcast(child_id)
		} else if body_start == 2 && i == 0 {
			t.transform_lvalue(child_id)
		} else {
			t.transform_expr(child_id)
		}
	}
	if branch.value == 'recv_assign' && body_start == 2 {
		t.invalidate_smartcast_for_lvalue(t.a.child(&branch, 0))
	}
	if bound_name.len > 0 {
		lhs_id := t.a.child(&branch, 0)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident {
			recv_id := t.a.child(&branch, 1)
			mut recv_type := t.node_type(recv_id)
			if recv_type.len == 0 {
				recv := t.a.nodes[int(recv_id)]
				if recv.kind == .prefix && recv.op == .arrow && recv.children_count > 0 {
					channel_id := t.a.child(&recv, 0)
					if !isnil(t.tc) {
						channel_resolved := t.tc.resolve_type(channel_id)
						if channel_resolved is types.Channel {
							recv_type = channel_resolved.elem_type.name()
						}
					}
					mut channel_type := t.normalize_type_alias(t.node_type(channel_id)).trim_space()
					for channel_type.starts_with('&') {
						channel_type = channel_type[1..].trim_space()
					}
					if channel_type.starts_with('chan ') {
						recv_type = channel_type[5..].trim_space()
					}
				}
			}
			if recv_type.len > 0 {
				t.set_var_type(bound_name, recv_type)
			}
		}
	}
	for i in body_start .. branch.children_count {
		child_id := t.a.child(&branch, i)
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(node_kind_id(child)) {
			for expanded in t.transform_stmt(child_id) {
				children << expanded
			}
		} else {
			children << t.transform_expr(child_id)
		}
	}
	if bound_name.len > 0 {
		t.restore_var_types(saved_var_types)
		if saved_smartcasts.len > 0 {
			t.restore_shadowed_smartcast_state(bound_name, saved_smartcasts, saved_invalidated)
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           .select_branch
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            branch.pos
		value:          branch.value
		typ:            branch.typ
	})
}

fn smartcasts_without_binding(contexts []SmartcastContext, name string) []SmartcastContext {
	mut keep := []SmartcastContext{cap: contexts.len}
	for sc in contexts {
		if smartcast_key_is_within_binding(sc.expr_name, name) {
			continue
		}
		keep << sc
	}
	return keep
}

fn smartcast_key_is_within_binding(key string, name string) bool {
	return key == name || key.starts_with('${name}.')
}

fn (mut t Transformer) restore_shadowed_smartcast_state(name string, base_smartcasts []SmartcastContext, base_invalidated map[string]bool) {
	mut restored_invalidated := map[string]bool{}
	for key, invalidated in t.invalidated_smartcasts {
		if !smartcast_key_is_within_binding(key, name) {
			restored_invalidated[key] = invalidated
		}
	}
	for key, invalidated in base_invalidated {
		if smartcast_key_is_within_binding(key, name) {
			restored_invalidated[key] = invalidated
		}
	}
	t.invalidated_smartcasts = restored_invalidated.move()
	t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
}

// Generic handler: rebuild a node with all children recursively transformed.
fn (mut t Transformer) transform_children_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(node_kind_id(child)) {
			expanded := t.transform_stmt(child_id)
			for eid in expanded {
				new_children << eid
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	count := new_children.len
	new_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return arr1(new_id)
}

// --- expr handlers (skeleton - identity transforms with child recursion) ---

// transform_children_expr transforms transform children expr data for transform.
fn (mut t Transformer) transform_children_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	mut changed := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			new_children << child_id
			continue
		}
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(node_kind_id(child)) {
			expanded := t.transform_stmt(child_id)
			if expanded.len == 1 {
				new_children << expanded[0]
				if expanded[0] != child_id {
					changed = true
				}
			} else {
				new_children << t.make_block(expanded)
				changed = true
			}
		} else {
			nc := t.transform_expr(child_id)
			new_children << nc
			if nc != child_id {
				changed = true
			}
		}
	}
	if !changed {
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_infix_expr transforms transform infix expr data for transform.
fn (mut t Transformer) transform_infix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if node.op in [.logical_and, .logical_or] {
		return t.transform_and_chain_smartcasts(id)
	}
	if node.op == .left_shift {
		lhs_id := t.a.children[node.children_start]
		rhs_id := t.a.children[node.children_start + 1]
		new_lhs := t.transform_expr(lhs_id)
		new_rhs := t.transform_expr(rhs_id)
		start := t.a.children.len
		t.a.children << new_lhs
		t.a.children << new_rhs
		new_id := t.a.add_node(flat.Node{
			kind:           .infix
			op:             node.op
			children_start: start
			children_count: 2
			pos:            node.pos
			value:          node.value
			typ:            node.typ
		})
		t.annotate_left_shift(new_id)
		return new_id
	}
	if str_result := t.transform_infix_string_ops(id, node) {
		return str_result
	}
	if array_result := t.transform_infix_array_ops(id, node) {
		return array_result
	}
	if map_result := t.transform_infix_map_ops(id, node) {
		return map_result
	}
	if optional_result := t.transform_infix_optional_none_ops(id, node) {
		return optional_result
	}
	if interface_result := t.transform_infix_interface_ops(id, node) {
		return interface_result
	}
	if sum_result := t.transform_infix_sum_ops(id, node) {
		return sum_result
	}
	if struct_result := t.transform_infix_struct_ops(id, node) {
		return struct_result
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	new_lhs := t.transform_expr(lhs_id)
	new_rhs := t.transform_expr(rhs_id)
	if !t.validate_specialized_comparison_operands(node, lhs_id, rhs_id, new_lhs, new_rhs) {
		return t.make_empty()
	}
	if struct_result := t.transform_transformed_struct_eq(node, new_lhs, new_rhs) {
		return struct_result
	}
	if new_lhs == lhs_id && new_rhs == rhs_id {
		// Nothing was lowered (the common case for plain arithmetic): reuse the original
		// node instead of allocating an identical copy. Under -gc none these copies are
		// never freed, so avoiding them cuts both transform time and peak RAM.
		return id
	}
	start := t.a.children.len
	t.a.children << new_lhs
	t.a.children << new_rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             node.op
		children_start: start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_call_expr transforms transform call expr data for transform.
fn (mut t Transformer) transform_call_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len > 0 && node.value == '__v_compile_error' {
		t.record_selected_compile_error_call(node)
	}
	call_id := t.normalize_generic_call_expr(id, node)
	mut call_node := t.a.nodes[int(call_id)]
	mut resolved_typ := t.concrete_generic_call_return_type(call_id, call_node)
	if resolved_typ.len == 0 {
		if array_typ := t.array_call_type_name(call_node) {
			resolved_typ = array_typ
		}
	}
	if resolved_typ.len == 0 && call_node.typ.len > 0 {
		call_typ := t.normalize_type_alias(call_node.typ)
		if call_typ !in ['array', 'map', 'unknown'] {
			resolved_typ = call_typ
		}
	}
	if resolved_typ.len == 0 {
		resolved_typ = t.new_map_call_type(call_node)
	}
	if resolved_typ.len == 0 {
		resolved_typ = t.get_call_return_type(call_id, call_node)
	}
	if resolved_typ.len == 0 {
		resolved_typ = t.current_call_return_type(call_node)
	}
	if resolved_typ.len > 0 {
		t.set_node_typ(int(call_id), resolved_typ)
		call_node.typ = resolved_typ
	}
	if t.is_disabled_fn_call(call_id, call_node) && !t.is_cgen_magic_json_call(call_id, call_node) {
		if resolved_typ.len == 0 || resolved_typ == 'void' {
			return t.make_empty()
		}
		return t.zero_value_for_type(resolved_typ)
	}
	if lowered := t.try_lower_builtin_call(call_id, call_node) {
		return lowered
	}
	if lowered := t.try_lower_array_repeat_call(call_id, call_node) {
		return lowered
	}
	if lowered := t.try_lower_join_path_call(call_id, call_node) {
		return lowered
	}
	return t.transform_call_args(call_id, call_node)
}

fn (mut t Transformer) record_selected_compile_error_call(node flat.Node) {
	if node.kind != .call || node.children_count == 0 {
		return
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != '__v_compile_error' {
		return
	}
	message := if node.children_count > 1 {
		arg := t.a.child_node(&node, 1)
		if arg.value.len > 0 {
			arg.value
		} else {
			'compile-time error'
		}
	} else {
		'compile-time error'
	}
	t.record_monomorph_error('compile-time error: ${message}')
}

fn (t &Transformer) is_cgen_magic_json_call(id flat.NodeId, node flat.Node) bool {
	return t.call_name_for_node(id, node) in ['json.decode', 'json.encode']
}

// is_disabled_fn_name reports whether is disabled fn name applies in transform.
fn (t &Transformer) is_disabled_fn_name(name string) bool {
	if name in t.a.disabled_fns {
		return true
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		return '${t.cur_module}.${name}' in t.a.disabled_fns
	}
	return false
}

// is_disabled_fn_call reports whether is disabled fn call applies in transform.
fn (t &Transformer) is_disabled_fn_call(id flat.NodeId, node flat.Node) bool {
	name := t.call_name_for_node(id, node)
	return t.is_disabled_fn_name(name)
}

// is_strings_builder_new_call reports whether is strings builder new call applies in transform.
fn (t &Transformer) is_strings_builder_new_call(id flat.NodeId, node flat.Node) bool {
	// Only `strings.new_builder` returns a `strings.Builder` (an alias for `[]u8`).
	// A bare `new_builder` must NOT be assumed to be the strings one: other modules
	// (e.g. `builder.new_builder`) and user code define their own `new_builder` that
	// return unrelated struct types. Resolve the call to its qualified name and only
	// match when it is genuinely the strings module's function.
	call_name := t.call_name_for_node(id, node)
	if call_name == 'strings.new_builder' {
		return true
	}
	if node.children_count == 0 {
		return false
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return false
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .ident {
		return fn_node.value == 'strings.new_builder'
	}
	if fn_node.kind == .selector && fn_node.value == 'new_builder' && fn_node.children_count > 0 {
		base := t.a.child_node(&fn_node, 0)
		return base.kind == .ident && base.value == 'strings'
	}
	return false
}

// transform_if_expr transforms transform if expr data for transform.
fn (mut t Transformer) transform_if_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if lowered := t.try_expand_if_expr_value(id, node) {
		return lowered
	}
	return t.transform_if_branches_with_smartcast(id, node)
}

fn transform_is_anonymous_struct_name(name string) bool {
	return name.all_after_last('.').starts_with('AnonStruct_')
}

// transform_struct_init transforms transform struct init data for transform.
fn (mut t Transformer) transform_struct_init(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value == 'struct' {
		mut concrete_type := t.raw_checker_node_type(id)
		if !transform_is_anonymous_struct_name(concrete_type) && t.expected_expr_node == int(id)
			&& transform_is_anonymous_struct_name(t.expected_expr_type) {
			concrete_type = t.expected_expr_type
		}
		if transform_is_anonymous_struct_name(concrete_type) {
			mut concrete := node
			concrete.value = concrete_type
			concrete.typ = concrete_type
			return t.transform_struct_fields(id, concrete)
		}
	}
	if node.value.len > 0 {
		clean_value := t.normalize_type_alias(node.value)
		if node.children_count == 0 {
			if default_sum := t.make_default_sum_value(clean_value) {
				return default_sum
			}
		}
		if clean_value.starts_with('[]') {
			array_node := flat.Node{
				kind:           .array_init
				op:             node.op
				children_start: node.children_start
				children_count: node.children_count
				pos:            node.pos
				value:          clean_value[2..]
				typ:            clean_value
			}
			lowered := t.lower_array_init_to_runtime(id, array_node)
			if lowered != id {
				return lowered
			}
		}
		if clean_value.starts_with('map[') {
			map_node := flat.Node{
				kind:           .map_init
				op:             node.op
				children_start: node.children_start
				children_count: node.children_count
				pos:            node.pos
				value:          clean_value
				typ:            clean_value
			}
			lowered := t.lower_map_init_to_runtime(id, map_node)
			if lowered != id {
				if node.value != clean_value {
					t.set_node_typ(int(lowered), node.value)
				}
				return lowered
			}
		}
		if t.is_optional_type_name(clean_value) {
			optional_target := t.qualify_optional_type(clean_value)
			payload_type := t.optional_base_type(optional_target)
			_ := t.lookup_struct_info(payload_type) or {
				return t.transform_struct_fields(id, node)
			}
			if node.children_count == 0 {
				return t.make_optional_none(optional_target)
			}
			payload_node := flat.Node{
				kind:           .struct_init
				op:             node.op
				children_start: node.children_start
				children_count: node.children_count
				pos:            node.pos
				value:          payload_type
				typ:            payload_type
			}
			payload := t.transform_struct_fields(id, payload_node)
			return t.make_optional_some(payload, optional_target)
		}
	}
	if !node.value.contains('[') {
		if t.expected_expr_node == int(id) && t.expected_expr_type.contains('[')
			&& t.expected_expr_type.all_before('[').all_after_last('.') == node.value.all_after_last('.') {
			mut concrete := node
			concrete.value = t.expected_expr_type
			concrete.typ = t.expected_expr_type
			return t.transform_struct_fields(id, concrete)
		}
		checker_type := t.raw_checker_node_type(id)
		if checker_type.contains('[')
			&& checker_type.all_before('[').all_after_last('.') == node.value.all_after_last('.') {
			mut concrete := node
			concrete.value = checker_type
			concrete.typ = checker_type
			return t.transform_struct_fields(id, concrete)
		}
	}
	return t.transform_struct_fields(id, node)
}

// transform_index_expr transforms transform index expr data for transform.
// lower_gated_scalar_index rewrites a scalar gated index `base#[i]` into a
// plain index whose position wraps negative values from the end:
// `base[if i < 0 { i + base.len } else { i }]`. Range forms lower in cgen
// (slice_ni/substr_ni); the `or {}` form wraps in the index-or lowering.
fn (mut t Transformer) lower_gated_scalar_index(node flat.Node) ?flat.NodeId {
	if node.op != .gated_index || node.value == 'range' || node.children_count != 2 {
		return none
	}
	base_child := t.a.child(&node, 0)
	base := t.stable_expr_for_reuse(base_child)
	idx := t.stable_expr_for_reuse(t.a.child(&node, 1))
	mut base_type := t.node_type(base)
	if base_type.len == 0 {
		base_type = t.node_type(base_child)
	}
	if base_type.len == 0 {
		base_type = t.resolve_expr_type(base_child)
	}
	base_type = t.normalize_type_alias(t.trim_pointer_type(base_type))
	// A fixed-array base has no runtime `len` member; fold the length here
	// (cgen only folds fixed `.len` for ident bases, not selectors).
	len_sel := if t.is_fixed_array_type(base_type) {
		t.make_fixed_array_len_expr(base_type)
	} else {
		t.make_selector(base, 'len', 'int')
	}
	cond := t.make_infix(.lt, idx, t.make_int_literal(0))
	t.set_node_typ(int(cond), 'bool')
	wrapped := t.make_infix(.plus, idx, len_sel)
	t.set_node_typ(int(wrapped), 'int')
	then_block := t.make_block(arr1(t.make_expr_stmt(wrapped)))
	else_block := t.make_block(arr1(t.make_expr_stmt(idx)))
	if_start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	pos_expr := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: 3
		typ:            'int'
	})
	mut index_typ := node.typ
	if index_typ.len == 0 {
		index_typ = node.value
	}
	return t.make_index(base, pos_expr, index_typ)
}

fn (mut t Transformer) transform_index_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if lowered := t.try_lower_map_index_expr(id, node) {
		return lowered
	}
	if lowered := t.lower_gated_scalar_index(node) {
		return lowered
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	mut changed := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut new_child := t.transform_expr(child_id)
		if i == 0 {
			base := t.a.nodes[int(new_child)]
			if base.kind == .cast_expr {
				base_type := t.node_type(new_child)
				new_child = t.make_paren(new_child)
				if base_type.len > 0 {
					t.set_node_typ(int(new_child), base_type)
				}
			}
		}
		if new_child != child_id {
			changed = true
		}
		new_children << new_child
	}
	// Children unchanged: update the type annotation in place (applying the same
	// `typ = node.value when empty` fixup the rebuild would) instead of copying the node.
	mut index_typ := node.typ
	elem_type := t.index_expr_type(id, node)
	if elem_type == 'u8' || (index_typ.len == 0 && elem_type.len > 0) {
		index_typ = elem_type
	} else if index_typ.len == 0 && node.value.len > 0 {
		index_typ = node.value
	}
	if !changed {
		if index_typ.len > 0 {
			t.set_node_typ(int(id), index_typ)
		}
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .index
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            index_typ
	})
	return new_id
}

// transform_string_interp transforms transform string interp data for transform.
fn (mut t Transformer) transform_string_interp(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_string_literal('')
	}
	if t.string_interp_has_unresolved_generic_part(node) {
		return id
	}
	// Some parts (arrays, optionals) lower into a prelude pushed onto pending_stmts, which
	// runs before the containing statement. If such a part follows a part with side effects,
	// keeping the earlier part inline in the string__plus chain would evaluate it after the
	// hoisted prelude, reversing source order. So once any part hoists statements, bind every
	// part to a temp in source order; while nothing hoists, keep the cheap inline form.
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut inline_parts := []flat.NodeId{cap: int(node.children_count)}
	mut temps := []flat.NodeId{cap: int(node.children_count)}
	mut hoisting := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		part := t.transform_string_interp_part(child_id)
		mut part_stmts := []flat.NodeId{}
		t.drain_pending(mut part_stmts)
		if !hoisting && part_stmts.len == 0 {
			inline_parts << part
			continue
		}
		if !hoisting {
			hoisting = true
			// Earlier parts had no prelude; bind them to temps first so their side effects
			// still happen before this part's hoisted statements.
			for earlier in inline_parts {
				earlier_name := t.new_temp('interp_part')
				t.pending_stmts << t.make_decl_assign_typed(earlier_name, earlier, 'string')
				temps << t.make_ident(earlier_name)
			}
		}
		for st in part_stmts {
			t.pending_stmts << st
		}
		name := t.new_temp('interp_part')
		t.pending_stmts << t.make_decl_assign_typed(name, part, 'string')
		temps << t.make_ident(name)
	}
	// The interp's own statements must run after any pending the surrounding context queued.
	mut interp_stmts := []flat.NodeId{}
	t.drain_pending(mut interp_stmts)
	for st in outer_pending {
		t.pending_stmts << st
	}
	for st in interp_stmts {
		t.pending_stmts << st
	}
	parts := if hoisting { temps } else { inline_parts }
	mut result := if parts.len == 0 { t.make_string_literal('') } else { parts[0] }
	for i in 1 .. parts.len {
		result = t.string_plus(result, parts[i])
	}
	t.set_node_typ(int(result), 'string')
	return result
}

fn (t &Transformer) string_interp_has_unresolved_generic_part(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if t.string_interp_child_has_unresolved_generic_part(child_id) {
			return true
		}
	}
	return false
}

fn (t &Transformer) string_interp_child_has_unresolved_generic_part(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	mut candidates := []string{cap: 5}
	candidates << t.node_type(id)
	if node.typ.len > 0 {
		candidates << t.normalize_type_alias(node.typ)
	}
	if node.kind == .ident {
		candidates << t.var_type(node.value)
	}
	candidates << t.lvalue_type(id)
	candidates << t.reliable_stringify_type(id)
	for typ in candidates {
		if t.stringify_type_has_generic_placeholder(typ) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) ensure_stringify_generic_instances_for_type(typ string) {
	clean := typ.trim_space()
	if clean.len == 0 || t.stringify_type_has_generic_placeholder(clean) {
		return
	}
	if clean.starts_with('&') {
		t.ensure_stringify_generic_instances_for_type(clean[1..])
		return
	}
	if clean.starts_with('mut ') {
		t.ensure_stringify_generic_instances_for_type(clean[4..])
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.ensure_stringify_generic_instances_for_type(clean[1..])
		return
	}
	if clean.starts_with('...') {
		t.ensure_stringify_generic_instances_for_type(clean[3..])
		return
	}
	if clean.starts_with('[]') {
		t.ensure_stringify_generic_instances_for_type(clean[2..])
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.ensure_stringify_generic_instances_for_type(clean[4..bracket_end])
			t.ensure_stringify_generic_instances_for_type(clean[bracket_end + 1..])
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			t.ensure_stringify_generic_instances_for_type(clean[bracket_end + 1..])
		}
		return
	}
	base, args, ok := generic_app_parts(clean)
	if !ok {
		return
	}
	for arg in args {
		t.ensure_stringify_generic_instances_for_type(arg)
	}
	if clean in t.structs {
		return
	}
	base_info := t.lookup_struct_info_direct(base) or { return }
	if !t.struct_info_matches_generic_base(base, base_info) {
		return
	}
	params := t.generic_struct_params_for_stringify(base)
	mut fields := []FieldInfo{cap: base_info.fields.len}
	for field in base_info.fields {
		field_typ := if params.len > 0 {
			substitute_generic_type_text_with_params(field.typ, args, params)
		} else {
			substitute_generic_type_text(field.typ, args)
		}
		fields << FieldInfo{
			name:         field.name
			typ:          field_typ
			raw_typ:      field.raw_typ
			default_expr: field.default_expr
		}
	}
	t.structs[clean] = StructInfo{
		name:      clean
		module:    base_info.module
		is_params: base_info.is_params
		fields:    fields
	}
}

fn (t &Transformer) struct_info_matches_generic_base(base string, info StructInfo) bool {
	if base.contains('.') || info.module.len == 0 || info.module == 'main' || isnil(t.tc) {
		return true
	}
	qualified_base := '${info.module}.${base}'
	if _ := t.tc.struct_generic_params[qualified_base] {
		return true
	}
	if params := t.tc.struct_generic_params[base] {
		return struct_info_uses_generic_params(info, params)
	}
	return false
}

fn struct_info_uses_generic_params(info StructInfo, params []string) bool {
	for field in info.fields {
		if type_text_uses_any_generic_param(field.typ, params)
			|| type_text_uses_any_generic_param(field.raw_typ, params) {
			return true
		}
	}
	return false
}

fn type_text_uses_any_generic_param(typ string, params []string) bool {
	for param in params {
		if type_text_uses_generic_param(typ, param) {
			return true
		}
	}
	return false
}

fn type_text_uses_generic_param(typ string, param string) bool {
	if typ.len == 0 || param.len == 0 {
		return false
	}
	mut i := 0
	for i < typ.len {
		idx := typ[i..].index(param) or { return false }
		pos := i + idx
		before_ok := pos == 0 || !generic_param_ident_char(typ[pos - 1])
		after := pos + param.len
		after_ok := after >= typ.len || !generic_param_ident_char(typ[after])
		if before_ok && after_ok {
			return true
		}
		i = after
	}
	return false
}

fn generic_param_ident_char(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch == `_`
}

fn (t &Transformer) generic_struct_info_for_stringify(typ string) ?StructInfo {
	clean := typ.trim_space().trim_left('&')
	base, args, ok := generic_app_parts(clean)
	if !ok || args.len == 0 {
		return none
	}
	base_info := t.lookup_struct_info_direct(base) or { return none }
	if !t.struct_info_matches_generic_base(base, base_info) {
		return none
	}
	params := t.generic_struct_params_for_stringify(base)
	mut fields := []FieldInfo{cap: base_info.fields.len}
	for field in base_info.fields {
		field_typ := if params.len > 0 {
			substitute_generic_type_text_with_params(field.typ, args, params)
		} else {
			substitute_generic_type_text(field.typ, args)
		}
		fields << FieldInfo{
			name:         field.name
			typ:          field_typ
			raw_typ:      field_typ
			default_expr: field.default_expr
			is_embedded:  field.is_embedded
		}
	}
	return StructInfo{
		name:      clean
		module:    base_info.module
		is_params: base_info.is_params
		fields:    fields
	}
}

fn (t &Transformer) generic_struct_params_for_stringify(base string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	if params := t.tc.struct_generic_params[base] {
		return params
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		if params := t.tc.struct_generic_params['${t.cur_module}.${base}'] {
			return params
		}
	}
	if base.contains('.') {
		if params := t.tc.struct_generic_params[base.all_after_last('.')] {
			return params
		}
	}
	return []string{}
}

fn (t &Transformer) stringify_type_has_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if clean == 'generic' {
		return true
	}
	if is_generic_fn_placeholder_name(clean) {
		return !t.is_known_concrete_type_name(clean)
	}
	if clean.starts_with('&') {
		return t.stringify_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('mut ') {
		return t.stringify_type_has_generic_placeholder(clean[4..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.stringify_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('...') {
		return t.stringify_type_has_generic_placeholder(clean[3..])
	}
	if clean.starts_with('[]') {
		return t.stringify_type_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.stringify_type_has_generic_placeholder(clean[4..bracket_end])
				|| t.stringify_type_has_generic_placeholder(clean[bracket_end + 1..])
		}
		return false
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.stringify_type_has_generic_placeholder(clean[bracket_end + 1..])
		}
		return false
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if t.stringify_type_has_generic_placeholder(arg) {
				return true
			}
		}
	}
	return false
}

fn stringify_type_has_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if is_generic_placeholder_type_name(clean) {
		return true
	}
	if clean.starts_with('&') {
		return stringify_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return stringify_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('...') {
		return stringify_type_has_generic_placeholder(clean[3..])
	}
	if clean.starts_with('[]') {
		return stringify_type_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return stringify_type_has_generic_placeholder(clean[4..bracket_end])
				|| stringify_type_has_generic_placeholder(clean[bracket_end + 1..])
		}
		return false
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return stringify_type_has_generic_placeholder(clean[bracket_end + 1..])
		}
		return false
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if stringify_type_has_generic_placeholder(arg) {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) transform_selector_base_expr(id flat.NodeId) flat.NodeId {
	// `in_selector_base` suppresses the pointer-value rvalue deref in
	// transform_ident_expr, but that must apply only to the *direct* receiver ident of
	// a selector (`x.field`, where `x` stays `&T` so the selector emits arrow access).
	// A compound base such as a call (`wrap(x).field`) or index expr may contain nested
	// idents (e.g. the call argument `x`) that still need their rvalue deref, so only
	// engage the flag when the base is a plain ident — including one wrapped in
	// transparent parentheses (`(x).field`, `((x)).field`), where `x` is still the
	// direct receiver.
	if !t.selector_base_is_ident_receiver(id) {
		return t.transform_expr(id)
	}
	old_in_selector_base := t.in_selector_base
	t.in_selector_base = true
	transformed := t.transform_expr(id)
	t.in_selector_base = old_in_selector_base
	return transformed
}

// selector_base_is_ident_receiver reports whether a selector base is a plain ident
// receiver, seeing through transparent parenthesis chains (`(x)`, `((x))`). Compound
// bases like `wrap(x)` or `a[i]` are not idents and stay on the normal transform path.
fn (t &Transformer) selector_base_is_ident_receiver(id flat.NodeId) bool {
	mut node := t.a.nodes[int(id)]
	for node.kind == .paren && node.children_count > 0 {
		node = t.a.nodes[int(t.a.child(&node, 0))]
	}
	return node.kind == .ident
}

fn (t &Transformer) transformed_selector_type(node flat.Node) string {
	resolved := t.resolve_selector_type(node)
	if resolved.starts_with('&') && !node.typ.starts_with('&') {
		return resolved
	}
	return if node.typ.len > 0 { node.typ } else { resolved }
}

// transform_selector_expr transforms transform selector expr data for transform.
fn (mut t Transformer) transform_selector_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if node.value in t.sum_variant_fields {
		return id
	}
	base_id0 := t.a.child(&node, 0)
	if variant_type := t.generated_variant_access_type(base_id0) {
		new_base := t.transform_selector_base_expr(base_id0)
		clean_variant_type := t.trim_pointer_type(variant_type)
		sel_typ := if ftyp := t.lookup_struct_field_type(clean_variant_type, node.value) {
			ftyp
		} else {
			t.transformed_selector_type(node)
		}
		return t.make_selector_op(new_base, node.value, sel_typ, if variant_type.starts_with('&') {
			.arrow
		} else {
			node.op
		})
	}
	base_node0 := t.a.nodes[int(base_id0)]
	if base_node0.kind == .typeof_expr {
		if node.value == 'name' {
			return t.transform_typeof_expr(base_id0, base_node0)
		}
		if node.value == 'idx' {
			return t.transform_typeof_idx_expr(base_node0)
		}
		if node.value in ['key_type', 'value_type', 'element_type'] {
			base_type := t.typeof_type_name(base_node0)
			if member_type := t.generic_comptime_type_member(base_type, node.value) {
				return t.make_int_literal(t.comptime_field_type_id(member_type, t.cur_module))
			}
		}
	}
	if fixed_len := t.transform_fixed_array_len(id, node) {
		return fixed_len
	}
	full_key := t.expr_key(id)
	if full_key.len > 0 {
		contexts := t.smartcasts_for(full_key)
		if contexts.len > 0 {
			plain := t.make_plain_selector_expr(id, node)
			return t.apply_smartcast_contexts(plain, t.original_expr_type(id), contexts)
		}
	}
	base_id := base_id0
	sc_key := t.expr_key(base_id)
	if sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		if contexts.len > 0 {
			plain_base := t.make_plain_expr_for_smartcast(base_id)
			variant_sel := t.apply_smartcast_contexts(plain_base, t.original_expr_type(base_id),
				contexts)
			variant_type := t.node_type(variant_sel)
			if shared_typ := t.sum_shared_field_type_name(variant_type, node.value) {
				return t.lower_sum_shared_field_selector(variant_sel, variant_type, node.value,
					shared_typ)
			}
			sel_start := t.a.children.len
			t.a.children << variant_sel
			sel_typ := t.transformed_selector_type(node)
			return t.a.add_node(flat.Node{
				kind:           .selector
				op:             if node.op == .arrow || variant_type.starts_with('&') {
					flat.Op.arrow
				} else {
					flat.Op.dot
				}
				children_start: sel_start
				children_count: 1
				pos:            node.pos
				value:          node.value
				typ:            sel_typ
			})
		}
	}
	base_type0 := t.node_type(base_id)
	base_clean := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if info := t.lookup_struct_info(base_clean) {
		has_direct_field := (t.struct_field_type(info, node.value) or { '' }).len > 0
		if !has_direct_field {
			if embedded := t.embedded_field_for_promoted_field(info, node.value) {
				new_base := t.transform_selector_base_expr(base_id)
				embedded_op := if base_type0.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				embedded_sel := t.make_selector_op(new_base, embedded.name, embedded.typ,
					embedded_op)
				sel_typ := t.transformed_selector_type(node)
				final_op := if embedded.typ.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				return t.make_selector_op(embedded_sel, node.value, sel_typ, final_op)
			}
		}
	}
	if shared_typ := t.sum_shared_field_type_name(base_type0, node.value) {
		transformed_base := t.transform_selector_base_expr(base_id)
		transformed_base_type := t.node_type(transformed_base)
		clean_transformed_base_type := if transformed_base_type.starts_with('&') {
			transformed_base_type[1..]
		} else {
			transformed_base_type
		}
		if clean_transformed_base_type.len > 0
			&& t.normalize_type_alias(clean_transformed_base_type) != t.normalize_type_alias(base_type0) {
			if ftyp := t.lookup_struct_field_type(clean_transformed_base_type, node.value) {
				new_base := t.selector_base_for_field(transformed_base, transformed_base_type)
				return t.make_selector_op(new_base, node.value, if node.typ.len > 0 {
					node.typ
				} else {
					ftyp
				}, if transformed_base_type.starts_with('&') {
					.arrow
				} else {
					.dot
				})
			}
			if new_shared_typ := t.sum_shared_field_type_name(transformed_base_type, node.value) {
				new_base := t.selector_base_for_field(transformed_base, transformed_base_type)
				return t.lower_sum_shared_field_selector(new_base, transformed_base_type,
					node.value, new_shared_typ)
			}
		}
		new_base := t.selector_base_for_field(transformed_base, base_type0)
		return t.lower_sum_shared_field_selector(new_base, base_type0, node.value, shared_typ)
	}
	new_base := t.transform_selector_base_expr(base_id)
	mut changed := new_base != base_id
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << new_base
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		nc := t.transform_expr(child_id)
		if nc != child_id {
			changed = true
		}
		new_children << nc
	}
	sel_typ := t.transformed_selector_type(node)
	base_type := t.node_type(new_base)
	sel_op := if node.op == .arrow || base_type.starts_with('&') { flat.Op.arrow } else { node.op }
	if !changed && sel_op == node.op {
		// Children and op unchanged; only the type annotation may differ. Update it in
		// place rather than allocating an identical copy (cuts -gc none peak RAM). (`op`
		// is an immutable Node field, so a differing op still needs a fresh node below.)
		t.set_node_typ(int(id), sel_typ)
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             sel_op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            sel_typ
	})
}

// make_plain_selector_expr builds make plain selector expr data for transform.
fn (mut t Transformer) make_plain_selector_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	new_base := t.selector_base_for_field(t.transform_selector_base_expr(base_id), base_type)
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << new_base
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	sel_typ := t.transformed_selector_type(node)
	transformed_base_type := t.node_type(new_base)
	sel_op := if node.op == .arrow || transformed_base_type.starts_with('&') {
		flat.Op.arrow
	} else {
		node.op
	}
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             sel_op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            sel_typ
	})
}

// make_plain_expr_for_smartcast builds make plain expr for smartcast data for transform.
fn (mut t Transformer) make_plain_expr_for_smartcast(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			expr := t.make_ident(node.value)
			typ := t.original_expr_type(id)
			if typ.len > 0 {
				t.set_node_typ(int(expr), typ)
			}
			return expr
		}
		.selector {
			return t.make_plain_selector_expr(id, node)
		}
		.index {
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			for i in 0 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for nc in new_children {
				t.a.children << nc
			}
			return t.a.add_node(flat.Node{
				kind:           .index
				op:             node.op
				children_start: start
				children_count: node.children_count
				pos:            node.pos
				value:          node.value
				typ:            if node.typ.len > 0 { node.typ } else { node.value }
			})
		}
		else {
			return t.transform_expr(id)
		}
	}
}

// selector_base_for_field supports selector base for field handling for Transformer.
fn (mut t Transformer) selector_base_for_field(base flat.NodeId, typ string) flat.NodeId {
	if int(base) < 0 {
		return base
	}
	node := t.a.nodes[int(base)]
	if node.kind in [.if_expr, .block] {
		return t.stable_transformed_expr_for_reuse(base, typ, 'sel_base')
	}
	return base
}

// sum_shared_field_type_name supports sum shared field type name handling for Transformer.
fn (t &Transformer) sum_shared_field_type_name(sum_type string, field string) ?string {
	mut visited := map[string]bool{}
	return t.sum_shared_field_type_name_inner(sum_type, field, mut visited)
}

fn (t &Transformer) sum_shared_field_type_name_inner(sum_type string, field string, mut visited map[string]bool) ?string {
	clean_sum := if sum_type.starts_with('&') { sum_type[1..] } else { sum_type }
	resolved_sum := t.resolve_sum_name(clean_sum)
	// A recursive sum (`Any = ... | []Any | map[string]Any`) revisits itself
	// through its variants; treat the cycle as "no shared field". `visited`
	// tracks only the current DESCENT PATH — the mark is removed on the way
	// out so a diamond shape (two sibling variants nesting the same sum) is
	// not mistaken for a cycle.
	if visited[resolved_sum] {
		return none
	}
	visited[resolved_sum] = true
	defer {
		visited.delete(resolved_sum)
	}
	variants := t.sum_types[resolved_sum] or { return none }
	mut common := ''
	for variant in variants {
		ftyp := t.sum_variant_field_type_name_inner(variant, field, mut visited) or { return none }
		if common.len == 0 {
			common = ftyp
			continue
		}
		if t.normalize_type_alias(common) != t.normalize_type_alias(ftyp) {
			return none
		}
	}
	if common.len == 0 {
		return none
	}
	return common
}

// sum_variant_field_type_name supports sum variant field type name handling for Transformer.
fn (t &Transformer) sum_variant_field_type_name(variant string, field string) ?string {
	mut visited := map[string]bool{}
	return t.sum_variant_field_type_name_inner(variant, field, mut visited)
}

fn (t &Transformer) sum_variant_field_type_name_inner(variant string, field string, mut visited map[string]bool) ?string {
	if ftyp := t.lookup_struct_field_type(variant, field) {
		return ftyp
	}
	if ftyp := t.sum_shared_field_type_name_inner(variant, field, mut visited) {
		return ftyp
	}
	return none
}

// lower_sum_shared_field_selector builds lower sum shared field selector data for transform.
fn (mut t Transformer) lower_sum_shared_field_selector(base flat.NodeId, sum_type string, field string, field_type string) flat.NodeId {
	clean_sum := if sum_type.starts_with('&') { sum_type[1..] } else { sum_type }
	resolved_sum := t.resolve_sum_name(clean_sum)
	variants := t.sum_types[resolved_sum] or { return base }
	return t.build_sum_shared_field_chain(base, sum_type, resolved_sum, variants, field,
		field_type, 0)
}

// build_sum_shared_field_chain builds sum shared field chain data for transform.
fn (mut t Transformer) build_sum_shared_field_chain(base flat.NodeId, sum_type string, resolved_sum string, variants []string, field string, field_type string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.zero_value_for_type(field_type)
	}
	variant := variants[idx]
	tag := t.make_sum_tag_selector(base, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(resolved_sum, variant)))
	qv := t.resolve_variant(resolved_sum, variant)
	sum_field := t.sum_field_name(qv)
	use_ptr := t.variant_references_sum(qv, resolved_sum)
	variant_base := t.make_selector_op(base, sum_field, if use_ptr { '&${qv}' } else { qv }, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	value := if _ := t.sum_shared_field_type_name(qv, field) {
		t.lower_sum_shared_field_selector(variant_base, qv, field, field_type)
	} else {
		t.make_selector_op(variant_base, field, field_type, if use_ptr { .arrow } else { .dot })
	}
	then_block := t.make_block(arr1(t.make_expr_stmt(value)))
	else_expr := t.build_sum_shared_field_chain(base, sum_type, resolved_sum, variants, field,
		field_type, idx + 1)
	else_block := t.make_block(arr1(t.make_expr_stmt(else_expr)))
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            field_type
	})
}

// transform_or_expr transforms transform or expr data for transform.
fn (mut t Transformer) transform_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if t.in_const_init {
		return id
	}
	if lowered := t.transform_match_trailing_or_expr(id, node) {
		return lowered
	}
	expr_id := t.a.child(&node, 0)
	if node.value == '?' && t.expr_has_option_unwrap_smartcast(expr_id) {
		return t.transform_expr(expr_id)
	}
	if t.is_channel_receive_or_expr(node) {
		return t.transform_channel_receive_or_expr(id, node)
	}
	expr_type, value_type := t.or_expr_types(expr_id, node.typ)
	if expr_type.contains('unknown') || value_type.contains('unknown')
		|| t.type_text_has_generic_placeholder(expr_type, t.cur_module)
		|| t.type_text_has_generic_placeholder(value_type, t.cur_module) {
		return id
	}
	if t.is_map_index_or_expr(node) {
		return t.transform_map_index_or_expr(id, node)
	}
	if t.is_array_index_or_expr(node) {
		return t.transform_array_index_or_expr(id, node)
	}
	if t.is_string_slice_or_expr(node) {
		return t.transform_string_slice_or_expr(id, node)
	}
	if t.is_enum_from_string_or_expr(node) {
		return t.transform_enum_from_string_or_expr(id, node)
	}
	return t.lower_or_expr_to_temp(id, node)
}

fn (mut t Transformer) transform_match_trailing_or_expr(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.kind != .or_expr || node.children_count < 2 {
		return none
	}
	match_id := t.a.child(&node, 0)
	match_node := t.a.nodes[int(match_id)]
	if match_node.kind != .match_stmt || match_node.children_count == 0 {
		return none
	}
	match_expr_id := t.a.child(&match_node, 0)
	body_id := t.a.child(&node, 1)
	body_type := t.stmt_value_type(body_id)
	expr_type, _ := t.or_expr_types(match_expr_id, body_type)
	match_or_start := t.a.children.len
	t.a.children << match_expr_id
	t.a.children << body_id
	match_or_id := t.a.add_node(flat.Node{
		kind:           .or_expr
		op:             node.op
		children_start: match_or_start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            expr_type
	})
	mut children := []flat.NodeId{cap: int(match_node.children_count)}
	children << match_or_id
	for i in 1 .. match_node.children_count {
		children << t.a.child(&match_node, i)
	}
	match_start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	new_match := t.a.add_node(flat.Node{
		kind:           .match_stmt
		children_start: match_start
		children_count: flat.child_count(children.len)
		pos:            match_node.pos
		value:          match_node.value
		typ:            match_node.typ
	})
	return t.transform_expr(new_match)
}

// transform_prefix_expr transforms transform prefix expr data for transform.
// mut_param_amp_decl_type detects `&param` (possibly as an unsafe-block tail)
// over a `mut` value param and returns the param's pointer type for the decl.
fn (t &Transformer) mut_param_amp_decl_type(rhs_id flat.NodeId) ?string {
	if int(rhs_id) < 0 {
		return none
	}
	mut node := t.a.nodes[int(rhs_id)]
	if node.kind == .block && node.children_count > 0 {
		tail_id := t.a.child(&node, int(node.children_count) - 1)
		if int(tail_id) < 0 {
			return none
		}
		mut tail := t.a.nodes[int(tail_id)]
		if tail.kind == .expr_stmt && tail.children_count > 0 {
			inner := t.a.child(&tail, 0)
			if int(inner) < 0 {
				return none
			}
			tail = t.a.nodes[int(inner)]
		}
		node = tail
	}
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child := t.a.child_node(&node, 0)
	if child.kind != .ident || !t.mut_param_values[child.value] {
		return none
	}
	mut vt := t.var_type(child.value)
	if vt.starts_with('mut ') {
		vt = '&' + vt[4..].trim_space()
	}
	if !vt.starts_with('&') {
		return none
	}
	return vt
}

fn (mut t Transformer) transform_prefix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if node.op in [.plus, .minus] && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		if signed_str_call := t.rewrite_signed_literal_str_call(node.op, child_id) {
			return t.transform_expr(signed_str_call)
		}
	}
	if node.op == .mul && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.original_expr_type(child_id)
		}
		if child.kind != .cast_expr && child_type.len > 0 && !child_type.starts_with('&') {
			return t.transform_expr(child_id)
		}
	}
	if node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		// `&param` where `param` is a `mut` value param IS the pointer the
		// caller passed; adding another `&` would take the address of the
		// parameter slot (`map** t = table` + `*t = ...` clobbers the caller).
		if child.kind == .ident && t.mut_param_values[child.value] {
			mut vt := t.var_type(child.value)
			if vt.starts_with('mut ') {
				vt = '&' + vt[4..].trim_space()
			}
			if vt.starts_with('&') {
				new_id := t.transform_expr(child_id)
				t.set_node_typ(int(new_id), vt)
				return new_id
			}
		}
		if child.kind == .struct_init {
			// `&T{...}` (address of a struct literal) is ALWAYS a heap allocation in V,
			// in any context — not just in a return. Keeping it as a `.prefix .amp`
			// struct_init routes it through cgen's gen_heap_struct_init; otherwise the
			// generic fall-through lowers it to `&<stack temp>`, which dangles once the
			// frame dies (e.g. `arr << &T{...}` storing a stack pointer in the array).
			if expr := t.transform_amp_struct_init_for_type(id, node, node.typ) {
				return expr
			}
		}
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.resolve_expr_type(child_id)
		}
		if child.kind in [.array_init, .array_literal]
			&& t.normalize_type_alias(child_type).starts_with('[]') {
			// `&[]T{...}` owns a heap-allocated array header. Allocate that header
			// explicitly instead of taking the address of a short-lived stabilization
			// temporary.
			value := t.transform_expr(child_id)
			result_type := if node.typ.len > 0 { node.typ } else { '&${child_type}' }
			return t.make_call_typed('v3_heap_array', arr1(value), result_type)
		}
		if expr := t.transform_amp_assoc_expr_for_type(id, node, node.typ) {
			return expr
		}
		if child.kind == .cast_expr && child.children_count > 0 {
			cast_arg_id := t.a.child(&child, 0)
			target_sum := t.resolve_sum_name(t.normalize_type_alias(child.value))
			if target_sum.len > 0 && target_sum in t.sum_types {
				cast_arg := t.a.nodes[int(cast_arg_id)]
				if cast_arg.kind == .nil_literal {
					return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id),
						'&${child.value}')
				}
				wrapped := t.wrap_sum_value(cast_arg_id, target_sum)
				addr := t.make_prefix(.amp, wrapped)
				t.set_node_typ(int(addr),
					if node.typ.len > 0 { node.typ } else { '&${target_sum}' })
				return addr
			}
			// `&InterfaceType(x)` (e.g. `&PRNG(rng)`): box the concrete into a
			// heap-allocated interface so the resulting pointer stays valid, rather
			// than emitting a plain `(Interface*)x` reinterpret cast.
			mut iface := t.resolve_interface_type_name(child.value)
			if iface.len == 0 {
				mut prefix_type := node.typ
				if prefix_type.len == 0 {
					prefix_type = t.node_type(id)
				}
				target_iface := t.resolve_interface_type_name(prefix_type)
				if target_iface.len > 0
					&& t.interface_cast_matches_target(child.value, target_iface) {
					iface = target_iface
				}
			}
			if iface.len > 0 && !t.is_builtin_ierror_interface_name(iface) {
				if boxed := t.transform_interface_value_for_type(cast_arg_id, '&${iface}', false) {
					return boxed
				}
			}
			cast_arg := t.a.nodes[int(cast_arg_id)]
			if cast_arg.kind == .nil_literal {
				return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id),
					'&${child.value}')
			}
			return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id), '&${child.value}')
		}
		if child.kind == .or_expr && child.children_count >= 2
			&& t.or_body_is_nil(t.a.child(&child, 1)) {
			index_id := t.a.child(&child, 0)
			if info := t.map_index_info(index_id) {
				map_expr := t.stable_expr_for_reuse(info.base_id)
				key_name := t.new_temp('map_key')
				t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(info.key_id,
					info.key_type), info.key_storage_type)
				ptr := t.make_map_get_check_expr(map_expr, info.base_type, key_name)
				return t.make_cast('&${info.value_type}', ptr, '&${info.value_type}')
			}
		}
		if child.kind == .or_expr && child.children_count >= 2 {
			if addr := t.transform_amp_optional_unwrap(node, child) {
				return addr
			}
		}
		if child.kind == .call && child.children_count == 2 {
			callee := t.a.child_node(&child, 0)
			arg_id := t.a.child(&child, 1)
			arg := t.a.nodes[int(arg_id)]
			if callee.kind == .selector && callee.children_count > 0
				&& (arg.kind == .nil_literal || callee.value.len > 0) {
				base := t.a.child_node(callee, 0)
				if base.kind == .ident && callee.value.len > 0
					&& (base.value == 'C' || (callee.value[0] >= `A` && callee.value[0] <= `Z`)) {
					target_type := '${base.value}.${callee.value}'
					return t.make_cast('&${target_type}', t.transform_expr(arg_id),
						'&${target_type}')
				}
			}
		}
		if child.kind == .selector && (t.selector_chain_has_sum_shared_field(child_id)
			|| t.selector_chain_has_sum_variant_field(child_id)) {
			value := t.transform_expr(child_id)
			mut value_type := t.node_type(child_id)
			if value_type.len == 0 {
				value_type = t.node_type(value)
			}
			stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
			addr := t.make_prefix(.amp, stable)
			if value_type.len > 0 {
				t.set_node_typ(int(addr), '&${value_type}')
			}
			return addr
		}
		if child.kind == .ident && child.value.len > 0 && t.has_smartcast(child.value)
			&& node.typ.starts_with('&') && t.is_sum_type_name(node.typ[1..]) {
			sum_type := node.typ[1..]
			wrapped := t.wrap_sum_value(child_id, sum_type)
			tmp_name := t.new_temp('sum_ref')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, sum_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.set_node_typ(int(addr), node.typ)
			return addr
		}
		value := t.transform_expr(child_id)
		if !t.expr_can_take_address(value) {
			mut value_type := t.node_type(child_id)
			if value_type.len == 0 {
				value_type = t.node_type(value)
			}
			stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
			addr := t.make_prefix(.amp, stable)
			if value_type.len > 0 {
				t.set_node_typ(int(addr), '&${value_type}')
			}
			return addr
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut new_child := if node.op == .not {
			t.transform_expr_for_type(child_id, 'bool')
		} else {
			t.transform_expr(child_id)
		}
		if node.op == .not {
			child := t.a.nodes[int(new_child)]
			if child.kind == .infix {
				new_child = t.make_paren(new_child)
			}
		}
		new_children << new_child
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .prefix
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	if node.children_count == 1 {
		child_type := t.node_type(new_children[0])
		if node.op == .amp && child_type.len > 0 {
			t.set_node_typ(int(new_id), '&${child_type}')
		} else if node.op == .mul && child_type.starts_with('&') {
			t.set_node_typ(int(new_id), child_type[1..])
		}
	}
	return new_id
}

fn (mut t Transformer) rewrite_signed_literal_str_call(op flat.Op, child_id flat.NodeId) ?flat.NodeId {
	child := t.a.nodes[int(child_id)]
	if child.kind != .call || child.children_count != 1 {
		return none
	}
	callee_id := t.a.child(&child, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .selector || callee.value != 'str' || callee.children_count != 1 {
		return none
	}
	base_id := t.a.child(&callee, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind !in [.int_literal, .float_literal] {
		return none
	}
	signed_base := t.make_prefix(op, base_id)
	return t.make_method_call(signed_base, 'str', []flat.NodeId{})
}

fn (mut t Transformer) transform_amp_optional_unwrap(node flat.Node, child flat.Node) ?flat.NodeId {
	source_id := t.a.child(&child, 0)
	body_id := t.a.child(&child, 1)
	mut source_type := t.optional_result_expr_type_name(source_id)
	if !t.is_optional_type_name(source_type) {
		source_type = t.original_expr_type(source_id)
	}
	if !t.is_optional_type_name(source_type) {
		source_type = t.raw_expr_type_without_smartcast(source_id)
	}
	mut use_plain_source := false
	if t.expr_has_option_unwrap_smartcast(source_id) {
		mut raw_source_type := t.raw_expr_type_without_smartcast(source_id)
		if raw_source_type.len == 0 {
			raw_source_type = t.original_expr_type(source_id)
		}
		if t.is_optional_type_name(raw_source_type) {
			source_type = raw_source_type
			use_plain_source = true
		}
	}
	if !t.is_optional_type_name(source_type) || !t.expr_can_take_address(source_id) {
		return none
	}
	value_type := t.optional_base_type(t.qualify_optional_type(source_type))
	if value_type.len == 0 || value_type == 'void' {
		return none
	}
	raw_target_type := if node.typ.len > 0 {
		node.typ
	} else {
		'&${value_type}'
	}
	target_type := if t.is_optional_type_name(raw_target_type) {
		t.optional_base_type(t.qualify_optional_type(raw_target_type))
	} else {
		raw_target_type
	}
	if !target_type.starts_with('&') {
		return none
	}
	source := if use_plain_source {
		t.make_plain_expr_for_smartcast(source_id)
	} else {
		t.transform_expr(source_id)
	}
	if use_plain_source {
		t.set_node_typ(int(source), source_type)
	}
	source_actual_type := t.node_type(source)
	if source_actual_type.len > 0 && !t.is_optional_type_name(source_actual_type) {
		payload := if t.is_optional_type_name(source_type) {
			t.make_selector(t.make_plain_expr_for_smartcast(source_id), 'value', value_type)
		} else {
			source
		}
		addr := t.make_prefix(.amp, payload)
		t.set_node_typ(int(addr), target_type)
		return addr
	}
	not_ok := t.make_prefix(.not, t.make_selector(source, 'ok', 'bool'))
	err_expr := t.make_selector(source, 'err', 'IError')
	else_block := t.make_block(t.lower_or_body_to_stmts_with_err_expr(body_id, '', '', child.value,
		err_expr))
	t.pending_stmts << t.make_if(not_ok, else_block, t.make_empty())
	value := t.make_selector(source, 'value', value_type)
	addr := t.make_prefix(.amp, value)
	t.set_node_typ(int(addr), target_type)
	return addr
}

// transform_amp_sum_cast_from_as_expr supports transform_amp_sum_cast_from_as_expr handling.
fn (mut t Transformer) transform_amp_sum_cast_from_as_expr(cast_node flat.Node, cast_arg_id flat.NodeId) ?flat.NodeId {
	target_sum := t.resolve_sum_name(cast_node.value)
	if target_sum.len == 0 || target_sum !in t.sum_types || int(cast_arg_id) < 0 {
		return none
	}
	mut arg_id := cast_arg_id
	for {
		arg0 := t.a.nodes[int(arg_id)]
		if arg0.kind != .paren || arg0.children_count == 0 {
			break
		}
		arg_id = t.a.child(&arg0, 0)
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .as_expr || arg.children_count == 0 || arg.value.len == 0 {
		return none
	}
	source_id := t.a.child(&arg, 0)
	mut source_type := t.node_type(source_id)
	if source_type.len == 0 {
		source_type = t.original_expr_type(source_id)
	}
	mut source_sum := t.resolve_sum_name(t.trim_pointer_type(source_type))
	mut use_plain_source := false
	if source_sum.len == 0 || source_sum !in t.sum_types {
		raw_source_type := t.raw_expr_type_without_smartcast(source_id)
		raw_source_sum := t.resolve_sum_name(t.trim_pointer_type(raw_source_type))
		if raw_source_sum.len > 0 && raw_source_sum in t.sum_types {
			source_type = raw_source_type
			source_sum = raw_source_sum
			use_plain_source = true
		}
	}
	if source_sum.len == 0 || source_sum !in t.sum_types {
		return none
	}
	variant := t.resolve_variant(source_sum, arg.value)
	if variant.len == 0 || !t.variant_references_sum(variant, source_sum) {
		return none
	}
	source := if use_plain_source {
		t.make_plain_expr_for_smartcast(source_id)
	} else {
		t.transform_expr(source_id)
	}
	field_name := t.sum_field_name(variant)
	field_sel := t.make_selector_op(source, field_name, '&${variant}', if source_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.make_cast('&${cast_node.value}', field_sel, '&${cast_node.value}')
}

// raw_expr_type_without_smartcast
// supports helper handling in transform.
fn (t &Transformer) raw_expr_type_without_smartcast(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			typ := t.normalize_type_alias(t.var_type(node.value))
			if typ.len > 0 {
				return typ
			}
			return t.normalize_type_alias(node.typ)
		}
		.selector {
			return t.raw_selector_type_without_smartcast(id)
		}
		else {
			return t.normalize_type_alias(node.typ)
		}
	}
}

// raw_selector_type_without_smartcast supports raw_selector_type_without_smartcast handling.
fn (t &Transformer) raw_selector_type_without_smartcast(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.raw_expr_type_without_smartcast(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	clean_base_type := t.trim_pointer_type(base_type)
	if ftyp := t.lookup_struct_field_type(clean_base_type, node.value) {
		return ftyp
	}
	return t.normalize_type_alias(node.typ)
}

// selector_chain_has_sum_shared_field supports selector_chain_has_sum_shared_field handling.
fn (t &Transformer) selector_chain_has_sum_shared_field(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	if _ := t.sum_shared_field_type_name(base_type, node.value) {
		return true
	}
	return t.selector_chain_has_sum_shared_field(base_id)
}

// selector_chain_has_sum_variant_field supports selector_chain_has_sum_variant_field handling.
fn (t &Transformer) selector_chain_has_sum_variant_field(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	if t.sum_has_variant_field(base_type, node.value) {
		return true
	}
	return t.selector_chain_has_sum_variant_field(base_id)
}

// sum_has_variant_field converts sum has variant field data for transform.
fn (t &Transformer) sum_has_variant_field(sum_type string, field string) bool {
	clean_sum := if sum_type.starts_with('&') { sum_type[1..] } else { sum_type }
	resolved_sum := t.resolve_sum_name(clean_sum)
	variants := t.sum_types[resolved_sum] or { return false }
	for variant in variants {
		if _ := t.sum_variant_field_type_name(variant, field) {
			return true
		}
	}
	return false
}

// transform_paren_expr transforms transform paren expr data for transform.
fn (mut t Transformer) transform_paren_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	child_id := t.a.child(&node, 0)
	new_child := if t.expected_expr_node == int(id) && t.expected_expr_type.len > 0 {
		t.transform_expr_for_type(child_id, t.expected_expr_type)
	} else {
		t.transform_expr(child_id)
	}
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .paren
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_postfix_expr transforms transform postfix expr data for transform.
fn (mut t Transformer) transform_postfix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if node.op == .not && t.expr_has_option_unwrap_smartcast(child_id) {
		return t.transform_expr(child_id)
	}
	if node.op == .not && child.kind == .array_literal {
		mut node_type := t.node_type(id)
		checker_type := t.checker_node_type(id)
		if t.is_fixed_array_type(checker_type) {
			node_type = checker_type
		}
		if node_type.len == 0 || node_type == 'void' || node_type == 'unknown' {
			node_type = checker_type
		}
		if !t.is_fixed_array_type(node_type) {
			mut elem_type := t.checker_node_type(child_id)
			if elem_type.starts_with('[]') {
				elem_type = elem_type[2..]
			}
			if elem_type.len == 0 || elem_type in ['array', 'unknown'] {
				elem_type = if child.children_count > 0 {
					t.node_type(t.a.child(&child, 0))
				} else {
					'int'
				}
			}
			node_type = '[${child.children_count}]${elem_type}'
		}
		if lowered := t.transform_fixed_array_literal_for_type(child_id, child, node_type) {
			return lowered
		}
	}
	new_child := if child.kind == .ident && t.pointer_value_lvalues[child.value] {
		t.make_paren(t.make_prefix(.mul, t.make_ident(child.value)))
	} else {
		t.transform_expr(child_id)
	}
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .postfix
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_cast_expr transforms transform cast expr data for transform.
fn (mut t Transformer) transform_cast_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	target_type := t.normalize_type_alias(node.value)
	if target_type.starts_with('&') && !t.is_interface_type(target_type) {
		mut new_children := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			new_children << t.transform_expr(child_id)
		}
		start := t.a.children.len
		for nc in new_children {
			t.a.children << nc
		}
		return t.a.add_node(flat.Node{
			kind:           .cast_expr
			op:             node.op
			children_start: start
			children_count: node.children_count
			pos:            node.pos
			value:          node.value
			typ:            node.typ
		})
	}
	if t.is_optional_type_name(node.value) {
		child_id := t.a.child(&node, 0)
		expr := t.transform_expr_for_type(child_id, node.value)
		mut expr_type := t.node_type(expr)
		if expr_type.len == 0 {
			expr_type = t.resolve_expr_type(child_id)
		}
		if t.is_optional_type_name(expr_type) {
			return t.coerce_transformed_expr_to_type(expr, child_id, node.value)
		}
		return t.make_optional_some(expr, t.qualify_optional_type(node.value))
	}
	if t.is_sum_type_name(target_type) {
		return t.wrap_sum_value(t.a.child(&node, 0), target_type)
	}
	// An explicit cast to an interface (`Animal(dog)`, `&PRNG(rng)`) boxes the
	// concrete value into the interface representation, just like an implicit
	// conversion does.
	if t.is_interface_type(target_type) {
		if boxed := t.transform_interface_value_for_type(t.a.child(&node, 0), node.value, false) {
			return boxed
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if target_type in ['f32', 'f64'] {
			new_children << t.transform_expr_for_type(child_id, target_type)
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_array_literal transforms transform array literal data for transform.
fn (mut t Transformer) transform_array_literal(id flat.NodeId, node flat.Node) flat.NodeId {
	lowered := t.lower_array_literal_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .array_literal
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_map_init transforms transform map init data for transform.
fn (mut t Transformer) transform_map_init(id flat.NodeId, node flat.Node) flat.NodeId {
	return t.transform_map_init_expr(id, node)
}

// transform_typeof_expr transforms transform typeof expr data for transform.
// typeof_fn_type_display normalizes a fn type name for `typeof(...).name` output the way
// the reference compiler renders it: parameter names are dropped and `fn` is separated from
// the parameter list by one space, e.g. `fn(s string, x u32) (int, f32)` becomes
// `fn (string, u32) (int, f32)`.
fn typeof_fn_type_display(typ string) string {
	clean := typ.trim_space()
	if !clean.starts_with('fn(') && !clean.starts_with('fn (') {
		return typ
	}
	open := clean.index_u8(`(`)
	mut depth := 0
	mut close := -1
	for i in open .. clean.len {
		c := clean[i]
		if c == `(` || c == `[` {
			depth++
		} else if c == `)` || c == `]` {
			depth--
			if depth == 0 && c == `)` {
				close = i
				break
			}
		}
	}
	if close < 0 {
		return typ
	}
	params_text := clean[open + 1..close]
	rest := clean[close + 1..]
	mut parts := []string{}
	mut start := 0
	mut d := 0
	for i in 0 .. params_text.len {
		c := params_text[i]
		if c == `(` || c == `[` {
			d++
		} else if c == `)` || c == `]` {
			d--
		} else if c == `,` && d == 0 {
			parts << params_text[start..i]
			start = i + 1
		}
	}
	parts << params_text[start..]
	mut cleaned := []string{cap: parts.len}
	for p0 in parts {
		p := p0.trim_space()
		if p.len == 0 {
			continue
		}
		mut prefix := ''
		mut body := p
		for body.starts_with('mut ') || body.starts_with('shared ') {
			word := body.all_before(' ')
			prefix += word + ' '
			body = body.all_after(' ').trim_space()
		}
		if space := body.index(' ') {
			name := body[..space]
			if typeof_display_is_param_name(name) {
				body = body[space + 1..].trim_space()
			}
		}
		cleaned << prefix + body
	}
	return 'fn (' + cleaned.join(', ') + ')' + rest
}

fn typeof_display_is_param_name(name string) bool {
	if name.len == 0 || name in ['fn', 'chan', 'map', 'thread', 'atomic', 'struct'] {
		return false
	}
	if !(name[0].is_letter() || name[0] == `_`) {
		return false
	}
	for c in name {
		if !(c.is_letter() || c.is_digit() || c == `_`) {
			return false
		}
	}
	return true
}

fn (mut t Transformer) transform_typeof_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len > 0 {
		return t.make_string_literal(typeof_fn_type_display(node.value))
	}
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .selector && expr.children_count > 0
		&& expr.value in ['key_type', 'value_type', 'element_type'] {
		base := t.a.child_node(&expr, 0)
		if base.kind == .typeof_expr {
			base_type := t.typeof_type_name(base)
			if member_type := t.generic_comptime_type_member(base_type, expr.value) {
				return t.make_string_literal(generic_type_name_display(member_type))
			}
		}
	}
	if expr.kind == .int_literal {
		return t.make_string_literal('int literal')
	}
	mut typ := ''
	if sc := t.find_smartcast(t.expr_key(expr_id)) {
		typ = t.resolve_variant(sc.sum_type_name, sc.variant_name)
	}
	if expr.kind == .ident {
		if typ.len == 0 {
			typ = t.raw_var_type(expr.value)
		}
	}
	if typ.len == 0 {
		typ = t.node_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.reliable_stringify_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.resolve_expr_type(expr_id)
	}
	if typ.len == 0 {
		typ = 'unknown'
	}
	if t.cur_fn_is_generic && is_generic_fn_placeholder_name(typ) {
		return t.a.add_node(flat.Node{
			kind:  .typeof_expr
			value: generic_type_name_marker(typ)
			typ:   'string'
			pos:   node.pos
		})
	}
	if !isnil(t.tc) {
		resolved := t.tc.resolve_type(expr_id)
		if resolved is types.ArrayFixed {
			return t.make_string_literal(typeof_display_resolved_type_text(resolved))
		}
	}
	return t.make_string_literal(typeof_display_type_text(typeof_fn_type_display(generic_type_name_display(typ))))
}

fn typeof_display_resolved_type_text(typ types.Type) string {
	if typ is types.ArrayFixed {
		len_text := if typ.len_expr.len > 0 { typ.len_expr } else { typ.len.str() }
		return '[${len_text}]' + typeof_display_type_text(typ.elem_type.name())
	}
	return typeof_display_type_text(typ.name())
}

// typeof_display_type_text canonicalizes internal suffix-form fixed-array
// texts (`[]int[3]`) back to V syntax (`[][3]int`) for `typeof(x).name`.
fn typeof_display_type_text(name string) string {
	if name.starts_with('[]') {
		return '[]' + typeof_display_type_text(name[2..])
	}
	if name.starts_with('&') {
		return '&' + typeof_display_type_text(name[1..])
	}
	if name.starts_with('?') || name.starts_with('!') {
		return name[..1] + typeof_display_type_text(name[1..])
	}
	if name.starts_with('mut ') {
		return 'mut ' + typeof_display_type_text(name[4..])
	}
	if name.starts_with('shared ') {
		return 'shared ' + typeof_display_type_text(name[7..])
	}
	if name.starts_with('chan ') {
		return 'chan ' + typeof_display_type_text(name[5..])
	}
	if name.starts_with('map[') {
		close := typeof_display_matching_bracket(name, 3)
		if close > 3 && close < name.len - 1 {
			key := typeof_display_type_text(name[4..close])
			value := typeof_display_type_text(name[close + 1..])
			return 'map[${key}]${value}'
		}
	}
	if name.starts_with('fn(') || name.starts_with('fn (') {
		return typeof_display_fn_type_text(name)
	}
	if name.ends_with(']') && !name.starts_with('[') && !name.starts_with('map[') {
		outer_open := name.index_u8(`[`)
		if outer_open > 0 && typeof_display_matching_bracket(name, outer_open) == name.len - 1 {
			args_text := name[outer_open + 1..name.len - 1]
			if !typeof_display_is_fixed_array_len_text(args_text) {
				return name[..outer_open] + '[' + typeof_display_type_list(args_text) + ']'
			}
		}
		if open_idx := name.last_index('[') {
			if open_idx > 0 {
				len_text := name[open_idx + 1..name.len - 1]
				if typeof_display_is_fixed_array_len_text(len_text) {
					return '[${len_text}]' + typeof_display_type_text(name[..open_idx])
				}
			}
		}
	}
	return name
}

// typeof_display_is_fixed_array_len_text distinguishes a suffix-form fixed-array
// length from the type argument of a generic application.
fn typeof_display_is_fixed_array_len_text(text string) bool {
	clean := text.trim_space()
	if clean.len == 0 || clean.contains(',') || clean.contains('[') || clean.contains(']') {
		return false
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') || clean.starts_with('chan ')
		|| clean.starts_with('shared ') || clean.starts_with('atomic ') || clean.starts_with('mut ')
		|| clean.starts_with('thread ') {
		return false
	}
	if is_decimal_text(clean) || (clean[0] >= `0` && clean[0] <= `9`) {
		return true
	}
	if clean[0] == `(` && clean.ends_with(')') {
		return typeof_display_is_fixed_array_len_text(clean[1..clean.len - 1])
	}
	if types.is_builtin_type_name(clean) || is_generic_fn_placeholder_name(clean) {
		return false
	}
	for i, ch in clean {
		if ch in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`] || ((ch == `-` || ch == `&`) && i > 0) {
			return true
		}
	}
	name := clean.all_after_last('.')
	return name.len > 0 && name[0] >= `a` && name[0] <= `z`
}

fn typeof_display_fn_type_text(name string) string {
	clean := name.trim_space()
	open := clean.index_u8(`(`)
	close := typeof_display_matching_paren(clean, open)
	if close < 0 {
		return name
	}
	params := typeof_display_type_list(clean[open + 1..close])
	mut result := 'fn (${params})'
	ret := clean[close + 1..].trim_space()
	if ret.len == 0 {
		return result
	}
	if ret.starts_with('(') {
		ret_close := typeof_display_matching_paren(ret, 0)
		if ret_close == ret.len - 1 {
			return result + ' (' + typeof_display_type_list(ret[1..ret_close]) + ')'
		}
	}
	result += ' ' + typeof_display_type_text(ret)
	return result
}

fn typeof_display_matching_paren(text string, open int) int {
	if open < 0 || open >= text.len || text[open] != `(` {
		return -1
	}
	mut depth := 0
	for i in open .. text.len {
		if text[i] == `(` {
			depth++
		} else if text[i] == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn typeof_display_matching_bracket(text string, open int) int {
	if open < 0 || open >= text.len || text[open] != `[` {
		return -1
	}
	mut depth := 0
	for i in open .. text.len {
		if text[i] == `[` {
			depth++
		} else if text[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn typeof_display_type_list(text string) string {
	mut parts := []string{}
	mut start := 0
	mut paren_depth := 0
	mut bracket_depth := 0
	for i in 0 .. text.len {
		match text[i] {
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
			}
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			`,` {
				if paren_depth == 0 && bracket_depth == 0 {
					parts << typeof_display_type_text(text[start..i].trim_space())
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < text.len {
		parts << typeof_display_type_text(text[start..].trim_space())
	}
	return parts.join(', ')
}

fn (mut t Transformer) transform_typeof_idx_expr(node flat.Node) flat.NodeId {
	type_name := t.typeof_type_name(node)
	return t.make_int_literal(t.comptime_field_type_id(type_name, t.cur_module))
}

fn (t &Transformer) typeof_type_name(node flat.Node) string {
	if node.value.len > 0 {
		return node.value
	}
	if node.children_count == 0 {
		return ''
	}
	expr_id := t.a.child(&node, 0)
	expr := t.a.nodes[int(expr_id)]
	mut typ := ''
	if sc := t.find_smartcast(t.expr_key(expr_id)) {
		typ = t.resolve_variant(sc.sum_type_name, sc.variant_name)
	}
	if expr.kind == .ident {
		if typ.len == 0 {
			typ = t.raw_var_type(expr.value)
		}
	}
	if typ.len == 0 {
		typ = t.node_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.reliable_stringify_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.resolve_expr_type(expr_id)
	}
	return typ
}

fn (t &Transformer) type_index_for_type_name(type_name string) int {
	if type_name.len == 0 {
		return 0
	}
	// Builtin types keep V's stable ast `*_type_idx` values (int==8, string==21, ...), so
	// `typeof[T]().idx` comparisons against `v.ast` constants behave like the reference
	// compiler.
	builtin_idx := comptime_builtin_type_idx(type_name)
	if builtin_idx > 0 {
		return builtin_idx
	}
	mut variants := []string{cap: 2}
	variants << type_name
	normalized := t.normalize_type_in_module(type_name, t.cur_module)
	if normalized.len > 0 && normalized !in variants {
		variants << normalized
	}
	mut sum_names := []string{}
	if t.cur_module.len > 0 {
		sum_names << '${t.cur_module}.Primitive'
	}
	sum_names << 'orm.Primitive'
	sum_names << 'Primitive'
	for sum_name in sum_names {
		if sum_name !in t.sum_types {
			continue
		}
		for variant in variants {
			idx := t.sum_type_index(sum_name, variant)
			if idx != 0 {
				return idx
			}
		}
	}
	for variant in variants {
		sum_name := t.find_sum_type_for_variant(variant)
		if sum_name.len > 0 {
			idx := t.sum_type_index(sum_name, variant)
			if idx != 0 {
				return idx
			}
		}
	}
	return 0
}

// transform_ident_expr transforms transform ident expr data for transform.
fn (mut t Transformer) transform_ident_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	match node.value {
		'@VMODROOT' {
			return t.make_string_literal(t.vmod_root())
		}
		else {
			if smartcasted := t.smartcast_ident_value(node.value) {
				return smartcasted
			}
			// Idents are the most common node; re-annotating them in place (rather than
			// allocating a fresh node) avoids cascading rebuilds of every enclosing
			// expression and the associated allocations (critical under -gc none).
			if !t.in_call_callee {
				if fn_name := t.resolve_fn_value_ident(node.value) {
					t.set_node_value(int(id), fn_name)
					return id
				}
			}
			typ := t.var_type(node.value)
			if typ.len > 0 && typ != node.typ {
				t.set_node_typ(int(id), typ)
			}
			if !t.in_selector_base && t.pointer_value_rvalues[node.value] && typ.starts_with('&') {
				deref := t.make_prefix(.mul, id)
				t.set_node_typ(int(deref), typ[1..])
				return deref
			}
			if !t.in_selector_base && t.pointer_value_rvalues[node.value] && typ.starts_with('&') {
				deref := t.make_prefix(.mul, id)
				t.a.nodes[int(deref)].typ = typ[1..]
				return deref
			}
			return id
		}
	}
}

// smartcast_ident_value supports smartcast ident value handling for Transformer.
fn (mut t Transformer) smartcast_ident_value(name string) ?flat.NodeId {
	if t.smartcast_stack.len == 0 {
		return none
	}
	contexts := t.smartcasts_for(name)
	if contexts.len == 0 {
		return none
	}
	return t.apply_smartcast_contexts(t.make_ident(name), t.var_type(name), contexts)
}

// apply_smartcast_contexts supports apply smartcast contexts handling for Transformer.
fn (mut t Transformer) apply_smartcast_contexts(base flat.NodeId, typ string, contexts []SmartcastContext) flat.NodeId {
	mut current := base
	mut current_type := typ
	for i, sc in contexts {
		if sc.sum_type_name == option_unwrap_marker {
			// current is the Optional_T struct value itself. Type annotations on
			// the rebuilt expr may already report the smartcast base type (which
			// can be a pointer and would make cgen emit `->`), so pin the node's
			// type back to the option before selecting `.value` from it.
			if int(current) >= 0 && t.a.nodes[int(current)].kind in [.ident, .selector] {
				t.a.nodes[int(current)].typ = '?${sc.variant_name}'
			}
			field_op := if current_type.starts_with('&?') { flat.Op.arrow } else { flat.Op.dot }
			current = t.make_selector_op(current, 'value', sc.variant_name, field_op)
			current_type = sc.variant_name
			continue
		}
		if t.is_interface_type_name(sc.sum_type_name) {
			qv := t.interface_variant_type(sc.variant_name)
			field_op := if current_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
			object := t.make_selector_op(current, '_object', 'voidptr', field_op)
			cast := t.make_cast('&${qv}', object, '&${qv}')
			current = t.make_prefix(.mul, cast)
			t.set_node_typ(int(current), qv)
			current_type = qv
			continue
		}
		mut path := t.sum_variant_path(sc.sum_type_name, sc.variant_name)
		if path.len == 0 {
			path = [t.resolve_variant(sc.sum_type_name, sc.variant_name)]
		}
		mut current_sum := sc.sum_type_name
		for j, qv in path {
			if t.expr_is_variant_access(current, qv) {
				current_type = qv
				current_sum = qv
				continue
			}
			field := t.sum_field_name(qv)
			use_ptr := t.variant_references_sum(qv, current_sum)
			field_typ := if use_ptr { '&${qv}' } else { qv }
			field_op := if current_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
			field_sel := t.make_selector_op(current, field, field_typ, field_op)
			if use_ptr && i == contexts.len - 1 && j == path.len - 1 {
				current = t.make_prefix(.mul, field_sel)
				t.set_node_typ(int(current), qv)
				current_type = qv
			} else {
				current = field_sel
				current_type = field_typ
			}
			current_sum = qv
		}
	}
	return current
}

// expr_is_variant_access supports expr is variant access handling for Transformer.
fn (t &Transformer) expr_is_variant_access(id flat.NodeId, variant string) bool {
	if int(id) < 0 || variant.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	field := t.sum_field_name(variant)
	match node.kind {
		.selector {
			return node.value == field
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				return t.expr_is_variant_access(t.a.child(&node, 0), variant)
			}
			return false
		}
		.paren {
			if node.children_count > 0 {
				return t.expr_is_variant_access(t.a.child(&node, 0), variant)
			}
			return false
		}
		else {
			return false
		}
	}
}

// is_sum_variant_field_name reports whether is sum variant field name applies in transform.
fn (t &Transformer) is_sum_variant_field_name(name string) bool {
	return name in t.sum_variant_fields
}

// variant_type_from_sum_field_name converts variant type from sum field name data for transform.
fn (t &Transformer) variant_type_from_sum_field_name(name string) ?string {
	if variant := t.sum_variant_fields[name] {
		return variant
	}
	return none
}

// generated_variant_access_type supports generated variant access type handling for Transformer.
fn (t &Transformer) generated_variant_access_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.selector {
			variant := t.variant_type_from_sum_field_name(node.value) or { return none }
			if node.typ.starts_with('&') {
				return node.typ
			}
			return variant
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				variant := t.generated_variant_access_type(t.a.child(&node, 0)) or { return none }
				return t.trim_pointer_type(variant)
			}
			return none
		}
		.paren {
			if node.children_count > 0 {
				return t.generated_variant_access_type(t.a.child(&node, 0))
			}
			return none
		}
		else {
			return none
		}
	}
}

// original_expr_type supports original expr type handling for Transformer.
fn (t &Transformer) original_expr_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			typ := t.normalize_type_alias(t.var_type(node.value))
			if typ.len > 0 {
				return typ
			}
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return ''
		}
		.selector {
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return t.resolve_selector_type(node)
		}
		else {
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return t.resolve_expr_type(id)
		}
	}
}

// smartcasts_for supports smartcasts for handling for Transformer.
fn (t &Transformer) smartcasts_for(expr_name string) []SmartcastContext {
	if expr_name.len == 0 || t.smartcast_stack.len == 0 {
		return []SmartcastContext{}
	}
	mut result := []SmartcastContext{cap: 1}
	for sc in t.smartcast_stack {
		if sc.expr_name == expr_name {
			result << sc
		}
	}
	return result
}

// has_smartcast reports whether has smartcast applies in transform.
fn (t &Transformer) has_smartcast(expr_name string) bool {
	if expr_name.len == 0 || t.smartcast_stack.len == 0 {
		return false
	}
	for sc in t.smartcast_stack {
		if sc.expr_name == expr_name {
			return true
		}
	}
	return false
}

// resolve_fn_value_ident resolves resolve fn value ident information for transform.
fn (t &Transformer) resolve_fn_value_ident(name string) ?string {
	if name.len == 0 || name.contains('.') || t.var_type(name).len > 0 {
		return none
	}
	mut candidates := []string{}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${name}'
	}
	candidates << name
	for candidate in candidates {
		if candidate in t.fn_ret_types {
			return candidate
		}
		if !isnil(t.tc) && (candidate in t.tc.fn_ret_types || candidate in t.tc.fn_param_types) {
			return candidate
		}
	}
	return none
}

// --- helper methods ---

// new_temp supports new temp handling for Transformer.
pub fn (mut t Transformer) new_temp(prefix string) string {
	name := '__${prefix}_${t.temp_counter}'
	t.temp_counter++
	return name
}

// make_ident builds make ident data for transform.
pub fn (mut t Transformer) make_ident(name string) flat.NodeId {
	id := t.a.add_val(.ident, name)
	typ := t.var_type(name)
	if typ.len > 0 {
		t.set_node_typ(int(id), typ)
	}
	return id
}

// make_decl_assign builds make decl assign data for transform.
pub fn (mut t Transformer) make_decl_assign(name string, rhs flat.NodeId) flat.NodeId {
	lhs := t.make_ident(name)
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .decl_assign
		children_start: start
		children_count: 2
	})
}

// make_expr_stmt builds make expr stmt data for transform.
pub fn (mut t Transformer) make_expr_stmt(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: start
		children_count: 1
	})
}

// make_assign builds make assign data for transform.
pub fn (mut t Transformer) make_assign(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	return t.make_assign_op(lhs, rhs, .assign)
}

// make_assign_op builds make assign op data for transform.
pub fn (mut t Transformer) make_assign_op(lhs flat.NodeId, rhs flat.NodeId, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .assign
		op:             op
		children_start: start
		children_count: 2
	})
}

// make_block builds make block data for transform.
pub fn (mut t Transformer) make_block(stmts []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for id in stmts {
		t.a.children << id
	}
	return t.a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: flat.child_count(stmts.len)
	})
}

// make_infix builds make infix data for transform.
pub fn (mut t Transformer) make_infix(op flat.Op, lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             op
		children_start: start
		children_count: 2
	})
}

// make_prefix builds make prefix data for transform.
pub fn (mut t Transformer) make_prefix(op flat.Op, expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             op
		children_start: start
		children_count: 1
	})
}

// make_paren builds make paren data for transform.
pub fn (mut t Transformer) make_paren(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .paren
		children_start: start
		children_count: 1
	})
}

// make_if builds make if data for transform.
pub fn (mut t Transformer) make_if(cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	if int(else_block) >= 0 {
		t.a.children << else_block
		return t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: start
			children_count: 3
		})
	}
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 2
	})
}

// push_smartcast updates push smartcast state for Transformer.
pub fn (mut t Transformer) push_smartcast(expr_name string, variant string, sum_type string) {
	t.invalidated_smartcasts.delete(expr_name)
	t.smartcast_stack << SmartcastContext{
		expr_name:     expr_name
		variant_name:  variant
		sum_type_name: sum_type
	}
}

// pop_smartcast updates pop smartcast state for Transformer.
pub fn (mut t Transformer) pop_smartcast() {
	if t.smartcast_stack.len > 0 {
		t.smartcast_stack.delete_last()
	}
}

// find_smartcast resolves find smartcast information for transform.
pub fn (t &Transformer) find_smartcast(expr_name string) ?SmartcastContext {
	// Search from top of stack (most recent) to bottom
	mut i := t.smartcast_stack.len - 1
	for i >= 0 {
		if t.smartcast_stack[i].expr_name == expr_name {
			return t.smartcast_stack[i]
		}
		i--
	}
	return none
}

// expr_key supports expr key handling for Transformer.
fn (t &Transformer) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count >= 1 {
		base_id := t.a.child(&node, 0)
		base_key := t.expr_key(base_id)
		if base_key.len > 0 {
			return '${base_key}.${node.value}'
		}
	}
	if node.kind == .index && node.children_count >= 2 {
		base_key := t.expr_key(t.a.child(&node, 0))
		index_key := t.expr_key_part(t.a.child(&node, 1))
		if base_key.len > 0 && index_key.len > 0 {
			return '${base_key}[${index_key}]'
		}
	}
	if node.kind in [.as_expr, .paren] && node.children_count >= 1 {
		return t.expr_key(t.a.child(&node, 0))
	}
	return ''
}

// expr_key_part supports expr key part handling for Transformer.
fn (t &Transformer) expr_key_part(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.int_literal, .string_literal, .char_literal, .enum_val {
			return node.value
		}
		else {
			return t.expr_key(id)
		}
	}
}

// qualify_variant supports qualify variant handling for Transformer.
fn (t &Transformer) qualify_variant(variant string, sum_type_name string) string {
	if variant.contains('.') {
		return variant
	}
	resolved_sum := t.resolve_sum_name(sum_type_name)
	if resolved_variant := t.sum_variant_name(resolved_sum, variant) {
		return resolved_variant
	}
	if sum_type_name.contains('.') {
		mod := sum_type_name.all_before_last('.')
		return '${mod}.${variant}'
	}
	return variant
}

// sum_variant_name supports sum variant name handling for Transformer.
fn (t &Transformer) sum_variant_name(sum_name string, variant string) ?string {
	for v in t.concrete_sum_variants_for_candidate(sum_name) {
		if t.variant_names_match(v, variant) {
			return v
		}
	}
	resolved_sum := t.resolve_sum_name(sum_name)
	variants := t.sum_types[resolved_sum] or { return none }
	for v in variants {
		if t.variant_names_match(v, variant) {
			return v
		}
	}
	return none
}

fn (t &Transformer) variant_names_match(a string, b string) bool {
	if a == b || t.variant_short_name(a) == t.variant_short_name(b) {
		return true
	}
	a_base, a_args, a_generic := generic_app_parts(a)
	b_base, b_args, b_generic := generic_app_parts(b)
	if a_generic || b_generic {
		a_match_base := if a_generic { a_base } else { a }
		b_match_base := if b_generic { b_base } else { b }
		if a_generic && b_generic && !t.generic_variant_args_are_open(a_args)
			&& !t.generic_variant_args_are_open(b_args) {
			return false
		}
		return t.variant_short_name(a_match_base) == t.variant_short_name(b_match_base)
	}
	return false
}

fn (t &Transformer) generic_variant_args_are_open(args []string) bool {
	for arg in args {
		if t.generic_arg_is_unresolved(arg) {
			return true
		}
	}
	return false
}

fn (t &Transformer) variant_short_name(name string) string {
	if isnil(t.variant_short_name_cache) {
		return variant_short_name_text(name)
	}
	mut cache := t.variant_short_name_cache
	if cached := cache.entries[name] {
		return cached
	}
	short := variant_short_name_text(name)
	cache.entries[name] = short
	return short
}

fn generic_base_name_text(name string) string {
	if name.starts_with('[') {
		return name
	}
	bracket := name.index_u8(`[`)
	if bracket <= 0 {
		return name
	}
	return name[..bracket]
}

fn variant_short_name_text(name string) string {
	if name.starts_with('&') {
		return '&' + variant_short_name_text(name[1..])
	}
	if name.starts_with('[]') {
		return '[]' + variant_short_name_text(name[2..])
	}
	if name.starts_with('map[') {
		bracket_end := name.index(']') or { return name }
		key := name[4..bracket_end]
		value := name[bracket_end + 1..]
		return 'map[${variant_short_name_text(key)}]${variant_short_name_text(value)}'
	}
	return if name.contains('.') { name.all_after_last('.') } else { name }
}

// sum_field_name supports sum field name handling for Transformer.
fn (t &Transformer) sum_field_name(variant string) string {
	if variant.starts_with('&') {
		return t.sum_field_name(variant[1..])
	}
	if variant.starts_with('?') {
		return '_Option_${c_name(variant[1..])}'
	}
	if variant.starts_with('!') {
		return '_Result_${c_name(variant[1..])}'
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('.') {
		return t.sum_field_name(variant[3..])
	}
	if variant.starts_with('ptr') && variant.len > 3 && variant[3..].contains('__') {
		return t.sum_field_name(variant[3..].replace('__', '.'))
	}
	if variant.starts_with('[]') {
		return '_Array_${c_name(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${c_name(variant[4..].replace(']', '_'))}'
	}
	return match variant {
		'int' { '_int' }
		'i8' { '_i8' }
		'i16' { '_i16' }
		'i64' { '_i64' }
		'u8', 'byte' { '_u8' }
		'u16' { '_u16' }
		'u32' { '_u32' }
		'u64' { '_u64' }
		'f32' { '_f32' }
		'f64' { '_f64' }
		'bool' { '_bool' }
		'string' { '_string' }
		else { c_name(variant) }
	}
}

// variant_references_sum supports variant references sum handling for Transformer.
fn (t &Transformer) variant_references_sum(variant string, sum_name string) bool {
	_ = t
	_ = variant
	_ = sum_name
	return true
}

// tc_variant_refs_sum_inner supports tc variant refs sum inner handling for Transformer.
fn (t &Transformer) tc_variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	if variant == sum_name || variant.all_after_last('.') == sum_name.all_after_last('.') {
		return true
	}
	if variant in visited {
		return false
	}
	visited[variant] = true
	mut lookup := variant
	if lookup !in t.tc.structs && !lookup.contains('.') && sum_name.contains('.') {
		qlookup := '${sum_name.all_before_last('.')}.${lookup}'
		if qlookup in t.tc.structs {
			lookup = qlookup
		}
	}
	if lookup !in t.tc.structs && lookup.contains('.') {
		short := lookup.all_after_last('.')
		if short in t.tc.structs {
			lookup = short
		}
	}
	if lookup in t.tc.structs {
		for f in t.tc.structs[lookup] {
			if t.tc_type_references_sum(f.typ, sum_name, mut visited) {
				return true
			}
		}
	}
	return false
}

// tc_type_references_sum supports tc type references sum handling for Transformer.
fn (t &Transformer) tc_type_references_sum(typ types.Type, sum_name string, mut visited map[string]bool) bool {
	clean := types.unwrap_pointer(typ)
	if clean is types.Struct && clean.name == sum_name {
		return true
	}
	if clean is types.SumType && clean.name == sum_name {
		return true
	}
	if clean is types.SumType {
		return true
	}
	if clean is types.Struct {
		if t.tc_variant_refs_sum_inner(clean.name, sum_name, mut visited) {
			return true
		}
	}
	if clean is types.Array {
		return t.tc_type_references_sum(clean.elem_type, sum_name, mut visited)
	}
	return false
}

// variant_refs_sum_inner supports variant refs sum inner handling for Transformer.
fn (t &Transformer) variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	short_v := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	short_s := if sum_name.contains('.') { sum_name.all_after_last('.') } else { sum_name }
	if short_v == short_s {
		return true
	}
	if variant in visited {
		return false
	}
	visited[variant] = true
	qualified := if sum_name.contains('.') && !variant.contains('.') {
		'${sum_name.all_before_last('.')}.${variant}'
	} else {
		variant
	}
	lookup := if qualified in t.structs {
		qualified
	} else if variant in t.structs {
		variant
	} else {
		short_v
	}
	if lookup in t.structs {
		for f in t.structs[lookup].fields {
			if f.typ.starts_with('&') || f.typ.starts_with('[]') {
				continue
			}
			ftyp := f.typ
			short_f := if ftyp.contains('.') { ftyp.all_after_last('.') } else { ftyp }
			if ftyp == sum_name || short_f == short_s {
				return true
			}
			qftyp := if sum_name.contains('.') && !ftyp.contains('.') {
				'${sum_name.all_before_last('.')}.${ftyp}'
			} else {
				ftyp
			}
			if qftyp in t.sum_types {
				return true
			}
			if ftyp in t.structs || short_f in t.structs || qftyp in t.structs {
				inner_lookup := if ftyp in t.structs {
					ftyp
				} else if short_f in t.structs {
					short_f
				} else {
					qftyp
				}
				if t.variant_refs_sum_inner(inner_lookup, sum_name, mut visited) {
					return true
				}
			}
		}
	}
	return false
}

// drain_pending supports drain pending handling for Transformer.
pub fn (mut t Transformer) drain_pending(mut result []flat.NodeId) {
	for id in t.pending_stmts {
		result << id
	}
	t.pending_stmts.clear()
}

// with_pending_before supports with pending before handling for Transformer.
fn (mut t Transformer) with_pending_before(stmt flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << stmt
	return result
}

// is_stmt_kind_id reports whether is stmt kind id applies in transform.
fn (t &Transformer) is_stmt_kind_id(kind_id int) bool {
	return kind_id == 39 || kind_id == 40 || kind_id == 41 || kind_id == 42 || kind_id == 43
		|| kind_id == 44 || kind_id == 45 || kind_id == 46 || kind_id == 47 || kind_id == 48
		|| kind_id == 49 || kind_id == 50 || kind_id == 52 || kind_id == 53 || kind_id == 54
		|| kind_id == 55 || kind_id == 15 || kind_id == 56 || kind_id == 57 || kind_id == 60
}

// is_stmt_kind reports whether is stmt kind applies in transform.
fn (t &Transformer) is_stmt_kind(kind flat.NodeKind) bool {
	return t.is_stmt_kind_id(int(kind))
}

// --- type resolution helpers (will move to types.v later) ---

// infer_decl_type resolves infer decl type information for transform.
fn (t &Transformer) infer_decl_type(node &flat.Node) string {
	if node.children_count >= 2 {
		rhs_id := t.a.child(node, 1)
		rhs := t.a.nodes[int(rhs_id)]
		if rhs.kind == .infix && rhs.op == .right_shift_unsigned {
			rhs_type := t.decl_rhs_type(rhs_id)
			if decl_type_is_usable(rhs_type) {
				return rhs_type
			}
		}
	}
	if decl_type_is_usable(node.typ) {
		return node.typ
	}
	if node.children_count >= 2 {
		rhs_id := t.a.child(node, 1)
		return t.decl_rhs_type(rhs_id)
	}
	return ''
}

fn (t &Transformer) raw_decl_type_for_rhs(rhs flat.Node, fallback string) string {
	if rhs.kind == .struct_init && rhs.value.len > 0 && !isnil(t.tc) {
		if rhs.value in t.tc.type_aliases {
			return rhs.value
		}
		if !rhs.value.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${rhs.value}'
			if qname in t.tc.type_aliases {
				return qname
			}
		}
	}
	return fallback
}

// resolve_expr_type resolves resolve expr type information for transform.
fn (t &Transformer) resolve_expr_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if sc := t.find_smartcast(node.value) {
				return t.smartcast_target_type(sc)
			}
			local_type := t.normalize_type_alias(t.var_type(node.value))
			if local_type.len > 0 {
				return local_type
			}
			if global_type := t.globals[node.value] {
				return t.normalize_type_alias(global_type)
			}
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qglobal := '${t.cur_module}.${node.value}'
				if global_type := t.globals[qglobal] {
					return t.normalize_type_alias(global_type)
				}
			}
			if !isnil(t.tc) {
				if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
					qname := '${t.cur_module}.${node.value}'
					if name := t.const_type_name(qname) {
						return name
					}
				}
				if name := t.const_type_name(node.value) {
					return name
				}
			}
			return ''
		}
		.call {
			concrete_typ := t.concrete_node_type_name(node)
			if concrete_typ.len > 0 {
				return concrete_typ
			}
			new_map_typ := t.new_map_call_type(node)
			if new_map_typ.len > 0 {
				return new_map_typ
			}
			if array_typ := t.array_call_type_name(node) {
				return array_typ
			}
			if call_is_wait_selector(t.a, node) {
				mut wait_ret := t.get_call_return_type(id, node)
				if wait_ret.len == 0 {
					wait_ret = t.current_call_return_type(node)
				}
				if wait_ret.len > 0 {
					return wait_ret
				}
			}
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ !in ['array', 'map', 'unknown'] {
					return typ
				}
			}
			mut ret := t.get_call_return_type(id, node)
			if ret.len == 0 {
				ret = t.current_call_return_type(node)
			}
			if ret.len > 0 {
				return ret
			}
			return ''
		}
		.cast_expr {
			if node.value.len > 0 {
				return node.value
			}
			return node.typ
		}
		.as_expr {
			if node.value.len == 0 {
				return node.typ
			}
			if node.children_count > 0 {
				subject_type := t.trim_pointer_type(t.original_expr_type(t.a.child(&node, 0)))
				if resolved := t.resolve_sum_variant_pattern_for_subject(subject_type, node.value) {
					return resolved
				}
				if t.is_interface_type_name(subject_type) {
					if resolved := t.resolve_interface_pattern(node.value, subject_type) {
						return resolved
					}
				}
			}
			return t.qualify_type(node.value)
		}
		.array_literal {
			if node.children_count > 0 {
				elem_type := t.node_type(t.a.child(&node, 0))
				if t.is_fn_pointer_type_name(elem_type) {
					return '[]${elem_type}'
				}
			}
			if !isnil(t.tc) {
				if typ := t.tc.expr_type(id) {
					name := typ.name()
					if name.starts_with('[]') {
						return t.normalize_type_alias(name)
					}
				}
			}
			if node.typ.len > 0 {
				typ := if checker_alias_type := t.array_literal_checker_alias_type(id) {
					checker_alias_type
				} else if alias_type := t.array_literal_alias_type(node) {
					alias_type
				} else {
					t.normalize_type_alias(node.typ)
				}
				if typ != 'array' {
					return typ
				}
			}
			if node.children_count > 0 {
				elem_type := t.array_literal_elem_type(node)
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
			return '[]int'
		}
		.array_init {
			if node.value.starts_with('[]') {
				return '[]${node.value}'
			}
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ != 'array' {
					return typ
				}
			}
			if t.is_fixed_array_type(node.value) {
				return node.value
			}
			if node.value.len > 0 {
				return '[]${node.value}'
			}
			return '[]int'
		}
		.map_init {
			if node.value.len > 0 {
				return node.value
			}
			if node.children_count >= 2 {
				first_id := t.a.child(&node, 0)
				first := t.a.nodes[int(first_id)]
				if first.kind == .prefix && first.value == '...' && first.children_count > 0 {
					return t.node_type(t.a.child(&first, 0))
				}
				key_type := t.node_type(first_id)
				value_type := t.node_type(t.a.child(&node, 1))
				if key_type.len > 0 && value_type.len > 0 {
					return 'map[${key_type}]${value_type}'
				}
			}
			return ''
		}
		.selector {
			resolved_selector_type := t.resolve_selector_type(node)
			if resolved_selector_type.len > 0 {
				return resolved_selector_type
			}
			if t.smartcast_stack.len == 0 {
				typ := t.concrete_node_type_name(node)
				if typ.len > 0 {
					return typ
				}
			}
			if !isnil(t.tc) && node.children_count > 0 {
				base := t.a.child_node(&node, 0)
				if base.kind == .ident {
					qname := '${base.value}.${node.value}'
					if name := t.const_type_name(qname) {
						return name
					}
				}
			}
			return ''
		}
		.index {
			return t.index_expr_type(id, node)
		}
		.paren {
			if node.children_count > 0 {
				return t.node_type(t.a.child(&node, 0))
			}
			return ''
		}
		.prefix {
			if node.children_count > 0 {
				child_type := t.node_type(t.a.child(&node, 0))
				if node.op == .amp && child_type.len > 0 {
					return '&${child_type}'
				}
				if node.op == .mul && child_type.starts_with('&') {
					return child_type[1..]
				}
				if node.op == .not {
					return 'bool'
				}
				if node.op in [.plus, .minus, .bit_not] {
					return child_type
				}
			}
			return ''
		}
		.block {
			return t.stmt_value_type(id)
		}
		.bool_literal {
			return 'bool'
		}
		.float_literal {
			return 'f64'
		}
		.char_literal {
			return 'rune'
		}
		.string_literal, .string_interp {
			return 'string'
		}
		.int_literal {
			return 'int'
		}
		.nil_literal {
			return 'voidptr'
		}
		.none_expr {
			return '?void'
		}
		.infix {
			if node.children_count >= 2 {
				if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
					return 'bool'
				}
				lhs_type := t.node_type(t.a.child(&node, 0))
				ret_type := t.infix_struct_operator_result_type(node, lhs_type)
				if ret_type.len > 0 {
					return ret_type
				}
				if node.op == .right_shift_unsigned && lhs_type.len > 0 {
					return t.unsigned_shift_type_text(lhs_type)
				}
				if node.op == .plus && lhs_type == 'string' {
					return 'string'
				}
				rhs_type := t.node_type(t.a.child(&node, 1))
				if node.op == .plus && rhs_type == 'string' {
					return 'string'
				}
				if node.op in [.plus, .minus] && lhs_type.starts_with('&')
					&& t.is_integer_type_name(rhs_type) {
					return lhs_type
				}
				if node.op == .plus && rhs_type.starts_with('&') && t.is_integer_type_name(lhs_type) {
					return rhs_type
				}
				if node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor] {
					if lhs_type.len > 0 && rhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type)
						&& t.is_numeric_stringify_type(rhs_type) {
						if promoted := promote_numeric_literal_infix_type(t.a.nodes[int(t.a.child(&node,
							0))], lhs_type, t.a.nodes[int(t.a.child(&node, 1))], rhs_type)
						{
							return promoted
						}
						return promote_numeric_stringify_type(lhs_type, rhs_type)
					}
					if lhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type) {
						return lhs_type
					}
					if rhs_type.len > 0 && t.is_numeric_stringify_type(rhs_type) {
						return rhs_type
					}
				}
			}
			return ''
		}
		.or_expr {
			if node.children_count > 0 {
				inner_type := t.resolve_expr_type(t.a.child(&node, 0))
				if inner_type.starts_with('!') {
					return inner_type[1..]
				}
				if inner_type.starts_with('?') {
					return inner_type[1..]
				}
				return inner_type
			}
			return ''
		}
		.if_expr {
			return t.if_expr_result_type(id, node)
		}
		.match_stmt {
			return t.match_expr_type(node)
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) array_literal_elem_type(node flat.Node) string {
	if node.children_count == 0 {
		return 'int'
	}
	if alias_type := t.array_literal_alias_type(node) {
		return alias_type[2..]
	}
	elem_type := t.node_type(t.a.child(&node, 0))
	if !is_numeric_type_name(elem_type) {
		return elem_type
	}
	mut has_f32 := false
	mut has_f64 := false
	mut has_explicit_f64 := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		child_type := t.node_type(child_id)
		if !is_numeric_type_name(child_type) {
			return elem_type
		}
		if child_type == 'f32' {
			has_f32 = true
		}
		if child_type == 'f64' {
			has_f64 = true
			if !t.is_untyped_float_literal_expr(child) {
				has_explicit_f64 = true
			}
		}
	}
	if has_explicit_f64 {
		return 'f64'
	}
	if has_f32 {
		return 'f32'
	}
	if has_f64 {
		return 'f64'
	}
	return elem_type
}

fn (t &Transformer) is_untyped_float_literal_expr(node flat.Node) bool {
	match node.kind {
		.float_literal {
			return true
		}
		.prefix {
			if node.op !in [.plus, .minus] || node.children_count == 0 {
				return false
			}
			return t.is_untyped_float_literal_expr(t.a.child_node(&node, 0))
		}
		.paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return t.is_untyped_float_literal_expr(t.a.child_node(&node, 0))
		}
		else {
			return false
		}
	}
}

fn is_numeric_type_name(name string) bool {
	return is_integer_type_name(name) || is_float_type_name(name)
}

fn is_integer_type_name(name string) bool {
	return name == 'int' || name == 'i8' || name == 'i16' || name == 'i64' || name == 'u8'
		|| name == 'byte' || name == 'u16' || name == 'u32' || name == 'u64' || name == 'isize'
		|| name == 'usize' || name == 'rune'
}

fn is_float_type_name(name string) bool {
	return name == 'f32' || name == 'f64'
}

fn call_is_wait_selector(a &flat.FlatAst, node flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_id := a.children[int(node.children_start)]
	if int(fn_id) < 0 || int(fn_id) >= a.nodes.len {
		return false
	}
	fn_node := a.nodes[int(fn_id)]
	return fn_node.kind == .selector && fn_node.value == 'wait'
}

fn (t &Transformer) current_call_return_type(node flat.Node) string {
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident {
			local_type := t.var_type(fn_node.value)
			if local_type.len > 0 {
				if ret := t.local_fn_value_return_type_from_type(local_type) {
					return t.call_return_type_name(ret, node)
				}
			} else {
				if ret := t.local_fn_decl_return_type(fn_node.value) {
					return t.call_return_type_name(ret, node)
				}
			}
		}
	}
	name := t.resolve_call_name(node)
	if name.len == 0 {
		return ''
	}
	if ret := t.fn_ret_types[name] {
		return t.call_return_type_name(ret, node)
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[name] {
			return t.call_return_type_name(ret.name(), node)
		}
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if ret := t.fn_ret_types[qname] {
			return t.call_return_type_name(ret, node)
		}
		if !isnil(t.tc) {
			if ret := t.tc.fn_ret_types[qname] {
				return t.call_return_type_name(ret.name(), node)
			}
		}
	}
	return ''
}

fn (t &Transformer) is_local_fn_value_call(node flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return false
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .ident || fn_node.value.len == 0 {
		return false
	}
	local_type := t.var_type(fn_node.value)
	return local_type.starts_with('fn ') || t.is_fn_pointer_type_name(local_type)
}

// const_type_name supports const type name handling for Transformer.
fn (t &Transformer) const_type_name(name string) ?string {
	if isnil(t.tc) || name.len == 0 {
		return none
	}
	key := t.const_type_key(name) or { return none }
	typ := t.tc.const_types[key] or { return none }
	if tname := t.const_entry_type_name(key, typ) {
		return tname
	}
	return none
}

// const_type_key supports const type key handling for Transformer.
fn (t &Transformer) const_type_key(name string) ?string {
	if name.len == 0 || isnil(t.tc) {
		return none
	}
	if name in t.tc.const_types {
		return name
	}
	if key := t.const_suffixes[name] {
		if key.len > 0 {
			return key
		}
	}
	return none
}

fn (t &Transformer) const_type_key_in_context(name string, module_name string, file string) ?string {
	if name.len == 0 || isnil(t.tc) {
		return none
	}
	if !name.contains('.') {
		if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
			qname := '${module_name}.${name}'
			if qname in t.tc.const_types {
				return qname
			}
		}
		if name in t.tc.const_types {
			return name
		}
		if key := t.const_suffixes[name] {
			if key.len > 0 {
				return key
			}
		}
		return none
	}
	if name in t.tc.const_types {
		return name
	}
	base := name.all_before_last('.')
	field := name.all_after_last('.')
	resolved_base := if mod := t.tc.file_imports[file_import_key(file, base)] {
		mod
	} else {
		base
	}
	qname := '${resolved_base}.${field}'
	if qname in t.tc.const_types {
		return qname
	}
	if key := t.const_suffixes[qname] {
		if key.len > 0 {
			return key
		}
	}
	if key := t.const_suffixes[name] {
		if key.len > 0 {
			return key
		}
	}
	return none
}

// const_entry_type_name supports const entry type name handling for Transformer.
fn (t &Transformer) const_entry_type_name(name string, typ types.Type) ?string {
	tname := t.normalize_type_alias(typ.name())
	if tname.len > 0 && tname != 'unknown' {
		if t.is_fixed_array_type(tname) {
			if expr_id := t.tc.const_exprs[name] {
				expr := t.a.nodes[int(expr_id)]
				if expr.kind == .call {
					return '[]${fixed_array_elem_type(tname)}'
				}
			}
		}
		return tname
	}
	if name.ends_with('.scanner_matcher') {
		mod_name := name.all_before_last('.')
		return '${mod_name}.KeywordsMatcherTrie'
	}
	if expr_id := t.tc.const_exprs[name] {
		if etyp := t.tc.expr_type(expr_id) {
			ename := t.normalize_type_alias(etyp.name())
			if ename.len > 0 && ename != 'unknown' {
				return ename
			}
		}
		ename := t.resolve_expr_type(expr_id)
		if ename.len > 0 && ename != 'unknown' {
			return ename
		}
	}
	return none
}

// match_expr_type supports match expr type handling for Transformer.
fn (t &Transformer) match_expr_type(node flat.Node) string {
	if node.kind != .match_stmt || node.children_count < 2 {
		return ''
	}
	match_expr_id := t.a.child(&node, 0)
	mut first_type := ''
	mut concrete_type := ''
	mut has_optional := false
	mut has_result := false
	mut numeric_type := ''
	mut all_numeric := true
	mut has_value_type := false
	for i in 1 .. node.children_count {
		branch := t.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(*branch) }
		if branch.children_count <= body_start {
			continue
		}
		contexts := t.match_branch_type_contexts(match_expr_id, *branch)
		for j := branch.children_count - 1; j >= body_start; j-- {
			stmt_id := t.a.child(branch, j)
			typ := if contexts.len > 0 {
				t.stmt_value_type_with_smartcasts(stmt_id, contexts)
			} else {
				t.stmt_value_type(stmt_id)
			}
			if typ.len > 0 {
				has_value_type = true
				if first_type.len == 0 {
					first_type = typ
				}
				base_typ := if t.is_optional_type_name(typ) {
					t.optional_base_type(typ)
				} else {
					typ
				}
				if t.is_numeric_stringify_type(base_typ) {
					numeric_type = if numeric_type.len == 0 {
						base_typ
					} else {
						promote_numeric_stringify_type(numeric_type, base_typ)
					}
				} else {
					all_numeric = false
				}
				if t.is_optional_type_name(typ) {
					has_optional = true
					if typ[0] == `!` {
						has_result = true
					}
					base := t.optional_base_type(typ)
					if base.len > 0 && base != 'void' && concrete_type.len == 0 {
						concrete_type = base
					}
				} else if concrete_type.len == 0 {
					concrete_type = typ
				}
				break
			}
		}
	}
	if has_value_type && all_numeric && numeric_type.len > 0 {
		if has_optional {
			prefix := if has_result { '!' } else { '?' }
			return '${prefix}${numeric_type}'
		}
		return numeric_type
	}
	if has_optional && concrete_type.len > 0 {
		prefix := if has_result { '!' } else { '?' }
		return '${prefix}${concrete_type}'
	}
	return first_type
}

// match_branch_type_contexts supports match branch type contexts handling for Transformer.
fn (t &Transformer) match_branch_type_contexts(match_expr_id flat.NodeId, branch flat.Node) []SmartcastContext {
	if branch.value == 'else' {
		return []SmartcastContext{}
	}
	n_conds := t.count_conds(branch)
	if n_conds != 1 {
		return []SmartcastContext{}
	}
	cond_val_id := t.a.child(&branch, 0)
	subj := t.expr_key(match_expr_id)
	sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) or {
		return []SmartcastContext{}
	}
	if subj.len == 0 {
		return []SmartcastContext{}
	}
	return [
		SmartcastContext{
			expr_name:     subj
			variant_name:  sc.variant_name
			sum_type_name: sc.sum_type_name
		},
	]
}

// stmt_value_type supports stmt value type handling for Transformer.
fn (t &Transformer) stmt_value_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return ''
		}
		.expr_stmt {
			if node.children_count > 0 {
				return t.expr_value_type(t.a.child(&node, node.children_count - 1))
			}
			return ''
		}
		.block {
			for i := node.children_count - 1; i >= 0; i-- {
				typ := t.stmt_value_type(t.a.child(&node, i))
				if typ.len > 0 {
					return typ
				}
			}
			return ''
		}
		else {
			return t.expr_value_type(id)
		}
	}
}

fn (t &Transformer) expr_value_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		if ret := t.ierror_call_return_type(node) {
			return ret
		}
		call_ret := t.get_call_return_type(id, node)
		if call_ret.len > 0 && call_ret !in ['unknown', 'void'] {
			return call_ret
		}
	}
	return t.node_type(id)
}

fn (t &Transformer) ierror_call_return_type(node flat.Node) ?string {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_type := t.node_type(t.a.child(fn_node, 0))
	clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if clean_base !in ['IError', 'builtin.IError'] {
		return none
	}
	match fn_node.value {
		'msg', 'str' {
			return 'string'
		}
		'code' {
			return 'int'
		}
		else {
			return none
		}
	}
}

// --- match lowering (existing, will move to expr.v later) ---

// lower_match_stmts builds lower match stmts data for transform.
fn (mut t Transformer) lower_match_stmts() {
	for i, node in t.a.nodes {
		if node.kind == .match_stmt {
			if_id := t.lower_one_match(node)
			t.set_node(i, t.a.nodes[int(if_id)])
		} else if node.kind == .expr_stmt && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .match_stmt {
				if_id := t.lower_one_match(child)
				t.set_node(i, flat.Node{
					kind:           .expr_stmt
					children_start: t.a.children.len
					children_count: 1
				})
				t.a.children << if_id
			}
		}
	}
}

// lower_remaining_matches_in_used_fns lowers match nodes that become reachable only after
// monomorphization roots a late helper. It walks emitted function subtrees, not the entire flat
// arena, so unused generic templates do not add transform time or memory.
fn (mut t Transformer) lower_remaining_matches_in_used_fns() {
	t.ensure_node_module_map()
	old_module := t.cur_module
	old_file := t.cur_file
	limit := t.a.nodes.len
	for i in 0 .. limit {
		node := t.a.nodes[i]
		if node.kind != .fn_decl || t.fn_decl_has_unresolved_generics(node, t.node_module_or(i, '')) {
			continue
		}
		t.cur_module = t.node_module_or(i, '')
		if !t.should_transform_fn(node) {
			continue
		}
		mut seen := map[int]bool{}
		for child_id in t.a.children_of(&node) {
			t.lower_remaining_match_subtree(child_id, mut seen)
		}
	}
	t.cur_module = old_module
	t.cur_file = old_file
}

fn (mut t Transformer) lower_remaining_match_subtree(id flat.NodeId, mut seen map[int]bool) {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len || seen[idx] {
		return
	}
	seen[idx] = true
	mut node := t.a.nodes[idx]
	if node.kind == .match_stmt {
		lowered := t.lower_one_match(node)
		t.set_node(idx, t.a.nodes[int(lowered)])
		node = t.a.nodes[idx]
	}
	children := t.a.children_of(&node).clone()
	for child_id in children {
		t.lower_remaining_match_subtree(child_id, mut seen)
	}
}

// lower_one_match builds lower one match data for transform.
fn (mut t Transformer) lower_one_match(node flat.Node) flat.NodeId {
	match_expr_id := t.a.child(&node, 0)
	match_expr := t.a.nodes[int(match_expr_id)]
	result_type := t.match_expr_type(node)

	needs_temp := match_expr.kind !in [.ident, .int_literal, .bool_literal, .string_literal,
		.char_literal]

	mut actual_expr_id := match_expr_id
	mut prefix_id := flat.empty_node
	mut match_prelude := []flat.NodeId{}

	if needs_temp {
		tmp_name := '__match_tmp_${int(match_expr_id)}'
		mut match_type := t.node_type(match_expr_id)
		outer_pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		transformed_match_expr := if match_expr.kind == .or_expr
			&& t.is_optional_type_name(match_type) {
			match_type = t.optional_base_type(t.qualify_optional_type(match_type))
			t.transform_expr_for_type(match_expr_id, match_type)
		} else {
			t.transform_expr(match_expr_id)
		}
		t.drain_pending(mut match_prelude)
		t.pending_stmts = outer_pending
		tmp_ident := t.a.add_val(.ident, tmp_name)
		t.set_node_typ(int(tmp_ident), match_type)
		decl_start := t.a.children.len
		t.a.children << tmp_ident
		t.a.children << transformed_match_expr
		prefix_id = t.a.add_node(flat.Node{
			kind:           .decl_assign
			children_start: decl_start
			children_count: 2
			typ:            match_type
		})
		actual_expr_id = t.a.add_val(.ident, tmp_name)
		t.set_node_typ(int(actual_expr_id), match_type)
	}

	mut branches := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branches << t.a.child(&node, i)
	}
	if_id := t.build_match_chain(actual_expr_id, match_expr_id, branches, 0)

	if needs_temp {
		block_start := t.a.children.len
		for id in match_prelude {
			t.a.children << id
		}
		t.a.children << prefix_id
		t.a.children << if_id
		block_id := t.a.add_node(flat.Node{
			kind:           .block
			children_start: block_start
			children_count: flat.child_count(match_prelude.len + 2)
			typ:            result_type
		})
		return block_id
	}
	if result_type.len > 0 {
		t.set_node_typ(int(if_id), result_type)
	}
	return if_id
}

// build_match_chain builds match chain data for transform.
fn (mut t Transformer) build_match_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'

	// count_conds scans the branch's condition children; compute it once and reuse
	// (build_match_chain runs per branch, and the compiler has very large matches).
	n_conds := if is_else { 0 } else { t.count_conds(branch) }
	body_start_idx := n_conds
	if !is_else && n_conds > 1 && t.match_branch_all_type_patterns(match_expr_id, branch) {
		return t.build_match_type_branch_chain(match_expr_id, orig_expr_id, branch, branches, idx,
			0)
	}
	// Push a smartcast around the body transform when this branch matches a
	// single sum-type variant, so selectors inside the body get narrowed.
	mut sc_pushed := 0
	if !is_else {
		if n_conds == 1 {
			cond_val_id := t.a.child(&branch, 0)
			if sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) {
				subj := t.expr_key(match_expr_id)
				if subj.len > 0 {
					t.push_smartcast(subj, sc.variant_name, sc.sum_type_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj {
					t.push_smartcast(orig_subj, sc.variant_name, sc.sum_type_name)
					sc_pushed++
				}
			}
		}
	}
	mut body_ids := []flat.NodeId{cap: int(branch.children_count) - body_start_idx}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	new_body := t.transform_stmts(body_ids)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}
	body_block := t.make_block(new_body)

	if is_else {
		return body_block
	}

	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	cond_id := t.build_match_cond(match_expr_id, branch)
	mut cond_prelude := []flat.NodeId{}
	t.drain_pending(mut cond_prelude)
	t.pending_stmts = outer_pending

	mut if_ids := []flat.NodeId{}
	if_ids << cond_id
	if_ids << body_block
	if idx + 1 < branches.len {
		else_part := t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
		if_ids << else_part
	}

	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	if_id := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(if_ids.len)
	})
	if cond_prelude.len > 0 {
		cond_prelude << if_id
		return t.make_block(cond_prelude)
	}
	return if_id
}

// build_match_value_stmts builds match value stmts data for transform.
fn (mut t Transformer) build_match_value_stmts(node flat.Node, target_name string, target_type string) []flat.NodeId {
	match_expr_id := t.a.child(&node, 0)
	match_expr := t.a.nodes[int(match_expr_id)]
	needs_temp := match_expr.kind !in [.ident, .int_literal, .bool_literal, .string_literal,
		.char_literal]

	mut actual_expr_id := match_expr_id
	mut result := []flat.NodeId{}
	if needs_temp {
		tmp_name := '__match_tmp_${int(match_expr_id)}'
		mut match_type := t.node_type(match_expr_id)
		transformed_match_expr := if match_expr.kind == .or_expr
			&& t.is_optional_type_name(match_type) {
			match_type = t.optional_base_type(t.qualify_optional_type(match_type))
			t.transform_expr_for_type(match_expr_id, match_type)
		} else {
			t.transform_expr(match_expr_id)
		}
		t.drain_pending(mut result)
		tmp_ident := t.a.add_val(.ident, tmp_name)
		t.set_node_typ(int(tmp_ident), match_type)
		result << t.make_decl_assign_typed(tmp_name, transformed_match_expr, match_type)
		actual_expr_id = t.a.add_val(.ident, tmp_name)
		t.set_node_typ(int(actual_expr_id), match_type)
	}

	mut branches := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branches << t.a.child(&node, i)
	}
	result << t.build_match_value_chain(actual_expr_id, match_expr_id, branches, 0, target_name,
		target_type)
	return result
}

// build_match_value_chain builds match value chain data for transform.
fn (mut t Transformer) build_match_value_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int, target_name string, target_type string) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'
	body_start_idx := if is_else { 0 } else { t.count_conds(branch) }
	if !is_else && t.match_branch_all_type_patterns(match_expr_id, branch)
		&& t.count_conds(branch) > 1 {
		return t.build_match_value_type_branch_chain(match_expr_id, orig_expr_id, branch, branches,
			idx, 0, target_name, target_type)
	}

	mut sc_pushed := 0
	if !is_else {
		n_conds := t.count_conds(branch)
		if n_conds == 1 {
			cond_val_id := t.a.child(&branch, 0)
			if sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) {
				subj := t.expr_key(match_expr_id)
				if subj.len > 0 {
					t.push_smartcast(subj, sc.variant_name, sc.sum_type_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj {
					t.push_smartcast(orig_subj, sc.variant_name, sc.sum_type_name)
					sc_pushed++
				}
			}
		}
	}

	mut body_ids := []flat.NodeId{cap: int(branch.children_count) - body_start_idx}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	raw_body := t.make_block(body_ids)
	body_block := t.if_value_branch_block(raw_body, target_name, target_type)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	if is_else {
		return body_block
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	cond_id := t.build_match_cond(match_expr_id, branch)
	mut cond_prelude := []flat.NodeId{}
	t.drain_pending(mut cond_prelude)
	t.pending_stmts = outer_pending
	mut if_ids := []flat.NodeId{}
	if_ids << cond_id
	if_ids << body_block
	if idx + 1 < branches.len {
		else_part := t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1,
			target_name, target_type)
		if_ids << else_part
	}
	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	if_id := t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(if_ids.len)
	})
	if cond_prelude.len > 0 {
		cond_prelude << if_id
		return t.make_block(cond_prelude)
	}
	return if_id
}

// build_match_value_type_branch_chain supports build_match_value_type_branch_chain handling.
fn (mut t Transformer) build_match_value_type_branch_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branch flat.Node, branches []flat.NodeId, idx int, cond_idx int, target_name string, target_type string) flat.NodeId {
	n_conds := t.count_conds(branch)
	if cond_idx >= n_conds {
		return if idx + 1 < branches.len {
			t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1, target_name,
				target_type)
		} else {
			t.a.add(flat.NodeKind.empty)
		}
	}
	cond_val_id := t.a.child(&branch, cond_idx)
	variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) or {
		return t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1,
			target_name, target_type)
	}
	is_start := t.a.children.len
	t.a.children << match_expr_id
	is_id := t.a.add_node(flat.Node{
		kind:           .is_expr
		value:          variant_name
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) or {
		SmartcastContext{
			variant_name:  variant_name
			sum_type_name: ''
		}
	}
	subj := t.expr_key(match_expr_id)
	if subj.len > 0 && sc.sum_type_name.len > 0 {
		t.push_smartcast(subj, sc.variant_name, sc.sum_type_name)
		sc_pushed++
	}
	orig_subj := t.expr_key(orig_expr_id)
	if orig_subj.len > 0 && orig_subj != subj && sc.sum_type_name.len > 0 {
		t.push_smartcast(orig_subj, sc.variant_name, sc.sum_type_name)
		sc_pushed++
	}

	mut body_ids := []flat.NodeId{cap: int(branch.children_count) - n_conds}
	for i in n_conds .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	raw_body := t.make_block(body_ids)
	body_block := t.if_value_branch_block(raw_body, target_name, target_type)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_match_value_type_branch_chain(match_expr_id, orig_expr_id, branch,
		branches, idx, cond_idx + 1, target_name, target_type)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
	})
}

// build_match_type_branch_chain builds match type branch chain data for transform.
fn (mut t Transformer) build_match_type_branch_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branch flat.Node, branches []flat.NodeId, idx int, cond_idx int) flat.NodeId {
	n_conds := t.count_conds(branch)
	if cond_idx >= n_conds {
		return if idx + 1 < branches.len {
			t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
		} else {
			t.a.add(flat.NodeKind.empty)
		}
	}
	cond_val_id := t.a.child(&branch, cond_idx)
	variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) or {
		return t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
	}
	is_start := t.a.children.len
	t.a.children << match_expr_id
	is_id := t.a.add_node(flat.Node{
		kind:           .is_expr
		value:          variant_name
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) or {
		SmartcastContext{
			variant_name:  variant_name
			sum_type_name: ''
		}
	}
	subj := t.expr_key(match_expr_id)
	if subj.len > 0 && sc.sum_type_name.len > 0 {
		t.push_smartcast(subj, sc.variant_name, sc.sum_type_name)
		sc_pushed++
	}
	orig_subj := t.expr_key(orig_expr_id)
	if orig_subj.len > 0 && orig_subj != subj && sc.sum_type_name.len > 0 {
		t.push_smartcast(orig_subj, sc.variant_name, sc.sum_type_name)
		sc_pushed++
	}

	mut body_ids := []flat.NodeId{cap: int(branch.children_count) - n_conds}
	for i in n_conds .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	body_block := t.make_block(t.transform_stmts(body_ids))
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_match_type_branch_chain(match_expr_id, orig_expr_id, branch, branches,
		idx, cond_idx + 1)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
	})
}

// make_match_eq builds the equality test between a match subject and a branch
// value, lowering string comparisons to string__eq (the transformer owns string
// lowering; the backend no longer special-cases it).
fn (mut t Transformer) make_match_eq(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	eq_id := t.a.add_node(flat.Node{
		kind:           .infix
		op:             .eq
		children_start: start
		children_count: 2
	})
	return t.transform_infix_expr(eq_id, t.a.nodes[int(eq_id)])
}

// make_match_range builds make match range data for transform.
fn (mut t Transformer) make_match_range(lhs flat.NodeId, range_id flat.NodeId) flat.NodeId {
	range := t.a.nodes[int(range_id)]
	if range.children_count < 2 {
		return t.make_bool_literal(false)
	}
	low_id := t.a.children[range.children_start]
	high_id := t.a.children[range.children_start + 1]
	low := t.match_cond_value(lhs, low_id)
	high := t.match_cond_value(lhs, high_id)
	ge_cmp := t.make_infix(.ge, lhs, low)
	le_cmp := t.make_infix(.le, lhs, high)
	return t.make_infix(.logical_and, ge_cmp, le_cmp)
}

// match_cond_value supports match cond value handling for Transformer.
fn (mut t Transformer) match_cond_value(match_expr_id flat.NodeId, cond_val_id flat.NodeId) flat.NodeId {
	cond_val := t.a.nodes[int(cond_val_id)]
	if cond_val.kind == .enum_val {
		return t.transform_enum_shorthand(cond_val_id, cond_val, t.node_type(match_expr_id))
	}
	return t.transform_expr(cond_val_id)
}

// build_match_cond builds match cond data for transform.
fn (mut t Transformer) build_match_cond(match_expr_id flat.NodeId, branch flat.Node) flat.NodeId {
	n_conds := t.count_conds(branch)
	if n_conds == 1 {
		cond_val_id := t.a.child(&branch, 0)
		cond_val := t.a.nodes[int(cond_val_id)]
		if variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) {
			is_start := t.a.children.len
			t.a.children << match_expr_id
			is_id := t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          variant_name
				children_start: is_start
				children_count: 1
			})
			return t.transform_is_expr(is_id, t.a.nodes[int(is_id)])
		}
		if cond_val.kind == .range {
			return t.make_match_range(match_expr_id, cond_val_id)
		}
		return t.make_match_eq(match_expr_id, t.match_cond_value(match_expr_id, cond_val_id))
	}
	mut result := flat.empty_node
	for i in 0 .. n_conds {
		cond_val_id := t.a.child(&branch, i)
		cond_val := t.a.nodes[int(cond_val_id)]
		variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) or { '' }
		cmp := if variant_name.len > 0 {
			is_start := t.a.children.len
			t.a.children << match_expr_id
			is_id := t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          variant_name
				children_start: is_start
				children_count: 1
			})
			t.transform_is_expr(is_id, t.a.nodes[int(is_id)])
		} else if cond_val.kind == .range {
			t.make_match_range(match_expr_id, cond_val_id)
		} else {
			t.make_match_eq(match_expr_id, t.match_cond_value(match_expr_id, cond_val_id))
		}
		if int(result) < 0 {
			result = cmp
		} else {
			or_start := t.a.children.len
			t.a.children << result
			t.a.children << cmp
			result = t.a.add_node(flat.Node{
				kind:           .infix
				op:             .logical_or
				children_start: or_start
				children_count: 2
			})
		}
	}
	return result
}

// match_type_pattern supports match type pattern handling for Transformer.
fn (t &Transformer) match_type_pattern(cond_val_id flat.NodeId) ?string {
	if int(cond_val_id) < 0 {
		return none
	}
	pattern := t.type_pattern_name(cond_val_id)
	if pattern.len > 0 && t.is_sum_variant(pattern) {
		return pattern
	}
	return none
}

fn (t &Transformer) match_type_pattern_for_subject(match_expr_id flat.NodeId, cond_val_id flat.NodeId) ?string {
	if int(cond_val_id) < 0 {
		return none
	}
	pattern := t.type_pattern_name(cond_val_id)
	if pattern.len == 0 {
		return none
	}
	subject_type := t.trim_pointer_type(t.original_expr_type(match_expr_id))
	if t.is_interface_type_name(subject_type) {
		return t.resolve_interface_pattern(pattern, subject_type)
	}
	if resolved_variant := t.resolve_sum_variant_pattern_for_subject(subject_type, pattern) {
		return resolved_variant
	}
	if t.is_sum_variant(pattern) {
		return pattern
	}
	return none
}

fn (t &Transformer) resolve_sum_variant_pattern_for_subject(subject_type string, pattern string) ?string {
	if pattern.len == 0 {
		return none
	}
	for candidate in t.sum_subject_type_candidates(subject_type) {
		resolved_sum := t.resolve_sum_name(candidate)
		if resolved_variant := t.sum_variant_name(resolved_sum, pattern) {
			return resolved_variant
		}
		if !isnil(t.tc) {
			if resolved := t.tc.sum_variant_type_for_pattern(candidate, pattern) {
				return resolved
			}
		}
	}
	return none
}

fn (t &Transformer) match_type_smartcast_context(match_expr_id flat.NodeId, cond_val_id flat.NodeId) ?SmartcastContext {
	variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) or { return none }
	subject_type := t.trim_pointer_type(t.original_expr_type(match_expr_id))
	sum_name := if t.is_interface_type_name(subject_type) {
		resolved := t.resolve_interface_type_name(subject_type)
		if resolved.len > 0 {
			resolved
		} else {
			subject_type
		}
	} else {
		t.sum_type_for_is_expr(subject_type, variant_name)
	}
	if sum_name.len == 0 {
		return none
	}
	return SmartcastContext{
		variant_name:  variant_name
		sum_type_name: sum_name
	}
}

fn (t &Transformer) match_pattern_implements_interface(pattern string, subject_type string) bool {
	return t.resolve_interface_pattern(pattern, subject_type) != none
}

fn (t &Transformer) resolve_interface_pattern(pattern string, subject_type string) ?string {
	if !t.is_interface_type_name(subject_type) {
		return none
	}
	resolved_iface := t.resolve_interface_type_name(subject_type)
	iface := if resolved_iface.len > 0 { resolved_iface } else { subject_type }
	for candidate in t.interface_pattern_candidates(pattern) {
		if t.is_builtin_ierror_interface_name(iface) {
			if t.tc.named_type_compatible_with_ierror(candidate) {
				return candidate
			}
		} else if t.tc.named_type_implements_interface(candidate, iface) {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) interface_pattern_candidates(pattern string) []string {
	mut candidates := []string{}
	if !pattern.contains('.') {
		mut has_scoped_candidate := false
		if t.cur_file.len > 0 {
			for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, pattern)] or {
				[]string{}
			} {
				if t.interface_pattern_candidate_known(candidate) {
					candidates << candidate
					has_scoped_candidate = true
				}
			}
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			local := '${t.cur_module}.${pattern}'
			if t.interface_pattern_candidate_known(local) {
				candidates << local
				has_scoped_candidate = true
			}
		}
		if !has_scoped_candidate {
			candidates << pattern
		}
	} else if resolved := t.resolve_import_alias_pattern(pattern) {
		candidates << resolved
		candidates << pattern
	} else {
		candidates << pattern
	}
	qpattern := t.tc.qualify_name(pattern)
	if qpattern != pattern {
		candidates << qpattern
	}
	mut result := []string{}
	mut seen := map[string]bool{}
	for candidate in candidates {
		if candidate.len == 0 || candidate in seen {
			continue
		}
		seen[candidate] = true
		result << candidate
	}
	return result
}

fn (t &Transformer) interface_pattern_candidate_known(candidate string) bool {
	return candidate in t.tc.type_aliases || candidate in t.tc.structs
		|| candidate in t.tc.interface_names || candidate in t.tc.flag_enums
		|| candidate in t.tc.enum_names || candidate in t.tc.sum_types
}

fn (t &Transformer) resolve_import_alias_pattern(pattern string) ?string {
	if t.cur_file.len == 0 {
		return none
	}
	dot := pattern.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	alias := pattern[..dot]
	resolved := t.tc.file_imports[file_import_key(t.cur_file, alias)] or { return none }
	return '${resolved}.${pattern[dot + 1..]}'
}

// match_branch_all_type_patterns supports match branch all type patterns handling for Transformer.
fn (t &Transformer) match_branch_all_type_patterns(match_expr_id flat.NodeId, branch flat.Node) bool {
	n_conds := t.count_conds(branch)
	if n_conds == 0 {
		return false
	}
	for i in 0 .. n_conds {
		cond_val_id := t.a.child(&branch, i)
		if _ := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) {
			continue
		}
		return false
	}
	return true
}

// count_conds supports count conds handling for Transformer.
fn (t &Transformer) count_conds(branch flat.Node) int {
	if branch.value.len > 0 && branch.value != 'else' {
		if branch.value[0] >= `0` && branch.value[0] <= `9` {
			return branch.value.int()
		}
	}
	mut count := 0
	for i in 0 .. branch.children_count {
		child := t.a.child_node(&branch, i)
		if child.kind == .int_literal || child.kind == .ident || child.kind == .string_literal
			|| child.kind == .enum_val || child.kind == .bool_literal || child.kind == .char_literal
			|| child.kind == .selector || child.kind == .range || child.kind == .prefix {
			count++
		} else {
			break
		}
	}
	return count
}

// is_sum_variant reports whether is sum variant applies in transform.
pub fn (t &Transformer) is_sum_variant(name string) bool {
	return name in t.sum_variant_names || t.variant_short_name(name) in t.sum_variant_names
}

// --- array append lowering (existing, will move to expr.v later) ---

// lower_array_appends builds lower array appends data for transform.
fn (mut t Transformer) lower_array_appends() {
	for i, node in t.a.nodes {
		if node.kind == .module_decl {
			t.cur_module = node.value
			continue
		}
		if node.kind == .fn_decl {
			t.reset_var_types()
			t.annotate_fn_body(node)
			continue
		}
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs := t.a.child_node(&node, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(node)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if node.kind == .expr_stmt && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			mut child := &t.a.nodes[int(child_id)]
			if child.kind == .infix && child.op == .left_shift {
				t.annotate_left_shift(child_id)
			}
		}
		if node.kind == .assign && node.op == .left_shift_assign && node.children_count >= 2 {
			lhs_id := t.a.child(&node, 0)
			lhs_type := t.lvalue_type(lhs_id)
			clean_lhs_type := t.clean_array_append_lhs_type(lhs_type)
			if clean_lhs_type.starts_with('[]') {
				rhs_id := t.a.child(&node, 1)
				rhs_type := t.lvalue_type(rhs_id)
				elem_type := clean_lhs_type[2..]
				val := if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type) {
					'push_many'
				} else {
					'push'
				}
				t.set_node(i, flat.Node{
					kind:           node.kind
					op:             node.op
					children_start: node.children_start
					children_count: node.children_count
					value:          val
					typ:            elem_type
				})
			}
		}
	}
}

// annotate_fn_body supports annotate fn body handling for Transformer.
fn (mut t Transformer) annotate_fn_body(fn_node flat.Node) {
	for i in 0 .. fn_node.children_count {
		child_id := t.a.child(&fn_node, i)
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
			typ := t.normalize_type_alias(child.typ)
			if child.is_mut || child.op == .amp || child.typ.starts_with('mut ') {
				t.mut_param_values[child.value] = true
			}
			t.set_var_type(child.value, typ)
		}
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if child.kind == .expr_stmt && child.children_count == 1 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .infix && inner.op == .left_shift {
				t.annotate_left_shift(inner_id)
			}
		}
		t.annotate_block_stmts(child_id)
	}
}

// annotate_block_stmts supports annotate block stmts handling for Transformer.
fn (mut t Transformer) annotate_block_stmts(node_id flat.NodeId) {
	if int(node_id) < 0 {
		return
	}
	node := t.a.nodes[int(node_id)]
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if child.kind == .expr_stmt && child.children_count == 1 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .infix && inner.op == .left_shift {
				t.annotate_left_shift(inner_id)
			}
		}
		t.annotate_block_stmts(child_id)
	}
}

// annotate_left_shift supports annotate left shift handling for Transformer.
fn (mut t Transformer) annotate_left_shift(node_id flat.NodeId) {
	node := t.a.nodes[int(node_id)]
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	mut lhs_type := t.lvalue_type(lhs_id)
	if lhs_type == 'strings.Builder' || lhs_type == '&strings.Builder' || lhs_type == 'Builder'
		|| lhs_type == '&Builder' {
		lhs_type = '[]u8'
	}
	clean_lhs_type := t.clean_array_append_lhs_type(lhs_type)
	if !clean_lhs_type.starts_with('[]') {
		return
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.lvalue_type(rhs_id)
	elem_type := clean_lhs_type[2..]
	if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type) {
		t.set_node(int(node_id), flat.Node{
			kind:           .infix
			op:             .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value:          'push_many'
			typ:            elem_type
		})
	} else {
		t.set_node(int(node_id), flat.Node{
			kind:           .infix
			op:             .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value:          'push'
			typ:            elem_type
		})
	}
}

// annotate_left_shift_assign supports annotate left shift assign handling for Transformer.
fn (mut t Transformer) annotate_left_shift_assign(node_id flat.NodeId) {
	node := t.a.nodes[int(node_id)]
	if node.kind != .assign || node.op != .left_shift_assign || node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return
	}
	lhs_type := t.lvalue_type(lhs_id)
	clean_lhs_type := t.clean_array_append_lhs_type(lhs_type)
	if !clean_lhs_type.starts_with('[]') {
		return
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.lvalue_type(rhs_id)
	elem_type := clean_lhs_type[2..]
	val := if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type) {
		'push_many'
	} else {
		'push'
	}
	t.set_node(int(node_id), flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: node.children_start
		children_count: node.children_count
		value:          val
		typ:            elem_type
	})
}

// --- public query helpers ---

// get_struct_info returns get struct info data for Transformer.
pub fn (t &Transformer) get_struct_info(name string) ?StructInfo {
	if info := t.structs[name] {
		return info
	}
	return none
}

// get_global_type returns get global type data for Transformer.
pub fn (t &Transformer) get_global_type(name string) ?string {
	if typ := t.globals[name] {
		return typ
	}
	return none
}
