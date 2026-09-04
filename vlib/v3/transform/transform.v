module transform

import os
import time
import v3.flat
import v3.gen.c.naming
import v3.types

@[inline]
fn same_transform_text(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	if unsafe { a.str == b.str } {
		return true
	}
	return a == b
}

// short_name_view returns the suffix after the final dot without allocating.
@[direct_array_access; inline]
fn short_name_view(name string) string {
	for i := name.len - 1; i >= 0; i-- {
		if name[i] == `.` {
			return unsafe { name.substr_unsafe(i + 1, name.len) }
		}
	}
	return name
}

@[direct_array_access; inline]
fn owner_name_view(name string) string {
	for i := name.len - 1; i >= 0; i-- {
		if name[i] == `.` {
			return unsafe { name.substr_unsafe(0, i) }
		}
	}
	return name
}

// option_unwrap_marker tags a SmartcastContext produced by an `x != none`
// condition: variant_name holds the option's base type and the access is
// lowered to the option's `.value` field instead of a sum union field.
pub const option_unwrap_marker = '?opt'
const skip_scope_drops_block_value = '__v3_skip_scope_drops'
const prefix_scope_drops_block_value = '__v3_prefix_scope_drops'

const generated_variant_access_marker = '__v3_generated_variant_access'
const optional_wrapper_access_marker = '__v3_optional_wrapper_access'
const transformed_option_unwrap_access_marker = '__v3_transformed_option_unwrap_access'
const non_aliasing_allocation_call_marker = '__v3_non_aliasing_allocation_call'
const source_deref_marker = '__v3_source_deref'
const source_mut_pointer_deref_marker = '__v3_source_mut_pointer_deref'
const stack_value_decl_marker = '__v3_stack_value_decl'

// SumEqRequest records where a sum type's equality helper was first requested,
// so the helper body is built under that module/file resolution context. The
// helper module can differ for program-specific generic specializations, whose
// generated functions and helpers must stay in the main cache segment.
pub struct SumEqRequest {
pub:
	sum_name      string
	module        string
	file          string
	helper_module string
}

// AutoStrRequest records where an automatic aggregate string helper was first
// requested while compiling V itself.
struct AutoStrRequest {
	module        string
	file          string
	helper_module string
}

// DefaultCloneRequest records the source resolution context for a recursive
// compiler-provided IClone helper. The helper is emitted after parallel body
// transformation so mutually recursive aggregate layouts do not recurse in the
// transformer itself.
struct DefaultCloneRequest {
	module string
	file   string
}

// MonomorphCacheSpec identifies one concrete generic function body. Dependency
// specialization caches use it to restore signatures without cloning unchanged
// module templates into the program AST.
pub struct MonomorphCacheSpec {
pub:
	decl_key string
	module   string
	args     []string
}

// SmartcastContext stores smartcast context state used by transform.
pub struct SmartcastContext {
pub:
	expr_name     string // the expression being smartcast (e.g. "node")
	variant_name  string // the variant type name (e.g. "Ident")
	sum_type_name string // the parent sum type name (e.g. "Expr")
}

struct LocalClosureFieldCandidate {
	source_id       int
	owner_id        int
	decl_id         int
	aggregate_name  string
	aggregate_scope int
	field_key       string
}

struct LocalClosureDeclCandidate {
	source_id int
	decl_id   int
	scope_id  int
	name      string
}

struct InplaceChildRewrite {
	slot  int
	child flat.NodeId
}

// Transformer represents transformer data used by transform.
pub struct Transformer {
mut:
	a                             &flat.FlatAst = unsafe { nil }
	tc                            &types.TypeChecker = unsafe { nil }
	structs                       map[string]StructInfo
	embedded_fields               map[string][]FieldInfo
	struct_short_name_index       map[string]string
	struct_short_name_index_ready bool
	non_main_type_short_names     map[string]bool
	non_main_type_index_ready     bool
	unique_fields                 map[string]string
	alias_methods                 map[string]string
	globals                       map[string]string
	sum_types                     map[string][]string
	sum_variant_parents           map[string][]string
	sum_variant_names             map[string]bool
	sum_variant_fields            map[string]string
	qualified_types               map[string]string
	fn_ret_types                  map[string]string
	fn_ret_types_log              []string
	tc_signature_names_log        []string
	generated_capture_contexts    []string
	struct_maps_shared            bool
	multi_return_fn_ret_types     map[string]types.Type
	receiver_method_suffix_index  map[string]string
	declared_fn_name_counts       map[string]u8
	variadic_suffix_index         map[string]i8
	const_suffixes                map[string]string
	source_parent_ids             []int
	shared_local_decl_names       map[string]bool
	// const_array_fixed_storage_ready marks the cache below as fully populated
	// (by the overlapped pre-dispatch scan), making the precompute a no-op.
	const_array_fixed_storage_ready bool
	// defer_pre_scan_indexes routes the AST/tc-only index builders in prepare()
	// to the overlapped pre-scan helper thread (see prepare_with_pre_scans).
	defer_pre_scan_indexes bool
	// merge_regions_relocated marks worker regions as already id-relocated in
	// place (parallel pass), so merge_worker compacts with plain memmoves.
	merge_regions_relocated bool
	// const_array_fixed_storage_cache avoids rescanning the complete AST for
	// repeated uses of the same array constant in one transform worker.
	const_array_fixed_storage_cache map[string]i8
	enum_types                      map[string][]string
	enum_backing_types              map[string]string
	runtime_type_indexes            map[string]int
	cur_file                        string
	cur_module                      string
	cur_fn_name                     string
	cur_fn_source_file              string
	cur_fn_source_module            string
	cur_fn_receiver_name            string
	cur_fn_ret_type                 string
	cur_fn_is_generic               bool
	cur_fn_manualfree               bool
	literal_free_fn_body            bool // work item is proven to contain no closure literals
	cur_fn_variadic_param           string
	skip_generics                   bool
	building_v                      bool
	var_types                       []VarTypeBinding
	var_type_indices                map[string]int
	var_type_cache                  &VarTypeIndexCache = unsafe { nil }
	refined_node_types              map[int]string
	fn_value_locals                 map[string]string
	mut_param_values                map[string]bool
	fixed_array_param_values        map[string]bool
	mut_value_ident_nodes           map[int]bool
	ordering_snapshot_names         map[string]bool
	pointer_value_lvalues           map[string]bool
	pointer_value_rvalues           map[string]bool
	addr_lvalue_pointer_locals      map[string]bool
	orm_initialized_fields          map[string][]string
	sql_query_data_aliases          map[string][]string
	bound_method_arrays             map[string]BoundMethodArrayInfo
	temp_counter                    int
	global_temp_counter             int
	pending_stmts                   []flat.NodeId
	smartcast_stack                 []SmartcastContext
	invalidated_smartcasts          map[string]bool
	in_call_callee                  bool
	in_monomorphize_scan            bool
	validating_generic_spec         bool
	allow_comptime_enum_int_assign  bool
	monomorph_errors                []string
	monomorph_error_seen            map[string]bool
	in_spawn_expr                   bool
	has_spawn_expr                  bool
	in_const_init                   bool
	in_return_expr                  bool
	in_string_interp_part           bool
	expected_expr_node              int = -1
	expected_expr_type              string
	in_selector_base                bool
	// Set while transforming the base of a bound-method-value selector, where a
	// `first()`/`last()` accessor base must keep its copying semantics instead of being
	// borrowed in place (see borrow_first_last_accessor / transform_selector_expr).
	suppress_first_last_accessor_borrow bool

	autolock_depth                int
	alias_cache                   &AliasCache = unsafe { nil }
	sum_cache                     &AliasCache = unsafe { nil }
	module_type_cache             &AliasCache = unsafe { nil }
	struct_guess_cache            &AliasCache = unsafe { nil }
	generic_unresolved_cache      &GenericUnresolvedCache = unsafe { nil }
	generic_spec_decode_cache     &LookupCache = unsafe { nil }
	struct_field_type_cache       &LookupCache = unsafe { nil }
	variant_short_name_cache      &AliasCache = unsafe { nil }
	selector_type_cache           &SelectorTypeCache = unsafe { nil }
	resolved_call_return_cache    &ResolvedCallReturnCache = unsafe { nil }
	variant_match_cache           &VariantMatchCache = unsafe { nil }
	interface_type_cache          &ContextLookupCache = unsafe { nil }
	enum_expected_cache           &LookupCache = unsafe { nil }
	type_alias_name_cache         &ContextBoolLookupCache = unsafe { nil }
	interface_box_param_cache     &BoolLookupCache = unsafe { nil }
	alias_receiver_method_cache   &LookupCache = unsafe { nil }
	receiver_method_cache         &ReceiverMethodCache = unsafe { nil }
	promote_text_cache            &PromoteTextCache = unsafe { nil }
	call_variadic_cache           &BoolLookupCache = unsafe { nil }
	str_alias_cache               &LookupCache = unsafe { nil }
	generic_alias_names           map[string]bool
	type_alias_suffixes           map[string]string
	local_decl_nodes_by_name      map[string][]int
	fn_decl_offsets_by_file       map[int][]int
	struct_field_decl_metas_cache map[string]map[string]FieldDeclMeta
	comptime_field_metas_cache    map[string][]FieldMeta
	comptime_reflected_for_roles  map[string]u8
	comptime_reflected_for_ready  bool
	call_param_types_decl_cache   map[int][]types.Type
	call_param_types_decl_misses  map[string]bool
	call_param_types_decl_index   map[string]FnParamDeclRef
	call_param_types_index_ready  bool
	call_param_types_prepared     bool
	used_fns                      map[string]bool
	used_fns_parent               &map[string]bool = unsafe { nil }
	used_fns_root                 &map[string]bool = unsafe { nil }
	comptime_reflected_params     map[string][]ParamMeta
	// sum_eq_types records sum types whose deep-equality helper fn
	// (__v3_sum_eq_<name>) is called somewhere, keyed by the concrete helper name with the
	// module/file context of the requesting call site (type resolution inside
	// the helper body needs that context). The helpers are synthesized
	// serially after the (possibly parallel) transform completes.
	sum_eq_types                  map[string]SumEqRequest
	sum_eq_synthesized            map[string]bool
	sum_eq_helper_module          string
	auto_str_types                map[string]AutoStrRequest
	auto_str_synthesized          map[string]bool
	auto_str_helper_module        string
	auto_str_synthesis_type       string
	default_clone_types           map[string]DefaultCloneRequest
	default_clone_synthesized     map[string]bool
	default_clone_expansion_stack []string
	interface_boxed_types         map[string]bool
	// interface_boxed_types_late records concrete boxes discovered after the
	// source-level box index is frozen. Each transform worker owns this map, so
	// auto-string lowering can extend its local dispatch index without mutating
	// the shared immutable source index.
	interface_boxed_types_late     map[string]bool
	interface_boxed_impl_processed map[string]bool
	interface_boxed_types_done     bool
	interface_boxed_types_frozen   bool
	interface_impl_indexes         map[string]&types.InterfaceImplIndex
	interface_impl_spec_count      int
	ierror_none_type_id            int
	interface_var_concrete_types   map[string]string
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
	ignored_comptime_for_nodes  []bool
	ignored_comptime_for_log    []int
	ignored_comptime_log_active bool
	// cloning_generic_fn_depth > 0 while a generic specialization is cloned with a live,
	// seeded parameter scope. Ident inference should use that scope rather than scan annotations.
	cloning_generic_fn_depth  int
	specialization_node_start int = -1
	// escaping_amp_ptrs holds the names of pointer locals `p` declared as `p := &v`
	// (v a value local) whose pointer escapes the function (is returned or retained
	// in nonlocal storage). V semantics auto-heap such a `v`; v3 otherwise takes the
	// address of a stack local that dies on return. Recomputed per function (structural
	// pre-pass in transform_fn_body), consumed when the `p := &v` decl is transformed
	// (RHS rewritten to a heap copy).
	escaping_amp_ptrs map[string]bool
	// escaping_amp_sources holds the source locals `v` of such `p := &v` escapes. A returned
	// pointer local can collect more than one source through later assignments (`p = &w`);
	// each possible source whose address may leave the frame is moved to the heap at its
	// declaration so mutations before the return remain visible to the caller.
	escaping_amp_sources map[string]bool
	// heaped_amp_locals records which of those sources were actually moved to the heap, so
	// the `p := &v` alias emits `p = v` (the heap pointer) instead of a fresh memdup copy.
	heaped_amp_locals map[string]bool
	// escaping_interface_box_locals holds interface locals boxed from stack-backed pointer
	// sources and later returned. The box can alias the stack value while in scope, but its
	// `_object` needs a heap copy before the interface value leaves the frame.
	escaping_interface_box_locals map[string]bool
	// local_closure_cleanup_decls maps source declaration node ids to runtime
	// closure locals that do not escape their lexical scope.
	local_closure_cleanup_decls map[int]string
	// local_closure_cleanup_values maps individual RHS expression node ids in
	// multi-variable declarations to their non-escaping closure bindings.
	local_closure_cleanup_values map[int]string
	// local_closure_cleanup_assigns maps source assignment node ids to the exact
	// scope-owned closure binding they overwrite. Identifier spelling alone is
	// insufficient because disjoint lexical scopes may reuse the same name.
	local_closure_cleanup_assigns map[int]string
	// local_closure_field_cleanups contains assignments and aggregate initializer values that
	// create a fresh closure in a non-escaping lexical local. Their generated temporary owns the
	// allocation until that lexical scope exits.
	local_closure_field_cleanups map[int]bool
	// exclusive_closure_return_fns contains source functions proven to return a newly
	// allocated closure without storing, passing, or otherwise aliasing that closure.
	// Discarded call results may be reclaimed only for functions in this frozen pre-pass map.
	exclusive_closure_return_fns   map[string]bool
	exclusive_closure_returns_done bool
	// mut_fixed_array_capture_sources records locals captured as `mut` fixed arrays by
	// closures that are not proven local. Their storage is moved to the heap so the outer
	// binding and escaped closure context keep sharing the same durable array.
	mut_fixed_array_capture_sources    map[string]bool
	active_specialization_args         []string
	active_specialization_main_types   map[string]bool
	specialization_main_type_closures  map[string]map[string]bool
	generic_specialization_args        map[string][]string
	generic_specialization_args_log    []string
	generic_specialization_args_parent &map[string][]string = unsafe { nil }
	generic_fn_specs_in_progress       map[string]bool
	generic_fn_spec_nodes              map[string]flat.NodeId
	monomorph_cache_specs              map[string]MonomorphCacheSpec
	generic_clone_children             []flat.NodeId
	node_context_stack                 []flat.NodeId
	specialization_decl_nodes_by_name  map[string][]int
	defer_nested_generic_emissions     bool
	parallel_monomorphize              bool
	parallel_monomorph_worker          bool
	generic_signatures_pre_registered  bool
	pending_generic_fn_specs           []PendingGenericFnSpec
	pending_generic_fn_spec_keys       map[string]bool
	generic_fn_decls_cache             map[string]GenericFnDecl
	generic_receiver_methods_by_name   map[string][]string
	generic_fn_call_names              map[string]bool
	generic_fn_decls_ready             bool
	generic_struct_specs_cache         map[string]string
	generic_sum_specs_cache            map[string]GenericSpecContext
	generic_materialization_scan_from  int
	generic_materialization_ready      bool
	parallel_monomorph_scan_nodes      []int
	parallel_monomorph_scan_start      int
	parallel_monomorph_scan_end        int
	fn_scan_costs                      []int
	fn_escape_scan_flags               []u8
	literal_fn_decls                   []int
	literal_fn_decls_ready             bool
	parallel_monomorph_struct_specs    map[string]string
	parallel_monomorph_sum_specs       map[string]GenericSpecContext
	generic_call_spec_cache            map[int]GenericCallSpec
	generic_call_spec_misses           map[int]bool
	stringify_stack                    []string
	stringify_depth_cap                int = max_stringify_nesting_depth
	struct_autostr_recurse_types       map[string]bool
	str_expansion_memo                 map[string]int
	deferred_expansion_items           []FnWorkItem
	deferred_expansion_count           int
	node_module_map_cache              []string
	node_file_map_cache                []string
	node_module_map_nodes              int = -1
	node_context_read_only             bool
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
	base_write_intercept    bool
	defer_oor_writes        bool
	shared_base_nodes       int = -1
	shared_base_children    int = -1
	item_range_lo           int = -1
	item_range_hi           int = -1
	item_escape_scan_known  bool
	item_escape_scan_needed bool
	memo_node_types         bool
	node_type_memo          &NodeTypeMemo = unsafe { nil }
	deferred_base_writes    []DeferredBaseWrite
	// Prealloc self-host builds put helper-thread scratch allocations in
	// disposable arenas. The worker's surviving AST strings are cloned by the
	// master before that arena is released.
	scope_parallel_workers bool
	parallel_enabled       bool
	fast_escape_precheck   bool
	// The skip-generics body pass owns disjoint source-node ranges. Reusing a
	// source parent when its transformed child count is unchanged avoids
	// copying that parent and its child span into the append-only AST.
	inplace_child_rewrites       bool
	inplace_fn_child_rewrites    bool
	inplace_assign_rewrites      bool
	inplace_simple_rewrites      bool
	inplace_lvalue_rewrites      bool
	inplace_block_expr_rewrites  bool
	inplace_decl_assign_rewrites bool
	inplace_scalar_prefixes      bool
	lean_struct_init_fields      bool
	inplace_struct_fields        bool
	memo_call_param_type_names   bool
	memo_semantic_type_names     bool
	prefix_param_scan            bool
	// Child-only lowering preserves the parent expression's checked semantic
	// type. Keep that dense sidecar entry so later transform/cgen queries do not
	// reconstruct the same type from the lowered text tree.
	preserve_inplace_expr_types bool
	inplace_child_log           []InplaceChildRewrite
	worker_scope                voidptr
	scoped_base_nodes           int = -1
	scoped_owned_base_nodes     map[int]bool
	scoped_owned_base_log       []int
	scoped_base_log_active      bool
	scoped_promoted_texts       map[string]string
	retain_worker_results       bool
	retained_worker_regions     []ScopedTransformRegion
	stage_scope                 voidptr
	scoped_monomorphize         bool
	monomorph_worker_scopes     []voidptr
	signature_maps_shared       bool
	signature_maps_changed      bool
}

// AliasCache memoizes normalize_type_alias results. It lives on the heap so the
// many `&Transformer` (read-only) query methods can populate it through the
// pointer. normalize_type_alias is a pure function of (cur_file, cur_module, typ) plus the
// collected type maps (which never change during transform), so the cache is
// keyed by typ and cleared whenever the source context changes.
struct AliasCache {
mut:
	module             string
	file               string
	recent_generation  u32 = 1
	recent_types       [1024]string
	recent_results     [1024]string
	recent_generations [1024]u32
	canonical_types    [1024]string
	canonical_results  [1024]string
	entries            map[string]string
}

@[inline]
fn alias_cache_slot(typ string) int {
	return int((u64(voidptr(typ.str)) >> 4 ^ u64(typ.len)) & 1023)
}

// trimmed_transform_text avoids allocating a substring for the overwhelmingly
// common case where canonical type text has no surrounding whitespace.
@[direct_array_access; inline]
fn trimmed_transform_text(s string) string {
	if s.len == 0 {
		return s
	}
	first := s[0]
	last := s[s.len - 1]
	if first != ` ` && first != `\n` && first != `\t` && first != `\v` && first != `\f`
		&& first != `\r` && last != ` ` && last != `\n` && last != `\t` && last != `\v`
		&& last != `\f` && last != `\r` {
		return s
	}
	return s.trim_space()
}

@[inline]
fn (mut c AliasCache) put_recent(typ string, result string) {
	slot := alias_cache_slot(typ)
	c.recent_types[slot] = typ
	c.recent_results[slot] = result
	c.recent_generations[slot] = c.recent_generation
}

@[inline]
fn (mut c AliasCache) clear_recent() {
	c.recent_generation++
}

struct LookupCache {
mut:
	entries map[string]string
	misses  map[string]bool
}

struct ContextLookupCache {
mut:
	module  string
	file    string
	entries map[string]string
	misses  map[string]bool
}

struct SelectorTypeCache {
mut:
	generation  u32 = 1
	keys        [1024]int
	value_ptrs  [1024]voidptr
	value_lens  [1024]int
	generations [1024]u32
	results     [1024]string
}

struct ResolvedCallReturnCache {
mut:
	generation  u32 = 1
	keys        [1024]int
	value_ptrs  [1024]voidptr
	value_lens  [1024]int
	generations [1024]u32
	results     [1024]string
}

struct VariantMatchCache {
mut:
	generation  u32 = 1
	module      string
	file        string
	a_ptrs      [2048]voidptr
	b_ptrs      [2048]voidptr
	a_lens      [2048]int
	b_lens      [2048]int
	generations [2048]u32
	results     [2048]i8
}

struct BoolLookupCache {
mut:
	entries    map[string]i8 // 1 = true, -1 = false
	last_name  string
	last_value i8
}

struct ContextBoolLookupCache {
mut:
	module     string
	entries    map[string]i8 // 1 = true, -1 = false
	last_name  string
	last_value i8
}

@[inline]
fn (mut c BoolLookupCache) get(name string) i8 {
	if c.last_value != 0 && c.last_name.len == name.len
		&& (unsafe { c.last_name.str == name.str } || c.last_name == name) {
		return c.last_value
	}
	if cached := c.entries[name] {
		c.last_name = name
		c.last_value = cached
		return cached
	}
	return 0
}

@[inline]
fn (mut c BoolLookupCache) put(name string, value i8) {
	c.entries[name] = value
	c.last_name = name
	c.last_value = value
}

@[inline]
fn (mut c ContextBoolLookupCache) get(name string) i8 {
	if c.last_value != 0 && c.last_name.len == name.len
		&& (unsafe { c.last_name.str == name.str } || c.last_name == name) {
		return c.last_value
	}
	if cached := c.entries[name] {
		c.last_name = name
		c.last_value = cached
		return cached
	}
	return 0
}

@[inline]
fn (mut c ContextBoolLookupCache) put(name string, value i8) {
	c.entries[name] = value
	c.last_name = name
	c.last_value = value
}

struct VarTypeIndexCache {
mut:
	name   string
	index  int = -1
	name2  string
	index2 int = -1
	name3  string
	index3 int = -1
	name4  string
	index4 int = -1
}

@[inline]
fn (mut c VarTypeIndexCache) put(name string, index int) {
	c.name4 = c.name3
	c.index4 = c.index3
	c.name3 = c.name2
	c.index3 = c.index2
	c.name2 = c.name
	c.index2 = c.index
	c.name = name
	c.index = index
}

@[inline]
fn (mut c VarTypeIndexCache) clear() {
	c.name = ''
	c.index = -1
	c.name2 = ''
	c.index2 = -1
	c.name3 = ''
	c.index3 = -1
	c.name4 = ''
	c.index4 = -1
}

// GenericUnresolvedCache memoizes generic_arg_is_unresolved results. Lives on
// the heap so `&Transformer` query methods can populate it; keyed by type text
// and cleared on module switch (resolution consults module-qualified names).
struct GenericUnresolvedCache {
mut:
	module     string
	entries    map[string]i8 // 1 = unresolved, -1 = resolved
	last_name  string
	last_value i8
}

@[inline]
fn (mut c GenericUnresolvedCache) get(name string) i8 {
	if c.last_value != 0 && c.last_name.len == name.len
		&& (unsafe { c.last_name.str == name.str } || c.last_name == name) {
		return c.last_value
	}
	if cached := c.entries[name] {
		c.last_name = name
		c.last_value = cached
		return cached
	}
	return 0
}

@[inline]
fn (mut c GenericUnresolvedCache) put(name string, value i8) {
	c.entries[name] = value
	c.last_name = name
	c.last_value = value
}

// ReceiverMethodCache memoizes resolve_receiver_method_for_type results. Lives
// on the heap so `&Transformer` query methods can populate it; keyed by
// `type\nmethod`, cleared on module switch (candidate construction consults
// cur_module) and whenever the transformer registers new fn signatures
// (closure lifting / synthesized helpers can change name resolution).
struct ReceiverMethodCache {
mut:
	module   string
	fn_count int
	entries  map[string]string
	misses   map[string]bool
}

// PromoteTextCache short-circuits repeated promote_scoped_result_text calls on
// the SAME string instance (shallow string copies share `.str`, so one batch
// promotes the same pointer thousands of times). Pointer keys are only valid
// while the source scope is alive, so the cache only serves lookups inside an
// explicit window (absorb_scoped_batch) that ends before the scope is freed;
// outside a window promote falls back to the content-hash tables.
struct PromoteTextCache {
mut:
	active      bool
	generation  u32 = 1
	ptrs        [2048]voidptr
	generations [2048]u32
	results     [2048]string
}

// begin_promote_text_window arms the pointer cache for one promotion pass over
// a still-live scope. end_promote_text_window MUST run before that scope is
// freed; unpaired promote calls simply bypass the cache.
@[inline]
fn (mut t Transformer) begin_promote_text_window() {
	if !isnil(t.promote_text_cache) {
		t.promote_text_cache.generation++
		t.promote_text_cache.active = true
	}
}

@[inline]
fn (mut t Transformer) end_promote_text_window() {
	if !isnil(t.promote_text_cache) {
		t.promote_text_cache.active = false
	}
}

// StructInfo stores struct info metadata used by transform.
pub struct StructInfo {
pub:
	name       string
	module     string
	is_params  bool
	is_aligned bool
	alignment  string
	fields     []FieldInfo
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
	name            string
	typ             string
	raw_typ         string
	is_implicit_err bool
}

struct BoundMethodArrayInfo {
	receiver_type string
	fn_type       string
	method        string
	return_type   string
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

// ScopedTransformRegion describes AST payloads owned by one retained worker
// arena. The driver canonicalizes this region before releasing the arena.
pub struct ScopedTransformRegion {
pub:
	scope      voidptr
	new_start  int
	new_end    int
	base_nodes []int
}

// --- entry point ---

// PreparedSelfhostTransform owns the read-only transformer indexes built while
// markused walks the same checked AST on the driver thread.
@[heap]
pub struct PreparedSelfhostTransform {
mut:
	transformer Transformer
	scope       voidptr
	ready       bool
}

// take_scope transfers ownership of the preparation arena to the driver. The
// arena can back lowered AST text, so it must remain alive through codegen.
pub fn (mut prepared PreparedSelfhostTransform) take_scope() voidptr {
	scope := prepared.scope
	prepared.scope = unsafe { nil }
	return scope
}

// prepare_selfhost_transform builds the immutable transform indexes on a
// helper-local arena. The caller keeps the returned value alive until
// transform_prepared_selfhost_owned consumes it.
pub fn prepare_selfhost_transform(a &flat.FlatAst, tc &types.TypeChecker, enabled bool) &PreparedSelfhostTransform {
	mut prepared := &PreparedSelfhostTransform{}
	if !enabled {
		return prepared
	}
	scope := transform_worker_scope_begin(true)
	prep_tc := tc.fork_for_parallel_transform(a)
	mut t := new_transformer_view(a, prep_tc, map[string]bool{})
	configure_transformer(mut t, true, true, true, true, false, unsafe { nil })
	t.prepare_with_pre_scans()
	// This whole-AST scan is independent of reachability and otherwise leaves
	// the persistent worker pool idle at the start of transform.
	t.collect_exclusive_closure_return_fns()
	prepared = &PreparedSelfhostTransform{
		transformer: t
		scope: scope
		ready: true
	}
	transform_worker_scope_leave(scope)
	return prepared
}

// transform_prepared_selfhost_owned completes a self-host transform whose
// immutable indexes were prepared concurrently with markused.
pub fn transform_prepared_selfhost_owned(mut prepared PreparedSelfhostTransform, mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, stage_scope voidptr) (map[string]bool, bool, []string, []int, []ScopedTransformRegion) {
	if !prepared.ready {
		return transform_with_used_opt_config_scoped_workers_checked_impl(mut a, tc, used_fns, true, true, true, true, true, stage_scope)
	}
	mut t := &prepared.transformer
	t.a = unsafe { &a }
	t.tc = unsafe { tc }
	t.used_fns = used_fns.clone()
	configure_transformer(mut t, true, true, true, true, true, stage_scope)
	augmented, was_parallel, errors, owned_base_nodes, retained_regions := transform_after_prepare(mut t, mut a, used_fns, true, true)
	prepared.ready = false
	return augmented, was_parallel, errors, owned_base_nodes, retained_regions
}

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
	return transform_with_used_opt_config_scoped_workers(mut a, tc, used_fns, want_parallel, skip_generics, false)
}

// transform_with_used_opt_config_scoped_workers optionally gives parallel
// helpers disposable prealloc arenas. It is used by prealloc self-host builds
// to retain parallel latency without retaining every helper's scratch memory.
pub fn transform_with_used_opt_config_scoped_workers(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool, scope_parallel_workers bool) (map[string]bool, bool) {
	augmented, was_parallel, _ := transform_with_used_opt_config_scoped_workers_checked(mut a, tc, used_fns, want_parallel, skip_generics, scope_parallel_workers, false)
	return augmented, was_parallel
}

// transform_with_used_opt_config_scoped_workers_checked also returns diagnostics selected while
// normal comptime reflection loops are unrolled.
pub fn transform_with_used_opt_config_scoped_workers_checked(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool, scope_parallel_workers bool, building_v bool) (map[string]bool, bool, []string) {
	augmented, was_parallel, errors, _, _ := transform_with_used_opt_config_scoped_workers_checked_impl(mut a, tc, used_fns, want_parallel, skip_generics, scope_parallel_workers, building_v, false, unsafe { nil })
	return augmented, was_parallel, errors
}

// transform_with_used_opt_config_scoped_workers_checked_owned additionally
// reports source AST nodes whose owned payloads were replaced during a scoped
// transform, so the driver can promote exactly those escaping values.
pub fn transform_with_used_opt_config_scoped_workers_checked_owned(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool, scope_parallel_workers bool, building_v bool, stage_scope voidptr) (map[string]bool, bool, []string, []int, []ScopedTransformRegion) {
	return transform_with_used_opt_config_scoped_workers_checked_impl(mut a, tc, used_fns, want_parallel, skip_generics, scope_parallel_workers, building_v, true, stage_scope)
}

// transform_selected_functions lowers only the named function bodies after the
// incremental driver has proved that declarations and every other body are
// unchanged. Whole-program interface and comptime scans remain lazy: lowering a
// selected body still triggers them if that body actually needs the metadata.
// It also returns synthesized helper names that incremental Cgen must emit.
pub fn transform_selected_functions(mut a flat.FlatAst, tc &types.TypeChecker, selected map[string]bool) (map[string]bool, []string, []string) {
	mut t := new_transformer(mut a, tc, selected)
	t.skip_generics = true
	t.prepare()
	t.collect_exclusive_closure_return_fns()
	base_node_count := t.a.nodes.len
	t.transformed_fns = []bool{len: t.a.nodes.len}
	for i in 0 .. t.a.nodes.len {
		node := t.a.nodes[i]
		if node.kind == .file {
			t.cur_file = node.value
			t.cur_module = t.tc.file_modules[node.value] or { '' }
			continue
		}
		if node.kind == .module_decl {
			t.cur_module = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		qname := if t.cur_module in ['', 'main', 'builtin'] {
			node.value
		} else {
			'${t.cur_module}.${node.value}'
		}
		if !selected[qname] && !selected[node.value] {
			continue
		}
		t.transform_fn_body(i)
	}
	t.apply_ignored_comptime_for_nodes()
	t.run_default_clone_synthesis_rounds(base_node_count)
	t.run_sum_eq_synthesis_rounds(base_node_count)
	t.apply_ignored_comptime_for_nodes()
	mut synthesized_helpers := []string{}
	for idx in base_node_count .. t.a.nodes.len {
		node := t.a.nodes[idx]
		if node.kind == .fn_decl && (node.value.starts_with('__v3_sum_eq_')
			|| node.value.starts_with('__v3_default_clone_')) {
			synthesized_helpers << node.value
		}
	}
	return t.used_fns, t.monomorph_errors, synthesized_helpers
}

fn transform_with_used_opt_config_scoped_workers_checked_impl(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, want_parallel bool, skip_generics bool, scope_parallel_workers bool, building_v bool, retain_worker_results bool, stage_scope voidptr) (map[string]bool, bool, []string, []int, []ScopedTransformRegion) {
	mut impl_sw := time.new_stopwatch()
	mut t := new_transformer(mut a, tc, used_fns)
	configure_transformer(mut t, want_parallel, skip_generics, scope_parallel_workers, building_v, retain_worker_results, stage_scope)
	t.prepare_with_pre_scans()
	t.timing_profile('  [ttime] new+prepare        ${f64(impl_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	return transform_after_prepare(mut t, mut a, used_fns, want_parallel, skip_generics)
}

fn configure_transformer(mut t Transformer, want_parallel bool, skip_generics bool, scope_parallel_workers bool, building_v bool, retain_worker_results bool, stage_scope voidptr) {
	t.skip_generics = skip_generics
	t.building_v = building_v
	t.memo_node_types = building_v && os.getenv('V3_NO_NODE_TYPE_MEMO') == ''
	t.fast_escape_precheck = building_v && os.getenv('V3_NO_ESCAPE_PRECHECK') == ''
	t.inplace_child_rewrites = building_v && os.getenv('V3_NO_INPLACE_TRANSFORM_CHILDREN') == ''
	t.inplace_fn_child_rewrites = t.inplace_child_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_FN_CHILDREN') == ''
	t.inplace_assign_rewrites = t.inplace_child_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_ASSIGN_CHILDREN') == ''
	t.inplace_simple_rewrites = t.inplace_child_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_SIMPLE_CHILDREN') == ''
	t.inplace_lvalue_rewrites = t.inplace_simple_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_LVALUE_CHILDREN') == ''
	t.inplace_block_expr_rewrites = t.inplace_simple_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_BLOCK_EXPR_CHILDREN') == ''
	t.inplace_decl_assign_rewrites = t.inplace_child_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_DECL_ASSIGN_CHILDREN') == ''
	t.inplace_scalar_prefixes = t.inplace_simple_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_SCALAR_PREFIXES') == ''
	t.lean_struct_init_fields = building_v && os.getenv('V3_NO_LEAN_TRANSFORM_STRUCT_FIELDS') == ''
	t.inplace_struct_fields = t.inplace_child_rewrites
		&& os.getenv('V3_NO_INPLACE_TRANSFORM_STRUCT_FIELDS') == ''
	t.memo_call_param_type_names = building_v && os.getenv('V3_NO_TRANSFORM_TYPE_NAME_MEMO') == ''
	t.memo_semantic_type_names = building_v && os.getenv('V3_TRANSFORM_TYPE_NAME_MEMO_ALL') != ''
	t.prefix_param_scan = building_v && os.getenv('V3_NO_PREFIX_PARAM_SCAN') == ''
	t.preserve_inplace_expr_types = t.inplace_child_rewrites
		&& os.getenv('V3_NO_PRESERVE_INPLACE_EXPR_TYPES') == ''
	t.scope_parallel_workers = scope_parallel_workers
	t.parallel_enabled = want_parallel
	t.retain_worker_results = retain_worker_results
	t.stage_scope = stage_scope
	if scope_parallel_workers {
		t.scoped_base_nodes = t.a.nodes.len
	}
}

fn transform_after_prepare(mut t Transformer, mut a flat.FlatAst, _used_fns map[string]bool, want_parallel bool, skip_generics bool) (map[string]bool, bool, []string, []int, []ScopedTransformRegion) {
	mut impl_sw := time.new_stopwatch()
	t.cache_comptime_param_reflection_metadata()
	if want_parallel {
		reserve_parallel_transform_ast(mut a, skip_generics)
	}
	t.timing_profile('  [ttime] reflect+reserve    ${f64(impl_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	impl_sw.restart()
	base_node_count := t.a.nodes.len
	if t.scope_parallel_workers {
		t.scoped_base_nodes = base_node_count
	}
	t.transformed_fns = []bool{len: t.a.nodes.len}
	used_log_was_active := t.used_fns_log_active
	used_log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	was_parallel := t.transform_all_dispatch(want_parallel)
	impl_sw.restart()
	t.retain_current_worker_scope_all()
	t.apply_ignored_comptime_for_nodes()
	// The late-name scan backfills call names that raw-AST markused could not
	// resolve before narrowing and other type-aware lowering. This is needed for
	// non-generic programs too: a call on a narrowed sum-type variant can become a
	// concrete primitive method only after transform.
	used_log_end := t.used_fns_log.len
	mut late_scan_names := []string{}
	if !t.building_v {
		// Interface implementers can become reachable while their interface calls
		// are transformed. Include them in the type-aware call scan so dependencies
		// from their already-transformed bodies are queued too.
		late_candidate_names := t.late_transform_candidate_name_filter(base_node_count)
		late_scan_names = t.new_call_names_from_used_fn_bodies(unsafe { &t.used_fns }, &late_candidate_names, t.a.nodes.len)
	}
	t.timing_profile('  [ttime] late names         ${f64(impl_sw.elapsed().microseconds()) / 1000.0:7.2f} ms (n: ${used_log_end - used_log_start + late_scan_names.len})')
	impl_sw.restart()
	t.transform_late_used_fn_bodies(&t.used_fns_log, used_log_start, used_log_end, base_node_count)
	if late_scan_names.len > 0 {
		t.transform_late_used_fn_bodies(&late_scan_names, 0, late_scan_names.len, base_node_count)
	}
	t.timing_profile('  [ttime] late bodies        ${f64(impl_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	impl_sw.restart()
	t.run_auto_str_synthesis_rounds(base_node_count)
	t.run_default_clone_synthesis_rounds(base_node_count)
	t.run_sum_eq_synthesis_rounds(base_node_count)
	t.apply_ignored_comptime_for_nodes()
	t.retain_current_worker_scope_all()
	t.timing_profile('  [ttime] sum_eq+tail        ${f64(impl_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	mut owned_base_nodes := t.scoped_owned_base_nodes.keys()
	owned_base_nodes << t.scoped_owned_base_log
	// The per-item resolve memo was allocated inside this stage's disposable
	// arena; drop the master checker's pointer before the driver releases it.
	if !isnil(t.tc) {
		t.tc.reset_body_resolve_memo()
	}
	t.used_fns_log_active = used_log_was_active
	return t.used_fns, was_parallel, t.monomorph_errors, owned_base_nodes, t.retained_worker_regions
}

fn (mut t Transformer) retain_current_worker_scope_all() {
	if !t.retain_worker_results || t.worker_scope == unsafe { nil } {
		return
	}
	if !t.retained_worker_regions.any(it.scope == t.worker_scope) {
		mut base_nodes := t.scoped_owned_base_nodes.keys()
		base_nodes << t.scoped_owned_base_log
		t.retained_worker_regions << ScopedTransformRegion{
			scope: t.worker_scope
			new_start: 0
			new_end: t.a.nodes.len
			base_nodes: base_nodes
		}
	}
	t.worker_scope = unsafe { nil }
}

// reserve_parallel_transform_ast reserves persistent append regions before a
// disposable transform arena is entered.
pub fn reserve_parallel_transform_ast(mut a flat.FlatAst, skip_generics bool) {
	reserve_parallel_transform_ast_with_cache_mode(mut a, skip_generics, false)
}

// reserve_parallel_transform_cache_ast adds overflow headroom for cache population,
// which transforms every module body rather than just the program-reachable subset.
pub fn reserve_parallel_transform_cache_ast(mut a flat.FlatAst, skip_generics bool) {
	reserve_parallel_transform_ast_with_cache_mode(mut a, skip_generics, true)
}

fn reserve_parallel_transform_ast_with_cache_mode(mut a flat.FlatAst, skip_generics bool, cache_mode bool) {
	// The shared-base parallel path partitions this headroom between workers.
	// Generic lowering needs more headroom than self-host transform because worker
	// regions cannot grow while their disposable arenas are active. The shared path
	// admits at most half of this pool's estimated growth, leaving the other half for
	// uneven per-chunk expansion without retaining a 3x/4x slab for large programs.
	// Per-chunk cost-proportional slices amplify local growth outliers, so the
	// capacities still stay well above the observed whole-program growth.
	nodes_factor_num, nodes_factor_den := if skip_generics {
		if cache_mode { 5, 2 } else { 9, 4 }
	} else {
		9, 4
	}
	children_factor_num, children_factor_den := if skip_generics {
		if cache_mode { 3, 1 } else { 8, 3 }
	} else {
		3, 1
	}
	nodes_cap := a.nodes.len * nodes_factor_num / nodes_factor_den
	children_cap := a.children.len * children_factor_num / children_factor_den
	grow_nodes := nodes_cap > a.nodes.cap
	grow_children := children_cap > a.children.cap
	// The two grown arrays copy ~150MB of stable payload; shard the copies
	// across helper threads (the worker pool is idle before dispatch).
	if grow_nodes && grow_children && a.nodes.len >= 262_144 {
		old_nodes := a.nodes
		mut grown_nodes := []flat.Node{cap: nodes_cap}
		old_children := a.children
		mut grown_children := []flat.NodeId{cap: children_cap}
		unsafe {
			grown_nodes.grow_len(old_nodes.len)
			grown_children.grow_len(old_children.len)
		}
		third := old_nodes.len / 3
		copies := [
			TransformByteCopy{
				dst: unsafe { voidptr(&grown_children[0]) }
				src: unsafe { voidptr(&old_children[0]) }
				bytes: u64(old_children.len) * u64(old_children.element_size)
			},
			TransformByteCopy{
				dst: unsafe { voidptr(&grown_nodes[0]) }
				src: unsafe { voidptr(&old_nodes[0]) }
				bytes: u64(third) * u64(old_nodes.element_size)
			},
			TransformByteCopy{
				dst: unsafe { voidptr(&grown_nodes[third]) }
				src: unsafe { voidptr(&old_nodes[third]) }
				bytes: u64(third) * u64(old_nodes.element_size)
			},
		]
		copy_thread0 := spawn transform_byte_copy_thread(unsafe { voidptr(&copies[0]) })
		copy_thread1 := spawn transform_byte_copy_thread(unsafe { voidptr(&copies[1]) })
		copy_thread2 := spawn transform_byte_copy_thread(unsafe { voidptr(&copies[2]) })
		tail_start := third * 2
		unsafe {
			vmemcpy(voidptr(&grown_nodes[tail_start]), voidptr(&old_nodes[tail_start]), isize(u64(old_nodes.len - tail_start) * u64(old_nodes.element_size)))
		}
		_ = copy_thread0.wait()
		_ = copy_thread1.wait()
		_ = copy_thread2.wait()
		a.nodes = grown_nodes
		a.file_node_ids = []int{}
		a.children = grown_children
		return
	}
	if grow_nodes {
		old_nodes := a.nodes
		a.nodes = []flat.Node{cap: nodes_cap}
		a.file_node_ids = []int{}
		a.nodes << old_nodes
	}
	if grow_children {
		old_children := a.children
		a.children = []flat.NodeId{cap: children_cap}
		a.children << old_children
	}
}

// TransformByteCopy is one shard of a large stable-payload array copy.
struct TransformByteCopy {
	dst   voidptr
	src   voidptr
	bytes u64
}

fn transform_byte_copy_thread(arg voidptr) voidptr {
	c := unsafe { &TransformByteCopy(arg) }
	unsafe { vmemcpy(c.dst, c.src, isize(c.bytes)) }
	return unsafe { nil }
}

// transform_worker_scope_begin starts a helper-local disposable arena in
// prealloc self-host builds. Ordinary builds retain their existing allocator.
fn transform_worker_scope_begin(enabled bool) voidptr {
	$if prealloc {
		if enabled {
			return unsafe { prealloc_scope_begin() }
		}
	}
	return unsafe { nil }
}

fn transform_worker_scope_leave(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_leave(scope) }
		}
	}
}

fn transform_worker_scope_free(scope voidptr) {
	$if prealloc {
		if scope != unsafe { nil } {
			unsafe { prealloc_scope_free_after(scope) }
		}
	}
}

fn transform_stage_scope_suspend(scope voidptr) voidptr {
	$if prealloc {
		if scope != unsafe { nil } {
			return unsafe { prealloc_scope_suspend(scope) }
		}
	}
	return unsafe { nil }
}

fn transform_stage_scope_resume(scope voidptr, state voidptr) {
	$if prealloc {
		if scope != unsafe { nil } && state != unsafe { nil } {
			unsafe { prealloc_scope_resume(scope, state) }
		}
	}
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
		t.transform_late_used_fn_bodies(&new_names, 0, new_names.len, node_limit)
	}
}

fn (mut t Transformer) run_default_clone_synthesis_rounds(node_limit int) {
	for _ in 0 .. 16 {
		new_names := t.synthesize_default_clone_helpers()
		if new_names.len == 0 {
			return
		}
		t.transform_late_used_fn_bodies(&new_names, 0, new_names.len, node_limit)
	}
}

fn (mut t Transformer) run_auto_str_synthesis_rounds(node_limit int) {
	for _ in 0 .. 16 {
		new_names := t.synthesize_auto_str_helpers()
		if new_names.len > 0 {
			t.transform_late_used_fn_bodies(&new_names, 0, new_names.len, node_limit)
		}
		if !t.has_pending_auto_str_helpers() {
			return
		}
	}
}

fn (mut t Transformer) new_call_names_from_used_fn_bodies(used &map[string]bool, candidate_names &map[string]bool, node_limit int) []string {
	if used.len == 0 || node_limit <= 0 {
		return []string{}
	}
	limit := if node_limit < t.a.nodes.len { node_limit } else { t.a.nodes.len }
	cands := t.collect_late_scan_candidates(limit)
	return t.scan_late_call_names_dispatch(cands, used, candidate_names)
}

fn (mut t Transformer) late_transform_candidate_name_filter(node_limit int) map[string]bool {
	mut names := map[string]bool{}
	limit := if node_limit < t.a.nodes.len { node_limit } else { t.a.nodes.len }
	for cand in t.collect_late_scan_candidates(limit) {
		if (cand.idx < t.transformed_fns.len && t.transformed_fns[cand.idx])
			|| t.fn_decl_has_unresolved_generics(t.a.nodes[cand.idx], cand.module) {
			continue
		}
		for key in late_candidate_match_keys(t.a.nodes[cand.idx].value, cand.module) {
			names[key] = true
		}
	}
	return names
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
		kind_id := int(node.kind)
		if kind_id == 77 {
			cur_file = node.value
			cur_module = ''
		} else if kind_id == 73 {
			cur_module = node.value
		} else if kind_id == 61 {
			cands << LateFnCandidate{
				idx: i
				file: cur_file
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
fn (mut t Transformer) scan_late_call_names_range(cands []LateFnCandidate, used &map[string]bool, candidate_names &map[string]bool, start int, end int) []string {
	mut names := []string{}
	old_module := t.cur_module
	old_file := t.cur_file
	for ci in start .. end {
		cand := cands[ci]
		node := t.a.nodes[cand.idx]
		if t.fn_decl_has_unresolved_generics(node, cand.module) {
			continue
		}
		if !transform_is_generated_fn_after_markused(node.value)
			&& !late_used_fn_matches(*used, node, cand.module) {
			continue
		}
		t.cur_file = cand.file
		t.cur_module = cand.module
		for call_name in t.generated_fn_body_candidate_call_names(flat.NodeId(cand.idx), candidate_names) {
			if call_name.len == 0 || (!(*candidate_names)[call_name]
				&& !t.used_struct_operator_fns[call_name]
				&& !t.late_name_may_expand_interface(call_name)) {
				continue
			}
			if (*used)[call_name] || (*used)[c_name(call_name)]
				|| late_used_fn_contains_in_module(*used, call_name, cand.module) {
				continue
			}
			names << call_name
		}
	}
	t.cur_module = old_module
	t.cur_file = old_file
	return names
}

fn (t &Transformer) monomorph_profile(message string) {
	t.timing_profile(message)
}

fn (t &Transformer) timing_profile(message string) {
	if !isnil(t.tc) && t.tc.verbose {
		eprintln(message)
	}
}

pub fn monomorphize_with_used(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	result, _ := monomorphize_with_used_checked(mut a, tc, used_fns)
	return result
}

// monomorphize_with_used_checked also reports semantic errors that can only be
// resolved after generic parameters have concrete types.
pub fn monomorphize_with_used_checked(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) (map[string]bool, []string) {
	return monomorphize_with_used_checked_config(mut a, tc, used_fns, true)
}

// monomorphize_with_used_checked_config controls whether independent generic
// specializations can be cloned concurrently.
pub fn monomorphize_with_used_checked_config(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, parallel bool) (map[string]bool, []string) {
	return monomorphize_with_used_checked_config_scoped(mut a, tc, used_fns, parallel, unsafe { nil })
}

// monomorphize_with_used_checked_config_scoped keeps transient specialization
// state in `stage_scope` while promoting escaping AST payloads directly to its
// parent arena.
pub fn monomorphize_with_used_checked_config_scoped(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, parallel bool, stage_scope voidptr) (map[string]bool, []string) {
	result, errors, _ := monomorphize_with_used_checked_config_scoped_cached(mut a, tc, used_fns, parallel, stage_scope, []MonomorphCacheSpec{})
	return result, errors
}

// monomorphize_with_used_checked_config_scoped_cached restores concrete generic
// signatures from `cached_specs` and returns the complete specialization set.
// A restored signature rewrites current program calls normally, while its
// unchanged dependency body can remain in the persistent compiled prefix.
pub fn monomorphize_with_used_checked_config_scoped_cached(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, parallel bool, stage_scope voidptr, cached_specs []MonomorphCacheSpec) (map[string]bool, []string, []MonomorphCacheSpec) {
	debug_started := time.ticks()
	mut t := new_transformer(mut a, tc, used_fns)
	t.parallel_monomorphize = parallel
	t.stage_scope = stage_scope
	t.scoped_monomorphize = stage_scope != unsafe { nil }
	$if prealloc {
		t.scope_parallel_workers = true
	}
	t.prepare()
	// Interface conversions are explicit struct-init nodes after transform. Scan
	// only that lowered form here; the semantic source scan already ran in the
	// preceding transform and would be far more expensive on the enlarged AST.
	t.interface_boxed_types_done = true
	t.interface_boxed_types_frozen = false
	t.collect_lowered_interface_boxed_types_range(0, t.a.nodes.len)
	t.interface_boxed_types_frozen = true
	t.refresh_interface_impl_indexes_for_boxed_types()
	t.seed_cached_monomorph_specs(cached_specs)
	t.monomorph_profile('mono wrapper prepare: ${time.ticks() - debug_started} ms')
	base_node_count := t.a.nodes.len
	mut late_log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	// A specialization can make a previously dead non-generic body reachable, and
	// transforming that body can expose another generic call. Keep the templates
	// intact and alternate both passes until late reachability stops growing.
	for {
		generated_names := t.monomorphize_pass()
		t.materialize_monomorph_signature_types(t.sorted_monomorph_cache_specs())
		t.monomorph_profile('mono wrapper pass: ${time.ticks() - debug_started} ms')
		for name in generated_names {
			t.mark_used_fn_key(name)
			t.mark_used_fn_key(c_name(name))
		}
		t.materialize_generic_structs(false)
		t.monomorph_profile('mono wrapper generated: ${time.ticks() - debug_started} ms')
		// generated_fn_used_names records callees while each specialization is
		// transformed. Seed the late-body queue from those new names; that queue
		// follows further callees recursively as it transforms each body.
		t.monomorph_profile('mono wrapper calls: ${time.ticks() - debug_started} ms')
		used_after_pass := t.used_fn_count()
		late_log_end := t.used_fns_log.len
		t.transform_late_used_fn_bodies(&t.used_fns_log, late_log_start, late_log_end, base_node_count)
		late_log_start = t.used_fns_log.len
		t.monomorph_profile('mono wrapper late: ${time.ticks() - debug_started} ms')
		remaining_match_log_start := t.used_fns_log.len
		t.lower_remaining_matches_in_used_fns()
		t.monomorph_profile('mono wrapper matches: ${time.ticks() - debug_started} ms')
		remaining_match_log_end := t.used_fns_log.len
		t.transform_late_used_fn_bodies(&t.used_fns_log, remaining_match_log_start, remaining_match_log_end, base_node_count)
		late_log_start = t.used_fns_log.len
		t.monomorph_profile('mono wrapper late matches: ${time.ticks() - debug_started} ms')
		// Late-body transformation requests concrete generic work immediately. If
		// reachability grew but queued no specialization, another whole-AST pass
		// cannot materialize anything new.
		if t.used_fn_count() == used_after_pass || t.pending_generic_fn_specs.len == 0 {
			break
		}
	}
	t.erase_generic_fn_decls(t.cached_generic_fn_decls())
	t.materialize_generic_structs(true)
	t.monomorph_profile('mono wrapper structs: ${time.ticks() - debug_started} ms')
	t.run_auto_str_synthesis_rounds(base_node_count)
	t.run_default_clone_synthesis_rounds(base_node_count)
	t.run_sum_eq_synthesis_rounds(base_node_count)
	t.monomorph_profile('mono wrapper sums: ${time.ticks() - debug_started} ms')
	t.apply_ignored_comptime_for_nodes()
	t.monomorph_profile('mono wrapper ignored: ${time.ticks() - debug_started} ms')
	final_specs := t.sorted_monomorph_cache_specs()
	if stage_scope != unsafe { nil } {
		parent_state := transform_stage_scope_suspend(stage_scope)
		// Rebuild the canonical table outside the disposable monomorph arena, then
		// publish all worker-owned node strings while their arenas are still live.
		t.a.promote_transform_texts_from(0, stage_scope)
		t.monomorph_profile('mono wrapper text table: ${time.ticks() - debug_started} ms')
		t.release_monomorph_worker_scopes()
		transform_stage_scope_resume(stage_scope, parent_state)
	} else {
		t.release_monomorph_worker_scopes()
	}
	return t.used_fns, t.monomorph_errors, final_specs
}

fn (mut t Transformer) release_monomorph_worker_scopes() {
	started := time.ticks()
	// This is also the final monomorph text-publication barrier when no helper
	// scope was retained. Keep it unconditional so the driver does not need a
	// second full-AST canonicalization pass after this function returns.
	t.a.intern_node_texts_from(0)
	t.monomorph_profile('mono wrapper intern: ${time.ticks() - started} ms')
	if !free_worker_scopes_parallel(t.a, t.monomorph_worker_scopes) {
		for scope in t.monomorph_worker_scopes {
			if scope != unsafe { nil } {
				transform_worker_scope_free(scope)
			}
		}
	}
	t.monomorph_worker_scopes = []voidptr{}
	t.monomorph_profile('mono wrapper release: ${time.ticks() - started} ms')
}

// register_cached_monomorph_signatures installs persistent concrete signatures
// and their named signature types before the disposable monomorphization arena
// is entered.
pub fn register_cached_monomorph_signatures(a &flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool, cached_specs []MonomorphCacheSpec) {
	if cached_specs.len == 0 {
		return
	}
	mut t := new_transformer_view(a, tc, used_fns)
	t.prepare()
	t.seed_cached_monomorph_specs(cached_specs)
	t.materialize_monomorph_signature_types(cached_specs)
}

fn new_transformer(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) Transformer {
	return new_transformer_view(a, tc, used_fns)
}

// new_transformer_view creates a transformer with private mutable state over an
// existing AST. It is also the baseline for parallel workers: worker forks add
// only the frozen program indexes they are allowed to share.
fn new_transformer_view(a &flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) Transformer {
	// Map fields not listed here are auto-initialized to empty maps by V; only
	// the non-default fields (the AST/type-checker views and the pointer-backed
	// lookup caches, which would otherwise be nil) need explicit values.
	return Transformer{
		a: a
		tc: unsafe { tc }
		has_spawn_expr: tc.threads_condition_value()
		used_fns: used_fns.clone()
		interface_box_param_cache: &BoolLookupCache{
			entries: map[string]i8{}
		}
		alias_receiver_method_cache: &LookupCache{
			entries: map[string]string{}
			misses: map[string]bool{}
		}
		call_variadic_cache: &BoolLookupCache{
			entries: map[string]i8{}
		}
		str_alias_cache: &LookupCache{
			entries: map[string]string{}
			misses: map[string]bool{}
		}
	}
}

fn (mut t Transformer) mark_fn_used_name(name string) {
	if name.len == 0 {
		return
	}
	if !t.used_fn_contains_name(name) && !t.used_fn_contains_name(c_name(name))
		&& t.enum_method_name_shadows_field(name) {
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

fn (t &Transformer) enum_method_name_shadows_field(name string) bool {
	if !name.contains('.') {
		return false
	}
	receiver := name.all_before_last('.')
	field := name.all_after_last('.')
	enum_name := t.enum_type_name_from_selector_name(receiver) or { return false }
	fields := t.enum_types[enum_name] or { return false }
	return field in fields
}

fn (mut t Transformer) mark_struct_operator_used_name(name string) {
	if name.len == 0 {
		return
	}
	t.used_struct_operator_fns[name] = true
	t.used_struct_operator_fns[c_name(name)] = true
	if name.starts_with('main.') {
		short := name['main.'.len..]
		t.used_struct_operator_fns[short] = true
		t.used_struct_operator_fns[c_name(short)] = true
	}
	if name.contains('[') && name.contains(']') {
		return
	}
	if !name.contains('__') {
		t.mark_fn_used_name(name)
	}
}

// mark_used_fn_key inserts one spelling into used_fns, recording first-time
// insertions in used_fns_log while the late-used-fn-bodies pass is running.
fn (mut t Transformer) mark_used_fn_key(key string) {
	if t.used_fn_contains_name(key) {
		return
	}
	if t.used_fns_log_active {
		t.used_fns_log << key
	}
	t.used_fns[key] = true
}

fn (t &Transformer) has_any_used_fns() bool {
	return t.used_fns.len > 0
		|| (!isnil(t.used_fns_parent) && t.used_fns_parent.len > 0)
		|| (!isnil(t.used_fns_root) && t.used_fns_root.len > 0)
}

fn (t &Transformer) used_fn_count() int {
	return t.used_fns.len + if isnil(t.used_fns_parent) {
		0
	} else {
		t.used_fns_parent.len
	} + if isnil(t.used_fns_root) {
		0
	} else {
		t.used_fns_root.len
	}
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
	// Optional experimental override of the autostr nesting cap; see
	// max_stringify_nesting_depth. Left in place as a runtime knob for tuning the
	// combinatorial autostr expansion without a recompile.
	cap_env := os.getenv('V3_STR_CAP')
	if cap_env != '' {
		t.stringify_depth_cap = cap_env.int()
	}
	mut psw := time.new_stopwatch()
	t.collect_types()
	t.rebuild_struct_autostr_recurse_index()
	if !t.defer_pre_scan_indexes {
		t.build_source_parent_index()
	}
	t.timing_profile('  [ttime]   prep collect_types ${f64(psw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	psw.restart()
	t.rebuild_embedded_fields_index()
	t.prepare_runtime_type_indexes()
	t.rebuild_struct_short_name_index()
	t.rebuild_non_main_type_short_names()
	if !t.defer_pre_scan_indexes {
		t.collect_multi_return_fn_ret_types()
	}
	t.collect_const_suffixes()
	t.timing_profile('  [ttime]   prep small idx     ${f64(psw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	psw.restart()
	// Alias normalization consults the type maps populated by collect_types.
	// Keep it on the owning thread after those maps are complete; unlike the
	// AST/tc-only indexes, it is not safe to overlap with type-map construction
	// on a shared Transformer.
	t.collect_alias_methods()
	t.rebuild_receiver_method_suffix_index()
	if !t.defer_pre_scan_indexes {
		t.rebuild_variadic_suffix_index()
	}
	t.build_generic_alias_name_index()
	t.build_type_alias_suffix_index()
	t.build_struct_field_decl_metas_cache()
	t.timing_profile('  [ttime]   prep suffix+decl   ${f64(psw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	psw.restart()
	// Enable the alias cache only now that the type maps are fully populated.
	// During collection those maps are incomplete, so caching there would poison
	// entries with results computed against a partial view.
	t.alias_cache = &AliasCache{}
	t.sum_cache = &AliasCache{}
	t.module_type_cache = &AliasCache{}
	t.struct_guess_cache = &AliasCache{}
	t.var_type_cache = &VarTypeIndexCache{}
	t.generic_unresolved_cache = &GenericUnresolvedCache{}
	t.generic_spec_decode_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	t.receiver_method_cache = &ReceiverMethodCache{}
	t.promote_text_cache = &PromoteTextCache{}
	t.struct_field_type_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	t.variant_short_name_cache = &AliasCache{}
	t.selector_type_cache = &SelectorTypeCache{}
	t.resolved_call_return_cache = &ResolvedCallReturnCache{}
	t.variant_match_cache = &VariantMatchCache{}
	t.interface_type_cache = &ContextLookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	t.enum_expected_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	t.type_alias_name_cache = &ContextBoolLookupCache{
		entries: map[string]i8{}
	}
	t.prepare_interface_impl_indexes()
	t.ierror_none_type_id = t.interface_impl_type_id('IError', 'None__') or { 0 }
}

fn (mut t Transformer) rebuild_embedded_fields_index() {
	t.embedded_fields = map[string][]FieldInfo{}
	for name, info in t.structs {
		mut fields := []FieldInfo{}
		for field in info.fields {
			if t.is_embedded_field(field) {
				fields << field
			}
		}
		if fields.len > 0 {
			t.embedded_fields[name] = fields
		}
	}
}

fn (mut t Transformer) prepare_runtime_type_indexes() {
	if isnil(t.tc) {
		t.runtime_type_indexes = map[string]int{}
		return
	}
	t.runtime_type_indexes = types.stable_type_indexes(t.tc.runtime_type_index_names())
}

fn (mut t Transformer) prepare_interface_impl_indexes() {
	if isnil(t.tc) {
		t.interface_impl_indexes = map[string]&types.InterfaceImplIndex{}
		return
	}
	t.interface_impl_indexes = t.tc.interface_impl_indexes.clone()
}

fn (mut t Transformer) refresh_interface_impl_indexes_for_generic_specs(specs map[string]string) {
	if isnil(t.tc) {
		return
	}
	mut refreshed := t.interface_impl_indexes.clone()
	for iface_name in t.tc.interface_names.keys() {
		if t.is_builtin_ierror_interface_name(iface_name) {
			continue
		}
		old_index := t.interface_impl_indexes[iface_name] or {
			&types.InterfaceImplIndex{
				names: []string{}
				ids: map[string]int{}
			}
		}
		mut impls := old_index.names.clone()
		mut seen := map[string]bool{}
		for name in impls {
			seen[name] = true
		}
		mut added := []string{}
		for spec, _ in specs {
			for candidate in [spec, c_name(spec)] {
				if candidate.len == 0 || seen[candidate]
					|| !t.tc.named_type_implements_interface(candidate, iface_name) {
					continue
				}
				seen[candidate] = true
				added << candidate
			}
		}
		added.sort()
		impls << added
		refreshed[iface_name] = &types.InterfaceImplIndex{
			names: impls
			ids: types.stable_interface_type_ids_preserving_prefix(old_index.names, impls)
		}
	}
	t.interface_impl_indexes = refreshed.move()
}

fn (mut t Transformer) refresh_interface_impl_indexes_for_boxed_types() {
	if isnil(t.tc) {
		return
	}
	mut boxed_types := map[string][]string{}
	mut boxed_seen := map[string]bool{}
	mut iface_names_cache := map[string]string{}
	mut runtime_type_names := []string{}
	mut boxed_keys := t.interface_boxed_types.keys()
	boxed_keys << t.interface_boxed_types_late.keys()
	for key in boxed_keys {
		if t.interface_boxed_impl_processed[key] {
			continue
		}
		separator := key.index_u8(`\n`)
		if separator <= 0 || separator + 1 >= key.len {
			continue
		}
		raw_iface := key[..separator]
		raw_concrete := key[separator + 1..]
		if t.generic_arg_is_unresolved(raw_concrete)
			|| !t.interface_boxed_impl_name_is_direct(raw_concrete) {
			continue
		}
		t.interface_boxed_impl_processed[key] = true
		iface := if cached := iface_names_cache[raw_iface] {
			cached
		} else {
			resolved := t.resolve_interface_type_name(raw_iface)
			iface_names_cache[raw_iface] = resolved
			resolved
		}
		if iface.len == 0 {
			continue
		}
		concrete := t.interface_concrete_impl_name(raw_concrete) or { continue }
		seen_key := '${iface}\n${concrete}'
		if boxed_seen[seen_key] {
			continue
		}
		boxed_seen[seen_key] = true
		mut concrete_types := boxed_types[iface] or { []string{} }
		concrete_types << concrete
		boxed_types[iface] = concrete_types
		runtime_type_names << concrete
	}
	if boxed_types.len == 0 {
		return
	}
	types.extend_stable_type_indexes_ref(mut t.runtime_type_indexes, &runtime_type_names)
	mut refreshed := t.interface_impl_indexes.clone()
	mut iface_names := boxed_types.keys()
	iface_names.sort()
	for iface_name in iface_names {
		old_index := t.interface_impl_indexes[iface_name] or { continue }
		mut impls := old_index.names.clone()
		mut seen := map[string]bool{}
		for name in impls {
			seen[name] = true
		}
		mut concrete_types := boxed_types[iface_name] or { []string{} }
		concrete_types.sort()
		for concrete in concrete_types {
			if !seen[concrete] {
				impls << concrete
				seen[concrete] = true
			}
		}
		if impls.len == old_index.names.len {
			continue
		}
		refreshed[iface_name] = &types.InterfaceImplIndex{
			names: impls
			ids: types.stable_interface_type_ids_preserving_prefix(old_index.names, impls)
		}
	}
	t.interface_impl_indexes = refreshed.move()
}

fn (t &Transformer) interface_boxed_impl_name_is_direct(name string) bool {
	if name.len == 0 {
		return false
	}
	if name.starts_with('fn(') || name.starts_with('fn ') || name.starts_with('[]')
		|| name.starts_with('map[') || name.starts_with('builtin.') {
		return true
	}
	if name in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte', 'u16',
		'u32', 'u64', 'f32', 'f64', 'string', 'char', 'rune'] {
		return true
	}
	if name in t.tc.structs || name in t.tc.type_aliases {
		return true
	}
	base, _, is_generic_app := generic_app_parts(name)
	return is_generic_app && (base in t.tc.structs || base in t.tc.type_aliases)
}

fn (t &Transformer) interface_impl_index_for_transform(iface_name string) &types.InterfaceImplIndex {
	if index := t.interface_impl_indexes[iface_name] {
		return index
	}
	if isnil(t.tc) {
		return &types.InterfaceImplIndex{
			names: []string{}
			ids: map[string]int{}
		}
	}
	impls := if t.is_builtin_ierror_interface_name(iface_name) {
		t.tc.ierror_impl_names()
	} else {
		t.tc.interface_impl_names(iface_name)
	}
	return &types.InterfaceImplIndex{
		names: impls
		ids: types.stable_interface_type_ids(impls)
	}
}

fn (mut t Transformer) collect_multi_return_fn_ret_types() {
	if isnil(t.tc) {
		return
	}
	t.multi_return_fn_ret_types = map[string]types.Type{}
	for name, ret_type in t.tc.fn_ret_types {
		if type_contains_multi_return(ret_type) {
			t.multi_return_fn_ret_types[name] = ret_type
		}
	}
}

fn type_contains_multi_return(typ types.Type) bool {
	if typ is types.MultiReturn {
		return true
	}
	if typ is types.OptionType {
		return type_contains_multi_return(typ.base_type)
	}
	if typ is types.ResultType {
		return type_contains_multi_return(typ.base_type)
	}
	return false
}

@[inline]
fn (t &Transformer) semantic_type_name(typ types.Type) string {
	if t.memo_semantic_type_names && !isnil(t.tc) {
		return t.tc.type_name(typ)
	}
	return typ.name()
}

const struct_short_name_ambiguous = '__v_struct_short_name_ambiguous__'

fn (mut t Transformer) rebuild_struct_short_name_index() {
	t.struct_short_name_index = map[string]string{}
	for qualified in t.structs.keys() {
		if !qualified.contains('.') {
			continue
		}
		short_name := qualified.all_after_last('.')
		if previous := t.struct_short_name_index[short_name] {
			if previous != qualified {
				t.struct_short_name_index[short_name] = struct_short_name_ambiguous
			}
		} else {
			t.struct_short_name_index[short_name] = qualified
		}
	}
	t.struct_short_name_index_ready = true
}

fn (mut t Transformer) rebuild_non_main_type_short_names() {
	t.non_main_type_short_names = map[string]bool{}
	for candidate, info in t.structs {
		owner := if info.module.len > 0 { info.module } else { candidate.all_before_last('.') }
		if candidate.contains('.') && owner !in ['', 'main', 'builtin'] {
			t.non_main_type_short_names[candidate.all_after_last('.')] = true
		}
	}
	for candidate, _ in t.sum_types {
		if candidate.contains('.') && candidate.all_before_last('.') !in ['', 'main', 'builtin'] {
			t.non_main_type_short_names[candidate.all_after_last('.')] = true
		}
	}
	for candidate, _ in t.enum_types {
		if candidate.contains('.') && candidate.all_before_last('.') !in ['', 'main', 'builtin'] {
			t.non_main_type_short_names[candidate.all_after_last('.')] = true
		}
	}
	t.non_main_type_index_ready = true
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
fn (mut t Transformer) record_inplace_child_rewrite(slot int, child flat.NodeId) {
	// Helper chunks append at temporary region offsets. Preserve the source-slot
	// write so region relocation can shift its generated child id alongside the
	// append block. The master chunk already appends at the final offset.
	if !t.defer_oor_writes && slot >= 0 && slot < t.shared_base_children {
		t.inplace_child_log << InplaceChildRewrite{
			slot: slot
			child: child
		}
	}
}

@[direct_array_access]
fn (mut t Transformer) rewrite_children_in_place(id flat.NodeId, children []flat.NodeId) bool {
	idx := int(id)
	if !t.inplace_child_rewrites || !t.skip_generics || !t.base_write_intercept
		|| t.smartcast_stack.len > 0 || idx < 0 || idx >= t.a.nodes.len
		|| !t.base_write_allowed(idx) {
		return false
	}
	node := t.a.nodes[idx]
	if int(node.children_count) != children.len {
		return false
	}
	for i, child in children {
		slot := int(node.children_start) + i
		t.a.children[slot] = child
		t.record_inplace_child_rewrite(slot, child)
	}
	t.invalidate_node_type_memo(idx)
	if !t.preserve_inplace_expr_types && !isnil(t.tc) {
		t.tc.invalidate_checked_expr_type(idx)
	}
	return true
}

@[inline]
fn (mut t Transformer) rewrite_one_child_in_place(id flat.NodeId, child flat.NodeId) bool {
	idx := int(id)
	if !t.inplace_child_rewrites || !t.skip_generics || !t.base_write_intercept
		|| t.smartcast_stack.len > 0 || idx < 0 || idx >= t.a.nodes.len
		|| !t.base_write_allowed(idx) {
		return false
	}
	node := t.a.nodes[idx]
	if node.children_count != 1 {
		return false
	}
	t.a.children[node.children_start] = child
	t.record_inplace_child_rewrite(int(node.children_start), child)
	t.invalidate_node_type_memo(idx)
	if !t.preserve_inplace_expr_types && !isnil(t.tc) {
		t.tc.invalidate_checked_expr_type(idx)
	}
	return true
}

@[inline]
fn (mut t Transformer) rewrite_two_children_in_place(id flat.NodeId, first flat.NodeId, second flat.NodeId) bool {
	idx := int(id)
	if !t.inplace_child_rewrites || !t.skip_generics || !t.base_write_intercept
		|| t.smartcast_stack.len > 0 || idx < 0 || idx >= t.a.nodes.len
		|| !t.base_write_allowed(idx) {
		return false
	}
	node := t.a.nodes[idx]
	if node.children_count != 2 {
		return false
	}
	t.a.children[node.children_start] = first
	t.a.children[node.children_start + 1] = second
	t.record_inplace_child_rewrite(int(node.children_start), first)
	t.record_inplace_child_rewrite(int(node.children_start) + 1, second)
	t.invalidate_node_type_memo(idx)
	if !t.preserve_inplace_expr_types && !isnil(t.tc) {
		t.tc.invalidate_checked_expr_type(idx)
	}
	return true
}

@[inline]
fn (mut t Transformer) set_node_typ(idx int, typ string) {
	if t.base_write_allowed(idx) {
		t.invalidate_node_type_memo(idx)
		t.a.nodes[idx].typ = typ
		t.a.nodes[idx].set_type_text_id(0)
		t.mark_scoped_owned_base_node(idx)
		if !isnil(t.tc) {
			t.tc.invalidate_checked_expr_type(idx)
		}
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx: idx
			kind: 0
			str: typ
		}
	}
}

@[inline]
fn (mut t Transformer) set_node_value(idx int, value string) {
	if t.base_write_allowed(idx) {
		t.invalidate_node_type_memo(idx)
		t.a.nodes[idx].value = value
		t.mark_scoped_owned_base_node(idx)
		if !isnil(t.tc) {
			t.tc.invalidate_checked_expr_type(idx)
		}
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx: idx
			kind: 1
			str: value
		}
	}
}

@[inline]
fn (mut t Transformer) set_node(idx int, node flat.Node) {
	if t.base_write_allowed(idx) {
		t.invalidate_node_type_memo(idx)
		mut stored := node
		stored.set_type_text_id(t.a.node_type_text_id(stored.typ, stored.type_text_id()))
		t.a.nodes[idx] = stored
		t.mark_scoped_owned_base_node(idx)
		if !isnil(t.tc) {
			t.tc.invalidate_checked_expr_type(idx)
		}
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx: idx
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
	if t.ignored_comptime_log_active {
		t.ignored_comptime_for_log << idx
		node := t.a.nodes[idx]
		for i in 0 .. node.children_count {
			t.ignore_comptime_for_subtree(t.a.child(&node, i))
		}
		return
	}
	if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
		t.ignored_comptime_for_nodes.ensure_cap(t.a.nodes.cap)
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
		t.invalidate_node_type_memo(idx)
		t.a.nodes[idx] = flat.Node{
			kind: .empty
			pos: old.pos
		}
		t.clear_typechecker_node_cache(idx)
	}
	t.ignored_comptime_for_nodes = []bool{}
}

@[inline]
fn (mut t Transformer) set_node_generic_params(idx int, gparams []string) {
	if t.base_write_allowed(idx) {
		t.invalidate_node_type_memo(idx)
		t.a.nodes[idx].set_generic_params(gparams)
		t.mark_scoped_owned_base_node(idx)
		if !isnil(t.tc) {
			t.tc.invalidate_checked_expr_type(idx)
		}
		return
	}
	if t.defer_oor_writes {
		t.deferred_base_writes << DeferredBaseWrite{
			idx: idx
			kind: 3
			gparams: gparams
		}
	}
}

@[inline]
fn (mut t Transformer) mark_scoped_owned_base_node(idx int) {
	if t.scope_parallel_workers && idx >= 0 && idx < t.scoped_base_nodes {
		if t.scoped_base_log_active {
			t.scoped_owned_base_log << idx
		} else {
			t.scoped_owned_base_nodes[idx] = true
		}
	}
}

// flush_deferred_base_writes applies the master's parked out-of-range writes
// in original program order once every worker has been joined.
fn (mut t Transformer) flush_deferred_base_writes() {
	for w in t.deferred_base_writes {
		t.invalidate_node_type_memo(w.idx)
		match w.kind {
			0 {
				t.a.nodes[w.idx].typ = w.str
				t.a.nodes[w.idx].set_type_text_id(0)
			}
			1 {
				t.a.nodes[w.idx].value = w.str
			}
			2 {
				mut stored := w.node
				stored.set_type_text_id(t.a.node_type_text_id(stored.typ, stored.type_text_id()))
				t.a.nodes[w.idx] = stored
			}
			else {
				t.a.nodes[w.idx].set_generic_params(w.gparams)
			}
		}
		t.mark_scoped_owned_base_node(w.idx)
		if !isnil(t.tc) {
			t.tc.invalidate_checked_expr_type(w.idx)
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
	t.ensure_private_signature_maps()
	t.receiver_method_suffix_index.clear()
	for name, _ in t.fn_ret_types {
		t.add_receiver_method_suffix_index(name)
	}
	if isnil(t.tc) {
		return
	}
	for name, _ in t.tc.fn_ret_types {
		if name in t.fn_ret_types {
			continue
		}
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
			// Zero-copy suffix view: map lookups only read it and map inserts
			// clone their key, so the allocation per suffix is unnecessary.
			suffix := unsafe { tos(name.str + i + 1, name.len - i - 1) }
			t.set_receiver_method_suffix_index(suffix, name)
		}
	}
}

fn (mut t Transformer) ensure_private_signature_maps() {
	if t.signature_maps_shared {
		t.fn_ret_types = t.fn_ret_types.clone()
		t.receiver_method_suffix_index = t.receiver_method_suffix_index.clone()
		t.signature_maps_shared = false
	}
	t.signature_maps_changed = true
}

fn (mut t Transformer) set_fn_ret_type(name string, typ string) {
	t.ensure_private_signature_maps()
	t.fn_ret_types[name] = typ
	t.fn_ret_types_log << name
}

fn (mut t Transformer) set_receiver_method_suffix_index(key string, name string) {
	if key.len == 0 {
		return
	}
	if existing := t.receiver_method_suffix_index[key] {
		if existing != name {
			t.ensure_private_signature_maps()
			t.receiver_method_suffix_index[key] = receiver_method_suffix_ambiguous
		}
		return
	}
	t.ensure_private_signature_maps()
	t.receiver_method_suffix_index[key] = name
}

// reset_var_types updates reset var types state for transform.
fn (mut t Transformer) reset_var_types() {
	t.var_types.clear()
	t.var_type_indices.clear()
	if !isnil(t.var_type_cache) {
		t.var_type_cache.clear()
	}
	t.fn_value_locals.clear()
	t.mut_param_values.clear()
	t.fixed_array_param_values.clear()
	t.interface_var_concrete_types.clear()
	t.addr_lvalue_pointer_locals.clear()
	t.orm_initialized_fields.clear()
	t.sql_query_data_aliases.clear()
}

fn (mut t Transformer) rebuild_variadic_suffix_index() {
	t.variadic_suffix_index.clear()
	if isnil(t.tc) {
		return
	}
	for name, is_variadic in t.tc.fn_variadic {
		mut offset := name.index('.') or { continue }
		for offset >= 0 && offset + 1 < name.len {
			// Zero-copy suffix view (see add_receiver_method_suffix_index).
			suffix := unsafe { tos(name.str + offset + 1, name.len - offset - 1) }
			if suffix in t.variadic_suffix_index {
				t.variadic_suffix_index[suffix] = 2
			} else {
				t.variadic_suffix_index[suffix] = if is_variadic { i8(1) } else { i8(-1) }
			}
			next := suffix.index('.') or { break }
			offset += next + 1
		}
	}
}

// set_var_type updates set var type state for transform.
fn (mut t Transformer) set_var_type(name string, typ string) {
	t.set_var_type_with_raw(name, typ, typ)
}

fn (mut t Transformer) set_implicit_err_var_type() {
	t.set_var_type_binding('err', 'IError', 'IError', true)
}

fn (t &Transformer) implicit_err_binding_active() bool {
	i := t.var_type_index('err')
	return i >= 0 && t.var_types[i].is_implicit_err
}

fn (mut t Transformer) set_var_type_with_raw(name string, typ string, raw_typ string) {
	t.set_var_type_binding(name, typ, raw_typ, false)
}

fn (mut t Transformer) set_var_type_binding(name string, typ string, raw_typ string, is_implicit_err bool) {
	if name.len == 0 {
		return
	}
	raw := if raw_typ.len > 0 { raw_typ } else { typ }
	i := t.var_type_index(name)
	if i >= 0 {
		t.var_type_indices[name] = i
		t.var_types[i] = VarTypeBinding{
			name: name
			typ: typ
			raw_typ: raw
			is_implicit_err: is_implicit_err
		}
		return
	}
	t.var_type_indices[name] = t.var_types.len
	if !isnil(t.var_type_cache) {
		t.var_type_cache.put(name, t.var_types.len)
	}
	t.var_types << VarTypeBinding{
		name: name
		typ: typ
		raw_typ: raw
		is_implicit_err: is_implicit_err
	}
}

fn (mut t Transformer) set_decl_var_type(node flat.Node, name string, typ string) {
	t.set_decl_var_type_with_raw(node, name, typ, typ)
}

fn (mut t Transformer) set_decl_var_type_with_raw(node flat.Node, name string, typ string, raw_typ string) {
	t.set_var_type_with_raw(name, typ, t.decl_var_raw_type(node, typ, raw_typ))
}

fn (t &Transformer) decl_var_raw_type(node flat.Node, typ string, raw_typ string) string {
	raw := if raw_typ.len > 0 { raw_typ } else { typ }
	if !decl_assign_value_is_shared(node.value) {
		return raw
	}
	mut clean := raw.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	if clean.starts_with('shared ') {
		return clean
	}
	if clean.len == 0 {
		return raw
	}
	return 'shared ${clean}'
}

fn decl_assign_value_is_shared(value string) bool {
	return value == 'shared' || value.starts_with('shared:')
}

// unset_var_type supports unset var type handling for Transformer.
fn (mut t Transformer) unset_var_type(name string) {
	i := t.var_type_index(name)
	if i >= 0 {
		t.var_types.delete(i)
		t.var_type_indices.delete(name)
		for j in i .. t.var_types.len {
			t.var_type_indices[t.var_types[j].name] = j
		}
	}
	if !isnil(t.var_type_cache) {
		t.var_type_cache.clear()
	}
	t.fn_value_locals.delete(name)
	t.interface_var_concrete_types.delete(name)
}

// var_type supports var type handling for Transformer.
@[inline]
fn (t &Transformer) var_type(name string) string {
	i := t.var_type_index(name)
	if i >= 0 {
		return t.var_types[i].typ
	}
	return ''
}

fn (t &Transformer) raw_var_type(name string) string {
	i := t.var_type_index(name)
	if i >= 0 {
		binding := t.var_types[i]
		return if binding.raw_typ.len > 0 { binding.raw_typ } else { binding.typ }
	}
	return ''
}

fn (mut t Transformer) record_orm_initialized_fields(name string, rhs_id flat.NodeId) {
	if name.len == 0 || int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
		return
	}
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind != .struct_init {
		t.orm_initialized_fields.delete(name)
		return
	}
	mut fields := []string{}
	for i in 0 .. rhs.children_count {
		field := t.a.child_node(&rhs, i)
		if field.kind == .field_init && field.value.len > 0 && field.value !in fields {
			fields << field.value
		}
	}
	t.orm_initialized_fields[name] = fields
}

fn (mut t Transformer) record_sql_query_data_alias(name string, rhs_id flat.NodeId) {
	if name.len == 0 || int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
		return
	}
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind != .sql_expr {
		t.sql_query_data_aliases.delete(name)
		return
	}
	tokens := sql_clean_tokens(rhs.value.split(' '))
	if tokens.len == 0 || tokens[0] != 'querydata' {
		t.sql_query_data_aliases.delete(name)
		return
	}
	t.sql_query_data_aliases[name] = tokens[1..].clone()
}

fn (mut t Transformer) update_sql_query_data_aliases_for_assignment(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := t.a.child(&node, i)
		rhs_id := t.a.child(&node, i + 1)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value.len > 0 {
			if node.op == .assign {
				t.record_sql_query_data_alias(lhs.value, rhs_id)
			} else {
				t.sql_query_data_aliases.delete(lhs.value)
			}
		}
		i += 2
	}
}

fn (mut t Transformer) update_orm_initialized_fields_for_assignment(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := t.a.child(&node, i)
		rhs_id := t.a.child(&node, i + 1)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value.len > 0 {
			if node.op == .assign {
				t.record_orm_initialized_fields(lhs.value, rhs_id)
			} else {
				t.orm_initialized_fields.delete(lhs.value)
			}
		} else if root_name := t.orm_initialized_lvalue_root(lhs_id) {
			t.orm_initialized_fields.delete(root_name)
		}
		i += 2
	}
}

fn (t &Transformer) orm_initialized_lvalue_root(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if node.value.len > 0 {
				return node.value
			}
		}
		.selector, .index, .paren {
			if node.children_count > 0 {
				return t.orm_initialized_lvalue_root(t.a.child(&node, 0))
			}
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				return t.orm_initialized_lvalue_root(t.a.child(&node, 0))
			}
		}
		else {}
	}

	return none
}

@[inline]
fn (t &Transformer) var_type_index(name string) int {
	if !isnil(t.var_type_cache) {
		mut cache := t.var_type_cache
		if unsafe { cache.name.str == name.str } && cache.name.len == name.len {
			return cache.index
		}
		if unsafe { cache.name2.str == name.str } && cache.name2.len == name.len {
			return cache.index2
		}
		if unsafe { cache.name3.str == name.str } && cache.name3.len == name.len {
			return cache.index3
		}
		if unsafe { cache.name4.str == name.str } && cache.name4.len == name.len {
			return cache.index4
		}
	}
	if i := t.var_type_indices[name] {
		if !isnil(t.var_type_cache) {
			mut cache := t.var_type_cache
			cache.put(name, i)
		}
		return i
	}
	if t.var_type_indices.len == t.var_types.len {
		if !isnil(t.var_type_cache) {
			mut cache := t.var_type_cache
			cache.put(name, -1)
		}
		return -1
	}
	// Focused tests can seed var_types directly. Production transformers keep
	// the O(1) index synchronized through the setters above.
	for i, binding in t.var_types {
		if binding.name == name {
			if !isnil(t.var_type_cache) {
				mut cache := t.var_type_cache
				cache.put(name, i)
			}
			return i
		}
	}
	return -1
}

fn (mut t Transformer) restore_var_types(saved []VarTypeBinding) {
	if !isnil(t.var_type_cache) {
		t.var_type_cache.clear()
	}
	// Branch transforms usually only change binding types or append inner-scope
	// locals. In that common case the name-to-index layout is still valid; keep
	// it instead of clearing and rebuilding the whole map at every branch exit.
	if t.var_types.len >= saved.len {
		mut same_prefix := true
		for i, binding in saved {
			if t.var_types[i].name != binding.name {
				same_prefix = false
				break
			}
		}
		if same_prefix {
			for i in saved.len .. t.var_types.len {
				t.var_type_indices.delete(t.var_types[i].name)
			}
			t.var_types = saved
			return
		}
	}
	t.var_types = saved
	t.var_type_indices.clear()
	for i, binding in t.var_types {
		t.var_type_indices[binding.name] = i
	}
}

// --- type collection ---

// collect_types updates collect types state for transform.
@[direct_array_access]
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
				cur_mod = t.tc.file_modules[node.value] or { '' }
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
					raw_field_typ := if f.typ.len > 0 { f.typ } else { f.value }
					field_typ := t.normalize_field_type(raw_field_typ, owner_type)
					fields << FieldInfo{
						name: f.value
						typ: field_typ
						raw_typ: raw_field_typ
						default_expr: default_expr
						is_embedded: field_decl_is_embedded(f.value, raw_field_typ)
					}
				}
				info := StructInfo{
					name: node.value
					module: cur_mod
					is_params: 'params' in node.typ.split(',')
					is_aligned: transform_struct_decl_alignment_is_set(node.typ)
					alignment: transform_struct_decl_alignment_value(node.typ)
					fields: fields
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
						variants << t.normalize_sum_variant_type(v.value, cur_mod, node.generic_params())
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
				params := node.generic_params()
				backing_storage_type := if params.len > 0 && params[0].len > 0 {
					t.normalize_type_in_module(params[0], cur_mod)
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
				if t.declared_fn_name_counts[node.value] < 2 {
					t.declared_fn_name_counts[node.value]++
				}
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
		if params.len == 0 || name.index_u8(`.`) < 0 {
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
			return clean[..bracket_end + 1] + t.normalize_sum_variant_type(clean[bracket_end + 1..], mod, generic_params)
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
	mut transformed_count := 0
	for i in 0 .. node_count {
		node := t.a.nodes[i]
		kind_id := int(node.kind)
		if kind_id == 77 {
			t.cur_file = node.value
			t.cur_module = t.tc.file_modules[node.value] or { '' }
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
			transformed_count++
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
	if !isnil(t.tc) {
		return 'main' in t.tc.fn_ret_types
	}
	mut cur_module := ''
	for node in t.a.nodes {
		kind_id := int(node.kind)
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
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt, .for_in_stmt, .if_expr, .comptime_if, .comptime_for, .match_stmt, .assert_stmt, .defer_stmt, .block {
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
		kind: .file
		op: file_node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: file_node.pos
		value: file_node.value
		typ: file_node.typ
		payload: file_node.payload
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
	fn_idx                    int
	range_lo                  int // first node id of this fn's subtree (fn subtree = [range_lo, fn_idx])
	file                      string
	module                    string
	cost                      int
	rank                      i64
	map_expansion_estimate    int
	interp_expansion_estimate int
	escape_scan_known         bool
	escape_scan_needed        bool
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
	mut dpsw := time.new_stopwatch()
	t.collect_exclusive_closure_return_fns()
	t.timing_profile('  [ttime]   dsp closure ret  ${f64(dpsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	dpsw.restart()
	t.precompute_const_array_fixed_storage()
	t.timing_profile('  [ttime]   dsp const fixed  ${f64(dpsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	dpsw.restart()
	// Every forked worker checker otherwise lazily rebuilds the source-error
	// embedding index by rescanning all struct declarations; build it once in
	// the master cache so forks inherit it through the frozen base.
	if !isnil(t.tc) {
		t.tc.precompute_source_error_embed_index()
	}
	t.timing_profile('  [ttime]   dsp embed idx    ${f64(dpsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	dpsw.restart()
	// Collect source-level interface conversions before any worker rewrites its
	// private AST. Equality and automatic string lowering can then generate the
	// same bounded tag dispatch independently of worker scheduling.
	if t.building_v {
		// The V compiler does not use generic interface boxing. Its method tables
		// come from the checker indexes, so avoid rescanning the complete AST just
		// to produce an empty boxed-type set during every self-host generation.
		t.interface_boxed_types_done = true
		t.interface_boxed_types_frozen = true
	} else {
		t.collect_interface_boxed_types_dispatch(want_parallel)
	}
	t.refresh_interface_impl_indexes_for_boxed_types()
	t.timing_profile('  [ttime]   dsp boxed+iface  ${f64(dpsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	if !want_parallel {
		if t.scope_parallel_workers && t.retain_worker_results {
			$if !v3_no_parallel ? {
				has_entry_main := t.has_entry_main()
				literal_decls := t.collect_literal_fn_decls(t.a.nodes.len)
				pure_items := t.transform_serial_then_collect_pure(literal_decls)
				t.prepare_parallel_call_param_types()
				t.transform_scoped_helper_batches(pure_items, scoped_transform_batches)
				t.transform_deferred_expansion_items()
				if !has_entry_main {
					t.transform_top_level_user_stmts()
				}
			} $else {
				t.transform_all()
			}
		} else {
			t.transform_all()
		}
		return false
	}
	has_entry_main := t.has_entry_main()
	mut ttsw := time.new_stopwatch()
	// Serial phase: transform consts/globals and every function whose body
	// contains a function literal (the only construct that lifts new top-level
	// declarations and mutates the shared TypeChecker). Collect the remaining,
	// closure-free functions as parallelizable work items.
	literal_decls := if t.literal_fn_decls_ready {
		t.literal_fn_decls
	} else {
		t.collect_literal_fn_decls(t.a.nodes.len)
	}
	t.timing_profile('  [ttime] literal_decls      ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	ttsw.restart()
	pure_items := t.transform_serial_then_collect_pure(literal_decls)
	t.timing_profile('  [ttime] serial+collect     ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms (items: ${pure_items.len})')
	ttsw.restart()
	base_nodes := t.a.nodes.len
	base_children := t.a.children.len
	was_parallel := t.run_parallel_transform(pure_items, base_nodes, base_children)
	t.timing_profile('  [ttime] parallel run       ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	ttsw.restart()
	// Functions with oversized auto-str or const-map expansion were held back
	// from cost-proportional worker slots; lower them against the growable arena.
	t.transform_deferred_expansion_items()
	t.timing_profile('  [ttime] deferred expansion ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms (items: ${t.deferred_expansion_count})')
	ttsw.restart()
	if !has_entry_main {
		t.transform_top_level_user_stmts()
		t.timing_profile('  [ttime] top_level_stmts    ${f64(ttsw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	}
	return was_parallel
}

fn (mut t Transformer) collect_interface_boxed_types_dispatch(want_parallel bool) {
	if t.interface_boxed_types_done {
		return
	}
	if !want_parallel || !t.scope_parallel_workers {
		t.collect_interface_boxed_types()
		return
	}
	if t.collect_interface_boxed_types_parallel() {
		return
	}
	t.tc.freeze_type_cache_for_forks()
	scratch_scope := transform_worker_scope_begin(true)
	scan_tc := t.tc.fork_for_parallel_transform(t.a)
	mut scan := t.fork_scan_worker(scan_tc)
	scan.collect_interface_boxed_types()
	transform_worker_scope_leave(scratch_scope)
	mut boxed_types := map[string]bool{}
	for key, value in scan.interface_boxed_types {
		boxed_types[key.clone()] = value
	}
	t.interface_boxed_types = boxed_types.move()
	t.interface_boxed_types_done = true
	t.interface_boxed_types_frozen = true
	transform_worker_scope_free(scratch_scope)
	t.tc.unfreeze_type_cache_after_forks()
}

// transform_serial_then_collect_pure walks the top level once: it transforms
// const/global declarations and closure-bearing functions in place (serially),
// and returns work items for the closure-free functions left to transform.
fn (mut t Transformer) transform_serial_then_collect_pure(literal_decls []int) []FnWorkItem {
	mut pure := []FnWorkItem{}
	mut literal_decl_idx := 0
	scan_fn_literals := literal_decls.len > 0
	// The checker's top-level index gives the exact subtree range of each fn
	// ((previous top-level decl of ANY kind, fn_idx]); the shared-base parallel
	// transform relies on those ranges being disjoint per item.
	use_checker_tl := t.tc.top_level_idx.len > 0 && t.tc.top_level_idx_nodes_len == t.a.nodes.len
	mut rebuilt_tl := []int{}
	if !use_checker_tl {
		// Hand-built test ASTs and transforms after declaration synthesis do not
		// have a current checker index. Rebuild only in that uncommon case.
		rebuilt_tl = []int{cap: 1024}
		for i, node in t.a.nodes {
			if node.kind in [.file, .module_decl, .struct_decl, .type_decl, .interface_decl,
				.enum_decl, .import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl] {
				rebuilt_tl << i
			}
		}
	}
	tl := if use_checker_tl { t.tc.top_level_idx } else { rebuilt_tl }
	mut prev_tl_any := -1
	mut const_ms := f64(0)
	mut lit_ms := f64(0)
	mut est_ms := f64(0)
	sc_profile := !isnil(t.tc) && t.tc.verbose
	mut scsw := time.new_stopwatch()
	for i in tl {
		range_lo := prev_tl_any + 1
		span_cost := i - prev_tl_any
		prev_tl_any = i
		node := t.a.nodes[i]
		kind_id := int(node.kind)
		if kind_id == 77 {
			t.cur_file = node.value
			t.cur_module = t.tc.file_modules[node.value] or { '' }
		} else if kind_id == 73 {
			t.cur_module = node.value
		} else if kind_id == 61 {
			// The parser builds nodes bottom-up, so a declaration's subtree
			// precedes its node. Use the checker's exact top-level boundary:
			// counting from the previous function/const/global also included
			// intervening type declarations and badly skewed worker loads.
			for literal_decl_idx < literal_decls.len && literal_decls[literal_decl_idx] < i {
				literal_decl_idx++
			}
			has_literal := literal_decl_idx < literal_decls.len
				&& literal_decls[literal_decl_idx] == i
			if has_literal {
				literal_decl_idx++
			}
			if t.fn_decl_has_unresolved_generics(node, t.cur_module) {
				continue
			}
			if !t.should_transform_fn(node) {
				continue
			}
			scan_cost := if i < t.fn_scan_costs.len { t.fn_scan_costs[i] } else { 0 }
			cost := if scan_cost > 0 {
				scan_cost
			} else if scan_fn_literals {
				if span_cost > 0 { span_cost } else { 1 }
			} else {
				int(node.children_count) + 1
			}
			escape_scan_flags := if i < t.fn_escape_scan_flags.len {
				t.fn_escape_scan_flags[i]
			} else {
				u8(0)
			}
			if has_literal {
				old_range_lo := t.item_range_lo
				old_range_hi := t.item_range_hi
				t.item_range_lo = range_lo
				t.item_range_hi = i
				scsw.restart()
				t.transform_fn_body(i)
				lit_ms += f64(scsw.elapsed().microseconds()) / 1000.0
				t.item_range_lo = old_range_lo
				t.item_range_hi = old_range_hi
			} else {
				// Clone-free workers need conservative expansion estimates because they
				// append into fixed .nogrow regions. Generic transform workers have
				// private growable ASTs, so avoid rescanning every function solely to
				// estimate capacity they do not use.
				if sc_profile {
					scsw.restart()
				}
				mut str_est := 0
				mut str_needs_deferred_lowering := false
				if t.skip_generics {
					if t.building_v && t.parallel_enabled {
						// The compiler's interpolated types are all bounded primitives and
						// metadata names; none can trigger aggregate auto-str expansion.
						str_est = 0
					} else {
						str_est, str_needs_deferred_lowering = t.fn_span_interp_estimate(range_lo, i)
					}
				}
				map_est := if t.skip_generics {
					t.fn_span_map_expansion_estimate(range_lo, i)
				} else {
					0
				}
				if sc_profile {
					est_ms += f64(scsw.elapsed().microseconds()) / 1000.0
				}
				if t.skip_generics
					&& (str_needs_deferred_lowering || map_est > deferred_map_expansion_threshold) {
					t.deferred_expansion_items << FnWorkItem{
						fn_idx: i
						range_lo: range_lo
						file: t.cur_file
						module: t.cur_module
						cost: cost
						rank: i64(cost) * 1_000_000_000 - i64(i)
						escape_scan_known: escape_scan_flags & 1 != 0
						escape_scan_needed: escape_scan_flags & 2 != 0
					}
				} else {
					adj_cost := cost + str_est + map_est
					pure << FnWorkItem{
						fn_idx: i
						range_lo: range_lo
						file: t.cur_file
						module: t.cur_module
						cost: adj_cost
						rank: i64(adj_cost) * 1_000_000_000 - i64(i)
						map_expansion_estimate: map_est
						interp_expansion_estimate: str_est
						escape_scan_known: escape_scan_flags & 1 != 0
						escape_scan_needed: escape_scan_flags & 2 != 0
					}
				}
			}
		} else if kind_id == 65 {
			scsw.restart()
			t.transform_const_decl(node)
			const_ms += f64(scsw.elapsed().microseconds()) / 1000.0
		} else if kind_id == 64 {
			scsw.restart()
			t.transform_global_decl(node)
			const_ms += f64(scsw.elapsed().microseconds()) / 1000.0
		}
	}
	t.fn_scan_costs = []int{}
	t.fn_escape_scan_flags = []u8{}
	t.timing_profile('  [ttime]   sc consts ${const_ms:.2f} ms, closures ${lit_ms:.2f} ms, expansion est ${est_ms:.2f} ms')
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
fn (mut t Transformer) collect_literal_fn_decls(limit int) []int {
	mut result := []int{cap: 64}
	mut flags := []u8{len: limit}
	mut escape_flags := []u8{len: limit}
	if scan_literal_decl_flags_parallel(t, limit, mut flags, mut escape_flags) {
		mut literal_pending := false
		mut escape_scan_needed := false
		mut span_cost := 0
		t.fn_scan_costs = []int{len: limit}
		t.fn_escape_scan_flags = []u8{len: limit}
		for i in 0 .. flags.len {
			flag := flags[i]
			span_cost += int(flag & 15)
			if escape_flags[i] != 0 {
				escape_scan_needed = true
			}
			if flag & 16 != 0 {
				literal_pending = true
			}
			if flag & 32 != 0 {
				if literal_pending {
					result << i
				}
				literal_pending = false
				t.fn_scan_costs[i] = span_cost
				t.fn_escape_scan_flags[i] = 1 | if escape_scan_needed { u8(2) } else { u8(0) }
			} else if flag & 64 != 0 {
				literal_pending = false
			}
			if flag & 128 != 0 {
				span_cost = 0
				escape_scan_needed = false
			}
		}
		return result
	}
	mut literal_pending := false
	for i in 0 .. limit {
		node := t.a.nodes[i]
		kid := int(node.kind)
		// 21 = fn_literal, 32 = lambda_expr (see fn_subtree_scan's old check).
		if kid == 21 || kid == 32 {
			literal_pending = true
		} else if kid == 61 {
			if literal_pending {
				result << i
			}
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
	// NOTE: arming the checker's BodyResolveMemo per item here was measured a
	// wash (2026-08): transform's tc-level resolve repeats are already absorbed
	// by trust_checked_expr_types and the transformer's own caches.
	old_literal_free_fn_body := t.literal_free_fn_body
	t.literal_free_fn_body = true
	defer {
		t.literal_free_fn_body = old_literal_free_fn_body
	}
	for it in items {
		t.cur_file = it.file
		t.cur_module = it.module
		t.item_range_lo = it.range_lo
		t.item_range_hi = it.fn_idx
		t.item_escape_scan_known = it.escape_scan_known
		t.item_escape_scan_needed = it.escape_scan_needed
		if t.memo_node_types {
			t.begin_node_type_memo(it.range_lo, it.fn_idx)
		}
		$if v3_ttime ? {
			mut item_sw := time.new_stopwatch()
			t.transform_fn_body(it.fn_idx)
			elapsed_us := item_sw.elapsed().microseconds()
			if elapsed_us >= 5_000 {
				eprintln('  [ttime]       item ${it.module}.${t.a.nodes[it.fn_idx].value} cost=${it.cost} ${f64(elapsed_us) / 1000.0:.2f} ms')
			}
			continue
		}
		t.transform_fn_body(it.fn_idx)
	}
	t.end_node_type_memo()
	t.item_range_lo = -1
	t.item_range_hi = -1
	t.item_escape_scan_known = false
	t.item_escape_scan_needed = false
}

// transform_deferred_expansion_items lowers functions whose auto-str or const-map
// expansion cannot fit safely in a bounded parallel worker region. They run
// serially against the growable master arena after the workers finish.
fn (mut t Transformer) transform_deferred_expansion_items() {
	t.deferred_expansion_count = t.deferred_expansion_items.len
	if t.deferred_expansion_items.len == 0 {
		return
	}
	items := t.deferred_expansion_items
	t.deferred_expansion_items = []FnWorkItem{}
	t.transform_pure_items_serial(items)
}

// clone_ast_base produces a private FlatAst holding an independent copy of the
// first base_nodes nodes / base_children children, so a worker can append its own
// transformed nodes without racing the master or other workers. Read-only metadata
// (disabled_fns) is shared, while per-node specialization maps are private because
// worker-local appended ids need relocation during merge. The copies carry headroom
// for the worker's own appends:
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
		nodes: nodes
		children: children
		user_code_start: t.a.user_code_start
		disabled_fns: t.a.disabled_fns
		noreturn_fns: t.a.noreturn_fns
		source_files: t.a.source_files
		template_call_sites: t.a.template_call_sites
		template_actions: t.a.template_actions
		source_buffers: t.a.source_buffers
		text_values: t.a.text_values
		text_ids: t.a.text_ids
		worker_pool: t.a.worker_pool
		specialized_fn_nodes: t.a.specialized_fn_nodes.clone()
		specialized_fn_modules: t.a.specialized_fn_modules.clone()
		specialized_fn_files: t.a.specialized_fn_files.clone()
	}
}

// fork_worker builds a worker Transformer that shares this transformer's
// read-only collected maps (structs, sum types, fn return types, …) and
// operates on its own cloned AST `ast` and forked TypeChecker `wtc`. All
// per-function mutable state, helper-root tracking, used-fn additions, and
// memoization caches are reset/private so the worker can run on its own thread.
fn (t &Transformer) fork_worker(ast &flat.FlatAst, wtc &types.TypeChecker) &Transformer {
	return t.fork_worker_config(ast, wtc, !t.retain_worker_results)
}

fn (t &Transformer) fork_scoped_batch_worker(ast &flat.FlatAst, wtc &types.TypeChecker) &Transformer {
	return t.fork_worker_config(ast, wtc, false)
}

fn (t &Transformer) fork_worker_config(ast &flat.FlatAst, wtc &types.TypeChecker, copy_used_fns bool) &Transformer {
	used := if copy_used_fns {
		t.used_fns
	} else {
		map[string]bool{}
	}
	mut w := t.fork_program_view(ast, wtc, used, copy_used_fns)
	w.used_struct_operator_fns = t.used_struct_operator_fns.clone()
	if !copy_used_fns {
		w.used_fns_parent = unsafe { &t.used_fns }
		w.used_fns_root = if !isnil(t.used_fns_root) {
			t.used_fns_root
		} else {
			t.used_fns_parent
		}
	}
	w.alias_cache = &AliasCache{}
	w.sum_cache = &AliasCache{}
	w.module_type_cache = &AliasCache{}
	w.struct_guess_cache = &AliasCache{}
	w.var_type_cache = &VarTypeIndexCache{}
	w.generic_unresolved_cache = &GenericUnresolvedCache{}
	w.generic_spec_decode_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.receiver_method_cache = &ReceiverMethodCache{}
	w.promote_text_cache = &PromoteTextCache{}
	w.struct_field_type_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.variant_short_name_cache = &AliasCache{}
	w.selector_type_cache = &SelectorTypeCache{}
	w.resolved_call_return_cache = &ResolvedCallReturnCache{}
	w.variant_match_cache = &VariantMatchCache{}
	w.interface_type_cache = &ContextLookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.enum_expected_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.type_alias_name_cache = &ContextBoolLookupCache{
		entries: map[string]i8{}
	}
	w.alias_receiver_method_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.interface_box_param_cache = &BoolLookupCache{
		entries: map[string]i8{}
	}
	w.call_variadic_cache = &BoolLookupCache{
		entries: map[string]i8{}
	}
	w.str_alias_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.generic_fn_decls_cache = map[string]GenericFnDecl{}
	w.generic_fn_decls_ready = false
	w.generic_call_spec_cache = map[int]GenericCallSpec{}
	w.generic_call_spec_misses = map[int]bool{}
	// run_parallel_transform snapshots declaration signatures before workers
	// start. Keep the shared read-only index and signature cache; rebuilding
	// either would read fn_decl nodes while shared-base workers rewrite them.
	// Misses stay private because unknown call names can still be queried.
	if t.node_context_read_only {
		w.node_module_map_cache = t.node_module_map_cache
		w.node_file_map_cache = t.node_file_map_cache
		w.node_module_map_nodes = t.node_module_map_nodes
		w.node_context_read_only = true
	} else {
		w.node_module_map_cache = []string{}
		w.node_module_map_nodes = -1
	}
	w.var_types = []VarTypeBinding{}
	w.var_type_indices = map[string]int{}
	w.refined_node_types = t.refined_node_types.clone()
	w.mut_param_values = map[string]bool{}
	w.fixed_array_param_values = map[string]bool{}
	w.smartcast_stack = []SmartcastContext{}
	w.invalidated_smartcasts = map[string]bool{}
	w.pending_stmts = []flat.NodeId{}
	w.pointer_value_lvalues = map[string]bool{}
	w.pointer_value_rvalues = map[string]bool{}
	w.orm_initialized_fields = map[string][]string{}
	w.sql_query_data_aliases = map[string][]string{}
	w.escaping_amp_ptrs = map[string]bool{}
	w.escaping_amp_sources = map[string]bool{}
	w.heaped_amp_locals = map[string]bool{}
	w.escaping_interface_box_locals = map[string]bool{}
	w.generic_fn_specs_in_progress = map[string]bool{}
	w.monomorph_errors = []string{}
	w.monomorph_error_seen = map[string]bool{}
	// Fields added after the fork/merge machinery was first written. They are
	// mutated during body transforms (or lazily built), so each worker needs
	// private backing storage — a plain struct copy would share the master's.
	w.stringify_stack = []string{}
	w.interface_boxed_types_done = true
	w.interface_boxed_types_frozen = t.interface_boxed_types_frozen
	w.sum_eq_helper_module = ''
	w.default_clone_expansion_stack = []string{}
	w.generic_receiver_methods_by_name = map[string][]string{}
	w.used_fns_log = []string{}
	w.used_fns_log_active = false
	w.deferred_base_writes = []DeferredBaseWrite{}
	w.ignored_comptime_for_nodes = []bool{}
	w.ignored_comptime_for_log = []int{}
	w.ignored_comptime_log_active = false
	w.worker_scope = unsafe { nil }
	// Serial scoped helpers have no shared-base parallel boundary. Keep the
	// transform-stage base boundary so their in-place node writes are still
	// recorded and published before the helper arena is released.
	w.scoped_base_nodes = if t.shared_base_nodes >= 0 {
		t.shared_base_nodes
	} else {
		t.scoped_base_nodes
	}
	w.scoped_owned_base_nodes = map[int]bool{}
	w.scoped_owned_base_log = []int{}
	w.scoped_base_log_active = false
	w.scoped_promoted_texts = map[string]string{}
	// Workers do not record transformed fns (that would write the master's
	// shared backing array); the master marks each worker's chunk items when it
	// merges the worker.
	w.transformed_fns = []bool{}
	w.temp_counter = 0
	w.cur_file = ''
	w.cur_module = ''
	w.cur_fn_name = ''
	w.cur_fn_source_file = ''
	w.cur_fn_source_module = ''
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
	mut w := t.fork_program_view(t.a, wtc, map[string]bool{}, false)
	w.alias_cache = &AliasCache{}
	w.sum_cache = &AliasCache{}
	w.module_type_cache = &AliasCache{}
	w.struct_guess_cache = &AliasCache{}
	w.var_type_cache = &VarTypeIndexCache{}
	w.generic_unresolved_cache = &GenericUnresolvedCache{}
	w.generic_spec_decode_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.receiver_method_cache = &ReceiverMethodCache{}
	w.promote_text_cache = &PromoteTextCache{}
	w.struct_field_type_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.variant_short_name_cache = &AliasCache{}
	w.selector_type_cache = &SelectorTypeCache{}
	w.resolved_call_return_cache = &ResolvedCallReturnCache{}
	w.variant_match_cache = &VariantMatchCache{}
	w.interface_type_cache = &ContextLookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.enum_expected_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.type_alias_name_cache = &ContextBoolLookupCache{
		entries: map[string]i8{}
	}
	w.alias_receiver_method_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.interface_box_param_cache = &BoolLookupCache{
		entries: map[string]i8{}
	}
	w.call_variadic_cache = &BoolLookupCache{
		entries: map[string]i8{}
	}
	w.str_alias_cache = &LookupCache{
		entries: map[string]string{}
		misses: map[string]bool{}
	}
	w.generic_fn_decls_cache = map[string]GenericFnDecl{}
	w.generic_fn_decls_ready = false
	w.generic_call_spec_cache = map[int]GenericCallSpec{}
	w.generic_call_spec_misses = map[int]bool{}
	w.node_module_map_cache = []string{}
	w.node_module_map_nodes = -1
	w.var_types = []VarTypeBinding{}
	w.var_type_indices = map[string]int{}
	w.mut_param_values = map[string]bool{}
	w.fixed_array_param_values = map[string]bool{}
	w.smartcast_stack = []SmartcastContext{}
	w.invalidated_smartcasts = map[string]bool{}
	w.pending_stmts = []flat.NodeId{}
	w.pointer_value_lvalues = map[string]bool{}
	w.pointer_value_rvalues = map[string]bool{}
	w.escaping_amp_ptrs = map[string]bool{}
	w.escaping_amp_sources = map[string]bool{}
	w.heaped_amp_locals = map[string]bool{}
	w.escaping_interface_box_locals = map[string]bool{}
	w.generic_fn_specs_in_progress = map[string]bool{}
	w.stringify_stack = []string{}
	w.interface_boxed_types = map[string]bool{}
	w.interface_boxed_types_done = false
	w.interface_boxed_types_frozen = false
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

// fork_program_view constructs a worker from explicit compilation-wide state.
// All fields omitted here come from new_transformer_view as fresh worker-local
// state, so adding a mutable Transformer field cannot silently share its backing
// storage with a helper thread.
fn (t &Transformer) fork_program_view(ast &flat.FlatAst, wtc &types.TypeChecker, used_fns map[string]bool, copy_generic_state bool) Transformer {
	// The pre-scan freezes the boxed-type set before skip-generics workers
	// start, so sharing it below is read-only and avoids one map clone per worker.
	return Transformer{
		a: ast
		tc: wtc
		structs: t.structs
		embedded_fields: t.embedded_fields
		struct_short_name_index: t.struct_short_name_index
		struct_short_name_index_ready: t.struct_short_name_index_ready
		non_main_type_short_names: t.non_main_type_short_names
		non_main_type_index_ready: t.non_main_type_index_ready
		unique_fields: t.unique_fields
		alias_methods: t.alias_methods
		globals: t.globals
		sum_types: t.sum_types
		sum_variant_parents: t.sum_variant_parents
		sum_variant_names: t.sum_variant_names
		sum_variant_fields: t.sum_variant_fields
		qualified_types: t.qualified_types
		fn_ret_types: t.fn_ret_types
		multi_return_fn_ret_types: t.multi_return_fn_ret_types
		receiver_method_suffix_index: t.receiver_method_suffix_index
		declared_fn_name_counts: t.declared_fn_name_counts
		variadic_suffix_index: t.variadic_suffix_index
		const_suffixes: t.const_suffixes
		source_parent_ids: t.source_parent_ids
		shared_local_decl_names: t.shared_local_decl_names
		const_array_fixed_storage_cache: t.const_array_fixed_storage_cache
		enum_types: t.enum_types
		enum_backing_types: t.enum_backing_types
		runtime_type_indexes: t.runtime_type_indexes
		generic_alias_names: t.generic_alias_names
		type_alias_suffixes: t.type_alias_suffixes
		local_decl_nodes_by_name: t.local_decl_nodes_by_name
		fn_decl_offsets_by_file: t.fn_decl_offsets_by_file
		struct_field_decl_metas_cache: t.struct_field_decl_metas_cache
		comptime_field_metas_cache: map[string][]FieldMeta{}
		comptime_reflected_for_roles: t.comptime_reflected_for_roles
		comptime_reflected_for_ready: t.comptime_reflected_for_ready
		// Late scoped workers can discover parameter signatures for generic
		// declarations added after the parallel pre-scan. Keep those recursive
		// Type values worker-local: their payloads belong to the worker arena and
		// must not outlive it through the master's shared cache.
		call_param_types_decl_cache: t.call_param_types_decl_cache.clone()
		call_param_types_decl_misses: t.call_param_types_decl_misses.clone()
		call_param_types_decl_index: t.call_param_types_decl_index
		call_param_types_index_ready: t.call_param_types_index_ready
		call_param_types_prepared: t.call_param_types_prepared
		comptime_reflected_params: t.comptime_reflected_params
		// Function-body lowering records operator helpers as used. Each parallel
		// worker therefore needs private map storage; sharing this map races on
		// concurrent insertions and can corrupt the allocator.
		used_struct_operator_fns: t.used_struct_operator_fns.clone()
		generic_specialization_args: if !copy_generic_state {
			map[string][]string{}
		} else if t.skip_generics {
			t.generic_specialization_args
		} else {
			t.generic_specialization_args.clone()
		}
		generic_specialization_args_parent: if copy_generic_state {
			unsafe { nil }
		} else {
			unsafe { &t.generic_specialization_args }
		}
		interface_var_concrete_types: map[string]string{}
		interface_boxed_types: if t.skip_generics && t.interface_boxed_types_frozen {
			t.interface_boxed_types
		} else {
			t.interface_boxed_types.clone()
		}
		interface_boxed_types_done: t.interface_boxed_types_done
		interface_boxed_types_frozen: t.interface_boxed_types_frozen
		interface_impl_indexes: t.interface_impl_indexes
		interface_impl_spec_count: t.interface_impl_spec_count
		ierror_none_type_id: t.ierror_none_type_id
		sum_eq_types: t.sum_eq_types.clone()
		sum_eq_synthesized: t.sum_eq_synthesized.clone()
		auto_str_types: t.auto_str_types.clone()
		auto_str_synthesized: t.auto_str_synthesized.clone()
		auto_str_helper_module: t.auto_str_helper_module
		auto_str_synthesis_type: t.auto_str_synthesis_type
		default_clone_types: t.default_clone_types.clone()
		default_clone_synthesized: t.default_clone_synthesized.clone()
		used_fns: used_fns.clone()
		mut_param_values: map[string]bool{}
		pointer_value_lvalues: map[string]bool{}
		pointer_value_rvalues: map[string]bool{}
		orm_initialized_fields: map[string][]string{}
		sql_query_data_aliases: map[string][]string{}
		invalidated_smartcasts: map[string]bool{}
		escaping_amp_ptrs: map[string]bool{}
		escaping_amp_sources: map[string]bool{}
		heaped_amp_locals: map[string]bool{}
		local_closure_cleanup_decls: map[int]string{}
		local_closure_cleanup_values: map[int]string{}
		local_closure_cleanup_assigns: map[int]string{}
		local_closure_field_cleanups: map[int]bool{}
		exclusive_closure_return_fns: t.exclusive_closure_return_fns
		exclusive_closure_returns_done: t.exclusive_closure_returns_done
		mut_fixed_array_capture_sources: map[string]bool{}
		generic_fn_specs_in_progress: map[string]bool{}
		generic_fn_spec_nodes: map[string]flat.NodeId{}
		specialization_decl_nodes_by_name: map[string][]int{}
		pending_generic_fn_spec_keys: map[string]bool{}
		generic_receiver_methods_by_name: map[string][]string{}
		generic_call_spec_cache: map[int]GenericCallSpec{}
		generic_call_spec_misses: map[int]bool{}
		monomorph_error_seen: map[string]bool{}
		skip_generics: t.skip_generics
		building_v: t.building_v
		memo_node_types: t.memo_node_types
		stringify_depth_cap: t.stringify_depth_cap
		struct_autostr_recurse_types: t.struct_autostr_recurse_types
		has_spawn_expr: t.has_spawn_expr
		base_write_intercept: t.base_write_intercept
		defer_oor_writes: t.defer_oor_writes
		shared_base_nodes: t.shared_base_nodes
		shared_base_children: t.shared_base_children
		scoped_base_nodes: t.scoped_base_nodes
		scope_parallel_workers: t.scope_parallel_workers
		fast_escape_precheck: t.fast_escape_precheck
		inplace_child_rewrites: t.inplace_child_rewrites
		inplace_fn_child_rewrites: t.inplace_fn_child_rewrites
		inplace_assign_rewrites: t.inplace_assign_rewrites
		inplace_simple_rewrites: t.inplace_simple_rewrites
		inplace_lvalue_rewrites: t.inplace_lvalue_rewrites
		inplace_block_expr_rewrites: t.inplace_block_expr_rewrites
		inplace_decl_assign_rewrites: t.inplace_decl_assign_rewrites
		inplace_scalar_prefixes: t.inplace_scalar_prefixes
		lean_struct_init_fields: t.lean_struct_init_fields
		inplace_struct_fields: t.inplace_struct_fields
		memo_call_param_type_names: t.memo_call_param_type_names
		memo_semantic_type_names: t.memo_semantic_type_names
		prefix_param_scan: t.prefix_param_scan
		preserve_inplace_expr_types: t.preserve_inplace_expr_types
		retain_worker_results: t.retain_worker_results
		stage_scope: t.stage_scope
		scoped_monomorphize: t.scoped_monomorphize
		node_context_read_only: t.node_context_read_only
		signature_maps_shared: true
		signature_maps_changed: false
		struct_maps_shared: true
	}
}

fn (mut t Transformer) merge_worker_used_fns(w &Transformer) {
	t.merge_worker_signatures(w)
	t.merge_worker_capture_contexts(w)
	scoped := w.worker_scope != unsafe { nil }
	for name, used in w.used_fns {
		if used {
			owned_name := if scoped && !t.retain_worker_results { name.clone() } else { name }
			t.mark_used_fn_key(owned_name)
		}
	}
	for name, used in w.used_struct_operator_fns {
		if used && name !in t.used_struct_operator_fns {
			owned_name := if scoped && !t.retain_worker_results { name.clone() } else { name }
			t.used_struct_operator_fns[owned_name] = true
		}
	}
	for name, req in w.sum_eq_types {
		if name !in t.sum_eq_types {
			if scoped {
				t.sum_eq_types[name.clone()] = SumEqRequest{
					sum_name: req.sum_name.clone()
					module: req.module.clone()
					file: req.file.clone()
					helper_module: req.helper_module.clone()
				}
			} else {
				t.sum_eq_types[name] = req
			}
		}
	}
	for name, req in w.auto_str_types {
		if name !in t.auto_str_types {
			if scoped {
				t.auto_str_types[name.clone()] = AutoStrRequest{
					module: req.module.clone()
					file: req.file.clone()
					helper_module: req.helper_module.clone()
				}
			} else {
				t.auto_str_types[name] = req
			}
		}
	}
	for name, req in w.default_clone_types {
		if name !in t.default_clone_types {
			if scoped {
				t.default_clone_types[name.clone()] = DefaultCloneRequest{
					module: req.module.clone()
					file: req.file.clone()
				}
			} else {
				t.default_clone_types[name] = req
			}
		}
	}
}

fn clone_struct_info_owned(info StructInfo) StructInfo {
	mut fields := []FieldInfo{cap: info.fields.len}
	for field in info.fields {
		fields << FieldInfo{
			name: field.name.clone()
			typ: field.typ.clone()
			raw_typ: field.raw_typ.clone()
			default_expr: field.default_expr
			is_embedded: field.is_embedded
		}
	}
	return StructInfo{
		name: info.name.clone()
		module: info.module.clone()
		is_params: info.is_params
		is_aligned: info.is_aligned
		alignment: info.alignment.clone()
		fields: fields
	}
}

fn clone_struct_fields_owned(fields []types.StructField) []types.StructField {
	mut cloned := []types.StructField{cap: fields.len}
	for field in fields {
		cloned << types.StructField{
			name: field.name.clone()
			typ: types.clone_owned_type(field.typ)
			has_default: field.has_default
			is_embed: field.is_embed
			is_mut: field.is_mut
			is_volatile: field.is_volatile
		}
	}
	return cloned
}

// merge_worker_capture_contexts publishes closure context metadata created by
// monomorph workers. Their struct maps are private because peers can lift
// different literals concurrently.
fn (mut t Transformer) merge_worker_capture_contexts(w &Transformer) {
	if w.generated_capture_contexts.len == 0 {
		return
	}
	for name in w.generated_capture_contexts {
		if info := w.structs[name] {
			t.structs[name.clone()] = clone_struct_info_owned(info)
		}
	}
	if isnil(t.tc) || isnil(w.tc) {
		return
	}
	mut master_tc := unsafe { &types.TypeChecker(voidptr(t.tc)) }
	for name in w.generated_capture_contexts {
		if fields := w.tc.structs[name] {
			owned_name := name.clone()
			master_tc.structs[owned_name] = clone_struct_fields_owned(fields)
			master_tc.struct_modules[owned_name] = (w.tc.struct_modules[name] or { '' }).clone()
			master_tc.struct_files[owned_name] = (w.tc.struct_files[name] or { '' }).clone()
			master_tc.register_short_type_name(owned_name)
		}
	}
}

// merge_worker_signatures publishes declarations synthesized by a private
// transform worker before its disposable arena is released.
fn (mut t Transformer) merge_worker_signatures(w &Transformer) {
	if !w.signature_maps_changed && (isnil(w.tc) || !w.tc.transform_signatures_changed()) {
		return
	}
	if w.signature_maps_changed {
		t.ensure_private_signature_maps()
	}
	for name in w.fn_ret_types_log {
		ret := w.fn_ret_types[name] or { continue }
		if name !in t.fn_ret_types {
			owned_name := name.clone()
			t.fn_ret_types[owned_name] = ret.clone()
			t.add_receiver_method_suffix_index(owned_name)
		}
	}
	if isnil(t.tc) || isnil(w.tc) || !w.tc.transform_signatures_changed() {
		return
	}
	// The master checker is shared as `&TypeChecker`, but signature merging runs only on
	// the master thread after every worker has been joined (their results are already
	// consumed above), so nothing reads or writes it concurrently here. Reinterpret the
	// shared reference as mutable to publish the worker's new signatures into the
	// uniquely-owned master checker.
	mut master_tc := unsafe { &types.TypeChecker(voidptr(t.tc)) }
	master_tc.ensure_private_transform_signatures()
	mut tc_signature_names := w.tc_signature_names_log.clone()
	tc_signature_names << w.fn_ret_types_log
	tc_signature_names << w.tc.transform_signature_names_log
	for name in tc_signature_names {
		owned_name := name.clone()
		if ret := w.tc.fn_ret_types[name] {
			if name !in master_tc.fn_ret_types {
				master_tc.fn_ret_types[owned_name] = types.clone_owned_type(ret)
			}
		}
		if params := w.tc.fn_param_types[name] {
			if name !in master_tc.fn_param_types {
				master_tc.register_generated_fn_param_types(owned_name, types.clone_owned_types(params))
			}
		}
		master_tc.fn_variadic[owned_name] = w.tc.fn_variadic[name]
		if w.tc.specialized_generic_fns[name] {
			master_tc.specialized_generic_fns[owned_name] = true
		}
		if module_name := w.tc.fn_type_modules[name] {
			master_tc.fn_type_modules[owned_name] = module_name.clone()
		}
		if file := w.tc.fn_type_files[name] {
			master_tc.fn_type_files[owned_name] = file.clone()
		}
	}
}

fn (mut t Transformer) clone_sum_eq_types_owned() {
	mut cloned := map[string]SumEqRequest{}
	for name, req in t.sum_eq_types {
		cloned[name.clone()] = SumEqRequest{
			sum_name: req.sum_name.clone()
			module: req.module.clone()
			file: req.file.clone()
			helper_module: req.helper_module.clone()
		}
	}
	t.sum_eq_types = cloned.move()
}

fn (mut t Transformer) clone_auto_str_types_owned() {
	mut cloned := map[string]AutoStrRequest{}
	for name, req in t.auto_str_types {
		cloned[name.clone()] = AutoStrRequest{
			module: req.module.clone()
			file: req.file.clone()
			helper_module: req.helper_module.clone()
		}
	}
	t.auto_str_types = cloned.move()
}

fn (mut t Transformer) clone_default_clone_types_owned() {
	mut cloned := map[string]DefaultCloneRequest{}
	for name, req in t.default_clone_types {
		cloned[name.clone()] = DefaultCloneRequest{
			module: req.module.clone()
			file: req.file.clone()
		}
	}
	t.default_clone_types = cloned.move()
}

@[inline]
fn transform_scope_owns(scope voidptr, ptr voidptr) bool {
	$if prealloc {
		return unsafe { prealloc_scope_owns(scope, ptr) }
	}
	return false
}

// clone_scoped_worker_node publishes a node's owned fields through the
// compilation text table before its helper arena is released.
fn (mut t Transformer) clone_scoped_worker_node(idx int, scope voidptr) {
	if idx < 0 || idx >= t.a.nodes.len {
		return
	}
	mut node := unsafe { &t.a.nodes[idx] }
	if node.value.len > 0 && transform_scope_owns(scope, node.value.str) {
		_, value := t.a.intern_text(node.value)
		node.value = value
	}
	if node.typ.len > 0 && transform_scope_owns(scope, node.typ.str) {
		_, typ := t.a.intern_text(node.typ)
		node.typ = typ
	}
	if isnil(node.payload) {
		return
	}
	old_params := node.generic_params()
	if old_params.len > 0 {
		mut needs_owned_params := transform_scope_owns(scope, node.payload)
			|| transform_scope_owns(scope, old_params.data)
		if !needs_owned_params {
			for param in old_params {
				if param.len > 0 && transform_scope_owns(scope, param.str) {
					needs_owned_params = true
					break
				}
			}
		}
		if needs_owned_params {
			mut params := []string{cap: old_params.len}
			for param in old_params {
				if param.len > 0 && transform_scope_owns(scope, param.str) {
					_, canonical := t.a.intern_text(param)
					params << canonical
				} else {
					params << param
				}
			}
			node.set_generic_params(params)
		}
	}
}

// relocate_region_in_place rewrites one worker region's appended-node child
// references and children_start offsets to their final merged values, using the
// same shift arithmetic merge_worker applies during its fused copy+relocate.
@[direct_array_access]
fn (mut t Transformer) relocate_region_in_place(node_start int, node_end int, child_start int, child_end int, node_shift i32, child_shift i32) {
	for k in node_start .. node_end {
		if t.a.nodes[k].children_start >= child_start {
			t.a.nodes[k] = t.a.nodes[k].with_shifted_children(child_shift)
		}
	}
	for j in child_start .. child_end {
		cid := t.a.children[j]
		if int(cid) >= node_start {
			t.a.children[j] = flat.NodeId(int(cid) + int(node_shift))
		}
	}
	for rewrite in t.inplace_child_log {
		t.a.children[rewrite.slot] = if int(rewrite.child) >= node_start {
			flat.NodeId(int(rewrite.child) + int(node_shift))
		} else {
			rewrite.child
		}
	}
}

// merge_worker folds a finished worker's transformed output back into the master
// AST. The worker created its new nodes/children at indices base_nodes/base_children
// (matching the master at fork time); here they are appended to the master and every
// reference to a worker-local new node or new children block is shifted by the
// distance the block moved. `items` lists the function indices this worker owned, so
// their rewritten top-level nodes can be copied into place.
fn (mut t Transformer) merge_worker(w &Transformer, items []FnWorkItem, base_nodes int, base_children int, clear_node_caches bool) {
	node_shift := i32(t.a.nodes.len - base_nodes)
	child_shift := i32(t.a.children.len - base_children)
	if !t.merge_regions_relocated {
		for rewrite in w.inplace_child_log {
			t.a.children[rewrite.slot] = if int(rewrite.child) >= base_nodes {
				flat.NodeId(int(rewrite.child) + int(node_shift))
			} else {
				rewrite.child
			}
		}
	}
	// New children: bulk-copy the worker block, then relocate references to
	// worker-local new nodes in place (a per-element push paid a capacity check
	// and branch per child id).
	new_children := w.a.children.len - base_children
	if new_children > 0 {
		old_len := t.a.children.len
		unsafe {
			t.a.children.grow_len(new_children)
		}
		if t.merge_regions_relocated {
			// A parallel pass already applied node_shift inside the region;
			// compaction is a plain leftward move.
			unsafe {
				vmemmove(&t.a.children[old_len], &w.a.children[base_children], isize(new_children) * isize(t.a.children.element_size))
			}
		} else {
			// Fused copy+relocate: under the shared-base path compaction copies
			// leftward within one array, so the forward pass reads ahead of its own
			// writes; one streamed pass replaces the former memmove + fixup pass.
			for j in 0 .. new_children {
				cid := w.a.children[base_children + j]
				t.a.children[old_len + j] = if int(cid) >= base_nodes {
					flat.NodeId(int(cid) + int(node_shift))
				} else {
					cid
				}
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
		}
		if clear_node_caches {
			// The merged ids form one contiguous range; clearing it with range
			// fills beats a branchy per-node clear (this runs for every merged
			// node of every worker).
			t.clear_typechecker_node_cache_range(nodes_old_len, t.a.nodes.len)
		}
		clone_worker_nodes := w.worker_scope != unsafe { nil } && !t.retain_worker_results
			&& t.stage_scope == unsafe { nil }
		if t.merge_regions_relocated && !clone_worker_nodes {
			// A parallel pass already applied child_shift inside the region.
			unsafe {
				vmemmove(&t.a.nodes[nodes_old_len], &w.a.nodes[base_nodes], isize(new_nodes) * isize(t.a.nodes.element_size))
			}
		} else {
			// Fused copy+relocate, same leftward-overlap argument as the children
			// block above.
			for j in 0 .. new_nodes {
				node := w.a.nodes[base_nodes + j]
				k := nodes_old_len + j
				t.a.nodes[k] = if node.children_start >= base_children {
					node.with_shifted_children(child_shift)
				} else {
					node
				}
				if clone_worker_nodes {
					t.clone_scoped_worker_node(k, w.worker_scope)
				}
			}
		}
	}
	// Specializations emitted by this worker are keyed by worker-local appended
	// node ids. Replay only those new entries under the same shift as the nodes;
	// entries below base_nodes were copied from the master when the worker forked.
	for idx, specialized in w.a.specialized_fn_nodes {
		if idx < base_nodes || !specialized {
			continue
		}
		t.a.specialized_fn_nodes[idx + int(node_shift)] = true
	}
	for idx, module_name in w.a.specialized_fn_modules {
		if idx < base_nodes {
			continue
		}
		shifted := idx + int(node_shift)
		t.a.specialized_fn_modules[shifted] = if w.worker_scope != unsafe { nil }
			&& !t.retain_worker_results {
			module_name.clone()
		} else {
			module_name
		}
	}
	for idx, file in w.a.specialized_fn_files {
		if idx < base_nodes {
			continue
		}
		shifted := idx + int(node_shift)
		t.a.specialized_fn_files[shifted] = if w.worker_scope != unsafe { nil }
			&& !t.retain_worker_results {
			file.clone()
		} else {
			file
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
		if w.worker_scope != unsafe { nil } && !t.retain_worker_results
			&& t.stage_scope == unsafe { nil } {
			t.clone_scoped_worker_node(it.fn_idx, w.worker_scope)
		}
	}
	if t.scope_parallel_workers && t.retain_worker_results {
		for idx in w.scoped_owned_base_nodes.keys() {
			t.scoped_owned_base_log << idx
		}
		t.scoped_owned_base_log << w.scoped_owned_base_log
	}
	if w.worker_scope != unsafe { nil } && !t.retain_worker_results
		&& t.stage_scope == unsafe { nil } {
		for idx in w.scoped_owned_base_nodes.keys() {
			t.clone_scoped_worker_node(idx, w.worker_scope)
		}
		for idx in w.scoped_owned_base_log {
			t.clone_scoped_worker_node(idx, w.worker_scope)
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
			owned_name := if w.worker_scope != unsafe { nil } && !t.retain_worker_results {
				name.clone()
			} else {
				name
			}
			t.set_resolved_call_entry(shifted, owned_name)
		}
		for idx, name in w.tc.fork_overlay.resolved_fn_values {
			shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
			owned_name := if w.worker_scope != unsafe { nil } && !t.retain_worker_results {
				name.clone()
			} else {
				name
			}
			t.set_resolved_fn_value_entry(shifted, owned_name)
		}
	}
	for message in w.monomorph_errors {
		owned_message := if w.worker_scope != unsafe { nil } && !t.retain_worker_results {
			message.clone()
		} else {
			message
		}
		t.record_monomorph_error(owned_message)
	}
	for idx, spec in w.generic_call_spec_cache {
		shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
		if shifted in t.generic_call_spec_cache {
			continue
		}
		t.generic_call_spec_cache[shifted] = GenericCallSpec{
			decl_key: if w.worker_scope != unsafe { nil } {
				spec.decl_key.clone()
			} else {
				spec.decl_key
			}
			args: spec.args.clone()
		}
	}
	for idx, missed in w.generic_call_spec_misses {
		if !missed {
			continue
		}
		shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
		if shifted !in t.generic_call_spec_cache {
			t.generic_call_spec_misses[shifted] = true
		}
	}
	for idx, typ in w.refined_node_types {
		if idx < base_nodes {
			continue
		}
		shifted := idx + int(node_shift)
		owned_typ := if w.worker_scope != unsafe { nil } { typ.clone() } else { typ }
		t.record_refined_node_type(shifted, owned_typ)
	}
	if w.ignored_comptime_for_nodes.len > 0 {
		if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
			t.ignored_comptime_for_nodes.ensure_cap(t.a.nodes.cap)
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
	if w.ignored_comptime_for_log.len > 0 {
		if t.ignored_comptime_for_nodes.len < t.a.nodes.len {
			t.ignored_comptime_for_nodes.ensure_cap(t.a.nodes.cap)
			t.ignored_comptime_for_nodes << []bool{len: t.a.nodes.len - t.ignored_comptime_for_nodes.len}
		}
		for idx in w.ignored_comptime_for_log {
			shifted := if idx >= base_nodes { idx + int(node_shift) } else { idx }
			if shifted >= 0 && shifted < t.ignored_comptime_for_nodes.len {
				t.ignored_comptime_for_nodes[shifted] = true
			}
		}
	}
}

fn (mut t Transformer) set_resolved_call_entry(idx int, name string) {
	if t.tc.parallel_check_sparse && idx >= t.tc.resolved_call_names.len {
		t.tc.sparse_resolved_call_names[idx] = t.tc.canonical_symbol(name)
		return
	}
	for t.tc.resolved_call_names.len <= idx {
		t.tc.resolved_call_names << ''
		t.tc.resolved_call_set << false
	}
	t.tc.resolved_call_names[idx] = t.tc.canonical_symbol(name)
	t.tc.resolved_call_set[idx] = true
}

// set_generated_resolved_call records the exact target of a call synthesized
// by the transformer. Parallel workers publish the entry through their private
// overlay, while serial transforms can update the dense checker cache directly.
fn (mut t Transformer) set_generated_resolved_call(id flat.NodeId, name string) {
	if isnil(t.tc) || int(id) < 0 || name.len == 0 {
		return
	}
	if !isnil(t.tc.fork_overlay) {
		t.tc.fork_overlay.resolved_call_names[int(id)] = name
		return
	}
	t.set_resolved_call_entry(int(id), name)
}

fn (mut t Transformer) set_resolved_fn_value_entry(idx int, name string) {
	if t.tc.parallel_check_sparse && idx >= t.tc.resolved_fn_value_names.len {
		t.tc.sparse_resolved_fn_values[idx] = t.tc.canonical_symbol(name)
		return
	}
	for t.tc.resolved_fn_value_names.len <= idx {
		t.tc.resolved_fn_value_names << ''
		t.tc.resolved_fn_value_set << false
	}
	t.tc.resolved_fn_value_names[idx] = t.tc.canonical_symbol(name)
	t.tc.resolved_fn_value_set[idx] = true
}

fn (mut t Transformer) record_refined_node_type(idx int, typ string) {
	if idx < 0 || typ.len == 0 {
		return
	}
	if existing := t.refined_node_types[idx] {
		if existing.len > 0 && t.concrete_generic_type_refines(typ, existing) {
			return
		}
	}
	t.refined_node_types[idx] = typ
}

// clear_typechecker_node_cache_range is clear_typechecker_node_cache for a
// contiguous id range: the bool sets become memsets and the range-level guards
// replace one branch per node per table.
fn (mut t Transformer) clear_typechecker_node_cache_range(start int, end int) {
	if isnil(t.tc) || start >= end || start < 0 {
		return
	}
	call_end := if end < t.tc.resolved_call_set.len { end } else { t.tc.resolved_call_set.len }
	if start < call_end {
		unsafe {
			vmemset(&t.tc.resolved_call_set[start], 0, call_end - start)
		}
		for k in start .. call_end {
			t.tc.resolved_call_names[k] = ''
		}
	}
	fn_value_end := if end < t.tc.resolved_fn_value_set.len {
		end
	} else {
		t.tc.resolved_fn_value_set.len
	}
	if start < fn_value_end {
		unsafe {
			vmemset(&t.tc.resolved_fn_value_set[start], 0, fn_value_end - start)
		}
		for k in start .. fn_value_end {
			t.tc.resolved_fn_value_names[k] = ''
		}
	}
	expr_end := if end < t.tc.expr_type_set.len { end } else { t.tc.expr_type_set.len }
	if start < expr_end {
		unsafe {
			vmemset(&t.tc.expr_type_set[start], 0, expr_end - start)
		}
	}
	stmt_end := if end < t.tc.statement_nodes.len { end } else { t.tc.statement_nodes.len }
	if start < stmt_end {
		unsafe {
			vmemset(&t.tc.statement_nodes[start], 0, stmt_end - start)
		}
	}
	if t.tc.sparse_resolved_call_names.len > 0 || t.tc.sparse_resolved_fn_values.len > 0
		|| t.tc.sparse_expr_type_values.len > 0 || t.tc.sparse_statement_nodes.len > 0 {
		for k in start .. end {
			if t.tc.sparse_resolved_call_names.len > 0 {
				t.tc.sparse_resolved_call_names.delete(k)
			}
			if t.tc.sparse_resolved_fn_values.len > 0 {
				t.tc.sparse_resolved_fn_values.delete(k)
			}
			if t.tc.sparse_expr_type_values.len > 0 {
				t.tc.sparse_expr_type_values.delete(k)
			}
			if t.tc.sparse_statement_nodes.len > 0 {
				t.tc.sparse_statement_nodes.delete(k)
			}
		}
	}
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
			// The persistent pool waits for every submitted callback before the
			// master can merge. Keep shared-base chunks evenly loaded; staggering
			// their finish times only lengthens the wait for the heaviest bucket.
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

fn (t &Transformer) late_name_may_expand_interface(name string) bool {
	dot := name.last_index('.') or { return false }
	// The receiver is only probed while `name` owns its backing bytes.
	receiver := unsafe { name[..dot] }
	base_start := if receiver.len > 0 && receiver[0] == `&` { 1 } else { 0 }
	generic_start := receiver.index_u8(`[`)
	base_end := if generic_start > base_start { generic_start } else { receiver.len }
	base := unsafe { receiver[base_start..base_end] }
	return base in t.tc.interface_names || base in t.tc.type_aliases
		|| t.type_alias_suffixes[base].len > 0 || base == 'IError' || base == 'builtin.IError'
}

fn (mut t Transformer) transform_late_used_fn_bodies(names &[]string, names_start int, names_end int, node_limit int) {
	if names_end <= names_start || node_limit <= 0 {
		return
	}
	mut late := map[string]bool{}
	mut queued := map[string]bool{}
	mut pending := []string{}
	old_module := t.cur_module
	old_file := t.cur_file
	t.cur_module = ''
	t.cur_file = ''
	mut interface_seed_names := []string{}
	for ni in names_start .. names_end {
		name := (*names)[ni]
		if !t.late_name_may_expand_interface(name) {
			continue
		}
		dot := name.last_index('.') or { continue }
		// These non-owning slices are only used while `name` keeps their backing
		// bytes alive. Avoid copying very large generic-instantiation spellings.
		receiver := unsafe { name[..dot] }
		method := unsafe { name[dot + 1..] }
		iface_name := t.resolve_interface_type_name(receiver)
		if iface_name.len > 0 {
			interface_seed_names << t.interface_method_implementer_names(iface_name, method)
		}
	}
	for ni in names_start .. names_end {
		name := (*names)[ni]
		t.mark_fn_used_name(name)
	}
	for name in interface_seed_names {
		t.mark_fn_used_name(name)
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
	// node-kind reads) plus a repeated fn_decl_has_unresolved_generics check
	// on every non-matching fn_decl.
	mut candidates := []LateFnCandidate{}
	mut scan_file := ''
	mut scan_module := ''
	late_scan_ids := if t.building_v && t.tc.top_level_idx.len > 0 {
		t.tc.top_level_idx
	} else {
		[]int{}
	}
	scan_count := if late_scan_ids.len > 0 { late_scan_ids.len } else { limit }
	for scan_pos in 0 .. scan_count {
		i := if late_scan_ids.len > 0 { late_scan_ids[scan_pos] } else { scan_pos }
		if i < 0 || i >= limit {
			continue
		}
		node := t.a.nodes[i]
		kind_id := int(node.kind)
		if kind_id == 77 {
			scan_file = node.value
			scan_module = ''
		} else if kind_id == 73 {
			scan_module = node.value
		} else if kind_id == 61 && !(i < t.transformed_fns.len && t.transformed_fns[i])
			&& !t.fn_decl_has_unresolved_generics(node, scan_module) {
			candidates << LateFnCandidate{
				idx: i
				file: scan_file
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
	// `names` can contain very large generic-instantiation spellings. Only copy
	// a name into the late-work maps when the index proves that it can match a
	// body; unmatched names still remain marked used for later compiler stages.
	for ni in names_start .. names_end {
		name := (*names)[ni]
		if name.len == 0
			|| (candidate_index[name].len == 0 && candidate_index[c_name(name)].len == 0) {
			continue
		}
		add_late_used_fn_name(name, mut late, mut pending, mut queued)
	}
	for name in interface_seed_names {
		if name.len == 0
			|| (candidate_index[name].len == 0 && candidate_index[c_name(name)].len == 0) {
			continue
		}
		add_late_used_fn_name(name, mut late, mut pending, mut queued)
	}
	was_log_active := t.used_fns_log_active
	t.used_fns_log_active = true
	mut transformed_scoped := false
	$if !v3_no_parallel ? {
		if t.scope_parallel_workers && t.retain_worker_results {
			t.transform_late_candidates_scoped(candidate_index, mut candidates, mut late, mut pending, mut queued)
			transformed_scoped = true
		}
	}
	if !transformed_scoped {
		for pending.len > 0 {
			name := pending.pop()
			t.transform_late_candidates_for(name, candidate_index, mut candidates, mut late, mut pending, mut queued)
		}
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
	// Transforming a late body can root implementation methods indirectly (for
	// example, when lowering an interface call). Those insertions are not always
	// represented by a direct call node in the transformed body, so enqueue the
	// insertion log too and recursively transform their bodies.
	for i in log_start .. t.used_fns_log.len {
		add_late_used_fn_name(t.used_fns_log[i], mut late, mut pending, mut queued)
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
	if !t.used_fn_contains_name(spelling) {
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
	return t.has_any_used_fns() && t.used_fn_contains_name('main')
}

fn (t &Transformer) used_fn_contains_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if t.used_fns[name] {
		return true
	}
	if isnil(t.used_fns_parent) {
		return !isnil(t.used_fns_root) && unsafe { t.used_fns_root[name] }
	}
	return unsafe { t.used_fns_parent[name] }
		|| (!isnil(t.used_fns_root) && unsafe { t.used_fns_root[name] })
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
	old_tc_file := t.tc.cur_file
	old_tc_module := t.tc.cur_module
	t.tc.cur_file = t.cur_file
	t.tc.cur_module = t.cur_module
	defer {
		t.tc.cur_file = old_tc_file
		t.tc.cur_module = old_tc_module
	}
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
				.map_init, .fn_literal, .lambda_expr] {
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
		kind: .block
		children_start: start
		children_count: flat.child_count(pending.len + 1)
		typ: t.node_type(transformed)
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
		kind: .or_expr
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: if const_typ.len > 0 { const_typ } else { node.typ }
	})
}

// transform_global_decl transforms transform global decl data for transform.
fn (mut t Transformer) transform_global_decl(node flat.Node) {
	old_tc_file := t.tc.cur_file
	old_tc_module := t.tc.cur_module
	t.tc.cur_file = t.cur_file
	t.tc.cur_module = t.cur_module
	defer {
		t.tc.cur_file = old_tc_file
		t.tc.cur_module = old_tc_module
	}
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
			if val.kind == .cast_expr && val.value.starts_with('&') {
				if preserved := t.transform_global_amp_interface_cast(val, val.value) {
					t.a.children[gf.children_start] = preserved
					continue
				}
			}
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
	if nested_match := t.transform_nested_match_string_interp_node(node) {
		return nested_match
	}
	if nested_if := t.transform_nested_if_string_interp_node(node) {
		return nested_if
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
		expr = t.make_call_typed('string__plus', [expr, parts[i]], 'string')
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

fn (mut t Transformer) transform_nested_if_string_interp_node(node flat.Node) ?flat.NodeId {
	if node.children_count != 5 {
		return none
	}
	first_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	middle_id := t.a.child(&node, 2)
	else_id := t.a.child(&node, 3)
	last_id := t.a.child(&node, 4)
	first := t.a.nodes[int(first_id)]
	middle := t.a.nodes[int(middle_id)]
	last := t.a.nodes[int(last_id)]
	if first.kind != .string_literal || middle.kind != .string_literal
		|| last.kind != .string_literal {
		return none
	}
	cond_text := nested_if_interp_prefix_condition(first.value) or { return none }
	if !middle.value.contains('} else {') || !last.value.trim_space().ends_with('}}') {
		return none
	}
	cond := t.simple_nested_condition_expr(cond_text) or { return none }
	result_name := t.new_temp('nested_interp')
	decl := t.make_decl_assign_typed(result_name, t.make_string_literal(''), 'string')
	outer_pending := t.pending_stmts.clone()

	t.pending_stmts.clear()
	then_expr := t.transform_string_interp_part(then_id)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	then_body << t.make_assign(t.make_ident(result_name), then_expr)

	t.pending_stmts.clear()
	else_expr := t.transform_string_interp_part(else_id)
	mut else_body := []flat.NodeId{}
	t.drain_pending(mut else_body)
	else_body << t.make_assign(t.make_ident(result_name), else_expr)

	t.pending_stmts = outer_pending
	t.pending_stmts << decl
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_block(else_body))
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'string')
	return result
}

fn nested_if_interp_prefix_condition(value string) ?string {
	clean := value.trim_space()
	if !clean.starts_with('\$' + '{if ') {
		return none
	}
	mut open := -1
	for i := clean.len - 1; i >= 0; i-- {
		if clean[i] == `{` {
			open = i
			break
		}
	}
	if open <= 5 {
		return none
	}
	return clean[5..open].trim_space()
}

fn (mut t Transformer) transform_nested_match_string_interp_node(node flat.Node) ?flat.NodeId {
	if node.children_count != 5 {
		return none
	}
	first_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	middle_id := t.a.child(&node, 2)
	else_id := t.a.child(&node, 3)
	last_id := t.a.child(&node, 4)
	first := t.a.nodes[int(first_id)]
	middle := t.a.nodes[int(middle_id)]
	last := t.a.nodes[int(last_id)]
	if first.kind != .string_literal || middle.kind != .string_literal
		|| last.kind != .string_literal {
		return none
	}
	subject_text, label_text := nested_match_interp_prefix(first.value) or { return none }
	if !nested_match_has_else_separator(middle.value) || !last.value.trim_space().ends_with('}}') {
		return none
	}
	subject, _ := t.simple_nested_interp_expr(subject_text) or { return none }
	label, _ := t.simple_nested_interp_expr(label_text) or { return none }
	cond := t.make_infix(.eq, subject, label)
	t.set_node_typ(int(cond), 'bool')
	return t.lower_nested_two_branch_string_expr(cond, then_id, else_id)
}

fn (mut t Transformer) lower_nested_two_branch_string_expr(cond flat.NodeId, then_id flat.NodeId, else_id flat.NodeId) flat.NodeId {
	result_name := t.new_temp('nested_interp')
	decl := t.make_decl_assign_typed(result_name, t.make_string_literal(''), 'string')
	outer_pending := t.pending_stmts.clone()

	t.pending_stmts.clear()
	then_expr := t.transform_string_interp_part(then_id)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	then_body << t.make_assign(t.make_ident(result_name), then_expr)

	t.pending_stmts.clear()
	else_expr := t.transform_string_interp_part(else_id)
	mut else_body := []flat.NodeId{}
	t.drain_pending(mut else_body)
	else_body << t.make_assign(t.make_ident(result_name), else_expr)

	t.pending_stmts = outer_pending
	t.pending_stmts << decl
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_block(else_body))
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'string')
	return result
}

fn nested_match_interp_prefix(value string) ?(string, string) {
	clean := value.trim_space()
	if !clean.starts_with('\$' + '{match ') {
		return none
	}
	first_open := nested_top_level_byte(clean, `{`, 8) or { return none }
	mut second_open := -1
	for i := clean.len - 1; i > first_open; i-- {
		if clean[i] == `{` {
			second_open = i
			break
		}
	}
	if second_open <= first_open {
		return none
	}
	subject := clean[8..first_open].trim_space()
	label := clean[first_open + 1..second_open].trim_space()
	if subject.len == 0 || label.len == 0 {
		return none
	}
	return subject, label
}

fn nested_match_has_else_separator(value string) bool {
	for i, ch in value {
		if ch != `}` {
			continue
		}
		mut j := i + 1
		for j < value.len && nested_interp_space_byte(value[j]) {
			j++
		}
		if j + 4 > value.len || value[j..j + 4] != 'else' {
			continue
		}
		j += 4
		for j < value.len && nested_interp_space_byte(value[j]) {
			j++
		}
		if j < value.len && value[j] == `{` {
			return true
		}
	}
	return false
}

fn nested_interp_space_byte(ch u8) bool {
	return ch == ` ` || ch == `\t` || ch == `\n` || ch == `\r`
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
	saved_in_string_interp_part := t.in_string_interp_part
	t.in_string_interp_part = true
	// route a value `match`/`if` interpolation operand (e.g. `'${match x { ... }}'`)
	// through its target type so its propagating arms are lowered as values.
	mut transformed := t.transform_value_operand(expr_id)
	t.in_string_interp_part = saved_in_string_interp_part
	// The source annotation remains `?T` inside `if value != none`, but the
	// transformed expression is the narrowed `.value` selector. Prefer that
	// smartcast type so interpolation stringifies `T`, not a second Option wrapper.
	mut typ := ''
	key := t.expr_key(expr_id)
	if key.len > 0 {
		for sc in t.smartcasts_for(key) {
			if sc.sum_type_name == option_unwrap_marker {
				typ = sc.variant_name
			}
		}
	}
	if typ.len == 0 {
		typ = t.raw_alias_type_for_expr(expr_id)
	}
	expr_node := t.a.nodes[int(expr_id)]
	if typ.len == 0 && expr_node.kind == .ident {
		raw_var_type := t.raw_var_type(expr_node.value)
		if t.is_optional_type_name(raw_var_type) {
			typ = raw_var_type
		}
	}
	if typ.len == 0 {
		typ = t.declared_selector_pointer_alias_type(expr_id) or { '' }
	}
	if typ.len == 0 {
		raw_type := t.raw_expr_type_without_smartcast(expr_id)
		if t.is_optional_type_name(raw_type) {
			typ = raw_type
		}
	}
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
	if ref_typ := t.string_interp_interface_smartcast_ref_type(expr_id) {
		transformed = t.make_prefix(.amp, transformed)
		t.set_node_typ(int(transformed), ref_typ)
		typ = ref_typ
	}
	is_shared_ident := expr_node.kind == .ident
		&& (t.raw_var_type(expr_node.value).trim_space().starts_with('shared ')
			|| t.local_decl_is_shared_before(expr_node.value, expr_id))
	if format != 'p' && is_shared_ident {
		// A shared scalar identifier has pointer-shaped semantic storage, but its
		// transformed expression is already the wrapper's `.val` field.
		for typ.starts_with('shared ') {
			typ = typ[7..].trim_space()
		}
		if typ.starts_with('&') {
			typ = typ[1..]
		}
		// Do not reuse the source ident here: its checker annotation describes the
		// lock-wrapper storage pointer and can reintroduce a dereference when the
		// synthesized str call is transformed. Cgen still resolves the fresh ident
		// through the shared declaration and emits its `.val` field.
		transformed = t.make_ident(expr_node.value)
		t.set_node_typ(int(transformed), typ)
		shared_value_name := t.new_temp('shared_str_value')
		t.pending_stmts << t.make_decl_assign_typed(shared_value_name, transformed, typ)
		t.set_var_type(shared_value_name, typ)
		transformed = t.make_ident(shared_value_name)
		t.set_node_typ(int(transformed), typ)
	} else if format != 'p' && expr_node.kind == .ident
		&& t.string_interp_needs_value_read(expr_node.value, typ) {
		transformed = t.make_prefix(.mul, transformed)
		typ = typ[1..]
	}
	if typ.len == 0 {
		typ = 'string'
	}
	if format.len > 0 && t.normalize_type_alias(typ) == 'string'
		&& t.string_interp_borrows_array_accessor_field(expr_id) {
		// Formatting may return its input unchanged when no padding is needed. Give it
		// independent storage so a returned formatted value cannot retain the array field.
		transformed = t.make_compiler_default_clone_value(transformed, 'string', false)
	}
	return t.wrap_formatted_string_conversion(transformed, typ, format)
}

fn (t &Transformer) string_interp_borrows_array_accessor_field(id flat.NodeId) bool {
	if isnil(t.tc) {
		return false
	}
	mut current := id
	for int(current) >= 0 && int(current) < t.a.nodes.len {
		node := t.a.nodes[int(current)]
		if node.kind in [.paren, .selector] && node.children_count > 0 {
			current = t.a.child(&node, 0)
			continue
		}
		return node.kind == .call && t.tc.array_accessor_result_is_borrowed(current)
	}
	return false
}

fn (t &Transformer) string_interp_interface_smartcast_ref_type(expr_id flat.NodeId) ?string {
	key := t.expr_key(expr_id)
	if key.len == 0 {
		return none
	}
	sc := t.find_smartcast(key) or { return none }
	source_type := t.trim_pointer_type(t.original_expr_type(expr_id))
	if !t.is_interface_type_name(source_type) {
		return none
	}
	target_type := t.trim_pointer_type(t.smartcast_target_type(sc))
	// Builtin scalar values (notably string) are represented by structs in C,
	// but a language-level smartcast yields the value, not an aggregate pointer.
	if types.is_builtin_type_name(target_type) {
		return none
	}
	if aggregate := t.stringify_aggregate_type_name(target_type) {
		return '&${aggregate}'
	}
	return none
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
	// A name whose own tracked type is already `&T` is genuinely pointer-backed
	// storage (mutable for-in bindings, `@[heap]`-promoted locals, ...). Those
	// still read as a plain value everywhere else via `pointer_value_rvalues`
	// (see e.g. `heap_escaping_source_decl`); string conversion must match, or a
	// heap-promoted local prints as a nil-checked pointer instead of its value.
	if t.pointer_value_rvalues[name] {
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
	if lhs.kind != .ident {
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
	if lhs.value !in t.escaping_amp_ptrs {
		return false
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
	dup := t.make_memdup_call_for_type(addr, local_type)
	return t.make_cast('&${local_type}', dup, '&${local_type}')
}

fn transform_struct_decl_alignment_is_set(typ string) bool {
	for part in typ.split(',') {
		if part == 'aligned' || part.starts_with('aligned=') {
			return true
		}
	}
	return false
}

fn transform_struct_decl_alignment_value(typ string) string {
	for part in typ.split(',') {
		if part.starts_with('aligned=') {
			return part.all_after('=')
		}
	}
	return ''
}

fn (t &Transformer) struct_alignment_for_type(type_name string) ?string {
	mut candidates := []string{}
	clean := t.trim_pointer_type(t.normalize_type_alias(type_name))
	for candidate in [clean, type_name] {
		if candidate.len > 0 && candidate !in candidates {
			candidates << candidate
		}
		base, _, is_generic := generic_app_parts(candidate)
		if is_generic && base.len > 0 && base !in candidates {
			candidates << base
		}
		if candidate.contains('.') {
			short := candidate.all_after_last('.')
			if short.len > 0 && short !in candidates {
				candidates << short
			}
		}
	}
	for candidate in candidates {
		info := t.structs[candidate] or { continue }
		if info.is_aligned {
			return info.alignment
		}
	}
	return none
}

fn (t &Transformer) struct_alignment_type_name(type_name string) string {
	clean := t.trim_pointer_type(t.normalize_type_alias(type_name))
	for candidate in [clean, type_name] {
		info := t.structs[candidate] or { continue }
		if info.module in ['', 'main'] {
			return 'main.${info.name}'
		}
		if info.module != 'builtin' && !candidate.contains('.') {
			return '${info.module}.${info.name}'
		}
		return candidate
	}
	return type_name
}

fn (mut t Transformer) make_memdup_call_for_type(addr flat.NodeId, type_name string) flat.NodeId {
	size := t.make_sizeof_type(type_name)
	if align := t.struct_alignment_for_type(type_name) {
		align_arg := if align.len > 0 {
			t.make_int_literal_typed(align, 'usize')
		} else {
			t.make_call_typed('__alignof__', [
				t.make_ident(t.struct_alignment_type_name(type_name)),
			], 'usize')
		}
		return t.make_non_aliasing_allocation_call('v3_aligned_memdup', [addr, size, align_arg], 'voidptr')
	}
	return t.make_non_aliasing_allocation_call('memdup', [addr, size], 'voidptr')
}

fn (mut t Transformer) make_non_aliasing_allocation_call(name string, args []flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << t.make_ident(name)
	for arg in args {
		t.a.children << arg
	}
	return t.a.add_node(flat.Node{
		kind: .call
		children_start: start
		children_count: flat.child_count(1 + args.len)
		value: non_aliasing_allocation_call_marker
		typ: typ
	})
}

// heapable_value_type reports whether a local of this declared type can be moved to the heap
// as a `&T` — a plain value type, not an already-reference / container / optional type (those
// either carry their own indirection or are not addressable as a single `T`).
fn (t &Transformer) heapable_value_type(typ string) bool {
	if typ.len == 0 || typ.starts_with('&') || typ.starts_with('[]') || typ.starts_with('map[')
		|| typ.starts_with('?') || typ.starts_with('!') || typ.starts_with('[') || typ == 'unknown'
		|| typ == 'void' {
		return false
	}
	// Function values are already pointers in C. `&callback` is accepted as the
	// same callable value and must not move the function-pointer slot to the heap.
	return isnil(t.tc) || types.unalias_type(t.tc.parse_type(typ)) !is types.FnType
}

// heap_attr_struct_type reports whether `typ` is a plain (non-pointer) struct type
// declared `@[heap]`. Such structs must always be heap-allocated at construction, not
// only when escape analysis detects an address later leaving the stack frame.
fn (t &Transformer) heap_attr_struct_type(typ string) bool {
	if isnil(t.tc) || !t.heapable_value_type(typ) || typ.starts_with('thread ') {
		return false
	}
	clean_type := types.unalias_type(t.tc.parse_type(typ))
	if clean_type !is types.Struct {
		return false
	}
	return t.tc.type_has_declaration_attribute(clean_type, 'heap')
}

fn (mut t Transformer) collect_exclusive_closure_return_fns() {
	if t.exclusive_closure_returns_done {
		return
	}
	t.exclusive_closure_returns_done = true
	t.exclusive_closure_return_fns.clear()
	old_module := t.cur_module
	defer {
		t.cur_module = old_module
	}
	mut module_name := ''
	mut literal_pending := false
	mut span_cost := 0
	t.literal_fn_decls = []int{cap: 64}
	t.fn_scan_costs = []int{len: t.a.nodes.len}
	for idx in 0 .. t.a.nodes.len {
		node := t.a.nodes[idx]
		span_cost += match node.kind {
			.call, .struct_init { 8 }
			.selector { 6 }
			.assign, .decl_assign, .selector_assign, .index_assign { 5 }
			.array_literal, .array_init, .map_init, .fn_literal, .lambda_expr, .string_interp { 4 }
			.index, .if_expr, .match_stmt, .for_stmt, .for_in_stmt, .select_stmt { 3 }
			.infix, .cast_expr, .as_expr, .or_expr, .return_stmt { 2 }
			else { 1 }
		}
		if node.kind in [.fn_literal, .lambda_expr] {
			literal_pending = true
		} else if node.kind == .fn_decl {
			if literal_pending {
				t.literal_fn_decls << idx
			}
			literal_pending = false
			t.fn_scan_costs[idx] = span_cost
		} else if node.kind in [.const_decl, .global_decl] {
			literal_pending = false
		}
		if node.kind in [.file, .module_decl, .struct_decl, .type_decl, .interface_decl, .enum_decl,
			.import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl] {
			span_cost = 0
		}
		if node.kind == .module_decl {
			module_name = node.value
			t.cur_module = module_name
			continue
		}
		if node.kind != .fn_decl || node.generic_params().len > 0
			|| !t.fn_decl_returns_fn_pointer(node, module_name)
			|| !t.fn_decl_exclusively_returns_fresh_closure(node) {
			continue
		}
		qname := if module_name in ['', 'main', 'builtin']
			|| node.value.starts_with('${module_name}.') {
			node.value
		} else {
			'${module_name}.${node.value}'
		}
		t.exclusive_closure_return_fns[qname] = true
		if module_name in ['main', 'builtin'] && !node.value.contains('.') {
			t.exclusive_closure_return_fns['${module_name}.${node.value}'] = true
		}
	}
	t.literal_fn_decls_ready = true
}

fn (t &Transformer) fn_decl_returns_fn_pointer(node flat.Node, module_name string) bool {
	if node.typ.starts_with('fn ') {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	qname := if module_name in ['', 'main', 'builtin'] || node.value.starts_with('${module_name}.') {
		node.value
	} else {
		'${module_name}.${node.value}'
	}
	if ret := t.tc.fn_ret_types[qname] {
		return ret is types.FnType || (ret is types.Alias && ret.base_type is types.FnType)
	}
	if qname != node.value {
		if ret := t.tc.fn_ret_types[node.value] {
			return ret is types.FnType || (ret is types.Alias && ret.base_type is types.FnType)
		}
	}
	return t.is_fn_pointer_type_name(node.typ)
}

fn (t &Transformer) fn_decl_exclusively_returns_fresh_closure(node flat.Node) bool {
	mut body_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if t.a.nodes[int(child_id)].kind != .param {
			body_ids << child_id
		}
	}
	mut candidate_decls := map[string]int{}
	mut binding_counts := map[string]int{}
	for i in 0 .. node.children_count {
		t.collect_fresh_closure_return_candidates(t.a.child(&node, i), mut candidate_decls, mut binding_counts)
	}
	mut return_exprs := []flat.NodeId{}
	for id in body_ids {
		t.collect_outer_return_exprs(id, mut return_exprs)
	}
	if return_exprs.len == 0 {
		return false
	}
	for return_id in return_exprs {
		if t.expr_allocates_fresh_runtime_closure(return_id) {
			continue
		}
		name := t.wrapped_ident_name(return_id) or { return false }
		decl_id := candidate_decls[name] or { return false }
		if binding_counts[name] != 1
			|| !t.closure_return_candidate_is_unaliased(body_ids, name, flat.NodeId(decl_id)) {
			return false
		}
	}
	return true
}

fn (t &Transformer) collect_fresh_closure_return_candidates(id flat.NodeId, mut candidates map[string]int, mut binding_counts map[string]int) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if node.kind == .param && node.value.len > 0 {
		binding_counts[node.value] = (binding_counts[node.value] or { 0 }) + 1
		return
	}
	if node.kind == .decl_assign && node.children_count == 2 {
		lhs := t.a.child_node(&node, 0)
		if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
			binding_counts[lhs.value] = (binding_counts[lhs.value] or { 0 }) + 1
			if t.expr_allocates_fresh_runtime_closure(t.a.child(&node, 1)) {
				candidates[lhs.value] = int(id)
			}
		}
	}
	for i in 0 .. node.children_count {
		t.collect_fresh_closure_return_candidates(t.a.child(&node, i), mut candidates, mut binding_counts)
	}
}

fn (t &Transformer) collect_outer_return_exprs(id flat.NodeId, mut return_exprs []flat.NodeId) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if node.kind == .return_stmt {
		if node.children_count != 1 {
			return_exprs << flat.NodeId(-1)
			return
		}
		return_exprs << t.a.child(&node, 0)
		return
	}
	for i in 0 .. node.children_count {
		t.collect_outer_return_exprs(t.a.child(&node, i), mut return_exprs)
	}
}

fn (t &Transformer) expr_allocates_fresh_runtime_closure(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .cast_expr, .expr_stmt {
			if node.children_count == 1 {
				return t.expr_allocates_fresh_runtime_closure(t.a.child(&node, 0))
			}
		}
		.block {
			return node.children_count > 0
				&& t.expr_allocates_fresh_runtime_closure(t.a.child(&node, node.children_count - 1))
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			for i in 1 .. node.children_count {
				if !t.expr_allocates_fresh_runtime_closure(t.a.child(&node, i)) {
					return false
				}
			}
			return true
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			for i in 1 .. node.children_count {
				branch_id := t.a.child(&node, i)
				branch := t.a.nodes[int(branch_id)]
				if branch.kind != .match_branch
					|| !t.expr_allocates_fresh_runtime_closure(branch_id) {
					return false
				}
			}
			return true
		}
		.match_branch {
			body_start := if node.value == 'else' { 0 } else { t.count_conds(node) }
			return node.children_count > body_start
				&& t.expr_allocates_fresh_runtime_closure(t.a.child(&node, node.children_count - 1))
		}
		else {}
	}
	return t.fn_literal_has_runtime_captures(id)
		|| t.bound_method_value_allocates_runtime_closure(id)
}

fn (t &Transformer) fresh_runtime_closure_expr_captures_name(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .cast_expr, .expr_stmt {
			return node.children_count == 1
				&& t.fresh_runtime_closure_expr_captures_name(t.a.child(&node, 0), name)
		}
		.block, .match_branch {
			return node.children_count > 0
				&& t.fresh_runtime_closure_expr_captures_name(t.a.child(&node, node.children_count - 1), name)
		}
		.if_expr {
			for i in 1 .. node.children_count {
				if t.fresh_runtime_closure_expr_captures_name(t.a.child(&node, i), name) {
					return true
				}
			}
			return false
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				if t.fresh_runtime_closure_expr_captures_name(t.a.child(&node, i), name) {
					return true
				}
			}
			return false
		}
		else {
			return t.fn_literal_captures_name(id, name)
		}
	}
}

fn (t &Transformer) fresh_runtime_closure_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .cast_expr, .expr_stmt {
			if node.children_count == 1 {
				return t.fresh_runtime_closure_type(t.a.child(&node, 0))
			}
		}
		.block, .match_branch {
			if node.children_count > 0 {
				return t.fresh_runtime_closure_type(t.a.child(&node, node.children_count - 1))
			}
		}
		.if_expr {
			if node.children_count >= 3 {
				return t.fresh_runtime_closure_type(t.a.child(&node, 1))
			}
		}
		.match_stmt {
			if node.children_count >= 2 {
				return t.fresh_runtime_closure_type(t.a.child(&node, 1))
			}
		}
		else {}
	}
	if node.kind == .selector && t.bound_method_value_allocates_runtime_closure(id) {
		if fn_type := fn_value_type_name_from_type(t.tc.resolve_type(id)) {
			return t.normalize_type_alias(fn_type)
		}
		method_name := t.resolve_receiver_method_name(t.a.child(&node, 0), node.value)
		params := t.tc.fn_param_types[method_name] or { []types.Type{} }
		ret := t.tc.fn_ret_types[method_name] or { types.Type(types.void_) }
		bound_params := if params.len > 1 { params[1..].clone() } else { []types.Type{} }
		return fn_literal_value_type_text(bound_params, ret.name())
	}
	if fn_type := t.fn_value_type_name(id) {
		return fn_type
	}
	return none
}

fn (mut t Transformer) set_fresh_runtime_closure_expr_type(id flat.NodeId, typ string) {
	if typ.len == 0 || int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	t.set_node_typ(int(id), typ)
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		t.set_fresh_runtime_closure_expr_type(t.a.child(&node, 0), typ)
	}
}

fn (mut t Transformer) mark_fresh_runtime_closure_methods_used(id flat.NodeId) {
	if int(id) < 0 || int(id) >= t.a.nodes.len || isnil(t.tc) {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector && node.children_count > 0
		&& t.bound_method_value_allocates_runtime_closure(id) {
		method_name := t.resolve_receiver_method_name(t.a.child(&node, 0), node.value)
		if method_name.len > 0 {
			t.mark_fn_used_name(method_name)
		}
		return
	}
	for i in 0 .. node.children_count {
		t.mark_fresh_runtime_closure_methods_used(t.a.child(&node, i))
	}
}

fn (t &Transformer) wrapped_ident_name(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value.len > 0 {
		return node.value
	}
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.wrapped_ident_name(t.a.child(&node, 0))
	}
	return none
}

fn (t &Transformer) closure_return_candidate_is_unaliased(body_ids []flat.NodeId, name string, decl_id flat.NodeId) bool {
	for id in body_ids {
		if !t.closure_return_candidate_use_is_safe(id, name, decl_id) {
			return false
		}
	}
	return true
}

fn (t &Transformer) closure_return_candidate_use_is_safe(id flat.NodeId, name string, decl_id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || id == decl_id {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind == .return_stmt && node.children_count == 1 {
		if returned_name := t.wrapped_ident_name(t.a.child(&node, 0)) {
			if returned_name == name {
				return true
			}
		}
	}
	if node.kind == .ident && node.value == name {
		return false
	}
	for i in 0 .. node.children_count {
		if !t.closure_return_candidate_use_is_safe(t.a.child(&node, i), name, decl_id) {
			return false
		}
	}
	return true
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
		stmts << t.make_stack_value_decl_assign_typed(tmp, transformed_init, elem_typ)
		addr := t.make_prefix(.amp, t.make_ident(tmp))
		dup := t.make_memdup_call_for_type(addr, elem_typ)
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
// `p := &v`, `r := Interface(&v)` or `r := Interface(p)` declarations whose
// pointer/interface alias is later returned or retained in a map/nonlocal field. Such a `v`
// is a local value whose address escapes, so it must be heap-copied (V auto-heaps it); the names
// are recorded in `escaping_amp_ptrs` and consumed by the decl-assign transform. The walk is
// structural apart from using resolved expression types to distinguish method-value selectors
// from fields; the source type is checked at rewrite time when `v`'s type is known.
fn (mut t Transformer) mark_escaping_amp_ptrs(body_ids []flat.NodeId) {
	t.reset_escaping_amp_state()
	if t.fast_escape_precheck {
		if t.item_escape_scan_known {
			if !t.item_escape_scan_needed {
				return
			}
		} else if !t.escape_scan_may_be_needed(body_ids) {
			return
		}
	}
	mut amp_ptrs := map[string]bool{}
	mut amp_sources := map[string][]string{} // pointer `p` -> possible source locals `v`
	mut ptr_aliases := map[string]string{} // copy `q := p` -> aliased pointer `p`
	mut method_value_receivers := map[string]string{} // callback `cb := p.method` -> receiver `p`
	mut closure_capture_aliases := map[string][]string{} // callback `cb := fn [p]` -> captures
	mut interface_boxes := map[string]bool{}
	mut returned := map[string]bool{}
	mut local_stack_names := map[string]bool{}
	for binding in t.var_types {
		if binding.name.len > 0 {
			local_stack_names[binding.name] = true
		}
	}
	mut local_stack_added := []string{}
	for id in body_ids {
		t.scan_escape_pass(id, mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, true)
	}
	// A pointer may be returned through a copy (`p := &v; q := p; return q`): `q` is collected
	// as returned but `p` is not. A method value can hide the same pointer one level deeper
	// (`p := &v; cb := p.read; return cb`), as can a capturing literal assigned to a local.
	// Propagate "returned" backward through all alias kinds until a fixpoint, then recognise
	// `p` and its source `v` as escaping below.
	for _ in 0 .. ptr_aliases.len + method_value_receivers.len + closure_capture_aliases.len {
		mut changed := false
		for q, p in ptr_aliases {
			if q in returned && p !in returned {
				returned[p] = true
				changed = true
			}
		}
		for callback, receiver in method_value_receivers {
			if callback in returned && receiver !in returned {
				returned[receiver] = true
				changed = true
			}
		}
		for callback, captures in closure_capture_aliases {
			if callback !in returned {
				continue
			}
			for capture in captures {
				if capture !in returned {
					returned[capture] = true
					changed = true
				}
			}
		}
		if !changed {
			break
		}
	}
	for name, _ in amp_ptrs {
		if name in returned {
			t.escaping_amp_ptrs[name] = true
			for src in amp_sources[name] {
				t.escaping_amp_sources[src] = true
			}
		}
	}
	for name, _ in interface_boxes {
		if name in returned {
			// An interface box initialized from `&local` already aliases a local
			// that is moved to the heap above. Copying the boxed concrete value
			// again would lose that alias and can pull unrelated interface
			// implementers into the generated C.
			if sources := amp_sources[name] {
				if sources.len > 0 {
					continue
				}
			}
			t.escaping_interface_box_locals[name] = true
		}
	}
}

fn (t &Transformer) escape_scan_may_be_needed(body_ids []flat.NodeId) bool {
	for id in body_ids {
		if t.escape_subtree_may_need_scan(id) {
			return true
		}
	}
	return false
}

@[direct_array_access]
fn (t &Transformer) escape_subtree_may_need_scan(id flat.NodeId) bool {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[idx]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if node.kind == .prefix && node.op == .amp {
		return true
	}
	// Passing a value local to a void-pointer parameter implicitly takes its
	// address. Checked direct calls expose their exact signature here; unresolved
	// function-value calls stay conservative and use the full escape walk.
	if node.kind == .call && node.children_count > 1 {
		if isnil(t.tc) {
			return true
		}
		name := t.tc.resolved_call_name(id) or { return true }
		params := t.tc.fn_param_types[name] or { return true }
		for param in params {
			if escape_type_is_void_pointer(param) {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.escape_subtree_may_need_scan(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

@[direct_array_access]
fn (mut t Transformer) collect_mut_capture_sources(id flat.NodeId) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	if int(id) in t.local_closure_cleanup_decls || int(id) in t.local_closure_cleanup_values {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .fn_literal {
		for i in 0 .. node.children_count {
			capture := t.a.child_node(&node, i)
			if capture.kind == .ident && capture.is_mut && capture.value.len > 0 {
				t.mut_fixed_array_capture_sources[capture.value] = true
			}
		}
		// A nested literal's body belongs to the lifted function, not this frame.
		return
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) in t.local_closure_cleanup_values {
			continue
		}
		// A capturing literal used as the callee is consumed by this call. It is
		// materialized as a scoped temporary during call lowering, so its mutable
		// fixed-array captures can keep borrowing this frame's storage.
		if node.kind == .call && i == 0 && t.fn_literal_has_runtime_captures(child_id) {
			continue
		}
		t.collect_mut_capture_sources(child_id)
	}
}

fn (mut t Transformer) reset_escaping_amp_state() {
	t.escaping_amp_ptrs.clear()
	t.escaping_amp_sources.clear()
	t.heaped_amp_locals.clear()
	t.escaping_interface_box_locals.clear()
	t.mut_fixed_array_capture_sources.clear()
	// Cleared per function: heaped locals add their names below (in heap_escaping_source_decl);
	// for-loop element vars set and restore their own entries within the loop body.
	t.pointer_value_lvalues.clear()
	t.pointer_value_rvalues.clear()
	t.addr_lvalue_pointer_locals.clear()
}

fn (mut t Transformer) mark_local_closure_cleanup_decls(body_ids []flat.NodeId) {
	t.local_closure_cleanup_decls.clear()
	t.local_closure_cleanup_values.clear()
	t.local_closure_cleanup_assigns.clear()
	t.local_closure_field_cleanups.clear()
	// The compiler build excludes the optional backend sources that contain
	// capturing literals, and its remaining function literal is non-capturing.
	// Avoid the whole-body escape analysis for every compiler function.
	if t.building_v {
		return
	}
	mut candidates := []LocalClosureDeclCandidate{}
	mut field_candidates := []LocalClosureFieldCandidate{}
	for id in body_ids {
		t.collect_local_closure_cleanup_candidates(id, true, flat.NodeId(-1), mut candidates, mut field_candidates)
	}
	for candidate in candidates {
		mut bound_uses := map[int]bool{}
		mut bound_assigns := map[int]bool{}
		decl_id := flat.NodeId(candidate.decl_id)
		scope_id := flat.NodeId(candidate.scope_id)
		if int(scope_id) < 0 {
			t.collect_local_closure_binding_uses(body_ids, candidate.name, decl_id, false, mut bound_uses, mut bound_assigns)
		} else {
			t.collect_local_closure_binding_uses_in_scope(scope_id, candidate.name, decl_id, false, mut bound_uses, mut bound_assigns)
		}
		t.collect_local_closure_alias_binding_uses(body_ids, scope_id, mut bound_uses)
		mut escapes := false
		for body_id in body_ids {
			if t.local_closure_binding_escapes(body_id, bound_uses, decl_id) {
				escapes = true
				break
			}
		}
		if !escapes {
			decl := t.a.nodes[candidate.decl_id]
			if decl.children_count == 2 {
				t.local_closure_cleanup_decls[candidate.decl_id] = candidate.name
			} else {
				t.local_closure_cleanup_values[candidate.source_id] = candidate.name
			}
			for assign_id, _ in bound_assigns {
				t.local_closure_cleanup_assigns[assign_id] = candidate.name
			}
			t.mark_local_method_value_receiver_borrow(flat.NodeId(candidate.source_id))
		}
	}
	for candidate in field_candidates {
		mut bound_uses := map[int]bool{}
		mut bound_assigns := map[int]bool{}
		decl_id := if candidate.decl_id >= 0 {
			flat.NodeId(candidate.decl_id)
		} else {
			t.local_closure_binding_decl_in_scope(body_ids, candidate) or { continue }
		}
		if candidate.aggregate_scope < 0 {
			t.collect_local_closure_binding_uses(body_ids, candidate.aggregate_name, decl_id, false, mut bound_uses, mut bound_assigns)
		} else {
			t.collect_local_closure_binding_uses_in_scope(flat.NodeId(candidate.aggregate_scope), candidate.aggregate_name, decl_id, false, mut bound_uses, mut bound_assigns)
		}
		mut scope_owned_field_reads := map[int]bool{}
		aliases_escape := t.collect_scope_owned_local_closure_field_aliases(body_ids, bound_uses, candidate.field_key, mut scope_owned_field_reads)
		mut escapes := false
		if aliases_escape {
			escapes = true
		} else {
			for body_id in body_ids {
				if t.local_closure_field_binding_escapes(body_id, bound_uses, decl_id, candidate.field_key, scope_owned_field_reads) {
					escapes = true
					break
				}
			}
		}
		if !escapes {
			t.local_closure_field_cleanups[candidate.source_id] = true
			t.mark_local_method_value_receiver_borrow(flat.NodeId(candidate.source_id))
		}
	}
}

fn (mut t Transformer) mark_local_method_value_receiver_borrow(owner_id flat.NodeId) {
	if int(owner_id) < 0 || int(owner_id) >= t.a.nodes.len {
		return
	}
	owner := t.a.nodes[int(owner_id)]
	if owner.kind in [.decl_assign, .assign, .selector_assign, .index_assign]
		&& owner.children_count == 2 {
		t.mark_local_method_value_receiver_borrows_in_expr(t.a.child(&owner, 1))
		return
	}
	t.mark_local_method_value_receiver_borrows_in_expr(owner_id)
}

fn (mut t Transformer) mark_local_method_value_receiver_borrows_in_expr(id flat.NodeId) {
	if int(id) < 0 || int(id) >= t.a.nodes.len || isnil(t.tc) {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector && node.children_count > 0 && t.tc.expr_is_method_value(id) {
		mut params := node.generic_params().clone()
		if flat.method_value_borrow_receiver_marker !in params {
			params << flat.method_value_borrow_receiver_marker
			t.set_node_generic_params(int(id), params)
		}
		return
	}
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	for i in 0 .. node.children_count {
		t.mark_local_method_value_receiver_borrows_in_expr(t.a.child(&node, i))
	}
}

fn (t &Transformer) local_closure_binding_decl_in_scope(body_ids []flat.NodeId, candidate LocalClosureFieldCandidate) ?flat.NodeId {
	mut ids := body_ids.clone()
	if candidate.aggregate_scope >= 0 {
		scope := t.a.nodes[candidate.aggregate_scope]
		ids = []flat.NodeId{cap: int(scope.children_count)}
		for i in 0 .. scope.children_count {
			ids << t.a.child(&scope, i)
		}
	}
	mut decl_id := flat.NodeId(-1)
	for id in ids {
		if int(id) < 0 || int(id) >= t.a.nodes.len {
			continue
		}
		if int(id) == candidate.owner_id {
			if int(decl_id) >= 0 {
				return decl_id
			}
			return none
		}
		node := t.a.nodes[int(id)]
		if node.kind != .decl_assign {
			continue
		}
		for i := 0; i < int(node.children_count); i += 2 {
			lhs := t.a.child_node(&node, i)
			if lhs.kind == .ident && lhs.value == candidate.aggregate_name {
				decl_id = id
			}
		}
	}
	return none
}

fn (mut t Transformer) collect_local_closure_cleanup_candidates(id flat.NodeId, statement_position bool, scope_id flat.NodeId, mut candidates []LocalClosureDeclCandidate, mut field_candidates []LocalClosureFieldCandidate) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		// Nested callable bodies are transformed in their own frame, where their
		// local closure cleanup candidates are collected separately.
		return
	}
	if statement_position && node.kind == .decl_assign && node.children_count >= 2 {
		lhs_count := t.multi_assign_lhs_count(node)
		rhs_count := t.multi_assign_rhs_count(node)
		pair_count := if lhs_count < rhs_count { lhs_count } else { rhs_count }
		for i in 0 .. pair_count {
			lhs := t.a.nodes[int(t.multi_assign_lhs_id(node, i))]
			rhs_id := t.multi_assign_rhs_id(node, i)
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
				continue
			}
			if t.expr_allocates_fresh_runtime_closure(rhs_id)
				&& !t.fresh_runtime_closure_expr_captures_name(rhs_id, lhs.value) {
				candidates << LocalClosureDeclCandidate{
					source_id: int(rhs_id)
					decl_id: int(id)
					scope_id: int(scope_id)
					name: lhs.value
				}
			}
			t.collect_local_closure_initializer_field_candidates(id, id, rhs_id, lhs.value, lhs.value, scope_id, mut field_candidates)
		}
	}
	if statement_position && node.kind in [.assign, .selector_assign, .index_assign]
		&& node.op == .assign && node.children_count == 2 {
		lhs_id := t.a.child(&node, 0)
		rhs_id := t.a.child(&node, 1)
		if aggregate_name := t.escape_address_root_name(lhs_id) {
			field_prefix := t.expr_key(lhs_id)
			t.collect_local_closure_initializer_field_candidates(flat.NodeId(-1), id, rhs_id, aggregate_name, field_prefix, scope_id, mut field_candidates)
		}
		if t.expr_allocates_fresh_runtime_closure(rhs_id) {
			if aggregate_name := t.escape_address_root_name(lhs_id) {
				mut field_key := t.expr_key(lhs_id)
				lhs := t.a.nodes[int(lhs_id)]
				index_key_is_static := lhs.kind == .index && lhs.children_count >= 2
					&& t.a.child_node(&lhs, 1).kind in [.int_literal, .string_literal, .char_literal,
						.enum_val]
				if lhs.kind == .index && !index_key_is_static && lhs.children_count > 0 {
					base_key := t.expr_key(t.a.child(&lhs, 0))
					if base_key.len > 0 {
						field_key = '${base_key}[*]'
					}
				}
				if field_key.len > aggregate_name.len
					&& (field_key.starts_with('${aggregate_name}.')
						|| (lhs.kind == .index && field_key.starts_with('${aggregate_name}['))) {
					field_candidates << LocalClosureFieldCandidate{
						source_id: int(id)
						owner_id: int(id)
						decl_id: -1
						aggregate_name: aggregate_name
						aggregate_scope: int(scope_id)
						field_key: field_key
					}
				}
			}
		}
	}
	if statement_position && node.kind == .expr_stmt && node.children_count == 1 {
		append_id := t.a.child(&node, 0)
		append := t.a.nodes[int(append_id)]
		if append.kind == .infix && append.op == .left_shift && append.children_count >= 2 {
			lhs_id := t.a.child(&append, 0)
			rhs_id := t.a.child(&append, 1)
			lhs_type := t.clean_array_append_lhs_type(t.lvalue_type(lhs_id))
			if lhs_type.starts_with('[]') {
				t.collect_local_closure_append_candidates(append_id, id, lhs_id, rhs_id, scope_id, mut field_candidates)
			}
		}
	}
	match node.kind {
		.block {
			for i in 0 .. node.children_count {
				t.collect_local_closure_cleanup_candidates(t.a.child(&node, i), true, id, mut candidates, mut field_candidates)
			}
		}
		.for_stmt {
			for i in 0 .. node.children_count {
				child_scope := if i >= 3 { id } else { scope_id }
				t.collect_local_closure_cleanup_candidates(t.a.child(&node, i), i >= 3, child_scope, mut candidates, mut field_candidates)
			}
		}
		.for_in_stmt {
			body_start := if node.value.int() >= 0 && node.value.int() <= node.children_count {
				node.value.int()
			} else {
				2
			}
			for i in 0 .. node.children_count {
				child_scope := if i >= body_start { id } else { scope_id }
				t.collect_local_closure_cleanup_candidates(t.a.child(&node, i), i >= body_start, child_scope, mut candidates, mut field_candidates)
			}
		}
		.match_branch {
			condition_count := if node.value == 'else' { 0 } else { node.value.int() }
			for i in 0 .. node.children_count {
				child_scope := if i >= condition_count { id } else { scope_id }
				t.collect_local_closure_cleanup_candidates(t.a.child(&node, i), i >= condition_count, child_scope, mut candidates, mut field_candidates)
			}
		}
		else {
			for i in 0 .. node.children_count {
				t.collect_local_closure_cleanup_candidates(t.a.child(&node, i), false, scope_id, mut candidates, mut field_candidates)
			}
		}
	}
}

fn (mut t Transformer) collect_local_closure_append_candidates(append_id flat.NodeId, owner_id flat.NodeId, lhs_id flat.NodeId, rhs_id flat.NodeId, scope_id flat.NodeId, mut field_candidates []LocalClosureFieldCandidate) {
	aggregate_name := t.escape_address_root_name(lhs_id) or { return }
	field_prefix := t.expr_key(lhs_id)
	if field_prefix.len == 0 {
		return
	}
	t.collect_local_closure_append_value_candidates(owner_id, rhs_id, append_id, aggregate_name, '${field_prefix}[*]', scope_id, mut field_candidates)
}

fn (mut t Transformer) collect_local_closure_append_value_candidates(owner_id flat.NodeId, value_id flat.NodeId, cleanup_source_id flat.NodeId, aggregate_name string, field_key string, scope_id flat.NodeId, mut field_candidates []LocalClosureFieldCandidate) {
	if int(value_id) < 0 || int(value_id) >= t.a.nodes.len {
		return
	}
	value := t.a.nodes[int(value_id)]
	if (value.kind in [.paren, .cast_expr, .as_expr] || (value.kind == .postfix
		&& value.op == .not)) && value.children_count == 1 {
		t.collect_local_closure_append_value_candidates(owner_id, t.a.child(&value, 0), cleanup_source_id, aggregate_name, field_key, scope_id, mut field_candidates)
		return
	}
	if value.kind == .array_literal {
		for i in 0 .. value.children_count {
			elem_id := t.a.child(&value, i)
			elem := t.a.nodes[int(elem_id)]
			if elem.kind == .prefix && elem.value == '...' {
				return
			}
			t.collect_local_closure_append_value_candidates(owner_id, elem_id, elem_id, aggregate_name, field_key, scope_id, mut field_candidates)
		}
		return
	}
	if !t.expr_allocates_fresh_runtime_closure(value_id) {
		return
	}
	field_candidates << LocalClosureFieldCandidate{
		source_id: int(cleanup_source_id)
		owner_id: int(owner_id)
		decl_id: -1
		aggregate_name: aggregate_name
		aggregate_scope: int(scope_id)
		field_key: field_key
	}
}

fn (mut t Transformer) collect_local_closure_initializer_field_candidates(decl_id flat.NodeId, owner_id flat.NodeId, init_id flat.NodeId, aggregate_name string, field_prefix string, scope_id flat.NodeId, mut field_candidates []LocalClosureFieldCandidate) {
	if int(init_id) < 0 || int(init_id) >= t.a.nodes.len {
		return
	}
	init := t.a.nodes[int(init_id)]
	if init.kind == .postfix && init.op == .not && init.children_count == 1 {
		t.collect_local_closure_initializer_field_candidates(decl_id, owner_id, t.a.child(&init, 0), aggregate_name, field_prefix, scope_id, mut field_candidates)
		return
	}
	if init.kind == .array_literal {
		for i in 0 .. init.children_count {
			value := t.a.child_node(&init, i)
			if value.kind == .prefix && value.value == '...' {
				// A spread makes following runtime indices data-dependent. Keep this
				// conservative until aggregate paths can represent dynamic slots, but
				// preserve the statically indexed prefix collected before the spread.
				return
			}
			value_id := t.a.child(&init, i)
			value_key := '${field_prefix}[${i}]'
			if t.expr_allocates_fresh_runtime_closure(value_id) {
				field_candidates << LocalClosureFieldCandidate{
					source_id: int(value_id)
					owner_id: int(owner_id)
					decl_id: int(decl_id)
					aggregate_name: aggregate_name
					aggregate_scope: int(scope_id)
					field_key: value_key
				}
				continue
			}
			t.collect_local_closure_initializer_field_candidates(decl_id, owner_id, value_id, aggregate_name, value_key, scope_id, mut field_candidates)
		}
		return
	}
	if init.kind == .map_init {
		for i := 0; i + 1 < int(init.children_count); i += 2 {
			key_id := t.a.child(&init, i)
			key := t.a.nodes[int(key_id)]
			if key.kind == .prefix && key.value == '...' {
				return
			}
			value_id := t.a.child(&init, i + 1)
			key_is_static := key.kind in [.int_literal, .string_literal, .char_literal, .enum_val]
			mut value_key := '${field_prefix}[*]'
			if key_is_static {
				key_part := t.expr_key_part(key_id)
				if key_part.len == 0 {
					continue
				}
				value_key = '${field_prefix}[${key_part}]'
			}
			if t.expr_allocates_fresh_runtime_closure(value_id) {
				field_candidates << LocalClosureFieldCandidate{
					source_id: int(value_id)
					owner_id: int(owner_id)
					decl_id: int(decl_id)
					aggregate_name: aggregate_name
					aggregate_scope: int(scope_id)
					field_key: value_key
				}
				continue
			}
			t.collect_local_closure_initializer_field_candidates(decl_id, owner_id, value_id, aggregate_name, value_key, scope_id, mut field_candidates)
		}
		return
	}
	if init.kind != .struct_init {
		return
	}
	info := t.lookup_struct_info(init.value) or { StructInfo{} }
	for i in 0 .. init.children_count {
		field_id := t.a.child(&init, i)
		if int(field_id) < 0 || int(field_id) >= t.a.nodes.len {
			continue
		}
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			''
		}
		if field_name.len == 0 {
			continue
		}
		field_key := '${field_prefix}.${field_name}'
		value_id := t.a.child(&field, 0)
		if t.expr_allocates_fresh_runtime_closure(value_id) {
			field_candidates << LocalClosureFieldCandidate{
				source_id: int(field_id)
				owner_id: int(owner_id)
				decl_id: int(decl_id)
				aggregate_name: aggregate_name
				aggregate_scope: int(scope_id)
				field_key: field_key
			}
			continue
		}
		t.collect_local_closure_initializer_field_candidates(decl_id, owner_id, value_id, aggregate_name, field_key, scope_id, mut field_candidates)
	}
}

fn (t &Transformer) collect_local_closure_binding_uses(ids []flat.NodeId, name string, decl_id flat.NodeId, initially_bound bool, mut bound_uses map[int]bool, mut bound_assigns map[int]bool) {
	mut is_bound := initially_bound
	for id in ids {
		is_bound = t.collect_local_closure_binding_uses_in_stmt(id, name, decl_id, is_bound, mut bound_uses, mut bound_assigns)
	}
}

fn (t &Transformer) collect_local_closure_binding_uses_in_scope(id flat.NodeId, name string, decl_id flat.NodeId, initially_bound bool, mut bound_uses map[int]bool, mut bound_assigns map[int]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.a.child(&node, i)
	}
	t.collect_local_closure_binding_uses(children, name, decl_id, initially_bound, mut bound_uses, mut bound_assigns)
}

fn (t &Transformer) collect_local_closure_binding_uses_in_stmt(id flat.NodeId, name string, decl_id flat.NodeId, is_bound bool, mut bound_uses map[int]bool, mut bound_assigns map[int]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return is_bound
	}
	if id == decl_id {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind == .param && node.value == name {
		return false
	}
	if node.kind == .decl_assign && node.children_count == 2 {
		lhs := t.a.child_node(&node, 0)
		if lhs.kind == .ident && lhs.value == name {
			if is_bound {
				t.collect_local_closure_binding_uses_in_expr(t.a.child(&node, 1), name, decl_id, true, mut bound_uses, mut bound_assigns)
			}
			return false
		}
	}
	if is_bound {
		t.collect_local_closure_binding_uses_in_expr(id, name, decl_id, true, mut bound_uses, mut bound_assigns)
	}
	return is_bound
}

fn (t &Transformer) collect_local_closure_binding_uses_in_expr(id flat.NodeId, name string, decl_id flat.NodeId, is_bound bool, mut bound_uses map[int]bool, mut bound_assigns map[int]bool) {
	if !is_bound || int(id) < 0 || int(id) >= t.a.nodes.len || id == decl_id {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value == name {
			bound_uses[int(id)] = true
		}
		return
	}
	if node.kind == .decl_assign && node.children_count == 2 {
		t.collect_local_closure_binding_uses_in_expr(t.a.child(&node, 1), name, decl_id, true, mut bound_uses, mut bound_assigns)
		return
	}
	if node.kind == .assign && node.op == .assign {
		for i := 0; i < int(node.children_count); i += 2 {
			lhs_id := t.a.child(&node, i)
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value == name {
				bound_uses[int(lhs_id)] = true
				bound_assigns[int(id)] = true
			} else {
				t.collect_local_closure_binding_uses_in_expr(lhs_id, name, decl_id, true, mut bound_uses, mut bound_assigns)
			}
			if i + 1 < int(node.children_count) {
				t.collect_local_closure_binding_uses_in_expr(t.a.child(&node, i + 1), name, decl_id, true, mut bound_uses, mut bound_assigns)
			}
		}
		return
	}
	match node.kind {
		.block, .for_stmt, .for_in_stmt, .match_branch, .fn_literal, .lambda_expr {
			t.collect_local_closure_binding_uses_in_scope(id, name, decl_id, true, mut bound_uses, mut bound_assigns)
			return
		}
		else {}
	}
	for i in 0 .. node.children_count {
		t.collect_local_closure_binding_uses_in_expr(t.a.child(&node, i), name, decl_id, true, mut bound_uses, mut bound_assigns)
	}
}

fn (t &Transformer) collect_local_closure_alias_binding_uses(body_ids []flat.NodeId, scope_id flat.NodeId, mut bound_uses map[int]bool) {
	mut ids := body_ids.clone()
	if int(scope_id) >= 0 {
		scope := t.a.nodes[int(scope_id)]
		ids = []flat.NodeId{cap: int(scope.children_count)}
		for i in 0 .. scope.children_count {
			ids << t.a.child(&scope, i)
		}
	}
	for id in ids {
		t.collect_local_closure_alias_binding_uses_in_node(id, true, scope_id, ids, mut bound_uses)
	}
}

fn (t &Transformer) collect_local_closure_alias_binding_uses_in_node(id flat.NodeId, statement_position bool, scope_id flat.NodeId, root_ids []flat.NodeId, mut bound_uses map[int]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if statement_position && node.kind == .decl_assign && node.children_count >= 2 {
		lhs_count := t.multi_assign_lhs_count(node)
		rhs_count := t.multi_assign_rhs_count(node)
		pair_count := if lhs_count < rhs_count { lhs_count } else { rhs_count }
		for i in 0 .. pair_count {
			lhs_id := t.multi_assign_lhs_id(node, i)
			lhs := t.a.nodes[int(lhs_id)]
			rhs_id := t.multi_assign_rhs_id(node, i)
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_'
				|| !t.local_closure_alias_source_is_bound(rhs_id, bound_uses) {
				continue
			}
			bound_uses[int(lhs_id)] = true
			mut alias_assigns := map[int]bool{}
			if int(scope_id) < 0 {
				t.collect_local_closure_binding_uses(root_ids, lhs.value, id, false, mut bound_uses, mut alias_assigns)
			} else {
				t.collect_local_closure_binding_uses_in_scope(scope_id, lhs.value, id, false, mut bound_uses, mut alias_assigns)
			}
		}
	}
	match node.kind {
		.block {
			for i in 0 .. node.children_count {
				t.collect_local_closure_alias_binding_uses_in_node(t.a.child(&node, i), true, id, root_ids, mut bound_uses)
			}
		}
		.for_stmt {
			for i in 0 .. node.children_count {
				child_scope := if i >= 3 { id } else { scope_id }
				t.collect_local_closure_alias_binding_uses_in_node(t.a.child(&node, i), i >= 3, child_scope, root_ids, mut bound_uses)
			}
		}
		.for_in_stmt {
			body_start := if node.value.int() >= 0 && node.value.int() <= node.children_count {
				node.value.int()
			} else {
				2
			}
			for i in 0 .. node.children_count {
				child_scope := if i >= body_start { id } else { scope_id }
				t.collect_local_closure_alias_binding_uses_in_node(t.a.child(&node, i), i >= body_start, child_scope, root_ids, mut bound_uses)
			}
		}
		.match_branch {
			condition_count := if node.value == 'else' { 0 } else { node.value.int() }
			for i in 0 .. node.children_count {
				child_scope := if i >= condition_count { id } else { scope_id }
				t.collect_local_closure_alias_binding_uses_in_node(t.a.child(&node, i), i >= condition_count, child_scope, root_ids, mut bound_uses)
			}
		}
		else {
			for i in 0 .. node.children_count {
				t.collect_local_closure_alias_binding_uses_in_node(t.a.child(&node, i), false, scope_id, root_ids, mut bound_uses)
			}
		}
	}
}

fn (t &Transformer) local_closure_alias_source_is_bound(id flat.NodeId, bound_uses map[int]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return int(id) in bound_uses
	}
	if node.kind in [.paren, .cast_expr, .as_expr] && node.children_count == 1 {
		return t.local_closure_alias_source_is_bound(t.a.child(&node, 0), bound_uses)
	}
	return false
}

fn (t &Transformer) collect_scope_owned_local_closure_field_aliases(body_ids []flat.NodeId, aggregate_uses map[int]bool, field_key string, mut scope_owned_reads map[int]bool) bool {
	mut aliases := []LocalClosureDeclCandidate{}
	for id in body_ids {
		t.collect_local_closure_field_alias_candidates(id, true, flat.NodeId(-1), aggregate_uses, field_key, mut aliases)
	}
	for alias in aliases {
		mut alias_uses := map[int]bool{}
		mut alias_assigns := map[int]bool{}
		decl_id := flat.NodeId(alias.decl_id)
		scope_id := flat.NodeId(alias.scope_id)
		if alias.scope_id < 0 {
			t.collect_local_closure_binding_uses(body_ids, alias.name, decl_id, false, mut alias_uses, mut alias_assigns)
		} else {
			t.collect_local_closure_binding_uses_in_scope(scope_id, alias.name, decl_id, false, mut alias_uses, mut alias_assigns)
		}
		t.collect_local_closure_alias_binding_uses(body_ids, scope_id, mut alias_uses)
		for body_id in body_ids {
			if t.local_closure_binding_escapes(body_id, alias_uses, decl_id) {
				return true
			}
		}
		scope_owned_reads[alias.source_id] = true
	}
	return false
}

fn (t &Transformer) collect_local_closure_field_alias_candidates(id flat.NodeId, statement_position bool, scope_id flat.NodeId, aggregate_uses map[int]bool, field_key string, mut aliases []LocalClosureDeclCandidate) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if statement_position && node.kind == .decl_assign && node.children_count >= 2 {
		lhs_count := t.multi_assign_lhs_count(node)
		rhs_count := t.multi_assign_rhs_count(node)
		pair_count := if lhs_count < rhs_count { lhs_count } else { rhs_count }
		for i in 0 .. pair_count {
			lhs := t.a.nodes[int(t.multi_assign_lhs_id(node, i))]
			rhs_id := t.multi_assign_rhs_id(node, i)
			if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_'
				&& t.local_closure_field_alias_source_matches(rhs_id, aggregate_uses, field_key) {
				aliases << LocalClosureDeclCandidate{
					source_id: int(rhs_id)
					decl_id: int(id)
					scope_id: int(scope_id)
					name: lhs.value
				}
			}
		}
	}
	match node.kind {
		.block {
			for i in 0 .. node.children_count {
				t.collect_local_closure_field_alias_candidates(t.a.child(&node, i), true, id, aggregate_uses, field_key, mut aliases)
			}
		}
		.for_stmt {
			for i in 0 .. node.children_count {
				child_scope := if i >= 3 { id } else { scope_id }
				t.collect_local_closure_field_alias_candidates(t.a.child(&node, i), i >= 3, child_scope, aggregate_uses, field_key, mut aliases)
			}
		}
		.for_in_stmt {
			body_start := if node.value.int() >= 0 && node.value.int() <= node.children_count {
				node.value.int()
			} else {
				2
			}
			for i in 0 .. node.children_count {
				child_scope := if i >= body_start { id } else { scope_id }
				t.collect_local_closure_field_alias_candidates(t.a.child(&node, i), i >= body_start, child_scope, aggregate_uses, field_key, mut aliases)
			}
		}
		.match_branch {
			condition_count := if node.value == 'else' { 0 } else { node.value.int() }
			for i in 0 .. node.children_count {
				child_scope := if i >= condition_count { id } else { scope_id }
				t.collect_local_closure_field_alias_candidates(t.a.child(&node, i), i >= condition_count, child_scope, aggregate_uses, field_key, mut aliases)
			}
		}
		else {
			for i in 0 .. node.children_count {
				t.collect_local_closure_field_alias_candidates(t.a.child(&node, i), false, scope_id, aggregate_uses, field_key, mut aliases)
			}
		}
	}
}

fn (t &Transformer) local_closure_field_alias_source_matches(id flat.NodeId, aggregate_uses map[int]bool, field_key string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr, .as_expr, .expr_stmt] && node.children_count == 1 {
		return t.local_closure_field_alias_source_matches(t.a.child(&node, 0), aggregate_uses, field_key)
	}
	if node.kind !in [.selector, .index] || node.children_count == 0
		|| !t.local_closure_binding_mentioned(t.a.child(&node, 0), aggregate_uses) {
		return false
	}
	return local_closure_field_key_matches(field_key, t.expr_key(id))
}

fn (t &Transformer) fn_literal_has_runtime_captures(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.fn_literal_has_runtime_captures(t.a.child(&node, 0))
	}
	if node.kind != .fn_literal {
		return false
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 && child.value !in t.active_generic_params {
			return true
		}
	}
	return false
}

fn (t &Transformer) bound_method_value_allocates_runtime_closure(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.bound_method_value_allocates_runtime_closure(t.a.child(&node, 0))
	}
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	if t.tc.expr_is_method_value(id) {
		return true
	}
	return t.resolve_receiver_method_name(t.a.child(&node, 0), node.value).len > 0
}

fn (t &Transformer) immediate_bound_method_value_allocates_runtime_closure(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.expr_allocates_fresh_runtime_closure(t.a.child(&node, 0))
	}
	return node.kind in [.if_expr, .match_stmt] && t.expr_allocates_fresh_runtime_closure(id)
}

fn (t &Transformer) local_closure_binding_mentioned(id flat.NodeId, bound_uses map[int]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	if int(id) in bound_uses {
		return true
	}
	node := t.a.nodes[int(id)]
	for i in 0 .. node.children_count {
		if t.local_closure_binding_mentioned(t.a.child(&node, i), bound_uses) {
			return true
		}
	}
	return false
}

fn (t &Transformer) local_closure_binding_escapes(id flat.NodeId, bound_uses map[int]bool, decl_id flat.NodeId) bool {
	return t.local_closure_binding_escapes_in_value_context(id, bound_uses, decl_id, false)
}

// A callback value that only flows back into its own local remains scope-owned.
fn (t &Transformer) local_closure_binding_escapes_in_value_context(id flat.NodeId, bound_uses map[int]bool, decl_id flat.NodeId, flows_back_to_local bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || id == decl_id {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return int(id) in bound_uses && !flows_back_to_local
	}
	if node.kind == .decl_assign {
		lhs_count := t.multi_assign_lhs_count(node)
		rhs_count := t.multi_assign_rhs_count(node)
		pair_count := if lhs_count < rhs_count { lhs_count } else { rhs_count }
		for i in 0 .. pair_count {
			lhs_id := t.multi_assign_lhs_id(node, i)
			rhs_id := t.multi_assign_rhs_id(node, i)
			if t.local_closure_binding_escapes_in_value_context(rhs_id, bound_uses, decl_id, int(lhs_id) in bound_uses) {
				return true
			}
		}
		return false
	}
	if node.kind == .assign && node.op == .assign {
		for i := 0; i < int(node.children_count); i += 2 {
			lhs_id := t.a.child(&node, i)
			lhs_is_local := int(lhs_id) in bound_uses
			if !lhs_is_local {
				if t.local_closure_binding_escapes(lhs_id, bound_uses, decl_id) {
					return true
				}
			}
			if i + 1 >= int(node.children_count) {
				continue
			}
			rhs_id := t.a.child(&node, i + 1)
			if t.local_closure_binding_escapes_in_value_context(rhs_id, bound_uses, decl_id, lhs_is_local) {
				return true
			}
		}
		return false
	}
	if flows_back_to_local {
		match node.kind {
			.paren, .cast_expr, .expr_stmt {
				if node.children_count == 1 {
					return t.local_closure_binding_escapes_in_value_context(t.a.child(&node, 0), bound_uses, decl_id, true)
				}
			}
			.block {
				for i in 0 .. node.children_count {
					if t.local_closure_binding_escapes_in_value_context(t.a.child(&node, i), bound_uses, decl_id, i == int(node.children_count) - 1) {
						return true
					}
				}
				return false
			}
			.if_expr {
				for i in 0 .. node.children_count {
					if t.local_closure_binding_escapes_in_value_context(t.a.child(&node, i), bound_uses, decl_id, i > 0) {
						return true
					}
				}
				return false
			}
			.match_stmt {
				for i in 0 .. node.children_count {
					if t.local_closure_binding_escapes_in_value_context(t.a.child(&node, i), bound_uses, decl_id, i > 0) {
						return true
					}
				}
				return false
			}
			.match_branch {
				body_start := if node.value == 'else' { 0 } else { t.count_conds(node) }
				for i in 0 .. node.children_count {
					is_tail_value := i >= body_start && i == int(node.children_count) - 1
					if t.local_closure_binding_escapes_in_value_context(t.a.child(&node, i), bound_uses, decl_id, is_tail_value) {
						return true
					}
				}
				return false
			}
			else {}
		}
	}
	if node.kind == .spawn_expr {
		return t.local_closure_binding_mentioned(id, bound_uses)
	}
	if node.kind == .call && node.children_count > 0 {
		callee_id := t.a.child(&node, 0)
		if int(callee_id) in bound_uses {
			for i in 1 .. node.children_count {
				if t.local_closure_binding_escapes(t.a.child(&node, i), bound_uses, decl_id) {
					return true
				}
			}
			return false
		}
	}
	for i in 0 .. node.children_count {
		if t.local_closure_binding_escapes(t.a.child(&node, i), bound_uses, decl_id) {
			return true
		}
	}
	return false
}

fn (t &Transformer) local_closure_field_binding_escapes(id flat.NodeId, bound_uses map[int]bool, decl_id flat.NodeId, field_key string, scope_owned_reads map[int]bool) bool {
	return t.local_closure_field_binding_escapes_in_context(id, bound_uses, decl_id, field_key, false, false, scope_owned_reads)
}

fn (t &Transformer) local_closure_field_binding_escapes_in_context(id flat.NodeId, bound_uses map[int]bool, decl_id flat.NodeId, field_key string, field_is_scope_owned bool, aggregate_is_selector_base bool, scope_owned_reads map[int]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || id == decl_id {
		return false
	}
	if int(id) in scope_owned_reads {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return int(id) in bound_uses && !aggregate_is_selector_base && !field_is_scope_owned
	}
	if node.kind in [.selector, .index] && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		if t.local_closure_binding_mentioned(base_id, bound_uses) {
			key := t.expr_key(id)
			if local_closure_field_key_matches(field_key, key) && !field_is_scope_owned {
				return true
			}
			if key.len > 0 && (field_key.starts_with('${key}.') || field_key.starts_with('${key}['))
				&& !aggregate_is_selector_base {
				return true
			}
			return t.local_closure_field_binding_escapes_in_context(base_id, bound_uses, decl_id, field_key, false, true, scope_owned_reads)
		}
	}
	if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
		lhs_id := t.a.child(&node, 0)
		lhs_key := t.expr_key(lhs_id)
		if field_key == '${lhs_key}[*]' {
			if t.local_closure_field_binding_escapes_in_context(lhs_id, bound_uses, decl_id, field_key, true, true, scope_owned_reads) {
				return true
			}
			return t.local_closure_field_binding_escapes_in_context(t.a.child(&node, 1), bound_uses, decl_id, field_key, false, false, scope_owned_reads)
		}
	}
	if node.kind in [.assign, .selector_assign, .index_assign] && node.op == .assign {
		for i := 0; i < int(node.children_count); i += 2 {
			lhs_id := t.a.child(&node, i)
			lhs_key := t.expr_key(lhs_id)
			lhs_is_owned_field := local_closure_field_key_matches(field_key, lhs_key)
				|| (lhs_key.len > 0 && (field_key.starts_with('${lhs_key}.')
					|| field_key.starts_with('${lhs_key}[')))
			if t.local_closure_field_binding_escapes_in_context(lhs_id, bound_uses, decl_id, field_key, lhs_is_owned_field, false, scope_owned_reads) {
				return true
			}
			if i + 1 < int(node.children_count)
				&& t.local_closure_field_binding_escapes_in_context(t.a.child(&node, i + 1), bound_uses, decl_id, field_key, false, false, scope_owned_reads) {
				return true
			}
		}
		return false
	}
	if node.kind == .call && node.children_count > 0 {
		callee_id := t.a.child(&node, 0)
		callee_is_owned_field := local_closure_field_key_matches(field_key, t.expr_key(callee_id))
		if !callee_is_owned_field && t.local_closure_binding_mentioned(callee_id, bound_uses) {
			// An arbitrary aggregate method can retain its receiver. Only invoking the
			// callback field itself is proven to keep the closure scope-owned.
			return true
		}
		if t.local_closure_field_binding_escapes_in_context(callee_id, bound_uses, decl_id, field_key, callee_is_owned_field, false, scope_owned_reads) {
			return true
		}
		for i in 1 .. node.children_count {
			if t.local_closure_field_binding_escapes_in_context(t.a.child(&node, i), bound_uses, decl_id, field_key, false, false, scope_owned_reads) {
				return true
			}
		}
		return false
	}
	for i in 0 .. node.children_count {
		if t.local_closure_field_binding_escapes_in_context(t.a.child(&node, i), bound_uses, decl_id, field_key, false, false, scope_owned_reads) {
			return true
		}
	}
	return false
}

fn local_closure_field_key_matches(field_key string, key string) bool {
	if field_key == key {
		return true
	}
	wildcard_index := field_key.index('[*]') or { return false }
	prefix := field_key[..wildcard_index]
	if !key.starts_with(prefix) || prefix.len >= key.len || key[prefix.len] != `[` {
		return false
	}
	mut depth := 0
	mut close_index := -1
	for i := prefix.len; i < key.len; i++ {
		if key[i] == `[` {
			depth++
		} else if key[i] == `]` {
			depth--
			if depth == 0 {
				close_index = i
				break
			}
		}
	}
	if close_index < 0 {
		return false
	}
	return local_closure_field_key_matches(field_key[wildcard_index + 3..], key[close_index + 1..])
}

fn add_escape_amp_source(mut amp_sources map[string][]string, ptr_name string, source_name string) {
	mut sources := amp_sources[ptr_name]
	if source_name !in sources {
		sources << source_name
		amp_sources[ptr_name] = sources
	}
}

fn (t &Transformer) escape_address_root_name(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if node.value.len > 0 {
				return node.value
			}
		}
		.paren {
			if node.children_count == 1 {
				return t.escape_address_root_name(t.a.child(&node, 0))
			}
		}
		.selector, .index {
			if node.children_count > 0 {
				return t.escape_address_root_name(t.a.child(&node, 0))
			}
		}
		else {}
	}

	return none
}

fn escape_alias_sources(name string, amp_sources map[string][]string, ptr_aliases map[string]string) []string {
	mut current := name
	mut seen := map[string]bool{}
	for _ in 0 .. ptr_aliases.len + 1 {
		if current.len == 0 || seen[current] {
			break
		}
		seen[current] = true
		if sources := amp_sources[current] {
			return sources
		}
		current = ptr_aliases[current] or { break }
	}
	return []string{}
}

fn (t &Transformer) escape_address_sources(id flat.NodeId, amp_sources map[string][]string, ptr_aliases map[string]string) []string {
	root := t.escape_address_root_name(id) or { return []string{} }
	sources := escape_alias_sources(root, amp_sources, ptr_aliases)
	if sources.len > 0 {
		return sources
	}
	return [root]
}

fn (t &Transformer) escape_method_value_receiver(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.escape_method_value_receiver(t.a.child(&node, 0))
	}
	if node.kind != .selector || node.children_count == 0
		|| !t.is_fn_pointer_type_name(t.node_type(id)) {
		return none
	}
	return t.escape_address_root_name(t.a.child(&node, 0))
}

fn (t &Transformer) method_value_has_pointer_receiver(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.method_value_has_pointer_receiver(t.a.child(&node, 0))
	}
	if node.kind != .selector || node.children_count == 0 || !t.tc.expr_is_method_value(id) {
		return false
	}
	base_id := t.a.child(&node, 0)
	method_name := t.resolve_receiver_method_name(base_id, node.value)
	if params := t.tc.fn_param_types[method_name] {
		return params.len > 0 && params[0] is types.Pointer
	}
	return t.tc.mut_receiver_methods[method_name]
}

// method_receiver_is_reference reports whether the method `method` resolved on `base_id`
// takes its receiver by reference (a `mut` or `&` receiver). Such a receiver must keep its
// lvalue identity when stabilized (only its dynamic base/index components spilled) so the
// call still mutates through the lvalue; an ordinary by-value receiver is spilled by value so
// its value is read in source order — a later branch prelude that mutates its container cannot
// then change the observed receiver value.
fn (t &Transformer) method_receiver_is_reference(base_id flat.NodeId, method string) bool {
	if isnil(t.tc) {
		return false
	}
	method_name := t.resolve_receiver_method_name(base_id, method)
	if method_name.len == 0 {
		return false
	}
	if params := t.tc.fn_param_types[method_name] {
		return params.len > 0 && params[0] is types.Pointer
	}
	return t.tc.mut_receiver_methods[method_name]
}

fn (mut t Transformer) mark_callback_method_value_receiver_escape(id flat.NodeId, amp_sources map[string][]string, ptr_aliases map[string]string, local_stack_names map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	if t.method_value_has_pointer_receiver(id) {
		receiver := t.escape_method_value_receiver(id) or { return }
		mut sources := escape_alias_sources(receiver, amp_sources, ptr_aliases)
		if sources.len == 0 && receiver in local_stack_names {
			sources = [receiver]
		}
		for source in sources {
			if source in local_stack_names {
				// A callee may retain a callback argument. Move the original receiver,
				// rather than a closure-private copy, so synchronous calls and escaped
				// callbacks observe the same mutable object.
				t.escaping_amp_sources[source] = true
			}
		}
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if node.kind in [.field_init, .paren, .cast_expr, .as_expr, .struct_init, .array_literal,
		.array_init, .map_init, .if_expr, .match_stmt, .match_branch] {
		for i in 0 .. node.children_count {
			t.mark_callback_method_value_receiver_escape(t.a.child(&node, i), amp_sources, ptr_aliases, local_stack_names)
		}
	}
}

fn (mut t Transformer) scan_escape_pointer_write(lhs flat.Node, rhs flat.Node, mut amp_ptrs map[string]bool, mut amp_sources map[string][]string, mut ptr_aliases map[string]string) {
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	if rhs.kind == .prefix && rhs.op == .amp && rhs.children_count > 0 {
		sources := t.escape_address_sources(t.a.child(&rhs, 0), amp_sources, ptr_aliases)
		if sources.len > 0 {
			amp_ptrs[lhs.value] = true
			for source_name in sources {
				add_escape_amp_source(mut amp_sources, lhs.value, source_name)
			}
			ptr_aliases.delete(lhs.value)
		}
		return
	}
	if rhs.kind == .cast_expr && rhs.children_count == 1
		&& t.resolve_interface_type_name(rhs.value).len > 0 {
		cast_arg := t.a.nodes[int(t.a.child(&rhs, 0))]
		if cast_arg.kind == .prefix && cast_arg.op == .amp && cast_arg.children_count > 0 {
			sources := t.escape_address_sources(t.a.child(&cast_arg, 0), amp_sources, ptr_aliases)
			if sources.len > 0 {
				amp_ptrs[lhs.value] = true
				for source_name in sources {
					add_escape_amp_source(mut amp_sources, lhs.value, source_name)
				}
				ptr_aliases.delete(lhs.value)
			}
		} else if cast_arg.kind == .ident && cast_arg.value.len > 0 {
			sources := escape_alias_sources(cast_arg.value, amp_sources, ptr_aliases)
			if sources.len > 0 {
				amp_ptrs[lhs.value] = true
				for source in sources {
					add_escape_amp_source(mut amp_sources, lhs.value, source)
				}
				ptr_aliases[lhs.value] = cast_arg.value
			}
		}
		return
	}
	if rhs.kind == .ident && rhs.value.len > 0 {
		// `q := p` / `q = p` aliases an existing pointer; recorded so a returned alias still
		// marks the underlying `p := &v` as escaping.
		ptr_aliases[lhs.value] = rhs.value
	}
}

fn (t &Transformer) escape_fn_literal_capture_names(id flat.NodeId) []string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return []string{}
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.escape_fn_literal_capture_names(t.a.child(&node, 0))
	}
	if node.kind != .fn_literal {
		return []string{}
	}
	mut captures := []string{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 && child.value !in t.active_generic_params && child.value !in captures {
			captures << child.value
		}
	}
	return captures
}

// scan_escape_pass recursively collects, in a function-body subtree, (a) the LHS
// names of `p := &local`, `p = &local`, `r := Interface(&local)` and
// `r := Interface(pointer_alias)` assignments into `amp_ptrs` (with all source root
// names in `amp_sources[p]`), (b) plain pointer copies `q := p`/`q = p` into
// `ptr_aliases[q] = p`, (c) callback aliases `cb := p.method` into
// `method_value_receivers`, (d) captured names in `cb := fn [p]` into
// `closure_capture_aliases`, and (e) every ident name appearing inside a return
// statement, map value assignment, or nonlocal field store into `returned`.
@[direct_array_access]
fn (mut t Transformer) scan_escape_pass(id flat.NodeId, mut amp_ptrs map[string]bool, mut amp_sources map[string][]string, mut ptr_aliases map[string]string, mut method_value_receivers map[string]string, mut closure_capture_aliases map[string][]string, mut interface_boxes map[string]bool, mut returned map[string]bool, mut local_stack_names map[string]bool, mut local_stack_added []string, can_clear_interface_boxes bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.if_expr, .match_stmt, .match_branch, .for_stmt] {
		for i in 0 .. node.children_count {
			t.scan_escape_pass(t.a.child(&node, i), mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, false)
		}
		return
	}
	if node.kind == .block {
		scope_mark := local_stack_added.len
		for i in 0 .. node.children_count {
			t.scan_escape_pass(t.a.child(&node, i), mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, can_clear_interface_boxes)
		}
		pop_escape_local_stack_names(scope_mark, mut local_stack_names, mut local_stack_added)
		return
	}
	if node.kind == .for_in_stmt {
		t.scan_for_in_escape_pass(node, mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, can_clear_interface_boxes)
		return
	}
	// Nested function bodies have their own frame and escape analysis. Their
	// return expressions must not mark captures as escaping from the outer frame.
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if node.kind in [.decl_assign, .assign] && node.children_count >= 2 {
		mut declared_names := []string{}
		mut i := 0
		for i + 1 < node.children_count {
			lhs := t.a.nodes[int(t.a.child(&node, i))]
			rhs_id := t.a.child(&node, i + 1)
			rhs := t.a.nodes[int(rhs_id)]
			mut lhs_marks_interface_box := false
			if node.kind == .decl_assign && lhs.kind == .ident && lhs.value.len > 0 {
				declared_names << lhs.value
			}
			t.scan_escape_pointer_write(lhs, rhs, mut amp_ptrs, mut amp_sources, mut ptr_aliases)
			mut lhs_marks_method_value := false
			if lhs.kind == .ident && lhs.value.len > 0 {
				if receiver := t.escape_method_value_receiver(rhs_id) {
					method_value_receivers[lhs.value] = receiver
					lhs_marks_method_value = true
				}
			}
			if can_clear_interface_boxes && node.kind == .assign && lhs.kind == .ident
				&& lhs.value.len > 0 && !lhs_marks_method_value {
				method_value_receivers.delete(lhs.value)
			}
			mut lhs_marks_closure_capture := false
			if lhs.kind == .ident && lhs.value.len > 0 {
				captures := t.escape_fn_literal_capture_names(rhs_id)
				if captures.len > 0 {
					closure_capture_aliases[lhs.value] = captures
					lhs_marks_closure_capture = true
				}
			}
			if can_clear_interface_boxes && node.kind == .assign && lhs.kind == .ident
				&& lhs.value.len > 0 && !lhs_marks_closure_capture {
				closure_capture_aliases.delete(lhs.value)
			}
			if lhs.kind == .ident && lhs.value.len > 0 && rhs.kind == .ident && rhs.value.len > 0
				&& rhs.value in interface_boxes {
				interface_boxes[lhs.value] = true
				lhs_marks_interface_box = true
			} else if lhs.kind == .ident && lhs.value.len > 0
				&& t.interface_box_from_stack_pointer_source(rhs_id, amp_ptrs, ptr_aliases, local_stack_names) {
				interface_boxes[lhs.value] = true
				lhs_marks_interface_box = true
			}
			if can_clear_interface_boxes && node.kind == .assign && lhs.kind == .ident
				&& lhs.value.len > 0 && !lhs_marks_interface_box {
				interface_boxes.delete(lhs.value)
			}
			if lhs.kind == .ident && lhs.value.len > 0 {
				for source_name in t.escape_aggregate_address_sources(rhs_id, amp_sources, ptr_aliases) {
					add_escape_amp_source(mut amp_sources, lhs.value, source_name)
					amp_ptrs[lhs.value] = true
				}
			}
			i += 2
		}
		for name in declared_names {
			add_escape_local_stack_name(name, mut local_stack_names, mut local_stack_added)
		}
	}
	if node.kind == .return_stmt {
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			boxed_pointer := t.return_slot_is_boxed_pointer(child_id)
			if !boxed_pointer
				&& t.return_slot_consumes_pointer_value(child_id, i, int(node.children_count)) {
				continue
			}
			t.collect_return_escape_idents(child_id, mut returned)
			if boxed_pointer {
				// A direct `&local` is copied by interface/error boxing. Pointer aliases still need
				// the returned-name propagation above so their shared source is heaped.
				continue
			}
			for source_name in t.escape_aggregate_address_sources(child_id, amp_sources, ptr_aliases) {
				t.escaping_amp_sources[source_name] = true
			}
		}
	}
	if node.kind == .spawn_expr {
		t.mark_spawn_argument_address_escapes(node, amp_sources, ptr_aliases, local_stack_names)
	}
	if node.kind == .call && node.children_count > 1 {
		for i in 1 .. node.children_count {
			t.mark_callback_method_value_receiver_escape(t.a.child(&node, i), amp_sources, ptr_aliases, local_stack_names)
		}
		t.mark_implicit_voidptr_argument_escapes(id, node, local_stack_names)
	}
	if node.kind in [.assign, .selector_assign, .index_assign] && node.op == .assign
		&& node.children_count == 2 {
		lhs_id := t.a.child(&node, 0)
		if t.escape_index_assign_retains_value(lhs_id)
			|| (node.kind == .selector_assign
				&& t.escape_selector_assign_retains_value(lhs_id, amp_ptrs, ptr_aliases)) {
			rhs_id := t.a.child(&node, 1)
			// A map or caller-owned field may retain its value after this stack frame returns.
			// Track pointer aliases through `returned`, and record direct `&local` values
			// immediately.
			t.collect_return_escape_idents(rhs_id, mut returned)
			for source_name in t.escape_aggregate_address_sources(rhs_id, amp_sources, ptr_aliases) {
				t.escaping_amp_sources[source_name] = true
			}
		}
	}
	for i in 0 .. node.children_count {
		t.scan_escape_pass(t.a.child(&node, i), mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, can_clear_interface_boxes)
	}
}

fn (mut t Transformer) return_slot_is_boxed_pointer(id flat.NodeId) bool {
	if isnil(t.tc) || t.cur_fn_ret_type.len == 0 {
		return false
	}
	expected := t.tc.parse_type(t.cur_fn_ret_type)
	if expected is types.Interface {
		return escape_type_is_pointer(t.tc.resolve_type(id))
	}
	if expected is types.OptionType {
		return t.return_expr_is_propagated_err(id, expected.base_type.name())
	}
	if expected is types.ResultType {
		return t.return_expr_is_propagated_err(id, expected.base_type.name())
	}
	return false
}

fn (mut t Transformer) return_slot_consumes_pointer_value(id flat.NodeId, index int, count int) bool {
	if isnil(t.tc) || t.cur_fn_ret_type.len == 0 {
		return false
	}
	actual := t.tc.resolve_type(id)
	if !escape_type_is_pointer(actual) {
		return false
	}
	mut expected := t.tc.parse_type(t.cur_fn_ret_type)
	if expected is types.OptionType {
		expected = expected.base_type
	} else if expected is types.ResultType {
		expected = expected.base_type
	}
	if count > 1 {
		if expected is types.MultiReturn {
			if index < 0 || index >= expected.types.len {
				return false
			}
			expected = expected.types[index]
		} else {
			return false
		}
	}
	return escape_return_type_consumes_pointer_value(expected)
}

fn escape_type_is_pointer(typ types.Type) bool {
	if typ is types.Pointer {
		return true
	}
	if typ is types.Alias {
		return escape_type_is_pointer(typ.base_type)
	}
	return false
}

fn escape_return_type_consumes_pointer_value(typ types.Type) bool {
	if typ is types.Alias {
		return escape_return_type_consumes_pointer_value(typ.base_type)
	}
	return typ !is types.Pointer && typ !is types.Interface && typ !is types.SumType
		&& typ !is types.OptionType && typ !is types.ResultType && typ !is types.FnType
		&& typ !is types.MultiReturn && typ !is types.Unknown && typ !is types.Void
}

fn (mut t Transformer) mark_implicit_voidptr_argument_escapes(call_id flat.NodeId, call flat.Node, local_stack_names map[string]bool) {
	call_name := t.call_name_for_node(call_id, call)
	params := t.call_param_types_for_node(call_name, call)
	if params.len == 0 {
		return
	}
	param_offset := t.call_param_offset_for_node(call_name, call, params)
	for child_idx in 1 .. call.children_count {
		param_idx := child_idx - 1 + param_offset
		if param_idx < 0 || param_idx >= params.len
			|| !escape_type_is_void_pointer(params[param_idx]) {
			continue
		}
		mut arg_id := t.a.child(&call, child_idx)
		mut arg := t.a.nodes[int(arg_id)]
		for arg.kind in [.paren, .expr_stmt] && arg.children_count == 1 {
			arg_id = t.a.child(&arg, 0)
			arg = t.a.nodes[int(arg_id)]
		}
		if arg.kind != .ident || arg.value !in local_stack_names {
			continue
		}
		if !escape_type_is_struct_value(t.tc.resolve_type(arg_id)) {
			continue
		}
		// Passing a value local to a void pointer parameter implicitly takes its
		// address. The callee can retain that opaque pointer (callback userdata is
		// the common case), so preserve V's auto-heap behavior for the source local.
		t.escaping_amp_sources[arg.value] = true
	}
}

fn escape_type_is_void_pointer(typ types.Type) bool {
	if typ is types.Pointer {
		base := typ.base_type
		return base is types.Void || (base is types.Alias && escape_type_is_void_pointer(base))
	}
	if typ is types.Alias {
		return escape_type_is_void_pointer(typ.base_type)
	}
	return false
}

fn escape_type_is_struct_value(typ types.Type) bool {
	if typ is types.Struct {
		return true
	}
	if typ is types.Alias {
		return escape_type_is_struct_value(typ.base_type)
	}
	return false
}

fn (mut t Transformer) mark_spawn_argument_address_escapes(spawn_node flat.Node, amp_sources map[string][]string, ptr_aliases map[string]string, local_stack_names map[string]bool) {
	if spawn_node.children_count == 0 {
		return
	}
	call_id := t.a.child(&spawn_node, 0)
	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len {
		return
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 2 {
		return
	}
	for i in 1 .. call.children_count {
		arg_id := t.a.child(&call, i)
		for source in t.escape_aggregate_address_sources(arg_id, amp_sources, ptr_aliases) {
			if source in local_stack_names {
				// The spawned thread can outlive this frame. Move the original local
				// to the heap so explicit addresses and pointer aliases keep sharing
				// the same value instead of copying it into the thread argument block.
				t.escaping_amp_sources[source] = true
			}
		}
	}
}

fn (t &Transformer) escape_selector_assign_retains_value(lhs_id flat.NodeId, amp_ptrs map[string]bool, ptr_aliases map[string]string) bool {
	if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
		return false
	}
	mut root_id := lhs_id
	for int(root_id) >= 0 && int(root_id) < t.a.nodes.len {
		root := t.a.nodes[int(root_id)]
		if root.kind !in [.selector, .index, .paren] || root.children_count == 0 {
			break
		}
		root_id = t.a.child(&root, 0)
	}
	if int(root_id) < 0 || int(root_id) >= t.a.nodes.len {
		return false
	}
	root := t.a.nodes[int(root_id)]
	if root.kind == .ident {
		if root.value in t.mut_param_values {
			return true
		}
		if _ := t.global_ident_type(root.value) {
			return true
		}
	}
	root_type := t.normalize_type_alias(t.address_expr_type_name(root_id))
	if !address_expr_base_is_indirect_storage(root_type) {
		return false
	}
	return !t.escape_address_indirect_base_is_stack_backed(root_id, amp_ptrs, ptr_aliases)
}

fn (t &Transformer) escape_index_assign_retains_value(lhs_id flat.NodeId) bool {
	if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
		return false
	}
	mut lhs := t.a.nodes[int(lhs_id)]
	for lhs.kind == .selector && lhs.children_count > 0 {
		base_id := t.a.child(&lhs, 0)
		if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
			return false
		}
		lhs = t.a.nodes[int(base_id)]
	}
	if lhs.kind != .index || lhs.children_count == 0 {
		return false
	}
	base_id := t.a.child(&lhs, 0)
	return t.clean_map_type(t.address_expr_type_name(base_id)).starts_with('map[')
}

fn (t &Transformer) escape_aggregate_address_sources(id flat.NodeId, amp_sources map[string][]string, ptr_aliases map[string]string) []string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return []string{}
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				return t.escape_address_sources(t.a.child(&node, 0), amp_sources, ptr_aliases)
			}
			return []string{}
		}
		.ident {
			return escape_alias_sources(node.value, amp_sources, ptr_aliases)
		}
		.selector {
			if !t.method_value_has_pointer_receiver(id) {
				return []string{}
			}
			receiver := t.escape_method_value_receiver(id) or { return []string{} }
			sources := escape_alias_sources(receiver, amp_sources, ptr_aliases)
			if sources.len > 0 {
				return sources
			}
			return [receiver]
		}
		.call {
			if t.escape_call_is_allocation_helper(id, node) {
				// These allocation helpers copy from the address argument; the returned
				// pointer cannot alias the source stack local.
				return []string{}
			}
			if !isnil(t.tc) && escape_type_is_scalar_value(t.tc.resolve_type(id)) {
				// A scalar result cannot carry an address argument through the call.
				// Without this guard, returning `child(&node)` heap-promotes `node`
				// even though `child` returns only an integer node id. That pattern is
				// ubiquitous in the compiler and makes each temporary an allocation.
				return []string{}
			}
			mut sources := []string{}
			// The callee selector is consumed by the call; only argument addresses
			// can flow through a returned pointer value.
			for i in 1 .. node.children_count {
				for source_name in t.escape_aggregate_address_sources(t.a.child(&node, i), amp_sources, ptr_aliases) {
					if source_name !in sources {
						sources << source_name
					}
				}
			}
			return sources
		}
		.field_init, .paren, .cast_expr, .as_expr, .struct_init, .array_literal, .array_init, .map_init {
			mut sources := []string{}
			for i in 0 .. node.children_count {
				for source_name in t.escape_aggregate_address_sources(t.a.child(&node, i), amp_sources, ptr_aliases) {
					if source_name !in sources {
						sources << source_name
					}
				}
			}
			return sources
		}
		else {}
	}

	return []string{}
}

fn escape_type_is_scalar_value(typ types.Type) bool {
	return match typ {
		types.Alias { escape_type_is_scalar_value(typ.base_type) }
		types.Primitive, types.Char, types.Rune, types.ISize, types.USize, types.Enum { true }
		else { false }
	}
}

fn (t &Transformer) escape_call_is_allocation_helper(id flat.NodeId, node flat.Node) bool {
	if node.children_count == 0 || isnil(t.tc) {
		return false
	}
	callee_id := t.a.child(&node, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .ident {
		return false
	}
	if node.value == non_aliasing_allocation_call_marker
		&& callee.value in ['memdup', 'memdup_noscan', 'v3_aligned_memdup'] {
		return true
	}
	if callee.value in ['memdup', 'memdup_noscan'] {
		return t.tc.resolved_call_is_builtin(id, callee.value)
	}
	return false
}

fn (mut t Transformer) scan_for_in_escape_pass(node flat.Node, mut amp_ptrs map[string]bool, mut amp_sources map[string][]string, mut ptr_aliases map[string]string, mut method_value_receivers map[string]string, mut closure_capture_aliases map[string][]string, mut interface_boxes map[string]bool, mut returned map[string]bool, mut local_stack_names map[string]bool, mut local_stack_added []string, can_clear_interface_boxes bool) {
	header_count := node.value.int()
	header_end := if header_count > 0 && header_count <= int(node.children_count) {
		header_count
	} else {
		0
	}
	if header_end > 2 {
		for i in 2 .. header_end {
			t.scan_escape_pass(t.a.child(&node, i), mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, can_clear_interface_boxes)
		}
	}
	scope_mark := local_stack_added.len
	for i in 0 .. 2 {
		if i >= int(node.children_count) {
			break
		}
		part_id := t.a.child(&node, i)
		if int(part_id) >= 0 {
			part := t.a.nodes[int(part_id)]
			if part.kind == .ident && part.value.len > 0 {
				add_escape_local_stack_name(part.value, mut local_stack_names, mut local_stack_added)
			}
		}
	}
	for i in header_end .. node.children_count {
		t.scan_escape_pass(t.a.child(&node, i), mut amp_ptrs, mut amp_sources, mut ptr_aliases, mut method_value_receivers, mut closure_capture_aliases, mut interface_boxes, mut returned, mut local_stack_names, mut local_stack_added, false)
	}
	pop_escape_local_stack_names(scope_mark, mut local_stack_names, mut local_stack_added)
}

fn add_escape_local_stack_name(name string, mut local_stack_names map[string]bool, mut local_stack_added []string) {
	if name.len == 0 {
		return
	}
	if name !in local_stack_names {
		local_stack_added << name
	}
	local_stack_names[name] = true
}

fn pop_escape_local_stack_names(mark int, mut local_stack_names map[string]bool, mut local_stack_added []string) {
	for local_stack_added.len > mark {
		name := local_stack_added.pop()
		local_stack_names.delete(name)
	}
}

fn (t &Transformer) interface_box_from_stack_pointer_source(id flat.NodeId, amp_ptrs map[string]bool, ptr_aliases map[string]string, local_stack_names map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .cast_expr || node.children_count == 0
		|| t.resolve_interface_type_name(node.value).len == 0 {
		return false
	}
	arg_id := t.a.child(&node, 0)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .prefix && arg.op == .amp && arg.children_count == 1 {
		return t.escape_address_expr_is_stack_local(t.a.child(&arg, 0), local_stack_names, amp_ptrs, ptr_aliases)
	}
	return arg.kind == .ident
		&& t.escape_pointer_ident_is_stack_backed(arg.value, amp_ptrs, ptr_aliases)
}

fn (t &Transformer) pointer_alias_cast_address_is_stack_local(id flat.NodeId, local_stack_names map[string]bool, amp_ptrs map[string]bool, ptr_aliases map[string]string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .cast_expr || node.children_count == 0 || node.value.starts_with('&')
		|| !t.normalize_type_alias(node.value).starts_with('&') {
		return false
	}
	arg_id := t.a.child(&node, 0)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .prefix || arg.op != .amp || arg.children_count == 0 {
		return false
	}
	return t.escape_address_expr_is_stack_local(t.a.child(&arg, 0), local_stack_names, amp_ptrs, ptr_aliases)
}

fn (t &Transformer) escape_pointer_ident_is_stack_backed(name string, amp_ptrs map[string]bool, ptr_aliases map[string]string) bool {
	if name.len == 0 {
		return false
	}
	mut current := name
	for _ in 0 .. ptr_aliases.len + 1 {
		if current in amp_ptrs {
			return true
		}
		next := ptr_aliases[current] or { return false }
		if next == current {
			return false
		}
		current = next
	}
	return false
}

fn (t &Transformer) escape_address_expr_is_stack_local(id flat.NodeId, local_stack_names map[string]bool, amp_ptrs map[string]bool, ptr_aliases map[string]string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return t.escape_ident_is_stack_local(node.value, local_stack_names)
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			if address_expr_base_is_indirect_storage(t.address_expr_type_name(base_id)) {
				return t.escape_address_indirect_base_is_stack_backed(base_id, amp_ptrs, ptr_aliases)
			}
			return t.escape_address_expr_is_stack_local(base_id, local_stack_names, amp_ptrs, ptr_aliases)
		}
		.index {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			base_type := t.normalize_type_alias(t.address_expr_type_name(base_id))
			if !t.is_fixed_array_type(base_type) {
				return false
			}
			return t.escape_address_expr_is_stack_local(base_id, local_stack_names, amp_ptrs, ptr_aliases)
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return t.escape_address_expr_is_stack_local(t.a.child(&node, 0), local_stack_names, amp_ptrs, ptr_aliases)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) escape_address_indirect_base_is_stack_backed(id flat.NodeId, amp_ptrs map[string]bool, ptr_aliases map[string]string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value.len > 0 {
		return t.escape_pointer_ident_is_stack_backed(node.value, amp_ptrs, ptr_aliases)
	}
	if node.kind == .paren && node.children_count > 0 {
		return t.escape_address_indirect_base_is_stack_backed(t.a.child(&node, 0), amp_ptrs, ptr_aliases)
	}
	return false
}

fn (t &Transformer) address_expr_type_name(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	mut typ := t.node_type(id)
	if typ.len == 0 {
		typ = t.original_expr_type(id)
	}
	node := t.a.nodes[int(id)]
	if typ.len == 0 && node.kind == .ident {
		typ = t.var_type(node.value)
	}
	return typ.trim_space()
}

fn address_expr_base_is_indirect_storage(typ string) bool {
	clean := typ.trim_space()
	return clean.starts_with('&') || clean.starts_with('[]') || clean.starts_with('map[')
}

fn (t &Transformer) escape_ident_is_stack_local(name string, local_stack_names map[string]bool) bool {
	if name.len == 0 {
		return false
	}
	if name in local_stack_names {
		return true
	}
	if _ := t.global_ident_type(name) {
		return false
	}
	if !isnil(t.tc) {
		if _ := t.tc.file_scope.lookup(name) {
			return false
		}
		if t.cur_module.len > 0 {
			qname := '${t.cur_module}.${name}'
			if qname != name {
				if _ := t.tc.file_scope.lookup(qname) {
					return false
				}
			}
		}
	}
	return false
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

fn (t &Transformer) next_temp_counter_for_fn(fn_node flat.Node) int {
	if t.item_range_lo >= 0 && t.item_range_hi >= t.item_range_lo
		&& t.item_range_hi <= t.a.nodes.len {
		mut next := 0
		for idx in t.item_range_lo .. t.item_range_hi {
			node := t.a.nodes[idx]
			if node.kind == .ident && node.value.starts_with('__') {
				next = next_temp_counter_after_name(node.value, next)
			}
		}
		return next
	}
	mut seen := map[int]bool{}
	mut next := 0
	for i in 0 .. fn_node.children_count {
		next = t.scan_existing_temp_suffix(t.a.child(&fn_node, i), mut seen, next)
	}
	return next
}

fn (t &Transformer) scan_existing_temp_suffix(id flat.NodeId, mut seen map[int]bool, current int) int {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len || idx in seen {
		return current
	}
	mut next := current
	seen[idx] = true
	node := t.a.nodes[idx]
	if node.kind == .ident && node.value.starts_with('__') {
		next = next_temp_counter_after_name(node.value, next)
	}
	for i in 0 .. node.children_count {
		next = t.scan_existing_temp_suffix(t.a.child(&node, i), mut seen, next)
	}
	return next
}

fn next_temp_counter_after_name(name string, current int) int {
	suffix := name.all_after_last('_')
	if !suffix.is_int() {
		return current
	}
	value := suffix.int()
	return if value >= current { value + 1 } else { current }
}

fn (mut t Transformer) transform_fn_body(fn_idx int) {
	if !isnil(t.selector_type_cache) {
		t.selector_type_cache.generation++
	}
	if !isnil(t.resolved_call_return_cache) {
		t.resolved_call_return_cache.generation++
	}
	if !isnil(t.variant_match_cache) {
		t.variant_match_cache.generation++
	}
	old_tc_file := t.tc.cur_file
	old_tc_module := t.tc.cur_module
	t.tc.cur_file = t.cur_file
	t.tc.cur_module = t.cur_module
	defer {
		t.tc.cur_file = old_tc_file
		t.tc.cur_module = old_tc_module
	}
	// Synthesized expression temporaries are function-local. Preserve the
	// surrounding counter used by const/global lowering and give every function
	// the same namespace regardless of serial/parallel scheduling.
	outer_temp_counter := t.temp_counter
	fn_node := t.a.nodes[fn_idx]
	t.temp_counter = t.next_temp_counter_for_fn(fn_node)
	if fn_idx >= 0 && fn_idx < t.transformed_fns.len {
		t.transformed_fns[fn_idx] = true
	}
	t.cur_fn_name = fn_node.value
	old_source_file := t.cur_fn_source_file
	old_source_module := t.cur_fn_source_module
	t.cur_fn_source_file = t.node_file_or(fn_idx, t.cur_file)
	t.cur_fn_source_module = t.node_module_or(fn_idx, t.cur_module)
	old_receiver_name := t.cur_fn_receiver_name
	t.cur_fn_receiver_name = ''
	old_is_generic := t.cur_fn_is_generic
	old_manualfree := t.cur_fn_manualfree
	t.cur_fn_is_generic = if t.skip_generics {
		false
	} else {
		t.fn_decl_has_unresolved_generics(fn_node, t.cur_module)
	}
	// Generic specializations carry a template's `manualfree` attribute in the
	// function node because the checker's declaration-attribute index only
	// contains parsed declarations.
	t.cur_fn_manualfree = fn_node.skip_ownership_drops
		|| t.tc.declaration_has_attribute(flat.NodeId(fn_idx), 'manualfree')
	param_count := t.fn_body_param_count(fn_node)
	param_types := t.fn_body_param_types(fn_node, param_count)
	t.cur_fn_ret_type = t.fn_body_return_type(fn_node)
	t.reset_var_types()
	t.cur_fn_variadic_param = ''
	t.smartcast_stack.clear()
	t.invalidated_smartcasts.clear()
	// Collect param types
	mut param_idx := 0
	mut source_mut_params := []string{}
	mut source_pointer_value_params := []string{}
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if int(child.kind) != 75 {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if child.value.len == 0 {
			continue
		}
		if child.op == .dot {
			t.cur_fn_receiver_name = child.value
		}
		if child.typ.starts_with('...') {
			t.cur_fn_variadic_param = child.value
		}
		mut raw_source_typ := if child.typ.starts_with('...') {
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
		mut typ := if t.validating_generic_spec && raw_source_typ.contains('main.') {
			// A generic clone uses `main.` to pin a caller-owned type that collides
			// with a type in the declaration module. The checker's semantic type was
			// resolved before cloning, so preferring it here would rebase the param to
			// the declaration module and discard that lock.
			raw_source_typ
		} else if param_idx < param_types.len && param_types[param_idx] !is types.Unknown {
			t.normalize_type_alias(param_types[param_idx].name())
		} else if raw_typ.len > 0 {
			raw_typ
		} else if param_idx == 0 {
			t.fn_body_receiver_type(fn_node.value)
		} else {
			''
		}
		if child.is_mut && child.op == .amp && typ.starts_with('&') {
			typ = '&${typ}'
		}
		if child.is_mut {
			typ = mut_optional_param_value_type(typ)
			raw_source_typ = mut_optional_param_value_type(raw_source_typ)
		}
		if typ.starts_with('&') && raw_typ.len > 0 && !raw_typ.starts_with('&')
			&& t.normalize_type_alias(typ[1..]) == raw_typ {
			typ = raw_typ
		}
		if typ.len > 0 {
			t.set_var_type_with_raw(child.value, typ, raw_source_typ)
			if t.is_fixed_array_type(typ) {
				t.fixed_array_param_values[child.value] = true
			}
		}
		if child.is_mut || child.op == .amp || child.typ.starts_with('mut ') {
			t.mut_param_values[child.value] = true
			source_mut_params << child.value
			if child.op == .amp {
				source_pointer_value_params << child.value
			}
		}
		param_idx++
	}
	mut body_ids := []flat.NodeId{cap: int(fn_node.children_count)}
	if t.prefix_param_scan {
		for i in param_count .. fn_node.children_count {
			child_id := t.a.children[fn_node.children_start + i]
			if int(child_id) >= 0 {
				body_ids << child_id
			}
		}
	} else {
		for i in 0 .. fn_node.children_count {
			child_id := t.a.children[fn_node.children_start + i]
			if int(child_id) < 0 {
				continue
			}
			child := t.a.nodes[int(child_id)]
			if int(child.kind) != 75 {
				body_ids << child_id
			}
		}
	}
	t.mark_escaping_amp_ptrs(body_ids)
	for name in source_mut_params {
		t.pointer_value_lvalues[name] = true
	}
	for name in source_pointer_value_params {
		t.pointer_value_rvalues[name] = true
	}
	t.mark_local_closure_cleanup_decls(body_ids)
	if !t.literal_free_fn_body {
		for id in body_ids {
			t.collect_mut_capture_sources(id)
		}
	}
	// The escape pre-pass may query expression types before local declarations
	// have populated var_types. Do not let those provisional answers leak into
	// the declaration-ordered body transform.
	t.clear_node_type_memo()
	new_body := t.transform_stmts(body_ids)
	// Rebuild function children: params then new body
	mut new_children := []flat.NodeId{cap: int(fn_node.children_count)}
	if t.prefix_param_scan {
		for i in 0 .. param_count {
			child_id := t.a.children[fn_node.children_start + i]
			if int(child_id) >= 0 {
				new_children << child_id
			}
		}
	} else {
		for i in 0 .. fn_node.children_count {
			child_id := t.a.children[fn_node.children_start + i]
			if int(child_id) < 0 {
				continue
			}
			child := t.a.nodes[int(child_id)]
			if int(child.kind) == 75 {
				new_children << child_id
			}
		}
	}
	new_children << new_body
	if !t.inplace_fn_child_rewrites
		|| !t.rewrite_children_in_place(flat.NodeId(fn_idx), new_children) {
		start := t.a.children.len
		t.a.children << new_children
		t.set_node(fn_idx, flat.Node{
			kind: .fn_decl
			op: fn_node.op
			children_start: start
			children_count: flat.child_count(new_children.len)
			pos: fn_node.pos
			value: fn_node.value
			typ: fn_node.typ
			payload: fn_node.payload
			skip_ownership_drops: fn_node.skip_ownership_drops
		})
	}
	t.smartcast_stack.clear()
	t.invalidated_smartcasts.clear()
	t.cur_fn_is_generic = old_is_generic
	t.cur_fn_manualfree = old_manualfree
	t.cur_fn_receiver_name = old_receiver_name
	t.cur_fn_source_file = old_source_file
	t.cur_fn_source_module = old_source_module
	t.temp_counter = outer_temp_counter
}

fn mut_optional_param_value_type(typ string) string {
	mut clean := typ.trim_space()
	if clean.starts_with('mut ') {
		clean = clean[4..].trim_space()
	}
	if clean.starts_with('&?') {
		clean = clean[1..]
	}
	if !clean.starts_with('?') {
		return typ
	}
	payload := clean[1..].trim_space()
	if payload.starts_with('&') {
		return '?${payload}'
	}
	return '?&${payload}'
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
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		n++
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
@[direct_array_access]
pub fn (mut t Transformer) transform_stmts(ids []flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{cap: ids.len}
	had_base_smartcasts := t.smartcast_stack.len > 0
	base_smartcasts := if had_base_smartcasts {
		t.smartcast_stack.clone()
	} else {
		t.smartcast_stack
	}
	defer {
		if had_base_smartcasts {
			t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
		} else {
			t.smartcast_stack.clear()
		}
	}
	mut i := 0
	for i < ids.len {
		id := ids[i]
		if int(id) >= 0 && i + 1 < ids.len {
			node := t.a.nodes[int(id)]
			if int(node.kind) == 44 && node.children_count == 0 && t.cur_fn_ret_type.len > 0
				&& t.cur_fn_ret_type != 'void' {
				next_id := ids[i + 1]
				next_node := t.a.nodes[int(next_id)]
				if int(next_node.kind) == 39 && next_node.children_count > 0 {
					expr_id := t.a.child(&next_node, 0)
					start := t.a.children.len
					t.a.children << expr_id
					merged_return := t.a.add_node(flat.Node{
						kind: .return_stmt
						children_start: start
						children_count: 1
						typ: node.typ
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
		for info in t.post_assert_smartcasts(id) {
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
		kind: loop_node.kind
		op: loop_node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos: loop_node.pos
		value: loop_node.value
		typ: loop_node.typ
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
@[direct_array_access]
pub fn (mut t Transformer) transform_stmt(id flat.NodeId) []flat.NodeId {
	if int(id) < 0 {
		return [id]
	}
	node := t.a.nodes[int(id)]
	kind_id := int(node.kind)
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
		return [t.lower_one_match(node)]
	}
	if kind_id == 52 {
		return t.transform_defer_stmt(id, node)
	}
	if kind_id == 53 {
		return t.transform_assert_stmt(id, node)
	}
	if kind_id == 56 {
		return t.transform_select_stmt(id, node)
	}
	if kind_id == 22 {
		transformed := t.transform_or_expr(id, node)
		transformed_node := t.a.nodes[int(transformed)]
		if t.is_stmt_kind_id(int(transformed_node.kind)) {
			return [transformed]
		}
		return [t.make_expr_stmt(transformed)]
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
			return [t.lower_one_match(node)]
		}
		.defer_stmt {
			return t.transform_defer_stmt(id, node)
		}
		.assert_stmt {
			return t.transform_assert_stmt(id, node)
		}
		.select_stmt {
			return t.transform_select_stmt(id, node)
		}
		.or_expr {
			transformed := t.transform_or_expr(id, node)
			transformed_node := t.a.nodes[int(transformed)]
			if t.is_stmt_kind_id(int(transformed_node.kind)) {
				return [transformed]
			}
			return [t.make_expr_stmt(transformed)]
		}
		else {
			return [id]
		}
	}
}

// transform_expr transforms transform expr data for transform.
@[direct_array_access]
pub fn (mut t Transformer) transform_expr(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && node.children_count == 1
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		if target := t.comptime_type_expr_type(t.a.child(&node, 0)) {
			return if node.value == '__v3_comptime_new' {
				t.comptime_new_value(target)
			} else {
				t.zero_value_for_type(target)
			}
		}
	}
	if optional_wrapper_access_marker in node.generic_params()
		|| transformed_option_unwrap_access_marker in node.generic_params() {
		return id
	}
	kind_id := int(node.kind)
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
	if kind_id == 35 {
		return t.transform_dump_expr(node)
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
	if kind_id == 30 {
		return t.transform_spawn_expr(id, node)
	}
	if kind_id == 56 {
		return t.transform_select_expr(id, node)
	}
	if node.kind == .string_literal {
		return t.transform_nested_string_literal_expr(id, node)
	}
	if kind_id == 30 || kind_id == 27 || kind_id == 57 {
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
		.string_literal {
			return t.transform_nested_string_literal_expr(id, node)
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
		.sizeof_expr {
			contexts := t.smartcasts_for(node.value)
			if contexts.len > 0 {
				return t.make_sizeof_type(contexts.last().variant_name)
			}
			return id
		}
		.dump_expr {
			return t.transform_dump_expr(node)
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
			return t.transform_select_expr(id, node)
		}
		.lambda_expr, .range, .select_branch {
			return t.transform_children_expr(id, node)
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .nil_literal, .none_expr, .enum_val, .offsetof_expr {
			// leaf/simple nodes - pass through unchanged
			return id
		}
		else {
			return id
		}
	}
}

fn dump_relative_source_path(path string) string {
	normalized := os.real_path(path).replace('\\', '/')
	cwd := os.getwd().replace('\\', '/').trim_right('/')
	if cwd.len > 0 && normalized.starts_with(cwd + '/') {
		return normalized[cwd.len + 1..]
	}
	if relative := normalized.index('/vlib/') {
		return normalized[relative + 1..]
	}
	return path.replace('\\', '/')
}

fn dump_pointer_depth(typ string) int {
	mut depth := 0
	for depth < typ.len && typ[depth] == `&` {
		depth++
	}
	return depth
}

fn (mut t Transformer) dump_value_string(expr flat.NodeId, typ string) flat.NodeId {
	if !typ.starts_with('&') || t.is_fixed_array_type(typ) {
		return t.wrap_string_conversion(expr, typ)
	}
	elem_type := typ[1..]
	ptr_name := t.new_temp('dump_ptr')
	text_name := t.new_temp('dump_ptr_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, typ)
	t.set_var_type(ptr_name, typ)
	nil_text := '&'.repeat(dump_pointer_depth(typ)) + 'nil'
	t.pending_stmts << t.make_decl_assign_typed(text_name, t.make_string_literal(nil_text), 'string')

	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), elem_type)
	value_text := t.dump_value_string(value, elem_type)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(text_name), t.string_plus(t.make_string_literal('&'), value_text))
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(text_name)
}

fn (mut t Transformer) transform_dump_expr(node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_empty()
	}
	child_id := t.a.child(&node, 0)
	mut typ := t.node_type(child_id)
	child_node := t.a.nodes[int(child_id)]
	if closure_type := t.fresh_runtime_closure_type(child_id) {
		typ = closure_type
		t.mark_fn_used_name('closure.closure_create_with_data')
		t.mark_fresh_runtime_closure_methods_used(child_id)
	}
	if child_node.kind == .call {
		concrete_ret := t.concrete_generic_call_return_type(child_id, child_node)
		if concrete_ret.len > 0 && !t.stringify_type_has_generic_placeholder(concrete_ret) {
			typ = concrete_ret
		}
	}
	if typ.len == 0 || typ == 'unknown' {
		typ = t.resolve_expr_type(child_id)
	}
	raw_alias_type := t.raw_alias_type_for_expr(child_id)
	if raw_alias_type.len > 0 {
		typ = raw_alias_type
	}
	if child_node.kind == .ident {
		raw := t.raw_var_type(child_node.value).trim_space()
		if raw.starts_with('shared ') {
			typ = t.normalize_type_alias(raw[7..].trim_space().trim_left('&'))
		}
	} else if child_node.kind == .selector && child_node.children_count > 0 && !isnil(t.tc) {
		base_id := t.a.child(&child_node, 0)
		mut base_type := t.raw_expr_type_without_smartcast(base_id)
		if base_type.len == 0 {
			base_type = t.node_type(base_id)
		}
		if raw, owner_type := t.lookup_struct_field_raw_type_with_owner(t.trim_pointer_type(base_type), child_node.value) {
			if raw.trim_space().starts_with('shared ') {
				typ = t.normalize_field_type(raw.trim_space()[7..], owner_type)
			}
		}
	}
	if child_node.kind == .or_expr && child_node.children_count > 0 {
		_, value_type := t.or_expr_types(t.a.child(&child_node, 0), child_node.typ)
		if value_type.len > 0 && value_type != 'unknown' && !t.generic_arg_is_unresolved(value_type) {
			typ = value_type
		}
	}
	// route a value `match`/`if` dumped operand (e.g. `dump(match x { ... })`)
	// through value lowering so its propagating arms are lowered as values.
	child := if t.is_value_match_or_if_operand(child_id) {
		t.transform_value_operand(child_id)
	} else {
		t.transform_expr(child_id)
	}
	temp_name := t.new_temp('dump')
	t.pending_stmts << t.make_decl_assign_typed(temp_name, child, typ)
	if isnil(t.tc) || !t.tc.suppress_dump_output {
		value := t.make_ident(temp_name)
		value_text := t.dump_value_string(value, typ)
		mut path := t.cur_file
		mut line := 0
		if file := t.a.source_files[node.pos.id] {
			path = file.name
			line = file.position(node.pos).line
		}
		expr_text := if node.value.len > 0 { node.value } else { 'dump expression' }
		prefix :=
			t.make_string_literal('[${dump_relative_source_path(path)}:${line}] ${expr_text}: ')
		message := t.string_plus(prefix, value_text)
		t.pending_stmts << t.make_expr_stmt(t.make_call('eprintln', [message]))
	}
	return t.make_ident(temp_name)
}

fn (mut t Transformer) transform_nested_string_literal_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if !t.in_string_interp_part {
		return id
	}
	if expr := t.complex_nested_string_interpolation(node.value) {
		return expr
	}
	if inner := nested_interp_literal_inner(node.value) {
		expr, typ := t.simple_nested_interp_expr(inner) or { return id }
		return t.wrap_string_conversion(expr, typ)
	}
	if expr := t.simple_nested_string_interpolation(node.value) {
		return expr
	}
	return id
}

fn (mut t Transformer) simple_nested_string_interpolation(value string) ?flat.NodeId {
	mut parts := []flat.NodeId{}
	mut start := 0
	mut i := 0
	for i < value.len - 1 {
		if value[i] != `$` || value[i + 1] != `{` || nested_interp_start_is_escaped(value, i) {
			i++
			continue
		}
		end := nested_interp_closing_brace(value, i + 2) or { return none }
		if start < i {
			parts << t.make_string_literal(value[start..i])
		}
		inner := value[i + 2..end].trim_space()
		if inner.len == 0 || string_has_interp_start_bytes(inner) || string_has_newline_byte(inner) {
			return none
		}
		expr, typ := t.simple_nested_interp_expr(inner) or { return none }
		parts << t.wrap_string_conversion(expr, typ)
		i = end + 1
		start = i
	}
	if parts.len == 0 {
		return none
	}
	if start < value.len {
		parts << t.make_string_literal(value[start..])
	}
	mut result := parts[0]
	for part in parts[1..] {
		result = t.string_plus(result, part)
	}
	return result
}

fn (mut t Transformer) complex_nested_string_interpolation(value string) ?flat.NodeId {
	inner := nested_interp_literal_inner_loose(value) or { return none }
	return t.complex_nested_interp_expr(inner)
}

fn (mut t Transformer) complex_nested_interp_expr(inner string) ?flat.NodeId {
	if match_expr := t.complex_nested_match_interp_expr(inner) {
		return match_expr
	}
	cond_text, then_text, else_text := split_nested_if_interp(inner) or { return none }
	cond := t.simple_nested_condition_expr(cond_text) or { return none }
	result_name := t.new_temp('nested_interp')
	decl := t.make_decl_assign_typed(result_name, t.make_string_literal(''), 'string')
	outer_pending := t.pending_stmts.clone()

	t.pending_stmts.clear()
	then_expr := t.nested_string_branch_expr(then_text) or {
		t.pending_stmts = outer_pending
		return none
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	then_body << t.make_assign(t.make_ident(result_name), then_expr)

	t.pending_stmts.clear()
	else_expr := t.nested_string_branch_expr(else_text) or {
		t.pending_stmts = outer_pending
		return none
	}
	mut else_body := []flat.NodeId{}
	t.drain_pending(mut else_body)
	else_body << t.make_assign(t.make_ident(result_name), else_expr)

	t.pending_stmts = outer_pending
	t.pending_stmts << decl
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_block(else_body))
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'string')
	return result
}

fn (mut t Transformer) complex_nested_match_interp_expr(inner string) ?flat.NodeId {
	subject_text, label_text, then_text, else_text := split_nested_match_interp(inner) or {
		return none
	}
	subject, _ := t.simple_nested_interp_expr(subject_text) or { return none }
	label, _ := t.simple_nested_interp_expr(label_text) or { return none }
	cond := t.make_infix(.eq, subject, label)
	t.set_node_typ(int(cond), 'bool')
	result_name := t.new_temp('nested_interp')
	decl := t.make_decl_assign_typed(result_name, t.make_string_literal(''), 'string')
	outer_pending := t.pending_stmts.clone()

	t.pending_stmts.clear()
	then_expr := t.nested_match_branch_expr(then_text) or {
		t.pending_stmts = outer_pending
		return none
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	then_body << t.make_assign(t.make_ident(result_name), then_expr)

	t.pending_stmts.clear()
	else_expr := t.nested_match_branch_expr(else_text) or {
		t.pending_stmts = outer_pending
		return none
	}
	mut else_body := []flat.NodeId{}
	t.drain_pending(mut else_body)
	else_body << t.make_assign(t.make_ident(result_name), else_expr)

	t.pending_stmts = outer_pending
	t.pending_stmts << decl
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_block(else_body))
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'string')
	return result
}

fn (mut t Transformer) nested_match_branch_expr(text string) ?flat.NodeId {
	if quoted := nested_quoted_string_literal_value(text) {
		return t.nested_string_branch_expr(quoted)
	}
	expr, typ := t.simple_nested_interp_expr(text.trim_space()) or { return none }
	return t.wrap_string_conversion(expr, typ)
}

fn (mut t Transformer) nested_string_branch_expr(text string) ?flat.NodeId {
	mut value := text.trim_space()
	if quoted := nested_quoted_string_literal_value(text) {
		value = quoted
	}
	if expr := t.complex_nested_string_interpolation(value) {
		return expr
	}
	if inner := nested_interp_literal_inner(value) {
		expr, typ := t.simple_nested_interp_expr(inner) or { return none }
		return t.wrap_string_conversion(expr, typ)
	}
	if expr := t.simple_nested_string_interpolation(value) {
		return expr
	}
	return t.make_string_literal(value)
}

fn split_nested_if_interp(inner string) ?(string, string, string) {
	text := inner.trim_space()
	if !text.starts_with('if ') {
		return none
	}
	then_open := nested_top_level_byte(text, `{`, 2) or { return none }
	cond := text[2..then_open].trim_space()
	then_close := nested_matching_brace(text, then_open) or { return none }
	rest := text[then_close + 1..].trim_space()
	if !rest.starts_with('else') {
		return none
	}
	else_text := rest[4..].trim_space()
	if else_text.len == 0 || else_text[0] != `{` {
		return none
	}
	else_close := nested_matching_brace(else_text, 0) or { return none }
	if else_text[else_close + 1..].trim_space().len > 0 {
		return none
	}
	return cond, text[then_open + 1..then_close].trim_space(), else_text[1..else_close].trim_space()
}

fn split_nested_match_interp(inner string) ?(string, string, string, string) {
	text := inner.trim_space()
	if !text.starts_with('match ') {
		return none
	}
	match_open := nested_top_level_byte(text, `{`, 6) or { return none }
	match_close := nested_matching_brace(text, match_open) or { return none }
	if text[match_close + 1..].trim_space().len > 0 {
		return none
	}
	subject := text[6..match_open].trim_space()
	body := text[match_open + 1..match_close].trim_space()
	then_open := nested_top_level_byte(body, `{`, 0) or { return none }
	label := body[..then_open].trim_space()
	then_close := nested_matching_brace(body, then_open) or { return none }
	rest := body[then_close + 1..].trim_space()
	if !rest.starts_with('else') {
		return none
	}
	else_text := rest[4..].trim_space()
	if else_text.len == 0 || else_text[0] != `{` {
		return none
	}
	else_close := nested_matching_brace(else_text, 0) or { return none }
	if else_text[else_close + 1..].trim_space().len > 0 {
		return none
	}
	return subject, label, body[then_open + 1..then_close].trim_space(), else_text[1..else_close].trim_space()
}

fn nested_top_level_byte(text string, target u8, start int) ?int {
	mut quote := u8(0)
	mut escaped := false
	for i := start; i < text.len; i++ {
		ch := text[i]
		if quote != 0 {
			if escaped {
				escaped = false
			} else if ch == `\\` {
				escaped = true
			} else if ch == quote {
				quote = 0
			}
			continue
		}
		if ch == 39 || ch == 34 || ch == 96 {
			quote = ch
			continue
		}
		if ch == target {
			return i
		}
	}
	return none
}

fn nested_matching_brace(text string, open int) ?int {
	if open < 0 || open >= text.len || text[open] != `{` {
		return none
	}
	mut quote := u8(0)
	mut escaped := false
	mut depth := 0
	for i := open; i < text.len; i++ {
		ch := text[i]
		if quote != 0 {
			if escaped {
				escaped = false
			} else if ch == `\\` {
				escaped = true
			} else if ch == quote {
				quote = 0
			}
			continue
		}
		if ch == 39 || ch == 34 || ch == 96 {
			quote = ch
			continue
		}
		if ch == `{` {
			depth++
		} else if ch == `}` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn nested_quoted_string_literal_value(text string) ?string {
	clean := text.trim_space()
	if clean.len < 2 {
		return none
	}
	quote := clean[0]
	if quote !in [u8(39), u8(34), u8(96)] || clean[clean.len - 1] != quote {
		return none
	}
	return clean[1..clean.len - 1]
}

fn nested_interp_start_is_escaped(value string, dollar_idx int) bool {
	mut slash_count := 0
	mut i := dollar_idx - 1
	for i >= 0 && value[i] == `\\` {
		slash_count++
		i--
	}
	return slash_count % 2 == 1
}

fn nested_interp_closing_brace(value string, start int) ?int {
	for i in start .. value.len {
		if value[i] == `}` {
			return i
		}
		if i < value.len - 1 && value[i] == `$` && value[i + 1] == `{` {
			return none
		}
	}
	return none
}

fn nested_interp_literal_inner(value string) ?string {
	if value.len < 3 || value[0] != `$` || value[1] != `{` || !value.ends_with('}') {
		return none
	}
	inner := value[2..value.len - 1].trim_space()
	if inner.len == 0 || string_has_interp_start_bytes(inner) || string_has_newline_byte(inner) {
		return none
	}
	return inner
}

fn nested_interp_literal_inner_loose(value string) ?string {
	if value.len < 3 || value[0] != `$` || value[1] != `{` || !value.ends_with('}') {
		return none
	}
	inner := value[2..value.len - 1].trim_space()
	if inner.len == 0 {
		return none
	}
	return inner
}

fn string_has_interp_start_bytes(value string) bool {
	if value.len < 2 {
		return false
	}
	for i in 0 .. value.len - 1 {
		if value[i] == `$` && value[i + 1] == `{` {
			return true
		}
	}
	return false
}

fn string_has_newline_byte(value string) bool {
	for ch in value {
		if ch == 10 {
			return true
		}
	}
	return false
}

fn (mut t Transformer) simple_nested_interp_expr(inner string) ?(flat.NodeId, string) {
	if expr, typ := t.simple_nested_binary_expr(inner) {
		return expr, typ
	}
	if expr, typ := t.simple_nested_atom_expr(inner) {
		return expr, typ
	}
	return none
}

fn (mut t Transformer) simple_nested_condition_expr(inner string) ?flat.NodeId {
	expr, typ := t.simple_nested_interp_expr(inner) or { return none }
	if typ == 'bool' {
		return expr
	}
	return none
}

fn (mut t Transformer) simple_nested_binary_expr(inner string) ?(flat.NodeId, string) {
	clean := inner.trim_space()
	for op_text in ['==', '!=', '>=', '<=', '>', '<', '+', '-'] {
		op_idx := simple_nested_operator_index(clean, op_text) or { continue }
		lhs_text := clean[..op_idx].trim_space()
		rhs_text := clean[op_idx + op_text.len..].trim_space()
		if lhs_text.len == 0 || rhs_text.len == 0 {
			continue
		}
		lhs, lhs_typ := t.simple_nested_atom_expr(lhs_text) or { return none }
		rhs, rhs_typ := t.simple_nested_atom_expr(rhs_text) or { return none }
		op := match op_text {
			'==' { flat.Op.eq }
			'!=' { flat.Op.ne }
			'>=' { flat.Op.ge }
			'<=' { flat.Op.le }
			'>' { flat.Op.gt }
			'<' { flat.Op.lt }
			'-' { flat.Op.minus }
			else { flat.Op.plus }
		}

		typ := if op in [.eq, .ne, .ge, .le, .gt, .lt] {
			'bool'
		} else if lhs_typ == rhs_typ && lhs_typ.len > 0 {
			lhs_typ
		} else {
			'int'
		}
		expr := t.make_infix(op, lhs, rhs)
		t.set_node_typ(int(expr), typ)
		return expr, typ
	}
	return none
}

fn simple_nested_operator_index(text string, op string) ?int {
	for i := 1; i + op.len < text.len; i++ {
		if text[i..i + op.len] == op {
			return i
		}
	}
	return none
}

fn (mut t Transformer) simple_nested_atom_expr(inner string) ?(flat.NodeId, string) {
	if inner.starts_with('&') {
		name := inner[1..].trim_space()
		if !is_simple_ident_name(name) {
			return none
		}
		value := t.make_ident(name)
		mut value_type := t.raw_var_type(name)
		if value_type.len == 0 {
			value_type = t.var_type(name)
		}
		if value_type.len == 0 {
			value_type = t.node_type(value)
		}
		if value_type.len == 0 {
			return none
		}
		return t.make_prefix(.amp, value), '&' + value_type
	}
	if inner == 'true' || inner == 'false' {
		return t.make_bool_literal(inner == 'true'), 'bool'
	}
	if is_decimal_text(inner) {
		return t.make_int_literal_typed(inner, 'int'), 'int'
	}
	if is_simple_ident_name(inner) {
		expr := t.make_ident(inner)
		mut typ := t.raw_var_type(inner)
		if typ.len == 0 {
			typ = t.var_type(inner)
		}
		if typ.len == 0 {
			typ = t.node_type(expr)
		}
		if typ.len == 0 {
			return none
		}
		return expr, typ
	}
	return none
}

fn is_simple_ident_name(name string) bool {
	if name.len == 0 {
		return false
	}
	first := name[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for ch in name {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}
	return true
}

fn (mut t Transformer) transform_spawn_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	old_in_spawn_expr := t.in_spawn_expr
	t.in_spawn_expr = true
	spawn_id := t.rewrite_spawn_fn_value_alias(id, node)
	spawn_node := t.a.nodes[int(spawn_id)]
	result := t.transform_children_expr(spawn_id, spawn_node)
	result_node := t.a.node(result)
	if result_node.kind == .spawn_expr && result_node.children_count > 0 {
		call_id := t.a.child(result_node, 0)
		mut call_type := t.node_type(call_id)
		if call_type.len == 0 || call_type == 'unknown' {
			call_node := t.a.node(call_id)
			if call_node.kind == .call && call_node.children_count > 0 {
				callee := t.a.child_node(call_node, 0)
				if callee.kind == .selector && callee.children_count > 0 && !isnil(t.tc) {
					base := t.a.child_node(callee, 0)
					base_type := if base.kind == .ident {
						t.var_type(base.value)
					} else {
						t.node_type(t.a.child(callee, 0))
					}
					if info := t.tc.resolve_generic_struct_method(base_type, callee.value) {
						call_type = info.return_type.name()
					}
				}
			}
		}
		if call_type.len > 0 && call_type !in ['void', 'unknown'] {
			t.set_node_typ(int(result), 'thread ${call_type}')
		}
	}
	t.in_spawn_expr = old_in_spawn_expr
	return result
}

fn (mut t Transformer) rewrite_spawn_fn_value_alias(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	call_id := t.a.child(&node, 0)
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count == 0 {
		return id
	}
	callee_id := t.a.child(&call, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .ident {
		return id
	}
	lifted := t.fn_value_locals[callee.value] or { return id }
	if !lifted.contains('__anon_fn_') {
		return id
	}
	mut call_children := []flat.NodeId{cap: int(call.children_count)}
	call_children << t.make_ident(lifted)
	for i in 1 .. call.children_count {
		call_children << t.a.child(&call, i)
	}
	call_start := t.a.children.len
	for child in call_children {
		t.a.children << child
	}
	new_call := t.a.add_node(flat.Node{
		kind: .call
		op: call.op
		children_start: call_start
		children_count: call.children_count
		pos: call.pos
		value: call.value
		typ: call.typ
		is_mut: call.is_mut
	})
	spawn_start := t.a.children.len
	t.a.children << new_call
	return t.a.add_node(flat.Node{
		kind: .spawn_expr
		op: node.op
		children_start: spawn_start
		children_count: 1
		pos: node.pos
		value: node.value
		typ: node.typ
		is_mut: node.is_mut
	})
}

fn (t &Transformer) pointer_optional_unwrap_lvalue_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .mul || node.children_count == 0 {
		return none
	}
	or_id := t.a.child(&node, 0)
	or_node := t.a.nodes[int(or_id)]
	if or_node.kind != .or_expr || or_node.value !in ['?', '!'] || or_node.children_count < 2 {
		return none
	}
	source_id := t.a.child(&or_node, 0)
	mut source_type := t.raw_expr_type_without_smartcast(source_id)
	if source_type.len == 0 {
		source_type = t.original_expr_type(source_id)
	}
	if !source_type.starts_with('&') || !t.is_optional_type_name(source_type[1..]) {
		return none
	}
	value_type := t.optional_base_type(t.qualify_optional_type(source_type[1..]))
	if value_type.len == 0 || value_type == 'void' {
		return none
	}
	return value_type
}

fn (mut t Transformer) transform_pointer_optional_unwrap_lvalue(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .or_expr || node.value !in ['?', '!'] || node.children_count < 2 {
		return none
	}
	source_id := t.a.child(&node, 0)
	mut source_type := t.raw_expr_type_without_smartcast(source_id)
	if source_type.len == 0 {
		source_type = t.original_expr_type(source_id)
	}
	if !source_type.starts_with('&') || !t.is_optional_type_name(source_type[1..]) {
		return none
	}
	optional_type := t.qualify_optional_type(source_type[1..])
	value_type := t.optional_base_type(optional_type)
	if value_type.len == 0 || value_type == 'void' {
		return none
	}
	source := t.stable_transformed_expr_for_reuse(t.transform_expr(source_id), source_type, 'opt_lvalue')
	wrapper := t.make_prefix(.mul, source)
	t.set_node_typ(int(wrapper), optional_type)
	err_expr := t.make_selector(wrapper, 'err', 'IError')
	not_ok := t.make_prefix(.not, t.make_selector(wrapper, 'ok', 'bool'))
	body_id := t.a.child(&node, 1)
	else_block := t.make_or_else_block(node.value, t.lower_or_body_to_stmts_with_err_expr(body_id, '', '', node.value, err_expr))
	t.pending_stmts << t.make_if(not_ok, else_block, t.make_empty())
	return t.make_selector(wrapper, 'value', value_type)
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
			if t.inplace_lvalue_rewrites && t.rewrite_children_in_place(id, new_children) {
				return id
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind: .selector
				op: node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos: node.pos
				value: node.value
				typ: node.typ
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
			new_children << t.transform_index_base_expr(t.a.child(&node, 0))
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			if t.inplace_lvalue_rewrites && t.rewrite_children_in_place(id, new_children) {
				return id
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind: .index
				op: node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos: node.pos
				value: node.value
				typ: node.typ
			})
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				child_id := t.a.child(&node, 0)
				if value := t.transform_pointer_optional_unwrap_lvalue(child_id) {
					return value
				}
				child_node := t.a.nodes[int(child_id)]
				// The source prefix already performs the value read for a pointer-backed
				// local (for example a `for mut item` binding). Do not let the ordinary
				// rvalue ident path insert a second dereference beneath it.
				mut child := if child_node.kind == .ident
					&& t.pointer_value_rvalues[child_node.value] {
					child_id
				} else {
					t.transform_expr(child_id)
				}
				if child_node.kind == .ident && t.mut_param_values[child_node.value]
					&& t.var_type(child_node.value).starts_with('&&') {
					child = t.make_prefix(.mul, child)
					t.set_node_typ(int(child), t.var_type(child_node.value)[1..])
				}
				if t.inplace_lvalue_rewrites && t.rewrite_one_child_in_place(id, child) {
					return id
				}
				start := t.a.children.len
				t.a.children << child
				return t.a.add_node(flat.Node{
					kind: .prefix
					op: node.op
					children_start: start
					children_count: 1
					pos: node.pos
					value: node.value
					typ: node.typ
				})
			}
			return t.transform_expr(id)
		}
		.paren {
			if node.children_count == 0 {
				return id
			}
			child := t.transform_lvalue(t.a.child(&node, 0))
			if t.inplace_lvalue_rewrites && t.rewrite_one_child_in_place(id, child) {
				return id
			}
			start := t.a.children.len
			t.a.children << child
			return t.a.add_node(flat.Node{
				kind: .paren
				op: node.op
				children_start: start
				children_count: 1
				pos: node.pos
				value: node.value
				typ: node.typ
			})
		}
		.call {
			if node.children_count > 0 {
				callee := t.a.child_node(&node, 0)
				if callee.kind == .selector && callee.value in ['first', 'last']
					&& callee.children_count > 0 {
					base_id := t.a.child(callee, 0)
					base_type := t.normalize_type_alias(t.lvalue_type(base_id))
					clean_base_type := base_type.trim_left('&')
					if clean_base_type.starts_with('[]') {
						mut base := t.transform_lvalue(base_id)
						if base_type.starts_with('&') {
							base = t.make_prefix(.mul, base)
							t.set_node_typ(int(base), clean_base_type)
						}
						if callee.value == 'last' {
							base = t.stabilize_transformed_lvalue_for_reuse(base)
						}
						index := if callee.value == 'first' {
							t.make_int_literal(0)
						} else {
							t.make_infix(.minus, t.make_selector(base, 'len', 'int'), t.make_int_literal(1))
						}
						return t.make_index(base, index, clean_base_type[2..])
					}
				}
			}
			return t.transform_expr(id)
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
		return [id]
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
		payload_type := t.optional_base_type(t.qualify_optional_type(t.cur_fn_ret_type))
		if t.is_optional_type_name(t.cur_fn_ret_type)
			&& t.return_expr_is_propagated_err(child_id, payload_type) {
			err_expr := t.transform_expr(child_id)
			ret := t.make_none_return_stmt_with_err_expr(err_expr)
			t.mark_transformed_return(ret, source_return_id)
			return t.with_pending_before(ret)
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_return_child(child_id, i, int(node.children_count))
	}
	if t.rewrite_children_in_place(id, new_children) {
		return t.with_pending_before(id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind: .return_stmt
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: node.typ
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

@[direct_array_access]
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
	target_type := t.return_child_target_type(child_index, total_children)
	mut return_child_id := child_id
	if rewritten := t.rewrite_escaping_interface_box_return_expr(child_id) {
		return_child_id = rewritten
	}
	if target_type.len > 0 && t.is_optional_type_name(target_type) {
		child := t.a.nodes[int(return_child_id)]
		if child.kind == .none_expr {
			return t.transform_expr(return_child_id)
		}
		if t.is_error_call(child) {
			return t.transform_expr(return_child_id)
		}
		if child.kind in [.lambda_expr, .fn_literal] {
			return t.transform_expr_for_type(return_child_id, target_type)
		}
		payload_type := t.optional_base_type(t.qualify_optional_type(target_type))
		resolved_payload_type := t.resolve_sum_name(payload_type)
		if child.kind == .or_expr {
			if resolved_payload_type in t.sum_types {
				return t.wrap_sum_value(t.transform_expr(return_child_id), resolved_payload_type)
			}
			return t.transform_expr_for_type(return_child_id, target_type)
		}
		if child.kind in [.if_expr, .match_stmt]
			&& t.return_expr_is_optional_result(return_child_id) {
			return t.transform_expr_for_type(return_child_id, target_type)
		}
		if resolved_payload_type in t.sum_types {
			return t.clone_borrowed_projection(return_child_id, t.wrap_sum_value(return_child_id, resolved_payload_type), resolved_payload_type)
		}
		return t.clone_borrowed_projection(return_child_id, t.transform_expr_for_type(return_child_id, payload_type), payload_type)
	}
	resolved_target_type := t.resolve_sum_name(target_type)
	if target_type.len > 0 && resolved_target_type in t.sum_types {
		return t.clone_borrowed_projection(return_child_id, t.transform_sum_value_for_type(return_child_id, resolved_target_type), resolved_target_type)
	}
	if target_type.len > 0 && !t.is_optional_type_name(target_type) {
		return t.clone_borrowed_projection(return_child_id, t.transform_expr_for_type(return_child_id, target_type), target_type)
	}
	return t.wrap_sum_return_expr(return_child_id)
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
	mut addr_id := child_id
	mut ret_ptr_type := t.cur_fn_ret_type
	node := t.a.nodes[int(child_id)]
	if node.kind == .cast_expr && node.value.starts_with('&') && node.children_count == 1 {
		addr_id = t.a.child(&node, 0)
		ret_ptr_type = node.value
	}
	addr_node := t.a.nodes[int(addr_id)]
	if addr_node.kind != .prefix || addr_node.op != .amp || addr_node.children_count != 1 {
		return none
	}
	inner_id := t.a.child(&addr_node, 0)
	inner := t.a.nodes[int(inner_id)]
	local_type := t.local_address_return_type(inner_id, inner) or { return none }
	if local_type.len == 0 {
		return none
	}
	ret_base_type := ret_ptr_type[1..]
	if ret_base_type.len == 0 {
		return none
	}
	clean_local_type := t.normalize_type_alias(local_type)
	clean_ret_type := t.normalize_type_alias(ret_base_type)
	if clean_local_type != clean_ret_type && local_type != ret_base_type {
		return none
	}
	addr := t.make_prefix(.amp, t.transform_expr(inner_id))
	dup := t.make_memdup_call_for_type(addr, ret_base_type)
	return t.make_cast(ret_ptr_type, dup, ret_ptr_type)
}

fn (mut t Transformer) local_address_return_type(inner_id flat.NodeId, inner flat.Node) ?string {
	if inner.kind == .ident {
		if inner.value.len == 0 {
			return none
		}
		return t.var_type(inner.value)
	}
	if inner.kind !in [.selector, .index] || !t.expr_can_take_address(inner_id)
		|| !t.selector_root_is_local(inner_id) {
		return none
	}
	mut local_type := t.node_type(inner_id)
	if local_type.len == 0 {
		local_type = t.resolve_expr_type(inner_id)
	}
	return local_type
}

fn (t &Transformer) selector_root_is_local(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	mut cur := t.a.nodes[int(id)]
	for cur.kind in [.selector, .index] && cur.children_count > 0 {
		next_id := t.a.child(&cur, 0)
		if int(next_id) < 0 || int(next_id) >= t.a.nodes.len {
			return false
		}
		next := t.a.nodes[int(next_id)]
		mut next_type := t.address_expr_type_name(next_id)
		if !address_expr_base_is_indirect_storage(next_type) && next.kind == .selector {
			next_type = t.resolve_selector_type(next)
		}
		if address_expr_base_is_indirect_storage(next_type) {
			return false
		}
		cur = next
	}
	if cur.kind != .ident || cur.value.len == 0 {
		return false
	}
	if t.mut_param_values[cur.value] {
		return false
	}
	root_type := t.var_type(cur.value)
	return root_type.len > 0 && !address_expr_base_is_indirect_storage(root_type)
}

fn (mut t Transformer) rewrite_escaping_interface_box_return_expr(child_id flat.NodeId) ?flat.NodeId {
	if isnil(t.tc) || t.escaping_interface_box_locals.len == 0 {
		return none
	}
	mut replacements := map[string]string{}
	return t.rewrite_escaping_interface_box_return_expr_rec(child_id, mut replacements)
}

fn (mut t Transformer) rewrite_escaping_interface_box_return_expr_rec(id flat.NodeId, mut replacements map[string]string) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value in t.escaping_interface_box_locals {
		tmp_name := if existing := replacements[node.value] {
			existing
		} else {
			new_tmp := t.heap_copy_escaping_interface_box_local(node.value) or { return none }
			replacements[node.value] = new_tmp
			new_tmp
		}
		return t.make_ident(tmp_name)
	}
	if node.kind == .if_expr {
		return t.rewrite_escaping_interface_box_if_return_expr(node, mut replacements)
	}
	if node.kind == .match_stmt {
		return t.rewrite_escaping_interface_box_match_return_expr(node, mut replacements)
	}
	if node.kind == .match_branch {
		return t.rewrite_escaping_interface_box_match_branch_return_expr(node, mut replacements)
	}
	if node.kind == .block {
		return t.rewrite_escaping_interface_box_block_return_expr(node, mut replacements)
	}
	if !return_expr_node_can_hold_escaping_interface_box(node) {
		return none
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	mut changed := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if rewritten := t.rewrite_escaping_interface_box_return_expr_rec(child_id, mut replacements) {
			children << rewritten
			changed = true
		} else {
			children << child_id
		}
	}
	if !changed {
		return none
	}
	return t.copy_node_with_children(node, children)
}

fn (mut t Transformer) rewrite_escaping_interface_box_if_return_expr(node flat.Node, mut replacements map[string]string) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << t.a.child(&node, 0)
	mut changed := false
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		if rewritten := t.rewrite_escaping_interface_box_return_expr_rec(child_id, mut replacements) {
			children << rewritten
			changed = true
		} else {
			children << child_id
		}
	}
	if !changed {
		return none
	}
	return t.copy_node_with_children(node, children)
}

fn (mut t Transformer) rewrite_escaping_interface_box_match_return_expr(node flat.Node, mut replacements map[string]string) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << t.a.child(&node, 0)
	mut changed := false
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		if rewritten := t.rewrite_escaping_interface_box_return_expr_rec(child_id, mut replacements) {
			children << rewritten
			changed = true
		} else {
			children << child_id
		}
	}
	if !changed {
		return none
	}
	return t.copy_node_with_children(node, children)
}

fn (mut t Transformer) rewrite_escaping_interface_box_match_branch_return_expr(node flat.Node, mut replacements map[string]string) ?flat.NodeId {
	body_start := if node.value == 'else' { 0 } else { t.count_conds(node) }
	if node.children_count <= body_start {
		return none
	}
	return t.rewrite_escaping_interface_box_tail_child(node, body_start, mut replacements)
}

fn (mut t Transformer) rewrite_escaping_interface_box_block_return_expr(node flat.Node, mut replacements map[string]string) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	return t.rewrite_escaping_interface_box_tail_child(node, 0, mut replacements)
}

fn (mut t Transformer) rewrite_escaping_interface_box_tail_child(node flat.Node, min_tail_index int, mut replacements map[string]string) ?flat.NodeId {
	tail_idx := int(node.children_count) - 1
	if tail_idx < min_tail_index {
		return none
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	mut changed := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == tail_idx {
			if rewritten := t.rewrite_escaping_interface_box_return_expr_rec(child_id, mut replacements) {
				children << rewritten
				changed = true
			} else {
				children << child_id
			}
		} else {
			children << child_id
		}
	}
	if !changed {
		return none
	}
	return t.copy_node_with_children(node, children)
}

fn (mut t Transformer) copy_node_with_children(node flat.Node, children []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		pos: node.pos
		value: node.value
		typ: node.typ
		payload: flat.node_payload(node.generic_params().clone())
		is_mut: node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
	})
}

fn return_expr_node_can_hold_escaping_interface_box(node flat.Node) bool {
	return node.kind in [.array_literal, .array_init, .struct_init, .field_init, .map_init, .paren,
		.expr_stmt]
}

fn (mut t Transformer) heap_copy_escaping_interface_box_local(name string) ?string {
	if name.len == 0 || isnil(t.tc) {
		return none
	}
	local_type := t.var_type(name)
	iface_name := t.resolve_interface_type_name(local_type)
	if iface_name.len == 0 {
		return none
	}
	impls := if t.is_builtin_ierror_interface_name(iface_name) {
		t.tc.ierror_impl_names()
	} else {
		t.interface_impl_index_for_transform(iface_name).names
	}
	if impls.len == 0 {
		return none
	}
	source := t.transform_expr(t.make_ident(name))
	tmp_name := t.new_temp('iface_ret')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, source, iface_name)
	for impl in impls {
		impl_id := t.interface_impl_type_id(iface_name, impl) or { continue }
		tmp := t.make_ident(tmp_name)
		typ := t.make_selector(tmp, '_typ', 'int')
		cond := t.make_infix(.eq, typ, t.make_int_literal(impl_id))
		object_lhs := t.make_selector(t.make_ident(tmp_name), '_object', 'voidptr')
		object_rhs := t.make_selector(t.make_ident(tmp_name), '_object', 'voidptr')
		dup := t.make_memdup_call_for_type(object_rhs, impl)
		t.pending_stmts << t.make_if(cond, t.make_block([
			t.make_assign(object_lhs, dup),
		]), t.make_empty())
	}
	return tmp_name
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
	return t.fixed_array_value_to_owned_array(value_id, child_type, array_type)
}

fn (mut t Transformer) const_array_literal_storage_type_name_for_expr(id flat.NodeId) ?string {
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

fn (mut t Transformer) const_array_literal_requires_fixed_storage(key string) bool {
	if cached := t.const_array_fixed_storage_cache[key] {
		return cached > 0
	}
	result := t.const_array_literal_requires_fixed_storage_uncached(key)
	t.const_array_fixed_storage_cache[key] = if result { i8(1) } else { i8(-1) }
	return result
}

// precompute_const_array_fixed_storage classifies every array-literal constant
// in one AST pass. The parallel transform used to run the complete scan below
// once per constant and per worker batch, putting a multi-millisecond lazy
// initialization cost on otherwise tiny function bodies.
fn (mut t Transformer) precompute_const_array_fixed_storage() {
	if isnil(t.tc) || t.const_array_fixed_storage_ready {
		return
	}
	t.const_array_fixed_storage_ready = true
	mut candidate_ids := map[string]int{}
	mut candidate_names := map[string]bool{}
	mut candidate_keys := []string{}
	for key, expr_id in t.tc.const_exprs {
		if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
			continue
		}
		expr := t.a.nodes[int(expr_id)]
		if expr.kind != .array_literal || expr.children_count == 0 {
			continue
		}
		candidate_ids[key] = candidate_keys.len

		candidate_names[if key.contains('.') {
			key.all_after_last('.')
		} else {
			key
		}] = true
		candidate_keys << key
	}
	if candidate_keys.len == 0 {
		return
	}
	mut ref_candidates := []int{len: t.a.nodes.len}
	mut ref_states := []u8{len: t.a.nodes.len}
	mut unmatched := []int{len: candidate_keys.len}
	mut fixed_candidates := []bool{len: candidate_keys.len}
	mut invalid_candidates := []bool{len: candidate_keys.len}
	mut cur_module := 'main'
	mut cur_file := ''
	for idx, node in t.a.nodes {
		kind_id := int(node.kind)
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
				if candidate := t.const_array_candidate_for_expr(base_id, cur_module, cur_file, candidate_ids, candidate_names) {
					invalid_candidates[candidate] = true
				}
			}
		}
		if node.kind in [.ident, .selector, .as_expr, .paren] {
			if candidate := t.const_array_candidate_for_expr(flat.NodeId(idx), cur_module, cur_file, candidate_ids, candidate_names) {
				ref_candidates[idx] = candidate + 1
				if ref_states[idx] != 1 {
					ref_states[idx] = 2
					unmatched[candidate]++
				}
			}
		}
		if node.kind == .selector && node.value == 'len' && node.children_count > 0 {
			t.mark_const_array_ref_safe(mut ref_candidates, mut ref_states, mut unmatched, t.a.child(&node, 0))
		}
		if node.kind == .index && node.children_count > 0 {
			base_id := t.a.child(&node, 0)
			if candidate := t.const_array_candidate_for_expr(base_id, cur_module, cur_file, candidate_ids, candidate_names) {
				fixed_candidates[candidate] = true
			}
			t.mark_const_array_ref_safe(mut ref_candidates, mut ref_states, mut unmatched, base_id)
		}
		if node.kind == .for_in_stmt && node.value.int() == 3 && node.children_count > 2 {
			container_id := t.a.child(&node, 2)
			if int(container_id) >= 0 && t.a.nodes[int(container_id)].kind != .range {
				if candidate := t.const_array_candidate_for_expr(container_id, cur_module, cur_file, candidate_ids, candidate_names) {
					fixed_candidates[candidate] = true
				}
				t.mark_const_array_ref_safe(mut ref_candidates, mut ref_states, mut unmatched, container_id)
			}
		}
	}
	for idx, key in candidate_keys {
		result := !invalid_candidates[idx] && fixed_candidates[idx] && unmatched[idx] == 0
		t.const_array_fixed_storage_cache[key] = if result { i8(1) } else { i8(-1) }
	}
}

fn (t &Transformer) const_array_candidate_for_expr(id flat.NodeId, module_name string, file string, candidates map[string]int, candidate_names map[string]bool) ?int {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.as_expr, .paren] && node.children_count > 0 {
		return t.const_array_candidate_for_expr(t.a.child(&node, 0), module_name, file, candidates, candidate_names)
	}
	if node.kind !in [.ident, .selector] || !candidate_names[node.value] {
		return none
	}
	name := t.expr_key(id)
	key := t.const_type_key_in_context(name, module_name, file) or { return none }
	return candidates[key] or { return none }
}

fn (t &Transformer) mark_const_array_ref_safe(mut candidates []int, mut states []u8, mut unmatched []int, id flat.NodeId) {
	idx := int(id)
	if idx < 0 || idx >= t.a.nodes.len {
		return
	}
	candidate := candidates[idx] - 1
	if candidate >= 0 {
		if states[idx] == 2 {
			unmatched[candidate]--
		}
		states[idx] = 1
	}
	node := t.a.nodes[idx]
	if node.kind == .paren && node.children_count > 0 {
		t.mark_const_array_ref_safe(mut candidates, mut states, mut unmatched, t.a.child(&node, 0))
	}
}

fn (t &Transformer) const_array_literal_requires_fixed_storage_uncached(key string) bool {
	// 0 = unseen, 1 = fixed-storage-safe context, 2 = unmatched reference.
	// Parents normally follow their children, but rewritten ASTs can contain
	// forward edges; retaining both states makes this equivalent to the old
	// mark-then-classify scans while visiting the complete AST only once.
	mut ref_states := []u8{len: t.a.nodes.len}
	mut unmatched_count := 0
	mut cur_module := 'main'
	mut cur_file := ''
	mut fixed_candidate := false
	key_dot := key.last_index_u8(`.`)
	key_short := if key_dot >= 0 {
		unsafe { key.substr_unsafe(key_dot + 1, key.len) }
	} else {
		key
	}
	for idx, node in t.a.nodes {
		kind_id := int(node.kind)
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
				if t.const_ref_matches_key_in_context(base_id, cur_module, cur_file, key, key_short) {
					return false
				}
			}
		}
		if node.kind in [.ident, .selector, .as_expr, .paren] {
			if t.const_ref_matches_key_in_context(flat.NodeId(idx), cur_module, cur_file, key, key_short) {
				if ref_states[idx] != 1 {
					ref_states[idx] = 2
					unmatched_count++
				}
			}
		}
		if node.kind == .selector && node.value == 'len' && node.children_count > 0 {
			unmatched_count -= t.mark_const_ref_descendants_safe(mut ref_states, t.a.child(&node, 0))
		}
		if node.kind == .index && node.children_count > 0 {
			base_id := t.a.child(&node, 0)
			unmatched_count -= t.mark_const_ref_descendants_safe(mut ref_states, base_id)
			if t.const_ref_matches_key_in_context(base_id, cur_module, cur_file, key, key_short) {
				fixed_candidate = true
			}
		}
		if node.kind == .for_in_stmt && node.value.int() == 3 && node.children_count > 2 {
			container_id := t.a.child(&node, 2)
			if int(container_id) >= 0 && t.a.nodes[int(container_id)].kind != .range {
				unmatched_count -= t.mark_const_ref_descendants_safe(mut ref_states, container_id)
				if t.const_ref_matches_key_in_context(container_id, cur_module, cur_file, key, key_short) {
					fixed_candidate = true
				}
			}
		}
	}
	return fixed_candidate && unmatched_count == 0
}

fn (t &Transformer) mark_const_ref_descendants_safe(mut states []u8, id flat.NodeId) int {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return 0
	}
	mut cleared := 0
	if int(id) < states.len {
		if states[int(id)] == 2 {
			cleared++
		}
		states[int(id)] = 1
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		cleared += t.mark_const_ref_descendants_safe(mut states, t.a.child(&node, 0))
	}
	return cleared
}

fn (t &Transformer) const_ref_matches_key_in_context(id flat.NodeId, module_name string, file string, key string, key_short string) bool {
	if !t.const_ref_may_match_key(id, key, key_short, file) {
		return false
	}
	name := t.expr_key(id)
	if name.len == 0 || key.len == 0 || isnil(t.tc) {
		return false
	}
	if !name.contains('.') {
		if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
			&& qualified_const_key_matches(key, module_name, name) {
			return true
		}
		if name == key {
			return true
		}
		return (t.const_suffixes[name] or { '' }) == key
	}
	if name == key {
		return true
	}
	base := name.all_before_last('.')
	field := short_name_view(name)
	resolved_base := t.tc.file_imports[file_import_key(file, base)] or { base }
	if qualified_const_key_matches(key, resolved_base, field) {
		return true
	}
	qname := '${resolved_base}.${field}'
	if (t.const_suffixes[qname] or { '' }) == key {
		return true
	}
	if (t.const_suffixes[name] or { '' }) == key {
		return true
	}
	return false
}

fn (t &Transformer) const_ref_may_match_key(id flat.NodeId, key string, key_short string, file string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || key.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.as_expr, .paren] && node.children_count > 0 {
		return t.const_ref_may_match_key(t.a.child(&node, 0), key, key_short, file)
	}
	if node.kind !in [.ident, .selector] || node.value.len == 0 {
		return false
	}
	if key == node.value {
		return true
	}
	has_dot := node.value.index_u8(`.`) >= 0
	if !has_dot {
		return node.value == key_short
	}
	if key.len > node.value.len && key[key.len - 1] == node.value[node.value.len - 1]
		&& key[key.len - node.value.len - 1] == `.` && key.ends_with(node.value) {
		return true
	}
	// const_suffixes contains only dot-delimited suffixes of `key`, all covered by
	// the comparison above. Avoid hashing nearly every identifier in the AST here.
	if node.kind == .ident && !isnil(t.tc) {
		base := node.value.all_before_last('.')
		if resolved_base := t.tc.file_imports[file_import_key(file, base)] {
			return qualified_const_key_matches(key, resolved_base, short_name_view(node.value))
		}
	}
	return false
}

fn qualified_const_key_matches(key string, qualifier string, name string) bool {
	return qualifier.len > 0 && name.len > 0 && key.len == qualifier.len + name.len + 1
		&& key.starts_with(qualifier) && key[qualifier.len] == `.` && key.ends_with(name)
}

// transform_assign_stmt transforms transform assign stmt data for transform.
fn (mut t Transformer) transform_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return [id]
	}
	if expanded := t.try_expand_multi_return_assign(node) {
		return expanded
	}
	if expanded := t.try_expand_plain_multi_assign(node) {
		return expanded
	}
	if discarded := t.try_lower_discarded_closure_assign(node) {
		return discarded
	}
	t.update_orm_initialized_fields_for_assignment(node)
	t.update_sql_query_data_aliases_for_assignment(node)
	if lowered := t.try_lower_sum_shared_field_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_interface_field_assign(node) {
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
	if lowered := t.try_lower_map_index_fixed_array_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_map_index_selector_assign(node) {
		return lowered
	}
	if lowered := t.try_lower_map_index_assign(id, node) {
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
			if lhs.kind == .ident && lhs.value == '_' {
				new_children << t.transform_expr(child_id)
				continue
			}
			mut lhs_type := if lhs.kind in [.selector, .index] {
				t.lvalue_type(lhs_id)
			} else {
				t.original_expr_type(lhs_id)
			}
			if lhs_type.len == 0 {
				lhs_type = t.lvalue_type(lhs_id)
			}
			if value_type := t.pointer_optional_unwrap_lvalue_type(lhs_id) {
				lhs_type = value_type
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
				&& !t.pointer_value_rvalues[lhs.value] && !lhs_type.starts_with('&&') {
				lhs_type = lhs_type[1..]
			}
			sum_target := t.assignment_sum_target(lhs_id, child_id, lhs_type)
			if node.op == .assign && sum_target.len > 0 {
				new_children << t.clone_borrowed_assignment_value(child_id, t.transform_sum_value_for_type(child_id, sum_target), sum_target)
			} else if node.op in [.plus_assign, .minus_assign] && lhs_type.starts_with('&') {
				new_children << t.transform_expr(child_id)
			} else {
				value := t.transform_expr_for_type(child_id, lhs_type)
				new_children << if node.op == .assign {
					t.clone_borrowed_assignment_value(child_id, value, lhs_type)
				} else {
					value
				}
			}
		}
	}
	// Capture the lvalue's declared storage type before the optional-assign
	// smartcast unwraps it (`?string` -> `string`), so the drop-before-assign
	// temp below keeps the optional storage type the assigned value is wrapped
	// to, instead of a `string` temp initialised with an `Optional_string`.
	mut pre_smartcast_lhs_type := ''
	if node.kind == .assign && node.op == .assign && node.children_count == 2 {
		pre_smartcast_lhs_type = t.lvalue_type(t.a.child(&node, 0))
		t.update_option_assignment_smartcast(t.a.child(&node, 0), t.a.child(&node, 1))
	}
	if node.kind in [.assign, .selector_assign, .index_assign] && node.op == .assign
		&& !node.skip_ownership_drops && node.children_count == 2 && !isnil(t.tc) {
		lhs_id := t.a.child(&node, 0)
		rhs_id := t.a.child(&node, 1)
		lhs_node := t.a.nodes[int(lhs_id)]
		// Whole-variable assignment overwrites the variable's storage type, even
		// when an active optional smartcast makes the expression read as its payload.
		mut lhs_type_name := pre_smartcast_lhs_type
		if lhs_type_name.len == 0 {
			lhs_type_name = if lhs_node.kind == .ident {
				t.var_type(lhs_node.value)
			} else {
				t.lvalue_type(t.a.child(&node, 0))
			}
		}
		if lhs_type_name.len == 0 {
			lhs_type_name = t.lvalue_type(new_children[0])
		}
		if lhs_type_name.len == 0 {
			lhs_type_name = t.original_expr_type(t.a.child(&node, 0))
		}
		lhs_type := t.tc.parse_type(lhs_type_name)
		// V1 autofree leaves aggregate field/index replacement shallow. In particular,
		// parser token rotations rely on moving `prev = current; current = peek`
		// without destroying `current` between those two assignments.
		autofree_aggregate_lvalue := t.tc.autofree_enabled()
			&& node.kind in [.selector_assign, .index_assign]
		if !t.cur_fn_manualfree && !autofree_aggregate_lvalue
			&& t.tc.ownership_type_requires_destruction(lhs_type)
			&& !t.tc.ownership_assignment_reinitializes_moved_value(id)
			&& !t.tc.ownership_expr_moves_storage(rhs_id, lhs_id) {
			mut result := []flat.NodeId{}
			t.drain_pending(mut result)
			tmp_name := t.new_temp('drop_assign')
			tmp_type := lhs_type.name()
			mut tmp_value := new_children[1]
			// A clone call can retain the assignment's optional expected type on its
			// transformed call node even though the lowered call still returns the
			// payload. The overwrite temporary must therefore wrap a non-optional
			// source explicitly before it is declared as the optional LHS type.
			if t.is_optional_type_name(tmp_type) {
				source_type := t.original_expr_type(rhs_id)
				payload_type := t.optional_base_type(t.qualify_optional_type(tmp_type))
				value_node := t.a.nodes[int(tmp_value)]
				value_is_wrapper := value_node.kind == .struct_init && t.is_optional_type_name(if value_node.value.len > 0 {
					value_node.value
				} else {
					value_node.typ
				})
				transformed_type := if value_node.kind == .ident {
					t.var_type(value_node.value)
				} else {
					''
				}
				value_is_optional := t.is_optional_type_name(transformed_type)
				if !value_is_wrapper && !value_is_optional && !t.is_optional_type_name(source_type)
					&& t.normalize_type_alias(source_type) == t.normalize_type_alias(payload_type) {
					t.set_node_typ(int(tmp_value), payload_type)
					tmp_value = t.make_optional_some(tmp_value, tmp_type)
				}
			}
			result << t.make_decl_assign_typed(tmp_name, tmp_value, tmp_type)
			lvalue := t.stabilize_transformed_lvalue_for_reuse(new_children[0])
			t.drain_pending(mut result)
			drop_call := t.make_call_typed('drop_owned', [lvalue], 'void')
			result << t.make_expr_stmt(drop_call)
			result << t.make_assign_after_owned_drop(lvalue, t.make_ident(tmp_name))
			t.invalidate_smartcast_for_lvalue(t.a.child(&node, 0))
			return result
		}
	}
	for i := 0; i < int(node.children_count); i += 2 {
		lhs_id := t.a.child(&node, i)
		preserves_smartcast := node.op == .assign && i + 1 < int(node.children_count)
			&& t.assignment_preserves_smartcast(lhs_id, t.a.child(&node, i + 1))
		if !preserves_smartcast {
			t.invalidate_smartcast_for_lvalue(lhs_id)
		}
	}
	if int(id) in t.local_closure_field_cleanups {
		mut result := []flat.NodeId{}
		t.drain_pending(mut result)
		lhs_id := t.a.child(&node, 0)
		mut closure_type := t.lvalue_type(lhs_id)
		if closure_type.len == 0 {
			closure_type = t.node_type(new_children[1])
		}
		closure_name := t.new_temp('field_closure')
		t.set_var_type(closure_name, closure_type)
		result << t.make_decl_assign_typed(closure_name, new_children[1], closure_type)
		result << t.make_assign(new_children[0], t.make_ident(closure_name))
		result << t.make_local_closure_cleanup_defer(closure_name)
		return result
	}
	if cleanup_name := t.local_closure_overwrite_name(id, node) {
		mut result := []flat.NodeId{}
		t.drain_pending(mut result)
		closure_type := t.var_type(cleanup_name)
		tmp_name := t.new_temp('closure_assign')
		result << t.make_decl_assign_typed(tmp_name, new_children[1], closure_type)
		destroy_condition := t.make_infix(.ne, t.make_ident(tmp_name), t.make_ident(cleanup_name))
		result << t.make_if(destroy_condition, t.make_block([
			t.make_local_closure_destroy_stmt(cleanup_name),
		]), t.make_empty())
		result << t.make_assign(new_children[0], t.make_ident(tmp_name))
		if post_assign := t.fn_value_self_capture_refresh_stmt(node, new_children) {
			result << post_assign
		}
		return result
	}
	preserve_self_capture_source := t.fn_value_self_capture_refresh_source(node)
	mut new_id := id
	if !t.inplace_assign_rewrites || preserve_self_capture_source
		|| !t.rewrite_children_in_place(id, new_children) {
		start := t.a.children.len
		for nc in new_children {
			t.a.children << nc
		}
		new_id = t.a.add_node(flat.Node{
			kind: node.kind
			op: node.op
			children_start: start
			children_count: node.children_count
			pos: node.pos
			value: node.value
			typ: node.typ
			skip_ownership_drops: node.skip_ownership_drops
		})
	}
	if node.kind == .assign && node.op == .left_shift_assign {
		t.annotate_left_shift_assign(new_id)
	}
	mut result := t.with_pending_before(new_id)
	if preserve_self_capture_source {
		if post_assign := t.fn_value_self_capture_refresh_stmt(node, new_children) {
			result << post_assign
		}
	}
	return result
}

fn (mut t Transformer) try_lower_discarded_closure_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.op != .assign || node.children_count != 2 {
		return none
	}
	lhs := t.a.child_node(&node, 0)
	if lhs.kind != .ident || lhs.value != '_' {
		return none
	}
	return t.lower_discarded_closure_value(t.a.child(&node, 1))
}

fn (mut t Transformer) lower_discarded_closure_value(id flat.NodeId) ?[]flat.NodeId {
	fn_type := t.discarded_closure_value_type(id) or { return none }
	if !t.used_fn_contains_name('closure.closure_create_with_data') {
		return none
	}
	if !t.discarded_closure_value_is_exclusive(id) {
		return none
	}

	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	closure_value := t.transform_expr_for_type(id, fn_type)
	t.drain_pending(mut result)
	closure_name := t.new_temp('discarded_closure')
	t.set_var_type(closure_name, fn_type)
	result << t.make_decl_assign_typed(closure_name, closure_value, fn_type)
	result << t.make_local_closure_destroy_stmt(closure_name)
	return result
}

fn (t &Transformer) discarded_closure_value_is_exclusive(id flat.NodeId) bool {
	if t.expr_allocates_fresh_runtime_closure(id) {
		return true
	}
	return t.call_returns_exclusive_closure(id)
}

fn (t &Transformer) call_returns_exclusive_closure(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.call_returns_exclusive_closure(t.a.child(&node, 0))
	}
	if node.kind != .call {
		return false
	}
	name := t.call_name_for_node(id, node)
	return t.exclusive_closure_return_fns[name]
}

fn (t &Transformer) discarded_closure_value_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	if fn_type := t.fn_value_type_name(id) {
		return fn_type
	}
	node := t.a.nodes[int(id)]
	mut candidates := [
		node.typ,
		t.node_type(id),
		t.resolve_expr_type(id),
		t.original_expr_type(id),
	]
	if node.kind == .call {
		candidates << t.current_call_return_type(node)
	}
	for candidate in candidates {
		if t.is_fn_pointer_type_name(candidate) {
			return t.normalize_type_alias(candidate)
		}
	}
	return none
}

fn (t &Transformer) local_closure_overwrite_name(id flat.NodeId, node flat.Node) ?string {
	if node.kind != .assign || node.op != .assign || node.children_count != 2 {
		return none
	}
	return t.local_closure_cleanup_assigns[int(id)] or { none }
}

fn (t &Transformer) fn_value_self_capture_refresh_source(node flat.Node) bool {
	if node.op != .assign || node.children_count != 2 {
		return false
	}
	lhs := t.a.child_node(&node, 0)
	return lhs.kind == .ident && lhs.value.len > 0
		&& t.fn_literal_captures_name(t.a.child(&node, 1), lhs.value)
}

fn (mut t Transformer) update_option_assignment_smartcast(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	key := t.expr_key(lhs_id)
	if key.len == 0 {
		return
	}
	lhs_type := t.original_expr_type(lhs_id)
	if !t.is_optional_type_name(lhs_type) {
		return
	}
	t.invalidate_smartcast_for_lvalue(lhs_id)
	base_type := t.optional_base_type(t.qualify_optional_type(lhs_type))
	mut rhs_type := t.node_type(rhs_id)
	if rhs_type.len == 0 {
		rhs_type = t.resolve_expr_type(rhs_id)
	}
	if t.is_optional_type_name(rhs_type)
		|| t.normalize_type_alias(rhs_type) != t.normalize_type_alias(base_type) {
		return
	}
	t.push_smartcast(key, base_type, option_unwrap_marker)
}

fn (mut t Transformer) fn_value_self_capture_refresh_stmt(node flat.Node, new_children []flat.NodeId) ?flat.NodeId {
	if node.op != .assign || node.children_count != 2 || new_children.len != 2 {
		return none
	}
	lhs := t.a.nodes[int(new_children[0])]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	rhs_source_id := t.a.child(&node, 1)
	if !t.fn_literal_captures_name(rhs_source_id, lhs.value) {
		return none
	}
	context_type := t.runtime_closure_context_type(new_children[1]) or { return none }
	fn_type := t.var_type(lhs.value)
	if fn_type.len == 0 {
		return none
	}
	closure_value := t.make_ident(lhs.value)
	t.set_node_typ(int(closure_value), fn_type)
	closure_ptr := t.make_cast('voidptr', closure_value, 'voidptr')
	context_data := t.make_call_typed('closure.closure_data', [closure_ptr], 'voidptr')
	t.mark_fn_used_name('closure.closure_data')
	context_ptr_type := '&${context_type}'
	context_ptr := t.make_cast(context_ptr_type, context_data, context_ptr_type)
	context_field := t.make_selector_op(context_ptr, lhs.value, fn_type, .arrow)
	refresh_value := t.make_ident(lhs.value)
	t.set_node_typ(int(refresh_value), fn_type)
	return t.make_assign(context_field, refresh_value)
}

fn (t &Transformer) runtime_closure_context_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .struct_init && node.value.ends_with('_Ctx') {
		return node.value
	}
	for i in 0 .. node.children_count {
		if context_type := t.runtime_closure_context_type(t.a.child(&node, i)) {
			return context_type
		}
	}
	return none
}

fn (t &Transformer) fn_literal_captures_name(id flat.NodeId, name string) bool {
	if int(id) < 0 || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .fn_literal {
		return false
	}
	for i in 0 .. node.children_count {
		child := t.a.nodes[int(t.a.child(&node, i))]
		if child.kind == .ident && child.value == name {
			return true
		}
	}
	return false
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
	if sc.sum_type_name == option_unwrap_marker {
		mut rhs_type := t.node_type(rhs_id)
		if rhs_type.len == 0 {
			rhs_type = t.resolve_expr_type(rhs_id)
		}
		return !t.is_optional_type_name(rhs_type)
			&& t.normalize_type_alias(rhs_type) == t.normalize_type_alias(sc.variant_name)
	}
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

// stabilize_transformed_lvalue_for_reuse rebuilds an lvalue so evaluating it repeatedly
// does not repeat side effects. It preserves the lvalue shape instead of copying its value:
// indexed assignments still reach their normal array/map lowering, while dynamic bases,
// indices and dereferenced pointers are materialized exactly once.
fn (mut t Transformer) stabilize_transformed_lvalue_for_reuse(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
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
			mut children := []flat.NodeId{cap: int(node.children_count)}
			children << t.stabilize_transformed_lvalue_for_reuse(t.a.child(&node, 0))
			for i in 1 .. node.children_count {
				children << t.stabilize_transformed_lvalue_component(t.a.child(&node, i), 'drop_lvalue_selector')
			}
			return t.rebuild_transformed_lvalue(node, children)
		}
		.index {
			if node.children_count == 0 {
				return id
			}
			mut children := []flat.NodeId{cap: int(node.children_count)}
			children << t.stabilize_transformed_lvalue_for_reuse(t.a.child(&node, 0))
			for i in 1 .. node.children_count {
				children << t.stabilize_transformed_lvalue_component(t.a.child(&node, i), 'drop_lvalue_index')
			}
			return t.rebuild_transformed_lvalue(node, children)
		}
		.prefix {
			if node.op != .mul || node.children_count == 0 {
				return id
			}
			child := t.stabilize_transformed_lvalue_component(t.a.child(&node, 0), 'drop_lvalue_pointer')
			return t.rebuild_transformed_lvalue(node, [child])
		}
		.paren {
			if node.children_count == 0 {
				return id
			}
			child := t.stabilize_transformed_lvalue_for_reuse(t.a.child(&node, 0))
			return t.rebuild_transformed_lvalue(node, [child])
		}
		else {
			return id
		}
	}
}

fn (mut t Transformer) stabilize_transformed_lvalue_component(id flat.NodeId, prefix string) flat.NodeId {
	if t.is_stable_expr_for_reuse(id) {
		return id
	}
	tmp_name := t.new_temp(prefix)
	typ := t.node_type(id)
	if typ.len > 0 {
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, id, typ)
	} else {
		t.pending_stmts << t.make_decl_assign(tmp_name, id)
	}
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) rebuild_transformed_lvalue(node flat.Node, children []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos: node.pos
		value: node.value
		typ: node.typ
		is_mut: node.is_mut
	})
}

// stabilize_original_lvalue_receiver spills the non-stable dynamic index/base components of
// an *untransformed* lvalue receiver into temps while preserving the lvalue shape and its
// untransformed base, so the caller's re-dispatch transforms the receiver exactly once and a
// mutable receiver keeps its identity (e.g. `items[next()].update(...)` still mutates
// `items[next()]`). Returns none for a non-lvalue (rvalue) receiver, which the caller spills
// by value instead.
fn (mut t Transformer) stabilize_original_lvalue_receiver(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	// A receiver/argument whose value is a pointer reaches its target through that pointer; a
	// later branch prelude can reassign the pointer (`holder.ptr.update(match ... {
	// retarget(mut holder)! } ...)`) before the rebuilt lvalue is read, retargeting the
	// mutation. A pointer is a reference handle that needs no lvalue identity, so capture its
	// value in source order.
	if t.lvalue_type(id).starts_with('&') {
		return t.snapshot_expr_for_reuse(id)
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return id
		}
		.paren {
			if node.children_count == 0 {
				return none
			}
			child := t.a.child(&node, 0)
			inner := t.stabilize_original_lvalue_receiver(child)?
			if inner == child {
				return id
			}
			return t.rebuild_transformed_lvalue(node, [inner])
		}
		.prefix {
			if node.op != .mul || node.children_count == 0 {
				return none
			}
			child_id := t.a.child(&node, 0)
			new_child := if t.is_pure_constant_expr(child_id) {
				child_id
			} else {
				t.spill_original_lvalue_component(child_id, 'recv_deref')
			}
			if new_child == child_id {
				return id
			}
			return t.rebuild_transformed_lvalue(node, [new_child])
		}
		.selector {
			if node.children_count == 0 {
				return none
			}
			base_child := t.a.child(&node, 0)
			base := t.stabilize_original_lvalue_receiver(base_child)?
			if base == base_child {
				return id
			}
			mut children := [base]
			for i in 1 .. node.children_count {
				children << t.a.child(&node, i)
			}
			return t.rebuild_transformed_lvalue(node, children)
		}
		.index {
			if node.children_count == 0 {
				return none
			}
			base_child := t.a.child(&node, 0)
			// If the container base is a reassignable array/map, snapshot it so a later branch
			// prelude that replaces the container (`items[i].update(match ... { replace(mut
			// items)! } ...)`) cannot retarget the in-place mutation — the snapshot shares the
			// original backing storage, so the element mutation still reaches the source-order
			// container. (A pointer base is captured by the top-level check above.)
			base_type := t.normalize_type_alias(t.trim_pointer_type(t.lvalue_type(base_child)))
			base := if (base_type.starts_with('[]') || base_type.starts_with('map['))
				&& !t.is_pure_constant_expr(base_child) {
				t.snapshot_expr_for_reuse(base_child)
			} else {
				t.stabilize_original_lvalue_receiver(base_child)?
			}
			mut changed := base != base_child
			mut children := [base]
			for i in 1 .. node.children_count {
				comp_id := t.a.child(&node, i)
				// Snapshot a value-bearing index component (an ident/selector a later branch
				// prelude could mutate) into a temp while keeping the surrounding lvalue shape,
				// so `items[idx].update(match ... { change(mut idx)! } ...)` mutates the element
				// at the source-order index. A pure constant index needs no snapshot.
				component := if t.is_pure_constant_expr(comp_id) {
					comp_id
				} else {
					t.spill_original_lvalue_component(comp_id, 'recv_index')
				}
				if component != comp_id {
					changed = true
				}
				children << component
			}
			if !changed {
				return id
			}
			return t.rebuild_transformed_lvalue(node, children)
		}
		else {
			return none
		}
	}
}

fn (mut t Transformer) spill_original_lvalue_component(id flat.NodeId, prefix string) flat.NodeId {
	if t.is_ordering_snapshot_temp(id) {
		return id
	}
	transformed := t.transform_expr(id)
	if t.is_ordering_snapshot_temp(transformed) {
		return transformed
	}
	tmp_name := t.new_temp(prefix)
	mut typ := t.node_type(transformed)
	if typ.len == 0 {
		typ = t.node_type(id)
	}
	if typ.len > 0 {
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, transformed, typ)
	} else {
		t.pending_stmts << t.make_decl_assign(tmp_name, transformed)
	}
	t.ordering_snapshot_names[tmp_name] = true
	return t.make_ident(tmp_name)
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
	result << t.make_if(not_ok, t.make_or_else_block(guard_mode, guard_stmts), t.make_empty())
	lhs_type := t.lvalue_type(lhs_id)
	sum_target := t.assignment_sum_target(lhs_id, rhs_id, lhs_type)
	mut rhs := if node.op == .assign && sum_target.len > 0 {
		t.wrap_sum_value(rhs_id, sum_target)
	} else {
		t.transform_expr_for_type(rhs_id, lhs_type)
	}
	if node.op == .assign {
		clone_type := if sum_target.len > 0 { sum_target } else { lhs_type }
		rhs = t.clone_borrowed_assignment_value(rhs_id, rhs, clone_type)
	}
	t.drain_pending(mut result)
	if node.op == .assign && lhs_type.len > 0 && !isnil(t.tc) {
		parsed_lhs_type := t.tc.parse_type(lhs_type)
		if t.tc.ownership_type_requires_destruction(parsed_lhs_type)
			&& !t.tc.ownership_expr_moves_storage(rhs_id, lhs_id) {
			rhs_name := t.new_temp('optional_selector_value')
			result << t.make_decl_assign_typed(rhs_name, rhs, lhs_type)
			stable_lhs := t.stabilize_transformed_lvalue_for_reuse(lowered_lhs)
			t.drain_pending(mut result)
			t.append_owned_lvalue_drop_before_assign(stable_lhs, lhs_type, mut result)
			result << t.make_assign_after_owned_drop(stable_lhs, t.make_ident(rhs_name))
			return result
		}
	}
	result << t.make_assign_op(lowered_lhs, rhs, node.op)
	return result
}

fn (mut t Transformer) optional_selector_lvalue_guard_stmts(body_id flat.NodeId, mode string, guard_source flat.NodeId) []flat.NodeId {
	err_expr := t.make_selector(guard_source, 'err', 'IError')
	if mode == '!' || mode == '?' {
		if t.is_optional_type_name(t.cur_fn_ret_type) {
			return [
				t.make_return(t.make_optional_none_with_err(t.cur_fn_ret_type, err_expr), t.cur_fn_ret_type),
			]
		}
		return [t.make_panic_stmt('option/result propagation failed')]
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
		kind: .selector
		op: node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: node.pos
		value: node.value
		typ: node.typ
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
		kind: .selector
		op: if value_type.starts_with('&') { flat.Op.arrow } else { node.op }
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: node.pos
		value: node.value
		typ: if lhs_type.len > 0 { lhs_type } else { node.typ }
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
	if node.kind !in [.assign, .selector_assign] || node.children_count != 2 {
		return none
	}
	op_name := compound_assign_struct_operator_symbol(node.op) or { return none }
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && lhs.value.len == 0 {
		return none
	}
	if lhs.kind !in [.ident, .selector] || (lhs.kind == .selector && !t.optional_selector_lvalue_source(lhs_id)) {
		return none
	}
	mut lhs_type := if lhs.kind == .ident { t.var_type(lhs.value) } else { t.lvalue_type(lhs_id) }
	if lhs_type.len == 0 {
		lhs_type = t.original_expr_type(lhs_id)
	}
	if lhs_type.starts_with('&') {
		return none
	}
	operator_type := t.compound_assign_operator_type(lhs_id, lhs_type, op_name) or { return none }
	method_name := t.struct_operator_fn_name(operator_type, op_name) or { return none }
	rhs := t.transform_expr_for_type(rhs_id, lhs_type)
	t.mark_fn_used_name(method_name)
	read_lhs := if lhs.kind == .ident {
		t.make_ident(lhs.value)
	} else {
		t.transform_expr(lhs_id)
	}
	write_lhs := if lhs.kind == .ident {
		t.make_ident(lhs.value)
	} else {
		t.transform_lvalue(lhs_id)
	}
	call := t.make_call_typed(method_name, [read_lhs, rhs], lhs_type)
	return [t.make_assign(write_lhs, call)]
}

fn (mut t Transformer) compound_assign_operator_type(lhs_id flat.NodeId, lhs_type string, op_name string) ?string {
	if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
		return none
	}
	lhs := t.a.nodes[int(lhs_id)]
	mut candidates := []string{}
	if lhs.kind == .ident {
		candidates << t.raw_var_type(lhs.value)
		candidates << t.var_type(lhs.value)
	}
	candidates << lhs.typ
	candidates << t.raw_checker_node_type(lhs_id)
	candidates << lhs_type
	for candidate in candidates {
		if operator_type := t.compound_assign_operator_type_candidate(candidate, op_name) {
			return operator_type
		}
	}
	if isnil(t.tc) || lhs_type.len == 0 {
		return none
	}
	clean_lhs_type := t.trim_pointer_type(lhs_type.trim_space())
	if is_numeric_type_name(clean_lhs_type)
		|| clean_lhs_type in ['bool', 'char', 'string', 'voidptr', 'byteptr', 'charptr'] {
		return none
	}
	normalized_lhs := t.normalize_type_alias(lhs_type)
	for alias, target in t.tc.type_aliases {
		if t.normalize_type_alias(target) != normalized_lhs {
			continue
		}
		if operator_type := t.compound_assign_operator_type_candidate(alias, op_name) {
			return operator_type
		}
		short_alias := alias.all_after_last('.')
		if short_alias != alias {
			if operator_type := t.compound_assign_operator_type_candidate(short_alias, op_name) {
				return operator_type
			}
		}
	}
	return none
}

fn (t &Transformer) compound_assign_operator_type_candidate(candidate string, op_name string) ?string {
	clean := t.trim_pointer_type(candidate.trim_space())
	if clean.len == 0 {
		return none
	}
	if is_numeric_type_name(clean)
		|| clean in ['bool', 'char', 'string', 'voidptr', 'byteptr', 'charptr'] {
		return none
	}
	// Prefer an operator declared on the alias itself before resolving the alias
	// to its parent struct. `Color3 += value`, for example, must call `Color3.+`
	// even when `Color3` aliases a `Vec3` that also declares `+`.
	if _ := t.struct_operator_fn_name(clean, op_name) {
		return clean
	}
	if !isnil(t.tc) {
		for alias_name in [clean, t.tc.qualify_name(clean)] {
			alias_target := t.tc.type_aliases[alias_name] or { continue }
			target := t.trim_pointer_type(alias_target)
			if _ := t.struct_operator_fn_name(target, op_name) {
				return target
			}
		}
	}
	normalized := t.trim_pointer_type(t.normalize_type_alias(clean))
	if normalized != clean {
		if _ := t.struct_operator_fn_name(normalized, op_name) {
			return normalized
		}
		normalized_struct := t.struct_lookup_name(normalized)
		if normalized_struct.len > 0 {
			if _ := t.struct_operator_fn_name(normalized_struct, op_name) {
				return normalized_struct
			}
		}
	}
	struct_type := t.struct_lookup_name(clean)
	if struct_type.len > 0 {
		if _ := t.struct_operator_fn_name(struct_type, op_name) {
			return struct_type
		}
	}
	return none
}

fn compound_assign_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus_assign {
			return '+'
		}
		.minus_assign {
			return '-'
		}
		.mul_assign {
			return '*'
		}
		.power_assign {
			return '**'
		}
		.div_assign {
			return '/'
		}
		.mod_assign {
			return '%'
		}
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
		value := t.transform_expr_for_type(rhs_id, field_type)
		t.clone_borrowed_assignment_value(rhs_id, value, field_type)
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
	mut resolved_sum := t.resolve_sum_name(t.trim_pointer_type(sum_type))
	mut sum_candidate := t.trim_pointer_type(sum_type)
	if t.active_specialization_args.len > 0 {
		sum_candidate = t.subst_type(sum_candidate, t.active_specialization_args)
	}
	if resolved := t.resolve_sum_name_from_c_name(sum_candidate) {
		sum_candidate = resolved
	}
	variants := t.concrete_sum_variants_for_candidate(sum_candidate)
	if variants.len == 0 {
		return none
	}
	resolved_sum = sum_candidate
	stmt := t.build_sum_shared_field_assign_chain(base, sum_type, resolved_sum, variants, lhs.value, field_type, rhs, node.op, 0)
	return t.with_pending_before(stmt)
}

fn (mut t Transformer) try_lower_interface_field_assign(node flat.Node) ?[]flat.NodeId {
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
	iface_name := t.resolve_interface_type_name(base_type)
	if iface_name.len == 0 || isnil(t.tc) {
		return none
	}
	field_type := t.interface_field_type_name(iface_name, lhs.value) or { return none }
	mut storage_type := t.raw_expr_type_without_smartcast(base_id)
	if storage_type.len == 0 {
		storage_type = t.original_expr_type(base_id)
	}
	mut storage_iface := t.resolve_interface_type_name(storage_type)
	if storage_iface.len == 0 {
		storage_iface = iface_name
		storage_type = base_type
	}
	has_cached_field := t.interface_has_direct_field(storage_iface, lhs.value)
	mut base := t.transform_lvalue(base_id)
	mut base_ptr := if storage_type.starts_with('&') {
		base
	} else {
		addr := t.make_prefix(.amp, base)
		t.set_node_typ(int(addr), '&${storage_iface}')
		addr
	}
	if !t.is_stable_expr_for_reuse(base_ptr) {
		tmp_name := t.new_temp('iface_lhs')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, base_ptr, '&${storage_iface}')
		base_ptr = t.make_ident(tmp_name)
	}
	mut rhs := if node.op == .assign {
		value := t.transform_expr_for_type(rhs_id, field_type)
		t.clone_borrowed_assignment_value(rhs_id, value, field_type)
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
	rhs = t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'iface_assign')
	impl_index := t.interface_impl_index_for_transform(iface_name)
	object_assign := t.build_interface_field_assign_chain(base_ptr, impl_index, lhs.value, field_type, rhs, node.op, 0)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	if has_cached_field {
		cached_lhs := t.make_selector_op(base_ptr, lhs.value, field_type, .arrow)
		result << t.make_assign_op(cached_lhs, rhs, node.op)
	}
	result << object_assign
	return result
}

fn (t &Transformer) interface_field_type_name(iface_name string, field_name string) ?string {
	if isnil(t.tc) {
		return none
	}
	mut seen := map[string]bool{}
	return t.interface_field_type_name_inner(iface_name, field_name, mut seen)
}

fn (t &Transformer) interface_field_type_name_inner(iface_name string, field_name string, mut seen map[string]bool) ?string {
	if iface_name.len == 0 || iface_name in seen {
		return none
	}
	seen[iface_name] = true
	for embed in t.tc.interface_embeds[iface_name] or { []string{} } {
		if field_type := t.interface_field_type_name_inner(embed, field_name, mut seen) {
			return field_type
		}
	}
	for field in t.tc.interface_fields[iface_name] or { []types.StructField{} } {
		if field.name == field_name {
			return t.normalize_type_alias(field.typ.name())
		}
	}
	return none
}

fn (t &Transformer) interface_has_direct_field(iface_name string, field_name string) bool {
	if isnil(t.tc) {
		return false
	}
	for field in t.tc.interface_fields[iface_name] or { []types.StructField{} } {
		if field.name == field_name {
			return !transform_interface_field_type_contains_self_by_value(field.typ, iface_name)
		}
	}
	return false
}

fn transform_interface_field_type_contains_self_by_value(typ types.Type, name string) bool {
	match typ {
		types.Interface {
			return typ.name == name
		}
		types.OptionType {
			return transform_interface_field_type_contains_self_by_value(typ.base_type, name)
		}
		types.ResultType {
			return transform_interface_field_type_contains_self_by_value(typ.base_type, name)
		}
		types.Alias {
			return transform_interface_field_type_contains_self_by_value(typ.base_type, name)
		}
		types.ArrayFixed {
			return transform_interface_field_type_contains_self_by_value(typ.elem_type, name)
		}
		else {
			return false
		}
	}
}

fn (mut t Transformer) build_interface_field_assign_chain(base_ptr flat.NodeId, impl_index &types.InterfaceImplIndex, field string, field_type string, rhs flat.NodeId, op flat.Op, idx int) flat.NodeId {
	if idx >= impl_index.names.len {
		return t.make_empty()
	}
	impl := impl_index.names[idx]
	type_id := impl_index.ids[impl] or {
		return t.build_interface_field_assign_chain(base_ptr, impl_index, field, field_type, rhs, op, idx + 1)
	}
	tag := t.make_selector_op(base_ptr, '_typ', 'int', .arrow)
	cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
	object := t.make_selector_op(base_ptr, '_object', 'voidptr', .arrow)
	object_ptr := t.make_cast('&${impl}', object, '&${impl}')
	field_lhs := t.struct_field_selector_for_type(object_ptr, impl, field, field_type, true) or {
		return t.build_interface_field_assign_chain(base_ptr, impl_index, field, field_type, rhs, op, idx + 1)
	}
	then_stmt := t.make_assign_op(field_lhs, rhs, op)
	else_stmt := t.build_interface_field_assign_chain(base_ptr, impl_index, field, field_type, rhs, op, idx + 1)
	return t.make_if(cond, t.make_block([then_stmt]), else_stmt)
}

fn (mut t Transformer) lower_interface_field_selector(base flat.NodeId, base_type string, iface_name string, field string, field_type string) flat.NodeId {
	impl_index := t.interface_impl_index_for_transform(iface_name)
	op := if base_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	fallback := if t.interface_has_direct_field(iface_name, field) {
		t.make_selector_op(base, field, field_type, op)
	} else {
		t.zero_value_for_type(field_type)
	}
	return t.build_interface_field_selector_chain(base, base_type, iface_name, impl_index, field, field_type, fallback, 0)
}

fn (mut t Transformer) build_interface_field_selector_chain(base flat.NodeId, base_type string, iface_name string, impl_index &types.InterfaceImplIndex, field string, field_type string, fallback flat.NodeId, idx int) flat.NodeId {
	if idx >= impl_index.names.len {
		return fallback
	}
	impl := impl_index.names[idx]
	if t.has_used_fn_filter() && !t.interface_boxed_type_used(iface_name, impl) {
		return t.build_interface_field_selector_chain(base, base_type, iface_name, impl_index, field, field_type, fallback, idx + 1)
	}
	type_id := impl_index.ids[impl] or {
		return t.build_interface_field_selector_chain(base, base_type, iface_name, impl_index, field, field_type, fallback, idx + 1)
	}
	base_op := if base_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	tag := t.make_selector_op(base, '_typ', 'int', base_op)
	object := t.make_selector_op(base, '_object', 'voidptr', base_op)
	tag_matches := t.make_infix(.eq, tag, t.make_int_literal(type_id))
	object_not_nil := t.make_infix(.ne, object, t.a.add(.nil_literal))
	cond := t.make_infix(.logical_and, tag_matches, object_not_nil)
	object_ptr := t.make_cast('&${impl}', object, '&${impl}')
	value := t.struct_field_selector_for_type(object_ptr, impl, field, field_type, true) or {
		return t.build_interface_field_selector_chain(base, base_type, iface_name, impl_index, field, field_type, fallback, idx + 1)
	}
	then_block := t.make_block([t.make_expr_stmt(value)])
	else_expr := t.build_interface_field_selector_chain(base, base_type, iface_name, impl_index, field, field_type, fallback, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: field_type
	})
}

fn (mut t Transformer) struct_field_selector_for_type(base flat.NodeId, struct_type string, field string, field_type string, base_is_ptr bool) ?flat.NodeId {
	path := t.struct_field_path_for_field(struct_type, field) or { return none }
	mut cur := base
	mut cur_is_ptr := base_is_ptr
	for embedded in path {
		cur = t.make_selector_op(cur, embedded.name, embedded.typ, if cur_is_ptr {
			.arrow
		} else {
			.dot
		})
		cur_is_ptr = embedded.typ.starts_with('&')
	}
	return t.make_selector_op(cur, field, field_type, if cur_is_ptr { .arrow } else { .dot })
}

fn (t &Transformer) struct_field_path_for_field(struct_type string, field string) ?[]FieldInfo {
	mut seen := map[string]bool{}
	return t.struct_field_path_for_field_inner(struct_type, field, mut seen)
}

fn (t &Transformer) struct_field_path_for_field_inner(struct_type string, field string, mut seen map[string]bool) ?[]FieldInfo {
	clean := t.trim_pointer_type(t.normalize_type_alias(struct_type))
	info := t.lookup_struct_info(clean) or { return none }
	mut lookup_key := t.alias_target_type_preserving_main_lock(clean) or { clean }
	if !lookup_key.contains('.') && info.module.len > 0 && info.module !in ['main', 'builtin'] {
		lookup_key = '${info.module}.${lookup_key}'
	}
	if lookup_key.len == 0 {
		lookup_key = info.name
	}
	if lookup_key in seen {
		return none
	}
	seen[lookup_key] = true
	for f in info.fields {
		if f.name == field {
			return []FieldInfo{}
		}
	}
	for f in info.fields {
		if !t.is_embedded_field(f) {
			continue
		}
		embedded_type := t.trim_pointer_type(f.typ)
		if path := t.struct_field_path_for_field_inner(embedded_type, field, mut seen) {
			mut result := []FieldInfo{cap: path.len + 1}
			result << f
			result << path
			return result
		}
	}
	return none
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
	use_ptr := t.variant_references_sum(qv, resolved_sum) && !t.sum_variant_is_direct_pointer(qv)
	variant_base := t.make_selector_op(base, sum_field, if use_ptr { '&${qv}' } else { qv }, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	mut then_stmt := t.make_empty()
	if nested_field_type := t.sum_shared_field_type_name(qv, field) {
		nested_sum := t.resolve_sum_name(qv)
		if nested_variants := t.sum_types[nested_sum] {
			nested_base_type := if use_ptr { '&${qv}' } else { qv }
			then_stmt = t.build_sum_shared_field_assign_chain(variant_base, nested_base_type, nested_sum, nested_variants, field, nested_field_type, rhs, op, 0)
		}
	} else {
		field_lhs := t.struct_field_selector_for_type(variant_base, qv, field, field_type, use_ptr) or {
			t.make_selector_op(variant_base, field, field_type, if use_ptr {
				.arrow
			} else {
				.dot
			})
		}
		then_stmt = t.make_assign_op(field_lhs, rhs, op)
	}
	then_block := t.make_block([then_stmt])
	else_stmt := t.build_sum_shared_field_assign_chain(base, sum_type, resolved_sum, variants, field, field_type, rhs, op, idx + 1)
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

fn (mut t Transformer) transform_sum_value_for_type(rhs_id flat.NodeId, sum_target string) flat.NodeId {
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind == .match_stmt {
		return t.transform_expr_for_type(rhs_id, sum_target)
	}
	if rhs.kind == .call {
		concrete_ret := t.concrete_generic_call_return_type(rhs_id, rhs)
		if concrete_ret.len > 0
			&& t.resolve_sum_name(concrete_ret) == t.resolve_sum_name(sum_target) {
			// A cloned generic call can still carry the return annotation of an
			// earlier specialization. Resolve/retype it before deciding whether its
			// value needs to be boxed as a sum variant.
			return t.transform_expr_for_type(rhs_id, sum_target)
		}
	}
	return t.wrap_sum_value(rhs_id, sum_target)
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
	lhs_value_type_raw := lhs_type[1..]
	lhs_value_type := t.normalize_type_alias(lhs_value_type_raw)
	if node.op != .assign {
		if !t.pointer_value_lvalues[lhs.value] {
			return none
		}
		new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
		return [t.make_assign_op(new_lhs, t.transform_expr(rhs_id), node.op)]
	}
	if !t.pointer_value_lvalues[lhs.value] {
		return none
	}
	if lhs.value in t.heaped_amp_locals {
		new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
		value := t.transform_expr_for_type(rhs_id, lhs_value_type_raw)
		return [
			t.make_assign(new_lhs, t.clone_borrowed_assignment_value(rhs_id, value, lhs_value_type_raw)),
		]
	}
	rhs_node := t.a.nodes[int(rhs_id)]
	if rhs_node.kind == .prefix && rhs_node.op == .amp {
		return none
	}
	mut rhs_type := t.normalize_type_alias(t.node_type(rhs_id))
	if rhs_type.len == 0 || rhs_type == 'unknown' {
		rhs_type = t.normalize_type_alias(t.resolve_expr_type(rhs_id))
	}
	if rhs_node.kind == .ident && t.pointer_value_rvalues[rhs_node.value]
		&& rhs_type.starts_with('&') {
		// Both locals may have been auto-heaped because their addresses escape.
		// Their source-level type is still the pointee value, so `v = w` copies
		// `*w` into `*v`; only an explicit pointer RHS such as `&other` rebinds.
		rhs_type = rhs_type[1..]
	}
	// A pointer-valued RHS rebinds the local pointer (`p = &other`); only a
	// value RHS writes through it (`p = value`). Let the regular assignment path
	// handle pointer rebinding so its lvalue remains `p` instead of `*p`.
	if !t.pointer_value_assign_rhs_matches(lhs_value_type_raw, lhs_value_type, rhs_type) {
		return none
	}
	new_lhs := t.make_prefix(.mul, t.make_ident(lhs.value))
	value := t.transform_expr_for_type(rhs_id, lhs_value_type_raw)
	return [
		t.make_assign(new_lhs, t.clone_borrowed_assignment_value(rhs_id, value, lhs_value_type_raw)),
	]
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
fn (t &Transformer) optional_conversion_source_type(id flat.NodeId) string {
	if int(id) >= 0 && int(id) < t.a.nodes.len {
		node := t.a.nodes[int(id)]
		if node.kind == .ident {
			local_type := t.var_type(node.value)
			if local_type.len > 0 && local_type != 'unknown' {
				return local_type
			}
		}
	}
	mut source_type := t.node_type(id)
	original_source_type := t.original_expr_type(id)
	if t.is_optional_type_name(source_type) && !t.is_optional_type_name(original_source_type)
		&& original_source_type.len > 0 && original_source_type != 'unknown' {
		return original_source_type
	}
	if source_type.len == 0 || source_type == 'unknown' || t.generic_arg_is_unresolved(source_type) {
		checker_source_type := t.raw_checker_node_type(id)
		if checker_source_type.len > 0 && checker_source_type != 'unknown'
			&& !t.generic_arg_is_unresolved(checker_source_type) {
			return checker_source_type
		}
		source_type = t.resolve_expr_type(id)
	}
	return source_type
}

@[direct_array_access]
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
		if node.kind == .enum_val {
			resolved := t.transform_enum_shorthand(id, node, target_type)
			if resolved != id {
				return resolved
			}
		}
		if storage := t.pointer_storage_expr_for_value_target(id, target_type) {
			return storage
		}
		if node.kind == .none_expr && t.is_optional_type_name(target_type) {
			return t.make_optional_none(t.qualify_optional_type(target_type))
		}
		if node.kind == .none_expr && t.is_ierror_type(target_type) {
			return t.make_ierror_none()
		}
		if t.is_optional_type_name(target_type) && node.kind in [.lambda_expr, .fn_literal] {
			optional_target := t.qualify_optional_type(target_type)
			payload_type := t.optional_base_type(optional_target)
			if t.is_fn_pointer_type_name(t.normalize_type_alias(payload_type)) {
				value := t.transform_expr_for_type(id, payload_type)
				return t.make_optional_some(value, optional_target)
			}
		}
		if t.is_optional_type_name(target_type) && node.kind == .ident {
			optional_target := t.qualify_optional_type(target_type)
			payload_type := t.optional_base_type(optional_target)
			if t.is_fn_pointer_type_name(t.normalize_type_alias(payload_type)) {
				if fn_name := t.resolved_ident_fn_value(id, node.value) {
					t.mark_fn_used_name(fn_name)
					value := t.make_ident(fn_name)
					t.set_node_typ(int(value), payload_type)
					return t.make_optional_some(value, optional_target)
				}
			}
		}
		if node.kind == .lambda_expr {
			if lifted := t.lift_lambda_expr_for_fn_param(id, node, target_type) {
				return lifted
			}
		}
		if node.kind == .fn_literal {
			if lifted := t.lift_fn_literal_for_fn_param(id, node, target_type) {
				return lifted
			}
		}
		if node.kind == .ident && t.is_fn_pointer_type_name(t.normalize_type_alias(target_type)) {
			if fn_name := t.resolved_ident_fn_value(id, node.value) {
				t.mark_fn_used_name(fn_name)
				value := t.make_ident(fn_name)
				t.set_node_typ(int(value), target_type)
				return value
			}
		}
		if t.is_interface_type(target_type) {
			share_source := t.interface_target_should_share_source(id, target_type)
			if expr := t.transform_interface_value_for_type(id, target_type, share_source) {
				return expr
			}
			if expr := t.transform_interface_value_for_type(id, target_type, false) {
				return expr
			}
		}
		if t.is_optional_type_name(target_type) && node.kind in [.array_init, .array_literal] {
			optional_target := t.qualify_optional_type(target_type)
			payload_type := t.optional_base_type(optional_target)
			if payload_type.starts_with('[]') || t.is_fixed_array_type(payload_type) {
				value := t.transform_expr_for_type(id, payload_type)
				return t.make_optional_some(value, optional_target)
			}
		}
		if t.is_optional_type_name(target_type) && node.kind == .postfix && node.op == .not
			&& node.children_count == 1 {
			optional_target := t.qualify_optional_type(target_type)
			payload_type := t.optional_base_type(optional_target)
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .array_literal
				&& (payload_type.starts_with('[]') || t.is_fixed_array_type(payload_type)) {
				value := t.transform_expr_for_type(child_id, payload_type)
				return t.make_optional_some(value, optional_target)
			}
		}
		if t.is_optional_type_name(target_type) {
			optional_target := t.qualify_optional_type(target_type)
			if node.kind in [.ident, .selector] {
				source_type := t.original_expr_type(id)
				if t.is_optional_type_name(source_type)
					&& t.qualify_optional_type(source_type) == optional_target {
					return t.make_plain_expr_for_smartcast(id)
				}
			}
			target_payload := t.optional_base_type(optional_target)
			source_type := t.optional_conversion_source_type(id)
			if t.is_optional_type_name(source_type)
				&& t.qualify_optional_type(source_type) == optional_target
				&& node.kind !in [.block, .if_expr, .match_stmt] {
				return t.transform_expr(id)
			}
			if !t.is_optional_type_name(source_type) && t.is_interface_type(target_payload) {
				share_source := t.interface_target_should_share_source(id, target_payload)
				if value := t.transform_interface_value_for_type(id, target_payload, share_source) {
					return t.make_optional_some(value, optional_target)
				}
			}
			if !t.is_optional_type_name(source_type) && t.is_sum_type_name(target_payload) {
				value := t.transform_sum_value_for_type(id, target_payload)
				return t.make_optional_some(value, optional_target)
			}
			if t.is_optional_type_name(source_type) && t.is_sum_type_name(target_payload) {
				if t.sum_target_accepts_variant_type(target_payload, source_type) {
					value := t.wrap_sum_value(id, target_payload)
					return t.make_optional_some(value, optional_target)
				}
				if t.sum_target_accepts_variant_type(target_payload, t.optional_base_type(source_type)) {
					if value := t.transform_optional_value_to_sum(id, source_type, optional_target) {
						return value
					}
				}
			}
			if t.is_optional_type_name(source_type) && target_payload.starts_with('&')
				&& !t.optional_base_type(source_type).starts_with('&') {
				if value := t.transform_optional_value_to_pointer(id, source_type, optional_target) {
					return value
				}
			}
		}
		if target_type.starts_with('&') && node.kind == .ident
			&& t.pointer_global_arg_matches_param(node.value, target_type) {
			return t.transform_expr(id)
		}
		if target_type.starts_with('&') && node.kind == .struct_init
			&& (node.value.starts_with('&') || node.typ.starts_with('&')) {
			// A generic `T{}` retains pointer type when T specializes to `&U`.
			// Keep that storage type authoritative instead of letting the template's
			// pre-specialization checker type coerce the pointer back to `U`.
			value := t.transform_struct_init(id, node)
			t.set_node_typ(int(value), target_type)
			return value
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
		if array_value := t.transform_array_value_for_type(id, target_type) {
			return array_value
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
	}
	expr := t.transform_expr(id)
	source_node := if int(id) >= 0 && int(id) < t.a.nodes.len {
		t.a.nodes[int(id)]
	} else {
		flat.Node{}
	}
	if int(expr) >= 0 && source_node.kind == .call && source_node.typ.contains('[')
		&& target_type.len > 0 && !t.is_optional_type_name(target_type) {
		t.set_node_typ(int(expr), target_type)
	}
	return t.coerce_transformed_expr_to_type(expr, id, target_type)
}

// pointer_storage_expr_for_value_target keeps the storage pointer of a mutable
// container binding when the surrounding typed context already performs the
// value load. Returning an eagerly dereferenced node here makes C generation
// apply that expected-type load a second time.
fn (mut t Transformer) pointer_storage_expr_for_value_target(id flat.NodeId, target_type string) ?flat.NodeId {
	if int(id) < 0 || target_type.len == 0 || target_type.starts_with('&') {
		return none
	}
	mut source_id := id
	mut source := t.a.nodes[int(source_id)]
	if source.kind == .prefix && source.op == .mul && source.children_count == 1 {
		source_id = t.a.child(&source, 0)
		source = t.a.nodes[int(source_id)]
	}
	if source.kind != .ident || !t.pointer_value_rvalues[source.value] {
		return none
	}
	storage_type := t.var_type(source.value)
	if !storage_type.starts_with('&')
		|| t.normalize_type_alias(storage_type[1..]) != t.normalize_type_alias(target_type) {
		return none
	}
	value := t.make_ident(source.value)
	t.set_node_typ(int(value), storage_type)
	return value
}

fn (mut t Transformer) transform_array_value_for_type(id flat.NodeId, target_type string) ?flat.NodeId {
	if isnil(t.tc) || int(id) < 0 || target_type.len == 0 {
		return none
	}
	mut expected_type_name := target_type
	if !expected_type_name.starts_with('[]') {
		for _ in 0 .. 8 {
			next := t.normalize_type_alias(expected_type_name)
			if next == expected_type_name {
				return none
			}
			expected_type_name = next
			if expected_type_name.starts_with('[]') {
				break
			}
		}
		if !expected_type_name.starts_with('[]') {
			return none
		}
	}
	node := t.a.nodes[int(id)]
	if node.kind == .index && node.value == 'range' {
		actual_type_name := t.generic_call_arg_type_for_inference(id)
		if t.normalize_type_alias(actual_type_name) == t.normalize_type_alias(expected_type_name) {
			t.set_node_typ(int(id), expected_type_name)
			return none
		}
	}
	expected_type := t.tc.parse_type(expected_type_name)
	if forwarded_return_type_is_unresolved(expected_type) {
		return none
	}
	expected_base := forwarded_return_unalias_type(expected_type)
	if expected_base is types.Array {
		concrete_call_type := if node.kind == .call {
			t.concrete_generic_call_return_type(id, node)
		} else {
			''
		}
		actual_type_name := if concrete_call_type.len > 0 {
			concrete_call_type
		} else if t.node_type(id).len > 0 {
			t.node_type(id)
		} else {
			t.resolve_expr_type(id)
		}
		actual_type := if actual_type_name.len > 0 {
			t.tc.parse_type(actual_type_name)
		} else {
			t.tc.expr_type(id) or { t.tc.resolve_type(id) }
		}
		if forwarded_return_type_is_unresolved(actual_type) {
			return none
		}
		actual_base := forwarded_return_unalias_type(actual_type)
		if actual_base is types.Array {
			if actual_base.elem_type.name() != expected_base.elem_type.name()
				&& !forwarded_array_elems_storage_identical(actual_base.elem_type, expected_base.elem_type) {
				return t.convert_forwarded_array_to_dynamic(id, actual_type, actual_base.elem_type, expected_type, expected_base.elem_type, false)
			}
		} else if actual_base is types.ArrayFixed {
			return t.convert_forwarded_array_to_dynamic(id, actual_type, actual_base.elem_type, expected_type, expected_base.elem_type, true)
		}
	}
	return none
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
			kind: .struct_init
			children_start: start
			children_count: 1
			value: clean_target
			typ: clean_target
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
	} else if last.kind in [.block, .match_stmt, .if_expr] && t.stmt_value_type(last_id).len > 0 {
		// A block whose value tail is a bare `match`/`if` expression, e.g.
		// `unsafe { match x { ... } }`. Treat the statement-shaped tail as the
		// value expression so the target type reaches its (possibly propagating)
		// branch tails instead of lowering them in a value-less statement context.
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
	t.set_node_value(int(new_block), node.value)
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
	if actual_result_type.len == 0 || actual_result_type == 'void'
		|| t.generic_arg_is_unresolved(actual_result_type) {
		actual_result_type = target_type
	}
	if t.is_fn_pointer_type_name(target_type) {
		actual_result_type = target_type
	}
	if t.sum_target_accepts_variant_type(target_type, actual_result_type) {
		actual_result_type = target_type
	}
	tmp_name := t.new_temp('match_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	prelude << t.make_decl_assign_typed(tmp_name, t.zero_value_for_type(actual_result_type), actual_result_type)
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
		kind: .prefix
		op: node.op
		children_start: start
		children_count: 1
		pos: node.pos
		value: node.value
		typ: if target_type.len > 0 { target_type } else { node.typ }
	})
}

// coerce_transformed_expr_to_type converts coerce transformed expr to type data for transform.
fn (mut t Transformer) coerce_transformed_expr_to_type(expr flat.NodeId, source_id flat.NodeId, target_type string) flat.NodeId {
	if target_type.len == 0 || int(expr) < 0 {
		return expr
	}
	mut expr_type := t.node_type(expr)
	if expr_type.len == 0 {
		expr_type = t.node_type(source_id)
	}
	if expr_type.len == 0 {
		expr_type = t.resolve_expr_type(source_id)
	}
	if expr_type == target_type {
		return expr
	}
	shared_raw_expr_type := expr_type.trim_space()
	mut target := t.normalize_type_alias(target_type)
	if target.len == 0 {
		return expr
	}
	// Shared identifiers already lower to the lock wrapper's `.val` expression.
	// Coercing `shared T` to `T` therefore changes only the semantic type; adding
	// a pointer dereference would produce `*wrapper->val` even though `.val` is a value.
	if shared_raw_expr_type.starts_with('shared ')
		&& t.normalize_type_alias(shared_raw_expr_type[7..].trim_space()) == target {
		t.set_node_typ(int(expr), target)
		return expr
	}
	expr_type = t.normalize_type_alias(expr_type)
	if target == 'string' && int(expr) >= 0 && int(expr) < t.a.nodes.len {
		expr_node := t.a.nodes[int(expr)]
		if expr_node.kind == .call {
			mut call_ret := t.get_call_return_type(expr, expr_node)
			if call_ret.len == 0 {
				call_ret = t.current_call_return_type(expr_node)
			}
			if t.normalize_type_alias(call_ret) == target {
				return expr
			}
		}
	}
	mut optional_target := if t.is_optional_type_name(target_type) {
		t.qualify_optional_type(target_type)
	} else {
		target
	}
	optional_target = t.infer_typed_optional_target(optional_target, expr_type)
	if optional_target.starts_with('!') && t.is_ierror_type(expr_type) {
		return t.make_optional_none_with_err(optional_target, expr)
	}
	if t.is_optional_type_name(optional_target) && int(expr) >= 0 && int(expr) < t.a.nodes.len {
		raw_expr_type := t.a.nodes[int(expr)].typ
		if t.is_optional_type_name(raw_expr_type) {
			return expr
		}
	}
	if t.is_optional_type_name(optional_target) && !t.is_optional_type_name(expr_type) {
		source := if int(source_id) >= 0 { t.a.nodes[int(source_id)] } else { flat.Node{} }
		if source.kind != .none_expr {
			payload_type := t.optional_base_type(optional_target)
			value := if payload_type.starts_with('&') && !expr_type.starts_with('&') {
				t.coerce_transformed_expr_to_type(expr, source_id, payload_type)
			} else if payload_type == 'string' && t.is_ierror_type(expr_type) {
				t.wrap_string_conversion(expr, expr_type)
			} else {
				expr
			}
			return t.make_optional_some(value, optional_target)
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
		if int(source_id) >= 0 {
			source := t.a.nodes[int(source_id)]
			if source.kind == .int_literal && source.value == '0' {
				return t.make_cast(target, expr, target)
			}
		}
		if expr_type in ['voidptr', '&void', 'byteptr', 'charptr'] {
			return t.make_cast(target, expr, target)
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
	// `voidptr` receives the pointer value itself, even when its pointee is also `voidptr`.
	if target == 'voidptr' && expr_type.starts_with('&') {
		return t.make_cast(target, expr, target)
	}
	if expr_type.starts_with('&') {
		expr_value_type := t.normalize_type_alias(expr_type[1..])
		if expr_value_type == target || t.type_alias_targets_type(expr_type[1..], target) {
			if nil_value := t.nil_pointer_address_value(expr, target) {
				return nil_value
			}
			deref := t.make_prefix(.mul, expr)
			t.set_node_typ(int(deref), target)
			return deref
		}
	}
	return expr
}

// nil_pointer_address_value cancels the contextual auto-dereference of an
// address taken from a typed nil pointer value. The checker accepts expressions
// such as `voidptr_field: unsafe { &voidptr(nil) }`; lowering that as
// `*(&voidptr(nil))` would dereference address zero instead of producing nil.
fn (mut t Transformer) nil_pointer_address_value(id flat.NodeId, target_type string) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.children_count != 1 {
		return none
	}
	child_id := t.a.child(&node, 0)
	if node.kind in [.block, .expr_stmt, .paren] {
		return t.nil_pointer_address_value(child_id, target_type)
	}
	if node.kind != .prefix || node.op != .amp || !t.expr_is_nil_like(child_id) {
		return none
	}
	t.set_node_typ(int(child_id), target_type)
	return child_id
}

// is_ierror_type reports whether is ierror type applies in transform.
fn (t &Transformer) is_ierror_type(name string) bool {
	clean := t.trim_pointer_type(t.normalize_type_alias(name))
	if clean == 'IError' || clean == 'builtin.IError' {
		return true
	}
	return !isnil(t.tc) && clean.len > 0 && clean !in ['void', 'unknown'] && t.tc.named_type_compatible_with_ierror(clean)
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
	if node.kind == .int_literal {
		return node.value.len == 0 || node.value == '0'
	}
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return t.expr_is_nil_like(t.a.child(&node, 0))
	}
	if node.kind == .selector && node.value == 'NULL' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		return base.kind == .ident && base.value == 'C'
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
		fields << t.make_sum_literal_field('value', value, t.shared_alias_storage_type(base_type))
	}
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: flat.child_count(fields.len)
		value: optional_type
		typ: optional_type
	})
}

// make_optional_none builds make optional none data for transform.
fn (mut t Transformer) make_optional_none(optional_type string) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(false), 'bool')
	start := t.a.children.len
	t.a.children << ok_field
	return t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: 1
		value: optional_type
		typ: optional_type
	})
}

fn (mut t Transformer) make_ierror_none() flat.NodeId {
	none_value := t.make_struct_init('None__')
	addr := t.make_prefix(.amp, none_value)
	size := t.make_sizeof_type('None__')
	dup := t.make_non_aliasing_allocation_call('memdup', [addr, size], 'voidptr')
	object := t.make_cast('&None__', dup, '&None__')
	type_id := t.ierror_none_type_id
	fields := [
		t.make_sum_literal_field('_typ', t.make_int_literal(type_id), 'int'),
		t.make_sum_literal_field('_object', object, '&None__'),
	]
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: flat.child_count(fields.len)
		value: 'IError'
		typ: 'IError'
	})
}

fn (mut t Transformer) make_ierror_none_type_check(typ flat.NodeId, iface string) flat.NodeId {
	type_id := if t.ierror_none_type_id != 0 && t.is_builtin_ierror_interface_name(iface) {
		t.ierror_none_type_id
	} else {
		t.interface_impl_type_id(iface, 'None__') or { 0 }
	}
	zero_check := t.make_infix(.eq, typ, t.make_int_literal(0))
	none_check := t.make_infix(.eq, typ, t.make_int_literal(type_id))
	return t.make_infix(.logical_or, zero_check, none_check)
}

// make_optional_none_with_err builds make optional none with err data for transform.
fn (mut t Transformer) make_optional_none_with_err(optional_type string, err_expr flat.NodeId) flat.NodeId {
	ok_field := t.make_sum_literal_field('ok', t.make_bool_literal(false), 'bool')
	err_field := t.make_sum_literal_field('err', err_expr, 'IError')
	start := t.a.children.len
	t.a.children << ok_field
	t.a.children << err_field
	return t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: 2
		value: optional_type
		typ: optional_type
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
			if t.selector_is_enum_value(id) {
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
		.call {
			return t.array_accessor_call_can_take_address(node)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) array_accessor_call_can_take_address(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value !in ['first', 'last'] || callee.children_count == 0 {
		return false
	}
	base_id := t.a.child(callee, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	clean := t.normalize_type_alias(base_type.trim_left('&'))
	return clean.starts_with('[]') && t.expr_can_take_address(base_id)
}

fn (t &Transformer) selector_is_enum_value(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 || node.value.len == 0 {
		return false
	}
	base_name := t.selector_expr_name(t.a.child(&node, 0))
	if base_name.len == 0 {
		return false
	}
	enum_name := t.enum_type_name_from_selector_name(base_name) or { return false }
	fields := t.enum_types[enum_name] or { return false }
	return node.value in fields
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
	new_rhs := if t.normalize_type_alias(t.node_type(rhs_id)) in ['char', 'rune'] {
		t.stringify_expr(rhs_id)
	} else {
		t.transform_expr(rhs_id)
	}
	lhs_copy := t.make_ident(lhs.value)
	concat := t.make_call('string__plus', [lhs_copy, new_rhs])
	new_lhs := t.make_ident(lhs.value)
	return [t.make_assign(new_lhs, concat)]
}

// transform_decl_assign_stmt transforms transform decl assign stmt data for transform.
@[direct_array_access]
fn (mut t Transformer) transform_decl_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return [id]
	}
	source_rhs_id := if node.children_count == 2 {
		t.a.child(&node, 1)
	} else {
		flat.empty_node
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
		rhs := t.a.node(rhs_id)
		if rhs.kind == .call {
			concrete := t.concrete_generic_call_return_type(rhs_id, *rhs)
			if concrete.len > 0 {
				t.set_node_typ(int(rhs_id), concrete)
			}
		}
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
			t.record_orm_initialized_fields(lhs.value, t.a.child(&node, 1))
			t.record_sql_query_data_alias(lhs.value, t.a.child(&node, 1))
			mut typ := t.infer_decl_type(node)
			rhs_id := t.a.child(&node, 1)
			rhs := t.a.nodes[int(rhs_id)]
			if rhs.kind == .string_literal && rhs.children_count == 1
				&& rhs.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
				if target := t.comptime_type_expr_type(t.a.child(&rhs, 0)) {
					typ = if rhs.value == '__v3_comptime_new' { '&${target}' } else { target }
				}
			}
			if rhs.kind == .cast_expr && t.is_optional_type_name(rhs.value) {
				// Preserve the optional wrapper when an alias payload is cast
				// explicitly (`a := ?OptFn(callback)`). Alias normalization alone
				// yields the underlying fn type and would make later `a != none`
				// checks see a plain function value.
				typ = t.qualify_optional_type(rhs.value)
			}
			sum_constructor_type := if rhs.kind == .call {
				t.sum_constructor_call_type(rhs)
			} else {
				''
			}
			if sum_constructor_type.len > 0 {
				typ = sum_constructor_type
			} else if rhs.kind == .call {
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
				} else if inferred := t.building_v_math_generic_call_type(rhs) {
					typ = inferred
				}
				if t.generic_arg_is_unresolved(typ) {
					checker_typ := t.checker_node_type(rhs_id)
					if decl_type_is_usable(checker_typ) && !t.generic_arg_is_unresolved(checker_typ) {
						typ = checker_typ
					}
				}
				if fn_value_type := t.checker_call_fn_value_return_type(rhs_id) {
					typ = fn_value_type
					t.set_node_typ(int(t.a.child(&node, 0)), fn_value_type)
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
				if decl_type_is_usable(match_typ) || !decl_type_is_usable(typ) {
					typ = match_typ
				}
			} else if rhs.kind == .block {
				block_typ := t.stmt_value_type(rhs_id)
				if block_typ.len > 0 {
					typ = block_typ
				}
			} else if rhs.kind == .or_expr && rhs.children_count > 0 {
				or_source_id := t.a.child(&rhs, 0)
				if aggregate_type := t.sql_or_expr_aggregate_optional_type(rhs) {
					typ = aggregate_type
				} else if info := t.map_index_info(or_source_id) {
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
				} else if info := t.array_get_call_info(or_source_id) {
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
					fallback_type := if decl_type_is_usable(typ)
						&& !t.generic_arg_is_unresolved(typ) {
						typ
					} else {
						t.stmt_value_type(or_body_id)
					}
					expr_type, value_type := t.or_expr_types(or_source_id, fallback_type)
					if t.is_optional_type_name(expr_type) && value_type.len > 0
						&& value_type != 'void' {
						typ = value_type
					}
				}
			}
			if rhs.kind == .ident
				&& (t.pointer_value_rvalues[rhs.value] || t.mut_param_values[rhs.value])
				&& typ.starts_with('&') {
				typ = typ[1..]
			}
			if node.typ.len == 0 {
				if rhs.kind == .array_literal && rhs.typ.len == 0 && t.is_fixed_array_type(typ) {
					typ = '[]${fixed_array_elem_type(typ)}'
					t.set_node_typ(int(rhs_id), typ)
				}
			}
			if amp_vt := t.pointer_storage_amp_decl_type(rhs_id) {
				// Mut parameters, mutable captures, and heap-promoted value locals use
				// pointer storage. Their source-level address is that pointer itself.
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
					raw_typ = t.raw_decl_type_for_rhs(rhs_id, rhs, typ)
				}
				if rhs.kind == .call {
					generic_raw_typ := t.raw_generic_call_return_type(rhs_id, rhs)
					if generic_raw_typ.len > 0 {
						raw_typ = generic_raw_typ
					}
				}
				if raw_typ.len == 0 || raw_typ == typ {
					if call_raw_typ := t.raw_call_decl_return_type(rhs_id, rhs) {
						raw_typ = call_raw_typ
					}
				}
				if raw_typ.len == 0 || raw_typ == typ {
					if op_raw_typ := t.raw_infix_operator_decl_return_type(rhs) {
						raw_typ = op_raw_typ
					}
				}
				if node.value == 'shared' || node.value.starts_with('shared:') {
					clean_raw := raw_typ.trim_space()
					raw_typ = if clean_raw.starts_with('shared ') {
						clean_raw
					} else {
						'shared ${clean_raw}'
					}
				} else if node.value == 'atomic' || node.value.starts_with('atomic:') {
					clean_raw := raw_typ.trim_space()
					raw_typ = if clean_raw.starts_with('atomic ') {
						clean_raw
					} else {
						'atomic ${clean_raw}'
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
		if src.kind == .ident && src.value in t.mut_fixed_array_capture_sources
			&& src.value !in t.heaped_amp_locals && t.is_fixed_array_type(inferred_typ) {
			return t.heap_escaping_source_decl(node, src.value, inferred_typ)
		}
		if src.kind == .ident && src.value in t.escaping_amp_sources
			&& src.value !in t.heaped_amp_locals && t.heapable_value_type(inferred_typ) {
			return t.heap_escaping_source_decl(node, src.value, inferred_typ)
		}
		// A struct declared `@[heap]` is always heap-allocated at its own declaration,
		// regardless of whether its address is later taken (`@[heap]` is an unconditional
		// promise, not an escape-analysis trigger).
		if src.kind == .ident && node.value != stack_value_decl_marker
			&& src.value !in t.heaped_amp_locals && t.heap_attr_struct_type(inferred_typ) {
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
					t.set_decl_var_type(node, t.a.nodes[int(t.a.child(&node, 0))].value, inferred_typ)
				}
			}
		} else {
			lhs_id := t.a.child(&node, 0)
			mut lhs_type := if inferred_typ.len > 0 {
				inferred_typ
			} else if decl_type_is_usable(node.typ) {
				node.typ
			} else {
				t.lvalue_type(lhs_id)
			}
			if t.array_map_decl_type_needs_refinement(child_id, lhs_type) {
				lhs_type = ''
			}
			sum_target := t.assignment_sum_target(lhs_id, child_id, lhs_type)
			if sum_target.len > 0 && !t.expr_has_smartcast(child_id) {
				new_children << t.clone_borrowed_projection(child_id, t.transform_sum_value_for_type(child_id, sum_target), sum_target)
			} else {
				mut clone_type := lhs_type
				if clone_type.len == 0 {
					clone_type = t.original_expr_type(child_id)
				}
				new_children << t.clone_borrowed_projection(child_id, t.transform_expr_for_type(child_id, lhs_type), clone_type)
			}
		}
	}
	if node.children_count == 2 {
		lhs := t.a.nodes[int(new_children[0])]
		if lhs.kind == .ident && lhs.value.len > 0 {
			rhs_typ := t.node_type(new_children[1])
			unusable_decl_refined := !decl_type_is_usable(node.typ) && (inferred_typ.len == 0
				|| inferred_typ in ['array', 'map', 'unknown']
				|| t.generic_arg_is_unresolved(inferred_typ)
				|| t.concrete_generic_type_refines(inferred_typ, rhs_typ))
			if decl_type_is_usable(rhs_typ) && (unusable_decl_refined
				|| t.decl_should_adopt_lowered_rhs_type(t.a.child(&node, 1), inferred_typ, rhs_typ)) {
				t.set_decl_var_type(node, lhs.value, rhs_typ)
				t.set_node_typ(int(new_children[0]), rhs_typ)
				lhs_original_id := t.a.child(&node, 0)
				rhs_original_id := t.a.child(&node, 1)
				t.record_refined_node_type(int(lhs_original_id), rhs_typ)
				t.record_refined_node_type(int(rhs_original_id), rhs_typ)
				t.record_refined_node_type(int(id), rhs_typ)
				t.set_node_typ(int(lhs_original_id), rhs_typ)
				t.set_node_typ(int(rhs_original_id), rhs_typ)
				t.set_node_typ(int(id), rhs_typ)
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
			if info := t.bound_method_array_expr_info(new_children[1]) {
				t.bound_method_arrays[t.bound_method_array_key(lhs.value)] = info
			} else {
				t.bound_method_arrays.delete(t.bound_method_array_key(lhs.value))
			}
		}
	}
	if inferred_typ.len > 0 && new_children.len > 0 {
		lhs := t.a.nodes[int(new_children[0])]
		if lhs.kind == .ident && (lhs.typ.len == 0 || t.generic_arg_is_unresolved(lhs.typ)
			|| (t.is_sum_type_name(inferred_typ)
				&& t.resolve_sum_name(lhs.typ) != t.resolve_sum_name(inferred_typ))) {
			t.set_node_typ(int(new_children[0]), inferred_typ)
		}
	}
	output_typ := if inferred_typ.len > 0 { inferred_typ } else { node.typ }
	mut new_id := id
	if !t.inplace_decl_assign_rewrites || !t.rewrite_children_in_place(id, new_children) {
		start := t.a.children.len
		for nc in new_children {
			t.a.children << nc
		}
		new_id = t.a.add_node(flat.Node{
			kind: .decl_assign
			op: node.op
			children_start: start
			children_count: node.children_count
			pos: node.pos
			value: node.value
			typ: output_typ
			is_mut: node.is_mut
		})
	} else if output_typ != node.typ {
		t.set_node_typ(int(id), output_typ)
	}
	mut result := t.with_pending_before(new_id)
	if cleanup_name := t.local_closure_cleanup_decls[int(id)] {
		result << t.make_local_closure_cleanup_defer(cleanup_name)
	}
	if node.children_count == 2 {
		lhs := t.a.child_node(&node, 0)
		if lhs.kind == .ident && lhs.value.len > 0 {
			aggregate_type := if inferred_typ.len > 0 {
				inferred_typ
			} else {
				t.node_type(source_rhs_id)
			}
			t.append_local_closure_initializer_cleanups(lhs.value, source_rhs_id, aggregate_type, mut result)
		}
	}
	return result
}

fn (t &Transformer) building_v_math_generic_call_type(node flat.Node) ?string {
	if !t.building_v || t.cur_module != 'math' || node.kind != .call || node.children_count < 2 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value !in ['min', 'max', 'abs'] {
		return none
	}
	typ := t.node_type(t.a.child(&node, 1))
	if decl_type_is_usable(typ) && !t.generic_arg_is_unresolved(typ) {
		return typ
	}
	return none
}

fn (mut t Transformer) append_local_closure_initializer_cleanups(name string, rhs_id flat.NodeId, aggregate_type string, mut result []flat.NodeId) {
	t.append_local_closure_initializer_cleanups_for_value(t.make_ident(name), rhs_id, aggregate_type, mut result)
}

fn (mut t Transformer) append_local_closure_initializer_cleanups_for_value(base flat.NodeId, rhs_id flat.NodeId, aggregate_type string, mut result []flat.NodeId) {
	if int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(rhs_id)]
	if (node.kind in [.paren, .cast_expr, .as_expr] || (node.kind == .postfix && node.op == .not))
		&& node.children_count == 1 {
		t.append_local_closure_initializer_cleanups_for_value(base, t.a.child(&node, 0), aggregate_type, mut result)
		return
	}
	elem_type := if aggregate_type.starts_with('[]') && aggregate_type.len > 2 {
		aggregate_type[2..]
	} else if t.is_fixed_array_type(aggregate_type) {
		fixed_array_elem_type(aggregate_type)
	} else {
		''
	}
	if elem_type.len > 0 && node.kind == .array_literal {
		for index in 0 .. node.children_count {
			value_id := t.a.child(&node, index)
			value := t.a.nodes[int(value_id)]
			if value.kind == .prefix && value.value == '...' {
				return
			}
			elem := t.make_index(base, t.make_int_literal(index), elem_type)
			if int(value_id) in t.local_closure_field_cleanups {
				t.append_local_closure_aggregate_value_cleanup(elem, elem_type, 'array_closure', mut result)
				continue
			}
			t.append_local_closure_initializer_cleanups_for_value(elem, value_id, elem_type, mut result)
		}
		return
	}
	map_type := t.clean_map_type(aggregate_type)
	if map_type.starts_with('map[') && node.kind == .map_init {
		key_type, value_type := t.map_type_parts(map_type)
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			key_id := t.a.child(&node, i)
			key_node := t.a.nodes[int(key_id)]
			if key_node.kind == .prefix && key_node.value == '...' {
				return
			}
			if key_node.kind !in [.int_literal, .string_literal, .char_literal, .enum_val] {
				continue
			}
			value_id := t.a.child(&node, i + 1)
			if int(value_id) in t.local_closure_field_cleanups {
				// Map lowering already retained the per-entry value temporary before
				// map__set can overwrite this key.
				continue
			}
			key := t.transform_expr_for_type(key_id, key_type)
			elem := t.make_index(base, key, value_type)
			t.append_local_closure_initializer_cleanups_for_value(elem, value_id, value_type, mut result)
		}
		return
	}
	if node.kind != .struct_init {
		return
	}
	info := t.lookup_struct_info(node.value) or { StructInfo{} }
	for i in 0 .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			''
		}
		if field_name.len == 0 {
			continue
		}
		// Direct callback fields are materialized and cleaned while transforming the
		// struct initializer. Descend only when the owned callback is nested in the
		// field's aggregate value.
		if int(field_id) in t.local_closure_field_cleanups {
			continue
		}
		mut target_field_name := field_name
		mut field_type := t.lookup_struct_field_type(aggregate_type, field_name) or { '' }
		if field_type.len == 0 {
			field_type = t.lookup_struct_field_type(node.value, field_name) or { '' }
		}
		if field_type.len == 0 {
			for info_field in info.fields {
				if info_field.name == field_name || (info_field.name.contains('.')
					&& info_field.name.all_after_last('.') == field_name) {
					target_field_name = info_field.name
					field_type = info_field.typ
					break
				}
			}
		}
		if field_type.len == 0 {
			continue
		}
		field_value := t.make_selector(base, target_field_name, field_type)
		t.append_local_closure_initializer_cleanups_for_value(field_value, t.a.child(&field, 0), field_type, mut result)
	}
}

fn (mut t Transformer) append_local_closure_aggregate_value_cleanup(value flat.NodeId, typ string, prefix string, mut result []flat.NodeId) {
	closure_name := t.new_temp(prefix)
	t.set_var_type(closure_name, typ)
	result << t.make_decl_assign_typed(closure_name, value, typ)
	result << t.make_local_closure_cleanup_defer(closure_name)
}

fn (mut t Transformer) make_local_closure_cleanup_defer(name string) flat.NodeId {
	destroy_stmt := t.make_local_closure_destroy_stmt(name)
	body := t.make_block([destroy_stmt])
	start := t.a.children.len
	t.a.children << body
	return t.a.add_node(flat.Node{
		kind: .defer_stmt
		children_start: start
		children_count: 1
	})
}

fn (mut t Transformer) make_local_closure_destroy_stmt(name string) flat.NodeId {
	closure_value := t.make_ident(name)
	closure_type := t.var_type(name)
	if closure_type.len > 0 {
		t.set_node_typ(int(closure_value), closure_type)
	}
	closure_ptr := t.make_cast('voidptr', closure_value, 'voidptr')
	destroy := t.make_call_typed('closure.closure_try_destroy', [closure_ptr], 'void')
	t.mark_fn_used_name('closure.closure_try_destroy')
	return t.make_expr_stmt(destroy)
}

fn (t &Transformer) concrete_generic_type_refines(current string, refined string) bool {
	if current.len == 0 || refined.len == 0 || !refined.contains('[') {
		return false
	}
	base, args, ok := generic_app_parts(refined)
	if !ok || args.len == 0 || t.generic_args_have_placeholders(args) {
		return false
	}
	clean_current := t.trim_pointer_type(t.normalize_type_alias(current))
	clean_base := t.trim_pointer_type(t.normalize_type_alias(base))
	return clean_current == clean_base
		|| clean_current.all_after_last('.') == clean_base.all_after_last('.')
}

fn (t &Transformer) decl_should_adopt_lowered_rhs_type(rhs_id flat.NodeId, inferred_typ string, rhs_typ string) bool {
	if inferred_typ.len == 0 || rhs_typ.len == 0 || inferred_typ == rhs_typ {
		return false
	}
	if int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
		return false
	}
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind == .map_init {
		return rhs_typ.starts_with('map[')
	}
	if rhs.kind != .call || rhs.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&rhs, 0)
	return callee.kind == .selector && callee.value == 'map'
}

fn (t &Transformer) array_map_decl_type_needs_refinement(rhs_id flat.NodeId, typ string) bool {
	if !typ.starts_with('[]') {
		return false
	}
	elem_type := typ[2..]
	if elem_type != 'void' && decl_type_is_usable(elem_type)
		&& !t.generic_arg_is_unresolved(elem_type) {
		return false
	}
	if int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
		return false
	}
	rhs := t.a.nodes[int(rhs_id)]
	if rhs.kind != .call || rhs.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&rhs, 0)
	return callee.kind == .selector && callee.value == 'map'
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
		if typ.len == 0 && decl_type_is_usable(rhs_authority)
			&& !t.generic_arg_is_unresolved(rhs_authority) {
			typ = rhs_authority
		}
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
		if typ.len > 0 {
			t.append_local_closure_initializer_cleanups(lhs.value, rhs_id, typ, mut result)
		}
		if cleanup_name := t.local_closure_cleanup_values[int(rhs_id)] {
			result << t.make_local_closure_cleanup_defer(cleanup_name)
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
	if rhs.kind == .match_stmt {
		if expanded := t.expand_multi_return_match_decl(rhs_id, rhs, lhs_ids) {
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
	if rhs.kind == .match_stmt {
		if expanded := t.expand_multi_return_match_assign(rhs_id, rhs, lhs_ids) {
			return [expanded]
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
			mut lvalue := t.transform_lvalue(lhs_id)
			t.drain_pending(mut result)
			if !t.tc.ownership_expr_moves_storage(rhs_id, lhs_id)
				&& t.tc.ownership_type_requires_destruction(field_type) {
				lvalue = t.stabilize_transformed_lvalue_for_reuse(lvalue)
				t.drain_pending(mut result)
				t.append_owned_lvalue_drop_before_assign(lvalue, field_type_name, mut result)
			}
			result << t.make_assign_after_owned_drop(lvalue, field)
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
	mut lhs_was_moved := []bool{len: lhs_count}
	if !isnil(t.tc) {
		for i in 0 .. lhs_count {
			lhs_id := t.multi_assign_lhs_id(node, i)
			for j in 0 .. rhs_count {
				if t.tc.ownership_expr_moves_storage(t.multi_assign_rhs_id(node, j), lhs_id) {
					lhs_was_moved[i] = true
					break
				}
			}
		}
	}
	for i in 0 .. lhs_count {
		lhs_id := t.multi_assign_lhs_id(node, i)
		rhs_id := t.multi_assign_rhs_id(node, i)
		lhs_ids << lhs_id
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			rhs := t.transform_expr(rhs_id)
			t.drain_pending(mut result)
			result << t.make_expr_stmt(rhs)
			tmp_names << ''
			continue
		}
		mut lhs_type := if lhs.kind in [.selector, .index] {
			t.lvalue_type(lhs_id)
		} else {
			t.original_expr_type(lhs_id)
		}
		if lhs_type.len == 0 {
			lhs_type = t.lvalue_type(lhs_id)
		}
		if lhs.kind == .ident && t.pointer_value_lvalues[lhs.value] && lhs_type.starts_with('&') {
			lhs_type = lhs_type[1..]
		}
		mut rhs := if lhs_type.len > 0 {
			t.transform_expr_for_type(rhs_id, lhs_type)
		} else {
			t.transform_expr(rhs_id)
		}
		if lhs_type.len > 0 {
			rhs = t.clone_borrowed_assignment_value(rhs_id, rhs, lhs_type)
		}
		t.drain_pending(mut result)
		if lhs.kind == .ident && lhs.value == '_' {
			// A discarded slot still has to evaluate its RHS, but it does not need the
			// typed temporary used to preserve values for the later assignments.
			result << t.make_assign(t.make_ident('_'), rhs)
			tmp_names << ''
			continue
		}
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
		mut lvalue := if lhs.kind == .ident && t.pointer_value_lvalues[lhs.value] {
			t.make_prefix(.mul, t.make_ident(lhs.value))
		} else {
			t.transform_lvalue_without_smartcast(lhs_id)
		}
		t.drain_pending(mut result)
		mut lhs_type := t.lvalue_type(lhs_id)
		if lhs_type.len == 0 {
			lhs_type = t.original_expr_type(lhs_id)
		}
		if !lhs_was_moved[i] && !isnil(t.tc) && lhs_type.len > 0
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(lhs_type)) {
			lvalue = t.stabilize_transformed_lvalue_for_reuse(lvalue)
			t.drain_pending(mut result)
			t.append_owned_lvalue_drop_before_assign(lvalue, lhs_type, mut result)
		}
		result << t.make_assign_after_owned_drop(lvalue, t.make_ident(tmp_names[i]))
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
	node := t.a.nodes[int(id)]
	// A call expression can carry the surrounding expected tuple type in the
	// checker cache. Prefer the callee declaration so forwarded returns still
	// see the concrete source slots that need conversion.
	if node.kind == .call {
		if specialized_ret := t.specialized_interface_method_call_return_type(id, node) {
			if items := multi_return_types_from_type(t.tc.parse_type(specialized_ret), expected_count) {
				return items
			}
		}
		ret := t.get_call_return_type(id, node)
		if ret.len > 0 && !t.generic_arg_is_unresolved(ret) {
			if items := multi_return_types_from_type(t.tc.parse_type(ret), expected_count) {
				return items
			}
		}
		if node.typ.len > 0 && !t.generic_arg_is_unresolved(node.typ) {
			if items := multi_return_types_from_type(t.tc.parse_type(node.typ), expected_count) {
				return items
			}
		}
	}
	if typ := t.tc.expr_type(id) {
		if items := multi_return_types_from_type(typ, expected_count) {
			return items
		}
	}
	if node.kind == .call {
		if items := t.find_multi_return_call_types(node, expected_count) {
			return items
		}
	}
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
		return t.multi_return_types_for_expr(t.a.child(&node, node.children_count - 1), expected_count)
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
		branch_id := t.a.child(&node, i)
		branch := t.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(branch) }
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
		for key, ret in t.multi_return_fn_ret_types {
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

fn (mut t Transformer) expand_multi_return_match_decl(rhs_id flat.NodeId, rhs flat.Node, lhs_ids []flat.NodeId) ?[]flat.NodeId {
	if lhs_ids.len == 0 {
		return none
	}
	value_types := t.tc.multi_expr_tail_types_for_transform(rhs_id, lhs_ids.len) or { return none }
	mut result := []flat.NodeId{}
	for i, lhs_id in lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value == '_' {
			continue
		}
		typ := if i < value_types.len { value_types[i].name() } else { 'int' }
		t.set_var_type(lhs.value, t.normalize_type_alias(typ))
		result << t.make_decl_assign_typed(lhs.value, t.zero_value_for_type(typ), typ)
	}
	result << t.expand_multi_return_match_assign(rhs_id, rhs, lhs_ids) or { return none }
	return result
}

fn (mut t Transformer) expand_multi_return_match_assign(_rhs_id flat.NodeId, rhs flat.Node, lhs_ids []flat.NodeId) ?flat.NodeId {
	rewritten := t.rewrite_multi_return_match_assign(rhs, lhs_ids) or { return none }
	return t.lower_one_match(rewritten)
}

fn (mut t Transformer) rewrite_multi_return_match_assign(node flat.Node, lhs_ids []flat.NodeId) ?flat.Node {
	if node.kind != .match_stmt || node.children_count < 2 || lhs_ids.len == 0 {
		return none
	}
	mut match_children := []flat.NodeId{cap: int(node.children_count)}
	match_children << t.a.child(&node, 0)
	for i in 1 .. node.children_count {
		branch := t.a.child_node(&node, i)
		if branch.kind != .match_branch {
			return none
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(branch) }
		mut prefix := []flat.NodeId{}
		mut assignment := flat.empty_node
		if parts := t.match_branch_tuple_parts(branch, body_start, lhs_ids.len) {
			prefix = parts.prefix.clone()
			assignment = t.make_multi_value_assign(lhs_ids, parts.values)
		} else if t.match_branch_tail_exits(branch, body_start) {
			// An exiting branch (return/break/continue/noreturn call) supplies
			// no tuple values; keep its body unchanged.
			for j in body_start .. int(branch.children_count) {
				prefix << t.a.child(branch, j)
			}
		} else {
			tail_id := t.match_branch_multi_return_expr(branch, body_start, lhs_ids.len) or {
				return none
			}
			for j in body_start .. int(branch.children_count) - 1 {
				prefix << t.a.child(branch, j)
			}
			assignment = t.make_multi_return_assign(lhs_ids, tail_id)
		}
		mut branch_children := []flat.NodeId{cap: body_start + prefix.len + 1}
		for j in 0 .. body_start {
			branch_children << t.a.child(branch, j)
		}
		branch_children << prefix
		if assignment != flat.empty_node {
			branch_children << assignment
		}
		start := t.a.children.len
		t.a.children << branch_children
		match_children << t.a.add_node(flat.Node{
			kind: branch.kind
			op: branch.op
			value: branch.value
			typ: branch.typ
			payload: branch.payload
			children_start: start
			children_count: flat.child_count(branch_children.len)
			pos: branch.pos
			is_mut: branch.is_mut
			skip_ownership_drops: branch.skip_ownership_drops
		})
	}
	start := t.a.children.len
	t.a.children << match_children
	return flat.Node{
		kind: node.kind
		op: node.op
		value: node.value
		typ: node.typ
		payload: node.payload
		children_start: start
		children_count: flat.child_count(match_children.len)
		pos: node.pos
		is_mut: node.is_mut
		skip_ownership_drops: node.skip_ownership_drops
	}
}

// match_branch_tail_exits reports whether a match branch body always leaves
// the enclosing scope, so a lowered multi-value match needs no assignment there.
fn (t &Transformer) match_branch_tail_exits(branch flat.Node, body_start int) bool {
	if branch.children_count <= body_start {
		return false
	}
	return t.stmt_tail_exits(t.a.child(&branch, branch.children_count - 1))
}

fn (t &Transformer) stmt_tail_exits(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt, .break_stmt, .continue_stmt {
			return true
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return t.stmt_tail_exits(t.a.child(&node, node.children_count - 1))
		}
		.expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return t.stmt_tail_exits(t.a.child(&node, 0))
		}
		.call {
			return t.is_noreturn_call(id)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) match_branch_multi_return_expr(branch flat.Node, body_start int, count int) ?flat.NodeId {
	if count <= 1 || branch.children_count <= body_start {
		return none
	}
	tail := t.a.child_node(&branch, branch.children_count - 1)
	if tail.kind != .expr_stmt || tail.children_count != 1 {
		return none
	}
	expr_id := t.a.child(tail, 0)
	_ := t.multi_return_types_for_expr(expr_id, count) or { return none }
	return expr_id
}

fn (mut t Transformer) make_multi_value_assign(lhs_ids []flat.NodeId, values []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for i, lhs_id in lhs_ids {
		t.a.children << lhs_id
		t.a.children << values[i]
	}
	return t.a.add_node(flat.Node{
		kind: .assign
		op: .assign
		value: lhs_ids.len.str()
		children_start: start
		children_count: flat.child_count(lhs_ids.len * 2)
	})
}

fn (mut t Transformer) make_multi_return_assign(lhs_ids []flat.NodeId, value flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs_ids[0]
	t.a.children << value
	for lhs_id in lhs_ids[1..] {
		t.a.children << lhs_id
	}
	return t.a.add_node(flat.Node{
		kind: .assign
		op: .assign
		value: lhs_ids.len.str()
		children_start: start
		children_count: flat.child_count(lhs_ids.len + 1)
	})
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
	if _ := t.multi_if_branch_multi_return_expr(branch_id, count) {
		return true
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
			return t.branch_has_tuple_tail_values(t.a.child(&branch, branch.children_count - 1), count)
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
	all_is := t.extract_all_is_exprs(cond_id)
	all_none_eq := t.extract_else_branch_smartcasts(cond_id)
	new_cond := t.transform_and_chain_smartcasts(cond_id)
	t.drain_pending(mut result)
	saved_var_types := t.var_types.clone()
	base_smartcasts := t.smartcast_stack.clone()
	base_invalidated := t.invalidated_smartcasts.clone()
	for info in all_is {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	then_block := t.multi_if_assign_block(then_id, lhs_ids)
	mut branch_invalidated := t.invalidated_smartcasts.clone()
	t.smartcast_stack = base_smartcasts.clone()
	t.invalidated_smartcasts = base_invalidated.clone()
	t.restore_var_types(saved_var_types)
	mut else_block := t.make_empty()
	if node.children_count >= 3 {
		for info in all_none_eq {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			else_stmts := t.lower_multi_if_assign(else_node, lhs_ids)
			else_block = t.make_block(else_stmts)
		} else {
			else_block = t.multi_if_assign_block(else_id, lhs_ids)
		}
		for key, invalidated in t.invalidated_smartcasts {
			if invalidated {
				branch_invalidated[key] = true
			}
		}
	}
	t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
	t.invalidated_smartcasts = branch_invalidated.move()
	t.restore_var_types(saved_var_types)
	result << t.make_if(new_cond, then_block, else_block)
	return result
}

// multi_if_assign_block supports multi if assign block handling for Transformer.
fn (mut t Transformer) multi_if_assign_block(block_id flat.NodeId, lhs_ids []flat.NodeId) flat.NodeId {
	parts := t.tuple_block_parts(block_id, lhs_ids.len) or {
		if multi_block := t.multi_if_multi_return_assign_block(block_id, lhs_ids) {
			return multi_block
		}
		if nested := t.nested_multi_tail_assign_block(block_id, lhs_ids) {
			return nested
		}
		return t.transform_expr(block_id)
	}
	stmts := t.multi_if_assign_stmts(parts, lhs_ids)
	return t.make_block(stmts)
}

fn (t &Transformer) multi_if_branch_multi_return_expr(block_id flat.NodeId, count int) ?flat.NodeId {
	if int(block_id) < 0 || count <= 0 {
		return none
	}
	block := t.a.nodes[int(block_id)]
	if block.kind != .block || block.children_count == 0 {
		return none
	}
	last_id := t.a.child(&block, block.children_count - 1)
	last := t.a.nodes[int(last_id)]
	if last.kind != .expr_stmt || last.children_count != 1 {
		return none
	}
	expr_id := t.a.child(&last, 0)
	_ := t.multi_return_types_for_expr(expr_id, count) or { return none }
	return expr_id
}

fn (mut t Transformer) multi_if_multi_return_assign_block(block_id flat.NodeId, lhs_ids []flat.NodeId) ?flat.NodeId {
	expr_id := t.multi_if_branch_multi_return_expr(block_id, lhs_ids.len) or { return none }
	items := t.multi_return_types_for_expr(expr_id, lhs_ids.len) or { return none }
	block := t.a.nodes[int(block_id)]
	mut stmts := t.transform_stmts(t.a.children_of(&block)[..int(block.children_count) - 1])
	value := t.transform_expr(expr_id)
	t.drain_pending(mut stmts)
	tmp_name := t.new_temp('multi_if_ret')
	stmts << t.make_decl_assign_typed(tmp_name, value, t.multi_return_type_name(items))
	for i, lhs_id in lhs_ids {
		if i >= items.len {
			break
		}
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		field_type := items[i].name()
		field := t.make_selector(t.make_ident(tmp_name), 'arg${i}', field_type)
		mut lvalue := t.transform_lvalue(lhs_id)
		t.drain_pending(mut stmts)
		if !isnil(t.tc) && !t.tc.ownership_expr_moves_storage(expr_id, lhs_id)
			&& t.tc.ownership_type_requires_destruction(items[i]) {
			lvalue = t.stabilize_transformed_lvalue_for_reuse(lvalue)
			t.drain_pending(mut stmts)
			t.append_owned_lvalue_drop_before_assign(lvalue, field_type, mut stmts)
		}
		stmts << t.make_assign_after_owned_drop(lvalue, field)
	}
	return t.make_block(stmts)
}

fn (mut t Transformer) multi_if_assign_stmts(parts TupleBlockParts, lhs_ids []flat.NodeId) []flat.NodeId {
	mut stmts := t.transform_stmts(parts.prefix)
	mut tmp_names := []string{cap: parts.values.len}
	mut lhs_was_moved := []bool{len: lhs_ids.len}
	if !isnil(t.tc) {
		for i, lhs_id in lhs_ids {
			for value_id in parts.values {
				if t.tc.ownership_expr_moves_storage(value_id, lhs_id) {
					lhs_was_moved[i] = true
					break
				}
			}
		}
	}
	for i, value_id in parts.values {
		is_blank := if i < lhs_ids.len {
			lhs := t.a.nodes[int(lhs_ids[i])]
			lhs.kind == .ident && lhs.value == '_'
		} else {
			false
		}
		target_type := if i < lhs_ids.len && !is_blank { t.lvalue_type(lhs_ids[i]) } else { '' }
		value := if target_type.len > 0 {
			t.transform_if_branch_value(value_id, target_type)
		} else {
			t.transform_expr(value_id)
		}
		t.drain_pending(mut stmts)
		tmp_name := t.new_temp('multi_if')
		mut tmp_type := if target_type.len > 0 { target_type } else { t.node_type(value_id) }
		if tmp_type.len == 0 {
			tmp_type = t.node_type(value)
		}
		if tmp_type.len == 0 {
			tmp_type = t.resolve_expr_type(value_id)
		}
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
		mut lvalue := t.transform_lvalue(lhs_id)
		t.drain_pending(mut stmts)
		mut lhs_type := t.lvalue_type(lhs_id)
		if lhs_type.len == 0 {
			lhs_type = t.original_expr_type(lhs_id)
		}
		if !lhs_was_moved[i] && !isnil(t.tc) && lhs_type.len > 0
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(lhs_type)) {
			lvalue = t.stabilize_transformed_lvalue_for_reuse(lvalue)
			t.drain_pending(mut stmts)
			t.append_owned_lvalue_drop_before_assign(lvalue, lhs_type, mut stmts)
		}
		stmts << t.make_assign_after_owned_drop(lvalue, t.make_ident(tmp_name))
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
		return [id]
	}
	child_id := t.a.children[node.children_start]
	child := t.a.nodes[int(child_id)]
	if child.kind == .call && t.is_disabled_fn_call(child_id, child) {
		return []flat.NodeId{}
	}
	if discarded := t.lower_discarded_closure_value(child_id) {
		return discarded
	}
	if t.autolock_depth == 0 {
		if lock_id := t.shared_postfix_autolock_target(child_id) {
			body := t.make_block([id])
			start := t.a.children.len
			t.a.children << lock_id
			t.a.children << body
			auto_lock := t.a.add_node(flat.Node{
				kind: .lock_expr
				value: 'lock'
				children_start: start
				children_count: 2
			})
			return t.transform_lock_stmt(auto_lock, t.a.nodes[int(auto_lock)])
		}
		if lock_id := t.shared_array_append_autolock_target(child_id) {
			return t.lower_shared_array_append_autolock_stmt(lock_id, id)
		}
	}
	if child.kind == .or_expr && !t.is_map_index_or_expr(child) && !t.is_array_index_or_expr(child)
		&& !t.is_string_slice_or_expr(child) && !t.is_channel_receive_or_expr(child) {
		if t.is_void_test_propagation(child) {
			preserved := t.preserve_or_expr_for_codegen(child_id, child)
			return t.with_pending_before(t.make_expr_stmt(preserved))
		}
		if lowered := t.transform_match_trailing_or_expr(child_id, child) {
			return t.with_pending_before(lowered)
		}
		if lowered := t.try_lower_array_append_or_stmt(child) {
			return lowered
		}
		if child.children_count > 0 {
			expr_id := t.a.child(&child, 0)
			expr_type, _ := t.or_expr_types(expr_id, child.typ)
			expr_node := t.a.nodes[int(expr_id)]
			if !t.is_optional_type_name(expr_type) || expr_node.kind == .infix {
				if lowered := t.transform_nested_optional_or_expr(expr_id, child) {
					return t.with_pending_before(t.make_expr_stmt(lowered))
				}
			}
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
	if lowered := t.try_lower_nested_map_index_postfix_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_map_index_postfix_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_array_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_ignored_owned_array_pop_stmt(child_id, child) {
		return lowered
	}
	if lowered := t.try_lower_flag_enum_stmt(child_id) {
		return [lowered]
	}
	new_child := t.transform_expr(child_id)
	if child.kind == .select_stmt && t.a.node(new_child).kind == .block {
		return t.with_pending_before(new_child)
	}
	if t.rewrite_one_child_in_place(id, new_child) {
		return t.with_pending_before(id)
	}
	start := t.a.children.len
	t.a.children << new_child
	new_id := t.a.add_node(flat.Node{
		kind: .expr_stmt
		op: node.op
		children_start: start
		children_count: 1
		pos: node.pos
		value: node.value
		typ: node.typ
	})
	return t.with_pending_before(new_id)
}

fn (t &Transformer) is_void_test_propagation(node flat.Node) bool {
	return node.value in ['!', '?'] && t.cur_fn_ret_type == 'void'
		&& t.cur_fn_name.starts_with('test_') && t.cur_file.ends_with('_test.v')
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
	clean_typ := typ.trim_space()
	if clean_typ.starts_with('shared ')
		|| (clean_typ.len == 0 && t.local_decl_is_shared_before(base.value, id)) {
		return base_id
	}
	return none
}

fn (t &Transformer) shared_array_append_autolock_target(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return none
	}
	raw_type := t.raw_var_type(lhs.value)
	typ := if raw_type.len > 0 { raw_type } else { t.var_type(lhs.value) }
	if !typ.trim_space().starts_with('shared ') {
		return none
	}
	if !t.clean_array_append_lhs_type(typ).starts_with('[]') {
		return none
	}
	return lhs_id
}

fn (mut t Transformer) try_lower_shared_array_append_autolock_stmt(append_id flat.NodeId) ?[]flat.NodeId {
	if t.autolock_depth != 0 {
		return none
	}
	lock_id := t.shared_array_append_autolock_target(append_id) or { return none }
	return t.lower_shared_array_append_autolock_stmt(lock_id, t.make_expr_stmt(append_id))
}

fn (mut t Transformer) lower_shared_array_append_autolock_stmt(lock_id flat.NodeId, stmt_id flat.NodeId) []flat.NodeId {
	body := t.make_block([stmt_id])
	start := t.a.children.len
	t.a.children << lock_id
	t.a.children << body
	auto_lock := t.a.add_node(flat.Node{
		kind: .lock_expr
		value: 'lock'
		children_start: start
		children_count: 2
	})
	return t.transform_lock_stmt(auto_lock, t.a.nodes[int(auto_lock)])
}

fn (t &Transformer) local_decl_is_shared_before(name string, before flat.NodeId) bool {
	if name.len == 0 || int(before) < 0 || int(before) >= t.a.nodes.len {
		return false
	}
	// Prepared transforms index all shared declaration names. Most interpolation
	// identifiers are ordinary locals, so reject them before reconstructing their
	// enclosing scope path through the compiler-sized AST.
	if t.source_parent_ids.len > 0 && !t.shared_local_decl_names[name] {
		return false
	}
	return t.local_binding_before(name, before) or { false }
}

// local_binding_before reports whether `name` resolves to a visible parameter or local at
// `before`; the returned bool records whether that binding is shared. It follows the use's
// ancestor path so declarations in sibling blocks do not leak into the lookup.
fn (t &Transformer) local_binding_before(name string, before flat.NodeId) ?bool {
	if name.len == 0 || int(before) < 0 || int(before) >= t.a.nodes.len {
		return none
	}
	// Follow the mutation's ancestor path and inspect only declarations preceding that
	// path in each enclosing scope; bindings inside sibling blocks must not leak out.
	mut path := [int(before)]
	mut cursor := int(before)
	mut found_fn_scope := false
	for _ in 0 .. t.a.nodes.len {
		parent_id := t.source_parent_id(cursor)
		if parent_id < 0 {
			break
		}
		path << parent_id
		parent := t.a.nodes[parent_id]
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			found_fn_scope = true
			break
		}
		cursor = parent_id
	}
	if !found_fn_scope {
		return none
	}
	mut found := false
	mut is_shared := false
	for path_idx := path.len - 1; path_idx > 0; path_idx-- {
		parent := t.a.nodes[path[path_idx]]
		next_id := path[path_idx - 1]
		mut inside_for_in_body := false
		if parent.kind == .for_in_stmt {
			header_count := parent.value.int()
			if header_count >= 3 && header_count < parent.children_count {
				for i in header_count .. parent.children_count {
					if int(t.a.child(&parent, i)) == next_id {
						inside_for_in_body = true
						break
					}
				}
			}
		}
		for i in 0 .. parent.children_count {
			child_id := int(t.a.child(&parent, i))
			if child_id == next_id {
				break
			}
			// An if-guard declaration is visible only in the guarded (then) branch.
			// Do not let it shadow a constant while resolving an else-branch use.
			if parent.kind == .if_expr && i == 0 && parent.children_count > 1
				&& next_id != int(t.a.child(&parent, 1)) {
				continue
			}
			if child_id < 0 || child_id >= t.a.nodes.len {
				continue
			}
			child := t.a.nodes[child_id]
			if child.kind == .param && child.value == name {
				found = true
				is_shared = child.typ.trim_space().starts_with('shared ')
			} else if parent.kind == .for_in_stmt && inside_for_in_body && i < 2
				&& child.kind == .ident && child.value == name {
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
	if !found {
		return none
	}
	return is_shared
}

fn (mut t Transformer) build_source_parent_index() {
	t.source_parent_ids = []int{len: t.a.nodes.len, init: -1}
	mut decls := map[string][]int{}
	mut fn_offsets := map[int][]int{}
	mut shared_names := map[string]bool{}
	for parent_id, node in t.a.nodes {
		for i in 0 .. node.children_count {
			child_id := int(t.a.child(&node, i))
			if child_id >= 0 && child_id < t.source_parent_ids.len && child_id != parent_id {
				t.source_parent_ids[child_id] = parent_id
			}
		}
		if node.kind == .fn_decl && node.pos.is_valid() {
			fn_offsets[node.pos.id] << node.pos.offset
		}
		if node.kind != .decl_assign || node.children_count < 2 {
			continue
		}
		lhs_id := t.a.child(&node, 0)
		if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
			continue
		}
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value.len > 0 {
			decls[lhs.value] << parent_id
			if node.value == 'shared' || node.value.starts_with('shared:') {
				shared_names[lhs.value] = true
			}
		}
	}
	for file_id, offsets in fn_offsets {
		mut sorted := offsets.clone()
		sorted.sort()
		fn_offsets[file_id] = sorted
	}
	t.local_decl_nodes_by_name = decls.move()
	t.fn_decl_offsets_by_file = fn_offsets.move()
	t.shared_local_decl_names = shared_names.move()
}

fn (t &Transformer) source_parent_id(child_id int) int {
	if child_id >= 0 && child_id < t.source_parent_ids.len {
		return t.source_parent_ids[child_id]
	}
	// Hand-built transform tests and nodes synthesized after prepare have no entry
	// in the immutable source index. Synthesized parent/child nodes are appended
	// together, so find them from the newest end of the compiler-sized AST.
	for parent_id := t.a.nodes.len - 1; parent_id >= 0; parent_id-- {
		node := t.a.nodes[parent_id]
		for i in 0 .. node.children_count {
			if int(t.a.child(&node, i)) == child_id && child_id != parent_id {
				return parent_id
			}
		}
	}
	return -1
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
fn (mut t Transformer) transform_block_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	if t.rewrite_children_in_place(id, new_children) {
		return [id]
	}
	new_block := t.make_block(new_children)
	t.set_node_value(int(new_block), node.value)
	return [new_block]
}

fn (mut t Transformer) transform_comptime_if_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	take_then := t.comptime_type_condition_value(node.value) or { return [_id] }
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
			// A type can start with `&&` (for example `&&char`). That is two pointer
			// indirections, not a logical AND with an empty left operand. Keep looking
			// for a real binary operator later in the condition.
			if needle in ['&&', '||']
				&& (s[..i].trim_space().len == 0 || s[i + needle.len..].trim_space().len == 0) {
				continue
			}
			if needle == '&&' && s[..i].trim_space().trim('&').len == 0 {
				continue
			}
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
			matches := if left.starts_with('\$') && !right.starts_with('\$') {
				t.comptime_type_matches(right, left) or { return none }
			} else {
				t.comptime_type_matches(left, right) or { return none }
			}
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
		l := t.comptime_condition_int_value(left) or { continue }
		r := t.comptime_condition_int_value(right) or { continue }
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

fn (t &Transformer) comptime_condition_int_value(raw string) ?int {
	clean := raw.trim_space()
	if value := comptime_const_int_value(clean) {
		return value
	}
	if t.cur_fn_is_generic {
		return none
	}
	reflected_type, reflected_member := generic_comptime_typeof_operand(clean) or { return none }
	if reflected_member != 'idx' {
		return none
	}
	resolved := t.resolve_substituted_type_text(reflected_type)
	if resolved.len == 0 {
		return none
	}
	return t.comptime_field_type_id(resolved, t.cur_module)
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
		// A real generic parameter remains undecidable until specialization. A
		// known capitalized type can still be compared, while an undeclared name
		// simply is not the requested type (the reference compiler's behaviour).
		if clean_actual in t.active_generic_params {
			return none
		}
		if isnil(t.tc) || t.tc.parse_type(clean_actual) is types.Unknown {
			return false
		}
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
		'\$array' {
			return normalized.starts_with('[]') || transform_type_text_is_fixed_array(normalized)
		}
		'\$array_dynamic' {
			return normalized.starts_with('[]')
		}
		'\$array_fixed' {
			return transform_type_text_is_fixed_array(normalized)
		}
		'\$map' {
			return normalized.starts_with('map[')
		}
		'\$function' {
			return normalized.starts_with('fn(') || normalized.starts_with('fn (')
		}
		'\$option' {
			return normalized.starts_with('?')
		}
		'\$shared' {
			return normalized.starts_with('shared ')
		}
		'\$pointer' {
			return normalized.starts_with('&') || normalized in ['voidptr', 'byteptr', 'charptr']
		}
		'\$voidptr' {
			return normalized == 'voidptr'
		}
		'\$int' {
			if typ := types.builtin_type(normalized) {
				return typ.is_integer()
			}
			return false
		}
		'\$float' {
			if typ := types.builtin_type(normalized) {
				return typ.is_float()
			}
			return false
		}
		'\$string' {
			return normalized == 'string'
		}
		'\$struct' {
			if _ := types.builtin_type(normalized) {
				return false
			}
			return t.comptime_struct_type_known(normalized)
		}
		'\$enum' {
			return t.comptime_enum_type_known(normalized)
		}
		'\$alias' {
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
		'\$sumtype' {
			return !isnil(t.tc)
				&& (clean_actual in t.tc.sum_types || t.resolve_sum_name(clean_actual) in t.tc.sum_types
					|| normalized in t.tc.sum_types)
		}
		'\$interface' {
			return !isnil(t.tc) && normalized in t.tc.interface_names
		}
		else {}
	}

	expected_normalized := t.normalize_type_alias(clean_expected)
	if (normalized.starts_with('fn(') || normalized.starts_with('fn ('))
		&& (expected_normalized.starts_with('fn(')
			|| expected_normalized.starts_with('fn (')) {
		return transform_sum_fn_variant_key(normalized) == transform_sum_fn_variant_key(expected_normalized)
	}
	if !isnil(t.tc) && expected_normalized in t.tc.interface_names {
		if t.tc.type_text_implements_interface(clean_actual, expected_normalized)
			|| (normalized != clean_actual
				&& t.tc.type_text_implements_interface(normalized, expected_normalized)) {
			return true
		}
	}
	if normalized == expected_normalized {
		return true
	}
	// Main-module types can retain either their source spelling (`Foo`) or their
	// checker spelling (`main.Foo`) while reflected generic bodies are cloned.
	// They name the same declaration; imported qualified types must stay distinct.
	if normalized.starts_with('main.') || expected_normalized.starts_with('main.') {
		actual_main := if normalized.starts_with('main.') { normalized[5..] } else { normalized }
		expected_main := if expected_normalized.starts_with('main.') {
			expected_normalized[5..]
		} else {
			expected_normalized
		}
		if actual_main == expected_main {
			return true
		}
	}
	return false
}

fn comptime_const_int_value(raw string) ?int {
	clean := comptime_condition_strip_outer_parens(raw.trim_space())
	if clean.len == 0 {
		return none
	}
	if clean.starts_with('int(') {
		end := comptime_condition_matching_paren(clean, 'int'.len)
		if end == clean.len - 1 {
			return comptime_const_int_value(clean['int('.len..clean.len - 1])
		}
	}
	for op in [' | ', ' ^ ', ' & ', ' << ', ' >> ', ' + ', ' - ', ' * ', ' / ', ' % ', '&'] {
		idx := comptime_condition_top_level_index(clean, op)
		if idx < 0 {
			continue
		}
		left := comptime_const_int_value(clean[..idx]) or { return none }
		right := comptime_const_int_value(clean[idx + op.len..]) or { return none }
		if op in [' / ', ' % '] && right == 0 {
			return none
		}
		return match op {
			' | ' {
				left | right
			}
			' ^ ' {
				left ^ right
			}
			' & ', '&' {
				left & right
			}
			' << ' {
				int(u64(left) << right)
			}
			' >> ' {
				left >> right
			}
			' + ' {
				left + right
			}
			' - ' {
				left - right
			}
			' * ' {
				left * right
			}
			' / ' {
				left / right
			}
			else {
				left % right
			}
		}
	}
	if clean.starts_with('0x') || clean.starts_with('0X') {
		mut value := 0
		if clean.len == 2 {
			return none
		}
		for c in clean[2..] {
			digit := if c >= `0` && c <= `9` {
				int(c - `0`)
			} else if c >= `a` && c <= `f` {
				int(c - `a`) + 10
			} else if c >= `A` && c <= `F` {
				int(c - `A`) + 10
			} else {
				return none
			}
			value = value * 16 + digit
		}
		return value
	}
	if comptime_is_int(clean) {
		return clean.int()
	}
	return none
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
fn (mut t Transformer) transform_block_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	new_block := if t.inplace_block_expr_rewrites && t.rewrite_children_in_place(id, new_children) {
		id
	} else {
		t.make_block(new_children)
	}
	t.set_node_value(int(new_block), node.value)
	mut block_typ := t.checker_expr_type_name(id) or { '' }
	if !decl_type_is_usable(block_typ) && node.children_count > 0 {
		last_id := t.a.child(&node, node.children_count - 1)
		last := t.a.nodes[int(last_id)]
		tail_id := if last.kind == .expr_stmt && last.children_count > 0 {
			t.a.child(&last, last.children_count - 1)
		} else {
			last_id
		}
		block_typ = t.checker_expr_type_name(tail_id) or { '' }
	}
	if !decl_type_is_usable(block_typ) {
		block_typ = t.stmt_value_type(new_block)
	}
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
	} else if t.is_stmt_kind_id(int(body.kind)) {
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
	if t.rewrite_children_in_place(id, children) {
		if lock_typ != node.typ {
			t.set_node_typ(int(id), lock_typ)
		}
		return id
	}
	start := t.a.children.len
	t.a.children << children
	return t.a.add_node(flat.Node{
		kind: .lock_expr
		value: node.value
		typ: lock_typ
		op: node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos: node.pos
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
				return [t.transform_block_expr(branch_id, branch)]
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
		return [id]
	}
	body_id := t.a.child(&node, 0)
	if int(body_id) < 0 {
		return [id]
	}
	body := t.a.nodes[int(body_id)]
	new_body := if body.kind == .block {
		t.transform_block_expr(body_id, body)
	} else if t.is_stmt_kind_id(int(body.kind)) {
		t.make_block(t.transform_stmt(body_id))
	} else {
		t.make_block([t.transform_expr(body_id)])
	}
	new_id := if t.rewrite_one_child_in_place(id, new_body) {
		id
	} else {
		start := t.a.children.len
		t.a.children << new_body
		t.a.add_node(flat.Node{
			kind: .defer_stmt
			children_start: start
			children_count: 1
			pos: node.pos
			value: node.value
			typ: node.typ
		})
	}
	return [new_id]
}

fn (mut t Transformer) transform_select_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return [t.transform_select_expr(id, node)]
}

fn (mut t Transformer) transform_select_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 1 {
		branch := t.a.child_node(&node, 0)
		if branch.kind == .select_branch && branch.value == 'else' {
			mut body := []flat.NodeId{}
			for i in 0 .. branch.children_count {
				child_id := t.a.child(branch, i)
				child := t.a.node(child_id)
				if t.is_stmt_kind_id(int(child.kind)) {
					body << t.transform_stmt(child_id)
				} else {
					body << t.transform_expr(child_id)
				}
			}
			return t.make_block(body)
		}
	}
	// A later send case whose value hoists a value branch materializes its prelude into
	// pending_stmts, which is drained before the whole select while gen_select evaluates
	// earlier case values during select setup — so `second` would run before `first` (and the
	// prelude could mutate an earlier case's channel first). When any case hoists, capture each
	// case's channel and send value into temps in case order so the preludes land in
	// pending_stmts in source order.
	mut order_cases := false
	for i in 0 .. node.children_count {
		if t.select_case_hoists_value_branch(t.a.child(&node, i)) {
			order_cases = true
			break
		}
	}
	mut branches := []flat.NodeId{cap: int(node.children_count)}
	if t.smartcast_stack.len == 0 {
		for i in 0 .. node.children_count {
			branches << t.transform_select_branch(t.a.child(&node, i), order_cases)
		}
	} else {
		base_smartcasts := t.smartcast_stack.clone()
		base_invalidated := t.invalidated_smartcasts.clone()
		mut merged_invalidated := base_invalidated.clone()
		for i in 0 .. node.children_count {
			t.smartcast_stack = base_smartcasts.clone()
			t.invalidated_smartcasts = base_invalidated.clone()
			branches << t.transform_select_branch(t.a.child(&node, i), order_cases)
			for key, invalidated in t.invalidated_smartcasts {
				if invalidated {
					merged_invalidated[key] = true
				}
			}
		}
		t.invalidated_smartcasts = merged_invalidated.move()
		t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
	}
	if t.rewrite_children_in_place(id, branches) {
		return id
	}
	start := t.a.children.len
	t.a.children << branches
	return t.a.add_node(flat.Node{
		kind: .select_stmt
		children_start: start
		children_count: flat.child_count(branches.len)
		pos: node.pos
		typ: node.typ
	})
}

fn (mut t Transformer) transform_select_branch(id flat.NodeId, order_cases bool) flat.NodeId {
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
		child := t.a.nodes[int(child_id)]
		children << if branch.value == 'recv_assign' && body_start == 2 && i == 0 {
			t.transform_lvalue_without_smartcast(child_id)
		} else if body_start == 2 && i == 0 {
			t.transform_lvalue(child_id)
		} else if order_cases && child.kind == .infix && child.op == .arrow
			&& child.children_count >= 2 {
			// Send case `ch <- value`: capture channel and value in source order.
			t.transform_select_send_ordered(child)
		} else if order_cases && child.kind == .prefix && child.op == .arrow
			&& child.children_count > 0 {
			// Receive case `<-ch`: capture the channel in source order.
			t.transform_select_recv_ordered(child)
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
		if t.is_stmt_kind_id(int(child.kind)) {
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
	if t.rewrite_children_in_place(id, children) {
		return id
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: .select_branch
		children_start: start
		children_count: flat.child_count(children.len)
		pos: branch.pos
		value: branch.value
		typ: branch.typ
	})
}

// select_case_hoists_value_branch reports whether a select case's channel or send value hoists
// a value `match`/`if` whose materialization prelude would otherwise be drained before the whole
// select. It covers a send case's channel and value, and a receive case's channel.
fn (t &Transformer) select_case_hoists_value_branch(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	branch := t.a.nodes[int(id)]
	if branch.kind != .select_branch || branch.children_count == 0 {
		return false
	}
	first := t.a.nodes[int(t.a.child(&branch, 0))]
	// Send case `ch <- value`: either the channel or the value can hoist a branch.
	if first.kind == .infix && first.op == .arrow && first.children_count >= 2 {
		return t.operand_hoists_value_branch(t.a.child(&first, 0))
			|| t.operand_hoists_value_branch(t.a.child(&first, 1))
	}
	// Receive case `<-ch`: the channel can hoist a branch.
	if first.kind == .prefix && first.op == .arrow && first.children_count > 0 {
		return t.operand_hoists_value_branch(t.a.child(&first, 0))
	}
	// Receive-assign case `x := <-ch`: child 1 is the receive prefix.
	if branch.children_count >= 2 {
		second := t.a.nodes[int(t.a.child(&branch, 1))]
		if second.kind == .prefix && second.op == .arrow && second.children_count > 0 {
			return t.operand_hoists_value_branch(t.a.child(&second, 0))
		}
	}
	return false
}

// snapshot_select_operand lowers a select-case channel or send value in source order. A value
// `match`/`if` operand (directly or nested) is materialized through the value-aware path; a
// nonconstant (value-bearing) operand is snapshotted into a temp so a later case's hoisted
// prelude cannot mutate a stable identifier before select setup reads it; a pure constant is
// left inline.
fn (mut t Transformer) snapshot_select_operand(id flat.NodeId, prefix string) flat.NodeId {
	val := t.transform_value_operand(id)
	if t.is_value_match_or_if_operand(id) {
		// Already materialized into a value temp above.
		return val
	}
	if t.operand_needs_ordering_snapshot(val) {
		return t.snapshot_transformed_expr_for_reuse(val, t.node_type(val), prefix)
	}
	return val
}

// transform_select_send_ordered lowers a select send case `ch <- value` capturing the channel
// and the send value into temps (in source order) so their evaluation lands in pending_stmts
// before a later case's hoisted prelude, matching gen_select's per-case setup order.
fn (mut t Transformer) transform_select_send_ordered(infix flat.Node) flat.NodeId {
	chan_expr := t.snapshot_select_operand(t.a.child(&infix, 0), 'select_chan')
	val_expr := t.snapshot_select_operand(t.a.child(&infix, 1), 'select_send_val')
	start := t.a.children.len
	t.a.children << chan_expr
	t.a.children << val_expr
	return t.a.add_node(flat.Node{
		kind: .infix
		op: .arrow
		children_start: start
		children_count: 2
		pos: infix.pos
		typ: infix.typ
	})
}

// transform_select_recv_ordered lowers a select receive case `<-ch` capturing the channel into a
// temp so a later case's hoisted prelude cannot change the channel before it is read.
fn (mut t Transformer) transform_select_recv_ordered(prefix flat.Node) flat.NodeId {
	chan_expr := t.snapshot_select_operand(t.a.child(&prefix, 0), 'select_chan')
	start := t.a.children.len
	t.a.children << chan_expr
	return t.a.add_node(flat.Node{
		kind: .prefix
		op: .arrow
		children_start: start
		children_count: 1
		pos: prefix.pos
		typ: prefix.typ
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
		return [id]
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(int(child.kind)) {
			expanded := t.transform_stmt(child_id)
			for eid in expanded {
				new_children << eid
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	if t.rewrite_children_in_place(id, new_children) {
		return [id]
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	count := new_children.len
	new_id := t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: flat.child_count(count)
		pos: node.pos
		value: node.value
		typ: node.typ
	})
	return [new_id]
}

// transform_assert_stmt lowers a value `if`/`match` used as the assert
// condition into a temp bool (with its guard/branch prelude) before the
// assert. Without this, cgen calls `gen_expr` on the bare `if`-expression,
// which is not a C expression, and emits an empty `if (!())` condition.
fn (mut t Transformer) transform_assert_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return [id]
	}
	cond_id := t.a.child(&node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind !in [.if_expr, .match_stmt] {
		return t.transform_children_stmt(id, node)
	}
	lowered := t.transform_expr_for_type(cond_id, 'bool')
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << lowered
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind_id(int(child.kind)) {
			for eid in t.transform_stmt(child_id) {
				new_children << eid
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	if t.rewrite_children_in_place(id, new_children) {
		return [id]
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	count := new_children.len
	new_id := t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: flat.child_count(count)
		pos: node.pos
		value: node.value
		typ: node.typ
	})
	return [new_id]
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
		if t.is_stmt_kind_id(int(child.kind)) {
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
	if t.rewrite_children_in_place(id, new_children) {
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: node.pos
		value: node.value
		typ: node.typ
	})
}

fn (mut t Transformer) transform_channel_send_value(value_id flat.NodeId) flat.NodeId {
	value := t.transform_value_operand(value_id)
	mut value_type := t.node_type(value_id)
	if value_type.len == 0 {
		value_type = t.checker_node_type(value_id)
	}
	return t.clone_borrowed_projection(value_id, value, value_type)
}

// transform_value_operand transforms an operand of an infix/prefix expression,
// routing a value `match`/`if` operand (e.g. `1 + (match x { ... })` or
// `-(match x { ... })`) through `transform_expr_for_type` so its (possibly
// propagating) branch tails are lowered as values instead of in a value-less
// statement context.
fn (mut t Transformer) transform_value_operand(id flat.NodeId) flat.NodeId {
	if t.is_value_match_or_if_operand(id) {
		mut typ := t.node_type(id)
		if typ.len == 0 {
			typ = t.resolve_expr_type(id)
		}
		if typ.len > 0 && typ != 'void' {
			return t.transform_expr_for_type(id, typ)
		}
	}
	return t.transform_expr(id)
}

// materialize_value_branch_operand is transform_value_operand for a caller that rebuilds its
// node over the materialized operands and re-dispatches over the rebuilt node. That protocol
// needs the rewrite to make progress: `transform_value_operand` only materializes a branch when
// it has a usable value type, and otherwise falls back to plain `transform_expr`, which rebuilds
// the branch with the same shape under a fresh id (e.g. `f(a, b, c, if cond { .arrow } else {
// .dot })`, whose enum-shorthand arms leave the `if` untyped). The caller would then see a
// changed operand that is still a branch and re-dispatch forever, overflowing the stack. Detect
// that, drop the prelude the failed attempt queued, and return the operand unchanged so the
// caller leaves it to its ordinary operand lowering.
fn (mut t Transformer) materialize_value_branch_operand(id flat.NodeId) flat.NodeId {
	pending_mark := t.pending_stmts.len
	value := t.transform_value_operand(id)
	if value != id && t.operand_hoists_value_branch(value) {
		if t.pending_stmts.len > pending_mark {
			t.pending_stmts = t.pending_stmts[..pending_mark].clone()
		}
		return id
	}
	return value
}

// transform_infix_expr transforms transform infix expr data for transform.
fn (mut t Transformer) transform_infix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if node.op == .arrow {
		rhs_id := t.a.child(&node, 1)
		rhs := t.a.nodes[int(rhs_id)]
		if rhs.kind == .or_expr && rhs.children_count >= 2 {
			t.mark_fn_used('sync__Channel__try_push_priv')
			t.mark_fn_used('sync__Channel__closed_error')
			send_prelude_start := t.pending_stmts.len
			target_id := t.a.child(&node, 0)
			// Route a value `match`/`if` channel target through value lowering so its propagating
			// arm tail is materialized as a value temp, e.g.
			// `(match node { First { channel_first(node)! } ... }) <- 1 or { return }`; otherwise
			// it is lowered in a value-less statement context and emits an empty channel
			// expression. `transform_value_operand` is a no-op for the common non-branch targets.
			mut lhs := t.transform_value_operand(target_id)
			sent_value_id := t.a.child(&rhs, 0)
			// A send target is a channel reference handle, not an lvalue that must be written
			// through, so when the sent value hoists a value branch, snapshot the target's channel
			// value before that branch's prelude. This captures the source-order channel even if
			// the prelude reassigns a stable target
			// (`target <- (match ... { retarget(mut target)! } ...)`) or mutates a side-effecting
			// target's components (`channels[next()] <- (match ...)`). A value-branch target is
			// already materialized into a temp above.
			if t.operand_hoists_value_branch(sent_value_id)
				&& !t.is_value_match_or_if_operand(target_id) {
				lhs = t.snapshot_transformed_expr_for_reuse(lhs, t.node_type(lhs), 'chan_target')
			}
			// Route a value `match`/`if` sent value through value lowering so its propagating
			// arm tail is materialized as a value, e.g.
			// `ch <- (match node { First { get_first(node)! } ... }) or { return }`.
			// `transform_channel_send_value` is a no-op for ordinary ownership-free values.
			value := t.transform_channel_send_value(sent_value_id)
			// Detach the channel target + sent value materialization prelude so transforming the
			// `or {}` handler below does not capture it into the handler body; re-queue it
			// afterwards so it is emitted before the channel send (target index before value).
			mut send_prelude := []flat.NodeId{}
			if t.pending_stmts.len > send_prelude_start {
				send_prelude = t.pending_stmts[send_prelude_start..].clone()
				t.pending_stmts = t.pending_stmts[..send_prelude_start].clone()
			}
			saved_var_types := t.var_types.clone()
			t.set_implicit_err_var_type()
			body := t.transform_expr(t.a.child(&rhs, 1))
			t.restore_var_types(saved_var_types)
			for stmt in send_prelude {
				t.pending_stmts << stmt
			}
			or_start := t.a.children.len
			t.a.children << value
			t.a.children << body
			new_or := t.a.add_node(flat.Node{
				kind: .or_expr
				children_start: or_start
				children_count: 2
				pos: rhs.pos
				value: rhs.value
				typ: rhs.typ
			})
			start := t.a.children.len
			t.a.children << lhs
			t.a.children << new_or
			return t.a.add_node(flat.Node{
				kind: .infix
				op: .arrow
				children_start: start
				children_count: 2
				pos: node.pos
				value: node.value
				typ: node.typ
			})
		}
	}
	if node.op in [.logical_and, .logical_or] {
		return t.transform_and_chain_smartcasts(id)
	}
	if node.op == .left_shift {
		lhs_id := t.a.children[node.children_start]
		rhs_id := t.a.children[node.children_start + 1]
		mut rhs_target_type := ''
		lhs_type := t.clean_array_append_lhs_type(t.lvalue_type(lhs_id))
		if lhs_type.starts_with('[]') {
			elem_type := lhs_type[2..]
			rhs_type := t.lvalue_type(rhs_id)
			mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
			if push_many && t.array_append_rhs_is_sum_variant_value(rhs_id, rhs_type, elem_type) {
				push_many = false
			}
			if !push_many {
				rhs_target_type = elem_type
			}
		}
		// Route `match`/`if` value operands through value lowering: a numeric shift
		// such as `1 << (match x { First { get_a()! } else { get_b()! } })` leaves
		// `rhs_target_type` empty, so a propagating branch tail would otherwise be
		// lowered with plain `transform_expr` in a value-less statement context and
		// emit an empty expression. `transform_value_operand` is a no-op for the
		// common non-branch operands.
		// Preserve LHS-before-RHS evaluation order: for a numeric shift whose RHS hoists a
		// value branch — directly or nested inside a compound RHS (`mark_lhs() << (1 +
		// (match ...))`) — its materialization below queues prelude statements, so stabilize a
		// side-effecting LHS first so it runs before that prelude, e.g.
		// `mark_lhs() << (match x { ... mark_rhs()! ... })`. An array-append LHS
		// (`rhs_target_type` set) is a mutated lvalue and must not be spilled; a value-branch
		// LHS is already materialized in order by `transform_value_operand`.
		rhs_is_value_branch := t.operand_hoists_value_branch(rhs_id)
		mut new_lhs := if rhs_target_type.len == 0 && rhs_is_value_branch
			&& !t.is_value_match_or_if_operand(lhs_id) && t.operand_needs_ordering_snapshot(lhs_id) {
			t.snapshot_expr_for_reuse(lhs_id)
		} else {
			t.transform_value_operand(lhs_id)
		}
		// For an array append (`rhs_target_type` set) whose RHS is a value branch that
		// hoists a prelude, stabilize the LHS lvalue's dynamic base/index components into
		// temps first — without spilling the mutated array value — so a side-effecting
		// index (e.g. `arrays[next(mut trace)] << (match ...)`) evaluates before the RHS
		// prelude, preserving source order.
		if rhs_target_type.len > 0 && rhs_is_value_branch {
			new_lhs = t.stabilize_transformed_lvalue_for_reuse(new_lhs)
		}
		new_rhs := if rhs_target_type.len > 0 {
			t.transform_expr_for_type(rhs_id, rhs_target_type)
		} else {
			t.transform_value_operand(rhs_id)
		}
		if t.rewrite_two_children_in_place(id, new_lhs, new_rhs) {
			t.annotate_left_shift(id)
			return id
		}
		start := t.a.children.len
		t.a.children << new_lhs
		t.a.children << new_rhs
		new_id := t.a.add_node(flat.Node{
			kind: .infix
			op: node.op
			children_start: start
			children_count: 2
			pos: node.pos
			value: node.value
			typ: node.typ
		})
		t.annotate_left_shift(new_id)
		return new_id
	}
	// A value-context `match`/`if` operand (e.g. `(match x { First { get_a()! }
	// else { get_b()! } }) + suffix`) must be materialized as a value before the
	// type-specialized handlers below dispatch on operand type. Those handlers
	// (string/array/map/interface/sum/struct ops) lower their operands with plain
	// `transform_expr`, which would lower the (possibly propagating) branch tails
	// in a value-less statement context and emit an empty expression. Materialize
	// only the value-branch operand(s) into value temps here, then re-dispatch over
	// the rewritten node so every handler sees a plain, typed operand. The other
	// operand is left as its original node so it is transformed exactly once.
	infix_lhs_id := t.a.children[node.children_start]
	infix_rhs_id := t.a.children[node.children_start + 1]
	// Detect a value branch that either side hoists — directly or nested inside a compound
	// operand (`trace_left() + (1 + (match ...))`) — so the other, side-effecting operand is
	// stabilized before that operand's materialization prelude, preserving left-to-right order.
	// A directly-branch operand is materialized in order by `transform_value_operand` below; a
	// nested one is materialized by its `transform_expr` recursion.
	lhs_is_value_branch := t.operand_hoists_value_branch(infix_lhs_id)
	rhs_is_value_branch := t.operand_hoists_value_branch(infix_rhs_id)
	if lhs_is_value_branch || rhs_is_value_branch {
		// Evaluate operands left-to-right so their materialization statements land in
		// `pending_stmts` in source order (LHS before RHS). Materializing only one side
		// would emit its prelude before the other operand is evaluated — e.g.
		// `mark('L') + (match x { ... mark_result('R')! ... })` would run the RHS prelude
		// before the LHS call, reversing observable evaluation order. When one side is a
		// value branch, spill a non-stable (side-effecting) other operand to a temp first
		// so its evaluation still precedes the branch's prelude; stable operands
		// (idents/literals) are left untouched for the re-dispatch to transform once.
		pending_start := t.pending_stmts.len
		new_lhs := if lhs_is_value_branch {
			t.materialize_value_branch_operand(infix_lhs_id)
		} else if rhs_is_value_branch && t.operand_needs_ordering_snapshot(infix_lhs_id) {
			t.snapshot_expr_for_reuse(infix_lhs_id)
		} else {
			infix_lhs_id
		}
		mut lhs_pending := []flat.NodeId{}
		if t.pending_stmts.len > pending_start {
			lhs_pending = t.pending_stmts[pending_start..].clone()
			t.pending_stmts = t.pending_stmts[..pending_start].clone()
		}
		new_rhs := if rhs_is_value_branch {
			t.materialize_value_branch_operand(infix_rhs_id)
		} else if lhs_is_value_branch && !t.is_stable_expr_for_reuse(infix_rhs_id) {
			t.stable_expr_for_reuse(infix_rhs_id)
		} else {
			infix_rhs_id
		}
		if lhs_pending.len > 0 {
			rhs_pending := t.pending_stmts[pending_start..].clone()
			t.pending_stmts = t.pending_stmts[..pending_start].clone()
			for stmt in lhs_pending {
				t.pending_stmts << stmt
			}
			for stmt in rhs_pending {
				t.pending_stmts << stmt
			}
		}
		if new_lhs != infix_lhs_id || new_rhs != infix_rhs_id {
			start := t.a.children.len
			t.a.children << new_lhs
			t.a.children << new_rhs
			new_id := t.a.add_node(flat.Node{
				kind: .infix
				op: node.op
				children_start: start
				children_count: 2
				pos: node.pos
				value: node.value
				typ: node.typ
			})
			return t.transform_infix_expr(new_id, t.a.nodes[int(new_id)])
		}
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
	pending_start := t.pending_stmts.len
	preserve_pointer_values := node.op in [.eq, .ne]
	new_lhs := if t.is_value_match_or_if_operand(lhs_id) {
		t.transform_value_operand(lhs_id)
	} else if preserve_pointer_values && t.infix_operand_is_language_pointer(lhs_id) {
		t.transform_expr_preserving_pointer_value(lhs_id)
	} else {
		t.transform_expr(lhs_id)
	}
	mut lhs_pending := []flat.NodeId{}
	if t.pending_stmts.len > pending_start {
		lhs_pending = t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
	}
	new_rhs := if node.op == .arrow {
		t.transform_channel_send_value(rhs_id)
	} else if t.is_value_match_or_if_operand(rhs_id) {
		t.transform_value_operand(rhs_id)
	} else if preserve_pointer_values && t.infix_operand_is_language_pointer(rhs_id) {
		t.transform_expr_preserving_pointer_value(rhs_id)
	} else {
		t.transform_expr(rhs_id)
	}
	if lhs_pending.len > 0 {
		rhs_pending := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		for stmt in lhs_pending {
			t.pending_stmts << stmt
		}
		for stmt in rhs_pending {
			t.pending_stmts << stmt
		}
	}
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
	if t.rewrite_two_children_in_place(id, new_lhs, new_rhs) {
		return id
	}
	start := t.a.children.len
	t.a.children << new_lhs
	t.a.children << new_rhs
	return t.a.add_node(flat.Node{
		kind: .infix
		op: node.op
		children_start: start
		children_count: 2
		pos: node.pos
		value: node.value
		typ: node.typ
	})
}

// transform_call_expr transforms transform call expr data for transform.
@[direct_array_access]
fn (mut t Transformer) transform_call_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len > 0 && node.value == '__v_compile_warn' {
		return t.make_empty()
	}
	if node.value.len > 0 && node.value == '__v_compile_error' {
		t.record_selected_compile_error_call(node)
	}
	// Materialize value `match`/`if` method receivers and arguments before builtin/method
	// dispatch, so builtin lowerings (e.g. `(match ...).clone()` -> make_array_clone_call, or
	// `values.index(match ...)` -> lower_array_index_expr, which lower the receiver/needle with
	// plain `transform_expr` / `stable_expr_for_reuse`) receive plain value temps rather than
	// lowering the propagating arm tail in a value-less statement context. Rebuild the
	// selector/call over the materialized operands and re-dispatch; a no-op for the common
	// non-branch receivers/arguments.
	if node.children_count > 0 {
		recv_fn_id := t.a.children[node.children_start]
		recv_fn := t.a.nodes[int(recv_fn_id)]
		is_selector_call := recv_fn.kind == .selector && recv_fn.children_count > 0
		recv_sel_base_id := if is_selector_call {
			t.a.children[recv_fn.children_start]
		} else {
			flat.empty_node
		}
		// A module/type-qualified callee (`os.abs_path(...)`, `Type.make(...)`) is a selector
		// whose base names a compile-time namespace, not a value: it has no runtime storage to
		// stabilize or snapshot, so only its arguments take ordering treatment.
		is_namespace_call := is_selector_call
			&& t.call_selector_base_is_namespace(recv_sel_base_id, recv_fn.value, node.value)
		// A function-valued field callee (`p.callback(...)`) is a selector but not a method call:
		// the field holds a function value that an argument prelude can replace (via a
		// reference-backed holder), so it must be snapshotted whole like any other runtime callee
		// rather than treated as a method that only stabilizes its receiver.
		is_fn_field_callee := is_selector_call && !is_namespace_call
			&& t.receiver_selector_is_fn_field(t.normalize_type_alias(t.trim_pointer_type(t.lvalue_type(recv_sel_base_id))), recv_fn.value)
		is_method := is_selector_call && !is_fn_field_callee && !is_namespace_call
		recv_id := if is_method { recv_sel_base_id } else { flat.empty_node }
		// A plain (non-method) call whose callee is itself a value branch —
		// `(match node { ... make_cb(node)! ... })()` — must materialize operand 0 too;
		// otherwise transform_call_args lowers child 0 with plain transform_expr and leaves the
		// propagating branch tail in a value-less statement context, emitting an empty callee.
		callee_is_value_branch := !is_method && t.is_value_match_or_if_operand(recv_fn_id)
		// Position of the last operand that hoists a value branch (0 = method receiver or a
		// branch callee, 1.. = arguments). An argument counts even when the branch is nested
		// inside a compound expression (`1 + (match ...)`, `i64(match ...)`): lowering it still
		// materializes the inner branch into pending_stmts, so an earlier operand must be
		// stabilized to keep source order.
		mut last_branch := if (is_method && t.is_value_match_or_if_operand(recv_id))
			|| callee_is_value_branch {
			0
		} else {
			-1
		}
		for i in 1 .. node.children_count {
			if t.operand_hoists_value_branch(t.a.child(&node, i)) {
				last_branch = i
			}
		}
		if last_branch >= 0 {
			// Evaluate operands in source order (method receiver, then arguments). A value branch
			// is materialized into a value temp; a non-stable operand that precedes a later branch
			// is stabilized first so its side effects run before that branch's hoisted prelude.
			// Stabilization preserves an lvalue's identity (spilling only its dynamic base/index
			// components, so `mut`/mutable operands still mutate through, e.g.
			// `items[next()].update(match ...)` or `apply(mut items[next()], match ...)`); an rvalue
			// is spilled by value. Applies to method calls and plain function calls alike. Stable
			// operands are left for the re-dispatch to transform once.
			mut changed := false
			mut new_fn_id := recv_fn_id
			if is_method {
				new_recv := if t.is_value_match_or_if_operand(recv_id) {
					r := t.materialize_value_branch_operand(recv_id)
					if r != recv_id {
						changed = true
					}
					r
				} else if last_branch > 0 && t.operand_needs_ordering_snapshot(recv_id)
					&& !t.callee_base_is_not_a_runtime_value(recv_id) {
					// A `mut`/reference receiver keeps its lvalue identity (only its dynamic
					// base/index components are spilled) so the call still mutates through the
					// lvalue. An ordinary by-value receiver is spilled by value, so its value is
					// read in source order — a later branch prelude that mutates its container
					// (e.g. `items[next()].read(match ... { mutate(mut items)! } ...)`) cannot
					// then change the observed receiver value.
					r := if t.method_receiver_is_reference(recv_id, recv_fn.value) {
						if stabilized := t.stabilize_original_lvalue_receiver(recv_id) {
							stabilized
						} else {
							t.snapshot_expr_for_reuse(recv_id)
						}
					} else {
						t.snapshot_expr_for_reuse(recv_id)
					}
					if r != recv_id {
						changed = true
					}
					r
				} else {
					recv_id
				}
				sel_start := t.a.children.len
				t.a.children << new_recv
				for i in 1 .. recv_fn.children_count {
					t.a.children << t.a.child(&recv_fn, i)
				}
				new_fn_id = t.a.add_node(flat.Node{
					kind: .selector
					op: recv_fn.op
					value: recv_fn.value
					typ: recv_fn.typ
					children_start: sel_start
					children_count: recv_fn.children_count
					pos: recv_fn.pos
				})
			} else if callee_is_value_branch {
				r := t.materialize_value_branch_operand(recv_fn_id)
				if r != recv_fn_id {
					new_fn_id = r
					changed = true
				}
			} else if last_branch > 0 && !is_namespace_call
				&& t.callee_needs_ordering_snapshot(recv_fn_id) {
				// A non-method runtime callee (make_cb(mut trace)(match ...), or a function-valued
				// variable a branch could reassign) must evaluate before a later branch argument's
				// hoisted prelude, so snapshot it in source order.
				r := t.snapshot_expr_for_reuse(recv_fn_id)
				if r != recv_fn_id {
					new_fn_id = r
					changed = true
				}
			}
			mut new_args := []flat.NodeId{cap: int(node.children_count)}
			for i in 1 .. node.children_count {
				arg_id := t.a.child(&node, i)
				na := if t.is_value_match_or_if_operand(arg_id) {
					t.materialize_value_branch_operand(arg_id)
				} else if i < last_branch && t.operand_needs_ordering_snapshot(arg_id) {
					// A `mut` argument keeps its lvalue identity (only its dynamic base/index
					// components are spilled) so it still mutates through. An ordinary argument is
					// spilled by value, so its value is read in source order — a later branch
					// prelude that mutates its container cannot change the observed value.
					if t.a.nodes[int(arg_id)].is_mut {
						if stabilized := t.stabilize_original_lvalue_receiver(arg_id) {
							stabilized
						} else {
							t.snapshot_expr_for_reuse(arg_id)
						}
					} else {
						t.snapshot_expr_for_reuse(arg_id)
					}
				} else {
					arg_id
				}
				if na != arg_id {
					changed = true
				}
				new_args << na
			}
			if changed {
				call_start := t.a.children.len
				t.a.children << new_fn_id
				for a in new_args {
					t.a.children << a
				}
				new_call_id := t.a.add_node(flat.Node{
					kind: .call
					op: node.op
					value: node.value
					typ: node.typ
					children_start: call_start
					children_count: node.children_count
					pos: node.pos
				})
				return t.transform_call_expr(new_call_id, t.a.nodes[int(new_call_id)])
			}
		}
	}
	if lowered := t.try_lower_bound_method_array_call(node) {
		return lowered
	}
	implicit_receiver_call_id := t.normalize_implicit_receiver_generic_call(id, node)
	implicit_receiver_call := t.a.nodes[int(implicit_receiver_call_id)]
	call_id := t.normalize_generic_call_expr(implicit_receiver_call_id, implicit_receiver_call)
	mut call_node := t.a.nodes[int(call_id)]
	mut resolved_typ := t.concrete_generic_call_return_type(call_id, call_node)
	if resolved_typ.len > 0 && t.rewrite_contextual_generic_plain_call(call_id, call_node) {
		call_node = t.a.nodes[int(call_id)]
	}
	if resolved_typ.len == 0 {
		if array_typ := t.array_call_type_name(call_id, call_node) {
			resolved_typ = array_typ
		}
	}
	if resolved_typ.len == 0 {
		resolved_typ = t.new_map_call_type(call_node)
	}
	if resolved_typ.len == 0 {
		if ret := t.checker_resolved_non_builtin_return_type(call_id, call_node) {
			resolved_typ = ret
		} else {
			resolved_typ = t.get_call_return_type(call_id, call_node)
		}
	}
	if resolved_typ.len == 0 {
		resolved_typ = t.current_call_return_type(call_node)
	}
	if resolved_typ.len == 0 && call_node.typ.len > 0 {
		call_typ := t.normalize_type_alias(call_node.typ)
		if call_typ !in ['array', 'map', 'unknown'] {
			resolved_typ = call_typ
		}
	}
	if fn_value_type := t.checker_call_fn_value_return_type(call_id) {
		resolved_typ = fn_value_type
	}
	if resolved_typ.len > 0 {
		t.set_node_typ(int(call_id), resolved_typ)
		call_node.typ = resolved_typ
	}
	sum_constructor_type := t.sum_constructor_call_type(call_node)
	if sum_constructor_type.len > 0 && call_node.children_count == 2 {
		return t.wrap_sum_value(t.a.child(&call_node, 1), sum_constructor_type)
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
	if !t.validate_specialized_plain_generic_call_target(call_id, call_node) {
		return t.make_empty()
	}
	return t.transform_call_args(call_id, call_node)
}

fn (mut t Transformer) validate_specialized_plain_generic_call_target(id flat.NodeId, node flat.Node) bool {
	if !t.validating_generic_spec || node.children_count == 0 {
		return true
	}
	explicit := t.explicit_generic_call_args(node, t.cur_module) or { return true }
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value.len == 0 || t.is_known_fn_name(callee.value)
		|| t.is_known_type_name(callee.value) {
		return true
	}
	decls := t.cached_generic_fn_decls()
	if _ := t.generic_call_decl_key(id, node, t.cur_module, decls) {
		return true
	}
	display := if explicit.len > 0 {
		'${callee.value}[${explicit.join(', ')}]'
	} else {
		callee.value
	}
	t.record_monomorph_error('unknown function `${display}`')
	return false
}

fn (mut t Transformer) try_lower_bound_method_array_call(node flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := t.a.child(&node, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .index || callee.children_count < 2 {
		return none
	}
	base_id := t.a.child(&callee, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind != .ident {
		return none
	}
	info := t.bound_method_arrays[t.bound_method_array_key(base.value)] or { return none }
	index_id := t.a.child(&callee, 1)
	encoded := t.make_index(t.transform_expr(base_id), t.transform_expr(index_id), info.fn_type)
	as_usize := t.make_cast('usize', encoded, 'usize')
	receiver := t.make_cast(info.receiver_type, as_usize, info.receiver_type)
	selector := t.make_selector(receiver, info.method, '')
	mut args := []flat.NodeId{cap: int(node.children_count) - 1}
	for i in 1 .. node.children_count {
		args << t.a.child(&node, i)
	}
	call_id := t.make_call_expr_typed(selector, args, info.return_type)
	call_node := t.a.nodes[int(call_id)]
	if lowered := t.try_lower_receiver_method_call(call_id, call_node) {
		return lowered
	}
	return t.transform_call_args(call_id, call_node)
}

fn (t &Transformer) bound_method_array_expr_info(id flat.NodeId) ?BoundMethodArrayInfo {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if info := t.bound_method_arrays[t.bound_method_array_key(node.value)] {
			return info
		}
		return none
	}
	if node.kind == .paren && node.children_count > 0 {
		return t.bound_method_array_expr_info(t.a.child(&node, 0))
	}
	return none
}

fn (t &Transformer) bound_method_array_key(name string) string {
	return '${t.cur_file}|${t.cur_module}|${t.cur_fn_name}|${name}'
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
	if node.value == 'struct' || transform_is_anonymous_struct_name(node.value) {
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
			if clean_value == 'string' || clean_value == 'bool'
				|| clean_value in ['f32', 'f64', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize',
					'u8', 'byte', 'u16', 'u32', 'u64', 'rune', 'char'] {
				return t.zero_value_for_type(clean_value)
			}
			if default_sum := t.make_default_sum_value(clean_value) {
				return default_sum
			}
		}
		if clean_value.starts_with('[]') {
			array_node := flat.Node{
				kind: .array_init
				op: node.op
				children_start: node.children_start
				children_count: node.children_count
				pos: node.pos
				value: clean_value[2..]
				typ: clean_value
			}
			lowered := t.lower_array_init_to_runtime(id, array_node)
			if lowered != id {
				return lowered
			}
		}
		if clean_value.starts_with('map[') {
			map_node := flat.Node{
				kind: .map_init
				op: node.op
				children_start: node.children_start
				children_count: node.children_count
				pos: node.pos
				value: clean_value
				typ: clean_value
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
			if t.is_lowered_optional_struct_init(node) {
				return id
			}
			payload_type := t.optional_base_type(optional_target)
			if t.is_fixed_array_type(payload_type) {
				if node.children_count == 0 {
					return t.make_optional_none(optional_target)
				}
				mut payload_node := node
				payload_node.value = payload_type
				payload_node.typ = payload_type
				if payload := t.transform_fixed_array_init_expr(payload_node) {
					return t.make_optional_some(payload, optional_target)
				}
			}
			_ := t.lookup_struct_info(payload_type) or {
				return t.transform_struct_fields(id, node)
			}
			if node.children_count == 0 {
				return t.make_optional_none(optional_target)
			}
			payload_node := flat.Node{
				kind: .struct_init
				op: node.op
				children_start: node.children_start
				children_count: node.children_count
				pos: node.pos
				value: payload_type
				typ: payload_type
			}
			payload := t.transform_struct_fields(id, payload_node)
			return t.make_optional_some(payload, optional_target)
		}
		if node.typ.len == 0 && clean_value != node.value && clean_value.contains('[') {
			mut concrete := node
			concrete.typ = clean_value
			return t.transform_struct_fields(id, concrete)
		}
	}
	if !node.value.contains('[') && !node.value.starts_with('main.') {
		checker_type := t.raw_checker_node_type(id)
		if checker_type.len > 0 && checker_type != node.value
			&& checker_type.all_after_last('.') == node.value.all_after_last('.')
			&& t.struct_lookup_name(checker_type).len > 0 {
			t.record_refined_node_type(int(id), checker_type)
			t.set_node_typ(int(id), checker_type)
			mut concrete := node
			concrete.value = checker_type
			concrete.typ = checker_type
			return t.transform_struct_fields(id, concrete)
		}
		if t.expected_expr_node == int(id) && t.expected_expr_type.contains('[')
			&& t.expected_expr_type.all_before('[').all_after_last('.') == node.value.all_after_last('.') {
			mut concrete := node
			concrete.value = t.expected_expr_type
			concrete.typ = t.expected_expr_type
			return t.transform_struct_fields(id, concrete)
		}
		if checker_type.contains('[')
			&& checker_type.all_before('[').all_after_last('.') == node.value.all_after_last('.') {
			t.record_refined_node_type(int(id), checker_type)
			t.set_node_typ(int(id), checker_type)
			mut concrete := node
			concrete.value = checker_type
			concrete.typ = checker_type
			return t.transform_struct_fields(id, concrete)
		}
		if inferred_type := t.infer_bare_generic_struct_init_type(node) {
			t.record_refined_node_type(int(id), inferred_type)
			t.set_node_typ(int(id), inferred_type)
			mut concrete := node
			concrete.value = inferred_type
			concrete.typ = inferred_type
			return t.transform_struct_fields(id, concrete)
		}
	}
	return t.transform_struct_fields(id, node)
}

fn (t &Transformer) is_lowered_optional_struct_init(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	first := t.a.child_node(&node, 0)
	return first.kind == .field_init && first.value == 'ok' && !first.pos.is_valid()
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
	idx_child := t.a.child(&node, 1)
	// Route value `match`/`if` operands through value lowering: `stable_expr_for_reuse`
	// lowers via plain `transform_expr`, which would lower a propagating branch tail in a
	// value-less statement context and emit an empty expression, e.g.
	// `values#[match n { First { get_index()! } else { other_index()! } }]`.
	// `transform_value_operand` materializes such an operand into a value temp (already
	// stable for the multiple uses below); non-branch operands keep `stable_expr_for_reuse`.
	// The base is evaluated before the index: if the index hoists a value branch whose prelude
	// can reassign a syntactically stable base (`values#[match n { First { replace(mut values)!
	// } ... }]`), snapshot the base's source-order value so the gated access reads it before
	// that prelude.
	base := if t.is_value_match_or_if_operand(base_child) {
		t.transform_value_operand(base_child)
	} else if t.operand_hoists_value_branch(idx_child)
		&& t.operand_needs_ordering_snapshot(base_child) {
		t.snapshot_expr_for_reuse(base_child)
	} else {
		t.stable_expr_for_reuse(base_child)
	}
	idx := if t.is_value_match_or_if_operand(idx_child) {
		t.transform_value_operand(idx_child)
	} else {
		t.stable_expr_for_reuse(idx_child)
	}
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
	then_block := t.make_block([t.make_expr_stmt(wrapped)])
	else_block := t.make_block([t.make_expr_stmt(idx)])
	if_start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	pos_expr := t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: if_start
		children_count: 3
		typ: 'int'
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
	full_key := t.expr_key(id)
	if full_key.len > 0 {
		contexts := t.smartcasts_for(full_key)
		if contexts.len > 0 {
			plain := t.make_plain_expr_for_smartcast(id)
			return t.apply_smartcast_contexts(plain, t.original_expr_type(id), contexts)
		}
	}
	if lowered := t.try_lower_map_index_expr(id, node) {
		return lowered
	}
	if lowered := t.lower_gated_scalar_index(node) {
		return t.lower_owned_array_index_move(id, lowered)
	}
	// A later child (index / slice bound) that hoists a value `match`/`if` — directly or
	// nested inside a compound child (`make_values(mut tr)[1 + (match n { ... })]`) — lifts
	// its propagation prelude into `pending_stmts`; a preceding side-effecting child left
	// inline would then run after that prelude. Find the last hoisting child so earlier
	// children can be stabilized first, preserving left-to-right evaluation order, e.g.
	// `make_values(mut tr)[match n { ... tr.index_result()! ... }]`. Index reads only
	// reach here (`.index`); lvalue targets are the separate `.index_assign` kind.
	mut last_value_branch := -1
	for i in 0 .. node.children_count {
		if t.operand_hoists_value_branch(t.a.child(&node, i)) {
			last_value_branch = i
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	mut changed := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		// route a value `match`/`if` operand (e.g. `values[match x { ... }]`)
		// through value lowering so its propagating arms are lowered as values;
		// stabilize an earlier side-effecting operand before a later hoisting one.
		// The index base (`i == 0`) keeps master's dedicated base lowering.
		mut new_child := if i == 0 {
			t.transform_index_base_expr(child_id)
		} else if i < last_value_branch && !t.is_value_match_or_if_operand(child_id)
			&& t.operand_needs_ordering_snapshot(child_id) {
			t.snapshot_expr_for_reuse(child_id)
		} else {
			t.transform_value_operand(child_id)
		}
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
	if elem_type == 'u8' || (node.value == 'range' && elem_type.len > 0)
		|| (index_typ.len == 0 && elem_type.len > 0) {
		index_typ = elem_type
	} else if index_typ.len == 0 && node.value.len > 0 {
		index_typ = node.value
	}
	if !changed {
		if index_typ.len > 0 {
			t.set_node_typ(int(id), index_typ)
		}
		return t.lower_owned_array_index_move(id, id)
	}
	if t.rewrite_children_in_place(id, new_children) {
		if index_typ.len > 0 {
			t.set_node_typ(int(id), index_typ)
		}
		return t.lower_owned_array_index_move(id, id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind: .index
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: index_typ
	})
	return t.lower_owned_array_index_move(id, new_id)
}

fn (mut t Transformer) transform_index_base_expr(id flat.NodeId) flat.NodeId {
	node := t.a.node(id)
	if node.kind == .prefix && node.op == .mul && node.children_count == 1 {
		inner_id := t.a.child(node, 0)
		inner := t.a.node(inner_id)
		if inner.kind == .ident && t.mut_param_values[inner.value]
			&& t.pointer_value_rvalues[inner.value] {
			return inner_id
		}
	}
	if node.kind == .ident && t.mut_param_values[node.value] && t.pointer_value_rvalues[node.value] {
		// `mut value &T` is a pointer-valued parameter with an implicit
		// dereference only for bare value reads/writes. As an index base (`buf[i]`)
		// it must remain `T*`, matching selector-base handling.
		old_in_selector_base := t.in_selector_base
		t.in_selector_base = true
		transformed := t.transform_expr(id)
		t.in_selector_base = old_in_selector_base
		return transformed
	}
	return t.transform_expr(id)
}

// lower_owned_array_index_move materializes an indexed value that ownership analysis
// consumed, then clears the source slot so the enclosing container cannot destroy it again.
fn (mut t Transformer) lower_owned_array_index_move(source_id flat.NodeId, index_id flat.NodeId) flat.NodeId {
	if isnil(t.tc) || !t.tc.ownership_index_read_moves_value(source_id) || int(index_id) < 0 {
		return index_id
	}
	index_node := t.a.nodes[int(index_id)]
	if index_node.kind != .index || index_node.value == 'range' || index_node.children_count < 2 {
		return index_id
	}
	base_id := t.a.child(&index_node, 0)
	index_value_id := t.a.child(&index_node, 1)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	is_pointer := base_type.starts_with('&')
	clean_base_type := t.normalize_type_alias(base_type.trim_left('&'))
	if !clean_base_type.starts_with('[]') && !t.is_fixed_array_type(clean_base_type) {
		return index_id
	}
	elem_type := t.node_type(index_id)
	if elem_type.len == 0 || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type)) {
		return index_id
	}
	source_is_owned_temporary := !is_pointer && !t.expr_can_take_address(base_id)
	stable_base := t.stable_transformed_expr_for_reuse(base_id, base_type, 'owned_index_array_source')
	mut array_value := stable_base
	if is_pointer {
		array_value = t.make_prefix(.mul, stable_base)
		t.set_node_typ(int(array_value), clean_base_type)
	}
	index_value := t.stable_transformed_expr_for_reuse(index_value_id, 'int', 'owned_index_array_index')
	slot := t.make_index(array_value, index_value, elem_type)
	result_name := t.new_temp('owned_index_value')
	t.pending_stmts << t.make_decl_assign_typed(result_name, slot, elem_type)
	t.pending_stmts << t.make_index_assign(t.make_index(array_value, index_value, elem_type), t.zero_value_for_type(elem_type))
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			array_value,
		], 'void'))
	}
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), elem_type)
	return result
}

// transform_string_interp transforms transform string interp data for transform.
fn (mut t Transformer) transform_string_interp(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_string_literal('')
	}
	if t.string_interp_has_unresolved_generic_part(node) {
		return id
	}
	if nested_match := t.transform_nested_match_string_interp_node(node) {
		return nested_match
	}
	if nested_if := t.transform_nested_if_string_interp_node(node) {
		return nested_if
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
	if node.kind == .directive && node.value == 'string_interp_format' {
		if node.children_count == 0 {
			return false
		}
		return t.string_interp_child_has_unresolved_generic_part(t.a.child(&node, 0))
	}
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
			name: field.name
			typ: field_typ
			raw_typ: field.raw_typ
			default_expr: field.default_expr
		}
	}
	t.structs[clean] = StructInfo{
		name: clean
		module: base_info.module
		is_params: base_info.is_params
		fields: fields
	}
	// A later unqualified lookup must include this newly materialized generic
	// struct, so use the exact legacy scan after the collected index changes.
	t.struct_short_name_index_ready = false
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
			name: field.name
			typ: field_typ
			raw_typ: field_typ
			default_expr: field.default_expr
			is_embedded: field.is_embedded
		}
	}
	return StructInfo{
		name: clean
		module: base_info.module
		is_params: base_info.is_params
		fields: fields
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

// borrow_first_last_accessor lowers a `first()`/`last()` array accessor used as a borrow
// (e.g. the base of a field selector `arr.last().field`) into an in-place element access
// `arr[0]` / `arr[len - 1]`. The stored element stays owned by the array, so this avoids
// the independent-clone path `first()`/`last()` otherwise takes for ownership-bearing
// element types — a path that has no valid lowering when the element has no `clone()`
// method and would otherwise emit an empty placeholder (`(0)`). Restricted to owned elements
// so non-owned elements keep their existing accessor lowering, and gated on the checker's
// `array_accessor_result_is_borrowed` predicate so the two stay in lock-step: a bound method
// value or a selector chain whose final value owns data is not borrowed here, matching the
// checker's suppressed diagnostic. Returns none when the base is not such an accessor.
fn (mut t Transformer) borrow_first_last_accessor(call_id flat.NodeId) ?flat.NodeId {
	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len || isnil(t.tc) {
		return none
	}
	mut node_id := call_id
	mut node := t.a.nodes[int(call_id)]
	// `(arr.last()).field` is the same borrow as `arr.last().field`; unwrap transparent
	// parentheses so the accessor is matched (the checker's predicate does the same).
	for node.kind == .paren && node.children_count > 0 {
		node_id = t.a.child(&node, 0)
		node = t.a.nodes[int(node_id)]
	}
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value !in ['first', 'last'] || callee.children_count == 0 {
		return none
	}
	base_id := t.a.child(callee, 0)
	base_type := t.normalize_type_alias(t.lvalue_type(base_id))
	clean_base_type := base_type.trim_left('&')
	if !clean_base_type.starts_with('[]') {
		return none
	}
	elem_type := clean_base_type[2..]
	if !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type)) {
		return none
	}
	// Defer to the checker's borrowed-field predicate so the two stay in lock-step: a bound
	// method value (`arr.last().method`) or a chain whose final value owns data
	// (`arr.last().name`) must keep the copying accessor semantics rather than borrow the
	// live array element in place — otherwise an owned field could escape aliasing freed
	// storage. Only reached for owned elements, so the walk stays off the default path.
	if !t.tc.array_accessor_result_is_borrowed(node_id) {
		return none
	}
	mut base := t.transform_lvalue(base_id)
	if base_type.starts_with('&') {
		base = t.make_prefix(.mul, base)
		t.set_node_typ(int(base), clean_base_type)
	}
	if callee.value == 'last' {
		// `last()` reads the base twice (`arr[arr.len - 1]`), so it must evaluate once.
		// A plain lvalue (`t.entries`) is left untouched; a non-lvalue receiver such as
		// `make_entries()` is bound to a temp instead of being duplicated.
		base = t.stable_transformed_expr_for_reuse(base, clean_base_type, 'first_last_borrow_base')
	}
	index := if callee.value == 'first' {
		t.make_int_literal(0)
	} else {
		t.make_infix(.minus, t.make_selector(base, 'len', 'int'), t.make_int_literal(1))
	}
	return t.make_index(base, index, elem_type)
}

fn (mut t Transformer) transform_selector_base_expr(id flat.NodeId) flat.NodeId {
	// A `first()`/`last()` accessor used as a selector base (`arr.last().field`) borrows
	// the stored element rather than returning an independent copy; lower it in place so
	// ownership-bearing element types without a `clone()` method still resolve.
	if borrowed := t.borrow_first_last_accessor(id) {
		return borrowed
	}
	// `in_selector_base` suppresses the pointer-value rvalue deref in
	// transform_ident_expr, but that must apply only to the *direct* receiver ident of
	// a selector (`x.field`, where `x` stays `&T` so the selector emits arrow access).
	// A compound base such as a call (`wrap(x).field`) or index expr may contain nested
	// idents (e.g. the call argument `x`) that still need their rvalue deref, so only
	// engage the flag when the base is a plain ident — including one wrapped in
	// transparent parentheses (`(x).field`, `((x)).field`), where `x` is still the
	// direct receiver.
	if !t.selector_base_is_ident_receiver(id) {
		// route a value `match`/`if` receiver (e.g. `(match x { ... }).field`)
		// through its target type so its propagating arms are lowered as values.
		return t.transform_value_operand(id)
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

fn (t &Transformer) selector_base_is_explicit_as_expr(id flat.NodeId) bool {
	mut node := t.a.nodes[int(id)]
	for node.kind == .paren && node.children_count > 0 {
		node = t.a.nodes[int(t.a.child(&node, 0))]
	}
	return node.kind == .as_expr
}

fn (t &Transformer) transformed_selector_type(node flat.Node) string {
	resolved := t.resolve_selector_type(node)
	if resolved.starts_with('&') && !node.typ.starts_with('&') {
		return resolved
	}
	if resolved.len > 0 && node.children_count > 0 {
		base_type := t.trim_pointer_type(t.resolve_expr_type(t.a.child(&node, 0)))
		if _ := t.lookup_struct_field_type(base_type, node.value) {
			// Field declarations are resolved in their owning module. Prefer that
			// canonical type over the selector's source spelling: an imported
			// `other.Holder.typ Type` must not bind `Type` to a colliding type in
			// the caller's module while the selector is lowered.
			return resolved
		}
	}
	return if node.typ.len > 0 { node.typ } else { resolved }
}

fn (mut t Transformer) comptime_type_expr_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .array_init && node.value == '__v3_comptime_type_array'
		&& node.children_count == 1 {
		elem := t.comptime_type_expr_type(t.a.child(&node, 0)) or { return none }
		return '[]${elem}'
	}
	if node.kind == .typeof_expr {
		typ := t.typeof_type_name(node)
		if typ.len > 0 && !typ.contains('unknown') {
			return typ
		}
		return none
	}
	if node.kind == .ident {
		typ := t.raw_var_type(node.value)
		if typ.len > 0 && !t.generic_arg_is_unresolved(typ) {
			return typ
		}
		if node.value.len > 0 {
			return t.normalize_type_in_module(node.value, t.cur_module)
		}
		return none
	}
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := t.comptime_type_expr_type(t.a.child(&node, 0)) or { return none }
	return match node.value {
		'typ' {
			base
		}
		'unaliased_typ' {
			t.comptime_normalize_type_alias_chain(base)
		}
		'payload_type', 'pointee_type', 'element_type', 'key_type', 'value_type' {
			t.generic_comptime_type_member(base, node.value)
		}
		else {
			none
		}
	}
}

fn (t &Transformer) selector_base_is_comptime_type_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .typeof_expr {
		return true
	}
	if node.kind == .array_init && node.value == '__v3_comptime_type_array' {
		return true
	}
	if node.kind == .ident {
		return node.value in t.active_generic_params
			|| node.value in t.cloning_comptime_for_vars
			|| (t.cur_fn_is_generic && is_generic_fn_placeholder_name(node.value))
	}
	if node.kind == .selector && node.children_count > 0
		&& node.value in ['typ', 'unaliased_typ', 'payload_type', 'pointee_type', 'element_type',
			'key_type', 'value_type'] {
		return t.selector_base_is_comptime_type_value(t.a.child(&node, 0))
	}
	return false
}

fn (mut t Transformer) owned_method_receiver_clone_helper(site flat.NodeId, typ string) string {
	name := '__v3_method_receiver_clone_${int(site)}'
	generated_module := t.current_source_module()
	module_name := if generated_module.len > 0 { generated_module } else { 'main' }
	qname := if module_name !in ['main', 'builtin'] { '${module_name}.${name}' } else { name }
	if qname in t.fn_ret_types || name in t.fn_ret_types {
		return qname
	}
	saved_pending := t.pending_stmts
	saved_vars := t.var_types.clone()
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	t.pending_stmts = []flat.NodeId{}
	t.reset_var_types()
	t.cur_fn_name = qname
	t.cur_fn_ret_type = typ
	param_name := '__method_receiver'
	param_type := 'voidptr'
	param := t.a.add_node(flat.Node{
		kind: .param
		value: param_name
		typ: param_type
	})
	t.set_var_type(param_name, param_type)
	typed_pointer := t.make_cast('&${typ}', t.make_ident(param_name), '&${typ}')
	source := t.make_prefix(.mul, typed_pointer)
	t.set_node_typ(int(source), typ)
	cloned := t.make_compiler_default_clone_value(source, typ, true)
	mut body := t.pending_stmts.clone()
	body << t.make_return(cloned, typ)
	t.pending_stmts = saved_pending
	t.restore_var_types(saved_vars)
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	t.add_generated_fn_decl_context(module_name)
	start := t.a.children.len
	t.a.children << param
	t.a.children << body
	fn_decl := t.a.add_node(flat.Node{
		kind: .fn_decl
		value: name
		typ: typ
		children_start: i32(start)
		children_count: flat.child_count(1 + body.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, module_name, t.cur_file)
	t.set_fn_ret_type(name, typ)
	t.set_fn_ret_type(qname, typ)
	t.mark_fn_used_name(qname)
	return qname
}

// transform_selector_expr transforms transform selector expr data for transform.
fn (mut t Transformer) transform_selector_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	// Smartcast payload selectors are already fully lowered and carry a marker
	// identifying the concrete variant. Reprocessing one such as `sum.i32`
	// can mistake the storage field for a same-named receiver method and turn
	// the value access into a bound-method closure.
	if _ := t.generated_variant_access_type(id) {
		return id
	}
	if node.value in t.sum_variant_fields {
		return id
	}
	base_id0 := t.a.child(&node, 0)
	if node.value == 'typ' && t.selector_base_is_comptime_type_value(base_id0) {
		if base_type := t.comptime_type_expr_type(base_id0) {
			return t.make_int_literal(t.comptime_field_type_id(base_type, t.cur_module))
		}
	}
	if node.value in ['payload_type', 'pointee_type', 'element_type', 'key_type', 'value_type']
		&& t.selector_base_is_comptime_type_value(base_id0) {
		if base_type := t.comptime_type_expr_type(base_id0) {
			if member_type := t.generic_comptime_type_member(base_type, node.value) {
				return t.make_int_literal(t.comptime_field_type_id(member_type, t.cur_module))
			}
		}
	}
	if node.value == 'variant_types' {
		if base_type := t.comptime_type_expr_type(base_id0) {
			resolved := t.resolve_sum_name(base_type)
			if variants := t.sum_types[resolved] {
				mut ids := []flat.NodeId{cap: variants.len}
				for variant in variants {
					ids << t.make_int_literal(t.comptime_field_type_id(variant, t.cur_module))
				}
				return t.make_array_literal_typed(ids, '[]int')
			}
		}
	}
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
			return t.transform_typeof_name_expr(base_id0, base_node0)
		}
		if node.value == 'idx' {
			return t.transform_typeof_idx_expr(base_node0)
		}
		if node.value == 'indirections' {
			return t.make_int_literal(generic_type_indirections(t.typeof_type_name(base_node0)))
		}
		if node.value == 'unaliased_typ' {
			base_type := t.typeof_type_name(base_node0)
			unaliased := t.comptime_normalize_type_alias_chain(base_type)
			return t.make_int_literal(t.comptime_field_type_id(unaliased, t.cur_module))
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
	if !t.selector_base_is_explicit_as_expr(base_id) && sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		if contexts.len > 0 {
			plain_base := t.make_plain_expr_for_smartcast(base_id)
			variant_sel := t.apply_smartcast_contexts(plain_base, t.original_expr_type(base_id), contexts)
			variant_type := t.node_type(variant_sel)
			if shared_typ := t.sum_shared_field_type_name(variant_type, node.value) {
				return t.lower_sum_shared_field_selector(variant_sel, variant_type, node.value, shared_typ)
			}
			sel_start := t.a.children.len
			t.a.children << variant_sel
			clean_variant_type := t.trim_pointer_type(variant_type)
			sel_typ := if ftyp := t.lookup_struct_field_type(clean_variant_type, node.value) {
				ftyp
			} else {
				t.transformed_selector_type(node)
			}
			return t.a.add_node(flat.Node{
				kind: .selector
				op: if node.op == .arrow || variant_type.starts_with('&') {
					flat.Op.arrow
				} else {
					flat.Op.dot
				}
				children_start: sel_start
				children_count: 1
				pos: node.pos
				value: node.value
				typ: sel_typ
			})
		}
	}
	base_type0 := t.node_type(base_id)
	iface_name := t.resolve_interface_type_name(base_type0)
	if iface_name.len > 0 && node.value !in ['_typ', '_object'] {
		if field_type := t.interface_field_type_name(iface_name, node.value) {
			transformed_base := t.transform_selector_base_expr(base_id)
			base := t.stable_transformed_expr_for_reuse(transformed_base, base_type0, 'iface_field')
			return t.lower_interface_field_selector(base, base_type0, iface_name, node.value, field_type)
		}
	}
	base_clean := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if info := t.lookup_struct_info(base_clean) {
		has_direct_field := (t.struct_field_type(info, node.value) or { '' }).len > 0
		if !has_direct_field {
			if embedded := t.embedded_field_for_direct_selector(info, node.value) {
				new_base := t.transform_selector_base_expr(base_id)
				embedded_op := if base_type0.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				return t.make_selector_op(new_base, embedded.name, embedded.typ, embedded_op)
			}
			if embedded := t.embedded_field_for_promoted_field(info, node.value) {
				new_base := t.transform_selector_base_expr(base_id)
				embedded_op := if base_type0.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				embedded_sel := t.make_selector_op(new_base, embedded.name, embedded.typ, embedded_op)
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
				return t.lower_sum_shared_field_selector(new_base, transformed_base_type, node.value, new_shared_typ)
			}
		}
		new_base := t.selector_base_for_field(transformed_base, base_type0)
		return t.lower_sum_shared_field_selector(new_base, base_type0, node.value, shared_typ)
	}
	mut new_base := t.transform_selector_base_expr(base_id)
	mut selector_generic_params := node.generic_params().clone()
	if !isnil(t.tc) && t.tc.expr_is_method_value(id) {
		method_value_name := t.resolve_receiver_method_name(new_base, node.value)
		method_params := t.call_param_types(method_value_name)
		if method_params.len > 0 && method_params[0] !is types.Pointer {
			receiver_type_name := t.node_type(new_base)
			receiver_type := t.tc.parse_type(receiver_type_name)
			if t.tc.ownership_type_requires_destruction(receiver_type)
				&& t.tc.ownership_default_clone_missing_method(receiver_type) == none {
				clone_helper := t.owned_method_receiver_clone_helper(id, receiver_type_name)
				selector_generic_params << flat.method_value_clone_receiver_marker_prefix + clone_helper
			}
		}
	}
	method_name := if t.validating_generic_spec && t.is_fn_pointer_type_name(t.expected_expr_type) {
		t.resolve_receiver_method_name(new_base, node.value)
	} else {
		''
	}
	if method_name.len > 0 {
		// Generic receiver selectors can become concrete method values only while
		// their specialized body is transformed, after the initial markused pass.
		// Root that concrete method here so the late-body scan emits its declaration.
		t.mark_fn_used_name(method_name)
	}
	mut changed := new_base != base_id || selector_generic_params != node.generic_params()
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
	if sel_op == node.op && selector_generic_params == node.generic_params()
		&& t.rewrite_children_in_place(id, new_children) {
		t.set_node_typ(int(id), sel_typ)
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind: .selector
		op: sel_op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: sel_typ
		payload: flat.node_payload(selector_generic_params)
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
		kind: .selector
		op: sel_op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: sel_typ
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
				kind: .index
				op: node.op
				children_start: start
				children_count: node.children_count
				pos: node.pos
				value: node.value
				typ: if node.typ.len > 0 { node.typ } else { node.value }
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
	mut resolved_sum := t.resolve_sum_name(clean_sum)
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
	mut sum_candidate := clean_sum
	if t.active_specialization_args.len > 0 {
		sum_candidate = t.subst_type(sum_candidate, t.active_specialization_args)
	}
	if resolved := t.resolve_sum_name_from_c_name(sum_candidate) {
		sum_candidate = resolved
	}
	variants := t.concrete_sum_variants_for_candidate(sum_candidate)
	if variants.len == 0 {
		return none
	}
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
	if ftyp := t.checker_struct_field_type_name(variant, field) {
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
	mut resolved_sum := t.resolve_sum_name(clean_sum)
	mut sum_candidate := clean_sum
	if t.active_specialization_args.len > 0 {
		sum_candidate = t.subst_type(sum_candidate, t.active_specialization_args)
	}
	if resolved := t.resolve_sum_name_from_c_name(sum_candidate) {
		sum_candidate = resolved
	}
	variants := t.concrete_sum_variants_for_candidate(sum_candidate)
	if variants.len == 0 {
		return base
	}
	resolved_sum = sum_candidate
	return t.build_sum_shared_field_chain(base, sum_type, resolved_sum, variants, field, field_type, 0)
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
	use_ptr := t.variant_references_sum(qv, resolved_sum) && !t.sum_variant_is_direct_pointer(qv)
	variant_base := t.make_selector_op(base, sum_field, if use_ptr { '&${qv}' } else { qv }, if sum_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	value := if _ := t.sum_shared_field_type_name(qv, field) {
		nested_base_type := if use_ptr { '&${qv}' } else { qv }
		t.lower_sum_shared_field_selector(variant_base, nested_base_type, field, field_type)
	} else {
		t.struct_field_selector_for_type(variant_base, qv, field, field_type, use_ptr) or {
			t.make_selector_op(variant_base, field, field_type, if use_ptr { .arrow } else { .dot })
		}
	}
	then_block := t.make_block([t.make_expr_stmt(value)])
	else_expr := t.build_sum_shared_field_chain(base, sum_type, resolved_sum, variants, field, field_type, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: field_type
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
	if addr := t.transform_map_index_address_or_nil(node, expr_id, false) {
		return addr
	}
	if node.value == '?' && t.expr_has_option_unwrap_smartcast(expr_id) {
		return t.transform_expr(expr_id)
	}
	if t.is_channel_receive_or_expr(node) {
		return t.transform_channel_receive_or_expr(id, node)
	}
	if sql_aggregate := t.transform_sql_aggregate_or_expr(id, node) {
		return sql_aggregate
	}
	if t.is_enum_from_string_or_expr(node) {
		return t.transform_enum_from_string_or_expr(id, node)
	}
	if t.is_array_index_or_expr(node) {
		return t.transform_array_index_or_expr(id, node)
	}
	fallback_type := if decl_type_is_usable(node.typ) && !t.generic_arg_is_unresolved(node.typ) {
		node.typ
	} else {
		t.stmt_value_type(t.a.child(&node, 1))
	}
	expr_type, value_type := t.or_expr_types(expr_id, fallback_type)
	if expr_type.contains('unknown') || value_type.contains('unknown')
		|| t.type_text_has_generic_placeholder(expr_type, t.cur_module)
		|| t.type_text_has_generic_placeholder(value_type, t.cur_module) {
		return id
	}
	expr_node := t.a.nodes[int(expr_id)]
	if !t.is_optional_type_name(expr_type) || expr_node.kind == .infix {
		if lowered := t.transform_nested_optional_or_expr(expr_id, node) {
			return lowered
		}
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
	if !t.is_optional_type_name(expr_type) {
		return t.preserve_or_expr_for_codegen(id, node)
	}
	return t.lower_or_expr_to_temp(id, node)
}

fn (mut t Transformer) transform_map_index_address_or_nil(node flat.Node, expr_id flat.NodeId, allow_bare_index bool) ?flat.NodeId {
	if node.children_count < 2 || !t.or_body_is_nil(t.a.child(&node, 1)) || int(expr_id) < 0 {
		return none
	}
	source := t.a.nodes[int(expr_id)]
	mut index_id := expr_id
	if source.kind == .prefix && source.op == .amp && source.children_count == 1 {
		index_id = t.a.child(&source, 0)
	} else if !allow_bare_index || source.kind != .index {
		return none
	}
	info := t.map_index_info(index_id) or { return none }
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(info.key_id, info.key_type), info.key_storage_type)
	ptr := t.make_map_get_check_expr(map_expr, info.base_type, key_name)
	target_type := if node.typ.starts_with('&') { node.typ } else { '&${info.value_type}' }
	return t.make_cast(target_type, ptr, target_type)
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
		kind: .or_expr
		op: node.op
		children_start: match_or_start
		children_count: 2
		pos: node.pos
		value: node.value
		typ: expr_type
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
		kind: .match_stmt
		children_start: match_start
		children_count: flat.child_count(children.len)
		pos: match_node.pos
		value: match_node.value
		typ: match_node.typ
	})
	return t.transform_expr(new_match)
}

// transform_prefix_expr transforms transform prefix expr data for transform.
// pointer_storage_amp_decl_type detects `&value` (possibly as an unsafe-block tail)
// when `value` is a source-level value backed by pointer storage.
fn (t &Transformer) pointer_storage_amp_decl_type(rhs_id flat.NodeId) ?string {
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
	if child.kind != .ident
		|| (!t.mut_param_values[child.value] && !t.pointer_value_rvalues[child.value]) {
		return none
	}
	mut vt := t.var_type(child.value)
	if vt.starts_with('mut ') {
		vt = '&' + vt[4..].trim_space()
	}
	if !vt.starts_with('&') && vt.len > 0 {
		vt = '&${vt}'
	}
	return if vt.starts_with('&') { vt } else { none }
}

fn (mut t Transformer) transform_prefix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	// Smartcast payload accesses are fully lowered when they are built. Rewalking
	// their dereference would smartcast the original sum receiver a second time
	// (for example `*a._int` becoming `*(*a._int)._int` in a nested match).
	if _ := t.generated_variant_access_type(id) {
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
		if child.kind == .ident && t.pointer_value_rvalues[child.value] {
			value := t.transform_expr_preserving_pointer_value(child_id)
			mut result := t.make_prefix(.mul, value)
			if node.pos.end > node.pos.offset {
				// Cgen must distinguish this source dereference from the synthetic
				// dereference that reads a mutable parameter's pointer slot.
				t.a.nodes[int(result)].value = source_mut_pointer_deref_marker
			}
			mut value_type := t.node_type(value)
			if child.value in t.mut_param_values && value_type.starts_with('&&') {
				t.set_node_typ(int(result), value_type[1..])
				value_type = value_type[1..]
				result = t.make_prefix(.mul, result)
			}
			if value_type.starts_with('&') {
				t.set_node_typ(int(result), value_type[1..])
			}
			return result
		}
		// A shared value is represented by a pointer to a lock wrapper, but an
		// expression naming it already lowers to the wrapper's `.val`. The generic
		// `T.indirections == 1` branch can still spell that expression as `*val`;
		// discard the source dereference so cgen emits `val->val`, not `*val->val`.
		if child.kind == .ident {
			raw_child_type := t.raw_var_type(child.value).trim_space()
			if raw_child_type.starts_with('shared ') {
				return t.transform_expr(child_id)
			}
		}
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.original_expr_type(child_id)
		}
		if child.kind == .or_expr {
			value := t.transform_expr(child_id)
			value_type := t.node_type(value)
			if !value_type.starts_with('&') {
				return value
			}
			result := t.make_prefix(.mul, value)
			t.set_node_typ(int(result), value_type[1..])
			return result
		}
		if child.kind != .cast_expr && child_type.len > 0 && !child_type.starts_with('&') {
			return t.transform_expr(child_id)
		}
	}
	if node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .or_expr && child.children_count > 0 {
			if addr := t.transform_map_index_address_or_nil(child, t.a.child(&child, 0), true) {
				return addr
			}
		}
		mut child_type := t.node_type(child_id)
		original_child_type := t.original_expr_type(child_id)
		if t.is_optional_type_name(child_type) && !t.is_optional_type_name(original_child_type)
			&& original_child_type.len > 0 && original_child_type != 'unknown' {
			child_type = original_child_type
		} else if child_type.len == 0 {
			child_type = t.resolve_expr_type(child_id)
		}
		if child.kind == .struct_init && t.is_optional_type_name(child.value) {
			child_type = child.value
		}
		if t.is_optional_type_name(child_type) {
			if expr := t.transform_amp_optional_value(node, child_id, child, child_type) {
				return expr
			}
		}
		// Pointer-backed values already name their storage address. Preserve that
		// pointer instead of taking the address of the pointer slot and forming `&&T`.
		if vt := t.pointer_storage_amp_decl_type(id) {
			new_id := t.transform_expr_preserving_pointer_value(child_id)
			t.set_node_typ(int(new_id), vt)
			return new_id
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
		if child.kind in [.array_init, .array_literal]
			&& t.normalize_type_alias(child_type).starts_with('[]') {
			// `&[]T{...}` owns a heap-allocated array header. Allocate that header
			// explicitly instead of taking the address of a short-lived stabilization
			// temporary.
			value := t.transform_expr(child_id)
			result_type := if node.typ.len > 0 { node.typ } else { '&${child_type}' }
			return t.make_call_typed('v3_heap_array', [value], result_type)
		}
		if child.kind == .map_init && t.normalize_type_alias(child_type).starts_with('map[') {
			// Like `&[]T{}`, `&map[K]V{}` owns a heap-allocated container header.
			// A stabilized stack header can escape through a sum type (for example
			// `Value(&map[string]Value{})`), leaving the stored pointer dangling.
			value := t.transform_expr(child_id)
			stable := t.stable_transformed_expr_for_reuse(value, child_type, 'addr')
			addr := t.make_prefix(.amp, stable)
			t.set_node_typ(int(addr), '&${child_type}')
			dup := t.make_memdup_call_for_type(addr, child_type)
			result_type := if node.typ.len > 0 { node.typ } else { '&${child_type}' }
			return t.make_cast(result_type, dup, result_type)
		}
		if expr := t.transform_amp_assoc_expr_for_type(id, node, node.typ) {
			return expr
		}
		if child.kind == .cast_expr && child.children_count > 0 {
			if extracted := t.transform_amp_sum_variant_cast(child) {
				return extracted
			}
			cast_arg_id := t.a.child(&child, 0)
			target_sum := t.resolve_sum_name(t.normalize_type_alias(child.value))
			if target_sum.len > 0 && target_sum in t.sum_types {
				cast_arg := t.a.nodes[int(cast_arg_id)]
				if cast_arg.kind == .nil_literal {
					return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id), '&${child.value}')
				}
				mut cast_arg_type := t.node_type(cast_arg_id)
				if cast_arg_type.len == 0 {
					cast_arg_type = t.resolve_expr_type(cast_arg_id)
				}
				if cast_arg_type == 'voidptr' || cast_arg_type == '&void' {
					return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id), '&${child.value}')
				}
				wrapped := t.wrap_sum_value(cast_arg_id, target_sum)
				addr := t.make_prefix(.amp, wrapped)
				t.set_node_typ(int(addr), if node.typ.len > 0 { node.typ } else { '&${target_sum}' })
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
				mut cast_arg_type := t.node_type(cast_arg_id)
				if cast_arg_type.len == 0 {
					cast_arg_type = t.resolve_expr_type(cast_arg_id)
				}
				if cast_arg_type == 'voidptr' || cast_arg_type == '&void' {
					if boxed_value := t.transform_interface_value_for_type(cast_arg_id, iface, false) {
						target_ptr_type := if node.typ.len > 0 { node.typ } else { '&${iface}' }
						return t.heap_copy_interface_expr(boxed_value, iface, target_ptr_type)
					}
				}
				if boxed := t.transform_interface_value_for_type(cast_arg_id, '&${iface}', false) {
					return boxed
				}
			}
			cast_arg := t.a.nodes[int(cast_arg_id)]
			if cast_arg.kind == .nil_literal {
				return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id), '&${child.value}')
			}
			mut cast_arg_type := t.node_type(cast_arg_id)
			if cast_arg_type.len == 0 {
				cast_arg_type = t.resolve_expr_type(cast_arg_id)
			}
			source_iface := t.resolve_interface_type_name(cast_arg_type)
			if t.pointer_cast_target_implements_source_iface(child.value, source_iface) {
				source := t.transform_expr(cast_arg_id)
				object := t.make_selector_op(source, '_object', 'voidptr', if cast_arg_type.starts_with('&') {
					.arrow
				} else {
					.dot
				})
				return t.make_cast('&${child.value}', object, '&${child.value}')
			}
			return t.make_cast('&${child.value}', t.transform_expr(cast_arg_id), '&${child.value}')
		}
		if child.kind == .or_expr && child.children_count >= 2
			&& t.or_body_is_nil(t.a.child(&child, 1)) {
			index_id := t.a.child(&child, 0)
			if info := t.map_index_info(index_id) {
				map_expr := t.stable_expr_for_reuse(info.base_id)
				key_name := t.new_temp('map_key')
				t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(info.key_id, info.key_type), info.key_storage_type)
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
			if lowered_cast_id := t.try_lower_generic_named_type_cast_call(child) {
				lowered_cast := t.a.nodes[int(lowered_cast_id)]
				if extracted := t.transform_amp_sum_variant_cast(lowered_cast) {
					return extracted
				}
			}
			callee := t.a.child_node(&child, 0)
			arg_id := t.a.child(&child, 1)
			arg := t.a.nodes[int(arg_id)]
			if callee.kind == .selector && callee.children_count > 0
				&& (arg.kind == .nil_literal || callee.value.len > 0) {
				base := t.a.child_node(callee, 0)
				if base.kind == .ident && callee.value.len > 0
					&& (base.value == 'C' || (callee.value[0] >= `A` && callee.value[0] <= `Z`)) {
					target_type := '${base.value}.${callee.value}'
					return t.make_cast('&${target_type}', t.transform_expr(arg_id), '&${target_type}')
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
		// Route a value-context `match`/`if` operand (e.g. `&(match x { First { get_a()!
		// } else { get_b()! } })`) through value lowering so a propagating branch tail is
		// materialized as a value here instead of in a value-less statement context.
		// `transform_value_operand` is a no-op for the common non-branch operands.
		value := t.transform_value_operand(child_id)
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
			// route a value `match`/`if` operand (e.g. `-(match x { ... })`)
			// through its target type so its propagating arms are lowered as values.
			t.transform_value_operand(child_id)
		}
		if node.op == .not {
			child := t.a.nodes[int(new_child)]
			if child.kind == .infix {
				new_child = t.make_paren(new_child)
			}
		}
		new_children << new_child
	}
	if t.inplace_scalar_prefixes && node.op in [.plus, .minus, .bit_not]
		&& t.rewrite_children_in_place(id, new_children) {
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind: .prefix
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: if node.op == .mul && node.pos.end > node.pos.offset {
			source_deref_marker
		} else {
			node.value
		}
		typ: node.typ
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

fn (mut t Transformer) transform_amp_optional_value(node flat.Node, child_id flat.NodeId, child flat.Node, child_type string) ?flat.NodeId {
	source_type := t.qualify_optional_type(child_type)
	payload_type := t.optional_base_type(source_type)
	if payload_type.len == 0 || payload_type == 'void' {
		return none
	}
	target_type := if node.typ.len > 0 {
		t.qualify_optional_type(node.typ)
	} else if child.kind == .struct_init {
		'${source_type[..1]}&${payload_type}'
	} else {
		return none
	}
	if !t.is_optional_type_name(target_type) || !t.optional_base_type(target_type).starts_with('&') {
		return none
	}
	if child.kind == .struct_init {
		if child.children_count == 0 {
			return t.make_optional_none(target_type)
		}
		payload_node := flat.Node{
			kind: .struct_init
			op: child.op
			children_start: child.children_start
			children_count: child.children_count
			pos: child.pos
			value: payload_type
			typ: payload_type
		}
		payload_id := t.a.add_node(payload_node)
		payload := t.transform_struct_init(payload_id, payload_node)
		addr := t.make_prefix(.amp, payload)
		t.set_node_typ(int(addr), '&${payload_type}')
		return t.make_optional_some(addr, target_type)
	}
	return t.transform_optional_value_to_pointer(child_id, source_type, target_type)
}

fn (mut t Transformer) transform_optional_value_to_pointer(source_id flat.NodeId, source_type string, target_type string) ?flat.NodeId {
	payload_type := t.optional_base_type(t.qualify_optional_type(source_type))
	target_payload := t.optional_base_type(t.qualify_optional_type(target_type))
	if payload_type.len == 0 || target_payload != '&${payload_type}' {
		return none
	}
	source0 := t.transform_expr(source_id)
	source := t.optional_source_value_expr(source_id, source0, source_type)
	source_value := if t.expr_can_take_address(source) {
		source
	} else {
		t.stable_transformed_expr_for_reuse(source, source_type, 'opt_ref')
	}
	result_name := t.new_temp('opt_ref_result')
	err := t.make_selector(source_value, 'err', 'IError')
	initial := t.make_optional_none_with_err(target_type, err)
	t.pending_stmts << t.make_decl_assign_typed(result_name, initial, target_type)
	value := t.make_selector(source_value, 'value', payload_type)
	value_addr := t.make_prefix(.amp, value)
	dup := t.make_memdup_call_for_type(value_addr, payload_type)
	addr := t.make_cast(target_payload, dup, target_payload)
	some := t.make_optional_some(addr, target_type)
	assign := t.make_assign(t.make_ident(result_name), some)
	ok := t.make_selector(source_value, 'ok', 'bool')
	t.pending_stmts << t.make_if(ok, t.make_block([assign]), t.make_empty())
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), target_type)
	return result
}

fn (mut t Transformer) transform_optional_value_to_sum(source_id flat.NodeId, source_type string, target_type string) ?flat.NodeId {
	source_optional := t.qualify_optional_type(source_type)
	target_optional := t.qualify_optional_type(target_type)
	source_payload := t.optional_base_type(source_optional)
	target_payload := t.optional_base_type(target_optional)
	if !t.is_sum_type_name(target_payload)
		|| !t.sum_target_accepts_variant_type(target_payload, source_payload) {
		return none
	}
	source0 := t.transform_expr(source_id)
	source := t.optional_source_value_expr(source_id, source0, source_optional)
	source_value := t.stable_transformed_expr_for_reuse(source, source_optional, 'opt_sum')
	result_name := t.new_temp('opt_sum_result')
	err := t.make_selector(source_value, 'err', 'IError')
	initial := t.make_optional_none_with_err(target_optional, err)
	t.pending_stmts << t.make_decl_assign_typed(result_name, initial, target_optional)
	payload := t.make_selector(source_value, 'value', source_payload)
	sum_value := t.wrap_sum_value(payload, target_payload)
	some := t.make_optional_some(sum_value, target_optional)
	assign := t.make_assign(t.make_ident(result_name), some)
	ok := t.make_selector(source_value, 'ok', 'bool')
	t.pending_stmts << t.make_if(ok, t.make_block([assign]), t.make_empty())
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), target_optional)
	return result
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
	else_block := t.make_or_else_block(child.value, t.lower_or_body_to_stmts_with_err_expr(body_id, '', '', child.value, err_expr))
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

fn (mut t Transformer) transform_amp_sum_variant_cast(cast_node flat.Node) ?flat.NodeId {
	if cast_node.kind != .cast_expr || cast_node.children_count != 1 || cast_node.value.len == 0 {
		return none
	}
	cast_arg_id := t.a.child(&cast_node, 0)
	mut source_type := t.node_type(cast_arg_id)
	if source_type.len == 0 {
		source_type = t.original_expr_type(cast_arg_id)
	}
	source_sum := t.resolve_sum_name(t.trim_pointer_type(source_type))
	if source_sum.len == 0 || source_sum !in t.sum_types {
		return none
	}
	variant := t.resolve_variant(source_sum, cast_node.value)
	if variant.len == 0 || !t.variant_references_sum(variant, source_sum) {
		return none
	}
	source := t.transform_expr(cast_arg_id)
	field := t.make_selector_op(source, t.sum_field_name(variant), '&${variant}', if source_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.make_cast('&${cast_node.value}', field, '&${cast_node.value}')
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
			typ := t.normalize_type_alias(t.raw_var_type(node.value))
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
	// Ignore a smartcast on the selector itself, but retain narrowing of its
	// base. For `child.layout as Widget` inside `child is SubWindow`, the raw
	// selector type comes from `SubWindow.layout` (Layout), not from the outer
	// `child.layout is Widget` smartcast recorded on the full expression.
	if sc := t.find_smartcast(t.expr_key(base_id)) {
		base_target := t.trim_pointer_type(t.smartcast_target_type(sc))
		if base_target.len > 0 {
			if ftyp := t.lookup_struct_field_type(base_target, node.value) {
				return ftyp
			}
		}
	}
	mut base_type := t.raw_expr_type_without_smartcast(base_id)
	if base_type.len == 0 {
		base_type = t.original_expr_type(base_id)
	}
	clean_base_type := t.trim_pointer_type(base_type)
	base_iface := t.resolve_interface_type_name(clean_base_type)
	if base_iface.len > 0 {
		if ftyp := t.interface_field_type_name(base_iface, node.value) {
			return ftyp
		}
	}
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
	if t.rewrite_one_child_in_place(id, new_child) {
		return id
	}
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind: .paren
		op: node.op
		children_start: start
		children_count: 1
		pos: node.pos
		value: node.value
		typ: node.typ
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
	if t.rewrite_one_child_in_place(id, new_child) {
		return id
	}
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind: .postfix
		op: node.op
		children_start: start
		children_count: 1
		pos: node.pos
		value: node.value
		typ: node.typ
	})
}

// is_value_match_or_if_operand reports whether the node is a `match`/`if`
// expression used as a value, e.g. a cast operand like `i64(match x { ... })`.
// It looks through transparent wrappers: `(...)` parens, `unsafe { }` (a `.block`
// whose value tail is the expression), and a trailing `expr_stmt` — including
// compositions like `i64(unsafe { match ... })`. Such an operand must be
// transformed with its target type so its (possibly propagating) branch tails
// are lowered as values.
@[direct_array_access]
fn (t &Transformer) is_value_match_or_if_operand(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.is_value_match_or_if_operand(t.a.child(&node, 0))
	}
	if node.kind == .block && node.children_count > 0 {
		return t.is_value_match_or_if_operand(t.a.child(&node, node.children_count - 1))
	}
	return node.kind in [.match_stmt, .if_expr]
}

// operand_hoists_value_branch reports whether lowering `id` as a call operand (receiver or
// argument) can materialize a value `match`/`if` into pending_stmts — either directly, or
// nested inside a compound expression such as an infix, cast, index, prefix, nested call or
// composite literal (`1 + (match ...)`, `i64(match ...)`, `arr[match ...]`). The `last_branch`
// scan uses this to detect an operand that hoists a prelude so preceding operands can be
// stabilized for source order; `is_value_match_or_if_operand` alone stops at the outer
// wrapper and misses a branch buried inside such a compound operand. Recursion stops at
// constructs that lower into their own scope — a nested closure/lambda/spawn body materializes
// into that body, not the current pending. Over-detection is safe here: it only spills an
// extra preceding operand to a temp, which is always order-preserving.
fn (t &Transformer) operand_hoists_value_branch(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.match_stmt, .if_expr] {
		return true
	}
	if node.kind in [.fn_literal, .lambda_expr, .spawn_expr] {
		return false
	}
	for i in 0 .. node.children_count {
		if t.operand_hoists_value_branch(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

// transform_cast_expr transforms transform cast expr data for transform.
@[direct_array_access]
fn (mut t Transformer) transform_cast_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	target_type := t.normalize_type_alias(node.value)
	// Materialize a value-context `match`/`if` cast operand into a value temp before
	// the type-specific dispatch below. Several cast paths return early into helpers
	// that lower the operand with plain `transform_expr` — the optional-sum branch
	// (`?Shape(match ...)`), interface boxing (`Animal(match ...)`), the pointer-to-sum
	// branch (`&Shape(match ...)`) and the sum branch (`Shape(match ...)`) — which would
	// lower a propagating branch tail in a value-less statement context and emit an
	// empty expression. Re-dispatch over the rewritten temp so every path sees a plain,
	// typed operand. `transform_value_operand` is a no-op for the common non-branch operands.
	if node.children_count == 1 {
		match_cast_child := t.a.child(&node, 0)
		if t.is_value_match_or_if_operand(match_cast_child) {
			value := t.materialize_value_branch_operand(match_cast_child)
			if value != match_cast_child {
				start := t.a.children.len
				t.a.children << value
				new_id := t.a.add_node(flat.Node{
					kind: .cast_expr
					op: node.op
					children_start: start
					children_count: 1
					pos: node.pos
					value: node.value
					typ: node.typ
				})
				return t.transform_cast_expr(new_id, t.a.nodes[int(new_id)])
			}
		}
	}
	if target_type.starts_with('&') && t.is_interface_type(target_type) {
		child := t.a.child_node(&node, 0)
		if child.kind == .call && child.children_count > 0 {
			callee := t.a.child_node(child, 0)
			if callee.kind == .ident
				&& callee.value in ['array_get', 'array__get', 'map__get', 'map__get_check'] {
				// Container getters return raw storage. Their typed access helpers add this
				// pointer cast before dereferencing it; a later transform pass must not
				// reinterpret the cast as a concrete-to-interface conversion and box the
				// void pointer.
				return id
			}
		}
	}
	if rewritten := t.transform_pointer_alias_cast_from_heaped_local(node, target_type) {
		return rewritten
	}
	if node.children_count == 1 && transform_is_anonymous_struct_name(target_type) {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .struct_init && child.value == 'struct' {
			mut concrete := child
			concrete.value = target_type
			concrete.typ = target_type
			return t.transform_struct_fields(child_id, concrete)
		}
	}
	if target_type.starts_with('&') && !t.is_interface_type(target_type) {
		if node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			mut child_type := t.node_type(child_id)
			if child_type.len == 0 {
				child_type = t.resolve_expr_type(child_id)
			}
			if t.normalize_type_alias(child_type) == 'voidptr' {
				// A pointer cast from raw container/runtime storage is already the
				// representation read. Recursive sum types must not reinterpret the
				// void pointer itself as a new sum variant on a later transform pass.
				return id
			}
			source_iface := t.resolve_interface_type_name(child_type)
			if t.pointer_cast_target_implements_source_iface(target_type[1..], source_iface) {
				source := t.transform_expr(child_id)
				object := t.make_selector_op(source, '_object', 'voidptr', if child_type.starts_with('&') {
					.arrow
				} else {
					.dot
				})
				return t.make_cast(node.value, object, node.value)
			}
			target_value_type := t.normalize_type_alias(target_type[1..])
			if t.is_sum_type_name(target_value_type) {
				child := t.a.nodes[int(child_id)]
				if child.kind == .nil_literal {
					return t.make_cast(node.value, t.transform_expr(child_id), node.value)
				}
				resolved_sum := t.resolve_sum_name(target_value_type)
				wrapped := t.wrap_sum_value(child_id, resolved_sum)
				tmp_name := t.new_temp('sum_ref')
				t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, resolved_sum)
				addr := t.make_prefix(.amp, t.make_ident(tmp_name))
				t.set_node_typ(int(addr), node.value)
				return addr
			}
		}
		mut new_children := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			new_children << t.transform_expr_preserving_pointer_value(child_id)
		}
		if t.rewrite_children_in_place(id, new_children) {
			return id
		}
		start := t.a.children.len
		for nc in new_children {
			t.a.children << nc
		}
		return t.a.add_node(flat.Node{
			kind: .cast_expr
			op: node.op
			children_start: start
			children_count: node.children_count
			pos: node.pos
			value: node.value
			typ: node.typ
		})
	}
	optional_cast_type := if t.is_optional_type_name(node.value) {
		node.value
	} else if t.is_optional_type_name(node.typ) {
		node.typ
	} else {
		''
	}
	if optional_cast_type.len > 0 {
		child_id := t.a.child(&node, 0)
		child := t.a.node(child_id)
		optional_target := t.qualify_optional_type(optional_cast_type)
		payload_type := t.optional_base_type(optional_target)
		if child.kind == .none_expr {
			return t.make_optional_none(optional_target)
		}
		child_type := t.optional_conversion_source_type(child_id)
		if t.is_sum_type_name(payload_type) && !t.is_optional_type_name(child_type) {
			value := t.wrap_sum_value(child_id, payload_type)
			return t.make_optional_some(value, optional_target)
		}
		expr_target := if child.kind == .or_expr && child.children_count > 1
			&& t.or_body_is_none(t.a.child(child, 1)) {
			optional_target
		} else if t.is_optional_type_name(child_type) {
			optional_target
		} else {
			payload_type
		}
		expr := t.transform_expr_for_type(child_id, expr_target)
		mut expr_type := t.node_type(expr)
		if expr_type.len == 0 {
			expr_type = t.resolve_expr_type(child_id)
		}
		if t.is_optional_type_name(expr_type) {
			return t.coerce_transformed_expr_to_type(expr, child_id, optional_target)
		}
		return t.make_optional_some(expr, optional_target)
	}
	if t.is_sum_type_name(target_type) {
		// A value `match`/`if` operand here has already been materialized into a value
		// temp by the pre-dispatch guard above, so `wrap_sum_value` sees a plain operand.
		return t.wrap_sum_value(t.a.child(&node, 0), target_type)
	}
	// An explicit cast to an interface (`Animal(dog)`, `&PRNG(rng)`) boxes the
	// concrete value into the interface representation, just like an implicit
	// conversion does.
	if t.is_interface_type(target_type) {
		child_id := t.a.child(&node, 0)
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.resolve_expr_type(child_id)
		}
		iface_name := t.resolve_interface_type_name(target_type)
		share_source := t.interface_target_should_share_source(child_id, target_type)
		if target_type.starts_with('&') && (child_type == 'voidptr' || child_type == '&void')
			&& !t.expr_is_nil_like(child_id) {
			iface_value_type := target_type[1..]
			if boxed_value := t.transform_interface_value_for_type(child_id, iface_value_type, share_source) {
				return t.heap_copy_interface_expr(boxed_value, iface_name, target_type)
			}
		}
		if boxed := t.transform_interface_value_for_type(t.a.child(&node, 0), node.value, share_source) {
			return boxed
		}
	}
	cast_child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(cast_child_id)]
	if child.kind == .array_literal {
		if target_type.starts_with('[]') {
			if lowered := t.transform_array_literal_for_type(cast_child_id, child, target_type) {
				return lowered
			}
		}
		if t.is_fixed_array_type(target_type) {
			if lowered := t.transform_fixed_array_literal_for_type(cast_child_id, child, target_type) {
				return lowered
			}
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if target_type in ['f32', 'f64'] {
			new_children << t.transform_expr_for_type(child_id, target_type)
		} else if target_type in ['voidptr', 'byteptr', 'charptr'] {
			cast_arg := t.a.nodes[int(child_id)]
			if cast_arg.kind == .ident && t.pointer_value_rvalues[cast_arg.value]
				&& t.raw_var_type(cast_arg.value).starts_with('&&') {
				value := t.transform_expr_preserving_pointer_value(child_id)
				deref := t.make_prefix(.mul, value)
				t.set_node_typ(int(deref), t.raw_var_type(cast_arg.value)[1..])
				new_children << deref
			} else {
				new_children << t.transform_expr_preserving_pointer_value(child_id)
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	output_typ := if node.typ.len > 0 { t.normalize_type_alias(node.typ) } else { target_type }
	if t.rewrite_children_in_place(id, new_children) {
		if target_type != node.value {
			t.set_node_value(int(id), target_type)
		}
		if output_typ != node.typ {
			t.set_node_typ(int(id), output_typ)
		}
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind: .cast_expr
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: target_type
		typ: output_typ
	})
}

fn (mut t Transformer) transform_pointer_alias_cast_from_heaped_local(node flat.Node, target_type string) ?flat.NodeId {
	if node.children_count != 1 || node.value.starts_with('&') || !target_type.starts_with('&') {
		return none
	}
	child_id := t.a.child(&node, 0)
	if int(child_id) < 0 || int(child_id) >= t.a.nodes.len {
		return none
	}
	child := t.a.nodes[int(child_id)]
	if child.kind != .prefix || child.op != .amp || child.children_count == 0 {
		return none
	}
	source_id := t.a.child(&child, 0)
	if int(source_id) < 0 || int(source_id) >= t.a.nodes.len {
		return none
	}
	source := t.a.nodes[int(source_id)]
	if source.kind != .ident || source.value !in t.heaped_amp_locals {
		return none
	}
	return t.make_cast(node.value, t.transform_expr_preserving_pointer_value(source_id), node.value)
}

fn (t &Transformer) pointer_cast_target_implements_source_iface(target_base_type string, source_iface string) bool {
	if source_iface.len == 0 || isnil(t.tc) {
		return false
	}
	target_base := t.trim_pointer_type(t.normalize_type_alias(target_base_type))
	if target_base.len == 0 || target_base.starts_with('C.') || target_base.starts_with('JS.') {
		return false
	}
	if t.tc.named_type_implements_interface(target_base, source_iface) {
		return true
	}
	if !target_base.contains('.') && t.cur_module.len > 0 {
		return t.tc.named_type_implements_interface('${t.cur_module}.${target_base}', source_iface)
	}
	return false
}

fn (mut t Transformer) transform_expr_preserving_pointer_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return t.transform_expr(id)
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident
		|| (!t.pointer_value_rvalues[node.value] && !t.mut_param_values[node.value]) {
		return t.transform_expr(id)
	}
	had_pointer_value := t.pointer_value_rvalues[node.value]
	had_mut_param := t.mut_param_values[node.value]
	if had_pointer_value {
		t.pointer_value_rvalues.delete(node.value)
	}
	if had_mut_param {
		t.mut_param_values.delete(node.value)
	}
	transformed := t.transform_expr(id)
	if had_pointer_value {
		t.pointer_value_rvalues[node.value] = true
	}
	if had_mut_param {
		t.mut_param_values[node.value] = true
	}
	return transformed
}

// transform_array_literal transforms transform array literal data for transform.
fn (mut t Transformer) transform_array_literal(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.typ.len > 0 && t.is_fixed_array_type(t.normalize_type_alias(node.typ)) {
		if lowered := t.transform_fixed_array_literal_for_type(id, node, node.typ) {
			return lowered
		}
	}
	lowered := t.lower_array_literal_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
	if node.children_count == 0 {
		return id
	}
	array_type := t.node_type(id)
	elem_type := if array_type.starts_with('[]') && array_type.len > 2 {
		array_type[2..]
	} else if node.typ.starts_with('[]') && node.typ.len > 2 {
		node.typ[2..]
	} else {
		''
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << if elem_type.len > 0 {
			t.transform_owned_array_literal_element(child_id, elem_type)
		} else {
			t.transform_expr(child_id)
		}
	}
	if t.rewrite_children_in_place(id, new_children) {
		return id
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind: .array_literal
		op: node.op
		children_start: start
		children_count: node.children_count
		pos: node.pos
		value: node.value
		typ: node.typ
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
	return t.transform_typeof_expr_mode(id, node, true)
}

fn (mut t Transformer) transform_typeof_name_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	return t.transform_typeof_expr_mode(id, node, false)
}

fn (mut t Transformer) transform_typeof_expr_mode(id flat.NodeId, node flat.Node, runtime_sum bool) flat.NodeId {
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
	if t.typeof_expr_is_int_literal(expr_id) {
		return t.make_string_literal('int literal')
	}
	mut typ := ''
	if sc := t.find_smartcast(t.expr_key(expr_id)) {
		typ = if t.is_interface_type_name(sc.sum_type_name) {
			'&${t.interface_variant_type(sc.variant_name)}'
		} else {
			t.resolve_variant(sc.sum_type_name, sc.variant_name)
		}
	}
	if expr.kind == .ident {
		if typ.len == 0 {
			typ = t.raw_var_type(expr.value)
			if t.pointer_value_rvalues[expr.value] && typ.starts_with('&&') {
				typ = typ[1..]
			}
			if expr.value == t.cur_fn_variadic_param && typ.starts_with('[]') {
				typ = '...' + typ[2..]
			}
			if types.type_text_contains_typeof(typ) {
				resolved_local := t.var_type(expr.value)
				if resolved_local.len > 0 && !types.type_text_contains_typeof(resolved_local) {
					typ = resolved_local
				}
			}
			if t.mut_param_values[expr.value] {
				if !typ.starts_with('&') {
					typ = '&${typ}'
				}
			}
		}
	}
	if typ.len == 0 && expr.kind == .selector {
		if raw_field_type := t.raw_selector_field_type(expr_id) {
			typ = raw_field_type
			// Generic fields retain their source declaration (`T`) as the raw type.
			// For an applied receiver, substitute its concrete arguments before
			// folding `typeof(receiver.field).name` to a string literal.
			if t.generic_arg_is_unresolved(typ) && expr.children_count > 0 {
				base_id := t.a.child(&expr, 0)
				mut base_type := t.original_expr_type(base_id)
				if base_type.len == 0 {
					base_type = t.node_type(base_id)
				}
				if base_type.starts_with('&') {
					base_type = base_type[1..]
				}
				if concrete := t.lookup_struct_field_type(base_type, expr.value) {
					if !t.generic_arg_is_unresolved(concrete) {
						typ = concrete
					}
				}
			}
		}
	}
	if expr.kind == .call {
		if concrete := t.explicit_generic_call_return_type_for_typeof(expr_id, expr) {
			typ = concrete
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
	if types.type_text_contains_typeof(typ) {
		typ = t.resolve_typeof_type_text(typ)
	}
	parsed_type := if !isnil(t.tc) { t.tc.parse_type(typ) } else { types.Type(types.void_) }
	unaliased_type := forwarded_return_unalias_type(parsed_type)
	runtime_type := if unaliased_type is types.Pointer {
		forwarded_return_unalias_type(unaliased_type.base_type)
	} else {
		unaliased_type
	}
	if runtime_sum && runtime_type is types.SumType {
		// The active variant of a sum type is only known at runtime. Keep the
		// typeof node for cgen instead of folding it to the declared sum name.
		transformed_expr := t.transform_expr(expr_id)
		start := t.a.children.len
		t.a.children << transformed_expr
		return t.a.add_node(flat.Node{
			kind: .typeof_expr
			typ: 'string'
			pos: node.pos
			children_start: start
			children_count: 1
		})
	}
	if t.cur_fn_is_generic && is_generic_fn_placeholder_name(typ) {
		return t.a.add_node(flat.Node{
			kind: .typeof_expr
			value: generic_type_name_marker(typ)
			typ: 'string'
			pos: node.pos
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

fn (t &Transformer) explicit_generic_call_return_type_for_typeof(id flat.NodeId, node flat.Node) ?string {
	if node.children_count == 0 || isnil(t.tc) {
		return none
	}
	args := t.explicit_generic_call_args(node, t.cur_module) or { return none }
	mut names := []string{}
	if resolved := t.tc.resolved_call_name(id) {
		names << resolved
	}
	mut callee := t.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = t.a.child_node(callee, 0)
	}
	if callee.kind == .ident {
		names << callee.value
		if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] && !callee.value.contains('.') {
			names << '${t.cur_module}.${callee.value}'
		}
	}
	for name in names {
		params := t.tc.fn_generic_params[name] or { continue }
		if params.len == 0 {
			continue
		}
		if args.len != params.len || t.generic_args_have_placeholders(args) {
			continue
		}
		ret := t.tc.fn_ret_type_texts[name] or { t.fn_ret_types[name] or { continue } }
		concrete := substitute_generic_type_text_with_params(ret, args, params)
		if concrete.len > 0 && !t.generic_arg_is_unresolved(concrete) {
			return concrete
		}
	}
	return none
}

fn (t &Transformer) typeof_expr_is_int_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .int_literal {
		return true
	}
	if node.kind == .paren && node.children_count == 1 {
		return t.typeof_expr_is_int_literal(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.children_count == 1 && node.op in [.plus, .minus, .bit_not] {
		return t.typeof_expr_is_int_literal(t.a.child(&node, 0))
	}
	if node.kind != .infix || node.children_count != 2
		|| node.op !in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift, .right_shift,
			.right_shift_unsigned, .power] {
		return false
	}
	return t.typeof_expr_is_int_literal(t.a.child(&node, 0))
		&& t.typeof_expr_is_int_literal(t.a.child(&node, 1))
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
	if name.starts_with('main.') && !name['main.'.len..].contains('.') {
		return name['main.'.len..]
	}
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
	params := typeof_display_fn_param_type_list(clean[open + 1..close])
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

fn typeof_display_fn_param_type_list(text string) string {
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
					parts << typeof_display_fn_param_type_text(text[start..i].trim_space())
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < text.len {
		parts << typeof_display_fn_param_type_text(text[start..].trim_space())
	}
	return parts.join(', ')
}

fn typeof_display_fn_param_type_text(param string) string {
	mut text := param.trim_space()
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	if space := typeof_display_top_level_space_index(text) {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if typeof_display_fn_param_head_is_name(head, tail) {
			text = tail
		}
	}
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	if text.starts_with('&') {
		if is_mut {
			return 'mut ' + typeof_display_type_text(text[1..])
		}
		return typeof_display_type_text(text[1..])
	}
	if is_mut {
		return 'mut ' + typeof_display_type_text(text)
	}
	return typeof_display_type_text(text)
}

fn typeof_display_top_level_space_index(text string) ?int {
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
			` ` {
				if paren_depth == 0 && bracket_depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return none
}

fn typeof_display_fn_param_head_is_name(head string, tail string) bool {
	if head.len == 0 || tail.len == 0 || head.contains('.') {
		return false
	}
	if head in ['fn', 'mut', 'shared', 'atomic', 'chan', 'thread', 'map', 'struct'] {
		return false
	}
	if head.starts_with('&') || head.starts_with('[') || types.is_builtin_type_name(head) {
		return false
	}
	return typeof_display_is_param_name(head)
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
		if node.children_count > 0 {
			expr := t.a.child_node(&node, 0)
			if expr.kind == .ident {
				raw_type := t.raw_var_type(expr.value)
				if t.mut_param_values[expr.value] || (node.value.starts_with('&')
					&& raw_type.len > 0 && !raw_type.starts_with('&')) {
					return node.value.trim_string_left('&')
				}
			}
		}
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
			if t.pointer_value_rvalues[expr.value] && typ.starts_with('&&') {
				typ = typ[1..]
			}
			if expr.value == t.cur_fn_variadic_param && typ.starts_with('[]') {
				typ = '...' + typ[2..]
			}
			if types.type_text_contains_typeof(typ) {
				resolved_local := t.var_type(expr.value)
				if resolved_local.len > 0 && !types.type_text_contains_typeof(resolved_local) {
					typ = resolved_local
				}
			}
			if t.mut_param_values[expr.value] {
				typ = typ.trim_string_left('&')
			}
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
	if types.type_text_contains_typeof(typ) {
		typ = t.resolve_typeof_type_text(typ)
	}
	return typ
}

fn (t &Transformer) resolve_typeof_type_text(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('[]') {
		return '[]' + t.resolve_typeof_type_text(clean[2..])
	}
	if !clean.starts_with('typeof(') || !clean.ends_with(')') {
		return clean
	}
	inner := clean[7..clean.len - 1].trim_space()
	if inner.ends_with(']') {
		open := inner.last_index_u8(`[`)
		if open > 0 {
			base_name := inner[..open].trim_space()
			base_type := t.var_type(base_name)
			if base_type.starts_with('[]') {
				return base_type[2..]
			}
			if t.is_fixed_array_type(base_type) {
				return fixed_array_elem_type(base_type)
			}
			if base_type == 'string' {
				return 'u8'
			}
		}
	}
	local_type := t.var_type(inner)
	if local_type.len > 0 {
		return local_type
	}
	return clean
}

fn (t &Transformer) type_index_for_type_name(type_name string) int {
	if type_name.len == 0 {
		return 0
	}
	indirections := generic_type_indirections(type_name)
	mut base_name := type_name.trim_space()
	for base_name.starts_with('&') {
		base_name = base_name[1..].trim_space()
	}
	// Builtin types keep V's stable ast `*_type_idx` values (int==8, string==21, ...), so
	// `typeof[T]().idx` comparisons against `v.ast` constants behave like the reference
	// compiler.
	builtin_idx := comptime_builtin_type_idx(base_name)
	indirection_bits := int(u32(indirections) << 16)
	if builtin_idx > 0 {
		return builtin_idx | indirection_bits
	}
	normalized := t.normalize_type_in_module(base_name, t.cur_module)
	index_name := if normalized.len > 0 { normalized } else { base_name }
	base_idx := t.runtime_type_indexes[index_name] or { types.stable_type_index(index_name) }
	return base_idx | indirection_bits
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
			if !t.in_call_callee && !isnil(t.tc) {
				if fn_name := t.tc.resolved_fn_value_name(id) {
					t.set_node_value(int(id), fn_name)
					return id
				}
			}
			mut typ := t.var_type(node.value)
			if typ.len == 0 {
				if global_type := t.current_module_global_type(node.value) {
					typ = global_type
				}
			}
			is_file_import_selector_base := t.in_selector_base
				&& file_import_key(t.cur_file, node.value) in t.tc.file_imports
			if typ.len == 0 && !is_file_import_selector_base
				&& (!t.in_call_callee || !t.ident_is_direct_function_callee(node.value)) {
				if key := t.const_type_key_in_context(node.value, t.cur_module, t.cur_file) {
					t.tc.clear_resolved_fn_value(id)
					mut const_name := key
					if !const_name.contains('.')
						&& t.tc.const_owner_module(const_name) in ['', 'main'] {
						const_name = 'main.${const_name}'
					}
					if const_name != node.value {
						t.set_node_value(int(id), const_name)
					}
					return id
				}
			}
			if t.mut_value_ident_nodes[int(id)] && typ.starts_with('&') {
				// A `mut T` parameter is stored as `&T`, but ordinary identifier uses
				// still have the language-level type `T`. Keep that distinction stable
				// across repeated transform/monomorphize scans.
				typ = typ[1..]
			}
			// Idents are the most common node; re-annotating them in place (rather than
			// allocating a fresh node) avoids cascading rebuilds of every enclosing
			// expression and the associated allocations (critical under -gc none).
			if !t.in_call_callee {
				if fn_name := t.resolved_ident_fn_value(id, node.value) {
					t.set_node_value(int(id), fn_name)
					return id
				}
			}
			if typ.len > 0 && typ != node.typ {
				t.set_node_typ(int(id), typ)
			}
			if (!t.in_selector_base || typ.starts_with('&&')) && t.pointer_value_rvalues[node.value]
				&& typ.starts_with('&') {
				deref := t.make_prefix(.mul, id)
				t.set_node_typ(int(deref), typ[1..])
				return deref
			}
			return id
		}
	}
}

fn (t &Transformer) resolved_ident_fn_value(id flat.NodeId, name string) ?string {
	if isnil(t.tc) {
		return t.resolve_fn_value_ident(name)
	}
	if fn_name := t.tc.resolved_fn_value_name(id) {
		return fn_name
	}
	typ := t.tc.resolve_type(id)
	if typ !is types.FnType && typ !is types.Unknown {
		return none
	}
	return t.resolve_fn_value_ident(name)
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
	mut applied_path := []string{}
	for i, sc in contexts {
		if sc.sum_type_name == option_unwrap_marker {
			// current is the Optional_T struct value itself. Type annotations on
			// the rebuilt expr may already report the smartcast base type (which
			// can be a pointer and would make cgen emit `->`), so pin the node's
			// type back to the option before selecting `.value` from it.
			if int(current) >= 0 && t.a.nodes[int(current)].kind in [.ident, .selector] {
				t.set_node_typ(int(current), '?${sc.variant_name}')
			}
			field_op := if current_type.starts_with('&?') { flat.Op.arrow } else { flat.Op.dot }
			current = t.make_selector_op(current, 'value', sc.variant_name, field_op)
			mut params := t.a.nodes[int(current)].generic_params().clone()
			params << transformed_option_unwrap_access_marker
			t.set_node_generic_params(int(current), params)
			current_type = sc.variant_name
			continue
		}
		if t.is_interface_type_name(sc.sum_type_name) {
			if target_iface := t.resolve_interface_pattern_interface(sc.variant_name) {
				if converted := t.convert_interface_expr_to_interface(current, current_type, target_iface) {
					current = converted
					current_type = target_iface
					continue
				}
			}
			qv := t.interface_variant_type(sc.variant_name)
			for current_type.starts_with('&&') {
				current = t.make_prefix(.mul, current)
				current_type = current_type[1..]
				t.set_node_typ(int(current), current_type)
			}
			field_op := if current_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
			object := t.make_selector_op(current, '_object', 'voidptr', field_op)
			cast := t.make_cast('&${qv}', object, '&${qv}')
			current = t.make_prefix(.mul, cast)
			t.set_node_typ(int(current), qv)
			current_type = qv
			continue
		}
		clean_current_type := t.trim_pointer_type(current_type)
		continues_from_current_sum := clean_current_type.len > 0
			&& t.resolve_sum_name(clean_current_type) == t.resolve_sum_name(sc.sum_type_name)
		sum_for_path := if continues_from_current_sum {
			clean_current_type
		} else {
			sc.sum_type_name
		}
		if continues_from_current_sum {
			applied_path.clear()
		}
		mut path := t.sum_variant_path(sum_for_path, sc.variant_name)
		if path.len == 0 {
			path = [t.resolve_variant(sum_for_path, sc.variant_name)]
		}
		mut current_sum := sum_for_path
		for j, qv in path {
			// Nested `is` checks are recorded against the original expression. Their
			// paths are cumulative (`Node -> Stmt`, then `Node -> Stmt -> TypeDecl`).
			// Keep the common prefix already selected instead of rebuilding it for
			// every deeper context.
			if !continues_from_current_sum && j < applied_path.len
				&& t.variant_names_match(applied_path[j], qv) {
				current_type = qv
				current_sum = qv
				continue
			}
			if t.expr_is_variant_access(current, qv) {
				current_type = qv
				current_sum = qv
				if j == applied_path.len {
					applied_path << qv
				}
				continue
			}
			if !continues_from_current_sum && j < applied_path.len {
				applied_path = applied_path[..j].clone()
			}
			field := t.sum_field_name(qv)
			use_ptr := t.variant_references_sum(qv, current_sum)
				&& !t.sum_variant_is_direct_pointer(qv)
			field_typ := if use_ptr { '&${qv}' } else { qv }
			for current_type.starts_with('&&') {
				current = t.make_prefix(.mul, current)
				current_type = current_type[1..]
				t.set_node_typ(int(current), current_type)
			}
			field_op := if current_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
			field_sel := t.make_selector_op(current, field, field_typ, field_op)
			t.mark_generated_variant_access(field_sel, qv)
			if use_ptr && i == contexts.len - 1 && j == path.len - 1 {
				current = t.make_prefix(.mul, field_sel)
				t.set_node_typ(int(current), qv)
				current_type = qv
			} else {
				current = field_sel
				current_type = field_typ
			}
			current_sum = qv
			applied_path << qv
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
			params := node.generic_params()
			for i, param in params {
				if param == generated_variant_access_marker && i + 1 < params.len {
					return params[i + 1]
				}
			}
			if !node.value.starts_with('_') && !node.value.contains('__') {
				return none
			}
			variant := t.variant_type_from_sum_field_name(node.value) or {
				if node.children_count == 0 {
					return none
				}
				base_id := t.a.child(&node, 0)
				base_type := t.trim_pointer_type(t.normalize_type_alias(t.node_type(base_id)))
				sum_type := t.resolve_sum_name(base_type)
				if sum_type.len == 0 {
					return none
				}
				resolved := t.resolve_variant(sum_type, node.value)
				variants := t.sum_types[sum_type] or { return none }
				if resolved !in variants {
					return none
				}
				resolved
			}
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

fn (mut t Transformer) mark_generated_variant_access(id flat.NodeId, variant string) {
	if int(id) < 0 || variant.len == 0 {
		return
	}
	mut params := t.a.nodes[int(id)].generic_params().clone()
	params << generated_variant_access_marker
	params << variant
	t.set_node_generic_params(int(id), params)
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
			resolved := t.resolve_selector_type(node)
			if resolved.len > 0 {
				return resolved
			}
			if node.typ.len > 0 {
				return t.normalize_type_alias(node.typ)
			}
			return ''
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
			mut duplicate := false
			for existing in result {
				if t.variant_names_match(existing.variant_name, sc.variant_name)
					&& t.resolve_sum_name(existing.sum_type_name) == t.resolve_sum_name(sc.sum_type_name) {
					duplicate = true
					break
				}
			}
			if duplicate {
				continue
			}
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
	if _ := t.const_type_key_in_context(name, t.cur_module, t.cur_file) {
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

fn (t &Transformer) ident_is_direct_function_callee(name string) bool {
	if name.len == 0 || t.var_type(name).len > 0 {
		return false
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] && !name.contains('.') {
		if t.is_known_fn_name('${t.cur_module}.${name}') {
			return true
		}
	}
	return t.is_known_fn_name(name)
}

// --- helper methods ---

// new_temp supports new temp handling for Transformer.
pub fn (mut t Transformer) new_temp(prefix string) string {
	name := '__${prefix}_${t.temp_counter}'
	t.temp_counter++
	return name
}

fn (mut t Transformer) new_global_temp(prefix string) string {
	name := '__${prefix}_${t.global_temp_counter}'
	t.global_temp_counter++
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
		kind: .decl_assign
		children_start: start
		children_count: 2
	})
}

// make_expr_stmt builds make expr stmt data for transform.
pub fn (mut t Transformer) make_expr_stmt(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind: .expr_stmt
		children_start: start
		children_count: 1
	})
}

// make_assign builds make assign data for transform.
pub fn (mut t Transformer) make_assign(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	return t.make_assign_op(lhs, rhs, .assign)
}

// make_assign_without_ownership_drop overwrites storage whose previous value has already
// transferred ownership, or a non-owning shallow template used to build an independent clone.
fn (mut t Transformer) make_assign_without_ownership_drop(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	lhs_kind := t.a.nodes[int(lhs)].kind
	kind := if lhs_kind == .index {
		flat.NodeKind.index_assign
	} else if lhs_kind == .selector {
		flat.NodeKind.selector_assign
	} else {
		flat.NodeKind.assign
	}
	return t.a.add_node(flat.Node{
		kind: kind
		op: .assign
		children_start: start
		children_count: 2
		skip_ownership_drops: true
	})
}

// make_assign_after_owned_drop builds the final store for a lowering that has
// already destroyed the previous lvalue. Transform may revisit synthetic
// nodes, so mark this assignment to prevent a second drop-before-assign pass.
fn (mut t Transformer) make_assign_after_owned_drop(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	id := t.make_assign(lhs, rhs)
	t.a.nodes[int(id)].skip_ownership_drops = true
	return id
}

// make_assign_op builds make assign op data for transform.
pub fn (mut t Transformer) make_assign_op(lhs flat.NodeId, rhs flat.NodeId, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	lhs_kind := t.a.nodes[int(lhs)].kind
	kind := if lhs_kind == .index {
		flat.NodeKind.index_assign
	} else if lhs_kind == .selector {
		flat.NodeKind.selector_assign
	} else {
		flat.NodeKind.assign
	}
	return t.a.add_node(flat.Node{
		kind: kind
		op: op
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
		kind: .block
		children_start: start
		children_count: flat.child_count(stmts.len)
	})
}

fn (mut t Transformer) make_block_skip_scope_drops(stmts []flat.NodeId) flat.NodeId {
	id := t.make_block(stmts)
	t.a.nodes[int(id)].value = skip_scope_drops_block_value
	return id
}

fn (mut t Transformer) make_block_prefix_scope_drops(stmts []flat.NodeId) flat.NodeId {
	id := t.make_block(stmts)
	t.a.nodes[int(id)].value = prefix_scope_drops_block_value
	return id
}

// make_infix builds make infix data for transform.
pub fn (mut t Transformer) make_infix(op flat.Op, lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind: .infix
		op: op
		children_start: start
		children_count: 2
	})
}

// make_prefix builds make prefix data for transform.
pub fn (mut t Transformer) make_prefix(op flat.Op, expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind: .prefix
		op: op
		children_start: start
		children_count: 1
	})
}

// make_paren builds make paren data for transform.
pub fn (mut t Transformer) make_paren(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind: .paren
		children_start: start
		children_count: 1
	})
}

// make_if builds make if data for transform.
pub fn (mut t Transformer) make_if(cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	return t.make_if_with_ownership_drop_mode(cond, then_block, else_block, false)
}

// make_if_with_skip_ownership_drops builds a synthetic if that must not consume
// ownership-drop metadata recorded for source control-flow nodes.
pub fn (mut t Transformer) make_if_with_skip_ownership_drops(cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	return t.make_if_with_ownership_drop_mode(cond, then_block, else_block, true)
}

fn (mut t Transformer) make_if_with_ownership_drop_mode(cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId, skip_ownership_drops bool) flat.NodeId {
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	if int(else_block) >= 0 {
		t.a.children << else_block
		return t.a.add_node(flat.Node{
			kind: .if_expr
			children_start: start
			children_count: 3
			skip_ownership_drops: skip_ownership_drops
		})
	}
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 2
		skip_ownership_drops: skip_ownership_drops
	})
}

// push_smartcast updates push smartcast state for Transformer.
pub fn (mut t Transformer) push_smartcast(expr_name string, variant string, sum_type string) {
	t.invalidated_smartcasts.delete(expr_name)
	t.smartcast_stack << SmartcastContext{
		expr_name: expr_name
		variant_name: variant
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
		if t.variant_names_match(v, variant)
			|| t.concrete_variant_matches_generic_pattern(v, variant) {
			return v
		}
	}
	resolved_sum := t.resolve_sum_name(sum_name)
	variants := t.sum_types[resolved_sum] or { return none }
	for v in variants {
		if t.variant_names_match(v, variant)
			|| t.concrete_variant_matches_generic_pattern(v, variant) {
			return v
		}
	}
	return none
}

fn (t &Transformer) concrete_variant_matches_generic_pattern(concrete string, pattern string) bool {
	pattern_base, pattern_args, is_generic := generic_app_parts(pattern)
	if !is_generic {
		return false
	}
	if c_name(pattern) == concrete
		|| t.variant_short_name(c_name(pattern)) == t.variant_short_name(concrete) {
		return true
	}
	if !t.generic_variant_args_are_open(pattern_args) {
		return false
	}
	base_short := t.variant_short_name(pattern_base)
	concrete_short := t.variant_short_name(concrete)
	return concrete_short.starts_with('${base_short}_')
}

fn (t &Transformer) variant_names_match(a string, b string) bool {
	if a == b {
		return true
	}
	if isnil(t.variant_match_cache) {
		return t.variant_names_match_uncached(a, b)
	}
	mut cache := t.variant_match_cache
	if unsafe { cache.module.str != t.cur_module.str } || cache.module.len != t.cur_module.len
		|| unsafe { cache.file.str != t.cur_file.str } || cache.file.len != t.cur_file.len {
		cache.module = t.cur_module
		cache.file = t.cur_file
		cache.generation++
	}
	slot := int(((u64(voidptr(a.str)) >> 4) * 2654435761 ^ (u64(voidptr(b.str)) >> 4)) & 2047)
	if cache.generations[slot] == cache.generation && cache.a_ptrs[slot] == voidptr(a.str)
		&& cache.b_ptrs[slot] == voidptr(b.str) && cache.a_lens[slot] == a.len
		&& cache.b_lens[slot] == b.len {
		return cache.results[slot] > 0
	}
	result := t.variant_names_match_uncached(a, b)
	cache.a_ptrs[slot] = voidptr(a.str)
	cache.b_ptrs[slot] = voidptr(b.str)
	cache.a_lens[slot] = a.len
	cache.b_lens[slot] = b.len
	cache.generations[slot] = cache.generation
	cache.results[slot] = if result { i8(1) } else { i8(-1) }
	return result
}

fn (t &Transformer) variant_names_match_uncached(a string, b string) bool {
	a_has_bracket := a.contains('[')
	b_has_bracket := b.contains('[')
	if !a_has_bracket && !b_has_bracket {
		a_has_dot := a.contains('.')
		b_has_dot := b.contains('.')
		if a_has_dot || b_has_dot {
			if !a.starts_with('&') && !b.starts_with('&') {
				if short_name_view(a) == short_name_view(b) {
					return true
				}
			} else if t.variant_short_name(a) == t.variant_short_name(b) {
				return true
			}
		}
		if (a.contains('fn') || b.contains('fn'))
			&& canonical_fn_variant_name(a) == canonical_fn_variant_name(b) {
			return true
		}
		return false
	}
	a_is_container := a.starts_with('[]') || a.starts_with('map[') || t.is_fixed_array_type(a)
	b_is_container := b.starts_with('[]') || b.starts_with('map[') || t.is_fixed_array_type(b)
	if a_is_container && b_is_container && !t.generic_arg_is_unresolved(a)
		&& !t.generic_arg_is_unresolved(b) {
		resolved_a := t.normalize_type_alias(t.resolve_type_text_import_aliases(a))
		resolved_b := t.normalize_type_alias(t.resolve_type_text_import_aliases(b))
		return resolved_a == resolved_b
	}
	a_has_dot := a.contains('.')
	b_has_dot := b.contains('.')
	if (a_has_dot || b_has_dot) && t.variant_short_name(a) == t.variant_short_name(b) {
		return true
	}
	if (a.contains('fn') || b.contains('fn'))
		&& canonical_fn_variant_name(a) == canonical_fn_variant_name(b) {
		return true
	}
	if t.is_fixed_array_type(a) && t.is_fixed_array_type(b) {
		return t.resolved_fixed_array_canonical_type(a) == t.resolved_fixed_array_canonical_type(b)
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

fn canonical_fn_variant_name(name string) string {
	if !name.contains('fn') {
		return name
	}
	return name.replace('fn (', 'fn(')
}

fn (t &Transformer) generic_variant_args_are_open(args []string) bool {
	for arg in args {
		if t.generic_arg_is_unresolved(arg) {
			return true
		}
	}
	return false
}

@[inline]
fn (t &Transformer) variant_short_name(name string) string {
	if isnil(t.variant_short_name_cache) {
		return if name.contains('.') { variant_short_name_text(name) } else { name }
	}
	mut cache := t.variant_short_name_cache
	recent_slot := alias_cache_slot(name)
	if cache.recent_generations[recent_slot] == cache.recent_generation
		&& unsafe { cache.recent_types[recent_slot].str == name.str }
		&& cache.recent_types[recent_slot].len == name.len {
		return cache.recent_results[recent_slot]
	}
	if cached := cache.entries[name] {
		cache.put_recent(name, cached)
		return cached
	}
	short := if name.contains('.') { variant_short_name_text(name) } else { name }
	cache.entries[name] = short
	cache.put_recent(name, short)
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
	if variant.starts_with('fn(') || variant.starts_with('fn (') {
		return '_Fn_${transform_stable_key_hash(transform_sum_fn_variant_key(variant))}'
	}
	if sum_variant_needs_type_name_field(variant) {
		return '_${naming.type_name_part(variant)}'
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

fn sum_variant_needs_type_name_field(variant string) bool {
	return variant.contains('(') || variant.contains(')') || variant.contains(' ')
}

fn transform_sum_fn_variant_key(variant string) string {
	clean := variant.trim_space()
	open := clean.index('(') or { return clean.replace(' ', '') }
	close := clean.last_index(')') or { return clean.replace(' ', '') }
	params := clean[open + 1..close]
	ret := clean[close + 1..].trim_space().replace(' ', '')
	mut parts := []string{}
	for part in transform_sum_fn_split_top_level_commas(params) {
		ptyp := transform_sum_fn_param_type(part)
		if ptyp.len > 0 {
			parts << ptyp
		}
	}
	return 'fn(${parts.join(',')})${ret}'
}

fn transform_sum_fn_split_top_level_commas(params string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < params.len; i++ {
		ch := params[i]
		if ch == `(` || ch == `[` || ch == `{` {
			depth++
		} else if ch == `)` || ch == `]` || ch == `}` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			parts << params[start..i].trim_space()
			start = i + 1
		}
	}
	parts << params[start..].trim_space()
	return parts
}

fn transform_sum_fn_param_type(param string) string {
	clean := param.trim_space()
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return transform_sum_fn_variant_key(clean)
	}
	space := clean.index(' ') or { return clean }
	first := clean[..space]
	if transform_sum_fn_is_ident(first) && first !in ['fn', 'mut', 'shared'] {
		return clean[space + 1..].trim_space().replace(' ', '')
	}
	if first in ['mut', 'shared'] {
		rest := clean[space + 1..].trim_space()
		second_space := rest.index(' ') or { return clean.replace(' ', '') }
		second := rest[..second_space]
		if transform_sum_fn_is_ident(second) {
			return '${first}${rest[second_space + 1..].trim_space().replace(' ', '')}'
		}
	}
	return clean.replace(' ', '')
}

fn transform_sum_fn_is_ident(s string) bool {
	if s.len == 0 {
		return false
	}
	first := s[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for i := 1; i < s.len; i++ {
		ch := s[i]
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}
	return true
}

fn transform_stable_key_hash(key string) string {
	mut hash := u64(1469598103934665603)
	for b in key.bytes() {
		hash ^= u64(b)
		hash *= u64(1099511628211)
	}
	return '${hash}'
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
	if t.validating_generic_spec && node.children_count > 0 {
		lhs := t.a.child_node(node, 0)
		if lhs.typ.contains('main.') && decl_type_is_usable(lhs.typ) {
			// The cloned identifier carries the same caller-type lock as a cloned
			// struct literal. Keep it ahead of checker RHS authority, which still
			// describes the unspecialized declaration in its original module.
			return lhs.typ
		}
	}
	if t.validating_generic_spec && node.typ.contains('main.') && decl_type_is_usable(node.typ) {
		return node.typ
	}
	if node.children_count == 2 {
		rhs_id := t.a.child(node, 1)
		rhs := t.a.node(rhs_id)
		if rhs.kind == .fn_literal {
			if fn_type := t.fn_value_type_name(rhs_id) {
				return fn_type
			}
		}
		if rhs.kind == .selector && comptime_method_selector_marker in rhs.generic_params() {
			for param in rhs.generic_params() {
				if param.starts_with(comptime_method_selector_fn_type_prefix) {
					return param.all_after(comptime_method_selector_fn_type_prefix)
				}
			}
		}
	}
	if !isnil(t.tc) && node.children_count > 0 {
		// Use only the type that the checker recorded for this declaration. Falling
		// back to resolving the lhs identifier can select a same-named function
		// before the transformer has installed the new local binding (`copy := ...`).
		if lhs_semantic_type := t.tc.expr_type(t.a.child(node, 0)) {
			lhs_type := t.tc.type_name(lhs_semantic_type)
			if decl_type_is_usable(lhs_type) {
				// The declaration type can intentionally differ from the visible
				// smartcast of the rhs after an exiting guard or assertion.
				return lhs_type
			}
		}
	}
	mut rhs_authority := ''
	if node.children_count >= 2 {
		rhs_id := t.a.child(node, 1)
		rhs := t.a.nodes[int(rhs_id)]
		rhs_authority = t.decl_rhs_type(rhs_id)
		if t.is_fn_pointer_type_name(rhs_authority) {
			return rhs_authority
		}
		if rhs.kind == .infix && rhs.op == .right_shift_unsigned {
			if decl_type_is_usable(rhs_authority) {
				return rhs_authority
			}
		}
		if rhs.kind == .cast_expr {
			if t.is_sum_type_name(rhs_authority) {
				return rhs_authority
			}
		}
		if rhs.kind == .call {
			sum_constructor_type := t.sum_constructor_call_type(rhs)
			if sum_constructor_type.len > 0 {
				return sum_constructor_type
			}
		}
		if t.decl_type_should_override_fallback(rhs_authority, node.typ, rhs) {
			return rhs_authority
		}
	}
	if decl_type_is_usable(node.typ) {
		return node.typ
	}
	if node.children_count >= 2 {
		return rhs_authority
	}
	return ''
}

fn (t &Transformer) sum_constructor_call_type(node flat.Node) string {
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	callee := t.a.child_node(&node, 0)
	mut name := ''
	if callee.kind == .ident {
		name = callee.value
	} else if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		if base.kind == .ident && base.value.len > 0 {
			name = '${base.value}.${callee.value}'
		}
	}
	if name.len == 0 {
		return ''
	}
	if callee.kind == .ident && callee.value.contains(']')
		&& !callee.value.trim_space().ends_with(']') {
		return ''
	}
	short_name := short_name_view(name)
	if short_name.len == 0 || short_name[0] < `A` || short_name[0] > `Z` {
		return ''
	}
	if node.value.len > 0 {
		generic_name := '${name}[${node.value}]'
		resolved_generic := t.resolve_sum_name(t.normalize_type_alias(generic_name))
		if resolved_generic in t.sum_types {
			return generic_name
		}
	}
	resolved := t.resolve_sum_name(t.normalize_type_alias(name))
	if resolved in t.sum_types {
		return resolved
	}
	return ''
}

fn (t &Transformer) raw_decl_type_for_rhs(rhs_id flat.NodeId, rhs flat.Node, fallback string) string {
	if rhs.kind == .selector {
		if raw_type := t.raw_selector_field_type(rhs_id) {
			return raw_type
		}
	}
	if rhs.kind == .or_expr && !isnil(t.tc) {
		raw_type := t.raw_checker_node_type(rhs_id)
		clean := t.trim_pointer_type(raw_type.trim_space())
		if t.is_type_alias_name(clean) {
			// Unwrapping uses the alias target for storage, but declarations retain
			// the source alias for `typeof` and generated auto-stringification.
			return raw_type
		}
	}
	if rhs.kind == .cast_expr && rhs.value.len > 0 && !isnil(t.tc) {
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

fn (t &Transformer) raw_call_decl_return_type(id flat.NodeId, node flat.Node) ?string {
	if isnil(t.tc) {
		return none
	}
	if name := t.tc.resolved_call_name(id) {
		if raw := t.raw_return_type_for_fn_name(name, node) {
			return raw
		}
	}
	if node.kind == .call {
		call_name := t.resolve_call_name(node)
		if call_name.len > 0 {
			if raw := t.raw_return_type_for_fn_name(call_name, node) {
				return raw
			}
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
				&& !call_name.contains('.') {
				if raw := t.raw_return_type_for_fn_name('${t.cur_module}.${call_name}', node) {
					return raw
				}
			}
		}
	}
	return none
}

fn (t &Transformer) raw_infix_operator_decl_return_type(node flat.Node) ?string {
	if node.kind != .infix || node.children_count < 2 {
		return none
	}
	op_name := struct_operator_symbol(node.op) or { return none }
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	mut receiver := ''
	if lhs.kind == .ident {
		receiver = t.raw_var_type(lhs.value)
	}
	if receiver.len == 0 {
		receiver = t.node_type(lhs_id)
	}
	receiver = receiver.trim_space()
	if receiver.starts_with('&') {
		receiver = receiver[1..]
	}
	if receiver.len == 0 {
		return none
	}
	for candidate in t.operator_receiver_candidates(receiver) {
		if raw := t.raw_return_type_for_fn_name('${candidate}.${op_name}', node) {
			return raw
		}
		if raw := t.raw_return_type_for_fn_name(c_name('${candidate}.${op_name}'), node) {
			return raw
		}
	}
	return none
}

fn (t &Transformer) raw_return_type_for_fn_name(name string, node flat.Node) ?string {
	if ret := t.tc.fn_ret_type_texts[name] {
		raw := t.raw_call_return_type_name(ret, node)
		if t.raw_return_type_contains_alias(raw) {
			return raw
		}
	}
	if ret := t.tc.fn_ret_types[name] {
		raw := t.raw_call_return_type_name(ret.name(), node)
		if t.raw_return_type_contains_alias(raw) {
			return raw
		}
	}
	if ret := t.fn_ret_types[name] {
		raw := t.raw_call_return_type_name(ret, node)
		if t.raw_return_type_contains_alias(raw) {
			return raw
		}
	}
	return none
}

fn (t &Transformer) raw_return_type_contains_alias(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if t.generic_type_text_contains_alias(clean, t.cur_module) || t.is_type_alias_name(clean) {
		return true
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return t.raw_return_type_contains_alias(clean[1..])
	}
	if clean.starts_with('mut ') {
		return t.raw_return_type_contains_alias(clean[4..])
	}
	if clean.starts_with('shared ') || clean.starts_with('atomic ') {
		return t.raw_return_type_contains_alias(clean[7..])
	}
	return false
}

fn (t &Transformer) raw_call_return_type_name(ret_name string, node flat.Node) string {
	mut typ := ret_name
	if node.value.len > 0 {
		generic_arg := t.normalize_type_in_module(node.value, t.cur_module)
		if generic_arg.len > 0 {
			typ = t.specialize_generic_type_name(typ, generic_arg)
		}
	}
	return typ
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
			if global_type := t.current_module_global_type(node.value) {
				return t.normalize_type_alias(global_type)
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
			if sum_ctor_type := t.generic_sum_constructor_call_type(node) {
				return sum_ctor_type
			}
			if declared_ret := t.checker_resolved_non_builtin_return_type(id, node) {
				return declared_ret
			}
			concrete_typ := t.concrete_node_type_name(node)
			if concrete_typ.len > 0 {
				return concrete_typ
			}
			new_map_typ := t.new_map_call_type(node)
			if new_map_typ.len > 0 {
				return new_map_typ
			}
			if array_typ := t.array_call_type_name(id, node) {
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
			mut ret := t.get_call_return_type(id, node)
			if ret.len == 0 {
				ret = t.current_call_return_type(node)
			}
			if ret.len > 0 {
				return ret
			}
			if node.typ.len > 0 {
				typ := t.normalize_type_alias(node.typ)
				if typ !in ['array', 'map', 'unknown'] {
					return typ
				}
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
						if local_elem := t.array_literal_local_struct_elem_name(node) {
							if name[2..].all_after_last('.') == local_elem {
								return '[]${local_elem}'
							}
						}
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
			if node.value.starts_with('typeof(') && !isnil(t.tc) {
				if typ := t.tc.expr_type(id) {
					name := typ.name()
					if name.starts_with('[]') {
						return t.normalize_type_alias(name)
					}
				}
			}
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
				key_type := t.array_literal_child_value_type(first_id)
				value_type := t.array_literal_child_value_type(t.a.child(&node, 1))
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
			return if node.value.starts_with('c:') { '&u8' } else { 'rune' }
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
				lhs_id := t.a.child(&node, 0)
				rhs_id := t.a.child(&node, 1)
				lhs_type := t.node_type(lhs_id)
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
				rhs_type := t.node_type(rhs_id)
				if node.op == .plus && rhs_type == 'string' {
					return 'string'
				}
				if node.op in [.plus, .minus] && is_pointer_like_type_name(lhs_type)
					&& t.is_integer_type_name(rhs_type) {
					return lhs_type
				}
				if node.op == .plus && is_pointer_like_type_name(rhs_type)
					&& t.is_integer_type_name(lhs_type) {
					return rhs_type
				}
				if node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor] {
					if t.expr_is_c_selector(lhs_id) || t.expr_is_c_selector(rhs_id) {
						if t.is_numeric_stringify_type(lhs_type) {
							return lhs_type
						}
						if t.is_numeric_stringify_type(rhs_type) {
							return rhs_type
						}
						return 'int'
					}
					if lhs_type.len > 0 && rhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type)
						&& t.is_numeric_stringify_type(rhs_type) {
						if promoted := promote_numeric_literal_infix_type(t.a.nodes[int(t.a.child(&node, 0))], lhs_type, t.a.nodes[int(t.a.child(&node, 1))], rhs_type) {
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

fn (t &Transformer) expr_is_c_selector(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base := t.a.child_node(&node, 0)
	return base.kind == .ident && base.value == 'C'
}

fn (t &Transformer) array_literal_elem_type(node flat.Node) string {
	if node.children_count == 0 {
		return 'int'
	}
	if local_elem := t.array_literal_local_struct_elem_name(node) {
		return local_elem
	}
	if alias_type := t.array_literal_alias_type(node) {
		return alias_type[2..]
	}
	elem_type := t.array_literal_child_elem_type(t.a.child(&node, 0))
	if !is_numeric_type_name(elem_type) {
		return elem_type
	}
	mut has_f32 := false
	mut has_f64 := false
	mut has_explicit_f64 := false
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child_type := t.array_literal_child_elem_type(child_id)
		if !is_numeric_type_name(child_type) {
			return elem_type
		}
		if child_type == 'f32' {
			has_f32 = true
		}
		if child_type == 'f64' {
			has_f64 = true
			if !t.is_untyped_float_literal_expr(child_id) {
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

fn (t &Transformer) array_literal_child_elem_type(child_id flat.NodeId) string {
	child := t.a.nodes[int(child_id)]
	if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
		spread_type := t.normalize_type_alias(t.node_type(t.a.child(&child, 0))).trim_space()
		if spread_type.starts_with('[]') {
			return spread_type[2..]
		}
		if t.is_fixed_array_type(spread_type) {
			return fixed_array_elem_type(spread_type)
		}
	}
	return t.node_type(child_id)
}

fn (t &Transformer) array_literal_pointer_value_elem_type(node flat.Node) ?string {
	mut elem_type := ''
	for i in 0 .. node.children_count {
		child_type := t.pointer_value_expr_type(t.a.child(&node, i)) or { continue }
		if elem_type.len == 0 {
			elem_type = child_type
			continue
		}
		if elem_type != child_type {
			return none
		}
	}
	if elem_type.len > 0 {
		return elem_type
	}
	return none
}

fn (t &Transformer) array_literal_child_value_type(id flat.NodeId) string {
	if typ := t.pointer_value_expr_type(id) {
		return typ
	}
	return t.node_type(id)
}

fn (t &Transformer) pointer_value_expr_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	if !t.pointer_value_rvalues[node.value] && !t.mut_param_values[node.value] {
		return none
	}
	typ := t.var_type(node.value)
	if !typ.starts_with('&') {
		return none
	}
	return typ[1..]
}

fn (t &Transformer) is_untyped_float_literal_expr(id flat.NodeId) bool {
	mut const_expr_path := []flat.NodeId{}
	return t.is_untyped_float_literal_expr_with_const_path(id, mut const_expr_path)
}

fn (t &Transformer) is_untyped_float_literal_expr_with_const_path(id flat.NodeId, mut const_expr_path []flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.float_literal {
			return true
		}
		.prefix {
			if node.op !in [.plus, .minus] || node.children_count == 0 {
				return false
			}
			return t.is_untyped_float_literal_expr_with_const_path(t.a.child(&node, 0), mut const_expr_path)
		}
		.paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return t.is_untyped_float_literal_expr_with_const_path(t.a.child(&node, 0), mut const_expr_path)
		}
		.ident, .selector {
			expr_id := t.const_expr_for_arg(id) or { return false }
			if expr_id in const_expr_path {
				return false
			}
			const_expr_path << expr_id
			return t.is_untyped_float_literal_expr_with_const_path(expr_id, mut const_expr_path)
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
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
		&& !name.contains('.') {
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
	if ret := t.fn_ret_types[name] {
		return t.call_return_type_name(ret, node)
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[name] {
			return t.call_return_type_name(ret.name(), node)
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

// callee_needs_ordering_snapshot reports whether a plain-call callee (operand 0) must be
// snapshotted to keep callee-before-argument order before a later branch argument's hoisted
// prelude. A runtime callee expression (`make_cb(mut trace)(match ...)`) must evaluate once,
// in source order; a function-valued local variable can be reassigned by the prelude, so it is
// snapshotted too. A plain top-level function-name ident is a constant reference that
// name-based call dispatch relies on, so it is left inline. Already-snapshotted temps are not
// re-snapshotted.
fn (t &Transformer) callee_needs_ordering_snapshot(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if t.is_ordering_snapshot_temp(id) {
			return false
		}
		local_type := t.var_type(node.value)
		return local_type.starts_with('fn ') || t.is_fn_pointer_type_name(local_type)
	}
	return t.operand_needs_ordering_snapshot(id)
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
	field := short_name_view(name)
	resolved_base := if mod := t.tc.file_imports[file_import_key(file, base)] {
		mod
	} else {
		base
	}
	qname := '${resolved_base}.${field}'
	if qname in t.tc.const_types {
		return qname
	}
	if resolved_base in ['', 'main'] && field in t.tc.const_types
		&& t.tc.const_owner_module(field) in ['', 'main'] {
		return field
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
			if decl_type_is_usable(typ) && typ != 'void' {
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
			expr_name: subj
			variant_name: sc.variant_name
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
	if node.kind == .or_expr && node.children_count > 0 {
		source_id := t.a.child(&node, 0)
		source_node := t.a.nodes[int(source_id)]
		source_type := t.json_decode_or_expr_type(source_id, source_node) or {
			t.node_type(source_id)
		}
		if t.is_optional_type_name(source_type) {
			value_type := t.optional_base_type(source_type)
			if value_type.len > 0 && value_type !in ['unknown', 'void'] {
				return value_type
			}
		}
	}
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
					kind: .expr_stmt
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
		if node.kind != .fn_decl
			|| (i < t.transformed_fns.len && t.transformed_fns[i])
			|| t.a.specialized_fn_nodes[i]
			|| t.fn_decl_has_unresolved_generics(node, t.node_module_or(i, '')) {
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
		tmp_name := t.new_temp('match_tmp')
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
		transformed_match_type := t.node_type(transformed_match_expr)
		if transformed_match_type.len > 0 && transformed_match_type != 'unknown' {
			match_type = transformed_match_type
		}
		t.drain_pending(mut match_prelude)
		t.pending_stmts = outer_pending
		tmp_ident := t.a.add_val(.ident, tmp_name)
		t.set_node_typ(int(tmp_ident), match_type)
		decl_start := t.a.children.len
		t.a.children << tmp_ident
		t.a.children << transformed_match_expr
		prefix_id = t.a.add_node(flat.Node{
			kind: .decl_assign
			children_start: decl_start
			children_count: 2
			typ: match_type
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
			kind: .block
			children_start: block_start
			children_count: flat.child_count(match_prelude.len + 2)
			typ: result_type
		})
		return block_id
	}
	if result_type.len > 0 {
		t.set_node_typ(int(if_id), result_type)
	}
	return if_id
}

// build_match_chain builds match chain data for transform.
@[direct_array_access]
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
		return t.build_match_type_branch_chain(match_expr_id, orig_expr_id, branch, branches, idx, 0)
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
		kind: .if_expr
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
		tmp_name := t.new_temp('match_tmp')
		mut match_type := t.node_type(match_expr_id)
		transformed_match_expr := if match_expr.kind == .or_expr
			&& t.is_optional_type_name(match_type) {
			match_type = t.optional_base_type(t.qualify_optional_type(match_type))
			t.transform_expr_for_type(match_expr_id, match_type)
		} else {
			t.transform_expr(match_expr_id)
		}
		transformed_match_type := t.node_type(transformed_match_expr)
		if transformed_match_type.len > 0 && transformed_match_type != 'unknown' {
			match_type = transformed_match_type
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
	result << t.build_match_value_chain(actual_expr_id, match_expr_id, branches, 0, target_name, target_type)
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
		return t.build_match_value_type_branch_chain(match_expr_id, orig_expr_id, branch, branches, idx, 0, target_name, target_type)
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
		else_part := t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1, target_name, target_type)
		if_ids << else_part
	}
	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	if_id := t.a.add_node(flat.Node{
		kind: .if_expr
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
			t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1, target_name, target_type)
		} else {
			t.a.add(flat.NodeKind.empty)
		}
	}
	cond_val_id := t.a.child(&branch, cond_idx)
	variant_name := t.match_type_pattern_for_subject(match_expr_id, cond_val_id) or {
		return t.build_match_value_chain(match_expr_id, orig_expr_id, branches, idx + 1, target_name, target_type)
	}
	is_start := t.a.children.len
	t.a.children << match_expr_id
	is_id := t.a.add_node(flat.Node{
		kind: .is_expr
		value: variant_name
		typ: 'match_exact'
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) or {
		SmartcastContext{
			variant_name: variant_name
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
		body_id := t.a.child(&branch, i)
		body_ids << t.clone_match_variant_sizeof(body_id, subj, orig_subj, variant_name) or {
			body_id
		}
	}
	raw_body := t.make_block(body_ids)
	body_block := t.if_value_branch_block(raw_body, target_name, target_type)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_match_value_type_branch_chain(match_expr_id, orig_expr_id, branch, branches, idx, cond_idx + 1, target_name, target_type)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind: .if_expr
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
		kind: .is_expr
		value: variant_name
		typ: 'match_exact'
		children_start: is_start
		children_count: 1
	})
	cond_id := t.transform_is_expr(is_id, t.a.nodes[int(is_id)])

	mut sc_pushed := 0
	sc := t.match_type_smartcast_context(match_expr_id, cond_val_id) or {
		SmartcastContext{
			variant_name: variant_name
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
		body_id := t.a.child(&branch, i)
		body_ids << t.clone_match_variant_sizeof(body_id, subj, orig_subj, variant_name) or {
			body_id
		}
	}
	body_block := t.make_block(t.transform_stmts(body_ids))
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}

	else_part := t.build_match_type_branch_chain(match_expr_id, orig_expr_id, branch, branches, idx, cond_idx + 1)
	start := t.a.children.len
	t.a.children << cond_id
	t.a.children << body_block
	t.a.children << else_part
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
	})
}

// clone_match_variant_sizeof resolves the ambiguous `sizeof(subject)` leaf for one
// concrete arm of a multi-type match. The parser stores its argument as text rather
// than an expression child, so it cannot be narrowed by the regular ident smartcast.
fn (mut t Transformer) clone_match_variant_sizeof(id flat.NodeId, subject string, original_subject string, variant_name string) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .sizeof_expr && node.value.len > 0
		&& (node.value == subject || node.value == original_subject) {
		return t.a.add_node(flat.Node{
			kind: .sizeof_expr
			pos: node.pos
			value: variant_name
			typ: if node.typ.len > 0 { node.typ } else { 'usize' }
		})
	}
	if node.children_count == 0 {
		return none
	}
	mut changed := false
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if replacement := t.clone_match_variant_sizeof(child_id, subject, original_subject, variant_name) {
			children << replacement
			changed = true
		} else {
			children << child_id
		}
	}
	if !changed {
		return none
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: node.kind
		op: node.op
		pos: node.pos
		value: node.value
		typ: node.typ
		payload: flat.node_payload(node.generic_params().clone())
		is_mut: node.is_mut
		children_start: start
		children_count: node.children_count
		skip_ownership_drops: node.skip_ownership_drops
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
		kind: .infix
		op: .eq
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
				kind: .is_expr
				value: variant_name
				typ: 'match_exact'
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
				kind: .is_expr
				value: variant_name
				typ: 'match_exact'
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
				kind: .infix
				op: .logical_or
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
		if target_iface := t.resolve_interface_pattern_interface(pattern) {
			return target_iface
		}
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
		// Keep the concrete generic application while resolving the pattern.
		// Resolving `Tree[int]` to its declaration key `Tree` first would turn a
		// bare `Node` arm back into the open declaration variant `Node[T]`.
		if resolved_variant := t.sum_variant_name(candidate, pattern) {
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
		variant_name: variant_name
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
		if interface_pattern_is_collapsed_container_type(candidate) {
			container_type := t.tc.parse_type(candidate)
			if (container_type is types.Array || container_type is types.Map)
				&& t.tc.named_type_implements_interface(container_type.name(), iface) {
				return container_type.name()
			}
			continue
		}
		if t.is_builtin_ierror_interface_name(iface) {
			if t.tc.named_type_compatible_with_ierror(candidate) {
				return candidate
			}
		} else if target_iface := t.resolve_interface_pattern_interface(candidate) {
			if t.tc.interface_implements_interface(iface, target_iface)
				|| t.tc.interface_implements_interface(target_iface, iface) {
				return target_iface
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
					t.set_decl_var_type(node, lhs.value, typ)
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
				mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
				if push_many && t.array_append_rhs_is_sum_variant_value(rhs_id, rhs_type, elem_type) {
					push_many = false
				}
				val := if push_many {
					'push_many'
				} else {
					'push'
				}
				t.set_node(i, flat.Node{
					kind: node.kind
					op: node.op
					children_start: node.children_start
					children_count: node.children_count
					value: val
					typ: elem_type
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
			raw_source_typ := if child.typ.starts_with('...') {
				'[]' + child.typ[3..]
			} else {
				child.typ
			}
			typ := if child.typ.starts_with('...') {
				'[]' + t.normalize_type_alias(child.typ[3..])
			} else {
				t.normalize_type_alias(child.typ)
			}
			if child.is_mut || child.op == .amp || child.typ.starts_with('mut ') {
				t.mut_param_values[child.value] = true
			}
			t.set_var_type_with_raw(child.value, typ, raw_source_typ)
		}
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_decl_var_type(child, lhs.value, typ)
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
					t.set_decl_var_type(child, lhs.value, typ)
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
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	if push_many && t.array_append_rhs_is_sum_variant_value(rhs_id, rhs_type, elem_type) {
		push_many = false
	}
	if push_many {
		t.set_node(int(node_id), flat.Node{
			kind: .infix
			op: .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value: 'push_many'
			typ: elem_type
		})
	} else {
		t.set_node(int(node_id), flat.Node{
			kind: .infix
			op: .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value: 'push'
			typ: elem_type
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
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	if push_many && t.array_append_rhs_is_sum_variant_value(rhs_id, rhs_type, elem_type) {
		push_many = false
	}
	val := if push_many {
		'push_many'
	} else {
		'push'
	}
	t.set_node(int(node_id), flat.Node{
		kind: node.kind
		op: node.op
		children_start: node.children_start
		children_count: node.children_count
		value: val
		typ: elem_type
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
