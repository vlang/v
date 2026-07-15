module types

import os
import strings
import v3.flat
import v3.gen.c.naming

const comptime_field_members = [
	'name',
	'is_option',
	'is_opt',
	'is_embed',
	'is_array',
	'is_map',
	'is_chan',
	'is_struct',
	'is_enum',
	'is_alias',
	'is_shared',
	'is_atomic',
	'is_mut',
	'is_pub',
	'indirections',
	'attrs',
	'str',
	'typ',
	'unaliased_typ',
]

const comptime_enum_value_members = [
	'name',
	'value',
	'attrs',
]

const comptime_variant_members = [
	'typ',
]

const export_c_reserved_words = {
	'auto':     true
	'break':    true
	'case':     true
	'char':     true
	'const':    true
	'continue': true
	'default':  true
	'do':       true
	'double':   true
	'else':     true
	'enum':     true
	'extern':   true
	'float':    true
	'for':      true
	'goto':     true
	'if':       true
	'inline':   true
	'int':      true
	'long':     true
	'register': true
	'restrict': true
	'return':   true
	'short':    true
	'signed':   true
	'sizeof':   true
	'static':   true
	'struct':   true
	'switch':   true
	'typedef':  true
	'union':    true
	'unsigned': true
	'void':     true
	'volatile': true
	'while':    true
}

const export_v3_reserved_c_symbols = {
	'i8':            true
	'i16':           true
	'i32':           true
	'i64':           true
	'u8':            true
	'byte':          true
	'u16':           true
	'u32':           true
	'u64':           true
	'bool':          true
	'voidptr':       true
	'int_literal':   true
	'float_literal': true
	'chan':          true
	'string':        true
	'array':         true
	'Array':         true
	'map':           true
	'mapnode':       true
	'DenseArray':    true
	'SortedMap':     true
	'Optional':      true
	'IError':        true
	'true':          true
	'false':         true
	'elem_size':     true
	'c_name':        true
}

const export_c_libc_collision_symbols = {
	'rint':  true
	'y0':    true
	'y1':    true
	'yn':    true
	'j0':    true
	'j1':    true
	'jn':    true
	'drem':  true
	'scalb': true
}

// tarr1 supports tarr1 handling for types.
fn tarr1(a Type) []Type {
	mut r := []Type{}
	r << a
	return r
}

// tarr2 supports tarr2 handling for types.
fn tarr2(a Type, b Type) []Type {
	mut r := []Type{}
	r << a
	r << b
	return r
}

// tarr3 supports tarr3 handling for types.
fn tarr3(a Type, b Type, c Type) []Type {
	mut r := []Type{}
	r << a
	r << b
	r << c
	return r
}

// unknown_type supports unknown type handling for types.
fn unknown_type(reason string) Type {
	return Type(Unknown{
		reason: reason
	})
}

// TypeError represents type error data used by types.
pub struct TypeError {
pub:
	msg        string
	kind       TypeErrorKind
	node       flat.NodeId
	file       string
	node_kind  string
	node_value string
	node_pos   string
}

// TypeErrorKind lists type error kind values used by types.
pub enum TypeErrorKind {
	unknown_ident
	unknown_type
	unknown_fn
	unknown_field
	cannot_index
	if_branch_mismatch
	assignment_mismatch
	return_mismatch
	call_arg_mismatch
	condition_mismatch
	duplicate_decl
	unhandled_node
	unsupported_generic
	compile_error
}

// CallInfo stores call info metadata used by types.
pub struct CallInfo {
pub:
	name                 string
	params               []Type
	shared_params        []bool
	return_type          Type
	has_receiver         bool
	is_variadic          bool
	is_c_variadic        bool
	params_known         bool
	has_implicit_veb_ctx bool
	arg_offset           int
}

// LocalBinding represents local binding data used by types.
struct LocalBinding {
	name string
	typ  Type
}

// TypeCache represents type cache data used by types.
struct TypeCache {
mut:
	// Frozen fallback cache shared by all parallel-check forks: lookups miss the
	// own (private) maps first, then consult base read-only; writes always go to
	// the own maps. The master installs its warm post-collect cache as base for
	// the whole region (using a private overlay itself) so workers do not start
	// cold and re-derive every memoized type/index.
	base                       &TypeCache = unsafe { nil }
	parse_enabled              bool
	parse_entries              map[string]Type
	c_entries                  map[string]string
	c_name_entries             map[string]string
	struct_field_entries       map[string]Type
	struct_field_misses        map[string]bool
	ierror_compat_entries      map[string]int
	source_error_embed_entries map[string]int
	source_error_embed_indexed bool
	ierror_impl_names          []string
	ierror_impl_names_set      bool
	// short type name -> unique qualified name ('' = ambiguous); built lazily
	// by unique_qualified_type_name from the five type-name maps.
	short_type_name_index       map[string]string
	short_type_name_index_built bool
	// `${module}\x01${name}` for every fn_decl node, built lazily (and extended
	// incrementally as transform appends declarations) by local_fn_decl_exists,
	// which previously walked every AST node per query.
	local_fn_decl_index       map[string]bool
	local_fn_decl_indexed_len int
	local_fn_decl_last_module string
}

@[heap]
struct FileImportInfo {
mut:
	imports           map[string]string
	selective_imports map[string][]string
}

// TransformForkOverlay holds the call/fn-value resolutions a parallel-transform
// worker records for its transform-created (cloned) nodes. It lives on the heap
// (like TypeCache) so a worker's `&TypeChecker` fork can write through the
// pointer without mutating the shared node-indexed arrays; reads consult the
// overlay before those arrays, and merge_worker replays the entries into the
// master under the shifted node ids.
@[heap]
pub struct TransformForkOverlay {
pub mut:
	resolved_call_names map[int]string
	resolved_fn_values  map[int]string
}

// PendingIerrorError is an invalid-ierror-return candidate recorded while the
// called-fns gate set was still being computed on the collector thread.
struct PendingIerrorError {
	err      TypeError
	fn_qname string
}

// TypeChecker represents type checker data used by types.
@[heap]
pub struct TypeChecker {
pub mut:
	a                            &flat.FlatAst = unsafe { nil }
	fn_ret_types                 map[string]Type
	fn_param_types               map[string][]Type
	fn_shared_params             map[string][]bool
	fn_ret_type_texts            map[string]string   // generic struct method key -> original return type text (e.g. `Box[T].clone` -> `Box[T]`)
	fn_param_type_texts          map[string][]string // generic struct method key -> original param type texts (receiver first)
	fn_type_files                map[string]string
	fn_type_modules              map[string]string
	fn_generic_params            map[string][]string
	specialized_generic_fns      map[string]bool
	fn_variadic                  map[string]bool
	c_variadic_fns               map[string]bool
	fn_implicit_veb_ctx          map[string]bool
	receiver_method_suffix_index map[string]string
	structs                      map[string][]StructField
	struct_modules               map[string]string
	struct_files                 map[string]string
	soa_structs                  map[string]bool
	// set of `${file}\x01${module}\x01${name}` keys for every source-level
	// struct/type/interface/enum declaration, built once in `collect`. Replaces
	// the former full-node scan in `source_declares_type_in_scope`, which was
	// O(nodes) per call and dominated check/transform/cgen (called via qualify_name).
	declared_type_scope_keys           map[string]bool
	struct_error_embeds_shadow_builtin map[string]bool
	struct_generic_params              map[string][]string // generic struct base name -> type-param names (e.g. Vec4 -> [T])
	struct_implements                  map[string][]string
	struct_shared_fields               map[string]bool
	struct_field_c_abi_fns             map[string]string
	// concrete `Box[int].method` -> substituted CallInfo for a method *value* on a
	// generic receiver. The open `Box[T].method` registration is gone by cgen time, so
	// the checker stashes the resolved signature here for gen_method_value_closure.
	generic_method_value_info  map[string]CallInfo
	params_structs             map[string]bool
	unions                     map[string]bool
	type_aliases               map[string]string
	type_alias_c_abi_fns       map[string]string
	sum_types                  map[string][]string
	sum_generic_params         map[string][]string // generic sum type base name -> type-param names (e.g. Tree -> [T])
	enum_names                 map[string]bool
	enum_fields                map[string][]string
	flag_enums                 map[string]bool
	interface_names            map[string]bool
	interface_fields           map[string][]StructField
	interface_embeds           map[string][]string
	interface_abstract_methods map[string][]string // iface -> abstract (declared) method names

	c_globals               map[string]Type
	const_types             map[string]Type
	const_exprs             map[string]flat.NodeId
	const_modules           map[string]string
	const_files             map[string]string
	const_suffixes          map[string]string // dot-suffix -> full const key (O(1) lookup; '' if ambiguous)
	imports                 map[string]string // alias -> short module name
	file_imports            map[string]string
	file_selective_imports  map[string][]string
	file_imports_by_file    map[string]&FileImportInfo
	file_modules            map[string]string
	file_scope              &Scope = unsafe { nil }
	cur_scope               &Scope = unsafe { nil }
	scope_pool              []&Scope
	scope_pool_index        int
	has_builtins            bool
	cur_module              string
	cur_file                string
	errors                  []TypeError
	resolved_call_names     []string // node_id -> resolved function name
	resolved_call_set       []bool
	resolved_fn_value_names []string // node_id -> resolved function value name
	resolved_fn_value_set   []bool
	statement_nodes         []bool
	// Methods used as *values* (`recv.method` passed as a callback), recorded per enclosing
	// function during semantic checking — which has full scope/type info, runs before
	// markused, and (unlike a call) routes a value-context selector through check_selector.
	// markused seeds these (keeping the wrapper-only method out of the dead-code pruner)
	// only when their enclosing function is reachable.
	method_values_by_fn map[int][]string // enclosing fn node id -> method-value `Type.method` keys
	// Local variables bound to a method value (`cb := c.report`) in the current function.
	// Such an alias shares the same per-site static receiver slot as the bare selector, so it
	// escapes (and corrupts other live callbacks) just like `return c.report`; the escape
	// checks below treat a reference to one of these locals as a method value. Reset per fn.
	method_value_locals map[string]bool
	// Scope depth at which each method-value local was marked, so a reassignment to a
	// non-method value only clears the marker when it dominates later uses (same-or-shallower
	// scope); a reassignment in a deeper conditional/loop scope keeps the maybe-method marker.
	method_value_local_depth        map[string]int
	cur_fn_node_id                  int = -1
	cur_fn_mut_param_base_types     map[string]Type
	cur_fn_mut_param_binding_owners map[string]ScopeBindingOwner
	cur_fn_mut_local_binding_owners map[string]ScopeBindingOwner
	cur_fn_shared_binding_owners    map[string]ScopeBindingOwner
	cur_comptime_variant_loop_vars  []string
	expr_type_values                []Type // node_id -> complex/contextual resolved type
	expr_type_set                   []bool
	checking_nodes                  []bool
	parallel_check_sparse           bool
	// Node id range [check_range_lo, check_range_hi] of the fn item currently
	// being checked. Fn subtrees are disjoint contiguous ranges (each fn_decl at
	// index i owns (prev_top_level_idx, i]), so while parallel_check_sparse is
	// set, cache entries for in-range ids are written straight into the shared
	// node-indexed arrays (this checker is the range's only writer) and only
	// out-of-range ids (consts, other decls' nodes) go through the private
	// sparse maps that are merged after join.
	check_range_lo                int = -1
	check_range_hi                int = -1
	sparse_resolved_call_names    map[int]string
	sparse_resolved_fn_values     map[int]string
	sparse_statement_nodes        map[int]bool
	sparse_expr_type_values       map[int]Type
	sparse_checking_nodes         map[int]bool
	diagnose_unknown_calls        bool
	reject_unlowered_map_mutation bool
	reject_unsupported_generics   bool
	diagnostic_files              map[string]bool
	has_spawn_expr                int = -1
	inactive_top_level_node_ids   []int
	selected_file_called_fns      map[string]bool
	// Names newly inserted into selected_file_called_fns and not yet chased by
	// the transitive closure in collect_selected_file_called_fns_transitively.
	selected_file_worklist []string
	// During the parallel check region the called-fns closure is computed on its
	// own thread, so `selected_file_called_fns` is not yet available. Sites that
	// would gate on it park their candidate error here instead; the master
	// filters the list against the finished set after joining the thread.
	defer_ierror_gating   bool
	pending_ierror_errors []PendingIerrorError
	// Node indices of every top-level declaration node (file markers, module/import
	// decls, type-level decls, consts, globals, fn/c-fn decls), in AST order. These
	// kinds only occur at the top level, so a pass iterating this index visits the
	// same nodes in the same order as a full `a.nodes` scan that matches on them —
	// without streaming the ~100x larger node array each time. Built once in
	// `collect`; no later phase of the check step appends declarations. Phases
	// after the check (transform) may grow the AST: top_level_idx_nodes_len
	// records the node count the index covers.
	top_level_idx           []int
	top_level_idx_nodes_len int
	cur_fn_ret_type         Type = Type(void_)
	expected_expr_id        int  = -1
	expected_expr_type      Type = Type(void_)
	smartcasts              map[string]Type
	ownership               &OwnershipState = unsafe { nil }
	selfhost                bool
	// resolution_type_mode is enabled only after semantic checking, while transform
	// and codegen read synthesized generic-specialization type text. Source annotations
	// must keep normal module scoping and never enable this fallback.
	resolution_type_mode bool
	// fork_overlay is non-nil only on parallel-transform worker forks; see
	// TransformForkOverlay and fork_for_parallel_transform.
	fork_overlay &TransformForkOverlay = unsafe { nil }
mut:
	type_cache &TypeCache = unsafe { nil }
}

// new creates a TypeChecker value for types.
pub fn TypeChecker.new(a &flat.FlatAst) TypeChecker {
	fs := new_scope(unsafe { nil })
	return TypeChecker{
		a:                                  a
		fn_ret_types:                       map[string]Type{}
		fn_param_types:                     map[string][]Type{}
		fn_shared_params:                   map[string][]bool{}
		fn_ret_type_texts:                  map[string]string{}
		fn_param_type_texts:                map[string][]string{}
		fn_type_files:                      map[string]string{}
		fn_type_modules:                    map[string]string{}
		fn_generic_params:                  map[string][]string{}
		specialized_generic_fns:            map[string]bool{}
		fn_variadic:                        map[string]bool{}
		c_variadic_fns:                     map[string]bool{}
		fn_implicit_veb_ctx:                map[string]bool{}
		receiver_method_suffix_index:       map[string]string{}
		structs:                            map[string][]StructField{}
		struct_modules:                     map[string]string{}
		struct_files:                       map[string]string{}
		soa_structs:                        map[string]bool{}
		declared_type_scope_keys:           map[string]bool{}
		struct_error_embeds_shadow_builtin: map[string]bool{}
		struct_generic_params:              map[string][]string{}
		struct_implements:                  map[string][]string{}
		struct_shared_fields:               map[string]bool{}
		struct_field_c_abi_fns:             map[string]string{}
		generic_method_value_info:          map[string]CallInfo{}
		params_structs:                     map[string]bool{}
		unions:                             map[string]bool{}
		type_aliases:                       map[string]string{}
		type_alias_c_abi_fns:               map[string]string{}
		sum_types:                          map[string][]string{}
		sum_generic_params:                 map[string][]string{}
		enum_names:                         map[string]bool{}
		enum_fields:                        map[string][]string{}
		flag_enums:                         map[string]bool{}
		interface_names:                    map[string]bool{}
		interface_fields:                   map[string][]StructField{}
		interface_embeds:                   map[string][]string{}
		interface_abstract_methods:         map[string][]string{}
		c_globals:                          map[string]Type{}
		const_types:                        map[string]Type{}
		const_exprs:                        map[string]flat.NodeId{}
		const_modules:                      map[string]string{}
		const_files:                        map[string]string{}
		const_suffixes:                     map[string]string{}
		imports:                            map[string]string{}
		file_imports:                       map[string]string{}
		file_selective_imports:             map[string][]string{}
		file_imports_by_file:               map[string]&FileImportInfo{}
		file_modules:                       map[string]string{}
		file_scope:                         fs
		cur_scope:                          fs
		// The node-indexed cache arrays start empty: collect() sizes them via
		// reset_node_caches (allocating them here too paid for everything
		// twice), and extend_node_caches grows them on demand for any checker
		// used without a collect() call.
		resolved_call_names:             []string{}
		resolved_call_set:               []bool{}
		resolved_fn_value_names:         []string{}
		resolved_fn_value_set:           []bool{}
		statement_nodes:                 []bool{}
		method_values_by_fn:             map[int][]string{}
		method_value_locals:             map[string]bool{}
		method_value_local_depth:        map[string]int{}
		cur_fn_mut_param_base_types:     map[string]Type{}
		cur_fn_mut_param_binding_owners: map[string]ScopeBindingOwner{}
		cur_fn_mut_local_binding_owners: map[string]ScopeBindingOwner{}
		cur_fn_shared_binding_owners:    map[string]ScopeBindingOwner{}
		expr_type_values:                []Type{}
		expr_type_set:                   []bool{}
		checking_nodes:                  []bool{}
		sparse_resolved_call_names:      map[int]string{}
		sparse_resolved_fn_values:       map[int]string{}
		sparse_statement_nodes:          map[int]bool{}
		sparse_expr_type_values:         map[int]Type{}
		sparse_checking_nodes:           map[int]bool{}
		diagnostic_files:                map[string]bool{}
		selected_file_called_fns:        map[string]bool{}
		smartcasts:                      map[string]Type{}
		type_cache:                      &TypeCache{
			parse_entries:              map[string]Type{}
			c_entries:                  map[string]string{}
			struct_field_entries:       map[string]Type{}
			struct_field_misses:        map[string]bool{}
			ierror_compat_entries:      map[string]int{}
			source_error_embed_entries: map[string]int{}
		}
	}
}

// fork_for_parallel_transform returns a TypeChecker that shares all of `tc`'s
// read-only data (semantic maps and node-indexed cache arrays, which the transform
// pass only reads) but owns a fresh, private `type_cache` and a private AST view.
// During transform the only hidden mutation a TypeChecker performs through its `&`
// receiver is memoization into `type_cache` (parse_type / c_type); giving each
// worker its own cache makes concurrent use race-free without cloning the large
// semantic maps. `ast` must be the worker's own (cloned) FlatAst so that any
// expr_type lookup on a freshly-created node id indexes a valid array.
pub fn (tc &TypeChecker) fork_for_parallel_transform(ast &flat.FlatAst) &TypeChecker {
	mut forked := *tc
	forked.a = ast
	// The transformer propagates call/fn-value resolution metadata onto the call
	// nodes it clones (Transformer.copy_cloned_resolution). In a worker those
	// writes must not touch (or grow/realloc) the shared node-indexed arrays
	// while other threads read them, so each fork gets a private sparse overlay:
	// writes go only to the overlay, reads check it before the shared arrays,
	// and merge_worker replays the entries into the master under shifted ids.
	forked.fork_overlay = &TransformForkOverlay{}
	forked.type_cache = &TypeCache{
		// When the master froze its warm cache behind an overlay (see
		// freeze_type_cache_for_forks), every fork shares that frozen cache as
		// its read-only base instead of re-deriving each memoized type.
		base:                       if tc.type_cache != unsafe { nil } {
			tc.type_cache.base
		} else {
			&TypeCache(unsafe { nil })
		}
		parse_enabled:              if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries:              map[string]Type{}
		c_entries:                  map[string]string{}
		struct_field_entries:       map[string]Type{}
		struct_field_misses:        map[string]bool{}
		ierror_compat_entries:      map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
	return &forked
}

// freeze_type_cache_for_forks freezes this checker's warm type cache as the
// shared read-only base for parallel forks (fork_for_parallel_transform picks
// it up) and switches the checker itself to a private overlay so its own
// memoization writes cannot race fork reads. Callable on a shared reference:
// the transformer holds the checker as `&TypeChecker`.
pub fn (tc &TypeChecker) freeze_type_cache_for_forks() {
	mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
	mtc.install_type_cache_overlay()
}

// unfreeze_type_cache_after_forks folds the private overlay back into the
// frozen base once every fork has been joined, and reattaches the base as the
// live cache.
pub fn (tc &TypeChecker) unfreeze_type_cache_after_forks() {
	mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
	mtc.restore_type_cache_base()
}

// set_fresh_type_cache attaches a new empty TypeCache. Parallel-cgen worker
// checkers use this so the lazily-built lookup indexes and memoizations work
// per worker instead of falling back to their uncached full scans.
pub fn (mut tc TypeChecker) set_fresh_type_cache(parse_enabled bool) {
	tc.type_cache = &TypeCache{
		parse_enabled: parse_enabled
	}
}

// set_fresh_type_cache_based_on attaches a new empty TypeCache that falls back
// read-only to `src`'s frozen base cache (see freeze_type_cache_for_forks), so
// parallel-cgen workers start with every type memoized by the check/transform
// phases instead of re-deriving them from a cold cache.
pub fn (mut tc TypeChecker) set_fresh_type_cache_based_on(src &TypeChecker, parse_enabled bool) {
	base := if !isnil(src.type_cache) { src.type_cache.base } else { &TypeCache(unsafe { nil }) }
	tc.type_cache = &TypeCache{
		parse_enabled: parse_enabled
		base:          base
	}
}

// type_cache_parse_enabled reports whether parse_type memoization is enabled
// on this checker's type cache.
pub fn (tc &TypeChecker) type_cache_parse_enabled() bool {
	return !isnil(tc.type_cache) && tc.type_cache.parse_enabled
}

pub fn (tc &TypeChecker) clear_field_lookup_cache() {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := tc.type_cache
	cache.struct_field_entries.clear()
	cache.struct_field_misses.clear()
}

// free_parallel_transform_caches releases private memoization maps owned by a forked
// transform checker and leaves it valid if it is accidentally read again.
pub fn (mut tc TypeChecker) free_parallel_transform_caches() {
	parse_enabled := if tc.type_cache != unsafe { nil } {
		tc.type_cache.parse_enabled
	} else {
		false
	}
	if tc.type_cache != unsafe { nil } {
		unsafe {
			tc.type_cache.parse_entries.free()
			tc.type_cache.c_entries.free()
			tc.type_cache.struct_field_entries.free()
			tc.type_cache.struct_field_misses.free()
			tc.type_cache.ierror_compat_entries.free()
			tc.type_cache.source_error_embed_entries.free()
		}
	}
	tc.type_cache = &TypeCache{
		parse_enabled:              parse_enabled
		parse_entries:              map[string]Type{}
		c_entries:                  map[string]string{}
		struct_field_entries:       map[string]Type{}
		struct_field_misses:        map[string]bool{}
		ierror_compat_entries:      map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
}

// reset_node_caches updates reset node caches state for types.
fn (mut tc TypeChecker) reset_node_caches(n int) {
	tc.resolved_call_names = []string{len: n}
	tc.resolved_call_set = []bool{len: n}
	tc.resolved_fn_value_names = []string{len: n}
	tc.resolved_fn_value_set = []bool{len: n}
	tc.statement_nodes = []bool{len: n}
	// No init fill: every read of expr_type_values is guarded by expr_type_set,
	// so unset slots are never returned, and skipping the ~1M-element fill loop
	// keeps this a plain zeroed allocation.
	tc.expr_type_values = unsafe { []Type{len: n} }
	tc.expr_type_set = []bool{len: n}
	tc.checking_nodes = []bool{len: n}
	tc.parallel_check_sparse = false
}

fn (mut tc TypeChecker) extend_node_caches(n int) {
	if tc.parallel_check_sparse {
		return
	}
	if n <= tc.resolved_call_names.len && n <= tc.resolved_fn_value_names.len
		&& n <= tc.statement_nodes.len && n <= tc.expr_type_values.len && n <= tc.checking_nodes.len {
		return
	}
	extend_string_cache(mut tc.resolved_call_names, n)
	extend_bool_cache(mut tc.resolved_call_set, n)
	extend_string_cache(mut tc.resolved_fn_value_names, n)
	extend_bool_cache(mut tc.resolved_fn_value_set, n)
	extend_bool_cache(mut tc.statement_nodes, n)
	extend_type_cache(mut tc.expr_type_values, n)
	extend_bool_cache(mut tc.expr_type_set, n)
	extend_bool_cache(mut tc.checking_nodes, n)
}

fn extend_string_cache(mut values []string, n int) {
	if n > values.len {
		values << []string{len: n - values.len}
	}
}

fn extend_bool_cache(mut values []bool, n int) {
	if n > values.len {
		values << []bool{len: n - values.len}
	}
}

fn extend_type_cache(mut values []Type, n int) {
	if n > values.len {
		values << []Type{len: n - values.len, init: Type(void_)}
	}
}

// push_scope updates push scope state for TypeChecker.
pub fn (mut tc TypeChecker) push_scope() {
	tc.cur_scope = tc.reuse_scope(tc.cur_scope)
	$if ownership ? {
		tc.ownership_push_scope()
	}
}

// pop_scope updates pop scope state for TypeChecker.
pub fn (mut tc TypeChecker) pop_scope() {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	parent := tc.cur_scope.parent
	if parent == unsafe { nil } {
		return
	}
	$if ownership ? {
		tc.ownership_run_scope_defers()
		tc.ownership_pop_scope()
	}
	if tc.scope_pool_index > 0 && tc.cur_scope == tc.scope_pool[tc.scope_pool_index - 1] {
		tc.scope_pool_index--
	}
	tc.cur_scope = parent
}

// reuse_scope supports reuse scope handling for TypeChecker.
fn (mut tc TypeChecker) reuse_scope(parent &Scope) &Scope {
	if tc.scope_pool_index < tc.scope_pool.len {
		mut scope := tc.scope_pool[tc.scope_pool_index]
		scope.reset(parent)
		tc.scope_pool_index++
		return scope
	}
	scope := new_scope(parent)
	tc.scope_pool << scope
	tc.scope_pool_index++
	return scope
}

// record_error supports record error handling for TypeChecker.
fn (mut tc TypeChecker) record_error(kind TypeErrorKind, msg string, node flat.NodeId) {
	if !tc.should_diagnose(node) {
		return
	}
	tc.errors << TypeError{
		msg:        msg
		kind:       kind
		node:       node
		file:       tc.cur_file
		node_kind:  if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].kind.str()
		} else {
			''
		}
		node_value: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].value
		} else {
			''
		}
		node_pos:   if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].pos.str()
		} else {
			''
		}
	}
}

fn (mut tc TypeChecker) record_error_unfiltered(kind TypeErrorKind, msg string, node flat.NodeId) {
	tc.errors << tc.make_type_error(kind, msg, node)
}

fn (tc &TypeChecker) make_type_error(kind TypeErrorKind, msg string, node flat.NodeId) TypeError {
	return TypeError{
		msg:        msg
		kind:       kind
		node:       node
		file:       tc.cur_file
		node_kind:  if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].kind.str()
		} else {
			''
		}
		node_value: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].value
		} else {
			''
		}
		node_pos:   if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].pos.str()
		} else {
			''
		}
	}
}

fn (mut tc TypeChecker) record_unsupported_generic(msg string, node flat.NodeId) {
	if !tc.should_diagnose_unsupported_generic(node) {
		return
	}
	tc.errors << TypeError{
		msg:  msg
		kind: .unsupported_generic
		node: node
	}
}

fn split_sum_variant_texts(text string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	for i in 0 .. text.len {
		ch := text[i]
		if ch == `[` || ch == `(` || ch == `{` {
			depth++
		} else if ch == `]` || ch == `)` || ch == `}` {
			if depth > 0 {
				depth--
			}
		} else if ch == `|` && depth == 0 {
			part := text[start..i].trim_space()
			if part.len > 0 {
				parts << part
			}
			start = i + 1
		}
	}
	part := text[start..].trim_space()
	if part.len > 0 {
		parts << part
	}
	return parts
}

// collect supports collect handling for TypeChecker.
pub fn (mut tc TypeChecker) collect(a &flat.FlatAst) {
	tc.a = a
	tc.has_spawn_expr = -1
	tc.file_scope = new_scope(unsafe { nil })
	tc.cur_scope = tc.file_scope
	tc.scope_pool_index = 0
	tc.reset_node_caches(a.nodes.len)
	$if ownership ? {
		tc.ownership_reset()
	}
	tc.type_cache = &TypeCache{
		parse_entries:              map[string]Type{}
		c_entries:                  map[string]string{}
		struct_field_entries:       map[string]Type{}
		struct_field_misses:        map[string]bool{}
		ierror_compat_entries:      map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
	// One full declaration scan: build the top-level declaration index that every
	// later pass of the check step iterates instead of re-streaming all nodes,
	// detect builtins, and index every source-level type declaration by
	// (file, module, name) so `source_declares_type_in_scope` is an O(1) map
	// lookup instead of a full node scan. The type-scope index is built before
	// pass 1 because pass 1 already calls qualify_name, which depends on it.
	// No later phase adds declarations, so both indexes stay complete for the
	// whole compile.
	tc.declared_type_scope_keys = map[string]bool{}
	tc.top_level_idx = []int{cap: 65536}
	tc.prepare_threads_condition()
	inactive_comptime_nodes := tc.inactive_top_level_comptime_nodes()
	tc.inactive_top_level_node_ids.clear()
	mut idx_file := ''
	mut idx_module := ''
	for i, node in a.nodes {
		if inactive_comptime_nodes.len > 0 && inactive_comptime_nodes[i] {
			tc.inactive_top_level_node_ids << i
			continue
		}
		match node.kind {
			.file {
				idx_file = node.value
				idx_module = ''
				tc.top_level_idx << i
			}
			.module_decl {
				idx_module = node.value
				tc.top_level_idx << i
			}
			.struct_decl {
				if node.value == 'string' {
					tc.has_builtins = true
				}
				tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, node.value)] = true
				tc.top_level_idx << i
			}
			.type_decl, .interface_decl, .enum_decl {
				tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, node.value)] = true
				tc.top_level_idx << i
			}
			.import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl {
				tc.top_level_idx << i
			}
			else {}
		}
	}
	tc.top_level_idx_nodes_len = a.nodes.len
	// Pass 1: collect type-level names (aliases, enums, sum types)
	for tl_idx in tc.top_level_idx {
		node := a.nodes[tl_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.import_decl {
				tc.imports[node.typ] = node.value
				tc.register_file_import(node.typ, node.value)
				tc.register_selective_imports(node)
			}
			.enum_decl {
				qn := tc.qualify_name(node.value)
				tc.enum_names[qn] = true
				mut fields := []string{}
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind == .enum_field {
						fields << f.value
					}
				}
				tc.enum_fields[qn] = fields
				if node.typ == 'flag' {
					tc.flag_enums[qn] = true
				}
			}
			.struct_decl {
				qname := tc.qualify_name(node.value)
				if qname !in tc.structs {
					tc.structs[qname] = []StructField{}
				}
				tc.struct_modules[qname] = tc.cur_module
				tc.struct_files[qname] = tc.cur_file
				if node.generic_params.len > 0 {
					tc.struct_generic_params[qname] = node.generic_params.clone()
					if qname != node.value {
						tc.struct_generic_params[node.value] = node.generic_params.clone()
					}
				}
				implements_types := struct_decl_implements_from_typ(node.typ)
				if implements_types.len > 0 {
					tc.struct_implements[qname] = implements_types
				}
				if 'union' in node.typ.split(',') {
					tc.unions[qname] = true
				}
				if 'params' in node.typ.split(',') {
					tc.params_structs[qname] = true
				}
				if 'soa' in node.typ.split(',') {
					tc.soa_structs[qname] = true
				}
			}
			.type_decl {
				if node.children_count > 0 {
					mut variants := []string{}
					for i in 0 .. node.children_count {
						v := a.child_node(&node, i)
						variants << tc.qualify_sum_variant_name(v.value, node.generic_params)
					}
					qname := tc.qualify_name(node.value)
					tc.sum_types[qname] = variants
					if node.generic_params.len > 0 {
						tc.sum_generic_params[qname] = node.generic_params.clone()
						if qname != node.value {
							tc.sum_generic_params[node.value] = node.generic_params.clone()
						}
					}
				} else if node.typ.len > 0 {
					sum_variant_texts := split_sum_variant_texts(node.typ)
					if sum_variant_texts.len > 1 {
						mut variants := []string{}
						for part in sum_variant_texts {
							variants << tc.qualify_sum_variant_name(part, node.generic_params)
						}
						qname := tc.qualify_name(node.value)
						tc.sum_types[qname] = variants
						if node.generic_params.len > 0 {
							tc.sum_generic_params[qname] = node.generic_params.clone()
							if qname != node.value {
								tc.sum_generic_params[node.value] = node.generic_params.clone()
							}
						}
						continue
					}
					qname := tc.qualify_name(node.value)
					tc.type_aliases[qname] = tc.qualify_type_text(node.typ)
					if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(node.typ) {
						tc.type_alias_c_abi_fns[qname] = c_abi_fn
					}
					if tc.cur_module in ['', 'main', 'builtin'] && node.value !in tc.type_aliases {
						tc.type_aliases[node.value] = tc.qualify_type_text(node.typ)
						if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(node.typ) {
							tc.type_alias_c_abi_fns[node.value] = c_abi_fn
						}
					}
				}
			}
			.interface_decl {
				tc.interface_names[tc.qualify_name(node.value)] = true
			}
			else {}
		}
	}
	tc.check_c_struct_redeclarations(a)
	// Pass 2: collect struct fields, function signatures (type aliases now available)
	tc.type_cache.parse_enabled = true
	tc.cur_module = ''
	for tl_idx in tc.top_level_idx {
		node := a.nodes[tl_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := tc.qualify_fn_name(node.value)
				is_open_generic := node.generic_params.len > 0 || node.value.contains('[')
				ret_type := if is_open_generic {
					tc.parse_scope_param_type(node.typ)
				} else {
					tc.parse_type(node.typ)
				}
				mut ptypes := []Type{}
				mut param_texts := []string{}
				mut shared_params := []bool{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind == .param {
						if child.typ.starts_with('...') {
							is_variadic = true
						}
						ptypes << if is_open_generic {
							tc.parse_scope_param_type(child.typ)
						} else {
							tc.parse_type(child.typ)
						}
						param_texts << child.typ
						shared_params << param_type_text_is_shared(child.typ)
					}
				}
				needs_ctx := tc.fn_needs_implicit_veb_ctx(node)
				ptypes = tc.fn_param_types_with_implicit_veb_ctx(node, ptypes)
				shared_params = tc.fn_shared_params_with_implicit_veb_ctx(node, shared_params)
				tc.register_fn_signature(qname, ret_type, ptypes, shared_params, is_variadic,
					needs_ctx)
				if tc.cur_module in ['', 'main', 'builtin'] && qname != node.value
					&& node.value !in tc.fn_param_types {
					tc.register_fn_signature(node.value, ret_type, ptypes, shared_params,
						is_variadic, needs_ctx)
				}
				// A generic struct method (`Box[T].clone`) keeps its original signature
				// TEXT: the parsed types collapse a non-concrete `Box[T]` to the bare base,
				// so a concrete call must re-substitute the type arguments from the text to
				// recover applications like the receiver type in the return position
				// (`Box[T]` -> `Box[int]`). Only such methods carry `[` in their key.
				if node.generic_params.len > 0 || node.value.contains('[') {
					for name in [qname, node.value] {
						tc.fn_ret_type_texts[name] = node.typ
						tc.fn_param_type_texts[name] = param_texts.clone()
						tc.fn_type_files[name] = tc.cur_file
						tc.fn_type_modules[name] = tc.cur_module
						if node.generic_params.len > 0 {
							tc.fn_generic_params[name] = node.generic_params.clone()
						}
					}
				}
			}
			.struct_decl {
				mut fields := []StructField{}
				mut field_c_abi_fns := map[string]string{}
				mut shared_field_names := []string{}
				mut shadows_builtin_error_embed := false
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind != .field_decl {
						continue
					}
					field_typ := if f.typ.len > 0 { f.typ } else { f.value }
					field_is_embed := source_field_decl_is_embed(f, field_typ)
					shadows_builtin_error := field_is_embed
						&& field_typ in ['Error', 'MessageError']
						&& tc.unqualified_type_name_shadows_builtin_in_scope(field_typ, tc.cur_file, tc.cur_module)
					if shadows_builtin_error {
						shadows_builtin_error_embed = true
					}
					mut typ := tc.parse_type(field_typ)
					if field_is_embed && field_typ in ['Error', 'MessageError']
						&& !shadows_builtin_error {
						typ = Type(Struct{
							name: field_typ
						})
					} else if f.value == field_typ || shadows_builtin_error {
						typ = tc.resolve_known_field_type(field_typ, typ)
					}
					if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text(field_typ) {
						field_c_abi_fns[f.value] = c_abi_fn
					}
					if param_type_text_is_shared(field_typ) {
						shared_field_names << f.value
					}
					fields << StructField{
						name: f.value
						typ:  typ
					}
				}
				qname := tc.qualify_name(node.value)
				// A `C.` struct denotes a single external C type, but several modules may
				// mirror it with partial or imprecise field views (e.g. `C.termios` in both
				// `term` and `term.termios`). codegen emits one C struct, so the field table
				// must be a single canonical view. Keep the most complete (superset) view so
				// it is deterministic regardless of module collection order, instead of
				// letting whichever declaration is collected last silently win.
				if qname.starts_with('C.') {
					if existing := tc.structs[qname] {
						if fields.len <= existing.len {
							continue
						}
					}
				}
				tc.structs[qname] = fields
				tc.struct_modules[qname] = tc.cur_module
				tc.struct_files[qname] = tc.cur_file
				if shadows_builtin_error_embed {
					tc.struct_error_embeds_shadow_builtin[qname] = true
				}
				for field_name in shared_field_names {
					tc.struct_shared_fields[struct_field_c_abi_key(qname, field_name)] = true
				}
				for field_name, c_abi_fn in field_c_abi_fns {
					tc.struct_field_c_abi_fns[struct_field_c_abi_key(qname, field_name)] = c_abi_fn
				}
				if 'union' in node.typ.split(',') {
					tc.unions[qname] = true
				}
				if 'params' in node.typ.split(',') {
					tc.params_structs[qname] = true
				}
				if 'soa' in node.typ.split(',') {
					tc.soa_structs[qname] = true
				}
			}
			.c_fn_decl {
				ret_type := tc.parse_type(node.typ)
				mut ptypes := []Type{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind == .param {
						if child.typ.starts_with('...') {
							is_variadic = true
						}
						ptypes << tc.parse_type(child.typ)
					}
				}
				tc.register_fn_signature(node.value, ret_type, ptypes, []bool{}, is_variadic, false)
				if is_variadic {
					tc.register_c_variadic_fn(node.value)
				}
				if !node.value.starts_with('C.') {
					tc.register_fn_signature('C.${node.value}', ret_type, ptypes, []bool{},
						is_variadic, false)
					if is_variadic {
						tc.register_c_variadic_fn('C.${node.value}')
					}
				}
			}
			.interface_decl {
				iface_name := tc.qualify_name(node.value)
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind != .interface_field {
						continue
					}
					if f.op == .dot {
						mname := '${iface_name}.${f.value}'
						mut absm := tc.interface_abstract_methods[iface_name] or { []string{} }
						absm << f.value
						tc.interface_abstract_methods[iface_name] = absm
						ret_type := tc.parse_type(f.typ)
						mut ptypes := []Type{}
						mut shared_params := []bool{}
						mut is_variadic := false
						ptypes << Type(Pointer{
							base_type: Type(Interface{
								name: iface_name
							})
						})
						shared_params << false
						for j in 0 .. f.children_count {
							child := a.child_node(f, j)
							if child.kind == .param {
								if child.typ.starts_with('...') {
									is_variadic = true
								}
								ptypes << tc.parse_type(child.typ)
								shared_params << param_type_text_is_shared(child.typ)
							}
						}
						tc.register_fn_name_alias(mname, ret_type, ptypes, shared_params,
							is_variadic, false)
					} else if f.typ.len > 0 {
						mut fields := tc.interface_fields[iface_name] or { []StructField{} }
						fields << StructField{
							name: f.value
							typ:  tc.parse_type(f.typ)
						}
						tc.interface_fields[iface_name] = fields
					} else if f.value.len > 0 {
						mut embeds := tc.interface_embeds[iface_name] or { []string{} }
						embeds << tc.qualify_name(f.value)
						tc.interface_embeds[iface_name] = embeds
					}
				}
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.value.len > 0 && f.value.starts_with('C.') {
						tc.c_globals[f.value] = tc.parse_type(f.typ)
					} else if f.value.len > 0 {
						mut ft := tc.parse_type(f.typ)
						if ft is Void && f.children_count > 0 {
							ft = tc.resolve_type(a.child(f, 0))
						}
						qname := tc.qualify_name(f.value)
						tc.file_scope.insert(qname, ft)
					}
				}
			}
			.const_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.kind == .const_field && f.children_count > 0 {
						qname := tc.qualify_name(f.value)
						tc.const_types[qname] = unknown_type('pending const `${qname}`')
						tc.const_exprs[qname] = a.child(f, 0)
						tc.const_modules[qname] = tc.cur_module
						tc.const_files[qname] = tc.cur_file
					} else if f.kind == .const_field && f.typ.len > 0 {
						qname := tc.qualify_name(f.value)
						tc.const_types[qname] = tc.parse_type(f.typ)
						tc.const_modules[qname] = tc.cur_module
						tc.const_files[qname] = tc.cur_file
					}
				}
			}
			else {}
		}
	}
	tc.resolve_inferred_global_types(a)
	tc.resolve_const_types()
	tc.build_const_suffixes()
	$if ownership ? {
		tc.ownership_after_collect()
	}
}

fn (mut tc TypeChecker) resolve_inferred_global_types(a &flat.FlatAst) {
	tc.cur_module = ''
	tc.cur_file = ''
	for tl_idx in tc.top_level_idx {
		node := a.nodes[tl_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(&node, i)
					if f.value.len == 0 || f.value.starts_with('C.') || f.typ.len > 0
						|| f.children_count == 0 {
						continue
					}
					qname := tc.qualify_name(f.value)
					existing := tc.file_scope.lookup(qname) or { Type(void_) }
					if existing !is Unknown && existing !is Void {
						continue
					}
					ft := tc.resolve_type(a.child(f, 0))
					if ft is Unknown || ft is Void {
						continue
					}
					tc.file_scope.insert(qname, ft)
				}
			}
			else {}
		}
	}
}

fn (mut tc TypeChecker) check_c_struct_redeclarations(a &flat.FlatAst) {
	mut c_struct_decl_sigs := map[string]string{}
	mut c_struct_decl_files := map[string]string{}
	mut c_struct_decl_modules := map[string]string{}
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.struct_decl {
				qname := tc.qualify_name(node.value)
				if !qname.starts_with('C.') {
					continue
				}
				c_struct_sig := tc.c_struct_decl_signature(a, node)
				if qname in c_struct_decl_sigs {
					existing_sig := c_struct_decl_sigs[qname]
					if !c_struct_decl_signatures_compatible(existing_sig, c_struct_sig) {
						existing_file := c_struct_decl_files[qname] or { '' }
						existing_module := c_struct_decl_modules[qname] or { '' }
						if !tc.c_struct_redeclaration_allowed(qname, existing_file, tc.cur_file,
							existing_module, tc.cur_module) {
							tc.record_error_unfiltered(.duplicate_decl,
								'cannot redeclare C struct `${qname}`', flat.NodeId(node_idx))
						}
					}
					existing_fields := c_struct_decl_signature_field_count(existing_sig)
					current_fields := c_struct_decl_signature_field_count(c_struct_sig)
					if current_fields > existing_fields {
						c_struct_decl_sigs[qname] = c_struct_sig
						c_struct_decl_files[qname] = tc.cur_file
						c_struct_decl_modules[qname] = tc.cur_module
					}
				} else {
					c_struct_decl_sigs[qname] = c_struct_sig
					c_struct_decl_files[qname] = tc.cur_file
					c_struct_decl_modules[qname] = tc.cur_module
				}
			}
			else {}
		}
	}
}

fn (tc &TypeChecker) c_struct_redeclaration_allowed(qname string, first_file string, second_file string, first_module string, second_module string) bool {
	if qname == 'C.termios' && tc.c_struct_decl_is_vlib_termios_shim(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_termios_shim(second_file, second_module) {
		return true
	}
	if qname == 'C.cJSON' && tc.c_struct_decl_is_vlib_cjson(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_cjson(second_file, second_module) {
		return true
	}
	return false
}

fn (tc &TypeChecker) c_struct_decl_is_vlib_termios_shim(file string, module_name string) bool {
	if module_name !in ['term', 'termios', 'term.termios'] {
		return false
	}
	normalized := file.replace('\\', '/')
	return normalized.contains('/vlib/term/')
		|| (normalized.contains('/v3_module_cache_')
		&& normalized.all_after_last('/').starts_with('termios_') && normalized.ends_with('.vh'))
}

fn (tc &TypeChecker) c_struct_decl_is_vlib_cjson(file string, module_name string) bool {
	if module_name !in ['json', 'cjson', 'json.cjson'] {
		return false
	}
	normalized := file.replace('\\', '/')
	if normalized.contains('/vlib/json/json_primitives.c.v')
		|| normalized.contains('/vlib/json/cjson/cjson_wrapper.c.v') {
		return true
	}
	base := normalized.all_after_last('/')
	return normalized.contains('/v3_module_cache_') && normalized.ends_with('.vh')
		&& (base.starts_with('json_') || base.starts_with('cjson_'))
}

fn (tc &TypeChecker) c_struct_decl_signature(a &flat.FlatAst, node flat.Node) string {
	mut sig := node.typ
	mut has_fields := false
	for i in 0 .. node.children_count {
		f := a.child_node(&node, i)
		if f.kind != .field_decl {
			continue
		}
		has_fields = true
		field_name := if f.value.starts_with('@') { f.value[1..] } else { f.value }
		field_typ := if f.typ.len > 0 { f.typ } else { f.value }
		sig += '|${field_name}:${tc.c_type(tc.parse_type(field_typ))}'
	}
	if !has_fields {
		return ''
	}
	return sig
}

fn c_struct_decl_signatures_compatible(a string, b string) bool {
	if a.len == 0 || b.len == 0 || a == b {
		return true
	}
	a_parts := a.split('|')
	b_parts := b.split('|')
	if a_parts.len == 0 || b_parts.len == 0 || a_parts[0] != b_parts[0] {
		return false
	}
	if a_parts[0] != 'union' {
		return false
	}
	a_fields := a_parts[1..]
	b_fields := b_parts[1..]
	return c_struct_decl_fields_subset(a_fields, b_fields)
		|| c_struct_decl_fields_subset(b_fields, a_fields)
}

fn c_struct_decl_fields_subset(small []string, big []string) bool {
	for field in small {
		if field !in big {
			return false
		}
	}
	return true
}

fn c_struct_decl_signature_field_count(sig string) int {
	if sig.len == 0 {
		return 0
	}
	parts := sig.split('|')
	if parts.len <= 1 {
		return 0
	}
	return parts.len - 1
}

// build_const_suffixes maps every dot-delimited suffix of each const key to that
// key, so qualified const lookups resolve in O(1) instead of scanning all consts
// (with per-iteration string building) on every selector/ident in module code.
fn (mut tc TypeChecker) build_const_suffixes() {
	for key, _ in tc.const_types {
		mut i := 0
		for i < key.len {
			if key[i] == `.` {
				suffix := key[i + 1..]
				if existing := tc.const_suffixes[suffix] {
					if existing != key {
						tc.const_suffixes[suffix] = ''
					}
				} else {
					tc.const_suffixes[suffix] = key
				}
			}
			i++
		}
	}
}

// const_key_for_suffix returns the const key matching `.${name}` as a suffix,
// equivalent to scanning const_types for `key.ends_with('.' + name)` but O(1).
fn (tc &TypeChecker) const_key_for_suffix(name string) ?string {
	if key := tc.const_suffixes[name] {
		if key.len > 0 {
			return key
		}
	}
	return none
}

// resolve_const_types resolves resolve const types information for types.
fn (mut tc TypeChecker) resolve_const_types() {
	if tc.const_exprs.len == 0 {
		return
	}
	saved_module := tc.cur_module
	saved_file := tc.cur_file
	for _ in 0 .. tc.const_exprs.len {
		mut changed := false
		for name, expr_id in tc.const_exprs {
			tc.cur_module = tc.const_modules[name] or { '' }
			tc.cur_file = tc.const_files[name] or { '' }
			mut new_type := tc.resolve_type(expr_id)
			new_type = tc.const_type_from_initializer(name, new_type)
			if new_type is Unknown {
				continue
			}
			old_type := tc.const_types[name] or { Type(void_) }
			if old_type.name() != new_type.name() {
				tc.const_types[name] = new_type
				changed = true
			}
		}
		if !changed {
			break
		}
	}
	tc.cur_module = saved_module
	tc.cur_file = saved_file
}

// const_type_from_initializer converts const type from initializer data for types.
fn (tc &TypeChecker) const_type_from_initializer(name string, typ Type) Type {
	if typ !is Unknown {
		return typ
	}
	expr_id := tc.const_exprs[name] or { return typ }
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return typ
	}
	expr := tc.a.nodes[int(expr_id)]
	if fn_typ := tc.const_fn_value_type(expr) {
		return fn_typ
	}
	if expr.kind != .call || expr.children_count == 0 {
		return typ
	}
	fn_node := tc.a.child_node(&expr, 0)
	if fn_node.kind != .ident || fn_node.value.len == 0 {
		return typ
	}
	mod_name := tc.const_modules[name] or { '' }
	mut candidates := []string{}
	if mod_name.len > 0 && mod_name != 'main' && mod_name != 'builtin' {
		candidates << '${mod_name}.${fn_node.value}'
	}
	candidates << fn_node.value
	candidates << tc.cached_c_name(fn_node.value)
	for candidate in candidates {
		if ret := tc.fn_ret_types[candidate] {
			return ret
		}
	}
	if fn_node.value == 'new_keywords_matcher_trie' {
		type_name := if mod_name.len > 0 && mod_name != 'main' && mod_name != 'builtin' {
			'${mod_name}.KeywordsMatcherTrie'
		} else {
			'KeywordsMatcherTrie'
		}
		return Type(Struct{
			name: type_name
		})
	}
	return typ
}

fn (tc &TypeChecker) const_fn_value_type(expr flat.Node) ?Type {
	match expr.kind {
		.ident {
			return tc.fn_value_type(expr.value)
		}
		.selector {
			if expr.children_count == 0 {
				return none
			}
			base := tc.a.child_node(&expr, 0)
			if base.kind != .ident {
				return none
			}
			mod_name := tc.resolve_import_alias(base.value) or { base.value }
			return tc.fn_type_from_key('${mod_name}.${expr.value}')
		}
		else {
			return none
		}
	}
}

fn (tc &TypeChecker) const_type_for_name(name string) ?Type {
	qname := tc.qualify_name(name)
	if typ := tc.const_types[qname] {
		return tc.const_type_from_initializer(qname, typ)
	}
	if typ := tc.const_types[name] {
		return tc.const_type_from_initializer(name, typ)
	}
	return none
}

fn (tc &TypeChecker) const_type_for_selector(node flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_node := tc.a.child_node(&node, 0)
	if base_node.kind != .ident {
		return none
	}
	resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
	qname := '${resolved}.${node.value}'
	if typ := tc.const_types[qname] {
		return tc.const_type_from_initializer(qname, typ)
	}
	if key := tc.const_key_for_suffix(qname) {
		typ := tc.const_types[key] or { unknown_type('unknown const `${key}`') }
		return tc.const_type_from_initializer(key, typ)
	}
	return none
}

// selector_const_type returns the declared type for a selector const expression.
pub fn (tc &TypeChecker) selector_const_type(node flat.Node) ?Type {
	return tc.const_type_for_selector(node)
}

// qualify_fn_name supports qualify fn name handling for TypeChecker.
pub fn (tc &TypeChecker) qualify_fn_name(name string) string {
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name
	}
	return '${tc.cur_module}.${name}'
}

fn (tc &TypeChecker) local_bare_fn_key(name string) ?string {
	if name.len == 0 || name.contains('.') {
		return none
	}
	qfn := tc.qualify_fn_name(name)
	if qfn != name {
		if qfn in tc.fn_ret_types {
			return qfn
		}
		return none
	}
	if tc.local_fn_decl_exists(name) && name in tc.fn_ret_types {
		return name
	}
	return none
}

fn (tc &TypeChecker) local_bare_fn_signature_key(name string) ?string {
	key := tc.local_bare_fn_key(name) or { return none }
	if tc.fn_signature_known(key) {
		return key
	}
	return none
}

fn (tc &TypeChecker) local_fn_decl_exists(name string) bool {
	if name.len == 0 || name.contains('.') {
		return false
	}
	if isnil(tc.type_cache) {
		return tc.local_fn_decl_exists_scan(name)
	}
	// Walking every AST node per query is far too slow for a helper on the
	// expression-typing path. Memoize a (module, fn name) set in the private
	// type_cache and extend it incrementally when the AST has grown (transform
	// appends fn_decls), resuming the module tracking where the last build
	// stopped.
	mut cache := tc.type_cache
	if cache.local_fn_decl_indexed_len < tc.a.nodes.len {
		if !isnil(cache.base) && cache.base.local_fn_decl_indexed_len >= tc.a.nodes.len {
			return '${tc.cur_module}\x01${name}' in cache.base.local_fn_decl_index
		}
		mut cur_module := cache.local_fn_decl_last_module
		mut scan_start := cache.local_fn_decl_indexed_len
		if scan_start == 0 && tc.top_level_idx.len > 0 {
			// Initial build: module_decl/fn_decl nodes only occur at the top
			// level, so the check-time index covers its node range without a
			// full scan; nodes appended later (transform) are scanned below.
			for i in tc.top_level_idx {
				node := tc.a.nodes[i]
				match node.kind {
					.module_decl {
						cur_module = node.value
					}
					.fn_decl {
						cache.local_fn_decl_index['${cur_module}\x01${node.value}'] = true
					}
					else {}
				}
			}
			scan_start = tc.top_level_idx_nodes_len
		}
		for i in scan_start .. tc.a.nodes.len {
			node := tc.a.nodes[i]
			match node.kind {
				.module_decl {
					cur_module = node.value
				}
				.fn_decl {
					cache.local_fn_decl_index['${cur_module}\x01${node.value}'] = true
				}
				else {}
			}
		}
		cache.local_fn_decl_last_module = cur_module
		cache.local_fn_decl_indexed_len = tc.a.nodes.len
	}
	return '${tc.cur_module}\x01${name}' in cache.local_fn_decl_index
}

// local_fn_decl_exists_scan is the uncached fallback used when no type_cache is
// attached to the checker.
fn (tc &TypeChecker) local_fn_decl_exists_scan(name string) bool {
	mut cur_module := ''
	for node in tc.a.nodes {
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if cur_module == tc.cur_module && node.value == name {
					return true
				}
			}
			else {}
		}
	}
	return false
}

// qualify_name supports qualify name handling for TypeChecker.
pub fn (tc &TypeChecker) qualify_name(name string) string {
	// Qualify container / wrapper types by recursing into the element type first,
	// so imported dotted names inside `[]T`, `[N]T`, `map[K]V`, `&T`, `?T`, `!T`
	// still get resolved. The `.contains('.')` fast path below only understands the
	// bare `alias.Type` form, so it must not short-circuit these wrappers.
	if name.starts_with('[]') {
		return '[]' + tc.qualify_name(name[2..])
	}
	if name.starts_with('[') {
		idx := name.index_u8(`]`)
		if idx > 0 {
			return name[..idx + 1] + tc.qualify_name(name[idx + 1..])
		}
	}
	if name.starts_with('map[') {
		bracket_end := find_matching_bracket(name, 3)
		key_str := name[4..bracket_end]
		val_str := name[bracket_end + 1..]
		return 'map[${tc.qualify_name(key_str)}]${tc.qualify_name(val_str)}'
	}
	if name.starts_with('&') {
		return '&' + tc.qualify_name(name[1..])
	}
	if name.starts_with('?') {
		return '?' + tc.qualify_name(name[1..])
	}
	if name.starts_with('!') {
		return '!' + tc.qualify_name(name[1..])
	}
	if name.contains('.') {
		return tc.resolve_imported_type_text(name)
	}
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name
	}
	if is_builtin_type_name(name) {
		return name
	}
	if tc.unqualified_type_symbol_is_builtin(name) {
		return name
	}
	return tc.cur_module + '.' + name
}

// qualify_resolution_type_name qualifies a type name for RESOLUTION (not
// registration): a bare capitalized name that does not exist under the current
// module but does exist globally (a generic arg substituted from another
// module's caller, e.g. main's `Foo` inside a json2 specialization) stays bare
// instead of becoming a nonexistent `json2.Foo`. Registration must never use
// this - it is order-dependent during collect.
fn (tc &TypeChecker) qualify_resolution_type_name(name string) string {
	qualified := tc.qualify_name(name)
	if qualified != name && name.len > 0 && name[0] >= `A` && name[0] <= `Z`
		&& !tc.qualify_candidate_type_exists(qualified) && tc.qualify_candidate_type_exists(name) {
		return name
	}
	return qualified
}

fn (tc &TypeChecker) qualify_candidate_type_exists(name string) bool {
	return name in tc.structs || name in tc.sum_types || name in tc.interface_names
		|| name in tc.enum_names || name in tc.type_aliases || name in tc.struct_generic_params
}

fn (tc &TypeChecker) qualify_sum_variant_name(name string, generic_params []string) string {
	if generic_params.len == 0 {
		return tc.qualify_name(name)
	}
	clean := name.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean in generic_params {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + tc.qualify_sum_variant_name(clean[1..], generic_params)
	}
	if clean.starts_with('?') {
		return '?' + tc.qualify_sum_variant_name(clean[1..], generic_params)
	}
	if clean.starts_with('!') {
		return '!' + tc.qualify_sum_variant_name(clean[1..], generic_params)
	}
	if clean.starts_with('...') {
		return '...' + tc.qualify_sum_variant_name(clean[3..], generic_params)
	}
	if clean.starts_with('[]') {
		return '[]' + tc.qualify_sum_variant_name(clean[2..], generic_params)
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := tc.qualify_sum_variant_name(clean[4..bracket_end], generic_params)
			val := tc.qualify_sum_variant_name(clean[bracket_end + 1..], generic_params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + tc.qualify_sum_variant_name(clean[bracket_end +
				1..], generic_params)
		}
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_params(clean[bracket + 1..bracket_end]) {
				parts << tc.qualify_sum_variant_name(part, generic_params)
			}
			return tc.qualify_name(clean[..bracket]) + '[' + parts.join(', ') + ']' +
				clean[bracket_end + 1..]
		}
	}
	return tc.qualify_name(clean)
}

// qualify_type_text qualifies a type text for registration: bare names always
// get the current module prefix (order-independent during collect).
fn (tc &TypeChecker) qualify_type_text(typ string) string {
	return tc.qualify_type_text_impl(typ, false)
}

// qualify_resolution_type_text qualifies a type text in a resolution-only
// context (generic application args): a bare name substituted from another
// module's caller may stay bare when the module-qualified spelling does not
// exist. Never use for registration.
fn (tc &TypeChecker) qualify_resolution_type_text(typ string) string {
	return tc.qualify_type_text_impl(typ, true)
}

// parse_resolution_type parses type text that can mix declaration-local names with concrete
// generic arguments from another module.
pub fn (tc &TypeChecker) parse_resolution_type(typ string) Type {
	qualified := tc.qualify_resolution_type_text(typ)
	mut unscoped := *tc
	unscoped.cur_module = ''
	unscoped.resolution_type_mode = false
	return unscoped.parse_type(qualified)
}

fn (tc &TypeChecker) qualify_type_text_impl(typ string, resolution bool) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return typ
	}
	if clean.starts_with('&') {
		return '&' + tc.qualify_type_text_impl(clean[1..], resolution)
	}
	if clean.starts_with('mut ') {
		inner := tc.qualify_type_text_impl(clean[4..], resolution)
		if inner.starts_with('&') {
			return inner
		}
		return '&' + inner
	}
	if clean.starts_with('shared ') {
		return 'shared ' + tc.qualify_type_text_impl(clean[7..], resolution)
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + tc.qualify_type_text_impl(clean[7..], resolution)
	}
	if clean.starts_with('?') {
		return '?' + tc.qualify_type_text_impl(clean[1..], resolution)
	}
	if clean.starts_with('!') {
		return '!' + tc.qualify_type_text_impl(clean[1..], resolution)
	}
	if clean.starts_with('...') {
		return '...' + tc.qualify_type_text_impl(clean[3..], resolution)
	}
	if clean.starts_with('[]') {
		return '[]' + tc.qualify_type_text_impl(clean[2..], resolution)
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := tc.qualify_type_text_impl(clean[4..bracket_end], resolution)
			val := tc.qualify_type_text_impl(clean[bracket_end + 1..], resolution)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + tc.qualify_type_text_impl(clean[bracket_end +
				1..], resolution)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_params(clean[1..clean.len - 1]) {
			parts << tc.qualify_type_text_impl(part, resolution)
		}
		return '(' + parts.join(', ') + ')'
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return tc.qualify_fn_type_text(clean)
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_params(clean[bracket + 1..bracket_end]) {
				parts << tc.qualify_type_text_impl(part, resolution)
			}
			return tc.qualify_type_text_impl(clean[..bracket], resolution) + '[' +
				parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	if !clean.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(clean) {
			return resolved
		}
	}
	if resolution {
		return tc.qualify_resolution_type_name(clean)
	}
	return tc.qualify_name(clean)
}

// qualify_fn_type_text supports qualify fn type text handling for TypeChecker.
fn (tc &TypeChecker) qualify_fn_type_text(typ string) string {
	params_start := typ.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	params_str := typ[params_start..params_end]
	mut params := []string{}
	if params_str.trim_space().len > 0 {
		for part in split_params(params_str) {
			params << tc.qualify_type_text(normalize_fn_type_param_text(part))
		}
	}
	ret_str := typ[params_end + 1..].trim_space()
	if ret_str.len > 0 {
		return 'fn(${params.join(', ')}) ${tc.qualify_type_text(ret_str)}'
	}
	return 'fn(${params.join(', ')})'
}

// file_import_key supports file import key handling for types.
fn file_import_key(file string, alias string) string {
	return '${file}\n${alias}'
}

fn (mut tc TypeChecker) file_import_info() &FileImportInfo {
	if info := tc.file_imports_by_file[tc.cur_file] {
		return info
	}
	info := &FileImportInfo{
		imports:           map[string]string{}
		selective_imports: map[string][]string{}
	}
	tc.file_imports_by_file[tc.cur_file] = info
	return info
}

fn (mut tc TypeChecker) register_file_import(alias string, module_name string) {
	tc.file_imports[file_import_key(tc.cur_file, alias)] = module_name
	mut info := tc.file_import_info()
	info.imports[alias] = module_name
}

// enter_file supports enter file handling for TypeChecker.
fn (mut tc TypeChecker) enter_file(file string) {
	tc.cur_file = file
	tc.cur_module = tc.file_modules[file] or { '' }
}

// enter_module supports enter module handling for TypeChecker.
fn (mut tc TypeChecker) enter_module(name string) {
	tc.cur_module = name
	if tc.cur_file.len > 0 {
		tc.file_modules[tc.cur_file] = name
	}
}

fn (mut tc TypeChecker) register_selective_imports(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	mut info := tc.file_import_info()
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind != .ident {
			continue
		}
		key := file_import_key(tc.cur_file, child.value)
		if child.value in info.selective_imports {
			tc.file_selective_imports[key] = []string{}
			info.selective_imports[child.value] = []string{}
			tc.record_error_unfiltered(.unknown_fn, 'ambiguous selective import `${child.value}`',
				child_id)
			continue
		}
		mut candidates := []string{}
		path_name := '${node.value}.${child.value}'
		if path_name !in candidates {
			candidates << path_name
		}
		alias_name := '${node.typ}.${child.value}'
		if alias_name != path_name && alias_name !in candidates {
			candidates << alias_name
		}
		tc.file_selective_imports[key] = candidates
		info.selective_imports[child.value] = candidates
	}
}

// resolve_import_alias resolves resolve import alias information for types.
fn (tc &TypeChecker) resolve_import_alias(alias string) ?string {
	info := tc.file_imports_by_file[tc.cur_file] or { return none }
	if mod := info.imports[alias] {
		return mod
	}
	return none
}

fn (tc &TypeChecker) resolve_selective_import_symbol(name string) ?string {
	candidates := tc.selective_import_candidates(name) or { return none }
	for candidate in candidates {
		if tc.fn_signature_known(candidate) || candidate in tc.fn_ret_types
			|| candidate in tc.fn_param_types {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) resolve_selective_import_type_symbol(name string) ?string {
	candidates := tc.selective_import_candidates(name) or { return none }
	for candidate in candidates {
		if tc.type_symbol_known(candidate) {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) selective_import_candidates(name string) ?[]string {
	info := tc.file_imports_by_file[tc.cur_file] or { return none }
	return info.selective_imports[name] or { return none }
}

fn (tc &TypeChecker) type_symbol_known(name string) bool {
	return name in tc.type_aliases || name in tc.structs || name in tc.interface_names
		|| name in tc.flag_enums || name in tc.enum_names || name in tc.sum_types
}

fn (tc &TypeChecker) unqualified_type_symbol_is_builtin(name string) bool {
	if tc.unqualified_type_symbol_has_scoped_shadow(name) {
		return false
	}
	if mod_name := tc.struct_modules[name] {
		return mod_name == 'builtin'
	}
	return name == 'IError'
}

fn (tc &TypeChecker) unqualified_type_symbol_has_scoped_shadow(name string) bool {
	if name.len == 0 {
		return false
	}
	if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
		local := '${tc.cur_module}.${name}'
		if tc.type_symbol_known(local) {
			return true
		}
	}
	if tc.source_declares_type_in_scope(name, tc.cur_file, tc.cur_module) {
		return true
	}
	if file_import_key(tc.cur_file, name) in tc.file_selective_imports {
		if resolved := tc.resolve_selective_import_type_symbol(name) {
			return !tc.unqualified_resolved_type_symbol_is_builtin(resolved)
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) unqualified_resolved_type_symbol_is_builtin(name string) bool {
	if mod_name := tc.struct_modules[name] {
		return mod_name == 'builtin'
	}
	return name == 'IError' || name == 'builtin.IError'
}

fn (tc &TypeChecker) type_from_known_symbol(name string) ?Type {
	if name in tc.type_aliases {
		return Type(Alias{
			name:      name
			base_type: tc.parse_type(tc.type_aliases[name])
		})
	}
	if name in tc.structs {
		return Type(Struct{
			name: name
		})
	}
	if name in tc.interface_names {
		return Type(Interface{
			name: name
		})
	}
	if name in tc.flag_enums {
		return Type(Enum{
			name:    name
			is_flag: true
		})
	}
	if name in tc.enum_names {
		return Type(Enum{
			name: name
		})
	}
	if name in tc.sum_types {
		return Type(SumType{
			name: name
		})
	}
	return none
}

fn (tc &TypeChecker) selective_import_symbol_is_ambiguous(name string) bool {
	candidates := tc.selective_import_candidates(name) or { return false }
	return candidates.len == 0
}

fn (tc &TypeChecker) resolve_imported_type_text(typ string) string {
	if !typ.contains('.') || typ.starts_with('C.') {
		return typ
	}
	dot := typ.index_u8(`.`)
	if dot <= 0 {
		return typ
	}
	alias := typ[..dot]
	if resolved := tc.resolve_import_alias(alias) {
		if resolved != alias {
			return resolved + typ[dot..]
		}
	}
	return typ
}

// has_active_import reports whether has active import applies in types.
fn (tc &TypeChecker) has_active_import(alias string) bool {
	info := tc.file_imports_by_file[tc.cur_file] or { return false }
	return alias in info.imports
}

const receiver_method_suffix_ambiguous = '__v_receiver_method_suffix_ambiguous__'

// register_fn_signature updates register fn signature state for types.
fn (mut tc TypeChecker) register_fn_signature(name string, ret_type Type, params []Type, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	tc.register_fn_name_alias(name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
	lowered_name := tc.cached_c_name(name)
	if lowered_name != name {
		tc.register_fn_name_alias(lowered_name, ret_type, params, shared_params, is_variadic,
			implicit_veb_ctx)
	}
	if name.ends_with('.str') {
		receiver := name.all_before_last('.')
		legacy_name := '${receiver}_str'
		if !legacy_name.contains('.') {
			tc.register_fn_name_alias(legacy_name, ret_type, params, shared_params, is_variadic,
				implicit_veb_ctx)
		}
	}
}

// register_fn_name_alias updates register fn name alias state for types.
fn (mut tc TypeChecker) register_fn_name_alias(name string, ret_type Type, params []Type, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	tc.fn_ret_types[name] = ret_type
	tc.fn_param_types[name] = params.clone()
	if shared_params.len > 0 {
		tc.fn_shared_params[name] = shared_params.clone()
	} else {
		tc.fn_shared_params.delete(name)
	}
	if tc.cur_file.len > 0 {
		tc.fn_type_files[name] = tc.cur_file
		tc.fn_type_modules[name] = tc.cur_module
	}
	tc.fn_variadic[name] = is_variadic
	tc.fn_implicit_veb_ctx[name] = implicit_veb_ctx
	tc.add_receiver_method_suffix_index(name)
}

fn (mut tc TypeChecker) add_receiver_method_suffix_index(name string) {
	if name.len == 0 {
		return
	}
	tc.set_receiver_method_suffix_index(name, name)
	for i in 0 .. name.len {
		if name[i] == `.` && i + 1 < name.len {
			tc.set_receiver_method_suffix_index(name[i + 1..], name)
		}
	}
}

fn (mut tc TypeChecker) set_receiver_method_suffix_index(key string, name string) {
	if key.len == 0 {
		return
	}
	if existing := tc.receiver_method_suffix_index[key] {
		if existing != name {
			tc.receiver_method_suffix_index[key] = receiver_method_suffix_ambiguous
		}
		return
	}
	tc.receiver_method_suffix_index[key] = name
}

fn (mut tc TypeChecker) register_c_variadic_fn(name string) {
	if name.len == 0 {
		return
	}
	tc.c_variadic_fns[name] = true
	lowered_name := tc.cached_c_name(name)
	if lowered_name != name {
		tc.c_variadic_fns[lowered_name] = true
	}
	if !name.starts_with('C.') {
		tc.c_variadic_fns['C.${name}'] = true
	}
}

fn (mut tc TypeChecker) insert_fn_param_binding(p flat.Node) {
	if p.kind != .param || p.value.len == 0 {
		return
	}
	typ := tc.parse_scope_param_type(p.typ)
	owner := tc.cur_scope.insert_with_owner(p.value, typ)
	if p.is_mut {
		tc.cur_fn_mut_param_base_types[p.value] = mut_param_base_type(typ)
		tc.cur_fn_mut_param_binding_owners[p.value] = owner
	}
	if param_type_text_is_shared(p.typ) {
		tc.cur_fn_shared_binding_owners[p.value] = owner
	}
}

fn mut_param_base_type(typ Type) Type {
	if typ is Pointer {
		return typ.base_type
	}
	return typ
}

// annotate_types performs a scope-aware walk over every function body, tracking
// local variable types as they are declared, and records complex/contextual
// expression types. This mirrors what the v2 transformer relies on: the type
// checker runs BEFORE the transformer and publishes per-expression types, so the
// transformer can own type-dependent lowering (string ops, `in` membership, ...)
// instead of the backend.
//
// It uses a single flat scope per function (an over-approximation: a local stays
// visible after its block ends), which is harmless for type lookup since variable
// names are effectively unique within a function.
pub fn (mut tc TypeChecker) annotate_types() {
	tc.annotate_types_with_used(map[string]bool{})
}

// annotate_types_with_used annotates only functions that can be emitted when
// `used_fns` is non-empty. This mirrors transform/cgen pruning and avoids
// resolving types in dead, untransformed function bodies after markused.
pub fn (mut tc TypeChecker) annotate_types_with_used(used_fns map[string]bool) {
	tc.extend_node_caches(tc.a.nodes.len)
	tc.cur_module = ''
	for node in tc.a.nodes {
		if node.kind == .file {
			tc.enter_file(node.value)
		} else if node.kind == .module_decl {
			tc.enter_module(node.value)
		} else if node.kind == .fn_decl {
			if !tc.should_annotate_fn(node, used_fns) {
				continue
			}
			mut saved_mut_params := tc.cur_fn_mut_param_base_types.move()
			mut saved_mut_param_owners := tc.cur_fn_mut_param_binding_owners.move()
			mut saved_mut_local_owners := tc.cur_fn_mut_local_binding_owners.move()
			mut saved_shared_owners := tc.cur_fn_shared_binding_owners.move()
			tc.cur_fn_mut_param_base_types = map[string]Type{}
			tc.cur_fn_mut_param_binding_owners = map[string]ScopeBindingOwner{}
			tc.cur_fn_mut_local_binding_owners = map[string]ScopeBindingOwner{}
			tc.cur_fn_shared_binding_owners = map[string]ScopeBindingOwner{}
			tc.cur_scope = tc.file_scope
			tc.push_scope()
			for pi in 0 .. node.children_count {
				p := tc.a.child_node(&node, pi)
				tc.insert_fn_param_binding(p)
			}
			tc.insert_implicit_veb_ctx(node)
			for i in 0 .. node.children_count {
				child := tc.a.child_node(&node, i)
				if child.kind != .param {
					tc.annotate_node(tc.a.child(&node, i))
				}
			}
			tc.pop_scope()
			tc.cur_fn_mut_param_base_types = saved_mut_params.move()
			tc.cur_fn_mut_param_binding_owners = saved_mut_param_owners.move()
			tc.cur_fn_mut_local_binding_owners = saved_mut_local_owners.move()
			tc.cur_fn_shared_binding_owners = saved_shared_owners.move()
		}
	}
}

fn (tc &TypeChecker) should_annotate_fn(node flat.Node, used_fns map[string]bool) bool {
	if used_fns.len == 0 {
		return true
	}
	qname := checker_qualified_fn_name(tc.cur_module, node.value)
	if qname in tc.a.export_fn_names || tc.fn_needs_implicit_veb_ctx(node) {
		return true
	}
	if node.value in used_fns {
		return true
	}
	if qname in used_fns {
		return true
	}
	cname := tc.cached_c_name(qname)
	if cname != qname && cname in used_fns {
		return true
	}
	return false
}

fn checker_qualified_fn_name(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

// annotate_node supports annotate node handling for TypeChecker.
fn (mut tc TypeChecker) annotate_node(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.decl_assign {
			lhs_count := tc.multi_assign_lhs_count(node)
			rhs_count := tc.multi_assign_rhs_count(node)
			if rhs_count == 1 && lhs_count > 1 {
				tc.annotate_node(tc.a.child(&node, 1))
				return
			}
			pair_count := if lhs_count < rhs_count { lhs_count } else { rhs_count }
			for pair_idx in 0 .. pair_count {
				lhs_id := tc.multi_assign_lhs_id(node, pair_idx)
				rhs_id := tc.multi_assign_rhs_id(node, pair_idx)
				tc.annotate_node(rhs_id)
				lhs := tc.a.nodes[int(lhs_id)]
				if lhs.kind == .ident && lhs.value.len > 0 {
					mut typ := Type(void_)
					if node.children_count == 2 && node.typ.len > 0 {
						typ = tc.parse_type(node.typ)
						tc.annotate_expected_expr(rhs_id, typ)
					} else {
						typ = tc.resolve_type(rhs_id)
					}
					if typ !is MultiReturn && typ !is Void {
						tc.cur_scope.insert(lhs.value, typ)
						tc.remember_expr_type(lhs_id, typ)
					}
				}
			}
			return
		}
		.for_in_stmt {
			tc.annotate_for_in(id, node)
			return
		}
		.assign, .selector_assign, .index_assign {
			tc.annotate_assign_expected_exprs(node)
		}
		.struct_init {
			tc.annotate_struct_init_expected_exprs(node)
		}
		.call {
			tc.annotate_call_expected_exprs(id, node)
		}
		else {}
	}

	tc.remember_expr_type(id, tc.resolve_type(id))
	for i in 0 .. node.children_count {
		tc.annotate_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) annotate_expected_expr(id flat.NodeId, expected Type) {
	if int(id) >= 0 && int(id) < tc.a.nodes.len {
		node := tc.a.nodes[int(id)]
		if node.kind in [.if_expr, .match_stmt] && expected !is Void && expected !is Unknown {
			_ = tc.resolve_expr(id, expected)
			return
		}
	}
	if _ := fn_type_from_type(expected) {
		_ = tc.resolve_expr(id, expected)
	}
}

fn (mut tc TypeChecker) check_node_with_expected_context(id flat.NodeId, expected Type) {
	saved_id := tc.expected_expr_id
	saved_type := tc.expected_expr_type
	tc.expected_expr_id = int(id)
	tc.expected_expr_type = expected
	tc.check_node(id)
	tc.expected_expr_id = saved_id
	tc.expected_expr_type = saved_type
}

fn (tc &TypeChecker) expected_context_for_expr(id flat.NodeId) ?Type {
	if tc.expected_expr_id >= 0 && tc.expr_is_value_tail_of(flat.NodeId(tc.expected_expr_id), id)
		&& tc.expected_expr_type !is Void && tc.expected_expr_type !is Unknown {
		return tc.expected_expr_type
	}
	return tc.expr_type(id)
}

fn (tc &TypeChecker) expr_is_value_tail_of(root_id flat.NodeId, target_id flat.NodeId) bool {
	if root_id == target_id {
		return true
	}
	if !tc.valid_node_id(root_id) {
		return false
	}
	node := tc.a.nodes[int(root_id)]
	match node.kind {
		.paren, .expr_stmt {
			return node.children_count > 0
				&& tc.expr_is_value_tail_of(tc.a.child(&node, 0), target_id)
		}
		.block, .match_branch, .lock_expr {
			return node.children_count > 0
				&& tc.expr_is_value_tail_of(tc.a.child(&node, node.children_count - 1), target_id)
		}
		.if_expr, .match_stmt {
			for i in 1 .. node.children_count {
				if tc.expr_is_value_tail_of(tc.a.child(&node, i), target_id) {
					return true
				}
			}
		}
		.comptime_if {
			for i in 0 .. node.children_count {
				if tc.expr_is_value_tail_of(tc.a.child(&node, i), target_id) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (mut tc TypeChecker) annotate_assign_expected_exprs(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_type := tc.resolve_lvalue_type(lhs_id)
		expected_type := tc.assignment_expected_type(lhs_id, lhs_type)
		tc.annotate_expected_expr(rhs_id, expected_type)
		i += 2
	}
}

fn (mut tc TypeChecker) annotate_struct_init_expected_exprs(node flat.Node) {
	init_type := tc.parse_type(node.value)
	init_struct := struct_type_from_type(init_type) or { return }
	init_name := tc.struct_init_field_lookup_name(node.value, init_struct.name)
	fields := tc.struct_fields_for_init(init_name)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field, 0)
		mut expected := Type(void_)
		if field.value.len > 0 {
			expected = tc.struct_field_type(init_name, field.value) or { Type(void_) }
		} else if i < fields.len {
			expected = fields[i].typ
		}
		tc.annotate_expected_expr(value_id, expected)
	}
}

fn (mut tc TypeChecker) annotate_call_expected_exprs(id flat.NodeId, node flat.Node) {
	info0 := tc.resolve_call_info(id, node) or { return }
	info := tc.specialized_plain_generic_call_info(node, info0)
	if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
		tc.remember_resolved_call(id, info.name)
	}
	if !info.params_known || info.params.len == 0 {
		return
	}
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	actual_count := node.children_count - 1 - info.arg_offset - field_init_args + collapsed +
		recv_extra
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	for i in 1 + info.arg_offset .. node.children_count {
		raw_arg := tc.a.child_node(&node, i)
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if raw_arg.kind == .field_init {
			tc.annotate_params_field_expected_expr(arg_id, raw_arg.value, info)
			continue
		}
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) + arg_shift
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			continue
		}
		if param_idx >= info.params.len {
			continue
		}
		expected := tc.call_arg_expected_type(info, param_idx)
		tc.annotate_expected_expr(arg_id, expected)
	}
}

fn (tc &TypeChecker) call_arg_expected_type(info CallInfo, param_idx int) Type {
	expected := info.params[param_idx]
	if info.is_variadic && param_idx == info.params.len - 1 && expected is Array {
		return array_elem_type(expected)
	}
	return expected
}

fn (mut tc TypeChecker) annotate_params_field_expected_expr(arg_id flat.NodeId, field_name string, info CallInfo) {
	if field_name.len == 0 {
		return
	}
	for param in info.params {
		clean := unwrap_pointer(param)
		if clean is Struct {
			if expected := tc.struct_field_type(clean.name, field_name) {
				tc.annotate_expected_expr(arg_id, expected)
				return
			}
		}
	}
}

// annotate_for_in supports annotate for in handling for TypeChecker.
fn (mut tc TypeChecker) annotate_for_in(_id flat.NodeId, node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		return
	}
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	tc.annotate_node(container_id)
	has_val := int(val_id) >= 0
	if header == 4 {
		tc.insert_loop_var(key_id, tc.range_loop_var_type(container_id))
		tc.annotate_node(tc.a.child(&node, 3))
	} else {
		clean0 := unwrap_pointer(tc.resolve_type(container_id))
		clean := if clean0 is Alias { clean0.base_type } else { clean0 }
		if clean is Array {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is ArrayFixed {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is Map {
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				tc.insert_loop_var(val_id, clean.value_type)
			} else {
				tc.insert_loop_var(key_id, clean.value_type)
			}
		} else if clean is String {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, Type(u8_))
			} else {
				tc.insert_loop_var(key_id, Type(u8_))
			}
		} else if elem_type := tc.iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, elem_type)
			} else {
				tc.insert_loop_var(key_id, elem_type)
			}
		} else {
			container := tc.a.nodes[int(container_id)]
			if container.kind == .range {
				tc.insert_loop_var(key_id, tc.range_loop_var_type(tc.a.child(&container, 0)))
			}
		}
	}
	for i in header .. node.children_count {
		tc.annotate_node(tc.a.child(&node, i))
	}
}

pub fn (tc &TypeChecker) iterator_for_in_elem_type(typ Type) ?Type {
	info := tc.iterator_for_in_next_call_info(typ) or { return none }
	return iterator_for_in_elem_type_from_next_return(info.return_type)
}

pub fn (tc &TypeChecker) iterator_for_in_next_call_info(typ Type) ?CallInfo {
	clean := unwrap_pointer(typ)
	name := clean.name()
	if name == 'RunesIterator' || name == 'builtin.RunesIterator' {
		return CallInfo{
			name:         'RunesIterator.next'
			params:       tarr1(Type(Pointer{
				base_type: clean
			}))
			return_type:  Type(OptionType{
				base_type: Type(rune_)
			})
			has_receiver: true
			params_known: true
		}
	}
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	if info := tc.resolve_generic_struct_method(type_name, 'next') {
		if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
			return info
		}
	}
	for method_name in receiver_method_name_candidates(clean, 'next', tc.cur_module) {
		if method_name !in tc.fn_ret_types {
			continue
		}
		info := tc.call_info(method_name, true)
		if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
			return info
		}
	}
	return none
}

fn iterator_for_in_elem_type_from_next_return(ret Type) ?Type {
	if ret is OptionType {
		return ret.base_type
	}
	return none
}

fn (tc &TypeChecker) range_loop_var_type(low_id flat.NodeId) Type {
	low_type := tc.resolve_type(low_id)
	if fn_param_unalias_type(low_type).is_integer() {
		return low_type
	}
	return Type(int_)
}

// insert_loop_var updates insert loop var state for types.
fn (mut tc TypeChecker) insert_loop_var(id flat.NodeId, typ Type) {
	if int(id) < 0 {
		return
	}
	v := tc.a.nodes[int(id)]
	if v.kind == .ident && v.value.len > 0 {
		tc.cur_scope.insert(v.value, typ)
		tc.remember_expr_type(id, typ)
	}
}

// expr_type returns the resolved type recorded for a node during annotate_types.
pub fn (tc &TypeChecker) expr_type(id flat.NodeId) ?Type {
	if int(id) >= 0 {
		node := tc.a.nodes[int(id)]
		if node.kind == .call && node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
			return tc.parse_type(node.typ)
		}
	}
	if t := tc.resolved_call_type(id) {
		return t
	}
	if t := tc.cached_expr_type(id) {
		return t
	}
	return none
}

// resolved_call_type supports resolved call type handling for TypeChecker.
fn (tc &TypeChecker) resolved_call_type(id flat.NodeId) ?Type {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .call {
		return none
	}
	if t := tc.cached_expr_type(id) {
		return t
	}
	if name := tc.cached_resolved_call(id) {
		if t := tc.fn_ret_types[name] {
			return t
		}
	}
	return none
}

// in_check_range reports whether idx belongs to the fn item currently being
// checked (whose node-cache slots this checker exclusively owns).
@[inline]
fn (tc &TypeChecker) in_check_range(idx int) bool {
	return idx >= tc.check_range_lo && idx <= tc.check_range_hi
}

// cached_expr_type supports cached expr type handling for TypeChecker.
fn (tc &TypeChecker) cached_expr_type(id flat.NodeId) ?Type {
	idx := int(id)
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			if idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
				return tc.expr_type_values[idx]
			}
			return none
		}
		return tc.sparse_expr_type_values[idx] or { none }
	}
	if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		return tc.expr_type_values[idx]
	}
	return none
}

// cached_resolved_call supports cached resolved call handling for TypeChecker.
fn (tc &TypeChecker) cached_resolved_call(id flat.NodeId) ?string {
	idx := int(id)
	if !isnil(tc.fork_overlay) {
		if name := tc.fork_overlay.resolved_call_names[idx] {
			return name
		}
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
				return tc.resolved_call_names[idx]
			}
			return none
		}
		return tc.sparse_resolved_call_names[idx] or { none }
	}
	if idx >= 0 && idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
		return tc.resolved_call_names[idx]
	}
	return none
}

// resolved_call_name returns the checker-resolved function name for a call node.
pub fn (tc &TypeChecker) resolved_call_name(id flat.NodeId) ?string {
	return tc.cached_resolved_call(id)
}

// fn_param_types_for_name returns the collected parameter types for a resolved call name.
pub fn (tc &TypeChecker) fn_param_types_for_name(name string) []Type {
	if params := tc.fn_param_types[name] {
		return params
	}
	if name.len == 0 {
		return []Type{}
	}
	mut found := []Type{}
	mut matches := 0
	for candidate, params in tc.fn_param_types {
		if candidate.ends_with('.${name}') {
			found = params.clone()
			matches++
			if matches > 1 {
				return []Type{}
			}
		}
	}
	return found
}

// resolved_call_never_returns reports whether a call node resolved to a known no-return function.
pub fn (tc &TypeChecker) resolved_call_never_returns(id flat.NodeId) bool {
	name := tc.resolved_call_name(id) or { return false }
	return tc.name_never_returns(name)
}

fn resolved_name_never_returns(name string) bool {
	return name in ['panic', 'exit', 'os.exit', 'C.exit']
}

// name_never_returns reports whether a resolved call name is a builtin no-return
// function or one declared with the `@[noreturn]` attribute.
fn (tc &TypeChecker) name_never_returns(name string) bool {
	if resolved_name_never_returns(name) {
		return true
	}
	return name in tc.a.noreturn_fns
}

// resolved_fn_value_name returns the checker-resolved function name for a function value node.
pub fn (tc &TypeChecker) resolved_fn_value_name(id flat.NodeId) ?string {
	idx := int(id)
	if !isnil(tc.fork_overlay) {
		if name := tc.fork_overlay.resolved_fn_values[idx] {
			return name
		}
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			if idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
				return tc.resolved_fn_value_names[idx]
			}
			return none
		}
		return tc.sparse_resolved_fn_values[idx] or { none }
	}
	if idx >= 0 && idx < tc.resolved_fn_value_set.len && tc.resolved_fn_value_set[idx] {
		return tc.resolved_fn_value_names[idx]
	}
	return none
}

// copy_cloned_resolution copies checker-owned call/function-value resolution metadata
// from an original node to a transform-created clone.
pub fn (mut tc TypeChecker) copy_cloned_resolution(src_id flat.NodeId, dst_id flat.NodeId) {
	if name := tc.resolved_call_name(src_id) {
		tc.remember_resolved_call(dst_id, name)
	}
	if name := tc.resolved_fn_value_name(src_id) {
		tc.remember_resolved_fn_value(dst_id, name)
	}
}

// resolve_fn_value_name_for_expected resolves and records a function value in an expected FnType context.
fn (mut tc TypeChecker) resolve_fn_value_name_for_expected(id flat.NodeId, expected Type) ?string {
	if name := tc.resolved_fn_value_name(id) {
		return name
	}
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if tc.fn_value_shadowed_by_value(node) {
		return none
	}
	key := tc.fn_value_match_key(node, expected) or { return none }
	tc.remember_resolved_fn_value_chain(id, key)
	return key
}

// remember_resolved_call supports remember resolved call handling for TypeChecker.
fn (mut tc TypeChecker) remember_resolved_call(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.resolved_call_names.len {
			tc.resolved_call_names[idx] = name
			tc.resolved_call_set[idx] = true
			return
		}
		tc.sparse_resolved_call_names[idx] = name
		return
	}
	if idx >= tc.resolved_call_names.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_call_names.len {
		tc.resolved_call_names[idx] = name
		tc.resolved_call_set[idx] = true
	}
}

// remember_resolved_fn_value records the exact function declaration used by a function value.
fn (mut tc TypeChecker) remember_resolved_fn_value(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.resolved_fn_value_names.len {
			tc.resolved_fn_value_names[idx] = name
			tc.resolved_fn_value_set[idx] = true
			return
		}
		tc.sparse_resolved_fn_values[idx] = name
		return
	}
	if idx >= tc.resolved_fn_value_names.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_fn_value_names.len {
		tc.resolved_fn_value_names[idx] = name
		tc.resolved_fn_value_set[idx] = true
	}
}

fn (mut tc TypeChecker) remember_resolved_fn_value_chain(id flat.NodeId, name string) {
	tc.remember_resolved_fn_value(id, name)
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		tc.remember_resolved_fn_value_chain(tc.a.child(&node, 0), name)
	}
}

// register_synth_type records the type of a generated or transformed node.
pub fn (mut tc TypeChecker) register_synth_type(id flat.NodeId, typ Type) {
	tc.remember_expr_type(id, typ)
}

// remember_expr_type supports remember expr type handling for TypeChecker.
fn (mut tc TypeChecker) remember_expr_type(id flat.NodeId, typ Type) {
	if int(id) < 0 {
		return
	}
	kind := if int(id) < tc.a.nodes.len { tc.a.nodes[int(id)].kind } else { flat.NodeKind.empty }
	if should_cache_expr_type(kind, typ) {
		idx := int(id)
		if tc.parallel_check_sparse {
			if tc.in_check_range(idx) && idx < tc.expr_type_values.len {
				tc.expr_type_values[idx] = typ
				tc.expr_type_set[idx] = true
				return
			}
			tc.sparse_expr_type_values[idx] = typ
			return
		}
		if idx >= tc.expr_type_values.len {
			tc.extend_node_caches(tc.a.nodes.len)
		}
		if idx < tc.expr_type_values.len {
			tc.expr_type_values[idx] = typ
			tc.expr_type_set[idx] = true
		}
	}
}

// should_cache_expr_type reports whether should cache expr type applies in types.
fn should_cache_expr_type(kind flat.NodeKind, typ Type) bool {
	if typ is Void || typ is Unknown {
		return false
	}
	if typ is Array || typ is ArrayFixed || typ is Map || typ is Pointer || typ is FnType
		|| typ is OptionType || typ is ResultType || typ is Struct || typ is Interface
		|| typ is Enum || typ is SumType || typ is Alias || typ is MultiReturn {
		return true
	}
	kind_id := int(kind)
	return kind_id != 1 && kind_id != 2 && kind_id != 3 && kind_id != 4 && kind_id != 5
		&& kind_id != 28
}

// check_semantics validates check semantics state for types.
pub fn (mut tc TypeChecker) check_semantics() {
	tc.resolution_type_mode = false
	tc.collect_selected_file_called_fns()
	tc.check_export_attrs()
	tc.cur_module = ''
	tc.cur_file = ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.struct_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.check_struct_field_defaults(node)
			}
			.type_decl, .interface_decl {
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.enum_decl {
				tc.check_enum_backing_type(flat.NodeId(i), node)
				tc.check_enum_field_values(node)
			}
			.const_decl {
				tc.check_const_field_values(node)
			}
			.fn_decl {
				mut saved_mut_params := tc.cur_fn_mut_param_base_types.move()
				mut saved_mut_param_owners := tc.cur_fn_mut_param_binding_owners.move()
				mut saved_mut_local_owners := tc.cur_fn_mut_local_binding_owners.move()
				mut saved_shared_owners := tc.cur_fn_shared_binding_owners.move()
				tc.cur_fn_mut_param_base_types = map[string]Type{}
				tc.cur_fn_mut_param_binding_owners = map[string]ScopeBindingOwner{}
				tc.cur_fn_mut_local_binding_owners = map[string]ScopeBindingOwner{}
				tc.cur_fn_shared_binding_owners = map[string]ScopeBindingOwner{}
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.check_fn_decl_semantics(i, node, tc.cur_file, tc.cur_module)
				tc.cur_fn_mut_param_base_types = saved_mut_params.move()
				tc.cur_fn_mut_param_binding_owners = saved_mut_param_owners.move()
				tc.cur_fn_mut_local_binding_owners = saved_mut_local_owners.move()
				tc.cur_fn_shared_binding_owners = saved_shared_owners.move()
			}
			.c_fn_decl {
				if tc.reject_unsupported_generics {
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
			}
			else {}
		}

		_ = i
	}
	// All ordinary source annotations have now been validated with module-strict
	// lookup. Later transform/codegen stages also parse synthesized generic type
	// text, where concrete arguments can legitimately come from another module.
	tc.resolution_type_mode = true
}

fn (mut tc TypeChecker) collect_selected_file_called_fns() {
	tc.selected_file_called_fns = map[string]bool{}
	tc.selected_file_worklist = []string{}
	if tc.diagnostic_files.len == 0 {
		return
	}
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	saved_scope := tc.cur_scope
	saved_scope_pool_index := tc.scope_pool_index
	tc.cur_file = ''
	tc.cur_module = ''
	tc.cur_scope = tc.file_scope
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
				if i >= tc.a.user_code_start && tc.diagnostic_files[tc.cur_file] {
					tc.collect_selected_file_top_level_called_fns(node)
				}
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				if i < tc.a.user_code_start || !tc.diagnostic_files[tc.cur_file] {
					continue
				}
				tc.collect_selected_file_fn_body_called_fns(node)
			}
			else {}
		}
	}
	tc.collect_selected_file_called_fns_transitively()
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	tc.cur_scope = saved_scope
	tc.scope_pool_index = saved_scope_pool_index
}

// SelectedFnDecl locates the first fn_decl node for a qualified name, with the
// file/module context needed to walk its body.
struct SelectedFnDecl {
	idx  int
	file string
	mod  string
}

fn (mut tc TypeChecker) collect_selected_file_called_fns_transitively() {
	// Index the first user-code fn_decl per qualified name once, then chase the
	// worklist of newly discovered called names. The former fixpoint re-scanned
	// every node per round; this walks each reachable body exactly once (the
	// first declaration wins for duplicate names, matching the old scan order).
	mut decls := map[string]SelectedFnDecl{}
	mut cur_file := ''
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = tc.file_modules[node.value] or { '' }
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if i < tc.a.user_code_start || node.value.len == 0 {
					continue
				}
				qname := checker_qualified_fn_name(cur_module, node.value)
				if qname !in decls {
					decls[qname] = SelectedFnDecl{
						idx:  i
						file: cur_file
						mod:  cur_module
					}
				}
			}
			else {}
		}
	}
	tc.cur_scope = tc.file_scope
	mut visited := map[string]bool{}
	for tc.selected_file_worklist.len > 0 {
		name := tc.selected_file_worklist.pop()
		if name in visited {
			continue
		}
		visited[name] = true
		decl := decls[name] or { continue }
		tc.cur_file = decl.file
		tc.cur_module = decl.mod
		tc.collect_selected_file_fn_body_called_fns(tc.a.nodes[decl.idx])
	}
}

fn (mut tc TypeChecker) collect_selected_file_top_level_called_fns(node flat.Node) {
	tc.push_scope()
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		match child.kind {
			.fn_decl, .struct_decl, .type_decl, .interface_decl, .enum_decl, .c_fn_decl,
			.import_decl, .module_decl, .directive {
				continue
			}
			else {
				tc.collect_selected_file_node_called_fns(child_id)
			}
		}
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) collect_selected_file_fn_body_called_fns(node flat.Node) {
	tc.push_scope()
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param && child.value.len > 0 {
			tc.cur_scope.insert(child.value, tc.parse_type(child.typ))
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind != .param {
			tc.collect_selected_file_node_called_fns(child_id)
		}
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) collect_selected_file_node_called_fns(id flat.NodeId) {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.block {
			tc.push_scope()
			for i in 0 .. node.children_count {
				tc.collect_selected_file_node_called_fns(tc.a.child(&node, i))
			}
			tc.pop_scope()
			return
		}
		.decl_assign {
			tc.collect_selected_file_decl_assign_called_fns(node)
			return
		}
		.for_in_stmt {
			tc.collect_selected_file_for_in_called_fns(node)
			return
		}
		.call {
			if name := tc.selected_file_call_name(node) {
				if name !in tc.selected_file_called_fns {
					tc.selected_file_called_fns[name] = true
					tc.selected_file_worklist << name
				}
			}
		}
		else {}
	}

	for i in 0 .. node.children_count {
		tc.collect_selected_file_node_called_fns(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) collect_selected_file_for_in_called_fns(node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		for i in 0 .. node.children_count {
			tc.collect_selected_file_node_called_fns(tc.a.child(&node, i))
		}
		return
	}
	tc.push_scope()
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	tc.collect_selected_file_node_called_fns(container_id)
	has_val := int(val_id) >= 0
	if header == 4 {
		tc.insert_selected_file_decl_binding_type(key_id, tc.range_loop_var_type(container_id))
		tc.collect_selected_file_node_called_fns(tc.a.child(&node, 3))
	} else {
		clean := unwrap_pointer(tc.resolve_type(container_id))
		if clean is Array {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, clean.elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, clean.elem_type)
			}
		} else if clean is ArrayFixed {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, clean.elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, clean.elem_type)
			}
		} else if clean is Map {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, clean.key_type)
				tc.insert_selected_file_decl_binding_type(val_id, clean.value_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, clean.value_type)
			}
		} else if clean is String {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, Type(u8_))
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, Type(u8_))
			}
		} else if elem_type := tc.iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, elem_type)
			}
		} else {
			container := tc.a.nodes[int(container_id)]
			if container.kind == .range {
				tc.insert_selected_file_decl_binding_type(key_id, tc.range_loop_var_type(tc.a.child(&container,
					0)))
			}
		}
	}
	for i in header .. node.children_count {
		tc.collect_selected_file_node_called_fns(tc.a.child(&node, i))
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) collect_selected_file_decl_assign_called_fns(node flat.Node) {
	if node.children_count >= 3 {
		rhs_id := tc.a.child(&node, 1)
		rhs_type := tc.decl_assign_inferred_type(rhs_id)
		if rhs_type is MultiReturn {
			tc.collect_selected_file_node_called_fns(rhs_id)
			lhs_ids := tc.multi_assign_lhs_ids(node)
			for i, lhs_id in lhs_ids {
				if i < rhs_type.types.len {
					tc.insert_selected_file_decl_binding_type(lhs_id, rhs_type.types[i])
				}
			}
			return
		}
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		tc.collect_selected_file_node_called_fns(rhs_id)
		tc.insert_selected_file_decl_binding(lhs_id, rhs_id, node)
		i += 2
	}
}

fn (mut tc TypeChecker) insert_selected_file_decl_binding(lhs_id flat.NodeId, rhs_id flat.NodeId, node flat.Node) {
	if int(lhs_id) < 0 || int(lhs_id) >= tc.a.nodes.len {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	typ := if node.children_count == 2 && node.typ.len > 0 {
		tc.parse_type(node.typ)
	} else {
		tc.decl_assign_inferred_type(rhs_id)
	}
	tc.insert_selected_file_decl_binding_type(lhs_id, typ)
}

fn (mut tc TypeChecker) insert_selected_file_decl_binding_type(lhs_id flat.NodeId, typ Type) {
	if int(lhs_id) < 0 || int(lhs_id) >= tc.a.nodes.len {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	if typ is MultiReturn || typ is Void || typ is Unknown {
		return
	}
	tc.cur_scope.insert(lhs.value, typ)
}

fn (tc &TypeChecker) selected_file_call_name(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		return tc.selected_file_call_base_name(tc.a.child_node(fn_node, 0))
	}
	return tc.selected_file_call_base_name(fn_node)
}

fn (tc &TypeChecker) selected_file_call_base_name(fn_node flat.Node) ?string {
	match fn_node.kind {
		.ident {
			if local_name := tc.local_bare_fn_signature_key(fn_node.value) {
				return local_name
			}
			if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
				return imported_name
			}
			if tc.fn_signature_known(fn_node.value) {
				return fn_node.value
			}
		}
		.selector {
			if fn_node.children_count == 0 {
				return none
			}
			base_id := tc.a.child(&fn_node, 0)
			base_node := tc.a.nodes[int(base_id)]
			if base_node.kind == .ident {
				if _ := tc.cur_scope.lookup(base_node.value) {
					if name := tc.selected_file_receiver_method_name(base_id, fn_node.value) {
						return name
					}
					return none
				}
				if resolved_mod := tc.resolve_import_alias(base_node.value) {
					mod_name := '${resolved_mod}.${fn_node.value}'
					if tc.fn_signature_known(mod_name) {
						return mod_name
					}
				}
				if base_node.value == tc.cur_module {
					mod_name := '${tc.cur_module}.${fn_node.value}'
					if tc.fn_signature_known(mod_name) {
						return mod_name
					}
				}
			}
			if name := tc.selected_file_receiver_method_name(base_id, fn_node.value) {
				return name
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) selected_file_receiver_method_name(base_id flat.NodeId, method string) ?string {
	if method.len == 0 {
		return none
	}
	base_type := tc.resolve_type(base_id)
	clean := unwrap_pointer(base_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	for method_name in receiver_method_name_candidates(clean, method, tc.cur_module) {
		if !tc.fn_signature_known(method_name) {
			continue
		}
		if !tc.method_can_be_called_on_receiver(base_type, method, method_name) {
			continue
		}
		return method_name
	}
	if info := tc.embedded_method_call_info(type_name, method) {
		if info.name.len > 0 {
			return info.name
		}
	}
	if info := tc.resolve_generic_struct_method(type_name, method) {
		if info.name.len > 0 {
			return info.name
		}
	}
	return none
}

fn (mut tc TypeChecker) check_export_attrs() {
	mut natural_symbols := map[string]string{}
	synthetic_main_reserved := tc.has_synthetic_c_entry_main()
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
				cur_module = tc.cur_module
			}
			.module_decl {
				cur_module = node.value
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := export_qualified_fn_name(cur_module, node.value)
				natural_symbol := export_natural_c_symbol(cur_module, node.value)
				natural_symbols[natural_symbol] = qname
			}
			else {}
		}
	}
	mut export_symbols := map[string]string{}
	cur_module = ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				tc.enter_file(node.value)
				cur_module = tc.cur_module
			}
			.module_decl {
				cur_module = node.value
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := export_qualified_fn_name(cur_module, node.value)
				export_name := tc.a.export_fn_names[qname] or { continue }
				if export_name.len == 0 {
					tc.record_error_unfiltered(.unsupported_generic,
						'empty export name for `${qname}`', flat.NodeId(i))
					continue
				}
				if !is_valid_export_c_name(export_name) {
					tc.record_error_unfiltered(.unsupported_generic,
						'invalid export name `${export_name}` for `${qname}`', flat.NodeId(i))
				}
				if synthetic_main_reserved && export_name == 'main' {
					tc.record_error_unfiltered(.unsupported_generic,
						'export name `main` for `${qname}` collides with synthetic entry point `main`',
						flat.NodeId(i))
				}
				if node.generic_params.len > 0 {
					tc.record_error_unfiltered(.unsupported_generic,
						'generic function `${qname}` cannot be exported', flat.NodeId(i))
				}
				for pi in 0 .. node.children_count {
					p := tc.a.child_node(&node, pi)
					if p.kind == .param && (p.value.len == 0 || p.typ.len == 0) {
						tc.record_error_unfiltered(.unsupported_generic,
							'exported function `${qname}` must name all parameters', flat.NodeId(i))
					}
				}
				if existing := export_symbols[export_name] {
					if existing != qname {
						tc.record_error_unfiltered(.unsupported_generic,
							'duplicate export name `${export_name}` for `${qname}` and `${existing}`',
							flat.NodeId(i))
					}
				} else {
					export_symbols[export_name] = qname
				}
				if existing := natural_symbols[export_name] {
					tc.record_error_unfiltered(.unsupported_generic,
						'export name `${export_name}` for `${qname}` collides with `${existing}`',
						flat.NodeId(i))
				}
			}
			else {}
		}
	}
}

fn (tc &TypeChecker) has_synthetic_c_entry_main() bool {
	if tc.has_c_test_harness_main() {
		return true
	}
	if tc.has_main_module_fn_main() {
		return false
	}
	return tc.has_c_top_level_main()
}

fn (tc &TypeChecker) has_main_module_fn_main() bool {
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if node.value == 'main' && (cur_module.len == 0 || cur_module == 'main') {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn (tc &TypeChecker) has_c_test_harness_main() bool {
	for file_idx in tc.top_level_idx {
		file_node := tc.a.nodes[file_idx]
		if file_idx < tc.a.user_code_start || file_node.kind != .file || file_node.value.len == 0 {
			continue
		}
		if !tc.is_selected_input_file(file_node.value) {
			continue
		}
		module_name := tc.top_level_file_module_name(file_node)
		if is_c_backend_test_file(file_node.value) && (is_regular_v_test_file(file_node.value)
			|| module_name.len == 0 || module_name == 'main') {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) has_c_top_level_main() bool {
	for file_idx in tc.top_level_idx {
		file_node := tc.a.nodes[file_idx]
		if !tc.should_emit_c_top_level_file(file_idx, file_node) {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := tc.a.child(&file_node, i)
			if int(child_id) < tc.a.user_code_start {
				continue
			}
			if tc.is_c_top_level_stmt(child_id) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) should_emit_c_top_level_file(file_idx int, file_node flat.Node) bool {
	if file_idx < tc.a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	module_name := tc.top_level_file_module_name(file_node)
	return module_name.len == 0 || module_name == 'main'
}

fn (tc &TypeChecker) top_level_file_module_name(file_node flat.Node) string {
	if module_name := tc.file_modules[file_node.value] {
		return module_name
	}
	for i in 0 .. file_node.children_count {
		child := tc.a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn (tc &TypeChecker) is_c_top_level_stmt(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt,
		.for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt {
			true
		}
		.block, .comptime_if {
			for i in 0 .. node.children_count {
				if tc.is_c_top_level_stmt(tc.a.child(&node, i)) {
					return true
				}
			}
			false
		}
		else {
			false
		}
	}
}

fn (tc &TypeChecker) is_selected_input_file(file string) bool {
	return tc.diagnostic_files.len == 0 || tc.diagnostic_files[file]
}

fn is_c_backend_test_file(path string) bool {
	file := path.all_after_last('/').all_after_last('\\')
	if file.ends_with('_test.v') || file.ends_with('_test.c.v') {
		return true
	}
	if !file.ends_with('.v') {
		return false
	}
	base := file[..file.len - 2]
	if !base.contains('.') {
		return false
	}
	return base.all_after_last('.') == 'c' && base.all_before_last('.').ends_with('_test')
}

fn is_regular_v_test_file(path string) bool {
	return path.all_after_last('/').all_after_last('\\').ends_with('_test.v')
}

fn export_qualified_fn_name(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	return '${module_name}.${name}'
}

fn export_natural_c_symbol(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin' {
		return naming.c_name('${module_name}.${name}')
	}
	if name == 'free' {
		return 'v_free'
	}
	if name == 'exit' {
		return 'v_exit'
	}
	if name in export_c_libc_collision_symbols {
		return 'v_${name}'
	}
	return naming.c_name(name)
}

fn is_valid_export_c_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if name in export_c_reserved_words {
		return false
	}
	if name in export_v3_reserved_c_symbols {
		return false
	}
	first := name[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for i in 1 .. name.len {
		c := name[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		return false
	}
	return true
}

fn (mut tc TypeChecker) insert_implicit_veb_ctx(node flat.Node) {
	if !tc.fn_needs_implicit_veb_ctx(node) {
		return
	}
	tc.cur_scope.insert('ctx', tc.implicit_veb_ctx_type())
}

fn (tc &TypeChecker) fn_param_types_with_implicit_veb_ctx(node flat.Node, params []Type) []Type {
	if !tc.fn_needs_implicit_veb_ctx(node) {
		return params
	}
	insert_idx := tc.fn_implicit_veb_ctx_insert_index(node)
	mut result := []Type{cap: params.len + 1}
	for i, param in params {
		if i == insert_idx {
			result << tc.implicit_veb_ctx_type()
		}
		result << param
	}
	if insert_idx >= params.len {
		result << tc.implicit_veb_ctx_type()
	}
	return result
}

fn (tc &TypeChecker) fn_shared_params_with_implicit_veb_ctx(node flat.Node, flags []bool) []bool {
	if !tc.fn_needs_implicit_veb_ctx(node) {
		return flags
	}
	insert_idx := tc.fn_implicit_veb_ctx_insert_index(node)
	mut result := []bool{cap: flags.len + 1}
	for i, flag in flags {
		if i == insert_idx {
			result << false
		}
		result << flag
	}
	if insert_idx >= flags.len {
		result << false
	}
	return result
}

fn param_type_text_is_shared(raw string) bool {
	return raw.trim_space().starts_with('shared ')
}

fn decl_assign_is_shared_marker(value string) bool {
	return value == 'shared' || value.starts_with('shared:')
}

fn (tc &TypeChecker) implicit_veb_ctx_type() Type {
	return tc.parse_type('mut Context')
}

fn (tc &TypeChecker) fn_needs_implicit_veb_ctx(node flat.Node) bool {
	return tc.fn_returns_veb_result(node) && tc.fn_has_receiver_param(node)
		&& !tc.fn_receiver_type_is_context(node) && !tc.fn_has_param(node, 'ctx')
		&& tc.type_name_known_in_current_module('Context')
}

fn (tc &TypeChecker) fn_implicit_veb_ctx_insert_index(node flat.Node) int {
	if tc.fn_has_receiver_param(node) {
		return 1
	}
	return 0
}

fn (tc &TypeChecker) fn_has_receiver_param(node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := tc.a.child_node(&node, 0)
	if first.kind != .param || first.typ.len == 0 {
		return false
	}
	receiver := node.value.all_before_last('.').all_after_last('.')
	param_type := first.typ.trim_left('&').all_after_last('.')
	return receiver == param_type
}

fn (tc &TypeChecker) fn_receiver_type_is_context(node flat.Node) bool {
	if !tc.fn_has_receiver_param(node) {
		return false
	}
	first := tc.a.child_node(&node, 0)
	return first.typ.trim_left('&').all_after_last('.') == 'Context'
}

fn (tc &TypeChecker) fn_has_param(node flat.Node, name string) bool {
	for i in 0 .. node.children_count {
		p := tc.a.child_node(&node, i)
		if p.kind == .param && p.value == name {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_returns_veb_result(node flat.Node) bool {
	if node.typ == 'veb.Result' {
		return true
	}
	ret := tc.parse_type(node.typ)
	return ret.name() == 'veb.Result'
}

// check_fn_body validates check fn body state for types.
fn (mut tc TypeChecker) check_fn_body(node flat.Node) {
	saved_smartcasts := clone_smartcasts(tc.smartcasts)
	defer {
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		tc.check_stmt_node(child_id)
		tc.apply_post_if_exit_smartcasts(child_id)
	}
}

// check_decl_type_strings validates check decl type strings state for types.
fn (mut tc TypeChecker) check_decl_type_strings(node_id flat.NodeId, node flat.Node) {
	generic_params := tc.infer_decl_generic_params(node)
	if node.kind == .struct_decl {
		if 'generic' in node.typ.split(',') && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic struct `${node.value}`', node_id)
		}
	} else {
		if node.generic_params.len > 0 && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic declaration `${node.value}`',
				node_id)
		}
		tc.check_type_string_for_unsupported_generics(node.typ, node_id, generic_params)
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := tc.a.nodes[int(child_id)]
		// A `comptime_for` node stores its loop source (`val`, `T`) in `typ`, which is a value
		// or generic placeholder rather than a declared type; it is validated at unroll time.
		if child.kind == .comptime_for {
			continue
		}
		tc.check_type_string_for_unsupported_generics(child.typ, child_id, generic_params)
		if node.kind == .type_decl && child.value.len > 0 {
			tc.check_type_string_for_unsupported_generics(child.value, child_id, generic_params)
		}
		for j in 0 .. child.children_count {
			grandchild_id := tc.a.child(&child, j)
			if int(grandchild_id) < 0 {
				continue
			}
			grandchild := tc.a.nodes[int(grandchild_id)]
			if grandchild.kind == .comptime_for {
				continue
			}
			tc.check_type_string_for_unsupported_generics(grandchild.typ, grandchild_id,
				generic_params)
		}
	}
}

fn (tc &TypeChecker) infer_decl_generic_params(node flat.Node) map[string]bool {
	mut params := map[string]bool{}
	for name in node.generic_params {
		params[name] = true
	}
	tc.collect_generic_receiver_params(node, mut params)
	return params
}

fn (tc &TypeChecker) collect_generic_receiver_params(node flat.Node, mut params map[string]bool) {
	if node.kind != .fn_decl && node.kind != .c_fn_decl {
		return
	}
	if !node.value.contains('.') {
		return
	}
	if node.children_count == 0 {
		return
	}
	receiver_id := tc.a.child(&node, 0)
	if int(receiver_id) < 0 {
		return
	}
	receiver := tc.a.nodes[int(receiver_id)]
	if receiver.kind != .param {
		return
	}
	receiver_type := receiver.typ.trim_left('&')
	if receiver_type != node.value.all_before_last('.') {
		return
	}
	mut counts := map[string]int{}
	if !tc.collect_generic_param_candidates(receiver.typ, mut counts) {
		return
	}
	for name, _ in counts {
		params[name] = true
	}
}

fn (tc &TypeChecker) collect_generic_param_candidates(typ string, mut counts map[string]int) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return tc.collect_generic_param_candidates(clean[1..], mut counts)
	}
	if clean.starts_with('shared ') {
		return tc.collect_generic_param_candidates(clean[7..], mut counts)
	}
	if clean.starts_with('...') {
		return tc.collect_generic_param_candidates(clean[3..], mut counts)
	}
	if clean.starts_with('[]') {
		return tc.collect_generic_param_candidates(clean[2..], mut counts)
	}
	if clean.starts_with('map[') {
		mut found_context := false
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			if tc.collect_generic_param_candidates(clean[4..bracket_end], mut counts) {
				found_context = true
			}
			if tc.collect_generic_param_candidates(clean[bracket_end + 1..], mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 {
			return tc.collect_generic_param_candidates(clean[idx + 1..], mut counts)
		}
		return false
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		mut found_context := false
		for part in split_params(clean[1..clean.len - 1]) {
			if tc.collect_generic_param_candidates(part, mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		mut found_context := false
		params_start := clean.index_u8(`(`) + 1
		mut depth := 1
		mut params_end := params_start
		for params_end < clean.len {
			if clean[params_end] == `(` {
				depth++
			} else if clean[params_end] == `)` {
				depth--
				if depth == 0 {
					break
				}
			}
			params_end++
		}
		if params_end < clean.len {
			for part in split_params(clean[params_start..params_end]) {
				trimmed := part.trim_space()
				parts := trimmed.split(' ')
				param_type := if parts.len >= 2 { parts[parts.len - 1] } else { trimmed }
				if tc.collect_generic_param_candidates(param_type, mut counts) {
					found_context = true
				}
			}
			if tc.collect_generic_param_candidates(clean[params_end + 1..], mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if generic_type_application(clean) {
		bracket := clean.index_u8(`[`)
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			for part in split_params(clean[bracket + 1..bracket_end]) {
				tc.collect_generic_param_candidates(part, mut counts)
			}
		}
		return true
	}
	if is_bare_generic_param(clean) && !tc.type_name_known(clean) {
		counts[clean] = (counts[clean] or { 0 }) + 1
	}
	return false
}

// check_type_string_for_unsupported_generics
// validates helper state for types.
fn (mut tc TypeChecker) check_type_string_for_unsupported_generics(typ string, node_id flat.NodeId, generic_params map[string]bool) {
	clean := typ.trim_space()
	if clean.len == 0 {
		return
	}
	if clean in ['generic', 'params', 'union'] {
		return
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		tc.check_type_string_for_unsupported_generics(clean[1..], node_id, generic_params)
		return
	}
	if clean.starts_with('shared ') {
		tc.check_type_string_for_unsupported_generics(clean[7..], node_id, generic_params)
		return
	}
	if clean == 'thread' || clean == 'chan' {
		return
	}
	if clean.starts_with('thread ') {
		// `thread T` is a thread handle; only its element type T needs checking.
		tc.check_type_string_for_unsupported_generics(clean[7..], node_id, generic_params)
		return
	}
	if clean.starts_with('chan ') {
		tc.check_type_string_for_unsupported_generics(clean[5..], node_id, generic_params)
		return
	}
	if clean.starts_with('...') {
		tc.check_type_string_for_unsupported_generics(clean[3..], node_id, generic_params)
		return
	}
	if clean.starts_with('[]') {
		tc.check_type_string_for_unsupported_generics(clean[2..], node_id, generic_params)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			tc.check_type_string_for_unsupported_generics(clean[4..bracket_end], node_id,
				generic_params)
			tc.check_type_string_for_unsupported_generics(clean[bracket_end + 1..], node_id,
				generic_params)
		}
		return
	}
	if clean.starts_with('[') {
		idx := clean.index_u8(`]`)
		if idx > 0 {
			tc.check_type_string_for_unsupported_generics(clean[idx + 1..], node_id, generic_params)
		}
		return
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		for part in split_params(clean[1..clean.len - 1]) {
			tc.check_type_string_for_unsupported_generics(part, node_id, generic_params)
		}
		return
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		tc.check_fn_type_string_for_unsupported_generics(clean, node_id, generic_params)
		return
	}
	if generic_type_application(clean) {
		if tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic type application `${clean}`',
				node_id)
			return
		}
		bracket := clean.index_u8(`[`)
		bracket_end := find_matching_bracket(clean, bracket)
		base := clean[..bracket].trim_space()
		if should_check_named_type(base) && !tc.type_name_known(base) {
			tc.record_error(.unknown_type, 'unknown type `${base}`', node_id)
		}
		if bracket_end < clean.len {
			for part in split_params(clean[bracket + 1..bracket_end]) {
				tc.check_type_string_for_unsupported_generics(part, node_id, generic_params)
			}
		}
		return
	}
	if is_bare_generic_param(clean) && !tc.type_name_known(clean) {
		if tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic type parameter `${clean}`', node_id)
			return
		}
		if clean in generic_params {
			return
		}
	}
	if should_check_named_type(clean) && !tc.type_name_known(clean) {
		tc.record_error(.unknown_type, 'unknown type `${clean}`', node_id)
	}
}

// check_fn_type_string_for_unsupported_generics
// validates helper state for types.
fn (mut tc TypeChecker) check_fn_type_string_for_unsupported_generics(typ string, node_id flat.NodeId, generic_params map[string]bool) {
	params_start := typ.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= typ.len {
		return
	}
	for part in split_params(typ[params_start..params_end]) {
		trimmed := part.trim_space()
		parts := trimmed.split(' ')
		param_type := if parts.len >= 2 { parts[parts.len - 1] } else { trimmed }
		tc.check_type_string_for_unsupported_generics(param_type, node_id, generic_params)
	}
	ret := typ[params_end + 1..].trim_space()
	tc.check_type_string_for_unsupported_generics(ret, node_id, generic_params)
}

// generic_type_application supports generic type application handling for types.
fn generic_type_application(typ string) bool {
	_, _, ok := generic_type_application_parts(typ)
	return ok
}

fn (tc &TypeChecker) generic_args_are_concrete(args []string) bool {
	for arg in args {
		if tc.type_text_has_generic_placeholder(arg) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) type_text_has_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if is_bare_generic_param(clean) {
		return !tc.is_known_type_text(clean)
	}
	if clean.starts_with('&') {
		return tc.type_text_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('mut ') {
		return tc.type_text_has_generic_placeholder(clean[4..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return tc.type_text_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('...') {
		return tc.type_text_has_generic_placeholder(clean[3..])
	}
	if clean.starts_with('[]') {
		return tc.type_text_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return tc.type_text_has_generic_placeholder(clean[4..bracket_end])
				|| tc.type_text_has_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return tc.type_text_has_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	_, args, ok := generic_type_application_parts(clean)
	if ok {
		for arg in args {
			if tc.type_text_has_generic_placeholder(arg) {
				return true
			}
		}
	}
	if clean.contains('.') && is_bare_generic_param(clean.all_after_last('.')) {
		return !tc.is_known_type_text(clean)
	}
	return false
}

fn generic_type_application_parts(typ string) (string, []string, bool) {
	if typ.starts_with('[') || !typ.contains('[') {
		return '', []string{}, false
	}
	bracket := typ.index_u8(`[`)
	bracket_end := find_matching_bracket(typ, bracket)
	if bracket <= 0 || bracket_end <= bracket {
		return '', []string{}, false
	}
	inner := typ[bracket + 1..bracket_end].trim_space()
	if is_fixed_array_len_text(inner) {
		return '', []string{}, false
	}
	return typ[..bracket], split_params(inner), true
}

// is_fixed_array_len_text reports whether a postfix `Base[inner]` bracket holds a fixed-array
// length rather than a generic type argument. `ArrayFixed.name()` renders the length as a decimal
// (`u8[16]`), a non-decimal literal (`u8[0x10]`), or the source length expression (`u8[segs + 1]`);
// a generic argument is always a type, never a number or arithmetic expression. Recognising all
// three keeps such a postfix name parsing as a fixed array (e.g. when `[]thread T.wait()` recovers
// the spawned return type) instead of a bogus generic application.
fn is_fixed_array_len_text(inner string) bool {
	s := inner.trim_space()
	if s.len == 0 {
		return false
	}
	// A fixed-array length is a single integer expression; a comma means the brackets hold a
	// generic argument LIST (`Pair[int, &Node]`), not a length. Without this an `&` (or `-`)
	// that merely leads a later pointer type argument would be read as a length operator.
	if s.contains(',') {
		return false
	}
	if v_int_literal_value(s) != none {
		return true
	}
	for i in 0 .. s.len {
		c := s[i]
		if c in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`] {
			return true
		}
		// A leading `-`/`&` is a negative literal / pointer-type argument; elsewhere they are the
		// subtraction / bitwise-and operators of a length expression.
		if (c == `-` || c == `&`) && i > 0 {
			return true
		}
	}
	return false
}

// is_decimal_int_literal reports whether is decimal int literal applies in types.
fn is_decimal_int_literal(s string) bool {
	if s.len == 0 {
		return false
	}
	for i in 0 .. s.len {
		if s[i] < `0` || s[i] > `9` {
			return false
		}
	}
	return true
}

// v_int_literal_value parses a complete V integer literal — decimal, hex (`0x`), octal
// (`0o`), or binary (`0b`), with optional `_` digit separators — to its value. Returns
// none when `s` is not a whole integer literal (a const name, an expression, etc.), so
// const-length folding accepts `0xF & 6` / `[0b1100 >> 1]int`, not just decimal text.
fn v_int_literal_value(s string) ?int {
	if s.len == 0 {
		return none
	}
	t := s.replace('_', '')
	if t.len == 0 {
		return none
	}
	mut base := 10
	mut digits := t
	if t.len >= 2 && t[0] == `0` {
		c := t[1]
		if c == `x` || c == `X` {
			base = 16
			digits = t[2..]
		} else if c == `o` || c == `O` {
			base = 8
			digits = t[2..]
		} else if c == `b` || c == `B` {
			base = 2
			digits = t[2..]
		}
	}
	if digits.len == 0 {
		return none
	}
	mut value := 0
	for ch in digits {
		mut d := 0
		if ch >= `0` && ch <= `9` {
			d = int(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			d = int(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			d = int(ch - `A`) + 10
		} else {
			return none
		}
		if d >= base {
			return none
		}
		value = value * base + d
	}
	return value
}

// is_bare_generic_param reports whether is bare generic param applies in types.
fn is_bare_generic_param(typ string) bool {
	return typ.len == 1 && typ[0] >= `A` && typ[0] <= `Z`
}

fn unresolved_generic_receiver_type(typ Type) bool {
	if typ is Unknown {
		if _ := generic_placeholder_from_unknown(typ) {
			return true
		}
	}
	if typ is Struct {
		return is_bare_generic_param(typ.name)
	}
	return false
}

fn generic_param_index(name string) int {
	return match name {
		'T', 'A', 'K', 'X' { 0 }
		'U', 'B', 'V', 'Y' { 1 }
		'C', 'W', 'Z' { 2 }
		else { 0 }
	}
}

fn generic_placeholder_from_unknown(typ Unknown) ?string {
	start := typ.reason.index_u8(`\``)
	if start < 0 {
		return none
	}
	end := typ.reason[start + 1..].index_u8(`\``)
	if end < 0 {
		return none
	}
	name := typ.reason[start + 1..start + 1 + end]
	if !is_bare_generic_param(name) {
		return none
	}
	return name
}

fn (tc &TypeChecker) resolve_known_field_type(type_name string, fallback Type) Type {
	qname := tc.qualify_name(type_name)
	allow_bare_symbol := qname == type_name
	if qname in tc.structs {
		return Type(Struct{
			name: qname
		})
	}
	if allow_bare_symbol && type_name in tc.structs {
		return Type(Struct{
			name: type_name
		})
	}
	if qname in tc.interface_names {
		return Type(Interface{
			name: qname
		})
	}
	if allow_bare_symbol && type_name in tc.interface_names {
		return Type(Interface{
			name: type_name
		})
	}
	if qname in tc.type_aliases {
		return Type(Alias{
			name:      qname
			base_type: tc.parse_type(tc.type_aliases[qname])
		})
	}
	if allow_bare_symbol && type_name in tc.type_aliases {
		return Type(Alias{
			name:      type_name
			base_type: tc.parse_type(tc.type_aliases[type_name])
		})
	}
	return fallback
}

// type_name_known returns type name known data for TypeChecker.
fn (tc &TypeChecker) type_name_known(typ string) bool {
	if is_builtin_type_name(typ) || typ == 'unknown' || typ.starts_with('C.') {
		return true
	}
	qtyp := tc.qualify_name(typ)
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			return tc.type_symbol_known(resolved)
		}
	}
	return qtyp in tc.type_aliases || qtyp in tc.structs || qtyp in tc.interface_names
		|| qtyp in tc.enum_names || qtyp in tc.sum_types
}

fn (tc &TypeChecker) type_name_known_in_current_module(typ string) bool {
	qtyp := tc.qualify_name(typ)
	return qtyp in tc.type_aliases || qtyp in tc.structs || qtyp in tc.interface_names
		|| qtyp in tc.enum_names || qtyp in tc.sum_types
}

// should_check_named_type reports whether should check named type applies in types.
fn should_check_named_type(typ string) bool {
	if typ.len == 0 {
		return false
	}
	for i in 0 .. typ.len {
		c := typ[i]
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_` || c == `.`) {
			return false
		}
	}
	return true
}

// check_struct_field_defaults validates check struct field defaults state for types.
fn (mut tc TypeChecker) check_struct_field_defaults(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl || field.children_count == 0 {
			continue
		}
		default_id := tc.a.child(field, 0)
		expected := tc.parse_type(field.typ)
		tc.check_node(default_id)
		actual := tc.resolve_expr(default_id, expected)
		if !tc.expr_compatible(default_id, actual, expected)
			&& !tc.pointer_value_compatible(actual, expected) {
			tc.type_mismatch(.assignment_mismatch,
				'cannot initialize field `${field.value}` with `${actual.name()}`; expected `${expected.name()}`',
				default_id)
		}
	}
}

// check_enum_backing_type validates explicit enum backing storage types.
fn (mut tc TypeChecker) check_enum_backing_type(node_id flat.NodeId, node flat.Node) {
	if node.kind != .enum_decl || node.generic_params.len == 0 {
		return
	}
	backing := node.generic_params[0].trim_space()
	if backing.len == 0 {
		return
	}
	backing_type := tc.parse_type(backing)
	if backing_type is Unknown || type_contains_unknown(backing_type) {
		tc.record_error(.unknown_type, 'unknown type `${backing}`', node_id)
		return
	}
	clean := unalias_type(backing_type)
	if !clean.is_integer() {
		tc.type_mismatch(.assignment_mismatch,
			'enum backing type `${backing}` must be integer, not `${clean.name()}`', node_id)
		return
	}
	if bounds := enum_backing_value_bounds(clean) {
		tc.check_backed_enum_field_value_ranges(node, backing, bounds)
	}
}

struct EnumBackingValueBounds {
	min     int
	max     int
	has_min bool
	has_max bool
	bits    int
}

fn enum_backing_value_bounds(typ Type) ?EnumBackingValueBounds {
	clean := unalias_type(typ)
	if clean is Primitive {
		if !clean.props.has(.integer) {
			return none
		}
		bits := match clean.size {
			8 { 8 }
			16 { 16 }
			32 { 32 }
			64 { 64 }
			else { 32 }
		}

		if clean.props.has(.unsigned) {
			return match clean.size {
				8 {
					EnumBackingValueBounds{
						min:     0
						max:     255
						has_min: true
						has_max: true
						bits:    bits
					}
				}
				16 {
					EnumBackingValueBounds{
						min:     0
						max:     65535
						has_min: true
						has_max: true
						bits:    bits
					}
				}
				else {
					EnumBackingValueBounds{
						min:     0
						has_min: true
						bits:    bits
					}
				}
			}
		}
		return match clean.size {
			8 {
				EnumBackingValueBounds{
					min:     -128
					max:     127
					has_min: true
					has_max: true
					bits:    bits
				}
			}
			16 {
				EnumBackingValueBounds{
					min:     -32768
					max:     32767
					has_min: true
					has_max: true
					bits:    bits
				}
			}
			32, 0 {
				EnumBackingValueBounds{
					min:     -2147483647 - 1
					max:     2147483647
					has_min: true
					has_max: true
					bits:    bits
				}
			}
			else {
				EnumBackingValueBounds{
					bits: bits
				}
			}
		}
	}
	if clean is Rune {
		return EnumBackingValueBounds{
			min:     -2147483647 - 1
			max:     2147483647
			has_min: true
			has_max: true
			bits:    32
		}
	}
	if clean is USize {
		return EnumBackingValueBounds{
			min:     0
			has_min: true
			bits:    64
		}
	}
	if clean is ISize {
		return EnumBackingValueBounds{
			bits: 64
		}
	}
	return none
}

fn (mut tc TypeChecker) check_backed_enum_field_value_ranges(node flat.Node, backing string, bounds EnumBackingValueBounds) {
	is_flag := node.typ == 'flag'
	mut field_values := map[string]int{}
	mut field_exprs := map[string]flat.NodeId{}
	mut field_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.nodes[int(field_id)]
		if field.kind != .enum_field {
			continue
		}
		field_ids << field_id
		if field.children_count > 0 {
			field_exprs[field.value] = tc.a.child(&field, 0)
		}
	}
	mut next_val := 0
	for field_id in field_ids {
		field := tc.a.nodes[int(field_id)]
		mut val := next_val
		mut has_checked_value := true
		if expr_id := field_exprs[field.value] {
			mut resolving := map[string]bool{}
			if resolved := tc.comptime_static_enum_field_value(expr_id, tc.cur_module, node.value, mut
				field_values, field_exprs, mut resolving)
			{
				val = resolved
			} else {
				tc.record_error(.assignment_mismatch,
					'enum field `${field.value}` value must be a compile-time integer expression to fit backing type `${backing}`',
					expr_id)
				has_checked_value = false
			}
		}
		field_values[field.value] = val
		if has_checked_value && !enum_backing_value_fits(val, bounds, is_flag) {
			if is_flag {
				tc.record_error(.assignment_mismatch,
					'enum field `${field.value}` bit ${val} does not fit backing type `${backing}`',
					field_id)
			} else {
				tc.record_error(.assignment_mismatch,
					'enum field `${field.value}` value ${val} does not fit backing type `${backing}`',
					field_id)
			}
		}
		next_val = val + 1
	}
}

fn enum_backing_value_fits(value int, bounds EnumBackingValueBounds, is_flag bool) bool {
	if is_flag {
		if value < 0 || bounds.bits <= 0 {
			return false
		}
		limit := if bounds.has_min && bounds.min < 0 { bounds.bits - 1 } else { bounds.bits }
		return value < limit
	}
	if bounds.has_min && value < bounds.min {
		return false
	}
	if bounds.has_max && value > bounds.max {
		return false
	}
	return true
}

// check_enum_field_values validates check enum field values state for types.
fn (mut tc TypeChecker) check_enum_field_values(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .enum_field || field.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field, 0)
		tc.check_node(value_id)
		value_type := tc.resolve_type(value_id)
		if value_type is Unknown {
			continue
		}
		if !value_type.is_integer() {
			tc.type_mismatch(.assignment_mismatch,
				'enum field `${field.value}` value must be integer, not `${value_type.name()}`',
				value_id)
		}
	}
}

// check_const_field_values validates check const field values state for types.
fn (mut tc TypeChecker) check_const_field_values(node flat.Node) {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .const_field || field.children_count == 0 {
			continue
		}
		tc.check_node(tc.a.child(field, 0))
	}
}

// fn_body_definitely_returns supports fn body definitely returns handling for TypeChecker.
fn (tc &TypeChecker) fn_body_definitely_returns(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		if tc.stmt_definitely_returns(child_id) {
			return true
		}
	}
	return false
}

fn type_allows_implicit_return(typ Type) bool {
	if typ is Void {
		return true
	}
	if typ is OptionType {
		return typ.base_type is Void
	}
	if typ is ResultType {
		return typ.base_type is Void
	}
	return false
}

// valid_node_id supports valid node id handling for TypeChecker.
fn (tc &TypeChecker) valid_node_id(id flat.NodeId) bool {
	return int(id) >= 0 && tc.a != unsafe { nil } && int(id) < tc.a.nodes.len
}

// stmt_definitely_returns supports stmt definitely returns handling for TypeChecker.
fn (tc &TypeChecker) stmt_definitely_returns(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return true
		}
		.expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.stmt_definitely_returns(tc.a.child(&node, 0))
		}
		.call {
			return tc.call_never_returns(id)
		}
		.block {
			for i in 0 .. node.children_count {
				if tc.stmt_definitely_returns(tc.a.child(&node, i)) {
					return true
				}
			}
			return false
		}
		.comptime_if {
			if node.children_count == 0 {
				return false
			}
			if node.children_count == 1 {
				return comptime_condition_strip_outer_parens(node.value) == 'true'
					&& tc.stmt_definitely_returns(tc.a.child(&node, 0))
			}
			return tc.stmt_definitely_returns(tc.a.child(&node, 0))
				&& tc.stmt_definitely_returns(tc.a.child(&node, 1))
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return tc.stmt_definitely_returns(tc.a.child(&node, 1))
				&& tc.stmt_definitely_returns(tc.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			for i in 1 .. node.children_count {
				branch := tc.a.child_node(&node, i)
				if branch.kind != .match_branch {
					return false
				}
				if !tc.match_branch_definitely_returns(branch) {
					return false
				}
			}
			return tc.match_has_else_or_exhaustive_coverage(node)
		}
		.lock_expr {
			// lock/rlock: objects first, body block last; the statement returns
			// when its body always returns.
			if node.children_count == 0 {
				return false
			}
			body := tc.a.child_node(&node, node.children_count - 1)
			if body.kind != .block {
				return false
			}
			return tc.stmt_definitely_returns(tc.a.child(&node, node.children_count - 1))
		}
		.for_stmt {
			// `for { ... }` with no condition and no break never falls through.
			if node.children_count < 3 {
				return false
			}
			cond := tc.a.child_node(&node, 1)
			if cond.kind != .empty {
				return false
			}
			for i in 3 .. node.children_count {
				if tc.subtree_contains_break(tc.a.child(&node, i)) {
					return false
				}
			}
			return true
		}
		else {
			return false
		}
	}
}

// subtree_contains_break reports whether any node under id is a `break`.
// Conservative: a break in a nested inner loop also counts, so an infinite
// outer loop is only treated as no-fallthrough when its body has no break at all.
fn (tc &TypeChecker) subtree_contains_break(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .break_stmt {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.subtree_contains_break(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) call_never_returns(id flat.NodeId) bool {
	return tc.resolved_call_never_returns(id)
}

fn (mut tc TypeChecker) call_never_returns_resolving(id flat.NodeId) bool {
	if tc.resolved_call_never_returns(id) {
		return true
	}
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .call {
		return false
	}
	info := tc.resolve_call_info(id, node) or { return false }
	if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
		tc.remember_resolved_call(id, info.name)
	}
	return tc.name_never_returns(info.name)
}

fn (mut tc TypeChecker) expr_never_returns_resolving(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_never_returns_resolving(tc.a.child(&node, 0))
		}
		.call {
			return tc.call_never_returns_resolving(id)
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) expr_never_returns(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_never_returns(tc.a.child(&node, 0))
		}
		.call {
			return tc.call_never_returns(id)
		}
		else {
			return false
		}
	}
}

// match_covers_all_enum_variants reports whether a `match` over an enum subject
// lists every variant of that enum (so it is exhaustive without an `else`).
fn (tc &TypeChecker) match_covers_all_enum_variants(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	subject_type := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	mut enum_name := ''
	if subject_type is Enum {
		enum_name = subject_type.name
	} else {
		return false
	}
	// A `[flag]` enum value can hold combined or zero bits (`.read | .write`, `0`) that
	// no single-field branch covers, so listing every field is NOT exhaustive — such a
	// match needs an explicit `else`.
	if enum_name in tc.flag_enums {
		return false
	}
	all_fields := tc.enum_fields[enum_name] or { return false }
	if all_fields.len == 0 {
		return false
	}
	mut covered := map[string]bool{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			return false
		}
		if branch.value == 'else' {
			return true
		}
		n_conds := branch.value.int()
		for j in 0 .. n_conds {
			cond := tc.a.child_node(branch, j)
			if cond.kind == .enum_val {
				covered[cond.value.all_after_last('.')] = true
			}
		}
	}
	for f in all_fields {
		if f !in covered {
			return false
		}
	}
	return true
}

// match_branch_definitely_returns
// supports helper handling in types.
fn (tc &TypeChecker) match_branch_definitely_returns(branch &flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	for i in body_start .. branch.children_count {
		if tc.stmt_definitely_returns(tc.a.child(branch, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) match_without_else_exhaustive_enum_returns(node flat.Node) bool {
	subject_type := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	if subject_type is Enum {
		enum_name := tc.resolve_enum_name(subject_type.name) or { subject_type.name }
		if subject_type.is_flag || enum_name in tc.flag_enums {
			return false
		}
		fields := tc.enum_fields[enum_name] or { return false }
		if fields.len == 0 {
			return false
		}
		mut covered := map[string]bool{}
		for i in 1 .. node.children_count {
			branch := tc.a.child_node(&node, i)
			if branch.kind != .match_branch || branch.value == 'else' {
				return false
			}
			n_conds := branch.value.int()
			if n_conds <= 0 || n_conds > branch.children_count {
				return false
			}
			for j in 0 .. n_conds {
				cond := tc.a.child_node(branch, j)
				field := tc.match_enum_condition_field(cond, enum_name) or { return false }
				covered[field] = true
			}
		}
		for field in fields {
			if field !in covered {
				return false
			}
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) match_without_else_exhaustive_bool_returns(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	raw_subject_type := unalias_type(tc.resolve_type(tc.a.child(&node, 0)))
	if raw_subject_type is Pointer {
		return false
	}
	subject_type := raw_subject_type
	if subject_type !is Primitive {
		return false
	}
	if !subject_type.props.has(.boolean) {
		return false
	}
	mut covered_true := false
	mut covered_false := false
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch || branch.value == 'else' {
			return false
		}
		n_conds := branch.value.int()
		if n_conds <= 0 || n_conds > branch.children_count {
			return false
		}
		for j in 0 .. n_conds {
			cond := tc.a.child_node(branch, j)
			if cond.kind == .bool_literal {
				if cond.value == 'true' {
					covered_true = true
				} else if cond.value == 'false' {
					covered_false = true
				}
			}
		}
	}
	return covered_true && covered_false
}

fn (tc &TypeChecker) match_has_else_or_exhaustive_coverage(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			return false
		}
		if branch.value == 'else' {
			return true
		}
	}
	return tc.match_without_else_exhaustive_enum_returns(node)
		|| tc.match_without_else_exhaustive_bool_returns(node)
		|| tc.match_without_else_exhaustive_sumtype_returns(node)
}

// match_without_else_exhaustive_sumtype_returns reports whether a `match` over a
// sum-type subject lists every variant (so it is exhaustive without an `else`).
fn (tc &TypeChecker) match_without_else_exhaustive_sumtype_returns(node flat.Node) bool {
	subject_type := unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0))))
	mut sum_name := ''
	if subject_type is SumType {
		sum_name = subject_type.name
	} else {
		return false
	}
	base := tc.sum_base_name(sum_name)
	variants := tc.sum_types[base] or { return false }
	if variants.len == 0 {
		return false
	}
	mut covered := map[string]bool{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			return false
		}
		if branch.value == 'else' {
			return true
		}
		n_conds := branch.value.int()
		if n_conds <= 0 || n_conds > branch.children_count {
			return false
		}
		for j in 0 .. n_conds {
			cond := tc.a.child_node(branch, j)
			pattern := tc.match_type_pattern(cond) or { return false }
			qpattern := tc.qualify_name(pattern)
			mut matched := false
			for variant in variants {
				if variant == pattern || variant == qpattern {
					covered[variant] = true
					matched = true
				}
			}
			if matched {
				continue
			}
			// Short-name fallback: only sound when exactly one variant carries
			// this short name — for `type S = a.Foo | b.Foo`, a branch matching
			// one `Foo` must not mark the other as covered.
			pattern_short := short_type_name(pattern)
			mut short_match := ''
			mut short_count := 0
			for variant in variants {
				if short_type_name(variant) == pattern_short {
					short_count++
					short_match = variant
				}
			}
			if short_count == 1 {
				covered[short_match] = true
			}
		}
	}
	for variant in variants {
		if variant !in covered {
			return false
		}
	}
	return true
}

fn unalias_type(t Type) Type {
	if t is Alias {
		return unalias_type(t.base_type)
	}
	return t
}

fn unalias_and_unwrap_pointer_type(t Type) Type {
	mut cur := t
	for _ in 0 .. 32 {
		if cur is Alias {
			cur = cur.base_type
			continue
		}
		if cur is Pointer {
			cur = cur.base_type
			continue
		}
		return cur
	}
	return cur
}

fn (tc &TypeChecker) match_enum_condition_field(cond &flat.Node, enum_name string) ?string {
	match cond.kind {
		.enum_val {
			field := cond.value.all_after_last('.')
			if tc.enum_value_matches(cond.value, enum_name) {
				return field
			}
		}
		.selector {
			if typ := tc.enum_selector_type(cond) {
				if typ is Enum {
					cond_enum_name := tc.resolve_enum_name(typ.name) or { typ.name }
					if cond_enum_name == enum_name && tc.enum_has_field(enum_name, cond.value) {
						return cond.value
					}
				}
			}
		}
		else {}
	}

	return none
}

// node_kind_id supports node kind id handling for types.
fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

struct ComptimeStaticFieldCase {
	name          string
	typ           string
	unaliased_typ string
	is_option     bool
	is_embed      bool
	is_array      bool
	is_map        bool
	is_chan       bool
	is_struct     bool
	is_enum       bool
	is_alias      bool
	is_shared     bool
	is_atomic     bool
	is_mut        bool
	is_pub        bool
	has_decl_meta bool
	indirections  int
}

struct ComptimeStaticFieldCases {
	known bool
	cases []ComptimeStaticFieldCase
}

struct ComptimeStaticValueCase {
	name          string
	location      string
	value         int
	has_value     bool
	typ           string
	return_type   string
	is_pub        bool
	has_is_pub    bool
	arg           string
	has_arg       bool
	has_attr_meta bool
	attr_kind     int
	param_names   []string
	param_types   []string
}

struct ComptimeStaticValueCases {
	known bool
	cases []ComptimeStaticValueCase
}

struct ComptimeStaticFieldDeclMeta {
	is_mut   bool
	is_pub   bool
	is_embed bool
	raw_typ  string
}

struct ComptimeStaticFieldTypeFlags {
mut:
	is_shared    bool
	is_atomic    bool
	indirections int
}

struct ComptimeDeferredDeclSource {
	module_name string
	decl_name   string
}

fn (mut tc TypeChecker) check_comptime_for_members(_id flat.NodeId, node flat.Node) {
	parts := node.value.split('|')
	if parts.len != 2 || parts[0].len == 0 || node.children_count == 0 {
		return
	}
	is_variant_loop := parts[1] == 'variants'
	if is_variant_loop {
		tc.cur_comptime_variant_loop_vars << parts[0]
	}
	defer {
		if is_variant_loop {
			tc.cur_comptime_variant_loop_vars.pop()
		}
	}
	body_id := tc.a.child(&node, 0)
	match parts[1] {
		'fields' {
			tc.check_comptime_members(body_id, parts[0], comptime_field_members, 'FieldData')
		}
		'values' {
			tc.check_comptime_members(body_id, parts[0], comptime_enum_value_members, 'EnumData')
		}
		'variants' {
			tc.check_comptime_members(body_id, parts[0], comptime_variant_members, 'VariantData')
		}
		else {}
	}

	// Method and parameter loops are resolved to concrete declarations by the transformer.
	// Their bodies can also reference metadata from an enclosing reflection loop, which does
	// not exist as a runtime local while checking the original generic body.
	if parts[1] in ['methods', 'params', 'attributes'] {
		deferred_cases := tc.comptime_static_deferred_cases(node.typ, parts[1])
		if deferred_cases.known && deferred_cases.cases.len == 0 {
			return
		}
		tc.check_comptime_static_body(body_id, parts[0], parts[1], ComptimeStaticFieldCases{},
			deferred_cases)
		return
	}

	field_cases := if parts[1] == 'fields' {
		tc.comptime_static_field_cases(node.typ)
	} else {
		ComptimeStaticFieldCases{}
	}
	value_cases := if parts[1] == 'values' {
		tc.comptime_static_enum_value_cases(node.typ)
	} else {
		ComptimeStaticValueCases{}
	}
	if field_cases.known && field_cases.cases.len == 0 {
		return
	}
	if value_cases.known && value_cases.cases.len == 0 {
		return
	}
	tc.check_comptime_static_body(body_id, parts[0], parts[1], field_cases, value_cases)
}

fn (tc &TypeChecker) comptime_static_deferred_cases(source string, loop_kind string) ComptimeStaticValueCases {
	if loop_kind == 'methods' {
		return tc.comptime_static_method_cases(source)
	}
	if loop_kind == 'params' {
		return tc.comptime_static_param_cases(source)
	}
	if loop_kind == 'attributes' {
		return tc.comptime_static_attribute_cases(source)
	}
	return ComptimeStaticValueCases{}
}

fn (tc &TypeChecker) comptime_static_method_cases(source string) ComptimeStaticValueCases {
	base_type := tc.comptime_static_for_base_type(source)
	clean_type := comptime_static_unwrap_type_text(base_type)
	generic_base, generic_args, is_generic := generic_type_application_parts(clean_type)
	if is_generic && !tc.generic_args_are_concrete(generic_args) {
		return ComptimeStaticValueCases{}
	}
	lookup_type := if is_generic { generic_base } else { clean_type }
	struct_name := tc.comptime_static_struct_name(lookup_type) or {
		return ComptimeStaticValueCases{}
	}
	generic_params := if is_generic {
		tc.struct_generic_params[struct_name] or {
			tc.struct_generic_params[struct_name.all_after_last('.')] or { []string{} }
		}
	} else {
		[]string{}
	}
	if is_generic && generic_params.len != generic_args.len {
		return ComptimeStaticValueCases{}
	}
	mut wanted_module := tc.struct_modules[struct_name] or { '' }
	if wanted_module.len == 0 {
		if decl_file := tc.struct_files[struct_name] {
			wanted_module = tc.file_modules[decl_file] or { '' }
		}
	}
	if wanted_module.len == 0 {
		wanted_module = if struct_name.contains('.') {
			struct_name.all_before_last('.')
		} else if tc.cur_module.len > 0 {
			tc.cur_module
		} else {
			'main'
		}
	}
	wanted_receiver := struct_name.all_after_last('.').all_before('[')
	mut module_name := ''
	mut file_name := ''
	mut cases := []ComptimeStaticValueCase{}
	mut seen := map[string]bool{}
	mut line_offsets_by_file := map[string][]int{}
	for idx in tc.top_level_idx {
		candidate := tc.a.nodes[idx]
		if candidate.kind == .file {
			module_name = 'main'
			file_name = candidate.value
			continue
		}
		if candidate.kind == .module_decl {
			module_name = candidate.value
			continue
		}
		if candidate.kind != .fn_decl || !tc.fn_has_receiver_param(candidate) {
			continue
		}
		receiver := candidate.value.all_before_last('.').all_after_last('.').all_before('[')
		candidate_module := if module_name.len > 0 { module_name } else { 'main' }
		if candidate_module == wanted_module && receiver == wanted_receiver {
			name := candidate.value.all_after_last('.')
			if name.len > 0 && name !in seen {
				seen[name] = true
				if file_name !in line_offsets_by_file {
					line_offsets_by_file[file_name] = comptime_static_source_line_offsets(file_name)
				}
				mut param_names := []string{}
				mut param_types := []string{}
				for i in 1 .. candidate.children_count {
					param := tc.a.child_node(&candidate, i)
					if param.kind == .param {
						param_names << param.value
						param_types << subst_generic_text(param.typ, generic_args, generic_params)
					}
				}
				return_type := subst_generic_text(if candidate.typ.len > 0 {
					candidate.typ
				} else {
					'void'
				}, generic_args, generic_params)
				cases << ComptimeStaticValueCase{
					name:        name
					location:    comptime_static_source_location(file_name, candidate.pos.offset,
						line_offsets_by_file[file_name])
					typ:         comptime_static_method_type_text(param_types, return_type)
					return_type: return_type
					is_pub:      candidate.op == .arrow
					has_is_pub:  true
					param_names: param_names
					param_types: param_types
				}
			}
		}
	}
	return ComptimeStaticValueCases{
		known: true
		cases: cases
	}
}

fn (tc &TypeChecker) comptime_static_param_cases(source string) ComptimeStaticValueCases {
	wanted := tc.comptime_deferred_decl_source(source, false) or {
		return ComptimeStaticValueCases{}
	}
	mut module_name := ''
	for idx in tc.top_level_idx {
		candidate := tc.a.nodes[idx]
		if candidate.kind == .file {
			module_name = 'main'
			continue
		}
		if candidate.kind == .module_decl {
			module_name = candidate.value
			continue
		}
		if candidate.kind != .fn_decl {
			continue
		}
		candidate_module := if module_name.len > 0 { module_name } else { 'main' }
		if candidate_module != wanted.module_name || candidate.value != wanted.decl_name {
			continue
		}
		param_start := if tc.fn_has_receiver_param(candidate) { 1 } else { 0 }
		mut cases := []ComptimeStaticValueCase{}
		for i in param_start .. candidate.children_count {
			param := tc.a.child_node(&candidate, i)
			if param.kind == .param {
				cases << ComptimeStaticValueCase{
					name: param.value
					typ:  param.typ
				}
			}
		}
		return ComptimeStaticValueCases{
			known: true
			cases: cases
		}
	}
	return ComptimeStaticValueCases{}
}

fn (tc &TypeChecker) comptime_static_attribute_cases(source string) ComptimeStaticValueCases {
	wanted := tc.comptime_deferred_decl_source(source, true) or {
		return ComptimeStaticValueCases{}
	}
	mut module_name := ''
	mut decl_id := -1
	for idx in tc.top_level_idx {
		candidate := tc.a.nodes[idx]
		if candidate.kind == .file {
			module_name = 'main'
			continue
		}
		if candidate.kind == .module_decl {
			module_name = candidate.value
			continue
		}
		if candidate.kind !in [.struct_decl, .enum_decl, .fn_decl, .type_decl, .interface_decl] {
			continue
		}
		candidate_module := if module_name.len > 0 { module_name } else { 'main' }
		if candidate_module == wanted.module_name && candidate.value == wanted.decl_name {
			decl_id = idx
			break
		}
	}
	if decl_id < 0 {
		return ComptimeStaticValueCases{}
	}
	marker := '@attributes:${decl_id}'
	for candidate in tc.a.nodes {
		if candidate.kind == .directive && candidate.value == marker {
			mut cases := []ComptimeStaticValueCase{cap: candidate.generic_params.len}
			kinds := if candidate.typ.len > 0 { candidate.typ.split(',') } else { []string{} }
			for idx, raw in candidate.generic_params {
				item := comptime_static_attribute_case(raw, if idx < kinds.len {
					kinds[idx].int()
				} else {
					0
				})
				name := item.name
				if name.len > 0 {
					cases << item
				}
			}
			return ComptimeStaticValueCases{
				known: true
				cases: cases
			}
		}
	}
	return ComptimeStaticValueCases{
		known: true
	}
}

fn comptime_static_method_type_text(param_types []string, return_type string) string {
	ret := if return_type.len > 0 && return_type != 'void' { ' ${return_type}' } else { '' }
	return 'fn(${param_types.join(', ')})${ret}'
}

fn comptime_static_source_line_offsets(path string) []int {
	source := os.read_file(path) or { return []int{} }
	mut offsets := []int{cap: source.len / 40 + 1}
	offsets << 0
	for i, ch in source {
		if ch == `\n` {
			offsets << i + 1
		}
	}
	return offsets
}

fn comptime_static_source_location(path string, encoded_offset int, line_offsets []int) string {
	if path.len == 0 || encoded_offset <= 0 || line_offsets.len == 0 {
		return ''
	}
	offset := encoded_offset - 1
	mut lo := 0
	mut hi := line_offsets.len
	for lo < hi {
		mid := (lo + hi) / 2
		if line_offsets[mid] <= offset {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	line_index := if lo > 0 { lo - 1 } else { 0 }
	return '${path}:${line_index + 1}:${offset - line_offsets[line_index]}'
}

fn comptime_static_attribute_case(raw string, kind int) ComptimeStaticValueCase {
	clean := raw.trim_space()
	colon := clean.index_u8(`:`)
	has_arg := colon >= 0 && !(kind == 1
		&& !comptime_static_attr_is_string_literal(clean[colon + 1..].trim_space()))
	mut name := if has_arg { clean[..colon].trim_space() } else { clean }
	if name.len >= 2 && name[0] in [`'`, `"`] && name[name.len - 1] == name[0] {
		name = name[1..name.len - 1]
	}
	mut arg := if has_arg { clean[colon + 1..].trim_space() } else { '' }
	if arg.len >= 3 && arg[0] == `r` && arg[1] in [`'`, `"`] && arg[arg.len - 1] == arg[1] {
		arg = arg[2..arg.len - 1]
	} else if arg.len >= 2 && arg[0] in [`'`, `"`] && arg[arg.len - 1] == arg[0] {
		arg = comptime_static_unescape(arg[1..arg.len - 1])
	}
	return ComptimeStaticValueCase{
		name:          name
		arg:           arg
		has_arg:       has_arg
		has_attr_meta: true
		attr_kind:     kind
	}
}

fn comptime_static_attr_is_string_literal(raw string) bool {
	return (raw.len >= 2 && raw[0] in [`'`, `"`] && raw[raw.len - 1] == raw[0])
		|| (raw.len >= 3 && raw[0] == `r` && raw[1] in [`'`, `"`] && raw[raw.len - 1] == raw[1])
}

fn (tc &TypeChecker) comptime_deferred_decl_source(source string, allow_type bool) ?ComptimeDeferredDeclSource {
	clean := source.trim_space()
	if clean.len == 0 {
		return none
	}
	if !clean.contains('.') {
		if allow_type {
			if resolved := tc.resolve_selective_import_type_symbol(clean) {
				return comptime_deferred_qualified_source(resolved)
			}
		}
		if resolved := tc.resolve_selective_import_symbol(clean) {
			return comptime_deferred_qualified_source(resolved)
		}
		return ComptimeDeferredDeclSource{
			module_name: if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
			decl_name:   clean
		}
	}
	dot := clean.index_u8(`.`)
	first := clean[..dot]
	if resolved := tc.resolve_import_alias(first) {
		return ComptimeDeferredDeclSource{
			module_name: resolved
			decl_name:   clean[dot + 1..]
		}
	}
	if first == tc.cur_module {
		return ComptimeDeferredDeclSource{
			module_name: first
			decl_name:   clean[dot + 1..]
		}
	}
	return ComptimeDeferredDeclSource{
		module_name: if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
		decl_name:   clean
	}
}

fn comptime_deferred_qualified_source(source string) ComptimeDeferredDeclSource {
	return ComptimeDeferredDeclSource{
		module_name: source.all_before_last('.')
		decl_name:   source.all_after_last('.')
	}
}

fn (mut tc TypeChecker) check_comptime_members(id flat.NodeId, var_name string, members []string, meta_name string) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_for {
		tc.check_comptime_for_members(id, node)
		if comptime_for_declares_var_in_value(node.value, var_name) {
			return
		}
	}
	if node.kind == .comptime_if {
		if member := comptime_cond_unknown_member(node.value, var_name, members) {
			tc.record_error(.unknown_field, 'unknown ${meta_name} member `${member}`', id)
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_id := tc.a.child(&node, 0)
		if tc.valid_node_id(base_id) {
			base := tc.a.nodes[int(base_id)]
			if base.kind == .ident && base.value == var_name && node.value !in members {
				tc.record_error(.unknown_field, 'unknown ${meta_name} member `${node.value}`', id)
			}
		}
	}
	for i in 0 .. node.children_count {
		tc.check_comptime_members(tc.a.child(&node, i), var_name, members, meta_name)
	}
}

fn comptime_for_declares_var_in_value(value string, var_name string) bool {
	if idx := value.index('|') {
		return value[..idx] == var_name
	}
	return value == var_name
}

fn comptime_cond_unknown_member(cond string, var_name string, members []string) ?string {
	prefix := '${var_name}.'
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + prefix.len > cond.len || cond[offset..offset + prefix.len] != prefix {
			offset++
			continue
		}
		if offset > 0 && comptime_cond_name_char(cond[offset - 1]) {
			offset++
			continue
		}
		member_start := offset + prefix.len
		mut member_end := member_start
		for member_end < cond.len && comptime_cond_name_char(cond[member_end]) {
			member_end++
		}
		if member_end == member_start {
			offset = member_start
			continue
		}
		member := cond[member_start..member_end]
		if member !in members {
			return member
		}
		offset = member_end
	}
	return none
}

fn comptime_cond_skip_string(cond string, start int) int {
	quote := cond[start]
	mut i := start + 1
	for i < cond.len {
		if cond[i] == `\\` {
			i += 2
			continue
		}
		if cond[i] == quote {
			return i + 1
		}
		i++
	}
	return cond.len
}

fn comptime_cond_name_char(ch u8) bool {
	return ch.is_letter() || ch.is_digit() || ch == `_`
}

fn (mut tc TypeChecker) check_comptime_static_body(id flat.NodeId, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		// Locals registered while walking this block (declarations whose RHS
		// references the loop var) must not leak past it.
		tc.push_scope()
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		tc.pop_scope()
		return
	}
	if node.kind == .comptime_for {
		return
	}
	if node.kind == .if_expr {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		return
	}
	if node.kind in [.expr_stmt, .return_stmt] {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		return
	}
	if node.kind == .comptime_if && comptime_text_references_var(node.value, var_name) {
		if loop_kind in ['methods', 'params', 'attributes'] {
			tc.check_comptime_static_deferred_metadata_if(node, var_name, loop_kind, field_cases,
				value_cases)
			return
		}
		tc.check_comptime_static_metadata_if(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if !tc.comptime_subtree_references_var(id, var_name) {
		tc.check_node(id)
		return
	}
	if node.kind == .call {
		tc.check_comptime_static_call(id, node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	// A declaration whose RHS references the loop var is not checked here, but
	// its locals must still enter the scope: later statements in the unrolled
	// body use them (`mut fo := ...(field.attrs); ... fo.install_default(...)`),
	// and without a binding those uses report unknown identifiers.
	if node.kind == .decl_assign && node.children_count >= 2 {
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			lhs := tc.a.child_node(&node, i)
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
				continue
			}
			rhs_typ := tc.resolve_type(tc.a.child(&node, i + 1))
			typ := if rhs_typ is Void { Type(Unknown{}) } else { rhs_typ }
			tc.cur_scope.insert(lhs.value, typ)
		}
	}
}

fn (mut tc TypeChecker) check_comptime_static_deferred_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if !value_cases.known {
		return
	}
	mut check_then := false
	mut check_else := false
	for item in value_cases.cases {
		cond := comptime_static_subst_deferred_cond(node.value, var_name, loop_kind, item)
		if comptime_text_references_var(cond, var_name) {
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or { continue }
		if taken {
			check_then = true
		} else {
			check_else = true
		}
	}
	if check_then && node.children_count > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, field_cases,
			value_cases)
	}
	if check_else && node.children_count > 1 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases,
			value_cases)
	}
}

fn comptime_static_subst_deferred_cond(cond string, var_name string, loop_kind string, item ComptimeStaticValueCase) string {
	mut result := comptime_static_replace_unquoted(cond, '${var_name}.name',
		comptime_static_string_literal(item.name))
	if loop_kind == 'methods' {
		result = comptime_static_subst_method_param_cond(result, var_name, item)
		result = comptime_static_replace_unquoted(result, '${var_name}.args.len',
			item.param_names.len.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.params.len',
			item.param_names.len.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.location',
			comptime_static_string_literal(item.location))
		result = comptime_static_replace_unquoted(result, '${var_name}.return_type',
			comptime_static_deferred_type(item.return_type))
		result = comptime_static_replace_unquoted(result, '${var_name}.typ', item.typ)
		if item.has_is_pub {
			result = comptime_static_replace_unquoted(result, '${var_name}.is_pub',
				item.is_pub.str())
		}
	} else if loop_kind == 'params' {
		result = comptime_static_replace_unquoted(result, '${var_name}.typ',
			comptime_static_deferred_type(item.typ))
	} else if loop_kind == 'attributes' && item.has_attr_meta {
		result = comptime_static_replace_unquoted(result, '${var_name}.has_arg', item.has_arg.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.arg',
			comptime_static_string_literal(item.arg))
		kind := comptime_static_attribute_kind_value(item.attr_kind)
		result = comptime_static_replace_unquoted(result, '${var_name}.kind ==.', '${kind} == .')
		result = comptime_static_replace_unquoted(result, '${var_name}.kind !=.', '${kind} != .')
		result = comptime_static_replace_unquoted(result, '${var_name}.kind', kind)
	}
	return result
}

fn comptime_static_subst_method_param_cond(cond string, var_name string, item ComptimeStaticValueCase) string {
	mut result := cond
	for collection in ['args', 'params'] {
		prefix := '${var_name}.${collection}['
		mut offset := 0
		for offset < result.len {
			if result[offset] == `'` || result[offset] == `"` {
				offset = comptime_cond_skip_string(result, offset)
				continue
			}
			if !result[offset..].starts_with(prefix)
				|| (offset > 0 && comptime_cond_name_char(result[offset - 1])) {
				offset++
				continue
			}
			start := offset
			index_start := start + prefix.len
			rel_end := result[index_start..].index_u8(`]`)
			if rel_end < 0 {
				break
			}
			index_end := index_start + rel_end
			member_start := index_end + 1
			member := if result[member_start..].starts_with('.typ') {
				'typ'
			} else if result[member_start..].starts_with('.name') {
				'name'
			} else {
				''
			}
			member_end := member_start + member.len + 1
			if member.len == 0
				|| (member_end < result.len && comptime_cond_name_char(result[member_end])) {
				offset = member_start
				continue
			}
			index_text := result[index_start..index_end].trim_space()
			if !comptime_static_is_int(index_text) || index_text.starts_with('-') {
				offset = member_end
				continue
			}
			index := index_text.int()
			replacement := if index >= 0 && index < item.param_names.len {
				if member == 'name' {
					comptime_static_string_literal(item.param_names[index])
				} else if index < item.param_types.len {
					comptime_static_deferred_type(item.param_types[index])
				} else {
					'__v3_missing_method_param_type'
				}
			} else if member == 'name' {
				"''"
			} else {
				'__v3_missing_method_param_type'
			}
			result = result[..start] + replacement + result[member_end..]
			offset = start + replacement.len
		}
	}
	return result
}

fn comptime_static_deferred_type(typ string) string {
	return if typ == '&void' { 'voidptr' } else { typ }
}

fn comptime_static_attribute_kind_value(kind int) string {
	return match kind {
		1 { '.string' }
		2 { '.number' }
		3 { '.bool' }
		4 { '.comptime_define' }
		else { '.plain' }
	}
}

fn comptime_static_replace_unquoted(cond string, needle string, replacement string) string {
	if needle.len == 0 || !cond.contains(needle) {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + needle.len <= cond.len && cond[offset..offset + needle.len] == needle {
			before_ok := !comptime_cond_name_char(needle[0]) || offset == 0
				|| !comptime_cond_name_char(cond[offset - 1])
			after := offset + needle.len
			after_ok := !comptime_cond_name_char(needle[needle.len - 1]) || after >= cond.len
				|| !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset..offset + 1]
		offset++
	}
	return out
}

fn (mut tc TypeChecker) check_comptime_static_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if loop_kind == 'values' {
		tc.check_comptime_static_value_metadata_if(node, var_name, loop_kind, field_cases,
			value_cases)
		return
	}
	if !field_cases.known {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		return
	}
	mut check_then := false
	mut check_else := false
	for f in field_cases.cases {
		cond := tc.comptime_static_subst_field_cond(node.value, var_name, f)
		if comptime_text_references_var(cond, var_name) {
			check_then = true
			check_else = true
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or {
			check_then = true
			check_else = true
			continue
		}
		if taken {
			check_then = true
		} else {
			check_else = true
		}
	}
	if check_then && node.children_count > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, field_cases,
			value_cases)
	}
	if check_else && node.children_count > 1 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases,
			value_cases)
	}
}

fn (mut tc TypeChecker) check_comptime_static_value_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if !value_cases.known {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		return
	}
	mut check_then := false
	mut check_else := false
	for item in value_cases.cases {
		cond := comptime_static_subst_value_cond(node.value, var_name, item)
		if comptime_text_references_var(cond, var_name) {
			check_then = true
			check_else = true
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or {
			check_then = true
			check_else = true
			continue
		}
		if taken {
			check_then = true
		} else {
			check_else = true
		}
	}
	if check_then && node.children_count > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, field_cases,
			value_cases)
	}
	if check_else && node.children_count > 1 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases,
			value_cases)
	}
}

fn (tc &TypeChecker) comptime_subtree_references_var(id flat.NodeId, var_name string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_for && comptime_for_declares_var_in_value(node.value, var_name) {
		return false
	}
	if node.kind == .ident && node.value == var_name {
		return true
	}
	if node.kind == .comptime_if && comptime_text_references_var(node.value, var_name) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.comptime_subtree_references_var(tc.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_comptime_static_call(id flat.NodeId, node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if node.children_count == 0 {
		return
	}
	callee_id := tc.a.child(&node, 0)
	if tc.comptime_subtree_references_var(callee_id, var_name) {
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if _ := tc.sum_constructor_call_name(node) {
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if info0 := tc.resolve_call_info(id, node) {
		info := tc.specialized_plain_generic_call_info(node, info0)
		if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
			tc.remember_resolved_call(id, info.name)
		}
		if info.return_type !is Void && info.return_type !is Unknown {
			tc.remember_expr_type(id, info.return_type)
		}
		tc.check_comptime_static_call_metadata_arg_types(id, node, info, var_name, loop_kind)
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if tc.is_unsupported_hex_call(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
		}
		return
	}
	if tc.call_has_ambiguous_selective_import(node) {
		tc.record_error(.unknown_fn, 'ambiguous selective import `${tc.call_display_name(node)}`',
			id)
		return
	}
	if tc.should_diagnose(id) && !tc.is_known_call(node)
		&& !tc.call_generic_args_have_placeholders(node) {
		tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
	}
	tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
}

fn (mut tc TypeChecker) check_comptime_static_call_metadata_arg_types(id flat.NodeId, node flat.Node, info CallInfo, var_name string, loop_kind string) {
	if !info.params_known {
		return
	}
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	actual_count := node.children_count - 1 - info.arg_offset - field_init_args + collapsed +
		recv_extra
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	for i in 1 + info.arg_offset .. node.children_count {
		raw_arg := tc.a.child_node(&node, i)
		if raw_arg.kind == .field_init {
			continue
		}
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) + arg_shift
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			continue
		}
		if param_idx >= info.params.len {
			continue
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		actual := tc.comptime_static_metadata_expr_type(arg_id, var_name, loop_kind) or { continue }
		expected := tc.call_arg_expected_type(info, param_idx)
		if !tc.receiver_compatible(actual, expected) && !tc.type_compatible(actual, expected) {
			if expected is Pointer && tc.expr_tail_is_nil(arg_id) {
				continue
			}
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
		}
	}
}

fn (tc &TypeChecker) comptime_static_metadata_expr_type(id flat.NodeId, var_name string, loop_kind string) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.comptime_static_metadata_expr_type(tc.a.child(&node, 0), var_name, loop_kind)
	}
	if node.kind == .ident && node.value == var_name {
		metadata_type := match loop_kind {
			'methods' { 'FunctionData' }
			'params' { 'FunctionParam' }
			'attributes' { 'VAttribute' }
			'values' { 'EnumData' }
			'variants' { 'VariantData' }
			else { 'FieldData' }
		}

		return tc.parse_type(metadata_type)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return tc.comptime_static_metadata_member_type(node.value, loop_kind)
		}
	}
	return none
}

fn (tc &TypeChecker) comptime_static_metadata_member_type(member string, loop_kind string) ?Type {
	if loop_kind == 'methods' {
		return match member {
			'name', 'location' {
				tc.parse_type('string')
			}
			'is_pub' {
				tc.parse_type('bool')
			}
			'return_type', 'typ' {
				tc.parse_type('int')
			}
			'args', 'params' {
				tc.parse_type('[]FunctionParam')
			}
			'attrs' {
				tc.parse_type('[]string')
			}
			'attributes' {
				tc.parse_type('[]VAttribute')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'params' {
		return match member {
			'name' {
				tc.parse_type('string')
			}
			'typ' {
				tc.parse_type('int')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'attributes' {
		return match member {
			'name', 'arg' {
				tc.parse_type('string')
			}
			'has_arg' {
				tc.parse_type('bool')
			}
			'kind' {
				tc.parse_type('AttributeKind')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'variants' {
		return match member {
			'typ' {
				tc.parse_type('int')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'values' {
		return match member {
			'name' {
				tc.parse_type('string')
			}
			'value' {
				tc.parse_type('i64')
			}
			'attrs' {
				tc.parse_type('[]string')
			}
			else {
				none
			}
		}
	}
	return match member {
		'name' {
			tc.parse_type('string')
		}
		'typ', 'unaliased_typ' {
			tc.parse_type('int')
		}
		'attrs' {
			tc.parse_type('[]string')
		}
		'indirections' {
			tc.parse_type('u8')
		}
		'is_option', 'is_opt', 'is_embed', 'is_array', 'is_map', 'is_chan', 'is_struct', 'is_enum',
		'is_alias', 'is_shared', 'is_atomic', 'is_mut', 'is_pub' {
			tc.parse_type('bool')
		}
		else {
			none
		}
	}
}

fn (mut tc TypeChecker) check_comptime_static_call_args(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	for i in 1 .. node.children_count {
		tc.check_comptime_static_body(tc.call_arg_value(tc.a.child(&node, i)), var_name, loop_kind,
			field_cases, value_cases)
	}
}

fn (mut tc TypeChecker) comptime_static_enum_value_cases(base_type string) ComptimeStaticValueCases {
	source_type := tc.comptime_static_for_base_type(base_type)
	enum_name := tc.comptime_static_enum_name(source_type) or { return ComptimeStaticValueCases{} }
	names := tc.enum_fields[enum_name] or { return ComptimeStaticValueCases{
		known: true
	} }
	metas := tc.comptime_static_enum_decl_value_cases(enum_name)
	if metas.len > 0 {
		return ComptimeStaticValueCases{
			known: true
			cases: metas
		}
	}
	mut cases := []ComptimeStaticValueCase{cap: names.len}
	is_flag := enum_name in tc.flag_enums
	for idx, name in names {
		cases << ComptimeStaticValueCase{
			name:      name
			value:     if is_flag { 1 << idx } else { idx }
			has_value: true
		}
	}
	return ComptimeStaticValueCases{
		known: true
		cases: cases
	}
}

fn (tc &TypeChecker) comptime_static_enum_name(raw string) ?string {
	mut cur := raw.trim_space()
	mut seen := map[string]bool{}
	for cur.len > 0 && cur !in seen {
		seen[cur] = true
		mut candidates := [cur, tc.qualify_name(cur)]
		if resolved := tc.resolve_selective_import_type_symbol(cur) {
			candidates << resolved
		}
		for candidate in candidates {
			if candidate in tc.enum_names {
				return candidate
			}
		}
		next := tc.alias_target_type_text(cur) or { break }
		if next == cur {
			break
		}
		cur = next.trim_space()
	}
	return none
}

fn comptime_static_subst_value_cond(cond string, var_name string, item ComptimeStaticValueCase) string {
	mut c := cond
	if item.has_value {
		c = c.replace('${var_name}.value', item.value.str())
	}
	c = c.replace('${var_name}.name', "'${item.name}'")
	c = comptime_static_replace_bare_ident(c, var_name, 'EnumData')
	return c
}

fn comptime_text_references_member(cond string, var_name string) bool {
	prefix := '${var_name}.'
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + prefix.len > cond.len || cond[offset..offset + prefix.len] != prefix {
			offset++
			continue
		}
		if offset > 0 && comptime_cond_name_char(cond[offset - 1]) {
			offset++
			continue
		}
		member_start := offset + prefix.len
		if member_start < cond.len && comptime_cond_name_char(cond[member_start]) {
			return true
		}
		offset = member_start
	}
	return false
}

fn comptime_text_references_var(cond string, var_name string) bool {
	if var_name.len == 0 {
		return false
	}
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + var_name.len <= cond.len && cond[offset..offset + var_name.len] == var_name {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + var_name.len
			after_ok := after >= cond.len || !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				return true
			}
		}
		offset++
	}
	return false
}

fn (mut tc TypeChecker) comptime_static_field_cases(base_type string) ComptimeStaticFieldCases {
	source_type := tc.comptime_static_for_base_type(base_type)
	struct_name := tc.comptime_static_struct_name(source_type) or {
		return ComptimeStaticFieldCases{}
	}
	fields := tc.structs[struct_name] or { return ComptimeStaticFieldCases{
		known: true
	} }
	decl_metas := tc.comptime_static_field_decl_metas(struct_name)
	mut cases := []ComptimeStaticFieldCase{cap: fields.len}
	for field in fields {
		typ := field.typ.name()
		unaliased_typ := tc.comptime_type_match_type(typ).name()
		core_type := comptime_static_unwrap_field_type(field.typ)
		decl_meta := decl_metas[field.name] or { ComptimeStaticFieldDeclMeta{} }
		raw_typ := if decl_meta.raw_typ.len > 0 { decl_meta.raw_typ } else { typ }
		type_flags := comptime_static_field_type_flags(raw_typ)
		cases << ComptimeStaticFieldCase{
			name:          field.name
			typ:           typ
			unaliased_typ: unaliased_typ
			is_option:     field.typ is OptionType || tc.comptime_type_match_type(typ) is OptionType
			is_embed:      decl_meta.is_embed
			is_array:      core_type is Array || core_type is ArrayFixed
			is_map:        core_type is Map
			is_chan:       core_type is Channel
			is_struct:     tc.comptime_static_type_is_struct(core_type, typ, raw_typ)
			is_enum:       core_type is Enum && core_type.name() in tc.enum_names
			is_alias:      field.typ is Alias || typ in tc.type_aliases
				|| tc.qualify_name(typ) in tc.type_aliases
			is_shared:     type_flags.is_shared
			is_atomic:     type_flags.is_atomic
			is_mut:        decl_meta.is_mut
			is_pub:        decl_meta.is_pub
			has_decl_meta: field.name in decl_metas
			indirections:  type_flags.indirections
		}
	}
	return ComptimeStaticFieldCases{
		known: true
		cases: cases
	}
}

fn (tc &TypeChecker) comptime_static_type_is_struct(typ Type, typ_name string, raw_typ string) bool {
	if typ is Struct && tc.comptime_static_type_name_is_struct(typ.name()) {
		return true
	}
	if tc.comptime_static_type_name_is_struct(typ_name) {
		return true
	}
	return tc.comptime_static_type_name_is_struct(raw_typ)
}

fn (tc &TypeChecker) comptime_static_type_name_is_struct(name string) bool {
	clean := comptime_static_unwrap_type_text(name)
	if clean.len == 0 {
		return false
	}
	if clean in tc.structs {
		return true
	}
	base, _, is_generic := generic_type_application_parts(clean)
	if !is_generic {
		return false
	}
	if base in tc.structs || base in tc.struct_generic_params {
		return true
	}
	qbase := tc.qualify_name(base)
	if qbase in tc.structs || qbase in tc.struct_generic_params {
		return true
	}
	if resolved := tc.resolve_selective_import_type_symbol(base) {
		return resolved in tc.structs || resolved in tc.struct_generic_params
	}
	return false
}

fn comptime_static_unwrap_type_text(name string) string {
	mut clean := name.trim_space()
	for _ in 0 .. 16 {
		if clean.starts_with('?') || clean.starts_with('!') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('atomic ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		break
	}
	return clean
}

fn (tc &TypeChecker) comptime_static_for_base_type(raw string) string {
	if source := tc.comptime_static_for_value_source_type(raw) {
		return source
	}
	return raw
}

fn (tc &TypeChecker) comptime_static_for_value_source_type(raw string) ?string {
	clean := raw.trim_space()
	if clean.len == 0 {
		return none
	}
	parts := clean.split('.')
	if parts.len == 0 {
		return none
	}
	mut typ := tc.comptime_static_for_var_source_type(parts[0]) or { return none }
	for field in parts[1..] {
		typ = tc.comptime_static_for_field_source_type(typ, field) or { return none }
	}
	return typ
}

fn (tc &TypeChecker) comptime_static_for_var_source_type(name string) ?string {
	if tc.cur_scope != unsafe { nil } {
		if typ := tc.cur_scope.lookup(name) {
			return comptime_static_source_type_name(typ)
		}
	}
	if tc.file_scope != unsafe { nil } {
		if typ := tc.file_scope.lookup(name) {
			return comptime_static_source_type_name(typ)
		}
		qname := tc.qualify_name(name)
		if qname != name {
			if typ := tc.file_scope.lookup(qname) {
				return comptime_static_source_type_name(typ)
			}
		}
	}
	return none
}

fn (tc &TypeChecker) comptime_static_enum_decl_value_cases(enum_name string) []ComptimeStaticValueCase {
	mut cur_mod := ''
	if tc.top_level_idx.len > 0 {
		for idx in tc.top_level_idx {
			kind := tc.a.nodes[idx].kind
			if kind == .module_decl {
				cur_mod = tc.a.nodes[idx].value
				continue
			}
			if kind != .enum_decl {
				continue
			}
			if cases := tc.comptime_static_enum_decl_value_cases_for_node(enum_name, cur_mod,
				tc.a.nodes[idx])
			{
				return cases
			}
		}
		return []ComptimeStaticValueCase{}
	}
	for idx in 0 .. tc.a.nodes.len {
		kind := tc.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = tc.a.nodes[idx].value
			continue
		}
		if kind != .enum_decl {
			continue
		}
		if cases := tc.comptime_static_enum_decl_value_cases_for_node(enum_name, cur_mod,
			tc.a.nodes[idx])
		{
			return cases
		}
	}
	return []ComptimeStaticValueCase{}
}

fn (tc &TypeChecker) comptime_static_enum_decl_value_cases_for_node(enum_name string, cur_mod string, node flat.Node) ?[]ComptimeStaticValueCase {
	qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
		'${cur_mod}.${node.value}'
	} else {
		node.value
	}
	if enum_name != node.value && enum_name != qualified {
		return none
	}
	is_flag := node.typ == 'flag'
	mut field_order := []string{}
	mut field_exprs := map[string]flat.NodeId{}
	for i in 0 .. node.children_count {
		f := tc.a.child_node(&node, i)
		if f.kind != .enum_field {
			continue
		}
		field_order << f.value
		if f.children_count > 0 {
			field_exprs[f.value] = tc.a.child(f, 0)
		}
	}
	mut out := []ComptimeStaticValueCase{}
	mut field_values := map[string]int{}
	mut next_val := 0
	for field_name in field_order {
		mut val := next_val
		if expr_id := field_exprs[field_name] {
			mut resolving := map[string]bool{}
			if ev := tc.comptime_static_enum_field_value(expr_id, cur_mod, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				val = ev
			}
		}
		field_values[field_name] = val
		out << ComptimeStaticValueCase{
			name:      field_name
			value:     if is_flag { 1 << val } else { val }
			has_value: true
		}
		next_val = val + 1
	}
	return out
}

fn (tc &TypeChecker) comptime_static_enum_field_value(id flat.NodeId, enum_module string, enum_name string, mut field_values map[string]int, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?int {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			if v := v_int_literal_value(node.value) {
				return v
			}
		}
		.ident, .enum_val {
			if ev := tc.comptime_static_enum_field_ref_value(node.value, enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				return ev
			}
			return tc.const_int_value_in_module(node.value, enum_module, []string{})
		}
		.paren {
			if node.children_count > 0 {
				return tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
					enum_name, mut field_values, field_exprs, mut resolving)
			}
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			cast_type := unalias_type(tc.parse_type(node.value))
			if !cast_type.is_integer() {
				return none
			}
			return tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving) or { return none }
			return match node.op {
				.minus { -value }
				.plus { value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving) or { return none }
			right := tc.comptime_static_enum_field_value(tc.a.child(&node, 1), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving) or { return none }
			if (node.op == .div || node.op == .mod) && right == 0 {
				return none
			}
			if (node.op == .left_shift || node.op == .right_shift
				|| node.op == .right_shift_unsigned) && (right < 0 || right >= 64) {
				return none
			}
			return match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div { left / right }
				.mod { left % right }
				.left_shift { int(u64(left) << right) }
				.right_shift { left >> right }
				.right_shift_unsigned { int(u64(left) >> right) }
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				else { none }
			}
		}
		.selector {
			if field := tc.comptime_static_enum_selector_ref_field(id, enum_module, enum_name) {
				return tc.comptime_static_enum_field_ref_value(field, enum_module, enum_name, mut
					field_values, field_exprs, mut resolving)
			}
			return none
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) comptime_static_enum_field_ref_value(field_name string, enum_module string, enum_name string, mut field_values map[string]int, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?int {
	if field_name in field_values {
		return field_values[field_name]
	}
	expr_id := field_exprs[field_name] or { return none }
	if resolving[field_name] {
		return none
	}
	resolving[field_name] = true
	maybe_val := tc.comptime_static_enum_field_value(expr_id, enum_module, enum_name, mut
		field_values, field_exprs, mut resolving)
	resolving.delete(field_name)
	val := maybe_val?
	field_values[field_name] = val
	return val
}

fn (tc &TypeChecker) comptime_static_enum_selector_ref_field(id flat.NodeId, enum_module string, enum_name string) ?string {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	prefix := tc.comptime_static_enum_selector_base_text(tc.a.child(&node, 0))
	if !comptime_static_enum_ref_prefix_matches(prefix, enum_module, enum_name) {
		return none
	}
	return node.value
}

fn (tc &TypeChecker) comptime_static_enum_selector_base_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := tc.comptime_static_enum_selector_base_text(tc.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		else {
			return ''
		}
	}
}

fn comptime_static_enum_ref_prefix_matches(prefix string, enum_module string, enum_name string) bool {
	if prefix.len == 0 || enum_name.len == 0 {
		return false
	}
	short := enum_name.all_after_last('.')
	if prefix == enum_name || prefix == short {
		return true
	}
	if enum_module.len > 0 && prefix == '${enum_module}.${short}' {
		return true
	}
	return false
}

fn (tc &TypeChecker) comptime_static_for_field_source_type(owner_type string, field_name string) ?string {
	struct_name := tc.comptime_static_struct_name(owner_type) or { return none }
	typ := tc.struct_field_type(struct_name, field_name) or { return none }
	return comptime_static_source_type_name(typ)
}

fn comptime_static_source_type_name(typ Type) string {
	mut cur := typ
	for _ in 0 .. 16 {
		if cur is Pointer {
			cur = cur.base_type
			continue
		}
		if cur is OptionType {
			cur = cur.base_type
			continue
		}
		if cur is ResultType {
			cur = cur.base_type
			continue
		}
		return cur.name()
	}
	return cur.name()
}

fn (tc &TypeChecker) comptime_static_struct_name(raw string) ?string {
	mut cur := raw.trim_space()
	mut seen := map[string]bool{}
	for cur.len > 0 && cur !in seen {
		seen[cur] = true
		mut candidates := [cur, tc.qualify_name(cur)]
		if resolved := tc.resolve_selective_import_type_symbol(cur) {
			candidates << resolved
		}
		for candidate in candidates {
			if candidate in tc.structs {
				return candidate
			}
		}
		next := tc.alias_target_type_text(cur) or { break }
		if next == cur {
			break
		}
		cur = next.trim_space()
	}
	return none
}

fn (tc &TypeChecker) comptime_static_field_decl_metas(base_type string) map[string]ComptimeStaticFieldDeclMeta {
	mut out := map[string]ComptimeStaticFieldDeclMeta{}
	mut decl_name := base_type.trim_space()
	if idx := decl_name.index('[') {
		decl_name = decl_name[..idx]
	}
	mut cur_mod := ''
	if tc.top_level_idx.len > 0 {
		for idx in tc.top_level_idx {
			kind := tc.a.nodes[idx].kind
			if kind == .module_decl {
				cur_mod = tc.a.nodes[idx].value
				continue
			}
			if kind != .struct_decl {
				continue
			}
			if metas := tc.comptime_static_field_decl_metas_for_node(decl_name, cur_mod,
				tc.a.nodes[idx])
			{
				return metas
			}
		}
		return out
	}
	for idx in 0 .. tc.a.nodes.len {
		kind := tc.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = tc.a.nodes[idx].value
			continue
		}
		if kind != .struct_decl {
			continue
		}
		if metas := tc.comptime_static_field_decl_metas_for_node(decl_name, cur_mod,
			tc.a.nodes[idx])
		{
			return metas
		}
	}
	return out
}

fn (tc &TypeChecker) comptime_static_field_decl_metas_for_node(decl_name string, cur_mod string, node flat.Node) ?map[string]ComptimeStaticFieldDeclMeta {
	qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
		'${cur_mod}.${node.value}'
	} else {
		node.value
	}
	if decl_name != node.value && decl_name != qualified {
		return none
	}
	mut out := map[string]ComptimeStaticFieldDeclMeta{}
	for i in 0 .. node.children_count {
		f := tc.a.child_node(&node, i)
		if f.kind != .field_decl {
			continue
		}
		raw_typ := if f.typ.len > 0 { f.typ } else { f.value }
		mut is_mut := false
		mut is_pub := false
		if f.generic_params.len > 0 {
			flags := f.generic_params[0]
			is_mut = flags.contains('m')
			is_pub = flags.contains('p')
		}
		out[f.value] = ComptimeStaticFieldDeclMeta{
			is_mut:   is_mut
			is_pub:   is_pub
			is_embed: source_field_decl_is_embed(f, raw_typ)
			raw_typ:  raw_typ
		}
	}
	return out
}

fn comptime_static_unwrap_field_type(typ Type) Type {
	mut cur := typ
	for _ in 0 .. 16 {
		if cur is Alias {
			cur = cur.base_type
			continue
		}
		if cur is OptionType {
			cur = cur.base_type
			continue
		}
		if cur is Pointer {
			cur = cur.base_type
			continue
		}
		return cur
	}
	return cur
}

fn comptime_static_field_type_flags(raw string) ComptimeStaticFieldTypeFlags {
	mut core := raw.trim_space()
	mut flags := ComptimeStaticFieldTypeFlags{}
	if core.starts_with('?') {
		core = core[1..].trim_space()
	}
	if core.starts_with('shared ') {
		flags.is_shared = true
		flags.indirections++
		core = core[7..].trim_space()
	} else if core.starts_with('atomic ') {
		flags.is_atomic = true
		core = core[7..].trim_space()
	}
	for core.starts_with('&') {
		flags.indirections++
		core = core[1..].trim_space()
	}
	return flags
}

fn (mut tc TypeChecker) comptime_static_subst_field_cond(cond string, var_name string, field ComptimeStaticFieldCase) string {
	mut c := cond
	c = c.replace('${var_name}.unaliased_typ', field.unaliased_typ)
	c = c.replace('${var_name}.is_option', field.is_option.str())
	c = c.replace('${var_name}.is_opt', field.is_option.str())
	if field.has_decl_meta {
		c = c.replace('${var_name}.is_embed', field.is_embed.str())
		c = c.replace('${var_name}.is_mut', field.is_mut.str())
		c = c.replace('${var_name}.is_pub', field.is_pub.str())
	}
	c = c.replace('${var_name}.is_array', field.is_array.str())
	c = c.replace('${var_name}.is_map', field.is_map.str())
	c = c.replace('${var_name}.is_chan', field.is_chan.str())
	c = c.replace('${var_name}.is_struct', field.is_struct.str())
	c = c.replace('${var_name}.is_enum', field.is_enum.str())
	c = c.replace('${var_name}.is_alias', field.is_alias.str())
	c = c.replace('${var_name}.is_shared', field.is_shared.str())
	c = c.replace('${var_name}.is_atomic', field.is_atomic.str())
	c = c.replace('${var_name}.indirections', field.indirections.str())
	c = c.replace('${var_name}.typ', field.typ)
	c = c.replace('${var_name}.name', "'${field.name}'")
	c = comptime_static_replace_bare_ident(c, var_name, field.typ)
	return c
}

fn comptime_static_replace_bare_ident(cond string, ident string, replacement string) string {
	if ident.len == 0 {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + ident.len <= cond.len && cond[offset..offset + ident.len] == ident {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + ident.len
			after_ok := after >= cond.len
				|| (!comptime_cond_name_char(cond[after]) && cond[after] != `.`)
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset..offset + 1]
		offset++
	}
	return out
}

fn (mut tc TypeChecker) comptime_static_eval_field_cond(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond.trim_space())
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_static_eval_field_cond(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_static_eval_field_cond(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_static_eval_field_cond(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_static_eval_field_cond(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			matches := tc.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	for op in [' != ', ' == '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := comptime_static_unquote(clean[..op_idx].trim_space())
			right := comptime_static_unquote(clean[op_idx + op.len..].trim_space())
			eq := left == right
			return if op == ' == ' { eq } else { !eq }
		}
	}
	for op in [' !in', ' in'] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			after := op_idx + op.len
			if after < clean.len && clean[after] != ` ` && clean[after] != `[`
				&& clean[after] != `(` {
				continue
			}
			needle := comptime_static_unquote(clean[..op_idx].trim_space())
			found := comptime_static_list_contains(clean[after..].trim_space(), needle)
			return if op == ' in' { found } else { !found }
		}
	}
	for op in [' <= ', ' >= ', ' < ', ' > '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			if !comptime_static_is_int(left) || !comptime_static_is_int(right) {
				return none
			}
			l := left.int()
			r := right.int()
			return match op {
				' <= ' { l <= r }
				' >= ' { l >= r }
				' < ' { l < r }
				else { l > r }
			}
		}
	}
	if clean.starts_with('!') {
		value := tc.comptime_static_eval_field_cond(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn comptime_static_list_contains(list_text string, needle string) bool {
	clean := list_text.trim_space()
	if !clean.starts_with('[') || !clean.ends_with(']') {
		return false
	}
	inner := clean[1..clean.len - 1]
	for part in inner.split(',') {
		if comptime_static_unquote(part.trim_space()) == needle {
			return true
		}
	}
	return false
}

fn comptime_static_is_int(s string) bool {
	if s.len == 0 {
		return false
	}
	start := if s[0] == `-` || s[0] == `+` { 1 } else { 0 }
	if start >= s.len {
		return false
	}
	for i in start .. s.len {
		if !s[i].is_digit() {
			return false
		}
	}
	return true
}

fn comptime_static_unquote(s string) string {
	if s.len >= 2 && (s[0] == `'` || s[0] == `"`) && s[s.len - 1] == s[0] {
		return comptime_static_unescape(s[1..s.len - 1])
	}
	return s
}

fn comptime_static_string_literal(value string) string {
	mut out := strings.new_builder(value.len + 2)
	out.write_u8(`'`)
	for i := 0; i < value.len; i++ {
		if value[i] == `\\` || value[i] == `'` {
			out.write_u8(`\\`)
		}
		out.write_u8(value[i])
	}
	out.write_u8(`'`)
	return out.str()
}

fn comptime_static_unescape(value string) string {
	if !value.contains('\\') {
		return value
	}
	mut out := strings.new_builder(value.len)
	mut i := 0
	for i < value.len {
		if value[i] != `\\` || i + 1 >= value.len {
			out.write_u8(value[i])
			i++
			continue
		}
		next := value[i + 1]
		if next == `\n` {
			i += 2
			for i < value.len && value[i] in [` `, `\t`, `\r`] {
				i++
			}
			continue
		}
		if next == `\r` && i + 2 < value.len && value[i + 2] == `\n` {
			i += 3
			for i < value.len && value[i] in [` `, `\t`] {
				i++
			}
			continue
		}
		if next == `x` && i + 3 < value.len {
			if code := comptime_static_fixed_hex(value, i + 2, 2) {
				out.write_u8(u8(code))
				i += 4
				continue
			}
		}
		if next == `u` && i + 5 < value.len {
			if code := comptime_static_fixed_hex(value, i + 2, 4) {
				out.write_rune(rune(code))
				i += 6
				continue
			}
		}
		if next == `U` && i + 9 < value.len {
			if code := comptime_static_fixed_hex(value, i + 2, 8) {
				out.write_rune(rune(code))
				i += 10
				continue
			}
		}
		decoded := match next {
			`n` { int(`\n`) }
			`t` { int(`\t`) }
			`r` { int(`\r`) }
			`\\` { int(`\\`) }
			`'` { int(`'`) }
			`"` { int(`"`) }
			`$` { int(`$`) }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			else { -1 }
		}

		if decoded >= 0 {
			out.write_u8(u8(decoded))
		} else {
			out.write_u8(`\\`)
			out.write_u8(next)
		}
		i += 2
	}
	return out.str()
}

fn comptime_static_fixed_hex(value string, start int, count int) ?u32 {
	mut code := u32(0)
	for i in 0 .. count {
		if start + i >= value.len {
			return none
		}
		digit := comptime_static_hex_digit(value[start + i]) or { return none }
		code = (code << 4) | digit
	}
	return code
}

fn comptime_static_hex_digit(c u8) ?u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u32(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u32(c - `A` + 10)
	}
	return none
}

// check_node validates check node state for types.
fn (mut tc TypeChecker) check_node(id flat.NodeId) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.checking_nodes.len {
			if tc.checking_nodes[idx] {
				return
			}
			tc.checking_nodes[idx] = true
			defer {
				tc.checking_nodes[idx] = false
			}
		} else {
			if tc.sparse_checking_nodes[idx] {
				return
			}
			tc.sparse_checking_nodes[idx] = true
			defer {
				tc.sparse_checking_nodes.delete(idx)
			}
		}
	} else {
		if idx >= tc.checking_nodes.len {
			tc.extend_node_caches(tc.a.nodes.len)
		}
		if idx < tc.checking_nodes.len {
			if tc.checking_nodes[idx] {
				return
			}
			tc.checking_nodes[idx] = true
			defer {
				tc.checking_nodes[idx] = false
			}
		}
	}
	node := tc.a.nodes[idx]
	kind_id := node_kind_id(node)
	if kind_id == 1 || kind_id == 2 || kind_id == 3 || kind_id == 4 || kind_id == 5 || kind_id == 28
		|| kind_id == 29 {
		return
	}
	if kind_id == 45 {
		tc.check_block(id, node)
		return
	}
	if node.kind == .comptime_if {
		tc.check_comptime_if(id, node)
		return
	}
	if node.kind == .comptime_for {
		// The body references the loop variable (`field.name`, `field.typ`, ...) which only
		// exists once the transformer unrolls the loop against a concrete type, so it cannot
		// be type-checked here. Validate the known compile-time member surface, then skip it;
		// the unrolled statements are concrete.
		tc.check_comptime_for_members(id, node)
		return
	}
	if kind_id == 46 {
		tc.check_for_stmt(node)
		return
	}
	if kind_id == 47 {
		tc.check_for_in_stmt(node)
		return
	}
	if kind_id == 41 {
		tc.check_decl_assign(id, node)
		return
	}
	if kind_id == 40 || kind_id == 42 || kind_id == 43 {
		tc.check_assign(id, node)
		return
	}
	if kind_id == 44 {
		tc.check_return(id, node)
		return
	}
	if kind_id == 12 {
		tc.check_call(id, node)
		return
	}
	if kind_id == 21 {
		tc.check_fn_literal(node)
		return
	}
	if kind_id == 32 {
		tc.check_lambda_expr(node)
		return
	}
	if kind_id == 15 {
		tc.check_if_expr(id, node)
		return
	}
	if kind_id == 22 {
		tc.check_or_expr(node)
		return
	}
	if kind_id == 50 {
		tc.check_match_stmt(id, node)
		return
	}
	if kind_id == 37 {
		tc.check_is_expr(id, node)
		return
	}
	if kind_id == 10 {
		tc.check_postfix(id, node)
		return
	}
	if kind_id == 16 || kind_id == 26 {
		tc.check_struct_init(id, node)
		return
	}
	if kind_id == 13 {
		tc.check_selector(id, node)
		$if ownership ? {
			tc.ownership_check_expr(id)
		}
		return
	}
	if kind_id == 14 {
		tc.check_index(id, node)
		$if ownership ? {
			tc.ownership_check_expr(id)
		}
		return
	}
	if kind_id == 7 {
		tc.check_ident(id, node)
		return
	}
	if node.kind == .cast_expr {
		tc.check_cast_expr(id, node)
		return
	}
	if node.kind == .array_init {
		tc.check_array_init(node)
		$if ownership ? {
			if !tc.ownership_aggregate_consumption_deferred(id) {
				tc.ownership_consume_array_init_expr(node)
			}
		}
		return
	}
	if node.kind == .select_stmt {
		tc.check_select_stmt(node)
		return
	}
	if node.kind == .defer_stmt {
		$if ownership ? {
			tc.ownership_check_defer_stmt(id, node)
		} $else {
			tc.check_defer_stmt(node)
		}
		return
	}
	// A method value stored in a container escapes the single-use guarantee of its per-site
	// static receiver, so reject `[obj.method]` / `arr << obj.method` / `{'k': obj.method}`.
	if node.kind == .array_literal {
		for i in 0 .. node.children_count {
			tc.reject_stored_method_value(tc.a.child(&node, i))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, i))
		}
	} else if node.kind == .map_init {
		// children alternate key, value, key, value, ...; check the value positions.
		for j := 1; j < node.children_count; j += 2 {
			tc.reject_stored_method_value(tc.a.child(&node, j))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, j))
		}
	} else if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
		if unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0))) is Array {
			tc.reject_stored_method_value(tc.a.child(&node, 1))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, 1))
		}
	}
	if node.kind == .infix && node.op == .logical_and && node.children_count >= 2 {
		lhs_id := tc.a.child(&node, 0)
		rhs_id := tc.a.child(&node, 1)
		smartcasts := tc.extract_smartcasts(lhs_id)
		if smartcasts.len > 0 {
			tc.check_node(lhs_id)
			saved_smartcasts := clone_smartcasts(tc.smartcasts)
			for sc in smartcasts {
				if valid_string_data(sc.name) {
					tc.smartcasts[sc.name] = sc.typ
				}
			}
			tc.check_node(rhs_id)
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
			return
		}
	}

	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		$if ownership ? {
			defer_append_rhs := node.kind == .infix && node.op == .left_shift
				&& node.children_count >= 2 && i == 1
				&& unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0))) is Array
			tc.ownership_check_node_with_aggregate_consumption_mode(child_id, defer_append_rhs)
		} $else {
			tc.check_node(child_id)
		}
	}
	if node.kind == .infix {
		tc.check_infix(id, node)
	}
	$if ownership ? {
		if !tc.ownership_aggregate_consumption_deferred(id) {
			if node.kind == .array_literal {
				for i in 0 .. node.children_count {
					elem_id := tc.a.child(&node, i)
					tc.ownership_consume_expr(elem_id, 'array element', elem_id)
				}
			} else if node.kind == .map_init {
				for j := 0; j < node.children_count; j += 2 {
					key_id := tc.a.child(&node, j)
					tc.ownership_consume_expr(key_id, 'map key', key_id)
					if j + 1 < node.children_count {
						val_id := tc.a.child(&node, j + 1)
						tc.ownership_consume_expr(val_id, 'map value', val_id)
					}
				}
			}
		}
		if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
			array_id := tc.a.child(&node, 0)
			if unwrap_pointer(tc.resolve_type(array_id)) is Array {
				elem_id := tc.a.child(&node, 1)
				tc.ownership_mark_array_append_expr(array_id, elem_id, id)
			}
		} else if node.kind == .infix && node.op == .arrow && node.children_count >= 2 {
			value_id := tc.a.child(&node, 1)
			tc.ownership_consume_expr(value_id, 'channel send', id)
		}
	}
}

fn (mut tc TypeChecker) check_cast_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	tc.check_node(child_id)
	target := tc.parse_type(node.value)
	target_iface := cast_target_interface(target) or { return }
	if tc.expr_tail_is_nil(child_id) {
		return
	}
	actual := tc.resolve_type(child_id)
	if actual is Unknown || type_contains_unknown(actual) {
		return
	}
	if is_ierror_type(target)
		&& (actual is None || (actual is OptionType && actual.base_type is Void)) {
		return
	}
	if !tc.type_implements_interface(actual, target_iface) {
		tc.type_mismatch(.assignment_mismatch,
			'type `${actual.name()}` does not implement interface `${target_iface.name}`', id)
	}
}

fn cast_target_interface(target Type) ?Interface {
	mut current := target
	for _ in 0 .. 8 {
		if current is Interface {
			return current
		}
		if current is Alias {
			current = current.base_type
			continue
		}
		if current is Pointer {
			current = current.base_type
			continue
		}
		return none
	}
	return none
}

fn (mut tc TypeChecker) check_comptime_if(_id flat.NodeId, node flat.Node) {
	take_then := tc.comptime_type_condition_value(node.value) or { return }
	branch_index := if take_then { 0 } else { 1 }
	if branch_index >= node.children_count {
		return
	}
	tc.check_node(tc.a.child(&node, branch_index))
}

fn (tc &TypeChecker) mark_inactive_comptime_subtree(id flat.NodeId, mut inactive []bool) {
	if !tc.valid_node_id(id) {
		return
	}
	if inactive.len == 0 {
		inactive = []bool{len: tc.a.nodes.len}
	}
	inactive[int(id)] = true
	node := tc.a.nodes[int(id)]
	for i in 0 .. node.children_count {
		tc.mark_inactive_comptime_subtree(tc.a.child(&node, i), mut inactive)
	}
}

fn (tc &TypeChecker) mark_inactive_top_level_comptime(id flat.NodeId, mut inactive []bool) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_if {
		take_then := tc.comptime_threads_condition_value(node.value) or { return }
		active_branch := if take_then { 0 } else { 1 }
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			if i == active_branch {
				tc.mark_inactive_top_level_comptime(child_id, mut inactive)
			} else {
				tc.mark_inactive_comptime_subtree(child_id, mut inactive)
			}
		}
		return
	}
	if node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		tc.mark_inactive_top_level_comptime(tc.a.child(&node, i), mut inactive)
	}
}

fn (tc &TypeChecker) inactive_top_level_comptime_nodes() []bool {
	mut inactive := []bool{}
	for node in tc.a.nodes {
		if node.kind != .file || node.children_count == 0 {
			continue
		}
		for i in 0 .. node.children_count {
			tc.mark_inactive_top_level_comptime(tc.a.child(&node, i), mut inactive)
		}
	}
	return inactive
}

// prune_inactive_top_level_comptime removes declarations and expressions from inactive
// top-level compile-time branches after semantic checking and before later compiler stages.
pub fn (tc &TypeChecker) prune_inactive_top_level_comptime(mut a flat.FlatAst) {
	for i in tc.inactive_top_level_node_ids {
		if i < a.nodes.len {
			a.nodes[i] = flat.Node{}
		}
	}
}

fn (tc &TypeChecker) subtree_has_spawn_expr(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_if && comptime_condition_is_builtin_threads_guarded(node.value) {
		return false
	}
	if node_kind_id(node) == int(flat.NodeKind.spawn_expr) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.subtree_has_spawn_expr(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn comptime_condition_is_builtin_threads_guarded(cond string) bool {
	return comptime_condition_is_builtin_threads_guarded_with_negation(cond, false)
}

// A conjunction is guarded when either side is guarded; every alternative of a
// disjunction must be guarded. `negated` applies De Morgan's operator swap.
fn comptime_condition_is_builtin_threads_guarded_with_negation(cond string, negated bool) bool {
	clean := comptime_condition_strip_outer_parens(cond)
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := comptime_condition_is_builtin_threads_guarded_with_negation(clean[..or_idx],
			negated)
		right := comptime_condition_is_builtin_threads_guarded_with_negation(clean[or_idx + 2..],
			negated)
		return if negated { left || right } else { left && right }
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := comptime_condition_is_builtin_threads_guarded_with_negation(clean[..and_idx],
			negated)
		right := comptime_condition_is_builtin_threads_guarded_with_negation(clean[and_idx + 2..],
			negated)
		return if negated { left && right } else { left || right }
	}
	if clean.starts_with('!') {
		return comptime_condition_is_builtin_threads_guarded_with_negation(clean[1..], !negated)
	}
	return clean == 'threads'
}

fn module_file_matches_import_path(file string, imported_module string) bool {
	if !imported_module.contains('.') {
		return false
	}
	normalized_file := file.replace('\\', '/')
	module_dir := imported_module.replace('.', '/')
	file_dir := normalized_file.all_before_last('/')
	return file_dir == module_dir || file_dir.ends_with('/${module_dir}')
}

fn (tc &TypeChecker) scan_has_spawn_expr() bool {
	if tc.diagnostic_files.len == 0 {
		start := if tc.a.user_code_start > 0 { tc.a.user_code_start } else { 0 }
		mut ignored := []bool{}
		for node in tc.a.nodes[start..] {
			if node.kind == .comptime_if
				&& comptime_condition_is_builtin_threads_guarded(node.value) {
				for i in 0 .. node.children_count {
					tc.mark_inactive_comptime_subtree(tc.a.child(&node, i), mut ignored)
				}
			}
		}
		for idx, node in tc.a.nodes[start..] {
			absolute_idx := start + idx
			if (ignored.len == 0 || !ignored[absolute_idx])
				&& node_kind_id(node) == int(flat.NodeKind.spawn_expr) {
				return true
			}
		}
		return false
	}
	mut file_nodes := map[string]flat.NodeId{}
	mut file_imports := map[string][]string{}
	mut module_files := map[string][]string{}
	for idx, node in tc.a.nodes {
		if node.kind != .file || node.children_count == 0 || node.value.len == 0 {
			continue
		}
		file_nodes[node.value] = flat.NodeId(idx)
		mut imports := []string{}
		mut module_name := ''
		for i in 0 .. node.children_count {
			child := tc.a.child_node(&node, i)
			if child.kind == .module_decl {
				module_name = child.value
			} else if child.kind == .import_decl && child.value.len > 0 {
				imports << child.value
			}
		}
		file_imports[node.value] = imports
		if module_name.len > 0 {
			mut files := module_files[module_name] or { []string{} }
			files << node.value
			module_files[module_name] = files
		}
	}
	mut reachable_files := []string{}
	mut seen_files := map[string]bool{}
	for file, selected in tc.diagnostic_files {
		if selected && !file.starts_with('generic:') {
			reachable_files << file
			seen_files[file] = true
		}
	}
	mut pos := 0
	for pos < reachable_files.len {
		file := reachable_files[pos]
		pos++
		if file_id := file_nodes[file] {
			if tc.subtree_has_spawn_expr(file_id) {
				return true
			}
		}
		for imported_module in file_imports[file] or { []string{} } {
			mut imported_files := module_files[imported_module] or { []string{} }
			if imported_files.len == 0 && imported_module.contains('.') {
				short_name := imported_module.all_after_last('.')
				for candidate in module_files[short_name] or { []string{} } {
					if module_file_matches_import_path(candidate, imported_module) {
						imported_files << candidate
					}
				}
			}
			for imported_file in imported_files {
				if !seen_files[imported_file] {
					seen_files[imported_file] = true
					reachable_files << imported_file
				}
			}
		}
	}
	return false
}

// prepare_threads_condition caches whether selected inputs or their reachable imports use spawn.
pub fn (mut tc TypeChecker) prepare_threads_condition() {
	if tc.has_spawn_expr < 0 {
		tc.has_spawn_expr = if tc.scan_has_spawn_expr() { 1 } else { 0 }
	}
}

// threads_condition_value reports the cached `$if threads` condition, scanning lazily for
// direct TypeChecker users that do not run the regular compiler setup.
pub fn (tc &TypeChecker) threads_condition_value() bool {
	if tc.has_spawn_expr >= 0 {
		return tc.has_spawn_expr == 1
	}
	return tc.scan_has_spawn_expr()
}

fn (tc &TypeChecker) comptime_threads_condition_value(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond)
	if clean == 'threads' {
		return tc.threads_condition_value()
	}
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_threads_condition_value(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_threads_condition_value(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_threads_condition_value(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_threads_condition_value(clean[and_idx + 2..])
	}
	if clean.starts_with('!') {
		value := tc.comptime_threads_condition_value(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn (mut tc TypeChecker) comptime_type_condition_value(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond)
	if clean == 'threads' {
		return tc.threads_condition_value()
	}
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_type_condition_value(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_type_condition_value(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_type_condition_value(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_type_condition_value(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := clean[..op_idx].trim_space()
			right := clean[op_idx + op.len..].trim_space()
			matches := tc.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	if clean.starts_with('!') {
		value := tc.comptime_type_condition_value(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn (mut tc TypeChecker) comptime_type_matches(actual string, expected string) ?bool {
	clean_actual := actual.trim_space()
	clean_expected := expected.trim_space()
	if clean_actual.len == 0 || clean_expected.len == 0
		|| (is_bare_generic_param(clean_actual) && !tc.type_name_known(clean_actual)) {
		return none
	}
	actual_type := tc.comptime_type_match_type(clean_actual)
	normalized := actual_type.name()
	match clean_expected {
		'$array' {
			return actual_type is Array || actual_type is ArrayFixed
		}
		'$array_dynamic' {
			return actual_type is Array
		}
		'$array_fixed' {
			return actual_type is ArrayFixed
		}
		'$map' {
			return actual_type is Map
		}
		'$function' {
			return actual_type is FnType
		}
		'$option' {
			return actual_type is OptionType
		}
		'$shared' {
			return tc.comptime_type_text_is_shared(clean_actual)
		}
		'$pointer' {
			return actual_type is Pointer
		}
		'$voidptr' {
			return fn_param_is_voidptr_type(actual_type)
		}
		'$int' {
			return actual_type.is_integer()
		}
		'$float' {
			return actual_type.is_float()
		}
		'$string' {
			return actual_type is String
		}
		'$struct' {
			return actual_type is Struct && normalized in tc.structs
		}
		'$enum' {
			return normalized in tc.enum_names
		}
		'$alias' {
			return clean_actual in tc.type_aliases
				|| tc.qualify_name(clean_actual) in tc.type_aliases
		}
		'$sumtype' {
			return normalized in tc.sum_types
		}
		'$interface' {
			return normalized in tc.interface_names
		}
		else {}
	}

	expected_type := tc.comptime_type_match_type(clean_expected)
	if expected_type is Interface {
		return tc.type_implements_interface(actual_type, expected_type)
	}
	if expected_type.name() in tc.interface_names {
		return tc.type_implements_interface(actual_type, Interface{
			name: expected_type.name()
		})
	}
	return normalized == expected_type.name()
}

fn (tc &TypeChecker) comptime_type_text_is_shared(type_text string) bool {
	mut cur := type_text.trim_space()
	for _ in 0 .. 16 {
		if cur.starts_with('shared ') {
			return true
		}
		target := tc.alias_target_type_text(cur) or { return false }
		if target == cur {
			return false
		}
		cur = target.trim_space()
	}
	return false
}

fn (mut tc TypeChecker) comptime_type_match_type(type_text string) Type {
	typ := tc.parse_type(type_text)
	if typ is Alias {
		return typ.base_type
	}
	return typ
}

// type_text_implements_interface reports whether a concrete type expression
// satisfies an interface type expression in the current checker module context.
pub fn (mut tc TypeChecker) type_text_implements_interface(actual_text string, iface_text string) bool {
	actual := tc.parse_type(actual_text)
	expected := tc.comptime_type_match_type(iface_text)
	if expected is Interface {
		return tc.type_implements_interface(actual, expected)
	}
	expected_name := expected.name()
	if expected_name in tc.interface_names {
		return tc.type_implements_interface(actual, Interface{
			name: expected_name
		})
	}
	return false
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

		if paren_depth == 0 && bracket_depth == 0 && s[i..].starts_with(needle) {
			return i
		}
	}
	return -1
}

// check_infix validates type-sensitive infix operations that would otherwise reach CGen
// as raw helper calls with incompatible arguments.
fn (mut tc TypeChecker) check_infix(id flat.NodeId, node flat.Node) {
	if node.op != .plus || node.children_count < 2 || !tc.should_diagnose(id) {
		return
	}
	lhs_id := tc.a.child(&node, 0)
	rhs_id := tc.a.child(&node, 1)
	lhs_type := tc.infix_read_type(lhs_id)
	rhs_type := tc.infix_read_type(rhs_id)
	lhs_is_string := type_is_string_like(lhs_type)
	rhs_is_string := type_is_string_like(rhs_type)
	if lhs_is_string == rhs_is_string {
		return
	}
	if lhs_type is Unknown || rhs_type is Unknown {
		return
	}
	tc.record_error(.assignment_mismatch,
		'operator `+` cannot concatenate `${lhs_type.name()}` and `${rhs_type.name()}`', id)
}

fn (tc &TypeChecker) infix_read_type(id flat.NodeId) Type {
	typ := tc.resolve_type(id)
	if int(id) < 0 {
		return typ
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if base := tc.mut_param_base_for_current_ident(node.value, typ) {
			return base
		}
	}
	return typ
}

fn (tc &TypeChecker) mut_param_base_for_current_ident(name string, typ Type) ?Type {
	base := tc.cur_fn_mut_param_base_types[name] or { return none }
	if !tc.lvalue_matches_mut_param(typ, base) {
		return none
	}
	if !tc.mut_param_binding_matches_lvalue(name) {
		return none
	}
	return base
}

fn type_is_string_like(typ Type) bool {
	if typ is String {
		return true
	}
	if typ is Alias {
		return type_is_string_like(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) select_branch_is_timeout(branch flat.Node) bool {
	if branch.kind != .select_branch || branch.value == 'else' || branch.children_count == 0
		|| branch.value in ['recv', 'recv_assign'] || branch.value.starts_with('recv_compound:') {
		return false
	}
	first := tc.a.child_node(&branch, 0)
	return !((first.kind == .infix && first.op == .arrow)
		|| (first.kind == .prefix && first.op == .arrow))
}

// check_select_stmt validates a `select { ... }` statement. A receive declaration
// binds the channel element type in the branch scope, while a receive assignment
// is checked against its existing lvalue type before the branch body.
fn (mut tc TypeChecker) check_select_stmt(node flat.Node) {
	mut has_else := false
	mut has_timeout := false
	mut conflict_id := flat.empty_node
	mut duplicate_timeout_id := flat.empty_node
	for i in 0 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .select_branch {
			continue
		}
		if branch.value == 'else' {
			if has_timeout && int(conflict_id) < 0 {
				conflict_id = branch_id
			}
			has_else = true
		} else if tc.select_branch_is_timeout(branch) {
			if has_else && int(conflict_id) < 0 {
				conflict_id = branch_id
			}
			if has_timeout && int(duplicate_timeout_id) < 0 {
				duplicate_timeout_id = branch_id
			}
			has_timeout = true
		}
	}
	if tc.valid_node_id(conflict_id) {
		tc.record_error(.condition_mismatch,
			'`else` and timeout value are mutually exclusive `select` keys', conflict_id)
	}
	if tc.valid_node_id(duplicate_timeout_id) {
		tc.record_error(.condition_mismatch,
			'at most one timeout branch allowed in `select` block', duplicate_timeout_id)
	}
	$if ownership ? {
		tc.ownership_begin_branch_group()
	}
	base_smartcasts := clone_smartcasts(tc.smartcasts)
	mut invalidated_smartcasts := map[string]bool{}
	for i in 0 .. node.children_count {
		if base_smartcasts.len > 0 {
			tc.smartcasts = clone_smartcasts(base_smartcasts)
		}
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .select_branch {
			tc.check_node(branch_id)
			for key, _ in base_smartcasts {
				if key !in tc.smartcasts {
					invalidated_smartcasts[key] = true
				}
			}
			continue
		}
		$if ownership ? {
			tc.ownership_begin_branch()
			if branch.value == 'else' {
				tc.ownership_note_branch_group_else()
			}
		}
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(branch_id)
		}
		mut body_start := 0
		is_assignment_case := branch.value in ['recv', 'recv_assign']
			|| branch.value.starts_with('recv_compound:')
		mut has_receive_rhs := false
		if is_assignment_case && branch.children_count >= 2 {
			second := tc.a.child_node(&branch, 1)
			has_receive_rhs = second.kind == .prefix && second.op == .arrow
		}
		if is_assignment_case && !has_receive_rhs {
			tc.record_error(.assignment_mismatch,
				'select assignment case requires a channel receive on the right side', branch_id)
			body_start = if branch.children_count >= 2 { 2 } else { int(branch.children_count) }
		}
		if branch.value.starts_with('recv_compound:') && has_receive_rhs {
			recv_id := tc.a.child(&branch, 1)
			tc.check_node(recv_id)
			op := branch.value.all_after('recv_compound:')
			tc.record_error(.assignment_mismatch,
				'compound receive assignment `${op}` is not supported in `select`; use `=` or `:=`',
				branch_id)
			body_start = 2
		}
		receive_assignment := branch.value in ['recv', 'recv_assign'] && has_receive_rhs
		if receive_assignment {
			// children[0] = bound/assigned lvalue, children[1] = `<-ch` receive expr.
			var_id := tc.a.child(&branch, 0)
			recv_id := tc.a.child(&branch, 1)
			if branch.value == 'recv' {
				tc.check_node(recv_id)
				elem_type := tc.resolve_type(recv_id)
				if tc.valid_node_id(var_id) {
					var_node := tc.a.nodes[int(var_id)]
					if var_node.kind == .ident && var_node.value.len > 0 {
						tc.cur_scope.insert(var_node.value, elem_type)
						tc.remember_expr_type(var_id, elem_type)
						$if ownership ? {
							tc.ownership_note_binding(var_node.value, elem_type, var_id)
						}
					} else {
						tc.record_error(.assignment_mismatch,
							'select receive declaration requires a plain identifier on the left side',
							var_id)
					}
				}
			} else {
				lhs_type := tc.resolve_lvalue_type(var_id)
				tc.remember_expr_type(var_id, lhs_type)
				expected_type := tc.assignment_expected_type(var_id, lhs_type)
				tc.annotate_expected_expr(recv_id, expected_type)
				tc.check_node(recv_id)
				rhs_type := tc.resolve_expr(recv_id, expected_type)
				if !tc.assignment_types_compatible(recv_id, rhs_type, expected_type, .assign) {
					tc.type_mismatch(.assignment_mismatch,
						'cannot assign `${rhs_type.name()}` to `${expected_type.name()}`',
						branch_id)
				}
				lhs_key := tc.expr_key(var_id)
				if lhs_key.len > 0 {
					tc.smartcasts.delete(lhs_key)
				}
			}
			body_start = 2
		}
		tc.check_statement_sequence(branch, body_start, false)
		tc.pop_scope()
		$if ownership ? {
			tc.ownership_end_branch(branch_id)
		}
		for key, _ in base_smartcasts {
			if key !in tc.smartcasts {
				invalidated_smartcasts[key] = true
			}
		}
	}
	tc.smartcasts = clone_smartcasts(base_smartcasts)
	for key, _ in invalidated_smartcasts {
		tc.smartcasts.delete(key)
	}
	$if ownership ? {
		tc.ownership_end_branch_group()
	}
}

// check_array_init validates an `[]T{len: ..., init: ...}` initializer. The `init:`
// expression may reference the magic `index` variable (the current element index),
// so it is checked in a scope where `index` is bound to an int.
fn (mut tc TypeChecker) check_array_init(node flat.Node) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .field_init && child.value == 'init' {
			if child.children_count > 0 {
				tc.reject_stored_method_value(tc.a.child(&child, 0))
				tc.reject_stored_capturing_fn_literal(tc.a.child(&child, 0))
			}
			tc.push_scope()
			tc.cur_scope.insert('index', Type(int_))
			tc.check_node(child_id)
			tc.pop_scope()
		} else {
			tc.check_node(child_id)
		}
	}
}

// check_or_expr validates check or expr state for types.
fn (mut tc TypeChecker) check_or_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	inner_id := tc.a.child(&node, 0)
	tc.check_node(inner_id)
	$if ownership ? {
		if node.value in ['!', '?'] {
			tc.ownership_record_propagation_drops()
		}
	}
	if node.children_count < 2 || node.value in ['!', '?'] {
		return
	}
	$if ownership ? {
		tc.ownership_begin_value_branch_group()
		tc.ownership_begin_branch()
	}
	fallback_id := tc.a.child(&node, 1)
	tc.push_scope()
	tc.cur_scope.insert('err', tc.parse_type('IError'))
	tc.check_or_fallback_branch_node(fallback_id)
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_branch(fallback_id)
		tc.ownership_add_branch_group_base()
		tc.ownership_end_branch_group()
	}
}

fn (mut tc TypeChecker) check_or_fallback_branch_node(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(id)
		}
		tc.check_statement_sequence(node, 0, true)
		tc.ownership_record_or_fallback_error_return_drops(id)
		tc.pop_scope()
		return
	}
	tc.check_node(id)
	tc.ownership_record_or_fallback_error_return_drops(id)
}

fn (mut tc TypeChecker) ownership_record_or_fallback_error_return_drops(id flat.NodeId) {
	$if ownership ? {
		if tc.cur_fn_ret_type !is OptionType && tc.cur_fn_ret_type !is ResultType {
			return
		}
		tail_id := tc.branch_tail_expr_id(id)
		if !tc.branch_tail_is_error_literal(tail_id) {
			return
		}
		tc.ownership_record_propagation_drops()
	}
}

fn (mut tc TypeChecker) check_defer_stmt(node flat.Node) {
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
}

// check_fn_literal validates check fn literal state for types.
fn (mut tc TypeChecker) check_fn_literal(node flat.Node) {
	saved_ret := tc.cur_fn_ret_type
	mut saved_mut_params := tc.cur_fn_mut_param_base_types.move()
	mut saved_mut_param_owners := tc.cur_fn_mut_param_binding_owners.move()
	mut saved_mut_local_owners := tc.cur_fn_mut_local_binding_owners.move()
	mut saved_shared_owners := tc.cur_fn_shared_binding_owners.move()
	tc.cur_fn_mut_param_base_types = map[string]Type{}
	tc.cur_fn_mut_param_binding_owners = map[string]ScopeBindingOwner{}
	tc.cur_fn_mut_local_binding_owners = map[string]ScopeBindingOwner{}
	tc.cur_fn_shared_binding_owners = map[string]ScopeBindingOwner{}
	tc.cur_fn_ret_type = tc.parse_type(node.typ)
	$if ownership ? {
		tc.ownership_begin_fn_literal(node)
	}
	tc.push_scope()
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		tc.insert_fn_param_binding(child)
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param || child.kind == .ident {
			continue
		}
		tc.check_stmt_node(child_id)
	}
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
	tc.cur_fn_ret_type = saved_ret
	tc.cur_fn_mut_param_base_types = saved_mut_params.move()
	tc.cur_fn_mut_param_binding_owners = saved_mut_param_owners.move()
	tc.cur_fn_mut_local_binding_owners = saved_mut_local_owners.move()
	tc.cur_fn_shared_binding_owners = saved_shared_owners.move()
}

// check_lambda_expr validates check lambda expr state for types.
fn (mut tc TypeChecker) check_lambda_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	$if ownership ? {
		tc.ownership_begin_lambda_expr(node)
	}
	tc.push_scope()
	for i in 0 .. node.children_count - 1 {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			tc.cur_scope.insert(child.value, unknown_type('lambda parameter `${child.value}`'))
		}
	}
	tc.check_node(tc.a.child(&node, node.children_count - 1))
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
}

// check_block validates check block state for types.
fn (mut tc TypeChecker) check_block(id flat.NodeId, node flat.Node) {
	tc.push_scope()
	$if ownership ? {
		tc.ownership_mark_scope_node(id)
	}
	tc.check_statement_sequence(node, 0, false)
	tc.pop_scope()
}

// check_for_stmt validates check for stmt state for types.
fn (mut tc TypeChecker) check_for_stmt(node flat.Node) {
	tc.push_scope()
	$if ownership ? {
		if node.children_count > 0 {
			init_id := tc.a.child(&node, 0)
			if int(init_id) >= 0 && tc.a.nodes[int(init_id)].kind != .empty {
				tc.ownership_mark_scope_node(init_id)
			}
		}
	}
	if node.children_count > 0 {
		init_id := tc.a.child(&node, 0)
		if int(init_id) >= 0 {
			tc.check_node(init_id)
		}
	}
	if node.children_count > 1 {
		cond_id := tc.a.child(&node, 1)
		if int(cond_id) >= 0 {
			tc.check_bool_condition(cond_id)
		}
	}
	$if ownership ? {
		if node.children_count > 2 {
			post_id := tc.a.child(&node, 2)
			if int(post_id) >= 0 {
				tc.ownership_begin_suppressed_checks()
				tc.check_node(post_id)
				tc.ownership_end_suppressed_checks()
			}
		}
		tc.ownership_begin_loop_branch_group()
	} $else {
		if node.children_count > 2 {
			post_id := tc.a.child(&node, 2)
			if int(post_id) >= 0 {
				tc.check_node(post_id)
			}
		}
	}
	for i in 3 .. node.children_count {
		tc.check_stmt_node(tc.a.child(&node, i))
	}
	$if ownership ? {
		loop_may_skip_body := node.children_count > 1 && tc.a.child_node(&node, 1).kind != .empty
		body_reaches_post := tc.ownership_statement_sequence_can_reach_loop_post(node, 3)
		if node.children_count > 2 {
			post_id := tc.a.child(&node, 2)
			if int(post_id) >= 0 {
				post_frame := tc.ownership_snapshot_frame()
				tc.check_node(post_id)
				if !body_reaches_post {
					tc.ownership_restore_frame(post_frame)
				}
			}
			tc.ownership_apply_loop_continue_snapshots(post_id)
		} else {
			tc.ownership_merge_loop_continue_snapshots()
		}
		tc.ownership_record_current_loop_iteration_drops()
		if body_reaches_post {
			tc.ownership_end_loop_branch(node, 3)
		}
		if loop_may_skip_body {
			tc.ownership_add_branch_group_base()
		}
		tc.ownership_end_branch_group()
	}
	tc.pop_scope()
}

// check_for_in_stmt validates check for in stmt state for types.
fn (mut tc TypeChecker) check_for_in_stmt(node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		return
	}
	tc.push_scope()
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	tc.check_node(container_id)
	has_val := int(val_id) >= 0
	if header == 4 {
		tc.insert_loop_var(key_id, tc.range_loop_var_type(container_id))
		tc.check_node(tc.a.child(&node, 3))
	} else {
		clean0 := unwrap_pointer(tc.resolve_type(container_id))
		clean := if clean0 is Alias { clean0.base_type } else { clean0 }
		if clean is Array {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is ArrayFixed {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, clean.elem_type)
			} else {
				tc.insert_loop_var(key_id, clean.elem_type)
			}
		} else if clean is Map {
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				tc.insert_loop_var(val_id, clean.value_type)
			} else {
				tc.insert_loop_var(key_id, clean.value_type)
			}
		} else if clean is String {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, Type(u8_))
			} else {
				tc.insert_loop_var(key_id, Type(u8_))
			}
		} else if elem_type := tc.iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, elem_type)
			} else {
				tc.insert_loop_var(key_id, elem_type)
			}
		} else {
			container := tc.a.nodes[int(container_id)]
			if container.kind == .range {
				tc.insert_loop_var(key_id, tc.range_loop_var_type(tc.a.child(&container, 0)))
			} else if tc.should_diagnose(container_id) {
				tc.record_error(.cannot_index, 'cannot iterate over `${clean.name()}`',
					container_id)
			}
		}
	}
	$if ownership ? {
		tc.ownership_begin_loop_branch_group()
		tc.ownership_bind_for_in_vars(key_id, val_id, container_id, has_val)
	}
	for i in header .. node.children_count {
		tc.check_stmt_node(tc.a.child(&node, i))
	}
	$if ownership ? {
		tc.ownership_record_current_loop_iteration_drops()
		tc.ownership_merge_loop_continue_snapshots()
		tc.ownership_end_loop_branch(node, header)
		tc.ownership_add_branch_group_base()
		tc.ownership_end_branch_group()
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_storage_path_base_node(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	$if ownership ? {
		node := tc.a.nodes[int(id)]
		if node.kind in [.ident, .selector, .index] {
			tc.ownership_begin_suppressed_checks()
			tc.check_node(id)
			tc.ownership_end_suppressed_checks()
			return
		}
	}
	tc.check_node(id)
}

// check_decl_assign validates check decl assign state for types.
fn (mut tc TypeChecker) check_decl_assign(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.check_multi_return_decl_assign(id, node) {
		return
	}
	if tc.check_assignment_marker(id, node) {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		explicit_expected := if node.children_count == 2 && node.typ.len > 0 {
			tc.parse_type(node.typ)
		} else {
			Type(void_)
		}
		saved_expected_expr_id := tc.expected_expr_id
		saved_expected_expr_type := tc.expected_expr_type
		if explicit_expected !is Void {
			tc.expected_expr_id = int(rhs_id)
			tc.expected_expr_type = explicit_expected
		}
		$if ownership ? {
			if tc.ownership_should_defer_aggregate_consumption(lhs_id, .assign) {
				tc.ownership_begin_defer_aggregate_consumption(rhs_id)
				tc.check_node(rhs_id)
				tc.ownership_end_defer_aggregate_consumption(rhs_id)
			} else {
				tc.check_node(rhs_id)
			}
		} $else {
			tc.check_node(rhs_id)
		}
		tc.expected_expr_id = saved_expected_expr_id
		tc.expected_expr_type = saved_expected_expr_type
		mut rhs_type := tc.decl_assign_inferred_type(rhs_id)
		mut expected := rhs_type
		if explicit_expected !is Void {
			expected = explicit_expected
			rhs_type = tc.resolve_expr(rhs_id, expected)
			if !tc.expr_compatible(rhs_id, rhs_type, expected)
				&& !tc.pointer_value_compatible(rhs_type, expected) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${rhs_type.name()}` to `${expected.name()}`', id)
			}
		}
		owner := tc.insert_decl_lhs(lhs_id, expected, tc.decl_lhs_is_mut(node, lhs_id))
		if owner.storage_key().len > 0 && decl_assign_is_shared_marker(node.value) {
			lhs := tc.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value.len > 0 {
				tc.cur_fn_shared_binding_owners[lhs.value] = owner
			}
		}
		$if ownership ? {
			tc.ownership_after_decl_assign(lhs_id, rhs_id, expected, id)
		}
		tc.track_method_value_local(lhs_id, rhs_id)
		tc.reject_stored_capturing_fn_literal(rhs_id)
		i += 2
	}
}

// cur_scope_depth returns the number of enclosing scopes (the current scope's parent-chain
// length), used to tell a dominating top-level reassignment from one nested in a branch/loop.
fn (tc &TypeChecker) cur_scope_depth() int {
	mut d := 0
	mut s := tc.cur_scope
	for s != unsafe { nil } {
		d++
		s = s.parent
	}
	return d
}

// track_method_value_local records (or clears) a local variable bound to a method value, so a
// later `return cb` / `arr << cb` aliasing the same per-site static receiver is rejected as an
// escape just like the bare `return c.report`.
fn (mut tc TypeChecker) track_method_value_local(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	if int(lhs_id) < 0 {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	if tc.expr_is_method_value(rhs_id) {
		tc.method_value_locals[lhs.value] = true
		tc.method_value_local_depth[lhs.value] = tc.cur_scope_depth()
	} else if lhs.value in tc.method_value_locals {
		// Reassigned to a non-method-value. Only clear the marker when this reassignment
		// dominates later uses — at the same or a shallower scope than where the local was
		// marked. A reassignment in a deeper conditional/loop scope does not run on every path
		// (`mut cb := c.report; if x { cb = plain }; return cb`), so the local may still hold the
		// method value; keep the maybe-method marker and let the later escape be rejected.
		marked_depth := tc.method_value_local_depth[lhs.value] or { 0 }
		if tc.cur_scope_depth() <= marked_depth {
			tc.method_value_locals.delete(lhs.value)
			tc.method_value_local_depth.delete(lhs.value)
		}
	}
}

fn (mut tc TypeChecker) decl_assign_inferred_type(rhs_id flat.NodeId) Type {
	if int(rhs_id) < 0 || int(rhs_id) >= tc.a.nodes.len {
		return unknown_type('missing declaration initializer')
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind == .cast_expr && rhs.value.len > 0 {
		typ := tc.parse_type(rhs.value)
		if typ is Alias {
			return typ
		}
	}
	if typ := tc.infer_fn_value_decl_type(rhs_id) {
		return typ
	}
	if rhs.kind == .if_expr {
		return tc.if_expr_tail_type(rhs_id)
	}
	return tc.resolve_type(rhs_id)
}

fn (mut tc TypeChecker) infer_fn_value_decl_type(rhs_id flat.NodeId) ?Type {
	if int(rhs_id) < 0 || int(rhs_id) >= tc.a.nodes.len {
		return none
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if tc.fn_value_shadowed_by_value(rhs) {
		return none
	}
	key := tc.fn_value_key(rhs) or { return none }
	typ := tc.fn_type_from_key(key) or { return none }
	tc.remember_resolved_fn_value_chain(rhs_id, key)
	tc.register_synth_type(rhs_id, typ)
	return typ
}

fn (tc &TypeChecker) fn_value_shadowed_by_value(node flat.Node) bool {
	match node.kind {
		.ident {
			return tc.name_bound_as_value(node.value)
		}
		.selector {
			return tc.selector_base_bound_as_value(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.fn_value_shadowed_by_value(tc.a.child_node(&node, 0))
		}
		else {
			return false
		}
	}
}

// lvalue_is_local_var reports whether an assignment target is safe to receive a method value:
// the blank discard `_` (stores nothing) or a plain function-local variable bound under its bare
// name in the current scope. Non-local storage (a struct field `h.cb`, an array/map element
// `cbs[i]`, or a module-level global, which lives in file_scope under its qualified name and so
// misses a bare lookup) is not. A method value may alias a local (tracked for a later escape) but
// must not be stored into anything that outlives the call site.
fn (tc &TypeChecker) lvalue_is_local_var(lhs_id flat.NodeId) bool {
	if int(lhs_id) < 0 {
		return false
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return false
	}
	if lhs.value == '_' {
		return true
	}
	return tc.cur_scope.lookup(lhs.value) != none
}

fn (tc &TypeChecker) selector_base_bound_as_value(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	base := tc.a.child_node(&node, 0)
	match base.kind {
		.ident {
			return tc.name_bound_as_value(base.value)
		}
		.selector {
			return tc.selector_base_bound_as_value(base)
		}
		.cast_expr, .paren, .expr_stmt {
			if base.children_count == 0 {
				return false
			}
			return tc.fn_value_shadowed_by_value(tc.a.child_node(base, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) name_bound_as_value(name string) bool {
	if name.len == 0 {
		return false
	}
	if typ := tc.cur_scope.lookup(name) {
		return typ !is Void
	}
	if typ := tc.file_scope.lookup(name) {
		return typ !is Void
	}
	return false
}

// check_multi_return_decl_assign validates check multi return decl assign state for types.
fn (mut tc TypeChecker) check_multi_return_decl_assign(id flat.NodeId, node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs := tc.a.nodes[int(rhs_id)]
	lhs_ids := tc.multi_assign_lhs_ids(node)
	if tc.multi_assign_rhs_count(node) != 1 {
		return false
	}
	if rhs.kind == .match_stmt {
		tc.check_node(rhs_id)
		if !tc.match_has_else_or_exhaustive_coverage(rhs) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'match expression must be exhaustive for multi-return assignment', id)
			}
			return true
		}
		if rhs_types := tc.match_multi_return_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		// Tuple tails (`.a { c, '.zst', 'zstd' }`) resolve like if-expr branches.
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		if tc.should_diagnose(id) {
			if tc.match_has_tuple_tail_values(rhs_id, lhs_ids.len) {
				tc.record_error(.assignment_mismatch,
					'match expression branches cannot produce multiple assignment values', id)
			} else {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
		}
		return true
	}
	if rhs.kind == .if_expr {
		tc.check_node(rhs_id)
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		rhs_type := tc.resolve_type(rhs_id)
		if rhs_type !is MultiReturn || tc.expr_has_tuple_tail_values(rhs_id, lhs_ids.len) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
			return true
		}
	}
	if rhs.kind == .lock_expr {
		tc.check_node(rhs_id)
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
	}
	mut rhs_type := tc.resolve_type(rhs_id)
	mut rhs_checked := false
	mut rhs_multi := MultiReturn{}
	mut found_multi := false
	mut unhandled_multi := false
	for _ in 0 .. 2 {
		if multi := tc.multi_return_assignment_type(rhs_id, rhs_type) {
			rhs_multi = multi
			found_multi = true
			break
		}
		if _ := tc.unhandled_wrapped_multi_return_type(rhs_id, rhs_type) {
			unhandled_multi = true
			break
		}
		if rhs_checked {
			break
		}
		tc.check_node(rhs_id)
		rhs_checked = true
		rhs_type = tc.resolve_type(rhs_id)
	}
	rhs_type_name := rhs_type.name()
	if unhandled_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'multi-return assignment from `${rhs_type_name}` requires `or {}`, `!`, or `?` handling',
				id)
		}
		return true
	}
	if found_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if lhs_ids.len != rhs_multi.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: ${lhs_ids.len} variables but `${rhs_type_name}` has ${rhs_multi.types.len} values',
					id)
			}
			return true
		}
		for i, lhs_id in lhs_ids {
			tc.insert_decl_lhs(lhs_id, rhs_multi.types[i], tc.decl_lhs_is_mut(node, lhs_id))
		}
		$if ownership ? {
			tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, rhs_multi, id)
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) multi_expr_tail_types(expr_id flat.NodeId, count int) ?[]Type {
	groups := tc.multi_expr_tail_value_groups(expr_id, count, false) or { return none }
	if groups.len == 0 {
		return none
	}
	mut types := []Type{cap: count}
	for value_id in groups[0] {
		typ := tc.expr_type(value_id) or { tc.resolve_type(value_id) }
		if !type_has_runtime_value(typ) {
			return none
		}
		types << typ
	}
	for i in 1 .. groups.len {
		group := groups[i]
		if group.len != types.len {
			return none
		}
		for j, value_id in group {
			actual := tc.expr_type(value_id) or { tc.resolve_type(value_id) }
			if !type_has_runtime_value(actual) {
				return none
			}
			promoted := tc.promoted_multi_tail_type(types[j], actual) or { return none }
			types[j] = promoted
		}
	}
	return types
}

fn (mut tc TypeChecker) multi_expr_tail_assign_types(id flat.NodeId, expr_id flat.NodeId, lhs_ids []flat.NodeId) ?[]Type {
	groups := tc.multi_expr_tail_value_groups(expr_id, lhs_ids.len, false) or { return none }
	if groups.len == 0 {
		return none
	}
	mut lhs_types := []Type{cap: lhs_ids.len}
	for lhs_id in lhs_ids {
		lhs_types << tc.resolve_lvalue_type(lhs_id)
	}
	for group in groups {
		if group.len != lhs_types.len {
			return none
		}
		for i, value_id in group {
			actual := tc.resolve_expr(value_id, lhs_types[i])
			if !tc.type_compatible(actual, lhs_types[i]) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${actual.name()}` to `${lhs_types[i].name()}`', id)
			}
		}
	}
	return lhs_types
}

fn (tc &TypeChecker) match_multi_return_types(expr_id flat.NodeId, count int) ?[]Type {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return none
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		return none
	}
	mut types := []Type{}
	mut saw_value_branch := false
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			continue
		}
		tail_id := tc.branch_tail_expr_id(branch_id)
		if !tc.valid_node_id(tail_id) {
			return none
		}
		tail := tc.a.nodes[int(tail_id)]
		if tail.kind == .return_stmt || tc.expr_never_returns(tail_id) {
			continue
		}
		multi := multi_return_payload_type(tc.resolve_type(tail_id)) or { return none }
		if multi.types.len != count {
			return none
		}
		for typ in multi.types {
			if !type_has_runtime_value(typ) {
				return none
			}
		}
		if !saw_value_branch {
			types = multi.types.clone()
			saw_value_branch = true
			continue
		}
		for j, actual in multi.types {
			if actual.name() != types[j].name() {
				return none
			}
		}
	}
	if !saw_value_branch {
		return none
	}
	return types
}

fn (tc &TypeChecker) match_has_tuple_tail_values(expr_id flat.NodeId, count int) bool {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return false
	}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if groups := tc.tuple_tail_value_groups(branch_id, count, false) {
			if groups.len > 0 {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) expr_has_tuple_tail_values(expr_id flat.NodeId, count int) bool {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	if groups := tc.tuple_tail_value_groups(expr_id, count, false) {
		if groups.len > 0 {
			return true
		}
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count > 1 && tc.expr_has_tuple_tail_values(tc.a.child(&node, 1), count) {
				return true
			}
			return node.children_count > 2
				&& tc.expr_has_tuple_tail_values(tc.a.child(&node, 2), count)
		}
		.match_stmt {
			return tc.match_has_tuple_tail_values(expr_id, count)
		}
		.block, .match_branch {
			body_start := if node.kind == .match_branch {
				if node.value == 'else' { 0 } else { node.value.int() }
			} else {
				0
			}
			if node.children_count <= body_start {
				return false
			}
			return tc.expr_has_tuple_tail_values(tc.a.child(&node, node.children_count - 1), count)
		}
		.expr_stmt {
			return node.children_count > 0
				&& tc.expr_has_tuple_tail_values(tc.a.child(&node, 0), count)
		}
		else {
			return false
		}
	}
}

// multi_expr_tail_types_for_transform returns promoted multi-expression tail
// types for transform lowering without duplicating checker compatibility rules.
pub fn (tc &TypeChecker) multi_expr_tail_types_for_transform(expr_id flat.NodeId, count int) ?[]Type {
	return tc.multi_expr_tail_types(expr_id, count)
}

fn (tc &TypeChecker) promoted_multi_tail_type(current Type, actual Type) ?Type {
	if tc.type_compatible(actual, current) {
		return current
	}
	if tc.type_compatible(current, actual) {
		return actual
	}
	return none
}

fn (tc &TypeChecker) multi_expr_tail_value_groups(expr_id flat.NodeId, count int, explicit_comma_tail bool) ?[][]flat.NodeId {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count < 3 {
				return none
			}
			mut groups := [][]flat.NodeId{}
			then_groups := tc.tuple_tail_value_groups(tc.a.child(&node, 1), count,
				explicit_comma_tail) or { return none }
			for group in then_groups {
				groups << group
			}
			else_id := tc.a.child(&node, 2)
			else_groups := tc.multi_expr_tail_value_groups(else_id, count, explicit_comma_tail) or {
				return none
			}
			for group in else_groups {
				groups << group
			}
			return groups
		}
		.match_stmt {
			if node.children_count < 2 || !tc.match_has_else_or_exhaustive_coverage(node) {
				return none
			}
			mut groups := [][]flat.NodeId{}
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				// Match multi-value tails must be comma expressions, not adjacent statements.
				branch_groups := tc.tuple_tail_value_groups(branch_id, count, true) or {
					return none
				}
				for group in branch_groups {
					groups << group
				}
			}
			return groups
		}
		.block, .match_branch {
			return tc.tuple_tail_value_groups(expr_id, count, explicit_comma_tail)
		}
		.lock_expr {
			if node.children_count > 0 {
				return tc.multi_expr_tail_value_groups(tc.a.child(&node, node.children_count - 1),
					count, explicit_comma_tail)
			}
		}
		.expr_stmt {
			if node.children_count > 0 {
				return tc.multi_expr_tail_value_groups(tc.a.child(&node, 0), count,
					explicit_comma_tail)
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) tuple_tail_value_groups(body_id flat.NodeId, count int, explicit_comma_tail bool) ?[][]flat.NodeId {
	if count <= 0 || !tc.valid_node_id(body_id) {
		return none
	}
	body := tc.a.nodes[int(body_id)]
	body_start := if body.kind == .match_branch {
		if body.value == 'else' { 0 } else { body.value.int() }
	} else {
		0
	}
	if body.kind !in [.block, .match_branch] || body.children_count <= body_start {
		return none
	}
	last_id := tc.a.child(&body, body.children_count - 1)
	if tc.valid_node_id(last_id) {
		last := tc.a.nodes[int(last_id)]
		if last.kind in [.block, .match_branch, .if_expr, .match_stmt] {
			return tc.multi_expr_tail_value_groups(last_id, count, explicit_comma_tail)
		}
		if last.kind == .return_stmt {
			return [][]flat.NodeId{}
		}
		if tc.expr_never_returns(last_id) {
			return [][]flat.NodeId{}
		}
	}
	is_comma_tail := body.value == 'comma_exprs'
	if explicit_comma_tail && !is_comma_tail {
		return none
	}
	mut values := []flat.NodeId{}
	for i := int(body.children_count) - 1; i >= body_start; i-- {
		child_id := tc.a.child(&body, i)
		if !tc.valid_node_id(child_id) {
			return none
		}
		child := tc.a.nodes[int(child_id)]
		if child.kind != .expr_stmt || child.children_count == 0 {
			if is_comma_tail {
				return none
			}
			break
		}
		for j := int(child.children_count) - 1; j >= 0; j-- {
			values.prepend(tc.a.child(&child, j))
			if !is_comma_tail && values.len == count {
				break
			}
		}
		if !is_comma_tail && values.len == count {
			mut groups := [][]flat.NodeId{}
			groups << values
			return groups
		}
	}
	if is_comma_tail && values.len == count {
		return [values]
	}
	return none
}

// multi_assign_lhs_ids supports multi assign lhs ids handling for TypeChecker.
fn (tc &TypeChecker) multi_assign_lhs_ids(node flat.Node) []flat.NodeId {
	lhs_count := tc.multi_assign_lhs_count(node)
	mut lhs_ids := []flat.NodeId{cap: lhs_count}
	for i in 0 .. lhs_count {
		lhs_ids << tc.multi_assign_lhs_id(node, i)
	}
	return lhs_ids
}

fn (tc &TypeChecker) multi_assign_lhs_count(node flat.Node) int {
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

fn (tc &TypeChecker) multi_assign_rhs_count(node flat.Node) int {
	lhs_count := tc.multi_assign_lhs_count(node)
	rhs_count := int(node.children_count) - lhs_count
	return if rhs_count > 0 { rhs_count } else { 0 }
}

fn (tc &TypeChecker) multi_assign_lhs_id(node flat.Node, index int) flat.NodeId {
	rhs_count := tc.multi_assign_rhs_count(node)
	child_index := if index < rhs_count { index * 2 } else { rhs_count + index }
	return tc.a.child(&node, child_index)
}

fn (tc &TypeChecker) multi_assign_rhs_id(node flat.Node, index int) flat.NodeId {
	return tc.a.child(&node, index * 2 + 1)
}

// insert_decl_lhs updates insert decl lhs state for types.
fn (tc &TypeChecker) decl_lhs_is_mut(node flat.Node, lhs_id flat.NodeId) bool {
	if node.is_mut {
		return true
	}
	if int(lhs_id) < 0 || int(lhs_id) >= tc.a.nodes.len {
		return false
	}
	return tc.a.nodes[int(lhs_id)].is_mut
}

fn (mut tc TypeChecker) insert_decl_lhs(lhs_id flat.NodeId, typ Type, is_mut bool) ScopeBindingOwner {
	if int(lhs_id) < 0 || typ is Void {
		return ScopeBindingOwner{}
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && lhs.value.len > 0 {
		if lhs.value != '_' && (tc.visible_local_scope_owns_name(lhs.value)
			|| tc.mut_param_binding_matches_lvalue(lhs.value)) {
			tc.record_error(.assignment_mismatch, 'redefinition of ${lhs.value}', lhs_id)
			return ScopeBindingOwner{}
		}
		owner := tc.cur_scope.insert_with_owner(lhs.value, typ)
		if is_mut && lhs.value != '_' {
			tc.cur_fn_mut_local_binding_owners[lhs.value] = owner
		}
		tc.register_synth_type(lhs_id, typ)
		return owner
	}
	return ScopeBindingOwner{}
}

fn (tc &TypeChecker) visible_local_scope_owns_name(name string) bool {
	if name.len == 0 || tc.cur_scope == unsafe { nil } {
		return false
	}
	scope := tc.cur_scope
	for i := scope.names.len - 1; i >= 0; i-- {
		if scope.names[i] == name {
			return true
		}
	}
	return false
}

// check_assign validates check assign state for types.
fn (mut tc TypeChecker) check_assign(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	if node.kind == .index_assign && tc.reject_unlowered_map_mutation
		&& tc.index_assign_lhs_is_map(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'internal compiler error: unlowered map index assignment reached post-transform checker',
				id)
		}
		for i := 1; i < node.children_count; i += 2 {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	if tc.check_multi_return_assign(id, node) {
		return
	}
	if tc.check_assignment_marker(id, node) {
		return
	}
	mut i := 0
	mut ownership_lhs_ids := []flat.NodeId{}
	mut ownership_rhs_ids := []flat.NodeId{}
	mut ownership_lhs_types := []Type{}
	mut ownership_rhs_types := []Type{}
	mut smartcast_write_keys := []string{cap: int(node.children_count) / 2}
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_type := tc.resolve_lvalue_type(lhs_id)
		tc.remember_expr_type(lhs_id, lhs_type)
		expected_type := tc.assignment_expected_type(lhs_id, lhs_type)
		tc.annotate_expected_expr(rhs_id, expected_type)
		$if ownership ? {
			if tc.ownership_should_defer_aggregate_consumption(lhs_id, node.op) {
				tc.ownership_begin_defer_aggregate_consumption(rhs_id)
				tc.check_node_with_expected_context(rhs_id, expected_type)
				tc.ownership_end_defer_aggregate_consumption(rhs_id)
			} else {
				tc.check_node_with_expected_context(rhs_id, expected_type)
			}
		} $else {
			tc.check_node_with_expected_context(rhs_id, expected_type)
		}
		rhs_type := tc.resolve_expr(rhs_id, expected_type)
		if !tc.assignment_types_compatible(rhs_id, rhs_type, expected_type, node.op) {
			tc.type_mismatch(.assignment_mismatch,
				'cannot assign `${rhs_type.name()}` to `${expected_type.name()}`', id)
		}
		$if ownership ? {
			ownership_lhs_ids << lhs_id
			ownership_rhs_ids << rhs_id
			ownership_lhs_types << lhs_type
			ownership_rhs_types << rhs_type
		}
		if node.kind in [.assign, .selector_assign, .index_assign] {
			if tc.expr_is_method_value(rhs_id) && !tc.lvalue_is_local_var(lhs_id) {
				// Storing a method value into a struct field (`h.cb = ..`), an array/map element
				// (`cbs[i] = ..`), or a global lets it outlive the per-site static `_mvctx_N`
				// receiver slot, which the next evaluation of the same site overwrites — so every
				// stored callback would use the last receiver. Reject it like the other escapes.
				tc.reject_stored_method_value(rhs_id)
			} else {
				tc.track_method_value_local(lhs_id, rhs_id)
			}
			tc.reject_stored_capturing_fn_literal(rhs_id)
		}
		lhs_key := tc.expr_key(lhs_id)
		if lhs_key.len > 0 {
			smartcast_write_keys << lhs_key
		}
		i += 2
	}
	// All RHS expressions observe the pre-assignment values. Invalidate written
	// smartcasts only after every pair has been checked.
	for key in smartcast_write_keys {
		tc.smartcasts.delete(key)
	}
	$if ownership ? {
		tc.ownership_after_assign_pairs(ownership_lhs_ids, ownership_rhs_ids, ownership_lhs_types,
			ownership_rhs_types, node.op, id)
	} $else {
		_ = ownership_lhs_ids
		_ = ownership_rhs_ids
		_ = ownership_lhs_types
		_ = ownership_rhs_types
	}
}

fn (tc &TypeChecker) assignment_types_compatible(rhs_id flat.NodeId, rhs_type Type, expected_type Type, op flat.Op) bool {
	return tc.expr_compatible(rhs_id, rhs_type, expected_type)
		|| tc.pointer_value_compatible(rhs_type, expected_type)
		|| tc.pointer_arithmetic_assign_compatible(op, rhs_type, expected_type)
}

fn (mut tc TypeChecker) check_assignment_marker(id flat.NodeId, node flat.Node) bool {
	if node.value.starts_with('for init assignment mismatch:') {
		tc.record_error(.assignment_mismatch, node.value, id)
		return true
	}
	return false
}

// index_assign_lhs_is_map supports index assign lhs is map handling for TypeChecker.
fn (tc &TypeChecker) index_assign_lhs_is_map(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	lhs_id := tc.a.child(&node, 0)
	if int(lhs_id) < 0 {
		return false
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count < 2 {
		return false
	}
	base_type := unwrap_pointer(tc.resolve_type(tc.a.child(&lhs, 0)))
	return base_type is Map
}

// check_multi_return_assign validates check multi return assign state for types.
fn (mut tc TypeChecker) check_multi_return_assign(id flat.NodeId, node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs := tc.a.nodes[int(rhs_id)]
	lhs_ids := tc.multi_assign_lhs_ids(node)
	if tc.multi_assign_rhs_count(node) != 1 {
		return false
	}
	for lhs_id in lhs_ids {
		tc.remember_expr_type(lhs_id, tc.resolve_lvalue_type(lhs_id))
	}
	if rhs.kind == .match_stmt {
		tc.check_node(rhs_id)
		if !tc.match_has_else_or_exhaustive_coverage(rhs) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'match expression must be exhaustive for multi-return assignment', id)
			}
			return true
		}
		if rhs_types := tc.match_multi_return_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				lhs_type := tc.resolve_lvalue_type(lhs_id)
				if !tc.type_compatible(rhs_types[i], lhs_type) {
					tc.type_mismatch(.assignment_mismatch,
						'cannot assign `${rhs_types[i].name()}` to `${lhs_type.name()}`', id)
				}
			}
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		if rhs_types := tc.multi_expr_tail_assign_types(id, rhs_id, lhs_ids) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		if tc.should_diagnose(id) {
			if tc.match_has_tuple_tail_values(rhs_id, lhs_ids.len) {
				tc.record_error(.assignment_mismatch,
					'match expression branches cannot produce multiple assignment values', id)
			} else {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
		}
		return true
	}
	if rhs.kind == .if_expr {
		tc.check_node(rhs_id)
		if rhs_types := tc.multi_expr_tail_assign_types(id, rhs_id, lhs_ids) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		rhs_type := tc.resolve_type(rhs_id)
		if rhs_type !is MultiReturn || tc.expr_has_tuple_tail_values(rhs_id, lhs_ids.len) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
			return true
		}
	}
	if rhs.kind == .lock_expr {
		tc.check_node(rhs_id)
		if rhs_types := tc.multi_expr_tail_assign_types(id, rhs_id, lhs_ids) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
	}
	mut rhs_type := tc.resolve_type(rhs_id)
	mut rhs_checked := false
	mut rhs_multi := MultiReturn{}
	mut found_multi := false
	mut unhandled_multi := false
	for _ in 0 .. 2 {
		if multi := tc.multi_return_assignment_type(rhs_id, rhs_type) {
			rhs_multi = multi
			found_multi = true
			break
		}
		if _ := tc.unhandled_wrapped_multi_return_type(rhs_id, rhs_type) {
			unhandled_multi = true
			break
		}
		if rhs_checked {
			break
		}
		tc.check_node(rhs_id)
		rhs_checked = true
		rhs_type = tc.resolve_type(rhs_id)
	}
	rhs_type_name := rhs_type.name()
	if unhandled_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'multi-return assignment from `${rhs_type_name}` requires `or {}`, `!`, or `?` handling',
				id)
		}
		return true
	}
	if found_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if lhs_ids.len != rhs_multi.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: ${lhs_ids.len} variables but `${rhs_type_name}` has ${rhs_multi.types.len} values',
					id)
			}
			return true
		}
		for i, lhs_id in lhs_ids {
			lhs_type := tc.resolve_lvalue_type(lhs_id)
			expected_type := tc.assignment_expected_type(lhs_id, lhs_type)
			if !tc.type_compatible(rhs_multi.types[i], expected_type) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${rhs_multi.types[i].name()}` to `${expected_type.name()}`', id)
			}
		}
		$if ownership ? {
			tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, rhs_multi, id)
		}
		return true
	}
	return false
}

// check_postfix validates check postfix state for types.
fn (mut tc TypeChecker) check_postfix(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	tc.check_node(child_id)
	child := tc.a.nodes[int(child_id)]
	if node.op == .not && node.value == 'ragged_inferred_fixed_array' && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'inferred fixed-array literal rows must have the same size', id)
	}
	if child.kind == .index && child.children_count >= 2 {
		base_type := unwrap_pointer(tc.resolve_type(tc.a.child(&child, 0)))
		if base_type is Map && node.op in [.inc, .dec] && tc.reject_unlowered_map_mutation
			&& tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'internal compiler error: unlowered map index postfix mutation reached post-transform checker',
				id)
		}
	}
}

fn (tc &TypeChecker) assignment_expected_type(lhs_id flat.NodeId, lhs_type Type) Type {
	if int(lhs_id) < 0 {
		return lhs_type
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident {
		if lhs.value == '_' {
			return Type(Unknown{})
		}
		if base := tc.mut_param_base_for_current_ident(lhs.value, lhs_type) {
			return base
		}
	}
	return lhs_type
}

fn (tc &TypeChecker) lvalue_matches_mut_param(lhs_type Type, base_type Type) bool {
	if lhs_type is Pointer {
		return tc.type_compatible(lhs_type.base_type, base_type)
			&& tc.type_compatible(base_type, lhs_type.base_type)
	}
	return false
}

fn (tc &TypeChecker) mut_param_binding_matches_lvalue(name string) bool {
	if tc.cur_scope == unsafe { nil } {
		return false
	}
	if param_owner := tc.cur_fn_mut_param_binding_owners[name] {
		if owner := tc.cur_scope.lookup_owner(name) {
			return owner.scope == param_owner.scope && owner.index == param_owner.index
				&& owner.generation == param_owner.generation
		}
	}
	return false
}

// resolve_lvalue_type resolves resolve lvalue type information for types.
fn (mut tc TypeChecker) resolve_lvalue_type(lhs_id flat.NodeId) Type {
	if int(lhs_id) < 0 {
		return Type(void_)
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident {
		if typ := tc.cur_scope.lookup(lhs.value) {
			return typ
		}
		if typ := tc.file_scope.lookup(lhs.value) {
			return typ
		}
		qname := tc.qualify_name(lhs.value)
		if qname != lhs.value {
			if typ := tc.file_scope.lookup(qname) {
				return typ
			}
		}
		if tc.should_diagnose(lhs_id) && lhs.value != '_' {
			tc.record_error(.unknown_ident, 'unknown identifier `${lhs.value}`', lhs_id)
		}
		return unknown_type('unknown identifier `${lhs.value}`')
	}
	if lhs.kind == .selector {
		tc.check_selector(lhs_id, lhs)
		if typ := tc.selector_type(lhs_id, lhs) {
			return typ
		}
		return tc.resolve_type(lhs_id)
	}
	if lhs.kind == .index {
		tc.check_index(lhs_id, lhs)
		return tc.resolve_type(lhs_id)
	}
	if lhs.kind == .prefix && lhs.op == .mul && lhs.children_count > 0 {
		inner := tc.resolve_type(tc.a.child(&lhs, 0))
		if inner is Pointer {
			return inner.base_type
		}
		return inner
	}
	return tc.resolve_type(lhs_id)
}

// check_return validates check return state for types.
fn (mut tc TypeChecker) check_return(id flat.NodeId, node flat.Node) {
	// A returned method value escapes the function, where its per-site static receiver
	// can't keep multiple returned callbacks distinct (a factory `fn bind(c) fn () int {
	// return c.report }`); reject it rather than emitting invalid C.
	for i in 0 .. node.children_count {
		tc.reject_stored_method_value(tc.a.child(&node, i))
		tc.reject_stored_capturing_fn_literal(tc.a.child(&node, i))
	}
	expected := tc.cur_fn_ret_type
	saved_expected_expr_id := tc.expected_expr_id
	saved_expected_expr_type := tc.expected_expr_type
	if node.children_count == 1 {
		tc.expected_expr_id = int(tc.a.child(&node, 0))
		tc.expected_expr_type = expected
	}
	defer {
		tc.expected_expr_id = saved_expected_expr_id
		tc.expected_expr_type = saved_expected_expr_type
	}
	if expected is Void {
		if node.children_count > 0 && tc.should_diagnose(id) {
			tc.record_error(.return_mismatch, 'void function should not return a value', id)
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
		}
		$if ownership ? {
			tc.ownership_after_return(id, node)
		}
		return
	}
	if node.children_count == 0 {
		if type_allows_implicit_return(expected) {
			$if ownership ? {
				tc.ownership_after_return(id, node)
			}
			return
		}
		if tc.should_diagnose(id) {
			tc.record_error(.return_mismatch, 'missing return value of type `${expected.name()}`',
				id)
		}
		return
	}
	if node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		tc.annotate_expected_expr(child_id, expected)
		if tc.expr_never_returns_resolving(child_id) {
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
			$if ownership ? {
				tc.ownership_after_return(id, node)
			}
			return
		}
	}
	if expected is ResultType && node.children_count == 1
		&& !tc.result_return_uses_multi_tail(tc.a.child(&node, 0), expected) {
		child_id := tc.a.child(&node, 0)
		$if ownership ? {
			tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
		} $else {
			tc.check_node(child_id)
		}
		if bad_type := tc.invalid_ierror_return_expr_type_name(child_id, expected) {
			tc.record_invalid_ierror_return_error(id,
				'cannot return `${bad_type}` as `${Type(expected).name()}`')
			return
		}
		actual := tc.resolve_expr(child_id, expected)
		if tc.return_type_compatible(child_id, actual, expected) {
			$if ownership ? {
				tc.ownership_after_return(id, node)
			}
			return
		}
	}
	if multi := multi_return_payload_type(expected) {
		if node.children_count == 1 {
			child_id := tc.a.child(&node, 0)
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
			if msg := tc.tuple_tail_return_error(child_id, multi.types) {
				if tc.should_diagnose(id) {
					tc.record_error(.return_mismatch, msg, id)
				}
				return
			}
			actual := tc.resolve_expr(child_id, expected)
			if tc.type_compatible(actual, expected) {
				$if ownership ? {
					tc.ownership_after_return(id, node)
				}
				return
			}
			if actual_multi := multi_return_payload_type(actual) {
				if actual_multi.types.len == multi.types.len {
					mut slots_compatible := true
					for i, actual_type in actual_multi.types {
						if !tc.type_compatible(actual_type, multi.types[i]) {
							slots_compatible = false
							if tc.should_diagnose(id) {
								tc.type_mismatch(.return_mismatch,
									'cannot return `${actual_type.name()}` as `${multi.types[i].name()}`',
									id)
							}
						}
					}
					if slots_compatible {
						$if ownership ? {
							tc.ownership_after_return(id, node)
						}
					}
					return
				}
			}
			if ok := tc.multi_expr_tail_return_compatible(id, child_id, multi.types) {
				if ok {
					$if ownership ? {
						tc.ownership_after_return(id, node)
					}
				}
				return
			}
			if item_types := tc.multi_expr_tail_types(child_id, multi.types.len) {
				if item_types.len == multi.types.len {
					mut ok := true
					for i, item_type in item_types {
						if !tc.type_compatible(item_type, multi.types[i]) {
							ok = false
							if tc.should_diagnose(id) {
								tc.type_mismatch(.return_mismatch,
									'cannot return `${item_type.name()}` as `${multi.types[i].name()}`',
									id)
							}
						}
					}
					if ok {
						$if ownership ? {
							tc.ownership_after_return(id, node)
						}
					}
					return
				}
			}
		}
		if node.children_count != multi.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.return_mismatch,
					'return value count mismatch: expected ${multi.types.len}, got ${node.children_count}',
					id)
			}
			return
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
			actual := tc.resolve_expr(child_id, multi.types[i])
			if !tc.return_type_compatible(child_id, actual, multi.types[i]) {
				tc.type_mismatch(.return_mismatch,
					'cannot return `${actual.name()}` as `${multi.types[i].name()}`', id)
			}
		}
		$if ownership ? {
			tc.ownership_after_return(id, node)
		}
		return
	}
	if node.children_count != 1 {
		if tc.should_diagnose(id) {
			tc.record_error(.return_mismatch,
				'return value count mismatch: expected 1, got ${node.children_count}', id)
		}
		return
	}
	child_id := tc.a.child(&node, 0)
	$if ownership ? {
		tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
	} $else {
		tc.check_node(child_id)
	}
	if expected is ResultType {
		if bad_type := tc.invalid_ierror_return_expr_type_name(child_id, expected) {
			tc.record_invalid_ierror_return_error(id,
				'cannot return `${bad_type}` as `${Type(expected).name()}`')
			return
		}
	}
	actual := tc.resolve_expr(child_id, expected)
	if !tc.return_type_compatible(child_id, actual, expected) {
		tc.type_mismatch(.return_mismatch,
			'cannot return `${actual.name()}` as `${expected.name()}`', id)
		return
	}
	$if ownership ? {
		tc.ownership_after_return(id, node)
	}
}

fn (mut tc TypeChecker) multi_expr_tail_return_compatible(return_id flat.NodeId, expr_id flat.NodeId, expected []Type) ?bool {
	if !tc.multi_expr_tail_return_compat_supported(expr_id) {
		return none
	}
	groups := tc.multi_expr_tail_value_groups(expr_id, expected.len, false) or { return none }
	if groups.len == 0 {
		return none
	}
	mut ok := true
	for group in groups {
		if group.len != expected.len {
			return none
		}
		for i, value_id in group {
			actual := tc.resolve_expr(value_id, expected[i])
			if !type_has_runtime_value(actual) {
				return none
			}
			if !tc.return_type_compatible(value_id, actual, expected[i]) {
				ok = false
				tc.type_mismatch(.return_mismatch,
					'cannot return `${actual.name()}` as `${expected[i].name()}`', return_id)
			}
		}
	}
	return ok
}

fn (tc &TypeChecker) multi_expr_tail_return_compat_supported(expr_id flat.NodeId) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	// Raw tuple-tail return lowering is currently implemented for if- and match-expressions.
	if node.kind in [.if_expr, .match_stmt] {
		return true
	}
	if node.kind == .expr_stmt && node.children_count > 0 {
		return tc.multi_expr_tail_return_compat_supported(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) result_return_uses_multi_tail(expr_id flat.NodeId, expected Type) bool {
	multi := multi_return_payload_type(expected) or { return false }
	return tc.expr_has_tuple_tail_values(expr_id, multi.types.len)
}

fn tuple_tail_return_lowering_allowed(expected []Type) bool {
	for typ in expected {
		if tuple_tail_return_type_allows_lowering(typ) {
			return true
		}
	}
	return false
}

fn tuple_tail_return_type_allows_lowering(typ Type) bool {
	if typ is ArrayFixed || typ is Struct {
		return true
	}
	if typ is Alias {
		return tuple_tail_return_type_allows_lowering(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) tuple_tail_return_error(expr_id flat.NodeId, expected []Type) ?string {
	if tuple_tail_return_lowering_allowed(expected) {
		return none
	}
	if !tc.valid_node_id(expr_id) {
		return none
	}
	count := expected.len
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count > 0 {
				return tc.tuple_tail_return_error(tc.a.child(&node, 0), expected)
			}
		}
		.if_expr {
			if tc.expr_has_tuple_tail_values(expr_id, count) {
				return 'if expression branches cannot produce multiple return values'
			}
		}
		.match_stmt {
			if tc.expr_has_tuple_tail_values(expr_id, count) {
				return 'match expression branches cannot produce multiple return values'
			}
		}
		else {}
	}

	return none
}

fn (mut tc TypeChecker) return_type_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if tc.expr_compatible(expr_id, actual, expected) {
		return true
	}
	if tc.return_numeric_alias_compatible(actual, expected) {
		return true
	}
	if tc.pointer_value_compatible(actual, expected) {
		return true
	}
	if expected is OptionType {
		if tc.pointer_value_compatible(actual, expected.base_type) {
			return true
		}
	}
	if tc.expr_generic_expected_match(expr_id, actual, expected) {
		return true
	}
	if expected is ResultType {
		if tc.zero_literal_can_be_pointer(expr_id, expected.base_type) {
			return true
		}
		if tc.pointer_value_compatible(actual, expected.base_type) {
			return true
		}
		if is_ierror_type(actual) || tc.type_embeds_error(actual) {
			return true
		}
		if tc.type_compatible_with_ierror_payload(actual) {
			return true
		}
	}
	if base := tc.mut_param_expr_base(expr_id, actual) {
		if tc.type_compatible(base, expected) || tc.generic_receiver_base_match(base, expected) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) generic_expected_type_match(actual Type, expected Type) bool {
	if tc.generic_receiver_base_match(actual, expected) {
		return true
	}
	if expected is OptionType {
		if actual is OptionType {
			return tc.generic_expected_type_match(actual.base_type, expected.base_type)
		}
		return tc.generic_expected_type_match(actual, expected.base_type)
	}
	if expected is ResultType {
		if actual is ResultType {
			return tc.generic_expected_type_match(actual.base_type, expected.base_type)
		}
		return tc.generic_expected_type_match(actual, expected.base_type)
	}
	if actual is Array && expected is Array {
		return tc.generic_expected_type_match(actual.elem_type, expected.elem_type)
	}
	if actual is ArrayFixed && expected is ArrayFixed {
		return tc.fixed_array_lengths_compatible(actual, expected)
			&& tc.generic_expected_type_match(actual.elem_type, expected.elem_type)
	}
	if actual is Map && expected is Map {
		return tc.generic_expected_type_match(actual.key_type, expected.key_type)
			&& tc.generic_expected_type_match(actual.value_type, expected.value_type)
	}
	return false
}

fn (tc &TypeChecker) pointer_value_compatible(actual Type, expected Type) bool {
	if actual is Pointer {
		if !pointer_value_base_can_match(actual.base_type) {
			return false
		}
		return tc.type_compatible(actual.base_type, expected)
			|| pointer_value_type_names_match(Type(actual).name(), expected.name())
			|| bare_type_names_match(actual.base_type.name(), expected.name())
	}
	return pointer_value_type_names_match(actual.name(), expected.name())
}

fn pointer_value_base_can_match(typ Type) bool {
	clean := if typ is Alias { typ.base_type } else { typ }
	return clean is Struct || clean is Interface || clean is SumType || clean is Array
		|| clean is ArrayFixed || clean is Map || clean is Channel
}

fn pointer_value_type_names_match(actual string, expected string) bool {
	if !actual.starts_with('&') {
		return false
	}
	clean_actual := actual[1..]
	return clean_actual == expected
}

fn bare_type_names_match(actual string, expected string) bool {
	return actual == expected
}

fn (tc &TypeChecker) return_numeric_alias_compatible(actual Type, expected Type) bool {
	if expected is Alias {
		return type_is_numeric(expected.base_type) && tc.type_compatible(actual, expected.base_type)
	}
	return false
}

fn type_is_numeric(typ Type) bool {
	clean := if typ is Alias { typ.base_type } else { typ }
	return clean.is_integer() || clean.is_float()
}

fn (tc &TypeChecker) expr_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	return tc.type_compatible(actual, expected) || tc.zero_literal_can_be_pointer(expr_id, expected)
}

fn (tc &TypeChecker) pointer_arithmetic_assign_compatible(op flat.Op, actual Type, expected Type) bool {
	if op !in [.plus_assign, .minus_assign] {
		return false
	}
	clean_expected := if expected is Alias { expected.base_type } else { expected }
	if clean_expected !is Pointer {
		return false
	}
	clean_actual := if actual is Alias { actual.base_type } else { actual }
	return clean_actual.is_integer()
}

fn (tc &TypeChecker) zero_literal_can_be_pointer(expr_id flat.NodeId, expected Type) bool {
	if !tc.is_zero_literal(expr_id) && !tc.expr_is_unsafe_zero_literal(expr_id) {
		return false
	}
	clean := if expected is Alias { expected.base_type } else { expected }
	return clean is Pointer
}

fn (tc &TypeChecker) expr_is_unsafe_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .block {
		return false
	}
	return tc.expr_tail_is_zero_literal(id)
}

fn (tc &TypeChecker) expr_tail_is_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return node.value == '0'
		}
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_zero_literal(tc.a.child(&node, 0))
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_zero_literal(tc.a.child(&node, node.children_count - 1))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) mut_param_expr_base(expr_id flat.NodeId, typ Type) ?Type {
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind == .ident && node.value.len > 0 {
		return tc.mut_param_base_for_current_ident(node.value, typ)
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child := tc.a.nodes[int(tc.a.child(&node, 0))]
		if child.kind == .ident && child.value.len > 0 {
			base := tc.cur_fn_mut_param_base_types[child.value] or { return none }
			if !tc.mut_param_binding_matches_lvalue(child.value) {
				return none
			}
			return Type(Pointer{
				base_type: base
			})
		}
	}
	return none
}

fn (tc &TypeChecker) current_checked_fn_qname() ?string {
	if tc.cur_fn_node_id < 0 || tc.cur_fn_node_id >= tc.a.nodes.len {
		return none
	}
	fn_node := tc.a.nodes[tc.cur_fn_node_id]
	if fn_node.kind != .fn_decl || fn_node.value.len == 0 {
		return none
	}
	return checker_qualified_fn_name(tc.cur_module, fn_node.value)
}

// record_invalid_ierror_return_error records an invalid-ierror-return error,
// gating non-diagnostic-file sites on the called-fns closure. While that
// closure is still being computed on the collector thread (parallel check),
// the candidate is parked in pending_ierror_errors and filtered after join.
fn (mut tc TypeChecker) record_invalid_ierror_return_error(id flat.NodeId, msg string) {
	if tc.should_diagnose(id) {
		tc.record_error_unfiltered(.return_mismatch, msg, id)
		return
	}
	qname := tc.current_checked_fn_qname() or { return }
	if tc.defer_ierror_gating {
		tc.pending_ierror_errors << PendingIerrorError{
			err:      tc.make_type_error(.return_mismatch, msg, id)
			fn_qname: qname
		}
		return
	}
	if qname in tc.selected_file_called_fns {
		tc.record_error_unfiltered(.return_mismatch, msg, id)
	}
}

fn contextual_payload_type(typ Type) ?Type {
	if typ is OptionType {
		if typ.base_type is Void {
			return none
		}
		return typ.base_type
	}
	if typ is ResultType {
		if typ.base_type is Void {
			return none
		}
		return typ.base_type
	}
	return none
}

fn (mut tc TypeChecker) invalid_ierror_return_expr_type_name(id flat.NodeId, expected ResultType) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	// An explicit return in a match/if branch is checked against the enclosing
	// function independently; it is not a value tail of the outer result expr.
	if node.kind == .return_stmt {
		return none
	}
	raw_type := tc.resolve_type(id)
	if tc.type_compatible(raw_type, expected) {
		return none
	}
	if tc.type_compatible(raw_type, expected.base_type) {
		return none
	}
	if tc.zero_literal_can_be_pointer(id, expected.base_type) {
		return none
	}
	if tc.pointer_value_compatible(raw_type, expected.base_type) {
		return none
	}
	payload_type := tc.resolve_expr(id, expected.base_type)
	if tc.type_compatible(payload_type, expected.base_type) {
		return none
	}
	if tc.pointer_value_compatible(payload_type, expected.base_type) {
		return none
	}
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count > 0 {
				return tc.invalid_ierror_return_expr_type_name(tc.a.child(&node, 0), expected)
			}
		}
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				return tc.invalid_ierror_return_expr_type_name(tc.a.child(&node, 0), expected)
			}
		}
		.struct_init {
			concrete := tc.resolve_unqualified_builtin_error_struct_name(node.value) or {
				tc.resolve_selective_import_type_symbol(node.value) or {
					tc.qualify_name(node.value)
				}
			}
			if !tc.named_type_compatible_with_ierror(concrete) {
				return concrete
			}
		}
		.match_stmt {
			subject_id := tc.a.child(&node, 0)
			subject_key := tc.expr_key(subject_id)
			subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					continue
				}
				branch := tc.a.nodes[int(branch_id)]
				if branch.kind != .match_branch {
					continue
				}
				tail := tc.branch_tail_expr_id(branch_id)
				if subject_key.len > 0 && tc.expr_key(tail) == subject_key && branch.value != 'else'
					&& branch.value.int() == 1 && subject_type is SumType {
					cond := tc.a.node(tc.a.child(&branch, 0))
					if pattern := tc.match_type_pattern(cond) {
						variant := tc.sum_variant_type_for_pattern(subject_type.name, pattern) or {
							pattern
						}
						if tc.type_compatible(tc.parse_type(variant), expected.base_type) {
							continue
						}
					}
				}
				if bad_type := tc.invalid_ierror_return_expr_type_name(tail, expected) {
					return bad_type
				}
			}
		}
		.if_expr {
			if node.children_count > 1 {
				then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
				if bad_type := tc.invalid_ierror_return_expr_type_name(then_tail, expected) {
					return bad_type
				}
			}
			if node.children_count > 2 {
				else_id := tc.a.child(&node, 2)
				else_tail := if tc.valid_node_id(else_id)
					&& tc.a.nodes[int(else_id)].kind == .if_expr {
					else_id
				} else {
					tc.branch_tail_expr_id(else_id)
				}
				if bad_type := tc.invalid_ierror_return_expr_type_name(else_tail, expected) {
					return bad_type
				}
			}
		}
		else {
			if is_ierror_type(raw_type) || tc.type_compatible_with_ierror_payload(raw_type) {
				return none
			}
			if raw_type is Unknown || raw_type is Void {
				return none
			}
			return raw_type.name()
		}
	}

	return none
}

fn multi_return_payload_type(typ Type) ?MultiReturn {
	if typ is MultiReturn {
		return typ
	}
	if typ is OptionType {
		base := typ.base_type
		if base is MultiReturn {
			return base
		}
	}
	if typ is ResultType {
		base := typ.base_type
		if base is MultiReturn {
			return base
		}
	}
	return none
}

fn (tc &TypeChecker) multi_return_assignment_type(rhs_id flat.NodeId, rhs_type Type) ?MultiReturn {
	if rhs_type is MultiReturn {
		return rhs_type
	}
	if !tc.expr_has_option_result_handler(rhs_id) {
		return none
	}
	return multi_return_payload_type(rhs_type)
}

fn (tc &TypeChecker) unhandled_wrapped_multi_return_type(rhs_id flat.NodeId, rhs_type Type) ?MultiReturn {
	if rhs_type is MultiReturn || tc.expr_has_option_result_handler(rhs_id) {
		return none
	}
	return multi_return_payload_type(rhs_type)
}

fn (tc &TypeChecker) expr_has_option_result_handler(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .or_expr {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_has_option_result_handler(tc.a.child(&node, 0))
	}
	return false
}

// check_call validates check call state for types.
fn (mut tc TypeChecker) check_call(id flat.NodeId, node flat.Node) {
	if node.children_count > 0 {
		callee := tc.a.child_node(&node, 0)
		if callee.kind == .ident && callee.value == '__v_compile_error' {
			message := if node.children_count > 1 {
				arg := tc.a.child_node(&node, 1)
				if arg.value.len > 0 {
					arg.value
				} else {
					'compile-time error'
				}
			} else {
				'compile-time error'
			}
			tc.record_error_unfiltered(.compile_error, 'compile-time error: ${message}', id)
			return
		}
		if callee.kind == .selector && callee.value == '$' {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			return
		}
	}
	if sum_name := tc.sum_constructor_call_name(node) {
		tc.check_sum_constructor_call(id, node, sum_name)
		return
	}
	if info0 := tc.resolve_call_info(id, node) {
		info := tc.specialized_plain_generic_call_info(node, info0)
		if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
			tc.remember_resolved_call(id, info.name)
		}
		if info.return_type !is Void && info.return_type !is Unknown {
			tc.remember_expr_type(id, info.return_type)
		}
		tc.check_call_arg_types(id, node, info)
		$if ownership ? {
			tc.ownership_after_call(id, node, info)
		}
		return
	}
	if tc.is_unsupported_hex_call(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
		}
		return
	}
	if tc.call_has_ambiguous_selective_import(node) {
		tc.record_error(.unknown_fn, 'ambiguous selective import `${tc.call_display_name(node)}`',
			id)
		return
	}
	if tc.should_diagnose(id) && !tc.is_known_call(node)
		&& !tc.call_generic_args_have_placeholders(node) && !tc.call_receiver_type_is_unknown(node) {
		tc.record_error(.unknown_fn, 'unknown function `${tc.call_display_name(node)}`', id)
	}
	dsl_name := tc.unresolved_array_dsl_call_name(node)
	if dsl_name.len > 0 {
		tc.push_array_dsl_scope(node, dsl_name)
	}
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
	if dsl_name.len > 0 {
		tc.pop_scope()
	}
}

// cur_fn_is_generic_template reports whether the function currently being
// checked is a template that will be re-validated per instantiation: it
// declares generic parameters, or is a method on a generic receiver
// (`fn (mut so SetOf[T]) sort()`), whose own node has no generic_params.
fn (tc &TypeChecker) cur_fn_is_generic_template() bool {
	if tc.cur_fn_node_id < 0 || tc.cur_fn_node_id >= tc.a.nodes.len {
		return false
	}
	fn_node := tc.a.nodes[tc.cur_fn_node_id]
	if fn_node.generic_params.len > 0 {
		return true
	}
	for i in 0 .. fn_node.children_count {
		child := tc.a.child_node(&fn_node, i)
		if child.kind == .param && child.typ.len > 0
			&& tc.type_text_has_generic_placeholder(child.typ) {
			return true
		}
	}
	return false
}

// call_receiver_type_is_unknown reports whether a method call's receiver has
// an unresolvable type, so the unknown-function diagnostic must not fire for
// it. Besides generic templates (whose bodies the reference compiler also
// only validates per instantiation), the checker currently fails to resolve
// several legitimate receiver shapes — fields of generic struct instances
// (`json.decode[S[T]](s)!.val.unix()`), option/or-unwrapped results, and
// specialized generic call returns — so narrowing this to "provably generic"
// chains produces false errors on valid vlib code. The receiver expression
// itself IS checked, so `missing.method()` still reports its unknown
// identifier. Remaining gap: a template instantiated with a type lacking the
// method fails only in C compilation — v3 has no instantiation-time recheck
// of cloned template bodies yet.
fn (mut tc TypeChecker) call_receiver_type_is_unknown(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	base_id := tc.a.child(callee, 0)
	base_type := tc.resolve_type(base_id)
	if unwrap_pointer(base_type) !is Unknown {
		return false
	}
	tc.check_node(base_id)
	return true
}

// call_generic_args_have_placeholders reports whether the call carries explicit
// generic type args that are still uninstantiated placeholders (`p.read_element[T]()`
// inside a generic template). Such calls can only be validated after
// monomorphization, so unknown-function diagnostics must not fire for them.
// Outside a generic template a bare `missing[T]()` is real invalid code, so
// the deferral only applies within one. This deliberately includes callees
// with no known declaration: the reference compiler also accepts a template
// whose body calls a missing function as long as it is never instantiated
// (vlib code relies on this, e.g. asn1's commented-out `read_element`), and
// only reports it at instantiation time.
fn (tc &TypeChecker) call_generic_args_have_placeholders(node flat.Node) bool {
	if !tc.cur_fn_is_generic_template() {
		return false
	}
	// An explicit generic METHOD call whose type args still carry the
	// template's own placeholders (`p.read_element[T]()`) cannot be resolved
	// before instantiation - defer even when the target is not a known
	// declaration, matching v1, where an uninstantiated generic body is never
	// checked. The monomorph validator reports it if the template ever gets
	// specialized. A plain `missing[T]()` call still errors right away.
	if node.children_count > 0 {
		callee := tc.a.child_node(&node, 0)
		if callee.kind == .index && callee.children_count >= 2 && callee.value != 'range'
			&& tc.a.child_node(callee, 0).kind == .selector {
			for i in 1 .. int(callee.children_count) {
				arg := tc.a.child_node(callee, i)
				if arg.kind == .ident && arg.value.len > 0
					&& tc.type_text_has_generic_placeholder(arg.value) {
					return true
				}
			}
		}
	}
	if !tc.explicit_generic_call_target_is_known(node) {
		return false
	}
	if node.value.len > 0 {
		for arg in node.value.split(',') {
			if tc.type_text_has_generic_placeholder(arg.trim_space()) {
				return true
			}
		}
	}
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .index || callee.children_count < 2 || callee.value == 'range' {
		return false
	}
	for i in 1 .. int(callee.children_count) {
		arg := tc.a.child_node(callee, i)
		if arg.kind == .ident && arg.value.len > 0
			&& tc.type_text_has_generic_placeholder(arg.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) explicit_generic_call_target_is_known(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .index || callee.children_count == 0 || callee.value == 'range' {
		return tc.is_known_call(node)
	}
	base := tc.a.child_node(callee, 0)
	if _ := tc.generic_call_base_name(base) {
		return true
	}
	if base.kind != .selector || base.value.len == 0 {
		return false
	}
	// A method on an unresolved generic receiver cannot be tied to a concrete
	// receiver yet, but it still has to name a real method declaration. The
	// concrete specialization is validated after monomorphization.
	for name, _ in tc.fn_generic_params {
		if name.ends_with('.${base.value}') {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_sum_constructor_call(id flat.NodeId, node flat.Node, sum_name string) {
	expected := Type(SumType{
		name: sum_name
	})
	tc.remember_expr_type(id, expected)
	actual_count := node.children_count - 1
	if actual_count != 1 {
		if tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected 1, got ${actual_count}',
				id)
		}
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	tc.check_node(arg_id)
	arg_type := tc.resolve_expr(arg_id, expected)
	if !tc.type_compatible(arg_type, expected) {
		tc.type_mismatch(.call_arg_mismatch,
			'cannot use `${arg_type.name()}` as sum constructor payload; expected variant of `${sum_name}`',
			id)
	}
}

fn (tc &TypeChecker) sum_constructor_call_name(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	callee_id := tc.a.child(&node, 0)
	callee := tc.a.nodes[int(callee_id)]
	match callee.kind {
		.ident {
			if resolved := tc.resolve_selective_import_type_symbol(callee.value) {
				if sum_name := tc.known_sum_constructor_name(resolved) {
					return sum_name
				}
			}
			return tc.known_sum_constructor_name(callee.value)
		}
		.selector {
			base := tc.a.child_node(callee, 0)
			if base.kind == .ident {
				mod_name := tc.resolve_import_alias(base.value) or { base.value }
				return tc.known_sum_constructor_name('${mod_name}.${callee.value}')
			}
		}
		.index, .prefix, .array_init {
			type_name := tc.type_expr_name(callee_id)
			if type_name.len > 0 {
				return tc.known_sum_constructor_name(type_name)
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) type_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			if resolved := tc.resolve_selective_import_type_symbol(node.value) {
				return resolved
			}
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base_id := tc.a.child(&node, 0)
			base_node := tc.a.nodes[int(base_id)]
			base := if base_node.kind == .ident {
				tc.resolve_import_alias(base_node.value) or { tc.type_expr_name(base_id) }
			} else {
				tc.type_expr_name(base_id)
			}
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := tc.type_expr_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := tc.type_expr_name(tc.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len == 0 {
				return ''
			}
			return '[]${node.value}'
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := tc.type_expr_name(tc.a.child(&node, 0))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

fn (tc &TypeChecker) known_sum_constructor_name(name string) ?string {
	if name in tc.sum_types {
		return name
	}
	base_name := strip_generic_args_name(name)
	if base_name in tc.sum_types {
		return name
	}
	qname := tc.qualify_name(name)
	if qname in tc.sum_types {
		return qname
	}
	qbase := strip_generic_args_name(qname)
	if qbase in tc.sum_types {
		return qname
	}
	return none
}

// should_diagnose reports whether should diagnose applies in types.
fn (tc &TypeChecker) should_diagnose(id flat.NodeId) bool {
	if int(id) < 0 || int(id) < tc.a.user_code_start {
		return false
	}
	if int(id) < tc.a.nodes.len && !tc.a.nodes[int(id)].pos.is_valid() && !tc.diagnose_unknown_calls {
		return false
	}
	if tc.diagnostic_files.len == 0 {
		return true
	}
	return tc.cur_file in tc.diagnostic_files
}

fn (tc &TypeChecker) should_diagnose_unsupported_generic(id flat.NodeId) bool {
	if tc.should_diagnose(id) {
		return true
	}
	if int(id) < 0 || int(id) < tc.a.user_code_start {
		return false
	}
	if tc.diagnostic_files.len == 0 {
		return false
	}
	return tc.diagnostic_files['generic:' + tc.cur_file]
}

// should_diagnose_unknown_call reports whether should diagnose unknown call applies in types.
fn (tc &TypeChecker) should_diagnose_unknown_call(id flat.NodeId) bool {
	return tc.diagnose_unknown_calls && tc.should_diagnose(id)
}

fn (tc &TypeChecker) ident_resolves_to_value(name string) bool {
	if _ := tc.cur_scope.lookup(name) {
		return true
	}
	return false
}

// resolve_call_info resolves resolve call info information for types.
fn (mut tc TypeChecker) resolve_call_info(id flat.NodeId, node flat.Node) ?CallInfo {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if info := tc.resolve_generic_call_info(id, fn_node) {
		return info
	}
	if fn_node.kind == .index && fn_node.children_count > 0 {
		base_id := tc.a.child(fn_node, 0)
		fn_type := tc.resolve_type(base_id)
		if fn_typ := fn_type_from_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
		if unresolved_generic_receiver_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       []Type{}
				return_type:  Type(Unknown{
					reason: 'generic callable'
				})
				params_known: false
			}
		}
	}
	if fn_node.kind == .ident {
		fn_id := tc.a.child(&node, 0)
		mut fn_type := Type(void_)
		if smart_type := tc.smartcast_type(fn_id) {
			fn_type = smart_type
		} else if typ := tc.cur_scope.lookup(fn_node.value) {
			fn_type = typ
		} else if typ := tc.file_scope.lookup(fn_node.value) {
			fn_type = typ
		}
		if fn_typ := fn_type_from_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
		if fn_type is Unknown || unresolved_generic_receiver_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       []Type{}
				return_type:  Type(Unknown{
					reason: 'generic callable'
				})
				params_known: false
			}
		}
	}
	if fn_node.kind !in [.ident, .selector] {
		fn_type := tc.resolve_type(tc.a.child(&node, 0))
		if fn_typ := fn_type_from_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
	}
	if fn_node.kind == .selector {
		base_id := tc.a.child(fn_node, 0)
		base_node := tc.a.nodes[int(base_id)]
		if base_node.kind == .ident && base_node.value == 'C' {
			c_name := 'C.${fn_node.value}'
			if c_name in tc.fn_ret_types {
				return tc.call_info(c_name, false)
			}
			return none
		}
		if base_node.kind == .ident {
			base_is_value := tc.ident_resolves_to_value(base_node.value)
			if !base_is_value {
				if resolved_mod := tc.resolve_import_alias(base_node.value) {
					mod_name := '${resolved_mod}.${fn_node.value}'
					if mod_name in tc.fn_ret_types {
						if info := tc.decode_call_info_from_type_arg(node, mod_name, false) {
							return info
						}
						return tc.call_info(mod_name, false)
					}
				}
				if base_node.value == tc.cur_module {
					mod_name := '${tc.cur_module}.${fn_node.value}'
					if mod_name in tc.fn_ret_types {
						if info := tc.decode_call_info_from_type_arg(node, mod_name, false) {
							return info
						}
						return tc.call_info(mod_name, false)
					}
				}
				qbase := tc.qualify_name(base_node.value)
				static_name := '${qbase}.${fn_node.value}'
				if static_name in tc.fn_ret_types && (qbase in tc.structs
					|| qbase in tc.enum_names || qbase in tc.sum_types
					|| qbase in tc.interface_names || qbase in tc.type_aliases) {
					// `qbase in tc.type_aliases` covers static methods on a type alias,
					// e.g. `fn SimdFloat4.new()` for `type SimdFloat4 = vec.Vec4[f32]`.
					return tc.call_info(static_name, false)
				}
				if fn_node.value == 'from_string' {
					if enum_name := tc.resolve_enum_name(base_node.value) {
						return CallInfo{
							name:         ''
							params:       tarr1(Type(string_))
							return_type:  Type(OptionType{
								base_type: Type(Enum{
									name:    enum_name
									is_flag: enum_name in tc.flag_enums
								})
							})
							params_known: true
						}
					}
				}
				if fn_node.value == 'zero' && qbase in tc.flag_enums {
					return CallInfo{
						name:         ''
						params:       []Type{}
						return_type:  Type(Enum{
							name:    qbase
							is_flag: true
						})
						params_known: true
					}
				}
				if fn_node.value == 'from' && qbase in tc.flag_enums {
					return CallInfo{
						name:         ''
						params:       tarr1(Type(int_))
						return_type:  Type(OptionType{
							base_type: Type(Enum{
								name:    qbase
								is_flag: true
							})
						})
						params_known: true
					}
				}
			}
		} else if base_node.kind == .selector {
			if method_name := tc.module_const_receiver_method_name(base_node, fn_node.value) {
				return tc.call_info(method_name, true)
			}
			inner := tc.a.child_node(base_node, 0)
			if inner.kind == .ident {
				mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
				full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
				if full_name in tc.fn_ret_types {
					return tc.call_info(full_name, false)
				}
				if fn_node.value == 'from_string' {
					if enum_name := tc.resolve_enum_name('${mod_name}.${base_node.value}') {
						return CallInfo{
							name:         ''
							params:       tarr1(Type(string_))
							return_type:  Type(OptionType{
								base_type: Type(Enum{
									name:    enum_name
									is_flag: enum_name in tc.flag_enums
								})
							})
							params_known: true
						}
					}
				}
			}
		}
		if fn_typ := tc.selector_fn_type(fn_node) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
		base_type := tc.resolve_type(base_id)
		clean := unwrap_pointer(base_type)
		if fn_node.value == 'type_name' && tc.receiver_is_sum_type(clean) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'clone' && unresolved_generic_receiver_type(clean) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  base_type
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'wait' {
			if ret_type := tc.thread_wait_return_type(base_type) {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  ret_type
					has_receiver: true
					params_known: true
				}
			}
		}
		if info := tc.pointer_builtin_method_call_info(base_type, fn_node.value) {
			return info
		}
		if clean is Channel {
			match fn_node.value {
				'close' {
					return CallInfo{
						name:         'chan.close'
						params:       tarr2(base_type, tc.parse_type('IError'))
						return_type:  Type(void_)
						has_receiver: true
						is_variadic:  true
						params_known: true
					}
				}
				'try_push' {
					return CallInfo{
						name:         'chan.try_push'
						params:       tarr2(base_type, clean.elem_type)
						return_type:  tc.parse_type('ChanState')
						has_receiver: true
						params_known: true
					}
				}
				'try_pop' {
					return CallInfo{
						name:         'chan.try_pop'
						params:       tarr2(base_type, Type(Pointer{
							base_type: clean.elem_type
						}))
						return_type:  tc.parse_type('ChanState')
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if clean is String && fn_node.value == 'hex' && tc.is_builtin_hex_receiver(base_type) {
			return CallInfo{
				name:         'string.hex'
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		if info := tc.current_receiver_param_method_call_info(base_id, fn_node.value) {
			return info
		}
		if clean is Alias {
			if _ := array_type_from_receiver(clean) {
				for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
					if checker_is_raw_collection_method_name(mname, 'array.')
						|| mname !in tc.fn_ret_types {
						continue
					}
					if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
						continue
					}
					return tc.call_info(mname, true)
				}
			}
		}
		if clean_array := array_type_from_receiver(clean) {
			array_candidates := exact_array_receiver_method_candidates(clean_array, fn_node.value,
				tc.cur_module)
			for mname in array_candidates {
				if mname in tc.fn_ret_types {
					return tc.call_info(mname, true)
				}
			}
			if mname := tc.unique_receiver_method_suffix_match(array_candidates) {
				return tc.call_info(mname, true)
			}
			if fn_node.value in ['clone', 'reverse'] {
				return CallInfo{
					name:         'array.${fn_node.value}'
					params:       tarr1(base_type)
					return_type:  clean_array
					has_receiver: true
					params_known: true
				}
			}
		}
		if clean_map := map_type_from_receiver(clean) {
			_ = clean_map
			for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
				if checker_is_raw_collection_method_name(mname, 'map.') || mname !in tc.fn_ret_types {
					continue
				}
				if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
					continue
				}
				return tc.call_info(mname, true)
			}
			match fn_node.value {
				'clone' {
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if clean_map := map_type_from_receiver(clean) {
			if fn_node.value == 'keys' {
				return CallInfo{
					name:         'map.keys'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: clean_map.key_type
					})
					has_receiver: true
					params_known: true
				}
			}
			if fn_node.value == 'values' {
				return CallInfo{
					name:         'map.values'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: clean_map.value_type
					})
					has_receiver: true
					params_known: true
				}
			}
			map_method := 'map.${fn_node.value}'
			if map_method in tc.fn_ret_types {
				if info := tc.map_builtin_call_info(base_type, clean_map, fn_node.value, map_method) {
					return info
				}
				return tc.call_info(map_method, true)
			}
		}
		if clean_array := array_like_type_for_method(clean, fn_node.value) {
			match fn_node.value {
				'first', 'last', 'pop', 'pop_left' {
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  clean_array.elem_type
						has_receiver: true
						params_known: true
					}
				}
				'contains' {
					elem_type := tc.array_contains_elem_type(base_node, clean_array)
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, elem_type)
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'join' {
					return CallInfo{
						name:         'array.join'
						params:       tarr2(base_type, Type(String{}))
						return_type:  Type(String{})
						has_receiver: true
						params_known: true
					}
				}
				'index', 'last_index' {
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, clean_array.elem_type)
						return_type:  Type(int_)
						has_receiver: true
						params_known: true
					}
				}
				'hex' {
					if tc.is_builtin_hex_receiver(base_type) {
						return CallInfo{
							name:         '[]u8.hex'
							params:       tarr1(base_type)
							return_type:  Type(string_)
							has_receiver: true
							params_known: true
						}
					}
				}
				'repeat' {
					return CallInfo{
						name:         'array.repeat_to_depth'
						params:       tarr2(base_type, Type(int_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'repeat_to_depth' {
					return CallInfo{
						name:         'array.repeat_to_depth'
						params:       tarr3(base_type, Type(int_), Type(int_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'delete' {
					return CallInfo{
						name:         ''
						params:       tarr2(Type(Pointer{
							base_type: base_type
						}), Type(int_))
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'delete_last', 'clear' {
					return CallInfo{
						name:         ''
						params:       tarr1(Type(Pointer{
							base_type: base_type
						}))
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'insert', 'prepend' {
					params := if fn_node.value == 'insert' {
						tarr3(base_type, Type(int_), clean_array.elem_type)
					} else {
						tarr2(base_type, clean_array.elem_type)
					}
					return CallInfo{
						name:         'array.${fn_node.value}'
						params:       params
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'filter' {
					// filtering a fixed array yields a dynamic array
					filter_ret := if receiver_is_fixed_array(clean) {
						Type(Array{
							elem_type: clean_array.elem_type
						})
					} else {
						base_type
					}
					return CallInfo{
						name:         'array.filter'
						params:       tarr2(base_type, Type(bool_))
						return_type:  filter_ret
						has_receiver: true
						params_known: true
					}
				}
				'map' {
					elem_type := tc.array_map_return_elem_type(node)
					return CallInfo{
						name:         'array.map'
						params:       tarr2(base_type, elem_type)
						return_type:  Type(Array{
							elem_type: elem_type
						})
						has_receiver: true
						params_known: true
					}
				}
				'any', 'all' {
					return CallInfo{
						name:         'array.${fn_node.value}'
						params:       tarr2(base_type, Type(bool_))
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'count' {
					return CallInfo{
						name:         'array.count'
						params:       tarr2(base_type, Type(bool_))
						return_type:  Type(int_)
						has_receiver: true
						params_known: true
					}
				}
				'sort_with_compare' {
					return CallInfo{
						name:         'array.sort_with_compare'
						params:       [
							Type(Pointer{
								base_type: base_type
							}),
							Type(FnType{
								params:      [
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
								]
								return_type: Type(int_)
							}),
						]
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'sorted_with_compare' {
					return CallInfo{
						name:         'array.sorted_with_compare'
						params:       [base_type,
							Type(FnType{
								params:      [
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
								]
								return_type: Type(int_)
							})]
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'sort' {
					mut params := tarr1(Type(Pointer{
						base_type: base_type
					}))
					if call_explicit_arg_count(node) > 0 {
						params << Type(bool_)
					}
					return CallInfo{
						name:         'array.sort'
						params:       params
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'sorted' {
					mut params := tarr1(base_type)
					if call_explicit_arg_count(node) > 0 {
						params << Type(bool_)
					}
					return CallInfo{
						name:         'array.sorted'
						params:       params
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if fixed_array := tc.fixed_array_type_from_receiver(clean) {
			if fn_node.value == 'clone' {
				return CallInfo{
					name:         'array.clone'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: fixed_array.elem_type
					})
					has_receiver: true
					params_known: true
				}
			}
		}
		mut array_pointers_fallback := false
		if fn_node.value == 'pointers' {
			if _ := array_type_from_receiver(clean) {
				array_pointers_fallback = true
			}
		}
		if fixed_array := tc.fixed_array_type_from_receiver(clean) {
			if fn_node.value == 'wait' {
				if info := tc.fixed_array_thread_wait_call_info(base_type, fixed_array) {
					return info
				}
			}
			if info := tc.fixed_array_dynamic_receiver_call_info(base_type, fixed_array,
				fn_node.value)
			{
				return info
			}
			if fn_node.value == 'pointers' {
				if base_type !is Pointer && !tc.expr_can_take_address(base_id) {
					tc.record_error(.call_arg_mismatch,
						'fixed array receiver for `pointers` must be addressable', id)
				}
				if info := tc.fixed_array_pointers_call_info(base_type) {
					return info
				}
			}
		}
		type_name := resolve_type_name_for_method(clean)
		if type_name.len > 0 {
			if fn_node.value == 'str' && (clean is Primitive || clean is Char || clean is Rune) {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			if info := tc.resolve_generic_struct_method(type_name, fn_node.value) {
				return info
			}
			for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
				if mname in tc.fn_ret_types {
					if array_pointers_fallback && mname == 'array.pointers' {
						continue
					}
					if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
						continue
					}
					if clean_map := map_type_from_receiver(clean) {
						if info := tc.map_builtin_call_info(base_type, clean_map, fn_node.value,
							mname)
						{
							return info
						}
					}
					return tc.call_info(mname, true)
				}
			}
			if array_pointers_fallback {
				return CallInfo{
					name:         'array.pointers'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: Type(voidptr_)
					})
					has_receiver: true
					params_known: true
				}
			}
			if fixed_array := tc.fixed_array_type_from_receiver(clean) {
				if info := tc.fixed_array_dynamic_receiver_call_info(base_type, fixed_array,
					fn_node.value)
				{
					return info
				}
				if fn_node.value == 'pointers' {
					if base_type !is Pointer && !tc.expr_can_take_address(base_id) {
						tc.record_error(.call_arg_mismatch,
							'fixed array receiver for `pointers` must be addressable', id)
					}
					if info := tc.fixed_array_pointers_call_info(base_type) {
						return info
					}
				}
			}
			if clean is Interface {
				if info := tc.interface_receiver_method_call_info(clean.name, fn_node.value) {
					return info
				}
			}
			if info := tc.embedded_method_call_info(type_name, fn_node.value) {
				return info
			}
			if fn_node.value == 'use' && clean is Struct
				&& tc.struct_has_middleware_receiver(type_name) {
				return CallInfo{
					name:         '${type_name}.use'
					params:       []Type{}
					return_type:  Type(void_)
					has_receiver: true
					params_known: false
				}
			}
		}
		if clean is SumType {
			if info := tc.resolve_generic_sum_method(clean.name, fn_node.value) {
				return info
			}
			mname := '${clean.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		if clean is Enum {
			if clean.is_flag && fn_node.value in ['has', 'all'] {
				return CallInfo{
					name:         ''
					params:       tarr2(base_type, base_type)
					return_type:  Type(bool_)
					has_receiver: true
					params_known: true
				}
			}
			if clean.is_flag && fn_node.value in ['set', 'clear', 'toggle'] {
				return CallInfo{
					name:         ''
					params:       tarr2(base_type, base_type)
					return_type:  Type(void_)
					has_receiver: true
					params_known: true
				}
			}
			if clean.is_flag && fn_node.value in ['set_all', 'clear_all'] {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(void_)
					has_receiver: true
					params_known: true
				}
			}
			if clean.is_flag && fn_node.value == 'is_empty' {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(bool_)
					has_receiver: true
					params_known: true
				}
			}
			if fn_node.value == 'str' {
				return CallInfo{
					name:         '${clean.name}.str'
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			mname := '${clean.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		if fn_node.value == 'clone' && tc.type_has_compiler_default_clone(clean) {
			// `#[derive(Clone)]` in Rust maps to `implements IClone` in the ownership
			// translation, whose `clone()` is compiler-provided. V structs are value types,
			// so `.clone()` yields a copy of the receiver: resolve it to the (unwrapped)
			// receiver type. A user-defined `clone()` method is matched earlier via
			// `receiver_method_name_candidates`, so this only supplies the default.
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  clean
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'free' && type_has_runtime_value(clean) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(void_)
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'str' {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'hex' && tc.is_builtin_hex_receiver(base_type) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		return none
	}
	if fn_node.kind == .ident {
		if fn_node.value == 'error' || fn_node.value == 'error_with_code' {
			mut params := []Type{}
			if fn_node.value == 'error_with_code' {
				params = tarr2(Type(string_), Type(int_))
			} else {
				params = tarr1(Type(string_))
			}
			return CallInfo{
				name:         fn_node.value
				params:       params
				return_type:  tc.parse_type('IError')
				params_known: true
			}
		}
		if fn_node.value == 'malloc' {
			return CallInfo{
				name:         'malloc'
				params:       tarr1(Type(ISize{}))
				return_type:  Type(Pointer{
					base_type: Type(u8_)
				})
				params_known: true
			}
		}
		if typ := tc.cur_scope.lookup(fn_node.value) {
			if fn_typ := fn_type_from_type(typ) {
				return CallInfo{
					name:         ''
					params:       fn_typ.params
					return_type:  fn_typ.return_type
					params_known: true
				}
			}
		}
		if local_name := tc.local_bare_fn_key(fn_node.value) {
			return tc.call_info(local_name, false)
		}
		if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
			if info := tc.decode_call_info_from_type_arg(node, imported_name, false) {
				return info
			}
			return tc.call_info(imported_name, false)
		}
		if fn_node.value in tc.fn_ret_types {
			return tc.call_info(fn_node.value, false)
		}
		if is_builtin_void_call_name(fn_node.value) {
			return CallInfo{
				name:         fn_node.value
				params:       []Type{}
				return_type:  Type(void_)
				params_known: false
			}
		}
	}
	return none
}

fn (tc &TypeChecker) is_builtin_hex_receiver(typ Type) bool {
	if tc.type_is_pointer_receiver(typ) {
		return false
	}
	clean := typ
	if clean is Alias {
		return tc.is_builtin_hex_receiver(clean.base_type)
	}
	if clean is Array {
		return is_byte_type(clean.elem_type)
	}
	if clean is String {
		return true
	}
	if clean is Primitive {
		return prim_c_type_from(clean.props, clean.size) in ['u8', 'i8', 'u16', 'i16', 'u32', 'int',
			'u64', 'i64']
	}
	return clean is Rune || clean is Char
}

fn (tc &TypeChecker) current_receiver_param_method_call_info(base_id flat.NodeId, method string) ?CallInfo {
	if tc.cur_fn_node_id < 0 || int(base_id) < 0 || int(base_id) >= tc.a.nodes.len {
		return none
	}
	base := tc.a.nodes[int(base_id)]
	if base.kind != .ident {
		return none
	}
	fn_node := tc.a.nodes[tc.cur_fn_node_id]
	if fn_node.kind != .fn_decl || !fn_node.value.contains('.') {
		return none
	}
	receiver_name := fn_node.value.all_before_last('.')
	if receiver_name.len == 0 {
		return none
	}
	mut param_type := Type(void_)
	mut found_param := false
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(&fn_node, i)
		if param.kind == .param && param.value == base.value {
			param_type = tc.parse_type(param.typ)
			found_param = true
			break
		}
	}
	if !found_param {
		return none
	}
	receiver_type := tc.parse_type(receiver_name)
	if !tc.receiver_compatible(param_type, receiver_type) {
		return none
	}
	method_name := '${receiver_name}.${method}'
	qualified_method_name := checker_qualified_fn_name(tc.cur_module, method_name)
	mut candidates := []string{}
	if qualified_method_name != method_name {
		candidates << qualified_method_name
	}
	candidates << method_name
	for candidate in candidates {
		if candidate in tc.fn_ret_types {
			return tc.call_info(candidate, true)
		}
	}
	return none
}

fn (tc &TypeChecker) type_is_pointer_receiver(typ Type) bool {
	if typ is Pointer {
		return true
	}
	if typ is Alias {
		return tc.type_is_pointer_receiver(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) method_can_be_called_on_receiver(receiver Type, method string, method_name string) bool {
	if method != 'hex' || !tc.type_is_pointer_receiver(receiver) {
		return true
	}
	params := tc.fn_param_types[method_name] or { return false }
	if params.len == 0 {
		return false
	}
	return tc.type_is_pointer_receiver(params[0])
}

fn (tc &TypeChecker) receiver_expr_is_pointer(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if typ := tc.cur_scope.lookup(node.value) {
			return tc.type_is_pointer_receiver(typ)
		}
	}
	if node.typ.starts_with('&') {
		return true
	}
	return tc.type_is_pointer_receiver(tc.resolve_type(id))
}

fn is_byte_type(typ Type) bool {
	if typ is Alias {
		return is_byte_type(typ.base_type)
	}
	return typ is Primitive && typ.props.has(.integer) && typ.props.has(.unsigned) && typ.size == 8
}

fn (mut tc TypeChecker) resolve_generic_call_info(id flat.NodeId, fn_node flat.Node) ?CallInfo {
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return none
	}
	base_id := tc.a.child(&fn_node, 0)
	type_args := tc.generic_call_type_arg_names(fn_node)
	if type_args.len == 0 {
		return none
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.kind == .selector && base_node.children_count > 0 {
		recv_id := tc.a.child(&base_node, 0)
		base_type := tc.resolve_type(recv_id)
		clean := unwrap_pointer(base_type)
		type_name := resolve_type_name_for_method(clean)
		if type_name.len > 0 {
			call_name := '${type_name}.${base_node.value}'
			if call_name in tc.fn_ret_types {
				if tc.explicit_generic_arg_count_mismatch(call_name, type_args, id) {
					return tc.failed_explicit_generic_call_info(call_name)
				}
				if info := tc.explicit_generic_call_info(call_name, true, type_args) {
					return info
				}
				return tc.call_info(call_name, true)
			}
		}
	}
	call_name := tc.generic_call_base_name(base_node) or {
		if type_name := tc.generic_call_base_type_name(base_node) {
			return CallInfo{
				name:         ''
				params:       []Type{}
				return_type:  tc.parse_type('${type_name}[${type_args.join(', ')}]')
				params_known: false
			}
		}
		return none
	}
	if is_veb_run_at_call_name(call_name) {
		return CallInfo{
			name:         call_name
			params:       []Type{}
			return_type:  Type(ResultType{
				base_type: Type(void_)
			})
			params_known: false
		}
	}
	if call_name !in tc.fn_ret_types {
		return none
	}
	if is_decode_call_name(call_name) {
		if type_args.len != 1 {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'generic argument count mismatch for `${call_name}`: expected 1, got ${type_args.len}',
					id)
			}
			return tc.failed_explicit_generic_call_info(call_name)
		}
		return CallInfo{
			name:          call_name
			params:        tc.fn_param_types[call_name] or { []Type{} }
			shared_params: tc.fn_shared_params[call_name] or { []bool{} }
			return_type:   Type(ResultType{
				base_type: tc.parse_type(type_args[0])
			})
			has_receiver:  false
			is_variadic:   tc.fn_variadic[call_name] or { false }
			is_c_variadic: tc.c_variadic_fns[call_name] or { false }
			params_known:  call_name in tc.fn_param_types
		}
	}
	if tc.explicit_generic_arg_count_mismatch(call_name, type_args, id) {
		return tc.failed_explicit_generic_call_info(call_name)
	}
	if info := tc.explicit_generic_call_info(call_name, false, type_args) {
		return info
	}
	return tc.call_info(call_name, false)
}

fn (mut tc TypeChecker) explicit_generic_arg_count_mismatch(name string, type_args []string, id flat.NodeId) bool {
	generic_params := tc.fn_generic_params[name] or { return false }
	if type_args.len == generic_params.len {
		return false
	}
	if tc.should_diagnose(id) {
		tc.record_error(.call_arg_mismatch,
			'generic argument count mismatch for `${name}`: expected ${generic_params.len}, got ${type_args.len}',
			id)
	}
	return true
}

fn (tc &TypeChecker) failed_explicit_generic_call_info(name string) CallInfo {
	return CallInfo{
		name:         name
		params:       []Type{}
		return_type:  unknown_type('invalid explicit generic call `${name}`')
		params_known: false
	}
}

fn (mut tc TypeChecker) explicit_generic_call_info(name string, has_receiver bool, type_args []string) ?CallInfo {
	generic_params := tc.fn_generic_params[name] or { return none }
	param_texts := tc.fn_param_type_texts[name] or { return none }
	if generic_params.len == 0 || type_args.len != generic_params.len {
		return none
	}
	mut concrete_args := []string{cap: generic_params.len}
	for i in 0 .. generic_params.len {
		concrete_args << tc.qualify_resolution_type_text(type_args[i])
	}
	mut sub_params := []Type{}
	for param_text in param_texts {
		sub_params << tc.parse_fn_signature_type(name, subst_generic_text(param_text,
			concrete_args, generic_params))
	}
	ret_text := tc.fn_ret_type_texts[name] or { '' }
	sub_ret := if ret_text.len > 0 {
		tc.parse_fn_signature_type(name,
			subst_generic_text(ret_text, concrete_args, generic_params))
	} else {
		tc.fn_ret_types[name] or { Type(void_) }
	}
	return CallInfo{
		name:                 name
		params:               sub_params
		shared_params:        tc.fn_shared_params[name] or { []bool{} }
		return_type:          sub_ret
		has_receiver:         has_receiver
		is_variadic:          tc.fn_variadic[name] or { false }
		is_c_variadic:        tc.c_variadic_fns[name] or { false }
		params_known:         true
		has_implicit_veb_ctx: tc.fn_implicit_veb_ctx[name] or { false }
	}
}

fn (tc &TypeChecker) generic_call_base_type_name(base_node flat.Node) ?string {
	if base_node.kind == .ident {
		qname := tc.qualify_name(base_node.value)
		if tc.type_symbol_known(qname) {
			return qname
		}
		if tc.type_symbol_known(base_node.value) {
			return base_node.value
		}
		if resolved := tc.resolve_selective_import_type_symbol(base_node.value) {
			return resolved
		}
	}
	if base_node.kind == .selector && base_node.children_count > 0 {
		inner := tc.a.child_node(&base_node, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}'
			if tc.type_symbol_known(full_name) {
				return full_name
			}
		}
	}
	return none
}

fn is_decode_call_name(name string) bool {
	return name in ['json.decode', 'json2.decode', 'x.json2.decode']
}

fn (tc &TypeChecker) decode_call_info_from_type_arg(node flat.Node, name string, has_receiver bool) ?CallInfo {
	if !is_decode_call_name(name) || node.children_count < 2 {
		return none
	}
	type_arg_id := tc.a.child(&node, 1)
	type_arg := tc.generic_call_type_arg_name(type_arg_id)
	if type_arg.len == 0 {
		return none
	}
	info := tc.call_info(name, has_receiver)
	params := if info.params.len > 0 { info.params[1..].clone() } else { []Type{} }
	return CallInfo{
		name:                 info.name
		params:               params
		return_type:          Type(ResultType{
			base_type: tc.parse_type(type_arg)
		})
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         info.params_known
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset:           1
	}
}

fn is_veb_run_at_call_name(name string) bool {
	return name == 'veb.run_at'
}

fn (tc &TypeChecker) generic_call_base_name(base_node flat.Node) ?string {
	if base_node.kind == .ident {
		if local_name := tc.local_bare_fn_key(base_node.value) {
			return local_name
		}
		if imported_name := tc.resolve_selective_import_symbol(base_node.value) {
			return imported_name
		}
		if base_node.value in tc.fn_ret_types {
			return base_node.value
		}
		return none
	}
	if base_node.kind == .selector && base_node.children_count > 0 {
		inner := tc.a.child_node(&base_node, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}'
			if is_veb_run_at_call_name(full_name) {
				return full_name
			}
			if full_name in tc.fn_ret_types {
				return full_name
			}
		}
	}
	return none
}

fn (tc &TypeChecker) generic_call_type_arg_names(index_node flat.Node) []string {
	if index_node.kind != .index || index_node.children_count < 2 || index_node.value == 'range' {
		return []string{}
	}
	mut args := []string{}
	for i in 1 .. index_node.children_count {
		arg := tc.generic_call_type_arg_name(tc.a.child(&index_node, i))
		if arg.len == 0 {
			return []string{}
		}
		args << arg
	}
	return args
}

fn (tc &TypeChecker) generic_call_type_arg_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := tc.generic_call_type_arg_name(tc.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len > 0 {
				return '[]${node.value}'
			}
			return ''
		}
		.map_init {
			return node.value
		}
		.struct_decl {
			return node.value
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

// call_info updates call info state for TypeChecker.
fn (tc &TypeChecker) call_info(name string, has_receiver bool) CallInfo {
	mut params := []Type{}
	mut params_known := false
	if p := tc.fn_param_types[name] {
		params = p.clone()
		params_known = true
	}
	if params.len == 1 && is_print_style_fn_name(name)
		&& print_style_param_accepts_string(params[0]) {
		params[0] = unknown_type('print argument')
	}
	return CallInfo{
		name:                 name
		params:               params
		shared_params:        tc.fn_shared_params[name] or { []bool{} }
		return_type:          tc.fn_ret_types[name] or {
			unknown_type('unknown return type for `${name}`')
		}
		has_receiver:         has_receiver
		is_variadic:          tc.fn_variadic[name] or { false }
		is_c_variadic:        tc.c_variadic_fns[name] or { false }
		params_known:         params_known
		has_implicit_veb_ctx: tc.fn_implicit_veb_ctx[name] or { false }
	}
}

fn array_type_from_receiver(t Type) ?Array {
	if t is Array {
		return t
	}
	if t is Alias {
		return array_type_from_receiver(t.base_type)
	}
	return none
}

fn (tc &TypeChecker) thread_wait_return_type(t Type) ?Type {
	clean := unwrap_pointer(t)
	if clean is Struct {
		thread_name := clean.name.trim_space()
		if thread_name == 'thread' || thread_name.ends_with('.thread') {
			return Type(void_)
		}
		if thread_name.starts_with('thread ') {
			return tc.parse_type(thread_name[7..].trim_space())
		}
	}
	return none
}

// fixed_array_lowered_methods lists the builtin array methods the transform
// actually lowers for fixed-array receivers (it copies the fixed array into a
// dynamic temp and re-dispatches). Methods outside this list stay rejected:
// in-place mutators like `sort` would silently modify the temp copy, and
// `first`/`last`/`pop` are not fixed-array methods in V.
const fixed_array_lowered_methods = ['contains', 'index', 'last_index', 'any', 'all', 'count',
	'map', 'filter', 'str', 'wait']

fn receiver_is_fixed_array(t Type) bool {
	if t is ArrayFixed {
		return true
	}
	if t is Alias {
		return receiver_is_fixed_array(t.base_type)
	}
	return false
}

// array_like_type_for_method returns the receiver's array type for a builtin
// array method call. Fixed-array receivers are widened to a dynamic array type,
// but only for the methods the transform can lower for them.
fn array_like_type_for_method(t Type, method string) ?Array {
	if t is Array {
		return t
	}
	if t is ArrayFixed {
		if method in fixed_array_lowered_methods {
			return Array{
				elem_type: t.elem_type
			}
		}
		return none
	}
	if t is Alias {
		return array_like_type_for_method(t.base_type, method)
	}
	return none
}

fn map_type_from_receiver(t Type) ?Map {
	if t is Map {
		return t
	}
	if t is Alias {
		return map_type_from_receiver(t.base_type)
	}
	return none
}

fn (tc &TypeChecker) receiver_is_sum_type(t Type) bool {
	if t is SumType {
		return true
	}
	if t is Alias {
		return tc.receiver_is_sum_type(t.base_type)
	}
	name := t.name()
	if name.len == 0 {
		return false
	}
	if name in tc.sum_types {
		return true
	}
	if !name.contains('.') {
		qname := tc.qualify_name(name)
		if qname in tc.sum_types {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fixed_array_type_from_receiver(t Type) ?ArrayFixed {
	if t is ArrayFixed {
		return t
	}
	if t is Alias {
		return tc.fixed_array_type_from_receiver(t.base_type)
	}
	name := t.name()
	if name.starts_with('fn(') || name.starts_with('fn (') {
		return none
	}
	if name.contains('[') && !name.starts_with('[') {
		bracket := name.last_index_u8(`[`)
		bracket_end := name.last_index_u8(`]`)
		if bracket >= 0 && bracket_end > bracket {
			len_text := name[bracket + 1..bracket_end].trim_space()
			if !is_fixed_array_len_text(len_text)
				&& tc.const_int_value_in_module(len_text, tc.cur_module, []string{}) == none {
				return none
			}
			return ArrayFixed{
				elem_type: tc.parse_type(name[..bracket])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			}
		}
	}
	return none
}

fn (tc &TypeChecker) fixed_array_thread_wait_call_info(base_type Type, arr ArrayFixed) ?CallInfo {
	elem := arr.elem_type
	if elem is Struct {
		name := elem.name.trim_space()
		if name == 'thread' {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(void_)
				has_receiver: true
				params_known: true
			}
		}
		if name.starts_with('thread ') {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  tc.thread_array_wait_return_type(name[7..])
				has_receiver: true
				params_known: true
			}
		}
	}
	return none
}

fn (tc &TypeChecker) thread_array_wait_return_type(payload string) Type {
	ret_name := payload.trim_space()
	if ret_name == '?' {
		return Type(OptionType{
			base_type: Type(void_)
		})
	}
	if ret_name == '!' {
		return Type(ResultType{
			base_type: Type(void_)
		})
	}
	ret_type := tc.parse_type(ret_name)
	if ret_type is OptionType {
		if ret_type.base_type is Void {
			return ret_type
		}
		return Type(OptionType{
			base_type: Type(Array{
				elem_type: ret_type.base_type
			})
		})
	}
	if ret_type is ResultType {
		if ret_type.base_type is Void {
			return ret_type
		}
		return Type(ResultType{
			base_type: Type(Array{
				elem_type: ret_type.base_type
			})
		})
	}
	return Type(Array{
		elem_type: ret_type
	})
}

fn (tc &TypeChecker) fixed_array_dynamic_receiver_call_info(base_type Type, arr ArrayFixed, method string) ?CallInfo {
	array_type := Array{
		elem_type: arr.elem_type
	}
	mut candidates := []string{}
	push_receiver_method_candidate(mut candidates,
		'${resolve_type_name_for_method(Type(array_type))}.${method}')
	append_array_receiver_method_candidates(mut candidates, array_type, method, tc.cur_module)
	for mname in candidates {
		if mname !in tc.fn_ret_types {
			continue
		}
		info := tc.call_info(mname, true)
		mut params := info.params.clone()
		if params.len > 0 {
			params[0] = base_type
		}
		return CallInfo{
			name:                 info.name
			params:               params
			return_type:          info.return_type
			has_receiver:         info.has_receiver
			is_variadic:          info.is_variadic
			is_c_variadic:        info.is_c_variadic
			params_known:         info.params_known
			has_implicit_veb_ctx: info.has_implicit_veb_ctx
		}
	}
	return none
}

fn (tc &TypeChecker) fixed_array_pointers_call_info(base_type Type) ?CallInfo {
	mname := 'array.pointers'
	return_type := tc.fn_ret_types[mname] or { return none }
	return CallInfo{
		name:         mname
		params:       tarr1(base_type)
		return_type:  return_type
		has_receiver: true
		params_known: true
	}
}

fn (tc &TypeChecker) expr_can_take_address(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return true
		}
		.index {
			if node.value == 'range' {
				return false
			}
			if node.children_count > 0 {
				base_id := tc.a.child(&node, 0)
				base_type0 := unwrap_pointer(tc.resolve_type(base_id))
				mut base_type := base_type0
				if base_type0 is Alias {
					base_type = base_type0.base_type
				}
				if base_type is Map {
					return false
				}
			}
			return node.children_count > 0 && tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) flag_enum_mutating_receiver_method(fn_node flat.Node, recv_type Type, info CallInfo) ?string {
	if !info.has_receiver || fn_node.kind != .selector
		|| fn_node.value !in ['set', 'clear', 'toggle', 'set_all', 'clear_all'] {
		return none
	}
	clean := unwrap_pointer(recv_type)
	if clean is Enum && (clean.is_flag || clean.name in tc.flag_enums) {
		return fn_node.value
	}
	return none
}

fn (tc &TypeChecker) flag_enum_receiver_is_mutable_lvalue(recv_id flat.NodeId) bool {
	if !tc.expr_can_take_address(recv_id) {
		return false
	}
	return tc.expr_root_is_mutable_lvalue(recv_id)
}

fn (tc &TypeChecker) expr_root_is_mutable_lvalue(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return tc.ident_is_mutable_lvalue(node.value)
		}
		.index, .selector, .paren {
			return node.children_count > 0 && tc.expr_root_is_mutable_lvalue(tc.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) ident_is_mutable_lvalue(name string) bool {
	if name.len == 0 {
		return false
	}
	if tc.mut_param_binding_matches_lvalue(name) {
		return true
	}
	owner := tc.cur_fn_mut_local_binding_owners[name] or { return false }
	if tc.cur_scope == unsafe { nil } {
		return false
	}
	return tc.cur_scope.nearest_binding_owned_by(name, owner)
}

fn (tc &TypeChecker) map_builtin_call_info(base_type Type, m Map, method string, mname string) ?CallInfo {
	if !checker_is_raw_collection_method_name(mname, 'map.') {
		return none
	}
	params := match method {
		'delete' {
			tarr2(base_type, m.key_type)
		}
		'clear', 'free', 'keys', 'values' {
			tarr1(base_type)
		}
		'move' {
			tarr1(base_type)
		}
		'reserve' {
			tarr2(base_type, Type(u32_))
		}
		else {
			return none
		}
	}

	return_type := match method {
		'keys' {
			Type(Array{
				elem_type: m.key_type
			})
		}
		'values' {
			Type(Array{
				elem_type: m.value_type
			})
		}
		'move' {
			Type(m)
		}
		else {
			Type(void_)
		}
	}

	return CallInfo{
		name:         mname
		params:       params
		return_type:  return_type
		has_receiver: true
		params_known: true
	}
}

fn checker_is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

// is_print_style_fn_name reports whether is print style fn name applies in types.
fn is_print_style_fn_name(name string) bool {
	mut start := 0
	mut len := name.len
	if len > 8 {
		if len < 13 || len > 16 || !has_builtin_dot_prefix(name) {
			return false
		}
		start = 'builtin.'.len
		len -= start
	}
	return is_short_print_style_fn_name(name, start, len)
}

fn has_builtin_dot_prefix(name string) bool {
	return name[0] == `b` && name[1] == `u` && name[2] == `i` && name[3] == `l` && name[4] == `t`
		&& name[5] == `i` && name[6] == `n` && name[7] == `.`
}

fn is_short_print_style_fn_name(name string, start int, len int) bool {
	return match len {
		5 {
			name[start] == `p` && name[start + 1] == `r` && name[start + 2] == `i`
				&& name[start + 3] == `n` && name[start + 4] == `t`
		}
		6 {
			name[start] == `e` && name[start + 1] == `p` && name[start + 2] == `r`
				&& name[start + 3] == `i` && name[start + 4] == `n` && name[start + 5] == `t`
		}
		7 {
			name[start] == `p` && name[start + 1] == `r` && name[start + 2] == `i`
				&& name[start + 3] == `n` && name[start + 4] == `t` && name[start + 5] == `l`
				&& name[start + 6] == `n`
		}
		8 {
			name[start] == `e` && name[start + 1] == `p` && name[start + 2] == `r`
				&& name[start + 3] == `i` && name[start + 4] == `n` && name[start + 5] == `t`
				&& name[start + 6] == `l` && name[start + 7] == `n`
		}
		else {
			false
		}
	}
}

fn is_builtin_void_call_name(name string) bool {
	if is_short_print_style_fn_name(name, 0, name.len) {
		return true
	}
	return name.len == 5 && name[0] == `p` && name[1] == `a` && name[2] == `n` && name[3] == `i`
		&& name[4] == `c`
}

// print_style_param_accepts_string updates print style param accepts string state for types.
fn print_style_param_accepts_string(typ Type) bool {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	return clean is String
}

// array_insert_prepend_many_arg_compatible reports whether an insert/prepend
// value argument is a many-element operand for the receiver array.
fn (tc &TypeChecker) array_insert_prepend_many_arg_compatible(node flat.Node, info CallInfo, param_idx int, actual Type) bool {
	many_param_idx := array_insert_prepend_many_param_idx(info.name)
	if many_param_idx < 0 || param_idx != many_param_idx {
		return false
	}
	if info.params.len == 0 {
		return false
	}
	mut receiver_type := info.params[0]
	if node.children_count > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			receiver_id := tc.a.child(fn_node, 0)
			receiver_type = tc.cached_expr_type(receiver_id) or { tc.resolve_type(receiver_id) }
		}
	}
	elem_type := array_like_elem_type(unwrap_pointer(receiver_type)) or {
		array_like_elem_type(unwrap_pointer(info.params[0])) or { return false }
	}
	mut clean := actual
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	if clean is Array {
		return tc.receiver_compatible(clean.elem_type, elem_type)
	}
	if clean is ArrayFixed {
		return tc.receiver_compatible(clean.elem_type, elem_type)
	}
	return false
}

fn call_param_is_shared(info CallInfo, param_idx int) bool {
	return param_idx >= 0 && param_idx < info.shared_params.len && info.shared_params[param_idx]
}

fn (tc &TypeChecker) expr_is_shared_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.expr_is_shared_arg(tc.a.child(&node, 0))
	}
	if node.kind == .prefix && node.value == 'shared' && node.children_count > 0 {
		return tc.expr_is_shared_arg(tc.a.child(&node, 0))
	}
	if node.kind == .selector && node.children_count > 0 {
		return tc.selector_is_shared_arg(node)
	}
	if node.kind != .ident || node.value.len == 0 {
		return false
	}
	owner := tc.cur_fn_shared_binding_owners[node.value] or { return false }
	if tc.cur_scope == unsafe { nil } {
		return false
	}
	return tc.cur_scope.nearest_binding_owned_by(node.value, owner)
}

fn (tc &TypeChecker) expr_is_explicit_shared_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.expr_is_explicit_shared_arg(tc.a.child(&node, 0))
	}
	return node.kind == .prefix && node.value == 'shared' && node.children_count > 0
}

fn (tc &TypeChecker) selector_is_shared_arg(node flat.Node) bool {
	if node.children_count == 0 || node.value.len == 0 {
		return false
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.smartcast_type(base_id) or {
		tc.cached_expr_type(base_id) or { tc.resolve_type(base_id) }
	}
	clean := unalias_and_unwrap_pointer_type(base_type)
	if clean is Struct {
		return tc.struct_field_is_shared(clean.name, node.value)
	}
	return false
}

fn (tc &TypeChecker) struct_field_is_shared(struct_name string, field_name string) bool {
	if struct_name.len == 0 || field_name.len == 0 {
		return false
	}
	mut candidates := []string{cap: 4}
	candidates << struct_name
	base, _, is_generic := generic_type_application_parts(struct_name)
	if is_generic {
		candidates << base
	}
	if struct_name.contains('.') {
		candidates << struct_name.all_after_last('.')
	} else {
		qname := tc.qualify_name(struct_name)
		if qname != struct_name {
			candidates << qname
		}
	}
	for candidate in candidates {
		if tc.struct_shared_fields[struct_field_c_abi_key(candidate, field_name)] {
			return true
		}
	}
	return false
}

// check_call_arg_types validates check call arg types state for types.
fn (mut tc TypeChecker) check_call_arg_types(id flat.NodeId, node flat.Node, info0 CallInfo) {
	info := tc.specialized_plain_generic_call_info(node, info0)
	if node.children_count == 0 {
		return
	}
	if is_map_keys_values_call_name(info.name) {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		arg_count := node.children_count - 1
		if arg_count != 0 && tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected 0, got ${arg_count}',
				id)
		}
		return
	}
	if !info.params_known {
		dsl_name := tc.unresolved_array_dsl_call_name(node)
		if dsl_name.len > 0 {
			tc.push_array_dsl_scope(node, dsl_name)
		}
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		if dsl_name.len > 0 {
			tc.pop_scope()
		}
		return
	}
	// `@[params]` struct args: trailing `key: value` args collapse into one struct argument.
	// field_init args only appear for this syntax, so they are a reliable signal.
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	actual_count := node.children_count - 1 - info.arg_offset - field_init_args + collapsed +
		recv_extra
	// A hidden veb `Context` parameter may be supplied implicitly from the
	// enclosing handler instead of by the caller, so accept argument counts both
	// with the ctx (route dispatch) and without it (handler delegation).
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	if field_init_args > 0 && tc.should_diagnose(id) {
		// Trailing `key: value` args collapse into one struct argument; reject
		// them against a parameter that cannot take a struct literal (e.g. a
		// non-variadic `[]Point`), which cgen would otherwise zero-initialize.
		mut first_field := -1
		for i in 1 + info.arg_offset .. node.children_count {
			if tc.a.child_node(&node, i).kind == .field_init {
				first_field = i
				break
			}
		}
		if first_field >= 0 {
			arg_shift := if ctx_omitted { ctx_count } else { 0 }
			param_idx := first_field - 1 - info.arg_offset + recv_extra + arg_shift
			if param_idx >= 0 && param_idx < info.params.len {
				is_variadic_slot := info.is_variadic && param_idx == info.params.len - 1
				mut target := info.params[param_idx]
				if is_variadic_slot {
					if target is Array {
						target = target.elem_type
					}
				}
				clean_target := if target is Alias { target.base_type } else { target }
				if clean_target is Array || clean_target is ArrayFixed || clean_target is Map
					|| clean_target is String || clean_target is Primitive {
					tc.record_error(.call_arg_mismatch,
						'cannot use `key: value` arguments as `${info.params[param_idx].name()}` in call to `${tc.call_display_name(node)}`',
						id)
				}
			}
		}
	}
	min_count := tc.min_required_arg_count(info) - ctx_count
	if actual_count < min_count || (!info.is_variadic && actual_count > info.params.len) {
		if tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected ${info.params.len}, got ${actual_count}',
				id)
		}
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	if info.has_receiver && info.params.len > 0 {
		mut fn_node := tc.a.child_node(&node, 0)
		// For an explicit generic method call `recv.method[T](...)`, the call's fn is an
		// `.index` node (`recv.method` indexed by the type args), so its child 0 is the
		// `recv.method` selector — a method value — not the receiver. Descend through the
		// index to the underlying selector so the receiver resolves to `recv`.
		if fn_node.kind == .index && fn_node.children_count > 0 {
			fn_node = tc.a.child_node(fn_node, 0)
		}
		recv_id := tc.a.child(fn_node, 0)
		tc.check_node(recv_id)
		recv_type := tc.smartcast_type(recv_id) or {
			tc.cached_expr_type(recv_id) or { tc.resolve_type(recv_id) }
		}
		if call_param_is_shared(info, 0) && !tc.expr_is_shared_arg(recv_id) {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'cannot use non-shared `${recv_type.name()}` as receiver for `${tc.call_display_name(node)}`; expected `shared ${info.params[0].name()}`',
					id)
			}
		}
		if !tc.method_receiver_compatible(recv_type, info.params[0], info.name)
			&& !tc.receiver_embeds(recv_type, info.params[0]) {
			tc.type_mismatch(.call_arg_mismatch,
				'cannot use receiver `${recv_type.name()}` as `${info.params[0].name()}`', id)
		}
		if method := tc.flag_enum_mutating_receiver_method(fn_node, recv_type, info) {
			if !tc.flag_enum_receiver_is_mutable_lvalue(recv_id) && tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'flag enum method `${method}` requires a mutable receiver', id)
			}
		}
	}
	for i in 1 + info.arg_offset .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		// field_init args are fields of the collapsed `@[params]` struct, not positional params
		if tc.a.child_node(&node, i).kind == .field_init {
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(arg_id)
			} $else {
				tc.check_node(arg_id)
			}
			continue
		}
		// When the caller omitted the implicit veb `Context` parameter, skip it
		// (it is inserted right after the receiver) while mapping the caller's
		// positional arguments to the callee's params.
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) + arg_shift
		has_dsl_scope := tc.call_arg_needs_array_dsl_scope(info.name, param_idx)
		if has_dsl_scope {
			tc.push_array_dsl_scope(node, info.name)
		}
		$if ownership ? {
			tc.ownership_check_node_with_aggregate_consumption_mode(arg_id, tc.ownership_should_defer_call_arg_aggregate_consumption(node,
				info, i))
		} $else {
			tc.check_node(arg_id)
		}
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		if param_idx >= info.params.len {
			if info.is_variadic && info.params.len > 0 {
				variadic_raw := info.params[info.params.len - 1]
				if variadic_raw is Array {
					elem_type := array_elem_type(variadic_raw)
					if variadic_elem_accepts_any(elem_type) && tc.variadic_any_arg_is_scalar(arg_id) {
						if has_dsl_scope {
							tc.pop_scope()
						}
						continue
					}
					actual := tc.resolve_expr(arg_id, elem_type)
					if variadic_elem_accepts_any(elem_type) && !variadic_any_arg_has_value(actual) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${elem_type.name()}`',
							id)
					} else if !tc.receiver_compatible(actual, elem_type)
						&& !tc.type_compatible(actual, elem_type) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${elem_type.name()}`',
							id)
					}
				} else {
					actual := tc.resolve_expr(arg_id, variadic_raw)
					if !tc.receiver_compatible(actual, variadic_raw)
						&& !tc.type_compatible(actual, variadic_raw) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${variadic_raw.name()}`',
							id)
					}
				}
			}
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		mut expected := info.params[param_idx]
		if tc.is_zero_literal(arg_id) && is_fn_pointer_type(expected) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		expected_raw := expected
		if info.is_variadic && param_idx == info.params.len - 1 && expected_raw is Array {
			elem_type := array_elem_type(expected_raw)
			if variadic_elem_accepts_any(elem_type) && tc.variadic_any_arg_is_scalar(arg_id) {
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			actual := tc.resolve_expr(arg_id, elem_type)
			actual_name := actual.name()
			expected_name := elem_type.name()
			actual_raw := actual
			if variadic_elem_accepts_any(elem_type) && !variadic_any_arg_has_value(actual) {
				tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual_name}` as argument ${
					param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected_name}`',
					id)
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			if actual is Array {
				if !tc.receiver_compatible(actual_raw, elem_type)
					&& !tc.receiver_compatible(actual_raw, expected)
					&& !tc.type_compatible(actual_raw, elem_type)
					&& !tc.type_compatible(actual_raw, expected) {
					tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual_name}` as argument ${
						param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected_name}`',
						id)
				}
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			expected = elem_type
		}
		mut actual := Type(void_)
		if has_dsl_scope {
			actual = tc.resolve_expr(arg_id, expected)
			tc.pop_scope()
		} else {
			actual = tc.resolve_expr(arg_id, expected)
		}
		param_is_shared := call_param_is_shared(info, param_idx)
		if param_is_shared && !tc.expr_is_shared_arg(arg_id) {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch, 'cannot use non-shared `${actual.name()}` as argument ${
					param_idx + 1} to `${tc.call_display_name(node)}`; expected `shared ${expected.name()}`',
					id)
			}
			continue
		}
		if !param_is_shared && tc.expr_is_explicit_shared_arg(arg_id) {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch, 'cannot use explicit shared argument `${actual.name()}` as argument ${
					param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
					id)
			}
			continue
		}
		if info.name == 'chan.try_pop' && param_idx == 1
			&& !tc.chan_try_pop_destination_is_valid(arg_id, actual) {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'channel try_pop destination must be a mutable lvalue or pointer', id)
			}
			continue
		}
		if info.name.starts_with('C.') && fn_param_unalias_type(expected).is_integer()
			&& tc.c_literal_arg(arg_id) && !tc.c_scalar_byte_literal_arg(arg_id) {
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
			continue
		}
		if !tc.expr_receiver_compatible(arg_id, actual, expected)
			&& !tc.expr_compatible(arg_id, actual, expected) {
			if tc.c_call_arg_compatible(info.name, arg_id, expected) {
				continue
			}
			if expected is Pointer && tc.expr_tail_is_nil(arg_id) {
				continue
			}
			if json_encode_accepts_arg(info.name, param_idx, expected, actual) {
				continue
			}
			if free_array_arg_compatible(info.name, param_idx, expected, actual) {
				continue
			}
			if voidptr_arg_compatible(expected, actual) {
				continue
			}
			if tc.array_insert_prepend_many_arg_compatible(node, info, param_idx, actual) {
				continue
			}
			if tc.array_dsl_fn_arg_compatible(node, info, param_idx, actual) {
				continue
			}
			if tc.is_os_args_contains_call(node) && param_idx == 1 && actual is String {
				continue
			}
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
		}
	}
}

fn (tc &TypeChecker) unresolved_array_dsl_call_name(node flat.Node) string {
	if node.children_count == 0 {
		return ''
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return ''
	}
	name := 'array.${callee.value}'
	return if is_array_dsl_call_name(name) { name } else { '' }
}

fn (tc &TypeChecker) chan_try_pop_destination_is_valid(arg_id flat.NodeId, actual Type) bool {
	if int(arg_id) < 0 || int(arg_id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(arg_id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.chan_try_pop_destination_is_valid(tc.a.child(&node, 0), actual)
	}
	if node.kind == .prefix && node.op == .amp {
		if node.children_count == 0 {
			return false
		}
		child_id := tc.a.child(&node, 0)
		return tc.expr_can_take_address(child_id) && tc.expr_root_is_mutable_lvalue(child_id)
	}
	if actual is Pointer {
		return true
	}
	return tc.expr_can_take_address(arg_id) && tc.expr_root_is_mutable_lvalue(arg_id)
}

fn json_encode_accepts_arg(name string, param_idx int, expected Type, actual Type) bool {
	if param_idx != 0 || name !in ['json.encode', 'json.encode_pretty'] {
		return false
	}
	if expected is Pointer {
		if expected.base_type is Void {
			return type_has_runtime_value(actual)
		}
	}
	return false
}

fn free_array_arg_compatible(name string, param_idx int, expected Type, actual Type) bool {
	if param_idx != 0 || name !in ['free', 'builtin.free'] || !fn_param_is_voidptr_type(expected) {
		return false
	}
	mut clean := unwrap_pointer(actual)
	if clean is Alias {
		clean = clean.base_type
	}
	return clean is Array
}

fn (tc &TypeChecker) c_call_arg_compatible(name string, arg_id flat.NodeId, expected Type) bool {
	if !name.starts_with('C.') {
		return false
	}
	clean := fn_param_unalias_type(expected)
	if clean.is_integer() {
		return tc.c_scalar_byte_literal_arg(arg_id)
	}
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		if base is Char || (base is Primitive && base.name() == 'u8') {
			return tc.c_literal_arg(arg_id)
		}
	}
	return false
}

fn (tc &TypeChecker) c_scalar_byte_literal_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .char_literal {
		return c_literal_is_single_byte(node.value)
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.c_scalar_byte_literal_arg(tc.a.child(&node, 0))
	}
	return false
}

fn c_literal_is_single_byte(value string) bool {
	if !value.starts_with('c:') {
		return false
	}
	literal := value[2..]
	if literal.len == 1 {
		return true
	}
	if literal.len < 2 || literal[0] != `\\` {
		return false
	}
	if literal.len == 2 {
		return true
	}
	if literal[1] == `x` {
		mut decoded := 0
		for ch in literal[2..].bytes() {
			digit := if ch >= `0` && ch <= `9` {
				int(ch - `0`)
			} else if ch >= `a` && ch <= `f` {
				int(ch - `a`) + 10
			} else if ch >= `A` && ch <= `F` {
				int(ch - `A`) + 10
			} else {
				return false
			}
			decoded = decoded * 16 + digit
			if decoded > 0xff {
				return false
			}
		}
		return true
	}
	if literal[1] < `0` || literal[1] > `7` || literal.len > 4 {
		return false
	}
	mut decoded := 0
	for digit in literal[1..].bytes() {
		if digit < `0` || digit > `7` {
			return false
		}
		decoded = decoded * 8 + int(digit - `0`)
	}
	return decoded <= 0xff
}

fn (tc &TypeChecker) c_literal_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .char_literal {
		return node.value.starts_with('c:')
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.c_literal_arg(tc.a.child(&node, 0))
	}
	return false
}

fn voidptr_arg_compatible(expected Type, actual Type) bool {
	if !fn_param_is_voidptr_type(expected) {
		return false
	}
	return voidptr_arg_type_passes_direct(actual)
}

fn voidptr_arg_type_passes_direct(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	return clean is Pointer || clean is Nil
}

fn variadic_elem_accepts_any(typ Type) bool {
	if typ is Pointer {
		return typ.base_type is Void
	}
	return false
}

fn variadic_any_arg_has_value(typ Type) bool {
	return type_has_runtime_value(typ)
}

fn type_has_runtime_value(typ Type) bool {
	if typ is OptionType {
		return typ.base_type !is Void
	}
	if typ is ResultType {
		return typ.base_type !is Void
	}
	if typ is MultiReturn {
		return false
	}
	return typ !is Void && typ !is None && typ !is Unknown && !type_contains_unknown(typ)
}

fn (tc &TypeChecker) arg_is_spread(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .prefix && (node.value == '...' || node.op == .none) && node.children_count > 0 {
		return true
	}
	return false
}

fn (tc &TypeChecker) variadic_any_arg_is_scalar(id flat.NodeId) bool {
	if tc.arg_is_spread(id) {
		return false
	}
	if !tc.valid_node_id(id) {
		return false
	}
	if tc.a.nodes[int(id)].kind == .enum_val {
		return false
	}
	actual := tc.resolve_type(id)
	if !variadic_any_arg_has_value(actual) {
		return false
	}
	if _ := array_type_from_receiver(actual) {
		return false
	}
	return true
}

fn (mut tc TypeChecker) specialized_plain_generic_call_info(node flat.Node, info CallInfo) CallInfo {
	generic_params := tc.fn_generic_params[info.name] or { return info }
	param_texts := tc.fn_param_type_texts[info.name] or { return info }
	if generic_params.len == 0 || node.children_count <= 1 {
		return info
	}
	mut inferred := map[string]string{}
	mut first_param_idx := 0
	if info.has_receiver && param_texts.len > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			recv_id := tc.a.child(fn_node, 0)
			actual := tc.resolve_type(recv_id)
			tc.infer_generic_type_text_from_type(param_texts[0], actual, generic_params, mut
				inferred)
			first_param_idx = 1
		}
	}
	for param_idx in first_param_idx .. param_texts.len {
		arg_idx := param_idx - first_param_idx + 1 + info.arg_offset
		if arg_idx >= node.children_count {
			break
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, arg_idx))
		actual := tc.resolve_type(arg_id)
		tc.infer_generic_type_text_from_type(param_texts[param_idx], actual, generic_params, mut
			inferred)
	}
	mut concrete_args := []string{cap: generic_params.len}
	for param in generic_params {
		arg := inferred[param] or { return info }
		concrete_args << arg
	}
	mut sub_params := []Type{}
	for param_text in param_texts {
		sub_params << tc.parse_fn_signature_type(info.name, subst_generic_text(param_text,
			concrete_args, generic_params))
	}
	ret_text := tc.fn_ret_type_texts[info.name] or { '' }
	sub_ret := if ret_text.len > 0 {
		tc.parse_fn_signature_type(info.name, subst_generic_text(ret_text, concrete_args,
			generic_params))
	} else {
		info.return_type
	}
	return CallInfo{
		name:                 info.name
		params:               sub_params
		shared_params:        info.shared_params.clone()
		return_type:          sub_ret
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         true
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset:           info.arg_offset
	}
}

fn (tc &TypeChecker) parse_fn_signature_type(name string, typ string) Type {
	decl_file := tc.fn_type_files[name] or { return tc.parse_type(typ) }
	mut scoped := *tc
	scoped.cur_file = decl_file
	scoped.cur_module = tc.fn_type_modules[name] or {
		tc.file_modules[decl_file] or { tc.cur_module }
	}
	// Fully qualify symbols owned by the declaration module before parsing the
	// substituted signature. A bare concrete type can belong to the generic
	// call site (notably a type from `main`), so resolution deliberately leaves
	// that spelling bare and the neutral parse context preserves its authority.
	resolved_typ := scoped.qualify_resolution_type_text(typ)
	scoped.cur_module = ''
	scoped.type_cache = &TypeCache{
		parse_enabled:              if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries:              map[string]Type{}
		c_entries:                  map[string]string{}
		struct_field_entries:       map[string]Type{}
		struct_field_misses:        map[string]bool{}
		ierror_compat_entries:      map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
	return scoped.parse_type(resolved_typ)
}

fn (mut tc TypeChecker) infer_generic_type_text_from_type(param_text string, actual Type, generic_params []string, mut inferred map[string]string) {
	clean := param_text.trim_space()
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		if actual is Pointer {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		} else {
			tc.infer_generic_type_text_from_type(clean[1..], actual, generic_params, mut inferred)
		}
		return
	}
	if clean.starts_with('mut ') {
		tc.infer_generic_type_text_from_type(clean[4..], actual, generic_params, mut inferred)
		return
	}
	if clean.starts_with('...') {
		if actual is Array {
			tc.infer_generic_type_text_from_type(clean[3..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('[]') {
		if actual is Array {
			tc.infer_generic_type_text_from_type(clean[2..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('?') {
		if actual is OptionType {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') {
		if actual is ResultType {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if actual is FnType {
			tc.infer_generic_fn_type_text_from_type(clean, actual, generic_params, mut inferred)
		}
		return
	}
	if generic_type_application(clean) {
		actual_text := tc.generic_infer_type_text(actual)
		tc.infer_generic_type_text_from_text(clean, actual_text, generic_params, mut inferred)
		return
	}
	for param in generic_params {
		if clean == param && param !in inferred {
			mut actual_text := tc.generic_infer_type_text(actual)
			if actual_text == 'unknown' || actual_text == 'generic' {
				actual_text = param
			}
			inferred[param] = actual_text
			return
		}
	}
}

fn (tc &TypeChecker) generic_infer_type_text(actual Type) string {
	if actual is Unknown {
		if name := generic_placeholder_from_unknown(actual) {
			return name
		}
	}
	return actual.name()
}

fn (mut tc TypeChecker) infer_generic_type_text_from_text(param_text string, actual_text string, generic_params []string, mut inferred map[string]string) {
	clean := param_text.trim_space()
	actual := actual_text.trim_space()
	if clean.len == 0 || actual.len == 0 {
		return
	}
	for param in generic_params {
		if clean == param && param !in inferred {
			inferred[param] = if actual == 'unknown' || actual == 'generic' { param } else { actual }
			return
		}
	}
	if clean.starts_with('&') || actual.starts_with('&') {
		if clean.starts_with('&') && actual.starts_with('&') {
			tc.infer_generic_type_text_from_text(clean[1..], actual[1..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('mut ') {
		tc.infer_generic_type_text_from_text(clean[4..], actual.trim_left('&'), generic_params, mut
			inferred)
		return
	}
	if clean.starts_with('...') || actual.starts_with('...') {
		if clean.starts_with('...') && actual.starts_with('...') {
			tc.infer_generic_type_text_from_text(clean[3..], actual[3..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('[]') || actual.starts_with('[]') {
		if clean.starts_with('[]') && actual.starts_with('[]') {
			tc.infer_generic_type_text_from_text(clean[2..], actual[2..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('?') || actual.starts_with('?') {
		if clean.starts_with('?') && actual.starts_with('?') {
			tc.infer_generic_type_text_from_text(clean[1..], actual[1..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') || actual.starts_with('!') {
		if clean.starts_with('!') && actual.starts_with('!') {
			tc.infer_generic_type_text_from_text(clean[1..], actual[1..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('map[') || actual.starts_with('map[') {
		if !clean.starts_with('map[') || !actual.starts_with('map[') {
			return
		}
		clean_end := find_matching_bracket(clean, 3)
		actual_end := find_matching_bracket(actual, 3)
		if clean_end >= clean.len || actual_end >= actual.len {
			return
		}
		tc.infer_generic_type_text_from_text(clean[4..clean_end], actual[4..actual_end],
			generic_params, mut inferred)
		tc.infer_generic_type_text_from_text(clean[clean_end + 1..], actual[actual_end + 1..],
			generic_params, mut inferred)
		return
	}
	if clean.starts_with('[') || actual.starts_with('[') {
		if !clean.starts_with('[') || !actual.starts_with('[') {
			return
		}
		clean_end := find_matching_bracket(clean, 0)
		actual_end := find_matching_bracket(actual, 0)
		if clean_end >= clean.len || actual_end >= actual.len
			|| clean[..clean_end + 1] != actual[..actual_end + 1] {
			return
		}
		tc.infer_generic_type_text_from_text(clean[clean_end + 1..], actual[actual_end + 1..],
			generic_params, mut inferred)
		return
	}
	param_base, param_args, param_is_generic := generic_type_application_parts(clean)
	actual_base, actual_args, actual_is_generic := generic_type_application_parts(actual)
	if param_is_generic || actual_is_generic {
		if !param_is_generic || !actual_is_generic || param_args.len != actual_args.len
			|| !tc.generic_type_base_matches(param_base, actual_base) {
			return
		}
		for i in 0 .. param_args.len {
			tc.infer_generic_type_text_from_text(param_args[i], actual_args[i], generic_params, mut
				inferred)
		}
	}
}

fn (mut tc TypeChecker) infer_generic_fn_type_text_from_type(param_text string, actual FnType, generic_params []string, mut inferred map[string]string) {
	params_start := param_text.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < param_text.len {
		if param_text[params_end] == `(` {
			depth++
		} else if param_text[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= param_text.len {
		return
	}
	parts := split_params(param_text[params_start..params_end])
	for i, part in parts {
		if i >= actual.params.len {
			break
		}
		tc.infer_generic_type_text_from_type(normalize_fn_type_param_text(part), fn_param_type(actual,
			i), generic_params, mut inferred)
	}
	ret := param_text[params_end + 1..].trim_space()
	if ret.len > 0 {
		tc.infer_generic_type_text_from_type(ret, actual.return_type, generic_params, mut inferred)
	}
}

// array_map_return_elem_type supports array map return elem type handling for TypeChecker.
fn (mut tc TypeChecker) array_map_return_elem_type(node flat.Node) Type {
	if node.children_count < 2 {
		return Type(void_)
	}
	tc.push_array_dsl_scope(node, 'array.map')
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	tc.check_node(arg_id)
	elem_type := tc.resolve_type(arg_id)
	tc.pop_scope()
	if fn_typ := fn_type_from_type(elem_type) {
		return fn_typ.return_type
	}
	if elem_type is Void || elem_type is Unknown {
		return Type(void_)
	}
	return elem_type
}

fn (tc &TypeChecker) array_contains_elem_type(base_node flat.Node, array_type Array) Type {
	if base_node.kind == .selector && base_node.value == 'args' && base_node.children_count > 0 {
		parent := tc.a.child_node(&base_node, 0)
		if parent.kind == .ident && parent.value == 'os' {
			return Type(String{})
		}
	}
	return array_type.elem_type
}

fn (tc &TypeChecker) is_os_args_contains_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'contains' || fn_node.children_count == 0 {
		return false
	}
	base_node := tc.a.child_node(fn_node, 0)
	if base_node.kind != .selector || base_node.value != 'args' || base_node.children_count == 0 {
		return false
	}
	parent := tc.a.child_node(base_node, 0)
	return parent.kind == .ident && parent.value == 'os'
}

fn (tc &TypeChecker) pointer_builtin_method_call_info(base_type Type, method string) ?CallInfo {
	receiver := pointer_builtin_receiver_name(base_type)
	if receiver.len == 0 {
		return none
	}
	if receiver in ['charptr', 'byteptr'] && method in ['vstring', 'vstring_with_len'] {
		mut params := tarr1(base_type)
		if method == 'vstring_with_len' {
			params << Type(int_)
		}
		return CallInfo{
			name:         '${receiver}.${method}'
			params:       params
			return_type:  Type(string_)
			has_receiver: true
			params_known: true
		}
	}
	if receiver in ['byteptr', 'voidptr'] && method == 'vbytes' {
		return CallInfo{
			name:         '${receiver}.${method}'
			params:       [base_type, Type(int_)]
			return_type:  Type(Array{
				elem_type: Type(u8_)
			})
			has_receiver: true
			params_known: true
		}
	}
	return none
}

fn pointer_builtin_receiver_name(typ Type) string {
	if typ is Alias {
		if typ.name in ['charptr', 'byteptr', 'voidptr'] {
			return typ.name
		}
		return pointer_builtin_receiver_name(typ.base_type)
	}
	if typ is Pointer {
		base := typ.base_type
		if base is Alias {
			if base.name == 'byte' {
				return 'byteptr'
			}
			return pointer_builtin_receiver_name(base)
		}
		if base is Char {
			return 'charptr'
		}
		if base is Void {
			return 'voidptr'
		}
		if base is Primitive && prim_name(base) == 'u8' {
			return 'byteptr'
		}
	}
	if typ is Primitive && prim_name(typ) == 'u8' {
		return 'byteptr'
	}
	return ''
}

// min_required_arg_count supports min required arg count handling for TypeChecker.
fn (tc &TypeChecker) min_required_arg_count(info CallInfo) int {
	if info.is_variadic && info.params.len > 0 {
		return info.params.len - 1
	}
	mut n := info.params.len
	for n > 0 {
		param := info.params[n - 1]
		if tc.is_params_struct_type(param) {
			n--
			continue
		}
		break
	}
	return n
}

fn c_variadic_fixed_param_count(info CallInfo) int {
	if info.is_c_variadic && info.params.len > 0 && info.params[info.params.len - 1] is Array {
		return info.params.len - 1
	}
	return info.params.len
}

fn (tc &TypeChecker) is_params_struct_type(typ Type) bool {
	if typ is Struct {
		if typ.name in tc.params_structs {
			return true
		}
		qname := tc.qualify_name(typ.name)
		return qname in tc.params_structs
	}
	if typ is Alias {
		return tc.is_params_struct_type(typ.base_type)
	}
	return false
}

// call_arg_needs_array_dsl_scope updates call arg needs array dsl scope state for TypeChecker.
fn (tc &TypeChecker) call_arg_needs_array_dsl_scope(name string, param_idx int) bool {
	return param_idx == 1 && is_array_dsl_call_name(name)
}

fn (tc &TypeChecker) array_dsl_fn_arg_compatible(node flat.Node, info CallInfo, param_idx int, actual Type) bool {
	if param_idx != 1 || !is_array_filter_or_map_call_name(info.name) {
		return false
	}
	fn_typ := fn_type_from_type(actual) or { return false }
	if fn_typ.params.len != 1 {
		return false
	}
	arr := tc.call_receiver_array_type(node) or { return false }
	param := fn_param_type(fn_typ, 0)
	if !tc.receiver_compatible(param, arr.elem_type)
		&& !tc.receiver_compatible(arr.elem_type, param) {
		return false
	}
	if is_array_filter_call_name(info.name) {
		return tc.type_compatible(fn_typ.return_type, Type(bool_))
	}
	return true
}

// is_array_dsl_call_name reports whether is array dsl call name applies in types.
fn is_array_dsl_call_name(name string) bool {
	if name.len < 9 || name.len > 12 || !has_array_dot_prefix(name) {
		return false
	}
	start := 'array.'.len
	len := name.len - start
	return match len {
		3 {
			(name[start] == `a` && ((name[start + 1] == `n` && name[start + 2] == `y`)
				|| (name[start + 1] == `l` && name[start + 2] == `l`)))
				|| (name[start] == `m` && name[start + 1] == `a` && name[start + 2] == `p`)
		}
		4 {
			name[start] == `s` && name[start + 1] == `o` && name[start + 2] == `r`
				&& name[start + 3] == `t`
		}
		5 {
			name[start] == `c` && name[start + 1] == `o` && name[start + 2] == `u`
				&& name[start + 3] == `n` && name[start + 4] == `t`
		}
		6 {
			(name[start] == `f` && name[start + 1] == `i` && name[start + 2] == `l`
				&& name[start + 3] == `t` && name[start + 4] == `e` && name[start + 5] == `r`)
				|| (name[start] == `s` && name[start + 1] == `o` && name[start + 2] == `r`
				&& name[start + 3] == `t` && name[start + 4] == `e` && name[start + 5] == `d`)
		}
		else {
			false
		}
	}
}

fn has_array_dot_prefix(name string) bool {
	return name[0] == `a` && name[1] == `r` && name[2] == `r` && name[3] == `a` && name[4] == `y`
		&& name[5] == `.`
}

fn is_array_filter_or_map_call_name(name string) bool {
	if name.len != 9 && name.len != 12 {
		return false
	}
	if !has_array_dot_prefix(name) {
		return false
	}
	start := 'array.'.len
	if name.len == 9 {
		return name[start] == `m` && name[start + 1] == `a` && name[start + 2] == `p`
	}
	return is_array_filter_method_name(name, start)
}

fn is_array_filter_call_name(name string) bool {
	return name.len == 12 && has_array_dot_prefix(name)
		&& is_array_filter_method_name(name, 'array.'.len)
}

fn is_array_filter_method_name(name string, start int) bool {
	return name[start] == `f` && name[start + 1] == `i` && name[start + 2] == `l`
		&& name[start + 3] == `t` && name[start + 4] == `e` && name[start + 5] == `r`
}

fn array_insert_prepend_many_param_idx(name string) int {
	if name.len != 12 && name.len != 13 {
		return -1
	}
	if !has_array_dot_prefix(name) {
		return -1
	}
	start := 'array.'.len
	if name.len == 12 {
		if name[start] == `i` && name[start + 1] == `n` && name[start + 2] == `s`
			&& name[start + 3] == `e` && name[start + 4] == `r` && name[start + 5] == `t` {
			return 2
		}
		return -1
	}
	if name[start] == `p` && name[start + 1] == `r` && name[start + 2] == `e`
		&& name[start + 3] == `p` && name[start + 4] == `e` && name[start + 5] == `n`
		&& name[start + 6] == `d` {
		return 1
	}
	return -1
}

fn is_map_keys_values_call_name(name string) bool {
	if name.len != 8 && name.len != 10 {
		return false
	}
	if name[0] != `m` || name[1] != `a` || name[2] != `p` || name[3] != `.` {
		return false
	}
	if name.len == 8 {
		return name[4] == `k` && name[5] == `e` && name[6] == `y` && name[7] == `s`
	}
	return name[4] == `v` && name[5] == `a` && name[6] == `l` && name[7] == `u` && name[8] == `e`
		&& name[9] == `s`
}

// call_explicit_arg_count updates call explicit arg count state for types.
fn call_explicit_arg_count(node flat.Node) int {
	if node.children_count <= 1 {
		return 0
	}
	mut n := 0
	for i in 1 .. node.children_count {
		if int(node.children_start) + i < 0 {
			continue
		}
		n++
	}
	return n
}

// push_array_dsl_scope updates push array dsl scope state for TypeChecker.
fn (mut tc TypeChecker) push_array_dsl_scope(node flat.Node, name string) {
	tc.push_scope()
	arr := tc.call_receiver_array_type(node) or { return }
	if is_array_sort_dsl_call_name(name) {
		tc.cur_scope.insert('a', arr.elem_type)
		tc.cur_scope.insert('b', arr.elem_type)
		return
	}
	tc.cur_scope.insert('it', arr.elem_type)
}

fn is_array_sort_dsl_call_name(name string) bool {
	if name.len != 10 && name.len != 12 {
		return false
	}
	if !has_array_dot_prefix(name) {
		return false
	}
	start := 'array.'.len
	if name[start] != `s` || name[start + 1] != `o` || name[start + 2] != `r`
		|| name[start + 3] != `t` {
		return false
	}
	return name.len == 10 || (name[start + 4] == `e` && name[start + 5] == `d`)
}

// call_receiver_array_type updates call receiver array type state for TypeChecker.
fn (tc &TypeChecker) call_receiver_array_type(node flat.Node) ?Array {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := tc.a.child(fn_node, 0)
	if resolved := tc.expr_type(base_id) {
		if arr := call_receiver_array_from_type(resolved) {
			return arr
		}
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.typ.len > 0 {
		if arr := call_receiver_array_from_type(tc.parse_resolution_type(base_node.typ)) {
			return arr
		}
	}
	if arr := call_receiver_array_from_type(tc.resolve_type(base_id)) {
		return arr
	}
	return none
}

fn call_receiver_array_from_type(typ Type) ?Array {
	return match typ {
		Array {
			typ
		}
		ArrayFixed {
			Array{
				elem_type: typ.elem_type
			}
		}
		Alias {
			call_receiver_array_from_type(typ.base_type)
		}
		Pointer {
			call_receiver_array_from_type(typ.base_type)
		}
		else {
			none
		}
	}
}

// call_arg_value updates call arg value state for TypeChecker.
fn (tc &TypeChecker) call_arg_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .field_init && node.children_count > 0 {
		return tc.a.child(&node, 0)
	}
	return id
}

// receiver_compatible supports receiver compatible handling for TypeChecker.
fn (tc &TypeChecker) receiver_compatible(actual Type, expected Type) bool {
	if tc.type_compatible(actual, expected) {
		return true
	}
	// Check the generic-base relaxation before the pointer fallbacks: it unwraps
	// pointers itself, so a bare `&Box{...}` literal still matches an expected
	// `&Box[int]` (while `&Box[string]` vs `&Box[int]` stays rejected).
	if tc.generic_receiver_base_match(actual, expected) {
		return true
	}
	if expected is Pointer {
		return tc.type_compatible(actual, expected.base_type)
	}
	if actual is Pointer {
		return tc.type_compatible(actual.base_type, expected)
	}
	return false
}

fn (tc &TypeChecker) method_receiver_compatible(actual Type, expected Type, method_name string) bool {
	if tc.receiver_compatible(actual, expected) {
		return true
	}
	// Runtime array builtins are declared with the internal `array` receiver, which is
	// represented here as `[]void`. Keep that erasure scoped to raw `array.*`
	// methods; user receivers like `fn (xs []void) touch()` must not accept `[]int`.
	if checker_is_raw_collection_method_name(method_name, 'array.') && actual is Array
		&& expected is Array && expected.elem_type is Void {
		return true
	}
	return false
}

// generic_receiver_base_match relaxes compatibility between two same-base generic
// struct types when at least one side is the open/bare generic form — a bare
// `Vec4{...}` literal specializing to the expected `Vec4[f32]`, or a concrete
// `Vec4[f32]` value matching the open `Vec4` / `Vec4[T]` method-receiver form.
//
// It deliberately does NOT relax two *different concrete* instantiations. Because
// `receiver_compatible` is also used for ordinary call arguments, field inits,
// array literals, and expected-type propagation, conflating them would let
// `Box[string]` satisfy an expected `Box[int]` and emit incompatible C structs.
fn (tc &TypeChecker) generic_receiver_base_match(actual Type, expected Type) bool {
	// Both sides must share pointer shape before unwrapping: a `&Box{...}` value must
	// not satisfy an expected `Box[int]` value. The C argument path only
	// auto-dereferences when the actual/expected type names match exactly, so the bare
	// `Box` vs `Box[int]` mismatch would otherwise be emitted as a pointer where a value
	// is required. (A pointer receiver on a value-receiver method is handled by
	// receiver_compatible's pointer fallbacks, not here.)
	if (actual is Pointer) != (expected is Pointer) {
		return false
	}
	a_full := unwrap_pointer(actual).name()
	e_full := unwrap_pointer(expected).name()
	a := strip_generic_args_name(a_full)
	if a.len == 0 || a != strip_generic_args_name(e_full) {
		return false
	}
	if !(a in tc.struct_generic_params || tc.sum_params_for_base(a).len > 0) {
		return false
	}
	if a_full == e_full {
		return false
	}
	// Reject two *different concrete* instantiations (`Box[string]` vs `Box[int]`),
	// which produce incompatible C structs. Relax only when at least one side is
	// the open/bare generic form: a bare `Box{...}` literal specializing to the
	// expected `Box[int]`, or a concrete `Box[int]` value matching the open
	// `Box`/`Box[T]` method-receiver form.
	if tc.is_concrete_generic_instance(a_full) && tc.is_concrete_generic_instance(e_full) {
		return false
	}
	return true
}

// is_concrete_generic_instance reports whether `name` is a fully concrete generic
// instantiation (e.g. `Box[int]`), as opposed to the bare base (`Box`) or an open
// parameter form (`Box[T]`).
fn (tc &TypeChecker) is_concrete_generic_instance(name string) bool {
	_, args, ok := generic_type_application_parts(name)
	if !ok {
		return false
	}
	return tc.generic_args_are_concrete(args)
}

// bare_generic_literal_adopts reports whether a struct literal written as the bare
// generic base (`Box{...}`, no type args) should adopt the concrete `expected`
// instance (`Box[int]`, optionally behind a pointer). The base short-names must match
// and the base must be a known generic struct, so a non-generic same-named struct is
// left to ordinary checking.
fn (tc &TypeChecker) bare_generic_literal_adopts(lit_value string, expected Type) bool {
	if lit_value.len == 0 || lit_value.contains('[') {
		return false
	}
	e_base, _, e_ok := generic_type_application_parts(unwrap_pointer(expected).name())
	if !e_ok || e_base.all_after_last('.') != lit_value.all_after_last('.') {
		return false
	}
	return e_base in tc.struct_generic_params
		|| e_base.all_after_last('.') in tc.struct_generic_params
}

fn is_anonymous_struct_name(name string) bool {
	return name.all_after_last('.').starts_with('AnonStruct_')
}

fn (mut tc TypeChecker) anonymous_struct_literal_compatible(node flat.Node, expected Type) bool {
	struct_type := struct_type_from_type(expected) or { return false }
	if !is_anonymous_struct_name(struct_type.name) {
		return false
	}
	fields := tc.struct_fields_for_init(struct_type.name)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			return false
		}
		mut field_type := Type(void_)
		if field.value.len > 0 {
			field_type = tc.struct_field_type(struct_type.name, field.value) or { return false }
		} else if i < fields.len {
			field_type = fields[i].typ
		} else {
			return false
		}
		value_id := tc.a.child(field, 0)
		actual := tc.resolve_expr(value_id, field_type)
		if !tc.expr_compatible(value_id, actual, field_type)
			&& !tc.pointer_value_compatible(actual, field_type) {
			return false
		}
	}
	return true
}

// generic_literal_fields_compatible checks a bare generic struct literal's named
// field initializers against the expected concrete instantiation (`Box[int]`),
// substituting the struct's type parameters into each field's declared type. It
// returns false only on a *definite* mismatch (e.g. `Box{v: 'str'}` for `Box[int]`),
// so a clearly-unrelated literal yields a clean checker error instead of adopting the
// type and emitting broken C; unresolvable fields stay lenient.
fn (mut tc TypeChecker) generic_literal_fields_compatible(node flat.Node, expected Type) bool {
	e_base, e_args, e_ok := generic_type_application_parts(unwrap_pointer(expected).name())
	if !e_ok {
		return true
	}
	params := tc.struct_generic_params[e_base] or {
		tc.struct_generic_params[e_base.all_after_last('.')] or { return true }
	}
	if params.len != e_args.len {
		return true
	}
	fields := tc.structs[e_base] or { tc.structs[e_base.all_after_last('.')] or { return true } }
	for i in 0 .. node.children_count {
		fi := tc.a.child_node(&node, i)
		if fi.kind != .field_init || fi.children_count == 0 {
			continue
		}
		mut decl_typ := Type(void_)
		mut found := false
		if fi.value.len > 0 {
			// Named initializer (`Box{v: 'x'}`): match by field name.
			for f in fields {
				if f.name == fi.value {
					decl_typ = f.typ
					found = true
					break
				}
			}
		} else if i < fields.len {
			// Positional initializer (`Box{'x'}`): the parser emits a `field_init` with
			// an empty name, so match by field order like `check_struct_init` does.
			decl_typ = fields[i].typ
			found = true
		}
		if !found {
			continue
		}
		sub := tc.substitute_generic_type(decl_typ, e_args, params)
		if sub is Unknown || sub is Void {
			continue
		}
		actual := tc.resolve_expr(tc.a.child(fi, 0), sub)
		if !tc.expr_receiver_compatible(tc.a.child(fi, 0), actual, sub) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) expr_receiver_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if !tc.receiver_compatible(actual, expected) {
		return false
	}
	return tc.generic_expected_expr_fields_compatible(expr_id, expected)
}

fn (mut tc TypeChecker) expr_generic_expected_match(expr_id flat.NodeId, actual Type, expected Type) bool {
	return tc.generic_expected_type_match(actual, expected)
		&& tc.generic_expected_expr_fields_compatible(expr_id, expected)
}

fn (mut tc TypeChecker) generic_expected_expr_fields_compatible(expr_id flat.NodeId, expected Type) bool {
	if !tc.valid_node_id(expr_id) {
		return true
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.field_init, .expr_stmt, .paren {
			if node.children_count > 0 {
				return tc.generic_expected_expr_fields_compatible(tc.a.child(&node, 0), expected)
			}
		}
		.struct_init {
			if tc.bare_generic_literal_adopts(node.value, expected) {
				return tc.generic_literal_fields_compatible(node, expected)
			}
		}
		.prefix {
			if node.op == .amp && node.children_count == 1 && expected is Pointer {
				child := tc.a.nodes[int(tc.a.child(&node, 0))]
				if child.kind == .struct_init
					&& tc.bare_generic_literal_adopts(child.value, expected) {
					return tc.generic_literal_fields_compatible(child, expected)
				}
			}
		}
		else {}
	}

	return true
}

// strip_generic_args_name returns the base name of a generic instance type
// (`Box[int]` -> `Box`); array/map types (leading `[`) yield the name unchanged.
fn strip_generic_args_name(name string) string {
	bracket := name.index_u8(`[`)
	if bracket <= 0 {
		return name
	}
	return name[..bracket]
}

// is_zero_literal reports whether is zero literal applies in types.
fn (tc &TypeChecker) is_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	return node.kind == .int_literal && node.value == '0'
}

// is_fn_pointer_type reports whether is fn pointer type applies in types.
fn is_fn_pointer_type(typ Type) bool {
	clean0 := typ
	mut clean := clean0
	if clean0 is Alias {
		clean = clean0.base_type
	}
	return clean is FnType
}

// fn_type_from_type converts fn type from type data for types.
fn fn_type_from_type(typ Type) ?FnType {
	if typ is FnType {
		return typ
	}
	if typ is Alias {
		return fn_type_from_type(typ.base_type)
	}
	return none
}

fn struct_type_from_type(typ Type) ?Struct {
	if typ is Struct {
		return typ
	}
	if typ is Alias {
		return struct_type_from_type(typ.base_type)
	}
	return none
}

// selector_fn_type supports selector fn type handling for TypeChecker.
fn (tc &TypeChecker) selector_fn_type(node flat.Node) ?FnType {
	if node.children_count == 0 {
		return none
	}
	if !valid_string_data(node.value) {
		return none
	}
	if typ := tc.const_type_for_selector(node) {
		if fn_typ := fn_type_from_type(typ) {
			return fn_typ
		}
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.selector_fn_base_type(base_id) or { return none }
	clean := unalias_and_unwrap_pointer_type(base_type)
	if clean is Struct {
		if typ := tc.struct_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	return none
}

fn (tc &TypeChecker) method_value_type(receiver_name string, method string) ?Type {
	method_name := '${receiver_name}.${method}'
	mut ret_type := tc.fn_ret_types[method_name] or { Type(void_) }
	mut params := tc.fn_param_types[method_name] or { []Type{} }
	if method_name !in tc.fn_ret_types && method_name !in tc.fn_param_types {
		// A concrete generic receiver (`Box[int]`) has its methods registered under the
		// open key (`Box[T].method`); resolve and substitute so a method *value* on a
		// generic struct is typed instead of reported as an unknown field.
		ci := tc.resolve_generic_struct_method(receiver_name, method) or { return none }
		ret_type = ci.return_type
		params = ci.params.clone()
	}
	mut bound_params := []Type{}
	if params.len > 1 {
		bound_params = params[1..].clone()
	}
	return Type(FnType{
		params:      bound_params
		return_type: ret_type
	})
}

// selector_fn_base_type supports selector fn base type handling for TypeChecker.
fn (tc &TypeChecker) selector_fn_base_type(base_id flat.NodeId) ?Type {
	if typ := tc.cached_expr_type(base_id) {
		return typ
	}
	if int(base_id) < 0 {
		return none
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.typ.len > 0 && base_node.typ != 'unknown' {
		return tc.parse_type(base_node.typ)
	}
	if base_node.kind == .call {
		if typ := tc.resolved_call_type(base_id) {
			return typ
		}
		if typ := tc.direct_call_return_type(base_node) {
			return typ
		}
		return none
	}
	return tc.resolve_type(base_id)
}

// direct_call_return_type supports direct call return type handling for TypeChecker.
fn (tc &TypeChecker) direct_call_return_type(node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .ident {
		if local_name := tc.local_bare_fn_key(fn_node.value) {
			if typ := tc.fn_ret_types[local_name] {
				return typ
			}
		}
		if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
			if typ := tc.fn_ret_types[imported_name] {
				return typ
			}
		}
		if typ := tc.fn_ret_types[fn_node.value] {
			return typ
		}
		return none
	}
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_node := tc.a.child_node(fn_node, 0)
	if base_node.kind == .ident {
		base_is_value := tc.ident_resolves_to_value(base_node.value)
		if !base_is_value {
			if resolved := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved}.${fn_node.value}'
				if typ := tc.fn_ret_types[mod_name] {
					return typ
				}
			}
			qbase := tc.qualify_name(base_node.value)
			static_name := '${qbase}.${fn_node.value}'
			if static_name in tc.fn_ret_types && (qbase in tc.structs
				|| qbase in tc.enum_names || qbase in tc.sum_types
				|| qbase in tc.interface_names) {
				return tc.fn_ret_types[static_name] or { none }
			}
		}
		return none
	}
	if base_node.kind == .selector {
		inner := tc.a.child_node(base_node, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
			if typ := tc.fn_ret_types[full_name] {
				return typ
			}
		}
	}
	return none
}

fn (tc &TypeChecker) spawn_child_call_return_type(node flat.Node) ?Type {
	if ret := tc.direct_call_return_type(node) {
		return ret
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .ident {
		return none
	}
	mut candidates := []string{}
	candidates << fn_node.value
	qname := tc.qualify_fn_name(fn_node.value)
	if qname != fn_node.value {
		candidates << qname
	}
	if tc.cur_module.len > 0 {
		candidates << '${tc.cur_module}.${fn_node.value}'
	}
	candidates << 'main.${fn_node.value}'
	for candidate in candidates {
		if ret := tc.fn_ret_types[candidate] {
			return ret
		}
	}
	return none
}

// module_const_receiver_method_name supports module_const_receiver_method_name handling in types.
fn (tc &TypeChecker) module_const_receiver_method_name(base_node flat.Node, method string) ?string {
	if base_node.kind != .selector || base_node.children_count == 0 || method.len == 0 {
		return none
	}
	inner := tc.a.child_node(&base_node, 0)
	if inner.kind != .ident {
		return none
	}
	mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
	const_name := '${mod_name}.${base_node.value}'
	mut const_type := tc.const_types[const_name] or { Type(Unknown{}) }
	const_type = tc.const_type_from_initializer(const_name, const_type)
	if const_type is Unknown && base_node.value == 'scanner_matcher' {
		const_type = Type(Struct{
			name: '${mod_name}.KeywordsMatcherTrie'
		})
	}
	clean := unwrap_pointer(const_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	for method_name in receiver_method_name_candidates(clean, method, mod_name) {
		if method_name in tc.fn_ret_types {
			return method_name
		}
	}
	return none
}

// valid_string_data supports valid string data handling for types.
fn valid_string_data(s string) bool {
	if s.len == 0 {
		return true
	}
	ptr := unsafe { u64(voidptr(s.str)) }
	return ptr >= 4096 && ptr < 281474976710656 && s.len < 1048576
}

// clone_smartcasts supports clone smartcasts handling for types.
fn clone_smartcasts(src map[string]Type) map[string]Type {
	mut dst := map[string]Type{}
	for key, typ in src {
		if valid_string_data(key) {
			dst[key] = typ
		}
	}
	return dst
}

// array_elem_type supports array elem type handling for types.
fn array_elem_type(arr Array) Type {
	return arr.elem_type
}

// array_like_elem_type returns the element type of an `Array` or `ArrayFixed`.
fn array_like_elem_type(t Type) ?Type {
	if t is Array {
		return t.elem_type
	}
	if t is ArrayFixed {
		return t.elem_type
	}
	if t is Alias {
		return array_like_elem_type(t.base_type)
	}
	return none
}

// if_branch_types_compatible reports whether two if-expression branch types are
// compatible. Bare array literals (`[a, b, c]`) resolve to a fixed `T[n]`, but V
// treats them as dynamic `[]T`; two *literal* branches with compatible element
// types must therefore not be flagged as a mismatch merely because their lengths
// differ. The length-agnostic relaxation is limited to literal tails: genuine
// fixed-array values keep their length (handled by `type_compatible` above), so
// e.g. `[2]int` vs `[3]int` branches still mismatch.
fn (tc &TypeChecker) if_branch_types_compatible(a Type, b Type, a_is_array_lit bool, b_is_array_lit bool) bool {
	if (a is None && b is ResultType) || (b is None && a is ResultType) {
		return false
	}
	if tc.type_compatible(a, b) || tc.type_compatible(b, a) {
		return true
	}
	if !a_is_array_lit || !b_is_array_lit {
		return false
	}
	a_elem := array_like_elem_type(a) or { return false }
	b_elem := array_like_elem_type(b) or { return false }
	return tc.type_compatible(a_elem, b_elem) || tc.type_compatible(b_elem, a_elem)
}

fn (tc &TypeChecker) if_branch_types_compatible_with_expected(a Type, a_tail flat.NodeId, b Type, b_tail flat.NodeId, expected Type) bool {
	if expected is Void || expected is Unknown {
		return false
	}
	return tc.if_branch_type_compatible_with_context(a, a_tail, expected)
		&& tc.if_branch_type_compatible_with_context(b, b_tail, expected)
}

fn (tc &TypeChecker) if_branch_type_compatible_with_context(actual Type, tail_id flat.NodeId, expected Type) bool {
	if actual is None {
		return (expected is OptionType || is_ierror_type(expected))
			&& tc.branch_tail_is_none_literal(tail_id)
	}
	if is_option_void_type(actual) {
		return (expected is OptionType || is_ierror_type(expected))
			&& tc.branch_tail_is_none_literal(tail_id)
	}
	if is_result_void_type(actual) {
		return expected is ResultType && tc.branch_tail_is_error_literal(tail_id)
	}
	if is_ierror_type(actual) {
		return (expected is ResultType || is_ierror_type(expected))
			&& tc.branch_tail_is_error_literal(tail_id)
	}
	return tc.type_compatible(actual, expected)
}

fn (tc &TypeChecker) branch_tail_is_none_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.branch_tail_is_none_literal(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) branch_tail_is_error_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .call {
		return tc.call_display_name(node) in ['error', 'error_with_code']
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.branch_tail_is_error_literal(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) branch_failure_literal_matches_context(id flat.NodeId, expected Type) bool {
	if tc.branch_tail_is_none_literal(id) {
		return expected is OptionType || is_ierror_type(expected)
	}
	if tc.branch_tail_is_error_literal(id) {
		return expected is ResultType || is_ierror_type(expected)
	}
	return true
}

fn if_branch_type_needs_context(typ Type) bool {
	if typ is None || is_ierror_type(typ) {
		return true
	}
	if typ is OptionType {
		return typ.base_type is Void
	}
	if typ is ResultType {
		return typ.base_type is Void
	}
	return false
}

fn inferred_contextual_if_type(a Type, b Type) ?Type {
	if a is None {
		return optional_if_type_from_value(b)
	}
	if b is None {
		return optional_if_type_from_value(a)
	}
	if is_option_void_type(a) {
		return optional_if_type_from_value(b)
	}
	if is_option_void_type(b) {
		return optional_if_type_from_value(a)
	}
	if is_ierror_type(a) {
		return result_if_type_from_value(b)
	}
	if is_ierror_type(b) {
		return result_if_type_from_value(a)
	}
	if is_result_void_type(a) {
		return result_if_type_from_value(b)
	}
	if is_result_void_type(b) {
		return result_if_type_from_value(a)
	}
	return none
}

fn is_option_void_type(typ Type) bool {
	if typ is OptionType {
		return typ.base_type is Void
	}
	return false
}

fn is_result_void_type(typ Type) bool {
	if typ is ResultType {
		return typ.base_type is Void
	}
	return false
}

fn optional_if_type_from_value(value Type) ?Type {
	if value is OptionType {
		if value.base_type is Void {
			return none
		}
		return value
	}
	if value is ResultType {
		return none
	}
	if if_branch_type_needs_context(value) || value is Void || value is Unknown {
		return none
	}
	return Type(OptionType{
		base_type: value
	})
}

fn result_if_type_from_value(value Type) ?Type {
	if value is ResultType {
		if value.base_type is Void {
			return none
		}
		return value
	}
	if value is OptionType || if_branch_type_needs_context(value) || value is Void
		|| value is Unknown {
		return none
	}
	return Type(ResultType{
		base_type: value
	})
}

// branch_tail_is_array_literal reports whether a branch's value tail is a bare
// array literal (`[a, b, c]`) — directly, or through a const whose initializer is
// one. V types such values as dynamic `[]T` regardless of element count, so they
// must not constrain if-branch length compatibility. Explicit fixed-array
// initializers (`[N]T{...}`, parsed as `.array_init`) are genuine fixed arrays and
// keep their length.
fn (tc &TypeChecker) branch_tail_is_array_literal(id flat.NodeId) bool {
	return tc.expr_is_bare_array_literal(tc.branch_tail_expr_id(id))
}

// expr_is_bare_array_literal reports whether `id` is a bare `[a, b, c]` literal,
// directly or through a single const reference.
fn (tc &TypeChecker) expr_is_bare_array_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .array_literal {
		return true
	}
	if node.kind == .ident {
		for cand in [tc.qualify_name(node.value), node.value] {
			if expr_id := tc.const_exprs[cand] {
				if tc.valid_node_id(expr_id) {
					return tc.a.nodes[int(expr_id)].kind == .array_literal
				}
			}
		}
	}
	return false
}

// fixed_array_elem_type supports fixed array elem type handling for types.
fn fixed_array_elem_type(arr ArrayFixed) Type {
	return arr.elem_type
}

// map_value_type supports map value type handling for types.
fn map_value_type(m Map) Type {
	return m.value_type
}

// pointer_base_type supports pointer base type handling for types.
fn pointer_base_type(p Pointer) Type {
	return p.base_type
}

// fn_param_type supports fn param type handling for types.
fn fn_param_type(f FnType, idx int) Type {
	return f.params[idx]
}

// is_known_call reports whether is known call applies in types.
fn (tc &TypeChecker) is_known_call(node flat.Node) bool {
	if node.children_count == 0 {
		return true
	}
	if node.typ.len > 0 {
		return true
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .selector {
		base_node := tc.a.child_node(fn_node, 0)
		if base_node.kind == .ident {
			if base_node.value == 'C' {
				return true
			}
			if resolved_mod := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved_mod}.${fn_node.value}'
				if mod_name in tc.fn_ret_types || mod_name in tc.sum_types || mod_name in tc.structs
					|| mod_name in tc.enum_names {
					return true
				}
			}
			if base_node.value in tc.structs || base_node.value in tc.enum_names {
				qname := tc.qualify_name(base_node.value)
				if '${qname}.${fn_node.value}' in tc.fn_ret_types {
					return true
				}
			} else {
				qname := tc.qualify_name(base_node.value)
				if qname in tc.structs || qname in tc.enum_names {
					if '${qname}.${fn_node.value}' in tc.fn_ret_types {
						return true
					}
				}
			}
		} else if base_node.kind == .selector {
			inner := tc.a.child_node(base_node, 0)
			if inner.kind == .ident {
				mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
				if '${mod_name}.${base_node.value}.${fn_node.value}' in tc.fn_ret_types {
					return true
				}
			}
		}
		if _ := tc.selector_fn_type(fn_node) {
			return true
		}
		base_type := tc.resolve_type(tc.a.child(fn_node, 0))
		if fn_node.value == 'hex' && tc.type_is_pointer_receiver(base_type) {
			return false
		}
		clean_type := unwrap_pointer(base_type)
		if clean_type is Array || clean_type is ArrayFixed {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
			return tc.is_known_array_receiver_method(clean_type, fn_node.value)
		}
		if clean_type is Map {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
			return true
		}
		if clean_type is String {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
			return 'string.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Alias {
			mname := '${clean_type.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return true
			}
			base_name := resolve_type_name_for_method(clean_type.base_type)
			if base_name.len > 0 {
				for base_mname in receiver_method_name_candidates(clean_type.base_type,
					fn_node.value, tc.cur_module) {
					if base_mname in tc.fn_ret_types {
						return true
					}
				}
			}
		}
		if clean_type is Struct {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Interface {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is SumType {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Enum {
			if fn_node.value == 'str' {
				return true
			}
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Primitive {
			mname := '${prim_c_type_from(clean_type.props, clean_type.size)}.${fn_node.value}'
			return mname in tc.fn_ret_types
		}
		return false
	}
	if fn_node.kind == .ident {
		if typ := tc.cur_scope.lookup(fn_node.value) {
			return typ is FnType
		}
		qfn := tc.qualify_fn_name(fn_node.value)
		if qfn in tc.fn_ret_types || fn_node.value in tc.fn_ret_types {
			return true
		}
		if _ := tc.resolve_selective_import_symbol(fn_node.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) is_known_array_receiver_method(receiver Type, method string) bool {
	if receiver is Array {
		for mname in receiver_method_name_candidates(receiver, method, tc.cur_module) {
			if mname in tc.fn_ret_types {
				return true
			}
		}
		// Keep this in sync with the synthetic array receiver methods handled in
		// resolve_call_info/resolve_type, including `[]thread T.wait()`.
		return method in ['first', 'last', 'pop', 'pop_left', 'contains', 'join', 'index',
			'last_index', 'repeat', 'repeat_to_depth', 'delete', 'delete_last', 'clear', 'insert',
			'prepend', 'filter', 'map', 'any', 'all', 'count', 'sort_with_compare',
			'sorted_with_compare', 'sort', 'sorted', 'clone', 'reverse', 'equals', 'wait']
	}
	if receiver is ArrayFixed {
		array_type := Type(Array{
			elem_type: receiver.elem_type
		})
		for mname in receiver_method_name_candidates(array_type, method, tc.cur_module) {
			if mname in tc.fn_ret_types {
				return true
			}
		}
		return method == 'pointers'
	}
	return false
}

fn (tc &TypeChecker) is_unsupported_hex_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'hex' || fn_node.children_count == 0 {
		return false
	}
	base_id := tc.a.child(fn_node, 0)
	if tc.receiver_expr_is_pointer(base_id) {
		return true
	}
	base_type := tc.resolve_type(base_id)
	return !tc.is_builtin_hex_receiver(base_type)
}

fn (tc &TypeChecker) call_has_ambiguous_selective_import(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		return base.kind == .ident && tc.selective_import_symbol_is_ambiguous(base.value)
	}
	return fn_node.kind == .ident && tc.selective_import_symbol_is_ambiguous(fn_node.value)
}

// call_display_name updates call display name state for TypeChecker.
fn (tc &TypeChecker) call_display_name(node flat.Node) string {
	if node.children_count == 0 {
		return '<missing>'
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .ident {
		return fn_node.value
	}
	if fn_node.kind == .selector && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		if base.value.len > 0 {
			return '${base.value}.${fn_node.value}'
		}
	}
	if fn_node.kind in [.index, .prefix, .array_init] {
		type_name := tc.type_expr_name(tc.a.child(&node, 0))
		if type_name.len > 0 {
			return type_name
		}
	}
	return fn_node.value
}

// check_if_expr validates check if expr state for types.
fn (mut tc TypeChecker) check_if_expr(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	value_context := !tc.is_statement_node(id)
	cond_id := tc.a.child(&node, 0)
	guard_bindings := tc.check_condition(cond_id)
	smartcasts := tc.extract_smartcasts(cond_id)
	then_id := tc.a.child(&node, 1)
	then_uses_block_scope := guard_bindings.len == 0 && tc.valid_node_id(then_id)
		&& tc.a.nodes[int(then_id)].kind == .block
	saved_smartcasts := clone_smartcasts(tc.smartcasts)
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			tc.smartcasts[sc.name] = sc.typ
		}
	}
	$if ownership ? {
		if value_context {
			tc.ownership_begin_value_branch_group()
		} else {
			tc.ownership_begin_branch_group()
		}
	}
	tc.push_scope()
	$if ownership ? {
		if !then_uses_block_scope {
			tc.ownership_mark_scope_node(then_id)
		}
	}
	for binding in guard_bindings {
		$if ownership ? {
			tc.ownership_note_binding(binding.name, binding.typ, cond_id)
		}
		tc.cur_scope.insert(binding.name, binding.typ)
	}
	tc.check_branch_node(then_id, value_context)
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_branch(then_id)
	}
	tc.smartcasts = clone_smartcasts(saved_smartcasts)
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		else_smartcasts := tc.extract_else_branch_smartcasts(cond_id)
		for sc in else_smartcasts {
			if valid_string_data(sc.name) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		$if ownership ? {
			tc.ownership_begin_branch()
		}
		tc.check_branch_node(else_id, value_context)
		$if ownership ? {
			tc.ownership_end_branch(else_id)
		}
		if else_smartcasts.len > 0 {
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
		}
	} else {
		$if ownership ? {
			tc.ownership_add_branch_group_base()
		}
	}
	$if ownership ? {
		tc.ownership_end_branch_group()
	}
	if !value_context {
		return
	}
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			tc.smartcasts[sc.name] = sc.typ
		}
	}
	then_type := tc.branch_tail_type(then_id)
	tc.smartcasts = clone_smartcasts(saved_smartcasts)
	mut else_type := Type(void_)
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		else_type = tc.branch_tail_type(else_id)
	}
	if then_type !is Void && else_type !is Void {
		else_id := tc.a.child(&node, 2)
		if tc.branch_has_value_tail(then_id) && tc.branch_has_value_tail(else_id)
			&& !tc.if_branch_types_compatible(then_type, else_type, tc.branch_tail_is_array_literal(then_id), tc.branch_tail_is_array_literal(else_id)) {
			then_tail := tc.branch_tail_expr_id(then_id)
			else_tail := tc.branch_tail_expr_id(else_id)
			if tc.if_branch_none_has_option_context(then_type, then_tail, else_type, else_tail) {
				return
			}
			if tc.if_branch_enum_shorthand_compatible(then_type, then_tail, else_type, else_tail) {
				return
			}
			if expected := tc.expected_context_for_expr(id) {
				branches_match_expected := tc.if_branch_types_compatible_with_expected(then_type,
					then_tail, else_type, else_tail, expected)
				if branches_match_expected {
					return
				}
			}
			if tc.should_diagnose(id) {
				tc.record_error(.if_branch_mismatch,
					'if-expression branch type mismatch: then `${then_type.name()}` vs else `${else_type.name()}`',
					id)
			}
		}
	}
}

fn (tc &TypeChecker) if_branch_none_has_option_context(a Type, a_tail flat.NodeId, b Type, b_tail flat.NodeId) bool {
	if (a is None || is_option_void_type(a)) && tc.branch_tail_is_none_literal(a_tail) {
		return b is OptionType && b.base_type !is Void
	}
	if (b is None || is_option_void_type(b)) && tc.branch_tail_is_none_literal(b_tail) {
		return a is OptionType && a.base_type !is Void
	}
	return false
}

fn (mut tc TypeChecker) if_branch_enum_shorthand_compatible(a Type, a_tail flat.NodeId, b Type, b_tail flat.NodeId) bool {
	if a is Enum && tc.valid_node_id(b_tail) && tc.a.nodes[int(b_tail)].kind == .enum_val {
		return tc.type_compatible(tc.resolve_expr(b_tail, a), a)
	}
	if b is Enum && tc.valid_node_id(a_tail) && tc.a.nodes[int(a_tail)].kind == .enum_val {
		return tc.type_compatible(tc.resolve_expr(a_tail, b), b)
	}
	return false
}

fn (mut tc TypeChecker) check_stmt_node(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	idx := int(id)
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.statement_nodes.len {
			tc.statement_nodes[idx] = true
		} else {
			tc.sparse_statement_nodes[idx] = true
		}
		tc.check_node(id)
		$if ownership ? {
			tc.ownership_after_stmt_node(id)
		}
		return
	}
	if idx >= tc.statement_nodes.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.statement_nodes.len {
		tc.statement_nodes[idx] = true
	}
	tc.check_node(id)
	$if ownership ? {
		tc.ownership_after_stmt_node(id)
	}
}

fn (tc &TypeChecker) is_statement_node(id flat.NodeId) bool {
	idx := int(id)
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			return idx < tc.statement_nodes.len && tc.statement_nodes[idx]
		}
		return tc.sparse_statement_nodes[idx]
	}
	return idx >= 0 && idx < tc.statement_nodes.len && tc.statement_nodes[idx]
}

fn (mut tc TypeChecker) check_statement_sequence(node flat.Node, body_start int, value_tail bool) {
	saved_smartcasts := clone_smartcasts(tc.smartcasts)
	defer {
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
	}
	last_idx := int(node.children_count) - 1
	for i in body_start .. node.children_count {
		child_id := tc.a.child(&node, i)
		is_value_tail := value_tail && i == last_idx
		if is_value_tail {
			tc.check_node(child_id)
		} else {
			tc.check_stmt_node(child_id)
		}
		tc.apply_post_if_exit_smartcasts(child_id)
		$if ownership ? {
			if !is_value_tail {
				tc.ownership_flush_value_branch_moves()
			}
		}
	}
}

fn (mut tc TypeChecker) check_branch_node(id flat.NodeId, value_tail bool) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(id)
		}
		tc.check_statement_sequence(node, 0, value_tail)
		tc.pop_scope()
		return
	}
	if value_tail {
		tc.check_node(id)
	} else {
		tc.check_stmt_node(id)
	}
}

fn (mut tc TypeChecker) apply_post_if_exit_smartcasts(id flat.NodeId) {
	for binding in tc.post_if_exit_smartcasts(id) {
		if valid_string_data(binding.name) {
			tc.smartcasts[binding.name] = binding.typ
		}
	}
}

fn (tc &TypeChecker) post_if_exit_smartcasts(id flat.NodeId) []LocalBinding {
	if !tc.valid_node_id(id) {
		return []LocalBinding{}
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .if_expr || node.children_count < 2 {
		return []LocalBinding{}
	}
	cond_id := tc.a.child(&node, 0)
	then_id := tc.a.child(&node, 1)
	if binding := tc.negated_is_smartcast(cond_id) {
		if tc.stmt_definitely_returns(then_id) {
			return [binding]
		}
	}
	if node.children_count >= 3 {
		else_id := tc.a.child(&node, 2)
		if tc.stmt_definitely_returns(else_id) {
			return tc.extract_smartcasts(cond_id)
		}
	}
	return []LocalBinding{}
}

fn (tc &TypeChecker) negated_is_smartcast(cond_id flat.NodeId) ?LocalBinding {
	if !tc.valid_node_id(cond_id) {
		return none
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind != .prefix || cond.op != .not || cond.children_count == 0 {
		return none
	}
	inner_id := tc.a.child(&cond, 0)
	if !tc.valid_node_id(inner_id) {
		return none
	}
	inner := tc.a.nodes[int(inner_id)]
	if inner.kind != .is_expr || inner.children_count == 0 {
		return none
	}
	expr_id := tc.a.child(&inner, 0)
	key := tc.expr_key(expr_id)
	if key.len == 0 || !valid_string_data(key) || inner.value.len == 0 {
		return none
	}
	return LocalBinding{
		name: key
		typ:  tc.parse_type(inner.value)
	}
}

// branch_has_value_tail converts branch has value tail data for types.
fn (tc &TypeChecker) branch_has_value_tail(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		if node.children_count == 0 {
			return false
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return false
		}
		last := tc.a.nodes[int(last_id)]
		return last.kind == .expr_stmt
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return false
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return false
		}
		last := tc.a.nodes[int(last_id)]
		return last.kind == .expr_stmt
	}
	return node.kind !in [.assign, .decl_assign, .selector_assign, .index_assign, .return_stmt,
		.block]
}

// check_condition validates check condition state for types.
fn (mut tc TypeChecker) check_condition(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .decl_assign {
		return tc.check_if_guard(cond_id, cond)
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		lhs_id := tc.a.child(&cond, 0)
		rhs_id := tc.a.child(&cond, 1)
		mut bindings := tc.check_condition(lhs_id)
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		for sc in tc.extract_smartcasts(lhs_id) {
			if valid_string_data(sc.name) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		rhs_bindings := tc.check_condition(rhs_id)
		bindings << rhs_bindings
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
		return bindings
	}
	tc.check_bool_condition(cond_id)
	return []LocalBinding{}
}

// check_bool_condition validates check bool condition state for types.
fn (mut tc TypeChecker) check_bool_condition(cond_id flat.NodeId) {
	tc.check_node(cond_id)
	cond_type := tc.resolve_type(cond_id)
	if !tc.type_compatible(cond_type, Type(bool_)) && tc.should_diagnose(cond_id) {
		tc.record_error(.condition_mismatch,
			'if condition must be `bool`, not `${cond_type.name()}`', cond_id)
	}
}

// check_if_guard validates check if guard state for types.
fn (mut tc TypeChecker) check_if_guard(id flat.NodeId, node flat.Node) []LocalBinding {
	if node.children_count < 2 {
		return []LocalBinding{}
	}
	rhs_id := tc.a.child(&node, 1)
	tc.check_node(rhs_id)
	rhs_type := tc.resolve_type(rhs_id)
	mut payload := Type(void_)
	is_optional_result := rhs_type is OptionType || rhs_type is ResultType
	if rhs_type is OptionType {
		payload = rhs_type.base_type
	} else if rhs_type is ResultType {
		payload = rhs_type.base_type
	} else {
		rhs := tc.a.nodes[int(rhs_id)]
		if rhs.kind == .index && rhs.children_count > 0 {
			base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&rhs, 0)))
			if base_type is Map {
				payload = base_type.value_type
			} else if base_type is Array {
				payload = base_type.elem_type
			} else if base_type is ArrayFixed {
				payload = base_type.elem_type
			} else if base_type is String {
				payload = Type(u8_)
			}
		}
	}
	if payload is Void && !is_optional_result {
		if tc.should_diagnose(id) {
			tc.record_error(.condition_mismatch,
				'if guard expression must be optional or result, not `${rhs_type.name()}`', id)
		}
	}
	if payload is Void {
		return []LocalBinding{}
	}
	lhs_ids := tc.if_guard_lhs_ids(node)
	if payload is MultiReturn && lhs_ids.len > 1 {
		mut result := []LocalBinding{}
		for i, lhs_id in lhs_ids {
			if i >= payload.types.len {
				break
			}
			lhs := tc.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
				result << LocalBinding{
					name: lhs.value
					typ:  payload.types[i]
				}
			}
		}
		return result
	}
	if lhs_ids.len > 0 {
		lhs := tc.a.nodes[int(lhs_ids[0])]
		if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
			mut result := []LocalBinding{}
			result << LocalBinding{
				name: lhs.value
				typ:  payload
			}
			return result
		}
	}
	return []LocalBinding{}
}

fn (tc &TypeChecker) if_guard_lhs_ids(node flat.Node) []flat.NodeId {
	if node.children_count < 2 {
		return []flat.NodeId{}
	}
	mut lhs_ids := []flat.NodeId{cap: int(node.children_count) - 1}
	lhs_ids << tc.a.child(&node, 0)
	for i in 2 .. node.children_count {
		lhs_ids << tc.a.child(&node, i)
	}
	return lhs_ids
}

// check_match_stmt validates check match stmt state for types.
fn (mut tc TypeChecker) check_match_stmt(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	value_context := !tc.is_statement_node(id)
	subject_id := tc.a.child(&node, 0)
	tc.check_node(subject_id)
	subject_key := tc.expr_key(subject_id)
	subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
	$if ownership ? {
		if value_context {
			tc.ownership_begin_value_branch_group()
		} else {
			tc.ownership_begin_branch_group()
		}
	}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			tc.check_node(branch_id)
			continue
		}
		n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
		$if ownership ? {
			if branch.value == 'else' {
				tc.ownership_note_branch_group_else()
			}
		}
		if subject_type is SumType {
			for j in 0 .. n_conds {
				cond := tc.a.node(tc.a.child(branch, j))
				if pattern := tc.match_type_pattern(cond) {
					if _ := tc.sum_variant_type_for_pattern(subject_type.name, pattern) {
					} else {
						tc.record_error(.condition_mismatch,
							'`${pattern}` is not a variant of sum type `${subject_type.name}`', tc.a.child(branch,
							j))
					}
				}
			}
		} else if is_ierror_type(subject_type) {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern := tc.match_type_pattern(cond) {
					if _ := tc.resolve_ierror_match_pattern(pattern) {
					} else if tc.should_diagnose(cond_id) {
						tc.record_error(.condition_mismatch,
							'`${pattern}` is not compatible with `IError`', cond_id)
					}
				}
			}
		} else if subject_type is Interface {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern := tc.match_type_pattern(cond) {
					if concrete := tc.resolve_interface_match_pattern(pattern) {
						concrete_type := unalias_type(unwrap_pointer(tc.parse_type(concrete)))
						if concrete_type !is Interface
							&& !tc.named_type_implements_interface(concrete, subject_type.name)
							&& tc.should_diagnose(cond_id) {
							tc.record_error(.condition_mismatch,
								'`${pattern}` is not compatible with interface `${subject_type.name}`',
								cond_id)
						}
					} else if tc.should_diagnose(cond_id) {
						tc.record_error(.condition_mismatch, 'unknown type `${pattern}`', cond_id)
					}
				}
			}
		}
		$if ownership ? {
			tc.ownership_begin_branch()
		}
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		if subject_key.len > 0 && valid_string_data(subject_key) && n_conds == 1
			&& branch.children_count > 0 && (subject_type is SumType || is_ierror_type(subject_type)
			|| subject_type is Interface) {
			cond_id := tc.a.child(branch, 0)
			cond := tc.a.node(cond_id)
			if pattern := tc.match_type_pattern(cond) {
				smartcast_type := if subject_type is SumType {
					tc.sum_variant_type_for_pattern(subject_type.name, pattern) or { pattern }
				} else if is_ierror_type(subject_type) {
					tc.resolve_ierror_match_pattern(pattern) or { pattern }
				} else if subject_type is Interface {
					tc.resolve_interface_match_pattern(pattern) or { pattern }
				} else {
					pattern
				}
				tc.smartcasts[subject_key] = tc.parse_type(smartcast_type)
			}
		} else if subject_key.len > 0 && valid_string_data(subject_key) && n_conds > 1
			&& subject_type is SumType {
			for sc in tc.multi_match_common_field_smartcasts(subject_type, branch, n_conds,
				subject_key) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(branch_id)
		}
		tc.check_statement_sequence(branch, n_conds, value_context)
		tc.pop_scope()
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
		$if ownership ? {
			tc.ownership_end_branch(branch_id)
		}
	}
	$if ownership ? {
		if !tc.match_covers_all_variants(node) {
			tc.ownership_add_branch_group_base_if_no_else()
		}
		tc.ownership_end_branch_group()
	}
}

fn (tc &TypeChecker) multi_match_common_field_smartcasts(subject SumType, branch &flat.Node, n_conds int, subject_key string) []LocalBinding {
	mut common := []LocalBinding{}
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern := tc.match_type_pattern(cond) or { return []LocalBinding{} }
		variant_name := tc.sum_variant_type_for_pattern(subject.name, pattern) or {
			return []LocalBinding{}
		}
		variant_type := unalias_type(tc.parse_type(variant_name))
		if variant_type !is Struct {
			return []LocalBinding{}
		}
		variant := variant_type as Struct
		if i == 0 {
			for field in tc.structs[variant.name] or { return []LocalBinding{} } {
				common << LocalBinding{
					name: '${subject_key}.${field.name}'
					typ:  field.typ
				}
			}
			continue
		}
		mut intersection := []LocalBinding{cap: common.len}
		for candidate in common {
			field_name := candidate.name.all_after_last('.')
			field_type := tc.struct_field_type(variant.name, field_name) or { continue }
			if tc.type_compatible(field_type, candidate.typ)
				&& tc.type_compatible(candidate.typ, field_type) {
				intersection << candidate
			}
		}
		common = intersection.clone()
		if common.len == 0 {
			break
		}
	}
	return common
}

fn (tc &TypeChecker) resolve_interface_match_pattern(pattern string) ?string {
	for candidate in tc.interface_match_pattern_candidates(pattern) {
		if is_builtin_type_name(candidate) {
			return candidate
		}
		if tc.pattern_type_known(candidate) {
			return candidate
		}
		if tc.type_symbol_known(candidate) {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) pattern_type_known(pattern string) bool {
	clean := pattern.trim_space()
	if clean.starts_with('[]') || clean.starts_with('map[') || clean.starts_with('[')
		|| clean.starts_with('fn ') || clean.starts_with('fn(') {
		return tc.parse_type(clean) !is Unknown
	}
	return false
}

fn (tc &TypeChecker) resolve_ierror_match_pattern(pattern string) ?string {
	for candidate in tc.interface_match_pattern_candidates(pattern) {
		if tc.named_type_compatible_with_ierror(candidate) {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) interface_match_pattern_candidates(pattern string) []string {
	mut candidates := []string{}
	if !pattern.contains('.') {
		mut has_scoped_candidate := false
		if resolved := tc.resolve_selective_import_type_symbol(pattern) {
			candidates << resolved
			has_scoped_candidate = true
		}
		if tc.source_declares_type_in_scope(pattern, tc.cur_file, tc.cur_module) {
			candidates << pattern
			has_scoped_candidate = true
		}
		if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
			local := '${tc.cur_module}.${pattern}'
			if tc.type_symbol_known(local) {
				candidates << local
				has_scoped_candidate = true
			}
		}
		if !has_scoped_candidate {
			candidates << pattern
		}
	} else if resolved := tc.resolve_import_alias_pattern(pattern) {
		candidates << resolved
		candidates << pattern
	} else {
		candidates << pattern
	}
	qpattern := tc.qualify_name(pattern)
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

fn (tc &TypeChecker) resolve_import_alias_pattern(pattern string) ?string {
	dot := pattern.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	alias := pattern[..dot]
	resolved := tc.resolve_import_alias(alias) or { return none }
	return '${resolved}.${pattern[dot + 1..]}'
}

// check_is_expr validates check is expr state for types.
fn (mut tc TypeChecker) check_is_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	expr_id := tc.a.child(&node, 0)
	tc.check_node(expr_id)
	// `x is T` in a generic template stays undecided until monomorphization.
	// Only defer inside an actual generic function: in a non-generic one an
	// unknown single-letter pattern is a real error and must be validated.
	if node.value.len > 0 && tc.type_text_has_generic_placeholder(node.value)
		&& tc.cur_fn_is_generic_template() {
		return
	}
	// A `$for v in T.variants` loop variable is substituted by the comptime
	// unroll; `val is v` cannot be validated against the raw name.
	if node.value in tc.cur_comptime_variant_loop_vars || (node.value.contains('.')
		&& node.value.all_after_last('.') in tc.cur_comptime_variant_loop_vars) {
		return
	}
	mut expr_type := unalias_type(unwrap_pointer(tc.resolve_type(expr_id)))
	// A previous branch can narrow a variable to one variant and then assign it
	// another value. A later `is` still applies to the variable's declared sum
	// type, not only to the stale narrowed variant.
	if expr_type !is SumType && expr_type !is Interface {
		expr_node := tc.a.nodes[int(expr_id)]
		mut declared_type := Type(Unknown{})
		if expr_node.kind == .ident {
			declared_type = tc.cur_scope.lookup(expr_node.value) or { Type(Unknown{}) }
		} else if expr_node.kind == .selector {
			declared_type = tc.selector_type(expr_id, expr_node) or { Type(Unknown{}) }
		}
		declared_type = unalias_type(unwrap_pointer(declared_type))
		if declared_type is SumType || declared_type is Interface {
			expr_type = declared_type
		}
	}
	if expr_type is SumType {
		if node.value.len > 0 {
			if tc.sum_variant_type_for_pattern(expr_type.name, node.value) == none {
				if tc.should_diagnose(id) {
					tc.record_error(.condition_mismatch,
						'`${node.value}` is not a variant of sum type `${expr_type.name}`', id)
				}
			}
		}
		return
	}
	if is_ierror_type(expr_type) {
		if node.value.len > 0 {
			if node.value == 'none' {
			} else if _ := tc.resolve_ierror_match_pattern(node.value) {
			} else if tc.should_diagnose(id) {
				tc.record_error(.condition_mismatch,
					'`${node.value}` is not compatible with `IError`', id)
			}
		}
		return
	}
	if expr_type is Interface {
		if node.value.len > 0 {
			if concrete := tc.resolve_interface_match_pattern(node.value) {
				concrete_type := unalias_type(unwrap_pointer(tc.parse_type(concrete)))
				if concrete_type !is Interface
					&& !tc.named_type_implements_interface(concrete, expr_type.name)
					&& tc.should_diagnose(id) {
					tc.record_error(.condition_mismatch,
						'`${node.value}` is not compatible with interface `${expr_type.name}`', id)
				}
			} else if tc.should_diagnose(id) {
				tc.record_error(.condition_mismatch, 'unknown type `${node.value}`', id)
			}
		}
		return
	}
	if expr_type is Unknown {
		return
	}
	if tc.should_diagnose(id) {
		tc.record_error(.condition_mismatch,
			'`is` can only be used with sum type or interface values, not `${expr_type.name()}`',
			id)
	}
}

// branch_tail_type supports branch tail type handling for TypeChecker.
fn (tc &TypeChecker) branch_tail_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return Type(void_)
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .if_expr {
		return tc.if_expr_tail_type(id)
	}
	if node.kind == .block {
		if node.children_count == 0 {
			return Type(void_)
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return Type(void_)
		}
		last := tc.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			child_id := tc.a.child(&last, 0)
			if smart_type := tc.smartcast_type(child_id) {
				return smart_type
			}
			return tc.expr_type(child_id) or { tc.resolve_type(child_id) }
		}
		if smart_type := tc.smartcast_type(last_id) {
			return smart_type
		}
		return tc.expr_type(last_id) or { tc.resolve_type(last_id) }
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return Type(void_)
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return Type(void_)
		}
		last := tc.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			child_id := tc.a.child(&last, 0)
			if smart_type := tc.smartcast_type(child_id) {
				return smart_type
			}
			return tc.expr_type(child_id) or { tc.resolve_type(child_id) }
		}
		if smart_type := tc.smartcast_type(last_id) {
			return smart_type
		}
		return tc.expr_type(last_id) or { tc.resolve_type(last_id) }
	}
	if smart_type := tc.smartcast_type(id) {
		return smart_type
	}
	return tc.expr_type(id) or { tc.resolve_type(id) }
}

fn (tc &TypeChecker) branch_tail_type_with_smartcasts(id flat.NodeId, smartcasts []LocalBinding) Type {
	if smartcasts.len == 0 {
		return tc.branch_tail_type(id)
	}
	mut scoped := *tc
	scoped.smartcasts = clone_smartcasts(tc.smartcasts)
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			scoped.smartcasts[sc.name] = sc.typ
		}
	}
	return scoped.branch_tail_type(id)
}

// if_expr_tail_type supports if expr tail type handling for TypeChecker.
fn (tc &TypeChecker) if_expr_tail_type(id flat.NodeId) Type {
	mut cur_id := id
	mut result := Type(void_)
	for tc.valid_node_id(cur_id) {
		node := tc.a.nodes[int(cur_id)]
		if node.kind != .if_expr {
			return choose_if_tail_type(result, tc.branch_tail_type(cur_id))
		}
		if node.children_count > 1 {
			smartcasts := tc.extract_smartcasts(tc.a.child(&node, 0))
			then_type := tc.branch_tail_type_with_smartcasts(tc.a.child(&node, 1), smartcasts)
			result = choose_if_tail_type(result, then_type)
		}
		if node.children_count <= 2 {
			return result
		}
		else_id := tc.a.child(&node, 2)
		if !tc.valid_node_id(else_id) {
			return result
		}
		else_node := tc.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			cur_id = else_id
			continue
		}
		else_type := tc.branch_tail_type(else_id)
		return choose_if_tail_type(result, else_type)
	}
	return result
}

// choose_if_tail_type supports choose if tail type handling for types.
fn choose_if_tail_type(current Type, next Type) Type {
	if current is Void {
		return next
	}
	if next is Void {
		return current
	}
	if inferred := inferred_contextual_if_type(current, next) {
		return inferred
	}
	if current !is Primitive {
		return current
	}
	if next !is Primitive {
		return next
	}
	return current
}

// branch_tail_expr_id returns the value-producing tail expression of a branch
// body (a `block` or `match_branch`), unwrapping a trailing `expr_stmt`. Returns
// `empty_node` when the branch has no expression tail.
fn (tc &TypeChecker) branch_tail_expr_id(id flat.NodeId) flat.NodeId {
	if !tc.valid_node_id(id) {
		return flat.empty_node
	}
	node := tc.a.nodes[int(id)]
	mut last_id := flat.empty_node
	if node.kind == .block {
		if node.children_count == 0 {
			return flat.empty_node
		}
		last_id = tc.a.child(&node, node.children_count - 1)
	} else if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return flat.empty_node
		}
		last_id = tc.a.child(&node, node.children_count - 1)
	} else {
		return id
	}
	if !tc.valid_node_id(last_id) {
		return flat.empty_node
	}
	last := tc.a.nodes[int(last_id)]
	if last.kind == .expr_stmt {
		if last.children_count > 0 {
			return tc.a.child(&last, 0)
		}
		return flat.empty_node
	}
	return last_id
}

// branches_compatible_with propagates `expected` into every value-producing tail
// of a match/if expression (so context-dependent tails such as enum shorthand
// `.foo`, `none`, or fn literals type against it instead of defaulting to e.g.
// `int`). Returns true when the node is a match/if expression and every branch
// tail is compatible with `expected`.
fn (mut tc TypeChecker) branches_compatible_with(id flat.NodeId, expected Type) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .match_stmt {
		mut saw_branch := false
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(&node, i)
			if !tc.valid_node_id(branch_id) {
				continue
			}
			if tc.a.nodes[int(branch_id)].kind != .match_branch {
				continue
			}
			tail := tc.branch_tail_expr_id(branch_id)
			if !tc.valid_node_id(tail) {
				return false
			}
			saw_branch = true
			if !tc.branch_failure_literal_matches_context(tail, expected) {
				return false
			}
			actual := tc.resolve_expr(tail, expected)
			if !tc.if_branch_type_compatible_with_context(actual, tail, expected) {
				return false
			}
		}
		return saw_branch
	}
	if node.kind == .if_expr {
		// A value if-expression needs an else branch (child 2). children: cond,
		// then-block, else (block or nested if_expr).
		if node.children_count <= 2 {
			return false
		}
		then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
		if !tc.valid_node_id(then_tail) {
			return false
		}
		if !tc.branch_failure_literal_matches_context(then_tail, expected) {
			return false
		}
		then_actual := tc.resolve_expr(then_tail, expected)
		if !tc.if_branch_type_compatible_with_context(then_actual, then_tail, expected) {
			return false
		}
		else_id := tc.a.child(&node, 2)
		if !tc.valid_node_id(else_id) {
			return false
		}
		if tc.a.nodes[int(else_id)].kind == .if_expr {
			return tc.branches_compatible_with(else_id, expected)
		}
		else_tail := tc.branch_tail_expr_id(else_id)
		if !tc.valid_node_id(else_tail) {
			return false
		}
		if !tc.branch_failure_literal_matches_context(else_tail, expected) {
			return false
		}
		else_actual := tc.resolve_expr(else_tail, expected)
		return tc.if_branch_type_compatible_with_context(else_actual, else_tail, expected)
	}
	return false
}

// extract_smartcasts supports extract smartcasts handling for TypeChecker.
fn (tc &TypeChecker) extract_smartcasts(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count > 0 {
		expr_id := tc.a.child(&cond, 0)
		key := tc.expr_key(expr_id)
		if key.len > 0 && valid_string_data(key) && cond.value.len > 0 {
			mut result := []LocalBinding{}
			result << LocalBinding{
				name: key
				typ:  tc.parse_type(cond.value)
			}
			return result
		}
	}
	// `x != none` (or `!= nil` for `?&T`) unwraps the option expr inside the then-branch.
	if cond.kind == .infix && cond.op == .ne && cond.children_count >= 2 {
		if binding := tc.option_none_cmp_binding(cond) {
			return [binding]
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		mut result := tc.extract_smartcasts(tc.a.child(&cond, 0))
		result << tc.extract_smartcasts(tc.a.child(&cond, 1))
		return result
	}
	return []LocalBinding{}
}

// option_none_cmp_binding matches a `x != none` / `x == none` (also `nil`)
// comparison where x is an option and returns the unwrapped binding for x.
fn (tc &TypeChecker) option_none_cmp_binding(cond flat.Node) ?LocalBinding {
	lhs_id := tc.a.child(&cond, 0)
	rhs_id := tc.a.child(&cond, 1)
	lhs := tc.a.nodes[int(lhs_id)]
	rhs := tc.a.nodes[int(rhs_id)]
	mut opt_id := flat.NodeId(-1)
	if rhs.kind == .none_expr || rhs.kind == .nil_literal {
		opt_id = lhs_id
	} else if lhs.kind == .none_expr || lhs.kind == .nil_literal {
		opt_id = rhs_id
	}
	if int(opt_id) < 0 {
		return none
	}
	key := tc.expr_key(opt_id)
	if key.len == 0 || !valid_string_data(key) {
		return none
	}
	opt_type := tc.expr_type(opt_id) or { return none }
	if opt_type is OptionType {
		base := opt_type.base_type
		if base !is Void && base !is Unknown {
			return LocalBinding{
				name: key
				typ:  base
			}
		}
	}
	return none
}

// extract_else_branch_smartcasts returns bindings that apply to the ELSE branch:
// `if x == none { ... } else { <x unwrapped here> }`.
fn (tc &TypeChecker) extract_else_branch_smartcasts(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .infix && cond.op == .eq && cond.children_count >= 2 {
		if binding := tc.option_none_cmp_binding(cond) {
			return [binding]
		}
	}
	return []LocalBinding{}
}

// check_struct_init validates check struct init state for types.
fn (mut tc TypeChecker) check_struct_init(id flat.NodeId, node flat.Node) {
	init_type := tc.parse_type(node.value)
	if init_type is Struct {
		init_name := tc.struct_init_field_lookup_name(node.value, init_type.name)
		fields := tc.struct_fields_for_init(init_name)
		for i in 0 .. node.children_count {
			field_id := tc.a.child(&node, i)
			field := tc.a.nodes[int(field_id)]
			if field.kind != .field_init || field.children_count == 0 {
				tc.check_node(field_id)
				continue
			}
			value_id := tc.a.child(&field, 0)
			// A method value stored in a struct field escapes the evaluation site (several
			// instances from the same `Foo{cb: obj.method}` site would share one receiver).
			tc.reject_stored_method_value(value_id)
			tc.reject_stored_capturing_fn_literal(value_id)
			mut expected := Type(void_)
			if field.value.len > 0 {
				mut found := false
				if typ := tc.struct_field_type(init_name, field.value) {
					expected = typ
					found = true
				}
				if !found && tc.should_diagnose(field_id) && fields.len > 0 {
					tc.record_error(.unknown_field,
						'unknown field `${field.value}` in `${init_name}`', field_id)
				}
			} else if i < fields.len {
				expected = fields[i].typ
			}
			tc.check_node(value_id)
			$if ownership ? {
				if !tc.ownership_aggregate_consumption_deferred(id) {
					tc.ownership_consume_expr(value_id, 'struct field', value_id)
				}
			}
			if expected !is Void {
				actual := tc.resolve_expr(value_id, expected)
				if !tc.expr_compatible(value_id, actual, expected)
					&& !tc.pointer_value_compatible(actual, expected) {
					tc.type_mismatch(.assignment_mismatch,
						'cannot initialize field `${field.value}` with `${actual.name()}`; expected `${expected.name()}`',
						field_id)
				}
			}
		}
		return
	}
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	_ = id
}

// expr_is_method_value reports whether `id` is a selector that resolves to a *method
// value* — a struct method used as a value (`obj.draw`), not a field access or a method
// call. cgen backs such a value with a per-evaluation-site static receiver slot, which is
// only safe for an immediately-consumed callback; it cannot hold several live instances
// from the same site at once (see gen_method_value_closure).
fn (tc &TypeChecker) expr_is_method_value(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	// A local bound to a method value (`cb := c.report`) carries the same escape hazard as the
	// bare selector, so a reference to it counts as a method value here too.
	if node.kind == .ident {
		return node.value in tc.method_value_locals
	}
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	clean := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	if clean !is Struct {
		return false
	}
	sname := (clean as Struct).name
	if tc.struct_field_type(sname, node.value) != none {
		return false
	}
	if '${sname}.${node.value}' in tc.fn_param_types {
		return true
	}
	if _ := tc.resolve_generic_struct_method(sname, node.value) {
		return true
	}
	return false
}

// reject_stored_method_value reports a clear error when a method value escapes its
// evaluation site — stored in an array/map/struct field or returned. The per-site static
// receiver slot cannot keep several live instances distinct, so a factory like
// `fn bind(c Counter) fn () int { return c.report }` (or storage in a loop) would make
// every escaped callback use the last receiver; without this the value also reaches cgen
// and emits C referencing an unsupported helper. Pass method values directly as a
// callback argument instead (a real closure capture is not yet supported).
fn (mut tc TypeChecker) reject_stored_method_value(id flat.NodeId) {
	if tc.expr_is_method_value(id) && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'a method value (`obj.method`) cannot escape its call site (no closure capture); pass it directly as a callback argument',
			id)
	}
}

fn (tc &TypeChecker) expr_is_capturing_fn_literal_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.fn_literal {
			return tc.fn_literal_has_captures(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_is_capturing_fn_literal_value(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) fn_literal_has_captures(node flat.Node) bool {
	if node.kind != .fn_literal {
		return false
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) reject_stored_capturing_fn_literal(id flat.NodeId) {
	if tc.expr_is_capturing_fn_literal_value(id) && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'a capturing fn literal cannot be stored or returned (no closure capture); pass it directly as a callback argument',
			id)
	}
}

// check_selector validates check selector state for types.
fn (mut tc TypeChecker) check_selector(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	base := tc.a.nodes[int(base_id)]
	if tc.is_namespace_selector(node, base) {
		if typ := tc.enum_selector_type(&node) {
			tc.register_synth_type(id, typ)
		}
		return
	}
	tc.check_storage_path_base_node(base_id)
	base_type := tc.resolve_type(base_id)
	tc.register_synth_type(base_id, base_type)
	if smart_type := tc.smartcast_type(id) {
		tc.register_synth_type(id, smart_type)
		return
	}
	// A value-context selector whose name is a method (not a field) of a struct
	// receiver is a method value; record the concrete `Type.method` so it survives
	// dead-code elimination (cgen emits a wrapper that calls it).
	clean_recv := unwrap_pointer(base_type)
	if clean_recv is Struct {
		if tc.struct_field_type(clean_recv.name, node.value) == none {
			mut mkey := '${clean_recv.name}.${node.value}'
			if mkey !in tc.fn_param_types {
				// Generic receiver methods are registered under the open key
				// (`Box[T].method`); mark that one reachable for the wrapper, and stash
				// the substituted signature for cgen (the open form is gone by cgen time).
				if ci := tc.resolve_generic_struct_method(clean_recv.name, node.value) {
					tc.generic_method_value_info['${clean_recv.name}.${node.value}'] = ci
					mkey = ci.name
				}
			}
			if mkey in tc.fn_param_types && tc.cur_fn_node_id >= 0 {
				// Record per enclosing function so markused marks it only when that
				// function is reachable; over-marking an unreachable method value can pull
				// in (and fail to compile) an otherwise-unused specialization. A method
				// value can only appear inside a function body — escaping to a const/global
				// is rejected elsewhere — so a non-fn context needs no recording here.
				tc.method_values_by_fn[tc.cur_fn_node_id] << mkey
				// Also record the concrete instance key (`Box[int].report`) — distinct from the
				// open key (`Box[T].report`) above — so monomorphize can gate a generic method's
				// specialization on *this* instance's method value being reachable (it shares the
				// open key with every other instance, e.g. `Box[Pair]`).
				concrete_mkey := '${clean_recv.name}.${node.value}'
				if concrete_mkey != mkey {
					tc.method_values_by_fn[tc.cur_fn_node_id] << concrete_mkey
				}
			}
		}
	}
	if typ := tc.selector_type(id, node) {
		tc.register_synth_type(id, typ)
		return
	} else {
		if tc.should_diagnose(id) {
			if base_type is Unknown {
				return
			}
			tc.record_error(.unknown_field,
				'unknown field `${node.value}` on `${base_type.name()}`', id)
		}
	}
}

// is_namespace_selector reports whether is namespace selector applies in types.
fn (tc &TypeChecker) is_namespace_selector(node flat.Node, base flat.Node) bool {
	if base.kind != .ident {
		return false
	}
	if base.value == 'C' || tc.has_active_import(base.value) {
		return true
	}
	if resolved := tc.resolve_selective_import_type_symbol(base.value) {
		if resolved in tc.structs || resolved in tc.enum_names || resolved in tc.flag_enums
			|| resolved in tc.sum_types || resolved in tc.interface_names {
			return true
		}
	}
	qbase := tc.qualify_name(base.value)
	if qbase in tc.structs || qbase in tc.enum_names || qbase in tc.sum_types
		|| qbase in tc.interface_names {
		return true
	}
	// An alias of an enum (`type Col = Color`) is a namespace for its members: `Col.member`.
	if _ := tc.resolve_enum_name(base.value) {
		return true
	}
	qname := '${qbase}.${node.value}'
	return qname in tc.const_types || qname in tc.fn_ret_types || qname in tc.enum_names
}

// selector_type supports selector type handling for TypeChecker.
fn (tc &TypeChecker) selector_type(_id flat.NodeId, node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	if typ := tc.enum_selector_type(&node) {
		return typ
	}
	base_id := tc.a.child(&node, 0)
	base_node := tc.a.nodes[int(base_id)]
	if base_node.kind == .typeof_expr {
		if node.value == 'name' {
			return Type(String{})
		}
		if node.value == 'idx' {
			return Type(int_)
		}
	}
	base_type := tc.resolve_type(base_id)
	clean0 := unwrap_pointer(base_type)
	mut alias_receiver_name := ''
	if clean0 is Alias {
		alias_receiver_name = clean0.name
	}
	clean := unalias_and_unwrap_pointer_type(base_type)
	clean_name := clean.name()
	if typ := option_result_selector_type(clean, node.value) {
		return typ
	}
	if node.value == 'len' {
		if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
			return Type(int_)
		}
	}
	if clean is Channel && node.value == 'closed' {
		return Type(bool_)
	}
	if clean is Channel && node.value in ['len', 'cap'] {
		return Type(int_)
	}
	if clean is Struct {
		if typ := tc.struct_field_type(clean_name, node.value) {
			return typ
		}
		if typ := tc.method_value_type(clean_name, node.value) {
			return typ
		}
	}
	if alias_receiver_name.len > 0 {
		if typ := tc.method_value_type(alias_receiver_name, node.value) {
			return typ
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return typ
		}
		if typ := tc.method_value_type(clean.name, node.value) {
			return typ
		}
	}
	if clean is MultiReturn {
		if typ := multi_return_selector_type(clean, node.value) {
			return typ
		}
	}
	if is_builtin_ierror_name(clean_name) {
		if node.value == 'message' {
			return Type(String{})
		}
		if node.value == '_object' {
			return tc.parse_type('voidptr')
		}
	}
	if clean is SumType {
		if typ := tc.lowered_sum_selector_type(clean, node.value) {
			return typ
		}
		if typ := tc.sum_shared_field_type(clean, node.value) {
			return typ
		}
	}
	if clean is Array || clean is Map || clean is String {
		sname := if clean is Array {
			'array'
		} else if clean is Map {
			'map'
		} else {
			'string'
		}
		if fields := tc.structs[sname] {
			for f in fields {
				if f.name == node.value {
					return f.typ
				}
			}
		}
	}
	return none
}

// multi_return_selector_type supports multi return selector type handling for types.
fn multi_return_selector_type(typ MultiReturn, field string) ?Type {
	if !field.starts_with('arg') || field.len <= 3 {
		return none
	}
	idx_str := field[3..]
	idx := idx_str.int()
	if idx_str != idx.str() || idx < 0 || idx >= typ.types.len {
		return none
	}
	return typ.types[idx]
}

// lowered_sum_selector_type supports lowered sum selector type handling for TypeChecker.
fn (tc &TypeChecker) lowered_sum_selector_type(sum SumType, field string) ?Type {
	if field == 'typ' {
		return Type(int_)
	}
	variants := tc.sum_types[sum.name] or { return none }
	for variant in variants {
		short := if variant.contains('.') { variant.all_after_last('.') } else { variant }
		if field == variant || field == short || field == tc.cached_c_name(variant) {
			return tc.parse_type(variant)
		}
	}
	return none
}

// sum_shared_field_type supports sum shared field type handling for TypeChecker.
fn (tc &TypeChecker) sum_shared_field_type(sum SumType, field string) ?Type {
	mut visited := map[string]bool{}
	return tc.sum_shared_field_type_inner(sum.name, field, mut visited)
}

fn (tc &TypeChecker) sum_shared_field_type_inner(sum_name string, field string, mut visited map[string]bool) ?Type {
	base := tc.sum_base_name(sum_name)
	if visited[base] {
		return none
	}
	visited[base] = true
	defer {
		visited.delete(base)
	}
	variants := tc.sum_types[base] or { return none }
	if variants.len == 0 {
		return none
	}
	mut has_common := false
	mut common_typ := Type(void_)
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(sum_name, variant)
		variant_type := tc.parse_type(concrete)
		variant_field := if variant_type is SumType {
			tc.sum_shared_field_type_inner(variant_type.name, field, mut visited) or { return none }
		} else {
			tc.struct_field_type(concrete, field) or { return none }
		}
		if !has_common {
			common_typ = variant_field
			has_common = true
			continue
		}
		if !tc.type_compatible(variant_field, common_typ)
			|| !tc.type_compatible(common_typ, variant_field) {
			return none
		}
	}
	return common_typ
}

// option_result_selector_type supports option result selector type handling for types.
fn option_result_selector_type(typ Type, field string) ?Type {
	if typ is OptionType {
		if field == 'ok' {
			return Type(bool_)
		}
		if field == 'value' {
			return typ.base_type
		}
	}
	if typ is ResultType {
		if field == 'ok' {
			return Type(bool_)
		}
		if field == 'value' {
			return typ.base_type
		}
	}
	return none
}

// check_index validates check index state for types.
fn (mut tc TypeChecker) check_index(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	tc.check_storage_path_base_node(base_id)
	base_type_raw := tc.resolve_type(base_id)
	mut base_type := unalias_and_unwrap_pointer_type(base_type_raw)
	if base_type is OptionType {
		base_type = unalias_type(base_type.base_type)
	}
	if node.value == 'range' {
		if !(base_type is Array || base_type is ArrayFixed || base_type is String)
			&& tc.should_diagnose(id) {
			tc.record_error(.cannot_index, 'cannot slice `${base_type.name()}`', id)
		}
		for i in 1 .. node.children_count {
			bound_id := tc.a.child(&node, i)
			if int(bound_id) >= 0 {
				bound := tc.a.nodes[int(bound_id)]
				if bound.kind == .empty {
					continue
				}
				tc.check_node(bound_id)
				bound_type := unalias_type(tc.resolve_type(bound_id))
				if bound_type !is Unknown && !bound_type.is_integer() {
					tc.type_mismatch(.cannot_index,
						'slice bound must be integer, not `${bound_type.name()}`', bound_id)
				}
			}
		}
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if node.children_count > 2 {
		for i in 2 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.record_error(.cannot_index,
			'index expression accepts one index, got ${node.children_count - 1}', id)
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if node.children_count > 1 {
		index_id := tc.a.child(&node, 1)
		tc.check_node(index_id)
		if base_type is Map {
			actual_key := tc.resolve_expr(index_id, base_type.key_type)
			if !tc.type_compatible(actual_key, base_type.key_type) {
				tc.type_mismatch(.cannot_index,
					'map key must be `${base_type.key_type.name()}`, not `${actual_key.name()}`',
					index_id)
			}
			tc.register_synth_type(id, base_type.value_type)
			return
		}
		index_type := unalias_type(tc.resolve_type(index_id))
		if index_type !is Unknown && !index_type.is_integer() {
			tc.type_mismatch(.cannot_index, 'index must be integer, not `${index_type.name()}`',
				index_id)
		}
	}
	if !(base_type is Array || base_type is ArrayFixed || base_type is String
		|| base_type is Map || base_type is Unknown || base_type_raw is Pointer)
		&& tc.should_diagnose(id) {
		tc.record_error(.cannot_index, 'cannot index `${base_type.name()}`', id)
	}
	tc.register_synth_type(id, tc.resolve_index_type(node))
}

// check_ident validates check ident state for types.
fn (mut tc TypeChecker) check_ident(id flat.NodeId, node flat.Node) {
	if node.value.len == 0 || node.value == '_' {
		return
	}
	$if ownership ? {
		tc.ownership_check_ident(id, node)
	}
	if is_bare_generic_param(node.value) {
		tc.register_synth_type(id, unknown_type('generic placeholder `${node.value}`'))
		return
	}
	if typ := tc.cur_scope.lookup(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.file_scope.lookup(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	qname := tc.qualify_name(node.value)
	if qname != node.value {
		if typ := tc.file_scope.lookup(qname) {
			tc.register_synth_type(id, typ)
			return
		}
	}
	if node.value == 'err' {
		tc.register_synth_type(id, tc.parse_type('IError'))
		return
	}
	if typ := tc.const_types[qname] {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.const_types[node.value] {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.fn_value_type(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	if tc.selective_import_symbol_is_ambiguous(node.value) {
		tc.register_synth_type(id, unknown_type('ambiguous selective import `${node.value}`'))
		return
	}
	if node.value in tc.fn_ret_types || tc.qualify_fn_name(node.value) in tc.fn_ret_types {
		return
	}
	if tc.has_active_import(node.value) || qname in tc.structs || qname in tc.enum_names
		|| qname in tc.sum_types || qname in tc.interface_names {
		return
	}
	if tc.should_diagnose(id) {
		tc.record_error(.unknown_ident, 'unknown identifier `${node.value}`', id)
	}
}

// resolve_expr resolves resolve expr information for types.
fn (mut tc TypeChecker) resolve_expr(id flat.NodeId, expected Type) Type {
	if int(id) < 0 {
		return unknown_type('missing expression')
	}
	expected_raw := expected
	node := tc.a.nodes[int(id)]
	if node.kind == .field_init && node.children_count > 0 {
		return tc.resolve_expr(tc.a.child(&node, 0), expected)
	}
	if node.kind == .none_expr {
		if expected is OptionType || expected is ResultType || is_ierror_type(expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
		if is_ierror_type(expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
	}
	if payload := contextual_payload_type(expected) {
		actual := tc.resolve_expr(id, payload)
		if tc.type_compatible(actual, payload) {
			return actual
		}
	}
	if node.kind == .enum_val {
		// The expected type may be the enum directly, or an option/result wrapper around
		// it (`?Enum` / `!Enum`), e.g. `mut field ?LoggingMode` assigned `field = .debug`.
		// Unwrap the option/result payload so the shorthand resolves against the inner
		// enum. On success the node is typed as the *inner* enum (not the wrapper): a bare
		// enum value assigned into an option is auto-wrapped by the assignment/return
		// machinery, exactly like any other bare value assigned into an option, so the
		// node itself must stay unwrapped for codegen to emit the wrap.
		mut enum_expected := expected
		if payload := contextual_payload_type(expected) {
			enum_expected = payload
		}
		if enum_expected is Enum {
			if tc.enum_value_matches(node.value, enum_expected.name) {
				tc.register_synth_type(id, enum_expected)
				return enum_expected
			}
			tc.type_mismatch(.assignment_mismatch,
				'unknown enum field `${node.value}` for `${enum_expected.name}`', id)
			return Type(int_)
		}
	}
	// When several anonymous structs share the same field names, the parser leaves
	// the literal unresolved instead of choosing the most recently declared shape.
	// Its call/assignment/return context supplies the exact anonymous struct here.
	if node.kind == .struct_init && node.value == 'struct' && expected !is Pointer
		&& tc.anonymous_struct_literal_compatible(node, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	// A bare generic struct literal (`Box{...}` / `&Box{...}`) adopts a matching concrete
	// expected instance (`Box[int]` / `&Box[int]`), so `fn make() Box[int] { return
	// Box{...} }` and bare literals passed/assigned where a concrete instance is expected
	// type-check and carry the concrete type into codegen. A *value* literal only adopts a
	// *value* expectation: `bare_generic_literal_adopts` unwraps the pointer, so without
	// the `expected !is Pointer` guard `return Box{...}` would be accepted for an expected
	// `&Box[int]`, and cgen would emit a `Box_int` value where a `Box_int*` is required.
	// The pointer case is the `prefix .amp` (`&Box{...}`) path below.
	if node.kind == .struct_init && expected !is Pointer
		&& tc.bare_generic_literal_adopts(node.value, expected)
		&& tc.generic_literal_fields_compatible(node, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 && expected is Pointer {
		child_id := tc.a.child(&node, 0)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .struct_init && child.value == 'struct'
			&& tc.anonymous_struct_literal_compatible(child, expected.base_type) {
			tc.register_synth_type(child_id, expected.base_type)
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if child.kind == .struct_init && tc.bare_generic_literal_adopts(child.value, expected)
			&& tc.generic_literal_fields_compatible(child, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .postfix && node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		child := tc.a.nodes[int(child_id)]
		if node.op == .not && child.kind == .array_literal {
			array_expected := if expected is OptionType {
				expected.base_type
			} else if expected is ResultType {
				expected.base_type
			} else {
				expected
			}
			actual := tc.resolve_expr(child_id, array_expected)
			if tc.receiver_compatible(actual, array_expected) {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .array_literal {
		mut elem_expected := Type(void_)
		mut expected_is_array := false
		if expected is Array {
			elem_expected = array_elem_type(expected)
			expected_is_array = true
		} else if expected is ArrayFixed {
			elem_expected = fixed_array_elem_type(expected)
			expected_is_array = true
		}
		if expected_is_array {
			// Empty literal `[]` simply adopts the expected array type.
			if node.children_count == 0 {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			// Non-empty literal: propagate the expected element type down to each
			// element so context-dependent elements (enum shorthand `[.foo]`, `none`,
			// fn literals) type against it instead of defaulting to a fixed `int[N]`.
			// Adopt the expected array type when every element fits.
			mut all_ok := true
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				elem_actual := tc.resolve_expr(child_id, elem_expected)
				if !tc.expr_receiver_compatible(child_id, elem_actual, elem_expected) {
					all_ok = false
					break
				}
			}
			// A fixed-array expectation additionally requires the literal to have
			// exactly the expected number of elements; otherwise `[1, 2]` would be
			// accepted as e.g. `[4]int` and the C backend would copy/read past the
			// compound literal. Element-type propagation above still happens either
			// way; only the type adoption is gated. Unresolvable const lengths stay
			// lenient (we cannot verify them here).
			if all_ok && expected is ArrayFixed {
				if expected_len := tc.fixed_array_len_value(expected) {
					if node.children_count != expected_len {
						all_ok = false
					}
				}
			}
			if all_ok {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .map_init {
		if expected_map := map_type_from_receiver(expected) {
			if node.children_count == 0 {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			mut all_ok := true
			mut i := 0
			for i < node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
					update_actual := map_type_from_receiver(tc.resolve_type(tc.a.child(&child, 0))) or {
						all_ok = false
						break
					}
					if !tc.receiver_compatible(update_actual.key_type, expected_map.key_type)
						|| !tc.receiver_compatible(update_actual.value_type, expected_map.value_type) {
						all_ok = false
						break
					}
					i += 2
					continue
				}
				if i + 1 >= node.children_count {
					all_ok = false
					break
				}
				key_actual := tc.resolve_expr(tc.a.child(&node, i), expected_map.key_type)
				value_actual := tc.resolve_expr(tc.a.child(&node, i + 1), expected_map.value_type)
				if !tc.expr_receiver_compatible(tc.a.child(&node, i), key_actual, expected_map.key_type)
					|| !tc.expr_receiver_compatible(tc.a.child(&node, i + 1), value_actual, expected_map.value_type) {
					all_ok = false
					break
				}
				i += 2
			}
			if all_ok {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .match_stmt || node.kind == .if_expr {
		// Match/if used as a value expression: propagate the expected type into
		// each branch tail so enum-shorthand / `none` / fn-literal tails type
		// against it (e.g. `return match s { 'a' { .foo } ... }` with an enum
		// return type), then adopt the expected type when every branch fits.
		if expected !is Void && expected !is Unknown && tc.branches_compatible_with(id, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if _ := fn_type_from_type(expected) {
		if _ := tc.resolve_fn_value_name_for_expected(id, expected_raw) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .fn_literal || node.kind == .lambda_expr {
		if _ := fn_type_from_type(expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if tc.expr_is_unsafe_nil(id) {
		if _ := fn_type_from_type(expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if expected is Pointer {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	actual := tc.resolve_type(id)
	if tc.type_compatible(actual, expected) {
		if actual.name() == expected.name() {
			tc.register_synth_type(id, expected)
			return expected
		}
		if expected is OptionType || expected is ResultType {
			if actual is OptionType || actual is ResultType {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			return actual
		}
		if expected is SumType || expected is Enum {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if tc.expr_generic_expected_match(id, actual, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	return actual
}

// fn_value_match_key returns the single exact function declaration key accepted for a function value.
fn (tc &TypeChecker) fn_value_match_key(node flat.Node, expected Type) ?string {
	expected_fn := fn_type_from_type(expected) or { return none }
	key := tc.fn_value_key(node) or { return none }
	actual := tc.fn_type_from_key(key) or { return none }
	if actual is FnType {
		if actual.params.len != expected_fn.params.len {
			return none
		}
		for i in 0 .. actual.params.len {
			actual_param := fn_param_type(actual, i)
			expected_param := fn_param_type(expected_fn, i)
			if !tc.fn_param_compatible(actual_param, expected_param) {
				return none
			}
		}
		if tc.fn_return_compatible(actual.return_type, expected_fn.return_type) {
			return key
		}
	}
	return none
}

// fn_value_key resolves a function value expression to one exact function declaration key.
fn (tc &TypeChecker) fn_value_key(node flat.Node) ?string {
	if node.kind == .ident {
		return tc.ident_fn_value_key(node.value)
	}
	if node.kind == .selector {
		return tc.selector_fn_value_key(node)
	}
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return tc.fn_value_key(tc.a.child_node(&node, 0))
	}
	return none
}

fn (tc &TypeChecker) ident_fn_value_key(name string) ?string {
	if local_name := tc.local_bare_fn_signature_key(name) {
		return local_name
	}
	if imported_name := tc.resolve_selective_import_symbol(name) {
		return imported_name
	}
	if tc.fn_signature_known(name) {
		return name
	}
	return none
}

fn (tc &TypeChecker) selector_fn_value_key(node flat.Node) ?string {
	if node.children_count == 0 || !valid_string_data(node.value) {
		return none
	}
	base := tc.a.child_node(&node, 0)
	if base.kind == .ident {
		if base.value == 'C' {
			key := 'C.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			return none
		}
		if tc.ident_resolves_to_value(base.value) {
			return none
		}
		if resolved_mod := tc.resolve_import_alias(base.value) {
			key := '${resolved_mod}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			return none
		}
		qbase := tc.qualify_name(base.value)
		key := '${qbase}.${node.value}'
		if tc.fn_signature_known(key) && (qbase in tc.structs
			|| qbase in tc.enum_names || qbase in tc.sum_types
			|| qbase in tc.interface_names) {
			return key
		}
		return none
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			key := '${mod_name}.${base.value}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
		}
	}
	return none
}

fn (tc &TypeChecker) fn_signature_known(key string) bool {
	return key in tc.fn_ret_types && key in tc.fn_param_types
}

fn (tc &TypeChecker) expr_is_unsafe_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .block {
		return false
	}
	return tc.expr_tail_is_nil(id)
}

fn (tc &TypeChecker) expr_tail_is_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.nil_literal {
			return true
		}
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_nil(tc.a.child(&node, 0))
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_nil(tc.a.child(&node, node.children_count - 1))
		}
		else {
			return false
		}
	}
}

// fn_value_type supports fn value type handling for TypeChecker.
fn (tc &TypeChecker) fn_value_type(name string) ?Type {
	if local_name := tc.local_bare_fn_key(name) {
		return tc.fn_type_from_key(local_name)
	}
	if imported_name := tc.resolve_selective_import_symbol(name) {
		return tc.fn_type_from_key(imported_name)
	}
	if name in tc.fn_ret_types {
		return tc.fn_type_from_key(name)
	}
	return none
}

// fn_type_from_key converts fn type from key data for types.
fn (tc &TypeChecker) fn_type_from_key(key string) ?Type {
	params := tc.fn_param_types[key] or { return none }
	ret := tc.fn_ret_types[key] or { return none }
	return Type(FnType{
		params:      params.clone()
		return_type: ret
	})
}

// struct_field_c_abi_fn_ptr_type returns the C ABI function-pointer type for a struct field.
pub fn (tc &TypeChecker) struct_field_c_abi_fn_ptr_type(struct_name string, field_name string) ?string {
	key := struct_field_c_abi_key(struct_name, field_name)
	if typ := tc.struct_field_c_abi_fns[key] {
		return typ
	}
	return none
}

// enum_value_matches supports enum value matches handling for TypeChecker.
fn (tc &TypeChecker) enum_value_matches(value string, enum_name string) bool {
	if value.starts_with('.') {
		return tc.enum_has_field(enum_name, value[1..])
	}
	if value.contains('.') {
		prefix := value.all_before_last('.')
		field := value.all_after_last('.')
		if prefix != enum_name && short_type_name(prefix) != short_type_name(enum_name) {
			return false
		}
		return tc.enum_has_field(enum_name, field)
	}
	return tc.enum_has_field(enum_name, value)
}

// enum_has_field converts enum has field data for types.
fn (tc &TypeChecker) enum_has_field(enum_name string, field string) bool {
	fields := tc.enum_fields[enum_name] or { return false }
	return field in fields
}

// resolve_enum_name resolves resolve enum name information for types.
fn (tc &TypeChecker) resolve_enum_name(name string) ?string {
	if name in tc.enum_names {
		return name
	}
	qname := tc.qualify_name(name)
	if qname in tc.enum_names {
		return qname
	}
	if !name.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(name) {
			if resolved in tc.enum_names || resolved in tc.flag_enums {
				return resolved
			}
			if target := tc.resolve_enum_alias_target(resolved) {
				return target
			}
		}
	}
	if target := tc.resolve_enum_alias_target(name) {
		return target
	}
	if target := tc.resolve_enum_alias_target(qname) {
		return target
	}
	return none
}

fn (tc &TypeChecker) resolve_enum_alias_target(name string) ?string {
	mut cur := name
	for _ in 0 .. 16 {
		target := tc.alias_target_type_text(cur) or { return none }
		if target == cur {
			return none
		}
		if target in tc.enum_names || target in tc.flag_enums {
			return target
		}
		cur = target
	}
	return none
}

// enum_selector_type supports enum selector type handling for TypeChecker.
fn (tc &TypeChecker) enum_selector_type(node &flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := tc.a.child_node(node, 0)
	mut enum_name := ''
	if base.kind == .ident {
		enum_name = tc.resolve_enum_name(base.value) or { '' }
	} else if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			enum_name = tc.resolve_enum_name('${mod_name}.${base.value}') or { '' }
		}
	}
	if enum_name.len == 0 || !tc.enum_has_field(enum_name, node.value) {
		return none
	}
	return Type(Enum{
		name:    enum_name
		is_flag: enum_name in tc.flag_enums
	})
}

// type_compatible returns type compatible data for TypeChecker.
fn (tc &TypeChecker) type_compatible(actual Type, expected Type) bool {
	actual_raw := actual
	expected_raw := expected
	if actual.name() == expected.name() {
		return true
	}
	if actual is Unknown || expected is Unknown {
		return true
	}
	if type_contains_unknown(actual) || type_contains_unknown(expected) {
		return true
	}
	if (actual.name().contains('[') || expected.name().contains('['))
		&& tc.generic_type_name_matches(actual.name(), expected.name()) {
		return true
	}
	if tc.sum_variant_type_for_pattern(expected.name(), actual.name()) != none {
		return true
	}
	if actual is Alias && expected is Alias {
		return tc.type_compatible(actual.base_type, expected.base_type)
	}
	if actual is Alias {
		if expected is Interface && tc.type_implements_interface(actual, expected) {
			return true
		}
		return tc.type_compatible(actual.base_type, expected)
	}
	if expected is Alias {
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is String && is_ierror_type(actual) {
		return true
	}
	if actual is Array && is_runtime_array_type(expected) {
		return true
	}
	if actual is Array && expected is Array && actual.elem_type is Void {
		return true
	}
	if is_runtime_array_type(actual) && expected is Array {
		return true
	}
	if actual is None {
		return expected is OptionType || expected is ResultType || is_ierror_type(expected)
	}
	if is_option_void_type(actual) && is_ierror_type(expected) {
		return true
	}
	if expected is OptionType {
		if actual is OptionType {
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is ResultType {
		if actual is ResultType {
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	expected_named_interface := expected.name()
	if expected_named_interface in tc.interface_names {
		return tc.type_implements_interface(actual, Interface{
			name: expected_named_interface
		})
	}
	if expected is SumType {
		return tc.type_matches_sum(actual_raw, expected_raw)
	}
	if expected is Interface {
		return tc.type_implements_interface(actual, expected)
	}
	if expected is Enum && (expected.is_flag || expected.name in tc.flag_enums)
		&& actual is Primitive && actual.props.has(.integer) {
		return true
	}
	if actual is Interface {
		if expected is Interface {
			return tc.interface_implements_interface(actual.name, expected.name)
		}
		return false
	}
	if expected is Primitive {
		if actual is Primitive {
			if expected.props.has(.boolean) || actual.props.has(.boolean) {
				return expected.props.has(.boolean) && actual.props.has(.boolean)
			}
			if expected.props.has(.float) && actual.props.has(.integer) {
				return true
			}
			if expected.props.has(.integer) && actual.props.has(.integer) {
				return true
			}
			if expected.props.has(.float) && actual.props.has(.float) {
				return true
			}
		}
		if expected.props.has(.integer) && actual.is_integer() {
			return true
		}
	}
	if actual is Primitive && actual.props.has(.integer) && expected.is_integer() {
		return true
	}
	if expected is String {
		return actual is String || is_ierror_type(actual)
	}
	if expected is Char {
		return actual is Char || actual.name() == 'u8'
	}
	if expected is Pointer {
		if actual is Nil {
			return true
		}
		if expected.base_type is Void && actual is FnType {
			return true
		}
		if actual is Pointer {
			if expected.base_type is Interface
				&& tc.type_implements_interface(actual.base_type, expected.base_type) {
				return true
			}
			expected_base_name := expected.base_type.name()
			if expected_base_name in tc.interface_names && tc.type_implements_interface(actual.base_type, Interface{
				name: expected_base_name
			}) {
				return true
			}
			if expected.base_type is Void || actual.base_type is Void {
				return true
			}
			// C interop: `&char` and `&u8` share representation (`tos_clone(C.strdup(s))`).
			if (actual.base_type is Char && expected.base_type.name() == 'u8')
				|| (actual.base_type.name() == 'u8' && expected.base_type is Char) {
				return true
			}
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
	}
	if expected is Array {
		if actual is Array {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
		if actual is ArrayFixed {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is ArrayFixed {
		if actual is ArrayFixed {
			return tc.fixed_array_lengths_compatible(actual, expected)
				&& tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Channel {
		if actual is Channel {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Map {
		if actual is Map {
			return tc.type_compatible(actual.key_type, expected.key_type)
				&& tc.type_compatible(actual.value_type, expected.value_type)
		}
	}
	if expected is FnType {
		if actual is FnType {
			if actual.params.len != expected.params.len {
				return false
			}
			for i in 0 .. actual.params.len {
				actual_param := fn_param_type(actual, i)
				expected_param := fn_param_type(expected, i)
				if !tc.fn_param_compatible(actual_param, expected_param) {
					return false
				}
			}
			return tc.fn_return_compatible(actual.return_type, expected.return_type)
		}
	}
	return false
}

fn type_contains_unknown(typ Type) bool {
	if typ is Unknown {
		return true
	}
	if typ is Alias {
		return type_contains_unknown(typ.base_type)
	}
	if typ is Array {
		return type_contains_unknown(typ.elem_type)
	}
	if typ is ArrayFixed {
		return type_contains_unknown(typ.elem_type)
	}
	if typ is Map {
		return type_contains_unknown(typ.key_type) || type_contains_unknown(typ.value_type)
	}
	if typ is Pointer {
		return type_contains_unknown(typ.base_type)
	}
	if typ is OptionType {
		return type_contains_unknown(typ.base_type)
	}
	if typ is ResultType {
		return type_contains_unknown(typ.base_type)
	}
	if typ is FnType {
		for param in typ.params {
			if type_contains_unknown(param) {
				return true
			}
		}
		return type_contains_unknown(typ.return_type)
	}
	if typ is MultiReturn {
		for part in typ.types {
			if type_contains_unknown(part) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) fn_param_compatible(actual Type, expected Type) bool {
	if actual is Unknown || expected is Unknown {
		return false
	}
	if tc.c_type(actual) == tc.c_type(expected) {
		return true
	}
	return fn_param_can_cast_userdata_param(actual, expected)
}

fn (tc &TypeChecker) fn_return_compatible(actual Type, expected Type) bool {
	if actual.name() == expected.name() {
		return true
	}
	return fn_return_canonical_type_name(actual) == fn_return_canonical_type_name(expected)
}

fn fn_param_can_cast_userdata_param(actual Type, expected Type) bool {
	return (fn_param_is_voidptr_type(expected) && fn_param_is_nonvoid_pointer_type(actual))
		|| (fn_param_is_nonvoid_pointer_type(expected) && fn_param_is_voidptr_type(actual))
}

fn fn_param_is_voidptr_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		return base is Void
	}
	return false
}

fn fn_param_is_nonvoid_pointer_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		return base !is Void
	}
	return false
}

fn fn_param_unalias_type(typ Type) Type {
	if typ is Alias {
		return fn_param_unalias_type(typ.base_type)
	}
	return typ
}

fn fn_return_canonical_type_name(typ Type) string {
	if typ is Alias {
		return fn_return_canonical_type_name(typ.base_type)
	}
	return typ.name()
}

// is_ierror_type reports whether is ierror type applies in types.
fn is_builtin_ierror_name(name string) bool {
	return name == 'IError' || name == 'builtin.IError'
}

fn is_ierror_type(t Type) bool {
	if t is Alias {
		return is_builtin_ierror_name(t.name) || is_ierror_type(t.base_type)
	}
	if t is Pointer {
		return is_ierror_type(t.base_type)
	}
	if t is Struct {
		return is_builtin_ierror_name(t.name)
	}
	if t is Interface {
		return is_builtin_ierror_name(t.name)
	}
	return false
}

fn (tc &TypeChecker) type_embeds_error(t Type) bool {
	clean := unwrap_pointer(t)
	if clean is Alias {
		return tc.type_embeds_error(clean.base_type)
	}
	if clean is Struct {
		struct_name := clean.name
		if struct_name == 'Error' || struct_name.ends_with('.Error') {
			return true
		}
		return tc.receiver_embeds(clean, Type(Struct{
			name: 'Error'
		}))
	}
	return false
}

// is_runtime_array_type reports whether is runtime array type applies in types.
fn is_runtime_array_type(t Type) bool {
	if t is Alias {
		return is_runtime_array_type(t.base_type)
	}
	if t is Struct {
		return t.name == 'array'
	}
	return false
}

// fixed_array_lengths_compatible supports fixed array lengths compatible handling for TypeChecker.
fn (tc &TypeChecker) fixed_array_lengths_compatible(actual ArrayFixed, expected ArrayFixed) bool {
	if actual.len > 0 && expected.len > 0 {
		return actual.len == expected.len
	}
	actual_len := tc.fixed_array_len_value(actual) or {
		return actual.len_expr == expected.len_expr
	}
	expected_len := tc.fixed_array_len_value(expected) or {
		return actual.len_expr == expected.len_expr
	}
	return actual_len == expected_len
}

// fixed_array_len_value returns the evaluated fixed-array length when it can be resolved.
pub fn (tc &TypeChecker) fixed_array_len_value(arr ArrayFixed) ?int {
	if arr.len > 0 {
		return arr.len
	}
	if arr.len_expr.len == 0 {
		return none
	}
	return tc.const_int_value(arr.len_expr, []string{})
}

// const_expr_paren_wraps_whole reports whether `s` is a single parenthesised group that
// encloses the entire string (`(a + b)`), as opposed to one that only covers part of it
// (`(a) + (b)`), so a const length wrapped in redundant parentheses can be unwrapped.
fn const_expr_paren_wraps_whole(s string) bool {
	if s.len < 2 || s[0] != `(` || s[s.len - 1] != `)` {
		return false
	}
	mut depth := 0
	for i := 0; i < s.len; i++ {
		if s[i] == `(` {
			depth++
		} else if s[i] == `)` {
			depth--
			if depth == 0 {
				return i == s.len - 1
			}
		}
	}
	return false
}

// const_int_value supports const int value handling for TypeChecker.
pub fn (tc &TypeChecker) const_int_value(name string, seen []string) ?int {
	return tc.const_int_value_in_module(name, tc.cur_module, seen)
}

// const_int_value_in_module supports const int value handling for a specific module.
pub fn (tc &TypeChecker) const_int_value_in_module(name string, module_name string, seen []string) ?int {
	if name in seen {
		return none
	}
	mut candidates := []string{}
	candidates << name
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !name.contains('.') {
		candidates << '${module_name}.${name}'
	}
	qname := tc.qualify_name(name)
	if qname != name {
		candidates << qname
	}
	if key := tc.const_key_for_suffix(name) {
		candidates << key
	}
	for key in candidates {
		if key in seen {
			continue
		}
		if expr_id := tc.const_exprs[key] {
			mut next_seen := seen.clone()
			next_seen << key
			const_module := tc.const_modules[key] or { module_name }
			return tc.const_int_expr(expr_id, const_module, next_seen)
		}
	}
	if v := v_int_literal_value(name) {
		return v
	}
	// Simple const arithmetic in string form, e.g. a fixed-array size `[SEGS + 1]`,
	// `[SEGS+1]`, `[segs / 2]`, `[segs % 4]` or `[2 * (segs + 1)]`. A length wrapped
	// wholly in parentheses is the inner expression, so strip the outer pair and
	// re-evaluate. Otherwise split on the rightmost operator of the lowest precedence
	// level present (`+ -`, then `* / %`) that sits OUTSIDE any parentheses, so a nested
	// operator (the `+` inside `2 * (segs + 1)`) is not chosen; precedence and left
	// associativity hold and each side is trimmed and resolved recursively. A leading `-`
	// (unary) leaves an empty lhs and is skipped.
	expr := name.trim_space()
	if const_expr_paren_wraps_whole(expr) {
		return tc.const_int_value(expr[1..expr.len - 1].trim_space(), seen)
	}
	// Operators grouped by precedence level, lowest first, MATCHING the v3 parser's binding
	// power (token.left_binding_power) so a length text recovered from source folds to the
	// same value as the AST const evaluator below: `|` < `^` < `<< >> >>>` < `+ -` <
	// `* / % &`. The parser keeps shifts below additive operators (so `arr << a + b` appends
	// `a + b`), hence `1 << 2 + 1` groups as `1 << (2 + 1)` — split on `<<` before `+` here
	// too, not the reverse. Split on the rightmost top-level operator of the lowest level
	// present; longer operators match first (`>>>` before `>>`, two-char before one) and
	// `idx + op.len` skips the operator.
	for level in [['|'], ['^'], ['<<', '>>', '>>>'], ['+', '-'],
		['*', '/', '%', '&']] {
		mut idx := -1
		mut op := ''
		mut depth := 0
		mut i := 0
		for i < expr.len {
			ch := expr[i..i + 1]
			if ch == '(' {
				depth++
				i++
				continue
			}
			if ch == ')' {
				depth--
				i++
				continue
			}
			if depth == 0 {
				three := if i + 3 <= expr.len { expr[i..i + 3] } else { '' }
				if three.len == 3 && three in level {
					idx = i
					op = three
					i += 3
					continue
				}
				two := if i + 2 <= expr.len { expr[i..i + 2] } else { '' }
				if two.len == 2 && two in level {
					idx = i
					op = two
					i += 2
					continue
				}
				if ch in level {
					idx = i
					op = ch
				}
			}
			i++
		}
		if idx <= 0 {
			continue
		}
		lhs := expr[..idx].trim_space()
		rhs := expr[idx + op.len..].trim_space()
		if lhs.len == 0 || rhs.len == 0 {
			continue
		}
		l := tc.const_int_value_in_module(lhs, module_name, seen) or { return none }
		r := tc.const_int_value_in_module(rhs, module_name, seen) or { return none }
		if (op == '/' || op == '%') && r == 0 {
			return none
		}
		if (op == '<<' || op == '>>' || op == '>>>') && (r < 0 || r >= 64) {
			return none
		}
		return match op {
			'+' { l + r }
			'-' { l - r }
			'*' { l * r }
			'/' { l / r }
			'%' { l % r }
			'|' { l | r }
			'^' { l ^ r }
			'&' { l & r }
			'<<' { int(u64(l) << r) }
			'>>' { l >> r }
			else { int(u64(l) >> r) }
		}
	}
	return none
}

// const_int_expr supports const int expr handling for TypeChecker.
fn (tc &TypeChecker) const_int_expr(id flat.NodeId, module_name string, seen []string) ?int {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			if v := v_int_literal_value(node.value) {
				return v
			}
		}
		.ident {
			return tc.const_int_value_in_module(node.value, module_name, seen)
		}
		.paren {
			if node.children_count > 0 {
				return tc.const_int_expr(tc.a.child(&node, 0), module_name, seen)
			}
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := tc.const_int_expr(tc.a.child(&node, 0), module_name, seen) or { return none }
			return match node.op {
				.minus { -value }
				.plus { value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := tc.const_int_expr(tc.a.child(&node, 0), module_name, seen) or { return none }
			right := tc.const_int_expr(tc.a.child(&node, 1), module_name, seen) or { return none }
			match node.op {
				.plus {
					return left + right
				}
				.minus {
					return left - right
				}
				.mul {
					return left * right
				}
				.div {
					if right == 0 {
						return none
					}
					return left / right
				}
				.mod {
					if right == 0 {
						return none
					}
					return left % right
				}
				.amp {
					return left & right
				}
				.pipe {
					return left | right
				}
				.xor {
					return left ^ right
				}
				.left_shift {
					if right < 0 || right >= 64 {
						return none
					}
					return int(u64(left) << right)
				}
				.right_shift {
					if right < 0 || right >= 64 {
						return none
					}
					return left >> right
				}
				.right_shift_unsigned {
					if right < 0 || right >= 64 {
						return none
					}
					return int(u64(left) >> right)
				}
				else {
					return none
				}
			}
		}
		else {}
	}

	return none
}

// type_implements_interface returns type implements interface data for TypeChecker.
fn (tc &TypeChecker) type_implements_interface(actual Type, expected Interface) bool {
	clean := unwrap_pointer(actual)
	if clean is Unknown {
		return true
	}
	if tc.interface_has_no_requirements(expected.name) {
		return true
	}
	if clean is Interface {
		return tc.interface_implements_interface(clean.name, expected.name)
	}
	concrete_name := method_type_name(clean)
	if concrete_name.len == 0 {
		return false
	}
	return tc.named_type_implements_interface(concrete_name, expected.name)
}

// interface_implements_interface supports interface implements interface handling for TypeChecker.
fn (tc &TypeChecker) interface_implements_interface(actual_name string, expected_name string) bool {
	if actual_name == expected_name {
		return true
	}
	for method in tc.interface_method_names(expected_name) {
		actual_key := tc.interface_method_signature_key(actual_name, method) or {
			'${actual_name}.${method}'
		}
		expected_key := tc.interface_method_signature_key(expected_name, method) or {
			'${expected_name}.${method}'
		}
		if actual_key !in tc.fn_param_types
			|| !tc.method_signature_compatible(actual_key, expected_key) {
			return false
		}
	}
	for field in tc.interface_field_list(expected_name) {
		actual_field := tc.interface_field_type(actual_name, field.name) or { return false }
		if !tc.type_compatible(actual_field, field.typ)
			|| !tc.type_compatible(field.typ, actual_field) {
			return false
		}
	}
	return true
}

// named_type_implements_interface
// supports helper handling in types.
pub fn (tc &TypeChecker) named_type_implements_interface(concrete_name string, iface_name string) bool {
	if tc.interface_has_no_requirements(iface_name) {
		return true
	}
	// Only the abstract (declared) methods must be provided by the concrete type.
	// Methods defined directly on the interface (default implementations) are
	// inherited and need not be reimplemented.
	for method in tc.interface_abstract_method_names(iface_name) {
		expected_key := tc.interface_method_signature_key(iface_name, method) or {
			'${iface_name}.${method}'
		}
		if concrete_key := tc.concrete_method_signature_key(concrete_name, method) {
			if !tc.method_signature_compatible(concrete_key, expected_key) {
				return false
			}
			continue
		}
		if info := tc.resolve_generic_struct_method(concrete_name, method) {
			if !tc.method_call_info_signature_compatible(info, expected_key) {
				return false
			}
			continue
		}
		if info := tc.resolve_generic_sum_method(concrete_name, method) {
			if !tc.method_call_info_signature_compatible(info, expected_key) {
				return false
			}
			continue
		}
		return false
	}
	for field in tc.interface_field_list(iface_name) {
		actual_field := tc.struct_field_type(concrete_name, field.name) or { return false }
		if !tc.type_compatible(actual_field, field.typ)
			|| !tc.type_compatible(field.typ, actual_field) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) interface_has_no_requirements(iface_name string) bool {
	return tc.interface_abstract_method_names(iface_name).len == 0
		&& tc.interface_field_list(iface_name).len == 0
}

fn struct_decl_implements_from_typ(typ string) []string {
	mut out := []string{}
	for part in struct_decl_typ_parts(typ) {
		if !part.starts_with('implements=') {
			continue
		}
		for iface in part['implements='.len..].split('|') {
			clean := iface.trim_space()
			if clean.len > 0 {
				out << clean
			}
		}
	}
	return out
}

fn struct_decl_typ_parts(typ string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	for i, ch in typ {
		if ch == `[` {
			depth++
		} else if ch == `]` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			parts << typ[start..i]
			start = i + 1
		}
	}
	parts << typ[start..]
	return parts
}

fn marker_type_name(name string) string {
	mut clean := name.trim_space()
	base, _, ok := generic_type_application_parts(clean)
	if ok {
		clean = base
	}
	return clean
}

fn interface_marker_matches(name string, target string) bool {
	clean := marker_type_name(name)
	target_clean := marker_type_name(target)
	return clean == target_clean || clean.all_after_last('.') == target_clean
		|| clean == target_clean.all_after_last('.')
}

pub fn (tc &TypeChecker) named_type_implements_marker(concrete_name string, target string) bool {
	mut name := concrete_name.trim_space()
	if name.starts_with('&') {
		name = name[1..].trim_space()
	}
	name = marker_type_name(name)
	mut candidates := [name]
	if !name.contains('.') {
		qname := tc.qualify_name(name)
		if qname != name {
			candidates << qname
		}
	} else if name.starts_with('main.') {
		candidates << name['main.'.len..]
	}
	for candidate in candidates {
		impls := tc.struct_implements[candidate] or { continue }
		for impl_name in impls {
			if interface_marker_matches(impl_name, target) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) type_has_compiler_default_clone(t Type) bool {
	if t is Struct {
		return tc.named_type_implements_marker(t.name, 'IClone')
	}
	return false
}

// interface_impl_names returns the concrete type names (structs and type
// aliases) that implement `iface_name`, sorted by name.
pub fn (tc &TypeChecker) interface_impl_names(iface_name string) []string {
	mut candidate_set := map[string]bool{}
	if tc.interface_has_no_requirements(iface_name) {
		for name in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte',
			'u16', 'u32', 'u64', 'f32', 'f64', 'string', 'char', 'rune'] {
			candidate_set[name] = true
		}
	}
	for name, _ in tc.structs {
		candidate_set[interface_impl_candidate_name(name)] = true
	}
	for name, _ in tc.type_aliases {
		candidate_set[interface_impl_candidate_name(name)] = true
	}
	mut candidates := candidate_set.keys()
	candidates.sort()
	mut impls := []string{}
	for name in candidates {
		if tc.named_type_implements_interface(name, iface_name) {
			impls << name
		}
	}
	return impls
}

// stable_interface_type_ids assigns deterministic nonzero `_typ` dispatch IDs
// to interface implementers. Hash collisions are resolved in sorted name order
// with linear probing, so cgen and transformed `is` checks share one mapping.
pub fn stable_interface_type_ids(impl_names []string) map[string]int {
	mut names := impl_names.clone()
	names.sort()
	mut ids := map[string]int{}
	mut used := map[int]bool{}
	for name in names {
		if name in ids {
			continue
		}
		mut id := stable_interface_type_id_hash(name)
		for used[id] {
			if id == 0x7fffffff {
				id = 1
			} else {
				id++
			}
		}
		used[id] = true
		ids[name] = id
	}
	return ids
}

// interface_impl_set_signature returns the complete deterministic interface implementer set
// that controls collision-resolved dispatch IDs for the current program.
pub fn (tc &TypeChecker) interface_impl_set_signature() string {
	mut iface_names := tc.interface_names.keys()
	iface_names.sort()
	mut lines := []string{cap: iface_names.len}
	for iface_name in iface_names {
		impl_names := if iface_name in ['IError', 'builtin.IError'] {
			tc.ierror_impl_names()
		} else {
			tc.interface_impl_names(iface_name)
		}
		lines << '${iface_name}=${impl_names.join(',')}'
	}
	return lines.join('\n')
}

fn stable_interface_type_id_hash(name string) int {
	mut hash := u32(2166136261)
	for c in name.bytes() {
		hash = (hash ^ u32(c)) * u32(16777619)
	}
	id := int(hash & u32(0x7fffffff))
	return if id == 0 { 1 } else { id }
}

fn interface_impl_candidate_name(name string) string {
	if name.starts_with('builtin.') {
		return name['builtin.'.len..]
	}
	return name
}

// ierror_impl_names returns the concrete struct names that can be boxed as `IError`.
pub fn (tc &TypeChecker) ierror_impl_names() []string {
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if cache.ierror_impl_names_set {
			return cache.ierror_impl_names.clone()
		}
		if !isnil(cache.base) && cache.base.ierror_impl_names_set {
			return cache.base.ierror_impl_names.clone()
		}
	}
	mut struct_names := []string{}
	for name, _ in tc.structs {
		struct_names << name
	}
	struct_names.sort()
	mut impls := []string{}
	for name in struct_names {
		if tc.named_type_compatible_with_ierror(name) {
			impls << name
		}
	}
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		cache.ierror_impl_names = impls.clone()
		cache.ierror_impl_names_set = true
	}
	return impls
}

pub fn (tc &TypeChecker) concrete_method_signature_key(concrete_name string, method string) ?string {
	key := '${concrete_name}.${method}'
	if key in tc.fn_param_types || key in tc.fn_ret_types {
		return key
	}
	for candidate in tc.concrete_generic_method_signature_candidates(concrete_name, method) {
		if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
			return candidate
		}
		if indexed := tc.receiver_method_suffix_index[candidate] {
			if indexed != receiver_method_suffix_ambiguous {
				return indexed
			}
		}
	}
	if indexed := tc.receiver_method_suffix_index[key] {
		if indexed != receiver_method_suffix_ambiguous {
			return indexed
		}
	}
	if info := tc.embedded_method_call_info(concrete_name, method) {
		if info.name.len > 0 {
			return info.name
		}
	}
	return none
}

fn (tc &TypeChecker) concrete_generic_method_signature_candidates(concrete_name string, method string) []string {
	base, args, ok := generic_type_application_parts(concrete_name)
	if !ok || args.len == 0 || method.len == 0 {
		return []string{}
	}
	short_args := generic_type_args_short_for_signature(args)
	suffix := generic_type_suffix_for_signature(args)
	short_base := base.all_after_last('.')
	mut candidates := []string{}
	for receiver in [base, short_base] {
		candidates << '${receiver}[${short_args}].${method}'
		candidates << '${receiver}_${suffix}.${method}'
		candidates << tc.cached_c_name('${receiver}[${short_args}].${method}')
		candidates << tc.cached_c_name('${receiver}_${suffix}.${method}')
		candidates << '${tc.cached_c_name(receiver)}_${suffix}__${tc.cached_c_name(method)}'
	}
	return candidates
}

fn generic_type_args_short_for_signature(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << generic_type_arg_short_for_signature(arg)
	}
	return parts.join(', ')
}

fn generic_type_suffix_for_signature(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << naming.c_name(generic_type_arg_short_for_signature(arg).replace('[]', 'Array_').replace('&',
			'ptr_'))
	}
	return parts.join('_')
}

fn generic_type_arg_short_for_signature(type_arg string) string {
	clean := type_arg.trim_space()
	if clean.contains('.') {
		return clean
	}
	return clean
}

pub fn (tc &TypeChecker) named_type_compatible_with_ierror(concrete_name string) bool {
	cache_key := tc.ierror_compat_cache_key(concrete_name)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if !isnil(cache.base) {
			if cached := cache.base.ierror_compat_entries[cache_key] {
				return cached > 0
			}
		}
		if cached := cache.ierror_compat_entries[cache_key] {
			return cached > 0
		}
	}
	mut seen := map[string]bool{}
	result := tc.named_type_compatible_with_ierror_inner(concrete_name, mut seen)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		cache.ierror_compat_entries[cache_key] = if result { 1 } else { -1 }
	}
	return result
}

fn (tc &TypeChecker) ierror_compat_cache_key(concrete_name string) string {
	if concrete_name in ['Error', 'MessageError'] {
		return '${tc.cur_file}\n${tc.cur_module}\n${concrete_name}'
	}
	if concrete_name in tc.structs {
		return concrete_name
	}
	qname := tc.qualify_name(concrete_name)
	if qname in tc.structs {
		return qname
	}
	return concrete_name
}

// resolve_ierror_payload_name resolves scoped `Error`/`MessageError` names before
// falling back to the builtin error structs.
pub fn (tc &TypeChecker) resolve_ierror_payload_name(name string) string {
	if name !in ['Error', 'MessageError'] {
		return name
	}
	if resolved := tc.resolve_selective_import_type_symbol(name) {
		if resolved in tc.structs {
			return resolved
		}
	}
	if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
		local := '${tc.cur_module}.${name}'
		if local in tc.structs {
			return local
		}
	}
	return name
}

fn (tc &TypeChecker) type_compatible_with_ierror_payload(actual Type) bool {
	clean := tc.ierror_payload_concrete_type(actual)
	concrete_name := method_type_name(clean)
	if concrete_name.len == 0 {
		return false
	}
	return tc.named_type_compatible_with_ierror(concrete_name)
}

fn (tc &TypeChecker) ierror_payload_concrete_type(t Type) Type {
	mut clean := t
	mut seen := map[string]bool{}
	for {
		clean = unwrap_pointer(clean)
		if clean is Alias {
			if seen[clean.name] {
				return clean
			}
			seen[clean.name] = true
			clean = clean.base_type
			continue
		}
		return clean
	}
	return clean
}

fn (tc &TypeChecker) named_type_compatible_with_ierror_inner(concrete_name string, mut seen map[string]bool) bool {
	mut lookup := tc.resolve_ierror_payload_name(concrete_name)
	if lookup !in tc.structs {
		qname := tc.qualify_name(lookup)
		if qname in tc.structs {
			lookup = qname
		}
	}
	if lookup in ['Error', 'MessageError'] && tc.unqualified_type_name_shadows_builtin(lookup) {
		return false
	}
	if lookup in seen {
		return false
	}
	seen[lookup] = true
	if tc.is_builtin_error_struct_name(lookup) {
		return true
	}
	if tc.named_type_has_non_builtin_error_embed(lookup) {
		return false
	}
	if tc.named_type_implements_ierror_methods(lookup) {
		return true
	}
	struct_module := tc.struct_modules[lookup] or { tc.cur_module }
	struct_file := tc.struct_files[lookup] or { tc.cur_file }
	for field in tc.structs[lookup] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name.len == 0 {
			continue
		}
		if tc.embedded_field_is_scoped_builtin_error(field, embedded_name, struct_file,
			struct_module)
		{
			return true
		}
		if embedded_name in ['Error', 'MessageError'] && field.name == embedded_name {
			continue
		}
		if tc.is_builtin_error_struct_name(embedded_name) {
			return true
		}
		if tc.named_type_compatible_with_ierror_inner(embedded_name, mut seen) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) named_type_implements_ierror_methods(concrete_name string) bool {
	iface_name := if 'builtin.IError' in tc.interface_names { 'builtin.IError' } else { 'IError' }
	return tc.named_type_implements_interface(concrete_name, iface_name)
}

fn (tc &TypeChecker) named_type_has_non_builtin_error_embed(concrete_name string) bool {
	mut lookup := concrete_name
	if lookup !in tc.structs {
		qname := tc.qualify_name(lookup)
		if qname in tc.structs {
			lookup = qname
		}
	}
	if tc.struct_error_embeds_shadow_builtin[lookup] {
		return true
	}
	struct_module := tc.struct_modules[lookup] or { tc.cur_module }
	struct_file := tc.struct_files[lookup] or { tc.cur_file }
	if tc.source_struct_has_non_builtin_error_embed(lookup, struct_file, struct_module) {
		return true
	}
	for field in tc.structs[lookup] or { []StructField{} } {
		if field.name in ['Error', 'MessageError']
			&& tc.unqualified_type_name_shadows_builtin_in_scope(field.name, struct_file, struct_module) {
			return true
		}
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name.len == 0 {
			continue
		}
		if tc.embedded_field_is_scoped_builtin_error(field, embedded_name, struct_file,
			struct_module)
		{
			continue
		}
		if embedded_name in ['Error', 'MessageError'] && field.name == embedded_name {
			return true
		}
		if embedded_name.all_after_last('.') in ['Error', 'MessageError']
			&& !tc.is_builtin_error_struct_name(embedded_name) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) source_struct_has_non_builtin_error_embed(concrete_name string, file string, mod_name string) bool {
	if isnil(tc.a) {
		return false
	}
	key := source_error_embed_lookup_key(concrete_name, file, mod_name)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if !cache.source_error_embed_indexed {
			if !isnil(cache.base) && cache.base.source_error_embed_indexed {
				return cache.base.source_error_embed_entries[key] > 0
			}
			cache.source_error_embed_entries = tc.collect_source_error_embed_entries()
			cache.source_error_embed_indexed = true
		}
		return cache.source_error_embed_entries[key] > 0
	}
	entries := tc.collect_source_error_embed_entries()
	return entries[key] > 0
}

fn (tc &TypeChecker) collect_source_error_embed_entries() map[string]int {
	mut entries := map[string]int{}
	if isnil(tc.a) {
		return entries
	}
	mut cur_file := ''
	mut cur_module := ''
	if tc.top_level_idx.len > 0 && tc.a.nodes.len == tc.top_level_idx_nodes_len {
		// struct_decl nodes only occur at the top level, and the AST has not
		// grown since collect built the index.
		for i in tc.top_level_idx {
			node := tc.a.nodes[i]
			match node.kind {
				.file {
					cur_file = node.value
					cur_module = ''
				}
				.module_decl {
					cur_module = node.value
				}
				.struct_decl {
					if !tc.source_struct_decl_has_non_builtin_error_embed(node, cur_file,
						cur_module) {
						continue
					}
					target := node.value.all_after_last('.')
					module_key := source_error_embed_module_key(cur_module)
					entries[source_error_embed_entry_key(target, '', module_key)] = 1
					entries[source_error_embed_entry_key(target, cur_file, module_key)] = 1
				}
				else {}
			}
		}
		return entries
	}
	for node in tc.a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				if !tc.source_struct_decl_has_non_builtin_error_embed(node, cur_file, cur_module) {
					continue
				}
				target := node.value.all_after_last('.')
				module_key := source_error_embed_module_key(cur_module)
				entries[source_error_embed_entry_key(target, '', module_key)] = 1
				entries[source_error_embed_entry_key(target, cur_file, module_key)] = 1
			}
			else {}
		}
	}
	return entries
}

fn (tc &TypeChecker) source_struct_decl_has_non_builtin_error_embed(node flat.Node, cur_file string, cur_module string) bool {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_typ := if field.typ.len > 0 { field.typ } else { field.value }
		if !source_field_decl_is_embed(field, field_typ) {
			continue
		}
		if field_typ in ['Error', 'MessageError']
			&& tc.unqualified_type_name_shadows_builtin_in_scope(field_typ, cur_file, cur_module) {
			return true
		}
		resolved := tc.resolve_imported_type_text_in_file(field_typ, cur_file)
		if resolved.all_after_last('.') in ['Error', 'MessageError']
			&& !tc.is_builtin_error_struct_name_in_scope(resolved, cur_file, cur_module) {
			return true
		}
	}
	return false
}

fn source_error_embed_lookup_key(concrete_name string, file string, mod_name string) string {
	target := concrete_name.all_after_last('.')
	target_module := if concrete_name.contains('.') {
		concrete_name.all_before_last('.')
	} else {
		mod_name
	}
	file_key := if target_module.len > 0 { '' } else { file }
	return source_error_embed_entry_key(target, file_key,
		source_error_embed_module_key(target_module))
}

fn source_error_embed_entry_key(target string, file string, module_key string) string {
	return '${file}\n${module_key}\n${target}'
}

fn source_error_embed_module_key(mod_name string) string {
	return if mod_name == 'main' { '' } else { mod_name }
}

fn source_field_decl_is_embed(field flat.Node, field_typ string) bool {
	return field.typ.len == 0 || field.value.len == 0 || field.value == field_typ
		|| (field_typ.contains('.') && field.value == field_typ.all_after_last('.'))
}

fn (tc &TypeChecker) embedded_field_is_scoped_builtin_error(field StructField, embedded_name string, struct_file string, struct_module string) bool {
	if embedded_name !in ['Error', 'MessageError'] || field.name != embedded_name {
		return false
	}
	return !tc.unqualified_type_name_shadows_builtin_in_scope(embedded_name, struct_file,
		struct_module)
}

fn (tc &TypeChecker) resolve_unqualified_builtin_error_struct_name(name string) ?string {
	if name !in ['Error', 'MessageError'] {
		return none
	}
	if tc.unqualified_type_name_shadows_builtin(name) {
		return none
	}
	if mod_name := tc.struct_modules[name] {
		if mod_name == 'builtin' {
			return name
		}
	}
	if tc.has_builtins {
		return name
	}
	return none
}

fn (tc &TypeChecker) unqualified_type_name_shadows_builtin(name string) bool {
	return tc.unqualified_type_name_shadows_builtin_in_scope(name, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) unqualified_type_name_shadows_builtin_in_scope(name string, file string, mod_name string) bool {
	local_name := if mod_name.len > 0 && mod_name !in ['', 'main', 'builtin'] {
		'${mod_name}.${name}'
	} else {
		name
	}
	if local_name == name && mod_name != 'builtin'
		&& tc.source_declares_type_in_scope(name, file, mod_name) {
		return true
	}
	if local_name != name && tc.type_symbol_known(local_name) {
		return true
	}
	if file_import_key(file, name) in tc.file_selective_imports {
		if resolved := tc.resolve_selective_import_type_symbol_in_file(name, file) {
			return !tc.is_builtin_error_struct_name(resolved)
		}
		return true
	}
	if local_name != name {
		return false
	}
	if name in tc.structs {
		return tc.struct_modules[name] or { '' } != 'builtin'
	}
	if name in tc.type_aliases || name in tc.sum_types || name in tc.interface_names
		|| name in tc.enum_names {
		return true
	}
	return false
}

fn (tc &TypeChecker) source_declares_type_in_scope(name string, file string, mod_name string) bool {
	if file.len == 0 || isnil(tc.a) {
		return false
	}
	return scope_type_key(file, mod_name, name) in tc.declared_type_scope_keys
}

// scope_type_key builds the lookup key used by `declared_type_scope_keys`.
// The module is normalized so that '' and 'main' collapse to the same bucket,
// matching the old `module_names_match` semantics.
fn scope_type_key(file string, mod_name string, name string) string {
	norm_mod := if mod_name == '' || mod_name == 'main' { 'main' } else { mod_name }
	return '${file}\x01${norm_mod}\x01${name}'
}

fn (tc &TypeChecker) resolve_selective_import_type_symbol_in_file(name string, file string) ?string {
	candidates := tc.file_selective_imports[file_import_key(file, name)] or { return none }
	for candidate in candidates {
		if tc.type_symbol_known(candidate) {
			return candidate
		}
	}
	return none
}

pub fn (tc &TypeChecker) resolve_imported_type_text_in_file(typ string, file string) string {
	if !typ.contains('.') || typ.starts_with('C.') {
		return typ
	}
	dot := typ.index_u8(`.`)
	if dot <= 0 {
		return typ
	}
	alias := typ[..dot]
	if resolved := tc.file_imports[file_import_key(file, alias)] {
		if resolved != alias {
			return resolved + typ[dot..]
		}
	}
	return typ
}

fn (tc &TypeChecker) is_builtin_error_struct_name(name string) bool {
	return tc.is_builtin_error_struct_name_in_scope(name, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) is_builtin_error_struct_name_in_scope(name string, file string, mod_name string) bool {
	if name in ['builtin.Error', 'builtin.MessageError'] {
		return true
	}
	if name in ['Error', 'MessageError'] {
		if tc.unqualified_type_name_shadows_builtin_in_scope(name, file, mod_name) {
			return false
		}
		if mod := tc.struct_modules[name] {
			if mod == 'builtin' {
				return true
			}
		}
		return tc.has_builtins
	}
	return false
}

// interface_method_names supports interface method names handling for TypeChecker.
fn (tc &TypeChecker) interface_method_names(iface_name string) []string {
	mut seen := map[string]bool{}
	return tc.interface_method_names_inner(iface_name, mut seen)
}

// interface_abstract_method_names returns the methods an implementer must provide:
// the interface's own declared (abstract) methods plus those of any embedded
// interfaces. Default methods defined directly on the interface are excluded.
pub fn (tc &TypeChecker) interface_abstract_method_names(iface_name string) []string {
	mut seen := map[string]bool{}
	return tc.interface_abstract_method_names_inner(iface_name, mut seen)
}

// interface_abstract_method_names_inner supports interface_abstract_method_names_inner handling.
fn (tc &TypeChecker) interface_abstract_method_names_inner(iface_name string, mut seen map[string]bool) []string {
	if iface_name in seen {
		return []string{}
	}
	seen[iface_name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		for method in tc.interface_abstract_method_names_inner(embed, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	for method in tc.interface_abstract_methods[iface_name] or { []string{} } {
		if method !in methods {
			methods << method
		}
	}
	return methods
}

// interface_method_names_inner supports interface method names inner handling for TypeChecker.
fn (tc &TypeChecker) interface_method_names_inner(iface_name string, mut seen map[string]bool) []string {
	if iface_name in seen {
		return []string{}
	}
	seen[iface_name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		for method in tc.interface_method_names_inner(embed, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	prefix := '${iface_name}.'
	for key, _ in tc.fn_ret_types {
		if key.starts_with(prefix) {
			method := key[prefix.len..]
			if method !in methods {
				methods << method
			}
		}
	}
	return methods
}

pub fn (tc &TypeChecker) interface_method_signature_key(iface_name string, method string) ?string {
	key := '${iface_name}.${method}'
	if key in tc.fn_ret_types || key in tc.fn_param_types {
		return key
	}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		if found := tc.interface_method_signature_key(embed, method) {
			return found
		}
	}
	return none
}

fn (tc &TypeChecker) interface_receiver_method_call_info(iface_name string, method string) ?CallInfo {
	if iface_name !in tc.interface_names {
		return none
	}
	decl_key := tc.interface_method_signature_key(iface_name, method) or { return none }
	decl_params := tc.fn_param_types[decl_key] or { return none }
	mut params := []Type{cap: decl_params.len}
	params << Type(Pointer{
		base_type: Type(Interface{
			name: iface_name
		})
	})
	if decl_params.len > 1 {
		for i in 1 .. decl_params.len {
			params << decl_params[i]
		}
	}
	call_name := '${iface_name}.${method}'
	return CallInfo{
		name:          call_name
		params:        params
		shared_params: tc.fn_shared_params[decl_key] or { []bool{} }
		return_type:   tc.fn_ret_types[decl_key] or { Type(void_) }
		has_receiver:  true
		params_known:  true
	}
}

// interface_field_list supports interface field list handling for TypeChecker.
fn (tc &TypeChecker) interface_field_list(iface_name string) []StructField {
	mut seen := map[string]bool{}
	return tc.interface_field_list_inner(iface_name, mut seen)
}

// interface_field_list_inner supports interface field list inner handling for TypeChecker.
fn (tc &TypeChecker) interface_field_list_inner(iface_name string, mut seen map[string]bool) []StructField {
	if iface_name in seen {
		return []StructField{}
	}
	seen[iface_name] = true
	mut fields := []StructField{}
	for embed in tc.interface_embeds[iface_name] or { []string{} } {
		fields << tc.interface_field_list_inner(embed, mut seen)
	}
	for field in tc.interface_fields[iface_name] or { []StructField{} } {
		fields << field
	}
	return fields
}

// interface_field_type supports interface field type handling for TypeChecker.
fn (tc &TypeChecker) interface_field_type(iface_name string, field_name string) ?Type {
	for field in tc.interface_field_list(iface_name) {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// struct_field_type supports struct field type handling for TypeChecker.
fn (tc &TypeChecker) struct_init_field_lookup_name(literal_name string, parsed_name string) string {
	clean := literal_name.trim_space()
	if clean.len == 0 {
		return parsed_name
	}
	if generic_type_application(clean) {
		_, _, parsed_is_generic := generic_type_application_parts(parsed_name)
		if parsed_is_generic {
			return parsed_name
		}
		bracket := clean.index_u8(`[`)
		if bracket > 0 {
			base := if parsed_name.len > 0 { parsed_name } else { clean[..bracket].trim_space() }
			return base + clean[bracket..]
		}
	}
	if parsed_name.len > 0 {
		return parsed_name
	}
	return clean
}

fn (tc &TypeChecker) struct_fields_for_init(struct_name string) []StructField {
	base_name, generic_args, is_generic := generic_type_application_parts(struct_name)
	lookup_name := if is_generic { base_name } else { struct_name }
	fields := tc.structs[lookup_name] or { return []StructField{} }
	if !is_generic {
		return fields
	}
	params := tc.struct_generic_params[base_name] or { return fields }
	if params.len != generic_args.len {
		return fields
	}
	mut concrete_fields := []StructField{cap: fields.len}
	for field in fields {
		concrete_fields << StructField{
			name: field.name
			typ:  tc.substitute_generic_type(field.typ, generic_args, params)
		}
	}
	return concrete_fields
}

fn (tc &TypeChecker) struct_field_type(struct_name string, field_name string) ?Type {
	cache_key := '${struct_name}\n${field_name}'
	if !isnil(tc.type_cache) {
		if !isnil(tc.type_cache.base) {
			if typ := tc.type_cache.base.struct_field_entries[cache_key] {
				return typ
			}
			if tc.type_cache.base.struct_field_misses[cache_key] {
				return none
			}
		}
		if typ := tc.type_cache.struct_field_entries[cache_key] {
			return typ
		}
		if tc.type_cache.struct_field_misses[cache_key] {
			return none
		}
	}
	mut seen := map[string]bool{}
	if typ := tc.struct_field_type_inner(struct_name, field_name, mut seen) {
		if !isnil(tc.type_cache) {
			mut cache := tc.type_cache
			cache.struct_field_entries[cache_key] = typ
		}
		return typ
	}
	if !isnil(tc.type_cache) {
		mut cache := tc.type_cache
		cache.struct_field_misses[cache_key] = true
	}
	return none
}

// struct_field_type_name returns the canonical type name for a struct field.
pub fn (tc &TypeChecker) struct_field_type_name(struct_name string, field_name string) ?string {
	typ := tc.struct_field_type(struct_name, field_name) or { return none }
	return typ.name()
}

fn (tc &TypeChecker) struct_field_type_inner(struct_name string, field_name string, mut seen map[string]bool) ?Type {
	base_name, generic_args, is_generic := generic_type_application_parts(struct_name)
	lookup_name := if is_generic { base_name } else { struct_name }
	if lookup_name in seen {
		return none
	}
	seen[lookup_name] = true
	fields := tc.structs[lookup_name] or { []StructField{} }
	// The struct's own fields shadow promoted/embedded ones regardless of
	// declaration order, so scan all direct fields before any embed.
	for field in fields {
		if field.name == field_name {
			if is_generic {
				return tc.substitute_generic_type(field.typ, generic_args, tc.struct_generic_params[base_name] or {
					[]string{}
				})
			}
			return field.typ
		}
	}
	for field in fields {
		mut embedded_type := embedded_field_type(field) or { continue }
		embedded_type = if is_generic {
			tc.substitute_generic_type(embedded_type, generic_args, tc.struct_generic_params[base_name] or {
				[]string{}
			})
		} else {
			embedded_type
		}
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name.len == 0 {
			continue
		}
		// A `mod.Inner` embed is promoted under its short name: `o.Inner`.
		// Same-module embeds already match the direct-field pass above.
		if embedded_name != field_name && embedded_name.all_after_last('.') == field_name {
			return embedded_type
		}
		if typ := tc.struct_field_type_inner(embedded_name, field_name, mut seen) {
			return typ
		}
	}
	return none
}

// substitute_generic_type replaces generic placeholders in `typ` with the concrete
// `args`. When `param_names` (the struct/fn's declared type parameters, e.g.
// `['L', 'R']`) is provided, a placeholder is matched to its arg by its declared
// position — so `Pair[L, R]`'s `R` resolves to `args[1]`, not the letter-based
// `generic_param_index` guess (which maps any unrecognised name to 0). The
// letter-based fallback is kept only for callers that have no declared names.
fn (tc &TypeChecker) substitute_generic_type(typ Type, args []string, param_names []string) Type {
	if args.len == 0 {
		return typ
	}
	if typ is Unknown {
		if name := generic_placeholder_from_unknown(typ) {
			mut idx := if param_names.len > 0 { param_names.index(name) } else { -1 }
			if idx < 0 {
				idx = generic_param_index(name)
			}
			if idx >= 0 && idx < args.len {
				return tc.parse_type(args[idx].trim_space())
			}
		}
		return typ
	}
	if typ is Array {
		return Type(Array{
			elem_type: tc.substitute_generic_type(typ.elem_type, args, param_names)
		})
	}
	if typ is ArrayFixed {
		return Type(ArrayFixed{
			elem_type: tc.substitute_generic_type(typ.elem_type, args, param_names)
			len:       typ.len
			len_expr:  typ.len_expr
		})
	}
	if typ is Map {
		return Type(Map{
			key_type:   tc.substitute_generic_type(typ.key_type, args, param_names)
			value_type: tc.substitute_generic_type(typ.value_type, args, param_names)
		})
	}
	if typ is Pointer {
		return Type(Pointer{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is OptionType {
		return Type(OptionType{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is ResultType {
		return Type(ResultType{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is SumType {
		if typ.name.contains('[') {
			return tc.parse_type(subst_generic_text(typ.name, args, param_names))
		}
		return typ
	}
	if typ is FnType {
		mut params := []Type{}
		for param in typ.params {
			params << tc.substitute_generic_type(param, args, param_names)
		}
		return Type(FnType{
			params:      params
			return_type: tc.substitute_generic_type(typ.return_type, args, param_names)
		})
	}
	if typ is MultiReturn {
		mut parts := []Type{}
		for part in typ.types {
			parts << tc.substitute_generic_type(part, args, param_names)
		}
		return Type(MultiReturn{
			types: parts
		})
	}
	return typ
}

fn (tc &TypeChecker) embedded_method_call_info(struct_name string, method string) ?CallInfo {
	mut seen := map[string]bool{}
	return tc.embedded_method_call_info_inner(struct_name, method, mut seen)
}

fn (tc &TypeChecker) embedded_method_call_info_inner(struct_name string, method string, mut seen map[string]bool) ?CallInfo {
	if seen[struct_name] {
		return none
	}
	seen[struct_name] = true
	for field in tc.structs[struct_name] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		receiver := method_type_name(unwrap_pointer(embedded_type))
		if receiver.len == 0 {
			continue
		}
		mut method_names := ['${receiver}.${method}']
		base_name, _, is_generic := generic_type_application_parts(receiver)
		if is_generic {
			method_names << '${base_name}.${method}'
		}
		for method_name in method_names {
			if method_name in tc.fn_ret_types {
				info := tc.call_info(method_name, true)
				return CallInfo{
					name:          info.name
					params:        info.params
					shared_params: info.shared_params
					return_type:   info.return_type
					has_receiver:  info.has_receiver
					is_variadic:   info.is_variadic
					is_c_variadic: info.is_c_variadic
					params_known:  if receiver.contains('[') { false } else { info.params_known }
				}
			}
		}
		if info := tc.embedded_method_call_info_inner(receiver, method, mut seen) {
			return info
		}
	}
	return none
}

fn (tc &TypeChecker) struct_has_middleware_receiver(struct_name string) bool {
	if is_middleware_type_name(struct_name) {
		return true
	}
	for field in tc.structs[struct_name] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if is_middleware_type_name(embedded_name) {
			return true
		}
	}
	return false
}

fn is_middleware_type_name(name string) bool {
	base := if name.contains('[') { name.all_before('[') } else { name }
	return base == 'veb.Middleware'
}

fn (tc &TypeChecker) receiver_embeds(actual Type, expected Type) bool {
	actual_name := method_type_name(unwrap_pointer(actual))
	expected_name := method_type_name(unwrap_pointer(expected))
	if actual_name.len == 0 || expected_name.len == 0 {
		return false
	}
	mut seen := map[string]bool{}
	return tc.receiver_embeds_inner(actual_name, expected_name, mut seen)
}

fn (tc &TypeChecker) receiver_embeds_inner(actual_name string, expected_name string, mut seen map[string]bool) bool {
	if seen[actual_name] {
		return false
	}
	seen[actual_name] = true
	for field in tc.structs[actual_name] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name == expected_name {
			return true
		}
		if tc.receiver_embeds_inner(embedded_name, expected_name, mut seen) {
			return true
		}
	}
	return false
}

fn embedded_field_type(field StructField) ?Type {
	field_type_name := method_type_name(unwrap_pointer(field.typ))
	if field_type_name.len == 0 {
		return none
	}
	if field.name.len == 0 {
		return field.typ
	}
	mut names := [field_type_name]
	base_name, _, is_generic := generic_type_application_parts(field_type_name)
	if is_generic {
		names << base_name
	}
	for name in names {
		short_name := if name.contains('.') { name.all_after_last('.') } else { name }
		if field.name == name || field.name == short_name {
			return field.typ
		}
	}
	return none
}

// method_signature_compatible supports method signature compatible handling for TypeChecker.
fn (tc &TypeChecker) method_signature_compatible(actual_key string, expected_key string) bool {
	actual_params := tc.fn_param_types[actual_key] or { return false }
	expected_params := tc.fn_param_types[expected_key] or { return false }
	if actual_params.len != expected_params.len {
		return false
	}
	for i in 1 .. actual_params.len {
		if !tc.method_param_signature_compatible(actual_params[i], expected_params[i]) {
			return false
		}
	}
	actual_ret := tc.fn_ret_types[actual_key] or { Type(void_) }
	expected_ret := tc.fn_ret_types[expected_key] or { Type(void_) }
	return tc.type_compatible(actual_ret, expected_ret)
}

fn (tc &TypeChecker) method_call_info_signature_compatible(actual CallInfo, expected_key string) bool {
	expected_params := tc.fn_param_types[expected_key] or { return false }
	if actual.params.len != expected_params.len {
		return false
	}
	for i in 1 .. actual.params.len {
		if !tc.method_param_signature_compatible(actual.params[i], expected_params[i]) {
			return false
		}
	}
	expected_ret := tc.fn_ret_types[expected_key] or { Type(void_) }
	return tc.type_compatible(actual.return_type, expected_ret)
}

fn (tc &TypeChecker) method_param_signature_compatible(actual Type, expected Type) bool {
	if type_pointer_depth(actual) != type_pointer_depth(expected) {
		return false
	}
	return tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual)
}

fn type_pointer_depth(t Type) int {
	if t is Pointer {
		return 1 + type_pointer_depth(t.base_type)
	}
	if t is Alias {
		return type_pointer_depth(t.base_type)
	}
	return 0
}

// method_type_name supports method type name handling for types.
fn method_type_name(t Type) string {
	if t is Alias {
		return t.name
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is SumType {
		return t.name
	}
	if t is Enum {
		return t.name
	}
	if t is String {
		return 'string'
	}
	if t is Primitive {
		return prim_name(t)
	}
	if t is ISize {
		return 'isize'
	}
	if t is USize {
		return 'usize'
	}
	if t is Rune {
		return 'rune'
	}
	return ''
}

fn (tc &TypeChecker) sum_base_name(sum_name string) string {
	base, _, ok := generic_type_application_parts(sum_name)
	if ok {
		return base
	}
	if sum_name in tc.sum_types {
		return sum_name
	}
	qname := tc.qualify_name(sum_name)
	if qname in tc.sum_types {
		return qname
	}
	if sum_name.contains('.') {
		resolved := tc.resolve_imported_type_text(sum_name)
		if resolved in tc.sum_types {
			return resolved
		}
		if unique := tc.unique_qualified_type_name(sum_name.all_after_last('.')) {
			if unique in tc.sum_types {
				return unique
			}
		}
	}
	return sum_name
}

fn (tc &TypeChecker) sum_params_for_base(base string) []string {
	if params := tc.sum_generic_params[base] {
		return params
	}
	short := base.all_after_last('.')
	if params := tc.sum_generic_params[short] {
		return params
	}
	return []string{}
}

fn (tc &TypeChecker) concrete_sum_variant_name(sum_name string, variant string) string {
	base, args, ok := generic_type_application_parts(sum_name)
	if !ok {
		return variant
	}
	params := tc.sum_params_for_base(base)
	if params.len == 0 || params.len != args.len {
		return variant
	}
	return subst_generic_text(variant, args, params)
}

pub fn (tc &TypeChecker) generic_type_name_matches(a string, b string) bool {
	if a == b {
		return true
	}
	a_base, a_args, a_ok := generic_type_application_parts(a)
	b_base, b_args, b_ok := generic_type_application_parts(b)
	if a_ok || b_ok {
		if !a_ok || !b_ok || a_args.len != b_args.len {
			return false
		}
		if !tc.generic_type_base_matches(a_base, b_base) {
			return false
		}
		for i in 0 .. a_args.len {
			if !tc.generic_type_arg_matches(a_args[i], b_args[i]) {
				return false
			}
		}
		return true
	}
	return tc.generic_type_base_matches(a, b)
}

fn (tc &TypeChecker) generic_type_base_matches(a string, b string) bool {
	a_clean := a.trim_space()
	b_clean := b.trim_space()
	if a_clean == b_clean {
		return true
	}
	a_resolved := tc.resolve_generic_match_base(a_clean)
	b_resolved := tc.resolve_generic_match_base(b_clean)
	if a_resolved == b_resolved {
		return true
	}
	if a_clean.contains('.') || b_clean.contains('.') || a_resolved.contains('.')
		|| b_resolved.contains('.') {
		return false
	}
	return short_type_name(a_clean) == short_type_name(b_clean)
}

fn (tc &TypeChecker) resolve_generic_match_base(base string) string {
	if base.len == 0 {
		return base
	}
	if base.contains('.') {
		return tc.resolve_imported_type_text(base)
	}
	if resolved := tc.resolve_selective_import_type_symbol(base) {
		return resolved
	}
	qbase := tc.qualify_name(base)
	if qbase != base && tc.type_symbol_known(qbase) {
		return qbase
	}
	return base
}

fn (tc &TypeChecker) generic_type_arg_matches(a string, b string) bool {
	a_clean := a.trim_space()
	b_clean := b.trim_space()
	if a_clean == b_clean {
		return true
	}
	if tc.generic_match_arg_is_open_param(a_clean) || tc.generic_match_arg_is_open_param(b_clean) {
		return true
	}
	if a_clean.starts_with('&') || b_clean.starts_with('&') {
		return a_clean.starts_with('&') && b_clean.starts_with('&')
			&& tc.generic_type_arg_matches(a_clean[1..], b_clean[1..])
	}
	if a_clean.starts_with('mut ') || b_clean.starts_with('mut ') {
		return a_clean.starts_with('mut ') && b_clean.starts_with('mut ')
			&& tc.generic_type_arg_matches(a_clean[4..], b_clean[4..])
	}
	if a_clean.starts_with('?') || b_clean.starts_with('?') {
		return a_clean.starts_with('?') && b_clean.starts_with('?')
			&& tc.generic_type_arg_matches(a_clean[1..], b_clean[1..])
	}
	if a_clean.starts_with('!') || b_clean.starts_with('!') {
		return a_clean.starts_with('!') && b_clean.starts_with('!')
			&& tc.generic_type_arg_matches(a_clean[1..], b_clean[1..])
	}
	if a_clean.starts_with('...') || b_clean.starts_with('...') {
		return a_clean.starts_with('...') && b_clean.starts_with('...')
			&& tc.generic_type_arg_matches(a_clean[3..], b_clean[3..])
	}
	if a_clean.starts_with('[]') || b_clean.starts_with('[]') {
		return a_clean.starts_with('[]') && b_clean.starts_with('[]')
			&& tc.generic_type_arg_matches(a_clean[2..], b_clean[2..])
	}
	if a_clean.starts_with('map[') || b_clean.starts_with('map[') {
		if !a_clean.starts_with('map[') || !b_clean.starts_with('map[') {
			return false
		}
		a_bracket_end := find_matching_bracket(a_clean, 3)
		b_bracket_end := find_matching_bracket(b_clean, 3)
		if a_bracket_end >= a_clean.len || b_bracket_end >= b_clean.len {
			return false
		}
		return tc.generic_type_arg_matches(a_clean[4..a_bracket_end], b_clean[4..b_bracket_end])
			&& tc.generic_type_arg_matches(a_clean[a_bracket_end + 1..], b_clean[b_bracket_end + 1..])
	}
	if a_clean.starts_with('[') || b_clean.starts_with('[') {
		if !a_clean.starts_with('[') || !b_clean.starts_with('[') {
			return false
		}
		a_bracket_end := find_matching_bracket(a_clean, 0)
		b_bracket_end := find_matching_bracket(b_clean, 0)
		if a_bracket_end >= a_clean.len || b_bracket_end >= b_clean.len
			|| a_clean[..a_bracket_end + 1] != b_clean[..b_bracket_end + 1] {
			return false
		}
		return tc.generic_type_arg_matches(a_clean[a_bracket_end + 1..],
			b_clean[b_bracket_end + 1..])
	}
	a_base, a_args, a_ok := generic_type_application_parts(a_clean)
	b_base, b_args, b_ok := generic_type_application_parts(b_clean)
	if a_ok || b_ok {
		if !a_ok || !b_ok || a_args.len != b_args.len {
			return false
		}
		if !tc.generic_type_base_matches(a_base, b_base) {
			return false
		}
		for i in 0 .. a_args.len {
			if !tc.generic_type_arg_matches(a_args[i], b_args[i]) {
				return false
			}
		}
		return true
	}
	a_resolved := tc.resolve_generic_match_arg(a_clean)
	b_resolved := tc.resolve_generic_match_arg(b_clean)
	if a_resolved == b_resolved {
		return true
	}
	return false
}

fn (tc &TypeChecker) generic_match_arg_is_open_param(arg string) bool {
	return is_bare_generic_param(arg) && !tc.is_known_type_text(arg)
}

fn (tc &TypeChecker) resolve_generic_match_arg(arg string) string {
	if arg.len == 0 {
		return arg
	}
	if !arg.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(arg) {
			return resolved
		}
	}
	qarg := tc.qualify_type_text(arg)
	if qarg.len > 0 {
		return qarg
	}
	return arg
}

fn (tc &TypeChecker) bare_variant_name_matches(candidate string, declared string, concrete string) bool {
	if generic_type_application(candidate) {
		return false
	}
	candidate_base := candidate.trim_space()
	declared_base := strip_generic_args_name(declared).trim_space()
	concrete_base := strip_generic_args_name(concrete).trim_space()
	candidate_resolved := tc.resolve_generic_match_base(candidate_base)
	declared_resolved := tc.resolve_generic_match_base(declared_base)
	concrete_resolved := tc.resolve_generic_match_base(concrete_base)
	if candidate_resolved == declared_resolved || candidate_resolved == concrete_resolved {
		return true
	}
	if candidate_base.contains('.') || candidate_resolved.contains('.') {
		return false
	}
	return (!declared_base.contains('.') && candidate_base == short_type_name(declared_base))
		|| (!concrete_base.contains('.') && candidate_base == short_type_name(concrete_base))
}

// type_matches_sum returns type matches sum data for TypeChecker.
fn (tc &TypeChecker) type_matches_sum(actual Type, expected Type) bool {
	if expected is SumType {
		actual_name := actual.name()
		return tc.sum_variant_type_for_pattern(expected.name, actual_name) != none
	}
	return false
}

// sum_has_variant converts sum has variant data for types.
fn (tc &TypeChecker) sum_has_variant(sum_name string, variant_name string) bool {
	return tc.sum_variant_type_for_pattern(sum_name, variant_name) != none
}

pub fn (tc &TypeChecker) sum_variant_type_for_pattern(sum_name string, variant_name string) ?string {
	return tc.sum_variant_type_for_pattern_depth(sum_name, variant_name, 0)
}

fn (tc &TypeChecker) sum_variant_type_for_pattern_depth(sum_name string, variant_name string, depth int) ?string {
	if depth >= 16 {
		return none
	}
	base := tc.sum_base_name(sum_name)
	variants := tc.sum_types[base] or { return none }
	mut candidates := [variant_name]
	qvariant := tc.qualify_name(variant_name)
	if qvariant != variant_name {
		candidates << qvariant
	}
	if variant_name.contains('.') {
		resolved := tc.resolve_imported_type_text(variant_name)
		if resolved != variant_name {
			candidates << resolved
		}
		// `all_after_last('.')` on a container pattern (`map[string]ast.Value`)
		// strips the container and would resolve to the sum type itself.
		if !variant_name.contains('[') && !variant_name.contains(']') {
			if unique := tc.unique_qualified_type_name(variant_name.all_after_last('.')) {
				candidates << unique
			}
		}
	}
	for candidate in candidates.clone() {
		candidates << tc.alias_target_type_names(candidate)
	}
	for candidate in candidates {
		if tc.generic_type_name_matches(candidate, sum_name)
			|| tc.generic_type_name_matches(candidate, base) {
			return sum_name
		}
	}
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(sum_name, variant)
		for candidate in candidates {
			if tc.generic_type_name_matches(candidate, concrete)
				|| tc.bare_variant_name_matches(candidate, variant, concrete) {
				return concrete
			}
			if is_bare_generic_param(concrete) && !tc.is_known_type_text(concrete) {
				return candidate
			}
		}
		if nested := tc.nested_sum_variant_type_for_pattern(concrete, candidates, depth) {
			return nested
		}
	}
	return none
}

fn (tc &TypeChecker) alias_target_type_names(name string) []string {
	mut result := []string{}
	mut cur := name
	for _ in 0 .. 16 {
		target := tc.alias_target_type_text(cur) or { break }
		typ := tc.parse_type(target)
		target_name := typ.name()
		if target_name.len == 0 || target_name == cur || target_name in result {
			break
		}
		result << target_name
		cur = target_name
	}
	return result
}

fn (tc &TypeChecker) alias_target_type_text(name string) ?string {
	if target := tc.type_aliases[name] {
		return target
	}
	qname := tc.qualify_name(name)
	if target := tc.type_aliases[qname] {
		return target
	}
	if name.contains('.') {
		resolved := tc.resolve_imported_type_text(name)
		if target := tc.type_aliases[resolved] {
			return target
		}
		if unique := tc.unique_qualified_type_name(name.all_after_last('.')) {
			if target := tc.type_aliases[unique] {
				return target
			}
		}
	}
	return none
}

fn (tc &TypeChecker) nested_sum_variant_type_for_pattern(concrete string, candidates []string, depth int) ?string {
	typ := tc.parse_type(concrete)
	nested_name := if typ is SumType {
		typ.name
	} else if typ is Alias && typ.base_type is SumType {
		(typ.base_type as SumType).name
	} else {
		''
	}
	if nested_name.len == 0 {
		return none
	}
	for candidate in candidates {
		if nested := tc.sum_variant_type_for_pattern_depth(nested_name, candidate, depth + 1) {
			return nested
		}
	}
	return none
}

fn (tc &TypeChecker) match_sum_variant_type(subject Type, pattern string) ?Type {
	if subject is SumType {
		variants := tc.sum_types[subject.name] or { return none }
		variant_short := short_type_name(pattern)
		for variant in variants {
			if variant == pattern || short_type_name(variant) == variant_short {
				return tc.parse_type(variant)
			}
		}
		qpattern := tc.qualify_name(pattern)
		if qpattern != pattern {
			for variant in variants {
				if variant == qpattern || short_type_name(variant) == variant_short {
					return tc.parse_type(variant)
				}
			}
		}
		return none
	}
	if tc.type_symbol_known(pattern) {
		return tc.parse_type(pattern)
	}
	if is_builtin_type_name(pattern) {
		return builtin_type_value(pattern)
	}
	return none
}

// match_type_pattern supports match type pattern handling for TypeChecker.
fn (tc &TypeChecker) match_type_pattern(node &flat.Node) ?string {
	if node.kind == .array_init && node.typ.len > 0 {
		return node.typ
	}
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident {
			return '${base.value}.${node.value}'
		}
	}
	return none
}

// short_type_name supports short type name handling for types.
fn short_type_name(name string) string {
	if name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

// type_mismatch returns type mismatch data for TypeChecker.
fn (mut tc TypeChecker) type_mismatch(kind TypeErrorKind, msg string, node flat.NodeId) {
	if tc.should_diagnose(node) {
		tc.record_error(kind, msg, node)
	}
}

// expr_key supports expr key handling for TypeChecker.
fn (tc &TypeChecker) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if valid_string_data(node.value) {
			return node.value
		}
		return ''
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.expr_key(tc.a.child(&node, 0))
		if base.len > 0 && node.value.len > 0 && valid_string_data(node.value) {
			return '${base}.${node.value}'
		}
	}
	return ''
}

// smartcast_type supports smartcast type handling for TypeChecker.
fn (tc &TypeChecker) smartcast_type(id flat.NodeId) ?Type {
	key := tc.expr_key(id)
	if key.len == 0 {
		return none
	}
	if !valid_string_data(key) {
		return none
	}
	if typ := tc.smartcasts[key] {
		return typ
	}
	return none
}

// cached_c_name memoizes naming.c_name results in the type cache (falling
// back to the frozen base cache read-only, like every other entry kind).
// c_name is pure and called on hot resolution paths in every phase.
pub fn (tc &TypeChecker) cached_c_name(name string) string {
	if isnil(tc.type_cache) {
		return naming.c_name(name)
	}
	mut cache := unsafe { tc.type_cache }
	if !isnil(cache.base) {
		if cached := cache.base.c_name_entries[name] {
			return cached
		}
	}
	if cached := cache.c_name_entries[name] {
		return cached
	}
	result := naming.c_name(name)
	cache.c_name_entries[name] = result
	return result
}

// parse_type converts a V type string (from parser) to a structured Type.
pub fn (tc &TypeChecker) parse_type(typ string) Type {
	if tc.type_cache != unsafe { nil } && tc.type_cache.parse_enabled {
		mode := if tc.resolution_type_mode { 'resolution' } else { 'source' }
		key := mode + '\n' + tc.cur_file + '\n' + tc.cur_module + '\n' + typ
		mut cache := unsafe { tc.type_cache }
		// The frozen base holds the warm pre-region entries — check it first
		// (values are deterministic, so shadowing order does not matter).
		if !isnil(cache.base) {
			if cached := cache.base.parse_entries[key] {
				return cached
			}
		}
		if cached := cache.parse_entries[key] {
			return cached
		}
		result := tc.parse_type_uncached(typ)
		cache.parse_entries[key] = result
		return result
	}
	return tc.parse_type_uncached(typ)
}

// parse_scope_param_type preserves open generic struct applications for local parameter
// lookup. The global parser deliberately collapses `Box[T]` to `Box` in signatures, but
// inside a generic function/method body the parameter still needs the open application so
// field lookup and generic receiver method resolution can substitute `T`.
fn (tc &TypeChecker) parse_scope_param_type(typ string) Type {
	if preserved := tc.parse_open_generic_struct_type(typ) {
		return preserved
	}
	return tc.parse_type(typ)
}

fn (tc &TypeChecker) parse_open_generic_struct_type(typ string) ?Type {
	clean := typ.trim_space()
	if clean.len == 0 {
		return none
	}
	if clean.starts_with('&') {
		base := tc.parse_open_generic_struct_type(clean[1..]) or { return none }
		return Type(Pointer{
			base_type: base
		})
	}
	if clean.starts_with('mut ') {
		base := tc.parse_open_generic_struct_type(clean[4..]) or { return none }
		return Type(Pointer{
			base_type: base
		})
	}
	if clean.starts_with('shared ') {
		return tc.parse_open_generic_struct_type(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return tc.parse_open_generic_struct_type(clean[7..])
	}
	if clean.starts_with('?') {
		base := tc.parse_open_generic_struct_type(clean[1..]) or { return none }
		return Type(OptionType{
			base_type: base
		})
	}
	if clean.starts_with('!') {
		base := tc.parse_open_generic_struct_type(clean[1..]) or { return none }
		return Type(ResultType{
			base_type: base
		})
	}
	if clean.starts_with('...') {
		elem := tc.parse_open_generic_struct_type(clean[3..]) or { return none }
		return Type(Array{
			elem_type: elem
		})
	}
	if clean.starts_with('[]') {
		elem := tc.parse_open_generic_struct_type(clean[2..]) or { return none }
		return Type(Array{
			elem_type: elem
		})
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := tc.parse_scope_param_type(clean[4..bracket_end])
			value := tc.parse_open_generic_struct_type(clean[bracket_end + 1..]) or { return none }
			return Type(Map{
				key_type:   key
				value_type: value
			})
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			elem := tc.parse_open_generic_struct_type(clean[bracket_end + 1..]) or { return none }
			len_text := clean[1..bracket_end].trim_space()
			return Type(ArrayFixed{
				elem_type: elem
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	base, args, ok := generic_type_application_parts(clean)
	if !ok || tc.generic_args_are_concrete(args) {
		return none
	}
	mut qbase := base
	if !base.contains('.') {
		resolved := tc.qualify_name(base)
		if resolved in tc.structs || resolved in tc.struct_generic_params {
			qbase = resolved
		}
	}
	if qbase !in tc.structs && qbase !in tc.struct_generic_params && base !in tc.structs
		&& base !in tc.struct_generic_params {
		return none
	}
	mut preserved_args := []string{cap: args.len}
	for arg in args {
		trimmed := arg.trim_space()
		preserved_args << if is_generic_placeholder_type(trimmed) {
			trimmed.all_after_last('.')
		} else {
			trimmed
		}
	}
	suffix := '[${preserved_args.join(', ')}]'
	return Type(Struct{
		name: qbase + suffix
	})
}

// parse_type_uncached reads parse type uncached input for types.
fn (tc &TypeChecker) parse_type_uncached(typ string) Type {
	if typ.len == 0 {
		return Type(void_)
	}
	// Preserve open generic struct applications wherever they occur in a signature or
	// expression type. Qualifying their placeholder (`T` -> `module.T`) makes it look
	// concrete to later stages and can emit invalid C types such as `Box_module__T`.
	if preserved := tc.parse_open_generic_struct_type(typ) {
		return preserved
	}
	if typ.ends_with('.typ') {
		return tc.parse_type(typ[..typ.len - 4])
	}
	if is_generic_placeholder_type(typ) && !tc.is_known_type_text(typ) {
		return unknown_type('generic placeholder `${typ}`')
	}
	if typ.starts_with('&') {
		return Type(Pointer{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('mut ') {
		return Type(Pointer{
			base_type: tc.parse_type(typ[4..])
		})
	}
	if typ.starts_with('shared ') {
		return tc.parse_type(typ[7..])
	}
	if typ.starts_with('atomic ') {
		return tc.parse_type(typ[7..])
	}
	if typ.starts_with('?') {
		return Type(OptionType{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('!') {
		return Type(ResultType{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('chan ') {
		return Type(Channel{
			elem_type: tc.parse_type(typ[5..])
		})
	}
	if typ == 'chan' {
		return Type(Channel{
			elem_type: Type(void_)
		})
	}
	if typ.starts_with('thread ') || typ == 'thread' {
		// A thread handle. The element type (the spawned fn's return type) is kept
		// in the struct name (`thread T`) so `array_of_threads.wait()` can recover
		// `[]T`. The handle itself lowers to `void*` in C (see c_type).
		return Type(Struct{
			name: typ
		})
	}
	if typ.starts_with('...') {
		return Type(Array{
			elem_type: tc.parse_type(typ[3..])
		})
	}
	if typ.starts_with('[]') {
		return Type(Array{
			elem_type: tc.parse_type(typ[2..])
		})
	}
	if typ.starts_with('map[') {
		bracket_end := find_matching_bracket(typ, 3)
		if bracket_end >= typ.len {
			return Type(Unknown{
				reason: 'malformed map type'
			})
		}
		key_str := typ[4..bracket_end]
		val_str := typ[bracket_end + 1..]
		return Type(Map{
			key_type:   tc.parse_type(key_str)
			value_type: tc.parse_type(val_str)
		})
	}
	if typ.starts_with('[') {
		idx := typ.index_u8(`]`)
		if idx > 0 {
			len_text := typ[1..idx].trim_space()
			return Type(ArrayFixed{
				elem_type: tc.parse_type(typ[idx + 1..])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if typ.starts_with('(') && typ.contains(',') {
		inner := typ[1..typ.len - 1]
		parts := split_params(inner)
		mut types := []Type{}
		for p in parts {
			types << tc.parse_type(p.trim_space())
		}
		return Type(MultiReturn{
			types: types
		})
	}
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return tc.parse_fn_type(typ)
	}
	qtyp := if tc.resolution_type_mode {
		tc.qualify_resolution_type_name(typ)
	} else {
		tc.qualify_name(typ)
	}
	allow_bare_symbol := qtyp == typ
	if typ == 'array' && tc.has_builtins && typ in tc.structs {
		return Type(Struct{
			name: typ
		})
	}
	if typ == 'map' && tc.has_builtins {
		return Type(Struct{
			name: typ
		})
	}
	if typ == 'array' && tc.has_builtins {
		return Type(Struct{
			name: typ
		})
	}
	if is_builtin_type_name(typ) {
		return builtin_type_value(typ)
	}
	if typ == 'unknown' {
		return Type(Unknown{
			reason: 'unknown'
		})
	}
	if qtyp != typ {
		if scoped_type := tc.type_from_known_symbol(qtyp) {
			return scoped_type
		}
	}
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			if resolved_type := tc.type_from_known_symbol(resolved) {
				return resolved_type
			}
		}
	}
	if builtin_error_name := tc.resolve_unqualified_builtin_error_struct_name(typ) {
		return Type(Struct{
			name: builtin_error_name
		})
	}
	if typ.starts_with('C.') {
		return Type(Struct{
			name: typ
		})
	}
	if qtyp in tc.type_aliases {
		return Type(Alias{
			name:      qtyp
			base_type: tc.parse_type(tc.type_aliases[qtyp])
		})
	}
	if allow_bare_symbol && typ in tc.type_aliases {
		return Type(Alias{
			name:      typ
			base_type: tc.parse_type(tc.type_aliases[typ])
		})
	}
	if qtyp in tc.interface_names {
		return Type(Interface{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.interface_names {
		return Type(Interface{
			name: typ
		})
	}
	if qtyp in tc.structs {
		return Type(Struct{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.structs {
		return Type(Struct{
			name: typ
		})
	}
	if qtyp in tc.flag_enums {
		return Type(Enum{
			name:    qtyp
			is_flag: true
		})
	}
	if allow_bare_symbol && typ in tc.flag_enums {
		return Type(Enum{
			name:    typ
			is_flag: true
		})
	}
	if typ.contains('.') {
		short := typ.all_after_last('.')
		if qtyp !in tc.flag_enums && typ !in tc.flag_enums && short in tc.flag_enums {
			return Type(Enum{
				name:    short
				is_flag: true
			})
		}
	}
	if qtyp in tc.enum_names {
		return Type(Enum{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.enum_names {
		return Type(Enum{
			name: typ
		})
	}
	if typ.contains('.') {
		short := typ.all_after_last('.')
		if qtyp !in tc.enum_names && typ !in tc.enum_names && short in tc.enum_names {
			return Type(Enum{
				name: short
			})
		}
	}
	if qtyp in tc.sum_types {
		return Type(SumType{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.sum_types {
		return Type(SumType{
			name: typ
		})
	}
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			if resolved_type := tc.type_from_known_symbol(resolved) {
				return resolved_type
			}
		}
	}
	if allow_bare_symbol && !typ.contains('.') {
		if resolved := tc.unique_qualified_type_name(typ) {
			if resolved in tc.type_aliases {
				return Type(Alias{
					name:      resolved
					base_type: tc.parse_type(tc.type_aliases[resolved])
				})
			}
			if resolved in tc.structs {
				return Type(Struct{
					name: resolved
				})
			}
			if resolved in tc.interface_names {
				return Type(Interface{
					name: resolved
				})
			}
			if resolved in tc.flag_enums {
				return Type(Enum{
					name:    resolved
					is_flag: true
				})
			}
			if resolved in tc.enum_names {
				return Type(Enum{
					name: resolved
				})
			}
			if resolved in tc.sum_types {
				return Type(SumType{
					name: resolved
				})
			}
		}
	}
	if generic_type_application(typ) {
		bracket := typ.index_u8(`[`)
		base := typ[..bracket]
		resolved_base := tc.resolve_imported_type_text(base)
		generic_suffix := typ[bracket..]
		_, generic_args, _ := generic_type_application_parts(typ)
		is_concrete_generic := tc.generic_args_are_concrete(generic_args)
		struct_generic_suffix := if is_concrete_generic {
			tc.qualified_generic_suffix(generic_args)
		} else {
			generic_suffix
		}
		sum_generic_suffix := if is_concrete_generic {
			tc.qualified_generic_suffix(generic_args)
		} else {
			generic_suffix
		}
		mut qbase := if tc.resolution_type_mode {
			tc.qualify_resolution_type_name(resolved_base)
		} else {
			tc.qualify_name(resolved_base)
		}
		if qbase == resolved_base && resolved_base.contains('.') {
			qbase = tc.resolve_imported_type_text(resolved_base)
		}
		allow_bare_generic_base := qbase == resolved_base
		if qbase in tc.type_aliases {
			return Type(Alias{
				name:      qbase
				base_type: tc.parse_type(tc.type_aliases[qbase])
			})
		}
		if qbase in tc.structs {
			return Type(Struct{
				name: qbase + struct_generic_suffix
			})
		}
		if qbase in tc.interface_names {
			return Type(Interface{
				name: qbase
			})
		}
		if qbase in tc.sum_types {
			return Type(SumType{
				name: qbase + sum_generic_suffix
			})
		}
		if !resolved_base.contains('.') {
			if resolved := tc.resolve_selective_import_type_symbol(resolved_base) {
				if resolved in tc.type_aliases {
					return Type(Alias{
						name:      resolved
						base_type: tc.parse_type(tc.type_aliases[resolved])
					})
				}
				if resolved in tc.structs {
					return Type(Struct{
						name: resolved + struct_generic_suffix
					})
				}
				if resolved in tc.interface_names {
					return Type(Interface{
						name: resolved
					})
				}
				if resolved in tc.sum_types {
					return Type(SumType{
						name: resolved + sum_generic_suffix
					})
				}
			}
		}
		if allow_bare_generic_base && resolved_base in tc.type_aliases {
			return Type(Alias{
				name:      resolved_base
				base_type: tc.parse_type(tc.type_aliases[resolved_base])
			})
		}
		if allow_bare_generic_base && resolved_base in tc.structs {
			return Type(Struct{
				name: resolved_base + struct_generic_suffix
			})
		}
		if allow_bare_generic_base && resolved_base in tc.interface_names {
			return Type(Interface{
				name: resolved_base
			})
		}
		if allow_bare_generic_base && resolved_base in tc.sum_types {
			return Type(SumType{
				name: resolved_base + sum_generic_suffix
			})
		}
		if is_concrete_generic && !is_builtin_type_name(resolved_base) {
			// A concrete generic instance (`Vec4[f32]`) is a monomorphized struct, even
			// when the generic base decl has been erased after monomorphization. It is
			// never a fixed array, so don't fall through to the `[N]T` handler below.
			// Qualify an imported base (`Vec4` -> `vec.Vec4`) so its c_type matches the
			// materialized struct (`vec__Vec4_f32`) everywhere it appears. A builtin base
			// (`int[seg_count]`) cannot be a generic application, so let it fall through to
			// the fixed-array handler — its bracket is a const/expression length.
			mut full := qbase + generic_suffix
			if allow_bare_generic_base && !resolved_base.contains('.') {
				if resolved := tc.unique_qualified_type_name(resolved_base) {
					full = resolved + generic_suffix
				}
			}
			return Type(Struct{
				name: full
			})
		}
	}
	if is_generic_placeholder_type(typ) {
		return unknown_type('generic type parameter `${typ}`')
	}
	if typ.contains('[') && !typ.starts_with('[') {
		// Postfix fixed-array name (`ArrayFixed.name()`): the element comes first and
		// each dimension is appended, so the OUTERMOST dimension is the trailing `[N]`
		// (`int[3][2]` is `[2][3]int`). Split on the last bracket pair so a nested fixed
		// array recovers the outer length and recurses into the inner element, instead of
		// taking the first `[N]` and dropping the rest. For a single dimension the last
		// and first brackets coincide, so this matches the previous behaviour.
		bracket := typ.last_index_u8(`[`)
		bracket_end := typ.last_index_u8(`]`)
		if bracket >= 0 && bracket_end > bracket {
			len_text := typ[bracket + 1..bracket_end].trim_space()
			return Type(ArrayFixed{
				elem_type: tc.parse_type(typ[..bracket])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if qtyp != typ {
		return Type(Struct{
			name: qtyp
		})
	}
	return Type(Struct{
		name: typ
	})
}

fn (tc &TypeChecker) qualified_generic_suffix(args []string) string {
	mut qualified_args := []string{cap: args.len}
	for arg in args {
		// Generic application args may name types from the instantiating
		// module (main's `Foo` inside a json2 specialization).
		qualified_args << tc.qualify_resolution_type_text(arg)
	}
	return '[' + qualified_args.join(', ') + ']'
}

fn (tc &TypeChecker) array_literal_elem_type(node flat.Node) Type {
	if node.children_count == 0 {
		return Type(int_)
	}
	elem_type := tc.explicit_alias_constructor_type(tc.a.child(&node, 0)) or {
		tc.resolve_type(tc.a.child(&node, 0))
	}
	mut all_numeric := true
	mut has_f32 := false
	mut has_f64 := false
	mut has_explicit_f64 := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		child_type := tc.explicit_alias_constructor_type(child_id) or { tc.resolve_type(child_id) }
		if !(child_type.is_integer() || child_type.is_float()) {
			all_numeric = false
		}
		if child_type.name() == 'f32' {
			has_f32 = true
		}
		if child_type.name() == 'f64' {
			has_f64 = true
			if !tc.is_untyped_float_literal_expr(child) {
				has_explicit_f64 = true
			}
		}
	}
	if all_numeric && has_explicit_f64 {
		return Type(f64_)
	}
	if all_numeric && has_f32 {
		return tc.parse_type('f32')
	}
	if all_numeric && has_f64 {
		return Type(f64_)
	}
	return elem_type
}

fn (tc &TypeChecker) explicit_alias_constructor_type(id flat.NodeId) ?Type {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	mut type_name := ''
	if node.kind == .cast_expr {
		type_name = node.value
	} else if node.kind == .call && node.children_count > 0 {
		fn_node_id := tc.a.child(&node, 0)
		type_name = tc.type_expr_name(fn_node_id)
	}
	if type_name.len == 0 {
		return none
	}
	qname := tc.qualify_name(type_name)
	if type_name in tc.type_aliases || qname in tc.type_aliases {
		return tc.parse_type(type_name)
	}
	return none
}

fn (tc &TypeChecker) is_untyped_float_literal_expr(node flat.Node) bool {
	match node.kind {
		.float_literal {
			return true
		}
		.prefix {
			if node.op !in [.plus, .minus] || node.children_count == 0 {
				return false
			}
			return tc.is_untyped_float_literal_expr(tc.a.child_node(&node, 0))
		}
		.paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.is_untyped_float_literal_expr(tc.a.child_node(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) is_known_type_text(typ string) bool {
	qtyp := tc.qualify_name(typ)
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			return tc.type_symbol_known(resolved)
		}
	}
	return qtyp in tc.structs || qtyp in tc.interface_names || qtyp in tc.enum_names
		|| qtyp in tc.sum_types || qtyp in tc.type_aliases
}

// unique_qualified_type_name supports unique qualified type name handling for TypeChecker.
fn (tc &TypeChecker) unique_qualified_type_name(short_name string) ?string {
	if short_name.len == 0 {
		return none
	}
	// A full scan of the five type-name maps (with an all_after_last allocation
	// per entry) is far too expensive for a per-expression helper — this is
	// called from qualify_name and sum-variant pattern checks. Build the
	// short-name index once per checker (memoized in the heap-allocated
	// type_cache, so forked parallel workers each build their own).
	if isnil(tc.type_cache) {
		return tc.unique_qualified_type_name_scan(short_name)
	}
	mut cache := tc.type_cache
	if !cache.short_type_name_index_built {
		if !isnil(cache.base) && cache.base.short_type_name_index_built {
			found := cache.base.short_type_name_index[short_name] or { return none }
			if found.len == 0 {
				return none
			}
			return found
		}
		cache.short_type_name_index_built = true
		tc.build_short_type_name_index(mut cache.short_type_name_index)
	}
	found := cache.short_type_name_index[short_name] or { return none }
	// An empty entry marks an ambiguous short name (several qualified types).
	if found.len == 0 {
		return none
	}
	return found
}

// invalidate_short_type_name_index drops the memoized short-name index; callers
// that add or remove entries in the type-name maps after the checker ran (the
// monomorphizer specializing generic structs/sum types) must invalidate it so
// the next unique_qualified_type_name query rebuilds it.
pub fn (tc &TypeChecker) invalidate_short_type_name_index() {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := tc.type_cache
	if cache.short_type_name_index_built {
		cache.short_type_name_index_built = false
		cache.short_type_name_index.clear()
	}
}

fn (tc &TypeChecker) build_short_type_name_index(mut index map[string]string) {
	tc.index_short_type_names(tc.type_aliases.keys(), mut index)
	tc.index_short_type_names(tc.structs.keys(), mut index)
	tc.index_short_type_names(tc.interface_names.keys(), mut index)
	tc.index_short_type_names(tc.enum_names.keys(), mut index)
	tc.index_short_type_names(tc.sum_types.keys(), mut index)
}

fn (tc &TypeChecker) index_short_type_names(names []string, mut index map[string]string) {
	for name in names {
		short := name.all_after_last('.')
		if prev := index[short] {
			if prev != name {
				index[short] = ''
			}
		} else {
			index[short] = name
		}
	}
}

// unique_qualified_type_name_scan is the uncached fallback used when no
// type_cache is attached to the checker.
fn (tc &TypeChecker) unique_qualified_type_name_scan(short_name string) ?string {
	mut found := ''
	for name, _ in tc.type_aliases {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.structs {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.interface_names {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.enum_names {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.sum_types {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	if found.len == 0 {
		return none
	}
	return found
}

// is_generic_placeholder_type reports whether is generic placeholder type applies in types.
fn is_generic_placeholder_type(typ string) bool {
	if typ.contains('.') {
		last := typ.all_after_last('.')
		return is_generic_placeholder_type(last)
	}
	return is_bare_generic_param(typ)
}

// parse_fn_type reads parse fn type input for types.
fn (tc &TypeChecker) parse_fn_type(typ string) Type {
	params_start := typ.index_u8(`(`) + 1
	if params_start <= 0 || params_start >= typ.len {
		return unknown_type('malformed fn type `${typ}`')
	}
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= typ.len || depth != 0 {
		return unknown_type('malformed fn type `${typ}`')
	}
	params_str := typ[params_start..params_end]
	ret_str := typ[params_end + 1..].trim_left(' ')
	mut params := []Type{}
	if params_str.len > 0 {
		param_parts := split_params(params_str)
		for p in param_parts {
			trimmed := p.trim_space()
			param_type := normalize_fn_type_param_text(trimmed)
			params << tc.parse_type(param_type)
		}
	}
	mut ret_type := Type(Void{})
	if ret_str.len > 0 {
		ret_type = tc.parse_type(ret_str)
	}
	return Type(FnType{
		params:      params
		return_type: ret_type
	})
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_from_text(typ string) ?string {
	clean := typ.trim_space()
	if !clean.starts_with('fn(') && !clean.starts_with('fn (') {
		return none
	}
	params_start := clean.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < clean.len {
		if clean[params_end] == `(` {
			depth++
		} else if clean[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= clean.len {
		return none
	}
	params_str := clean[params_start..params_end]
	ret_str := clean[params_end + 1..].trim_left(' ')
	mut params := []string{}
	mut has_c_abi_param := false
	if params_str.trim_space().len > 0 {
		for part in split_params(params_str) {
			ct, is_c_abi := tc.c_abi_fn_param_type(part)
			params << ct
			if is_c_abi {
				has_c_abi_param = true
			}
		}
	}
	if !has_c_abi_param {
		return none
	}
	ret_type := if ret_str.len > 0 { tc.parse_type(ret_str) } else { Type(Void{}) }
	ret_ct := tc.fn_ptr_return_c_type(ret_type)
	params_ct := if params.len == 0 { 'void' } else { params.join(', ') }
	return 'fn_ptr:${ret_ct}|${params_ct}'
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text(typ string) ?string {
	mut seen := map[string]bool{}
	return tc.c_abi_fn_ptr_type_for_type_text_inner(typ.trim_space(), mut seen)
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text_inner(typ string, mut seen map[string]bool) ?string {
	if typ.len == 0 || seen[typ] {
		return none
	}
	seen[typ] = true
	if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(typ) {
		return c_abi_fn
	}
	for name in [tc.qualify_name(typ), typ] {
		if name.len == 0 {
			continue
		}
		if c_abi_fn := tc.type_alias_c_abi_fns[name] {
			return c_abi_fn
		}
		if target := tc.type_aliases[name] {
			if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text_inner(target, mut seen) {
				return c_abi_fn
			}
		}
	}
	return none
}

fn (tc &TypeChecker) c_abi_fn_param_type(param string) (string, bool) {
	clean := param.trim_space()
	param_type := normalize_fn_type_param_text(clean)
	if c_abi_fn_param_name(clean).starts_with('const_') && param_type.starts_with('&') {
		base_type := tc.parse_type(param_type[1..])
		return 'const ${tc.c_type(base_type)}*', true
	}
	if param_type.starts_with('&') {
		if ct := tc.c_abi_alias_pointer_param_c_type(param_type[1..]) {
			if clean.starts_with('mut ') {
				return '${ct}*', true
			}
			return 'const ${ct}*', true
		}
	}
	return tc.c_type(tc.parse_type(param_type)), false
}

fn (tc &TypeChecker) c_abi_alias_pointer_param_c_type(typ string) ?string {
	t := tc.parse_type(typ)
	if t is Alias {
		base := c_abi_alias_c_base_type(t.base_type) or { return none }
		return tc.c_type(base)
	}
	return none
}

fn c_abi_alias_c_base_type(t Type) ?Type {
	if t is Alias {
		return c_abi_alias_c_base_type(t.base_type)
	}
	if t is Struct && t.name.starts_with('C.') {
		return t
	}
	return none
}

fn c_abi_fn_param_name(param string) string {
	mut text := param.trim_space()
	if text.starts_with('mut ') {
		text = text[4..].trim_space()
	}
	space := top_level_space_index(text)
	if space <= 0 {
		return ''
	}
	head := text[..space].trim_space()
	tail := text[space + 1..].trim_space()
	if fn_type_param_head_is_name(head, tail) {
		return head
	}
	return ''
}

fn struct_field_c_abi_key(struct_name string, field_name string) string {
	return '${struct_name}\n${field_name}'
}

// resolve_type resolves resolve type information for types.
pub fn (tc &TypeChecker) resolve_type(id flat.NodeId) Type {
	if int(id) < 0 {
		return unknown_type('missing node')
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.resolve_type(tc.a.child(&node, 0))
	}
	if node.kind == .expr_stmt {
		if node.children_count == 0 {
			return Type(void_)
		}
		if node.children_count == 1 {
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		mut types := []Type{cap: node.children_count}
		for i in 0 .. node.children_count {
			value_id := tc.a.child(&node, i)
			types << tc.expr_type(value_id) or { tc.resolve_type(value_id) }
		}
		return Type(MultiReturn{
			types: types
		})
	}
	kind_id := node_kind_id(node)
	if kind_id == 1 {
		return Type(int_)
	}
	if kind_id == 2 {
		return Type(f64_)
	}
	if kind_id == 3 {
		return Type(bool_)
	}
	if kind_id == 4 {
		if node.value.starts_with('c:') {
			return Type(Pointer{
				base_type: Type(u8_)
			})
		}
		return Type(rune_)
	}
	if kind_id == 5 || kind_id == 6 {
		return Type(string_)
	}
	if kind_id == 28 {
		return Type(voidptr_)
	}
	if kind_id == 29 {
		return Type(OptionType{
			base_type: Type(void_)
		})
	}
	if kind_id == 21 {
		return tc.fn_literal_type(node)
	}
	if kind_id == 32 {
		return tc.lambda_expr_type(node)
	}
	if kind_id == 12 && node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
		return tc.parse_type(node.typ)
	}
	if t := tc.resolved_call_type(id) {
		return t
	}
	if kind_id == 22 {
		inner := tc.resolve_type(tc.a.child(&node, 0))
		if inner is OptionType {
			return inner.base_type
		}
		if inner is ResultType {
			return inner.base_type
		}
		return inner
	}
	if smart_type := tc.smartcast_type(id) {
		return smart_type
	}
	if kind_id == 14 {
		return tc.resolve_index_type(node)
	}
	if !(tc.smartcasts.len > 0 && (kind_id == 7 || kind_id == 13)) {
		if typ := tc.cached_expr_type(id) {
			return typ
		}
	}
	if node.kind == .selector {
		if typ := tc.const_type_for_selector(node) {
			return typ
		}
	}
	if node.typ.len > 0 && node.typ != 'unknown' && !(kind_id == 12
		&& node.typ in ['int', 'array', 'map']) {
		return tc.parse_type(node.typ)
	}
	match node.kind {
		.int_literal {
			return Type(int_)
		}
		.float_literal {
			return Type(f64_)
		}
		.bool_literal {
			return Type(bool_)
		}
		.char_literal {
			return Type(rune_)
		}
		.string_literal, .string_interp {
			return Type(string_)
		}
		.nil_literal {
			return Type(voidptr_)
		}
		.none_expr {
			return Type(OptionType{
				base_type: Type(void_)
			})
		}
		.spawn_expr {
			if node.children_count == 0 {
				return tc.parse_type('thread')
			}
			child_id := tc.a.child(&node, 0)
			child_node := tc.a.nodes[int(child_id)]
			mut spawn_ret := if child_node.kind == .call {
				tc.spawn_child_call_return_type(child_node) or { tc.resolve_type(child_id) }
			} else {
				tc.resolve_type(child_id)
			}
			if spawn_ret is Unknown && child_node.kind == .call {
				spawn_ret = tc.resolve_type(child_id)
			}
			if spawn_ret is Void || spawn_ret is Unknown {
				return tc.parse_type('thread')
			}
			return tc.parse_type('thread ${spawn_ret.name()}')
		}
		.enum_val {
			return Type(int_)
		}
		.ident {
			if is_bare_generic_param(node.value) {
				return unknown_type('generic placeholder `${node.value}`')
			}
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.cur_scope.lookup(node.value) {
				return typ
			}
			if typ := tc.file_scope.lookup(node.value) {
				return typ
			}
			qname := tc.qualify_name(node.value)
			if qname != node.value {
				if typ := tc.file_scope.lookup(qname) {
					return typ
				}
			}
			if qname in tc.const_types {
				return tc.const_types[qname] or { unknown_type('unknown const `${qname}`') }
			}
			if node.value in tc.const_types {
				return tc.const_types[node.value] or {
					unknown_type('unknown const `${node.value}`')
				}
			}
			if typ := tc.fn_value_type(node.value) {
				return typ
			}
			if tc.selective_import_symbol_is_ambiguous(node.value) {
				return unknown_type('ambiguous selective import `${node.value}`')
			}
			if node.value == 'err' {
				return tc.parse_type('IError')
			}
			return unknown_type('unknown identifier `${node.value}`')
		}
		.call {
			fn_node := tc.a.child_node(&node, 0)
			if fn_node.kind == .ident {
				if typ := tc.cur_scope.lookup(fn_node.value) {
					if typ is FnType {
						return typ.return_type
					}
				}
			}
			if fn_node.kind == .selector {
				base_node := tc.a.child_node(fn_node, 0)
				if base_node.kind == .ident && base_node.value == 'C' {
					c_fn_name := 'C.${fn_node.value}'
					if c_fn_name in tc.fn_ret_types {
						return tc.fn_ret_types[c_fn_name] or {
							unknown_type('unknown return type for `${c_fn_name}`')
						}
					}
					if fn_node.value in tc.fn_ret_types {
						return tc.fn_ret_types[fn_node.value] or {
							unknown_type('unknown return type for `${fn_node.value}`')
						}
					}
					return Type(Struct{
						name: c_fn_name
					})
				}
				if base_node.kind == .ident {
					base_is_value := tc.ident_resolves_to_value(base_node.value)
					if !base_is_value {
						if resolved := tc.resolve_import_alias(base_node.value) {
							mod_name := '${resolved}.${fn_node.value}'
							if mod_name in tc.fn_ret_types {
								return tc.fn_ret_types[mod_name] or {
									unknown_type('unknown return type for `${mod_name}`')
								}
							}
							if mod_name in tc.sum_types {
								return Type(SumType{
									name: mod_name
								})
							}
							if mod_name in tc.structs {
								return Type(Struct{
									name: mod_name
								})
							}
							if mod_name in tc.enum_names {
								return Type(Enum{
									name: mod_name
								})
							}
						}
					}
					if base_node.value in tc.structs || base_node.value in tc.enum_names {
						qname := tc.qualify_name(base_node.value)
						sname := '${qname}.${fn_node.value}'
						if sname in tc.fn_ret_types {
							return tc.fn_ret_types[sname] or {
								unknown_type('unknown return type for `${sname}`')
							}
						}
					} else {
						qname := tc.qualify_name(base_node.value)
						if qname in tc.structs || qname in tc.enum_names {
							sname := '${qname}.${fn_node.value}'
							if sname in tc.fn_ret_types {
								return tc.fn_ret_types[sname] or {
									unknown_type('unknown return type for `${sname}`')
								}
							}
						}
					}
				} else if base_node.kind == .selector {
					inner := tc.a.child_node(base_node, 0)
					if inner.kind == .ident {
						mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
						full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
						if full_name in tc.fn_ret_types {
							return tc.fn_ret_types[full_name] or {
								unknown_type('unknown return type for `${full_name}`')
							}
						}
					}
				}
				if fn_typ := tc.selector_fn_type(fn_node) {
					return fn_typ.return_type
				}
				base_type := tc.resolve_type(tc.a.child(fn_node, 0))
				clean_type := unwrap_pointer(base_type)
				if fn_node.value == 'wait' {
					if ret_type := tc.thread_wait_return_type(base_type) {
						return ret_type
					}
				}
				if clean_type is ArrayFixed && fn_node.value == 'clone' {
					return Type(Array{
						elem_type: clean_type.elem_type
					})
				}
				if clean_array := array_like_type_for_method(clean_type, fn_node.value) {
					if fn_node.value == 'clone' || fn_node.value == 'reverse' {
						return clean_type
					}
					if fn_node.value == 'filter' || fn_node.value == 'sorted' {
						if receiver_is_fixed_array(clean_type) {
							// filtering a fixed array yields a dynamic array
							return Type(Array{
								elem_type: clean_array.elem_type
							})
						}
						return base_type
					}
					if fn_node.value in ['any', 'all'] {
						return Type(bool_)
					}
					if fn_node.value == 'count' {
						return Type(int_)
					}
					if fn_node.value == 'sort' {
						return Type(void_)
					}
					if fn_node.value == 'last' || fn_node.value == 'first' || fn_node.value == 'pop' {
						return array_elem_type(clean_array)
					}
					if fn_node.value == 'contains' {
						return Type(bool_)
					}
					if fn_node.value == 'repeat' || fn_node.value == 'repeat_to_depth' {
						return base_type
					}
					if fn_node.value == 'index' {
						return Type(int_)
					}
					if fn_node.value == 'join' || fn_node.value == 'str' {
						return Type(string_)
					}
					if fn_node.value == 'map' {
						return Type(Array{
							elem_type: Type(Unknown{
								reason: 'array.map'
							})
						})
					}
					if fn_node.value == 'wait' {
						// `[]thread T`.wait() joins all threads and returns `[]T`. A bare
						// `[]thread` (threads with no return value) joins to `void`. Any
						// optional/result thread payload lifts over the whole array, so
						// `[]thread !T`.wait() is `![]T`, not `[]!T`.
						// other array element type is not a thread and `.wait()` is
						// unsupported, so reject it rather than mis-typing the call as the
						// receiver array (which would emit invalid C joining non-handles).
						elem := array_elem_type(clean_array)
						if elem is Struct {
							if elem.name == 'thread' {
								return Type(void_)
							}
							if elem.name.starts_with('thread ') {
								return tc.thread_array_wait_return_type(elem.name[7..])
							}
						}
						return unknown_type('`.wait()` requires an array of threads')
					}
					if fn_node.value == 'clone' {
						return clean_type
					}
					elem_type := array_elem_type(clean_array)
					elem_name := elem_type.name()
					mut short_elem := elem_name
					mut mod_prefix := ''
					if elem_name.contains('.') {
						short_elem = elem_name.all_after_last('.')
						mod_prefix = elem_name.all_before_last('.')
					}
					arr_mname1 := '[]${short_elem}.${fn_node.value}'
					if mod_prefix.len > 0 {
						arr_mkey := '${mod_prefix}.${arr_mname1}'
						if arr_mkey in tc.fn_ret_types {
							return tc.fn_ret_types[arr_mkey] or {
								unknown_type('unknown return type for `${arr_mkey}`')
							}
						}
					}
					if arr_mname1 in tc.fn_ret_types {
						return tc.fn_ret_types[arr_mname1] or {
							unknown_type('unknown return type for `${arr_mname1}`')
						}
					}
					array_mname := 'array.${fn_node.value}'
					if array_mname in tc.fn_ret_types {
						return tc.fn_ret_types[array_mname] or {
							unknown_type('unknown return type for `${array_mname}`')
						}
					}
					return unknown_type('unknown array method `${fn_node.value}`')
				}
				if clean_type is Map {
					if fn_node.value == 'clone' {
						return base_type
					}
					if fn_node.value in ['delete', 'clear', 'free'] {
						return Type(void_)
					}
					if fn_node.value == 'keys' {
						return Type(Array{
							elem_type: clean_type.key_type
						})
					}
					if fn_node.value == 'values' {
						return Type(Array{
							elem_type: clean_type.value_type
						})
					}
					map_mname := 'map.${fn_node.value}'
					if map_mname in tc.fn_ret_types {
						return tc.fn_ret_types[map_mname] or {
							unknown_type('unknown return type for `${map_mname}`')
						}
					}
					return unknown_type('unknown map method `${fn_node.value}`')
				}
				if clean_type is String {
					mname := 'string.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if fn_node.value == 'str'
					&& (clean_type is Primitive || clean_type is Char || clean_type is Rune) {
					return Type(string_)
				}
				if (clean_type is Void || clean_type is Primitive)
					&& fn_node.value in ['vstring', 'vstring_with_len'] {
					return Type(string_)
				}
				if clean_type is Alias {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
					base_name := resolve_type_name_for_method(clean_type.base_type)
					if base_name.len > 0 {
						for base_mname in receiver_method_name_candidates(clean_type.base_type,
							fn_node.value, tc.cur_module) {
							if base_mname in tc.fn_ret_types {
								return tc.fn_ret_types[base_mname] or {
									unknown_type('unknown return type for `${base_mname}`')
								}
							}
						}
					}
				}
				if clean_type is Struct {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
					// A method on a concrete generic instance (`Box[int].clone`) is
					// registered under the open form (`Box[T].clone`); resolve it so the
					// call types as the substituted return (`Box[int]`) rather than the
					// bare base the collapsed open signature would yield.
					if ci := tc.resolve_generic_struct_method(clean_type.name, fn_node.value) {
						return ci.return_type
					}
				}
				if clean_type is Interface {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is SumType {
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is Enum {
					if fn_node.value == 'str' {
						return Type(string_)
					}
					mname := '${clean_type.name}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
				if clean_type is Primitive {
					mname := '${prim_c_type_from(clean_type.props, clean_type.size)}.${fn_node.value}'
					if mname in tc.fn_ret_types {
						return tc.fn_ret_types[mname] or {
							unknown_type('unknown return type for `${mname}`')
						}
					}
				}
			}
			if local_name := tc.local_bare_fn_key(fn_node.value) {
				return tc.fn_ret_types[local_name] or {
					unknown_type('unknown return type for `${local_name}`')
				}
			}
			if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
				if imported_name in tc.fn_ret_types {
					return tc.fn_ret_types[imported_name] or {
						unknown_type('unknown return type for `${imported_name}`')
					}
				}
			}
			if fn_node.value in tc.fn_ret_types {
				return tc.fn_ret_types[fn_node.value] or {
					unknown_type('unknown return type for `${fn_node.value}`')
				}
			}
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			$if debug {
				eprintln('warning: unknown fn return type `${fn_node.value}`')
			}
			return unknown_type('unknown function `${fn_node.value}`')
		}
		.infix {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return Type(bool_)
			}
			lhs_id := tc.a.child(&node, 0)
			rhs_id := tc.a.child(&node, 1)
			lt := tc.resolve_type(lhs_id)
			lt_raw := lt
			rt := tc.resolve_type(rhs_id)
			rt_raw := rt
			if node.op in [.plus, .minus] {
				if lt is Pointer && rt.is_integer() {
					return lt_raw
				}
				if node.op == .plus && rt is Pointer && lt.is_integer() {
					return rt_raw
				}
			}
			if operator_ret := tc.infix_operator_return_type(node.op, lt, rt) {
				return operator_ret
			}
			if node.op == .right_shift_unsigned {
				return unsigned_shift_result_type(lt)
			}
			if lt is String {
				return lt_raw
			}
			if rt is String {
				return rt_raw
			}
			lhs := tc.a.nodes[int(lhs_id)]
			rhs := tc.a.nodes[int(rhs_id)]
			if int_promoted := int_literal_promoted_infix_type(lhs, rhs, rt) {
				return int_promoted
			}
			if int_promoted := int_literal_promoted_infix_type(rhs, lhs, lt) {
				return int_promoted
			}
			if lt.is_float() || rt.is_float() {
				if type_is_f32(lt) && rhs.kind == .float_literal {
					return Type(f32_)
				}
				if type_is_f32(rt) && lhs.kind == .float_literal {
					return Type(f32_)
				}
				return Type(f64_)
			}
			return lt
		}
		.prefix {
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			if node.op == .amp {
				child_id := tc.a.child(&node, 0)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .ident && child.value.len > 0 {
					if base := tc.cur_fn_mut_param_base_types[child.value] {
						if tc.mut_param_binding_matches_lvalue(child.value) {
							return Type(Pointer{
								base_type: base
							})
						}
					}
				}
				inner := tc.resolve_type(child_id)
				return Type(Pointer{
					base_type: inner
				})
			}
			if node.op == .mul {
				inner := tc.resolve_type(tc.a.child(&node, 0))
				if inner is Pointer {
					return inner.base_type
				}
				return inner
			}
			if node.op == .arrow {
				inner := tc.resolve_type(tc.a.child(&node, 0))
				if inner is Channel {
					return inner.elem_type
				}
			}
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		.paren {
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		.or_expr {
			inner := tc.resolve_type(tc.a.child(&node, 0))
			if inner is OptionType {
				return inner.base_type
			}
			if inner is ResultType {
				return inner.base_type
			}
			return inner
		}
		.struct_init {
			return tc.parse_type(node.value)
		}
		.assoc {
			if node.value.len > 0 {
				return tc.parse_type(node.value)
			}
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(&node, 0))
			}
			return unknown_type('missing assoc base')
		}
		.sizeof_expr {
			return Type(USize{})
		}
		.offsetof_expr {
			return Type(USize{})
		}
		.cast_expr {
			return tc.parse_type(node.value)
		}
		.selector {
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.enum_selector_type(&node) {
				return typ
			}
			base_node := tc.a.child_node(&node, 0)
			if base_node.kind == .typeof_expr {
				if node.value == 'name' {
					return Type(String{})
				}
				if node.value == 'idx' {
					return Type(int_)
				}
			}
			if base_node.kind == .ident {
				if base_node.value == 'os' && node.value == 'args' {
					return Type(Array{
						elem_type: Type(String{})
					})
				}
				if gt := tc.file_scope.lookup(node.value) {
					if gt !is Unknown {
						return gt
					}
				}
				resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
				qname := '${resolved}.${node.value}'
				if qname.starts_with('C.') {
					if gt := tc.c_globals[qname] {
						return gt
					}
				}
				if qname in tc.const_types {
					typ := tc.const_types[qname] or { unknown_type('unknown const `${qname}`') }
					return tc.const_type_from_initializer(qname, typ)
				}
				if key := tc.const_key_for_suffix(qname) {
					typ := tc.const_types[key] or { unknown_type('unknown const `${key}`') }
					return tc.const_type_from_initializer(key, typ)
				}
			}
			base_type := tc.resolve_type(tc.a.child(&node, 0))
			clean0 := unwrap_pointer(base_type)
			mut clean := clean0
			if clean0 is Alias {
				clean = clean0.base_type
			}
			if typ := option_result_selector_type(clean, node.value) {
				return typ
			}
			if node.value == 'len' {
				if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
					return Type(int_)
				}
			}
			if clean is Struct {
				if typ := tc.struct_field_type(clean.name, node.value) {
					return typ
				}
				if typ := tc.method_value_type(clean.name, node.value) {
					return typ
				}
			}
			if clean is Interface {
				if typ := tc.interface_field_type(clean.name, node.value) {
					return typ
				}
			}
			if clean is MultiReturn {
				if typ := multi_return_selector_type(clean, node.value) {
					return typ
				}
			}
			if clean is SumType {
				if typ := tc.lowered_sum_selector_type(clean, node.value) {
					return typ
				}
				if typ := tc.sum_shared_field_type(clean, node.value) {
					return typ
				}
			}
			if clean is Array || clean is Map || clean is String {
				sname := if clean is Array {
					'array'
				} else if clean is Map {
					'map'
				} else {
					'string'
				}
				if sname in tc.structs {
					for f in tc.structs[sname] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			if clean is Primitive && base_node.kind == .selector {
				vname := base_node.value.replace('__', '.')
				if vname in tc.structs {
					for f in tc.structs[vname] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			return unknown_type('unknown selector `${node.value}`')
		}
		.array_literal {
			if node.children_count > 0 {
				elem_type := tc.array_literal_elem_type(node)
				return Type(Array{
					elem_type: elem_type
				})
			}
			return Type(Array{
				elem_type: Type(int_)
			})
		}
		.postfix {
			if node.children_count == 0 {
				return unknown_type('missing postfix expression')
			}
			child_id := tc.a.child(&node, 0)
			child := tc.a.nodes[int(child_id)]
			if node.op == .not && child.kind == .array_literal {
				elem_type := if child.children_count > 0 {
					tc.resolve_type(tc.a.child(&child, 0))
				} else {
					Type(int_)
				}
				return Type(ArrayFixed{
					elem_type: elem_type
					len:       child.children_count
				})
			}
			return tc.resolve_type(child_id)
		}
		.index {
			return tc.resolve_index_type(node)
		}
		.array_init {
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			t := tc.parse_type(node.value)
			raw_t := t
			if t is ArrayFixed {
				return raw_t
			}
			return Type(Array{
				elem_type: t
			})
		}
		.map_init {
			if node.value.len > 0 {
				return tc.parse_type(node.value)
			}
			if node.children_count >= 2 {
				first_id := tc.a.child(&node, 0)
				first := tc.a.nodes[int(first_id)]
				if first.kind == .prefix && first.value == '...' && first.children_count > 0 {
					return tc.resolve_type(tc.a.child(&first, 0))
				}
				key_type := tc.resolve_type(first_id)
				mut value_type := tc.resolve_type(tc.a.child(&node, 1))
				if value_type is ArrayFixed {
					value_type = Type(Array{
						elem_type: value_type.elem_type
					})
				}
				return Type(Map{
					key_type:   key_type
					value_type: value_type
				})
			}
			return Type(Map{
				key_type:   Type(string_)
				value_type: Type(int_)
			})
		}
		.comptime_if {
			take_then := tc.comptime_threads_condition_value(node.value) or {
				return unknown_type('unresolved compile-time expression condition `${node.value}`')
			}
			branch_index := if take_then { 0 } else { 1 }
			if branch_index >= node.children_count {
				return Type(void_)
			}
			return tc.resolve_type(tc.a.child(&node, branch_index))
		}
		.if_expr {
			return tc.if_expr_tail_type(id)
		}
		.lock_expr {
			if node.children_count == 0 {
				return Type(void_)
			}
			return tc.resolve_type(tc.a.child(&node, node.children_count - 1))
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				t := tc.branch_tail_type(tc.a.child(&node, i))
				if t !is Void {
					return t
				}
			}
			return Type(void_)
		}
		.in_expr {
			return Type(bool_)
		}
		.block {
			if node.children_count > 0 {
				last_id := tc.a.child(&node, node.children_count - 1)
				last := tc.a.nodes[int(last_id)]
				if last.kind == .expr_stmt {
					return tc.resolve_type(tc.a.child(&last, 0))
				}
				return tc.resolve_type(last_id)
			}
			return Type(void_)
		}
		.as_expr {
			return tc.parse_type(node.value)
		}
		.is_expr {
			return Type(bool_)
		}
		else {
			$if debug {
				eprintln('warning: unhandled node kind .${node.kind} in resolve_type')
			}
			return unknown_type('unhandled node kind .${node.kind}')
		}
	}
}

// fn_literal_type supports fn literal type handling for TypeChecker.
fn (tc &TypeChecker) fn_literal_type(node flat.Node) Type {
	mut params := []Type{}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			params << tc.parse_type(normalize_fn_type_param_text(child.typ))
		}
	}
	return Type(FnType{
		params:      params
		return_type: tc.parse_type(node.typ)
	})
}

// lambda_expr_type supports lambda expr type handling for TypeChecker.
fn (tc &TypeChecker) lambda_expr_type(node flat.Node) Type {
	mut params := []Type{}
	if node.children_count > 0 {
		for _ in 0 .. node.children_count - 1 {
			params << unknown_type('lambda parameter')
		}
	}
	ret_type := if node.children_count > 0 {
		tc.resolve_type(tc.a.child(&node, node.children_count - 1))
	} else {
		Type(void_)
	}
	return Type(FnType{
		params:      params
		return_type: ret_type
	})
}

// resolve_index_type resolves resolve index type information for types.
fn (tc &TypeChecker) resolve_index_type(node flat.Node) Type {
	base_type0 := tc.resolve_type(tc.a.child(&node, 0))
	base_type := unalias_type(base_type0)
	if base_type is OptionType {
		inner := unalias_type(base_type.base_type)
		result := tc.resolve_index_base_type(inner, node)
		if result is Unknown {
			return result
		}
		return Type(OptionType{
			base_type: result
		})
	}
	return tc.resolve_index_base_type(base_type, node)
}

fn (tc &TypeChecker) resolve_index_base_type(base_type Type, node flat.Node) Type {
	if node.value == 'range' {
		if base_type is Array {
			return base_type
		}
		if base_type is ArrayFixed {
			return Type(Array{
				elem_type: fixed_array_elem_type(base_type)
			})
		}
		if base_type is Pointer {
			inner0 := pointer_base_type(base_type)
			mut inner := inner0
			if inner0 is Alias {
				inner = inner0.base_type
			}
			if inner is Array {
				return inner
			}
			if inner is ArrayFixed {
				return Type(Array{
					elem_type: fixed_array_elem_type(inner)
				})
			}
		}
		return Type(string_)
	}
	if base_type is Map {
		return map_value_type(base_type)
	}
	if base_type is Array {
		return array_elem_type(base_type)
	}
	if base_type is ArrayFixed {
		return fixed_array_elem_type(base_type)
	}
	if base_type is Pointer {
		inner0 := pointer_base_type(base_type)
		mut inner := inner0
		if inner0 is Alias {
			inner = inner0.base_type
		}
		if inner is Map {
			// `mut m map[K]V` params resolve to a pointer-to-map; indexing still
			// yields the value type V, not the whole map.
			return map_value_type(inner)
		}
		if inner is Array {
			return array_elem_type(inner)
		}
		if inner is ArrayFixed {
			return fixed_array_elem_type(inner)
		}
		return inner
	}
	if base_type is String {
		return Type(u8_)
	}
	return unknown_type('cannot index `${base_type.name()}`')
}

// c_type supports c type handling for TypeChecker.
pub fn (tc &TypeChecker) c_type(t Type) string {
	if t is Pointer || t is FnType || t is Struct || t is Interface || t is SumType || t is Alias
		|| t is MultiReturn || t is ArrayFixed {
		if tc.type_cache != unsafe { nil } {
			key := t.name()
			mut cache := unsafe { tc.type_cache }
			if !isnil(cache.base) {
				if cached := cache.base.c_entries[key] {
					return cached
				}
			}
			if cached := cache.c_entries[key] {
				return cached
			}
			result := tc.c_type_uncached(t)
			cache.c_entries[key] = result
			return result
		}
	}
	return tc.c_type_uncached(t)
}

// c_type_uncached supports c type uncached handling for TypeChecker.
fn (tc &TypeChecker) c_type_uncached(t Type) string {
	if t is Void {
		return 'void'
	}
	if t is Unknown {
		return 'int'
	}
	if t is Nil {
		return 'void*'
	}
	if t is None {
		return 'Optional'
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Rune {
		return 'u32'
	}
	if t is ISize {
		return 'ptrdiff_t'
	}
	if t is USize {
		return 'size_t'
	}
	if t is Primitive {
		return prim_c_type(t)
	}
	if t is Array {
		return 'Array'
	}
	if t is ArrayFixed {
		// The typedef name preserves the source length text while
		// the emitted C dimension is folded separately by fixed_array_len_value.
		len_text := if t.len_expr.len > 0 { t.len_expr } else { t.len.str() }
		len_name := naming.type_name_part(len_text)
		return 'Array_fixed_${naming.type_name_part(tc.fixed_array_elem_c_type(t.elem_type))}_${len_name}'
	}
	if t is Channel {
		return 'chan'
	}
	if t is Map {
		return 'map'
	}
	if t is Pointer {
		return tc.c_type(t.base_type) + '*'
	}
	if t is FnType {
		ret := tc.fn_ptr_return_c_type(t.return_type)
		if t.params.len == 0 {
			return 'fn_ptr:${ret}|void'
		}
		mut params := []string{}
		for i in 0 .. t.params.len {
			params << tc.c_type(fn_param_type(t, i))
		}
		return 'fn_ptr:${ret}|${params.join(', ')}'
	}
	if t is OptionType {
		return 'Optional'
	}
	if t is ResultType {
		return 'Optional'
	}
	if t is Struct {
		if t.name == 'thread' || t.name.ends_with('.thread') || t.name.starts_with('thread ') {
			return 'void*'
		}
		if t.name.starts_with('C.') {
			raw := t.name[2..]
			if raw.ends_with('_s')
				|| (raw.len > 0 && raw[0] >= `a` && raw[0] <= `z` && !raw.ends_with('_t')) {
				return 'struct ${raw}'
			}
			return raw
		}
		if t.name.contains('.') && t.name !in tc.structs && t.name !in tc.type_aliases {
			// An import-alias prefix (`json.Any` for `import x.json2 as json`)
			// can survive in recorded types; resolve by unique short name.
			if resolved := tc.unique_qualified_type_name(t.name.all_after_last('.')) {
				return tc.c_struct_type_name(resolved)
			}
		}
		return tc.c_struct_type_name(t.name)
	}
	if t is Interface {
		return naming.c_name(t.name)
	}
	if t is Enum {
		return 'int'
	}
	if t is SumType {
		return naming.c_name(t.name)
	}
	if t is Alias {
		// Follow the alias chain iteratively. A self-referential / cyclic alias (whose
		// base resolves back to itself) would otherwise recurse forever here — the cache
		// is only populated after the recursive call returns — overflowing the stack.
		mut cur := Type(t)
		for _ in 0 .. 1000 {
			if cur is Alias {
				cur = cur.base_type
			} else {
				return tc.c_type(cur)
			}
		}
		return 'void*'
	}
	if t is MultiReturn {
		mut parts := []string{}
		for ty in t.types {
			parts << naming.type_name_part(tc.c_type(ty))
		}
		return 'multi_return_${parts.join('_')}'
	}
	return 'int'
}

fn (tc &TypeChecker) c_struct_type_name(name string) string {
	base, args, ok := generic_type_application_parts(name)
	if !ok {
		return naming.c_name(name)
	}
	mut normalized_args := []string{cap: args.len}
	for arg in args {
		normalized_args << tc.c_generic_struct_arg_name(arg)
	}
	return naming.c_name('${base}[${normalized_args.join(', ')}]')
}

fn (tc &TypeChecker) c_generic_struct_arg_name(arg string) string {
	clean := arg.trim_space()
	if fixed := tc.c_generic_struct_fixed_array_arg_name(clean) {
		return fixed
	}
	if target := tc.type_aliases[clean] {
		if fixed := tc.c_generic_struct_fixed_array_arg_name(target) {
			return fixed
		}
	}
	if clean.contains('.') {
		short := clean.all_after_last('.')
		if target := tc.type_aliases[clean] {
			if fixed := tc.c_generic_struct_fixed_array_arg_name(target) {
				return fixed
			}
		}
		if clean !in tc.structs && clean !in tc.struct_generic_params
			&& clean !in tc.interface_names && clean !in tc.sum_types && clean !in tc.enum_names
			&& clean !in tc.flag_enums && clean !in tc.type_aliases {
			if short in tc.structs || short in tc.struct_generic_params
				|| short in tc.interface_names || short in tc.sum_types || short in tc.enum_names
				|| short in tc.flag_enums || short in tc.type_aliases {
				return short
			}
		}
	}
	return clean
}

fn (tc &TypeChecker) c_generic_struct_fixed_array_arg_name(arg string) ?string {
	clean := arg.trim_space()
	if !clean.starts_with('[') {
		return none
	}
	typ := tc.parse_type(clean)
	match typ {
		ArrayFixed {
			len_text := if typ.len_expr.len > 0 { typ.len_expr } else { typ.len.str() }
			len_name := naming.type_name_part(len_text)
			elem_name := naming.type_name_part(tc.c_type(typ.elem_type))
			return '${elem_name}_${len_name}'
		}
		else {
			return none
		}
	}
}

fn (tc &TypeChecker) fixed_array_elem_c_type(t Type) string {
	if t is OptionType {
		return tc.optional_c_type_name(t.base_type)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t.base_type)
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) fn_ptr_return_c_type(t Type) string {
	if t is Void {
		return 'void'
	}
	if t is OptionType {
		return tc.optional_c_type_name(t.base_type)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t.base_type)
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) optional_c_type_name(base_type Type) string {
	if base_type is Void {
		return 'Optional'
	}
	inner_ct := tc.c_type(base_type)
	if inner_ct == 'int' {
		return 'Optional'
	}
	return 'Optional_${inner_ct.replace('*', 'ptr').replace(' ', '_')}'
}

// resolve_type_name_for_method resolves resolve type name for method information for types.
// resolve_generic_struct_method resolves a method call on a generic-struct
// instance (e.g. `Vec4[f32].r_sqrt`). The method is registered against the
// generic form (`Vec4[T].r_sqrt`); this maps the instance's concrete type
// arguments onto the generic parameters and substitutes them into the method's
// signature, so the pre-transform checker accepts the call. The transformer's
// monomorphize pass later materialises the concrete method body.
pub fn (tc &TypeChecker) resolve_generic_struct_method(type_name string, method string) ?CallInfo {
	bracket := type_name.index_u8(`[`)
	if bracket <= 0 || !type_name.ends_with(']') {
		return none
	}
	base := type_name[..bracket]
	args_str := type_name[bracket + 1..type_name.len - 1]
	mut concrete_args := []string{}
	for a in split_generic_arg_list(args_str) {
		concrete_args << a.trim_space()
	}
	mut generic_base := ''
	mut params := []string{}
	for candidate in tc.generic_struct_method_base_candidates(base) {
		candidate_params := tc.struct_generic_params[candidate] or { continue }
		if candidate_params.len == 0 || candidate_params.len != concrete_args.len {
			continue
		}
		candidate_key := '${candidate}[${candidate_params.join(', ')}].${method}'
		if candidate_key !in tc.fn_ret_types {
			continue
		}
		generic_base = candidate
		params = candidate_params.clone()
		break
	}
	if generic_base.len == 0 {
		return none
	}
	if params.len == 0 || params.len != concrete_args.len {
		return none
	}
	generic_key := '${generic_base}[${params.join(', ')}].${method}'
	ret := tc.fn_ret_types[generic_key] or { return none }
	// Prefer substituting the original signature TEXT: parsing `Box[T]` collapses the
	// non-concrete application to the bare `Box`, so substituting the already-parsed type
	// cannot recover `Box[int]`. Re-substituting the text and re-parsing does.
	mut sub_ret := tc.substitute_generic_type(ret, concrete_args, params)
	if ret_text := tc.fn_ret_type_texts[generic_key] {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text,
			concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts := tc.fn_param_type_texts[generic_key] {
		for i, pt in param_texts {
			receiver_clean := pt.trim_space().trim_left('&').trim_space()
			receiver_base := if receiver_clean.contains('[') {
				receiver_clean.all_before('[')
			} else {
				receiver_clean
			}
			if i == 0 && (receiver_base == generic_base
				|| receiver_base == generic_base.all_after_last('.')
				|| receiver_base == base
				|| receiver_base == base.all_after_last('.')) {
				sub_params << generic_method_receiver_param(type_name, pt)
				continue
			}
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_text(pt,
				concrete_args, params))
		}
	} else if ptypes := tc.fn_param_types[generic_key] {
		for pt in ptypes {
			sub_params << tc.substitute_generic_type(pt, concrete_args, params)
		}
	}
	return CallInfo{
		name:          generic_key
		params:        sub_params
		shared_params: tc.fn_shared_params[generic_key] or { []bool{} }
		return_type:   sub_ret
		has_receiver:  true
		is_variadic:   tc.fn_variadic[generic_key] or { false }
		params_known:  true
	}
}

fn (tc &TypeChecker) generic_struct_method_base_candidates(base string) []string {
	mut candidates := []string{}
	push_receiver_method_candidate(mut candidates, base)
	if base.contains('.') {
		resolved := tc.resolve_imported_type_text(base)
		push_receiver_method_candidate(mut candidates, resolved)
		push_receiver_method_candidate(mut candidates, resolved.all_after_last('.'))
		push_receiver_method_candidate(mut candidates, base.all_after_last('.'))
	} else {
		if resolved := tc.resolve_selective_import_type_symbol(base) {
			push_receiver_method_candidate(mut candidates, resolved)
			push_receiver_method_candidate(mut candidates, resolved.all_after_last('.'))
		}
		qualified := tc.qualify_name(base)
		push_receiver_method_candidate(mut candidates, qualified)
	}
	return candidates
}

pub fn (tc &TypeChecker) resolve_generic_sum_method(type_name string, method string) ?CallInfo {
	mut base := type_name
	mut concrete_args := []string{}
	parsed_base, parsed_args, is_generic := generic_type_application_parts(type_name)
	if is_generic {
		base = parsed_base
		for arg in parsed_args {
			concrete_args << arg.trim_space()
		}
	}
	params := tc.sum_params_for_base(base)
	if params.len == 0 {
		return none
	}
	if !is_generic {
		concrete_args = params.clone()
	}
	if params.len != concrete_args.len {
		return none
	}
	generic_key := '${base}[${params.join(', ')}].${method}'
	ret := tc.fn_ret_types[generic_key] or { return none }
	mut sub_ret := tc.substitute_generic_type(ret, concrete_args, params)
	if ret_text := tc.fn_ret_type_texts[generic_key] {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text,
			concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts := tc.fn_param_type_texts[generic_key] {
		for pt in param_texts {
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_text(pt,
				concrete_args, params))
		}
	} else if ptypes := tc.fn_param_types[generic_key] {
		for pt in ptypes {
			sub_params << tc.substitute_generic_type(pt, concrete_args, params)
		}
	}
	return CallInfo{
		name:          generic_key
		params:        sub_params
		shared_params: tc.fn_shared_params[generic_key] or { []bool{} }
		return_type:   sub_ret
		has_receiver:  true
		is_variadic:   tc.fn_variadic[generic_key] or { false }
		params_known:  true
	}
}

fn generic_method_receiver_param(type_name string, param_text string) Type {
	if param_text.trim_space().starts_with('&') {
		return Type(Pointer{
			base_type: Type(Struct{
				name: type_name
			})
		})
	}
	return Type(Struct{
		name: type_name
	})
}

// subst_generic_text textually substitutes the generic parameter names `params` with the
// concrete argument texts `args` inside a type text, preserving the generic application
// form so a method signature mentioning the receiver type (`Box[T]`) becomes the concrete
// instance (`Box[int]`) when re-parsed, instead of collapsing to the bare base. Prefix and
// container forms (`&`, `mut`, `?`, `!`, `...`, `shared`, `[]`, `map[`, `[N]`) recurse into the
// element type; a bare parameter name is replaced with its argument.
fn subst_generic_text(typ string, args []string, params []string) string {
	clean := typ.trim_space()
	if clean.len == 0 || args.len == 0 || params.len != args.len {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + subst_generic_text(clean[4..], args, params)
	}
	if clean.starts_with('?') {
		return '?' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('!') {
		return '!' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('...') {
		return '...' + subst_generic_text(clean[3..], args, params)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + subst_generic_text(clean[7..], args, params)
	}
	if clean.starts_with('[]') {
		return '[]' + subst_generic_text(clean[2..], args, params)
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_params(clean[1..clean.len - 1]) {
			parts << subst_generic_text(part, args, params)
		}
		return '(${parts.join(', ')})'
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := subst_generic_text(clean[4..bracket_end], args, params)
			val := subst_generic_text(clean[bracket_end + 1..], args, params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + subst_generic_text(clean[bracket_end +
				1..], args, params)
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		// A function-type parameter (`fn (T) int`) carries the generic params in its own
		// signature; substitute each parameter and the return type so a `Box[string].apply`
		// callback is expected as `fn (string) int`, not the unsubstituted `fn (T) int`.
		params_start := clean.index_u8(`(`) + 1
		mut depth := 1
		mut params_end := params_start
		for params_end < clean.len {
			if clean[params_end] == `(` {
				depth++
			} else if clean[params_end] == `)` {
				depth--
				if depth == 0 {
					break
				}
			}
			params_end++
		}
		if params_end < clean.len {
			mut fn_parts := []string{}
			params_str := clean[params_start..params_end]
			if params_str.trim_space().len > 0 {
				for part in split_params(params_str) {
					fn_parts << subst_generic_text(normalize_fn_type_param_text(part), args, params)
				}
			}
			ret_str := clean[params_end + 1..].trim_space()
			if ret_str.len > 0 {
				return 'fn(${fn_parts.join(', ')}) ${subst_generic_text(ret_str, args, params)}'
			}
			return 'fn(${fn_parts.join(', ')})'
		}
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_params(clean[bracket + 1..bracket_end]) {
				parts << subst_generic_text(part, args, params)
			}
			return clean[..bracket] + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	for i, p in params {
		if clean == p {
			return args[i]
		}
	}
	return clean
}

// split_generic_arg_list splits a comma-separated generic argument list at the
// top bracket level (so nested types like `map[int]string` stay intact).
fn split_generic_arg_list(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `[` || c == `(` {
			depth++
		} else if c == `]` || c == `)` {
			depth--
		} else if c == `,` && depth == 0 {
			parts << s[start..i]
			start = i + 1
		}
	}
	parts << s[start..]
	return parts
}

fn resolve_type_name_for_method(t Type) string {
	if t is Alias {
		return t.name
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Rune {
		return 'rune'
	}
	if t is ISize {
		return 'isize'
	}
	if t is USize {
		return 'usize'
	}
	if t is Array {
		return '[]${nested_type_name(t.elem_type)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		return '${nested_type_name(t.elem_type)}[${len_text}]'
	}
	if t is Map {
		return 'map[${nested_type_name(t.key_type)}]${nested_type_name(t.value_type)}'
	}
	if t is Primitive {
		return prim_name(t)
	}
	return ''
}

fn receiver_type_name_variant(t Type, fixed_array_prefix bool, shorten_modules bool) string {
	if t is Alias {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Struct {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Interface {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is SumType {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Enum {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is String {
		return 'string'
	}
	if t is Array {
		return '[]${receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		elem := receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)
		if fixed_array_prefix {
			return '[${len_text}]${elem}'
		}
		return '${elem}[${len_text}]'
	}
	if t is Map {
		key := receiver_type_name_variant(t.key_type, fixed_array_prefix, shorten_modules)
		value := receiver_type_name_variant(t.value_type, fixed_array_prefix, shorten_modules)
		return 'map[${key}]${value}'
	}
	if t is Primitive {
		return prim_c_type_from(t.props, t.size)
	}
	return receiver_leaf_type_name(t.name(), shorten_modules)
}

fn receiver_leaf_type_name(name string, shorten_modules bool) string {
	if shorten_modules && name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

fn receiver_type_name_variants(t Type) []string {
	mut names := []string{}
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, false, false))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, false, true))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, true, false))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, true, true))
	return names
}

fn receiver_type_module_names(t Type) []string {
	mut names := []string{}
	if t is Array {
		for name in receiver_type_module_names(t.elem_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else if t is ArrayFixed {
		for name in receiver_type_module_names(t.elem_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else if t is Map {
		for name in receiver_type_module_names(t.key_type) {
			push_receiver_method_candidate(mut names, name)
		}
		for name in receiver_type_module_names(t.value_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else {
		name := t.name()
		if name.contains('.') {
			push_receiver_method_candidate(mut names, name.all_before_last('.'))
		}
	}
	return names
}

fn push_receiver_method_candidate(mut names []string, name string) {
	if name.len > 0 && name !in names {
		names << name
	}
}

fn (tc &TypeChecker) unique_receiver_method_suffix_match(candidates []string) ?string {
	mut found := ''
	for candidate in candidates {
		name := tc.receiver_method_suffix_index[candidate] or { continue }
		if name == receiver_method_suffix_ambiguous {
			return none
		}
		if found.len > 0 && found != name {
			return none
		}
		found = name
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn module_can_prefix_collection_receiver(module_name string) bool {
	return module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
}

fn exact_array_receiver_method_candidates(t Array, method string, module_name string) []string {
	mut names := []string{}
	append_array_receiver_method_candidates(mut names, t, method, module_name)
	return names
}

fn append_array_receiver_method_candidates(mut names []string, t Array, method string, module_name string) {
	elem_types := receiver_type_name_variants(t.elem_type)
	if elem_types.len == 0 {
		return
	}
	for elem_type in elem_types {
		push_receiver_method_candidate(mut names, '[]${elem_type}.${method}')
	}
	mut module_names := receiver_type_module_names(t.elem_type)
	if module_can_prefix_collection_receiver(module_name) {
		push_receiver_method_candidate(mut module_names, module_name)
	}
	for mod_name in module_names {
		for elem_type in elem_types {
			push_receiver_method_candidate(mut names, '${mod_name}.[]${elem_type}.${method}')
		}
	}
}

fn append_map_receiver_method_candidates(mut names []string, t Map, method string, module_name string) {
	key_types := receiver_type_name_variants(t.key_type)
	value_types := receiver_type_name_variants(t.value_type)
	if key_types.len == 0 || value_types.len == 0 {
		return
	}
	mut map_types := []string{}
	for key_type in key_types {
		for value_type in value_types {
			push_receiver_method_candidate(mut map_types, 'map[${key_type}]${value_type}')
		}
	}
	for map_type in map_types {
		push_receiver_method_candidate(mut names, '${map_type}.${method}')
	}
	mut module_names := []string{}
	if module_can_prefix_collection_receiver(module_name) {
		push_receiver_method_candidate(mut module_names, module_name)
	}
	for mod_name in receiver_type_module_names(t.key_type) {
		push_receiver_method_candidate(mut module_names, mod_name)
	}
	for mod_name in receiver_type_module_names(t.value_type) {
		push_receiver_method_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			push_receiver_method_candidate(mut names, '${mod_name}.${map_type}.${method}')
		}
	}
}

fn receiver_method_name_candidates(t Type, method string, module_name string) []string {
	mut names := []string{}
	type_name := resolve_type_name_for_method(t)
	if type_name.len > 0 {
		push_receiver_method_candidate(mut names, '${type_name}.${method}')
	}
	mut clean := t
	if clean is Alias {
		clean = clean.base_type
	}
	if clean is Array {
		append_array_receiver_method_candidates(mut names, clean, method, module_name)
		array_name := 'array.${method}'
		push_receiver_method_candidate(mut names, array_name)
	}
	if clean is Map {
		append_map_receiver_method_candidates(mut names, clean, method, module_name)
		map_name := 'map.${method}'
		push_receiver_method_candidate(mut names, map_name)
	}
	return names
}

fn (tc &TypeChecker) infix_operator_return_type(op flat.Op, lhs Type, rhs Type) ?Type {
	op_name := infix_operator_name(op) or { return none }
	lhs_name := resolve_type_name_for_method(unwrap_pointer(lhs))
	if lhs_name.len == 0 {
		return none
	}
	method_name := '${lhs_name}.${op_name}'
	for candidate in [method_name, tc.cached_c_name(method_name)] {
		ret := tc.fn_ret_types[candidate] or { continue }
		params := tc.fn_param_types[candidate] or { continue }
		if params.len < 2 {
			continue
		}
		if !tc.receiver_compatible(lhs, params[0]) {
			continue
		}
		if !tc.type_compatible(rhs, params[1]) {
			continue
		}
		return ret
	}
	return none
}

fn int_literal_promoted_infix_type(lit flat.Node, other flat.Node, other_type Type) ?Type {
	if lit.kind != .int_literal || other.kind == .int_literal {
		return none
	}
	value := v_int_literal_value(lit.value) or { return none }
	if unsigned_type_accepts_int_literal(other_type, value) {
		return other_type
	}
	return none
}

fn unsigned_type_accepts_int_literal(t Type, value int) bool {
	if value < 0 {
		return false
	}
	if t is Primitive {
		if !t.props.has(.integer) || !t.props.has(.unsigned) {
			return false
		}
		max := match t.size {
			8 { 255 }
			16 { 65535 }
			else { return true }
		}

		return value <= max
	}
	return false
}

fn type_is_f32(t Type) bool {
	if t is Primitive {
		return t.props.has(.float) && t.size == 32
	}
	return false
}

fn infix_operator_name(op flat.Op) ?string {
	match op {
		.plus { return '+' }
		.minus { return '-' }
		.mul { return '*' }
		.div { return '/' }
		.mod { return '%' }
		.pipe { return '|' }
		.xor { return '^' }
		.left_shift { return '<<' }
		.right_shift { return '>>' }
		.right_shift_unsigned { return '>>>' }
		else {}
	}

	return none
}

// prim_c_type_from supports prim c type from handling for types.
fn prim_c_type_from(props Properties, size u8) string {
	if props.has(.boolean) {
		return 'bool'
	}
	if props.has(.integer) {
		if props.has(.unsigned) {
			return match size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${size}' }
			}
		}
		return match size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${size}' }
		}
	}
	if props.has(.float) {
		return match size {
			32 { 'float' }
			64 { 'double' }
			else { 'double' }
		}
	}
	return 'int'
}

// prim_c_type supports prim c type handling for types.
fn prim_c_type(p Primitive) string {
	if p.props.has(.boolean) {
		return 'bool'
	}
	if p.props.has(.integer) {
		if p.props.has(.unsigned) {
			return match p.size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${p.size}' }
			}
		}
		return match p.size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${p.size}' }
		}
	}
	if p.props.has(.float) {
		return match p.size {
			32 { 'float' }
			64 { 'double' }
			else { 'double' }
		}
	}
	return 'int'
}

// find_matching_bracket resolves find matching bracket information for types.
fn find_matching_bracket(s string, start int) int {
	mut depth := 1
	for i := start + 1; i < s.len; i++ {
		if s[i] == `[` {
			depth++
		}
		if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

// split_params supports split params handling for types.
fn split_params(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			`,` {
				if depth == 0 {
					parts << s[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < s.len {
		parts << s[start..]
	}
	return parts
}

// normalize_fn_type_param_text transforms normalize fn type param text data for types.
fn normalize_fn_type_param_text(param string) string {
	mut text := param.trim_space()
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	space := top_level_space_index(text)
	if space > 0 {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if fn_type_param_head_is_name(head, tail) {
			text = tail
		}
	}
	if text.len > 0 {
		for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
			marker_idx := text.index(marker) or { continue }
			if marker_idx <= 0 {
				continue
			}
			head := text[..marker_idx].trim_space()
			tail := text[marker_idx..].trim_space()
			if fn_type_param_head_is_name(head, tail) {
				text = tail
			}
			break
		}
	}
	if is_mut && text.len > 0 && !text.starts_with('&') {
		return '&' + text
	}
	return text
}

// top_level_space_index supports top level space index handling for types.
fn top_level_space_index(s string) int {
	mut depth := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			` ` {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return -1
}

// fn_type_param_head_is_name supports fn type param head is name handling for types.
fn fn_type_param_head_is_name(head string, tail string) bool {
	if head.len == 0 || tail.len == 0 {
		return false
	}
	if head.starts_with('fn') || head.starts_with('&') || head.starts_with('[') {
		return false
	}
	if head in ['shared', 'atomic', 'chan', 'thread', 'map'] || head.contains('.') {
		return false
	}
	if is_builtin_type_name(head) {
		return false
	}
	return (head[0] >= `a` && head[0] <= `z`) || head[0] == `_`
}
