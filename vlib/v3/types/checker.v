module types

import os
import strconv
import time
import strings
import v3.flat
import v3.gen.c.naming
import v3.token
import v3.util

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

@[direct_array_access; inline]
fn path_leaf_view(path string) string {
	for i := path.len - 1; i >= 0; i-- {
		if path[i] == `/` || path[i] == `\\` {
			return unsafe { path.substr_unsafe(i + 1, path.len) }
		}
	}
	return path
}

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

const comptime_attribute_members = [
	'name',
	'arg',
	'has_arg',
	'kind',
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
	pos        token.Pos
	details    []string
	severity   string
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

// OwnershipCallResultSource records how a call argument can flow into a result projection.
pub struct OwnershipCallResultSource {
pub:
	arg_id        flat.NodeId
	source_suffix string
	target_suffix string
}

// LocalBinding represents local binding data used by types.
struct LocalBinding {
	name   string
	typ    Type
	is_mut bool
}

struct LocalDeclRhs {
	rhs    flat.NodeId
	file   int
	offset int
}

struct SharedAccessDiagnostic {
	name string
	pos  token.Pos
}

struct MatchSeenRange {
	start  int
	end    int
	branch int
}

struct ConstantScalar {
	kind       u8
	number     f64
	text       string
	bool_value bool
}

struct SqlOrmFieldTypeInfo {
	base_name   string
	is_array    bool
	is_multidim bool
}

struct FnDiagnosticParam {
	name       string
	typ        string
	is_mut     bool
	is_pointer bool
}

struct MissingReferenceField {
	path     string
	owner    string
	has_part bool
}

// ParseTypeCacheEntry keeps the identity components separate. The old
// cache built `${file}\n${module}\n${type}` for every lookup, allocating and
// hashing a temporary string before it could discover a cache hit.
struct ParseTypeCacheEntry {
	file           string
	module         string
	text           string
	generic_params []string
	resolution     bool
	typ            Type
}

struct SourceStructFieldDecl {
	name string
	typ  string
}

struct DeclarationVisibility {
	module_name string
	kind        flat.NodeKind
	is_pub      bool
}

@[heap]
struct VisibleMutationCache {
mut:
	decls            map[string]VisibleMutationFnDecl
	decl_misses      map[string]bool
	results          map[u64]bool
	rebind_results   map[u64]bool
	decl_index_ready bool
}

fn new_visible_mutation_cache() &VisibleMutationCache {
	return &VisibleMutationCache{
		decls: map[string]VisibleMutationFnDecl{}
		decl_misses: map[string]bool{}
		results: map[u64]bool{}
		rebind_results: map[u64]bool{}
	}
}

fn push_type_name_candidate(mut candidates []string, name string) {
	clean := trimmed_space(name)
	if clean.len > 0 && clean !in candidates {
		candidates << clean
	}
}

// TypeCache represents type cache data used by types.
struct TypeCache {
mut:
	// Frozen fallback cache shared by all parallel-check forks: lookups miss the
	// own (private) maps first, then consult base read-only; writes always go to
	// the own maps. The master installs its warm post-collect cache as base for
	// the whole region (using a private overlay itself) so workers do not start
	// cold and re-derive every memoized type/index.
	base                     &TypeCache = unsafe { nil }
	parse_enabled            bool
	parse_hits               i64
	parse_misses             i64
	c_hits                   i64
	c_misses                 i64
	parse_entries            map[u64]ParseTypeCacheEntry
	parse_context_file       string
	parse_context_module     string
	parse_context_generics   []string
	parse_context_resolution bool
	parse_context_hash       u64
	parse_context_valid      bool
	parse_last_entry         ParseTypeCacheEntry
	parse_last_valid         bool
	parse_key_recent_ptrs    [512]usize
	parse_key_recent_lens    [512]int
	parse_key_recent_context [512]u64
	parse_key_recent_values  [512]u64
	parse_key_recent_set     [512]bool
	// Canonical AST type texts repeat by pointer within one checker view. Cache
	// their parsed values directly so the common hit avoids the generic map
	// lookup after the existing context check.
	parse_value_recent_ptrs    [2048]usize
	parse_value_recent_lens    [2048]int
	parse_value_recent_context [2048]u64
	parse_value_recent_values  [2048]Type
	parse_value_recent_set     [2048]bool
	// Exact canonical text-id cache used by post-transform consumers. A lazy
	// 65536-slot table avoids collisions for every compact FlatAst text id.
	parse_text_id_context []u64
	parse_text_id_values  []Type
	parse_text_id_set     []bool
	// Alias targets can contain callbacks whose signatures mention the alias
	// itself (for example `type Handlers = map[string]fn (Handlers)`). Keep the
	// active expansion chain private to each checker/cache so parsing such a
	// recursive alias terminates without introducing shared parallel state.
	alias_parse_stack []string
	c_entries         map[TypeId]string
	c_name_entries    map[string]string
	// Lock-free recent slots for c_type keyed by the interned semantic TypeId.
	// A textual name would fold distinct semantic types that share a spelling
	// (`[size]int` with different resolved `size`, or same-named aliases over
	// different bases); raw interface words and retained Type payloads are not
	// stable either, since sum-type payload storage can reuse an address.
	c_recent_ids  [2048]TypeId
	c_recent_vals [2048]string
	c_recent_set  [2048]bool
	// type_name cannot use its result as its lookup key. Semantic hashes choose a
	// slot, and an owned Type copy verifies equality.
	name_recent_hashes [2048]u64
	name_recent_types  [2048]Type
	name_recent_vals   [2048]string
	name_recent_set    [2048]bool
	// Per-checker symbol probes front the compilation-wide interner. Resolved
	// names are usually repeated as the same canonical string pointer inside a
	// worker, so these slots avoid taking the interner mutex on every call.
	symbol_recent_ptrs          [2048]usize
	symbol_recent_lens          [2048]int
	symbol_recent_ids           [2048]SymbolId
	symbol_recent_vals          [2048]string
	symbol_recent_set           [2048]bool
	struct_field_entries        map[string]Type
	struct_field_misses         map[string]bool
	struct_field_last_struct    usize
	struct_field_last_field     usize
	struct_field_last_struct_n  int
	struct_field_last_field_n   int
	struct_field_last_value     Type = Type(void_)
	struct_field_last_state     i8
	struct_field_fn_diagnostics map[string]string
	sum_variant_pattern_entries map[string]string
	recv_pattern_entries        map[string]GenericReceiverMethodPatternMatch
	recv_pattern_misses         map[string]bool
	lexical_smartcast_entries   map[int]Type
	lexical_smartcast_misses    map[int]bool
	ierror_compat_entries       map[string]int
	interface_impl_entries      map[string][]string
	source_error_embed_entries  map[string]int
	source_error_embed_indexed  bool
	source_error_embed_shared   bool
	ierror_impl_names           []string
	ierror_impl_names_set       bool
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

fn (mut cache TypeCache) clear_c_type_entries() {
	cache.c_entries.clear()
	cache.c_name_entries.clear()
	for i in 0 .. cache.c_recent_set.len {
		cache.c_recent_set[i] = false
	}
}

// ResolutionTypeViewCache reuses the lookup-only, unscoped checker views used
// to parse compiler-generated qualified type text. The views are keyed by file
// because selective imports remain file-local.
struct ResolutionTypeViewCache {
mut:
	by_file map[string]&TypeChecker
}

// TypeCacheStats reports semantic cache effectiveness for compiler telemetry.
pub struct TypeCacheStats {
pub:
	parse_hits   i64
	parse_misses i64
	c_hits       i64
	c_misses     i64
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
	// Transform forks write overlay entries only for nodes appended after the
	// fork. IDs below this boundary are guaranteed to live in the shared dense
	// checker arrays, so reads can avoid probing two sparse maps per expression.
	base_node_count int
}

// PendingIerrorError is an invalid-ierror-return candidate recorded while the
// called-fns gate set was still being computed on the collector thread.
struct PendingIerrorError {
	err      TypeError
	fn_qname string
}

// FunctionCheckContext owns all semantic state whose lifetime is one function
// body. Parallel checker forks replace this value wholesale, so adding a new
// per-function cache cannot accidentally share its backing storage.
struct FunctionCheckContext {
mut:
	method_value_locals               map[string]bool
	method_value_local_owners         map[string][]ScopeBindingOwner
	method_value_local_depth          map[string]int
	method_value_stack_mut_owners     map[string]bool
	fn_value_variadic_locals          map[string]bool
	fn_value_variadic_local_owners    map[string][]ScopeBindingOwner
	fn_value_variadic_local_depth     map[string]int
	capturing_fn_literal_locals       map[string]bool
	capturing_fn_literal_local_owners map[string][]ScopeBindingOwner
	capturing_fn_literal_local_depth  map[string]int
	node_id                           int = -1
	// Cached at node_id assignment: should_diagnose consults this per node,
	// and deriving it re-parsed the function name on every call.
	concrete_generic_receiver_specialization bool
	mut_param_base_types                     map[string]Type
	mut_param_owners                         map[string]ScopeBindingOwner
	mut_local_owners                         map[string]ScopeBindingOwner
	closure_copy_owners                      map[string]ScopeBindingOwner
	shared_owners                            map[string][]ScopeBindingOwner
	shared_array_owners                      map[string][]ScopeBindingOwner
	locked_shared_names                      map[string]int
	locked_shared_modes                      map[string][]u8
	locked_shared_base_names                 map[string]string
	pointer_binding_value_keys               map[string][]string
	immutable_reference_aliases              map[string]bool
	unsafe_reference_alias_owners            map[string]bool
	unsafe_alias_break_states                [][]map[string]bool
	pointer_alias_break_states               [][]map[string][]string
	pointer_alias_continue_states            [][]map[string][]string
	pointer_alias_goto_states                map[string][]map[string][]string
	pointer_alias_backward_goto_targets      map[string]bool
	closure_forbidden_captures               map[string]bool
	local_decl_rhs_by_name                   map[string][]LocalDeclRhs
	local_decl_rhs_indexed                   bool
	bool_condition_exprs                     map[string]flat.NodeId
	has_goto_nodes                           bool
	closure_scope                            &Scope = unsafe { nil }
	lambda_no_captures                       bool
	generic_params                           []string
	return_type                              Type = Type(void_)
	undefined_variable_context_depth         int
	continue_after_unknown_ident             bool
}

fn new_function_check_context() FunctionCheckContext {
	// V maps initialize on first insertion. Most functions use only a small
	// subset of these ownership/capture maps, so eagerly constructing every map
	// multiplies allocator work by the number of checked functions.
	return FunctionCheckContext{}
}

fn clone_function_check_context(src FunctionCheckContext) FunctionCheckContext {
	return FunctionCheckContext{
		method_value_locals: src.method_value_locals.clone()
		method_value_local_owners: clone_scope_binding_owner_map(src.method_value_local_owners)
		method_value_local_depth: src.method_value_local_depth.clone()
		method_value_stack_mut_owners: src.method_value_stack_mut_owners.clone()
		fn_value_variadic_locals: src.fn_value_variadic_locals.clone()
		fn_value_variadic_local_owners: src.fn_value_variadic_local_owners.clone()
		fn_value_variadic_local_depth: src.fn_value_variadic_local_depth.clone()
		capturing_fn_literal_locals: src.capturing_fn_literal_locals.clone()
		capturing_fn_literal_local_owners: src.capturing_fn_literal_local_owners.clone()
		capturing_fn_literal_local_depth: src.capturing_fn_literal_local_depth.clone()
		node_id: src.node_id
		concrete_generic_receiver_specialization: src.concrete_generic_receiver_specialization
		mut_param_base_types: src.mut_param_base_types.clone()
		mut_param_owners: src.mut_param_owners.clone()
		mut_local_owners: src.mut_local_owners.clone()
		closure_copy_owners: src.closure_copy_owners.clone()
		shared_owners: src.shared_owners.clone()
		shared_array_owners: src.shared_array_owners.clone()
		locked_shared_names: src.locked_shared_names.clone()
		locked_shared_modes: src.locked_shared_modes.clone()
		locked_shared_base_names: src.locked_shared_base_names.clone()
		pointer_binding_value_keys: clone_pointer_binding_value_keys(src.pointer_binding_value_keys)
		immutable_reference_aliases: src.immutable_reference_aliases.clone()
		unsafe_reference_alias_owners: src.unsafe_reference_alias_owners.clone()
		unsafe_alias_break_states: clone_unsafe_alias_break_states(src.unsafe_alias_break_states)
		pointer_alias_break_states: clone_pointer_alias_loop_states(src.pointer_alias_break_states)
		pointer_alias_continue_states: clone_pointer_alias_loop_states(src.pointer_alias_continue_states)
		pointer_alias_goto_states: clone_pointer_alias_goto_states(src.pointer_alias_goto_states)
		pointer_alias_backward_goto_targets: src.pointer_alias_backward_goto_targets.clone()
		closure_forbidden_captures: src.closure_forbidden_captures.clone()
		local_decl_rhs_by_name: src.local_decl_rhs_by_name.clone()
		local_decl_rhs_indexed: src.local_decl_rhs_indexed
		bool_condition_exprs: src.bool_condition_exprs.clone()
		has_goto_nodes: src.has_goto_nodes
		closure_scope: src.closure_scope
		lambda_no_captures: src.lambda_no_captures
		generic_params: src.generic_params.clone()
		return_type: src.return_type
		undefined_variable_context_depth: src.undefined_variable_context_depth
		continue_after_unknown_ident: src.continue_after_unknown_ident
	}
}

fn clone_unsafe_alias_break_states(states [][]map[string]bool) [][]map[string]bool {
	mut result := [][]map[string]bool{cap: states.len}
	for loop_states in states {
		mut cloned_loop_states := []map[string]bool{cap: loop_states.len}
		for state in loop_states {
			cloned_loop_states << state.clone()
		}
		result << cloned_loop_states
	}
	return result
}

fn clone_pointer_alias_loop_states(states [][]map[string][]string) [][]map[string][]string {
	mut result := [][]map[string][]string{cap: states.len}
	for loop_states in states {
		mut cloned_loop_states := []map[string][]string{cap: loop_states.len}
		for state in loop_states {
			cloned_loop_states << clone_pointer_binding_value_keys(state)
		}
		result << cloned_loop_states
	}
	return result
}

fn clone_pointer_alias_goto_states(states map[string][]map[string][]string) map[string][]map[string][]string {
	mut result := map[string][]map[string][]string{}
	for label, label_states in states {
		mut cloned_label_states := []map[string][]string{cap: label_states.len}
		for state in label_states {
			cloned_label_states << clone_pointer_binding_value_keys(state)
		}
		result[label] = cloned_label_states
	}
	return result
}

fn clone_string_list_map(source map[string][]string) map[string][]string {
	mut result := map[string][]string{}
	result.reserve(u32(source.len))
	for key, values in source {
		result[key] = values.clone()
	}
	return result
}

pub struct InterfaceImplIndex {
pub:
	names []string
	ids   map[string]int
}

struct DeprecationInfo {
	name string
mut:
	message string
	after   string
}

// TypeChecker represents type checker data used by types.
@[heap]
pub struct TypeChecker {
pub mut:
	a                                &flat.FlatAst = unsafe { nil }
	compiler_vroot                   string
	verbose                          bool
	raw_type_equality                bool
	fast_parse_recent                bool
	fast_type_text_refs              bool
	fast_c_type_recent               bool
	memo_call_info                   bool
	method_suffix_prescreen          bool
	prefix_param_scan                bool
	building_v_fast                  bool
	valid_diagnostic_fast            bool
	valid_resolution_fast            bool
	defer_fn_ancillary               bool
	fn_ancillary_registrations       []FnAncillaryRegistration
	fn_c_variadic_registrations      []FnNamePairRegistration
	fn_mut_receiver_registrations    []FnNamePairRegistration
	fn_ret_text_registrations        []FnTextRegistration
	visible_mutation_registrations   []VisibleMutationRegistration
	enable_globals                   bool
	fn_ret_types                     map[string]Type
	fn_param_types                   map[string][]Type
	v_fn_semantic_names              map[string]bool
	c_fn_module_ret_types            map[string]Type
	c_fn_module_param_types          map[string][]Type
	c_fn_module_variadic             map[string]bool
	c_fn_abi_variadic_prefixes       map[string]int
	fn_shared_params                 map[string][]bool
	mut_receiver_methods             map[string]bool
	source_no_body_fns               map[string]bool
	source_no_body_fn_suffixes       map[string]bool
	unsafe_fns                       map[string]bool
	unsafe_c_fns                     map[string]bool
	fn_ret_type_texts                map[string]string // generic struct method key -> original return type text (e.g. `Box[T].clone` -> `Box[T]`)
	fn_param_type_texts              map[string][]string // generic struct method key -> original param type texts (receiver first)
	fn_type_files                    map[string]string
	fn_type_modules                  map[string]string
	transform_signature_maps_shared  bool
	transform_signature_maps_changed bool
	transform_signature_names_log    []string
	transform_struct_maps_shared     bool
	fn_generic_params                map[string][]string
	specialized_generic_fns          map[string]bool
	fn_variadic                      map[string]bool
	c_variadic_fns                   map[string]bool
	fn_implicit_veb_ctx              map[string]bool
	receiver_method_suffix_index     map[string]string
	generic_receiver_method_index    map[string][]string
	structs                          map[string][]StructField
	struct_modules                   map[string]string
	struct_files                     map[string]string
	soa_structs                      map[string]bool
	// set of `${file}\x01${module}\x01${name}` keys for every source-level
	// struct/type/interface/enum declaration, built once in `collect`. Replaces
	// the former full-node scan in `source_declares_type_in_scope`, which was
	// O(nodes) per call and dominated check/transform/cgen (called via qualify_name).
	declared_type_scope_keys           map[string]bool
	concrete_type_scope_keys           map[string]bool
	struct_error_embeds_shadow_builtin map[string]bool
	struct_generic_params              map[string][]string // generic struct base name -> type-param names (e.g. Vec4 -> [T])
	struct_implements                  map[string][]string
	struct_shared_fields               map[string]bool
	struct_shared_element_fields       map[string]bool
	struct_field_c_abi_fns             map[string]string
	// concrete `Box[int].method` -> substituted CallInfo for a method *value* on a
	// generic receiver. The open `Box[T].method` registration is gone by cgen time, so
	// the checker stashes the resolved signature here for gen_method_value_closure.
	generic_method_value_info             map[string]CallInfo
	params_structs                        map[string]bool
	c_typedef_structs                     map[string]bool
	unions                                map[string]bool
	type_aliases                          map[string]string
	type_alias_modules                    map[string]string
	type_alias_generic_params             map[string][]string // generic alias base name -> type-param names
	type_alias_c_abi_fns                  map[string]string
	recursive_alias_names                 map[string]bool
	sum_types                             map[string][]string
	sum_generic_params                    map[string][]string // generic sum type base name -> type-param names (e.g. Tree -> [T])
	enum_names                            map[string]bool
	enum_fields                           map[string][]string
	flag_enums                            map[string]bool
	interface_names                       map[string]bool
	interface_generic_params              map[string][]string
	interface_fields                      map[string][]StructField
	interface_embeds                      map[string][]string
	interface_abstract_methods            map[string][]string // iface -> abstract (declared) method names
	interface_impl_name_snapshots         map[string][]string
	interface_impl_candidates_at_snapshot map[string]bool
	interface_impl_candidates_at_index    map[string]bool
	interface_method_names_index          map[string][]string
	interface_abstract_index              map[string][]string
	interface_field_list_index            map[string][]StructField
	interface_impl_indexes                map[string]&InterfaceImplIndex
	interface_query_indexes_ready         bool

	c_globals               map[string]Type
	global_names            map[string]bool
	shared_global_names     map[string]bool
	const_types             map[string]Type
	const_exprs             map[string]flat.NodeId
	const_modules           map[string]string
	const_files             map[string]string
	const_suffixes          map[string]string // dot-suffix -> full const key (O(1) lookup; '' if ambiguous)
	declaration_visibility  map[string]DeclarationVisibility
	checked_const_names     map[string]bool
	imports                 map[string]string // alias -> short module name
	file_imports            map[string]string
	file_selective_imports  map[string][]string
	file_imports_by_file    map[string]&FileImportInfo
	file_modules            map[string]string
	translated_files        map[string]bool
	has_globals_files       map[string]bool
	deprecated_symbols      map[string]DeprecationInfo
	file_scope              &Scope = unsafe { nil }
	cur_scope               &Scope = unsafe { nil }
	scope_pool              []&Scope
	scope_pool_index        int
	has_builtins            bool
	cur_module              string
	cur_file                string
	unsafe_depth            int
	lock_depth              int
	comptime_static_depth   int
	errors                  []TypeError
	notices                 []TypeError
	resolved_call_names     []string // node_id -> resolved function name
	resolved_call_set       []bool
	resolved_fn_value_names []string // node_id -> resolved function value name
	resolved_fn_value_set   []bool
	statement_nodes         []bool
	// Exact call/function-value dependencies recorded while each function is
	// checked. Consumers such as markused can walk these resolved Symbol-like
	// names instead of reconstructing import and receiver resolution from syntax.
	direct_dependencies_by_fn map[int][]SymbolId // enclosing fn node id -> resolved function identities
	// Methods used as *values* (`recv.method` passed as a callback), recorded per enclosing
	// function during semantic checking — which has full scope/type info, runs before
	// markused, and (unlike a call) routes a value-context selector through check_selector.
	// markused seeds these (keeping the wrapper-only method out of the dead-code pruner)
	// only when their enclosing function is reachable.
	method_values_by_fn map[int][]string // enclosing fn node id -> method-value `Type.method` keys
	// Local variables bound to a method value (`cb := c.report`) in the current function.
	// Escape checks use these aliases to retain the lifetime hazard of mutable methods
	// borrowing addressable stack receivers. Reset per function.
	method_value_locals map[string]bool
	// Scope depth at which each method-value local was marked, so a reassignment to a
	// non-method value only clears the marker when it dominates later uses (same-or-shallower
	// scope); a reassignment in a deeper conditional/loop scope keeps the maybe-method marker.
	method_value_local_depth                map[string]int
	capturing_fn_literal_locals             map[string]bool
	capturing_fn_literal_local_depth        map[string]int
	capturing_fn_literal_return_unsupported map[string]bool
	cur_fn_node_id                          int = -1
	cur_fn_mut_param_base_types             map[string]Type
	cur_fn_mut_param_binding_owners         map[string]ScopeBindingOwner
	cur_fn_mut_local_binding_owners         map[string]ScopeBindingOwner
	cur_fn_shared_binding_owners            map[string]ScopeBindingOwner
	cur_comptime_variant_loop_vars          []string
	expr_type_values                        []Type // node_id -> complex/contextual resolved type
	expr_type_set                           []bool
	lexical_smartcast_misses                []bool
	checking_nodes                          []bool
	parallel_check_sparse                   bool
	scope_parallel_check_workers            bool
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
	checker_fixture_mode          bool
	autofree_mode                 bool
	no_main                       bool
	warns_are_errors              bool
	notes_are_errors              bool
	is_prod                       bool
	suppress_dump_output          bool
	diagnostic_files              map[string]bool
	multiple_module_import_lines  map[u64]bool
	source_texts_by_file          map[string]string
	ct_update_pos                 map[int]token.Pos
	ct_update_indexed             bool
	insert_include_dirs_by_file   map[string][]string
	has_spawn_expr                int = -1
	inactive_top_level_node_ids   []int
	selected_file_called_fns      map[string]bool
	// Names newly inserted into selected_file_called_fns and not yet chased by
	// the transitive closure in collect_selected_file_called_fns_transitively.
	selected_file_worklist []string
	// During a scoped check, sites that would gate on the called-fns closure park
	// their candidate error here. Successful builds skip the closure entirely;
	// the master computes it after checking only when a candidate needs filtering.
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
	// Anonymous and function-local struct declarations are synthesized below
	// the file's top-level declaration tree. The direct-parent pass records their
	// sorted node ids so collect_top_level_idx_fast can merge them without
	// rescanning every gap between parser-recorded declarations.
	synthetic_top_level_type_ids  []int
	expected_expr_id              int = -1
	expected_expr_type            Type = Type(void_)
	cur_fn_ret_type               Type = Type(void_)
	channel_send_or_expr_id       int = -1
	smartcasts                    map[string]Type
	ownership                     &OwnershipState = unsafe { nil }
	ownership_return_item_by_name map[string]int
	ownership_return_edges        []u64
	ownership_return_current_item int = -1
	ownership_return_record_calls bool
	ownership_return_item_changed bool
	ownership_param_item_by_name  map[string]int
	ownership_param_changed_items []bool
	ownership_param_current_item  int = -1
	ownership_param_track_changes bool
	// See QualifyNameCache: nil unless armed for a phase whose allocations
	// outlive every prealloc scope arena; forks must replace it with their own
	// instance. A long-lived armed cache written during a scoped driver stage
	// (markused/transform/cgen under -prealloc) stores map buckets and result
	// strings in the disposable scope arena, and later reads crash.
	qualify_name_cache &QualifyNameCache = unsafe { nil }
	// Per-fork resolve_type memo for the current check work item's node range;
	// nil until check_fn_items_serial arms it (see BodyResolveMemo).
	body_resolve_memo &BodyResolveMemo = unsafe { nil }
	import_info_cache &ImportInfoCache = unsafe { nil }
	// Nanoseconds spent in the ownership checker's per-fn boundary passes.
	// Only `-d ownership` builds ever write it (every writer lives in
	// checker_ownership_d_ownership.v), so plain builds report exactly 0.
	ownership_time_ns i64
	selfhost          bool
	// resolution_type_mode is enabled only after semantic checking, while transform
	// and codegen read synthesized generic-specialization type text. Source annotations
	// must keep normal module scoping and never enable this fallback.
	resolution_type_mode bool
	// trust_checked_expr_types serves resolve_type straight from the checker's
	// dense per-node type cache. Armed by the driver only after checking
	// completes; transform's node-write helpers invalidate rewritten ids, and
	// nodes appended after checking fall outside the cache and resolve normally.
	trust_checked_expr_types bool
	// fork_overlay is non-nil only on parallel-transform worker forks; see
	// TransformForkOverlay and fork_for_parallel_transform.
	fork_overlay &TransformForkOverlay = unsafe { nil }
mut:
	// Includes method-value aliases and binding-owner maps; all backing maps are
	// replaced together at every function/worker boundary.
	fn_context               FunctionCheckContext
	type_cache               &TypeCache = unsafe { nil }
	pre_transform_type_cache &TypeCache = unsafe { nil }
	resolution_type_views    &ResolutionTypeViewCache = unsafe { nil }
	visible_mutation_cache   &VisibleMutationCache = unsafe { nil }
	type_interner            &TypeInterner = unsafe { nil }
	symbols                  &SymbolInterner = unsafe { nil }
	// direct_parent_ids maps a parsed node to the first AST node that references
	// it as a child. It is immutable during semantic checking and shared by
	// checker workers. Transformed or appended nodes use the scan fallback in
	// direct_parent_id.
	direct_parent_ids           []flat.NodeId
	rewritten_parent_ids        []flat.NodeId
	value_used_nodes            []bool
	fn_check_costs              []int
	direct_parent_index_trusted bool
	has_goto_nodes              bool
	// Immutable declaration indexes shared by checker workers.
	declaration_attributes       map[int][]string
	type_declaration_ids         map[string][]int
	strings_builder_bindings     map[string]bool
	strings_builder_candidates   []int
	static_associated_fn_keys    map[string]bool
	declaration_param_mutability map[string][]bool
	strict_map_index_files       map[string]bool
	// short fn name -> first declaring top-level node index, in declaration
	// order (mirrors the expr_raw_fn_type_text scan's first-match rule).
	fn_decl_short_name_ids map[string]int
	// '${file}\x00${alias}' -> dotted import path, and '${file}\x00${last
	// segment}' -> dotted import path (first import wins), replacing per-call
	// scans over every top-level declaration.
	file_import_alias_paths  map[string]string
	file_import_suffix_paths map[string]string
	// struct name -> embedded receiver type names (empty entry when the struct
	// has no embeds). Structs added after collect (monomorphization) miss this
	// index and fall back to the field walk.
	struct_embed_receivers map[string][]string
	// Immutable node -> generic parameter index shared by checker workers.
	enclosing_generic_params_by_node map[int][]string
	enclosing_generic_param_masks    []u32
}

fn (tc &TypeChecker) timing_profile(message string) {
	if tc.verbose {
		eprintln(message)
	}
}

// enable_scoped_parallel_workers uses disposable prealloc arenas for parallel
// checker helpers. Ownership checking keeps its existing long-lived workers.
pub fn (mut tc TypeChecker) enable_scoped_parallel_workers() {
	$if !ownership ? {
		tc.scope_parallel_check_workers = true
	}
}

// scoped_parallel_workers_enabled reports whether compiler stages should use
// short-lived worker arenas with this checker.
pub fn (tc &TypeChecker) scoped_parallel_workers_enabled() bool {
	return tc.scope_parallel_check_workers
}

// new creates a TypeChecker value for types.
pub fn TypeChecker.new(a &flat.FlatAst) TypeChecker {
	fs := new_scope(unsafe { nil })
	type_interner := new_type_interner()
	symbols := new_symbol_interner()
	return TypeChecker{
		a: a
		raw_type_equality: os.getenv('V3_NO_RAW_TYPE_EQUALITY') == ''
		fast_parse_recent: os.getenv('V3_NO_FAST_PARSE_RECENT') == ''
		fast_type_text_refs: os.getenv('V3_NO_TYPE_TEXT_REFS') == ''
		fast_c_type_recent: os.getenv('V3_NO_FAST_C_TYPE_RECENT') == ''
		memo_call_info: os.getenv('V3_NO_CALL_INFO_MEMO') == ''
		method_suffix_prescreen: os.getenv('V3_NO_METHOD_SUFFIX_PRESCREEN') == ''
		prefix_param_scan: os.getenv('V3_NO_PREFIX_PARAM_SCAN') == ''
		fn_ret_types: map[string]Type{}
		fn_param_types: map[string][]Type{}
		c_fn_module_ret_types: map[string]Type{}
		c_fn_module_param_types: map[string][]Type{}
		c_fn_module_variadic: map[string]bool{}
		c_fn_abi_variadic_prefixes: map[string]int{}
		fn_shared_params: map[string][]bool{}
		mut_receiver_methods: map[string]bool{}
		source_no_body_fns: map[string]bool{}
		source_no_body_fn_suffixes: map[string]bool{}
		unsafe_fns: map[string]bool{}
		unsafe_c_fns: map[string]bool{}
		fn_ret_type_texts: map[string]string{}
		fn_param_type_texts: map[string][]string{}
		fn_type_files: map[string]string{}
		fn_type_modules: map[string]string{}
		fn_generic_params: map[string][]string{}
		specialized_generic_fns: map[string]bool{}
		fn_variadic: map[string]bool{}
		c_variadic_fns: map[string]bool{}
		fn_implicit_veb_ctx: map[string]bool{}
		receiver_method_suffix_index: map[string]string{}
		generic_receiver_method_index: map[string][]string{}
		structs: map[string][]StructField{}
		struct_modules: map[string]string{}
		struct_files: map[string]string{}
		soa_structs: map[string]bool{}
		declared_type_scope_keys: map[string]bool{}
		concrete_type_scope_keys: map[string]bool{}
		struct_error_embeds_shadow_builtin: map[string]bool{}
		struct_generic_params: map[string][]string{}
		struct_implements: map[string][]string{}
		struct_shared_fields: map[string]bool{}
		struct_shared_element_fields: map[string]bool{}
		struct_field_c_abi_fns: map[string]string{}
		generic_method_value_info: map[string]CallInfo{}
		params_structs: map[string]bool{}
		c_typedef_structs: map[string]bool{}
		unions: map[string]bool{}
		type_aliases: map[string]string{}
		type_alias_modules: map[string]string{}
		type_alias_generic_params: map[string][]string{}
		type_alias_c_abi_fns: map[string]string{}
		sum_types: map[string][]string{}
		sum_generic_params: map[string][]string{}
		enum_names: map[string]bool{}
		enum_fields: map[string][]string{}
		flag_enums: map[string]bool{}
		interface_names: map[string]bool{}
		interface_generic_params: map[string][]string{}
		interface_fields: map[string][]StructField{}
		interface_embeds: map[string][]string{}
		interface_abstract_methods: map[string][]string{}
		interface_impl_name_snapshots: map[string][]string{}
		interface_impl_candidates_at_snapshot: map[string]bool{}
		interface_impl_candidates_at_index: map[string]bool{}
		interface_method_names_index: map[string][]string{}
		interface_abstract_index: map[string][]string{}
		interface_field_list_index: map[string][]StructField{}
		interface_impl_indexes: map[string]&InterfaceImplIndex{}
		c_globals: map[string]Type{}
		global_names: map[string]bool{}
		shared_global_names: map[string]bool{}
		const_types: map[string]Type{}
		const_exprs: map[string]flat.NodeId{}
		const_modules: map[string]string{}
		const_files: map[string]string{}
		const_suffixes: map[string]string{}
		declaration_visibility: map[string]DeclarationVisibility{}
		imports: map[string]string{}
		file_imports: map[string]string{}
		file_selective_imports: map[string][]string{}
		file_imports_by_file: map[string]&FileImportInfo{}
		file_modules: map[string]string{}
		translated_files: map[string]bool{}
		has_globals_files: map[string]bool{}
		deprecated_symbols: map[string]DeprecationInfo{}
		file_scope: fs
		cur_scope: fs
		// The node-indexed cache arrays start empty: collect() sizes them via
		// reset_node_caches (allocating them here too paid for everything
		// twice), and extend_node_caches grows them on demand for any checker
		// used without a collect() call.
		resolved_call_names: []string{}
		resolved_call_set: []bool{}
		resolved_fn_value_names: []string{}
		resolved_fn_value_set: []bool{}
		statement_nodes: []bool{}
		method_values_by_fn: map[int][]string{}
		method_value_locals: map[string]bool{}
		method_value_local_depth: map[string]int{}
		capturing_fn_literal_locals: map[string]bool{}
		capturing_fn_literal_local_depth: map[string]int{}
		capturing_fn_literal_return_unsupported: map[string]bool{}
		cur_fn_mut_param_base_types: map[string]Type{}
		cur_fn_mut_param_binding_owners: map[string]ScopeBindingOwner{}
		cur_fn_mut_local_binding_owners: map[string]ScopeBindingOwner{}
		cur_fn_shared_binding_owners: map[string]ScopeBindingOwner{}
		expr_type_values: []Type{}
		expr_type_set: []bool{}
		lexical_smartcast_misses: []bool{}
		checking_nodes: []bool{}
		sparse_resolved_call_names: map[int]string{}
		sparse_resolved_fn_values: map[int]string{}
		sparse_statement_nodes: map[int]bool{}
		sparse_expr_type_values: map[int]Type{}
		sparse_checking_nodes: map[int]bool{}
		diagnostic_files: map[string]bool{}
		multiple_module_import_lines: map[u64]bool{}
		source_texts_by_file: map[string]string{}
		selected_file_called_fns: map[string]bool{}
		smartcasts: map[string]Type{}
		type_cache: &TypeCache{
			parse_entries: map[u64]ParseTypeCacheEntry{}
			c_entries: map[TypeId]string{}
			struct_field_entries: map[string]Type{}
			struct_field_misses: map[string]bool{}
			ierror_compat_entries: map[string]int{}
			source_error_embed_entries: map[string]int{}
		}
		resolution_type_views: &ResolutionTypeViewCache{
			by_file: map[string]&TypeChecker{}
		}
		visible_mutation_cache: new_visible_mutation_cache()
		type_interner: type_interner
		symbols: symbols
		enclosing_generic_params_by_node: map[int][]string{}
		declaration_attributes: map[int][]string{}
		type_declaration_ids: map[string][]int{}
		strings_builder_bindings: map[string]bool{}
		static_associated_fn_keys: map[string]bool{}
		declaration_param_mutability: map[string][]bool{}
		strict_map_index_files: map[string]bool{}
		fn_decl_short_name_ids: map[string]int{}
		file_import_alias_paths: map[string]string{}
		file_import_suffix_paths: map[string]string{}
		struct_embed_receivers: map[string][]string{}
	}
}

// fork_program_view builds a checker view over immutable, compilation-wide
// semantic data. Mutable scope, diagnostics, sparse caches, and function state
// start private. Dependency-map ownership is explicit: parallel workers pass
// a fresh map, while a synchronous subview may keep recording in its owning
// checker's private map. Keeping this constructor explicit prevents a
// newly-added mutable field from being silently shared by parallel workers.
fn (tc &TypeChecker) fork_program_view(ast &flat.FlatAst, direct_dependencies_by_fn map[int][]SymbolId) TypeChecker {
	fs := new_scope(tc.file_scope)
	return TypeChecker{
		a: ast
		compiler_vroot: tc.compiler_vroot
		raw_type_equality: tc.raw_type_equality
		fast_parse_recent: tc.fast_parse_recent
		fast_type_text_refs: tc.fast_type_text_refs
		fast_c_type_recent: tc.fast_c_type_recent
		memo_call_info: tc.memo_call_info
		method_suffix_prescreen: tc.method_suffix_prescreen
		prefix_param_scan: tc.prefix_param_scan
		building_v_fast: tc.building_v_fast
		valid_diagnostic_fast: tc.valid_diagnostic_fast
		valid_resolution_fast: tc.valid_resolution_fast
		enable_globals: tc.enable_globals
		fn_ret_types: tc.fn_ret_types
		fn_param_types: tc.fn_param_types
		c_fn_module_ret_types: tc.c_fn_module_ret_types
		c_fn_module_param_types: tc.c_fn_module_param_types
		c_fn_module_variadic: tc.c_fn_module_variadic
		c_fn_abi_variadic_prefixes: tc.c_fn_abi_variadic_prefixes
		fn_shared_params: tc.fn_shared_params
		mut_receiver_methods: tc.mut_receiver_methods
		source_no_body_fns: tc.source_no_body_fns
		source_no_body_fn_suffixes: tc.source_no_body_fn_suffixes
		unsafe_fns: tc.unsafe_fns
		unsafe_c_fns: tc.unsafe_c_fns
		fn_ret_type_texts: tc.fn_ret_type_texts
		fn_param_type_texts: tc.fn_param_type_texts
		fn_type_files: tc.fn_type_files
		fn_type_modules: tc.fn_type_modules
		fn_generic_params: tc.fn_generic_params
		transform_signature_names_log: []string{}
		specialized_generic_fns: tc.specialized_generic_fns
		fn_variadic: tc.fn_variadic
		c_variadic_fns: tc.c_variadic_fns
		fn_implicit_veb_ctx: tc.fn_implicit_veb_ctx
		receiver_method_suffix_index: tc.receiver_method_suffix_index
		generic_receiver_method_index: tc.generic_receiver_method_index
		structs: tc.structs
		struct_modules: tc.struct_modules
		struct_files: tc.struct_files
		declared_type_scope_keys: tc.declared_type_scope_keys
		concrete_type_scope_keys: tc.concrete_type_scope_keys
		struct_error_embeds_shadow_builtin: tc.struct_error_embeds_shadow_builtin
		struct_generic_params: tc.struct_generic_params
		struct_implements: tc.struct_implements
		struct_shared_fields: tc.struct_shared_fields
		struct_shared_element_fields: tc.struct_shared_element_fields
		struct_field_c_abi_fns: tc.struct_field_c_abi_fns
		generic_method_value_info: tc.generic_method_value_info
		params_structs: tc.params_structs
		c_typedef_structs: tc.c_typedef_structs
		unions: tc.unions
		type_aliases: tc.type_aliases
		type_alias_modules: tc.type_alias_modules
		type_alias_generic_params: tc.type_alias_generic_params
		type_alias_c_abi_fns: tc.type_alias_c_abi_fns
		sum_types: tc.sum_types
		sum_generic_params: tc.sum_generic_params
		enum_names: tc.enum_names
		enum_fields: tc.enum_fields
		flag_enums: tc.flag_enums
		interface_names: tc.interface_names
		interface_generic_params: tc.interface_generic_params
		interface_fields: tc.interface_fields
		interface_embeds: tc.interface_embeds
		interface_abstract_methods: tc.interface_abstract_methods
		interface_impl_candidates_at_index: tc.interface_impl_candidates_at_index
		interface_method_names_index: tc.interface_method_names_index
		interface_abstract_index: tc.interface_abstract_index
		interface_field_list_index: tc.interface_field_list_index
		interface_impl_indexes: tc.interface_impl_indexes
		interface_query_indexes_ready: tc.interface_query_indexes_ready
		c_globals: tc.c_globals
		global_names: tc.global_names
		shared_global_names: tc.shared_global_names
		const_types: tc.const_types
		const_exprs: tc.const_exprs
		const_modules: tc.const_modules
		const_files: tc.const_files
		const_suffixes: tc.const_suffixes
		declaration_visibility: tc.declaration_visibility
		imports: tc.imports
		file_imports: tc.file_imports
		file_selective_imports: tc.file_selective_imports
		file_imports_by_file: tc.file_imports_by_file
		file_modules: tc.file_modules
		translated_files: tc.translated_files
		has_globals_files: tc.has_globals_files
		deprecated_symbols: tc.deprecated_symbols
		file_scope: fs
		cur_scope: fs
		scope_pool: []&Scope{}
		has_builtins: tc.has_builtins
		cur_module: tc.cur_module
		cur_file: tc.cur_file
		resolution_type_mode: tc.resolution_type_mode
		trust_checked_expr_types: tc.trust_checked_expr_types
		errors: []TypeError{}
		notices: []TypeError{}
		resolved_call_names: tc.resolved_call_names
		resolved_call_set: tc.resolved_call_set
		resolved_fn_value_names: tc.resolved_fn_value_names
		resolved_fn_value_set: tc.resolved_fn_value_set
		statement_nodes: tc.statement_nodes
		direct_dependencies_by_fn: direct_dependencies_by_fn
		method_values_by_fn: tc.method_values_by_fn
		cur_comptime_variant_loop_vars: tc.cur_comptime_variant_loop_vars
		expr_type_values: tc.expr_type_values
		expr_type_set: tc.expr_type_set
		lexical_smartcast_misses: tc.lexical_smartcast_misses
		checking_nodes: tc.checking_nodes
		sparse_resolved_call_names: map[int]string{}
		sparse_resolved_fn_values: map[int]string{}
		sparse_statement_nodes: map[int]bool{}
		sparse_expr_type_values: map[int]Type{}
		sparse_checking_nodes: map[int]bool{}
		diagnose_unknown_calls: tc.diagnose_unknown_calls
		reject_unlowered_map_mutation: tc.reject_unlowered_map_mutation
		reject_unsupported_generics: tc.reject_unsupported_generics
		checker_fixture_mode: tc.checker_fixture_mode
		autofree_mode: tc.autofree_mode
		no_main: tc.no_main
		warns_are_errors: tc.warns_are_errors
		notes_are_errors: tc.notes_are_errors
		is_prod: tc.is_prod
		suppress_dump_output: tc.suppress_dump_output
		diagnostic_files: tc.diagnostic_files
		multiple_module_import_lines: tc.multiple_module_import_lines
		source_texts_by_file: tc.source_texts_by_file
		ct_update_pos: tc.ct_update_pos
		ct_update_indexed: tc.ct_update_indexed
		has_spawn_expr: tc.has_spawn_expr
		inactive_top_level_node_ids: tc.inactive_top_level_node_ids
		selected_file_called_fns: tc.selected_file_called_fns
		selected_file_worklist: tc.selected_file_worklist
		defer_ierror_gating: tc.defer_ierror_gating
		pending_ierror_errors: []PendingIerrorError{}
		top_level_idx: tc.top_level_idx
		top_level_idx_nodes_len: tc.top_level_idx_nodes_len
		synthetic_top_level_type_ids: tc.synthetic_top_level_type_ids
		direct_parent_ids: tc.direct_parent_ids
		rewritten_parent_ids: tc.rewritten_parent_ids
		value_used_nodes: tc.value_used_nodes
		direct_parent_index_trusted: tc.direct_parent_index_trusted
		has_goto_nodes: tc.has_goto_nodes
		declaration_attributes: tc.declaration_attributes
		type_declaration_ids: tc.type_declaration_ids
		strings_builder_bindings: tc.strings_builder_bindings
		static_associated_fn_keys: tc.static_associated_fn_keys
		declaration_param_mutability: tc.declaration_param_mutability
		strict_map_index_files: tc.strict_map_index_files
		fn_decl_short_name_ids: tc.fn_decl_short_name_ids
		file_import_alias_paths: tc.file_import_alias_paths
		file_import_suffix_paths: tc.file_import_suffix_paths
		struct_embed_receivers: tc.struct_embed_receivers
		enclosing_generic_params_by_node: tc.enclosing_generic_params_by_node
		enclosing_generic_param_masks: tc.enclosing_generic_param_masks
		expected_expr_id: -1
		expected_expr_type: Type(void_)
		smartcasts: tc.smartcasts
		ownership: tc.ownership
		selfhost: tc.selfhost
		fn_context: new_function_check_context()
		resolution_type_views: &ResolutionTypeViewCache{
			by_file: map[string]&TypeChecker{}
		}
		visible_mutation_cache: tc.visible_mutation_cache
		type_interner: tc.type_interner
		symbols: tc.symbols
	}
}

// fork_for_parallel_codegen returns a complete read-only semantic view with
// private scope and memoization state for one C-generation worker.
pub fn (tc &TypeChecker) fork_for_parallel_codegen() &TypeChecker {
	mut forked := tc.fork_program_view(tc.a, map[int][]SymbolId{})
	// Cgen resolves locals in child scopes but must keep the checked file scope
	// as the immutable root shared by all workers.
	forked.file_scope = tc.file_scope
	forked.cur_scope = new_scope(tc.file_scope)
	forked.errors = tc.errors.clone()
	forked.parallel_check_sparse = tc.parallel_check_sparse
	forked.check_range_lo = tc.check_range_lo
	forked.check_range_hi = tc.check_range_hi
	// These sparse transform results are immutable by cgen. Share their backing
	// maps just like the node-indexed semantic arrays in fork_program_view.
	unsafe {
		forked.sparse_resolved_call_names = tc.sparse_resolved_call_names
		forked.sparse_resolved_fn_values = tc.sparse_resolved_fn_values
		forked.sparse_statement_nodes = tc.sparse_statement_nodes
		forked.sparse_expr_type_values = tc.sparse_expr_type_values
		forked.sparse_checking_nodes = tc.sparse_checking_nodes
	}
	forked.visible_mutation_cache = unsafe { nil }
	forked.inherit_ownership_codegen_metadata_from(tc)
	forked.set_fresh_type_cache_based_on(tc, tc.type_cache_parse_enabled())
	if !isnil(tc.qualify_name_cache) {
		// Never share the memo across threads; each fork owns a private one.
		forked.qualify_name_cache = &QualifyNameCache{}
	}
	return &forked
}

// fork_type_parse_view creates a lookup-only view in an explicit source
// context. It shares the compilation's immutable indexes, interner, and
// synchronous memoization cache, but none of the caller's function state.
fn (tc &TypeChecker) fork_type_parse_view(file string, module_name string) TypeChecker {
	mut view := tc.fork_program_view(tc.a, map[int][]SymbolId{})
	view.cur_file = file
	view.cur_module = module_name
	view.type_cache = tc.type_cache
	// Signature parsing immediately creates an unscoped resolution view. Reuse
	// this worker's synchronous per-file view cache instead of allocating two
	// full TypeChecker views for every substituted parameter and return type.
	view.resolution_type_views = tc.resolution_type_views
	return view
}

// fork_smartcast_query_view preserves the active lexical/function context for
// a synchronous expression-type query while owning every mutable map it may
// consult. This avoids inheriting unrelated checker state through a whole
// struct copy.
fn (tc &TypeChecker) fork_smartcast_query_view() TypeChecker {
	// This query is synchronous, so any dependency it discovers remains owned
	// by the current master/worker checker rather than crossing worker threads.
	mut view := tc.fork_program_view(tc.a, tc.direct_dependencies_by_fn)
	view.file_scope = tc.file_scope
	view.cur_scope = tc.cur_scope
	view.scope_pool = tc.scope_pool
	view.scope_pool_index = tc.scope_pool_index
	view.expected_expr_id = tc.expected_expr_id
	view.expected_expr_type = tc.expected_expr_type
	view.fn_context = clone_function_check_context(tc.fn_context)
	view.smartcasts = clone_smartcasts(tc.smartcasts)
	view.type_cache = tc.type_cache
	return view
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
	mut forked := tc.fork_program_view(ast, map[int][]SymbolId{})
	// Most transform workers only read signatures. Share the immutable base and
	// clone it lazily if a worker actually synthesizes a declaration; eagerly
	// cloning all maps for every scoped batch dominates self-host memory.
	forked.transform_signature_maps_shared = true
	forked.transform_signature_maps_changed = false
	forked.transform_struct_maps_shared = true
	// Visible-mutation analysis only runs during checking. Do not allocate its
	// three private maps for the many short-lived transform forks.
	forked.visible_mutation_cache = unsafe { nil }
	if check_memos_enabled() {
		// Per-fork memos; the fork (and these caches) live inside one batch
		// scope, so cached scratch-arena strings can never outlive their arena.
		forked.import_info_cache = &ImportInfoCache{}
		forked.qualify_name_cache = &QualifyNameCache{}
	}
	// The transformer propagates call/fn-value resolution metadata onto the call
	// nodes it clones (Transformer.copy_cloned_resolution). In a worker those
	// writes must not touch (or grow/realloc) the shared node-indexed arrays
	// while other threads read them, so each fork gets a private sparse overlay:
	// writes go only to the overlay, reads check it before the shared arrays,
	// and merge_worker replays the entries into the master under shifted ids.
	forked.fork_overlay = &TransformForkOverlay{
		base_node_count: if os.getenv('V3_NO_OVERLAY_RANGE') == '' { ast.nodes.len } else { -1 }
	}
	// Transform helpers allocate inside disposable arenas. A shared interner
	// would let one helper publish map/array storage owned by its arena and leave
	// other helpers with dangling storage when that arena is released. Each
	// helper therefore gets a private canonical table; semantic Type values are
	// still compatible across tables, while TypeIds stay local to its cache.
	forked.type_interner = new_type_interner()
	forked.symbols = new_symbol_interner()
	forked.type_cache = &TypeCache{
		// When the master froze its warm cache behind an overlay (see
		// freeze_type_cache_for_forks), every fork shares that frozen cache as
		// its read-only base instead of re-deriving each memoized type.
		base: if tc.type_cache != unsafe { nil } {
			tc.type_cache.base
		} else {
			&TypeCache(unsafe { nil })
		}
		parse_enabled: if tc.type_cache != unsafe { nil } {
			tc.type_cache.parse_enabled
		} else {
			false
		}
		parse_entries: map[u64]ParseTypeCacheEntry{}
		c_entries: map[TypeId]string{}
		struct_field_entries: map[string]Type{}
		struct_field_misses: map[string]bool{}
		ierror_compat_entries: map[string]int{}
		source_error_embed_entries: map[string]int{}
	}
	return &forked
}

// ensure_private_transform_signatures detaches the signature tables before a
// transform worker writes them. The worker's private maps are merged while its
// disposable arena is still alive.
pub fn (mut tc TypeChecker) ensure_private_transform_signatures() {
	if tc.transform_signature_maps_shared {
		tc.fn_ret_types = tc.fn_ret_types.clone()
		tc.fn_param_types = tc.fn_param_types.clone()
		tc.receiver_method_suffix_index = tc.receiver_method_suffix_index.clone()
		tc.generic_receiver_method_index = clone_string_list_map(tc.generic_receiver_method_index)
		tc.fn_variadic = tc.fn_variadic.clone()
		tc.specialized_generic_fns = tc.specialized_generic_fns.clone()
		tc.fn_type_modules = tc.fn_type_modules.clone()
		tc.fn_type_files = tc.fn_type_files.clone()
		tc.transform_signature_maps_shared = false
	}
	tc.transform_signature_maps_changed = true
}

// transform_signatures_changed reports whether a transform fork detached and
// added signature state that its parent must merge.
pub fn (tc &TypeChecker) transform_signatures_changed() bool {
	return tc.transform_signature_maps_changed
}

// discard_transform_signature_changes makes a fork's private signature tables
// disposable when its parent has already registered the same generated
// signatures. Other worker results can then be merged without publishing
// pointers owned by the worker's temporary arena.
pub fn (mut tc TypeChecker) discard_transform_signature_changes() {
	tc.transform_signature_names_log = []string{}
	tc.transform_signature_maps_changed = false
}

// ensure_private_transform_structs detaches struct metadata before a transform
// worker publishes a generated capture context into its private result.
pub fn (mut tc TypeChecker) ensure_private_transform_structs() {
	if !tc.transform_struct_maps_shared {
		return
	}
	tc.structs = tc.structs.clone()
	tc.struct_modules = tc.struct_modules.clone()
	tc.struct_files = tc.struct_files.clone()
	tc.transform_struct_maps_shared = false
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

// discard_type_cache_overlay_after_forks reattaches the frozen cache without
// publishing memoized entries from parallel work. C generation uses this after
// its workers join because the driver replaces the cache at the end of the
// stage, and worker-arena values must not escape into the persistent base.
pub fn (tc &TypeChecker) discard_type_cache_overlay_after_forks() {
	mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
	if isnil(mtc.type_cache) || isnil(mtc.type_cache.base) {
		return
	}
	mtc.type_cache = mtc.type_cache.base
	if !isnil(mtc.resolution_type_views) {
		mtc.reset_resolution_type_view_cache()
	}
}

// set_fresh_type_cache attaches a new empty TypeCache. Parallel-cgen worker
// checkers use this so the lazily-built lookup indexes and memoizations work
// per worker instead of falling back to their uncached full scans.
pub fn (mut tc TypeChecker) set_fresh_type_cache(parse_enabled bool) {
	if isnil(tc.type_interner) {
		tc.type_interner = new_type_interner()
	}
	if isnil(tc.symbols) {
		tc.symbols = new_symbol_interner()
	}
	if !isnil(tc.pre_transform_type_cache) {
		tc.type_cache = tc.pre_transform_type_cache
		tc.pre_transform_type_cache = unsafe { nil }
		mut cache := tc.type_cache
		cache.base = unsafe { nil }
		cache.parse_enabled = parse_enabled
		cache.parse_hits = 0
		cache.parse_misses = 0
		cache.c_hits = 0
		cache.c_misses = 0
		cache.parse_entries.clear()
		cache.parse_context_valid = false
		cache.parse_last_valid = false
		cache.clear_c_type_entries()
		cache.struct_field_entries.clear()
		cache.struct_field_misses.clear()
		cache.recv_pattern_entries.clear()
		cache.recv_pattern_misses.clear()
		cache.ierror_compat_entries.clear()
		cache.interface_impl_entries.clear()
		cache.source_error_embed_entries.clear()
		cache.source_error_embed_indexed = false
		cache.ierror_impl_names.clear()
		cache.ierror_impl_names_set = false
		cache.short_type_name_index.clear()
		cache.short_type_name_index_built = false
		cache.local_fn_decl_index.clear()
		cache.local_fn_decl_indexed_len = 0
		cache.local_fn_decl_last_module = ''
		return
	}
	tc.type_cache = &TypeCache{
		parse_enabled: parse_enabled
	}
}

// reset_type_interners replaces semantic interners whose backing storage may
// have grown inside a disposable compiler-stage arena.
pub fn (mut tc TypeChecker) reset_type_interners() {
	tc.type_interner = new_type_interner()
	tc.symbols = new_symbol_interner()
}

// set_fresh_type_cache_based_on attaches a new empty TypeCache that falls back
// read-only to `src`'s frozen base cache (see freeze_type_cache_for_forks), so
// parallel-cgen workers start with every type memoized by the check/transform
// phases instead of re-deriving them from a cold cache.
pub fn (mut tc TypeChecker) set_fresh_type_cache_based_on(src &TypeChecker, parse_enabled bool) {
	base := if isnil(src.type_cache) {
		&TypeCache(unsafe { nil })
	} else if !isnil(src.type_cache.base) {
		src.type_cache.base
	} else {
		src.type_cache
	}
	tc.type_cache = &TypeCache{
		parse_enabled: parse_enabled
		base: base
	}
	// C-generation workers can also use disposable arenas. Keep their TypeIds
	// private instead of publishing arena-backed interner storage globally.
	tc.type_interner = new_type_interner()
	tc.symbols = new_symbol_interner()
}

// type_cache_parse_enabled reports whether parse_type memoization is enabled
// on this checker's type cache.
pub fn (tc &TypeChecker) type_cache_parse_enabled() bool {
	return !isnil(tc.type_cache) && tc.type_cache.parse_enabled
}

pub fn (tc &TypeChecker) clear_field_lookup_cache() {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	cache.struct_field_entries.clear()
	cache.struct_field_misses.clear()
}

// clear_c_type_cache invalidates C spellings after monomorphization changes
// concrete type identities and adds materialized generic types.
pub fn (tc &TypeChecker) clear_c_type_cache() {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	cache.clear_c_type_entries()
	if !isnil(cache.base) {
		mut base := cache.base
		base.clear_c_type_entries()
	}
}

// clear_interface_impl_cache invalidates memoized implementer lists after a type-table change.
pub fn (tc &TypeChecker) clear_interface_impl_cache() {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	cache.interface_impl_entries.clear()
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
			tc.type_cache.recv_pattern_entries.free()
			tc.type_cache.recv_pattern_misses.free()
			tc.type_cache.ierror_compat_entries.free()
			tc.type_cache.interface_impl_entries.free()
			tc.type_cache.source_error_embed_entries.free()
		}
	}
	tc.type_cache = &TypeCache{
		parse_enabled: parse_enabled
		parse_entries: map[u64]ParseTypeCacheEntry{}
		c_entries: map[TypeId]string{}
		struct_field_entries: map[string]Type{}
		struct_field_misses: map[string]bool{}
		ierror_compat_entries: map[string]int{}
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
	tc.lexical_smartcast_misses = []bool{len: n}
	tc.checking_nodes = []bool{len: n}
	tc.parallel_check_sparse = false
}

fn (mut tc TypeChecker) init_direct_parent_index(a &flat.FlatAst) {
	tc.direct_parent_ids = []flat.NodeId{len: a.nodes.len, init: flat.empty_node}
	tc.rewritten_parent_ids = []flat.NodeId{}
	tc.value_used_nodes = []bool{len: a.nodes.len}
	tc.fn_check_costs = if tc.building_v_fast { []int{len: a.nodes.len} } else { []int{} }
	tc.declaration_attributes = map[int][]string{}
	tc.insert_include_dirs_by_file = map[string][]string{}
	tc.translated_files = map[string]bool{}
	tc.has_globals_files = map[string]bool{}
	tc.strings_builder_candidates = []int{cap: 1024}
	tc.synthetic_top_level_type_ids = []int{cap: 2048}
	tc.has_goto_nodes = false
}

fn (mut tc TypeChecker) fill_direct_parent_edges(a &flat.FlatAst) {
	mut fn_cost := 0
	for parent_idx, node in a.nodes {
		// Node count alone severely underestimates index-heavy and control-flow
		// functions, which leaves one parallel checker worker running last.
		mut node_cost := 1 + int(node.children_count) * 2
		node_cost += match node.kind {
			.index { 64 }
			.call { 8 }
			.selector { 4 }
			.infix { 8 }
			.for_stmt, .for_in_stmt { 64 }
			.if_expr, .match_stmt { 16 }
			else { 0 }
		}
		fn_cost += node_cost
		if node.kind == .goto_stmt {
			tc.has_goto_nodes = true
		}
		if node.kind == .struct_decl
			&& (is_anonymous_struct_name(node.value) || node.value.contains('@local@')) {
			tc.synthetic_top_level_type_ids << parent_idx
		}
		for child_idx in 0 .. node.children_count {
			child := a.child(&node, child_idx)
			idx := int(child)
			if idx >= 0 && idx < tc.value_used_nodes.len
				&& node.kind !in [.expr_stmt, .block, .match_branch, .fn_decl, .comptime_for] {
				tc.value_used_nodes[idx] = true
			}
			if idx >= 0 && idx < tc.direct_parent_ids.len
				&& tc.direct_parent_ids[idx] == flat.empty_node {
				tc.direct_parent_ids[idx] = flat.NodeId(parent_idx)
			}
		}
		if node.kind == .fn_decl && parent_idx < tc.fn_check_costs.len {
			tc.fn_check_costs[parent_idx] = fn_cost
		}
		if node.kind in [.file, .module_decl, .struct_decl, .type_decl, .interface_decl, .enum_decl,
			.import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl] {
			fn_cost = 0
		}
	}
}

fn (mut tc TypeChecker) collect_direct_parent_metadata(a &flat.FlatAst) {
	for parent_idx, node in a.nodes {
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs := a.child_node(&node, 0)
			if lhs.kind == .ident && tc.expr_is_strings_new_builder_call(a.child(&node, 1)) {
				tc.strings_builder_candidates << parent_idx
			}
		} else if node.kind == .directive {
			if node.value.starts_with('@attributes:') {
				decl_id := node.value['@attributes:'.len..].int()
				if decl_id >= 0 && decl_id < a.nodes.len {
					tc.declaration_attributes[decl_id] = node.generic_params()
					decl := a.nodes[decl_id]
					if decl.kind == .module_decl {
						if source_file := a.source_files[decl.pos.id] {
							tc.collect_module_attributes(node, source_file.name)
						}
					}
				}
			} else if node.value == 'flag' && node.pos.is_valid() {
				if source_file := a.source_files[node.pos.id] {
					if raw_dir := checker_flag_include_dir(node.typ) {
						resolved := tc.resolve_insert_path(raw_dir, source_file.name)
						if resolved !in tc.insert_include_dirs_by_file[source_file.name] {
							tc.insert_include_dirs_by_file[source_file.name] << resolved
						}
					}
				}
			}
		}
	}
}

fn (mut tc TypeChecker) build_direct_parent_index(a &flat.FlatAst) {
	tc.init_direct_parent_index(a)
	tc.fill_direct_parent_edges(a)
	tc.collect_direct_parent_metadata(a)
	tc.direct_parent_index_trusted = true
}

// refresh_direct_parent_index rebuilds parent metadata after source-tree pruning.
pub fn (mut tc TypeChecker) refresh_direct_parent_index(a &flat.FlatAst) {
	tc.build_direct_parent_index(a)
}

// reuse_direct_parent_index_for_unchanged_ast restores the parsed-tree index's trusted
// status when an internal valid-build path has not structurally rewritten the AST.
pub fn (mut tc TypeChecker) reuse_direct_parent_index_for_unchanged_ast(a &flat.FlatAst) bool {
	if tc.direct_parent_ids.len != a.nodes.len || tc.value_used_nodes.len != a.nodes.len {
		return false
	}
	tc.direct_parent_index_trusted = true
	return true
}

// invalidate_direct_parent_index makes generated-node lookups validate parent metadata.
pub fn (mut tc TypeChecker) invalidate_direct_parent_index() {
	tc.direct_parent_index_trusted = false
}

// refresh_rewritten_parent_index rebuilds the node-parent edges after transform
// without resetting declaration metadata collected during semantic checking.
// Rewritten trees can contain hundreds of thousands of new nodes; leaving those
// nodes outside direct_parent_ids makes every parent query scan the whole AST.
pub fn (mut tc TypeChecker) refresh_rewritten_parent_index(a &flat.FlatAst) {
	// Lexical smartcasts deliberately apply only to parsed source nodes, so keep
	// direct_parent_ids at its original length and index appended nodes separately.
	tc.rewritten_parent_ids = []flat.NodeId{len: a.nodes.len, init: flat.empty_node}
	for parent_idx, node in a.nodes {
		for child_idx in 0 .. node.children_count {
			idx := int(a.child(&node, child_idx))
			if idx >= tc.direct_parent_ids.len && idx < tc.rewritten_parent_ids.len
				&& tc.rewritten_parent_ids[idx] == flat.empty_node {
				tc.rewritten_parent_ids[idx] = flat.NodeId(parent_idx)
			}
		}
	}
	// Keep validation enabled because transformed trees may intentionally share
	// a node or rewrite an edge after this index is built.
	tc.direct_parent_index_trusted = false
}

fn (mut tc TypeChecker) build_type_declaration_index(a &flat.FlatAst) {
	tc.type_declaration_ids = map[string][]int{}
	mut module_name := ''
	for index in tc.top_level_idx {
		node := a.nodes[index]
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind !in [.struct_decl, .type_decl] {
			continue
		}
		short_name := node.value.all_after_last('.')
		tc.type_declaration_ids[short_name] << index
		qualified := qualify_decl_name_in_module(node.value, module_name)
		if qualified != short_name {
			tc.type_declaration_ids[qualified] << index
		}
	}
}

fn strings_builder_binding_key(fn_index int, name string) string {
	return '${fn_index}\x00${name}'
}

fn (mut tc TypeChecker) build_fn_declaration_indexes(a &flat.FlatAst) {
	tc.strings_builder_bindings = map[string]bool{}
	tc.static_associated_fn_keys = map[string]bool{}
	tc.declaration_param_mutability = map[string][]bool{}
	tc.strict_map_index_files = map[string]bool{}
	tc.file_import_alias_paths = map[string]string{}
	tc.file_import_suffix_paths = map[string]string{}
	tc.fn_decl_short_name_ids = map[string]int{}
	mut module_name := ''
	mut file_name := ''
	for index in tc.top_level_idx {
		node := a.nodes[index]
		if node.kind == .file {
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			if tc.declaration_has_attribute(flat.NodeId(index), 'strict_map_index') {
				tc.strict_map_index_files[file_name] = true
			}
			continue
		}
		if node.kind == .import_decl {
			path := tc.import_module_path_text(node)
			if node.typ.len > 0 {
				tc.file_import_alias_paths['${file_name}\x00${node.typ}'] = path
			}
			if path.contains('.') {
				suffix_key := '${file_name}\x00${path.all_after_last('.')}'
				if suffix_key !in tc.file_import_suffix_paths {
					tc.file_import_suffix_paths[suffix_key] = path
				}
			}
			continue
		}
		if node.kind == .interface_decl {
			iface_name := qualify_decl_name_in_module(node.value, module_name)
			for child_index in 0 .. node.children_count {
				field := a.child_node(&node, child_index)
				if field.kind != .interface_field || field.op != .dot {
					continue
				}
				mut param_mutability := [field.is_mut]
				for param_index in 0 .. field.children_count {
					param := a.child_node(field, param_index)
					if param.kind != .param {
						if tc.prefix_param_scan {
							break
						}
						continue
					}
					param_mutability << param.is_mut
				}
				tc.declaration_param_mutability['${iface_name}.${field.value}'] = param_mutability
			}
			continue
		}
		if node.kind == .c_fn_decl {
			mut param_mutability := []bool{}
			for child_index in 0 .. node.children_count {
				param := a.child_node(&node, child_index)
				if param.kind != .param {
					if tc.prefix_param_scan {
						break
					}
					continue
				}
				param_mutability << param.is_mut
			}
			c_name := if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
			if c_name !in tc.declaration_param_mutability {
				tc.declaration_param_mutability[c_name] = param_mutability
			}
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		short_name := node.value.all_after_last('.')
		if short_name !in tc.fn_decl_short_name_ids {
			tc.fn_decl_short_name_ids[short_name] = index
		}
		qname := checker_qualified_fn_name(module_name, node.value)
		mut param_mutability := []bool{}
		for child_index in 0 .. node.children_count {
			param := a.child_node(&node, child_index)
			if param.kind != .param {
				if tc.prefix_param_scan {
					break
				}
				continue
			}
			param_mutability << param.is_mut
		}
		if node.value !in tc.declaration_param_mutability {
			tc.declaration_param_mutability[node.value] = param_mutability
		}
		if qname !in tc.declaration_param_mutability {
			tc.declaration_param_mutability[qname] = param_mutability
		}
		if node.value.contains('.') {
			is_static := node.children_count == 0 || a.child_node(&node, 0).kind != .param
				|| a.child_node(&node, 0).op != .dot
			if node.value !in tc.static_associated_fn_keys {
				tc.static_associated_fn_keys[node.value] = is_static
			}
			if qname !in tc.static_associated_fn_keys {
				tc.static_associated_fn_keys[qname] = is_static
			}
		}
	}
	for candidate_idx in tc.strings_builder_candidates {
		candidate := a.nodes[candidate_idx]
		lhs := a.child_node(&candidate, 0)
		mut ancestor := int(tc.direct_parent_ids[candidate_idx])
		for ancestor >= 0 && ancestor < a.nodes.len {
			ancestor_node := a.nodes[ancestor]
			if ancestor_node.kind == .fn_literal {
				break
			}
			if ancestor_node.kind == .fn_decl {
				tc.strings_builder_bindings[strings_builder_binding_key(ancestor, lhs.value)] = true
				break
			}
			next := int(tc.direct_parent_ids[ancestor])
			if next == ancestor {
				break
			}
			ancestor = next
		}
	}
	tc.strings_builder_candidates = []int{}
}

// has_fn_decl_short_name reports whether collection indexed a function
// declaration with the given unqualified name.
pub fn (tc &TypeChecker) has_fn_decl_short_name(name string) bool {
	return name in tc.fn_decl_short_name_ids
}

fn (mut tc TypeChecker) build_enclosing_generic_param_index(a &flat.FlatAst) {
	tc.enclosing_generic_params_by_node = map[int][]string{}
	tc.enclosing_generic_param_masks = []u32{len: a.nodes.len}
	mut previous_top_level_idx := -1
	for idx in tc.top_level_idx {
		node := a.nodes[idx]
		if node.kind != .fn_decl || node.generic_params().len == 0 {
			previous_top_level_idx = idx
			continue
		}
		params := node.generic_params()
		for child_idx in 0 .. node.children_count {
			child := a.child(&node, child_idx)
			if int(child) >= 0 {
				tc.enclosing_generic_params_by_node[int(child)] = params
			}
		}
		tc.fill_enclosing_generic_param_mask(previous_top_level_idx + 1, idx + 1, params)
		previous_top_level_idx = idx
	}
}

fn (mut tc TypeChecker) cache_fn_generic_params(a &flat.FlatAst) {
	mut previous_top_level_idx := -1
	for idx in tc.top_level_idx {
		node := a.nodes[idx]
		if node.kind != .fn_decl {
			previous_top_level_idx = idx
			continue
		}
		params := tc.infer_decl_generic_param_names(node)
		if params.len > 0 {
			tc.enclosing_generic_params_by_node[idx] = params
			tc.fill_enclosing_generic_param_mask(previous_top_level_idx + 1, idx + 1, params)
		}
		previous_top_level_idx = idx
	}
}

fn (mut tc TypeChecker) fill_enclosing_generic_param_mask(start int, end int, params []string) {
	mut mask := u32(0)
	for param in params {
		if param.len == 1 && param[0] >= `A` && param[0] <= `Z` {
			mask |= u32(1) << u32(param[0] - `A`)
		}
	}
	if mask == 0 {
		return
	}
	safe_start := int_max(start, 0)
	safe_end := int_min(end, tc.enclosing_generic_param_masks.len)
	for idx in safe_start .. safe_end {
		tc.enclosing_generic_param_masks[idx] |= mask
	}
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

// reserve_transform_node_caches reserves node-indexed semantic storage before
// a scoped transform starts, keeping the escaping slabs in the compilation arena.
pub fn (mut tc TypeChecker) reserve_transform_node_caches(n int) {
	reserve_string_cache(mut tc.resolved_call_names, n)
	reserve_bool_cache(mut tc.resolved_call_set, n)
	reserve_string_cache(mut tc.resolved_fn_value_names, n)
	reserve_bool_cache(mut tc.resolved_fn_value_set, n)
	reserve_bool_cache(mut tc.statement_nodes, n)
	reserve_type_cache(mut tc.expr_type_values, n)
	reserve_bool_cache(mut tc.expr_type_set, n)
	reserve_bool_cache(mut tc.checking_nodes, n)
}

// materialize_sparse_transform_node_caches compacts transform-created semantic
// entries into dense node-indexed arrays and reserves their expected final
// capacity before monomorphization appends more nodes.
pub fn (mut tc TypeChecker) materialize_sparse_transform_node_caches(n int, capacity int) {
	if !tc.parallel_check_sparse {
		tc.reserve_transform_node_caches(capacity)
		tc.extend_node_caches(n)
		return
	}
	target_cap := if capacity > n { capacity } else { n }
	tc.reserve_transform_node_caches(target_cap)
	tc.parallel_check_sparse = false
	tc.extend_node_caches(n)
	for idx, name in tc.sparse_resolved_call_names {
		if idx >= 0 && idx < n {
			tc.resolved_call_names[idx] = name
			tc.resolved_call_set[idx] = true
		}
	}
	for idx, name in tc.sparse_resolved_fn_values {
		if idx >= 0 && idx < n {
			tc.resolved_fn_value_names[idx] = name
			tc.resolved_fn_value_set[idx] = true
		}
	}
	for idx, is_statement in tc.sparse_statement_nodes {
		if is_statement && idx >= 0 && idx < n {
			tc.statement_nodes[idx] = true
		}
	}
	for idx, typ in tc.sparse_expr_type_values {
		if idx >= 0 && idx < n {
			tc.expr_type_values[idx] = typ
			tc.expr_type_set[idx] = true
		}
	}
	tc.sparse_resolved_call_names = map[int]string{}
	tc.sparse_resolved_fn_values = map[int]string{}
	tc.sparse_statement_nodes = map[int]bool{}
	tc.sparse_expr_type_values = map[int]Type{}
	tc.sparse_checking_nodes = map[int]bool{}
}

// reserve_scoped_transform_metadata keeps tables that receive escaping
// transform additions in the compilation arena in the common case while
// scratch allocations use a disposable arena. The signature maps are rebuilt
// after promotion, so this headroom is an optimization rather than an
// ownership requirement.
pub fn (mut tc TypeChecker) reserve_scoped_transform_metadata(signature_headroom int) {
	tc.fn_ret_types.reserve(u32(tc.fn_ret_types.len + signature_headroom))
	tc.fn_param_types.reserve(u32(tc.fn_param_types.len + signature_headroom))
	tc.fn_variadic.reserve(u32(tc.fn_variadic.len + signature_headroom))
	tc.specialized_generic_fns.reserve(u32(tc.specialized_generic_fns.len + signature_headroom))
	if !isnil(tc.type_interner) {
		mut interner := tc.type_interner
		interner.reserve(signature_headroom)
	}
	if !isnil(tc.symbols) {
		mut symbols := tc.symbols
		symbols.reserve(signature_headroom)
	}
}

// rebuild_scoped_transform_signature_maps moves signature maps, keys, and nested
// type metadata into the current arena after a disposable transform scope has
// been left.
pub fn (mut tc TypeChecker) rebuild_scoped_transform_signature_maps() {
	mut ret_types := map[string]Type{}
	for name, ret in tc.fn_ret_types {
		ret_types[name.clone()] = clone_owned_type(ret)
	}
	mut param_types := map[string][]Type{}
	for name, params in tc.fn_param_types {
		param_types[name.clone()] = clone_owned_types(params)
	}
	mut variadic := map[string]bool{}
	for name, is_variadic in tc.fn_variadic {
		variadic[name.clone()] = is_variadic
	}
	mut specialized := map[string]bool{}
	for name, is_specialized in tc.specialized_generic_fns {
		specialized[name.clone()] = is_specialized
	}
	tc.fn_ret_types = ret_types.move()
	tc.fn_param_types = param_types.move()
	tc.fn_variadic = variadic.move()
	tc.specialized_generic_fns = specialized.move()
	// The change log is only meaningful while a fork is waiting to be merged.
	// Its backing array may itself belong to the disposable transform scope, so
	// do not let a later monomorph pass append through that released storage.
	tc.transform_signature_names_log = []string{}
	tc.transform_signature_maps_shared = false
	tc.transform_signature_maps_changed = false
}

// begin_sparse_transform_node_caches keeps source-node entries in their dense
// checked arrays and records transform-created node metadata sparsely.
pub fn (mut tc TypeChecker) begin_sparse_transform_node_caches(base_nodes int) {
	tc.parallel_check_sparse = true
	tc.check_range_lo = 0
	tc.check_range_hi = base_nodes - 1
	if tc.scope_parallel_check_workers && isnil(tc.pre_transform_type_cache)
		&& !isnil(tc.type_cache) {
		tc.pre_transform_type_cache = tc.type_cache
		tc.type_cache = &TypeCache{
			base: tc.pre_transform_type_cache
			parse_enabled: tc.pre_transform_type_cache.parse_enabled
		}
	}
}

// promote_scoped_transform_interners moves additions made by a scoped
// transform into the current compilation arena before that scope is freed.
pub fn (mut tc TypeChecker) promote_scoped_transform_interners(type_start int, symbol_start int, scope voidptr) {
	if !isnil(tc.type_interner) {
		mut interner := tc.type_interner
		interner.promote_from(type_start, scope)
	}
	if !isnil(tc.symbols) {
		mut symbols := tc.symbols
		symbols.promote_from(symbol_start, scope)
	}
}

fn reserve_string_cache(mut values []string, n int) {
	if n > values.cap {
		unsafe { values.grow_cap(n - values.cap) }
	}
}

fn reserve_bool_cache(mut values []bool, n int) {
	if n > values.cap {
		unsafe { values.grow_cap(n - values.cap) }
	}
}

fn reserve_type_cache(mut values []Type, n int) {
	if n > values.cap {
		unsafe { values.grow_cap(n - values.cap) }
	}
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
	if tc.scope_pool_index > 0
		&& voidptr(tc.cur_scope) == voidptr(tc.scope_pool[tc.scope_pool_index - 1]) {
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
		msg: msg
		kind: kind
		node: node
		file: tc.cur_file
		node_kind: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].kind.str()
		} else {
			''
		}
		node_value: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].value
		} else {
			''
		}
		node_pos: tc.node_position_string(node)
		pos: tc.node_pos(node)
	}
}

fn (mut tc TypeChecker) record_error_unfiltered(kind TypeErrorKind, msg string, node flat.NodeId) {
	tc.errors << tc.make_type_error(kind, msg, node)
}

fn (mut tc TypeChecker) record_error_unfiltered_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) {
	tc.errors << tc.make_type_error_at(kind, msg, node, pos)
}

fn (tc &TypeChecker) make_type_error(kind TypeErrorKind, msg string, node flat.NodeId) TypeError {
	return tc.make_type_error_at(kind, msg, node, tc.node_pos(node))
}

fn (tc &TypeChecker) make_type_error_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) TypeError {
	return TypeError{
		msg: msg.replace('[fn(', '[fn (')
		kind: kind
		node: node
		file: tc.cur_file
		node_kind: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].kind.str()
		} else {
			''
		}
		node_value: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].value
		} else {
			''
		}
		node_pos: tc.node_position_string(node)
		pos: pos
	}
}

fn (mut tc TypeChecker) record_error_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) {
	if !tc.should_diagnose(node) {
		return
	}
	tc.errors << tc.make_type_error_at(kind, msg, node, pos)
}

fn (mut tc TypeChecker) record_error_severity_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos, severity string) {
	if !tc.should_diagnose(node) {
		return
	}
	base := tc.make_type_error_at(kind, msg, node, pos)
	tc.errors << TypeError{
		...base
		severity: severity
	}
}

// record_cgen_error_at records a post-transform validation error with cgen severity.
pub fn (mut tc TypeChecker) record_cgen_error_at(msg string, node flat.NodeId, source_id flat.NodeId, marker string) {
	tc.record_error_severity_at(.compile_error, msg, node, tc.propagation_operator_pos(source_id, node, marker), 'cgen error:')
}

fn (mut tc TypeChecker) record_error_with_details_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos, details []string) {
	if !tc.should_diagnose(node) {
		return
	}
	base := tc.make_type_error_at(kind, msg, node, pos)
	tc.errors << TypeError{
		...base
		details: details.clone()
	}
}

fn (mut tc TypeChecker) record_notice_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) {
	if !tc.should_diagnose(node) {
		return
	}
	if tc.notes_are_errors {
		if tc.errors.any(it.kind == kind && it.msg == msg && it.pos == pos) {
			return
		}
		base := tc.make_type_error_at(kind, msg, node, pos)
		tc.errors << TypeError{
			...base
			severity: 'error:'
		}
		return
	}
	if tc.notices.any(it.kind == kind && it.msg == msg && it.pos == pos) {
		return
	}
	tc.notices << tc.make_type_error_at(kind, msg, node, pos)
}

fn (mut tc TypeChecker) record_notice_with_details_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos, details []string) {
	if !tc.should_diagnose(node) {
		return
	}
	if tc.notes_are_errors {
		if tc.errors.any(it.kind == kind && it.msg == msg && it.pos == pos) {
			return
		}
		base := tc.make_type_error_at(kind, msg, node, pos)
		tc.errors << TypeError{
			...base
			details: details.clone()
			severity: 'error:'
		}
		return
	}
	if tc.notices.any(it.kind == kind && it.msg == msg && it.pos == pos) {
		return
	}
	base := tc.make_type_error_at(kind, msg, node, pos)
	tc.notices << TypeError{
		...base
		details: details.clone()
	}
}

fn (mut tc TypeChecker) record_warning_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) {
	if !tc.should_diagnose(node) {
		return
	}
	if tc.warns_are_errors {
		if tc.errors.any(it.kind == kind && it.msg == msg && it.pos == pos) {
			return
		}
		base := tc.make_type_error_at(kind, msg, node, pos)
		tc.errors << TypeError{
			...base
			severity: 'error:'
		}
		return
	}
	if tc.notices.any(it.kind == kind && it.msg == msg && it.pos == pos && it.severity == 'warning:') {
		return
	}
	base := tc.make_type_error_at(kind, msg, node, pos)
	tc.notices << TypeError{
		...base
		severity: 'warning:'
	}
}

fn (tc &TypeChecker) has_type_error(kind TypeErrorKind, msg string, node flat.NodeId) bool {
	return tc.errors.any(it.kind == kind && it.msg == msg && it.node == node)
}

fn (tc &TypeChecker) has_type_notice(kind TypeErrorKind, msg string, node flat.NodeId) bool {
	return tc.notices.any(it.kind == kind && it.msg == msg && it.node == node)
}

fn (tc &TypeChecker) node_pos(node flat.NodeId) token.Pos {
	if int(node) < 0 || int(node) >= tc.a.nodes.len {
		return token.Pos{}
	}
	return tc.a.nodes[int(node)].pos
}

fn (tc &TypeChecker) node_position_string(node flat.NodeId) string {
	if int(node) < 0 || int(node) >= tc.a.nodes.len {
		return ''
	}
	pos := tc.a.nodes[int(node)].pos
	if source_pos := tc.a.source_position(pos) {
		return source_pos.str()
	}
	return pos.str()
}

fn (mut tc TypeChecker) record_unsupported_generic(msg string, node flat.NodeId) {
	if !tc.should_diagnose_unsupported_generic(node) {
		return
	}
	tc.errors << TypeError{
		msg: msg
		kind: .unsupported_generic
		node: node
		pos: tc.node_pos(node)
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
			part := trimmed_space(text[start..i])
			if part.len > 0 {
				parts << part
			}
			start = i + 1
		}
	}
	part := trimmed_space(text[start..])
	if part.len > 0 {
		parts << part
	}
	return parts
}

@[direct_array_access]
fn comma_attr_text_has(text string, name string) bool {
	if text.len < name.len {
		return false
	}
	mut start := 0
	for start < text.len {
		for start < text.len && (text[start] == ` ` || text[start] == `\t` || text[start] == `,`) {
			start++
		}
		mut end := start
		for end < text.len && text[end] != `,` {
			end++
		}
		mut trimmed_end := end
		for trimmed_end > start && (text[trimmed_end - 1] == ` ` || text[trimmed_end - 1] == `\t`) {
			trimmed_end--
		}
		if trimmed_end - start == name.len {
			mut equal := true
			for i in 0 .. name.len {
				if text[start + i] != name[i] {
					equal = false
					break
				}
			}
			if equal {
				return true
			}
		}
		start = end + 1
	}
	return false
}

// collect supports collect handling for TypeChecker.
fn file_index_usable(a &flat.FlatAst) bool {
	return a.file_node_ids.len > 0 && a.file_node_ids.len % 2 == 0 && !a.file_index_incomplete
		&& os.getenv('V3_NO_FILE_IDX') == ''
}

// collect_top_level_idx_fast rebuilds the top-level declaration index from the
// parser-recorded (marker, trailing) .file node pairs instead of scanning every
// AST node: the trailing file node's children are exactly the file's top-level
// declarations (with comptime_if/block containers descended in child order,
// which matches ascending node-id order). Output must stay identical to the
// full scan in collect().
fn (mut tc TypeChecker) collect_top_level_idx_fast(a &flat.FlatAst, inactive []bool) {
	if inactive.len > 0 {
		// The scan records every flagged id (whole inactive subtrees, consumed
		// by prune_inactive_top_level_comptime).
		for i, f in inactive {
			if f {
				tc.inactive_top_level_node_ids << i
			}
		}
	}
	mut synthetic_pos := 0
	for k := 0; k + 1 < a.file_node_ids.len; k += 2 {
		marker := a.file_node_ids[k]
		trailing := a.file_node_ids[k + 1]
		for synthetic_pos < tc.synthetic_top_level_type_ids.len
			&& tc.synthetic_top_level_type_ids[synthetic_pos] <= marker {
			synthetic_pos++
		}
		idx_file := a.nodes[marker].value
		tc.top_level_idx << marker
		tnode := a.nodes[trailing]
		mut idx_module := ''
		for ci in 0 .. tnode.children_count {
			decl_idx := int(a.child(&tnode, ci))
			for synthetic_pos < tc.synthetic_top_level_type_ids.len
				&& tc.synthetic_top_level_type_ids[synthetic_pos] < decl_idx {
				synthetic_idx := tc.synthetic_top_level_type_ids[synthetic_pos]
				if synthetic_idx < trailing {
					idx_module = tc.collect_index_child(a, synthetic_idx, idx_file, idx_module, inactive)
				}
				synthetic_pos++
			}
			idx_module = tc.collect_index_child(a, decl_idx, idx_file, idx_module, inactive)
			if synthetic_pos < tc.synthetic_top_level_type_ids.len
				&& tc.synthetic_top_level_type_ids[synthetic_pos] == decl_idx {
				synthetic_pos++
			}
			// apply_decl_attrs emits module attributes as the node immediately
			// following the module declaration, outside the trailing file node's
			// declaration children.
			attr_idx := decl_idx + 1
			if decl_idx >= 0 && decl_idx < a.nodes.len && a.nodes[decl_idx].kind == .module_decl
				&& attr_idx < trailing {
				attr_node := a.nodes[attr_idx]
				if attr_node.kind == .directive && attr_node.value == '@attributes:${decl_idx}' {
					idx_module = tc.collect_index_child(a, attr_idx, idx_file, idx_module, inactive)
				}
			}
		}
		for synthetic_pos < tc.synthetic_top_level_type_ids.len
			&& tc.synthetic_top_level_type_ids[synthetic_pos] < trailing {
			synthetic_idx := tc.synthetic_top_level_type_ids[synthetic_pos]
			idx_module = tc.collect_index_child(a, synthetic_idx, idx_file, idx_module, inactive)
			synthetic_pos++
		}
		tc.top_level_idx << trailing
	}
}

fn (mut tc TypeChecker) collect_module_attributes(node flat.Node, file string) {
	for attr in node.generic_params() {
		match attr.all_before(':').trim_space() {
			'translated' {
				tc.translated_files[file] = true
			}
			'has_globals' {
				tc.has_globals_files[file] = true
			}
			else {}
		}
	}
}

fn (mut tc TypeChecker) check_insert_directive(id flat.NodeId, node flat.Node, file string, module_name string) {
	if node.value == 'flag' && node.typ.contains('`') {
		saved_file := tc.cur_file
		saved_module := tc.cur_module
		tc.cur_file = file
		tc.cur_module = if module_name.len > 0 { module_name } else { 'main' }
		flag := node.typ.trim_space()
		tc.record_error_at(.compile_error, 'bad #flag `${flag}`: shell command substitution with backticks is not supported; use #pkgconfig or explicit flags instead', id, node.pos)
		tc.cur_file = saved_file
		tc.cur_module = saved_module
		return
	}
	if node.value != 'insert' || node.typ.len < 2 || file.len == 0 {
		return
	}
	raw_target := node.typ.trim_space()
	target := raw_target.trim('"').trim("'")
	mut resolved := tc.resolve_insert_path(target, file)
	if os.exists(resolved) {
		return
	}
	if !os.is_abs_path(target) && !target.starts_with('@') {
		for include_dir in tc.insert_include_dirs_by_file[file] {
			resolved = os.join_path_single(include_dir, target)
			if os.exists(resolved) {
				return
			}
		}
	}
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	tc.cur_file = file
	tc.cur_module = if module_name.len > 0 { module_name } else { 'main' }
	tc.record_error_at(.compile_error, 'The file ${raw_target}, needed for insertion by module `${tc.cur_module}`, does not exist.', id, node.pos)
	tc.cur_file = saved_file
	tc.cur_module = saved_module
}

fn (tc &TypeChecker) resolve_insert_path(target string, file string) string {
	mut resolved := target
	if resolved.contains('@VEXEROOT') {
		resolved = resolved.replace('@VEXEROOT', tc.compiler_vroot)
	}
	if resolved.contains('@VMODROOT') {
		resolved = resolved.replace('@VMODROOT', checker_vmod_root_for_file(file))
	}
	if resolved.contains('@DIR') {
		resolved = resolved.replace('@DIR', os.real_path(os.dir(file)))
	}
	if !os.is_abs_path(resolved) {
		resolved = os.join_path(os.dir(file), resolved)
	}
	return os.real_path(resolved)
}

fn checker_vmod_root_for_file(file string) string {
	mut dir := if file.len > 0 { os.dir(file) } else { os.getwd() }
	original := dir
	for {
		if os.exists(os.join_path_single(dir, 'v.mod')) {
			return os.real_path(dir)
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return os.real_path(original)
		}
		dir = parent
	}
	return os.real_path(original)
}

fn checker_flag_include_dir(raw string) ?string {
	tokens := util.tokenize_c_flag(raw.trim_space())
	for i, token in tokens {
		if token in ['-I', '-isystem'] {
			if i + 1 >= tokens.len {
				return none
			}
			return tokens[i + 1].trim('"\'')
		}
		if token.starts_with('-I') && token.len > 2 {
			return token[2..].trim('"\'')
		}
	}
	return none
}

fn (mut tc TypeChecker) collect_index_child(a &flat.FlatAst, i int, idx_file string, idx_module string, inactive []bool) string {
	if i < 0 || i >= a.nodes.len {
		return idx_module
	}
	if inactive.len > 0 && i < inactive.len && inactive[i] {
		return idx_module
	}
	node := a.nodes[i]
	match node.kind {
		.directive {
			tc.check_insert_directive(flat.NodeId(i), node, idx_file, idx_module)
			if idx_file.len > 0 && node.value.starts_with('@attributes:') {
				decl_idx := node.value['@attributes:'.len..].int()
				if decl_idx >= 0 && decl_idx < a.nodes.len && a.nodes[decl_idx].kind == .module_decl {
					tc.collect_module_attributes(node, idx_file)
				}
			}
		}
		.module_decl {
			tc.top_level_idx << i
			return node.value
		}
		.struct_decl {
			if node.value == 'string' {
				tc.has_builtins = true
			}
			tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, node.value)] = true
			if node.generic_params().len == 0 {
				tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, node.value.all_after_last('.'))] = true
			}
			tc.top_level_idx << i
		}
		.type_decl, .interface_decl, .enum_decl {
			tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, node.value)] = true
			if node.kind != .enum_decl && node.generic_params().len == 0 {
				tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, node.value.all_after_last('.'))] = true
			}
			tc.top_level_idx << i
		}
		.import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl {
			tc.top_level_idx << i
		}
		.comptime_if, .block {
			// Top-level $if containers hold declarations the full scan reaches
			// by node order; descend in child order (same ascending ids).
			mut inner_module := idx_module
			for ci in 0 .. node.children_count {
				inner_module = tc.collect_index_child(a, int(a.child(&node, ci)), idx_file, inner_module, inactive)
			}
			return inner_module
		}
		else {}
	}
	return idx_module
}

pub fn (mut tc TypeChecker) collect(a &flat.FlatAst) {
	mut ck_c_sw := time.new_stopwatch()
	mut ck_part_sw := time.new_stopwatch()
	tc.a = a
	tc.visible_mutation_cache = new_visible_mutation_cache()
	tc.unsafe_c_fns.clear()
	tc.v_fn_semantic_names.clear()
	tc.has_spawn_expr = -1
	tc.direct_dependencies_by_fn = map[int][]SymbolId{}
	tc.file_scope = new_scope(unsafe { nil })
	tc.cur_scope = tc.file_scope
	tc.scope_pool_index = 0
	parallel_index_prep := tc.prepare_collect_index_parallel(a)
	if !parallel_index_prep {
		tc.reset_node_caches(a.nodes.len)
		if tc.verbose {
			tc.timing_profile('  [ttime]       ck idx reset ${f64(ck_part_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			ck_part_sw.restart()
		}
		tc.build_direct_parent_index(a)
		if tc.verbose {
			tc.timing_profile('  [ttime]       ck idx parent ${f64(ck_part_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
			ck_part_sw.restart()
		}
	} else if tc.verbose {
		tc.timing_profile('  [ttime]       ck idx parallel ${f64(ck_part_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		ck_part_sw.restart()
	}
	$if ownership ? {
		tc.ownership_reset()
	}
	tc.type_cache = &TypeCache{
		parse_entries: map[u64]ParseTypeCacheEntry{}
		c_entries: map[TypeId]string{}
		struct_field_entries: map[string]Type{}
		struct_field_misses: map[string]bool{}
		ierror_compat_entries: map[string]int{}
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
	tc.concrete_type_scope_keys = map[string]bool{}
	tc.top_level_idx = []int{cap: 65536}
	if !parallel_index_prep {
		tc.prepare_threads_condition()
	}
	inactive_comptime_nodes := tc.inactive_top_level_comptime_nodes()
	tc.inactive_top_level_node_ids.clear()
	if tc.verbose {
		tc.timing_profile('  [ttime]       ck idx prep  ${f64(ck_part_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		ck_part_sw.restart()
	}
	if file_index_usable(a) {
		tc.collect_top_level_idx_fast(a, inactive_comptime_nodes)
		tc.top_level_idx_nodes_len = a.nodes.len
		tc.reserve_collect_maps()
		tc.timing_profile('  [ttime]     ck c idx       ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms (fast)')
		tc.collect_after_index(a)
		return
	}
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
			.directive {
				tc.check_insert_directive(flat.NodeId(i), node, idx_file, idx_module)
				if idx_file.len > 0 && node.value.starts_with('@attributes:') {
					decl_idx := node.value['@attributes:'.len..].int()
					if decl_idx >= 0 && decl_idx < a.nodes.len
						&& a.nodes[decl_idx].kind == .module_decl {
						tc.collect_module_attributes(node, idx_file)
					}
				}
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
				if node.generic_params().len == 0 {
					tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, node.value.all_after_last('.'))] = true
				}
				tc.top_level_idx << i
			}
			.type_decl, .interface_decl, .enum_decl {
				tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, node.value)] = true
				if node.kind != .enum_decl && node.generic_params().len == 0 {
					tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, node.value.all_after_last('.'))] = true
				}
				tc.top_level_idx << i
			}
			.import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl {
				tc.top_level_idx << i
			}
			else {}
		}
	}
	tc.top_level_idx_nodes_len = a.nodes.len
	tc.reserve_collect_maps()
	tc.timing_profile('  [ttime]     ck c idx       ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	tc.collect_after_index(a)
}

fn (mut tc TypeChecker) reserve_collect_maps() {
	// The collection passes below fill the signature/type tables from empty;
	// with ~10k declarations each hot map otherwise pays a dozen doubling
	// rehashes. Reserve once from the now-known top-level count (short-name and
	// qualified spellings both register, hence the 2x factor).
	decl_estimate := u32(tc.top_level_idx.len)
	if decl_estimate > 512 {
		tc.fn_ret_types.reserve(u32(tc.fn_ret_types.len) + decl_estimate * 2)
		tc.fn_param_types.reserve(u32(tc.fn_param_types.len) + decl_estimate * 2)
		tc.fn_variadic.reserve(u32(tc.fn_variadic.len) + decl_estimate * 2)
		tc.fn_ret_type_texts.reserve(u32(tc.fn_ret_type_texts.len) + decl_estimate * 2)
		tc.v_fn_semantic_names.reserve(u32(tc.v_fn_semantic_names.len) + decl_estimate * 2)
		tc.structs.reserve(u32(tc.structs.len) + decl_estimate)
		tc.struct_modules.reserve(u32(tc.struct_modules.len) + decl_estimate)
		tc.struct_files.reserve(u32(tc.struct_files.len) + decl_estimate)
		tc.const_types.reserve(u32(tc.const_types.len) + decl_estimate)
		tc.const_exprs.reserve(u32(tc.const_exprs.len) + decl_estimate)
		tc.fn_type_files.reserve(u32(tc.fn_type_files.len) + decl_estimate)
		tc.fn_type_modules.reserve(u32(tc.fn_type_modules.len) + decl_estimate)
		if os.getenv('V3_NO_WIDE_COLLECT_RESERVES') == '' {
			// Pass 1 records one visibility entry per declaration, while pass 2
			// derives several method-suffix and visible-mutation keys per function.
			// These maps are populated from separate pool lanes, so reserving here
			// avoids their otherwise serial rehash ladders without sharing writes.
			tc.declaration_visibility.reserve(u32(tc.declaration_visibility.len) + decl_estimate * 2)
			tc.receiver_method_suffix_index.reserve(u32(tc.receiver_method_suffix_index.len) + decl_estimate * 3)
			if !isnil(tc.visible_mutation_cache) {
				mut mutation_cache := tc.visible_mutation_cache
				mutation_cache.decls.reserve(u32(mutation_cache.decls.len) + decl_estimate * 8)
			}
			if os.getenv('V3_NO_EXTENDED_COLLECT_RESERVES') == '' {
				tc.fn_type_files.reserve(u32(tc.fn_type_files.len) + decl_estimate * 3)
				tc.fn_type_modules.reserve(u32(tc.fn_type_modules.len) + decl_estimate * 3)
				tc.type_cache.c_name_entries.reserve(u32(tc.type_cache.c_name_entries.len) + decl_estimate * 3)
			}
		}
	}
}

fn (mut tc TypeChecker) check_alias_declaration_cycles() {
	mut module_name := ''
	mut seen_types := map[string]bool{}
	for index in tc.top_level_idx {
		node := tc.a.nodes[index]
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind in [.struct_decl, .type_decl, .interface_decl, .enum_decl] {
			qname := qualify_decl_name_in_module(node.value, module_name)
			if seen_types[qname] {
				continue
			}
			seen_types[qname] = true
		}
		if node.kind != .type_decl || node.children_count > 0 || node.typ.len == 0 {
			continue
		}
		name := node.value
		container := recursive_alias_container(name, node.typ)
		if container.len > 0 {
			tc.recursive_alias_names[name] = true
			tc.errors << tc.make_type_error_at(.unknown_type, 'alias `${name}` forms a recursive cycle; recursive declarations of aliases are not allowed - the alias `${name}` is used in the ${container}', flat.NodeId(index), node.pos)
			continue
		}
		target := trimmed_space(node.typ)
		if !is_plain_type_symbol(target) || !tc.alias_chain_reaches(name, target, 0) {
			continue
		}
		tc.recursive_alias_names[name] = true
		tc.errors << tc.make_type_error_at(.unknown_type, 'alias `${name}` forms a recursive cycle; alias `${name}` forms a cycle through `${target}`', flat.NodeId(index), node.pos)
	}
}

fn (tc &TypeChecker) alias_chain_reaches(start string, current string, depth int) bool {
	if current == start {
		return true
	}
	if depth >= tc.type_aliases.len {
		return false
	}
	target := tc.type_aliases[current] or { return false }
	clean := trimmed_space(target)
	if !is_plain_type_symbol(clean) {
		return false
	}
	return tc.alias_chain_reaches(start, clean, depth + 1)
}

fn recursive_alias_container(name string, type_text string) string {
	clean := trimmed_space(type_text)
	if !type_text_contains_symbol(clean, name) {
		return ''
	}
	if clean.starts_with('map[') && (clean.contains(']fn (') || clean.contains(']fn(')) {
		return ''
	}
	if clean.starts_with('[]') {
		return 'array'
	}
	if clean.starts_with('[') {
		close := clean.index_u8(`]`)
		if close > 1 {
			return 'fixed array'
		}
	}
	if clean.starts_with('map[') {
		close := find_matching_bracket(clean, 3)
		if close > 3 {
			if type_text_contains_symbol(clean[4..close], name) {
				return 'map key'
			}
			value_text := if close + 1 < clean.len { trimmed_space(clean[close + 1..]) } else { '' }
			if value_text.len > 0 && !value_text.starts_with('fn ')
				&& type_text_contains_symbol(value_text, name) {
				return 'map value'
			}
		}
	}
	return ''
}

fn type_text_contains_symbol(type_text string, name string) bool {
	if name.len == 0 || type_text.len < name.len {
		return false
	}
	mut start := 0
	for {
		relative := type_text[start..].index(name) or { return false }
		index := start + relative
		before_ok := index == 0 || !is_type_symbol_byte(type_text[index - 1])
		end := index + name.len
		after_ok := end == type_text.len || !is_type_symbol_byte(type_text[end])
		if before_ok && after_ok {
			return true
		}
		start = index + name.len
		if start >= type_text.len {
			return false
		}
	}
	return false
}

fn is_type_symbol_byte(ch u8) bool {
	return ch.is_alnum() || ch in [`_`, `.`]
}

fn is_plain_type_symbol(type_text string) bool {
	if type_text.len == 0 {
		return false
	}
	for ch in type_text {
		if !is_type_symbol_byte(ch) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) register_declaration_visibility(node flat.Node) {
	visibility := DeclarationVisibility{
		module_name: tc.cur_module
		kind: node.kind
		is_pub: node.op == .arrow
	}
	match node.kind {
		.fn_decl {
			name := checker_qualified_fn_name(tc.cur_module, node.value)
			tc.declaration_visibility[name] = visibility
			if tc.cur_module == 'builtin' && visibility.is_pub {
				tc.declaration_visibility['builtin.${node.value}'] = visibility
			}
		}
		.struct_decl, .type_decl, .interface_decl, .enum_decl {
			name := qualify_decl_name_in_module(node.value, tc.cur_module)
			tc.declaration_visibility[name] = visibility
		}
		.const_decl {
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .const_field {
					continue
				}
				name := qualify_decl_name_in_module(field.value, tc.cur_module)
				tc.declaration_visibility[name] = visibility
			}
		}
		.global_decl {
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .field_decl || field.value.starts_with('C.') {
					continue
				}
				name := qualify_decl_name_in_module(field.value, tc.cur_module)
				tc.declaration_visibility[name] = DeclarationVisibility{
					module_name: tc.cur_module
					kind: .global_decl
					is_pub: visibility.is_pub || field.op == .arrow
				}
			}
		}
		else {}
	}
}

// collect_after_index runs collection passes 1 and 2 plus the resolution tail
// over the already-built top-level index (shared by the fast file-index path
// and the full-scan fallback in collect()).
fn (mut tc TypeChecker) collect_after_index(a &flat.FlatAst) {
	mut ck_c_sw := time.new_stopwatch()
	if !tc.prepare_collect_declaration_indexes_parallel(a) {
		tc.build_enclosing_generic_param_index(a)
		tc.build_type_declaration_index(a)
		tc.build_fn_declaration_indexes(a)
	}
	if !tc.valid_diagnostic_fast {
		tc.index_multiple_module_import_lines(a)
	}
	// Pass 1: collect type-level names (aliases, enums, sum types)
	for tl_idx in tc.top_level_idx {
		node := a.nodes[tl_idx]
		node_ref := a.node(flat.NodeId(tl_idx))
		tc.register_declaration_visibility(node)
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
				qn := tc.qualify_decl_name(node.value)
				tc.enum_names[qn] = true
				mut fields := []string{}
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
					if f.kind == .enum_field {
						fields << escaped_identifier_name(f.value)
					}
				}
				tc.enum_fields[qn] = fields
				if node.typ == 'flag' {
					tc.flag_enums[qn] = true
				}
			}
			.struct_decl {
				qname := tc.qualify_decl_name(node.value)
				if qname !in tc.structs {
					tc.structs[qname] = []StructField{}
				}
				tc.struct_modules[qname] = tc.cur_module
				tc.struct_files[qname] = tc.cur_file
				if node.generic_params().len > 0 {
					tc.struct_generic_params[qname] = node.generic_params().clone()
					if qname != node.value {
						tc.struct_generic_params[node.value] = node.generic_params().clone()
					}
				}
				implements_types := struct_decl_implements_from_typ(node.typ)
				if implements_types.len > 0 {
					tc.struct_implements[qname] = implements_types
				}
				if comma_attr_text_has(node.typ, 'union') {
					tc.unions[qname] = true
				}
				if comma_attr_text_has(node.typ, 'params') {
					tc.params_structs[qname] = true
				}
				if comma_attr_text_has(node.typ, 'soa') {
					tc.soa_structs[qname] = true
				}
				if comma_attr_text_has(node.typ, 'typedef') {
					tc.c_typedef_structs[qname] = true
					tc.c_typedef_structs[node.value] = true
				}
			}
			.type_decl {
				if node.children_count > 0 {
					mut variants := []string{}
					for i in 0 .. node.children_count {
						v := a.child_node(node_ref, i)
						variants << tc.qualify_sum_variant_name(v.value, node.generic_params())
					}
					qname := tc.qualify_decl_name(node.value)
					tc.sum_types[qname] = variants
					if node.generic_params().len > 0 {
						tc.sum_generic_params[qname] = node.generic_params().clone()
						if qname != node.value {
							tc.sum_generic_params[node.value] = node.generic_params().clone()
						}
					}
				} else if node.typ.len > 0 {
					sum_variant_texts := split_sum_variant_texts(node.typ)
					if sum_variant_texts.len > 1 {
						mut variants := []string{}
						for part in sum_variant_texts {
							variants << tc.qualify_sum_variant_name(part, node.generic_params())
						}
						qname := tc.qualify_decl_name(node.value)
						tc.sum_types[qname] = variants
						if node.generic_params().len > 0 {
							tc.sum_generic_params[qname] = node.generic_params().clone()
							if qname != node.value {
								tc.sum_generic_params[node.value] = node.generic_params().clone()
							}
						}
						continue
					}
					qname := tc.qualify_decl_name(node.value)
					generic_params := node.generic_params()
					alias_target := if generic_params.len > 0 {
						tc.qualify_type_text_with_generic_params(node.typ, generic_params)
					} else {
						tc.qualify_type_text(node.typ)
					}
					tc.type_aliases[qname] = alias_target
					tc.type_alias_modules[qname] = tc.cur_module
					if generic_params.len > 0 {
						tc.type_alias_generic_params[qname] = generic_params.clone()
						if qname != node.value {
							tc.type_alias_generic_params[node.value] = generic_params.clone()
						}
					}
					if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(node.typ) {
						tc.type_alias_c_abi_fns[qname] = c_abi_fn
					}
					if tc.cur_module in ['', 'main', 'builtin'] && node.value !in tc.type_aliases {
						tc.type_aliases[node.value] = alias_target
						tc.type_alias_modules[node.value] = tc.cur_module
						if generic_params.len > 0 {
							tc.type_alias_generic_params[node.value] = generic_params.clone()
						}
						if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(node.typ) {
							tc.type_alias_c_abi_fns[node.value] = c_abi_fn
						}
					}
				}
			}
			.interface_decl {
				qname := tc.qualify_decl_name(node.value)
				tc.interface_names[qname] = true
				if node.generic_params().len > 0 {
					tc.interface_generic_params[qname] = node.generic_params().clone()
					if tc.cur_module in ['', 'main', 'builtin'] && qname != node.value {
						tc.interface_generic_params[node.value] = node.generic_params().clone()
					}
				}
			}
			.c_fn_decl {
				if !tc.c_fn_decl_is_explicit_c(node) && !tc.translated_files[tc.cur_file] {
					qname := tc.qualify_decl_name(node.value)
					if tl_idx >= a.user_code_start {
						tc.source_no_body_fn_suffixes[qname] = true
						tc.source_no_body_fn_suffixes[node.value] = true
						tc.source_no_body_fn_suffixes[node.value.all_after_last('.')] = true
					}
					tc.source_no_body_fns[qname] = true
					if tc.cur_module in ['', 'main'] {
						tc.source_no_body_fns[node.value] = true
					}
				}
			}
			else {}
		}
	}
	tc.check_alias_declaration_cycles()
	tc.cache_fn_generic_params(a)
	// Pass 1 can parse callback aliases before later modules with same-named
	// types have been indexed. Rebuild name-derived caches from the complete
	// declaration table before collecting concrete signatures in pass 2.
	tc.type_cache.clear_c_type_entries()
	tc.invalidate_short_type_name_index()
	tc.check_c_struct_redeclarations(a)
	tc.check_c_fn_redeclarations(a)
	tc.timing_profile('  [ttime]     ck c pass1     ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	ck_c_sw.restart()
	// Pass 2: collect struct fields, function signatures (type aliases now available)
	// The native backend does not yet preserve large aggregate arguments reliably
	// through the parse-cache helpers. Parsing uncached keeps native self-hosts
	// correct while the C-hosted compiler retains the full memoization path.
	$if 'c' == 'arm64' {
		tc.type_cache.parse_enabled = false
	} $else {
		tc.type_cache.parse_enabled = true
	}
	tc.cur_module = ''
	p2_profile := tc.verbose
	mut p2_fn_ns := u64(0)
	mut p2_struct_ns := u64(0)
	mut p2_t0 := u64(0)
	// The parse-heavy per-declaration computation fans out over the worker
	// pool; this serial walk then only replays the order-sensitive table
	// registrations. Empty when the pool is unavailable — then each fn_decl
	// computes inline exactly as before.
	pass2_preps := tc.collect_pass2_fn_preps_parallel()
	fast_pass2_registration := os.getenv('V3_NO_FAST_PASS2_REGISTRATION') == ''
	parallel_pass2_ancillary := fast_pass2_registration && tc.building_v_fast
		&& os.getenv('V3_NO_PAR_PASS2_REGISTRATION') == ''
	if parallel_pass2_ancillary {
		tc.defer_fn_ancillary = true
		tc.fn_ancillary_registrations = []FnAncillaryRegistration{cap: tc.top_level_idx.len * 2}
		tc.fn_c_variadic_registrations = []FnNamePairRegistration{cap: 64}
		tc.fn_mut_receiver_registrations = []FnNamePairRegistration{cap: tc.top_level_idx.len / 2}
		tc.fn_ret_text_registrations = []FnTextRegistration{cap: tc.top_level_idx.len * 2}
		tc.visible_mutation_registrations = []VisibleMutationRegistration{cap: tc.top_level_idx.len}
	}
	for pi, tl_idx in tc.top_level_idx {
		node := a.nodes[tl_idx]
		node_ref := a.node(flat.NodeId(tl_idx))
		if p2_profile {
			p2_t0 = time.sys_mono_now()
		}
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.fn_decl {
				qname := tc.qualify_fn_name(node.value)
				lowered_qname := if fast_pass2_registration {
					tc.cached_c_name(qname)
				} else {
					''
				}
				lowered_source_name := if fast_pass2_registration {
					if node.value == qname {
						lowered_qname
					} else {
						tc.cached_c_name(node.value)
					}
				} else {
					''
				}
				// A parsed source body supersedes a matching declaration imported from
				// a cached header. Pass 1 sees all bodyless declarations first, so clear
				// their marker while collecting concrete source signatures in pass 2.
				tc.source_no_body_fns.delete(qname)
				if tc.cur_module in ['', 'main'] {
					tc.source_no_body_fns.delete(node.value)
				}
				tc.v_fn_semantic_names[qname] = true
				if tc.cur_module in ['', 'main', 'builtin'] {
					tc.v_fn_semantic_names[node.value] = true
				}
				if fast_pass2_registration {
					tc.register_visible_mutation_fn_decl_with_lowered(tl_idx, tc.cur_module, qname, node.value, lowered_qname, lowered_source_name)
				} else {
					tc.register_visible_mutation_fn_decl(tl_idx, tc.cur_module, qname, node.value)
				}
				prep := if pi < pass2_preps.len && pass2_preps[pi].prepared {
					pass2_preps[pi]
				} else {
					tc.compute_pass2_fn_prep(node)
				}
				ret_type := prep.ret_type
				ptypes := prep.ptypes
				param_texts := prep.param_texts
				shared_params := prep.shared_params
				is_variadic := prep.is_variadic
				is_c_variadic := prep.is_c_variadic
				has_mut_receiver := prep.has_mut_receiver
				has_forwardable_ctx := prep.has_forwardable_ctx
				if fast_pass2_registration {
					// The signature arrays are immutable after registration. Keep one
					// master-arena copy shared by the semantic, lowered, legacy, and
					// builtin aliases instead of cloning it independently for each key.
					owned_ptypes := ptypes.clone()
					owned_shared_params := if shared_params.len > 0 {
						shared_params.clone()
					} else {
						shared_params
					}
					tc.register_fn_signature_owned(qname, lowered_qname, ret_type, owned_ptypes, owned_shared_params, is_variadic, has_forwardable_ctx)
					if is_c_variadic {
						tc.register_c_variadic_fn_with_lowered(qname, lowered_qname)
					}
					if has_mut_receiver {
						tc.register_mut_receiver_method_with_lowered(qname, lowered_qname)
					}
					tc.register_fn_ret_type_text(qname, node.typ)
					tc.register_fn_ret_type_text(lowered_qname, node.typ)
					if tc.cur_module in ['', 'main', 'builtin'] && qname != node.value
						&& node.value !in tc.fn_param_types {
						tc.register_fn_signature_owned(node.value, lowered_source_name, ret_type, owned_ptypes, owned_shared_params, is_variadic, has_forwardable_ctx)
						if is_c_variadic {
							tc.register_c_variadic_fn_with_lowered(node.value, lowered_source_name)
						}
						if has_mut_receiver {
							tc.register_mut_receiver_method_with_lowered(node.value, lowered_source_name)
						}
						tc.register_fn_ret_type_text(node.value, node.typ)
						tc.register_fn_ret_type_text(lowered_source_name, node.typ)
					}
				} else {
					tc.register_fn_signature(qname, ret_type, ptypes, shared_params, is_variadic, has_forwardable_ctx)
					if is_c_variadic {
						tc.register_c_variadic_fn(qname)
					}
					if has_mut_receiver {
						tc.register_mut_receiver_method(qname)
					}
					tc.register_fn_ret_type_text(qname, node.typ)
					tc.register_fn_ret_type_text(tc.cached_c_name(qname), node.typ)
					if tc.cur_module in ['', 'main', 'builtin'] && qname != node.value
						&& node.value !in tc.fn_param_types {
						tc.register_fn_signature(node.value, ret_type, ptypes, shared_params, is_variadic, has_forwardable_ctx)
						if is_c_variadic {
							tc.register_c_variadic_fn(node.value)
						}
						if has_mut_receiver {
							tc.register_mut_receiver_method(node.value)
						}
						tc.register_fn_ret_type_text(node.value, node.typ)
						tc.register_fn_ret_type_text(tc.cached_c_name(node.value), node.typ)
					}
				}
				// A generic struct method (`Box[T].clone`) keeps its original signature
				// TEXT: the parsed types collapse a non-concrete `Box[T]` to the bare base,
				// so a concrete call must re-substitute the type arguments from the text to
				// recover applications like the receiver type in the return position
				// (`Box[T]` -> `Box[int]`). Only such methods carry `[` in their key.
				if node.generic_params().len > 0 || node.value.contains('[') {
					for name in [qname, node.value] {
						if name != qname && tc.cur_module !in ['', 'main', 'builtin'] {
							continue
						}
						tc.fn_param_type_texts[name] = param_texts.clone()
						tc.fn_type_files[name] = tc.cur_file
						tc.fn_type_modules[name] = tc.cur_module
						if node.generic_params().len > 0
							&& (name == qname || tc.cur_module in ['', 'main', 'builtin']) {
							tc.fn_generic_params[name] = node.generic_params().clone()
						}
					}
				}
			}
			.struct_decl {
				mut fields := []StructField{}
				mut field_c_abi_fns := map[string]string{}
				mut shared_field_names := []string{}
				mut shared_element_field_names := []string{}
				mut shadows_builtin_error_embed := false
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
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
					mut typ := if node.generic_params().len > 0 {
						tc.parse_scope_param_type(field_typ)
					} else {
						tc.parse_type(field_typ)
					}
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
					if field_typ.trim_space().trim_left('&').trim_space().starts_with('[]shared ') {
						shared_element_field_names << f.value
					}
					fields << StructField{
						name: f.value
						typ: typ
						has_default: f.children_count > 0
						is_embed: field_is_embed
						is_mut: source_field_decl_is_mut(f)
						is_volatile: source_field_decl_is_volatile(f)
					}
				}
				qname := tc.qualify_decl_name(node.value)
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
				for field_name in shared_element_field_names {
					tc.struct_shared_element_fields[struct_field_c_abi_key(qname, field_name)] = true
				}
				for field_name, c_abi_fn in field_c_abi_fns {
					tc.struct_field_c_abi_fns[struct_field_c_abi_key(qname, field_name)] = c_abi_fn
				}
			}
			.c_fn_decl {
				c_name := if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
				tc.register_visible_mutation_fn_decl(tl_idx, tc.cur_module, c_name, c_name)
				ret_type := tc.parse_type(node.typ)
				mut ptypes := []Type{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(node_ref, i)
					if child.kind != .param {
						if tc.prefix_param_scan {
							break
						}
						continue
					}
					if child.typ.starts_with('...') {
						is_variadic = true
					}
					parsed_param_type := tc.parse_type(child.typ)
					ptypes << if child.is_mut {
						mut_param_semantic_type(parsed_param_type)
					} else {
						parsed_param_type
					}
				}
				module_key := c_fn_module_signature_key(tc.cur_module, c_name)
				tc.c_fn_module_ret_types[module_key] = ret_type
				tc.c_fn_module_param_types[module_key] = ptypes.clone()
				tc.c_fn_module_variadic[module_key] = is_variadic
				tc.unsafe_c_fns[module_key] = !node.is_mut
				if node.value !in tc.v_fn_semantic_names {
					tc.register_fn_signature(node.value, ret_type, ptypes, []bool{}, is_variadic, false)
				}
				if is_variadic {
					tc.c_fn_abi_variadic_prefixes[c_name] = if ptypes.len > 0 {
						ptypes.len - 1
					} else {
						0
					}
					tc.register_c_variadic_fn(node.value)
				}
				if !node.value.starts_with('C.') {
					tc.register_fn_signature('C.${node.value}', ret_type, ptypes, []bool{}, is_variadic, false)
					if is_variadic {
						tc.register_c_variadic_fn('C.${node.value}')
					}
				}
			}
			.interface_decl {
				iface_name := tc.qualify_decl_name(node.value)
				iface_generic_params := node.generic_params()
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
					if f.kind != .interface_field {
						continue
					}
					if f.op == .dot {
						mname := '${iface_name}.${f.value}'
						mut absm := tc.interface_abstract_methods[iface_name] or { []string{} }
						absm << f.value
						tc.interface_abstract_methods[iface_name] = absm
						ret_type := if iface_generic_params.len > 0 {
							tc.parse_scope_param_type(f.typ)
						} else {
							tc.parse_type(f.typ)
						}
						mut ptypes := []Type{}
						mut param_texts := []string{}
						mut shared_params := []bool{}
						mut param_mutability := [f.is_mut]
						mut is_variadic := false
						ptypes << Type(Pointer{
							base_type: Type(Interface{
								name: iface_name
							})
						})
						shared_params << false
						for j in 0 .. f.children_count {
							child := a.child_node(f, j)
							if child.kind != .param {
								if tc.prefix_param_scan {
									break
								}
								continue
							}
							if child.typ.starts_with('...') {
								is_variadic = true
							}
							parsed_param_type := if iface_generic_params.len > 0 {
								tc.parse_scope_param_type(child.typ)
							} else {
								tc.parse_type(child.typ)
							}
							ptypes << if child.is_mut {
								mut_param_semantic_type(parsed_param_type)
							} else {
								parsed_param_type
							}
							param_texts << child.typ
							shared_params << param_type_text_is_shared(child.typ)
							param_mutability << child.is_mut
						}
						tc.register_fn_name_alias(mname, ret_type, ptypes, shared_params, is_variadic, false)
						tc.declaration_param_mutability[mname] = param_mutability
						if f.is_mut {
							tc.register_mut_receiver_method(mname)
						}
						if iface_generic_params.len > 0 {
							tc.register_fn_ret_type_text(mname, f.typ)
							tc.fn_param_type_texts[mname] = [iface_name]
							tc.fn_param_type_texts[mname] << param_texts
						}
					} else if f.typ.len > 0 {
						mut fields := tc.interface_fields[iface_name] or { []StructField{} }
						fields << StructField{
							name: f.value
							typ: tc.parse_type(f.typ)
							is_mut: f.is_mut
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
					f := a.child_node(node_ref, i)
					if f.value.len > 0 && f.value.starts_with('C.') {
						tc.c_globals[f.value] = tc.parse_type(f.typ)
					} else if f.value.len > 0 {
						mut ft := tc.parse_type(f.typ)
						if ft is Void && f.children_count > 0 {
							ft = tc.resolve_type(a.child(f, 0))
						}
						qname := tc.qualify_name(f.value)
						tc.file_scope.insert(f.value, ft)
						tc.global_names[f.value] = true
						tc.file_scope.insert(qname, ft)
						tc.global_names[qname] = true
						if param_type_text_is_shared(f.typ) {
							tc.shared_global_names[f.value] = true
							tc.shared_global_names[qname] = true
						}
					}
				}
			}
			.const_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
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
		if p2_profile {
			p2_el := time.sys_mono_now() - p2_t0
			if node.kind == .fn_decl {
				p2_fn_ns += p2_el
			} else if node.kind == .struct_decl {
				p2_struct_ns += p2_el
			}
		}
	}
	if parallel_pass2_ancillary {
		tc.defer_fn_ancillary = false
		tc.finish_pass2_ancillary_registrations()
	}
	tc.collect_deprecated_symbols()
	if p2_profile {
		tc.timing_profile('  [ttime]       p2 fns ${f64(p2_fn_ns) / 1000000.0:.2f} ms, structs ${f64(p2_struct_ns) / 1000000.0:.2f} ms')
	}
	tc.timing_profile('  [ttime]     ck c pass2     ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	ck_c_sw.restart()
	tc.resolve_inferred_global_types(a)
	tc.resolve_const_types()
	tc.build_const_suffixes()
	tc.build_struct_embed_index()
	tc.timing_profile('  [ttime]     ck c resolve   ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	if !isnil(tc.visible_mutation_cache) {
		mut visible_mutation_cache := tc.visible_mutation_cache
		visible_mutation_cache.decl_index_ready = true
	}
	$if ownership ? {
		tc.ownership_after_collect()
	}
}

fn source_field_decl_is_mut(field flat.Node) bool {
	meta := field.generic_params()
	return meta.len > 0 && meta[0].contains('m')
}

fn source_field_decl_is_volatile(field flat.Node) bool {
	meta := field.generic_params()
	return meta.len > 0 && meta[0].contains('v')
}

@[direct_array_access]
fn (mut tc TypeChecker) collect_deprecated_symbols() {
	tc.deprecated_symbols.clear()
	tc.unsafe_fns.clear()
	mut module_name := ''
	mut file_name := ''
	for decl_id in tc.top_level_idx {
		node := tc.a.nodes[decl_id]
		if node.kind == .file {
			module_name = ''
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind == .struct_decl {
			qname := qualify_decl_name_in_module(node.value, module_name)
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .field_decl {
					continue
				}
				meta := field.generic_params()
				if meta.len <= 1 || !meta[0].contains('p') {
					continue
				}
				if info := deprecation_info_from_attrs(field.value, meta[1..]) {
					tc.deprecated_symbols['${qname}.${field.value}'] = info
				}
			}
		}
		// The parser appends a declaration's attribute directive immediately
		// after the declaration node. Use the existing top-level index instead
		// of streaming every AST node to find these sparse records.
		attr_id := decl_id + 1
		if attr_id >= tc.a.nodes.len {
			continue
		}
		attr_node := tc.a.nodes[attr_id]
		if attr_node.kind != .directive || !attr_node.value.starts_with('@attributes:')
			|| attr_node.value['@attributes:'.len..].int() != decl_id {
			continue
		}
		attrs := attr_node.generic_params()
		if node.kind == .fn_decl && 'unsafe' in attrs {
			tc.unsafe_fns[node.value] = true
			tc.unsafe_fns[checker_qualified_fn_name(module_name, node.value)] = true
		}
		if node.kind !in [.fn_decl, .struct_decl, .const_decl] {
			continue
		}
		if node.kind == .const_decl {
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .const_field {
					continue
				}
				qname := qualify_decl_name_in_module(field.value, module_name)
				if info := deprecation_info_from_attrs(qname, attrs) {
					tc.deprecated_symbols[qname] = info
				}
			}
			continue
		}
		display_name := if node.kind == .struct_decl {
			deprecated_struct_display_name(file_name, module_name, node.value)
		} else {
			node.value
		}
		if info := deprecation_info_from_attrs(display_name, attrs) {
			qname := if node.kind == .fn_decl {
				checker_qualified_fn_name(module_name, node.value)
			} else {
				qualify_decl_name_in_module(node.value, module_name)
			}
			tc.deprecated_symbols[qname] = info
			if module_name in ['', 'main', 'builtin'] {
				tc.deprecated_symbols[node.value] = info
			}
		}
	}
}

fn deprecation_info_from_attrs(name string, attrs []string) ?DeprecationInfo {
	mut message := ''
	mut after := ''
	mut deprecated := false
	for raw in attrs {
		attr_name := raw.all_before(':').trim_space()
		value := comptime_static_unquote(raw.all_after(':').trim_space())
		if attr_name == 'deprecated' {
			deprecated = true
			if raw.contains(':') {
				message = value
			}
		} else if attr_name == 'deprecated_after' {
			after = value
		}
	}
	if !deprecated {
		return none
	}
	return DeprecationInfo{
		name: name
		message: message
		after: after
	}
}

fn deprecated_struct_display_name(file_name string, module_name string, struct_name string) string {
	normalized := file_name.replace('\\', '/')
	if relative := normalized.index('/vlib/') {
		dir := os.dir(normalized[relative + '/vlib/'.len..]).replace('/', '.')
		if dir.len > 0 && dir != '.' {
			return '${dir}.${struct_name}'
		}
	}
	return qualify_decl_name_in_module(struct_name, module_name)
}

// rebind_ast updates the AST view after the driver clones transformed storage.
// All collected declaration/type metadata remains valid because cloning preserves node ids.
pub fn (mut tc TypeChecker) rebind_ast(a &flat.FlatAst) {
	tc.a = a
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
					if existing !is Unknown && existing !is Void
						&& !type_contains_unknown(existing)
						&& !generic_semantic_type_has_placeholder(existing)
						&& !tc.type_text_has_generic_placeholder(existing.name()) {
						continue
					}
					initializer_id := a.child(f, 0)
					initializer := a.node(initializer_id)
					mut ft := tc.resolve_type(initializer_id)
					// Function bodies are checked after globals are collected. Infer an
					// implicit generic call here so methods used on the global receiver see
					// its concrete type during body checking (`new_atomic(0)` is a common
					// example).
					if initializer.kind == .call {
						if call_info := tc.resolve_call_info(initializer_id, initializer) {
							specialized := tc.specialized_plain_generic_call_info(initializer, call_info)
							if specialized.return_type !is Unknown
								&& specialized.return_type !is Void
								&& !generic_semantic_type_has_placeholder(specialized.return_type)
								&& !tc.type_text_has_generic_placeholder(specialized.return_type.name()) {
								ft = specialized.return_type
								tc.remember_expr_type(initializer_id, ft)
							}
						}
					}
					if ft is Unknown || ft is Void || type_contains_unknown(ft)
						|| generic_semantic_type_has_placeholder(ft)
						|| tc.type_text_has_generic_placeholder(ft.name()) {
						continue
					}
					tc.file_scope.insert(f.value, ft)
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
						if !tc.c_struct_redeclaration_allowed(qname, existing_file, tc.cur_file, existing_module, tc.cur_module) {
							node_id := flat.NodeId(node_idx)
							keyword := if comma_attr_text_has(node.typ, 'union') {
								'union'
							} else {
								'struct'
							}
							tc.record_error_unfiltered_at(.duplicate_decl, 'cannot redeclare C struct `${qname}`', node_id, tc.declaration_keyword_name_pos(node_id, keyword))
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

struct CFnDeclSignature {
	return_type string
	params      []string
	is_variadic bool
}

fn (mut tc TypeChecker) check_c_fn_redeclarations(a &flat.FlatAst) {
	mut signatures := map[string]CFnDeclSignature{}
	mut modules := map[string]string{}
	mut positions := map[string]string{}
	tc.cur_module = ''
	tc.cur_file = ''
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.c_fn_decl {
				name := if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
				mut params := []string{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind != .param {
						if tc.prefix_param_scan {
							break
						}
						continue
					}
					if child.typ.starts_with('...') {
						is_variadic = true
					} else {
						params << tc.c_extern_abi_type(tc.parse_type(child.typ))
					}
				}
				signature := CFnDeclSignature{
					return_type: tc.c_extern_abi_type(tc.parse_type(node.typ))
					params: params
					is_variadic: is_variadic
				}
				if existing := signatures[name] {
					existing_module := modules[name] or { '' }
					if existing_module == 'builtin' || existing_module == tc.cur_module {
						signatures[name] = signature
						modules[name] = tc.cur_module
						positions[name] = tc.node_position_string(flat.NodeId(node_idx))
					} else if tc.cur_module != 'builtin'
						&& !c_fn_decl_signatures_compatible(existing, signature) {
						existing_pos := positions[name] or { '' }
						location := if existing_pos.len > 0 { ' at ${existing_pos}' } else { '' }
						tc.record_error_unfiltered(.duplicate_decl, 'C function `${name}` was already declared with a different signature in module `${existing_module}`${location}', flat.NodeId(node_idx))
					}
				} else {
					signatures[name] = signature
					modules[name] = tc.cur_module
					positions[name] = tc.node_position_string(flat.NodeId(node_idx))
				}
			}
			else {}
		}
	}
}

fn c_fn_decl_signatures_compatible(a CFnDeclSignature, b CFnDeclSignature) bool {
	if !c_fn_decl_abi_types_compatible(a.return_type, b.return_type) {
		return false
	}
	if a.is_variadic != b.is_variadic {
		variadic := if a.is_variadic { a } else { b }
		fixed := if a.is_variadic { b } else { a }
		if fixed.params.len < variadic.params.len
			|| !c_fn_decl_param_prefix_compatible(fixed.params, variadic.params, variadic.params.len) {
			return false
		}
		for i in variadic.params.len .. fixed.params.len {
			if !c_fn_fixed_param_is_compatible_with_variadic(fixed.params[i]) {
				return false
			}
		}
		return true
	}
	if a.params.len != b.params.len {
		return false
	}
	return c_fn_decl_param_prefix_compatible(a.params, b.params, a.params.len)
}

fn c_fn_fixed_param_is_compatible_with_variadic(typ string) bool {
	// These types are changed by C's default argument promotions. A fixed
	// declaration would therefore not describe the same argument ABI.
	return typ !in ['i8', 'i16', 'u8', 'u16', 'float', 'char', 'bool']
}

fn c_fn_decl_param_prefix_compatible(a []string, b []string, count int) bool {
	if a.len < count || b.len < count {
		return false
	}
	for i in 0 .. count {
		if !c_fn_decl_abi_types_compatible(a[i], b[i]) {
			return false
		}
	}
	return true
}

fn c_fn_decl_abi_types_compatible(a string, b string) bool {
	if a == b || (a in ['int', 'i32'] && b in ['int', 'i32']) {
		return true
	}
	return (a == 'void*' && b.ends_with('*')) || (b == 'void*' && a.ends_with('*'))
}

fn (tc &TypeChecker) c_struct_redeclaration_allowed(qname string, first_file string, second_file string, first_module string, second_module string) bool {
	if qname == 'C.termios' && tc.c_struct_decl_is_vlib_termios_shim(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_termios_shim(second_file, second_module) {
		return true
	}
	if qname == 'C.winsize' && tc.c_struct_decl_is_vlib_winsize_shim(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_winsize_shim(second_file, second_module) {
		return true
	}
	if qname == 'C.cJSON' && tc.c_struct_decl_is_vlib_cjson(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_cjson(second_file, second_module) {
		return true
	}
	return false
}

fn (tc &TypeChecker) c_struct_decl_is_vlib_winsize_shim(file string, module_name string) bool {
	if module_name !in ['term', 'ui', 'term.ui'] {
		return false
	}
	normalized := file.replace('\\', '/')
	return normalized.contains('/vlib/term/')
		|| (normalized.contains('/v3_module_cache_')
			&& normalized.all_after_last('/').starts_with('term_') && normalized.ends_with('.vh'))
}

fn (tc &TypeChecker) c_struct_decl_is_vlib_termios_shim(file string, module_name string) bool {
	if module_name !in ['term', 'termios', 'term.termios'] {
		return false
	}
	normalized := file.replace('\\', '/')
	if normalized.starts_with('vlib/term/') || normalized.contains('/vlib/term/') {
		return true
	}
	if !normalized.contains('/v3_module_cache_') || !normalized.ends_with('.vh') {
		return false
	}
	base := normalized.all_after_last('/')
	return (module_name == 'term' && base.starts_with('term_'))
		|| (module_name in ['termios', 'term.termios'] && base.starts_with('termios_'))
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
	// Attributes such as `@[typedef]` change how the C name is spelled, not the
	// aggregate's field layout. Only the struct/union distinction belongs in the
	// redeclaration signature.
	mut sig := if comma_attr_text_has(node.typ, 'union') { 'union' } else { '' }
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
	// Fixed-array constants have a complete storage type from their outer
	// literal shape. Publish that type before resolving their element values so
	// addressed references such as `&a[0]` can type-check inside `a` itself.
	for name, expr_id in tc.const_exprs {
		tc.cur_module = tc.const_modules[name] or { '' }
		tc.cur_file = tc.const_files[name] or { '' }
		if fixed_type := tc.syntactic_fixed_array_const_type(expr_id) {
			tc.const_types[name] = fixed_type
		}
	}
	for _ in 0 .. tc.const_exprs.len {
		mut changed := false
		for name, expr_id in tc.const_exprs {
			tc.cur_module = tc.const_modules[name] or { '' }
			tc.cur_file = tc.const_files[name] or { '' }
			// Const dependencies are not guaranteed to be visited in declaration order.
			// Re-resolve the initializer on every pass so an array DSL call that was
			// first seen through a still-pending const does not retain `[]unknown`.
			tc.invalidate_const_initializer_type(expr_id)
			mut new_type := tc.resolve_type(expr_id)
			new_type = tc.refine_const_initializer_type(expr_id, new_type)
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

fn (tc &TypeChecker) syntactic_fixed_array_const_type(id flat.NodeId) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind != .postfix || node.op != .not || node.children_count != 1 {
		return none
	}
	literal := tc.a.child_node(node, 0)
	if literal.kind != .array_literal {
		return none
	}
	elem_type := if literal.children_count > 0 {
		tc.syntactic_const_element_type(tc.a.child(literal, 0))?
	} else {
		Type(void_)
	}
	return Type(ArrayFixed{
		elem_type: elem_type
		len: int(literal.children_count)
	})
}

fn (tc &TypeChecker) syntactic_const_element_type(id flat.NodeId) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	return match node.kind {
		.struct_init { tc.parse_type(node.value) }
		.postfix { tc.syntactic_fixed_array_const_type(id)? }
		.int_literal { Type(int_) }
		.float_literal { Type(f64_) }
		.bool_literal { Type(bool_) }
		.char_literal { Type(rune_) }
		.string_literal { Type(string_) }
		else { none }
	}
}

fn (mut tc TypeChecker) invalidate_const_initializer_type(expr_id flat.NodeId) {
	idx := int(expr_id)
	if idx < 0 || idx >= tc.a.nodes.len {
		return
	}
	if idx < tc.expr_type_set.len {
		tc.expr_type_set[idx] = false
	}
	tc.sparse_expr_type_values.delete(idx)
	node := tc.a.nodes[idx]
	for i in 0 .. node.children_count {
		tc.invalidate_const_initializer_type(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) refine_const_initializer_type(expr_id flat.NodeId, typ Type) Type {
	if int(expr_id) >= 0 && int(expr_id) < tc.a.nodes.len {
		expr := tc.a.nodes[int(expr_id)]
		if expr.kind == .call {
			if info0 := tc.resolve_call_info(expr_id, expr) {
				info := tc.specialized_plain_generic_call_info(expr, info0)
				if info.return_type !is Unknown && info.return_type !is Void
					&& !generic_semantic_type_has_placeholder(info.return_type) {
					return info.return_type
				}
			}
		}
	}
	if typ is Array && typ.elem_type is Unknown && int(expr_id) >= 0
		&& int(expr_id) < tc.a.nodes.len {
		expr := tc.a.nodes[int(expr_id)]
		if expr.kind == .call && expr.children_count >= 2 {
			callee := tc.a.child_node(&expr, 0)
			if callee.kind == .selector && callee.value == 'map' {
				elem_type := tc.array_map_return_elem_type(expr)
				if elem_type !is Unknown && elem_type !is Void {
					return Type(Array{
						elem_type: elem_type
					})
				}
			}
		}
	}
	return typ
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
	if expr.kind == .or_expr && expr.children_count > 0 {
		inner_id := tc.a.child(&expr, 0)
		inner_node := tc.a.nodes[int(inner_id)]
		mut inner_type := tc.resolve_type(inner_id)
		if inner_type is Unknown && inner_node.kind == .call {
			inner_type = tc.direct_call_return_type(inner_node) or { inner_type }
		}
		match inner_type {
			OptionType, ResultType {
				if inner_type.base_type !is Unknown {
					return inner_type.base_type
				}
			}
			else {}
		}
	}
	if fn_typ := tc.const_fn_value_type(expr) {
		return fn_typ
	}
	if expr.kind != .call || expr.children_count == 0 {
		return typ
	}
	if ret := tc.direct_call_return_type(expr) {
		return ret
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
	key := tc.const_key_for_name(name)?
	typ := tc.const_types[key] or { return none }
	return tc.const_type_from_initializer(key, typ)
}

fn (tc &TypeChecker) const_key_for_name(name string) ?string {
	qname := tc.qualify_name(name)
	if qname in tc.const_types {
		return qname
	}
	if name in tc.const_types {
		return name
	}
	return none
}

fn (tc &TypeChecker) local_name_conflicts_with_current_module_const(name string) bool {
	key := tc.const_key_for_name(name) or { return false }
	owner := tc.const_modules[key] or { return true }
	current := if tc.cur_module.len == 0 { 'main' } else { tc.cur_module }
	normalized_owner := if owner.len == 0 { 'main' } else { owner }
	return normalized_owner == current
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
	if resolved == 'main' || resolved == tc.cur_module {
		if typ := tc.const_types[node.value] {
			return tc.const_type_from_initializer(node.value, typ)
		}
	}
	if key := tc.const_key_for_suffix(qname) {
		typ := tc.const_types[key] or { unknown_type('unknown const `${key}`') }
		return tc.const_type_from_initializer(key, typ)
	}
	return none
}

fn (tc &TypeChecker) global_type_for_selector(node flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_node := tc.a.child_node(&node, 0)
	if base_node.kind != .ident {
		return none
	}
	resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
	qname := '${resolved}.${node.value}'
	if qname in tc.global_names {
		return tc.file_scope.lookup(qname)
	}
	if resolved == 'main' || resolved == tc.cur_module {
		if node.value in tc.global_names {
			return tc.file_scope.lookup(node.value)
		}
	}
	return none
}

// selector_const_type returns the declared type for a selector const expression.
pub fn (tc &TypeChecker) selector_const_type(node flat.Node) ?Type {
	return tc.const_type_for_selector(node)
}

// selector_value_type returns the declared type for a selector value expression.
pub fn (tc &TypeChecker) selector_value_type(node flat.Node) ?Type {
	return tc.selector_declared_value_type(node)
}

// const_owner_module returns the declaration module for a checker-resolved constant key.
pub fn (tc &TypeChecker) const_owner_module(name string) string {
	return tc.const_modules[name] or { '' }
}

// qualify_fn_name supports qualify fn name handling for TypeChecker.
pub fn (tc &TypeChecker) qualify_fn_name(name string) string {
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name
	}
	return '${tc.cur_module}.${name}'
}

fn (tc &TypeChecker) local_bare_fn_key(name string) ?string {
	if name.len == 0 || name.index_u8(`.`) >= 0 {
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
	if name.len == 0 || name.index_u8(`.`) >= 0 {
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
	lookup_key := '${tc.cur_module}\x01${name}'
	if lookup_key in cache.local_fn_decl_index {
		return true
	}
	// The base chain can be more than one level deep during transform (the
	// scoped-transform overlay sits between forks and the warm check cache).
	mut fallback := cache.base
	for !isnil(fallback) {
		if lookup_key in fallback.local_fn_decl_index {
			return true
		}
		fallback = fallback.base
	}
	// Parallel forks only append transformed expressions to their private AST
	// regions; declarations remain the frozen master's authority. Scanning the
	// artificial shared-region length would index transformed fn_decl copies in
	// every helper and retain tens of megabytes of duplicate keys.
	if !isnil(cache.base) {
		return false
	}
	if cache.local_fn_decl_indexed_len < tc.a.nodes.len {
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
		incremental_transform_scan := tc.top_level_idx_nodes_len > 0
			&& scan_start >= tc.top_level_idx_nodes_len
		for i in scan_start .. tc.a.nodes.len {
			node := tc.a.nodes[i]
			match node.kind {
				.module_decl {
					cur_module = node.value
				}
				.fn_decl {
					if incremental_transform_scan && !local_fn_decl_is_transform_created(node.value) {
						continue
					}
					cache.local_fn_decl_index['${cur_module}\x01${node.value}'] = true
				}
				else {}
			}
		}
		cache.local_fn_decl_last_module = cur_module
		cache.local_fn_decl_indexed_len = tc.a.nodes.len
	}
	return lookup_key in cache.local_fn_decl_index
}

fn local_fn_decl_is_transform_created(name string) bool {
	return name.starts_with('__v3_') || name.starts_with('__anon_fn_')
		|| name.starts_with('__v_top_level_compile_') || name.contains('.__anon_fn_')
		|| (name.contains('[') && name.contains(']'))
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
// QualifyNameCache memoizes qualify_name per source and type-resolution context.
// Its table fingerprint invalidates entries while collection is still growing
// the declared-type tables; every checker fork gets its own instance.
pub struct QualifyNameCache {
pub mut:
	module               string
	file                 string
	resolution_type_mode bool
	fingerprint          int = -1
	entries              map[string]string
	last_name            string
	last_value           string
	last_valid           bool
}

// qualify_table_fingerprint tracks growth of every declared-type table
// qualify_name consults, so the memo stays valid even in phases that can
// still register types (e.g. anonymous structs during fn-body checking).
fn (tc &TypeChecker) qualify_table_fingerprint() int {
	return tc.structs.len + tc.type_aliases.len + tc.interface_names.len + tc.sum_types.len + tc.enum_names.len + tc.flag_enums.len
}

// ownership_time_spent_us reports the accumulated ownership-analysis time for
// the dedicated benchmark stage. Compiled-out ownership (no `-d ownership`)
// cannot spend time, so this returns 0 by construction in plain builds.
pub fn (tc &TypeChecker) ownership_time_spent_us() i64 {
	return tc.ownership_time_ns / 1000
}

pub fn (tc &TypeChecker) qualify_name(name string) string {
	if !isnil(tc.qualify_name_cache) {
		mut cache := tc.qualify_name_cache
		fingerprint := tc.qualify_table_fingerprint()
		if cache.module != tc.cur_module || cache.file != tc.cur_file
			|| cache.resolution_type_mode != tc.resolution_type_mode
			|| cache.fingerprint != fingerprint {
			cache.module = tc.cur_module
			cache.file = tc.cur_file
			cache.resolution_type_mode = tc.resolution_type_mode
			cache.fingerprint = fingerprint
			cache.entries.clear()
			cache.last_valid = false
		}
		if cache.last_valid && cache.last_name.len == name.len
			&& (unsafe { cache.last_name.str == name.str } || cache.last_name == name) {
			return cache.last_value
		}
		if cached := cache.entries[name] {
			cache.last_name = name
			cache.last_value = cached
			cache.last_valid = true
			return cached
		}
		result := tc.qualify_name_uncached(name)
		cache.entries[name] = result
		cache.last_name = name
		cache.last_value = result
		cache.last_valid = true
		return result
	}
	return tc.qualify_name_uncached(name)
}

fn (tc &TypeChecker) qualify_name_uncached(name string) string {
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
	qualified := tc.cur_module + '.' + name
	if qualified in tc.structs || qualified in tc.interface_names || qualified in tc.sum_types
		|| qualified in tc.enum_names || qualified in tc.flag_enums || qualified in tc.type_aliases {
		return qualified
	}
	// A declaration in the active source scope shadows an already-collected
	// unqualified symbol (notably `errors.Error` shadows `builtin.Error`).
	// Keep it module-qualified before the resolution-only generic fallback below.
	if tc.source_declares_type_in_scope(name, tc.cur_file, tc.cur_module) {
		return qualified
	}
	// A concrete generic argument can originate in another module and then be
	// substituted into a generic declaration while that declaration's module is
	// active. Preserve an already-known unqualified symbol (notably a `main`
	// type) instead of incorrectly rebasing it into the generic's module.
	if tc.resolution_type_mode && (name in tc.structs || name in tc.interface_names
		|| name in tc.sum_types || name in tc.enum_names || name in tc.flag_enums
		|| name in tc.type_aliases) {
		return name
	}
	return qualified
}

// qualify_decl_name gives a declaration its stable owner-module name without
// consulting declarations collected earlier in the same pass.
fn (tc &TypeChecker) qualify_decl_name(name string) string {
	if name.contains('.') {
		return tc.resolve_imported_type_text(name)
	}
	if tc.cur_module.len == 0 || tc.cur_module in ['main', 'builtin'] {
		return name
	}
	return '${tc.cur_module}.${name}'
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
	clean := trimmed_space(name)
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return tc.qualify_type_text(clean)
	}
	if generic_params.len == 0 {
		return tc.qualify_name(name)
	}
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
			return clean[..bracket_end + 1] + tc.qualify_sum_variant_name(clean[bracket_end + 1..], generic_params)
		}
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			inner := trimmed_space(clean[bracket + 1..bracket_end])
			if is_fixed_array_len_text(inner) || is_builtin_type_name(clean[..bracket]) {
				return tc.qualify_sum_variant_name(clean[..bracket], generic_params) + clean[bracket..]
			}
			mut parts := []string{}
			for part in split_params(inner) {
				parts << tc.qualify_sum_variant_name(part, generic_params)
			}
			return tc.qualify_name(clean[..bracket]) + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	return tc.qualify_name(clean)
}

// qualify_type_text qualifies a type text for registration: bare names always
// get the current module prefix (order-independent during collect).
fn (tc &TypeChecker) qualify_type_text(typ string) string {
	return tc.qualify_type_text_impl(typ, false, []string{})
}

fn (tc &TypeChecker) qualify_type_text_with_generic_params(typ string, generic_params []string) string {
	return tc.qualify_type_text_impl(typ, false, generic_params)
}

// qualify_resolution_type_text qualifies a type text in a resolution-only
// context (generic application args): a bare name substituted from another
// module's caller may stay bare when the module-qualified spelling does not
// exist. Never use for registration.
fn (tc &TypeChecker) qualify_resolution_type_text(typ string) string {
	return tc.qualify_type_text_impl(typ, true, []string{})
}

// parse_resolution_type parses type text that can mix declaration-local names with concrete
// generic arguments from another module.
pub fn (tc &TypeChecker) parse_resolution_type(typ string) Type {
	clean := trimmed_space(typ)
	// Generic specialization uses `main.Type` as an internal lock for a
	// caller-owned program type. Resolve that lock before qualification strips
	// `main.` and lets a same-named import in the declaration file capture the
	// resulting bare name.
	if clean.starts_with('main.') && !clean['main.'.len..].contains('.') {
		if _ := tc.resolve_import_alias('main') {
		} else if exact := tc.type_from_known_symbol(clean['main.'.len..]) {
			return exact
		}
	}
	qualified := tc.qualify_resolution_type_text(clean)
	if qualified.contains('.') {
		if exact := tc.type_from_known_symbol(qualified) {
			return exact
		}
	}
	if isnil(tc.resolution_type_views) {
		mut direct_view := tc.fork_type_parse_view(tc.cur_file, '')
		direct_view.resolution_type_mode = false
		return direct_view.parse_type(qualified)
	}
	mut views := unsafe { tc.resolution_type_views }
	if cached := views.by_file[tc.cur_file] {
		return cached.parse_type(qualified)
	}
	mut unscoped := tc.fork_type_parse_view(tc.cur_file, '')
	unscoped.resolution_type_mode = false
	view := &unscoped
	views.by_file[tc.cur_file] = view
	return view.parse_type(qualified)
}

// reset_resolution_type_view_cache discards lookup views that may have been
// created inside a completed scoped parallel phase.
pub fn (mut tc TypeChecker) reset_resolution_type_view_cache() {
	tc.resolution_type_views = &ResolutionTypeViewCache{
		by_file: map[string]&TypeChecker{}
	}
}

// disable_resolution_type_view_cache prevents a scoped cache from escaping the
// phase that allocated it.
pub fn (mut tc TypeChecker) disable_resolution_type_view_cache() {
	tc.resolution_type_views = unsafe { nil }
}

fn (tc &TypeChecker) qualify_type_text_impl(typ string, resolution bool, generic_params []string) string {
	clean := trimmed_space(typ)
	if clean.len == 0 {
		return typ
	}
	if clean in generic_params {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + tc.qualify_type_text_impl(clean[1..], resolution, generic_params)
	}
	if clean.starts_with('mut ') {
		inner := tc.qualify_type_text_impl(clean[4..], resolution, generic_params)
		if inner.starts_with('&') {
			return inner
		}
		return '&' + inner
	}
	if clean.starts_with('shared ') {
		return 'shared ' + tc.qualify_type_text_impl(clean[7..], resolution, generic_params)
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + tc.qualify_type_text_impl(clean[7..], resolution, generic_params)
	}
	if clean.starts_with('?') {
		return '?' + tc.qualify_type_text_impl(clean[1..], resolution, generic_params)
	}
	if clean.starts_with('!') {
		return '!' + tc.qualify_type_text_impl(clean[1..], resolution, generic_params)
	}
	if clean.starts_with('...') {
		return '...' + tc.qualify_type_text_impl(clean[3..], resolution, generic_params)
	}
	if clean.starts_with('[]') {
		return '[]' + tc.qualify_type_text_impl(clean[2..], resolution, generic_params)
	}
	if clean == 'chan' {
		return clean
	}
	if clean.starts_with('chan mut ') {
		return 'chan mut ' + tc.qualify_type_text_impl(clean[9..], resolution, generic_params)
	}
	if clean.starts_with('chan ') {
		return 'chan ' + tc.qualify_type_text_impl(clean[5..], resolution, generic_params)
	}
	if clean == 'thread' {
		return clean
	}
	if clean.starts_with('thread ') {
		return 'thread ' + tc.qualify_type_text_impl(clean[7..], resolution, generic_params)
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := tc.qualify_type_text_impl(clean[4..bracket_end], resolution, generic_params)
			val := tc.qualify_type_text_impl(clean[bracket_end + 1..], resolution, generic_params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + tc.qualify_type_text_impl(clean[bracket_end + 1..], resolution, generic_params)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_params(clean[1..clean.len - 1]) {
			parts << tc.qualify_type_text_impl(part, resolution, generic_params)
		}
		return '(' + parts.join(', ') + ')'
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return tc.qualify_fn_type_text(clean, resolution, generic_params)
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			inner := trimmed_space(clean[bracket + 1..bracket_end])
			if is_fixed_array_len_text(inner) || is_builtin_type_name(clean[..bracket]) {
				return tc.qualify_type_text_impl(clean[..bracket], resolution, generic_params) + clean[bracket..]
			}
			mut parts := []string{}
			for part in split_params(inner) {
				parts << tc.qualify_type_text_impl(part, resolution, generic_params)
			}
			return tc.qualify_type_text_impl(clean[..bracket], resolution, generic_params) + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	if resolution && clean.contains('.') {
		resolved := tc.resolve_imported_type_text(clean)
		if resolved != clean {
			return resolved
		}
		if tc.qualify_candidate_type_exists(clean) {
			return clean
		}
	}
	if !clean.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(clean) {
			return resolved
		}
		if candidates := tc.selective_import_candidates(clean) {
			if candidates.len == 1 {
				return candidates[0]
			}
		}
	}
	if resolution {
		return tc.qualify_resolution_type_name(clean)
	}
	return tc.qualify_name(clean)
}

// qualify_fn_type_text supports qualify fn type text handling for TypeChecker.
fn (tc &TypeChecker) qualify_fn_type_text(typ string, resolution bool, generic_params []string) string {
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
	if trimmed_space(params_str).len > 0 {
		for part in split_params(params_str) {
			clean_part := trimmed_space(part)
			is_mut := clean_part.starts_with('mut ')
			param_text := if is_mut { trimmed_space(clean_part[4..]) } else { clean_part }
			qualified := tc.qualify_type_text_impl(normalize_fn_type_param_text(param_text), resolution, generic_params)
			params << if is_mut { 'mut ${qualified}' } else { qualified }
		}
	}
	ret_str := trimmed_space(typ[params_end + 1..])
	if ret_str.len > 0 {
		return 'fn(${params.join(', ')}) ${tc.qualify_type_text_impl(ret_str, resolution, generic_params)}'
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
		imports: map[string]string{}
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

fn (mut tc TypeChecker) check_import_diagnostics() {
	mut first_imports := map[string]token.Pos{}
	mut declaration_seen_in_file := false
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			declaration_seen_in_file = false
			continue
		}
		if node.kind == .module_decl {
			tc.enter_module(node.value)
			continue
		}
		if node.kind != .import_decl {
			if node.kind != .empty {
				declaration_seen_in_file = true
			}
			continue
		}
		module_path := tc.import_module_path_text(node)
		module_base := module_path.all_after_last('.')
		explicit_alias := tc.import_has_explicit_alias(node)
		if missing_path := tc.a.missing_imports[idx] {
			tc.record_error_severity_at(.unknown_ident, 'cannot import module "${missing_path}" (not found)', flat.NodeId(idx), node.pos, 'builder error:')
		}
		tc.check_import_source_syntax(flat.NodeId(idx), node)
		if tc.selective_import_has_missing_value_symbol(node, module_path)
			|| tc.selective_import_has_const(node, module_path) {
			tc.record_unused_import_warning(flat.NodeId(idx), node)
		}
		tc.check_selective_const_imports(node, module_path)
		tc.check_selective_type_imports(node, module_path)
		if declaration_seen_in_file {
			tc.record_error_at(.duplicate_decl, '`import x` can only be declared at the beginning of the file', flat.NodeId(idx), token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 'import'.len))
		}
		if explicit_alias && node.typ == module_base {
			tc.record_error_at(.duplicate_decl, 'import alias `${module_path} as ${node.typ}` is redundant', flat.NodeId(idx), tc.import_alias_pos(node))
		}
		if module_base == tc.cur_module {
			tc.record_error_at(.duplicate_decl, 'cannot import `${module_path}` into a module with the same name', flat.NodeId(idx), tc.import_module_path_pos(node))
		}
		if node.typ == tc.cur_module {
			alias_pos := if explicit_alias {
				tc.import_alias_pos(node)
			} else {
				tc.import_module_basename_pos(node)
			}
			tc.record_error_at(.duplicate_decl, 'cannot import `${module_path}` as `${node.typ}` into a module with the same name', flat.NodeId(idx), alias_pos)
		}
		if module_path == 'json' && module_base != tc.cur_module {
			tc.record_warning_at(.duplicate_decl, 'module `json` has been deprecated; `json` will be removed soon; use the pure V `json2` module instead', flat.NodeId(idx), node.pos)
		}
		key := file_import_key(tc.cur_file, node.typ)
		if first_pos := first_imports[key] {
			first_line := tc.import_line_number(first_pos)
			pos := tc.import_module_path_pos(node)
			tc.record_error_at(.duplicate_decl, 'A module `${node.typ}` was already imported on line ${first_line}`.', flat.NodeId(idx), pos)
		} else {
			first_imports[key] = node.pos
		}
	}
}

fn (tc &TypeChecker) selective_import_has_const(node flat.Node, module_path string) bool {
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .ident || child.value.len == 0 {
			continue
		}
		for module_name in [node.value, module_path, module_path.all_after_last('.')] {
			if '${module_name}.${child.value}' in tc.const_types {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) selective_import_has_missing_value_symbol(node flat.Node, module_path string) bool {
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .ident || child.value.len == 0 || child.value[0].is_capital() {
			continue
		}
		mut symbol_exists := false
		for module_name in [node.value, module_path, module_path.all_after_last('.')] {
			name := '${module_name}.${child.value}'
			if tc.fn_signature_known(name) || name in tc.fn_ret_types || name in tc.const_types {
				symbol_exists = true
				break
			}
		}
		if !symbol_exists {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) private_declaration(name string) ?DeclarationVisibility {
	if name.len == 0 || is_regular_v_test_file(tc.cur_file) {
		return none
	}
	mut candidates := []string{}
	for candidate in [name, visible_mutation_fn_lookup_name(name)] {
		if candidate.len > 0 && candidate !in candidates {
			candidates << candidate
		}
	}
	mut shortened := name
	for shortened.contains('.') {
		tail := shortened.all_after('.')
		if !tail.contains('.') {
			break
		}
		shortened = tail
		for candidate in [shortened, visible_mutation_fn_lookup_name(shortened)] {
			if candidate.len > 0 && candidate !in candidates {
				candidates << candidate
			}
		}
	}
	for candidate in candidates {
		visibility := tc.declaration_visibility[candidate] or { continue }
		same_main_module := visibility.module_name in ['', 'main'] && tc.cur_module in ['', 'main']
		if !visibility.is_pub && visibility.module_name != tc.cur_module && !same_main_module {
			return visibility
		}
		return none
	}
	return none
}

fn (mut tc TypeChecker) check_selective_const_imports(node flat.Node, module_path string) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind != .ident || child.value.len == 0 {
			continue
		}
		mut is_const := false
		for module_name in [node.value, module_path, module_path.all_after_last('.')] {
			if '${module_name}.${child.value}' in tc.const_types {
				is_const = true
				break
			}
		}
		if is_const {
			tc.record_error_at(.duplicate_decl, 'cannot selectively import constant `${child.value}` from `${module_path}`, import `${module_path}` and use `${module_path}.${child.value}` instead', child_id, tc.node_value_diagnostic_pos(child_id))
			continue
		}
		if child.value[0].is_capital() {
			continue
		}
		mut symbol_name := ''
		for module_name in [node.value, module_path, module_path.all_after_last('.')] {
			name := '${module_name}.${child.value}'
			if tc.fn_signature_known(name) || name in tc.fn_ret_types {
				symbol_name = name
				break
			}
		}
		if symbol_name.len == 0 {
			tc.record_error_at(.unknown_fn, 'module `${module_path}` has no constant or function `${child.value}`', child_id, tc.node_value_diagnostic_pos(child_id))
		} else if _ := tc.private_declaration(symbol_name) {
			tc.record_error_at(.unknown_fn, 'module `${module_path}` function `${child.value}()` is private', child_id, tc.node_value_diagnostic_pos(child_id))
		}
	}
}

fn (mut tc TypeChecker) check_selective_type_imports(node flat.Node, module_path string) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind != .ident || child.value.len == 0 || !child.value[0].is_capital() {
			continue
		}
		mut symbol_name := ''
		for module_name in [node.value, module_path, module_path.all_after_last('.')] {
			name := '${module_name}.${child.value}'
			if tc.type_symbol_known(name) {
				symbol_name = name
				break
			}
		}
		if symbol_name.len == 0 {
			tc.record_error_at(.unknown_type, 'module `${module_path}` has no type `${child.value}`', child_id, tc.node_value_diagnostic_pos(child_id))
		} else if _ := tc.private_declaration(symbol_name) {
			tc.record_error_at(.unknown_type, 'module `${module_path}` type `${child.value}` is private', child_id, tc.node_value_diagnostic_pos(child_id))
		}
	}
}

fn (mut tc TypeChecker) check_unused_import_diagnostics() {
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			continue
		}
		if node.kind == .module_decl {
			tc.enter_module(node.value)
			continue
		}
		if tc.diagnostic_files.len > 0 && tc.cur_file !in tc.diagnostic_files {
			continue
		}
		if node.kind != .import_decl || !node.pos.is_valid() || node.typ == '_'
			|| tc.import_is_used(flat.NodeId(idx), node) {
			continue
		}
		if tc.errors.any(it.file == tc.cur_file && it.kind == .unknown_ident
			&& it.msg.starts_with('undefined variable') && it.node_value == node.typ) {
			continue
		}
		if tc.node_has_unused_import_warning(flat.NodeId(idx)) {
			continue
		}
		if tc.node_has_error(flat.NodeId(idx)) {
			continue
		}
		tc.record_unused_import_warning(flat.NodeId(idx), node)
	}
}

fn (mut tc TypeChecker) record_unused_import_warning(id flat.NodeId, node flat.Node) {
	module_path := tc.import_module_path_text(node)
	display_name := if node.typ == module_path {
		module_path
	} else {
		'${node.typ} (${module_path})'
	}
	tc.record_warning_at(.unknown_ident, "module '${display_name}' is imported but never used. Use `import ${display_name} as _`, to silence this warning, or just remove the unused import line", id, tc.import_module_path_pos(node))
}

fn (tc &TypeChecker) node_has_unused_import_warning(id flat.NodeId) bool {
	for warning in tc.notices {
		if warning.node == id && warning.msg.contains('is imported but never used') {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) node_has_error(id flat.NodeId) bool {
	for err in tc.errors {
		if err.node == id && err.severity != 'builder error:' {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_import_source_syntax(id flat.NodeId, node flat.Node) {
	file := tc.a.source_files[node.pos.id] or { return }
	source := tc.source_texts_by_file[file.name] or { return }
	start := node.pos.offset
	if start < 0 || start + 'import'.len > source.len {
		return
	}
	first_line_end := source.index_after('\n', start) or { source.len }
	mut cursor := start + 'import'.len
	for cursor < first_line_end && source[cursor] in [` `, `\t`, `\r`] {
		cursor++
	}
	if cursor >= first_line_end || source[cursor] == `\n` {
		mut next := first_line_end
		for next < source.len && source[next] in [` `, `\t`, `\r`, `\n`] {
			next++
		}
		end := import_source_ident_end(source, next)
		if end > next {
			tc.record_error_at(.duplicate_decl, '`import` statements must be a single line', id, token.new_span(node.pos.id, next, end))
		}
		return
	}
	module_start := cursor
	for cursor < first_line_end && (is_import_ident_byte(source[cursor]) || source[cursor] == `.`) {
		cursor++
	}
	if cursor == module_start {
		return
	}
	for cursor < first_line_end && source[cursor] in [` `, `\t`, `\r`] {
		cursor++
	}
	if cursor >= first_line_end || source[cursor] == `\n` {
		return
	}
	if source[cursor] == `,` {
		tc.record_error_at(.duplicate_decl, 'cannot import multiple modules at a time', id, token.new_span(node.pos.id, cursor, cursor + 1))
		return
	}
	if source[cursor] == `{` {
		tc.check_selective_import_source_syntax(id, node, source, cursor, first_line_end)
		return
	}
	if source[cursor..first_line_end].starts_with('as ') {
		return
	}
	if is_import_ident_byte(source[cursor]) {
		end := import_source_ident_end(source, cursor)
		tc.record_error_at(.duplicate_decl, 'cannot import multiple modules at a time', id, token.new_span(node.pos.id, cursor, end))
	}
}

fn (mut tc TypeChecker) check_selective_import_source_syntax(id flat.NodeId, node flat.Node, source string, open int, line_end int) {
	mut cursor := open + 1
	for cursor < line_end && source[cursor] in [` `, `\t`, `\r`] {
		cursor++
	}
	if cursor < line_end && source[cursor] == `}` {
		tc.record_error_at(.duplicate_decl, 'empty `${tc.import_module_path_text(node)}` import set, remove `{}`', id, token.new_span(node.pos.id, cursor, cursor + 1))
		return
	}
	mut close := -1
	for i := cursor; i < line_end; i++ {
		if source[i] == `}` {
			close = i
			break
		}
	}
	if close < 0 {
		mut diagnostic_offset := line_end - 1
		for diagnostic_offset > open && source[diagnostic_offset] in [` `, `\t`, `\r`, `\n`] {
			diagnostic_offset--
		}
		tc.record_error_at(.duplicate_decl, 'import syntax error, no closing `}`', id, token.new_span(node.pos.id, diagnostic_offset, diagnostic_offset + 1))
		return
	}
	if cursor < close && !is_import_ident_byte(source[cursor]) {
		tc.record_error_at(.duplicate_decl, 'import syntax error, please specify a valid fn or type name', id, token.new_span(node.pos.id, cursor, cursor + 1))
	}
}

fn import_source_ident_end(source string, start int) int {
	mut end := start
	for end < source.len && is_import_ident_byte(source[end]) {
		end++
	}
	return end
}

fn is_import_ident_byte(ch u8) bool {
	return ch.is_alnum() || ch == `_`
}

fn (tc &TypeChecker) import_is_used(import_id flat.NodeId, import_node flat.Node) bool {
	mut selective_names := []string{cap: int(import_node.children_count)}
	import_file := if file := tc.a.source_files[import_node.pos.id] {
		file.name
	} else {
		tc.cur_file
	}
	for i in 0 .. import_node.children_count {
		child := tc.a.child_node(&import_node, i)
		if child.kind == .ident {
			selective_names << child.value
		}
	}
	for idx, node in tc.a.nodes {
		if idx == int(import_id) || node.kind == .import_decl || node.pos.id != import_node.pos.id {
			continue
		}
		if node.kind == .selector && node.children_count > 0 {
			base := tc.a.child_node(&node, 0)
			if base.kind == .ident && base.value == import_node.typ {
				return true
			}
		}
		if type_text_contains_qualified_import(node.typ, import_node.typ)
			|| type_text_contains_qualified_import(node.value, import_node.typ) {
			return true
		}
		if selective_names.len == 0 {
			continue
		}
		if node.kind == .ident && node.value in selective_names {
			return true
		}
		if node.kind == .call {
			if resolved := tc.resolved_call_name(flat.NodeId(idx)) {
				for name in selective_names {
					if resolved == '${import_node.value}.${name}'
						|| resolved.starts_with('${import_node.value}.${name}[') {
						return true
					}
				}
			}
		}
		for name in selective_names {
			if !type_text_contains_symbol(node.typ, name)
				&& !type_text_contains_symbol(node.value, name) {
				continue
			}
			if resolved := tc.resolve_selective_import_type_symbol_in_file(name, import_file) {
				if resolved == '${import_node.value}.${name}' {
					return true
				}
			}
		}
	}
	return false
}

fn type_text_contains_qualified_import(text string, alias string) bool {
	if text.len <= alias.len || alias.len == 0 {
		return false
	}
	mut start := 0
	needle := alias + '.'
	for start < text.len {
		relative := text[start..].index(needle) or { return false }
		index := start + relative
		if index == 0 || !is_type_symbol_byte(text[index - 1]) {
			return true
		}
		start = index + needle.len
	}
	return false
}

fn (mut tc TypeChecker) check_deprecated_byte_types() {
	mut identifier_offsets := map[u64]bool{}
	for node in tc.a.nodes {
		if node.kind == .ident && node.value == 'byte' && node.pos.is_valid() {
			identifier_offsets[deprecated_byte_position_key(node.pos.id, node.pos.offset)] = true
		}
	}
	mut pending_file := ''
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			pending_file = node.value
			tc.enter_file(node.value)
			continue
		}
		if pending_file.len == 0 || !node.pos.is_valid() {
			continue
		}
		tc.check_deprecated_byte_types_in_file(flat.NodeId(idx), node.pos.id, pending_file, identifier_offsets)
		pending_file = ''
	}
}

fn deprecated_byte_position_key(file_id int, offset int) u64 {
	return (u64(u32(file_id)) << 32) | u64(u32(offset))
}

fn (mut tc TypeChecker) check_deprecated_byte_types_in_file(anchor flat.NodeId, file_id int, path string, identifier_offsets map[u64]bool) {
	if tc.diagnostic_files.len > 0 && path !in tc.diagnostic_files {
		return
	}
	source := os.read_file(path) or { return }
	mut i := 0
	for i < source.len {
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
			i = source.index_after('\n', i + 2) or { source.len }
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
			end := source.index_after('*/', i + 2) or { source.len }
			i = int_min(end + 2, source.len)
			continue
		}
		if source[i] in [`'`, `"`, `\``] {
			quote := source[i]
			i++
			for i < source.len {
				if source[i] == `\\` {
					i += 2
					continue
				}
				if source[i] == quote {
					i++
					break
				}
				i++
			}
			continue
		}
		if !source[i].is_letter() && source[i] != `_` {
			i++
			continue
		}
		start := i
		for i < source.len && (source[i].is_alnum() || source[i] == `_`) {
			i++
		}
		if source[start..i] != 'byte' || deprecated_byte_is_alias_base(source, start)
			|| deprecated_byte_position_key(file_id, start) in identifier_offsets {
			continue
		}
		mut end := i
		mut next := i
		for next < source.len && source[next] in [` `, `\t`] {
			next++
		}
		if next < source.len && source[next] == `(` {
			if close := matching_source_paren(source, next) {
				end = close + 1
			}
		}
		tc.errors << tc.make_type_error_at(.unknown_type, 'byte is deprecated, use u8 instead', anchor, token.new_span(file_id, start, end))
	}
}

fn (tc &TypeChecker) deprecated_byte_is_value_ident(file_id int, offset int) bool {
	for node in tc.a.nodes {
		if node.kind == .ident && node.value == 'byte' && node.pos.id == file_id
			&& node.pos.offset == offset {
			return true
		}
	}
	return false
}

fn deprecated_byte_is_alias_base(source string, offset int) bool {
	line_start := if offset > 0 {
		if idx := source[..offset].last_index('\n') { idx + 1 } else { 0 }
	} else {
		0
	}
	prefix := source[line_start..offset].trim_space()
	return prefix.starts_with('type ') && prefix.contains('=')
}

fn matching_source_paren(source string, open int) ?int {
	mut depth := 0
	for i := open; i < source.len; i++ {
		if source[i] == `(` {
			depth++
		} else if source[i] == `)` {
			depth--
			if depth == 0 {
				return i
			}
		} else if source[i] == `\n` && depth == 1 {
			return none
		}
	}
	return none
}

fn (tc &TypeChecker) import_module_path_pos(node flat.Node) token.Pos {
	start := node.pos.offset + 'import '.len
	file := tc.a.source_files[node.pos.id] or {
		return token.new_span(node.pos.id, start, start + node.value.len)
	}
	source := tc.source_texts_by_file[file.name] or {
		return token.new_span(node.pos.id, start, start + node.value.len)
	}
	end_limit := int_min(node.pos.end, source.len)
	if start < 0 || start >= end_limit {
		return node.pos
	}
	import_text := source[start..end_limit]
	end := if relative := import_text.index(' as ') {
		start + relative
	} else if relative := import_text.index(' {') {
		start + relative
	} else {
		end_limit
	}
	return token.new_span(node.pos.id, start, end)
}

fn (tc &TypeChecker) import_module_path_text(node flat.Node) string {
	pos := tc.import_module_path_pos(node)
	file := tc.a.source_files[pos.id] or { return node.value }
	source := tc.source_texts_by_file[file.name] or { return node.value }
	if pos.offset < 0 || pos.end > source.len || pos.end <= pos.offset {
		return node.value
	}
	return source[pos.offset..pos.end].trim_space()
}

fn (tc &TypeChecker) import_has_explicit_alias(node flat.Node) bool {
	file := tc.a.source_files[node.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	return end > start && source[start..end].contains(' as ')
}

fn (tc &TypeChecker) import_alias_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if end <= start {
		return node.pos
	}
	text := source[start..end]
	as_relative := text.index(' as ') or { return node.pos }
	alias_start := start + as_relative + ' as '.len
	return token.new_span(node.pos.id, alias_start, alias_start + node.typ.len)
}

fn (tc &TypeChecker) import_module_basename_pos(node flat.Node) token.Pos {
	path_pos := tc.import_module_path_pos(node)
	base_len := tc.import_module_path_text(node).all_after_last('.').len
	return token.new_span(path_pos.id, path_pos.end - base_len, path_pos.end)
}

fn (tc &TypeChecker) import_line_number(pos token.Pos) int {
	file := tc.a.source_files[pos.id] or { return 1 }
	source := tc.source_texts_by_file[file.name] or { return 1 }
	end := int_min(int_max(pos.offset, 0), source.len)
	mut line := 1
	for ch in source[..end] {
		if ch == `\n` {
			line++
		}
	}
	return line
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
		// A file's module is fixed by its own `module` declaration, which is the first
		// node after the file marker. Later passes (annotate/monomorphize) walk a node
		// array where appended, specialized nodes can place a foreign module's
		// `module_decl` after this file's marker without an intervening file marker of
		// their own; without this guard that would rebind `cur_file` to the foreign
		// module and mis-resolve same-named types (`main.Context` -> `veb.Context`).
		if existing := tc.file_modules[tc.cur_file] {
			if existing != name {
				return
			}
		}
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
			tc.record_error_unfiltered(.unknown_fn, 'ambiguous selective import `${child.value}`', child_id)
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

// check_memos_enabled gates the per-fork qualify/import memos, so a single
// binary can A/B them (`V3_NO_CHECK_MEMOS=1` disables).
fn check_memos_enabled() bool {
	return os.getenv('V3_NO_CHECK_MEMOS') == ''
}

// ImportInfoCache pins the current file's import table so the hot
// resolve_import_alias/selective-import paths skip re-hashing the long
// file-path key on every call. Refreshed whenever the file or the registration
// table length changes; the pinned &FileImportInfo is heap-stable and its maps
// stay live through the pin. Each checker fork owns a private instance.
struct ImportInfoCache {
mut:
	file     string
	info     &FileImportInfo = unsafe { nil }
	seen_len int = -1
}

fn (tc &TypeChecker) current_file_import_info() &FileImportInfo {
	mut cache := tc.import_info_cache
	if isnil(cache) {
		return tc.file_imports_by_file[tc.cur_file] or { return unsafe { &FileImportInfo(nil) } }
	}
	if cache.seen_len != tc.file_imports_by_file.len
		|| voidptr(cache.file.str) != voidptr(tc.cur_file.str) || cache.file.len != tc.cur_file.len {
		cache.file = tc.cur_file
		cache.seen_len = tc.file_imports_by_file.len
		cache.info = tc.file_imports_by_file[tc.cur_file] or { unsafe { &FileImportInfo(nil) } }
	}
	return cache.info
}

// resolve_import_alias resolves resolve import alias information for types.
fn (tc &TypeChecker) resolve_import_alias(alias string) ?string {
	info := tc.current_file_import_info()
	if isnil(info) {
		return none
	}
	if mod := info.imports[alias] {
		return mod
	}
	return none
}

fn (tc &TypeChecker) resolve_selective_import_symbol(name string) ?string {
	candidates := tc.selective_import_candidates(name) or {
		// Generic specializations can be checked while their cloned file context is
		// active. Recover a selected symbol from the source file only when it has one
		// unambiguous declaration across the registered imports.
		if !tc.resolution_type_mode {
			return none
		}
		mut resolved := ''
		suffix := '\n${name}'
		for key, fallback_candidates in tc.file_selective_imports {
			if !key.ends_with(suffix) {
				continue
			}
			for candidate in fallback_candidates {
				if !tc.fn_signature_known(candidate) && candidate !in tc.fn_ret_types && candidate !in tc.fn_param_types {
					continue
				}
				if resolved.len > 0 && resolved != candidate {
					return none
				}
				resolved = candidate
			}
		}
		if resolved.len > 0 {
			return resolved
		}
		return none
	}
	for candidate in candidates {
		if tc.fn_signature_known(candidate) || candidate in tc.fn_ret_types
			|| candidate in tc.fn_param_types {
			return candidate
		}
	}
	return none
}

// resolve_any_selective_import_fn resolves an unqualified selected function
// when every source file that selects the name agrees on the same declaration.
pub fn (tc &TypeChecker) resolve_any_selective_import_fn(name string) ?string {
	mut resolved := ''
	suffix := '\n${name}'
	for key, candidates in tc.file_selective_imports {
		if !key.ends_with(suffix) {
			continue
		}
		for candidate in candidates {
			if !tc.fn_signature_known(candidate) && candidate !in tc.fn_ret_types && candidate !in tc.fn_param_types {
				continue
			}
			if resolved.len > 0 && resolved != candidate {
				return none
			}
			resolved = candidate
		}
	}
	if resolved.len > 0 {
		return resolved
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
	info := tc.current_file_import_info()
	if isnil(info) {
		return none
	}
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
	// Builtin enum declarations use their unqualified source name, just like
	// builtin structs. Keep that name visible from imported modules unless a
	// declaration in the active scope shadows it.
	return name == 'IError' || name in tc.enum_names || name in tc.flag_enums
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
		return tc.parse_alias_type(name, tc.type_aliases[name])
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
			name: name
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
	// `main.Foo` is an explicit reference to a program-module type (bare-keyed),
	// not an import. Strip the prefix so an explicit generic argument locked to
	// `main` (see explicit_generic_concrete_arg_text) resolves to that program
	// type rather than staying a distinct `main.Foo` spelling or rebasing into
	// the active module. Only applies when `main` is not itself an import alias.
	if alias == 'main' {
		if _ := tc.resolve_import_alias('main') {
		} else {
			rest := typ[dot + 1..]
			// Transform qualifies main-module const/global references to
			// `main.<name>` for C codegen; the post-transform re-check must still
			// resolve them against the bare-keyed program symbol tables.
			if !rest.contains('.') {
				if is_builtin_type_name(rest) || tc.qualify_candidate_type_exists(rest)
					|| rest in tc.const_types {
					return rest
				}
				if _ := tc.file_scope.lookup(rest) {
					return rest
				}
			}
		}
	}
	if resolved := tc.resolve_import_alias(alias) {
		if resolved != alias {
			return resolved + typ[dot..]
		}
	}
	return typ
}

// imported_type_short_name returns the semantic short name for an active imported
// module prefix. Some legacy module collections intentionally retain short symbols;
// qualified source spelling still has to resolve to those symbols.
fn (tc &TypeChecker) imported_type_short_name(typ string) ?string {
	if !typ.contains('.') || typ.starts_with('C.') {
		return none
	}
	dot := typ.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	if _ := tc.resolve_import_alias(typ[..dot]) {
		return typ.all_after_last('.')
	}
	return none
}

// has_active_import reports whether has active import applies in types.
fn (tc &TypeChecker) has_active_import(alias string) bool {
	info := tc.file_imports_by_file[tc.cur_file] or { return false }
	return alias in info.imports
}

const receiver_method_suffix_ambiguous = '__v_receiver_method_suffix_ambiguous__'

struct FnAncillaryRegistration {
	name             string
	ret_type         Type
	shared_params    []bool
	is_variadic      bool
	implicit_veb_ctx bool
	file             string
	write_file       bool
}

struct FnNamePairRegistration {
	name         string
	lowered_name string
}

struct FnTextRegistration {
	name string
	text string
}

struct VisibleMutationRegistration {
	idx           int
	module_name   string
	qname         string
	source_name   string
	c_qname       string
	c_source_name string
}

fn (mut tc TypeChecker) register_mut_receiver_method(name string) {
	lowered_name := tc.cached_c_name(name)
	tc.register_mut_receiver_method_with_lowered(name, lowered_name)
}

fn (mut tc TypeChecker) register_mut_receiver_method_with_lowered(name string, lowered_name string) {
	if tc.defer_fn_ancillary {
		tc.fn_mut_receiver_registrations << FnNamePairRegistration{
			name: name
			lowered_name: lowered_name
		}
		return
	}
	tc.mut_receiver_methods[name] = true
	if lowered_name != name {
		tc.mut_receiver_methods[lowered_name] = true
	}
}

// register_fn_signature_owned installs aliases that may share immutable
// master-owned signature arrays. lowered_name must be cached_c_name(name).
fn (mut tc TypeChecker) register_fn_signature_owned(name string, lowered_name string, ret_type Type, params []Type, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	tc.register_fn_name_alias_owned(name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
	if lowered_name != name && !(name.starts_with('C.') && lowered_name in tc.v_fn_semantic_names) {
		tc.register_fn_name_alias_owned(lowered_name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
	}
	if name.ends_with('.str') {
		receiver := name.all_before_last('.')
		legacy_name := '${receiver}_str'
		if !legacy_name.contains('.') {
			tc.register_fn_name_alias_owned(legacy_name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
		}
	}
}

// register_fn_signature updates register fn signature state for types.
fn (mut tc TypeChecker) register_fn_signature(name string, ret_type Type, params []Type, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	tc.register_fn_name_alias(name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
	lowered_name := tc.cached_c_name(name)
	if lowered_name != name && !(name.starts_with('C.') && lowered_name in tc.v_fn_semantic_names) {
		tc.register_fn_name_alias(lowered_name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
	}
	if name.ends_with('.str') {
		receiver := name.all_before_last('.')
		legacy_name := '${receiver}_str'
		if !legacy_name.contains('.') {
			tc.register_fn_name_alias(legacy_name, ret_type, params, shared_params, is_variadic, implicit_veb_ctx)
		}
	}
}

// register_fn_name_alias updates register fn name alias state for types.
fn (mut tc TypeChecker) register_fn_name_alias(name string, ret_type Type, params []Type, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	owned_params := params.clone()
	owned_shared_params := if shared_params.len > 0 {
		shared_params.clone()
	} else {
		shared_params
	}
	tc.register_fn_name_alias_owned(name, ret_type, owned_params, owned_shared_params, is_variadic, implicit_veb_ctx)
}

fn (mut tc TypeChecker) register_fn_name_alias_owned(name string, ret_type Type, params []Type, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	// C externs live in one global namespace and modules routinely redeclare
	// them with refined types (builtin: `C.pthread_join(thread voidptr, ...)`,
	// v3.workers: `C.pthread_join(thread C.pthread_t, ...)`). The
	// first-registration guard below must not drop those refinements: cgen's
	// module-blind parameter lookup would then see `voidptr` and take the
	// address of struct-typed handles (gen-2 compilers panicked with
	// `failed to join compiler worker 0`).
	if !name.starts_with('C.') {
		if owner_module := tc.fn_type_modules[name] {
			if owner_module != tc.cur_module && tc.cur_module !in ['', 'main', 'builtin'] && !name.starts_with('${tc.cur_module}.') {
				return
			}
		}
	}
	tc.fn_param_types[name] = params
	mut write_file := false
	if tc.cur_file.len > 0 {
		// A refined C-extern redeclaration updates the signature tables above
		// but must not steal ownership: is_builtin_unsafe_c_call keys the
		// unsafe-block requirement for the C.m*/C.s* memory externs off the
		// builtin owner, so a module redeclaring C.malloc (json does) would
		// otherwise suppress that diagnostic program-wide.
		if !name.starts_with('C.') || name !in tc.fn_type_modules || tc.cur_module in ['', 'main'] {
			write_file = true
			tc.fn_type_modules[name] = tc.cur_module
		}
	}
	if tc.defer_fn_ancillary {
		tc.fn_ancillary_registrations << FnAncillaryRegistration{
			name: name
			ret_type: ret_type
			shared_params: shared_params
			is_variadic: is_variadic
			implicit_veb_ctx: implicit_veb_ctx
			file: tc.cur_file
			write_file: write_file
		}
	} else {
		tc.fn_ret_types[name] = ret_type
		if write_file {
			tc.fn_type_files[name] = tc.cur_file
		}
		tc.register_fn_ancillary_owned(name, shared_params, is_variadic, implicit_veb_ctx)
	}
}

fn (mut tc TypeChecker) register_fn_ancillary_owned(name string, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	if shared_params.len > 0 {
		tc.fn_shared_params[name] = shared_params
	} else if tc.fn_shared_params.len > 0 {
		// Deleting from an empty map is a paid no-op on this hot path.
		tc.fn_shared_params.delete(name)
	}
	tc.fn_variadic[name] = is_variadic
	if implicit_veb_ctx || tc.fn_implicit_veb_ctx.len > 0 {
		// All readers default absent entries to false, so false inserts only
		// matter once the map holds any true entry (veb builds).
		tc.fn_implicit_veb_ctx[name] = implicit_veb_ctx
	}
	tc.add_receiver_method_suffix_index(name)
}

// register_generated_fn_param_types records a synthesized function signature
// and keeps the receiver/method suffix index complete for post-check phases.
pub fn (mut tc TypeChecker) register_generated_fn_param_types(name string, params []Type) {
	tc.fn_param_types[name] = params
	tc.add_receiver_method_suffix_index(name)
	if tc.transform_signature_maps_changed {
		tc.transform_signature_names_log << name
	}
}

// rebuild_fn_param_suffix_index refreshes the suffix index after a batch
// replaces or removes synthesized signatures.
pub fn (mut tc TypeChecker) rebuild_fn_param_suffix_index() {
	// Allocate a new map so callers rebuilding after a disposable prealloc
	// scope do not retain its keys, values, or backing storage.
	tc.clear_generic_receiver_pattern_cache()
	tc.receiver_method_suffix_index = map[string]string{}
	tc.generic_receiver_method_index = map[string][]string{}
	tc.receiver_method_suffix_index.reserve(u32(tc.fn_param_types.len * 3))
	for name, _ in tc.fn_param_types {
		tc.add_receiver_method_suffix_index(name)
	}
}

fn (mut tc TypeChecker) register_fn_ret_type_text(name string, text string) {
	if tc.defer_fn_ancillary {
		tc.fn_ret_text_registrations << FnTextRegistration{
			name: name
			text: text
		}
		return
	}
	tc.fn_ret_type_texts[name] = text
}

fn (mut tc TypeChecker) add_receiver_method_suffix_index(name string) {
	if name.len == 0 {
		return
	}
	// Plain functions and C-lowered aliases cannot form receiver.method keys.
	// Dotted semantic methods already install both their full and short suffixes,
	// so indexing these aliases only adds paid map writes.
	if tc.method_suffix_prescreen && name.index_u8(`.`) < 0 {
		return
	}
	receiver := name.all_before_last('.')
	if receiver.contains('[') && receiver.ends_with(']') {
		method := name.all_after_last('.')
		mut indexed := tc.generic_receiver_method_index[method] or { []string{} }
		if name !in indexed {
			indexed << name
			tc.generic_receiver_method_index[method] = indexed
			tc.clear_generic_receiver_pattern_cache()
		}
	}
	tc.set_receiver_method_suffix_index(name, name)
	if name.contains('.') {
		bracket := receiver.index_u8(`[`)
		if bracket > 0 {
			tc.set_receiver_method_suffix_index('${receiver[..bracket]}.${name.all_after_last('.')}', name)
		}
	}
	for i in 0 .. name.len {
		if name[i] == `.` && i + 1 < name.len {
			tc.set_receiver_method_suffix_index(name[i + 1..], name)
		}
	}
}

fn (tc &TypeChecker) clear_generic_receiver_pattern_cache() {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := tc.type_cache
	cache.recv_pattern_entries.clear()
	cache.recv_pattern_misses.clear()
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
	lowered_name := tc.cached_c_name(name)
	tc.register_c_variadic_fn_with_lowered(name, lowered_name)
}

fn (mut tc TypeChecker) register_c_variadic_fn_with_lowered(name string, lowered_name string) {
	if name.len == 0 {
		return
	}
	if tc.defer_fn_ancillary {
		tc.fn_c_variadic_registrations << FnNamePairRegistration{
			name: name
			lowered_name: lowered_name
		}
		return
	}
	tc.c_variadic_fns[name] = true
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
	parsed_type := tc.parse_scope_param_type(p.typ)
	typ := mut_param_binding_type(parsed_type, p.is_mut, p.op == .amp)
	owner := tc.cur_scope.insert_with_owner(p.value, typ)
	tc.initialize_pointer_parameter_binding(owner, typ)
	if p.is_mut {
		// A `mut value &T` parameter still has one implicit caller-reference
		// layer: its binding is `&T` at the ABI boundary, but reads and writes in
		// the function body operate on `T`.
		tc.fn_context.mut_param_base_types[p.value] = mut_param_base_type(typ)
		tc.fn_context.mut_param_owners[p.value] = owner
	}
	if param_type_text_is_shared(p.typ) {
		tc.mark_shared_binding_owner(p.value, owner)
		if unalias_and_unwrap_pointer_type(parsed_type) is Array {
			tc.mark_shared_array_binding_owner(p.value, owner)
		}
	}
}

fn mut_param_type_is_allowed(typ Type) bool {
	if typ is Alias || typ is Unknown {
		return true
	}
	clean := unalias_type(typ)
	if clean is OptionType {
		return mut_param_type_is_allowed(clean.base_type)
	}
	return clean is Array || clean is ArrayFixed || clean is Interface || clean is Map
		|| clean is Pointer || clean is Struct || clean is SumType || clean is Enum
}

fn mut_param_base_type(typ Type) Type {
	if typ is Pointer {
		return typ.base_type
	}
	return typ
}

fn mut_param_semantic_type(typ Type) Type {
	if typ !is Pointer {
		return typ
	}
	pointer_type := typ as Pointer
	if pointer_type.base_type !is OptionType {
		return typ
	}
	option_type := pointer_type.base_type as OptionType
	if option_type.base_type is Pointer {
		return typ
	}
	if option_type.base_type !is Struct {
		return typ
	}
	return Type(Pointer{
		base_type: OptionType{
			base_type: Pointer{
				base_type: option_type.base_type
			}
		}
	})
}

fn mut_param_binding_type(typ Type, is_mut bool, is_explicit_reference bool) Type {
	if !is_mut || is_explicit_reference {
		return typ
	}
	if typ is Pointer {
		if unalias_type(typ.base_type) is Interface {
			return typ
		}
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
			tc.annotate_fn_node(node)
		}
	}
}

// annotate_types_with_used_missing_calls revisits only reachable functions
// containing calls whose checked binding was invalidated by lowering, plus
// functions synthesized after the source AST. Unchanged calls retain the
// binding recorded by semantic checking and copied by the transformer.
pub fn (mut tc TypeChecker) annotate_types_with_used_missing_calls(used_fns map[string]bool, source_node_count int) {
	tc.extend_node_caches(tc.a.nodes.len)
	tc.cur_module = ''
	mut pending := []flat.NodeId{cap: 1024}
	mut candidate_fns := 0
	mut annotated_fns := 0
	mut generated_fns := 0
	for idx, node in tc.a.nodes {
		if node.kind == .file {
			tc.enter_file(node.value)
		} else if node.kind == .module_decl {
			tc.enter_module(node.value)
		} else if node.kind == .fn_decl {
			candidate_fns++
			if !tc.should_annotate_fn(node, used_fns)
				|| (idx < source_node_count && !tc.fn_contains_unresolved_call(node, mut pending)) {
				continue
			}
			annotated_fns++
			if idx >= source_node_count {
				generated_fns++
			}
			tc.annotate_fn_node(node)
		}
	}
	tc.timing_profile('  [ttime]   annotate candidates ${candidate_fns}, selected ${annotated_fns}, generated ${generated_fns}')
}

fn (tc &TypeChecker) fn_contains_unresolved_call(root flat.Node, mut pending []flat.NodeId) bool {
	pending.clear()
	pending.ensure_cap(root.children_count)
	for i in 0 .. root.children_count {
		pending << tc.a.child(&root, i)
	}
	for pending.len > 0 {
		id := pending.pop()
		idx := int(id)
		if idx < 0 || idx >= tc.a.nodes.len {
			continue
		}
		node := tc.a.nodes[idx]
		if node.kind == .call && tc.cached_resolved_call(id) == none {
			return true
		}
		for i in 0 .. node.children_count {
			pending << tc.a.child(&node, i)
		}
	}
	return false
}

// annotate_fn_node records contextual types for one function body. The caller
// owns the function's node range, allowing independent bodies to run in
// parallel without sharing lexical state.
fn (mut tc TypeChecker) annotate_fn_node(node flat.Node) {
	saved_fn_context := tc.fn_context
	tc.fn_context = new_function_check_context()
	tc.fn_context.generic_params = tc.infer_decl_generic_param_names(node)
	tc.fn_context.return_type = tc.parse_type(node.typ)
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
	tc.fn_context = saved_fn_context
}

// diagnose_unused_private_declarations records notices for unreachable private
// functions and constants that are never referenced by any declaration.
// UnusedDeclCandidate is one private declaration that survived the used-fns
// filter and now only needs the referenced-name scan to confirm it is unused.
struct UnusedDeclCandidate {
	is_fn    bool
	name     string
	qname    string
	file     string
	node_id  flat.NodeId
	position token.Pos
mut:
	alive bool
}

pub fn (mut tc TypeChecker) diagnose_unused_private_declarations(used_fns map[string]bool) {
	if tc.errors.len > 0 || !tc.has_main_module_fn_main() {
		return
	}
	// Collect the (few) candidate declarations first, then scan the AST once
	// probing only against that small candidate set. Building referenced-name
	// maps over every ident/selector/call in the program did the same work as
	// this inverted form, but with hundreds of thousands of map inserts.
	mut candidates := []UnusedDeclCandidate{cap: 64}
	// Maps a referenced spelling (name or module-qualified name) to the
	// candidate indexes it keeps alive.
	mut fn_keys := map[string][]int{}
	mut const_keys := map[string][]int{}
	mut module_name := ''
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind == .fn_decl {
			if node.op == .arrow || node.value in ['main', 'init', 'cleanup']
				|| is_v_test_fn_name(node.value) || node.value.starts_with('__anon_fn_')
				|| node.value.contains('.') {
				continue
			}
			if tc.declaration_contains_error(node) {
				continue
			}
			qname := checker_qualified_fn_name(module_name, node.value)
			cname := tc.cached_c_name(qname)
			if used_fns[node.value] || used_fns[qname] || used_fns[cname] {
				continue
			}
			cand_idx := candidates.len
			candidates << UnusedDeclCandidate{
				is_fn: true
				name: node.value
				qname: qname
				file: tc.cur_file
				node_id: flat.NodeId(idx)
			}
			fn_keys[node.value] << cand_idx
			if qname != node.value {
				fn_keys[qname] << cand_idx
			}
			continue
		}
		if node.kind != .const_decl {
			continue
		}
		for i in 0 .. node.children_count {
			field_id := tc.a.child(&node, i)
			field := tc.a.node(field_id)
			if field.kind != .const_field || field.value.len == 0 || field.value.starts_with('C.')
				|| field.value.starts_with('_') {
				continue
			}
			qname := if module_name.len > 0 && module_name != 'main' {
				'${module_name}.${field.value}'
			} else {
				field.value
			}
			cand_idx := candidates.len
			candidates << UnusedDeclCandidate{
				is_fn: false
				name: field.value
				qname: qname
				file: tc.cur_file
				node_id: field_id
				position: field.pos
			}
			const_keys[field.value] << cand_idx
			if qname != field.value {
				const_keys[qname] << cand_idx
			}
		}
	}
	if candidates.len == 0 {
		return
	}
	// A reference through either the full spelling or its short (last-segment)
	// spelling keeps a candidate alive, mirroring the referenced-name maps the
	// former form built from both spellings of every reference. The scan only
	// probes the small candidate-key maps and sets alive flags, so disjoint
	// node shards can run on the worker pool and OR-merge deterministically.
	mut alive := []bool{len: candidates.len}
	tc.scan_unused_candidate_references(fn_keys, const_keys, mut alive)
	for cand_idx in 0 .. candidates.len {
		if alive[cand_idx] {
			candidates[cand_idx].alive = true
		}
	}
	// The diagnostic filter and recorded file both read tc.cur_file; replay each
	// candidate's file so deferred emission matches the former in-walk emission.
	walk_end_file := tc.cur_file
	for cand in candidates {
		if cand.alive {
			continue
		}
		tc.cur_file = cand.file
		if cand.is_fn {
			tc.record_notice_at(.unknown_ident, 'unused function: `${cand.name}`', cand.node_id, tc.node_value_diagnostic_pos(cand.node_id))
		} else {
			tc.record_notice_at(.unknown_ident, 'unused constant: `${cand.name}`', cand.node_id, cand.position)
		}
	}
	tc.cur_file = walk_end_file
}

fn (tc &TypeChecker) declaration_contains_error(node flat.Node) bool {
	for diagnostic in tc.errors {
		if diagnostic.pos.id == node.pos.id && diagnostic.pos.offset >= node.pos.offset
			&& diagnostic.pos.offset < node.pos.end {
			return true
		}
	}
	return false
}

// check_main_module_requirement rejects ordinary programs that contain no
// selected source file in the `main` module.
pub fn (mut tc TypeChecker) check_main_module_requirement(is_shared bool) {
	if tc.valid_diagnostic_fast {
		return
	}
	if is_shared || tc.has_c_test_harness_main() || (tc.checker_fixture_mode && tc.errors.len > 0) {
		return
	}
	// A source file without an explicit `module` declaration belongs to `main`.
	// Such a file has no module_decl node for the scan below to discover.
	if tc.has_main_module_fn_main() {
		return
	}
	for file, _ in tc.diagnostic_files {
		if file.ends_with('_test.v') {
			return
		}
	}
	for diagnostic in tc.errors {
		if diagnostic.msg.starts_with('invalid test file name `') {
			return
		}
	}
	mut first_module_id := flat.NodeId(-1)
	mut first_module_file := ''
	mut has_main := false
	mut has_postinclude := false
	for node in tc.a.nodes {
		if node.kind == .directive && node.value == 'postinclude' {
			has_postinclude = true
			break
		}
	}
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			continue
		}
		if node.kind != .module_decl || tc.cur_file !in tc.diagnostic_files {
			continue
		}
		if int(first_module_id) < 0 {
			first_module_id = flat.NodeId(idx)
			first_module_file = tc.cur_file
		}
		if node.value == 'main' {
			has_main = true
		}
	}
	if has_main || has_postinclude || int(first_module_id) < 0 {
		return
	}
	tc.enter_file(first_module_file)
	node := tc.a.node(first_module_id)
	file := tc.a.source_files[node.pos.id] or { &token.File{} }
	source := tc.source_texts_by_file[file.name] or { '' }
	line_start := if node.pos.offset >= 0 && node.pos.offset <= source.len {
		if idx := source[..node.pos.offset].last_index('\n') { idx + 1 } else { 0 }
	} else {
		node.pos.offset
	}
	tc.record_error_at(.duplicate_decl, 'project must include a `main` module or be a shared library (compile with `v -shared`)', first_module_id, token.new_span(node.pos.id, line_start, line_start + 1))
}

fn (tc &TypeChecker) should_annotate_fn(node flat.Node, used_fns map[string]bool) bool {
	if used_fns.len == 0 {
		return true
	}
	qname := checker_qualified_fn_name(tc.cur_module, node.value)
	if qname in tc.a.export_fn_names || tc.fn_is_veb_app_handler(node) {
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
	mut pending := [id]
	for pending.len > 0 {
		current_id := pending.pop()
		if int(current_id) < 0 {
			continue
		}
		node := tc.a.nodes[int(current_id)]
		match node.kind {
			.decl_assign {
				lhs_count := tc.multi_assign_lhs_count(node)
				rhs_count := tc.multi_assign_rhs_count(node)
				if rhs_count == 1 && lhs_count > 1 {
					tc.annotate_multi_return_decl_assign(node)
					continue
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
							owner := tc.cur_scope.insert_with_owner(lhs.value, typ)
							if lhs.value != '_' && tc.decl_lhs_is_mut(node, lhs_id) {
								tc.fn_context.mut_local_owners[lhs.value] = owner
							}
							if owner.storage_key().len > 0
								&& decl_assign_is_shared_marker(node.value) {
								tc.mark_shared_binding_owner(lhs.value, owner)
							}
							tc.remember_expr_type(lhs_id, typ)
						}
					}
				}
				continue
			}
			.for_in_stmt {
				tc.annotate_for_in(current_id, node)
				continue
			}
			.comptime_for {
				continue
			}
			.fn_literal {
				tc.annotate_fn_literal(node)
				continue
			}
			.selector {
				if node.children_count > 0 {
					base_id := tc.a.child(&node, 0)
					base_type := tc.resolve_type(base_id)
					if type_contains_unknown(base_type)
						|| ((base_type is OptionType || base_type is ResultType)
							&& node.value !in ['ok', 'value', 'err'])
						|| (tc.fn_context.generic_params.len > 0 && (base_type is OptionType
							|| base_type is ResultType)) {
						tc.annotate_node(base_id)
						continue
					}
				}
			}
			.assign, .selector_assign, .index_assign {
				if node.children_count > 0
					&& tc.annotation_storage_path_has_unknown(tc.a.child(&node, 0)) {
					for i in 0 .. node.children_count {
						tc.annotate_node(tc.a.child(&node, i))
					}
					continue
				}
				tc.annotate_assign_expected_exprs(node)
			}
			.struct_init {
				tc.annotate_struct_init_expected_exprs(node)
			}
			.call {
				if tc.fn_context.generic_params.len > 0 {
					dsl_name := tc.unresolved_array_dsl_call_name(node)
					if dsl_name.len > 0 {
						tc.push_array_dsl_scope(node, dsl_name)
					}
					for i in 0 .. node.children_count {
						tc.annotate_node(tc.a.child(&node, i))
					}
					tc.annotate_call_expected_exprs(current_id, node)
					if info := tc.resolve_call_info(current_id, node) {
						tc.remember_expr_type(current_id, info.return_type)
					}
					if dsl_name.len > 0 {
						tc.pop_scope()
					}
					continue
				}
				tc.annotate_call_expected_exprs(current_id, node)
				// The call annotation above records a more precise return type for
				// contextual builtins such as `map.move()`. Avoid replacing it with
				// the parser's broad `map`/`array` placeholder below.
				for i in 0 .. node.children_count {
					tc.annotate_node(tc.a.child(&node, i))
				}
				continue
			}
			.index {
				if generic_fn_type := tc.explicit_generic_fn_value_type(node) {
					tc.remember_expr_type(current_id, generic_fn_type)
					continue
				}
				if node.children_count > 0
					&& type_contains_unknown(tc.resolve_type(tc.a.child(&node, 0))) {
					for i in 0 .. node.children_count {
						tc.annotate_node(tc.a.child(&node, i))
					}
					continue
				}
			}
			else {}
		}

		tc.remember_expr_type(current_id, tc.resolve_type(current_id))
		for i := node.children_count - 1; i >= 0; i-- {
			pending << tc.a.child(&node, i)
		}
	}
}

fn (mut tc TypeChecker) annotation_storage_path_has_unknown(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind !in [.selector, .index] || node.children_count == 0 {
		return false
	}
	base_type := tc.resolve_type(tc.a.child(node, 0))
	return type_contains_unknown(base_type) || base_type is OptionType || base_type is ResultType
}

fn (mut tc TypeChecker) annotate_multi_return_decl_assign(node flat.Node) {
	rhs_id := tc.a.child(&node, 1)
	tc.annotate_node(rhs_id)
	lhs_ids := tc.multi_assign_lhs_ids(node)
	mut item_types := []Type{}
	rhs := tc.a.node(rhs_id)
	if rhs.kind == .match_stmt {
		item_types = tc.match_multi_return_types(rhs_id, lhs_ids.len) or { []Type{} }
	}
	if item_types.len == 0 {
		rhs_type := tc.resolve_type(rhs_id)
		if multi := tc.multi_return_assignment_type(rhs_id, rhs_type) {
			item_types = multi.types.clone()
		}
	}
	if item_types.len != lhs_ids.len {
		return
	}
	for i, lhs_id in lhs_ids {
		lhs := tc.a.node(lhs_id)
		if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
			continue
		}
		tc.cur_scope.insert(lhs.value, item_types[i])
		if tc.decl_lhs_is_mut(node, lhs_id) {
			if owner := tc.cur_scope.lookup_owner(lhs.value) {
				tc.fn_context.mut_local_owners[lhs.value] = owner
			}
		}
		tc.remember_expr_type(lhs_id, item_types[i])
	}
}

fn (mut tc TypeChecker) annotate_fn_literal(node flat.Node) {
	saved_fn_context := tc.fn_context
	tc.fn_context = new_function_check_context()
	tc.fn_context.node_id = saved_fn_context.node_id
	tc.fn_context.generic_params = saved_fn_context.generic_params.clone()
	tc.fn_context.return_type = tc.parse_type(node.typ)
	tc.push_scope()
	for i in 0 .. node.children_count {
		tc.insert_fn_param_binding(tc.a.child_node(&node, i))
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind !in [.param, .ident] {
			tc.annotate_node(child_id)
		}
	}
	tc.pop_scope()
	tc.fn_context = saved_fn_context
}

fn (mut tc TypeChecker) annotate_expected_expr(id flat.NodeId, expected Type) {
	if int(id) >= 0 && int(id) < tc.a.nodes.len {
		node := tc.a.nodes[int(id)]
		if node.kind in [.if_expr, .match_stmt, .array_literal] && expected !is Void
			&& expected !is Unknown {
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
	if sibling_type := tc.if_sibling_branch_array_context(id) {
		return sibling_type
	}
	parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .array_literal {
			if expected_parent := tc.expected_context_for_expr(parent_id) {
				context_type := unalias_type(contextual_payload_type(expected_parent) or {
					expected_parent
				})
				if elem_type := array_like_elem_type(context_type) {
					return elem_type
				}
			}
		}
		if parent.kind == .map_init {
			for i in 0 .. parent.children_count {
				if tc.a.child(parent, i) != id {
					continue
				}
				if expected_parent := tc.expected_context_for_expr(parent_id) {
					if expected_map := map_type_from_receiver(expected_parent) {
						return if i % 2 == 0 {
							expected_map.key_type
						} else {
							expected_map.value_type
						}
					}
				}
				// An untyped map literal can still infer an empty value such as `[]`
				// from another value in the same literal.
				if i % 2 == 1 {
					for sibling := 1; sibling < int(parent.children_count); sibling += 2 {
						sibling_id := tc.a.child(parent, sibling)
						if sibling_id == id || tc.expr_is_empty_bare_array_literal(sibling_id) {
							continue
						}
						sibling_type := tc.resolve_type(sibling_id)
						if sibling_type !is Void && sibling_type !is Unknown {
							return sibling_type
						}
					}
				}
				break
			}
		}
		if parent.kind == .infix && parent.op in [.eq, .ne] && parent.children_count >= 2 {
			lhs_id := tc.a.child(parent, 0)
			rhs_id := tc.a.child(parent, 1)
			other_id := if lhs_id == id {
				rhs_id
			} else if rhs_id == id {
				lhs_id
			} else {
				flat.NodeId(-1)
			}
			if tc.valid_node_id(other_id) {
				mut other_type := tc.resolve_type(other_id)
				if other_type is Pointer {
					other_type = other_type.base_type
				}
				if array_like_elem_type(other_type) != none {
					return other_type
				}
			}
		}
	}
	return tc.expr_type(id)
}

fn (tc &TypeChecker) if_sibling_branch_array_context(id flat.NodeId) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	mut child_id := id
	for _ in 0 .. 32 {
		parent_id := tc.direct_parent_id(child_id)
		if !tc.valid_node_id(parent_id) {
			return none
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .if_expr && parent.children_count > 1 {
			mut branch_index := -1
			for i in 1 .. parent.children_count {
				if tc.a.child(parent, i) == child_id
					|| tc.expr_is_value_tail_of(tc.a.child(parent, i), id) {
					branch_index = i
					break
				}
			}
			if branch_index >= 1 {
				for i in 1 .. parent.children_count {
					if i == branch_index {
						continue
					}
					tail_id := tc.branch_tail_expr_id(tc.a.child(parent, i))
					if !tc.valid_node_id(tail_id) || tc.expr_is_empty_bare_array_literal(tail_id) {
						continue
					}
					typ := tc.resolve_type(tail_id)
					if array_like_elem_type(unalias_type(typ)) != none {
						return typ
					}
				}
			}
		}
		if parent.kind == .match_stmt && parent.children_count > 1 {
			mut branch_index := -1
			for i in 1 .. parent.children_count {
				if tc.a.child(parent, i) == child_id
					|| tc.expr_is_value_tail_of(tc.a.child(parent, i), id) {
					branch_index = i
					break
				}
			}
			if branch_index >= 1 {
				for i in 1 .. parent.children_count {
					if i == branch_index {
						continue
					}
					tail_id := tc.branch_tail_expr_id(tc.a.child(parent, i))
					if !tc.valid_node_id(tail_id) || tc.expr_is_empty_bare_array_literal(tail_id) {
						continue
					}
					typ := tc.resolve_type(tail_id)
					if array_like_elem_type(unalias_type(typ)) != none {
						return typ
					}
				}
			}
		}
		if parent.kind !in [.paren, .expr_stmt, .block, .match_branch, .if_expr, .match_stmt] {
			return none
		}
		child_id = parent_id
	}
	return none
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
		.or_expr {
			return node.children_count >= 2 && node.value !in ['!', '?'] && tc.expr_is_value_tail_of(tc.a.child(&node, 1), target_id)
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
	if assignment_marker_value_is_error(node.value) {
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
	if tc.is_generic_enum_from_call(node) {
		tc.check_call_arg_types(id, node, info)
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
	mut actual_count := node.children_count - 1 - info.arg_offset - field_init_args + collapsed + recv_extra
	for i in 1 + info.arg_offset .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		arg_type := tc.cached_expr_type(arg_id) or { tc.resolve_type(arg_id) }
		if !info.is_variadic && arg_type is MultiReturn && i == node.children_count - 1
			&& !is_print_style_fn_name(info.name) {
			actual_count += arg_type.types.len - 1
		}
	}
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	mut expanded_arg_offset := 0
	for i in 1 + info.arg_offset .. node.children_count {
		raw_arg := tc.a.child_node(&node, i)
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if raw_arg.kind == .field_init {
			tc.annotate_params_field_expected_expr(arg_id, raw_arg.value, info)
			continue
		}
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) + arg_shift + expanded_arg_offset
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			continue
		}
		if param_idx >= info.params.len {
			continue
		}
		arg_type := tc.cached_expr_type(arg_id) or { tc.resolve_type(arg_id) }
		if !info.is_variadic && arg_type is MultiReturn && i == node.children_count - 1
			&& arg_type.types.len == info.params.len - param_idx {
			expanded_arg_offset += arg_type.types.len - 1
			continue
		}
		expected := if info.is_variadic && param_idx == info.params.len - 1
			&& tc.spread_arg_child(arg_id) != none {
			info.params[param_idx]
		} else {
			tc.call_arg_expected_type(info, param_idx)
		}
		dsl_name := if is_array_dsl_call_name(info.name) {
			info.name
		} else {
			tc.unresolved_array_dsl_call_name(node)
		}
		needs_dsl_scope := tc.call_arg_needs_array_dsl_scope(dsl_name, param_idx)
		if needs_dsl_scope {
			tc.push_array_dsl_scope(node, dsl_name)
		}
		tc.annotate_expected_expr(arg_id, expected)
		if needs_dsl_scope {
			tc.pop_scope()
		}
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
	if expected := tc.params_field_expected_type(field_name, info) {
		tc.annotate_expected_expr(arg_id, expected)
	}
}

fn (tc &TypeChecker) params_field_expected_type(field_name string, info CallInfo) ?Type {
	if field_name.len == 0 {
		return none
	}
	for param in info.params {
		param_struct := struct_type_from_type(unwrap_pointer(param)) or { continue }
		if expected := tc.struct_field_type(param_struct.name, field_name) {
			return expected
		}
	}
	return none
}

fn (tc &TypeChecker) params_field_owner(field_name string, info CallInfo) ?string {
	if field_name.len == 0 {
		return none
	}
	for param in info.params {
		param_struct := struct_type_from_type(unwrap_pointer(param)) or { continue }
		if tc.struct_field_type(param_struct.name, field_name) != none {
			return param_struct.name
		}
	}
	return none
}

fn params_field_owner_display(owner string) string {
	parts := owner.split('.')
	if parts.len > 1 {
		return parts[parts.len - 2..].join('.')
	}
	return owner
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
		tc.insert_loop_var(key_id, tc.range_loop_var_type(container_id, tc.a.child(&node, 3)))
		tc.annotate_node(tc.a.child(&node, 3))
	} else {
		clean := tc.for_in_iterable_type(container_id)
		yields_ref := node.op == .amp || tc.for_in_iterable_yields_ref(container_id)
		if clean is Array {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				if node.op == .amp {
					tc.insert_mut_loop_var(val_id, elem_type)
				} else {
					tc.insert_loop_var(val_id, elem_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var(key_id, elem_type)
				} else {
					tc.insert_loop_var(key_id, elem_type)
				}
			}
		} else if clean is ArrayFixed {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				if node.op == .amp {
					tc.insert_mut_loop_var(val_id, elem_type)
				} else {
					tc.insert_loop_var(val_id, elem_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var(key_id, elem_type)
				} else {
					tc.insert_loop_var(key_id, elem_type)
				}
			}
		} else if clean is Map {
			value_type := for_in_ref_binding_type(clean.value_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				if node.op == .amp {
					tc.insert_mut_loop_var(val_id, value_type)
				} else {
					tc.insert_loop_var(val_id, value_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var(key_id, value_type)
				} else {
					tc.insert_loop_var(key_id, value_type)
				}
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
				tc.insert_loop_var(key_id, tc.range_loop_var_type(tc.a.child(&container, 0), tc.a.child(&container, 1)))
			} else {
				// Annotation also walks deferred generic branches such as
				// `$if T is $array { for value in data { ... } }`. The source template's
				// `T` is intentionally unresolved here; keep its loop bindings visible so
				// annotation does not emit diagnostics for otherwise valid specializations.
				unresolved := unknown_type('unresolved for-in element during annotation')
				tc.insert_loop_var(key_id, unresolved)
				if has_val {
					tc.insert_loop_var(val_id, unresolved)
				}
			}
		}
	}
	for i in header .. node.children_count {
		tc.annotate_node(tc.a.child(&node, i))
	}
}

fn (tc &TypeChecker) for_in_iterable_type(container_id flat.NodeId) Type {
	mut clean := unwrap_pointer(tc.resolve_type(container_id))
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		if clean is OptionType {
			base := unalias_type(unwrap_pointer(clean.base_type))
			if base is Array || base is ArrayFixed {
				clean = base
				continue
			}
		}
		break
	}
	return clean
}

fn (tc &TypeChecker) for_in_iterable_yields_ref(container_id flat.NodeId) bool {
	if tc.expr_is_shared_arg(container_id) {
		return false
	}
	mut typ := tc.resolve_type(container_id)
	for _ in 0 .. 8 {
		if typ is Alias {
			typ = typ.base_type
			continue
		}
		if typ is OptionType {
			base := typ.base_type
			clean_base := unalias_type(unwrap_pointer(base))
			if clean_base is Array || clean_base is ArrayFixed {
				typ = base
				continue
			}
		}
		break
	}
	return false
}

pub fn (tc &TypeChecker) iterator_for_in_elem_type(typ Type) ?Type {
	info := tc.iterator_for_in_next_call_info(typ) or { return none }
	return iterator_for_in_elem_type_from_next_return(info.return_type)
}

fn (tc &TypeChecker) iterator_unbounded_next_generic(typ Type) ?string {
	info := tc.iterator_for_in_next_call_info(typ) or { return none }
	type_name := resolve_type_name_for_method(unwrap_pointer(typ))
	base, _, is_generic := generic_type_application_parts(type_name)
	owner_name := if is_generic { base } else { type_name }
	owner_params := tc.struct_generic_params[owner_name] or {
		tc.struct_generic_params[tc.qualify_name(owner_name)] or { []string{} }
	}
	method_params := tc.fn_generic_params[info.name] or {
		mut found := []string{}
		for candidate in receiver_method_name_candidates(unwrap_pointer(typ), 'next', tc.cur_module) {
			if params := tc.fn_generic_params[candidate] {
				found = params.clone()
				break
			}
		}
		found
	}
	for param in method_params {
		if param !in owner_params {
			return param
		}
	}
	return none
}

pub fn (tc &TypeChecker) iterator_for_in_next_call_info(typ Type) ?CallInfo {
	clean := unwrap_pointer(typ)
	name := clean.name()
	if clean is Interface {
		if info := tc.interface_receiver_method_call_info(name, 'next') {
			if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
				return info
			}
		}
	}
	if name == 'RunesIterator' || name == 'builtin.RunesIterator' {
		return CallInfo{
			name: 'RunesIterator.next'
			params: tarr1(Type(Pointer{
				base_type: clean
			}))
			return_type: Type(OptionType{
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
			return tc.specialize_generic_interface_method(name, info)
		}
	}
	for method_name in receiver_method_name_candidates(clean, 'next', tc.cur_module) {
		if method_name !in tc.fn_ret_types {
			continue
		}
		info := tc.call_info(method_name, true)
		if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
			return tc.specialize_generic_interface_method(name, info)
		}
	}
	return none
}

// iterator_for_in_next_call_info_text returns the specialized `next` call metadata for an iterator type.
pub fn (tc &TypeChecker) iterator_for_in_next_call_info_text(type_text string) ?CallInfo {
	info := tc.iterator_for_in_next_call_info(tc.parse_type(type_text)) or { return none }
	return tc.specialize_generic_interface_method(type_text.trim_left('&'), info)
}

fn (tc &TypeChecker) specialize_generic_interface_method(type_name string, info CallInfo) CallInfo {
	base, args, is_generic := generic_type_application_parts(type_name)
	if !is_generic || args.len == 0 {
		return info
	}
	params := tc.interface_generic_params[base] or {
		tc.interface_generic_params[tc.qualify_name(base)] or { return info }
	}
	if params.len != args.len {
		return info
	}
	mut concrete_types := []Type{cap: args.len}
	for arg in args {
		concrete_types << tc.parse_type(tc.qualify_resolution_type_text(arg))
	}
	mut specialized_params := []Type{cap: info.params.len}
	for param in info.params {
		specialized_params << tc.substitute_generic_type_values(param, concrete_types, params)
	}
	return CallInfo{
		...info
		params: specialized_params
		return_type: tc.substitute_generic_type_values(info.return_type, concrete_types, params)
	}
}

pub fn (tc &TypeChecker) index_overload_call_info(typ Type, setter bool) ?CallInfo {
	method := if setter { '[]=' } else { '[]' }
	clean := unwrap_pointer(typ)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	if info := tc.resolve_generic_struct_method(type_name, method) {
		return tc.specialized_index_overload_call_info(type_name, method, info)
	}
	if concrete := tc.concrete_method_signature_key(type_name, method) {
		return tc.call_info(concrete, true)
	}
	for method_name in receiver_method_name_candidates(clean, method, tc.cur_module) {
		if method_name !in tc.fn_ret_types {
			continue
		}
		if !tc.method_can_be_called_on_receiver(typ, method, method_name) {
			continue
		}
		return tc.call_info(method_name, true)
	}
	return none
}

fn (tc &TypeChecker) specialized_index_overload_call_info(type_name string, method string, info CallInfo) CallInfo {
	concrete := tc.concrete_method_signature_key(type_name, method) or {
		_, args, ok := generic_type_application_parts(type_name)
		if !ok || args.len == 0 {
			return info
		}
		'${type_name}.${method}'
	}
	if concrete == info.name {
		return info
	}
	return CallInfo{
		name: concrete
		params: info.params.clone()
		shared_params: info.shared_params.clone()
		return_type: info.return_type
		has_receiver: info.has_receiver
		is_variadic: info.is_variadic
		is_c_variadic: info.is_c_variadic
		params_known: info.params_known
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset: info.arg_offset
	}
}

fn iterator_for_in_elem_type_from_next_return(ret Type) ?Type {
	if ret is OptionType {
		return ret.base_type
	}
	return none
}

fn (tc &TypeChecker) range_loop_var_type(low_id flat.NodeId, high_id flat.NodeId) Type {
	low_type := tc.resolve_type(low_id)
	if tc.valid_node_id(high_id) {
		high_type := tc.resolve_type(high_id)
		if tc.range_endpoint_is_literal(low_id) && !tc.range_endpoint_is_literal(high_id)
			&& fn_param_unalias_type(high_type).is_integer() {
			return high_type
		}
		if !tc.range_endpoint_is_literal(low_id) && tc.range_endpoint_is_literal(high_id)
			&& fn_param_unalias_type(low_type).is_integer() {
			return low_type
		}
	}
	if fn_param_unalias_type(low_type).is_integer() {
		return low_type
	}
	return Type(int_)
}

// insert_loop_var updates insert loop var state for types.
fn (mut tc TypeChecker) insert_loop_var(id flat.NodeId, typ Type) ScopeBindingOwner {
	if int(id) < 0 {
		return ScopeBindingOwner{}
	}
	v := tc.a.nodes[int(id)]
	if v.kind == .ident && v.value.len > 0 {
		owner := tc.cur_scope.insert_with_owner(v.value, typ)
		tc.initialize_unknown_pointer_binding(owner, typ)
		tc.remember_expr_type(id, typ)
		return owner
	}
	return ScopeBindingOwner{}
}

fn (mut tc TypeChecker) insert_mut_loop_var(id flat.NodeId, typ Type) {
	owner := tc.insert_loop_var(id, typ)
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return
	}
	v := tc.a.nodes[int(id)]
	if v.kind != .ident || v.value.len == 0 || v.value == '_' {
		return
	}
	tc.fn_context.mut_local_owners[v.value] = owner
	if typ is Pointer {
		tc.fn_context.mut_param_base_types[v.value] = typ.base_type
	}
}

fn for_in_ref_binding_type(typ Type, yields_ref bool) Type {
	if yields_ref && typ !is Pointer {
		return Type(Pointer{
			base_type: typ
		})
	}
	return typ
}

// expr_type returns the resolved type recorded for a node during annotate_types.
@[direct_array_access]
pub fn (tc &TypeChecker) expr_type(id flat.NodeId) ?Type {
	if t := tc.resolved_call_type(id) {
		return t
	}
	if int(id) >= 0 {
		node := tc.a.nodes[int(id)]
		if node.kind == .call && node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
			return tc.parse_type(node.typ)
		}
	}
	if t := tc.cached_expr_type(id) {
		return t
	}
	return none
}

// resolved_call_type supports resolved call type handling for TypeChecker.
@[direct_array_access]
fn (tc &TypeChecker) resolved_call_type(id flat.NodeId) ?Type {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .call {
		return none
	}
	if node.children_count == 3 && tc.call_display_name(node) == 'C.va_arg' {
		type_arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		type_name := tc.generic_call_type_arg_name(type_arg_id)
		if type_name.len > 0 {
			target_type := tc.parse_type(type_name)
			if target_type !is Unknown && !type_contains_unknown(target_type) {
				return target_type
			}
		}
	}
	if t := tc.cached_expr_type(id) {
		if t !is Void {
			return t
		}
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
// invalidate_checked_expr_type drops the cached checked type for one node the
// transformer rewrote in place, so a trust_checked_expr_types resolve cannot
// return the pre-rewrite type (see resolve_type). Rewrites target ids inside
// the writer's own disjoint region, so the shared dense flag array sees no
// concurrent writes to the same slot.
@[direct_array_access; inline]
pub fn (tc &TypeChecker) invalidate_checked_expr_type(idx int) {
	if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		mut wtc := unsafe { tc }
		wtc.expr_type_set[idx] = false
	}
	// A body-local resolve memo can be active while the transformer rewrites
	// nodes inside the current work item; its positional entries go stale the
	// same way the dense cache does.
	mut memo := tc.body_resolve_memo
	if !isnil(memo) && memo.active && idx >= memo.lo && idx <= memo.hi {
		memo.filled[idx - memo.lo] = 0
		call_slot := idx & 2047
		if memo.call_generations[call_slot] == memo.call_generation
			&& memo.call_ids[call_slot] == idx {
			memo.call_generations[call_slot] = 0
		}
	}
}

@[direct_array_access]
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
@[direct_array_access]
fn (tc &TypeChecker) cached_resolved_call(id flat.NodeId) ?string {
	idx := int(id)
	if !isnil(tc.fork_overlay) && idx >= tc.fork_overlay.base_node_count {
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

// resolved_call_may_store_globally reports whether the resolved callee or one of its
// transitive callees belongs to a source file annotated with `@[has_globals]`.
pub fn (tc &TypeChecker) resolved_call_may_store_globally(id flat.NodeId) bool {
	name := tc.cached_resolved_call(id) or { return false }
	mut visiting := map[string]bool{}
	return tc.fn_may_store_globally(name, mut visiting)
}

// fn_value_may_store_globally reports whether invoking a function value can reach a
// declaration from a source file annotated with `@[has_globals]`. An unresolved function
// value is conservative because its body is opaque at this call site.
pub fn (tc &TypeChecker) fn_value_may_store_globally(id flat.NodeId) bool {
	name := tc.resolved_fn_value_name(id) or { return true }
	mut visiting := map[string]bool{}
	return tc.fn_may_store_globally(name, mut visiting)
}

fn (tc &TypeChecker) fn_may_store_globally(name string, mut visiting map[string]bool) bool {
	if visiting[name] {
		return false
	}
	visiting[name] = true
	file := tc.fn_type_files[name] or { return false }
	if tc.has_globals_files[file] {
		return true
	}
	decl_module := tc.fn_type_modules[name] or { '' }
	decl := tc.visible_mutation_fn_decl(name, decl_module) or { return false }
	for dependency in tc.direct_dependency_ids(decl.idx) {
		dependency_name := tc.frozen_symbol_name(dependency)
		if dependency_name.len > 0 && tc.fn_may_store_globally(dependency_name, mut visiting) {
			return true
		}
	}
	fn_node := tc.a.nodes[decl.idx]
	for i in 0 .. fn_node.children_count {
		if tc.node_calls_fn_that_may_store_globally(tc.a.child(&fn_node, i), decl.mod, mut visiting) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) node_calls_fn_that_may_store_globally(id flat.NodeId, caller_mod string, mut visiting map[string]bool) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return false
	}
	if node.kind == .call {
		if name := tc.cached_resolved_call(id) {
			if tc.fn_may_store_globally(name, mut visiting) {
				return true
			}
		}
		if node.children_count > 0 {
			callee := tc.a.child_node(&node, 0)
			if callee.kind == .ident {
				if tc.fn_may_store_globally(checker_qualified_fn_name(caller_mod, callee.value), mut visiting) {
					return true
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		if tc.node_calls_fn_that_may_store_globally(tc.a.child(&node, i), caller_mod, mut visiting) {
			return true
		}
	}
	return false
}

// resolved_call_is_builtin reports whether `id` resolved to the named builtin function.
pub fn (tc &TypeChecker) resolved_call_is_builtin(id flat.NodeId, name string) bool {
	resolved := tc.cached_resolved_call(id) or { return false }
	if resolved != name && resolved != 'builtin.${name}' {
		return false
	}
	return tc.fn_type_modules[resolved] or { '' } == 'builtin'
}

// reset_resolved_calls_for_reannotation discards pre-transform call-name
// bindings before the transformed AST is annotated again. The annotation walk
// resolves and records every reachable call against the final AST/signatures.
pub fn (mut tc TypeChecker) reset_resolved_calls_for_reannotation() {
	for idx in 0 .. tc.resolved_call_set.len {
		tc.resolved_call_set[idx] = false
	}
	tc.sparse_resolved_call_names.clear()
}

// fn_param_types_for_name returns the collected parameter types for a resolved call name.
pub fn (tc &TypeChecker) fn_param_types_for_name(name string) []Type {
	if params := tc.fn_param_types[name] {
		return params
	}
	if name.len == 0 {
		return []Type{}
	}
	if indexed := tc.receiver_method_suffix_index[name] {
		if indexed == receiver_method_suffix_ambiguous {
			return []Type{}
		}
		if params := tc.fn_param_types[indexed] {
			return params
		}
	}
	// add_receiver_method_suffix_index records every source declaration, while
	// register_generated_fn_param_types records post-check synthesized functions.
	// A missing key is therefore a definitive miss. Keep the scan only behind the
	// existing compatibility switch for diagnosing index construction issues.
	if tc.method_suffix_prescreen {
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
	if name in tc.a.noreturn_fns {
		return true
	}
	if name.contains('.') {
		for candidate, is_noreturn in tc.a.noreturn_fns {
			if is_noreturn && candidate.contains('.') && name.ends_with('.${candidate}') {
				return true
			}
		}
	}
	return false
}

// resolved_fn_value_name returns the checker-resolved function name for a function value node.
@[direct_array_access]
pub fn (tc &TypeChecker) resolved_fn_value_name(id flat.NodeId) ?string {
	idx := int(id)
	if !isnil(tc.fork_overlay) && idx >= tc.fork_overlay.base_node_count {
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

// clear_resolved_fn_value removes stale function-value metadata after a later
// transform proves that an identifier refers to a value declaration.
pub fn (mut tc TypeChecker) clear_resolved_fn_value(id flat.NodeId) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if !isnil(tc.fork_overlay) {
		tc.fork_overlay.resolved_fn_values.delete(idx)
	}
	if idx < tc.resolved_fn_value_set.len {
		tc.resolved_fn_value_names[idx] = ''
		tc.resolved_fn_value_set[idx] = false
	}
	if tc.sparse_resolved_fn_values.len > 0 {
		tc.sparse_resolved_fn_values.delete(idx)
	}
}

// direct_dependency_ids returns the checker-resolved function dependency
// identities of a function declaration node. The slice is read-only.
pub fn (tc &TypeChecker) direct_dependency_ids(fn_node_id int) []SymbolId {
	return tc.direct_dependencies_by_fn[fn_node_id] or { []SymbolId{} }
}

// direct_dependencies returns canonical dependency names for compatibility.
pub fn (tc &TypeChecker) direct_dependencies(fn_node_id int) []string {
	ids := tc.direct_dependency_ids(fn_node_id)
	mut names := []string{cap: ids.len}
	for id in ids {
		names << tc.symbol_name(id)
	}
	return names
}

// share_direct_dependencies_from reuses the immutable post-check dependency graph in a
// synchronous compiler stage view. Parallel checker/transform workers must keep their
// private dependency maps so resolution writes cannot race.
pub fn (mut tc TypeChecker) share_direct_dependencies_from(source &TypeChecker) {
	tc.direct_dependencies_by_fn = source.direct_dependencies_by_fn.clone()
	tc.symbols = source.symbols
}

fn (mut tc TypeChecker) record_direct_dependency(id SymbolId) {
	if tc.fn_context.node_id < 0 || id == 0 {
		return
	}
	mut dependencies := tc.direct_dependencies_by_fn[tc.fn_context.node_id] or { []SymbolId{} }
	if id !in dependencies {
		dependencies << id
		tc.direct_dependencies_by_fn[tc.fn_context.node_id] = dependencies
	}
}

fn (tc &TypeChecker) intern_symbol(name string) (SymbolId, string) {
	if isnil(tc.symbols) {
		return SymbolId(0), name
	}
	mut cache := unsafe { tc.type_cache }
	if !isnil(cache) {
		ptr := usize(name.str)
		slot := int((u64(ptr) >> 4 ^ u64(name.len)) & 2047)
		if cache.symbol_recent_set[slot] && cache.symbol_recent_ptrs[slot] == ptr
			&& cache.symbol_recent_lens[slot] == name.len {
			return cache.symbol_recent_ids[slot], cache.symbol_recent_vals[slot]
		}
		mut symbols := unsafe { tc.symbols }
		id, canonical := symbols.intern(name)
		cache.symbol_recent_ptrs[slot] = ptr
		cache.symbol_recent_lens[slot] = name.len
		cache.symbol_recent_ids[slot] = id
		cache.symbol_recent_vals[slot] = canonical
		cache.symbol_recent_set[slot] = true
		return id, canonical
	}
	mut symbols := unsafe { tc.symbols }
	return symbols.intern(name)
}

// intern_scoped_symbol bypasses the pointer-identity hot cache when promoting
// names from a worker arena. Those addresses can be reused after each scoped
// batch, so retaining them in the cache could alias a later, different name.
fn (tc &TypeChecker) intern_scoped_symbol(name string) (SymbolId, string) {
	if isnil(tc.symbols) {
		return SymbolId(0), name
	}
	mut symbols := unsafe { tc.symbols }
	return symbols.intern(name)
}

// symbol_name resolves a checker symbol identity to its canonical name.
pub fn (tc &TypeChecker) symbol_name(id SymbolId) string {
	if isnil(tc.symbols) {
		return ''
	}
	mut symbols := unsafe { tc.symbols }
	return symbols.name(id)
}

// frozen_symbol_name resolves an id after semantic checking has frozen the
// compilation's symbol table. Post-check reachability runs concurrently only
// with read-only transform preparation, so it does not need the interner lock.
pub fn (tc &TypeChecker) frozen_symbol_name(id SymbolId) string {
	if isnil(tc.symbols) {
		return ''
	}
	symbols := unsafe { tc.symbols }
	index := int(id) - 1
	if index < 0 || index >= symbols.names.len {
		return ''
	}
	return symbols.names[index]
}

// canonical_symbol returns the compilation-owned canonical spelling of name.
pub fn (tc &TypeChecker) canonical_symbol(name string) string {
	_, canonical := tc.intern_symbol(name)
	return canonical
}

// symbol_count returns the number of resolved names interned by the checker.
pub fn (tc &TypeChecker) symbol_count() int {
	if isnil(tc.symbols) {
		return 0
	}
	mut symbols := unsafe { tc.symbols }
	return symbols.len()
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
		actual := tc.fn_type_from_key(name) or { return none }
		if tc.fn_value_signature_compatible(actual, expected) {
			return name
		}
		return none
	}
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if tc.fn_value_shadowed_by_value(node) {
		return none
	}
	if key := tc.generic_fn_value_key(node.value) {
		if tc.generic_fn_value_matches_expected(key, expected) {
			tc.remember_resolved_fn_value_chain(id, key)
			return key
		}
	}
	key := tc.fn_value_match_key(node, expected) or { return none }
	tc.remember_resolved_fn_value_chain(id, key)
	return key
}

fn (mut tc TypeChecker) generic_fn_value_matches_expected(key string, expected Type) bool {
	expected_fn := fn_type_from_type(expected) or { return false }
	actual_type := tc.fn_type_from_key(key) or { return false }
	actual_fn := fn_type_from_type(actual_type) or { return false }
	if actual_fn.params.len != expected_fn.params.len {
		return false
	}
	generic_params := tc.fn_generic_params[key] or { return false }
	if generic_params.len == 0 {
		return false
	}
	mut inferred := map[string]Type{}
	for i in 0 .. actual_fn.params.len {
		tc.infer_generic_type_value_from_type(actual_fn.params[i].name(), expected_fn.params[i], generic_params, mut inferred)
	}
	tc.infer_generic_type_value_from_type(actual_fn.return_type.name(), expected_fn.return_type, generic_params, mut inferred)
	mut concrete_types := []Type{cap: generic_params.len}
	for param in generic_params {
		concrete_types << (inferred[param] or { return false })
	}
	mut specialized_params := []Type{cap: actual_fn.params.len}
	for param in actual_fn.params {
		specialized_params << tc.substitute_generic_type_values(param, concrete_types, generic_params)
	}
	specialized := Type(FnType{
		params: specialized_params
		params_mut: actual_fn.params_mut.clone()
		return_type: tc.substitute_generic_type_values(actual_fn.return_type, concrete_types, generic_params)
	})
	return tc.fn_value_signature_compatible(specialized, expected)
}

// remember_resolved_call supports remember resolved call handling for TypeChecker.
fn (mut tc TypeChecker) remember_resolved_call(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	symbol_id, canonical := tc.intern_symbol(name)
	tc.record_direct_dependency(symbol_id)
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.resolved_call_names.len {
			tc.resolved_call_names[idx] = canonical
			tc.resolved_call_set[idx] = true
			return
		}
		tc.sparse_resolved_call_names[idx] = canonical
		return
	}
	if idx >= tc.resolved_call_names.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_call_names.len {
		tc.resolved_call_names[idx] = canonical
		tc.resolved_call_set[idx] = true
	}
}

// remember_resolved_fn_value records the exact function declaration used by a function value.
fn (mut tc TypeChecker) remember_resolved_fn_value(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	symbol_id, canonical := tc.intern_symbol(name)
	tc.record_direct_dependency(symbol_id)
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.resolved_fn_value_names.len {
			tc.resolved_fn_value_names[idx] = canonical
			tc.resolved_fn_value_set[idx] = true
			return
		}
		tc.sparse_resolved_fn_values[idx] = canonical
		return
	}
	if idx >= tc.resolved_fn_value_names.len {
		tc.extend_node_caches(tc.a.nodes.len)
	}
	if idx < tc.resolved_fn_value_names.len {
		tc.resolved_fn_value_names[idx] = canonical
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
	mut memo := tc.body_resolve_memo
	idx := int(id)
	if !isnil(memo) && memo.active && idx >= memo.lo && idx <= memo.hi {
		mi := idx - memo.lo
		// A synthesized annotation can interact with a more specific active
		// smartcast, so force the normal resolution path to choose between them
		// instead of copying either one into the body-local memo.
		memo.filled[mi] = 0
	}
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
	if typ is Void {
		return kind in [.sql_expr, .or_expr]
	}
	if typ is Unknown {
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
	tc.checked_const_names = map[string]bool{}
	tc.check_comptime_struct_updates_preflight()
	tc.collect_selected_file_called_fns()
	tc.check_export_attrs()
	tc.check_import_diagnostics()
	tc.check_c_js_generic_declarations()
	tc.check_duplicate_fn_declarations()
	tc.check_deprecated_byte_types()
	tc.check_interface_reserved_parameter_names()
	tc.check_goto_labels()
	tc.check_labelled_loop_controls()
	tc.cur_module = ''
	tc.cur_file = ''
	blocking_import_files := tc.blocking_import_error_files()
	mut skip_file_semantics := false
	mut previous_top_level := -1
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		range_lo := previous_top_level + 1
		previous_top_level = i
		if node.kind == .file {
			skip_file_semantics = blocking_import_files[node.value]
		} else if skip_file_semantics && node.kind != .module_decl {
			continue
		}
		match node.kind {
			.file {
				tc.enter_file(node.value)
				if !skip_file_semantics {
					tc.check_top_level_file_statements(node)
				}
			}
			.module_decl {
				tc.enter_module(node.value)
				node_id := flat.NodeId(i)
				tc.check_invalid_test_file_name(node_id, node)
				if tc.should_check_source_name(node_id) && !snake_case_name_is_valid(node.value) {
					tc.check_snake_case_name(node_id, node.value, 'module name', tc.declaration_keyword_name_pos(node_id, 'module'))
				}
			}
			.struct_decl {
				node_id := flat.NodeId(i)
				if comma_attr_text_has(node.typ, 'typedef') && !node.value.starts_with('C.') {
					tc.record_error_at(.assignment_mismatch, '`typedef` attribute can only be used with C structs', node_id, tc.declaration_keyword_name_pos(node_id, 'struct'))
				}
				if tc.should_check_source_name(node_id) && !pascal_case_name_is_valid(node.value) {
					tc.check_pascal_case_name(node_id, node.value, 'struct name', tc.declaration_keyword_name_pos(node_id, 'struct'))
				}
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.check_struct_implements(flat.NodeId(i), node)
				tc.check_struct_field_defaults(node_id, node)
			}
			.type_decl, .interface_decl {
				node_id := flat.NodeId(i)
				if node.kind == .interface_decl {
					if tc.should_check_source_name(node_id)
						&& !pascal_case_name_is_valid(node.value) {
						tc.check_pascal_case_name(node_id, node.value, 'interface name', tc.source_line_declaration_pos(node_id))
					}
					tc.check_interface_member_names(node)
				} else {
					type_kind := if node.children_count > 0 {
						'sum type'
					} else if node.typ.starts_with('fn') {
						'fn type'
					} else {
						'type alias'
					}
					if tc.should_check_source_name(node_id)
						&& !pascal_case_name_is_valid(node.value) {
						tc.check_pascal_case_name(node_id, node.value, type_kind, tc.declaration_keyword_name_pos(node_id, 'type'))
					}
					is_c_alias := node.value.starts_with('C.') && node.children_count == 0
						&& split_sum_variant_texts(node.typ).len <= 1
					if !is_c_alias && tc.type_declaration_exists_before(node_id, node.value) {
						kind := if node.children_count > 0
							|| split_sum_variant_texts(node.typ).len > 1 {
							'sum type'
						} else {
							'alias'
						}
						tc.record_error_at(.duplicate_decl, 'cannot register ${kind} `${node.value}`, another type with this name exists', node_id, tc.declaration_keyword_name_pos(node_id, 'type'))
					}
				}
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.enum_decl {
				node_id := flat.NodeId(i)
				if tc.should_check_source_name(node_id) && !pascal_case_name_is_valid(node.value) {
					tc.check_pascal_case_name(node_id, node.value, 'enum name', tc.declaration_keyword_name_pos(node_id, 'enum'))
				}
				tc.check_enum_backing_type(flat.NodeId(i), node)
				tc.check_enum_field_values(flat.NodeId(i), node)
			}
			.const_decl {
				tc.check_const_field_values(node)
			}
			.global_decl {
				if !tc.enable_globals && !tc.has_globals_files[tc.cur_file] {
					tc.record_error_at(.duplicate_decl, 'use `v -enable-globals ...` to enable globals', flat.NodeId(i), node.pos)
				}
				tc.check_const_global_initializers(node)
			}
			.fn_decl {
				tc.check_fn_declaration_name(flat.NodeId(i), node)
				tc.check_method_field_name_collision(flat.NodeId(i), node)
				tc.check_main_fn_signature(flat.NodeId(i), node)
				tc.check_init_fn_signature(flat.NodeId(i), node)
				tc.check_str_method_signature(flat.NodeId(i), node)
				tc.check_free_method_signature(flat.NodeId(i), node)
				tc.check_sumtype_builtin_method_override(flat.NodeId(i), node)
				tc.check_test_fn_signature(flat.NodeId(i), node)
				tc.check_decl_type_strings(flat.NodeId(i), node)
				if tc.scope_parallel_check_workers {
					tc.check_fn_decl_semantics_scoped(i, range_lo, tc.cur_file, tc.cur_module)
				} else {
					tc.check_fn_decl_semantics(i, node, tc.cur_file, tc.cur_module)
				}
			}
			.c_fn_decl {
				tc.check_main_fn_signature(flat.NodeId(i), node)
				if tc.reject_unsupported_generics {
					tc.check_decl_type_strings(flat.NodeId(i), node)
				}
			}
			else {}
		}

		_ = i
	}
	tc.check_test_file_has_test_fn()
	tc.check_array_decompose_counts()
	tc.check_selective_builtin_import_diagnostics()
	tc.check_unused_import_diagnostics()
	tc.discard_cascading_fn_redefinition_diagnostics()
	tc.notices.sort_with_compare(compare_type_notices)
	// All ordinary source annotations have now been validated with module-strict
	// lookup. Later transform/codegen stages also parse synthesized generic type
	// text, where concrete arguments can legitimately come from another module.
	tc.direct_parent_index_trusted = false
	tc.resolution_type_mode = true
}

fn (mut tc TypeChecker) discard_cascading_fn_redefinition_diagnostics() {
	mut suppressed_groups := map[string]bool{}
	mut previous_top_level := -1
	for index in tc.top_level_idx {
		range_lo := previous_top_level + 1
		previous_top_level = index
		node := tc.a.nodes[index]
		if node.kind != .fn_decl
			|| !tc.errors.any(it.severity == 'conflicting declaration:' && int(it.node) == index) {
			continue
		}
		has_signature_error := tc.fn_declaration_signature_has_semantic_error(node)
		// The exact-output fixture runner preserves v1's omission of duplicate-function
		// diagnostics once that declaration already has another semantic error.
		has_fixture_compatibility_error := tc.checker_fixture_mode
			&& tc.fn_declaration_range_has_semantic_error(node, range_lo, index)
		if has_signature_error || has_fixture_compatibility_error {
			suppressed_groups[tc.fn_declaration_group_key(node)] = true
		}
	}
	if suppressed_groups.len == 0 {
		return
	}
	mut i := tc.errors.len
	for i > 0 {
		i--
		diagnostic := tc.errors[i]
		is_builder := diagnostic.severity == 'builder error:'
			&& diagnostic.msg.starts_with('redefinition of function `')
			&& suppressed_groups[diagnostic.node_value]
		is_conflict := diagnostic.severity == 'conflicting declaration:'
			&& tc.valid_node_id(diagnostic.node) && tc.a.node(diagnostic.node).kind == .fn_decl
			&& suppressed_groups[tc.fn_declaration_group_key(tc.a.node(diagnostic.node))]
		if is_builder || is_conflict {
			tc.errors.delete(i)
		}
	}
}

fn (tc &TypeChecker) fn_declaration_signature_has_semantic_error(node flat.Node) bool {
	header := tc.fn_declaration_diagnostic_pos(node)
	for diagnostic in tc.errors {
		if diagnostic.kind == .duplicate_decl || diagnostic.pos.id != header.id {
			continue
		}
		if diagnostic.pos.offset >= header.offset && diagnostic.pos.offset < header.end {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_declaration_range_has_semantic_error(node flat.Node, range_lo int, range_hi int) bool {
	for diagnostic in tc.errors {
		if diagnostic.kind == .duplicate_decl {
			continue
		}
		node_index := int(diagnostic.node)
		if node_index >= range_lo && node_index <= range_hi {
			return true
		}
		if diagnostic.pos.id == node.pos.id && diagnostic.pos.offset >= node.pos.offset
			&& diagnostic.pos.offset < node.pos.end {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_declaration_group_key(node flat.Node) string {
	file := tc.a.source_files[node.pos.id] or { return node.value }
	module_name := tc.file_modules[file.name] or { '' }
	return checker_qualified_fn_name(module_name, node.value)
}

fn (mut tc TypeChecker) check_fn_decl_semantics_scoped(fn_idx int, range_lo int, file string, module_name string) {
	item := CheckWorkItem{
		fn_idx: fn_idx
		range_lo: range_lo
		file: file
		module: module_name
	}
	scope := check_worker_scope_begin(true)
	mut scoped := tc.fork_for_parallel_check()
	scoped.check_fn_items_serial([item])
	check_worker_scope_leave(scope)
	tc.clone_parallel_worker_node_caches([item])
	tc.merge_parallel_check_worker_scoped(scoped, true)
	check_worker_scope_free(scope)
}

fn (tc &TypeChecker) blocking_import_error_files() map[string]bool {
	mut files := map[string]bool{}
	mut file := ''
	mut module_name := ''
	for idx in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(idx))
		if node.kind == .file {
			file = node.value
			module_name = tc.file_modules[file] or { '' }
			continue
		}
		if node.kind != .import_decl {
			continue
		}
		if _ := tc.a.missing_imports[idx] {
			files[file] = true
		}
		if node.typ == module_name {
			files[file] = true
		}
	}
	return files
}

fn (mut tc TypeChecker) check_selective_builtin_import_diagnostics() {
	for idx in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(idx))
		if node.kind == .file {
			tc.enter_file(node.value)
			continue
		}
		if node.kind == .module_decl {
			tc.enter_module(node.value)
			continue
		}
		if node.kind != .import_decl {
			continue
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(node, i)
			child := tc.a.node(child_id)
			if child.kind == .ident && is_builtin_type_name(child.value) {
				tc.record_error_at(.unknown_type, 'cannot import or override builtin type', child_id, tc.node_value_diagnostic_pos(child_id))
			}
		}
	}
}

fn (mut tc TypeChecker) check_c_js_generic_declarations() {
	for declaration_kind in [flat.NodeKind.struct_decl, flat.NodeKind.fn_decl] {
		tc.cur_module = ''
		tc.cur_file = ''
		for index in tc.top_level_idx {
			node := tc.a.node(flat.NodeId(index))
			if node.kind == .file {
				tc.enter_file(node.value)
				continue
			}
			if node.kind == .module_decl {
				tc.enter_module(node.value)
				continue
			}
			is_function := declaration_kind == .fn_decl
			if (is_function && node.kind !in [.fn_decl, .c_fn_decl])
				|| (!is_function && node.kind != .struct_decl) {
				continue
			}
			if node.generic_params().len == 0 && !node.value.contains('[') {
				continue
			}
			namespace := tc.c_js_declaration_namespace(flat.NodeId(index), node) or { continue }
			type_kind := if is_function { 'functions' } else { 'structs' }
			pos := if is_function {
				tc.source_line_declaration_pos(flat.NodeId(index))
			} else {
				tc.generic_declaration_head_pos(flat.NodeId(index))
			}
			tc.record_error_at(.unsupported_generic, '${namespace} ${type_kind} cannot be declared as generic', flat.NodeId(index), pos)
		}
	}
}

fn (tc &TypeChecker) c_js_declaration_namespace(id flat.NodeId, node &flat.Node) ?string {
	if node.value.starts_with('C.') {
		return 'C'
	}
	if node.value.starts_with('JS.') {
		return 'JS'
	}
	pos := tc.source_line_declaration_pos(id)
	file := tc.a.source_files[pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	if pos.offset < 0 || pos.end > source.len || pos.offset >= pos.end {
		return none
	}
	line := source[pos.offset..pos.end]
	if line.starts_with('fn C.') {
		return 'C'
	}
	if line.starts_with('fn JS.') {
		return 'JS'
	}
	return none
}

fn (tc &TypeChecker) generic_declaration_head_pos(id flat.NodeId) token.Pos {
	pos := tc.source_line_declaration_pos(id)
	file := tc.a.source_files[pos.id] or { return pos }
	source := tc.source_texts_by_file[file.name] or { return pos }
	if pos.offset < 0 || pos.end > source.len || pos.offset >= pos.end {
		return pos
	}
	line := source[pos.offset..pos.end]
	bracket := line.index_u8(`[`)
	if bracket <= 0 {
		return pos
	}
	return token.new_span(pos.id, pos.offset, pos.offset + bracket)
}

fn (mut tc TypeChecker) check_duplicate_fn_declarations() {
	mut groups := map[string][]int{}
	mut cur_module := ''
	for index in tc.top_level_idx {
		node := tc.a.nodes[index]
		if node.kind == .file {
			cur_module = tc.file_modules[node.value] or { '' }
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		key := checker_qualified_fn_name(cur_module, node.value)
		mut indexes := groups[key] or { []int{} }
		indexes << index
		groups[key] = indexes
	}
	mut keys := groups.keys()
	keys.sort()
	for key in keys {
		indexes := groups[key]
		if indexes.len < 2 || !indexes.any(it >= tc.a.user_code_start
			&& tc.node_is_in_selected_input_file(flat.NodeId(it))) {
			continue
		}
		display_name := tc.a.nodes[indexes[0]].value
		if display_name.all_after_last('.') == 'init'
			|| tc.fn_group_name_conflicts_with_import(indexes, display_name.all_after_last('.'))
			|| tc.fn_group_contains_builtin_declaration(indexes) {
			continue
		}
		tc.errors << TypeError{
			msg: 'redefinition of function `${display_name}`'
			kind: .duplicate_decl
			node: flat.empty_node
			node_value: key
			severity: 'builder error:'
		}
		for index in indexes {
			node_id := flat.NodeId(index)
			node := tc.a.nodes[index]
			pos := tc.fn_declaration_diagnostic_pos(node)
			base := tc.make_type_error_at(.duplicate_decl, tc.fn_declaration_source_signature(node, pos), node_id, pos)
			tc.errors << TypeError{
				...base
				severity: 'conflicting declaration:'
			}
		}
	}
}

fn (tc &TypeChecker) fn_group_contains_builtin_declaration(indexes []int) bool {
	for index in indexes {
		node := tc.a.nodes[index]
		file := tc.a.source_files[node.pos.id] or { continue }
		module_name := tc.file_modules[file.name] or { continue }
		if module_name == 'builtin' {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_group_name_conflicts_with_import(indexes []int, name string) bool {
	for index in indexes {
		node := tc.a.nodes[index]
		file := tc.a.source_files[node.pos.id] or { continue }
		info := tc.file_imports_by_file[file.name] or { continue }
		if name in info.imports {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) node_is_in_selected_input_file(id flat.NodeId) bool {
	if tc.diagnostic_files.len == 0 {
		return true
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return false }
	return file.name in tc.diagnostic_files
}

fn (tc &TypeChecker) fn_declaration_source_signature(node flat.Node, pos token.Pos) string {
	file := tc.a.source_files[pos.id] or { return 'fn ${node.value}()' }
	source := tc.source_texts_by_file[file.name] or { return 'fn ${node.value}()' }
	if pos.offset < 0 || pos.end > source.len || pos.offset >= pos.end {
		return 'fn ${node.value}()'
	}
	return source[pos.offset..pos.end].trim_space()
}

fn (mut tc TypeChecker) check_goto_labels() {
	if !tc.has_goto_nodes {
		return
	}
	for index in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(index))
		if node.kind == .fn_decl {
			tc.check_goto_boundary(flat.NodeId(index))
		}
	}
	for index in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(index))
		if node.kind == .file {
			tc.check_goto_boundary(flat.NodeId(index))
		}
	}
}

fn (mut tc TypeChecker) check_goto_boundary(id flat.NodeId) {
	mut labels := map[string]bool{}
	tc.collect_goto_boundary_labels(id, mut labels)
	tc.validate_goto_boundary_nodes(id, false, labels)
}

fn (tc &TypeChecker) collect_goto_boundary_labels(id flat.NodeId, mut labels map[string]bool) {
	node := tc.a.node(id)
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			continue
		}
		if child.kind == .label_stmt {
			labels[child.value] = true
		}
		tc.collect_goto_boundary_labels(child_id, mut labels)
	}
}

fn (mut tc TypeChecker) validate_goto_boundary_nodes(id flat.NodeId, inside_unsafe bool, labels map[string]bool) {
	node := tc.a.node(id)
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind in [.fn_literal, .lambda_expr] {
			tc.check_goto_boundary(child_id)
			continue
		}
		if child.kind == .fn_decl {
			continue
		}
		if child.kind == .goto_stmt {
			if !inside_unsafe && !tc.node_is_in_translated_file(child_id) {
				tc.record_goto_diagnostic(child_id, true, '`goto` requires `unsafe` (consider using labelled break/continue)')
			}
			if child.value !in labels {
				tc.record_goto_diagnostic(child_id, false, 'unknown label `${child.value}`')
			}
		}
		child_inside_unsafe := inside_unsafe || (child.kind == .block && child.value == 'unsafe')
		tc.validate_goto_boundary_nodes(child_id, child_inside_unsafe, labels)
	}
}

fn (mut tc TypeChecker) record_goto_diagnostic(id flat.NodeId, warning bool, message string) {
	if int(id) < tc.a.user_code_start || !tc.node_is_in_selected_input_file(id) {
		return
	}
	node := tc.a.node(id)
	base := tc.make_type_error_at(.unknown_ident, message, id, node.pos)
	if warning {
		if tc.warns_are_errors {
			tc.errors << TypeError{
				...base
				severity: 'error:'
			}
		} else {
			tc.notices << TypeError{
				...base
				severity: 'warning:'
			}
		}
	} else {
		tc.errors << base
	}
}

fn (mut tc TypeChecker) check_interface_reserved_parameter_names() {
	tc.cur_module = ''
	tc.cur_file = ''
	for index in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(index))
		if node.kind == .file {
			tc.enter_file(node.value)
			continue
		}
		if node.kind == .module_decl {
			tc.enter_module(node.value)
			continue
		}
		if node.kind != .interface_decl {
			continue
		}
		tc.check_interface_field_method_collisions(node)
		for i in 0 .. node.children_count {
			field := tc.a.child_node(node, i)
			if field.kind != .interface_field {
				continue
			}
			for j in 0 .. field.children_count {
				tc.check_reserved_parameter_name(tc.a.child(field, j))
			}
		}
	}
}

fn (mut tc TypeChecker) check_str_method_signature(id flat.NodeId, node flat.Node) {
	if !tc.should_check_source_name(id) || !node.value.ends_with('.str') {
		return
	}
	mut explicit_params := 0
	for i in 0 .. node.children_count {
		param := tc.a.child_node(&node, i)
		if param.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		if param.op != .dot {
			explicit_params++
		}
	}
	if !unalias_type(tc.parse_type(node.typ)).is_string() {
		tc.record_error_at(.return_mismatch, '.str() methods should return `string`', id, tc.fn_declaration_diagnostic_pos(node))
	}
	if explicit_params != 0 {
		tc.record_error_at(.call_arg_mismatch, '.str() methods should have 0 arguments', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (mut tc TypeChecker) check_free_method_signature(id flat.NodeId, node flat.Node) {
	if !tc.should_check_source_name(id) || !node.value.ends_with('.free') {
		return
	}
	mut receiver_id := flat.empty_node
	mut explicit_params := 0
	for i in 0 .. node.children_count {
		param_id := tc.a.child(&node, i)
		param := tc.a.node(param_id)
		if param.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		if param.op == .dot {
			receiver_id = param_id
		} else {
			explicit_params++
		}
	}
	if tc.valid_node_id(receiver_id) {
		receiver := tc.a.node(receiver_id)
		receiver_type := unalias_type(tc.parse_type(receiver.typ))
		if receiver_type !is Pointer && !receiver.is_mut {
			type_name := receiver_type.name()
			_, receiver_pos := tc.fn_receiver_source_text_pos(node)
			pos := if receiver_pos.end > receiver_pos.offset + 2 {
				token.new_span(receiver_pos.id, receiver_pos.offset + 1, receiver_pos.end - 1)
			} else {
				receiver_pos
			}
			tc.record_error_at(.call_arg_mismatch, '`.free()` methods should be defined on either a `(mut x &${type_name})`, or a `(x &${type_name})` receiver', id, pos)
		}
	}
	if unalias_type(tc.parse_type(node.typ)) !is Void {
		tc.record_error_at(.return_mismatch, '`.free()` methods should not have a return type', id, tc.fn_return_type_diagnostic_pos(node))
	}
	if explicit_params != 0 {
		tc.record_error_at(.call_arg_mismatch, '`.free()` methods should have 0 arguments', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (tc &TypeChecker) should_check_source_name(id flat.NodeId) bool {
	if int(id) < tc.a.user_code_start {
		return false
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or {
		if tc.translated_files[tc.cur_file] {
			return false
		}
		return tc.diagnostic_files.len == 0 || tc.cur_file in tc.diagnostic_files
	}
	if tc.translated_files[file.name] {
		return false
	}
	return tc.diagnostic_files.len == 0 || file.name in tc.diagnostic_files
}

fn snake_case_name_is_valid(name string) bool {
	if name.starts_with('C.') || name.starts_with('JS.') {
		return true
	}
	return (name.len <= 1 || (name[0] != `_` && !name.contains('._')))
		&& !util.contains_capital(name)
}

fn pascal_case_name_is_valid(name string) bool {
	if name.starts_with('C.') || name.starts_with('JS.') {
		return true
	}
	short_name := name.all_after_last('.')
	return short_name.len == 0 || short_name[0].is_capital()
}

fn (mut tc TypeChecker) check_invalid_test_file_name(id flat.NodeId, node flat.Node) {
	if node.value == 'main' || !tc.should_check_source_name(id) {
		return
	}
	base := os.file_name(tc.cur_file)
	if !base.starts_with('test_') || base.ends_with('_test.v') {
		return
	}
	tc.record_error_with_details_at(.unknown_type, 'invalid test file name `${base}`', id, tc.source_line_declaration_pos(id), [
		'Test files should have names ending with `_test.v`.',
	])
}

fn (mut tc TypeChecker) check_snake_case_name(id flat.NodeId, name string, identifier string, pos token.Pos) {
	if tc.translated_files[tc.cur_file] || name.starts_with('C.') || name.starts_with('JS.') {
		return
	}
	if name.starts_with('__v3_') || source_name_is_numbered_string_symbol(name) {
		return
	}
	if name.len > 1 && (name[0] == `_` || name.contains('._')) {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot start with `_`', id, pos)
	}
	if util.contains_capital(name) {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot contain uppercase letters, use snake_case instead', id, pos)
	}
}

fn source_name_is_numbered_string_symbol(name string) bool {
	return name.len > 5 && name.starts_with('_str_') && name[5..].bytes().all(it >= `0`
		&& it <= `9`)
}

fn (mut tc TypeChecker) check_pascal_case_name(id flat.NodeId, name string, identifier string, pos token.Pos) {
	if tc.translated_files[tc.cur_file] || name.starts_with('C.') || name.starts_with('JS.') {
		return
	}
	short_name := name.all_after_last('.')
	if short_name.len > 0 && !short_name[0].is_capital() {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` must begin with capital letter', id, pos)
	}
}

fn (mut tc TypeChecker) check_fn_declaration_name(id flat.NodeId, node flat.Node) {
	if !tc.should_check_source_name(id) || local_fn_decl_is_transform_created(node.value)
		|| tc.a.specialized_fn_nodes[int(id)] {
		return
	}
	tc.check_fn_if_attribute_return(id, node)
	tc.check_imported_module_prefix(id, node.value, 'fn')
	name := node.value.all_after_last('.')
	if !node.value.contains('.') && tc.cur_module in ['', 'main'] && is_builtin_type_name(name) {
		tc.record_error_at(.duplicate_decl, 'top level declaration cannot shadow builtin type', id, tc.fn_declaration_diagnostic_pos(node))
	}
	// V1 treats os and strconv like builtin modules. Their long-standing private
	// implementation methods intentionally use a leading underscore.
	source_module := if file := tc.a.source_files[node.pos.id] {
		tc.file_modules[file.name] or { '' }
	} else {
		''
	}
	if source_module in ['os', 'strconv'] {
		return
	}
	if name.len == 0 || (!name[0].is_letter() && name[0] != `_`) || snake_case_name_is_valid(name) {
		return
	}
	tc.check_snake_case_name(id, name, if node.value.contains('.') {
		'method name'
	} else {
		'function name'
	}, tc.fn_declaration_diagnostic_pos(node))
}

fn (mut tc TypeChecker) check_fn_if_attribute_return(id flat.NodeId, node flat.Node) {
	if tc.parse_type(node.typ) is Void {
		return
	}
	header_pos := tc.fn_declaration_diagnostic_pos(node)
	file := tc.a.source_files[header_pos.id] or { return }
	source := tc.source_texts_by_file[file.name] or { return }
	mut line_start := int_min(int_max(header_pos.offset, 0), source.len)
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	mut cursor := line_start
	for cursor > 0 {
		mut previous_start := cursor - 1
		for previous_start > 0 && source[previous_start - 1] != `\n` {
			previous_start--
		}
		line := source[previous_start..cursor].trim_space()
		if line.len == 0 {
			cursor = previous_start
			continue
		}
		if line.starts_with('@[if ') && line.ends_with(']') {
			condition := line[5..line.len - 1].trim_space()
			tc.record_error_at(.return_mismatch, 'only functions that do NOT return values can have `@[if ${condition}]` tags', id, header_pos)
		}
		return
	}
}

fn (mut tc TypeChecker) check_main_fn_signature(id flat.NodeId, node flat.Node) {
	if node.value != 'main' || tc.cur_module !in ['', 'main'] || !tc.should_check_source_name(id) {
		return
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			tc.record_error_at(.return_mismatch, 'function `main` cannot have arguments', id, tc.fn_header_declaration_pos(id))
			break
		}
		if tc.prefix_param_scan {
			break
		}
	}
	if tc.parse_type(node.typ) !is Void {
		tc.record_error_at(.return_mismatch, 'function `main` cannot return values', id, tc.fn_header_declaration_pos(id))
	}
	if node.kind == .c_fn_decl {
		tc.record_error_at(.return_mismatch, 'function `main` must declare a body', id, tc.fn_header_declaration_pos(id))
	}
}

fn (mut tc TypeChecker) check_init_fn_signature(id flat.NodeId, node flat.Node) {
	if node.value != 'init' || !tc.should_check_source_name(id) {
		return
	}
	// An `init()` hook has no parameters. A public API can still use the
	// ordinary name `init` when it takes arguments (for example `term.ui.init`).
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			return
		}
		if tc.prefix_param_scan {
			break
		}
	}
	pos := tc.fn_header_declaration_pos(id)
	if node.op == .arrow {
		tc.record_error_at(.return_mismatch, 'fn `init` must not be public', id, pos)
	}
	if tc.parse_type(node.typ) !is Void {
		tc.record_error_at(.return_mismatch, 'fn `init` cannot have a return type', id, pos)
	}
}

fn (tc &TypeChecker) fn_header_declaration_pos(id flat.NodeId) token.Pos {
	pos := tc.source_line_declaration_pos(id)
	file := tc.a.source_files[pos.id] or { return pos }
	source := tc.source_texts_by_file[file.name] or { return pos }
	mut end := int_min(pos.end, source.len)
	if end > pos.offset && source[end - 1] == `{` {
		end--
		for end > pos.offset && source[end - 1] in [` `, `\t`] {
			end--
		}
	}
	return token.new_span(pos.id, pos.offset, end)
}

fn (mut tc TypeChecker) check_fn_receiver_syntax(id flat.NodeId, node flat.Node) {
	if !node.value.contains('.') || !tc.should_check_source_name(id) {
		return
	}
	text, pos := tc.fn_receiver_source_text_pos(node)
	if text.len < 2 {
		return
	}
	parts := text[1..text.len - 1].fields()
	if parts.len < 3 {
		return
	}
	if parts[0] == 'mut' && parts[2].starts_with('&') {
		tc.record_error_at(.call_arg_mismatch, 'use `(mut ${parts[1]} ${parts[2][1..]})` or `(${parts[1]} ${parts[2]})` instead of `${text}`', id, pos)
	} else if parts[1] == 'mut' {
		tc.record_warning_at(.call_arg_mismatch, 'use `(mut ${parts[0]} ${parts[2]})` instead of `${text}`', id, pos)
	}
}

fn (tc &TypeChecker) fn_receiver_source_text_pos(node flat.Node) (string, token.Pos) {
	file := tc.a.source_files[node.pos.id] or { return '', node.pos }
	source := tc.source_texts_by_file[file.name] or { return '', node.pos }
	name_start := int_min(int_max(node.pos.offset, 0), source.len)
	relative := last_index_between(source, '\n', 0, name_start)
	line_start := if relative >= 0 {
		relative + 1
	} else {
		0
	}
	header := source[line_start..name_start]
	fn_relative := header.index('fn') or { return '', node.pos }
	open_relative := header[fn_relative..].index('(') or { return '', node.pos }
	open := line_start + fn_relative + open_relative
	close_relative := source[open..name_start].index(')') or { return '', node.pos }
	close := open + close_relative + 1
	return source[open..close], token.new_span(node.pos.id, open, close)
}

fn (mut tc TypeChecker) check_sumtype_builtin_method_override(id flat.NodeId, node flat.Node) {
	if !tc.should_check_source_name(id) || tc.cur_module == 'builtin' || !node.value.contains('.') {
		return
	}
	method := node.value.all_after_last('.')
	if method !in ['type_idx', 'type_name'] {
		return
	}
	receiver := node.value.all_before_last('.')
	qualified := tc.qualify_name(receiver)
	if receiver in tc.sum_types || qualified in tc.sum_types {
		tc.record_error_at(.duplicate_decl, 'method overrides built-in sum type method', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (mut tc TypeChecker) check_interface_member_names(node flat.Node) {
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.node(field_id)
		if field.kind != .interface_field || field.value.len == 0 {
			continue
		}
		if field.op != .dot && field.typ.len == 0 {
			// Embedded interfaces use their type name as `value`; they are not fields.
			continue
		}
		if !tc.should_check_source_name(field_id) || snake_case_name_is_valid(field.value) {
			continue
		}
		identifier := if field.op == .dot { 'method name' } else { 'field name' }
		pos := if field.op == .dot {
			tc.source_line_declaration_pos(field_id)
		} else {
			tc.node_value_diagnostic_pos(field_id)
		}
		tc.check_snake_case_name(field_id, field.value, identifier, pos)
	}
}

fn (mut tc TypeChecker) check_interface_field_method_collisions(node flat.Node) {
	mut field_names := map[string]bool{}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind == .interface_field && field.op != .dot && field.typ.len > 0
			&& unalias_type(tc.parse_type(field.typ)) is FnType {
			field_names[field.value] = true
		}
	}
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.node(field_id)
		if field.kind == .interface_field && field.op == .dot && field_names[field.value] {
			tc.record_error_at(.duplicate_decl, 'type `${node.value}` has both field and method named `${field.value}`', field_id, tc.source_line_declaration_pos(field_id))
		}
	}
}

fn (mut tc TypeChecker) check_method_field_name_collision(id flat.NodeId, node flat.Node) {
	if !node.value.contains('.') || !tc.should_check_source_name(id) {
		return
	}
	receiver := node.value.all_before_last('.')
	method := node.value.all_after_last('.')
	qualified_receiver := tc.qualify_name(receiver)
	fields := tc.structs[qualified_receiver] or { tc.structs[receiver] or { return } }
	if fields.any(it.name == method && unalias_type(it.typ) is FnType)
		&& !tc.struct_has_invalid_reference_default(receiver) {
		tc.record_error_at(.duplicate_decl, 'type `${receiver.all_after_last('.')}` has both field and method named `${method}`', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (tc &TypeChecker) struct_has_invalid_reference_default(receiver string) bool {
	decl := tc.source_struct_decl_for_name(receiver) or { return false }
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl || field.children_count == 0 {
			continue
		}
		default_id := tc.a.child(field, 0)
		if tc.errors.any(it.node == default_id
			&& it.msg == 'field is not reference but default value is reference') {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_reserved_parameter_name(id flat.NodeId) {
	if !tc.valid_node_id(id) || !tc.should_check_source_name(id) {
		return
	}
	param := tc.a.node(id)
	if param.kind != .param || param.value.len == 0 || !reserved_const_type_name(param.value) {
		return
	}
	tc.record_error_at(.duplicate_decl, 'invalid use of reserved type `${param.value}` as a parameter name', id, param.pos)
}

fn (tc &TypeChecker) declaration_keyword_name_pos(node_id flat.NodeId, keyword string) token.Pos {
	if !tc.valid_node_id(node_id) {
		return token.Pos{}
	}
	node := tc.a.node(node_id)
	name_pos := tc.node_value_diagnostic_pos(node_id)
	file := tc.a.source_files[name_pos.id] or { return name_pos }
	source := tc.source_texts_by_file[file.name] or { return name_pos }
	mut line_start := int_min(name_pos.offset, source.len)
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	name_end := int_min(name_pos.end, source.len)
	prefix := source[line_start..name_end]
	keyword_relative := prefix.last_index(keyword) or { return name_pos }
	keyword_start := line_start + keyword_relative
	mut start := keyword_start
	if prefix[..keyword_relative].trim_space() == 'pub' {
		start = line_start + (prefix.index('pub') or { keyword_relative })
	}
	mut name_start := keyword_start + keyword.len
	for name_start < source.len && source[name_start] in [` `, `\t`] {
		name_start++
	}
	end := name_start + node.value.all_after_last('.').len
	return token.new_span(name_pos.id, start, int_min(end, source.len))
}

fn (tc &TypeChecker) source_line_declaration_pos(node_id flat.NodeId) token.Pos {
	if !tc.valid_node_id(node_id) {
		return token.Pos{}
	}
	name_pos := tc.node_value_diagnostic_pos(node_id)
	file := tc.a.source_files[name_pos.id] or { return name_pos }
	source := tc.source_texts_by_file[file.name] or { return name_pos }
	mut start := int_min(name_pos.offset, source.len)
	for start > 0 && source[start - 1] != `\n` {
		start--
	}
	mut end := source.index_after('\n', start) or { source.len }
	for start < end && source[start] in [` `, `\t`] {
		start++
	}
	for end > start && source[end - 1] in [`\n`, `\r`, ` `, `\t`] {
		end--
	}
	return token.new_span(name_pos.id, start, end)
}

fn (mut tc TypeChecker) check_test_fn_signature(id flat.NodeId, node flat.Node) {
	source_file := tc.a.source_files[node.pos.id] or { return }
	if !is_regular_v_test_file(source_file.name) || !is_v_test_fn_name(node.value) {
		return
	}
	mut param_count := 0
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		param_count++
	}
	if param_count != 0 {
		tc.record_error_at(.call_arg_mismatch, 'invalid test signature: test functions should take 0 parameters', id, tc.fn_declaration_diagnostic_pos(node))
	}
	return_type := trimmed_space(node.typ)
	if return_type.len > 0 && return_type !in ['void', '?', '!'] && !return_type.starts_with('?void') && !return_type.starts_with('!void') {
		tc.record_error_at(.return_mismatch, 'invalid test signature: test functions should either return nothing at all, or be marked to return `?` or `!`', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (mut tc TypeChecker) check_test_file_has_test_fn() {
	mut has_test_file := false
	mut has_test_fn := false
	mut first_source_node := flat.NodeId(-1)
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		if node.kind == .file && is_regular_v_test_file(node.value) {
			has_test_file = true
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		source_file := tc.a.source_files[node.pos.id] or { continue }
		if !is_regular_v_test_file(source_file.name) {
			continue
		}
		if int(first_source_node) < 0 {
			first_source_node = flat.NodeId(i)
		}
		if is_v_test_fn_name(node.value) {
			has_test_fn = true
		}
	}
	if !has_test_file || has_test_fn {
		return
	}
	first_node := tc.a.nodes[int(first_source_node)]
	mut pos := tc.fn_declaration_diagnostic_pos(first_node)
	if pos.is_valid() {
		pos = token.new_span(pos.id, pos.offset, pos.offset + 1)
	}
	source_file := tc.a.source_files[pos.id] or { return }
	saved_file := tc.cur_file
	tc.cur_file = source_file.name
	tc.record_error_with_details_at(.unknown_fn, 'a _test.v file should have *at least* one `test_` function', first_source_node, pos, [
		'The name of a test function in V, should start with `test_`.',
		'The test function should take 0 parameters, and no return type. Example:',
		'fn test_xyz(){ assert 2 + 2 == 4 }',
	])
	tc.cur_file = saved_file
}

fn (tc &TypeChecker) fn_declaration_diagnostic_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	position := file.position(node.pos)
	line_start := file.line_start(position.line)
	line_end := source.index_after('\n', line_start) or { source.len }
	if line_start < line_end {
		line := source[line_start..line_end]
		fn_relative := line.index('fn ') or { return node.pos }
		open_relative := line[fn_relative..].index('{') or { return node.pos }
		start := line_start + fn_relative
		header := line[fn_relative..fn_relative + open_relative].trim_right(' \t')
		return token.new_span(node.pos.id, start, start + header.len)
	}
	return node.pos
}

fn is_v_test_fn_name(name string) bool {
	return name.starts_with('test_') || name.starts_with('testsuite_')
		|| name in ['before_each', 'after_each', 'before_all', 'after_all']
}

fn (mut tc TypeChecker) check_top_level_file_statements(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	saved_fn_context := tc.fn_context
	saved_scope := tc.cur_scope
	saved_return_type := tc.cur_fn_ret_type
	tc.fn_context = new_function_check_context()
	tc.cur_scope = tc.file_scope
	tc.cur_fn_ret_type = Type(void_)
	mut reported_malformed_const_lines := map[int]bool{}
	mut reported_unexpected_name_lines := map[int]bool{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind in [.comptime_if, .block] {
			tc.check_top_level_stmt_node(child_id)
			continue
		}
		if bad_pos := tc.malformed_const_keyword_pos(child_id) {
			if !reported_malformed_const_lines[bad_pos.offset] {
				tc.record_error_at(.unknown_ident, 'unexpected name `cosnt`', child_id, bad_pos)
				reported_malformed_const_lines[bad_pos.offset] = true
			}
			continue
		}
		if is_top_level_statement_kind(child.kind) {
			if unexpected_pos := tc.unexpected_top_level_name_pos(child_id) {
				if !reported_unexpected_name_lines[unexpected_pos.offset] {
					file := tc.a.source_files[unexpected_pos.id] or { continue }
					source := tc.source_texts_by_file[file.name] or { continue }
					name := source[unexpected_pos.offset..unexpected_pos.end]
					tc.record_error_at(.unknown_ident, 'unexpected name `${name}`', child_id, unexpected_pos)
					reported_unexpected_name_lines[unexpected_pos.offset] = true
				}
				continue
			}
		}
		if is_top_level_statement_kind(child.kind) && !tc.node_is_on_import_line(child_id) {
			tc.check_top_level_stmt_node(child_id)
		}
	}
	tc.record_unused_top_level_vars(node)
	tc.fn_context = saved_fn_context
	tc.cur_scope = saved_scope
	tc.cur_fn_ret_type = saved_return_type
}

fn (mut tc TypeChecker) check_top_level_stmt_node(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	// A compile-time-selected top-level branch is stored as a block, but its
	// executable statements belong to the synthetic main's file scope. Flatten
	// it here just as cgen does, while leaving declarations to their normal
	// top-level declaration/body passes.
	if node.kind in [.block, .comptime_if] {
		for i in 0 .. node.children_count {
			child_id := tc.a.child(node, i)
			child := tc.a.node(child_id)
			if is_top_level_statement_kind(child.kind) && !tc.node_is_on_import_line(child_id) {
				tc.check_top_level_stmt_node(child_id)
			}
		}
		return
	}
	tc.check_stmt_node(id)
}

fn (tc &TypeChecker) unexpected_top_level_name_pos(id flat.NodeId) ?token.Pos {
	if !tc.valid_node_id(id) {
		return none
	}
	pos := tc.a.node(id).pos
	file := tc.a.source_files[pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	mut line_start := int_min(int_max(pos.offset, 0), source.len)
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	line_end := source.index_after('\n', line_start) or { source.len }
	mut start := line_start
	for start < line_end && source[start] in [` `, `\t`] {
		start++
	}
	if start >= line_end || !source[start].is_capital() {
		return none
	}
	mut end := start + 1
	for end < line_end && (source[end].is_letter() || source[end].is_digit() || source[end] == `_`) {
		end++
	}
	if end >= line_end || source[end] !in [` `, `\t`] {
		return none
	}
	mut next := end
	for next < line_end && source[next] in [` `, `\t`] {
		next++
	}
	if next >= line_end || !source[next].is_letter() {
		return none
	}
	return token.new_span(pos.id, start, end)
}

fn (tc &TypeChecker) malformed_const_keyword_pos(id flat.NodeId) ?token.Pos {
	if !tc.valid_node_id(id) {
		return none
	}
	pos := tc.a.node(id).pos
	file := tc.a.source_files[pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	if pos.offset < 0 || pos.offset > source.len {
		return none
	}
	mut line_start := pos.offset
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	line_end := source.index_after('\n', line_start) or { source.len }
	indent := source[line_start..line_end].len - source[line_start..line_end].trim_left(' \t').len
	keyword_start := line_start + indent
	if keyword_start + 5 > source.len || source[keyword_start..keyword_start + 5] != 'cosnt' {
		return none
	}
	if keyword_start + 5 < line_end && source[keyword_start + 5] !in [` `, `\t`] {
		return none
	}
	return token.new_span(pos.id, keyword_start, keyword_start + 5)
}

fn (tc &TypeChecker) node_is_on_import_line(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	pos := tc.a.node(id).pos
	file := tc.a.source_files[pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	if pos.offset < 0 || pos.offset > source.len {
		return false
	}
	line_start := if relative := source[..pos.offset].last_index('\n') {
		relative + 1
	} else {
		0
	}
	return source[line_start..pos.offset].trim_left(' \t').starts_with('import ')
}

fn is_top_level_statement_kind(kind flat.NodeKind) bool {
	return kind in [
		.expr_stmt,
		.if_expr,
		.assign,
		.decl_assign,
		.selector_assign,
		.index_assign,
		.return_stmt,
		.block,
		.for_stmt,
		.for_in_stmt,
		.break_stmt,
		.continue_stmt,
		.match_stmt,
		.defer_stmt,
		.assert_stmt,
		.goto_stmt,
		.label_stmt,
		.select_stmt,
		.comptime_if,
		.comptime_for,
		.asm_stmt,
		.debugger_stmt,
	]
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
						idx: i
						file: cur_file
						mod: cur_module
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
			.fn_decl, .struct_decl, .type_decl, .interface_decl, .enum_decl, .c_fn_decl, .import_decl, .module_decl, .directive {
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
		if child.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		if child.value.len > 0 {
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
		tc.insert_selected_file_decl_binding_type(key_id, tc.range_loop_var_type(container_id, tc.a.child(&node, 3)))
		tc.collect_selected_file_node_called_fns(tc.a.child(&node, 3))
	} else {
		clean := tc.for_in_iterable_type(container_id)
		yields_ref := node.op == .amp || tc.for_in_iterable_yields_ref(container_id)
		if clean is Array {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, elem_type)
			}
		} else if clean is ArrayFixed {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, elem_type)
			}
		} else if clean is Map {
			value_type := for_in_ref_binding_type(clean.value_type, yields_ref)
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, clean.key_type)
				tc.insert_selected_file_decl_binding_type(val_id, value_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, value_type)
			}
		} else if clean is String {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, Type(u8_))
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, Type(u8_))
			}
		} else if generic_name := tc.iterator_unbounded_next_generic(clean) {
			_ = generic_name
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, Type(int_))
				tc.insert_selected_file_decl_binding_type(val_id, unknown_type('unbounded iterator generic'))
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, unknown_type('unbounded iterator generic'))
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
				tc.insert_selected_file_decl_binding_type(key_id, tc.range_loop_var_type(tc.a.child(&container, 0), tc.a.child(&container, 1)))
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
	if tc.valid_diagnostic_fast {
		return
	}
	mut natural_symbols := map[string]string{}
	mut natural_symbol_modules := map[string]string{}
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
				natural_symbol_modules[natural_symbol] = cur_module
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
			.const_decl {
				export_name := tc.declaration_attribute_value(flat.NodeId(i), 'export') or {
					continue
				}
				for field_idx in 0 .. node.children_count {
					field_id := tc.a.child(&node, field_idx)
					field := tc.a.node(field_id)
					if field.kind != .const_field {
						continue
					}
					name := if export_name.len > 0 { export_name } else { field.value }
					if !is_valid_export_c_name(name) {
						tc.record_error_at(.unsupported_generic, 'export name `${name}` should be a valid identifier', field_id, tc.node_value_diagnostic_pos(field_id))
					}
					if name in export_symbols {
						tc.record_error_at(.unsupported_generic, 'duplicate export name `${name}`', field_id, tc.node_value_diagnostic_pos(field_id))
					} else {
						export_symbols[name] = field.value
					}
				}
			}
			else {}
		}
	}
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
			.global_decl {
				for field_idx in 0 .. node.children_count {
					field_id := tc.a.child(&node, field_idx)
					field := tc.a.node(field_id)
					if field.value in export_symbols {
						tc.record_error_at(.unsupported_generic, 'duplicate export name `${field.value}`', field_id, tc.node_value_diagnostic_pos(field_id))
					}
				}
			}
			else {}
		}
	}
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
				if pos := tc.declaration_attribute_without_value_pos(flat.NodeId(i), 'export') {
					tc.record_error_at(.unsupported_generic, 'missing argument for @[export] attribute', flat.NodeId(i), pos)
					continue
				}
				export_name := tc.a.export_fn_names[qname] or {
					tc.a.export_fn_names[node.value] or {
						tc.a.export_fn_names['${cur_module}.${node.value}'] or { continue }
					}
				}
				if export_name.len == 0 {
					tc.record_error_unfiltered(.unsupported_generic, 'empty export name for `${qname}`', flat.NodeId(i))
					continue
				}
				if !is_valid_export_c_name(export_name) {
					message := if cur_module.len == 0 {
						'export name `${export_name}` should be a valid identifier for `${qname}`'
					} else {
						'export name `${export_name}` should be a valid identifier'
					}
					tc.record_error_at(.unsupported_generic, message, flat.NodeId(i), tc.fn_declaration_diagnostic_pos(node))
				}
				if synthetic_main_reserved && export_name == 'main' {
					tc.record_error_unfiltered(.unsupported_generic, 'export name `main` for `${qname}` collides with synthetic entry point `main`', flat.NodeId(i))
				}
				if node.generic_params().len > 0 {
					tc.record_error_unfiltered(.unsupported_generic, 'generic function `${qname}` cannot be exported', flat.NodeId(i))
				}
				for pi in 0 .. node.children_count {
					p := tc.a.child_node(&node, pi)
					if p.kind != .param {
						if tc.prefix_param_scan {
							break
						}
						continue
					}
					if p.value.len == 0 || p.typ.len == 0 {
						tc.record_error_unfiltered(.unsupported_generic, 'exported function `${qname}` must name all parameters', flat.NodeId(i))
					}
				}
				if existing := export_symbols[export_name] {
					if existing != qname {
						tc.record_error_at(.unsupported_generic, 'duplicate export name `${export_name}`', flat.NodeId(i), tc.fn_declaration_diagnostic_pos(node))
					}
				} else {
					export_symbols[export_name] = qname
				}
				if existing := natural_symbols[export_name] {
					// A main-module function can be prefixed in C when an explicit export
					// owns its otherwise natural symbol, matching the established backend.
					if existing != qname && natural_symbol_modules[export_name] !in ['', 'main'] {
						tc.record_error_unfiltered(.unsupported_generic, 'export name `${export_name}` for `${qname}` collides with `${existing}`', flat.NodeId(i))
					}
				}
			}
			else {}
		}
	}
	for qname, export_name in tc.a.export_fn_names {
		if is_valid_export_c_name(export_name)
			|| tc.errors.any(it.kind == .unsupported_generic
				&& it.msg.contains('export name `${export_name}` should be a valid identifier')) {
			continue
		}
		tc.record_error_unfiltered(.unsupported_generic, 'export name `${export_name}` should be a valid identifier for `${qname}`', flat.NodeId(0))
	}
}

fn (tc &TypeChecker) declaration_attribute_without_value_pos(node_id flat.NodeId, name string) ?token.Pos {
	if !tc.should_check_source_name(node_id) {
		return none
	}
	mut has_bare_attr := false
	for raw in tc.declaration_attributes[int(node_id)] {
		if raw.trim_space() == name {
			has_bare_attr = true
			break
		}
	}
	if !has_bare_attr {
		return none
	}
	node := tc.a.node(node_id)
	file := tc.a.source_files[node.pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	end := int_min(int_max(node.pos.offset, 0), source.len)
	needle := '@[${name}]'
	if relative := source[..end].last_index(needle) {
		return token.new_span(node.pos.id, relative, relative + needle.len)
	}
	return none
}

fn (tc &TypeChecker) declaration_attribute_value(node_id flat.NodeId, name string) ?string {
	for raw in tc.declaration_attributes[int(node_id)] {
		if raw.all_before(':').trim_space() != name || !raw.contains(':') {
			continue
		}
		value := raw.all_after(':').trim_space()
		if value.len >= 2 && ((value[0] == `'` && value[value.len - 1] == `'`)
			|| (value[0] == `"` && value[value.len - 1] == `"`)) {
			return value[1..value.len - 1]
		}
		return value
	}
	return none
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
		.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .for_stmt, .for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt {
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
	file := path_leaf_view(path)
	if file.ends_with('_test.v') || file.ends_with('_test.vv') || file.ends_with('_test.c.v') {
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
	file := path_leaf_view(path)
	if file.ends_with('_d_test.v') || file.ends_with('_d_test.vv') {
		return false
	}
	return file.ends_with('_test.v') || file.ends_with('_test.vv')
}

fn export_qualified_fn_name(module_name string, name string) string {
	if name.contains('.') {
		return name
	}
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
	// The implicit veb `ctx` is a `mut ctx Context` parameter — veb hands the
	// handler a mutable context. Register it as a mut-param binding (not a plain
	// immutable local) so mut-receiver methods like `ctx.json(...)` / `ctx.set_status(...)`
	// see a mutable lvalue receiver.
	typ := tc.implicit_veb_ctx_type()
	owner := tc.cur_scope.insert_with_owner('ctx', typ)
	tc.fn_context.mut_param_base_types['ctx'] = mut_param_base_type(typ)
	tc.fn_context.mut_param_owners['ctx'] = owner
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
	return trimmed_space(raw).starts_with('shared ')
}

fn decl_assign_is_shared_marker(value string) bool {
	return value == 'shared' || value.starts_with('shared:')
}

fn (tc &TypeChecker) implicit_veb_ctx_type() Type {
	return tc.parse_type('mut Context')
}

fn (tc &TypeChecker) fn_needs_implicit_veb_ctx(node flat.Node) bool {
	return tc.fn_is_veb_app_handler(node) && !tc.fn_has_veb_context_param(node)
}

fn (tc &TypeChecker) fn_is_veb_app_handler(node flat.Node) bool {
	return tc.fn_returns_veb_result(node) && tc.fn_has_receiver_param(node)
		&& !tc.fn_receiver_type_is_context(node) && tc.type_name_known_in_current_module('Context')
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

fn (tc &TypeChecker) fn_has_veb_context_param(node flat.Node) bool {
	for i in 0 .. node.children_count {
		p := tc.a.child_node(&node, i)
		if p.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		// The signature pass can run before every embedded struct relation is
		// available in a parallel worker. Recognize the conventional context
		// spelling directly so an explicit `mut ctx Context` is never also given
		// a hidden context parameter. Only the unqualified local `Context` counts:
		// a qualified `other.Context` (for example an imported alias of `string`)
		// shares the leaf name but is not a veb context, so it must fall through to
		// the semantic check below.
		if p.typ.trim_space().trim_string_left('mut ').trim_left('&') == 'Context' {
			return true
		}
		if tc.is_veb_context_type(tc.parse_type(p.typ)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_returns_veb_result(node flat.Node) bool {
	raw_type := node.typ.trim_space()
	if raw_type == 'veb.Result' {
		return true
	}
	// Most declarations cannot return veb.Result. Avoid resolving unrelated
	// expression-derived return types such as `typeof(value)` during collection.
	if raw_type != 'Result' && !raw_type.ends_with('.Result') {
		return false
	}
	ret := tc.parse_type(raw_type)
	return ret.name() == 'veb.Result'
}

fn (mut tc TypeChecker) check_veb_app_method_params(fn_id flat.NodeId, node flat.Node) {
	if !tc.fn_returns_veb_result(node) || !tc.fn_has_receiver_param(node)
		|| !tc.should_diagnose(fn_id) {
		return
	}
	receiver := tc.a.child_node(&node, 0)
	receiver_type := tc.parse_type(receiver.typ)
	if tc.is_veb_context_type(receiver_type) {
		return
	}
	method := node.value.all_after_last('.')
	for i in 1 .. node.children_count {
		param_id := tc.a.child(&node, i)
		param := tc.a.node(param_id)
		if param.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		param_type := tc.parse_type(param.typ)
		if tc.is_veb_context_type(param_type) {
			if !param.is_mut {
				display_type := param.typ.trim_left('&').all_after_last('.')
				tc.record_error_at(.call_arg_mismatch, 'veb app method `${method}` must declare context parameter `${param.value}` as mutable, e.g. `mut ${param.value} ${display_type}`', param_id, tc.node_value_diagnostic_pos(param_id))
			}
			continue
		}
		clean := fn_param_unalias_type(param_type)
		if clean is String || clean.is_integer() || clean.name() == 'bool' || clean is Interface {
			continue
		}
		tc.record_error_at(.call_arg_mismatch, 'veb app method `${method}` parameter `${param.value}` has unsupported type `${param.typ}`; parameters after the context are populated from strings and must be `string`, integer, or `bool`', param_id, tc.node_value_diagnostic_pos(param_id))
	}
}

// is_veb_context_type reports whether typ is veb.Context or embeds it.
pub fn (tc &TypeChecker) is_veb_context_type(typ Type) bool {
	clean := unalias_type(unwrap_all_pointers(typ))
	if clean.name() == 'veb.Context' {
		return true
	}
	return tc.receiver_embeds(clean, Type(Struct{
		name: 'veb.Context'
	}))
}

// check_fn_body validates check fn body state for types.
fn (mut tc TypeChecker) check_fn_body(node flat.Node) {
	if tc.fn_context.has_goto_nodes {
		tc.initialize_pointer_alias_goto_targets()
	}
	had_saved_smartcasts := tc.smartcasts.len > 0
	saved_smartcasts := if had_saved_smartcasts {
		clone_smartcasts(tc.smartcasts)
	} else {
		// The empty map may be reused throughout this function; the deferred clear
		// below restores the empty outer context without allocating a clone for
		// every checked function.
		tc.smartcasts
	}
	defer {
		if had_saved_smartcasts {
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
		} else {
			tc.smartcasts.clear()
		}
	}
	mut sequence_exited := false
	mut unreachable_id := flat.empty_node
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		if tc.valid_diagnostic_fast {
			tc.check_stmt_node(child_id)
			tc.fn_context.continue_after_unknown_ident = false
			tc.apply_post_if_exit_smartcasts(child_id)
			tc.apply_post_assert_smartcasts(child_id)
			continue
		}
		if child.kind == .label_stmt {
			sequence_exited = false
			unreachable_id = flat.empty_node
		} else if sequence_exited && !tc.valid_node_id(unreachable_id) {
			unreachable_id = child_id
		}
		error_count := tc.errors.len
		tc.check_stmt_node(child_id)
		continue_after_unknown_ident := tc.fn_context.continue_after_unknown_ident
		tc.fn_context.continue_after_unknown_ident = false
		tc.apply_post_if_exit_smartcasts(child_id)
		tc.apply_post_assert_smartcasts(child_id)
		if tc.statement_exits_sequence(child_id, child) {
			sequence_exited = true
		}
		if tc.new_error_kind_since(error_count, .unknown_ident) && child.kind != .comptime_if
			&& !tc.new_errors_are_forward_decl_unknowns(error_count)
			&& !continue_after_unknown_ident {
			break
		}
	}
	if tc.valid_node_id(unreachable_id) && tc.should_diagnose(unreachable_id) {
		tc.record_error_at(.return_mismatch, 'unreachable code', unreachable_id, tc.unreachable_statement_diagnostic_pos(unreachable_id))
	}
}

fn (tc &TypeChecker) new_errors_are_forward_decl_unknowns(start int) bool {
	if start < 0 || start >= tc.errors.len {
		return false
	}
	mut found := false
	for i in start .. tc.errors.len {
		err := tc.errors[i]
		if err.kind != .unknown_ident {
			continue
		}
		if !err.msg.contains('(used before declaration)')
			&& !err.msg.starts_with('unresolved variable:')
			&& !err.msg.contains('(use `:=` to declare a variable)') {
			return false
		}
		found = true
	}
	return found
}

fn (tc &TypeChecker) new_error_kind_since(start int, kind TypeErrorKind) bool {
	if start < 0 || start >= tc.errors.len {
		return false
	}
	for i in start .. tc.errors.len {
		if tc.errors[i].kind == kind {
			return true
		}
	}
	return false
}

// check_decl_type_strings validates check decl type strings state for types.
fn (mut tc TypeChecker) check_decl_type_strings(node_id flat.NodeId, node flat.Node) {
	mut generic_params := tc.infer_decl_generic_params(node)
	if node.kind == .type_decl && node.generic_params().len == 0 {
		for name in tc.fixed_array_map_alias_generic_params(node.typ) {
			generic_params[name] = true
		}
	}
	decl_generic_mentions_error := tc.check_struct_or_interface_decl_generic_mentions(node_id, node)
	mut invalid_generic_struct_alias := false
	if node.kind == .type_decl {
		invalid_generic_struct_alias = tc.check_type_alias_generic_struct_application(node_id, node)
		if imported_name := tc.selective_imported_builtin_in_type(node.typ) {
			base := 'unknown type `${imported_name}`'
			message := util.new_suggestion(imported_name, tc.known_type_name_candidates()).say(base)
			tc.record_error_at(.unknown_type, message, node_id, tc.type_diagnostic_pos(node_id, node.typ.trim_space()))
		}
		alias_name := node.typ.trim_space()
		alias_type := tc.parse_type(alias_name)
		if alias_type is Alias && alias_type.base_type is FnType {
			original_type := Type(alias_type.base_type).name().replace_once('fn(', 'fn (')
			tc.record_error_at(.unknown_type, 'type `${alias_name}` is an alias, use the original alias type `${original_type}` instead', node_id, tc.type_diagnostic_pos(node_id, alias_name))
		}
	}
	if node.kind == .fn_decl {
		tc.check_fn_decl_unmentioned_generic_types(node_id, node)
		tc.check_fn_bare_generic_signature_types(node_id, node)
	}
	if node.kind == .type_decl && node.children_count > 0 {
		tc.check_implicit_generic_sumtype_decl(node_id, node)
	}
	if node.kind == .type_decl
		&& (node.children_count > 0 || split_sum_variant_texts(node.typ).len > 1) {
		tc.check_sum_type_decl(node_id, node)
	}
	if node.kind == .fn_decl && node.value.contains('.') {
		receiver_name := node.value.all_before_last('.').all_after_last('.')
		mut is_static := node.children_count == 0
		if node.children_count > 0 {
			first := tc.a.child_node(&node, 0)
			is_static = first.kind != .param || first.op != .dot
		}
		if is_static && !tc.type_name_known(receiver_name) {
			tc.record_error_at(.unknown_type, 'unknown type `${receiver_name}`', node_id, tc.type_diagnostic_pos(node_id, receiver_name))
		}
	}
	if node.kind == .type_decl && node.typ.trim_space().starts_with('!') {
		tc.record_error_with_details_at(.unknown_type, 'cannot make an alias of Result type', node_id, tc.type_diagnostic_pos(node_id, node.typ.trim_space()), [
			'Result types cannot be stored and have to be unwrapped immediately',
		])
	}
	if node.kind == .type_decl && node.typ.trim_space() == 'none' {
		tc.record_error_at(.unknown_type, 'cannot create a type alias of `none` as it is a value', node_id, tc.type_diagnostic_pos(node_id, 'none'))
	}
	if node.kind == .struct_decl {
		if comma_attr_text_has(node.typ, 'generic') && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic struct `${node.value}`', node_id)
		}
	} else {
		if node.generic_params().len > 0 && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic declaration `${node.value}`', node_id)
		}
		if !invalid_generic_struct_alias && (!decl_generic_mentions_error
			|| tc.unmentioned_generic_names_in_type(node.typ, generic_param_map_from_names(node.generic_params())).len == 0) {
			tc.check_type_string_for_unsupported_generics(node.typ, node_id, generic_params)
		}
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
		explicit_decl_params := generic_param_map_from_names(node.generic_params())
		if !decl_generic_mentions_error
			|| tc.unmentioned_generic_names_in_type(child.typ, explicit_decl_params).len == 0 {
			tc.check_type_string_for_unsupported_generics(child.typ, child_id, generic_params)
		}
		if node.kind == .interface_decl && child.kind == .interface_field && child.op == .dot
			&& child.generic_params().len > 0 {
			tc.record_error_at(.unsupported_generic, "no need to add generic type names in generic interface's method", child_id, tc.interface_method_generic_param_pos(child, child.generic_params()[0]))
		}
		if node.kind == .interface_decl && child.kind == .interface_field && child.op != .dot {
			field_type := unalias_type(tc.parse_type(child.typ))
			if field_type is Interface
				&& tc.interface_metadata_name(field_type.name) == tc.interface_metadata_name(node.value) {
				tc.record_error_at(.assignment_mismatch, 'recursive interface fields are not allowed because they cannot be initialised', child_id, tc.type_diagnostic_pos(child_id, child.typ))
			}
		}
		if child.kind == .param && child.typ.trim_space().starts_with('!') {
			param_type := unalias_type(tc.parse_type(child.typ))
			mut supported_result_callback := false
			if param_type is ResultType {
				supported_result_callback = unalias_type(param_type.base_type) is FnType
			}
			if !supported_result_callback {
				tc.record_error_at(.unknown_type, 'result type arguments are not supported', child_id, tc.type_diagnostic_pos(child_id, child.typ.trim_space()))
			}
		}
		if node.kind == .struct_decl {
			tc.check_missing_struct_field_generic_type(child_id, child.typ, generic_params)
			field_type := unalias_type(tc.parse_type(child.typ))
			if child.kind == .field_decl && field_type is Map
				&& unalias_type(field_type.value_type) is ResultType {
				tc.record_error_at(.unknown_type, 'cannot use Result type as map value type', child_id, tc.struct_field_type_pos(child))
			}
		}
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
			if !decl_generic_mentions_error
				|| tc.unmentioned_generic_names_in_type(grandchild.typ, explicit_decl_params).len == 0 {
				tc.check_type_string_for_unsupported_generics(grandchild.typ, grandchild_id, generic_params)
			}
		}
	}
}

fn (mut tc TypeChecker) check_type_alias_generic_struct_application(node_id flat.NodeId, node flat.Node) bool {
	clean := node.typ.trim_space()
	if clean.len == 0 {
		return false
	}
	base, args, is_generic := generic_type_application_parts(clean)
	lookup := if is_generic { base } else { clean }
	qualified := tc.qualify_name(lookup)
	params := tc.struct_generic_params[lookup] or {
		tc.struct_generic_params[qualified] or { return false }
	}
	if params.len == 0 {
		return false
	}
	mut invalid := !is_generic
	if is_generic && node.generic_params().len == 0 {
		invalid = args.any(tc.type_text_has_generic_placeholder(it))
	}
	if !invalid {
		return false
	}
	short_name := lookup.all_after_last('.')
	tc.record_error_at(.unknown_type, '${short_name} type is generic struct, must specify the generic type names, e.g. ${short_name}[int]', node_id, tc.type_diagnostic_pos(node_id, clean))
	return true
}

fn (tc &TypeChecker) fixed_array_map_alias_generic_params(typ string) []string {
	clean := typ.trim_space()
	if !clean.starts_with('[') {
		return []string{}
	}
	bracket_end := find_matching_bracket(clean, 0)
	if bracket_end >= clean.len {
		return []string{}
	}
	element_type := clean[bracket_end + 1..].trim_space()
	if !element_type.starts_with('map[') {
		return []string{}
	}
	mut counts := map[string]int{}
	tc.collect_generic_param_candidates(element_type, mut counts)
	mut names := []string{cap: counts.len}
	for name, _ in counts {
		names << name
	}
	names.sort()
	return names
}

fn (tc &TypeChecker) selective_imported_builtin_in_type(type_text string) ?string {
	info := tc.current_file_import_info()
	if isnil(info) {
		return none
	}
	mut best_offset := type_text.len + 1
	mut imported_name := ''
	for name, candidates in info.selective_imports {
		if !is_builtin_type_name(name) || candidates.len != 1
			|| !type_text_contains_symbol(type_text, name) {
			continue
		}
		offset := type_text.index(name) or { continue }
		if offset < best_offset {
			best_offset = offset
			imported_name = candidates[0]
		}
	}
	if imported_name.len == 0 {
		return none
	}
	return imported_name
}

fn (tc &TypeChecker) interface_method_generic_param_pos(method flat.Node, param string) token.Pos {
	file := tc.a.source_files[method.pos.id] or { return method.pos }
	source := tc.source_texts_by_file[file.name] or { return method.pos }
	needle := '${method.value}[${param}'
	start := source.index(needle) or { return method.pos }
	param_start := start + method.value.len
	return token.new_span(method.pos.id, param_start, param_start + param.len)
}

fn (mut tc TypeChecker) check_fn_bare_generic_fntype_params(node flat.Node) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		if child.op != .dot {
			tc.check_bare_generic_fntype_param(child_id, child.typ)
		}
	}
}

fn (mut tc TypeChecker) check_struct_or_interface_decl_generic_mentions(node_id flat.NodeId, node flat.Node) bool {
	if node.kind !in [.struct_decl, .interface_decl] {
		return false
	}
	explicit_names := node.generic_params()
	explicit_params := generic_param_map_from_names(explicit_names)
	mut has_unmentioned := false
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		child_type := if node.kind == .interface_decl && child.op != .dot && child.typ.len == 0 {
			child.value
		} else {
			child.typ
		}
		if tc.unmentioned_generic_names_in_type(child_type, explicit_params).len > 0 {
			has_unmentioned = true
		}
		if node.kind == .interface_decl && child.op == .dot {
			for j in 0 .. child.children_count {
				param := tc.a.child_node(child, j)
				if tc.unmentioned_generic_names_in_type(param.typ, explicit_params).len > 0 {
					has_unmentioned = true
				}
			}
		}
	}
	if !has_unmentioned {
		return false
	}
	kind := if node.kind == .struct_decl { 'struct' } else { 'interface' }
	if explicit_names.len == 0 {
		pos := if node.kind == .struct_decl {
			tc.struct_declaration_name_pos(node)
		} else {
			tc.source_line_declaration_pos(node_id)
		}
		tc.record_error_at(.unsupported_generic, 'generic ${kind} `${node.value}` declaration must specify the generic type names, e.g. ${node.value}[T]', node_id, pos)
		return true
	}
	decl_name := if node.kind == .interface_decl {
		'${node.value}<${explicit_names.join(', ')}>'
	} else {
		'${node.value}[${explicit_names.join(', ')}]'
	}
	mut diagnosed := map[string]bool{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		child_type := if node.kind == .interface_decl && child.op != .dot && child.typ.len == 0 {
			child.value
		} else {
			child.typ
		}
		for name in tc.unmentioned_generic_names_in_type(child_type, explicit_params) {
			key := '${int(child_id)}\n${name}'
			if diagnosed[key] {
				continue
			}
			diagnosed[key] = true
			pos_text := if node.kind == .interface_decl && child.op != .dot && child.typ.len == 0 {
				base, _, is_generic := generic_type_application_parts(child_type)
				if is_generic {
					base
				} else {
					child_type
				}
			} else {
				child_type
			}
			tc.record_error_at(.unknown_type, 'generic type name `${name}` is not mentioned in ${kind} `${decl_name}`${unmentioned_generic_unknown_suffix(name)}', child_id, tc.type_diagnostic_pos(child_id, pos_text))
		}
		if node.kind != .interface_decl || child.op != .dot {
			continue
		}
		for j in 0 .. child.children_count {
			param_id := tc.a.child(child, j)
			param := tc.a.node(param_id)
			for name in tc.unmentioned_generic_names_in_type(param.typ, explicit_params) {
				key := '${int(param_id)}\n${name}'
				if diagnosed[key] {
					continue
				}
				diagnosed[key] = true
				tc.record_error_at(.unknown_type, 'generic type name `${name}` is not mentioned in ${kind} `${decl_name}`${unmentioned_generic_unknown_suffix(name)}', param_id, tc.type_diagnostic_pos(param_id, param.typ))
			}
		}
	}
	return true
}

fn (mut tc TypeChecker) check_fn_bare_generic_signature_types(node_id flat.NodeId, node flat.Node) {
	if node.generic_params().len > 0 {
		if name := tc.bare_generic_decl_type_name(node.typ) {
			kind, params := tc.generic_decl_kind_and_params(name)
			if kind.len > 0 {
				tc.record_error_at(.unsupported_generic, 'return generic ${kind} `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${params.join(', ')}]', node_id, tc.fn_return_type_diagnostic_pos(node))
			}
		}
	}
	if !node.value.contains('.') || node.children_count == 0 {
		return
	}
	receiver_id := tc.a.child(&node, 0)
	receiver := tc.a.node(receiver_id)
	if receiver.kind != .param || receiver.op != .dot {
		return
	}
	name := tc.bare_generic_decl_type_name(receiver.typ) or { return }
	kind, params := tc.generic_decl_kind_and_params(name)
	if kind.len == 0 {
		return
	}
	tc.record_error_at(.unsupported_generic, 'generic ${kind} `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${params.join(', ')}]', receiver_id, tc.type_diagnostic_pos(receiver_id, receiver.typ))
}

fn (tc &TypeChecker) fn_decl_has_bare_generic_signature_type(node flat.Node) bool {
	if node.generic_params().len > 0 && tc.bare_generic_decl_type_name(node.typ) != none {
		return true
	}
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	receiver := tc.a.child_node(&node, 0)
	return receiver.kind == .param && receiver.op == .dot
		&& tc.bare_generic_decl_type_name(receiver.typ) != none
}

fn (tc &TypeChecker) generic_decl_kind_and_params(name string) (string, []string) {
	qualified := tc.qualify_name(name)
	if name in tc.struct_generic_params {
		return 'struct', tc.struct_generic_params[name]
	}
	if qualified in tc.struct_generic_params {
		return 'struct', tc.struct_generic_params[qualified]
	}
	if name in tc.interface_generic_params {
		return 'interface', tc.interface_generic_params[name]
	}
	if qualified in tc.interface_generic_params {
		return 'interface', tc.interface_generic_params[qualified]
	}
	if name in tc.sum_generic_params {
		return 'sumtype', tc.sum_generic_params[name]
	}
	if qualified in tc.sum_generic_params {
		return 'sumtype', tc.sum_generic_params[qualified]
	}
	if name in tc.type_alias_generic_params {
		params := tc.type_alias_generic_params[name]
		kind := if unalias_type(tc.parse_type(name)) is FnType { 'function' } else { 'alias' }
		return kind, params
	}
	if qualified in tc.type_alias_generic_params {
		params := tc.type_alias_generic_params[qualified]
		kind := if unalias_type(tc.parse_type(qualified)) is FnType {
			'function'
		} else {
			'alias'
		}
		return kind, params
	}
	return '', []string{}
}

fn (mut tc TypeChecker) check_fn_decl_unmentioned_generic_types(node_id flat.NodeId, node flat.Node) {
	mut explicit_params := generic_param_map_from_names(node.generic_params())
	if node.value.contains('.') && node.children_count > 0 {
		receiver := tc.a.child_node(&node, 0)
		if receiver.kind == .param {
			mut receiver_text := receiver.typ.trim_space()
			for receiver_text.starts_with('shared ') || receiver_text.starts_with('atomic ') {
				receiver_text = receiver_text[7..].trim_space()
			}
			receiver_type := unwrap_pointer(tc.parse_type(receiver_text))
			// Generic struct and interface receiver parameters belong to the method declaration.
			// Alias and sum-type receivers still have to repeat their generic names,
			// matching the v1 declaration rules.
			if receiver_type is Struct || receiver_type is Interface {
				tc.collect_generic_receiver_params(node, mut explicit_params)
			}
		}
	}
	mut missing_ids := []flat.NodeId{}
	mut missing_names := []string{}
	mut missing_types := []string{}
	mut diagnosed := map[string]bool{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		if child.typ.len == 0 {
			continue
		}
		for name in tc.unmentioned_generic_names_in_type(child.typ, explicit_params) {
			key := '${int(child_id)}\n${name}'
			if diagnosed[key] {
				continue
			}
			diagnosed[key] = true
			missing_ids << child_id
			missing_names << name
			missing_types << child.typ.trim_space()
		}
	}
	if node.typ.len > 0 {
		for name in tc.unmentioned_generic_names_in_type(node.typ, explicit_params) {
			key := '${int(node_id)}\n${name}'
			if diagnosed[key] {
				continue
			}
			diagnosed[key] = true
			missing_ids << node_id
			missing_names << name
			missing_types << node.typ.trim_space()
		}
	}
	if missing_names.len == 0 {
		return
	}
	if node.generic_params().len == 0 {
		if node.value.contains('.') {
			tc.record_error_with_details_at(.unsupported_generic, 'generic method declaration must specify generic type names', node_id, tc.fn_declaration_diagnostic_pos(node), [
				'use `fn (r SomeType[T]) foo[T]() {`, not just `fn (r SomeType[T]) foo() {`',
			])
		} else {
			tc.record_error_with_details_at(.unsupported_generic, 'generic function declaration must specify generic type names', node_id, tc.fn_declaration_diagnostic_pos(node), [
				'use `fn foo[T](x T) {`, not just `fn foo(x T) {`',
			])
		}
	}
	for i, name in missing_names {
		tc.record_error_at(.unknown_type, 'generic type name `${name}` is not mentioned in fn `${node.value.all_after_last('.')}[${node.generic_params().join(', ')}]`${unmentioned_generic_unknown_suffix(name)}', missing_ids[i], tc.type_diagnostic_pos(missing_ids[i], missing_types[i]))
	}
}

fn (mut tc TypeChecker) check_bare_generic_fntype_param(node_id flat.NodeId, type_text string) {
	name := tc.bare_generic_decl_type_name(type_text) or { return }
	qualified := tc.qualify_name(name)
	interface_params := tc.interface_generic_params[name] or {
		tc.interface_generic_params[qualified] or { []string{} }
	}
	if interface_params.len > 0 {
		tc.record_error_at(.unsupported_generic, 'generic interface `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${interface_params.join(', ')}]', node_id, tc.type_diagnostic_pos(node_id, type_text))
		return
	}
	params := tc.type_alias_generic_params[name] or {
		tc.type_alias_generic_params[qualified] or { return }
	}
	if params.len == 0 || unalias_type(tc.parse_type(name)) !is FnType {
		return
	}
	tc.record_error_at(.unsupported_generic, 'generic function `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${params.join(', ')}]', node_id, tc.type_diagnostic_pos(node_id, type_text))
}

fn (mut tc TypeChecker) check_implicit_generic_sumtype_decl(node_id flat.NodeId, node flat.Node) {
	decl_params := node.generic_params()
	if decl_params.len > 0 {
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			variant := tc.a.node(child_id).value.trim_space()
			base, args, is_generic := generic_type_application_parts(variant)
			if !is_generic {
				continue
			}
			qualified := tc.qualify_name(base)
			struct_params := tc.struct_generic_params[base] or {
				tc.struct_generic_params[qualified] or { []string{} }
			}
			if struct_params.len == 0 {
				continue
			}
			for arg in args {
				if is_bare_generic_param(arg) && arg !in decl_params {
					tc.record_error_at(.unknown_type, 'generic type name `${arg}` of generic struct `${variant}` is not mentioned in sumtype `${node.value}[${decl_params.join(', ')}]`', child_id, tc.type_diagnostic_pos(node_id, variant))
				}
			}
		}
		return
	}
	mut requires_generic_params := false
	mut example_params := []string{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		variant := child.value.trim_space()
		if variant.len == 0 {
			continue
		}
		base, args, is_generic := generic_type_application_parts(variant)
		lookup := if is_generic { base } else { variant }
		if !is_generic && tc.concrete_type_declared_in_current_file(lookup) {
			continue
		}
		qualified := tc.qualify_name(lookup)
		params := tc.struct_generic_params[lookup] or {
			tc.struct_generic_params[qualified] or {
				tc.type_alias_generic_params[lookup] or {
					tc.type_alias_generic_params[qualified] or {
						tc.sum_generic_params[lookup] or {
							tc.sum_generic_params[qualified] or { []string{} }
						}
					}
				}
			}
		}
		if params.len == 0 {
			continue
		}
		if !is_generic {
			requires_generic_params = true
			if example_params.len == 0 {
				example_params = params.clone()
			}
			kind := if lookup in tc.struct_generic_params || qualified in tc.struct_generic_params {
				'struct'
			} else if unalias_type(tc.parse_type(lookup)) is FnType {
				'fntype'
			} else {
				'sumtype'
			}
			tc.record_error_at(.unknown_type, 'generic ${kind} `${lookup}` must specify generic type names, e.g. ${lookup}[${params.join(', ')}]', child_id, tc.type_diagnostic_pos(node_id, variant))
			continue
		}
		for arg in args {
			if is_bare_generic_param(arg) && !tc.type_name_known(arg) {
				requires_generic_params = true
				if example_params.len == 0 {
					example_params = params.clone()
				}
				break
			}
		}
	}
	if requires_generic_params {
		if example_params.len == 0 {
			example_params = ['T']
		}
		tc.record_error_at(.unsupported_generic, 'generic sumtype `${node.value}` must specify generic type names, e.g. ${node.value}[${example_params.join(', ')}]', node_id, tc.type_diagnostic_pos(node_id, node.value))
	}
}

fn (tc &TypeChecker) unmentioned_generic_names_in_type(typ string, explicit_params map[string]bool) []string {
	mut names := []string{}
	for i, ch in typ {
		if ch < `A` || ch > `Z` {
			continue
		}
		if i > 0 && is_type_symbol_byte(typ[i - 1]) {
			continue
		}
		if i + 1 < typ.len && (is_type_symbol_byte(typ[i + 1]) || typ[i + 1] == `.`) {
			continue
		}
		name := typ[i..i + 1]
		if explicit_params.len == 0 && name != 'T' {
			continue
		}
		if explicit_params[name] || tc.type_name_known(name) || tc.local_type_name_known(name)
			|| name in names {
			continue
		}
		names << name
	}
	return names
}

fn unmentioned_generic_unknown_suffix(name string) string {
	if name == 'T' {
		return ''
	}
	return '; unknown type `${name}`'
}

fn (tc &TypeChecker) local_type_name_known(name string) bool {
	prefix := '${name}@local@'
	for index in tc.top_level_idx {
		if index < 0 || index >= tc.a.nodes.len {
			continue
		}
		node := tc.a.nodes[index]
		if node.kind in [.struct_decl, .type_decl] && node.value.starts_with(prefix) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) unmentioned_generic_type_was_reported(name string, node_id flat.NodeId) bool {
	needle := 'generic type name `${name}` is not mentioned in fn '
	enclosing_fn := tc.diagnostic_enclosing_fn(node_id)
	for err in tc.errors {
		if err.msg.starts_with(needle) && tc.diagnostic_enclosing_fn(err.node) == enclosing_fn {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) diagnostic_enclosing_fn(node_id flat.NodeId) flat.NodeId {
	if !tc.valid_node_id(node_id) {
		return flat.empty_node
	}
	if tc.direct_parent_index_trusted {
		mut current := node_id
		for tc.valid_node_id(current) {
			if tc.a.node(current).kind == .fn_decl {
				return current
			}
			if int(current) >= tc.direct_parent_ids.len {
				break
			}
			parent := tc.direct_parent_ids[int(current)]
			if parent == current {
				break
			}
			current = parent
		}
	}
	for index in tc.top_level_idx {
		if tc.a.nodes[index].kind == .fn_decl
			&& tc.node_tree_contains(flat.NodeId(index), node_id, 0) {
			return flat.NodeId(index)
		}
	}
	return flat.empty_node
}

fn (mut tc TypeChecker) check_struct_implements(node_id flat.NodeId, node flat.Node) {
	mut seen_interfaces := map[string]bool{}
	mut reported_interfaces := map[string]bool{}
	for implements_index, type_name in struct_decl_implements_from_typ(node.typ) {
		base, args, is_generic := generic_type_application_parts(type_name)
		raw_lookup := if is_generic { base } else { type_name }
		lookup := tc.resolve_selective_import_type_symbol(raw_lookup) or { raw_lookup }
		qualified := tc.qualify_name(lookup)
		interface_params := tc.interface_generic_params[lookup] or {
			tc.interface_generic_params[qualified] or { []string{} }
		}
		diagnostic_pos := tc.struct_implements_type_pos(node, type_name, implements_index)
		if interface_params.len > 0 && !is_generic {
			tc.record_error_at(.unknown_type, 'missing generic type on ${lookup}', node_id, diagnostic_pos)
			continue
		}
		if is_generic && interface_params.len > 0 {
			mut unknown_arg := ''
			for raw_arg in args {
				arg := raw_arg.trim_space()
				if arg !in node.generic_params() && tc.type_text_has_generic_placeholder(arg) {
					unknown_arg = arg
					break
				}
			}
			if unknown_arg.len > 0 {
				base_pos := token.new_span(diagnostic_pos.id, diagnostic_pos.offset, int_min(diagnostic_pos.end, diagnostic_pos.offset + raw_lookup.len))
				tc.record_error_at(.unknown_type, 'unknown generic type ${unknown_arg}', node_id, base_pos)
				continue
			}
		}
		if lookup in tc.interface_names || qualified in tc.interface_names {
			interface_name := if lookup in tc.interface_names { lookup } else { qualified }
			if seen_interfaces[interface_name] {
				if !reported_interfaces[interface_name] {
					tc.record_error_at(.assignment_mismatch, 'struct type ${node.value} cannot implement interface `${interface_name} more than once`', node_id, diagnostic_pos)
					reported_interfaces[interface_name] = true
				}
				continue
			}
			seen_interfaces[interface_name] = true
			actual := Type(Struct{
				name: node.value
			})
			expected := Interface{
				name: interface_name
			}
			if !tc.type_implements_interface(actual, expected) {
				tc.record_interface_implementation_error(.assignment_mismatch, actual, expected, node_id, tc.struct_declaration_name_pos(node))
			}
			continue
		}
		if is_generic && (lookup in tc.interface_names || qualified in tc.interface_names) {
			continue
		}
		tc.record_error_at(.assignment_mismatch, '`${type_name}` is not an interface type', node_id, diagnostic_pos)
	}
}

fn (tc &TypeChecker) struct_implements_type_pos(node flat.Node, type_name string, ordinal int) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	decl_pos := tc.struct_declaration_name_pos(node)
	start_limit := int_max(0, int_min(decl_pos.offset, source.len))
	line_end := source[start_limit..].index('{') or { return node.pos }
	header_end := int_min(source.len, start_limit + line_end)
	header := source[start_limit..header_end]
	implements_at := header.index('implements') or { return node.pos }
	mut cursor := start_limit + implements_at + 'implements'.len
	for current in 0 .. ordinal + 1 {
		for cursor < header_end && source[cursor] in [` `, `\t`, `\n`, `\r`, `,`] {
			cursor++
		}
		item_start := cursor
		for cursor < header_end && source[cursor] != `,` {
			cursor++
		}
		mut item_end := cursor
		for item_end > item_start && source[item_end - 1] in [` `, `\t`, `\n`, `\r`] {
			item_end--
		}
		if current == ordinal {
			end := if type_name.contains('.') {
				int_min(item_end, item_start + type_name.all_before('.').len)
			} else {
				item_end
			}
			return token.new_span(node.pos.id, item_start, end)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) struct_declaration_name_pos(node flat.Node) token.Pos {
	if !node.pos.is_valid() {
		return node.pos
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	end := int_max(0, int_min(node.pos.end, source.len))
	needle := 'struct ${node.value}'
	start := source[..end].last_index(needle) or { return node.pos }
	return token.new_span(node.pos.id, start, start + needle.len)
}

fn (mut tc TypeChecker) check_missing_struct_field_generic_type(node_id flat.NodeId, type_text string, generic_params map[string]bool) {
	name := tc.bare_generic_decl_type_name(type_text) or { return }
	qualified := tc.qualify_name(name)
	mut kind := ''
	mut target_params := []string{}
	if name in tc.struct_generic_params || qualified in tc.struct_generic_params {
		kind = 'struct'
		target_params = tc.struct_generic_params[name] or {
			tc.struct_generic_params[qualified] or { []string{} }
		}
	} else if name in tc.interface_generic_params || qualified in tc.interface_generic_params {
		kind = 'interface'
		target_params = tc.interface_generic_params[name] or {
			tc.interface_generic_params[qualified] or { []string{} }
		}
	} else if name in tc.sum_generic_params || qualified in tc.sum_generic_params {
		kind = 'sumtype'
		target_params = tc.sum_generic_params[name] or {
			tc.sum_generic_params[qualified] or { []string{} }
		}
	} else if name in tc.type_alias_generic_params || qualified in tc.type_alias_generic_params {
		parsed := unalias_type(tc.parse_type(name))
		if parsed is FnType {
			kind = 'fn type'
		} else {
			kind = 'alias'
		}
		target_params = tc.type_alias_generic_params[name] or {
			tc.type_alias_generic_params[qualified] or { []string{} }
		}
	} else {
		return
	}
	if kind == 'interface' && bare_generic_type_is_inside_container(type_text) {
		return
	}
	mut context_name := 'int'
	if target_params.len > 0 {
		context_name = target_params[0]
	}
	mut context_names := generic_param_names_from_map(generic_params)
	if context_names.len > 0 {
		context_name = context_names[0]
	}
	node := tc.a.node(node_id)
	mut pos := if node.kind == .field_decl {
		tc.struct_field_type_pos(node)
	} else {
		tc.type_diagnostic_pos(node_id, type_text)
	}
	tc.record_error_at(.unknown_type, '`${name}` type is generic ${kind}, must specify the generic type names, e.g. ${name}[${context_name}], ${name}[int]', node_id, pos)
}

fn bare_generic_type_is_inside_container(type_text string) bool {
	mut clean := trimmed_space(type_text)
	for clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		clean = trimmed_space(clean[1..])
	}
	for prefix in ['shared ', 'atomic ', 'mut '] {
		if clean.starts_with(prefix) {
			clean = trimmed_space(clean[prefix.len..])
		}
	}
	return clean.starts_with('[]') || clean.starts_with('...') || clean.starts_with('map[')
		|| clean.starts_with('[')
}

fn (tc &TypeChecker) bare_generic_decl_type_name(type_text string) ?string {
	clean := trimmed_space(type_text)
	if clean.len == 0 {
		return none
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return tc.bare_generic_decl_type_name(clean[1..])
	}
	if clean.starts_with('shared ') {
		return tc.bare_generic_decl_type_name(clean[7..])
	}
	if clean.starts_with('...') {
		return tc.bare_generic_decl_type_name(clean[3..])
	}
	if clean.starts_with('[]') {
		return tc.bare_generic_decl_type_name(clean[2..])
	}
	if clean.starts_with('map[') {
		close := find_matching_bracket(clean, 3)
		if close > 3 && close + 1 < clean.len {
			if generic_name := tc.bare_generic_decl_type_name(clean[4..close]) {
				return generic_name
			}
			return tc.bare_generic_decl_type_name(clean[close + 1..])
		}
		return none
	}
	if clean.starts_with('[') {
		close := clean.index_u8(`]`)
		if close > 0 && close + 1 < clean.len {
			return tc.bare_generic_decl_type_name(clean[close + 1..])
		}
		return none
	}
	if generic_type_application(clean) || !should_check_named_type(clean) {
		return none
	}
	if scope_type_key(tc.cur_file, tc.cur_module, short_name_view(clean)) in tc.concrete_type_scope_keys {
		return none
	}
	qualified := tc.qualify_name(clean)
	if qualified != clean && tc.type_name_known_in_current_module(clean)
		&& qualified !in tc.struct_generic_params && qualified !in tc.sum_generic_params && qualified !in tc.type_alias_generic_params && qualified !in tc.interface_generic_params {
		return none
	}
	if clean in tc.struct_generic_params || qualified in tc.struct_generic_params
		|| clean in tc.sum_generic_params || qualified in tc.sum_generic_params
		|| clean in tc.type_alias_generic_params || qualified in tc.type_alias_generic_params
		|| clean in tc.interface_generic_params || qualified in tc.interface_generic_params {
		return clean
	}
	return none
}

fn (tc &TypeChecker) concrete_type_declared_in_current_file(name string) bool {
	return scope_type_key(tc.cur_file, tc.cur_module, name.all_after_last('.')) in tc.concrete_type_scope_keys
}

fn (mut tc TypeChecker) check_recursive_alias_decls() {
	mut invalid := []string{}
	for node_idx in tc.top_level_idx {
		node := tc.a.nodes[node_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.type_decl {
				if node.children_count == 0 && node.typ.len > 0
					&& tc.check_recursive_alias_decl(flat.NodeId(node_idx), node) {
					invalid << tc.qualify_decl_name(node.value)
				}
			}
			else {}
		}
	}
	// Invalid aliases must not reach interface-index preparation or type lowering,
	// both of which assume the alias graph is acyclic.
	for name in invalid {
		tc.type_aliases[name] = 'void'
	}
}

fn (mut tc TypeChecker) check_recursive_alias_decl(node_id flat.NodeId, node flat.Node) bool {
	start := tc.qualify_decl_name(node.value)
	if start.len == 0 || start !in tc.type_aliases {
		return false
	}
	mut seen := map[string]bool{}
	if !tc.unguarded_alias_reaches(start, start, mut seen) {
		return false
	}
	if tc.should_diagnose(node_id) {
		tc.record_error(.unknown_type, 'alias `${node.value}` forms a recursive cycle', node_id)
	}
	return true
}

fn (tc &TypeChecker) unguarded_alias_reaches(start string, current string, mut seen map[string]bool) bool {
	if current in seen {
		return false
	}
	seen[current] = true
	target := tc.type_aliases[current] or { return false }
	mut names := []string{}
	collect_unguarded_alias_type_names(target, mut names)
	for name in names {
		dependency := tc.alias_dependency_key(name, current) or { continue }
		if dependency == start {
			return true
		}
		if tc.unguarded_alias_reaches(start, dependency, mut seen) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) alias_dependency_key(name string, current string) ?string {
	clean := name.trim_space()
	if clean.len == 0 {
		return none
	}
	if clean in tc.type_aliases {
		return clean
	}
	if !clean.contains('.') && current.contains('.') {
		candidate := '${current.all_before_last('.')}.${clean}'
		if candidate in tc.type_aliases {
			return candidate
		}
	}
	return none
}

fn collect_unguarded_alias_type_names(raw string, mut names []string) {
	mut clean := raw.trim_space()
	if clean.len == 0 {
		return
	}
	for {
		if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('mut ') {
			clean = clean[4..].trim_space()
			continue
		}
		if clean.starts_with('shared ') || clean.starts_with('atomic ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('chan ') {
			clean = clean[5..].trim_space()
			continue
		}
		if clean.starts_with('thread ') {
			clean = clean[7..].trim_space()
			continue
		}
		break
	}
	// A callback introduces an indirection boundary: aliases such as
	// `type Handlers = map[string]fn (Handlers)` are recursive but finite.
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return
	}
	if clean.starts_with('...') {
		collect_unguarded_alias_type_names(clean[3..], mut names)
		return
	}
	if clean.starts_with('[]') {
		collect_unguarded_alias_type_names(clean[2..], mut names)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			collect_unguarded_alias_type_names(clean[4..bracket_end], mut names)
			collect_unguarded_alias_type_names(clean[bracket_end + 1..], mut names)
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			collect_unguarded_alias_type_names(clean[bracket_end + 1..], mut names)
		}
		return
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		for part in split_params(clean[1..clean.len - 1]) {
			collect_unguarded_alias_type_names(part, mut names)
		}
		return
	}
	base, args, is_generic := generic_type_application_parts(clean)
	if is_generic {
		push_type_name_candidate(mut names, base)
		for arg in args {
			collect_unguarded_alias_type_names(arg, mut names)
		}
		return
	}
	push_type_name_candidate(mut names, clean)
}

fn (tc &TypeChecker) infer_decl_generic_params(node flat.Node) map[string]bool {
	mut params := map[string]bool{}
	for name in node.generic_params() {
		params[name] = true
	}
	tc.collect_generic_receiver_params(node, mut params)
	return params
}

fn (tc &TypeChecker) infer_decl_generic_param_names(node flat.Node) []string {
	params := tc.infer_decl_generic_params(node)
	return generic_param_names_from_map(params)
}

fn generic_param_names_from_map(params map[string]bool) []string {
	mut names := []string{cap: params.len}
	for name, _ in params {
		names << name
	}
	names.sort()
	return names
}

fn generic_param_map_from_names(names []string) map[string]bool {
	mut params := map[string]bool{}
	for name in names {
		params[name] = true
	}
	return params
}

fn (tc &TypeChecker) active_generic_param(name string) bool {
	for param in tc.fn_context.generic_params {
		if param == name {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) node_has_enclosing_generic_param(node_id flat.NodeId, name string) bool {
	if name.len == 0 || int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(node_id)]
	if name in node.generic_params() {
		return true
	}
	params := tc.enclosing_generic_params_by_node[int(node_id)] or { return false }
	return name in params
}

@[direct_array_access]
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
	mut receiver_type := receiver.typ.trim_space()
	for receiver_type.starts_with('shared ') || receiver_type.starts_with('atomic ') {
		receiver_type = receiver_type[7..].trim_space()
	}
	receiver_type = receiver_type.trim_left('&')
	if receiver_type != owner_name_view(node.value) {
		return
	}
	receiver_base, receiver_args, is_generic := generic_type_application_parts(receiver_type)
	if is_generic {
		decl_params := tc.struct_generic_params[receiver_base] or {
			tc.struct_generic_params[receiver_base.all_after_last('.')] or { []string{} }
		}
		if receiver_args.len == decl_params.len {
			for arg in receiver_args {
				if arg in decl_params {
					params[arg] = true
				}
			}
		}
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
	clean := trimmed_space(typ)
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
				trimmed := trimmed_space(part)
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
	clean := trimmed_space(typ)
	if clean.len == 0 {
		return
	}
	if clean in ['generic', 'params', 'union'] {
		return
	}
	if clean == 'any' {
		tc.record_invalid_any_decl_type(node_id)
		return
	}
	if is_bare_generic_param(clean) {
		node := tc.a.node(node_id)
		if node.kind == .ident {
			return
		}
		if node.kind == .type_decl && clean !in generic_params && !tc.type_name_known(clean) {
			tc.record_error_at(.unknown_type, 'unknown aliased type `${clean}`', node_id, tc.type_diagnostic_pos(node_id, clean))
			return
		}
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
			tc.check_type_string_for_unsupported_generics(clean[4..bracket_end], node_id, generic_params)
			tc.check_type_string_for_unsupported_generics(clean[bracket_end + 1..], node_id, generic_params)
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
			tc.record_unsupported_generic('unsupported generic type application `${clean}`', node_id)
			return
		}
		bracket := clean.index_u8(`[`)
		bracket_end := find_matching_bracket(clean, bracket)
		base := trimmed_space(clean[..bracket])
		if should_check_named_type(base) && !tc.type_name_known(base) {
			tc.record_unknown_decl_type(base, node_id)
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
		if clean in generic_params || tc.active_generic_param(clean)
			|| tc.node_has_enclosing_generic_param(node_id, clean) {
			return
		}
	}
	if should_check_named_type(clean) && !tc.type_name_known(clean) {
		if is_bare_generic_param(clean) && tc.unmentioned_generic_type_was_reported(clean, node_id) {
			return
		}
		tc.record_unknown_decl_type(clean, node_id)
	}
}

fn (mut tc TypeChecker) check_sum_type_decl(node_id flat.NodeId, node flat.Node) {
	mut variants := []string{}
	mut variant_ids := []flat.NodeId{}
	if node.children_count > 0 {
		for i in 0 .. node.children_count {
			variant_id := tc.a.child(&node, i)
			variant := tc.a.node(variant_id)
			variants << variant.value
			variant_ids << variant_id
		}
	} else {
		for variant in split_sum_variant_texts(node.typ) {
			variants << variant
			variant_ids << node_id
		}
	}
	mut seen := map[string]bool{}
	mut result_reported := false
	mut pointer_reported := false
	for i, raw_variant in variants {
		variant_name := trimmed_space(raw_variant)
		variant_id := variant_ids[i]
		variant_type := tc.parse_type(variant_name)
		clean_variant_type := unalias_type(variant_type)
		semantic_name := variant_type.name()
		variant_key := if variant_type is Unknown { variant_name } else { semantic_name }
		if seen[variant_key] {
			tc.record_error_at(.duplicate_decl, 'sum type ${node.value} cannot hold the type `${semantic_name.all_after_last('.')}` more than once', variant_id, tc.type_diagnostic_pos(variant_id, variant_name))
			continue
		}
		seen[variant_key] = true
		is_builtin_pointer := variant_name in ['voidptr', 'byteptr', 'charptr']
			|| variant_type.name() in ['voidptr', 'byteptr', 'charptr']
		if clean_variant_type is Pointer && !is_builtin_pointer && !pointer_reported {
			display_variant := variant_name.trim_left('&').trim_space()
			display_type := tc.parse_type(display_variant)
			left, right := if unalias_type(display_type) is Struct {
				'{', '}'
			} else {
				'(', ')'
			}
			tc.record_error_with_details_at(.assignment_mismatch, 'sum type cannot hold a reference type', variant_id, tc.type_diagnostic_pos(variant_id, variant_name), [
				'declare the sum type with non-reference types: `${node.value} = ${display_variant} | ...`\nand use a reference to the sum type instead: `var := &${node.value}(${display_variant}${left}val${right})`',
			])
			pointer_reported = true
		}
		if variant_type is ResultType && !result_reported {
			tc.record_error_at(.assignment_mismatch, 'sum type cannot hold a Result type', variant_id, tc.type_diagnostic_pos(variant_id, variant_name))
			result_reported = true
		}
		sum_name := tc.sum_base_name(node.value)
		variant_sum := tc.sum_base_name(variant_name)
		if variant_sum == sum_name {
			tc.record_error_at(.assignment_mismatch, 'sum type cannot hold itself', variant_id, tc.type_diagnostic_pos(variant_id, variant_name))
		} else {
			mut seen_sums := map[string]bool{}
			if (variant_sum in tc.sum_types
				&& tc.sum_type_reaches_sum(variant_sum, sum_name, mut seen_sums))
				|| ((variant_name.starts_with('fn(')
					|| variant_name.starts_with('fn ('))
					&& type_text_contains_symbol(variant_name, node.value)) {
				pos := if variant_name.starts_with('fn(') || variant_name.starts_with('fn (') {
					tc.sum_type_variant_source_pos(node, variant_name)
				} else {
					tc.type_diagnostic_pos(variant_id, variant_name)
				}
				tc.record_error_at(.assignment_mismatch, 'sum type `${node.value}` cannot be defined recursively', variant_id, pos)
			}
		}
	}
}

fn (tc &TypeChecker) sum_type_variant_source_pos(node flat.Node, variant string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	line_start := if relative := source[..start].last_index('\n') { relative + 1 } else { 0 }
	line_end := source.index_after('\n', start) or { source.len }
	line := source[line_start..line_end]
	if variant.starts_with('fn(') || variant.starts_with('fn (') {
		fn_relative := line.index('fn ') or { line.index('fn(') or { -1 } }
		if fn_relative >= 0 {
			variant_start := line_start + fn_relative
			mut depth := 0
			mut variant_end := line_end
			for i in variant_start .. line_end {
				if source[i] in [`(`, `[`, `{`] {
					depth++
				} else if source[i] in [`)`, `]`, `}`] {
					if depth > 0 {
						depth--
					}
				} else if source[i] == `|` && depth == 0 {
					variant_end = i
					for variant_end > variant_start && source[variant_end - 1] in [` `, `\t`] {
						variant_end--
					}
					break
				}
			}
			return token.new_span(node.pos.id, variant_start, variant_end)
		}
	}
	variant_parts := split_sum_variant_texts(variant)
	needle := if variant_parts.len > 0 { variant_parts[0] } else { variant }
	if relative := line.index(needle) {
		variant_start := line_start + relative
		return token.new_span(node.pos.id, variant_start, variant_start + needle.len)
	}
	return node.pos
}

fn (tc &TypeChecker) sum_type_reaches_sum(current string, target string, mut seen map[string]bool) bool {
	current_base := tc.sum_base_name(current)
	target_base := tc.sum_base_name(target)
	if current_base == target_base {
		return true
	}
	if seen[current_base] {
		return false
	}
	seen[current_base] = true
	for variant in tc.sum_types[current_base] or { []string{} } {
		variant_base := tc.sum_base_name(variant)
		if variant_base in tc.sum_types
			&& tc.sum_type_reaches_sum(variant_base, target_base, mut seen) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) record_invalid_any_decl_type(node_id flat.NodeId) {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return
	}
	node := tc.a.nodes[int(node_id)]
	if node.kind !in [.fn_decl, .c_fn_decl, .param, .field_decl, .interface_field, .type_decl, .ident] {
		return
	}
	if node.kind == .param {
		tc.record_notice_at(.unknown_type, 'the `any` type is deprecated and will be removed soon - either use an empty interface, or a sum type', node_id, tc.node_value_diagnostic_pos(node_id))
		return
	}
	msg := if node.kind == .type_decl {
		'unknown aliased type `any`'
	} else {
		'cannot use type `any` here'
	}
	tc.record_error_at(.unknown_type, msg, node_id, tc.type_diagnostic_pos(node_id, 'any'))
}

fn (tc &TypeChecker) node_value_diagnostic_pos(node_id flat.NodeId) token.Pos {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return token.Pos{}
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	return closest_identifier_span(source, node.value, node.pos.offset, node.pos.id) or { node.pos }
}

fn (mut tc TypeChecker) record_unknown_decl_type(name string, node_id flat.NodeId) {
	should_diagnose := tc.should_diagnose(node_id)
	// Imported modules are normally outside the diagnostic-file set. Avoid
	// constructing source positions and the full type suggestion list for
	// their intentionally unresolved generic annotations. The sole exception
	// below is a bare annotation that only appears to resolve because a
	// selected main file declares the same type.
	qualified := if tc.cur_module.len > 0 { '${tc.cur_module}.${name}' } else { name }
	report_import_scope_error := !should_diagnose && !name.contains('.')
		&& name !in ['Error', 'MessageError', 'IError'] && tc.cur_module !in ['', 'main', 'builtin'] && tc.selected_main_file_declares_type(name) && !tc.qualify_candidate_type_exists(qualified)
		&& !tc.source_declares_type_in_scope(name, tc.cur_file, tc.cur_module)
	if !should_diagnose && !report_import_scope_error {
		return
	}
	pos := tc.type_diagnostic_pos(node_id, name)
	if tc.unknown_type_already_reported_on_line(name, pos) {
		return
	}
	msg := tc.unknown_type_message(name, node_id)
	if should_diagnose {
		tc.record_error_at(.unknown_type, msg, node_id, pos)
		return
	}
	if report_import_scope_error {
		tc.record_error_unfiltered(.unknown_type, msg, node_id)
	}
}

fn (tc &TypeChecker) unknown_type_already_reported_on_line(name string, pos token.Pos) bool {
	if !pos.is_valid() {
		return false
	}
	msg := 'unknown type `${name}`'
	current_position := tc.a.source_position(pos) or { return false }
	for err in tc.errors {
		if !err.msg.starts_with(msg) || !err.pos.is_valid() || pos.id != err.pos.id {
			continue
		}
		error_position := tc.a.source_position(err.pos) or { continue }
		if current_position.line == error_position.line {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) type_error_already_reported_on_line(kind TypeErrorKind, msg string, pos token.Pos) bool {
	if !pos.is_valid() {
		return false
	}
	current_position := tc.a.source_position(pos) or { return false }
	for err in tc.errors {
		if err.kind != kind || err.msg != msg || !err.pos.is_valid() || pos.id != err.pos.id {
			continue
		}
		error_position := tc.a.source_position(err.pos) or { continue }
		if current_position.line == error_position.line {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) unknown_type_message(name string, node_id flat.NodeId) string {
	mut display_name := name
	if name.contains('.') {
		alias := name.all_before('.')
		if import_path := tc.current_file_import_path_for_alias(alias) {
			display_name = import_path + name[alias.len..]
		}
	}
	base := 'unknown type `${display_name}`'
	if int(node_id) >= 0 && int(node_id) < tc.a.nodes.len {
		node := tc.a.nodes[int(node_id)]
		if node.kind == .type_decl && (node.typ.starts_with('fn(') || node.typ.starts_with('fn (')) {
			return base
		}
		if node.kind == .type_decl && node.typ.starts_with('[') {
			return '${base}.\nDid you mean `${node.value}`?'
		}
		if node.kind == .field_decl && node.typ.starts_with('[') && node.typ.contains('fn') {
			suggestion := tc.parse_type(node.typ).name().replace_once('fn(', 'fn (')
			return '${base}.\nDid you mean `${suggestion}`?'
		}
	}
	contextual_candidates := tc.contextual_generic_sum_type_candidates(node_id)
	candidates := if contextual_candidates.len > 0 {
		contextual_candidates
	} else {
		tc.known_type_name_candidates()
	}
	message := util.new_suggestion(name, candidates).say(base)
	if message != base || int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return message
	}
	unknown_node := tc.a.nodes[int(node_id)]
	if unknown_node.kind in [.interface_field, .param] {
		for index in tc.top_level_idx {
			decl := tc.a.nodes[index]
			if decl.kind != .interface_decl {
				continue
			}
			if tc.node_tree_contains(flat.NodeId(index), node_id, 0) {
				return '${base}.\nDid you mean `${decl.value.all_after_last('.')}`?'
			}
		}
	}
	if unknown_node.kind != .field_decl {
		return message
	}
	// Very short field type names have no useful bigrams for the default Dice
	// matcher. Fall back to a case-insensitive edit distance so `[]abc` beside
	// `struct Aaa` still receives the same useful suggestion as v1.
	mut closest := ''
	mut closest_distance := name.len + 1
	lower_name := name.to_lower()
	for candidate in candidates {
		lower_candidate := candidate.to_lower()
		if lower_name.len == 0 || lower_candidate.len == 0 || lower_name[0] != lower_candidate[0] {
			continue
		}
		distance := strings.levenshtein_distance(lower_name, lower_candidate)
		max_distance := if name.len <= 3 { 2 } else { int_max(1, name.len / 3) }
		if distance <= max_distance && distance < closest_distance {
			closest = candidate
			closest_distance = distance
		}
	}
	if closest.len > 0 {
		return '${base}.\nDid you mean `${closest}`?'
	}
	return message
}

fn (tc &TypeChecker) contextual_generic_sum_type_candidates(node_id flat.NodeId) []string {
	if !tc.valid_node_id(node_id) {
		return []string{}
	}
	node := tc.a.node(node_id)
	if node.kind != .type_decl {
		return []string{}
	}
	base, args, is_generic := generic_type_application_parts(node.typ.trim_space())
	if !is_generic {
		return []string{}
	}
	qualified := tc.qualify_name(base)
	params := tc.sum_generic_params[base] or {
		tc.sum_generic_params[qualified] or { return []string{} }
	}
	variants := tc.sum_types[base] or { tc.sum_types[qualified] or { return []string{} } }
	mut candidates := []string{cap: variants.len}
	for variant in variants {
		substituted := tc.substitute_generic_type(tc.parse_type(variant), args, params).name()
		if substituted.len > 0 && substituted !in candidates {
			candidates << substituted
		}
	}
	return candidates
}

fn (tc &TypeChecker) node_tree_contains(root_id flat.NodeId, target_id flat.NodeId, depth int) bool {
	if root_id == target_id {
		return true
	}
	if depth > 16 || !tc.valid_node_id(root_id) {
		return false
	}
	root := tc.a.nodes[int(root_id)]
	for child_index in 0 .. root.children_count {
		if tc.node_tree_contains(tc.a.child(&root, child_index), target_id, depth + 1) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) known_type_name_candidates() []string {
	mut names := []string{}
	for name, _ in tc.structs {
		push_diagnostic_type_name(mut names, name)
	}
	for name, _ in tc.type_aliases {
		push_diagnostic_type_name(mut names, name)
	}
	for name, _ in tc.sum_types {
		push_diagnostic_type_name(mut names, name)
	}
	for name, _ in tc.enum_names {
		push_diagnostic_type_name(mut names, name)
	}
	for name, _ in tc.interface_names {
		push_diagnostic_type_name(mut names, name)
	}
	for node in tc.a.nodes {
		if node.kind in [.struct_decl, .type_decl, .enum_decl, .interface_decl] {
			push_diagnostic_type_name(mut names, node.value)
		}
	}
	names.sort()
	return names
}

fn push_diagnostic_type_name(mut names []string, name string) {
	display_name := if name.starts_with('main.') {
		name[5..]
	} else if name.starts_with('builtin.') {
		name[8..]
	} else {
		name
	}
	if display_name.len > 0 && display_name !in names {
		names << display_name
	}
}

fn (tc &TypeChecker) type_diagnostic_pos(node_id flat.NodeId, name string) token.Pos {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return token.Pos{}
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	mut type_text := match node.kind {
		.fn_decl, .c_fn_decl, .param, .field_decl, .interface_field, .type_decl {
			node.typ
		}
		.ident {
			node.value
		}
		else {
			''
		}
	}
	if type_text.starts_with('chan ') || type_text.contains('fn(')
		|| type_text.contains('fn (') || !type_text.contains(name) {
		type_text = name
	}
	mut candidates := []string{}
	if type_text.len > 0 {
		candidates << type_text
	}
	if name.len > 0 && name !in candidates {
		candidates << name
	}
	anchor := int_max(0, int_min(node.pos.offset, source.len))
	for candidate in candidates {
		if span := closest_text_span(source, candidate, anchor, node.pos.id) {
			return span
		}
	}
	return node.pos
}

fn closest_text_span(source string, text string, anchor int, file_id int) ?token.Pos {
	if text.len == 0 || source.len < text.len {
		return none
	}
	mut search_from := 0
	mut best_start := -1
	mut best_distance := source.len + text.len
	for search_from <= source.len - text.len {
		relative := source[search_from..].index(text) or { break }
		start := search_from + relative
		distance := if start > anchor { start - anchor } else { anchor - start }
		if distance < best_distance {
			best_start = start
			best_distance = distance
		}
		search_from = start + 1
	}
	if best_start < 0 {
		return none
	}
	return token.new_span(file_id, best_start, best_start + text.len)
}

fn closest_identifier_span(source string, name string, anchor int, file_id int) ?token.Pos {
	if name.len == 0 || source.len < name.len {
		return none
	}
	// The anchor normally sits on (or immediately before) the identifier, so
	// search outward from it instead of scanning the file from the top: the
	// nearest word match on each side decides, exactly like the old full scan.
	mut left_start := -1
	for i := int_min(anchor, source.len - name.len); i >= 0; i-- {
		if !identifier_word_match_at(source, name, i) {
			continue
		}
		left_start = i
		break
	}
	mut right_start := -1
	mut from := int_max(anchor + 1, 0)
	for from <= source.len - name.len {
		start := source.index_after_(name, from)
		if start < 0 {
			break
		}
		if identifier_word_match_at(source, name, start) {
			right_start = start
			break
		}
		from = start + 1
	}
	best_start := if left_start < 0 {
		right_start
	} else if right_start < 0 {
		left_start
	} else if anchor - left_start <= right_start - anchor {
		left_start
	} else {
		right_start
	}
	if best_start < 0 {
		return none
	}
	return token.new_span(file_id, best_start, best_start + name.len)
}

@[direct_array_access]
fn identifier_word_match_at(source string, name string, start int) bool {
	if start < 0 || start + name.len > source.len {
		return false
	}
	for j in 0 .. name.len {
		if source[start + j] != name[j] {
			return false
		}
	}
	if start > 0 && (source[start - 1].is_alnum() || source[start - 1] == `_`) {
		return false
	}
	end := start + name.len
	if end < source.len && (source[end].is_alnum() || source[end] == `_`) {
		return false
	}
	return true
}

fn (tc &TypeChecker) selected_main_file_declares_type(name string) bool {
	for file, selected in tc.diagnostic_files {
		if selected && !file.starts_with('generic:')
			&& tc.source_declares_type_in_scope(name, file, 'main') {
			return true
		}
	}
	return false
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
	ret := trimmed_space(typ[params_end + 1..])
	tc.check_type_string_for_unsupported_generics(ret, node_id, generic_params)
	for part in split_params(typ[params_start..params_end]) {
		trimmed := trimmed_space(part)
		parts := trimmed.split(' ')
		param_type := if parts.len >= 2 { parts[parts.len - 1] } else { trimmed }
		tc.check_type_string_for_unsupported_generics(param_type, node_id, generic_params)
	}
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
	clean := trimmed_space(typ)
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

fn (tc &TypeChecker) type_text_has_unbound_generic_placeholder(typ string, bound []string) bool {
	clean := trimmed_space(typ)
	if is_bare_generic_param(clean) {
		return !tc.is_known_type_text(clean) && clean !in bound
	}
	if clean.starts_with('&') {
		return tc.type_text_has_unbound_generic_placeholder(clean[1..], bound)
	}
	if clean.starts_with('mut ') {
		return tc.type_text_has_unbound_generic_placeholder(clean[4..], bound)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return tc.type_text_has_unbound_generic_placeholder(clean[1..], bound)
	}
	if clean.starts_with('...') {
		return tc.type_text_has_unbound_generic_placeholder(clean[3..], bound)
	}
	if clean.starts_with('[]') {
		return tc.type_text_has_unbound_generic_placeholder(clean[2..], bound)
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return tc.type_text_has_unbound_generic_placeholder(clean[4..bracket_end], bound)
				|| tc.type_text_has_unbound_generic_placeholder(clean[bracket_end + 1..], bound)
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return tc.type_text_has_unbound_generic_placeholder(clean[bracket_end + 1..], bound)
		}
	}
	_, args, ok := generic_type_application_parts(clean)
	if ok {
		for arg in args {
			if tc.type_text_has_unbound_generic_placeholder(arg, bound) {
				return true
			}
		}
	}
	if clean.contains('.') && is_bare_generic_param(clean.all_after_last('.')) {
		short := clean.all_after_last('.')
		return !tc.is_known_type_text(clean) && short !in bound
	}
	return false
}

fn (tc &TypeChecker) type_text_has_generic_struct_placeholder_application(typ string) bool {
	clean := trimmed_space(typ)
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return tc.type_text_has_generic_struct_placeholder_application(clean[1..])
	}
	if clean.starts_with('mut ') {
		return tc.type_text_has_generic_struct_placeholder_application(clean[4..])
	}
	if clean.starts_with('...') {
		return tc.type_text_has_generic_struct_placeholder_application(clean[3..])
	}
	if clean.starts_with('[]') {
		return tc.type_text_has_generic_struct_placeholder_application(clean[2..])
	}
	if clean.starts_with('[') {
		close := find_matching_bracket(clean, 0)
		if close > 0 && close + 1 < clean.len {
			return tc.type_text_has_generic_struct_placeholder_application(clean[close + 1..])
		}
		return false
	}
	base, args, is_generic := generic_type_application_parts(clean)
	if !is_generic {
		return false
	}
	qualified := tc.qualify_name(base)
	if (base in tc.struct_generic_params || qualified in tc.struct_generic_params)
		&& args.any(tc.type_text_has_generic_placeholder(it)) {
		return true
	}
	for arg in args {
		if tc.type_text_has_generic_struct_placeholder_application(arg) {
			return true
		}
	}
	return false
}

fn generic_type_application_parts(typ string) (string, []string, bool) {
	if typ.len == 0 || typ[0] == `[` {
		return '', []string{}, false
	}
	bracket := typ.index_u8(`[`)
	if bracket < 0 {
		return '', []string{}, false
	}
	bracket_end := find_matching_bracket(typ, bracket)
	if bracket <= 0 || bracket_end <= bracket {
		return '', []string{}, false
	}
	inner := trimmed_space(typ[bracket + 1..bracket_end])
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
	s := trimmed_space(inner)
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
			name: qname
			base_type: tc.parse_type(tc.type_aliases[qname])
		})
	}
	if allow_bare_symbol && type_name in tc.type_aliases {
		return Type(Alias{
			name: type_name
			base_type: tc.parse_type(tc.type_aliases[type_name])
		})
	}
	return fallback
}

// type_name_known returns type name known data for TypeChecker.
fn (tc &TypeChecker) type_name_known(typ string) bool {
	if is_builtin_type_name(typ) || typ == 'unknown' || typ.starts_with('C.')
		|| tc.active_generic_param(typ) {
		return true
	}
	generic_base := strip_generic_args_name(typ)
	if generic_base != typ {
		return tc.type_name_known(generic_base)
	}
	qtyp := tc.qualify_name(typ)
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			return tc.type_symbol_known(resolved)
		}
	} else if short := tc.imported_type_short_name(typ) {
		if short in tc.type_aliases || short in tc.structs || short in tc.interface_names
			|| short in tc.enum_names || short in tc.sum_types {
			return true
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

fn (tc &TypeChecker) recursive_struct_declaration_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	needle := 'struct ${node.value.all_after_last('.')}'
	start := source.index(needle) or { return node.pos }
	return token.new_span(node.pos.id, start, start + needle.len)
}

// check_struct_field_defaults validates check struct field defaults state for types.
fn (mut tc TypeChecker) check_struct_field_defaults(node_id flat.NodeId, node flat.Node) {
	saved_generic_params := tc.fn_context.generic_params.clone()
	if node.generic_params().len > 0 {
		tc.fn_context.generic_params = node.generic_params().clone()
	}
	mut seen_field_names := map[string]bool{}
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_text := if field.typ.len > 0 { field.typ } else { field.value }
		if bound := fixed_array_bound_text(field_type_text) {
			if bound.starts_with('\$') && !bound.starts_with('\$d(') {
				tc.record_error_at(.assignment_mismatch, 'only \$d() is supported as fixed array size quantifier at compile time', field_id, tc.fixed_array_bound_pos(*field, bound))
			}
		}
		field_type_raw := tc.parse_type(field_type_text)
		field_type := unalias_type(field_type_raw)
		if field_type is OptionType {
			recursive_type := unalias_type(field_type.base_type)
			if recursive_type is Struct
				&& tc.qualify_name(recursive_type.name) == tc.qualify_name(node.value) {
				tc.record_error_at(.unknown_type, 'recursive struct is only possible with optional pointer (e.g. ?&${node.value.all_after_last('.')})', field_id, field.pos)
			}
		}
		is_embed := source_field_decl_is_embed(field, field_type_text)
		if is_embed && field_type is Struct
			&& tc.qualify_name(field_type.name) == tc.qualify_name(node.value) {
			tc.record_error_at(.unknown_type, 'invalid recursive struct `${node.value}`', node_id, tc.recursive_struct_declaration_pos(node))
		}
		if field_type_text == 'map' {
			tc.record_error_at(.unknown_type, 'cannot use the map type without key and value definition', field_id, tc.struct_field_type_pos(*field))
		}
		if !is_embed && field.value in seen_field_names {
			tc.record_error_at(.duplicate_decl, 'field name `${field.value}` duplicate', field_id, tc.struct_field_declaration_pos(*field))
		} else if !is_embed {
			seen_field_names[field.value] = true
		}
		if !is_embed && node.value != 'C.' && !node.value.starts_with('C.')
			&& tc.should_check_source_name(field_id) && !snake_case_name_is_valid(field.value) {
			tc.check_snake_case_name(field_id, field.value, 'field name', tc.struct_field_declaration_pos(*field))
		}
		if field_type is MultiReturn {
			tc.record_error_at(.assignment_mismatch, 'cannot use multi return as field type', field_id, tc.struct_field_type_pos(*field))
		}
		if field_type is None {
			tc.record_error_at(.assignment_mismatch, 'cannot use `none` as field type', field_id, tc.struct_field_type_pos(*field))
		}
		embedded_alias_target := unalias_type(unwrap_all_pointers(field_type))
		if is_embed && field_type_raw is Alias && embedded_alias_target !is Struct
			&& embedded_alias_target !is Interface && embedded_alias_target !is FnType {
			is_anonymous := is_anonymous_struct_name(node.value)
			if is_anonymous {
				tc.record_error_at(.assignment_mismatch, 'cannot embed non-struct `${field_type_text}`', field_id, tc.anonymous_struct_type_head_pos(node))
			}
			message := if is_anonymous {
				'cannot embed non-struct `${field_type_text}`'
			} else {
				'`${field_type_text}` (alias of `${field_type.name()}`) is not a struct'
			}
			tc.record_error_at(.assignment_mismatch, message, field_id, tc.struct_field_declaration_pos(*field))
		}
		if !is_embed && field_type_raw is Alias && field_type is Struct && field_type.name.starts_with('C.') && field_type.name in tc.c_typedef_structs && (tc.structs[field_type.name] or {
			[]StructField{}
		}).len == 0 {
			tc.record_error_at(.assignment_mismatch, 'cannot use opaque C struct `${field_type_text}` as a non-reference struct field; use `&${field_type_text}` instead', field_id, tc.struct_field_type_pos(*field))
		}
		if field.children_count > 0
			&& tc.struct_field_type_uses_comptime_define(field_id, field_type_text) {
			default_id := tc.a.child(field, 0)
			tc.record_error_at(.assignment_mismatch, 'cannot initialize a fixed size array field that uses `\$d()` as size quantifier since the size may change via -d', default_id, tc.a.node(default_id).pos)
		}
		if field_type is ResultType {
			type_pos := tc.struct_field_type_pos(*field)
			tc.record_error_at(.assignment_mismatch, 'struct field does not support storing Result', field_id, token.new_span(type_pos.id, type_pos.offset, type_pos.offset + 1))
		}
		if field.children_count > 0 {
			default_id := tc.a.child(field, 0)
			default := tc.a.node(default_id)
			if tc.type_has_declaration_attribute(field_type_raw, 'nocopy') {
				tc.record_error_at(.assignment_mismatch, 'cannot copy @[nocopy] struct: use a reference instead', default_id, default.pos)
			}
			if source_field_decl_is_mut(*field) && field_type is Map && default.kind == .ident
				&& tc.const_key_for_name(default.value) != none {
				tc.record_error_at(.assignment_mismatch, 'cannot copy map: call `clone` method (or use a reference)', default_id, default.pos)
			}
			if default.kind == .int_literal && default.value == '0' && field_type.is_integer() {
				tc.record_warning_at(.assignment_mismatch, 'unnecessary default value of `0`: struct fields are zeroed by default', default_id, default.pos)
			} else if default.kind == .string_literal && default.value.len == 0
				&& field_type is String {
				tc.record_warning_at(.assignment_mismatch, "unnecessary default value of '': struct fields are zeroed by default", default_id, default.pos)
			} else if default.kind == .bool_literal && default.value == 'false'
				&& field_type.name() == 'bool' {
				tc.record_warning_at(.assignment_mismatch, 'unnecessary default value `false`: struct fields are zeroed by default', default_id, default.pos)
			}
		}
		if field.children_count == 0 && field_type is FnType && !tc.translated_files[tc.cur_file]
			&& !struct_field_has_attr(*field, 'required') {
			tc.record_notice_at(.assignment_mismatch, 'uninitialized `fn` struct fields are not allowed, since they can result in segfaults; use `?fn` or `@[required]` or initialize the field with `=` (if you absolutely want to have unsafe function pointers, use `= unsafe { nil }`)', field_id, tc.struct_field_declaration_pos(*field))
		}
	}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl || field.children_count == 0
			|| unalias_type(tc.parse_type(field.typ)) !is OptionType {
			continue
		}
		default_id := tc.a.child(field, 0)
		if tc.a.node(default_id).kind == .none_expr {
			tc.record_warning_at(.assignment_mismatch, 'unnecessary default value of `none`: struct fields are zeroed by default', default_id, tc.a.node(default_id).pos)
		}
	}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl || field.children_count == 0
			|| unalias_type(tc.parse_type(field.typ)) !is OptionType {
			continue
		}
		default_id := tc.a.child(field, 0)
		if tc.a.node(default_id).kind == .nil_literal {
			tc.record_error_at(.assignment_mismatch, '`nil` is only allowed in `unsafe` code', default_id, tc.a.node(default_id).pos)
		}
	}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl || field.children_count == 0 {
			continue
		}
		default_id := tc.a.child(field, 0)
		if tc.a.node(default_id).kind == .nil_literal || tc.expr_is_unsafe_nil(default_id) {
			expected := unalias_type(tc.parse_type(field.typ))
			if expected is OptionType && unalias_type(expected.base_type) is Pointer {
				tc.record_error_at(.assignment_mismatch, 'cannot assign `nil` to option value', default_id, tc.array_element_diagnostic_pos(default_id))
			} else if expected !is Pointer && expected !is FnType && !(expected is OptionType
				&& unalias_type(expected.base_type) is FnType) {
				tc.record_error_at(.assignment_mismatch, 'cannot assign `nil` to a non-pointer field', default_id, tc.struct_field_type_pos(*field))
			}
		}
	}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl || field.children_count == 0 {
			continue
		}
		default_id := tc.a.child(field, 0)
		field_type := if field.typ.len > 0 { field.typ } else { field.value }
		expected := tc.parse_type(field_type)
		clean_expected := unalias_type(expected)
		default_node := tc.a.nodes[int(default_id)]
		if (default_node.kind == .nil_literal || tc.expr_is_unsafe_nil(default_id))
			&& clean_expected !is Pointer {
			continue
		}
		if clean_expected is OptionType && (default_node.kind in [.none_expr, .nil_literal]
			|| tc.expr_is_unsafe_nil(default_id)) {
			continue
		}
		if clean_expected is Struct
			&& clean_expected.name.all_after_last('.') == node.value.all_after_last('.') {
			tc.record_error_at(.assignment_mismatch, 'field `${field.value}` is part of `${node.value}`, they can not both have the same type', tc.a.child(&node, i), tc.struct_field_type_pos(field))
			continue
		}
		if clean_expected is Array {
			elem_type := unalias_type(clean_expected.elem_type)
			if elem_type is Struct
				&& elem_type.name.all_after_last('.') == node.value.all_after_last('.') {
				type_pos := tc.struct_field_type_pos(field)
				tc.record_error_at(.assignment_mismatch, 'cannot initialize array of same struct type that is being defined (recursion detected)', tc.a.child(&node, i), token.new_span(field.pos.id, field.pos.offset, type_pos.end))
				continue
			}
		}
		tc.annotate_expected_expr(default_id, expected)
		tc.check_node_with_expected_context(default_id, expected)
		actual := tc.resolve_expr(default_id, expected)
		if type_is_unsigned_integer(expected) && tc.expr_is_negative_integer_literal(default_id) {
			tc.record_error_at(.assignment_mismatch, 'cannot assign negative value to unsigned integer type', default_id, default_node.pos)
		}
		if clean_expected !is Pointer && unalias_type(actual) is Pointer {
			diagnostic_pos := if default_node.kind == .call && default_node.children_count > 0 {
				callee := tc.a.child_node(default_node, 0)
				if callee.kind == .selector {
					tc.method_call_name_pos(default_node, callee)
				} else {
					default_node.pos
				}
			} else {
				default_node.pos
			}
			tc.record_error_at(.assignment_mismatch, 'field is not reference but default value is reference', default_id, diagnostic_pos)
			continue
		}
		if expected.name() == 'voidptr' && unalias_type(actual).is_integer() {
			if tc.should_diagnose(default_id) {
				source_value := tc.source_text_for_node(default_id)
				tc.record_notice_at(.assignment_mismatch, 'voidptr variables may only be assigned voidptr values (e.g. unsafe { voidptr(${source_value}) })', default_id, default_node.pos)
			}
			continue
		}
		if clean_expected is Pointer && unalias_type(actual) !is Pointer
			&& default_node.kind in [.ident, .selector, .struct_init] {
			tc.record_error_at(.assignment_mismatch, 'reference field must be initialized with reference', default_id, default_node.pos)
			continue
		}
		unknown_struct_init := default_node.kind == .struct_init && default_node.value != 'struct'
			&& !default_node.value.starts_with('chan ') && default_node.value != 'chan'
			&& !tc.type_name_known(default_node.value)
		if unknown_struct_init || (!tc.expr_compatible(default_id, actual, expected)
			&& !tc.pointer_value_compatible(actual, expected)) {
			if unalias_type(actual) is OptionType && clean_expected !is OptionType {
				continue
			}
			actual_name := if unknown_struct_init { 'void' } else { actual.name() }
			tc.type_mismatch(.assignment_mismatch, 'cannot initialize field `${field.value}` with `${actual_name}`; expected `${field_type}`', default_id)
		}
	}
	tc.fn_context.generic_params = saved_generic_params
}

fn (tc &TypeChecker) struct_field_type_uses_comptime_define(field_id flat.NodeId, field_type_text string) bool {
	if field_type_text.contains('\$d(') {
		return true
	}
	source := tc.source_text_for_node(field_id)
	define_pos := source.index('\$d(') or { return false }
	assign_pos := source.index('=') or { source.len }
	return define_pos < assign_pos
}

fn (tc &TypeChecker) struct_field_type_pos(field flat.Node) token.Pos {
	if !field.pos.is_valid() || field.typ.len == 0 {
		return field.pos
	}
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	start := int_max(0, int_min(field.pos.offset, source.len))
	end := int_max(start, int_min(field.pos.end, source.len))
	if start >= end {
		return field.pos
	}
	relative := source[start..end].index(field.typ) or { return field.pos }
	type_start := start + relative
	return token.new_span(field.pos.id, type_start, type_start + field.typ.len)
}

fn (tc &TypeChecker) struct_field_declaration_pos(field flat.Node) token.Pos {
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	offset := int_min(int_max(field.pos.offset, 0), source.len)
	line_start := if offset > 0 {
		if relative := source[..offset].last_index('\n') { relative + 1 } else { 0 }
	} else {
		0
	}
	line_end := source.index_after('\n', line_start) or { source.len }
	line := source[line_start..line_end].trim_right('\r\n')
	if relative := line.index(field.value) {
		start := line_start + relative
		return token.new_span(field.pos.id, start, line_start + line.len)
	}
	return field.pos
}

fn (tc &TypeChecker) anonymous_struct_type_head_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_min(int_max(node.pos.offset, 0), source.len)
	end := int_min(int_max(node.pos.end, start), source.len)
	if relative := source[start..end].index('{') {
		return token.new_span(node.pos.id, start, start + relative + 1)
	}
	return node.pos
}

// check_enum_backing_type validates explicit enum backing storage types.
fn (mut tc TypeChecker) check_enum_backing_type(node_id flat.NodeId, node flat.Node) {
	if node.kind != .enum_decl || node.generic_params().len == 0 {
		return
	}
	backing := node.generic_params()[0].trim_space()
	if backing.len == 0 {
		return
	}
	backing_type := tc.parse_type(backing)
	if backing_type is Unknown || type_contains_unknown(backing_type) {
		tc.record_error(.unknown_type, 'unknown type `${backing}`', node_id)
		return
	}
	clean := unalias_type(backing_type)
	if bounds := enum_backing_value_bounds(clean) {
		tc.check_backed_enum_field_value_ranges(node, backing, bounds)
		return
	}
	tc.record_error_at(.assignment_mismatch, '`${backing}` is not one of `i8`,`i16`,`i32`,`int`,`i64`,`u8`,`u16`,`u32`,`u64`', node_id, tc.enum_backing_type_diagnostic_pos(node_id, backing))
	tc.check_backed_enum_field_value_ranges(node, backing, EnumBackingValueBounds{
		min: 0
		max: 0
		has_min: true
		has_max: true
	})
}

fn (tc &TypeChecker) enum_backing_type_diagnostic_pos(node_id flat.NodeId, backing string) token.Pos {
	name_pos := tc.node_value_diagnostic_pos(node_id)
	file := tc.a.source_files[name_pos.id] or { return name_pos }
	source := tc.source_texts_by_file[file.name] or { return name_pos }
	mut line_start := name_pos.offset
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	mut line_end := name_pos.end
	for line_end < source.len && source[line_end] != `\n` {
		line_end++
	}
	line := source[line_start..line_end]
	needle := 'as ${backing}'
	relative := line.index(needle) or { return name_pos }
	start := line_start + relative + 3
	return token.new_span(name_pos.id, start, start + backing.len)
}

struct EnumBackingValueBounds {
	min         int
	max         int
	has_min     bool
	has_max     bool
	bits        int
	is_unsigned bool
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
						min: 0
						max: 255
						has_min: true
						has_max: true
						bits: bits
						is_unsigned: true
					}
				}
				16 {
					EnumBackingValueBounds{
						min: 0
						max: 65535
						has_min: true
						has_max: true
						bits: bits
						is_unsigned: true
					}
				}
				else {
					EnumBackingValueBounds{
						min: 0
						has_min: true
						bits: bits
						is_unsigned: true
					}
				}
			}
		}
		return match clean.size {
			8 {
				EnumBackingValueBounds{
					min: -128
					max: 127
					has_min: true
					has_max: true
					bits: bits
				}
			}
			16 {
				EnumBackingValueBounds{
					min: -32768
					max: 32767
					has_min: true
					has_max: true
					bits: bits
				}
			}
			32, 0 {
				EnumBackingValueBounds{
					min: -2147483647 - 1
					max: 2147483647
					has_min: true
					has_max: true
					bits: bits
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
			min: -2147483647 - 1
			max: 2147483647
			has_min: true
			has_max: true
			bits: 32
		}
	}
	if clean is USize {
		return EnumBackingValueBounds{
			min: 0
			has_min: true
			bits: 64
			is_unsigned: true
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
	mut has_previous := false
	mut previous_is_max := false
	for field_id in field_ids {
		field := tc.a.nodes[int(field_id)]
		if !is_flag && field.children_count == 0 {
			val := if has_previous { next_val } else { 0 }
			if previous_is_max {
				tc.record_error_at(.assignment_mismatch, 'enum value overflows type `${backing}`, which has a maximum value of ${enum_backing_max_text(bounds)}', field_id, field.pos)
			}
			has_previous = true
			previous_is_max = enum_backing_int_is_max(val, bounds)
			next_val = val + 1
			continue
		}
		mut val := next_val
		mut has_checked_value := true
		if expr_id := field_exprs[field.value] {
			if !is_flag {
				if literal := tc.integer_literal_source(expr_id) {
					clean_literal := literal.replace('_', '')
					if enum_backing_literal_overflows(clean_literal, bounds) {
						tc.record_error_at(.assignment_mismatch, 'enum value `${clean_literal}` overflows the enum type `${backing}`, values of which have to be in [${enum_backing_min_text(bounds)}, ${enum_backing_max_text(bounds)}]', expr_id, tc.a.nodes[int(expr_id)].pos)
					}
					previous_is_max = enum_backing_literal_becomes_max(clean_literal, bounds)
				} else {
					expr_type := tc.resolve_type(expr_id)
					if expr_type !is Unknown && !expr_type.is_integer() {
						tc.record_error_at(.assignment_mismatch, 'enum field `${field.value}` value must be integer, not `${expr_type.name()}`', expr_id, tc.a.nodes[int(expr_id)].pos)
					}
				}
			}
			mut resolving := map[string]bool{}
			if resolved := tc.comptime_static_enum_field_value(expr_id, tc.cur_module, node.value, mut field_values, field_exprs, mut resolving) {
				val = resolved
			} else {
				has_checked_value = false
			}
		}
		field_values[field.value] = val
		if is_flag && has_checked_value && !enum_backing_value_fits(val, bounds, true) {
			if is_flag {
				tc.record_error(.assignment_mismatch, 'enum field `${field.value}` bit ${val} does not fit backing type `${backing}`', field_id)
			}
		}
		if !is_flag && has_checked_value && field.children_count > 0
			&& tc.integer_literal_source(tc.a.child(&field, 0)) == none {
			if !enum_backing_value_fits(val, bounds, false) {
				expr_id := tc.a.child(&field, 0)
				tc.record_error_at(.assignment_mismatch, 'enum value `${val}` overflows the enum type `${backing}`, values of which have to be in [${enum_backing_min_text(bounds)}, ${enum_backing_max_text(bounds)}]', expr_id, tc.a.nodes[int(expr_id)].pos)
			}
			previous_is_max = enum_backing_int_is_max(val, bounds)
		}
		has_previous = has_checked_value
		next_val = val + 1
	}
}

fn enum_backing_min_text(bounds EnumBackingValueBounds) string {
	if bounds.is_unsigned {
		return '0'
	}
	return match bounds.bits {
		8 { '-128' }
		16 { '-32768' }
		32 { '-2147483648' }
		64 { '-9223372036854775808' }
		else { bounds.min.str() }
	}
}

fn enum_backing_max_text(bounds EnumBackingValueBounds) string {
	if bounds.is_unsigned {
		return match bounds.bits {
			8 { '255' }
			16 { '65535' }
			32 { '4294967295' }
			64 { '18446744073709551615' }
			else { bounds.max.str() }
		}
	}
	return match bounds.bits {
		8 { '127' }
		16 { '32767' }
		32 { '2147483647' }
		64 { '9223372036854775807' }
		else { bounds.max.str() }
	}
}

fn enum_backing_literal_overflows(literal string, bounds EnumBackingValueBounds) bool {
	if literal.len == 0 {
		return false
	}
	is_negative := literal[0] == `-`
	mut magnitude := literal
	if literal[0] == `-` || literal[0] == `+` {
		magnitude = literal[1..]
	}
	if magnitude.len == 0 {
		return false
	}
	value, parse_error := strconv.common_parse_uint2(magnitude, 0, 64)
	if parse_error == -3 {
		return true
	}
	if parse_error != 0 {
		return false
	}
	if bounds.is_unsigned {
		if is_negative {
			return true
		}
		maximum := enum_backing_unsigned_max(bounds.bits)
		return value > maximum
	}
	minimum_magnitude := if bounds.bits >= 64 {
		u64(1) << 63
	} else if bounds.bits > 0 {
		u64(1) << (bounds.bits - 1)
	} else {
		u64(0)
	}
	maximum := if bounds.bits >= 64 {
		u64(max_i64)
	} else if bounds.bits > 0 {
		(u64(1) << (bounds.bits - 1)) - 1
	} else {
		u64(0)
	}
	if is_negative {
		return value > minimum_magnitude
	}
	// v1 historically rejects the signed maximum when it is written explicitly.
	return value >= maximum
}

fn enum_backing_literal_becomes_max(literal string, bounds EnumBackingValueBounds) bool {
	if literal.len == 0 || literal[0] == `-` {
		return false
	}
	mut magnitude := literal
	if literal[0] == `+` {
		magnitude = literal[1..]
	}
	value, parse_error := strconv.common_parse_uint2(magnitude, 0, 64)
	if bounds.is_unsigned {
		return parse_error == 0 && value == enum_backing_unsigned_max(bounds.bits)
	}
	maximum := if bounds.bits >= 64 {
		u64(max_i64)
	} else if bounds.bits > 0 {
		(u64(1) << (bounds.bits - 1)) - 1
	} else {
		u64(0)
	}
	if parse_error == 0 && value == maximum {
		return true
	}
	return bounds.bits == 64 && (parse_error == -3 || (parse_error == 0 && value > maximum))
}

fn enum_backing_unsigned_max(bits int) u64 {
	if bits >= 64 {
		return max_u64
	}
	if bits <= 0 {
		return 0
	}
	return (u64(1) << bits) - 1
}

fn enum_backing_int_is_max(value int, bounds EnumBackingValueBounds) bool {
	return bounds.has_max && value == bounds.max
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
fn (mut tc TypeChecker) check_enum_field_values(node_id flat.NodeId, node flat.Node) {
	mut enum_field_count := 0
	for i in 0 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .enum_field {
			enum_field_count++
		}
	}
	if enum_field_count == 0 {
		tc.record_error_at(.assignment_mismatch, 'enum cannot be empty', node_id, tc.enum_declaration_diagnostic_pos(node_id))
	}
	if node.value.len == 1 && node.value[0] >= `A` && node.value[0] <= `Z` {
		tc.record_error_at(.duplicate_decl, 'single letter capital names are reserved for generic template types.', node_id, tc.node_value_diagnostic_pos(node_id))
	}
	if tc.type_declaration_exists_before(node_id, node.value) {
		tc.record_error_at(.duplicate_decl, 'cannot register enum `${node.value}`, another type with this name exists', node_id, tc.enum_declaration_diagnostic_pos(node_id))
	}
	allow_multiple := tc.declaration_has_attribute(node_id, '_allow_multiple_values')
		|| tc.translated_files[tc.cur_file]
	mut field_exprs := map[string]flat.NodeId{}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind == .enum_field && field.children_count > 0 && field.value !in field_exprs {
			field_exprs[field.value] = tc.a.child(field, 0)
		}
	}
	mut seen_names := map[string]bool{}
	mut seen_values := map[int]bool{}
	mut field_values := map[string]int{}
	mut next_value := 0
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.child_node(&node, i)
		if field.kind != .enum_field {
			continue
		}
		if tc.should_check_source_name(field_id) && !field.value.starts_with('_')
			&& !snake_case_name_is_valid(field.value) {
			tc.check_snake_case_name(field_id, field.value, 'field name', tc.source_line_declaration_pos(field_id))
		}
		duplicate_name := seen_names[field.value]
		if duplicate_name {
			mut duplicate_pos := field.pos
			if field.children_count > 0 {
				duplicate_pos = token.new_span(field.pos.id, field.pos.offset, field.pos.offset + 1)
			}
			tc.record_error_at(.duplicate_decl, 'duplicate enum field name `${field.value}`', field_id, duplicate_pos)
		}
		seen_names[field.value] = true
		mut value := next_value
		mut value_known := true
		mut value_overflows_backing := false
		mut value_pos := field.pos
		if field.children_count > 0 {
			value_id := tc.a.child(field, 0)
			value_pos = tc.a.node(value_id).pos
			if referenced := tc.find_enum_value_in_node(value_id, node.value) {
				if referenced != field.value && !seen_names[referenced] {
					tc.record_error_at(.unknown_ident, '`${node.value}.${referenced}` should be declared before using it', value_id, value_pos)
					continue
				}
			}
			if node.generic_params().len > 0 {
				if bounds := enum_backing_value_bounds(tc.parse_type(node.generic_params()[0])) {
					if literal := tc.integer_literal_source(value_id) {
						value_overflows_backing = enum_backing_literal_overflows(literal.replace('_', ''), bounds)
					}
				}
			}
			tc.check_node(value_id)
			value_node := tc.a.node(value_id)
			if value_node.kind == .enum_val {
				if duplicate_name {
					continue
				}
				if value_node.value == field.value {
					if !allow_multiple {
						tc.record_error_with_details_at(.duplicate_decl, 'enum value `${field.value}` is not allowed to reference itself', field_id, value_pos, [
							'use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when needed',
						])
					}
					continue
				}
				if !seen_names[value_node.value] {
					tc.record_error_at(.unknown_ident, '`${node.value}.${value_node.value}` should be declared before using it', field_id, value_pos)
					continue
				}
			}
			value_type := tc.resolve_type(value_id)
			if value_type is Unknown {
				value_known = false
			} else if !value_type.is_integer() {
				if node.generic_params().len == 0 {
					tc.record_error_at(.assignment_mismatch, 'enum field `${field.value}` value must be integer, not `${value_type.name()}`', value_id, value_pos)
				}
				value_known = false
			}
			mut resolving := map[string]bool{}
			value = tc.comptime_static_enum_field_value(value_id, tc.cur_module, node.value, mut field_values, field_exprs, mut resolving) or {
				if tc.node_contains_runtime_call(value_id) {
					tc.record_error_at(.assignment_mismatch, 'enum field `${field.value}` value must be integer', value_id, value_pos)
				}
				value_known = false
				value
			}
		}
		if value_known && !value_overflows_backing && seen_values[value] && !allow_multiple {
			detail := if field.children_count > 0
				&& tc.a.node(tc.a.child(field, 0)).kind == .enum_val {
				'use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when needed'
			} else {
				'use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when it is needed'
			}
			tc.record_error_with_details_at(.duplicate_decl, 'enum value `${value}` already exists', field_id, value_pos, [
				detail,
			])
		}
		if value_known {
			seen_values[value] = true
			field_values[field.value] = value
			next_value = value + 1
		}
	}
}

fn (tc &TypeChecker) find_enum_value_in_node(id flat.NodeId, enum_name string) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .enum_val {
		return node.value
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(&node, 0)
		if base.kind == .ident && (base.value == enum_name || base.value.ends_with('.${enum_name}')) {
			return node.value
		}
	}
	for i in 0 .. node.children_count {
		if found := tc.find_enum_value_in_node(tc.a.child(&node, i), enum_name) {
			return found
		}
	}
	return none
}

fn (tc &TypeChecker) node_contains_runtime_call(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .call {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.node_contains_runtime_call(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

// declaration_has_attribute reports whether a declaration has the named attribute.
pub fn (tc &TypeChecker) declaration_has_attribute(node_id flat.NodeId, name string) bool {
	for raw in tc.declaration_attributes[int(node_id)] {
		if raw.all_before(':').trim_space() == name {
			return true
		}
	}
	return false
}

// autofree_enabled reports whether compatibility autofree lowering is active.
pub fn (tc &TypeChecker) autofree_enabled() bool {
	return tc.autofree_mode
}

// struct_module_for_type returns the module that declared the named struct.
pub fn (tc &TypeChecker) struct_module_for_type(name string) string {
	base, _, is_generic := generic_type_application_parts(name)
	candidate := if is_generic { base } else { name }
	if module_name := tc.struct_modules[candidate] {
		return module_name
	}
	if candidate.contains('.') {
		return candidate.all_before_last('.')
	}
	return ''
}

pub fn (tc &TypeChecker) type_has_declaration_attribute(typ Type, name string) bool {
	clean := unalias_type(unwrap_pointer(typ))
	type_name := strip_generic_args_name(clean.name())
	if type_name.len == 0 {
		return false
	}
	base, _, is_generic := generic_type_application_parts(type_name)
	declaration_name := if is_generic { base } else { type_name }
	for index in tc.type_declaration_ids[declaration_name.all_after_last('.')] {
		if tc.declaration_has_attribute(flat.NodeId(index), name) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) non_heap_pointer_param_struct(name string) ?string {
	if tc.mut_param_binding_matches_lvalue(name) {
		return none
	}
	if tc.current_fn_param_is_receiver(name) {
		// A pointer receiver is supplied by the caller and may legitimately be
		// retained in a handle returned by the method.
		return none
	}
	type_text := tc.current_fn_param_type_text(name) or { return none }
	if !type_text.trim_space().starts_with('&') {
		return none
	}
	param_type := tc.parse_type(type_text)
	clean := unalias_type(unwrap_pointer(param_type))
	if clean !is Struct {
		return none
	}
	struct_type := clean as Struct
	if tc.type_has_declaration_attribute(Type(struct_type), 'heap') {
		return none
	}
	return struct_type.name.all_after_last('.')
}

fn (tc &TypeChecker) current_fn_param_is_receiver(name string) bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	fn_node := tc.a.node(fn_id)
	if fn_node.children_count == 0 {
		return false
	}
	param := tc.a.child_node(fn_node, 0)
	return param.kind == .param && param.op == .dot && param.value == name
}

fn (tc &TypeChecker) current_fn_param_is_mut_receiver(name string) bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	fn_node := tc.a.node(fn_id)
	if fn_node.children_count == 0 {
		return false
	}
	param := tc.a.child_node(fn_node, 0)
	return param.kind == .param && param.op == .dot && param.is_mut && param.value == name
}

fn (mut tc TypeChecker) record_non_heap_pointer_param_escape(id flat.NodeId) bool {
	if tc.unsafe_depth > 0 || !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .ident {
		return false
	}
	struct_name := tc.non_heap_pointer_param_struct(node.value) or { return false }
	tc.record_error_at(.assignment_mismatch, '`${node.value}` cannot be assigned outside `unsafe` blocks as it might refer to an object stored on stack. Consider declaring `${struct_name}` as `@[heap]`.', id, node.pos)
	return true
}

fn (tc &TypeChecker) current_file_module_has_attribute(name string) bool {
	mut file_name := ''
	for index in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(index))
		if node.kind == .file {
			file_name = node.value
			continue
		}
		if node.kind == .module_decl && file_name == tc.cur_file
			&& tc.declaration_has_attribute(flat.NodeId(index), name) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) type_declaration_exists_before(node_id flat.NodeId, name string) bool {
	current_name := qualify_decl_name_in_module(name, tc.cur_module)
	mut module_name := ''
	for idx in tc.top_level_idx {
		if idx >= int(node_id) {
			break
		}
		candidate := tc.a.nodes[idx]
		if candidate.kind == .file {
			module_name = ''
			continue
		}
		if candidate.kind == .module_decl {
			module_name = candidate.value
			continue
		}
		if candidate.kind in [.struct_decl, .type_decl, .interface_decl, .enum_decl]
			&& qualify_decl_name_in_module(candidate.value, module_name) == current_name {
			return true
		}
	}
	return false
}

fn qualify_decl_name_in_module(name string, module_name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin'
		|| name.starts_with('${module_name}.') {
		return name
	}
	return '${module_name}.${name}'
}

fn (tc &TypeChecker) enum_declaration_diagnostic_pos(node_id flat.NodeId) token.Pos {
	name_pos := tc.node_value_diagnostic_pos(node_id)
	file := tc.a.source_files[name_pos.id] or { return name_pos }
	source := tc.source_texts_by_file[file.name] or { return name_pos }
	mut line_start := name_pos.offset
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	prefix := source[line_start..name_pos.offset]
	enum_relative := prefix.last_index('enum ') or { return name_pos }
	mut start := line_start + enum_relative
	if prefix[..enum_relative].trim_space() == 'pub' {
		if pub_relative := prefix.index('pub') {
			start = line_start + pub_relative
		}
	}
	return token.new_span(name_pos.id, start, name_pos.end)
}

// check_const_field_values validates check const field values state for types.
fn (mut tc TypeChecker) check_const_field_values(node flat.Node) {
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.node(field_id)
		if field.kind != .const_field || field.children_count == 0 {
			continue
		}
		qname := tc.qualify_name(field.value)
		module_name := if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
		duplicate_key := '${module_name}.${field.value}'
		if reserved_const_type_name(field.value) {
			tc.record_error_at(.duplicate_decl, 'invalid use of reserved type `${field.value}` as a const name', field_id, field.pos)
		}
		if tc.checked_const_names[duplicate_key] {
			tc.record_error_at(.duplicate_decl, 'duplicate const `${field.value}`', field_id, tc.node_value_diagnostic_pos(field_id))
		} else {
			tc.checked_const_names[duplicate_key] = true
		}
		if field.value == tc.cur_module && tc.cur_module !in ['', 'main'] && !tc.current_file_uses_nested_vlib_module_path() {
			tc.record_error_at(.duplicate_decl, 'duplicate of a module name `${qname}`', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		if field.value == '_' {
			tc.record_error_at(.duplicate_decl, 'cannot use `_` as a const name', field_id, tc.node_value_diagnostic_pos(field_id))
		} else if tc.should_check_source_name(field_id) && !field.value.starts_with('C.')
			&& field.value != field.value.to_lower() {
			tc.record_error_at(.duplicate_decl, 'const names cannot contain uppercase letters, use snake_case instead', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		if tc.has_active_import(field.value) {
			tc.record_error_at(.duplicate_decl, 'const `${field.value}` conflicts with imported module `${field.value}`', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		expr_id := tc.a.child(field, 0)
		if cycle_id := tc.find_ident_in_node(expr_id, field.value) {
			if tc.addressed_fixed_array_const_self_reference(qname, expr_id, field.value) {
				tc.check_node(expr_id)
				continue
			}
			tc.const_types[qname] = Type(void_)
			tc.record_error(.unknown_ident, 'cycle in constant `${field.value}`', cycle_id)
		}
		tc.check_node(expr_id)
		if tc.resolve_type(expr_id) is MultiReturn {
			tc.record_error_at(.assignment_mismatch, 'const declarations do not support multiple return values yet', expr_id, tc.a.nodes[int(expr_id)].pos)
		}
		if field.typ.len == 0 && tc.resolve_type(expr_id).name() == 'int'
			&& tc.implicit_int_literal_overflows(expr_id) {
			tc.record_error_at(.assignment_mismatch, 'overflow in implicit type `int`, use explicit type casting instead', expr_id, tc.a.nodes[int(expr_id)].pos)
		}
	}
}

fn (tc &TypeChecker) addressed_fixed_array_const_self_reference(qname string, expr_id flat.NodeId, name string) bool {
	if tc.const_types[qname] or { Type(void_) } !is ArrayFixed {
		return false
	}
	found, all_addressed := tc.const_self_reference_address_state(expr_id, name, false)
	return found && all_addressed
}

fn (tc &TypeChecker) const_self_reference_address_state(id flat.NodeId, name string, addressed bool) (bool, bool) {
	if !tc.valid_node_id(id) {
		return false, true
	}
	node := tc.a.node(id)
	if node.kind == .ident && node.value == name {
		return true, addressed
	}
	child_addressed := addressed || (node.kind == .prefix && node.op == .amp)
	mut found := false
	mut all_addressed := true
	for i in 0 .. node.children_count {
		child_found, child_ok := tc.const_self_reference_address_state(tc.a.child(node, i), name, child_addressed)
		found = found || child_found
		all_addressed = all_addressed && child_ok
	}
	return found, all_addressed
}

fn (mut tc TypeChecker) check_const_global_initializers(node flat.Node) {
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.node(field_id)
		if field.kind != .field_decl || 'const' !in field.generic_params() {
			continue
		}
		if field.children_count == 0 {
			tc.record_error_severity_at(.compile_error, 'const globals must have an explicit initializer', field_id, tc.node_value_diagnostic_pos(field_id), 'cgen error:')
			continue
		}
		expr_id := tc.a.child(field, 0)
		tc.check_node(expr_id)
		if !tc.global_const_expr_is_c_constant(expr_id) {
			tc.record_error_severity_at(.compile_error, 'const global `${field.value}` must be initialized with a C constant expression', field_id, tc.node_value_diagnostic_pos(field_id), 'cgen error:')
		}
	}
}

fn (tc &TypeChecker) global_const_expr_is_c_constant(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.string_literal, .int_literal, .float_literal, .bool_literal, .char_literal,
		.enum_val] {
		return true
	}
	if node.kind == .ident {
		return true
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident && base.value == 'C' {
			return true
		}
	}
	if node.kind in [.paren, .cast_expr, .prefix] {
		return node.children_count > 0 && tc.global_const_expr_is_c_constant(tc.a.child(node, 0))
	}
	if node.kind in [.infix, .array_literal, .array_init, .struct_init] {
		for i in 0 .. node.children_count {
			child_id := tc.a.child(node, i)
			child := tc.a.node(child_id)
			value_id := if child.kind == .field_init && child.children_count > 0 {
				tc.a.child(child, 0)
			} else {
				child_id
			}
			if !tc.global_const_expr_is_c_constant(value_id) {
				return false
			}
		}
		return true
	}
	return false
}

fn reserved_const_type_name(name string) bool {
	return name in [
		'bool',
		'char',
		'i8',
		'i16',
		'i32',
		'int',
		'i64',
		'u8',
		'u16',
		'u32',
		'u64',
		'f32',
		'f64',
		'map',
		'string',
		'rune',
		'usize',
		'isize',
		'voidptr',
	]
}

fn (tc &TypeChecker) find_ident_in_node(id flat.NodeId, name string) ?flat.NodeId {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return id
	}
	for i in 0 .. node.children_count {
		if found := tc.find_ident_in_node(tc.a.child(&node, i), name) {
			return found
		}
	}
	return none
}

// fn_body_definitely_returns supports fn body definitely returns handling for TypeChecker.
pub fn (tc &TypeChecker) fn_body_definitely_returns(node flat.Node) bool {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		if tc.stmt_definitely_returns(child_id)
			|| tc.stmt_has_v1_compatible_returning_or_fallback(child_id) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) stmt_has_v1_compatible_returning_or_fallback(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .or_expr && node.value !in ['?', '!'] && node.children_count >= 2 {
		return tc.stmt_definitely_returns(tc.a.child(node, 1))
	}
	if node.kind in [.expr_stmt, .paren] && node.children_count > 0 {
		return tc.stmt_has_v1_compatible_returning_or_fallback(tc.a.child(node, 0))
	}
	if node.kind !in [.decl_assign, .assign, .selector_assign, .index_assign] {
		return false
	}
	for i := 1; i < node.children_count; i += 2 {
		if tc.stmt_has_v1_compatible_returning_or_fallback(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_noreturn_fn_semantics(id flat.NodeId, node flat.Node, qname string) {
	if node.value !in tc.a.noreturn_fns && qname !in tc.a.noreturn_fns {
		return
	}
	mut tail_id := flat.NodeId(-1)
	mut has_return := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .param {
			continue
		}
		tail_id = child_id
		if tc.subtree_contains_return(child_id) {
			has_return = true
		}
	}
	if has_return {
		tc.record_error_at(.return_mismatch, '[noreturn] functions cannot use return statements', id, tc.fn_declaration_diagnostic_pos(node))
	}
	valid_tail := tc.valid_node_id(tail_id)
		&& (tc.stmt_definitely_returns(tail_id) || tc.expr_never_returns_resolving(tail_id))
		&& !tc.subtree_contains_return(tail_id)
	if valid_tail {
		return
	}
	pos := if tc.valid_node_id(tail_id) {
		tc.noreturn_invalid_tail_pos(tail_id)
	} else {
		tc.fn_declaration_diagnostic_pos(node)
	}
	tc.record_error_at(.return_mismatch, '@[noreturn] functions should end with a call to another @[noreturn] function, or with an infinite `for {}` loop', if tc.valid_node_id(tail_id) {
		tail_id
	} else {
		id
	}, pos)
}

fn (mut tc TypeChecker) check_unreachable_after_noreturn_call(node flat.Node) {
	mut previous_never_returns := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .param {
			continue
		}
		if previous_never_returns {
			tc.record_warning_at(.return_mismatch, 'unreachable code after a @[noreturn] call', child_id, tc.noreturn_statement_diagnostic_pos(child_id))
			return
		}
		if tc.is_prod && child.kind == .assert_stmt {
			continue
		}
		previous_never_returns = tc.expr_never_returns(child_id)
	}
}

fn (tc &TypeChecker) subtree_contains_return(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .return_stmt {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.subtree_contains_return(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) noreturn_invalid_tail_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .for_stmt {
		line_pos := tc.previous_source_line_matching(node.pos, 'for')
		file := tc.a.source_files[line_pos.id] or { return line_pos }
		source := tc.source_texts_by_file[file.name] or { return line_pos }
		if line_pos.offset >= 0 && line_pos.end <= source.len && line_pos.end > line_pos.offset {
			line := source[line_pos.offset..line_pos.end]
			brace := line.index_u8(`{`)
			if brace >= 0 {
				return token.new_span(line_pos.id, line_pos.offset + brace, line_pos.offset + brace + 1)
			}
		}
		return line_pos
	}
	return tc.noreturn_statement_diagnostic_pos(id)
}

fn (tc &TypeChecker) noreturn_statement_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .expr_stmt && node.children_count > 0 {
		return tc.a.node(tc.a.child(node, 0)).pos
	}
	if node.kind == .return_stmt {
		return tc.previous_source_line_matching(node.pos, 'return')
	}
	return node.pos
}

fn (tc &TypeChecker) previous_source_line_matching(pos token.Pos, prefix string) token.Pos {
	file := tc.a.source_files[pos.id] or { return pos }
	source := tc.source_texts_by_file[file.name] or { return pos }
	mut cursor := int_min(int_max(pos.offset, 0), source.len)
	for cursor >= 0 {
		line_start := if cursor > 0 {
			if relative := source[..cursor].last_index('\n') {
				relative + 1
			} else {
				0
			}
		} else {
			0
		}
		line_end := source.index_after('\n', line_start) or { source.len }
		line := source[line_start..line_end].trim_right('\r\n')
		trimmed := line.trim_left(' \t')
		if trimmed.starts_with(prefix) {
			start := line_start + line.len - line.trim_left(' \t').len
			return token.new_span(pos.id, start, line_start + line.len)
		}
		if line_start == 0 {
			break
		}
		cursor = line_start - 1
	}
	return pos
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
		.assert_stmt {
			return tc.assert_stmt_never_returns(node)
		}
		.block {
			return tc.stmt_sequence_definitely_returns(&node, 0)
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
				if !tc.match_branch_definitely_returns_with_context(node, branch) {
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
	if tc.resolved_call_never_returns(id) {
		return true
	}
	if !tc.valid_node_id(id) {
		return false
	}
	call := tc.a.nodes[int(id)]
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&call, 0)
	if callee.kind == .ident {
		if callee.value in ['panic', 'exit'] && tc.no_return_builtin_is_shadowed(callee.value) {
			return false
		}
		return callee.value in ['panic', 'exit', '__v_compile_error']
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := tc.a.child_node(callee, 0)
		if base.kind != .ident || callee.value != 'exit' || base.value !in ['os', 'C'] {
			return false
		}
		if base.value == 'os' && tc.no_return_builtin_is_shadowed(base.value) {
			return false
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) no_return_builtin_is_shadowed(name string) bool {
	if name in tc.smartcasts {
		return true
	}
	if _ := tc.non_file_scope_type(name) {
		return true
	}
	return false
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
		.assert_stmt {
			return tc.assert_stmt_never_returns(node)
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) assert_stmt_never_returns(node flat.Node) bool {
	if node.kind != .assert_stmt || node.children_count == 0 {
		return false
	}
	if tc.valid_node_id(flat.NodeId(tc.fn_context.node_id))
		&& tc.declaration_has_attribute(flat.NodeId(tc.fn_context.node_id), 'assert_continues') {
		return false
	}
	value := tc.constant_bool_value(tc.a.child(&node, 0)) or { return false }
	return !value
}

fn (tc &TypeChecker) branch_tail_never_returns(branch_id flat.NodeId) bool {
	if !tc.valid_node_id(branch_id) {
		return false
	}
	branch := tc.a.nodes[int(branch_id)]
	tail_id := tc.branch_tail_expr_id(branch_id)
	if !tc.valid_node_id(tail_id) {
		return false
	}
	tail := tc.a.nodes[int(tail_id)]
	if tail.kind in [.return_stmt, .break_stmt, .continue_stmt] {
		return true
	}
	if branch.kind !in [.block, .match_branch] {
		return tc.expr_never_returns(tail_id)
	}
	body_start := if branch.kind == .match_branch {
		if branch.value == 'else' { 0 } else { branch.value.int() }
	} else {
		0
	}
	// Only the `smartcasts` binding needs isolation here: a full `*tc` copy
	// shares every map's backing storage anyway and memmoves the ~11KB
	// TypeChecker per call, so swap in a scratch clone and restore instead.
	mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
	mut saved_smartcasts := mtc.smartcasts.move()
	mtc.smartcasts = clone_smartcasts(saved_smartcasts)
	defer {
		mtc.smartcasts = saved_smartcasts.move()
	}
	tail_index := int(branch.children_count) - 1
	for i in body_start .. tail_index {
		mtc.apply_return_analysis_local_binding(tc.a.child(&branch, i))
	}
	return tc.expr_never_returns(tail_id)
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
	return tc.stmt_sequence_definitely_returns(branch, body_start)
}

fn (tc &TypeChecker) stmt_sequence_definitely_returns(node &flat.Node, body_start int) bool {
	// Only the `smartcasts` binding needs isolation here (see
	// branch_tail_never_returns); avoid the ~11KB full struct copy.
	mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
	mut saved_smartcasts := mtc.smartcasts.move()
	mtc.smartcasts = clone_smartcasts(saved_smartcasts)
	defer {
		mtc.smartcasts = saved_smartcasts.move()
	}
	for i in body_start .. node.children_count {
		child_id := tc.a.child(node, i)
		if tc.stmt_definitely_returns(child_id) {
			return true
		}
		mtc.apply_return_analysis_local_binding(child_id)
	}
	return false
}

fn (mut tc TypeChecker) apply_return_analysis_local_binding(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .decl_assign || node.children_count < 2 {
		return
	}
	lhs_id := tc.a.child(&node, 0)
	rhs_id := tc.a.child(&node, 1)
	if !tc.valid_node_id(lhs_id) || !tc.valid_node_id(rhs_id) {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || !valid_string_data(lhs.value) {
		return
	}
	rhs_type := tc.smartcast_type(rhs_id) or { tc.expr_type(rhs_id) or { tc.resolve_type(rhs_id) } }
	if rhs_type is Unknown || rhs_type is Void {
		return
	}
	tc.smartcasts[lhs.value] = rhs_type
}

fn (tc &TypeChecker) match_branch_definitely_returns_with_context(node flat.Node, branch &flat.Node) bool {
	if branch.value == 'else' || node.children_count == 0 {
		return tc.match_branch_definitely_returns(branch)
	}
	n_conds := branch.value.int()
	if n_conds != 1 || branch.children_count == 0 {
		return tc.match_branch_definitely_returns(branch)
	}
	subject_id := tc.a.child(&node, 0)
	subject_key := tc.expr_key(subject_id)
	if subject_key.len == 0 || !valid_string_data(subject_key) {
		return tc.match_branch_definitely_returns(branch)
	}
	subject_type := unalias_and_unwrap_pointer_type(tc.resolve_type(subject_id))
	if subject_type !is SumType && !is_ierror_type(subject_type) && subject_type !is Interface {
		return tc.match_branch_definitely_returns(branch)
	}
	cond := tc.a.node(tc.a.child(branch, 0))
	pattern := tc.match_type_pattern(cond) or { return tc.match_branch_definitely_returns(branch) }
	smartcast_type := if subject_type is SumType {
		tc.sum_variant_type_for_pattern(subject_type.name, pattern) or {
			return tc.match_branch_definitely_returns(branch)
		}
	} else if is_ierror_type(subject_type) {
		tc.resolve_ierror_match_pattern(pattern) or {
			return tc.match_branch_definitely_returns(branch)
		}
	} else if subject_type is Interface {
		tc.resolve_interface_match_pattern(pattern) or {
			return tc.match_branch_definitely_returns(branch)
		}
	} else {
		return tc.match_branch_definitely_returns(branch)
	}
	// Only the `smartcasts` binding needs isolation here (see
	// branch_tail_never_returns); avoid the ~11KB full struct copy.
	mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
	mut saved_smartcasts := mtc.smartcasts.move()
	mtc.smartcasts = clone_smartcasts(saved_smartcasts)
	defer {
		mtc.smartcasts = saved_smartcasts.move()
	}
	mtc.smartcasts[subject_key] = tc.parse_type(smartcast_type)
	return tc.match_branch_definitely_returns(branch)
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
				if tc.generic_type_name_matches(variant, pattern)
					|| tc.generic_type_name_matches(variant, qpattern) {
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

// unalias_type follows an alias chain to its underlying type, unwrapping each
// Alias to its base type until a non-alias type is reached and returned.
pub fn unalias_type(t Type) Type {
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
	return int(node.kind)
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
	param_is_mut  []bool
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
	body_id := tc.a.child(&node, 0)
	if _ := tc.comptime_struct_update_id(body_id) {
		return
	}
	if _ := tc.comptime_struct_update_source_pos(node) {
		return
	}
	if !tc.comptime_subtree_references_var(body_id, parts[0]) {
		tc.record_warning_at(.unknown_ident, 'unused variable: `${parts[0]}`', _id, tc.comptime_for_variable_pos(node, parts[0]))
	}
	if tc.check_comptime_for_source_type(_id, node) {
		return
	}
	source_is_type := tc.type_name_known(node.typ)
	source_is_value := if tc.cur_scope == unsafe { nil } {
		false
	} else {
		tc.cur_scope.lookup(node.typ) != none
	}
	source_is_fn := node.typ in tc.fn_ret_types || tc.qualify_fn_name(node.typ) in tc.fn_ret_types
	if parts[1] == 'params' && source_is_type && !source_is_fn {
		tc.record_error_at(.unknown_type, 'iterating over `.params` is supported only for functions, and `${node.typ}` is not a function', _id, tc.comptime_for_source_pos(node))
		return
	}
	if !source_is_type && !source_is_value && !source_is_fn && node.typ.len > 0
		&& node.typ[0].is_capital() {
		tc.record_error_at(.unknown_type, '\$for expects a type name or variable name to be used here, but ${node.typ} is not a type or variable name', _id, tc.comptime_for_source_pos(node))
		return
	}
	current_fn_id := flat.NodeId(tc.fn_context.node_id)
	defer {
		if tc.valid_node_id(current_fn_id) {
			tc.check_comptime_selectors_outside_loop(current_fn_id, _id, parts[0])
		}
	}
	tc.check_comptime_reflection_condition_types(body_id, parts[0])
	if parts[1] == 'methods' {
		mut invalid_uses := []flat.NodeId{}
		tc.collect_anon_fn_comptime_method_uses(body_id, parts[0], false, false, mut invalid_uses)
		if invalid_uses.len > 0 {
			file := tc.a.source_files[node.pos.id] or { &token.File{} }
			source := tc.source_texts_by_file[file.name] or { '' }
			var_pos := closest_identifier_span(source, parts[0], node.pos.offset, node.pos.id) or {
				node.pos
			}
			tc.record_warning_at(.unknown_ident, 'unused variable: `${parts[0]}`', _id, var_pos)
			for use_id in invalid_uses {
				tc.record_error_at(.unknown_ident, 'undefined ident `${parts[0]}` in the anonymous function', use_id, tc.comptime_method_anon_fn_call_pos(use_id, parts[0]))
			}
		}
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
		'attributes' {
			tc.check_comptime_attribute_members(body_id, parts[0])
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
		tc.check_comptime_static_body(body_id, parts[0], parts[1], ComptimeStaticFieldCases{}, deferred_cases)
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

fn (tc &TypeChecker) comptime_struct_update_id(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.struct_init, .assoc] {
		for i in 0 .. node.children_count {
			child_id := tc.a.child(node, i)
			child := tc.a.node(child_id)
			is_update := node.kind == .assoc
				|| (child.kind == .prefix && child.value == '...')
				|| tc.node_has_ellipsis_prefix(child_id)
			if is_update && tc.node_source_contains(child_id, '\$(') {
				return child_id
			}
		}
	}
	for i in 0 .. node.children_count {
		if update_id := tc.comptime_struct_update_id(tc.a.child(node, i)) {
			return update_id
		}
	}
	return none
}

fn (tc &TypeChecker) ellipsis_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	if start + 3 <= source.len && source[start..start + 3] == '...' {
		return token.new_span(node.pos.id, start, start + 3)
	}
	mut cursor := start
	for cursor > 0 && source[cursor - 1] in [` `, `\t`, `\n`, `\r`] {
		cursor--
	}
	if cursor >= 3 && source[cursor - 3..cursor] == '...' {
		return token.new_span(node.pos.id, cursor - 3, cursor)
	}
	return node.pos
}

fn (tc &TypeChecker) comptime_struct_update_source_pos(node flat.Node) ?token.Pos {
	if tc.ct_update_indexed {
		return tc.ct_update_pos[node.pos.id] or { return none }
	}
	file := tc.a.source_files[node.pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	return comptime_struct_update_pos_in_source(node.pos.id, source)
}

fn (mut tc TypeChecker) check_comptime_struct_updates_preflight() {
	message := 'cannot use struct update syntax in compile time expressions'
	tc.ct_update_pos = map[int]token.Pos{}
	for node in tc.a.nodes {
		if node.kind != .comptime_for || node.children_count == 0 {
			continue
		}
		body_id := tc.a.child(&node, 0)
		if update_id := tc.comptime_struct_update_id(body_id) {
			if !tc.errors.any(it.node == update_id && it.msg == message) {
				tc.record_error_at(.assignment_mismatch, message, update_id, tc.ellipsis_diagnostic_pos(update_id))
			}
		}
	}
	for file_id, file in tc.a.source_files {
		if tc.diagnostic_files.len > 0 && file.name !in tc.diagnostic_files {
			continue
		}
		source := tc.source_texts_by_file[file.name] or { continue }
		pos := comptime_struct_update_pos_in_source(file_id, source) or { continue }
		tc.ct_update_pos[file_id] = pos
		if tc.errors.any(it.msg == message && it.pos == pos) {
			continue
		}
		node_id := tc.closest_source_node_id(pos)
		tc.errors << tc.make_type_error_at(.assignment_mismatch, message, node_id, pos)
	}
	tc.ct_update_indexed = true
}

fn comptime_struct_update_pos_in_source(file_id int, source string) ?token.Pos {
	mut cursor := source.index('\$for') or { return none }
	for cursor + 3 <= source.len {
		cursor = source.index_after('...', cursor) or { return none }
		line_end := source.index_after('\n', cursor) or { source.len }
		if last_index_between(source, '\$(', cursor, line_end) >= 0 {
			return token.new_span(file_id, cursor, cursor + 3)
		}
		cursor += 3
	}
	return none
}

fn (tc &TypeChecker) closest_source_node_id(pos token.Pos) flat.NodeId {
	mut best := -1
	mut best_span := int_max(1, tc.a.nodes.len)
	for index in tc.a.user_code_start .. tc.a.nodes.len {
		node_pos := tc.a.nodes[index].pos
		if node_pos.id != pos.id || node_pos.offset > pos.offset || node_pos.end < pos.end {
			continue
		}
		span := node_pos.end - node_pos.offset
		if best < 0 || span < best_span {
			best = index
			best_span = span
		}
	}
	return if best >= 0 { flat.NodeId(best) } else { flat.NodeId(tc.a.user_code_start) }
}

fn (mut tc TypeChecker) check_comptime_for_source_type(id flat.NodeId, node flat.Node) bool {
	parts := node.value.split('|')
	if parts.len != 2 || node.typ.len == 0 {
		return false
	}
	mut parent_id := tc.direct_parent_id(id)
	for tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .comptime_for
			&& comptime_for_declares_var_in_value(parent.value, node.typ) {
			return false
		}
		next_parent := tc.direct_parent_id(parent_id)
		if next_parent == parent_id {
			break
		}
		parent_id = next_parent
	}
	source_type := if typ := tc.cur_scope.lookup(node.typ) {
		unalias_type(unwrap_pointer(typ))
	} else {
		unalias_type(unwrap_pointer(tc.parse_type(node.typ)))
	}
	mut message := ''
	if parts[1] == 'fields' && source_type !is Unknown && source_type !is Void
		&& source_type !is Struct && source_type !is Interface {
		message = 'iterating over .fields is supported only for structs and interfaces, and ${node.typ} is neither'
	} else if parts[1] == 'values' && source_type !is Unknown && source_type !is Void
		&& source_type !is Enum {
		message = 'iterating over .values is supported only for enums, and ${node.typ} is not an enum'
	} else if parts[1] == 'variants' && source_type !is Unknown && source_type !is Void
		&& source_type !is SumType {
		message = '${source_type.name().all_after_last('.')} is not Sum type to use with .variants'
	}
	if message.len == 0 {
		return false
	}
	for diagnostic in tc.errors {
		if diagnostic.node == id && diagnostic.msg == message {
			return true
		}
	}
	tc.record_error_at(.unknown_type, message, id, tc.comptime_for_source_pos(node))
	return true
}

fn (mut tc TypeChecker) check_comptime_for_source_types_preflight() {
	for index, node in tc.a.nodes {
		if node.kind == .comptime_for {
			tc.check_comptime_for_source_type(flat.NodeId(index), node)
		}
	}
}

fn (mut tc TypeChecker) check_comptime_selectors_outside_loop(id flat.NodeId, loop_id flat.NodeId, var_name string) {
	if !tc.valid_node_id(id) || id == loop_id {
		return
	}
	node := tc.a.node(id)
	if node.kind == .comptime_for && comptime_for_declares_var_in_value(node.value, var_name) {
		return
	}
	if node.kind == .selector && node.value == '\$' && node.children_count >= 2 {
		field_expr := tc.a.child_node(node, 1)
		if field_expr.kind == .selector && field_expr.children_count > 0 {
			base := tc.a.child_node(field_expr, 0)
			if base.kind == .ident && base.value == var_name {
				tc.check_comptime_field_selector(id, *node, '', ComptimeStaticFieldCases{})
				return
			}
		}
	}
	for i in 0 .. node.children_count {
		tc.check_comptime_selectors_outside_loop(tc.a.child(node, i), loop_id, var_name)
	}
}

fn (tc &TypeChecker) comptime_for_variable_pos(node flat.Node, var_name string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if relative := source[start..end].index('\$for ${var_name}') {
		var_start := start + relative + '\$for '.len
		return token.new_span(node.pos.id, var_start, var_start + var_name.len)
	}
	return node.pos
}

fn (tc &TypeChecker) comptime_for_source_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	needle := ' in ${node.typ}'
	if relative := source[start..end].index(needle) {
		source_start := start + relative + ' in '.len
		return token.new_span(node.pos.id, source_start, source_start + node.typ.len)
	}
	return node.pos
}

fn (mut tc TypeChecker) check_comptime_reflection_condition_types(id flat.NodeId, loop_var string) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .comptime_if {
		mut offset := 0
		for offset < node.value.len {
			relative := node.value[offset..].index(' is ') or { break }
			type_start := offset + relative + ' is '.len
			mut type_end := type_start
			for type_end < node.value.len {
				c := node.value[type_end]
				if !(c.is_alnum() || c in [`_`, `.`, `$`, `[`, `]`, `?`, `!`, `&`]) {
					break
				}
				type_end++
			}
			typ := node.value[type_start..type_end]
			if typ.len > 0 && !typ.starts_with('\$') && !typ.starts_with('${loop_var}.')
				&& !tc.comptime_reflection_condition_type_known(typ) {
				tc.record_error_at(.unknown_type, 'unknown type `${typ}`', id, tc.comptime_condition_type_pos(*node, typ))
			}
			offset = int_max(type_end, type_start + 1)
		}
	}
	for i in 0 .. node.children_count {
		tc.check_comptime_reflection_condition_types(tc.a.child(node, i), loop_var)
	}
}

fn (tc &TypeChecker) comptime_reflection_condition_type_known(typ string) bool {
	mut clean := typ.trim_space()
	if clean == 'fn' || clean.starts_with('fn(') || clean.starts_with('fn (') {
		return true
	}
	for clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		clean = clean[1..].trim_space()
	}
	if clean.starts_with('[]') {
		return tc.comptime_reflection_condition_type_known(clean[2..])
	}
	if clean.starts_with('[') {
		end := find_matching_bracket(clean, 0)
		if end > 0 && end < clean.len - 1 {
			return tc.comptime_reflection_condition_type_known(clean[end + 1..])
		}
		return false
	}
	if clean.starts_with('map[') {
		end := find_matching_bracket(clean, 3)
		if end > 3 && end < clean.len - 1 {
			return tc.comptime_reflection_condition_type_known(clean[4..end])
				&& tc.comptime_reflection_condition_type_known(clean[end + 1..])
		}
		return false
	}
	return tc.type_name_known(clean)
}

fn (tc &TypeChecker) comptime_condition_type_pos(node flat.Node, typ string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	needle := 'is ${typ}'
	if relative := source[start..end].index(needle) {
		type_start := start + relative + 'is '.len
		return token.new_span(node.pos.id, type_start, type_start + typ.len)
	}
	return node.pos
}

fn (mut tc TypeChecker) check_comptime_attribute_members(id flat.NodeId, var_name string) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .comptime_if {
		if member := comptime_cond_unknown_member(node.value, var_name, comptime_attribute_members) {
			mut pos := node.pos
			if file := tc.a.source_files[node.pos.id] {
				if source := tc.source_texts_by_file[file.name] {
					start := int_max(0, node.pos.offset)
					end := int_min(source.len, node.pos.end)
					if start < end {
						if relative := source[start..end].index('.${member}') {
							member_start := start + relative + 1
							pos = token.new_span(node.pos.id, member_start, member_start + member.len)
						}
					}
				}
			}
			tc.record_error_at(.unknown_field, 'unknown field `${member}` from ${var_name}', id, pos)
			return
		}
	}
	for i in 0 .. node.children_count {
		tc.check_comptime_attribute_members(tc.a.child(node, i), var_name)
	}
}

fn (tc &TypeChecker) fn_literal_directly_captures_ident(node flat.Node, var_name string) bool {
	if node.kind !in [.fn_literal, .lambda_expr] {
		return false
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value == var_name {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) collect_anon_fn_comptime_method_uses(id flat.NodeId, var_name string, inside_anon_fn bool, captured bool, mut uses []flat.NodeId) {
	node := tc.a.node(id)
	is_inside := inside_anon_fn || node.kind in [.fn_literal, .lambda_expr]
	inside_capture := if node.kind in [.fn_literal, .lambda_expr] {
		tc.fn_literal_directly_captures_ident(node, var_name)
	} else {
		captured
	}
	if is_inside && !inside_capture && node.kind == .selector && node.value == '\$'
		&& node.children_count > 1 {
		method_var := tc.a.child_node(node, 1)
		if method_var.kind == .ident && method_var.value == var_name {
			uses << id
			return
		}
	}
	for i in 0 .. node.children_count {
		tc.collect_anon_fn_comptime_method_uses(tc.a.child(node, i), var_name, is_inside, inside_capture, mut uses)
	}
}

fn (tc &TypeChecker) comptime_method_anon_fn_call_pos(id flat.NodeId, var_name string) token.Pos {
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	if node.pos.offset < 0 || node.pos.offset >= source.len {
		return node.pos
	}
	line_start := if relative := source[..node.pos.offset].last_index('\n') {
		relative + 1
	} else {
		0
	}
	line_end := source.index_after('\n', node.pos.offset) or { source.len }
	line := source[line_start..line_end]
	marker := '.\$${var_name}()'
	dot_relative := line.index(marker) or { return node.pos }
	dot := line_start + dot_relative
	return token.new_span(node.pos.id, dot + 1, dot + marker.len)
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
				mut param_is_mut := []bool{}
				for i in 1 .. candidate.children_count {
					param := tc.a.child_node(&candidate, i)
					if param.kind == .param {
						param_names << param.value
						param_types << subst_generic_text(param.typ, generic_args, generic_params)
						param_is_mut << param.is_mut
					}
				}
				return_type := subst_generic_text(if candidate.typ.len > 0 {
					candidate.typ
				} else {
					'void'
				}, generic_args, generic_params)
				cases << ComptimeStaticValueCase{
					name: name
					location: comptime_static_source_location(file_name, candidate.pos.offset, line_offsets_by_file[file_name])
					typ: comptime_static_method_type_text(param_types, return_type)
					return_type: return_type
					is_pub: candidate.op == .arrow
					has_is_pub: true
					param_names: param_names
					param_types: param_types
					param_is_mut: param_is_mut
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
					typ: param.typ
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
			params := candidate.generic_params()
			mut cases := []ComptimeStaticValueCase{cap: params.len}
			kinds := if candidate.typ.len > 0 { candidate.typ.split(',') } else { []string{} }
			for idx, raw in params {
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
	clean := trimmed_space(raw)
	colon := clean.index_u8(`:`)
	has_arg := colon >= 0 && !(kind == 1
		&& !comptime_static_attr_is_string_literal(trimmed_space(clean[colon + 1..])))
	mut name := if has_arg { trimmed_space(clean[..colon]) } else { clean }
	if name.len >= 2 && name[0] in [`'`, `"`] && name[name.len - 1] == name[0] {
		name = name[1..name.len - 1]
	}
	mut arg := if has_arg { trimmed_space(clean[colon + 1..]) } else { '' }
	if arg.len >= 3 && arg[0] == `r` && arg[1] in [`'`, `"`] && arg[arg.len - 1] == arg[1] {
		arg = arg[2..arg.len - 1]
	} else if arg.len >= 2 && arg[0] in [`'`, `"`] && arg[arg.len - 1] == arg[0] {
		arg = comptime_static_unescape(arg[1..arg.len - 1])
	}
	return ComptimeStaticValueCase{
		name: name
		arg: arg
		has_arg: has_arg
		has_attr_meta: true
		attr_kind: kind
	}
}

fn comptime_static_attr_is_string_literal(raw string) bool {
	return (raw.len >= 2 && raw[0] in [`'`, `"`] && raw[raw.len - 1] == raw[0])
		|| (raw.len >= 3 && raw[0] == `r` && raw[1] in [`'`, `"`] && raw[raw.len - 1] == raw[1])
}

fn (tc &TypeChecker) comptime_deferred_decl_source(source string, allow_type bool) ?ComptimeDeferredDeclSource {
	clean := trimmed_space(source)
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
			decl_name: clean
		}
	}
	dot := clean.index_u8(`.`)
	first := clean[..dot]
	if resolved := tc.resolve_import_alias(first) {
		return ComptimeDeferredDeclSource{
			module_name: resolved
			decl_name: clean[dot + 1..]
		}
	}
	if first == tc.cur_module {
		return ComptimeDeferredDeclSource{
			module_name: first
			decl_name: clean[dot + 1..]
		}
	}
	return ComptimeDeferredDeclSource{
		module_name: if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
		decl_name: clean
	}
}

fn comptime_deferred_qualified_source(source string) ComptimeDeferredDeclSource {
	return ComptimeDeferredDeclSource{
		module_name: source.all_before_last('.')
		decl_name: source.all_after_last('.')
	}
}

fn (mut tc TypeChecker) check_comptime_members(id flat.NodeId, var_name string, members []string, meta_name string) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_for {
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
			if base.kind == .ident && base.value == var_name && node.value != '\$'
				&& node.value !in members {
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
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases, value_cases)
		}
		tc.pop_scope()
		return
	}
	if node.kind == .comptime_for {
		// Check nested reflection loops only after earlier declarations in the
		// enclosing static body have entered the current scope.
		tc.check_comptime_for_members(id, node)
		return
	}
	if node.kind == .if_expr {
		if loop_kind == 'methods' && value_cases.known && node.children_count >= 2 {
			tc.check_comptime_static_method_runtime_if(node, var_name, loop_kind, field_cases, value_cases)
			return
		}
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases, value_cases)
		}
		return
	}
	if node.kind in [.expr_stmt, .return_stmt] {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases, value_cases)
		}
		return
	}
	if node.kind == .comptime_if && comptime_text_references_var(node.value, var_name) {
		if loop_kind in ['methods', 'params', 'attributes'] {
			tc.check_comptime_static_deferred_metadata_if(node, var_name, loop_kind, field_cases, value_cases)
			return
		}
		tc.check_comptime_static_metadata_if(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if node.kind == .defer_stmt && loop_kind == 'fields'
		&& tc.comptime_subtree_references_var(id, var_name) {
		tc.record_deferred_comptime_field_errors(id, var_name)
		return
	}
	if loop_kind == 'fields' && tc.check_comptime_static_field_selectors(id, var_name, field_cases) {
		return
	}
	if !tc.comptime_subtree_references_var(id, var_name) {
		tc.comptime_static_depth++
		tc.check_node(id)
		tc.comptime_static_depth--
		return
	}
	if node.kind in [.assign, .selector_assign, .index_assign] {
		tc.check_comptime_static_assignment(node, var_name, field_cases)
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
			rhs_id := tc.a.child(&node, i + 1)
			rhs_typ := tc.comptime_static_method_call_return_type(rhs_id, var_name, loop_kind, value_cases) or { tc.resolve_type(rhs_id) }
			typ := if rhs_typ is Unknown {
				tc.comptime_static_reflected_field_expr_type(rhs_id, var_name, field_cases) or {
					Type(Unknown{})
				}
			} else if rhs_typ is Void {
				Type(Unknown{})
			} else {
				rhs_typ
			}
			rhs := tc.a.node(rhs_id)
			if rhs.kind == .selector && rhs.value == '\$' && unalias_type(typ) is Map {
				mut pos := rhs.pos
				if file := tc.a.source_files[rhs.pos.id] {
					if source := tc.source_texts_by_file[file.name] {
						start := int_max(0, rhs.pos.offset)
						end := int_min(source.len, rhs.pos.end)
						if start < end {
							if relative := source[start..end].index('.\$(') {
								pos = token.new_span(rhs.pos.id, start + relative + 1, end)
							}
						}
					}
				}
				tc.record_error_at(.assignment_mismatch, 'cannot copy map: call `move` or `clone` method (or use a reference)', rhs_id, pos)
			}
			tc.cur_scope.insert(lhs.value, typ)
		}
	}
}

fn (mut tc TypeChecker) record_deferred_comptime_field_errors(id flat.NodeId, var_name string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .selector && node.value == '\$' && node.children_count >= 2 {
		field_expr := tc.a.child_node(node, 1)
		if field_expr.kind == .selector && field_expr.value == 'name'
			&& field_expr.children_count > 0 {
			loop_var_id := tc.a.child(field_expr, 0)
			loop_var := tc.a.node(loop_var_id)
			if loop_var.kind == .ident && loop_var.value == var_name {
				pos := tc.node_value_diagnostic_pos(loop_var_id)
				tc.record_error_at(.unknown_field, 'compile time field access can only be used when iterating over `T.fields`', loop_var_id, pos)
				tc.record_error_at(.unknown_ident, 'unknown `\$for` variable `${var_name}`', loop_var_id, pos)
				return true
			}
		}
	}
	mut condition_has_error := false
	mut found := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child_found := tc.record_deferred_comptime_field_errors(child_id, var_name)
		if node.kind == .if_expr && i == 0 && child_found {
			condition_has_error = true
		}
		found = found || child_found
	}
	if condition_has_error && node.children_count > 0 {
		condition_id := tc.a.child(node, 0)
		condition := tc.a.node(condition_id)
		mut pos := condition.pos
		if file := tc.a.source_files[condition.pos.id] {
			if source := tc.source_texts_by_file[file.name] {
				start := int_max(0, condition.pos.offset)
				end := int_min(source.len, condition.pos.end)
				if start < end {
					if relative := source[start..end].index('.\$(') {
						pos = token.new_span(condition.pos.id, start + relative + 1, end)
					}
				}
			}
		}
		tc.record_error_at(.condition_mismatch, 'non-bool type `void` used as if condition', condition_id, pos)
	}
	return found
}

fn (mut tc TypeChecker) check_comptime_static_assignment(node flat.Node, var_name string, field_cases ComptimeStaticFieldCases) {
	if !field_cases.known || field_cases.cases.len == 0 || node.children_count < 2 {
		return
	}
	lhs_id := tc.a.child(&node, 0)
	lhs := tc.a.node(lhs_id)
	if lhs.kind != .selector || lhs.value != '\$' || lhs.children_count < 2
		|| !tc.comptime_subtree_references_var(tc.a.child(lhs, 1), var_name) {
		return
	}
	rhs_id := tc.a.child(&node, 1)
	actual := tc.resolve_type(rhs_id)
	mut field_types := []string{}
	for field in field_cases.cases {
		if field.typ !in field_types {
			field_types << field.typ
		}
	}
	rhs := tc.a.node(rhs_id)
	if field_types.len > 1 && tc.comptime_initializer_is_static(rhs_id) {
		tc.record_error_at(.assignment_mismatch, 'mismatched types: check field type with \$if to avoid this problem', rhs_id, rhs.pos)
	}
	for field in field_cases.cases {
		expected := tc.parse_type(field.typ)
		if unalias_type(expected) is Enum {
			if !actual.is_integer() {
				tc.record_error_at(.assignment_mismatch, 'enums can only be assigned `int` values', rhs_id, rhs.pos)
				return
			}
		} else if !type_contains_unknown(actual) && !tc.type_compatible(actual, expected) {
			diagnostic_pos := if rhs.kind == .index && rhs.children_count > 0 {
				base := tc.a.child_node(rhs, 0)
				token.new_span(rhs.pos.id, base.pos.end, rhs.pos.end)
			} else {
				rhs.pos
			}
			actual_name := tc.diagnostic_expr_type_name(rhs_id, actual)
			tc.record_error_at(.assignment_mismatch, 'cannot assign `${actual_name}` to `${expected.name()}`; cannot assign to `${tc.source_text_for_node(lhs_id)}`: expected `${expected.name()}`, not `${actual_name}`', rhs_id, diagnostic_pos)
			return
		}
	}
}

fn (mut tc TypeChecker) check_comptime_static_field_selectors(id flat.NodeId, var_name string, field_cases ComptimeStaticFieldCases) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .selector && node.value == '\$' {
		return tc.check_comptime_field_selector(id, *node, var_name, field_cases)
	}
	mut found_error := false
	for i in 0 .. node.children_count {
		if tc.check_comptime_static_field_selectors(tc.a.child(node, i), var_name, field_cases) {
			found_error = true
		}
	}
	return found_error
}

fn (mut tc TypeChecker) check_comptime_field_selector(id flat.NodeId, node flat.Node, loop_var string, field_cases ComptimeStaticFieldCases) bool {
	if node.children_count < 2 {
		tc.register_synth_type(id, Type(void_))
		return true
	}
	field_expr_id := tc.a.child(&node, 1)
	field_expr := tc.a.node(field_expr_id)
	if field_expr.kind != .selector {
		actual_name := if field_expr.kind == .ident && field_expr.value == loop_var
			&& loop_var.len > 0 {
			'FieldData'
		} else {
			tc.check_node(field_expr_id)
			tc.resolve_type(field_expr_id).name()
		}
		if actual_name != 'string' {
			tc.record_error_at(.unknown_field, 'expected `string` instead of `${actual_name}` (e.g. `field.name`)', field_expr_id, field_expr.pos)
		}
		tc.record_error_at(.unknown_field, 'expected selector expression e.g. `\$(field.name)`', field_expr_id, field_expr.pos)
		if loop_var.len == 0 {
			tc.record_invalid_comptime_selector_source_assignment(id, node)
		}
		tc.register_synth_type(id, Type(void_))
		return true
	}
	if field_expr.children_count == 0 {
		tc.register_synth_type(id, Type(void_))
		return true
	}
	field_var_id := tc.a.child(field_expr, 0)
	field_var := tc.a.node(field_var_id)
	field_var_name := tc.source_text_for_node(field_var_id)
	if loop_var.len == 0 || field_var.kind != .ident || field_var.value != loop_var {
		tc.check_node(field_expr_id)
		mut field_expr_type := tc.resolve_type(field_expr_id)
		if loop_var.len == 0 && field_var.kind == .ident
			&& tc.cur_scope.lookup(field_var.value) == none {
			tc.record_error_at(.unknown_field, '`${field_var.value}` does not return a value', field_expr_id, tc.node_value_diagnostic_pos(field_expr_id))
			field_expr_type = Type(void_)
		}
		if field_expr_type !is String {
			tc.record_error_at(.unknown_field, 'expected `string` instead of `${field_expr_type.name()}` (e.g. `field.name`)', field_expr_id, tc.node_value_diagnostic_pos(field_expr_id))
		}
		if loop_var.len == 0 {
			tc.record_error_at(.unknown_field, 'compile time field access can only be used when iterating over `T.fields`', field_var_id, field_var.pos)
		}
		tc.record_error_at(.unknown_ident, 'unknown `\$for` variable `${field_var_name}`', field_var_id, field_var.pos)
		tc.register_synth_type(id, Type(void_))
		return true
	}
	if field_expr.value != 'name' {
		tc.check_node(field_expr_id)
		actual := tc.resolve_type(field_expr_id)
		if actual !is String {
			tc.record_error_at(.unknown_field, 'expected `string` instead of `${actual.name()}` (e.g. `field.name`)', field_expr_id, tc.node_value_diagnostic_pos(field_expr_id))
			tc.register_synth_type(id, Type(void_))
			return true
		}
	}
	if field_cases.known && field_cases.cases.len > 0 {
		receiver_id := tc.a.child(&node, 0)
		receiver := tc.a.node(receiver_id)
		if receiver.kind == .ident && receiver.value == loop_var {
			return false
		}
		receiver_type := unalias_type(unwrap_pointer(tc.resolve_type(receiver_id)))
		for field in field_cases.cases {
			has_field := receiver_type is Struct
				&& tc.struct_field_type(receiver_type.name, field.name) != none
			if !has_field {
				tc.record_error_severity_at(.unknown_field, '`${tc.source_text_for_node(receiver_id)}` has no field named `${field.name}`', receiver_id, tc.a.node(receiver_id).pos, 'cgen error:')
				tc.register_synth_type(id, unknown_type('unknown imported enum'))
				return true
			}
		}
	}
	return false
}

fn (mut tc TypeChecker) record_invalid_comptime_selector_source_assignment(id flat.NodeId, node flat.Node) {
	file := tc.a.source_files[node.pos.id] or { return }
	source := tc.source_texts_by_file[file.name] or { return }
	mut cursor := int_max(0, int_min(node.pos.end, source.len))
	for cursor < source.len && source[cursor] in [` `, `\t`] {
		cursor++
	}
	if cursor >= source.len || source[cursor] != `=`
		|| (cursor + 1 < source.len && source[cursor + 1] == `=`) {
		return
	}
	cursor++
	for cursor < source.len && source[cursor] in [` `, `\t`] {
		cursor++
	}
	if cursor >= source.len || source[cursor] !in [`'`, `"`] {
		return
	}
	quote := source[cursor]
	mut end := cursor + 1
	for end < source.len && source[end] != quote && source[end] != `\n` {
		if source[end] == `\\` && end + 1 < source.len {
			end++
		}
		end++
	}
	if end < source.len && source[end] == quote {
		end++
	}
	tc.record_error_at(.assignment_mismatch, 'cannot assign to `${tc.source_text_for_node(id)}`: expected `void`, not `string`', id, token.new_span(node.pos.id, cursor, end))
}

fn (tc &TypeChecker) comptime_static_reflected_field_expr_type(id flat.NodeId, var_name string, field_cases ComptimeStaticFieldCases) ?Type {
	if !tc.valid_node_id(id) || !field_cases.known || field_cases.cases.len == 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.comptime_static_reflected_field_expr_type(tc.a.child(&node, 0), var_name, field_cases)
	}
	if node.kind == .call && node.children_count == 1 {
		callee := tc.a.child_node(&node, 0)
		if callee.kind == .selector && callee.value == 'clone' && callee.children_count > 0 {
			return tc.comptime_static_reflected_field_expr_type(tc.a.child(callee, 0), var_name, field_cases)
		}
	}
	if node.kind != .selector || node.value != '\$' || node.children_count < 2
		|| !tc.comptime_subtree_references_var(tc.a.child(&node, 1), var_name) {
		return none
	}
	first := field_cases.cases[0].typ
	for field in field_cases.cases[1..] {
		if field.typ != first {
			return none
		}
	}
	return tc.parse_type(first)
}

fn (mut tc TypeChecker) check_comptime_static_deferred_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if !value_cases.known {
		return
	}
	mut then_cases := []ComptimeStaticValueCase{}
	mut else_cases := []ComptimeStaticValueCase{}
	for item in value_cases.cases {
		cond := comptime_static_subst_deferred_cond(node.value, var_name, loop_kind, item)
		if comptime_text_references_var(cond, var_name) {
			then_cases << item
			else_cases << item
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or {
			then_cases << item
			else_cases << item
			continue
		}
		if taken {
			then_cases << item
		} else {
			else_cases << item
		}
	}
	if then_cases.len > 0 && node.children_count > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, field_cases, ComptimeStaticValueCases{
			known: true
			cases: then_cases
		})
	}
	if else_cases.len > 0 && node.children_count > 1 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases, ComptimeStaticValueCases{
			known: true
			cases: else_cases
		})
	}
}

fn (mut tc TypeChecker) check_comptime_static_method_runtime_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	condition_id := tc.a.child(&node, 0)
	mut then_cases := []ComptimeStaticValueCase{}
	mut else_cases := []ComptimeStaticValueCase{}
	for item in value_cases.cases {
		if taken := tc.comptime_static_method_condition_value(condition_id, var_name, item) {
			if taken {
				then_cases << item
			} else {
				else_cases << item
			}
		} else {
			then_cases << item
			else_cases << item
		}
	}
	tc.check_comptime_static_body(condition_id, var_name, loop_kind, field_cases, value_cases)
	if then_cases.len > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases, ComptimeStaticValueCases{
			known: true
			cases: then_cases
		})
	}
	if else_cases.len > 0 && node.children_count > 2 {
		tc.check_comptime_static_body(tc.a.child(&node, 2), var_name, loop_kind, field_cases, ComptimeStaticValueCases{
			known: true
			cases: else_cases
		})
	}
}

fn (tc &TypeChecker) comptime_static_method_condition_value(id flat.NodeId, var_name string, item ComptimeStaticValueCase) ?bool {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.comptime_static_method_condition_value(tc.a.child(node, 0), var_name, item)
	}
	if node.kind == .prefix && node.op == .not && node.children_count > 0 {
		value := tc.comptime_static_method_condition_value(tc.a.child(node, 0), var_name, item) or {
			return none
		}
		return !value
	}
	if node.kind != .infix || node.children_count != 2 {
		return none
	}
	if node.op == .logical_and {
		left := tc.comptime_static_method_condition_value(tc.a.child(node, 0), var_name, item) or {
			return none
		}
		return if left {
			tc.comptime_static_method_condition_value(tc.a.child(node, 1), var_name, item)
		} else {
			false
		}
	}
	if node.op == .logical_or {
		left := tc.comptime_static_method_condition_value(tc.a.child(node, 0), var_name, item) or {
			return none
		}
		return if left {
			true
		} else {
			tc.comptime_static_method_condition_value(tc.a.child(node, 1), var_name, item)
		}
	}
	if node.op !in [.eq, .ne] {
		return none
	}
	left := tc.comptime_static_method_string_value(tc.a.child(node, 0), var_name, item) or {
		return none
	}
	right := tc.comptime_static_method_string_value(tc.a.child(node, 1), var_name, item) or {
		return none
	}
	return if node.op == .eq { left == right } else { left != right }
}

fn (tc &TypeChecker) comptime_static_method_string_value(id flat.NodeId, var_name string, item ComptimeStaticValueCase) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.comptime_static_method_string_value(tc.a.child(node, 0), var_name, item)
	}
	if node.kind == .string_literal {
		return node.value
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident && base.value == var_name {
			return item.name
		}
	}
	return none
}

fn (tc &TypeChecker) comptime_static_method_call_return_type(id flat.NodeId, var_name string, loop_kind string, value_cases ComptimeStaticValueCases) ?Type {
	if loop_kind != 'methods' || !value_cases.known || value_cases.cases.len == 0
		|| !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if !tc.comptime_static_is_method_var_call(*node, var_name) {
		return none
	}
	first := value_cases.cases[0].return_type
	if first.len == 0 || first == 'void' {
		return none
	}
	for item in value_cases.cases[1..] {
		if item.return_type != first {
			return none
		}
	}
	return tc.parse_type(first)
}

fn comptime_static_subst_deferred_cond(cond string, var_name string, loop_kind string, item ComptimeStaticValueCase) string {
	mut result := comptime_static_replace_unquoted(cond, '${var_name}.name', comptime_static_string_literal(item.name))
	if loop_kind == 'methods' {
		result = comptime_static_subst_method_param_cond(result, var_name, item)
		result = comptime_static_replace_unquoted(result, '${var_name}.args.len', item.param_names.len.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.params.len', item.param_names.len.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.location', comptime_static_string_literal(item.location))
		result = comptime_static_replace_unquoted(result, '${var_name}.return_type', comptime_static_deferred_type(item.return_type))
		result = comptime_static_replace_unquoted(result, '${var_name}.typ', item.typ)
		if item.has_is_pub {
			result = comptime_static_replace_unquoted(result, '${var_name}.is_pub', item.is_pub.str())
		}
	} else if loop_kind == 'params' {
		result = comptime_static_replace_unquoted(result, '${var_name}.typ', comptime_static_deferred_type(item.typ))
	} else if loop_kind == 'attributes' && item.has_attr_meta {
		result = comptime_static_replace_unquoted(result, '${var_name}.has_arg', item.has_arg.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.arg', comptime_static_string_literal(item.arg))
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
			index_text := trimmed_space(result[index_start..index_end])
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
