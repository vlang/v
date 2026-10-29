module types

import os
import strconv
import time
import strings
import v.errors as compiler_errors
import v.flat
import v.gen.c.naming
import v.pref
import v.scanner
import v.token
import v.util

// short_name_view returns the suffix after the final dot without allocating.
@[direct_array_access; inline]
fn short_name_view(name string) string {
	i := name.last_index_u8(`.`)
	if i >= 0 { return unsafe { name.substr_unsafe(i + 1, name.len) } }
	return name
}

@[direct_array_access; inline]
fn owner_name_view(name string) string {
	i := name.last_index_u8(`.`)
	if i >= 0 { return unsafe { name.substr_unsafe(0, i) } }
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
	'attributes',
	'str',
	'typ',
	'unaliased_typ',
]!

const comptime_enum_value_members = [
	'name',
	'value',
	'attrs',
	'attributes',
]!

const comptime_variant_members = [
	'typ',
]!

const comptime_attribute_members = [
	'name',
	'arg',
	'has_arg',
	'kind',
]!

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
	'__v_option':    true
	'__v_result':    true
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

// unknown_type_quoting is the unknown type with the reason `<reason> `<name>``.
// When `name` is a type parameter (`T`), the unknown also stands for it.
fn unknown_type_quoting(reason string, name string) Type {
	return Type(Unknown{
		reason:      '${reason} `${name}`'
		placeholder: if is_bare_generic_param(name) { flat.source_type_id(name) } else { 0 }
	})
}

// unknown_type_quoting_id is unknown_type_quoting for the spelling `name_id`.
fn unknown_type_quoting_id(reason string, name_id u32) Type {
	name := flat.source_type_name(name_id)
	return Type(Unknown{
		reason:      '${reason} `${name}`'
		placeholder: if is_bare_generic_param(name) { name_id } else { 0 }
	})
}

// generic_placeholder_type is the type of the generic placeholder spelled
// `name_id`: the type parameter `T`, or a qualified `mod.T`, which stands for no
// type parameter (see UnknownData.placeholder).
fn generic_placeholder_type(name_id u32) Type {
	name := flat.source_type_name(name_id)
	return Type(Unknown{
		reason:      'generic placeholder `${name}`'
		placeholder: if is_bare_generic_param(name) { name_id } else { 0 }
		generic:     true
	})
}

const unresolved_name_reason = 'unresolved name'

// unresolved_name_type is the type of an expression built on a name that did not
// resolve, and of a variable declared from one. That error is already reported,
// so the checks of anything that depends on the type stay silent.
fn unresolved_name_type() Type {
	return unknown_type(unresolved_name_reason)
}

// is_unresolved_name_type reports whether `typ` comes from a name that did not
// resolve (see unresolved_name_type).
fn is_unresolved_name_type(typ Type) bool {
	return typ is Unknown && typ.reason == unresolved_name_reason
}

// unresolved_initializer_type is the type of a variable whose initializer uses a
// name that did not resolve: the initializer's type when it is still known
// (`n := len(missing)`), and unresolved_name_type otherwise.
fn unresolved_initializer_type(initializer Type) Type {
	if initializer is Void || initializer is Unknown || initializer is MultiReturn {
		return unresolved_name_type()
	}
	return initializer
}

// ident_error_type is the type of an identifier that was reported as unknown:
// unresolved_name_type when its name did not resolve, `void` for the other
// reports (an invalid variable, a missing closure capture).
fn (tc &TypeChecker) ident_error_type(id flat.NodeId) ?Type {
	mut reported := false
	for err in tc.errors {
		if err.node != id || err.kind != .unknown_ident {
			continue
		}
		if err.msg.starts_with('undefined ') {
			return unresolved_name_type()
		}
		reported = true
	}
	if reported {
		return builtin_void_type
	}
	return none
}

// mark_unresolved_receiver_call gives a method call on a receiver that did not
// resolve the unresolved type, whatever its check made of the unknown method.
fn (mut tc TypeChecker) mark_unresolved_receiver_call(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return
	}
	if is_unresolved_name_type(tc.resolve_type(tc.a.child(callee, 0))) {
		tc.register_synth_type(id, unresolved_name_type())
	}
}

// TypeError represents type error data used by types.
pub struct TypeError {
pub:
	msg              string
	kind             TypeErrorKind
	node             flat.NodeId
	file             string
	node_kind        string
	node_value       string
	node_pos         string
	pos              token.Pos
	details          []string
	severity         string
	diagnostic_order int
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
	// source_is_prefix reports that the result aliases storage at or below `source_suffix`
	// of the argument, but not a known exact path (a path widened through recursion).
	source_is_prefix bool
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
	base_name   u32
	is_array    bool
	is_multidim bool
}

struct FnDiagnosticParam {
	name       string
	typ        u32
	is_mut     bool
	is_pointer bool
}

struct MissingReferenceField {
	path     string
	owner    u32
	has_part bool
}

// ParseTypeCacheEntry keeps the identity components separate. The old
// cache built `${file}\n${module}\n${type}` for every lookup, allocating and
// hashing a temporary string before it could discover a cache hit.
struct ParseTypeCacheEntry {
	file           string
	module         string
	text           u32
	generic_params []u32
	resolution     bool
	typ            &Type = empty_type
}

// IndexedTypeCache bridges checked expression types to the current interner.
// It is private to one checker view, just like the other TypeCache entries.
struct ParsedScalarTypeCache {
mut:
	owner  &TypeInterner = unsafe { nil }
	texts  [128]u32
	ids    [128]TypeId
	values []&Type
}

struct IndexedTypeCache {
mut:
	owner    &TypeInterner = unsafe { nil }
	nodes    [4096]int
	types    [4096]TypeId
	set      [4096]bool
	name_ids [4096]TypeId
	names    [4096]u32
	name_set [4096]bool
}

struct SourceStructFieldDecl {
	name string
	typ  u32
}

struct DeclarationVisibility {
	module_name string
	kind        flat.NodeKind
	is_pub      bool
}

@[heap]
struct VisibleMutationCache {
mut:
	base                  &VisibleMutationCache = unsafe { nil }
	storage_query         bool
	storage_query_results map[string][]StorageQueryResult
	storage_query_trace   &StorageQueryTrace    = unsafe { nil }
	storage_query_owner   &VisibleMutationCache = unsafe { nil }
	storage_query_scopes  []voidptr
	storage_query_count   int
	storage_query_bytes   int
	// decls holds the module-qualified keys (`mod\x01name`) and global_decls the
	// normalized names alone. Keeping these maps separate also lets collection
	// fill them on separate pool lanes.
	decls            map[string]VisibleMutationFnDecl
	global_decls     map[string]VisibleMutationFnDecl
	decl_misses      map[string]bool
	results          map[u64]bool
	rebind_results   map[u64]bool
	decl_index_ready bool
}

fn new_visible_mutation_cache() &VisibleMutationCache {
	return &VisibleMutationCache{
		decls:          map[string]VisibleMutationFnDecl{}
		global_decls:   map[string]VisibleMutationFnDecl{}
		decl_misses:    map[string]bool{}
		results:        map[u64]bool{}
		rebind_results: map[u64]bool{}
	}
}

fn push_type_name_candidate(mut candidates []string, name string) {
	clean := trimmed_space(name)
	if clean.len > 0 && clean !in candidates {
		candidates << clean
	}
}

// Recent probes are private to each short-lived checker view; map caches
// retain entries evicted by a collision in these bounded front caches.
const type_cache_recent_slots = 512

// StructFieldTypeEntries owns one source spelling ID's field-name lookup table.
@[heap]
struct StructFieldTypeEntries {
mut:
	values map[string]&Type
}

// StructFieldMissEntries owns the misses for one source spelling ID.
@[heap]
struct StructFieldMissEntries {
mut:
	values map[string]bool
}

// SourceTypeQueryKey carries a source owner separately from its file/module context.
struct SourceTypeQueryKey {
	owner_id u32
	scope    string
}

// TypeQueryScopeEntries owns the context keys for one source owner.
@[heap]
struct TypeQueryScopeEntries {
mut:
	values map[string]int
}

// SumVariantPatternFileEntries owns the module tables for one source file.
@[heap]
struct SumVariantPatternFileEntries {
	file string
mut:
	modules map[string]&SumVariantPatternModuleEntries
}

// SumVariantPatternModuleEntries owns the numeric source-type pairs in one context.
@[heap]
struct SumVariantPatternModuleEntries {
	file   string
	module string
mut:
	values map[u64]u32
}

struct BorrowedProjectionCloneCopies {
mut:
	reads map[int]bool
}

// TypeCache represents type cache data used by types.
struct TypeCache {
mut:
	checked_fn_name_node   int = -1
	checked_fn_name_source string
	checked_fn_name_module string
	checked_fn_name_value  string
	parsed_scalars         &ParsedScalarTypeCache = unsafe { nil }
	indexed_types          &IndexedTypeCache      = unsafe { nil }
	// Frozen fallback cache shared by all parallel-check forks: lookups miss the
	// own (private) maps first, then consult base read-only; writes always go to
	// the own maps. The master installs its warm post-collect cache as base for
	// the whole region (using a private overlay itself) so workers do not start
	// cold and re-derive every memoized type/index.
	base &TypeCache = unsafe { nil }
	// Generated nodes have no parsed parent slot. Keep validated scan results
	// private to the worker, just like the other mutable lookup caches.
	generated_parent_entries map[int]flat.NodeId
	// Only ownership-enabled transforms allocate this worker-private overlay.
	// Generated ids must never be read from the shared base cache.
	borrowed_projection_clone_copies &BorrowedProjectionCloneCopies = unsafe { nil }
	parse_enabled                    bool
	parse_hits                       i64
	parse_misses                     i64
	c_hits                           i64
	c_misses                         i64
	parse_entries                    map[u64]ParseTypeCacheEntry
	parse_context_file               string
	parse_context_module             string
	parse_context_generics           []u32
	parse_context_resolution         bool
	parse_context_hash               u64
	parse_context_valid              bool
	parse_last_entry                 ParseTypeCacheEntry
	parse_last_valid                 bool
	// Source IDs are immutable across workers and allocation arenas.
	parse_value_recent_ids     [type_cache_recent_slots]u32
	parse_value_recent_context [type_cache_recent_slots]u64
	parse_value_recent_values  [type_cache_recent_slots]&Type
	parse_value_recent_set     [type_cache_recent_slots]bool
	// Bounded source-ID cache; retain the complete u32 ID to distinguish collisions.
	parse_text_id_context []u64
	parse_text_id_values  []&Type
	parse_text_ids        []u32
	// Bounded parse_canonical_type cache keyed by exact spelling ID and context.
	canonical_texts    []u32
	canonical_contexts []u64
	canonical_values   []&Type
	// Alias targets can contain callbacks whose signatures mention the alias
	// itself (for example `type Handlers = map[string]fn (Handlers)`). Keep the
	// active expansion chain private to each checker/cache so parsing such a
	// recursive alias terminates without introducing shared parallel state.
	alias_parse_stack  []u32
	c_entries          map[TypeId]u32
	allocation_layouts map[TypeId]AllocationLayout
	c_name_entries     map[string]string
	// Lock-free recent slots for c_type keyed by the interned semantic TypeId.
	// A textual name would fold distinct semantic types that share a spelling
	// (`[size]int` with different resolved `size`, or same-named aliases over
	// different bases); raw interface words and retained Type payloads are not
	// stable either, since sum-type payload storage can reuse an address.
	c_recent_ids  [type_cache_recent_slots]TypeId
	c_recent_vals [type_cache_recent_slots]u32
	c_recent_set  [type_cache_recent_slots]bool
	// type_name cannot use its result as its lookup key. Semantic hashes choose a
	// slot, and an owned Type copy verifies equality.
	name_recent_hashes [type_cache_recent_slots]u64
	name_recent_types  [type_cache_recent_slots]&Type
	name_recent_vals   [type_cache_recent_slots]u32
	name_recent_set    [type_cache_recent_slots]bool
	// Per-checker symbol probes front the compilation-wide interner. Resolved
	// names are usually repeated as the same canonical string pointer inside a
	// worker, so these slots avoid taking the interner mutex on every call.
	symbol_recent_ptrs          [type_cache_recent_slots]uint
	symbol_recent_lens          [type_cache_recent_slots]int
	symbol_recent_ids           [type_cache_recent_slots]SymbolId
	symbol_recent_vals          [type_cache_recent_slots]string
	symbol_recent_set           [type_cache_recent_slots]bool
	struct_field_entries        map[u32]&StructFieldTypeEntries
	struct_field_misses         map[u32]&StructFieldMissEntries
	struct_field_shared         map[u32]&StructFieldTypeEntries // immutable declaration types, without local TypeIds
	struct_field_complete       map[u32]bool                    // direct fields cover every lookup for these owners
	struct_field_recent_structs [256]u32
	struct_field_recent_fields  [256]string
	struct_field_recent_values  [256]&Type
	struct_field_recent_states  [256]i8
	// Only these slots can bypass the C-module and escaped-keyword guards.
	struct_field_recent_unguarded [256]bool
	struct_field_fn_diagnostics   map[string]u64 // displayed fn type << 32 | fn alias; 0: a miss
	sum_variant_pattern_entries   map[string]&SumVariantPatternFileEntries
	recv_pattern_entries          map[string]GenericReceiverMethodPatternMatch
	recv_pattern_misses           map[string]bool
	lexical_smartcast_pages       [][]u32 // see lexical_smartcast_state
	lexical_smartcast_types       []&Type
	ierror_compat_entries         map[u32]int
	ierror_scoped_compat_entries  map[u32]&TypeQueryScopeEntries
	interface_impl_entries        map[u32]flat.TypeTextList
	source_error_embed_entries    map[u32]&TypeQueryScopeEntries
	source_error_embed_indexed    bool
	source_error_embed_shared     bool
	ierror_impl_names             flat.TypeTextList
	ierror_impl_names_set         bool
	// Directory entries own these strings; a clear invalidates the recorded count.
	sum_variant_pattern_recent       &SumVariantPatternModuleEntries = unsafe { nil }
	sum_variant_pattern_recent_count int
	// short type name -> unique qualified name ('' = ambiguous); built lazily
	// by unique_qualified_type_name from the five type-name maps.
	short_type_name_index       flat.SourceTypeBindings
	short_type_name_index_built bool
	// `${module}\x01${name}` for every fn_decl node, built lazily (and extended
	// incrementally as transform appends declarations) by local_fn_decl_exists,
	// which previously walked every AST node per query.
	local_fn_decl_index       map[string]bool
	local_fn_decl_indexed_len int
	local_fn_decl_last_module string
	// tail_decl_ids lists, in order, the file/module/fn/struct declaration nodes
	// in [tail_decl_start, tail_decl_end): the nodes appended after collect built
	// its top-level index. The index scans below only look at those kinds.
	tail_decl_ids   []i32
	tail_decl_start int = -1
	tail_decl_end   int = -1
}

fn new_type_cache(parse_enabled bool) &TypeCache {
	// Allocate the fixed caches in place instead of copying a large temporary.
	// Their owned map/array payloads need a scanned allocation under a GC.
	mut cache := unsafe { &TypeCache(malloc(int(sizeof(TypeCache)))) }
	unsafe { vmemset(cache, 0, int(sizeof(TypeCache))) }
	cache.parse_enabled = parse_enabled
	cache.generated_parent_entries = map[int]flat.NodeId{}
	cache.borrowed_projection_clone_copies = unsafe { nil }
	cache.parse_entries = map[u64]ParseTypeCacheEntry{}
	cache.parse_context_generics = []u32{}
	cache.parse_text_id_context = []u64{}
	cache.parse_text_id_values = []&Type{}
	cache.parse_text_ids = []u32{}
	cache.canonical_texts = []u32{}
	cache.canonical_contexts = []u64{}
	cache.canonical_values = []&Type{}
	cache.alias_parse_stack = []u32{}
	cache.c_entries = map[TypeId]u32{}
	cache.allocation_layouts = map[TypeId]AllocationLayout{}
	cache.c_name_entries = map[string]string{}
	cache.struct_field_entries = map[u32]&StructFieldTypeEntries{}
	cache.struct_field_misses = map[u32]&StructFieldMissEntries{}
	cache.struct_field_shared = map[u32]&StructFieldTypeEntries{}
	cache.struct_field_complete = map[u32]bool{}
	cache.struct_field_fn_diagnostics = map[string]u64{}
	cache.sum_variant_pattern_entries = map[string]&SumVariantPatternFileEntries{}
	cache.recv_pattern_entries = map[string]GenericReceiverMethodPatternMatch{}
	cache.recv_pattern_misses = map[string]bool{}
	cache.lexical_smartcast_pages = [][]u32{}
	cache.lexical_smartcast_types = []&Type{}
	cache.ierror_compat_entries = map[u32]int{}
	cache.ierror_scoped_compat_entries = map[u32]&TypeQueryScopeEntries{}
	cache.interface_impl_entries = map[u32]flat.TypeTextList{}
	cache.source_error_embed_entries = map[u32]&TypeQueryScopeEntries{}
	cache.ierror_impl_names = flat.TypeTextList{}
	cache.short_type_name_index = flat.SourceTypeBindings{}
	cache.local_fn_decl_index = map[string]bool{}
	cache.tail_decl_ids = []i32{}
	cache.tail_decl_start = -1
	cache.tail_decl_end = -1
	return cache
}

fn new_type_cache_with_base(parse_enabled bool, base &TypeCache) &TypeCache {
	mut cache := new_type_cache(parse_enabled)
	cache.base = base
	return cache
}

// lexical_smartcast_state returns the cached lexical smartcast state of the node
// `idx`. The states live in pages of 1024 nodes, which a checker allocates as it
// queries them: 0 = not computed yet, 1 = no narrowing, k + 2 = narrowed to
// lexical_smartcast_types[k].
@[direct_array_access; inline]
fn (cache &TypeCache) lexical_smartcast_state(idx int) u32 {
	page := idx >> 10
	if idx < 0 || page >= cache.lexical_smartcast_pages.len {
		return 0
	}
	states := cache.lexical_smartcast_pages[page]
	if states.len == 0 {
		return 0
	}
	return states[idx & 1023]
}

// lexical_smartcast_miss reports whether the node `idx` is cached as having no
// lexical narrowing.
pub fn (cache &TypeCache) lexical_smartcast_miss(idx int) bool {
	return cache.lexical_smartcast_state(idx) == 1
}

@[direct_array_access]
fn (mut cache TypeCache) set_lexical_smartcast_state(idx int, state u32) {
	if idx < 0 {
		return
	}
	page := idx >> 10
	for cache.lexical_smartcast_pages.len <= page {
		cache.lexical_smartcast_pages << []u32{}
	}
	if cache.lexical_smartcast_pages[page].len == 0 {
		cache.lexical_smartcast_pages[page] = []u32{len: 1024}
	}
	cache.lexical_smartcast_pages[page][idx & 1023] = state
}

fn (mut cache TypeCache) clear_c_type_entries() {
	cache.indexed_types = unsafe { nil }
	cache.c_entries.clear()
	cache.allocation_layouts.clear()
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
	// Import-aware views keyed by source file, reused across every declaration
	// type resolved in that file instead of forking a full checker per call.
	scoped_by_file map[string]&TypeChecker
	// Synchronous calls usually remain in one source file. The pointers belong to
	// the same cache as the maps and are discarded with it at every phase reset.
	recent_unscoped_file string
	recent_unscoped      &TypeChecker = unsafe { nil }
	recent_scoped_file   string
	recent_scoped        &TypeChecker = unsafe { nil }
}

// TypeCacheStats reports semantic cache effectiveness for compiler telemetry.
pub struct TypeCacheStats {
pub:
	parse_hits   i64
	parse_misses i64
	c_hits       i64
	c_misses     i64
}

// FileImportInfo holds the imports of one source file.
@[heap]
pub struct FileImportInfo {
pub mut:
	// selective_imports maps the spelling ID of a name selected by
	// `import mod { name }` to the spelling IDs of the declarations it can name
	// (by module path and by alias); an empty list marks an ambiguous selection.
	selective_imports map[u32][]u32
mut:
	imports            map[string]string
	imports_name_mask  u64
	imports_mask_ready bool
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
	captured_interface_value_patterns        map[string]bool
	shared_owners                            map[string][]ScopeBindingOwner
	shared_array_owners                      map[string][]ScopeBindingOwner
	locked_shared_names                      map[string]int
	locked_shared_modes                      map[string][]u8
	locked_shared_base_names                 map[string]string
	pointer_binding_value_keys               map[string][]string
	immutable_reference_aliases              map[string]bool
	fresh_map_fields                         map[string]FreshMapField
	unsafe_reference_alias_owners            map[string]bool
	unsafe_alias_break_states                [][]map[string]bool
	pointer_alias_break_states               [][]map[string][]string
	pointer_alias_continue_states            [][]map[string][]string
	pointer_alias_goto_states                map[string][]map[string][]string
	pointer_alias_backward_goto_targets      map[string]bool
	closure_forbidden_captures               map[string]bool
	local_decl_rhs_by_name                   map[string][]LocalDeclRhs
	local_decl_rhs_indexed                   bool
	local_decl_rhs_pending                   bool
	local_decl_rhs_range_lo                  int = -1
	local_decl_rhs_range_hi                  int = -1
	bool_condition_exprs                     map[string]flat.NodeId
	has_goto_nodes                           bool
	closure_scope                            &Scope = unsafe { nil }
	lambda_no_captures                       bool
	generic_params                           []u32
	return_type                              Type = builtin_void_type
	undefined_variable_context_depth         int
	continue_after_unknown_ident             bool
	mut_local_owners_shared                  bool
	// Branch snapshots may share the map until the next binding update.
	pointer_binding_values_shared bool
	unsafe_alias_owners_shared    bool
}

fn new_function_check_context() FunctionCheckContext {
	return FunctionCheckContext{}
}

// reset_for_sibling_check retains private map storage within one check batch.
fn (mut ctx FunctionCheckContext) reset_for_sibling_check() {
	ctx.method_value_locals.clear()
	ctx.method_value_local_owners.clear()
	ctx.method_value_local_depth.clear()
	ctx.method_value_stack_mut_owners.clear()
	ctx.fn_value_variadic_locals.clear()
	ctx.fn_value_variadic_local_owners.clear()
	ctx.fn_value_variadic_local_depth.clear()
	ctx.capturing_fn_literal_locals.clear()
	ctx.capturing_fn_literal_local_owners.clear()
	ctx.capturing_fn_literal_local_depth.clear()
	ctx.mut_param_base_types.clear()
	ctx.mut_param_owners.clear()
	if ctx.mut_local_owners_shared {
		ctx.mut_local_owners = map[string]ScopeBindingOwner{}
	} else {
		ctx.mut_local_owners.clear()
	}
	ctx.mut_local_owners_shared = false
	ctx.closure_copy_owners.clear()
	ctx.captured_interface_value_patterns.clear()
	ctx.shared_owners.clear()
	ctx.shared_array_owners.clear()
	ctx.locked_shared_names.clear()
	ctx.locked_shared_modes.clear()
	ctx.locked_shared_base_names.clear()
	if ctx.pointer_binding_values_shared {
		ctx.pointer_binding_value_keys = map[string][]string{}
	} else {
		ctx.pointer_binding_value_keys.clear()
	}
	ctx.pointer_binding_values_shared = false
	ctx.immutable_reference_aliases.clear()
	ctx.fresh_map_fields.clear()
	if ctx.unsafe_alias_owners_shared {
		ctx.unsafe_reference_alias_owners = map[string]bool{}
	} else {
		ctx.unsafe_reference_alias_owners.clear()
	}
	ctx.unsafe_alias_owners_shared = false
	ctx.pointer_alias_goto_states.clear()
	ctx.pointer_alias_backward_goto_targets.clear()
	ctx.closure_forbidden_captures.clear()
	ctx.local_decl_rhs_by_name.clear()
	ctx.bool_condition_exprs.clear()
	// Nested snapshots and declaration generic parameters can share backing
	// arrays with a synchronous view or the AST. Detach those headers.
	ctx.unsafe_alias_break_states = [][]map[string]bool{}
	ctx.pointer_alias_break_states = [][]map[string][]string{}
	ctx.pointer_alias_continue_states = [][]map[string][]string{}
	ctx.generic_params = []u32{}
	ctx.node_id = -1
	ctx.concrete_generic_receiver_specialization = false
	ctx.local_decl_rhs_indexed = false
	ctx.local_decl_rhs_pending = false
	ctx.local_decl_rhs_range_lo = -1
	ctx.local_decl_rhs_range_hi = -1
	ctx.has_goto_nodes = false
	ctx.closure_scope = unsafe { nil }
	ctx.lambda_no_captures = false
	ctx.return_type = builtin_void_type
	ctx.undefined_variable_context_depth = 0
	ctx.continue_after_unknown_ident = false
}

fn clone_function_check_context(src FunctionCheckContext) FunctionCheckContext {
	return FunctionCheckContext{
		method_value_locals:                      src.method_value_locals.clone()
		method_value_local_owners:                clone_scope_binding_owner_map(src.method_value_local_owners)
		method_value_local_depth:                 src.method_value_local_depth.clone()
		method_value_stack_mut_owners:            src.method_value_stack_mut_owners.clone()
		fn_value_variadic_locals:                 src.fn_value_variadic_locals.clone()
		fn_value_variadic_local_owners:           src.fn_value_variadic_local_owners.clone()
		fn_value_variadic_local_depth:            src.fn_value_variadic_local_depth.clone()
		capturing_fn_literal_locals:              src.capturing_fn_literal_locals.clone()
		capturing_fn_literal_local_owners:        src.capturing_fn_literal_local_owners.clone()
		capturing_fn_literal_local_depth:         src.capturing_fn_literal_local_depth.clone()
		node_id:                                  src.node_id
		concrete_generic_receiver_specialization: src.concrete_generic_receiver_specialization
		mut_param_base_types:                     src.mut_param_base_types.clone()
		mut_param_owners:                         src.mut_param_owners.clone()
		mut_local_owners:                         src.mut_local_owners.clone()
		closure_copy_owners:                      src.closure_copy_owners.clone()
		captured_interface_value_patterns:        src.captured_interface_value_patterns.clone()
		shared_owners:                            src.shared_owners.clone()
		shared_array_owners:                      src.shared_array_owners.clone()
		locked_shared_names:                      src.locked_shared_names.clone()
		locked_shared_modes:                      src.locked_shared_modes.clone()
		locked_shared_base_names:                 src.locked_shared_base_names.clone()
		pointer_binding_value_keys:               clone_pointer_binding_value_keys(src.pointer_binding_value_keys)
		immutable_reference_aliases:              src.immutable_reference_aliases.clone()
		fresh_map_fields:                         src.fresh_map_fields.clone()
		unsafe_reference_alias_owners:            src.unsafe_reference_alias_owners.clone()
		unsafe_alias_break_states:                clone_unsafe_alias_break_states(src.unsafe_alias_break_states)
		pointer_alias_break_states:               clone_pointer_alias_loop_states(src.pointer_alias_break_states)
		pointer_alias_continue_states:            clone_pointer_alias_loop_states(src.pointer_alias_continue_states)
		pointer_alias_goto_states:                clone_pointer_alias_goto_states(src.pointer_alias_goto_states)
		pointer_alias_backward_goto_targets:      src.pointer_alias_backward_goto_targets.clone()
		closure_forbidden_captures:               src.closure_forbidden_captures.clone()
		local_decl_rhs_by_name:                   src.local_decl_rhs_by_name.clone()
		local_decl_rhs_indexed:                   src.local_decl_rhs_indexed
		local_decl_rhs_pending:                   src.local_decl_rhs_pending
		local_decl_rhs_range_lo:                  src.local_decl_rhs_range_lo
		local_decl_rhs_range_hi:                  src.local_decl_rhs_range_hi
		bool_condition_exprs:                     src.bool_condition_exprs.clone()
		has_goto_nodes:                           src.has_goto_nodes
		closure_scope:                            src.closure_scope
		lambda_no_captures:                       src.lambda_no_captures
		generic_params:                           src.generic_params.clone()
		return_type:                              src.return_type
		undefined_variable_context_depth:         src.undefined_variable_context_depth
		continue_after_unknown_ident:             src.continue_after_unknown_ident
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

fn clone_owned_string_list_map(source map[string][]string) map[string][]string {
	mut result := map[string][]string{}
	result.reserve(u32(source.len))
	for key, values in source {
		mut owned_values := []string{cap: values.len}
		for value in values {
			owned_values << value.clone()
		}
		// The map stores its own clone of an inserted string key.
		result[key] = owned_values
	}
	return result
}

fn clone_owned_source_type_string_list_map(source map[u32][]string) map[u32][]string {
	mut result := map[u32][]string{}
	result.reserve(u32(source.len))
	for key, values in source {
		mut owned_values := []string{cap: values.len}
		for value in values {
			owned_values << value.clone()
		}
		result[key] = owned_values
	}
	return result
}

fn clone_owned_string_bool_map(source map[string]bool) map[string]bool {
	mut result := map[string]bool{}
	result.reserve(u32(source.len))
	for key, value in source {
		result[key] = value
	}
	return result
}

fn clone_owned_string_string_map(source map[string]string) map[string]string {
	mut result := map[string]string{}
	result.reserve(u32(source.len))
	for key, value in source {
		result[key] = value.clone()
	}
	return result
}

fn clone_owned_source_type_string_map(source map[u32]string) map[u32]string {
	mut result := map[u32]string{}
	result.reserve(u32(source.len))
	for key, value in source {
		result[key] = value.clone()
	}
	return result
}

fn clone_owned_struct_field_map(source map[string][]StructField) map[string][]StructField {
	mut result := map[string][]StructField{}
	result.reserve(u32(source.len))
	for key, fields in source {
		mut owned_fields := []StructField{cap: fields.len}
		for field in fields {
			owned_fields << StructField{
				name:        field.name.clone()
				typ:         clone_owned_type(field.typ)
				has_default: field.has_default
				is_embed:    field.is_embed
				is_mut:      field.is_mut
				is_volatile: field.is_volatile
			}
		}
		result[key] = owned_fields
	}
	return result
}

fn clone_owned_source_type_struct_field_map(source map[u32][]StructField) map[u32][]StructField {
	mut result := map[u32][]StructField{}
	result.reserve(u32(source.len))
	for key, fields in source {
		mut owned_fields := []StructField{cap: fields.len}
		for field in fields {
			owned_fields << StructField{
				name:        field.name.clone()
				typ:         clone_owned_type(field.typ)
				has_default: field.has_default
				is_embed:    field.is_embed
				is_mut:      field.is_mut
				is_volatile: field.is_volatile
			}
		}
		result[key] = owned_fields
	}
	return result
}

fn clone_owned_type_owner_field_flags(source map[u32]map[string]bool) map[u32]map[string]bool {
	mut result := map[u32]map[string]bool{}
	result.reserve(u32(source.len))
	for owner_id, fields in source {
		result[owner_id] = clone_owned_string_bool_map(fields)
	}
	return result
}

fn clone_owned_type_owner_c_abi_fields(source map[u32]map[string]string) map[u32]map[string]string {
	mut result := map[u32]map[string]string{}
	result.reserve(u32(source.len))
	for owner_id, fields in source {
		result[owner_id] = clone_owned_string_string_map(fields)
	}
	return result
}

// InterfaceImplIndex maps source spelling IDs to stable runtime dispatch IDs.
pub struct InterfaceImplIndex {
pub:
	names flat.TypeTextList
	ids   map[u32]int
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
	runtime_type_indexes             map[u32]int
	a                                &flat.FlatAst = unsafe { nil }
	compiler_vroot                   string
	module_search_paths              []string
	fs_memo                          &CheckerFsMemo = unsafe { nil } // see check_import_diagnostics
	verbose                          bool
	fast_parse_recent                bool
	fast_type_text_refs              bool
	fast_c_type_recent               bool
	memo_call_info                   bool
	method_suffix_prescreen          bool
	prefix_param_scan                bool
	building_v_fast                  bool
	parallel_check_min_items         int = min_parallel_check_items // the fewest function bodies a check splits among the worker pool
	logical_file_order               []int // the `.file` markers in the order to collect them, when not the node order
	prepared_collect                 &PreparedCollect  = unsafe { nil } // what collect_continue continues from
	vls_prefs                        &pref.Preferences = unsafe { nil } // what a question parses a file again with (see vls_add_skipped_branches)
	vls_reparsed_files               map[int]bool        // the files whose left-out branches have nodes
	vls_twins                        map[int]flat.NodeId // a node added for a left-out branch's file, to the checked node it parses again
	vls_added_start                  int = max_int // the first of the nodes that questions added
	vls_body_types                   map[int]Type // the types that a generic body gives its nodes in every instance (see vls_type_generic_body)
	vls_open_types                   map[int]Type // the types, with its type parameters, that a generic body gives the nodes that depend on them (see vls_type_generic_body)
	vls_typed_bodies                 map[int]bool // the generic functions whose bodies were typed so
	keep_placeholder_types           bool         // a fork keeps the types that are a type parameter itself, which the cache of types leaves out
	placeholder_types                map[int]Type // those types
	incremental                      &IncrementalCheck = unsafe { nil } // what an incremental check keeps (see start_incremental_check)
	capture_items                    bool                  // each body checked notes where its diagnostics end (see incremental_capture)
	item_marks                       []IncrementalItemMark // those notes
	valid_diagnostic_fast            bool
	valid_resolution_fast            bool
	defer_fn_ancillary               bool
	fn_ancillary_registrations       []FnAncillaryRegistration
	fn_c_variadic_registrations      []FnNamePairRegistration
	fn_mut_receiver_registrations    []FnNamePairRegistration
	fn_ret_text_registrations        []FnTextRegistration
	visible_mutation_registrations   []VisibleMutationRegistration
	enable_globals                   bool
	disable_explicit_mutability      bool
	fn_ret_types                     map[string]Type
	fn_ret_types_revision            u64
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
	source_no_body_method_names      map[string]bool
	unsafe_fns                       map[string]bool
	unsafe_c_fns                     map[string]bool
	fn_ret_type_texts                flat.TypeTextBindings     // generic struct method key -> original return type text (e.g. `Box[T].clone` -> `Box[T]`)
	fn_param_type_texts              flat.TypeTextListBindings // generic struct method key -> original param type texts (receiver first)
	fn_type_files                    map[string]string
	fn_type_modules                  map[string]string
	transform_signature_maps_shared  bool
	transform_signature_maps_changed bool
	// transform_signature_keys_borrowed marks a transform fork whose shared
	// signature maps belong to a checker that outlives it, so a detached copy
	// can borrow their keys instead of cloning every name.
	transform_signature_keys_borrowed bool
	transform_signature_names_log     []string
	transform_struct_maps_shared      bool
	fn_generic_params                 map[string][]u32
	constraints_scanned               bool // program_declares_constraints has looked
	declares_constraints              bool
	specialized_generic_fns           map[string]bool
	fn_variadic                       map[string]bool
	c_variadic_fns                    map[string]bool
	fn_implicit_veb_ctx               map[string]bool
	receiver_method_suffix_index      map[string]string
	receiver_method_short_names       map[string]bool
	collection_method_short_names     map[string]bool // short names of `[]T.m` and `map[K]V.m` methods
	generic_receiver_method_index     map[string][]string
	structs                           map[u32][]StructField
	c_struct_scoped_fields            map[u64][]StructField
	struct_modules                    map[u32]string
	struct_files                      map[u32]string
	soa_structs                       map[u32]bool
	// Packed source-context ID and type spelling ID keys for every source-level
	// struct/type/interface/enum declaration, built once in `collect`. Replaces
	// the former full-node scan in `source_declares_type_in_scope`, which was
	// O(nodes) per call and dominated check/transform/cgen (called via qualify_name).
	declared_type_scope_keys           map[u64]bool
	concrete_type_scope_keys           map[u64]bool
	struct_error_embeds_shadow_builtin map[u32]bool
	struct_generic_params              map[u32][]u32 // generic struct base name -> type-param spelling IDs (e.g. Vec4 -> [T])
	struct_implements                  flat.SourceTypeListBindings
	struct_shared_fields               map[u32]map[string]bool
	struct_shared_element_fields       map[u32]map[string]bool
	struct_field_c_abi_fns             map[u32]map[string]string
	// Concrete receiver spelling ID and method name -> substituted CallInfo for a method *value* on a
	// generic receiver. The open `Box[T].method` registration is gone by cgen time, so
	// the checker stashes the resolved signature here for gen_method_value_closure.
	generic_method_value_info             map[u32]map[string]CallInfo
	params_structs                        map[u32]bool
	c_typedef_structs                     map[u32]bool
	unions                                map[u32]bool
	type_aliases                          flat.SourceTypeBindings
	type_alias_modules                    map[u32]string
	type_alias_generic_params             map[u32][]u32 // generic alias base name -> type-param spelling IDs
	type_alias_c_abi_fns                  map[u32]string
	recursive_alias_names                 map[u32]bool
	sum_types                             flat.SourceTypeListBindings
	sum_generic_params                    map[u32][]u32 // generic sum type base name -> type-param spelling IDs (e.g. Tree -> [T])
	enum_names                            map[u32]bool
	enum_fields                           map[u32][]string
	flag_enums                            map[u32]bool
	interface_names                       map[u32]bool
	interface_generic_params              map[u32][]u32
	interface_fields                      map[u32][]StructField
	interface_embeds                      flat.SourceTypeListBindings
	interface_abstract_methods            map[u32][]string // iface -> abstract (declared) method names
	interface_impl_name_snapshots         map[u32]flat.TypeTextList
	interface_impl_candidates_at_snapshot map[u32]bool
	interface_impl_candidates_at_index    map[u32]bool
	interface_method_names_index          map[u32][]string
	interface_abstract_index              map[u32][]string
	interface_field_list_index            map[u32][]StructField
	interface_impl_indexes                map[u32]&InterfaceImplIndex
	interface_query_indexes_ready         bool

	c_globals                map[string]Type
	inferred_c_globals       []InferredCGlobal // the entries of c_globals that this checker added, for its merge
	global_names             map[string]bool
	shared_global_names      map[string]bool
	const_types              map[string]Type
	const_exprs              map[string]flat.NodeId
	comptime_const_values    map[string]ComptimeConstValue
	has_comptime_functions   bool
	const_modules            map[string]string
	const_files              map[string]string
	const_suffixes           map[string]string // dot-suffix -> full const key (O(1) lookup; '' if ambiguous)
	declaration_visibility   map[string]DeclarationVisibility
	checked_const_names      map[string]bool
	imports                  map[string]string // alias -> short module name
	file_imports             map[string]string
	file_imports_by_file     map[string]&FileImportInfo
	selective_import_files   map[string]&FileImportInfo // files with `import mod { name }`; shared entries of file_imports_by_file
	const_import_modules     map[string][]string
	const_import_files_count int = -1

	// Whether the source of a file (by id) has an `@[if ` attribute.
	if_attribute_files map[int]bool
	// The leading names of the root scope `root_scope_globals` (of `lifetime`)
	// that global_names holds (see root_scope_global_name_prefix).
	root_scope_globals         voidptr
	root_scope_global_lifetime int
	root_scope_global_prefix   int
	// Field attributes remain immutable after collection; a growing AST invalidates this filter.
	required_fields_ast_nodes int  = -1
	has_required_fields       bool = true
	file_modules              map[string]string
	translated_files          map[string]bool
	generated_files           map[string]bool // files of `@[generated]` modules: relaxed naming rules
	has_globals_files         map[string]bool
	deprecated_symbols        map[string]DeprecationInfo
	deprecated_modules        map[string]DeprecationInfo
	file_scope                &Scope = unsafe { nil }
	cur_scope                 &Scope = unsafe { nil }
	scope_pool                []&Scope
	scope_pool_index          int
	has_builtins              bool
	cur_module                string
	cur_file                  string
	generic_decl_file         string // declaring file of the generic param text being inferred ('' = cur_file)
	unsafe_depth              int
	defer_unsafe_depth        int // `unsafe_depth` at the start of the innermost `defer` body
	sort_comparator_depth     int
	lock_depth                int
	autolocked_map            string // the `shared` map locked by the operation being checked, see SharedMapAutolock
	comptime_static_depth     int
	enum_field_value_depth    int // > 0 while an enum field initializer is checked
	errors                    []TypeError
	notices                   []TypeError
	resolved_call_names       []&CachedName // node_id -> resolved function name
	resolved_call_set         []bool
	statement_nodes           []bool
	// Exact call/function-value dependencies recorded while each function is
	// checked. Consumers such as markused can walk these resolved Symbol-like
	// names instead of reconstructing import and receiver resolution from syntax.
	direct_dependencies_by_fn map[int][]SymbolId // enclosing fn node id -> resolved function identities
	// Read-only post-check call graph for transform forks (see
	// fork_for_parallel_transform); nil in the checker that owns the graph.
	post_check_dependencies &DependencyGraphView = unsafe { nil }
	global_store_cache      &FnGlobalStoreCache  = unsafe { nil }
	// call_param_flows_to_callback results computed before transform lowers the
	// bodies they read (see prepare_callback_param_flows); shared with forks.
	callback_param_flows &CallbackParamFlows = unsafe { nil }
	// The interface boxes the transform stage collected from the tree before
	// lowering, the node count it scanned and the library bodies it skipped, for
	// the monomorphization stage to start from. transform_boxed_node_count is -1
	// until the transform stage sets them.
	transform_boxed_types      []u64
	transform_boxed_values     []bool
	transform_boxed_skipped    []bool
	transform_boxed_node_count int = -1
	// Parameter storage and call provenance summaries computed before
	// lowering, keyed by fn_decl node, and the node count they cover, for the
	// monomorphization stage to start from.
	transform_borrow_params                 map[int][]bool
	transform_call_provenance_borrow_params map[int][]bool
	transform_borrow_node_count             int
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
	comptime_attr_narrow_state              &ComptimeAttributeNarrowState = unsafe { nil }
	expr_type_values                        []&Type // node_id -> complex/contextual resolved type
	expr_type_set                           []bool
	lexical_smartcast_misses                []bool
	checking_nodes                          []bool
	parallel_check_sparse                   bool
	scope_parallel_check_workers            bool
	// A pool worker's batch arena, kept between its batches and rewound for the
	// next one (see check_scoped_batches); nil unless the worker opted in.
	reuse_check_batch_scope bool
	check_batch_scope       voidptr
	// Node id range [check_range_lo, check_range_hi] of the fn item currently
	// being checked. Fn subtrees are disjoint contiguous ranges (each fn_decl at
	// index i owns (prev_top_level_idx, i]), so while parallel_check_sparse is
	// set, cache entries for in-range ids are written straight into the shared
	// node-indexed arrays (this checker is the range's only writer) and only
	// out-of-range ids (consts, other decls' nodes) go through the private
	// sparse maps that are merged after join.
	check_range_lo             int = -1
	check_range_hi             int = -1
	sparse_resolved_call_names map[int]string
	sparse_resolved_fn_values  map[int]string // the only fn-value name store: a few dozen entries, a node-indexed array cost ~12 MB
	// Only private transform/codegen snapshots enable this conservative filter.
	// Mutable masters and storage probes keep supporting direct map writes.
	sparse_fn_value_filter        [4]u64
	sparse_fn_value_filter_ready  bool
	fork_fn_value_writes          map[int]string // a transform fork's own fn-value writes ('' = cleared); its sparse map stays a read-only snapshot and only these are merged
	sparse_statement_nodes        map[int]bool
	sparse_expr_type_values       map[int]Type
	sparse_checking_nodes         map[int]bool
	diagnose_unknown_calls        bool
	reject_unlowered_map_mutation bool
	reject_unsupported_generics   bool
	checker_fixture_mode          bool
	is_test                       bool
	checked_comptime_method_calls map[u64][]ComptimeMethodCallInstance
	comptime_method_calls_by_decl map[int]map[int]bool
	check_concrete_generic_bodies bool                 // `-check` checks the concrete clones of the program's generics for fields and methods their types lack, and for arrays and maps with the wrong element layout
	check_program_lib_instances   bool                 // such a check also checks the instances of library generics that the program's own code asks for (see transform.LibraryBodies)
	library_instances             int                  // the instances of library generics that such a check cloned
	library_headers               int                  // how many of them it cloned without their bodies (see transform.LibraryBodies)
	library_bodies_kept_for       string               // why every one of them kept its body, when one had to
	check_generic_bodies          bool                 // the bodies of generic functions whose type parameters all have a constraint are checked
	type_param_texts              map[u32]u32          // in a fork that checks a generic body with its type parameters as types their constraints admit, the text of each (check_generic_fn_body_as)
	type_params_expanding         map[u32]bool         // the type parameters of type_param_texts whose own text is being parsed: `T` of `[T Comparable[T]]` stays open inside it
	concrete_parents_indexed      int                  // nodes the parent index covered when the last concrete clone was checked
	concrete_guard_options        map[flat.NodeId]bool // in check_concrete_instance_members, the `if x := a` operands declared as an option or result, smartcast to their payload
	checking_concrete_members     bool                 // check_concrete_instance_members is checking a clone
	module_diagnostic_root        string
	autofree_mode                 bool
	no_main                       bool
	nofloat                       bool
	is_js_backend                 bool
	warn_about_allocs             bool
	warns_are_errors              bool
	explicit_warns_are_errors     bool
	notes_are_errors              bool
	is_prod                       bool
	cross_target_prefs            &pref.Preferences = unsafe { nil }
	suppress_dump_output          bool
	diagnostic_files              map[string]bool
	skips_library_bodies          bool // see skip_unreachable_library_bodies
	// The short names of the globals, constants, sum types and type aliases, and how
	// many entries the tables had when they were taken (see prepare_check_name_filters), so that a
	// selector or callee can be rejected without hashing its qualified spellings.
	// const_suffix_key_count is the size of const_types that const_suffixes covers.
	global_short_names           map[string]bool
	global_short_names_count     int = -1
	sum_type_short_names         map[u32]bool
	sum_type_short_names_count   int = -1
	alias_short_names            map[u32]bool
	alias_short_names_count      int                 = -1
	const_suffix_key_count       int                 = -1
	const_selector_bloom         &ConstSelectorBloom = unsafe { nil } // of const_types and const_suffixes
	const_selector_suffix_count  int                 = -1
	const_expr_short_names       map[string]bool
	const_expr_short_names_count int = -1
	global_name_bloom            NameBloom // of global_names
	call_flag_bloom              NameBloom // of the set call_info flags, see call_flag_tables_count
	call_flag_lens               [4]int    // call_flag_table_lens when call_flag_bloom was built
	// The last call whose callee generic_compile_error_instantiation found to be
	// no generic function: its children start, callee name and module.
	nongeneric_call_start  int = -1
	nongeneric_call_name   string
	nongeneric_call_module string
	// Whether the receiver parameter of fn_decl node receiver_memo_fn, checked in
	// file and module receiver_memo_file / receiver_memo_module, has the type of
	// the receiver its name declares (current_receiver_param_method_call_info).
	receiver_memo_fn     int = -1
	receiver_memo_file   string
	receiver_memo_module string
	receiver_memo_ok     bool
	// Set by markused only after convergence for its returned reachability set.
	// Adding roots requires the ordinary late-body convergence check again.
	library_body_convergence_complete bool
	library_files                     map[string]bool // the files whose unreachable bodies the check leaves out
	reachable_library_fns             map[string]bool // the functions of those files that it checks
	shadow_diagnostic_root            string
	shadow_explicit_roots             []string
	shadow_dependency_roots           []string
	multiple_module_import_lines      map[u64]bool
	import_line_indexed_files         map[int]bool
	source_texts_by_file              map[string]string
	source_texts_by_id                map[int]string
	// Zero means unindexed; one rules out the exceptional `cosnt` diagnostic.
	malformed_const_source_states []u8
	ct_update_pos                 map[int]token.Pos
	ct_update_indexed             bool
	insert_include_dirs_by_file   map[string][]string
	has_spawn_expr                int = -1
	inactive_top_level_node_ids   []i32
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
	top_level_idx           []i32
	top_level_idx_nodes_len int
	// mut_binding_value_id is the value of the `mut` binding (`mut x := v`, or
	// `x = v` for a mutable `x`) being checked. Like V1, only a struct literal
	// that is that value (or one of its branch tails) rejects `&immutable` in its
	// `mut:` fields.
	mut_binding_value_id int = -1
	// Anonymous and function-local struct declarations are synthesized below
	// the file's top-level declaration tree. The direct-parent pass records their
	// sorted node ids so collect_top_level_idx_fast can merge them without
	// rescanning every gap between parser-recorded declarations.
	synthetic_top_level_type_ids  []i32
	expected_expr_id              int  = -1
	expected_expr_type            Type = builtin_void_type
	cur_fn_ret_type               Type = builtin_void_type
	channel_send_or_expr_id       int  = -1
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
	// The smartcast keys of the body being checked (see SmartcastKeyFilter);
	// nil outside check_fn_items_serial.
	smartcast_key_filter &SmartcastKeyFilter = unsafe { nil }
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
	// Immutable declaration indexes shared by checker workers. They are public, so
	// the tests of other compiler modules can register declarations directly.
	declaration_attributes map[int][]string
	type_declaration_ids   map[u32][]int
	// attribute_schemas indexes custom attribute schemas by short name
	// (vlang/rfcs#35). Several modules may declare the same short name;
	// uses resolve V-strict, see check_custom_attribute_uses.
	attribute_schemas map[string][]AttributeSchema
mut:
	checked_library_body_count int
	// Successful reachability frontiers only append notices. Keep their last
	// sorted size; error-producing batches retain the full sorting path.
	library_sorted_notice_count int  = -1
	cache_lexical_parents       bool = true
	// Source-validated bodies consisting solely of a known terminating call.
	// Their lowered temporaries belong to the exempt argument expression.
	noalloc_terminal_functions map[string]bool
	// Dropped with phase caches before a disposable arena is released. Forks
	// start with nil so their mutable topology slots are never shared.
	lexical_parent_memo &LexicalParentMemo = unsafe { nil }
	// Only set during check_semantics_opt; forks and views share the immutable
	// indexes (see CheckDeclIndexes).
	check_decl_indexes &CheckDeclIndexes = unsafe { nil }
	// Private to one checking checker: views do not inherit it.
	source_type_keyword_memo &SourceTypeKeywordMemo = unsafe { nil }
	// Only set on the checker that runs the serial interface index loop.
	interface_impl_memo &InterfaceImplMemo = unsafe { nil }
	// Includes method-value aliases and binding-owner maps; all backing maps are
	// replaced together at every function/worker boundary.
	fn_context                 FunctionCheckContext
	selective_import_index     &SelectiveImportIndex    = &SelectiveImportIndex{}
	type_cache                 &TypeCache               = unsafe { nil }
	pre_transform_type_cache   &TypeCache               = unsafe { nil }
	resolution_type_views      &ResolutionTypeViewCache = unsafe { nil }
	visible_mutation_cache     &VisibleMutationCache    = unsafe { nil }
	storage_query_probe        &StorageQueryProbe       = unsafe { nil }
	storage_query_probe_scopes []voidptr
	type_interner              &TypeInterner   = unsafe { nil }
	symbols                    &SymbolInterner = unsafe { nil }
	// direct_parent_ids maps a parsed node to the first AST node that references
	// it as a child. It is immutable during semantic checking and shared by
	// checker workers. Transformed or appended nodes use the scan fallback in
	// direct_parent_id.
	direct_parent_ids           []flat.NodeId
	preflight_node_ids          []i32
	preflight_index_nodes_len   int = -1
	rewritten_parent_ids        []flat.NodeId
	value_used_nodes            []bool
	fn_check_costs              []i32
	direct_parent_index_trusted bool
	has_goto_nodes              bool
	// Memo of comptime_methods_loop_exists for the AST at comptime_methods_scan_ast.
	comptime_methods_scan_ast   voidptr
	comptime_methods_scan_len   int
	comptime_methods_loop_found bool
	// Immutable declaration indexes shared by checker workers.
	strings_builder_bindings          map[string]bool
	strings_builder_candidates        []i32
	static_associated_fn_keys         map[string]bool
	static_associated_method_names    map[string]bool
	static_associated_signature_count int = -1
	declaration_param_mutability      map[string][]bool
	strict_map_index_files            map[string]bool
	// transform_static_method_names is static_associated_method_names taken again from
	// the signatures a batch of function bodies starts with: transform registers
	// signatures as it goes, which leaves the checked index behind for good.
	// transform_static_method_count is the number of signatures it lists, -1 for none.
	transform_static_method_names map[string]bool
	transform_static_method_count int = -1
	// short fn name -> first declaring top-level node index, in declaration
	// order (mirrors the expr_raw_fn_type_id scan's first-match rule).
	fn_decl_short_name_ids map[string]int
	// Raw and qualified names use the first matching declaration, including the
	// main-prefixed raw aliases accepted by recursive-str helper lookup.
	recursive_str_fn_decl_ids            map[string]int
	recursive_str_fn_decl_index_size     int = -1
	recursive_str_fn_decl_index_complete bool
	// '${file id}\x00${fn name}' for every top-level fn_decl, so a per-call
	// "does this file declare a bare fn of this name" lookup does not walk the
	// whole top-level index.
	file_bare_fn_names map[string]bool
	// '${file}\x00${alias}' -> dotted import path, and '${file}\x00${last
	// segment}' -> dotted import path (first import wins), replacing per-call
	// scans over every top-level declaration.
	file_import_alias_paths  map[string]string
	file_import_suffix_paths map[string]string
	// struct name -> embedded receiver type names (empty entry when the struct
	// has no embeds). Structs added after collect (monomorphization) miss this
	// index and fall back to the field walk.
	struct_embed_receivers flat.SourceTypeListBindings
	// Immutable node -> generic parameter index shared by checker workers.
	enclosing_generic_params_by_node map[int][]u32
	enclosing_generic_param_masks    []u32
	first_type_declaration_ids       map[u32]int
	// Immutable attribute-kind sidecars collected with declaration_attributes.
	declaration_attribute_kinds map[int]u32
}

fn (tc &TypeChecker) timing_profile(message string) {
	if tc.verbose {
		eprintln(message)
	}
}

// enable_scoped_parallel_workers uses disposable prealloc arenas for parallel
// checker helpers, including ownership snapshots and inferred metadata.
pub fn (mut tc TypeChecker) enable_scoped_parallel_workers() {
	tc.scope_parallel_check_workers = true
}

// scoped_parallel_workers_enabled reports whether compiler stages should use
// short-lived worker arenas with this checker.
pub fn (tc &TypeChecker) scoped_parallel_workers_enabled() bool {
	return tc.scope_parallel_check_workers
}

// TypeChecker.new creates a TypeChecker value for types.
pub fn TypeChecker.new(a &flat.FlatAst) TypeChecker {
	fs := new_scope(unsafe { nil })
	type_interner := new_type_interner()
	symbols := new_symbol_interner()
	return TypeChecker{
		a:                                       a
		fast_parse_recent:                       os.getenv('V3_NO_FAST_PARSE_RECENT') == ''
		fast_type_text_refs:                     os.getenv('V3_NO_TYPE_TEXT_REFS') == ''
		fast_c_type_recent:                      os.getenv('V3_NO_FAST_C_TYPE_RECENT') == ''
		memo_call_info:                          os.getenv('V3_NO_CALL_INFO_MEMO') == ''
		method_suffix_prescreen:                 os.getenv('V3_NO_METHOD_SUFFIX_PRESCREEN') == ''
		prefix_param_scan:                       os.getenv('V3_NO_PREFIX_PARAM_SCAN') == ''
		fn_ret_types:                            map[string]Type{}
		fn_param_types:                          map[string][]Type{}
		c_fn_module_ret_types:                   map[string]Type{}
		c_fn_module_param_types:                 map[string][]Type{}
		c_fn_module_variadic:                    map[string]bool{}
		c_fn_abi_variadic_prefixes:              map[string]int{}
		fn_shared_params:                        map[string][]bool{}
		mut_receiver_methods:                    map[string]bool{}
		source_no_body_fns:                      map[string]bool{}
		source_no_body_fn_suffixes:              map[string]bool{}
		source_no_body_method_names:             map[string]bool{}
		unsafe_fns:                              map[string]bool{}
		unsafe_c_fns:                            map[string]bool{}
		fn_ret_type_texts:                       flat.TypeTextBindings{}
		fn_param_type_texts:                     flat.TypeTextListBindings{}
		fn_type_files:                           map[string]string{}
		fn_type_modules:                         map[string]string{}
		fn_generic_params:                       map[string][]u32{}
		specialized_generic_fns:                 map[string]bool{}
		fn_variadic:                             map[string]bool{}
		c_variadic_fns:                          map[string]bool{}
		fn_implicit_veb_ctx:                     map[string]bool{}
		receiver_method_suffix_index:            map[string]string{}
		receiver_method_short_names:             map[string]bool{}
		collection_method_short_names:           map[string]bool{}
		generic_receiver_method_index:           map[string][]string{}
		structs:                                 map[u32][]StructField{}
		c_struct_scoped_fields:                  map[u64][]StructField{}
		struct_modules:                          map[u32]string{}
		struct_files:                            map[u32]string{}
		soa_structs:                             map[u32]bool{}
		declared_type_scope_keys:                map[u64]bool{}
		concrete_type_scope_keys:                map[u64]bool{}
		struct_error_embeds_shadow_builtin:      map[u32]bool{}
		struct_generic_params:                   map[u32][]u32{}
		struct_implements:                       flat.SourceTypeListBindings{}
		struct_shared_fields:                    map[u32]map[string]bool{}
		struct_shared_element_fields:            map[u32]map[string]bool{}
		struct_field_c_abi_fns:                  map[u32]map[string]string{}
		generic_method_value_info:               map[u32]map[string]CallInfo{}
		params_structs:                          map[u32]bool{}
		c_typedef_structs:                       map[u32]bool{}
		unions:                                  map[u32]bool{}
		type_aliases:                            flat.SourceTypeBindings{}
		type_alias_modules:                      map[u32]string{}
		type_alias_generic_params:               map[u32][]u32{}
		type_alias_c_abi_fns:                    map[u32]string{}
		sum_types:                               flat.SourceTypeListBindings{}
		sum_generic_params:                      map[u32][]u32{}
		enum_names:                              map[u32]bool{}
		enum_fields:                             map[u32][]string{}
		flag_enums:                              map[u32]bool{}
		interface_names:                         map[u32]bool{}
		interface_generic_params:                map[u32][]u32{}
		interface_fields:                        map[u32][]StructField{}
		interface_embeds:                        flat.SourceTypeListBindings{}
		interface_abstract_methods:              map[u32][]string{}
		interface_impl_name_snapshots:           map[u32]flat.TypeTextList{}
		interface_impl_candidates_at_snapshot:   map[u32]bool{}
		interface_impl_candidates_at_index:      map[u32]bool{}
		interface_method_names_index:            map[u32][]string{}
		interface_abstract_index:                map[u32][]string{}
		interface_field_list_index:              map[u32][]StructField{}
		interface_impl_indexes:                  map[u32]&InterfaceImplIndex{}
		c_globals:                               map[string]Type{}
		global_names:                            map[string]bool{}
		shared_global_names:                     map[string]bool{}
		const_types:                             map[string]Type{}
		const_exprs:                             map[string]flat.NodeId{}
		const_modules:                           map[string]string{}
		const_files:                             map[string]string{}
		const_suffixes:                          map[string]string{}
		declaration_visibility:                  map[string]DeclarationVisibility{}
		imports:                                 map[string]string{}
		file_imports:                            map[string]string{}
		file_imports_by_file:                    map[string]&FileImportInfo{}
		selective_import_files:                  map[string]&FileImportInfo{}
		file_modules:                            map[string]string{}
		translated_files:                        map[string]bool{}
		generated_files:                         map[string]bool{}
		has_globals_files:                       map[string]bool{}
		deprecated_symbols:                      map[string]DeprecationInfo{}
		deprecated_modules:                      map[string]DeprecationInfo{}
		file_scope:                              fs
		cur_scope:                               fs
		// The node-indexed cache arrays start empty: collect() sizes them via
		// reset_node_caches (allocating them here too paid for everything
		// twice), and extend_node_caches grows them on demand for any checker
		// used without a collect() call.
		resolved_call_names:                     []&CachedName{}
		resolved_call_set:                       []bool{}
		statement_nodes:                         []bool{}
		method_values_by_fn:                     map[int][]string{}
		method_value_locals:                     map[string]bool{}
		method_value_local_depth:                map[string]int{}
		capturing_fn_literal_locals:             map[string]bool{}
		capturing_fn_literal_local_depth:        map[string]int{}
		capturing_fn_literal_return_unsupported: map[string]bool{}
		cur_fn_mut_param_base_types:             map[string]Type{}
		cur_fn_mut_param_binding_owners:         map[string]ScopeBindingOwner{}
		cur_fn_mut_local_binding_owners:         map[string]ScopeBindingOwner{}
		cur_fn_shared_binding_owners:            map[string]ScopeBindingOwner{}
		expr_type_values:                        []&Type{}
		expr_type_set:                           []bool{}
		lexical_smartcast_misses:                []bool{}
		checking_nodes:                          []bool{}
		sparse_resolved_call_names:              map[int]string{}
		sparse_resolved_fn_values:               map[int]string{}
		sparse_statement_nodes:                  map[int]bool{}
		sparse_expr_type_values:                 map[int]Type{}
		sparse_checking_nodes:                   map[int]bool{}
		diagnostic_files:                        map[string]bool{}
		multiple_module_import_lines:            map[u64]bool{}
		source_texts_by_file:                    map[string]string{}
		selected_file_called_fns:                map[string]bool{}
		smartcasts:                              map[string]Type{}
		type_cache:                              new_type_cache(false)
		resolution_type_views:                   &ResolutionTypeViewCache{
			by_file: map[string]&TypeChecker{}
		}
		visible_mutation_cache:                  new_visible_mutation_cache()
		type_interner:                           type_interner
		symbols:                                 symbols
		enclosing_generic_params_by_node:        map[int][]u32{}
		declaration_attributes:                  map[int][]string{}
		attribute_schemas:                       map[string][]AttributeSchema{}
		noalloc_terminal_functions:              map[string]bool{}
		type_declaration_ids:                    map[u32][]int{}
		strings_builder_bindings:                map[string]bool{}
		static_associated_fn_keys:               map[string]bool{}
		declaration_param_mutability:            map[string][]bool{}
		strict_map_index_files:                  map[string]bool{}
		fn_decl_short_name_ids:                  map[string]int{}
		recursive_str_fn_decl_ids:               map[string]int{}
		file_bare_fn_names:                      map[string]bool{}
		file_import_alias_paths:                 map[string]string{}
		file_import_suffix_paths:                map[string]string{}
		struct_embed_receivers:                  flat.SourceTypeListBindings{}
	}
}

// fork_program_view builds a checker view over immutable, compilation-wide
// semantic data. Mutable scope, diagnostics, sparse caches, and function state
// start private. Dependency-map ownership is explicit: parallel workers pass
// a fresh map, while a synchronous subview may keep recording in its owning
// checker's private map. Keeping this constructor explicit prevents a
// newly-added mutable field from being silently shared by parallel workers.
fn (tc &TypeChecker) fork_program_view(ast &flat.FlatAst, direct_dependencies_by_fn map[int][]SymbolId) &TypeChecker {
	fs := new_scope(tc.file_scope)
	return &TypeChecker{
		a:                                     ast
		compiler_vroot:                        tc.compiler_vroot
		runtime_type_indexes:                  tc.runtime_type_indexes
		module_search_paths:                   tc.module_search_paths
		fast_parse_recent:                     tc.fast_parse_recent
		fast_type_text_refs:                   tc.fast_type_text_refs
		fast_c_type_recent:                    tc.fast_c_type_recent
		memo_call_info:                        tc.memo_call_info
		cache_lexical_parents:                 tc.cache_lexical_parents
		method_suffix_prescreen:               tc.method_suffix_prescreen
		prefix_param_scan:                     tc.prefix_param_scan
		building_v_fast:                       tc.building_v_fast
		valid_diagnostic_fast:                 tc.valid_diagnostic_fast
		valid_resolution_fast:                 tc.valid_resolution_fast
		is_js_backend:                         tc.is_js_backend
		cross_target_prefs:                    tc.cross_target_prefs
		enable_globals:                        tc.enable_globals
		disable_explicit_mutability:           tc.disable_explicit_mutability
		fn_ret_types:                          tc.fn_ret_types
		fn_param_types:                        tc.fn_param_types
		c_fn_module_ret_types:                 tc.c_fn_module_ret_types
		c_fn_module_param_types:               tc.c_fn_module_param_types
		c_fn_module_variadic:                  tc.c_fn_module_variadic
		c_fn_abi_variadic_prefixes:            tc.c_fn_abi_variadic_prefixes
		fn_shared_params:                      tc.fn_shared_params
		mut_receiver_methods:                  tc.mut_receiver_methods
		source_no_body_fns:                    tc.source_no_body_fns
		source_no_body_fn_suffixes:            tc.source_no_body_fn_suffixes
		source_no_body_method_names:           tc.source_no_body_method_names
		unsafe_fns:                            tc.unsafe_fns
		unsafe_c_fns:                          tc.unsafe_c_fns
		fn_ret_type_texts:                     tc.fn_ret_type_texts
		fn_param_type_texts:                   tc.fn_param_type_texts
		fn_type_files:                         tc.fn_type_files
		fn_type_modules:                       tc.fn_type_modules
		fn_generic_params:                     tc.fn_generic_params
		transform_signature_names_log:         []string{}
		specialized_generic_fns:               tc.specialized_generic_fns
		fn_variadic:                           tc.fn_variadic
		c_variadic_fns:                        tc.c_variadic_fns
		fn_implicit_veb_ctx:                   tc.fn_implicit_veb_ctx
		receiver_method_suffix_index:          tc.receiver_method_suffix_index
		receiver_method_short_names:           tc.receiver_method_short_names
		collection_method_short_names:         tc.collection_method_short_names
		generic_receiver_method_index:         tc.generic_receiver_method_index
		structs:                               tc.structs
		c_struct_scoped_fields:                tc.c_struct_scoped_fields
		struct_modules:                        tc.struct_modules
		struct_files:                          tc.struct_files
		declared_type_scope_keys:              tc.declared_type_scope_keys
		concrete_type_scope_keys:              tc.concrete_type_scope_keys
		struct_error_embeds_shadow_builtin:    tc.struct_error_embeds_shadow_builtin
		struct_generic_params:                 tc.struct_generic_params
		struct_implements:                     tc.struct_implements
		struct_shared_fields:                  tc.struct_shared_fields
		struct_shared_element_fields:          tc.struct_shared_element_fields
		struct_field_c_abi_fns:                tc.struct_field_c_abi_fns
		generic_method_value_info:             tc.generic_method_value_info
		params_structs:                        tc.params_structs
		c_typedef_structs:                     tc.c_typedef_structs
		unions:                                tc.unions
		soa_structs:                           tc.soa_structs
		type_aliases:                          tc.type_aliases
		type_alias_modules:                    tc.type_alias_modules
		type_alias_generic_params:             tc.type_alias_generic_params
		type_alias_c_abi_fns:                  tc.type_alias_c_abi_fns
		sum_types:                             tc.sum_types
		sum_generic_params:                    tc.sum_generic_params
		enum_names:                            tc.enum_names
		enum_fields:                           tc.enum_fields
		flag_enums:                            tc.flag_enums
		interface_names:                       tc.interface_names
		interface_generic_params:              tc.interface_generic_params
		interface_fields:                      tc.interface_fields
		interface_embeds:                      tc.interface_embeds
		interface_abstract_methods:            tc.interface_abstract_methods
		interface_impl_name_snapshots:         tc.interface_impl_name_snapshots
		interface_impl_candidates_at_snapshot: tc.interface_impl_candidates_at_snapshot
		interface_impl_candidates_at_index:    tc.interface_impl_candidates_at_index
		interface_method_names_index:          tc.interface_method_names_index
		interface_abstract_index:              tc.interface_abstract_index
		interface_field_list_index:            tc.interface_field_list_index
		interface_impl_indexes:                tc.interface_impl_indexes
		interface_query_indexes_ready:         tc.interface_query_indexes_ready
		c_globals:                             tc.c_globals
		global_names:                          tc.global_names
		shared_global_names:                   tc.shared_global_names
		const_types:                           tc.const_types
		comptime_const_values:                 tc.comptime_const_values
		has_comptime_functions:                tc.has_comptime_functions
		const_exprs:                           tc.const_exprs
		const_modules:                         tc.const_modules
		const_files:                           tc.const_files
		const_suffixes:                        tc.const_suffixes
		const_suffix_key_count:                tc.const_suffix_key_count
		const_selector_bloom:                  tc.const_selector_bloom
		const_selector_suffix_count:           tc.const_selector_suffix_count
		const_expr_short_names:                tc.const_expr_short_names
		const_expr_short_names_count:          tc.const_expr_short_names_count
		global_short_names:                    tc.global_short_names
		global_short_names_count:              tc.global_short_names_count
		sum_type_short_names:                  tc.sum_type_short_names
		sum_type_short_names_count:            tc.sum_type_short_names_count
		alias_short_names:                     tc.alias_short_names
		alias_short_names_count:               tc.alias_short_names_count
		global_name_bloom:                     tc.global_name_bloom
		call_flag_bloom:                       tc.call_flag_bloom
		call_flag_lens:                        tc.call_flag_lens
		declaration_visibility:                tc.declaration_visibility
		imports:                               tc.imports
		file_imports:                          tc.file_imports
		file_imports_by_file:                  tc.file_imports_by_file
		selective_import_files:                tc.selective_import_files
		required_fields_ast_nodes:             tc.required_fields_ast_nodes
		has_required_fields:                   tc.has_required_fields
		const_import_modules:                  tc.const_import_modules
		const_import_files_count:              tc.const_import_files_count
		file_modules:                          tc.file_modules
		translated_files:                      tc.translated_files
		generated_files:                       tc.generated_files
		has_globals_files:                     tc.has_globals_files
		deprecated_symbols:                    tc.deprecated_symbols
		deprecated_modules:                    tc.deprecated_modules
		file_scope:                            fs
		cur_scope:                             fs
		scope_pool:                            []&Scope{}
		has_builtins:                          tc.has_builtins
		cur_module:                            tc.cur_module
		cur_file:                              tc.cur_file
		resolution_type_mode:                  tc.resolution_type_mode
		trust_checked_expr_types:              tc.trust_checked_expr_types
		errors:                                []TypeError{}
		notices:                               []TypeError{}
		resolved_call_names:                   tc.resolved_call_names
		resolved_call_set:                     tc.resolved_call_set
		statement_nodes:                       tc.statement_nodes
		direct_dependencies_by_fn:             direct_dependencies_by_fn
		method_values_by_fn:                   tc.method_values_by_fn
		cur_comptime_variant_loop_vars:        tc.cur_comptime_variant_loop_vars
		comptime_attr_narrow_state:            tc.comptime_attr_narrow_state
		expr_type_values:                      tc.expr_type_values
		expr_type_set:                         tc.expr_type_set
		lexical_smartcast_misses:              tc.lexical_smartcast_misses
		checking_nodes:                        tc.checking_nodes
		sparse_resolved_call_names:            map[int]string{}
		sparse_resolved_fn_values:             map[int]string{}
		sparse_statement_nodes:                map[int]bool{}
		sparse_expr_type_values:               map[int]Type{}
		sparse_checking_nodes:                 map[int]bool{}
		diagnose_unknown_calls:                tc.diagnose_unknown_calls
		reject_unlowered_map_mutation:         tc.reject_unlowered_map_mutation
		reject_unsupported_generics:           tc.reject_unsupported_generics
		checker_fixture_mode:                  tc.checker_fixture_mode
		is_test:                               tc.is_test
		// Reflection argument caches belong to semantic checking, not lookup or
		// transform views that may outlive the arena where those caches were made.
		checked_comptime_method_calls:         map[u64][]ComptimeMethodCallInstance{}
		comptime_method_calls_by_decl:         map[int]map[int]bool{}
		check_concrete_generic_bodies:         tc.check_concrete_generic_bodies
		check_program_lib_instances:           tc.check_program_lib_instances
		check_generic_bodies:                  tc.check_generic_bodies
		module_diagnostic_root:                tc.module_diagnostic_root
		autofree_mode:                         tc.autofree_mode
		no_main:                               tc.no_main
		nofloat:                               tc.nofloat
		warn_about_allocs:                     tc.warn_about_allocs
		warns_are_errors:                      tc.warns_are_errors
		explicit_warns_are_errors:             tc.explicit_warns_are_errors
		notes_are_errors:                      tc.notes_are_errors
		is_prod:                               tc.is_prod
		suppress_dump_output:                  tc.suppress_dump_output
		diagnostic_files:                      tc.diagnostic_files
		shadow_diagnostic_root:                tc.shadow_diagnostic_root
		shadow_explicit_roots:                 tc.shadow_explicit_roots
		shadow_dependency_roots:               tc.shadow_dependency_roots
		multiple_module_import_lines:          tc.multiple_module_import_lines
		source_texts_by_file:                  tc.source_texts_by_file
		source_texts_by_id:                    tc.source_texts_by_id
		malformed_const_source_states:         tc.malformed_const_source_states
		ct_update_pos:                         tc.ct_update_pos
		ct_update_indexed:                     tc.ct_update_indexed
		has_spawn_expr:                        tc.has_spawn_expr
		inactive_top_level_node_ids:           tc.inactive_top_level_node_ids
		selected_file_called_fns:              tc.selected_file_called_fns
		selected_file_worklist:                tc.selected_file_worklist
		defer_ierror_gating:                   tc.defer_ierror_gating
		pending_ierror_errors:                 []PendingIerrorError{}
		top_level_idx:                         tc.top_level_idx
		check_decl_indexes:                    tc.check_decl_indexes
		top_level_idx_nodes_len:               tc.top_level_idx_nodes_len
		synthetic_top_level_type_ids:          tc.synthetic_top_level_type_ids
		direct_parent_ids:                     tc.direct_parent_ids
		rewritten_parent_ids:                  tc.rewritten_parent_ids
		value_used_nodes:                      tc.value_used_nodes
		direct_parent_index_trusted:           tc.direct_parent_index_trusted
		preflight_node_ids:                    tc.preflight_node_ids
		preflight_index_nodes_len:             tc.preflight_index_nodes_len
		has_goto_nodes:                        tc.has_goto_nodes
		comptime_methods_scan_ast:             tc.comptime_methods_scan_ast
		comptime_methods_scan_len:             tc.comptime_methods_scan_len
		comptime_methods_loop_found:           tc.comptime_methods_loop_found
		declaration_attribute_kinds:           tc.declaration_attribute_kinds
		declaration_attributes:                tc.declaration_attributes
		attribute_schemas:                     tc.attribute_schemas
		type_declaration_ids:                  tc.type_declaration_ids
		first_type_declaration_ids:            tc.first_type_declaration_ids
		strings_builder_bindings:              tc.strings_builder_bindings
		static_associated_fn_keys:             tc.static_associated_fn_keys
		static_associated_method_names:        tc.static_associated_method_names
		static_associated_signature_count:     tc.static_associated_signature_count
		transform_static_method_names:         tc.transform_static_method_names
		transform_static_method_count:         tc.transform_static_method_count
		declaration_param_mutability:          tc.declaration_param_mutability
		strict_map_index_files:                tc.strict_map_index_files
		fn_decl_short_name_ids:                tc.fn_decl_short_name_ids
		recursive_str_fn_decl_ids:             tc.recursive_str_fn_decl_ids
		recursive_str_fn_decl_index_size:      tc.recursive_str_fn_decl_index_size
		recursive_str_fn_decl_index_complete:  tc.recursive_str_fn_decl_index_complete
		file_bare_fn_names:                    tc.file_bare_fn_names
		file_import_alias_paths:               tc.file_import_alias_paths
		file_import_suffix_paths:              tc.file_import_suffix_paths
		struct_embed_receivers:                tc.struct_embed_receivers
		enclosing_generic_params_by_node:      tc.enclosing_generic_params_by_node
		enclosing_generic_param_masks:         tc.enclosing_generic_param_masks
		expected_expr_id:                      -1
		expected_expr_type:                    builtin_void_type
		smartcasts:                            tc.smartcasts
		ownership:                             tc.ownership
		callback_param_flows:                  tc.callback_param_flows
		selfhost:                              tc.selfhost
		fn_context:                            new_function_check_context()
		resolution_type_views:                 &ResolutionTypeViewCache{
			by_file: map[string]&TypeChecker{}
		}
		visible_mutation_cache:                tc.visible_mutation_cache
		storage_query_probe:                   tc.storage_query_probe
		storage_query_probe_scopes:            tc.storage_query_probe_scopes
		type_interner:                         tc.type_interner
		symbols:                               tc.symbols
	}
}

// reset_codegen_smartcasts discards function-local bindings before backend type queries.
// A scoped transform can leave even an empty map in its disposable arena.
pub fn (mut tc TypeChecker) reset_codegen_smartcasts() {
	tc.smartcasts = map[string]Type{}
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
	// Dynamic smartcasts are function-checking state. Cgen only needs the
	// immutable lexical smartcast metadata, and may run return-analysis queries
	// that temporarily move this map, so every worker must own an empty binding.
	forked.smartcasts = map[string]Type{}
	// These sparse transform results are immutable by cgen. Share their backing
	// maps just like the node-indexed semantic arrays in fork_program_view.
	unsafe {
		forked.sparse_resolved_call_names = tc.sparse_resolved_call_names
		forked.sparse_statement_nodes = tc.sparse_statement_nodes
		forked.sparse_expr_type_values = tc.sparse_expr_type_values
		forked.sparse_checking_nodes = tc.sparse_checking_nodes
	}
	// Fn-value names are the one sparse store the master may still write to
	// while cgen workers read (a type query on the declaration task can record
	// a resolution), so each worker reads a private snapshot of its few entries.
	forked.sparse_resolved_fn_values = tc.fn_value_snapshot()
	forked.rebuild_sparse_fn_value_filter()
	forked.visible_mutation_cache = unsafe { nil }
	forked.inherit_ownership_codegen_metadata_from(tc)
	forked.set_fresh_type_cache_based_on(tc, tc.type_cache_parse_enabled())
	if !isnil(tc.qualify_name_cache) {
		// Never share the memo across threads; each fork owns a private one.
		forked.qualify_name_cache = &QualifyNameCache{}
	}
	return forked
}

// fork_type_parse_view creates a lookup-only view in an explicit source
// context. It shares the compilation's immutable indexes, interner, and
// synchronous memoization cache, but none of the caller's function state.
fn (tc &TypeChecker) fork_type_parse_view(file string, module_name string) &TypeChecker {
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
fn (tc &TypeChecker) fork_smartcast_query_view() &TypeChecker {
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

// overlay_range_enabled is read once: getenv takes the process-wide environment
// lock, which every forked transform batch would otherwise contend on.
const overlay_range_enabled = os.getenv('V3_NO_OVERLAY_RANGE') == ''

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
	forked.transform_signature_keys_borrowed = true
	forked.transform_struct_maps_shared = true
	// Scoped reachability uses a transform fork too. Retain the master's body
	// policy so it does not eagerly collect the library bodies the check skipped.
	forked.skips_library_bodies = tc.skips_library_bodies
	forked.library_files = tc.library_files
	forked.reachable_library_fns = tc.reachable_library_fns
	// Visible-mutation analysis only runs during checking. Do not allocate its
	// three private maps for the many short-lived transform forks.
	forked.visible_mutation_cache = unsafe { nil }
	// Completed root queries are private to this immutable graph view.
	forked.global_store_cache = &FnGlobalStoreCache{}
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
		base_node_count: if overlay_range_enabled { ast.nodes.len } else { -1 }
	}
	// Source-node fn-value resolutions live only in the sparse map. The fork
	// reads a private snapshot of the parent's effective entries; its own
	// writes and tombstones go to fork_fn_value_writes (see
	// set_resolved_fn_value / clear_resolved_fn_value), which
	// Transformer.merge_worker / absorb_scoped_batch replay into the parent
	// through apply_forked_fn_value.
	forked.sparse_resolved_fn_values = tc.fn_value_snapshot()
	forked.rebuild_sparse_fn_value_filter()
	forked.fork_fn_value_writes = map[int]string{}
	// Transform helpers allocate inside disposable arenas. A shared interner
	// would let one helper publish map/array storage owned by its arena and leave
	// other helpers with dangling storage when that arena is released. Each
	// helper therefore gets a private canonical table; semantic Type values are
	// still compatible across tables, while TypeIds stay local to its cache.
	forked.type_interner = new_type_interner()
	forked.symbols = new_symbol_interner()
	// The fork's own dependency map only holds calls it resolves itself, under its
	// private symbol ids. Transitive call-graph queries read the post-check graph
	// of the checker that owns it instead; nested forks keep that same view.
	forked.post_check_dependencies = if !isnil(tc.post_check_dependencies) {
		tc.post_check_dependencies
	} else {
		&DependencyGraphView{
			by_fn:        tc.direct_dependencies_by_fn
			names:        if isnil(tc.symbols) { []string{} } else { tc.symbols.names }
			declarations: tc.visible_mutation_cache
		}
	}
	forked.type_cache = new_type_cache_with_base(if tc.type_cache != unsafe { nil } {
		tc.type_cache.parse_enabled
	} else {
		false
	}, if tc.type_cache != unsafe { nil } {
		// When the master froze its warm cache behind an overlay (see
		// freeze_type_cache_for_forks), every fork shares that frozen cache as
		// its read-only base instead of re-deriving each memoized type.
		tc.type_cache.base
	} else {
		&TypeCache(unsafe { nil })
	})
	return forked
}

// ensure_private_transform_signatures detaches the signature tables before a
// transform worker writes them. The worker's private maps are merged while its
// disposable arena is still alive.
pub fn (mut tc TypeChecker) ensure_private_transform_signatures() {
	if !isnil(tc.global_store_cache) {
		mut cache := tc.global_store_cache
		cache.values.clear()
	}
	if tc.transform_signature_maps_shared && tc.transform_signature_keys_borrowed {
		// The fork's private maps are discarded with its batch, before the parent's
		// keys, and deleting from them never frees a borrowed key. Cloning every
		// name of these whole-program tables dominated each detach.
		unsafe {
			map_clone_sharing_keys(&tc.fn_ret_types, &tc.fn_ret_types)
			map_clone_sharing_keys(&tc.fn_param_types, &tc.fn_param_types)
			map_clone_sharing_keys(&tc.receiver_method_suffix_index, &tc.receiver_method_suffix_index)
			map_clone_sharing_keys(&tc.receiver_method_short_names, &tc.receiver_method_short_names)
			map_clone_sharing_keys(&tc.collection_method_short_names, &tc.collection_method_short_names)
			map_clone_sharing_keys(&tc.fn_variadic, &tc.fn_variadic)
			map_clone_sharing_keys(&tc.specialized_generic_fns, &tc.specialized_generic_fns)
			map_clone_sharing_keys(&tc.fn_type_modules, &tc.fn_type_modules)
			map_clone_sharing_keys(&tc.fn_type_files, &tc.fn_type_files)
		}
		tc.generic_receiver_method_index = clone_string_list_map(tc.generic_receiver_method_index)
		tc.transform_signature_maps_shared = false
		// Only the tables shared at fork time are known to outlive this fork.
		tc.transform_signature_keys_borrowed = false
	} else if tc.transform_signature_maps_shared {
		tc.fn_ret_types = tc.fn_ret_types.clone()
		tc.fn_param_types = tc.fn_param_types.clone()
		tc.receiver_method_suffix_index = tc.receiver_method_suffix_index.clone()
		tc.receiver_method_short_names = tc.receiver_method_short_names.clone()
		tc.collection_method_short_names = tc.collection_method_short_names.clone()
		tc.generic_receiver_method_index = clone_string_list_map(tc.generic_receiver_method_index)
		tc.fn_variadic = tc.fn_variadic.clone()
		tc.specialized_generic_fns = tc.specialized_generic_fns.clone()
		tc.fn_type_modules = tc.fn_type_modules.clone()
		tc.fn_type_files = tc.fn_type_files.clone()
		tc.transform_signature_maps_shared = false
	}
	tc.transform_signature_maps_changed = true
}

// clone_transform_metadata_owned detaches every transform-mutated metadata map
// and its string payloads from a disposable worker or stage arena.
pub fn (mut tc TypeChecker) clone_transform_metadata_owned() {
	tc.runtime_type_indexes = tc.runtime_type_indexes.clone()
	tc.fn_ret_type_texts = tc.fn_ret_type_texts.clone_owned()
	tc.fn_param_type_texts = tc.fn_param_type_texts.clone_owned()
	tc.fn_generic_params = clone_owned_string_id_list_map(tc.fn_generic_params)
	tc.mut_receiver_methods = clone_owned_string_bool_map(tc.mut_receiver_methods)
	tc.c_variadic_fns = clone_owned_string_bool_map(tc.c_variadic_fns)
	tc.fn_implicit_veb_ctx = clone_owned_string_bool_map(tc.fn_implicit_veb_ctx)
	tc.promote_scoped_declaration_visibility()
	tc.fn_type_files = clone_owned_string_string_map(tc.fn_type_files)
	tc.fn_type_modules = clone_owned_string_string_map(tc.fn_type_modules)
	tc.structs = clone_owned_source_type_struct_field_map(tc.structs)
	tc.struct_modules = clone_owned_source_type_string_map(tc.struct_modules)
	tc.struct_files = clone_owned_source_type_string_map(tc.struct_files)
	tc.soa_structs = tc.soa_structs.clone()
	tc.declared_type_scope_keys = tc.declared_type_scope_keys.clone()
	tc.struct_error_embeds_shadow_builtin = tc.struct_error_embeds_shadow_builtin.clone()
	tc.struct_generic_params = clone_owned_id_list_map(tc.struct_generic_params)
	tc.struct_implements = tc.struct_implements.clone_owned()
	tc.struct_shared_fields = clone_owned_type_owner_field_flags(tc.struct_shared_fields)
	tc.struct_shared_element_fields = clone_owned_type_owner_field_flags(tc.struct_shared_element_fields)
	tc.struct_field_c_abi_fns = clone_owned_type_owner_c_abi_fields(tc.struct_field_c_abi_fns)
	tc.unions = tc.unions.clone()
	tc.params_structs = tc.params_structs.clone()
	tc.c_typedef_structs = tc.c_typedef_structs.clone()
	tc.type_alias_generic_params = clone_owned_id_list_map(tc.type_alias_generic_params)
	tc.type_alias_c_abi_fns = clone_owned_source_type_string_map(tc.type_alias_c_abi_fns)
	tc.sum_types = tc.sum_types.clone_owned()
	tc.sum_generic_params = clone_owned_id_list_map(tc.sum_generic_params)
	tc.enum_names = tc.enum_names.clone()
	tc.enum_fields = clone_owned_source_type_string_list_map(tc.enum_fields)
	tc.flag_enums = tc.flag_enums.clone()
	tc.interface_names = tc.interface_names.clone()
	tc.interface_generic_params = clone_owned_id_list_map(tc.interface_generic_params)
	tc.interface_fields = clone_owned_source_type_struct_field_map(tc.interface_fields)
	tc.interface_embeds = tc.interface_embeds.clone_owned()
	tc.interface_abstract_methods = clone_owned_source_type_string_list_map(tc.interface_abstract_methods)
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
	tc.lexical_parent_memo = unsafe { nil }
	// The import index can hold keys allocated in the same disposable stage arena.
	tc.selective_import_index = &SelectiveImportIndex{}
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
		cache.borrowed_projection_clone_copies = unsafe { nil }
		cache.parse_enabled = parse_enabled
		cache.checked_fn_name_node = -1
		cache.checked_fn_name_source = ''
		cache.checked_fn_name_module = ''
		cache.checked_fn_name_value = ''
		cache.parse_hits = 0
		cache.parse_misses = 0
		cache.c_hits = 0
		cache.c_misses = 0
		cache.parse_entries.clear()
		cache.parse_context_valid = false
		cache.parse_last_valid = false
		cache.clear_c_type_entries()
		cache.clear_struct_field_entries()
		cache.allocation_layouts.clear()
		cache.struct_field_shared = map[u32]&StructFieldTypeEntries{}
		cache.struct_field_complete = map[u32]bool{}
		cache.struct_field_recent_states = [256]i8{}
		cache.recv_pattern_entries.clear()
		cache.recv_pattern_misses.clear()
		cache.clear_ierror_compat_entries()
		cache.interface_impl_entries.clear()
		cache.clear_source_error_embed_entries()
		cache.ierror_impl_names = flat.TypeTextList{}
		cache.ierror_impl_names_set = false
		cache.short_type_name_index.clear()
		cache.short_type_name_index_built = false
		cache.local_fn_decl_index.clear()
		cache.local_fn_decl_indexed_len = 0
		cache.local_fn_decl_last_module = ''
		cache.tail_decl_ids = []i32{}
		cache.tail_decl_start = -1
		cache.tail_decl_end = -1
		return
	}
	tc.type_cache = new_type_cache(parse_enabled)
}

// reset_type_interners replaces semantic interners whose backing storage may
// have grown inside a disposable compiler-stage arena.
pub fn (mut tc TypeChecker) reset_type_interners() {
	if !isnil(tc.type_cache) {
		tc.type_cache.parsed_scalars = unsafe { nil }
		// Numeric identities start over in the replacement interner.
		tc.type_cache.clear_c_type_entries()
	}
	tc.type_interner = new_type_interner()
	tc.symbols = new_symbol_interner()
}

// set_fresh_type_cache_based_on attaches a new empty TypeCache that falls back
// read-only to `src`'s frozen base cache (see freeze_type_cache_for_forks), so
// parallel-cgen workers start with every type memoized by the check/transform
// phases instead of re-deriving them from a cold cache.
pub fn (mut tc TypeChecker) set_fresh_type_cache_based_on(src &TypeChecker, parse_enabled bool) {
	tc.lexical_parent_memo = unsafe { nil }
	tc.selective_import_index = &SelectiveImportIndex{}
	base := if isnil(src.type_cache) {
		&TypeCache(unsafe { nil })
	} else if !isnil(src.type_cache.base) {
		src.type_cache.base
	} else {
		src.type_cache
	}
	tc.type_cache = new_type_cache_with_base(parse_enabled, base)
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

// clear_field_lookup_cache invalidates field types after declaration metadata changes.
pub fn (tc &TypeChecker) clear_field_lookup_cache() {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	cache.clear_struct_field_entries()
	cache.allocation_layouts.clear()
	cache.struct_field_shared = map[u32]&StructFieldTypeEntries{}
	cache.struct_field_complete = map[u32]bool{}
	cache.struct_field_recent_states = [256]i8{}
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
			tc.type_cache.free_struct_field_entries()
			tc.type_cache.recv_pattern_entries.free()
			tc.type_cache.recv_pattern_misses.free()
			tc.type_cache.free_ierror_compat_entries()
			tc.type_cache.interface_impl_entries.free()
			tc.type_cache.free_source_error_embed_entries()
		}
	}
	tc.type_cache = new_type_cache(parse_enabled)
}

// reset_node_caches updates reset node caches state for types.
fn (mut tc TypeChecker) reset_node_caches(n int) {
	tc.reset_sparse_fn_values()
	for group in 0 .. 2 {
		tc.reset_node_cache_group(n, group)
	}
}

// reset_sparse_fn_values drops the resolved function-value names of a
// previous AST. The dense cache this map replaced was recreated by every
// node-cache reset; without this, a checker collected again would resolve
// recycled node ids to stale targets. Called on the collecting thread before
// both the serial and the parallel cache initialization.
fn (mut tc TypeChecker) reset_sparse_fn_values() {
	tc.sparse_resolved_fn_values.clear()
	tc.sparse_fn_value_filter_ready = false
	tc.fork_fn_value_writes.clear()
}

// Independent arrays can be initialized on separate persistent worker arenas.
fn (mut tc TypeChecker) reset_node_cache_group(n int, group int) {
	if group == 0 {
		if !isnil(tc.type_cache) {
			tc.type_cache.indexed_types = unsafe { nil }
		}
		tc.resolved_call_names = new_zeroed_name_cache(n)
		tc.resolved_call_set = []bool{len: n}
		return
	}
	tc.statement_nodes = []bool{len: n}
	// No init fill: every read of expr_type_values is guarded by expr_type_set,
	// so unset slots are never returned, and skipping the ~1M-element fill loop
	// keeps this a plain zeroed allocation.
	tc.expr_type_values = unsafe { []&Type{len: n} }
	tc.expr_type_set = []bool{len: n}
	tc.lexical_smartcast_misses = []bool{len: n}
	tc.checking_nodes = []bool{len: n}
	tc.parallel_check_sparse = false
}

fn new_zeroed_name_cache(n int) []&CachedName {
	mut values := []&CachedName{cap: n}
	if n > 0 {
		// Cache reads are guarded by set bits. Match the zero representation
		// used during transform growth without synthesizing per-element stores.
		unsafe {
			values.grow_len(n)
			vmemset(values.data, 0, int(n) * int(sizeof(&CachedName)))
		}
	}
	return values
}

fn (mut tc TypeChecker) init_direct_parent_index(a &flat.FlatAst) {
	tc.invalidate_lexical_parent_memo()
	tc.direct_parent_ids = flat.empty_node_ids(a.nodes.len)
	tc.rewritten_parent_ids = []flat.NodeId{}
	tc.value_used_nodes = []bool{len: a.nodes.len}
	tc.fn_check_costs = if tc.building_v_fast { []i32{len: a.nodes.len} } else { []i32{} }
	tc.declaration_attributes = map[int][]string{}
	tc.has_comptime_functions = false
	tc.comptime_const_values = map[string]ComptimeConstValue{}
	tc.declaration_attribute_kinds = map[int]u32{}
	tc.insert_include_dirs_by_file = map[string][]string{}
	tc.translated_files = map[string]bool{}
	tc.generated_files = map[string]bool{}
	tc.has_globals_files = map[string]bool{}
	tc.strings_builder_candidates = []i32{cap: 1024}
	tc.synthetic_top_level_type_ids = []i32{cap: 2048}
	tc.has_goto_nodes = false
	tc.preflight_node_ids = []i32{}
	tc.preflight_index_nodes_len = -1
	tc.comptime_methods_scan_ast = unsafe { nil }
}

struct DirectParentEdge {
	child      flat.NodeId
	parent     flat.NodeId
	value_used bool
}

struct DirectParentChunk {
mut:
	external_edges     []DirectParentEdge
	preflight_node_ids []i32
	metadata_node_ids  []i32
	synthetic_type_ids []i32
	has_goto_nodes     bool
}

fn (mut tc TypeChecker) fill_direct_parent_edges(a &flat.FlatAst) {
	chunk := tc.fill_direct_parent_edges_range(a, 0, a.nodes.len)
	tc.merge_direct_parent_chunk(chunk)
	// All parent edges are ready; replay the metadata found in the same source-order walk.
	for idx in chunk.metadata_node_ids {
		tc.collect_direct_parent_node_metadata(a, idx, a.nodes[idx])
	}
	tc.preflight_index_nodes_len = a.nodes.len
}

// Ranges start immediately after a declaration, where the running cost resets.
@[direct_array_access]
fn (mut tc TypeChecker) fill_direct_parent_edges_range(a &flat.FlatAst, start int, end int) DirectParentChunk {
	mut chunk := DirectParentChunk{}
	mut fn_cost := 0
	profile_fn_costs := tc.fn_check_costs.len > 0
	for parent_idx in start .. end {
		node := a.nodes[parent_idx]
		if node.kind in [.decl_assign, .directive] {
			chunk.metadata_node_ids << parent_idx
		}
		if node.kind in [.postfix, .for_in_stmt, .comptime_for, .break_stmt, .continue_stmt,
			.goto_stmt] {
			chunk.preflight_node_ids << parent_idx
		}
		if profile_fn_costs {
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
		}
		if node.kind == .goto_stmt {
			chunk.has_goto_nodes = true
		}
		if node.kind == .struct_decl
			&& (is_anonymous_aggregate_name(node.value) || node.value.contains('@local@')) {
			chunk.synthetic_type_ids << parent_idx
		}
		for child_idx in 0 .. node.children_count {
			child := a.child(&node, child_idx)
			idx := int(child)
			if idx < 0 || idx >= a.nodes.len {
				continue
			}
			value_used := node.kind !in [.expr_stmt, .block, .match_branch, .fn_decl, .comptime_for]
			if idx < start || idx >= end {
				// Each lane writes only its own child slots. Replay cross-range
				// references after joining, preserving the first parent in source order.
				chunk.external_edges << DirectParentEdge{child, flat.NodeId(parent_idx), value_used}
				continue
			}
			if value_used {
				tc.value_used_nodes[idx] = true
			}
			if tc.direct_parent_ids[idx] == flat.empty_node {
				tc.direct_parent_ids[idx] = flat.NodeId(parent_idx)
			}
		}
		if node.kind == .fn_decl && parent_idx < tc.fn_check_costs.len {
			tc.fn_check_costs[parent_idx] = fn_cost
		}
		if profile_fn_costs && node.kind in [.file, .module_decl, .struct_decl, .type_decl,
			.interface_decl, .enum_decl, .import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl] {
			fn_cost = 0
		}
	}
	return chunk
}

fn (mut tc TypeChecker) merge_direct_parent_chunk(chunk DirectParentChunk) {
	for edge in chunk.external_edges {
		old_parent := tc.direct_parent_ids[int(edge.child)]
		if old_parent == flat.empty_node || edge.parent < old_parent {
			tc.direct_parent_ids[int(edge.child)] = edge.parent
		}
		if edge.value_used {
			tc.value_used_nodes[int(edge.child)] = true
		}
	}
	tc.preflight_node_ids << chunk.preflight_node_ids
	tc.synthetic_top_level_type_ids << chunk.synthetic_type_ids
	tc.has_goto_nodes = tc.has_goto_nodes || chunk.has_goto_nodes
}

// preflight_nodes reuses the parsed-tree scan while its parent index is valid.
// Standalone checker clients and rewritten ASTs retain the full-scan fallback.
fn (tc &TypeChecker) preflight_nodes(kind flat.NodeKind) []i32 {
	mut ids := []i32{}
	if tc.direct_parent_index_trusted && tc.preflight_index_nodes_len == tc.a.nodes.len {
		for id in tc.preflight_node_ids {
			if tc.a.nodes[id].kind == kind {
				ids << id
			}
		}
	} else {
		for id, node in tc.a.nodes {
			if node.kind == kind {
				ids << id
			}
		}
	}
	return ids
}

// comptime_methods_loop_exists reports whether the AST has a `$for m in T.methods`
// loop. Without one, no call can reach a reflected method call. Nodes appended
// after an earlier answer are scanned on the next query.
@[direct_array_access]
fn (mut tc TypeChecker) comptime_methods_loop_exists() bool {
	if tc.comptime_methods_scan_ast != voidptr(tc.a) {
		tc.comptime_methods_scan_ast = voidptr(tc.a)
		tc.comptime_methods_scan_len = 0
		tc.comptime_methods_loop_found = false
	}
	n := tc.a.nodes.len
	if tc.comptime_methods_loop_found || tc.comptime_methods_scan_len >= n {
		return tc.comptime_methods_loop_found
	}
	mut found := false
	if tc.comptime_methods_scan_len == 0 && tc.direct_parent_index_trusted
		&& tc.preflight_index_nodes_len == n {
		for id in tc.preflight_node_ids {
			node := tc.a.nodes[id]
			if node.kind == .comptime_for && node.value.ends_with('|methods') {
				found = true
				break
			}
		}
	} else {
		for id in tc.comptime_methods_scan_len .. n {
			node := tc.a.nodes[id]
			if node.kind == .comptime_for && node.value.ends_with('|methods') {
				found = true
				break
			}
		}
	}
	tc.comptime_methods_scan_len = n
	tc.comptime_methods_loop_found = found
	return found
}

fn (mut tc TypeChecker) collect_direct_parent_metadata(a &flat.FlatAst) {
	for parent_idx, node in a.nodes {
		if node.kind in [.decl_assign, .directive] {
			tc.collect_direct_parent_node_metadata(a, parent_idx, node)
		}
	}
}

fn (mut tc TypeChecker) collect_direct_parent_node_metadata(a &flat.FlatAst, parent_idx int, node flat.Node) {
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
				if node.typ != 0 {
					tc.declaration_attribute_kinds[decl_id] = node.typ
				}
				decl := a.nodes[decl_id]
				if decl.kind == .fn_decl && tc.declaration_has_attribute(flat.NodeId(decl_id), 'comptime') {
					tc.has_comptime_functions = true
				}
				if decl.kind == .module_decl {
					if source_file := a.source_files[decl.pos.id] {
						tc.collect_module_attributes(node, source_file.name)
					}
				}
			}
		} else if node.value == 'flag' && node.pos.is_valid() {
			if source_file := a.source_files[node.pos.id] {
				if raw_dir := checker_flag_include_dir(node.typ_text()) {
					resolved := tc.resolve_insert_path(raw_dir, source_file.name)
					if resolved !in tc.insert_include_dirs_by_file[source_file.name] {
						tc.insert_include_dirs_by_file[source_file.name] << resolved
					}
				}
			}
		}
	}
}

fn (mut tc TypeChecker) build_direct_parent_index(a &flat.FlatAst) {
	tc.init_direct_parent_index(a)
	tc.fill_direct_parent_edges(a)
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
	tc.invalidate_lexical_parent_memo()
	tc.direct_parent_index_trusted = false
}

// refresh_rewritten_parent_index rebuilds the node-parent edges after transform
// without resetting declaration metadata collected during semantic checking.
// Rewritten trees can contain hundreds of thousands of new nodes; leaving those
// nodes outside direct_parent_ids makes every parent query scan the whole AST.
pub fn (mut tc TypeChecker) refresh_rewritten_parent_index(a &flat.FlatAst) {
	tc.invalidate_lexical_parent_memo()
	// Lexical smartcasts deliberately apply only to parsed source nodes, so keep
	// direct_parent_ids at its original length and index appended nodes separately.
	tc.rewritten_parent_ids = flat.empty_node_ids(a.nodes.len)
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

// index_rewritten_parents_after adds to that index the nodes appended from
// `start` on. A clone of a generic function is a new subtree, so its edges are
// all among those nodes.
pub fn (mut tc TypeChecker) index_rewritten_parents_after(start int) {
	tc.invalidate_lexical_parent_memo()
	n := tc.a.nodes.len
	for tc.rewritten_parent_ids.len < n {
		tc.rewritten_parent_ids << flat.empty_node
	}
	for parent_idx in start .. n {
		node := tc.a.nodes[parent_idx]
		for child_idx in 0 .. node.children_count {
			idx := int(tc.a.child(&node, child_idx))
			if idx >= tc.direct_parent_ids.len && idx < n
				&& tc.rewritten_parent_ids[idx] == flat.empty_node {
				tc.rewritten_parent_ids[idx] = flat.NodeId(parent_idx)
			}
		}
	}
	tc.direct_parent_index_trusted = false
}

fn (mut tc TypeChecker) build_type_declaration_index(a &flat.FlatAst) {
	tc.type_declaration_ids = map[u32][]int{}
	tc.first_type_declaration_ids = map[u32]int{}
	tc.index_type_declarations(a, tc.top_level_idx, '')
}

// index_type_declarations adds the type declarations of `entries`, whose code
// starts in module `start_module`, to the indexes of type declarations.
fn (mut tc TypeChecker) index_type_declarations(a &flat.FlatAst, entries []i32, start_module string) {
	// Module names enter the spelling table only as qualifiers of declaration names.
	mut module_id := flat.source_type_id(start_module)
	mut conflict_module_id := u32(0)
	for index in entries {
		node := a.nodes[index]
		if node.kind == .file {
			conflict_module_id = 0
			continue
		}
		if node.kind == .module_decl {
			module_id = node.source_value_id()
			conflict_module_id = module_id
			continue
		}
		if node.kind in [.struct_decl, .type_decl, .interface_decl, .enum_decl] {
			qualified_id := qualify_decl_name_in_module_id(node.source_value_id(), conflict_module_id)
			if qualified_id !in tc.first_type_declaration_ids {
				tc.first_type_declaration_ids[qualified_id] = index
			}
		}
		if node.kind !in [.struct_decl, .type_decl] {
			continue
		}
		short_id := flat.source_type_syntax(node.source_value_id()).unqualified()
		tc.type_declaration_ids[short_id] << index
		qualified_id := qualify_decl_name_in_module_id(node.source_value_id(), module_id)
		if qualified_id != short_id {
			tc.type_declaration_ids[qualified_id] << index
		}
	}
}

fn strings_builder_binding_key(fn_index int, name string) string {
	return '${fn_index}\x00${name}'
}

fn (mut tc TypeChecker) build_fn_declaration_indexes(a &flat.FlatAst) {
	tc.build_declaration_param_mutability_index(a)
	tc.build_fn_name_indexes(a)
	tc.build_file_declaration_indexes(a)
}

// build_declaration_param_mutability_index records the parameter mutability of
// every interface method, C function and V function declaration. It and the two
// builders below each own their maps, so collection runs them on separate lanes.
fn (mut tc TypeChecker) build_declaration_param_mutability_index(a &flat.FlatAst) {
	tc.declaration_param_mutability = map[string][]bool{}
	tc.index_declaration_param_mutability(a, tc.top_level_idx, '')
}

// declaration_param_mutability_for returns which parameters of the declaration `key`
// are `mut`, with the receiver first for a method, or an empty array when unknown.
// The returned array is shared with the index; clone it before changing it.
pub fn (tc &TypeChecker) declaration_param_mutability_for(key string) []bool {
	return tc.declaration_param_mutability[key] or { []bool{} }
}

fn (mut tc TypeChecker) index_declaration_param_mutability(a &flat.FlatAst, entries []i32, start_module string) {
	mut module_name := start_module
	for index in entries {
		node := a.nodes[index]
		if node.kind == .module_decl {
			module_name = node.value
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
	}
}

// build_fn_name_indexes records the short names, per-file bare names and
// static/associated keys of V and interop function declarations.
fn (mut tc TypeChecker) build_fn_name_indexes(a &flat.FlatAst) {
	tc.static_associated_fn_keys = map[string]bool{}
	tc.fn_decl_short_name_ids = map[string]int{}
	tc.recursive_str_fn_decl_ids = map[string]int{}
	tc.recursive_str_fn_decl_index_complete = true
	tc.file_bare_fn_names = map[string]bool{}
	tc.index_fn_names(a, tc.top_level_idx, '')
}

fn (mut tc TypeChecker) index_fn_names(a &flat.FlatAst, entries []i32, start_module string) {
	mut module_name := start_module
	mut recursive_str_module := start_module
	mut recursive_str_file := ''
	mut recursive_str_module_known := false
	for index in entries {
		node := a.nodes[index]
		if node.kind == .file {
			recursive_str_file = node.value
			recursive_str_module_known = node.value in tc.file_modules
			recursive_str_module = tc.file_modules[node.value] or { '' }
		}
		if node.kind == .module_decl {
			module_name = node.value
			recursive_str_module = node.value
			recursive_str_module_known = true
			continue
		}
		if node.kind !in [.fn_decl, .c_fn_decl] {
			continue
		}
		if node.kind == .fn_decl {
			short_name := node.value.all_after_last('.')
			if short_name !in tc.fn_decl_short_name_ids {
				tc.fn_decl_short_name_ids[short_name] = index
			}
			tc.file_bare_fn_names['${node.pos.id}\x00${node.value}'] = true
			tc.index_recursive_str_fn_name(node, index, recursive_str_file, recursive_str_module,
				recursive_str_module_known)
		}
		decl_name := if node.kind == .c_fn_decl && !node.value.starts_with('C.') {
			'C.${node.value}'
		} else {
			node.value
		}
		if decl_name.contains('.') || node.is_static_type_method() {
			qname := checker_qualified_fn_name(module_name, decl_name)
			is_static := node.is_static_type_method() || node.children_count == 0
				|| a.child_node(&node, 0).kind != .param || a.child_node(&node, 0).op != .dot
			// A declaration's canonical key takes priority over another module's bare alias.
			if qname == decl_name || decl_name !in tc.static_associated_fn_keys {
				tc.static_associated_fn_keys[decl_name] = is_static
			}
			if qname !in tc.static_associated_fn_keys {
				tc.static_associated_fn_keys[qname] = is_static
			}
		}
	}
	tc.recursive_str_fn_decl_index_size = tc.top_level_idx.len
}

fn (mut tc TypeChecker) index_recursive_str_fn_name(node flat.Node, index int, file string, module_name string, module_known bool) {
	if node.value !in tc.recursive_str_fn_decl_ids {
		tc.recursive_str_fn_decl_ids[node.value] = index
	}
	main_alias := 'main.${node.value}'
	if main_alias !in tc.recursive_str_fn_decl_ids {
		tc.recursive_str_fn_decl_ids[main_alias] = index
	}
	source_file := tc.a.source_files[node.pos.id] or {
		tc.recursive_str_fn_decl_index_complete = false
		return
	}
	if source_file.name != file || !module_known {
		tc.recursive_str_fn_decl_index_complete = false
		return
	}
	qname := checker_qualified_fn_name(module_name, node.value)
	if qname !in tc.recursive_str_fn_decl_ids {
		tc.recursive_str_fn_decl_ids[qname] = index
	}
}

// build_file_declaration_indexes records per-file module attributes and import
// paths, and resolves the strings.Builder bindings found while parsing.
fn (mut tc TypeChecker) build_file_declaration_indexes(a &flat.FlatAst) {
	tc.strings_builder_bindings = map[string]bool{}
	tc.strict_map_index_files = map[string]bool{}
	tc.file_import_alias_paths = map[string]string{}
	tc.file_import_suffix_paths = map[string]string{}
	tc.index_file_declarations(a, tc.top_level_idx)
}

fn (mut tc TypeChecker) index_file_declarations(a &flat.FlatAst, entries []i32) {
	mut file_name := ''
	for index in entries {
		node := a.nodes[index]
		if node.kind == .file {
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			if tc.declaration_has_attribute(flat.NodeId(index), 'strict_map_index') {
				tc.strict_map_index_files[file_name] = true
			}
			continue
		}
		if node.kind == .import_decl {
			path := tc.import_module_path_text(node)
			if node.typ != 0 {
				tc.file_import_alias_paths['${file_name}\x00${node.typ_text()}'] = path
			}
			if path.contains('.') {
				suffix_key := '${file_name}\x00${path.all_after_last('.')}'
				if suffix_key !in tc.file_import_suffix_paths {
					tc.file_import_suffix_paths[suffix_key] = path
				}
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
	tc.strings_builder_candidates = []i32{}
}

// has_fn_decl_short_name reports whether collection indexed a function
// declaration with the given unqualified name.
pub fn (tc &TypeChecker) has_fn_decl_short_name(name string) bool {
	return name in tc.fn_decl_short_name_ids
}

fn (mut tc TypeChecker) build_enclosing_generic_param_index(a &flat.FlatAst) {
	tc.enclosing_generic_params_by_node = map[int][]u32{}
	tc.enclosing_generic_param_masks = []u32{len: a.nodes.len}
	tc.index_enclosing_generic_params(a, tc.top_level_idx)
}

fn (mut tc TypeChecker) index_enclosing_generic_params(a &flat.FlatAst, entries []i32) {
	mut previous_top_level_idx := -1
	for idx in entries {
		node := a.nodes[idx]
		if node.kind != .fn_decl || node.generic_params().len == 0 {
			previous_top_level_idx = idx
			continue
		}
		params := generic_param_ids(node.generic_params())
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
	tc.cache_fn_generic_params_of(a, tc.top_level_idx)
}

fn (mut tc TypeChecker) cache_fn_generic_params_of(a &flat.FlatAst, entries []i32) {
	mut previous_top_level_idx := -1
	for idx in entries {
		node := a.nodes[idx]
		if node.kind != .fn_decl {
			previous_top_level_idx = idx
			continue
		}
		params := tc.infer_decl_generic_param_ids(node)
		// Keep non-generic declarations too, so call validation can reuse the miss.
		tc.enclosing_generic_params_by_node[idx] = params
		if params.len > 0 {
			tc.fill_enclosing_generic_param_mask(previous_top_level_idx + 1, idx + 1, params)
		}
		previous_top_level_idx = idx
	}
}

fn (mut tc TypeChecker) fill_enclosing_generic_param_mask(start int, end int, params []u32) {
	mut mask := u32(0)
	for param in params {
		mask |= generic_param_letter_mask(param)
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
	if n <= tc.resolved_call_names.len && n <= tc.statement_nodes.len
		&& n <= tc.expr_type_values.len && n <= tc.checking_nodes.len {
		return
	}
	extend_name_cache(mut tc.resolved_call_names, n)
	extend_bool_cache(mut tc.resolved_call_set, n)
	extend_bool_cache(mut tc.statement_nodes, n)
	extend_type_cache(mut tc.expr_type_values, n)
	extend_bool_cache(mut tc.expr_type_set, n)
	extend_bool_cache(mut tc.checking_nodes, n)
}

// reserve_transform_node_caches reserves node-indexed semantic storage before
// a scoped transform starts, keeping the escaping slabs in the compilation arena.
pub fn (mut tc TypeChecker) reserve_transform_node_caches(n int) {
	reserve_name_cache(mut tc.resolved_call_names, n)
	reserve_bool_cache(mut tc.resolved_call_set, n)
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
		tc.widen_mixed_integer_expr_types()
		return
	}
	target_cap := if capacity > n { capacity } else { n }
	tc.reserve_transform_node_caches(target_cap)
	tc.parallel_check_sparse = false
	tc.extend_node_caches(n)
	for idx, name in tc.sparse_resolved_call_names {
		if idx >= 0 && idx < n {
			tc.resolved_call_names[idx] = cached_name(name)
			tc.resolved_call_set[idx] = true
		}
	}
	for idx, is_statement in tc.sparse_statement_nodes {
		if is_statement && idx >= 0 && idx < n {
			tc.statement_nodes[idx] = true
		}
	}
	for idx, typ in tc.sparse_expr_type_values {
		if idx >= 0 && idx < n {
			tc.invalidate_checked_type_id(idx)
			tc.expr_type_values[idx] = tc.intern_type_reference(typ)
			tc.expr_type_set[idx] = true
		}
	}
	tc.sparse_resolved_call_names = map[int]string{}
	tc.sparse_statement_nodes = map[int]bool{}
	tc.sparse_expr_type_values = map[int]Type{}
	tc.sparse_checking_nodes = map[int]bool{}
	tc.widen_mixed_integer_expr_types()
}

// widen_mixed_integer_expr_types fixes the recorded type of every arithmetic node
// that mixes a 128-bit type with a narrower one. During checking a worker could not
// resolve operands outside its own range, so those nodes were recorded with the
// narrower operand's type and every reader of the record then cut the value to 64
// bits. Checking has finished by the time this runs, so the operand types are final.
fn (mut tc TypeChecker) widen_mixed_integer_expr_types() {
	for idx in 0 .. tc.expr_type_set.len {
		if !tc.expr_type_set[idx] || idx >= tc.a.nodes.len {
			continue
		}
		if tc.a.nodes[idx].kind != .infix {
			continue
		}
		typ := tc.expr_type_values[idx]
		if !mixed_integer_expr_type_can_widen(typ) {
			continue
		}
		widened := tc.widen_mixed_integer_infix_type(flat.NodeId(idx), typ)
		if widened.source_type_id() != typ.source_type_id() {
			tc.invalidate_checked_type_id(idx)
			tc.expr_type_values[idx] = tc.intern_type_reference(widened)
		}
	}
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
	// Inserting a string key stores the map's own clone of it.
	mut ret_types := map[string]Type{}
	ret_types.reserve(u32(tc.fn_ret_types.len))
	for name, ret in tc.fn_ret_types {
		ret_types[name] = clone_owned_type(ret)
	}
	mut param_types := map[string][]Type{}
	param_types.reserve(u32(tc.fn_param_types.len))
	for name, params in tc.fn_param_types {
		param_types[name] = clone_owned_types(params)
	}
	mut variadic := map[string]bool{}
	variadic.reserve(u32(tc.fn_variadic.len))
	for name, is_variadic in tc.fn_variadic {
		variadic[name] = is_variadic
	}
	mut specialized := map[string]bool{}
	specialized.reserve(u32(tc.specialized_generic_fns.len))
	for name, is_specialized in tc.specialized_generic_fns {
		specialized[name] = is_specialized
	}
	tc.fn_ret_types = ret_types.move()
	tc.fn_ret_types_revision++
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

// promote_scoped_declaration_visibility moves declaration ownership into the
// current arena before the scoped transform metadata is released.
pub fn (mut tc TypeChecker) promote_scoped_declaration_visibility() {
	mut declarations := map[string]DeclarationVisibility{}
	for name, visibility in tc.declaration_visibility {
		declarations[name.clone()] = DeclarationVisibility{
			...visibility
			module_name: visibility.module_name.clone()
		}
	}
	tc.declaration_visibility = declarations.move()
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
		tc.type_cache = new_type_cache_with_base(tc.pre_transform_type_cache.parse_enabled,
			tc.pre_transform_type_cache)
		if !isnil(tc.resolution_type_views) {
			// The cached parse views still memoize into the cache that is now the frozen
			// base. They would grow its maps in the transform's disposable arena.
			tc.reset_resolution_type_view_cache()
		}
	}
}

// promote_scoped_transform_interners moves additions made by a scoped
// transform into the current compilation arena before that scope is freed.
pub fn (mut tc TypeChecker) promote_scoped_transform_interners(type_start int, symbol_start int, scope voidptr) {
	if !isnil(tc.type_cache) {
		tc.type_cache.parsed_scalars = unsafe { nil }
		tc.type_cache.indexed_types = unsafe { nil }
	}
	if !isnil(tc.type_interner) {
		mut interner := tc.type_interner
		interner.promote_from(type_start, scope)
	}
	if !isnil(tc.symbols) {
		mut symbols := tc.symbols
		symbols.promote_from(symbol_start, scope)
	}
}

fn reserve_name_cache(mut values []&CachedName, n int) {
	if n > values.cap {
		unsafe { values.grow_cap(n - values.cap) }
	}
}

fn reserve_bool_cache(mut values []bool, n int) {
	if n > values.cap {
		unsafe { values.grow_cap(n - values.cap) }
	}
}

fn reserve_type_cache(mut values []&Type, n int) {
	if n > values.cap {
		unsafe { values.grow_cap(n - values.cap) }
	}
}

fn extend_name_cache(mut values []&CachedName, n int) {
	if n > values.len {
		start := values.len
		unsafe {
			values.grow_len(n - start)
			vmemset(&values[start], 0, int(n - start) * int(sizeof(&CachedName)))
		}
	}
}

fn extend_bool_cache(mut values []bool, n int) {
	if n > values.len {
		values << []bool{len: n - values.len}
	}
}

fn extend_type_cache(mut values []&Type, n int) {
	if n > values.len {
		values << []&Type{len: n - values.len, init: empty_type}
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
		msg:        tc.named_variant_diagnostic(msg, node)
		kind:       kind
		node:       node
		file:       tc.cur_file
		node_kind:  if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].kind.str()
		} else {
			''
		}
		node_value: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].value_text()
		} else {
			''
		}
		node_pos:   tc.node_position_string(node)
		pos:        tc.node_pos(node)
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
		msg:        tc.named_variant_diagnostic(msg, node).replace('[fn(', '[fn (')
		kind:       kind
		node:       node
		file:       tc.cur_file
		node_kind:  if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].kind.str()
		} else {
			''
		}
		node_value: if int(node) >= 0 && int(node) < tc.a.nodes.len {
			tc.a.nodes[int(node)].value_text()
		} else {
			''
		}
		node_pos:   tc.node_position_string(node)
		pos:        pos
	}
}

fn (mut tc TypeChecker) record_error_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) {
	if !tc.should_diagnose(node) {
		return
	}
	tc.errors << tc.make_type_error_at(kind, msg, node, pos)
}

fn (mut tc TypeChecker) record_ordered_error_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos, order int) {
	if !tc.should_diagnose(node) {
		return
	}
	base := tc.make_type_error_at(kind, msg, node, pos)
	tc.errors << TypeError{
		...base
		diagnostic_order: order
	}
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
	if !tc.should_diagnose_notice(node) {
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
	if !tc.should_diagnose_notice(node) {
		return
	}
	if tc.notes_are_errors {
		if tc.errors.any(it.kind == kind && it.msg == msg && it.pos == pos) {
			return
		}
		base := tc.make_type_error_at(kind, msg, node, pos)
		tc.errors << TypeError{
			...base
			details:  details.clone()
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
	tc.record_warning_or_error_at(kind, msg, node, pos, tc.warns_are_errors)
}

fn (mut tc TypeChecker) record_warning_or_error_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos, as_error bool) {
	if !tc.should_diagnose_notice(node) {
		return
	}
	if as_error {
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

fn (mut tc TypeChecker) warn_alloc(description string, id flat.NodeId, pos token.Pos) {
	if !tc.warn_about_allocs || tc.cur_module in ['strings', 'math', 'math.bits', 'builtin',
		'builtin.closure', 'strconv', 'os', 'sync', 'v.debug', 'v.embed_file'] {
		return
	}
	tc.warn_alloc_at(description, id, pos)
}

// record_transform_alloc_warning reports an allocation the transform adds
// where the source shows none, such as a local moved to the heap because its
// address escapes. Only project code is reported. The transform has already
// left out assignments marked `@[freed]`: the node may be one it made, which
// has no parents here to look for the attribute on.
pub fn (mut tc TypeChecker) record_transform_alloc_warning(id flat.NodeId, pos token.Pos, description string) {
	if !tc.warn_about_allocs || !tc.valid_node_id(id) {
		return
	}
	// should_diagnose judges by the file and function being checked, and
	// checking is over: judge by the file the position is in instead.
	file := tc.a.source_files[pos.id] or { return }
	saved_file := tc.cur_file
	saved_fn_node := tc.fn_context.node_id
	tc.cur_file = file.name
	tc.fn_context.node_id = -1
	tc.record_warning_at(.compile_error, 'allocation (${description})', id, pos)
	tc.cur_file = saved_file
	tc.fn_context.node_id = saved_fn_node
}

// record_transform_error reports an error that the transform finds after checking,
// such as a `$if` in a `$for` body whose condition cannot be decided. It is not
// limited to the project's own files: the transform only lowers code the program
// uses, and leaving it out would compile a library wrong.
pub fn (mut tc TypeChecker) record_transform_error(id flat.NodeId, pos token.Pos, msg string) {
	if tc.errors.any(it.msg == msg && it.pos == pos) {
		return
	}
	saved_file := tc.cur_file
	if file := tc.a.source_files[pos.id] {
		tc.cur_file = file.name
	}
	tc.record_error_unfiltered_at(.compile_error, msg, id, pos)
	tc.cur_file = saved_file
}

fn (mut tc TypeChecker) warn_alloc_at(description string, id flat.NodeId, pos token.Pos) {
	mut current := id
	mut direct_child := flat.empty_node
	mut value_path := true
	for tc.valid_node_id(current) {
		node := tc.a.node(current)
		if node.kind in [.fn_literal, .lambda_expr] {
			break
		}
		if node.kind == .typeof_expr && !tc.typeof_operand_is_runtime(current, node) {
			return
		}
		if node.kind in [.assign, .decl_assign, .selector_assign, .index_assign]
			&& node.is_freed_assignment() && value_path
			&& tc.assignment_child_is_value(node, direct_child) {
			return
		}
		parent := tc.direct_parent_id(current)
		if parent == current {
			break
		}
		if tc.valid_node_id(parent) && !tc.child_is_value_producing_path(parent, current) {
			value_path = false
		}
		direct_child = current
		current = parent
	}
	tc.record_warning_at(.compile_error, 'allocation (${description})', id, pos)
}

fn (tc &TypeChecker) typeof_operand_is_runtime(id flat.NodeId, node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .selector && parent.children_count > 0
			&& tc.a.child(parent, 0) == id {
			return false
		}
	}
	mut operand_type := unalias_type(tc.resolve_type(tc.a.child(node, 0)))
	if operand_type is Pointer {
		operand_type = unalias_type(operand_type.base_type)
	}
	return operand_type is SumType
}

fn (tc &TypeChecker) assignment_child_is_value(node flat.Node, child_id flat.NodeId) bool {
	if !tc.valid_node_id(child_id) {
		return false
	}
	for i in 0 .. tc.multi_assign_rhs_count(node) {
		if tc.multi_assign_rhs_id(node, i) == child_id {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) child_is_value_producing_path(parent_id flat.NodeId, child_id flat.NodeId) bool {
	parent := tc.a.node(parent_id)
	match parent.kind {
		.block, .match_branch, .lock_expr {
			return parent.children_count > 0
				&& tc.a.child(parent, parent.children_count - 1) == child_id
		}
		.if_expr, .match_stmt {
			for i in 1 .. parent.children_count {
				if tc.a.child(parent, i) == child_id {
					return true
				}
			}
			return false
		}
		.call, .selector, .index {
			return false
		}
		.infix {
			return tc.type_can_own_warned_allocation(tc.resolve_type(parent_id))
		}
		.as_expr, .in_expr, .is_expr {
			return false
		}
		else {
			return true
		}
	}
}

fn (tc &TypeChecker) type_can_own_warned_allocation(typ &Type) bool {
	clean := unalias_type(typ)
	return match clean {
		String, Array, ArrayFixed, Channel, Map, Pointer, FnType, Struct, Interface, SumType {
			true
		}
		OptionType {
			tc.type_can_own_warned_allocation(clean.base_type)
		}
		ResultType {
			tc.type_can_own_warned_allocation(clean.base_type)
		}
		MultiReturn {
			clean.types.any(tc.type_can_own_warned_allocation(it))
		}
		else {
			false
		}
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
		msg:  msg
		kind: .unsupported_generic
		node: node
		pos:  tc.node_pos(node)
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
	tc.collect_top_level_idx_fast_from(a, inactive, 0, -1, '', '')
}

// collect_top_level_idx_fast_from indexes the file pairs from the `first_pair`th
// entry of a.file_node_ids on, after the file whose trailing node is
// `previous_trailing`, of file `previous_file` and module `previous_module`.
fn (mut tc TypeChecker) collect_top_level_idx_fast_from(a &flat.FlatAst, inactive []bool, first_pair int, previous_trailing_start int, previous_file_start string, previous_module_start string) {
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
	for synthetic_pos < tc.synthetic_top_level_type_ids.len
		&& tc.synthetic_top_level_type_ids[synthetic_pos] <= previous_trailing_start {
		synthetic_pos++
	}
	mut previous_trailing := previous_trailing_start
	mut previous_file := previous_file_start
	mut previous_module := previous_module_start
	for k := first_pair; k + 1 < a.file_node_ids.len; k += 2 {
		marker := a.file_node_ids[k]
		trailing := a.file_node_ids[k + 1]
		// Implicit imports live between file pairs. Preserve the context that a
		// full node scan would carry from the preceding file through this gap.
		previous_file, previous_module = tc.collect_index_gap(a, previous_trailing + 1, marker, previous_file, previous_module, inactive)
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
		previous_trailing = trailing
		previous_file = idx_file
		previous_module = idx_module
	}
	if !isnil(tc.prepared_collect) {
		mut prepared := tc.prepared_collect
		prepared.last_trailing = previous_trailing
		prepared.last_file = previous_file
		prepared.last_module = previous_module
	}
	// Anything past the last indexed pair belongs to no later file pair.
	_, _ = tc.collect_index_gap(a, previous_trailing + 1, a.nodes.len, previous_file, previous_module, inactive)
}

// collect_index_gap indexes the raw node range between two indexed file pairs,
// where synthetic import insertion can leave whole bundle files. It mirrors the
// full scan's `.file` handling, which switches the active file and clears the
// active module, so a bundle's `module` declaration cannot leak into the file
// that follows it.
fn (mut tc TypeChecker) collect_index_gap(a &flat.FlatAst, start int, end int, file string, module_name string, inactive []bool) (string, string) {
	mut gap_file := file
	mut gap_module := module_name
	for idx in start .. end {
		if a.nodes[idx].kind == .file {
			gap_file = a.nodes[idx].value
			gap_module = ''
			tc.top_level_idx << idx
			continue
		}
		gap_module = tc.collect_index_child(a, idx, gap_file, gap_module, inactive)
	}
	return gap_file, gap_module
}

fn (mut tc TypeChecker) collect_module_attributes(node flat.Node, file string) {
	for attr in node.generic_params() {
		match attr.all_before(':').trim_space() {
			'translated' {
				tc.translated_files[file] = true
			}
			'generated' {
				tc.generated_files[file] = true
			}
			'has_globals' {
				tc.has_globals_files[file] = true
			}
			else {}
		}
	}
}

fn (mut tc TypeChecker) check_insert_directive(id flat.NodeId, node flat.Node, file string, module_name string) {
	if node.value == 'flag' && node.typ_text().contains('`') {
		saved_file := tc.cur_file
		saved_module := tc.cur_module
		tc.cur_file = file
		tc.cur_module = if module_name != '' { module_name } else { 'main' }
		flag := node.typ_text().trim_space()
		tc.record_error_at(.compile_error, 'bad #flag `${flag}`: shell command substitution with backticks is not supported; use #pkgconfig or explicit flags instead', id, node.pos)
		tc.cur_file = saved_file
		tc.cur_module = saved_module
		return
	}
	if node.value != 'insert' || node.typ_text().len < 2 || file == '' {
		return
	}
	raw_target := node.typ_text().trim_space()
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
	tc.cur_module = if module_name != '' { module_name } else { 'main' }
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
	mut dir := if file != '' { os.dir(file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	return util.nearest_vmod_root(file) or { os.real_path(dir) }
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
			if idx_file != '' && node.value.starts_with('@attributes:') {
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
			name_id := node.source_value_id()
			tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, name_id)] = true
			if node.generic_params().len == 0 {
				tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, scope_unqualified_type_name(name_id))] = true
			}
			tc.top_level_idx << i
		}
		.type_decl, .interface_decl, .enum_decl {
			name_id := node.source_value_id()
			tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, name_id)] = true
			if node.kind != .enum_decl && node.generic_params().len == 0 {
				tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, scope_unqualified_type_name(name_id))] = true
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

// collect indexes source declarations and resolves their types before body checking.
pub fn (mut tc TypeChecker) collect(a &flat.FlatAst) {
	mut ck_c_sw := time.new_stopwatch()
	mut ck_part_sw := time.new_stopwatch()
	tc.a = a
	tc.static_associated_signature_count = -1
	tc.visible_mutation_cache = new_visible_mutation_cache()
	tc.unsafe_c_fns.clear()
	tc.v_fn_semantic_names.clear()
	tc.checked_comptime_method_calls.clear()
	tc.comptime_method_calls_by_decl.clear()
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
	tc.type_cache = new_type_cache(false)
	// One full declaration scan: build the top-level declaration index that every
	// later pass of the check step iterates instead of re-streaming all nodes,
	// detect builtins, and index every source-level type declaration by
	// (file, module, name) so `source_declares_type_in_scope` is an O(1) map
	// lookup instead of a full node scan. The type-scope index is built before
	// pass 1 because pass 1 already calls qualify_name, which depends on it.
	// No later phase adds declarations, so both indexes stay complete for the
	// whole compile.
	tc.declared_type_scope_keys = map[u64]bool{}
	tc.concrete_type_scope_keys = map[u64]bool{}
	tc.top_level_idx = []i32{cap: 65536}
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
		tc.order_top_level_idx_by_files()
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
				name_id := node.source_value_id()
				tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, name_id)] = true
				if node.generic_params().len == 0 {
					tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, scope_unqualified_type_name(name_id))] = true
				}
				tc.top_level_idx << i
			}
			.type_decl, .interface_decl, .enum_decl {
				name_id := node.source_value_id()
				tc.declared_type_scope_keys[scope_type_key(idx_file, idx_module, name_id)] = true
				if node.kind != .enum_decl && node.generic_params().len == 0 {
					tc.concrete_type_scope_keys[scope_type_key(idx_file, idx_module, scope_unqualified_type_name(name_id))] = true
				}
				tc.top_level_idx << i
			}
			.import_decl, .const_decl, .global_decl, .fn_decl, .c_fn_decl {
				tc.top_level_idx << i
			}
			else {}
		}
	}
	tc.order_top_level_idx_by_files()
	tc.top_level_idx_nodes_len = a.nodes.len
	tc.reserve_collect_maps()
	tc.timing_profile('  [ttime]     ck c idx       ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	tc.collect_after_index(a)
}

// order_top_level_idx_by_files moves each file's part of the top-level index,
// from its `.file` marker to the next one, into logical_file_order: a
// diagnostics server parses the modules builtin imports before the user's code,
// and collects them where a one-shot check does, after it.
fn (mut tc TypeChecker) order_top_level_idx_by_files() {
	if tc.logical_file_order.len == 0 {
		return
	}
	mut is_marker := map[int]bool{}
	for marker in tc.logical_file_order {
		is_marker[marker] = true
	}
	mut parts := map[int][]i32{}
	mut head := []i32{}
	mut current := -1
	for idx in tc.top_level_idx {
		if is_marker[int(idx)] {
			current = int(idx)
		}
		if current < 0 {
			head << idx
		} else {
			parts[current] << idx
		}
	}
	mut ordered := []i32{cap: tc.top_level_idx.len}
	ordered << head
	for marker in tc.logical_file_order {
		ordered << parts[marker] or { []i32{} }
	}
	if ordered.len == tc.top_level_idx.len {
		tc.top_level_idx = ordered
	}
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
		tc.fn_ret_type_texts.reserve(u32(tc.fn_ret_type_texts.ids.len) + decl_estimate * 2)
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
				mutation_cache.decls.reserve(u32(mutation_cache.decls.len) + decl_estimate * 3 / 2)
				mutation_cache.global_decls.reserve(u32(mutation_cache.global_decls.len) +
					decl_estimate * 3 / 2)
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
	tc.check_alias_declaration_cycles_of(tc.top_level_idx, '')
}

fn (mut tc TypeChecker) check_alias_declaration_cycles_of(entries []i32, start_module string) {
	mut module_id := flat.source_type_id(start_module)
	mut seen_types := map[u32]bool{}
	mut invalid_aliases := []u32{}
	for index in entries {
		node := tc.a.nodes[index]
		if node.kind == .module_decl {
			module_id = node.source_value_id()
			continue
		}
		if node.kind in [.struct_decl, .type_decl, .interface_decl, .enum_decl] {
			qname_id := qualify_decl_name_in_module_id(node.source_value_id(), module_id)
			if seen_types[qname_id] {
				continue
			}
			seen_types[qname_id] = true
		}
		if node.kind != .type_decl || node.children_count > 0 || node.typ == 0 {
			continue
		}
		name := node.value
		name_id := node.source_value_id()
		qname_id := qualify_decl_name_in_module_id(name_id, module_id)
		container := recursive_alias_container(name_id, node.typ)
		if container.len > 0 {
			tc.recursive_alias_names[name_id] = true
			tc.recursive_alias_names[qname_id] = true
			invalid_aliases << qname_id
			tc.errors << tc.make_type_error_at(.unknown_type, 'recursive declarations of aliases are not allowed - the alias `${name}` is used in the ${container}', flat.NodeId(index), node.pos)
			continue
		}
		target_id := trimmed_source_type_id(node.typ)
		if !flat.source_type_syntax(target_id).named_type() || !tc.alias_chain_reaches(qname_id, target_id, 0) {
			continue
		}
		tc.recursive_alias_names[name_id] = true
		tc.recursive_alias_names[qname_id] = true
		invalid_aliases << qname_id
		tc.errors << tc.make_type_error_at(.unknown_type, 'alias `${name}` forms a cycle through `${flat.source_type_name(target_id)}`', flat.NodeId(index), node.pos)
	}
	// Pass 2 and later type lowering assume that aliases form an acyclic graph.
	// Keep reporting the source declaration above, but replace invalid targets
	// before any consumer can recursively expand them.
	for name_id in invalid_aliases {
		tc.type_aliases.set_id(name_id, flat.source_type_void)
	}
}

fn (tc &TypeChecker) alias_chain_reaches(start u32, current u32, depth int) bool {
	mut dependency_id := current
	if !tc.type_aliases.contains(dependency_id) && !flat.source_type_shape(current).has(.dotted) {
		if flat.source_type_shape(start).has(.dotted) {
			dependency_id = flat.source_type_composition_two(.qualified, flat.source_type_syntax(start).qualifier(), current)
		}
	}
	if dependency_id == start {
		return true
	}
	if depth >= tc.type_aliases.ids.len {
		return false
	}
	target := trimmed_source_type_id(tc.type_aliases.type_id(dependency_id) or { return false })
	if !flat.source_type_syntax(target).named_type() {
		return false
	}
	return tc.alias_chain_reaches(start, target, depth + 1)
}

fn recursive_alias_container(name_id u32, type_id u32) string {
	clean_id := trimmed_source_type_id(type_id)
	syntax := flat.source_type_syntax(clean_id)
	if name_id == 0 || name_id !in syntax.symbols() {
		return ''
	}
	shape := flat.source_type_shape(clean_id)
	if shape.has(.map_type) && syntax.map_function_fragment() {
		return ''
	}
	if shape.has(.array_type) {
		return 'array'
	}
	if shape.has(.leading_bracket) && syntax.first_bracket_raw_inner() != 0 {
		return 'fixed array'
	}
	if shape.has(.map_type) {
		if name_id in flat.source_type_syntax(syntax.bracket_source_inner()).symbols() {
			return 'map key'
		}
		value_id := syntax.bracket_base()
		if value_id != 0 && !flat.source_type_syntax(value_id).function_space_prefix()
			&& name_id in flat.source_type_syntax(value_id).symbols() {
			return 'map value'
		}
	}
	return ''
}

fn type_text_contains_symbol(type_text string, name string) bool {
	if name == '' || type_text.len < name.len {
		return false
	}
	// Find each `name` in turn, like an index search from past the previous one,
	// without copying the text.
	first := name[0]
	last := type_text.len - name.len
	mut index := 0
	for index <= last {
		if type_text[index] != first
			|| unsafe { vmemcmp(type_text.str + index, name.str, name.len) } != 0 {
			index++
			continue
		}
		before_ok := index == 0 || !is_type_symbol_byte(type_text[index - 1])
		end := index + name.len
		after_ok := end == type_text.len || !is_type_symbol_byte(type_text[end])
		if before_ok && after_ok {
			return true
		}
		index = end
		if index >= type_text.len {
			return false
		}
	}
	return false
}

fn is_type_symbol_byte(ch u8) bool {
	return ch.is_alnum() || ch in [`_`, `.`]
}

fn type_id_contains_symbol(type_id u32, name_id u32) bool {
	return name_id != 0 && name_id in flat.source_type_syntax(type_id).symbols()
}

fn (mut tc TypeChecker) register_declaration_visibility(node flat.Node, module_name string) {
	visibility := DeclarationVisibility{
		module_name: module_name
		kind:        node.kind
		is_pub:      node.op == .arrow
	}
	match node.kind {
		.fn_decl {
			name := checker_qualified_fn_name(module_name, node.value)
			tc.declaration_visibility[name] = visibility
			if module_name == 'builtin' && visibility.is_pub {
				tc.declaration_visibility['builtin.${node.value}'] = visibility
			}
		}
		.struct_decl, .type_decl, .interface_decl, .enum_decl {
			name := qualify_decl_name_in_module(node.value, module_name)
			tc.declaration_visibility[name] = visibility
			if node.kind == .interface_decl {
				for i in 0 .. node.children_count {
					field := tc.a.child_node(&node, i)
					if field.kind == .interface_field && field.op == .dot {
						// Abstract methods belong to the interface contract. Record their
						// qualified names so lookup cannot select an unrelated private method
						// on a concrete type with the same short name in another module.
						tc.declaration_visibility['${name}.${field.value}'] = DeclarationVisibility{
							module_name: module_name
							kind:        .fn_decl
							is_pub:      true
						}
					}
				}
			}
		}
		.const_decl {
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .const_field {
					continue
				}
				name := qualify_decl_name_in_module(field.value, module_name)
				tc.declaration_visibility[name] = visibility
			}
		}
		.global_decl {
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .field_decl || field.value.starts_with('C.') {
					continue
				}
				name := qualify_decl_name_in_module(field.value, module_name)
				tc.declaration_visibility[name] = DeclarationVisibility{
					module_name: module_name
					kind:        .global_decl
					is_pub:      visibility.is_pub || field.op == .arrow
				}
			}
		}
		else {}
	}
}

fn (mut tc TypeChecker) collect_declaration_visibility() {
	mut file_modules := tc.file_modules.clone()
	mut file := tc.cur_file
	mut module_name := tc.cur_module
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			file = node.value
			module_name = file_modules[file] or { '' }
		} else if node.kind == .module_decl {
			module_name = node.value
			if file.len > 0 && file !in file_modules {
				file_modules[file] = module_name
			}
		} else {
			tc.register_declaration_visibility(node, module_name)
		}
	}
}

// collect_after_index runs collection passes 1 and 2 plus the resolution tail
// over the already-built top-level index (shared by the fast file-index path
// and the full-scan fallback in collect()).
fn (mut tc TypeChecker) collect_after_index(a &flat.FlatAst) {
	mut ck_c_sw := time.new_stopwatch()
	parallel_declaration_indexes := tc.prepare_collect_declaration_indexes_parallel(a)
	if !parallel_declaration_indexes {
		tc.build_enclosing_generic_param_index(a)
		tc.build_type_declaration_index(a)
		tc.build_fn_declaration_indexes(a)
	}
	if !tc.valid_diagnostic_fast {
		tc.index_multiple_module_import_lines(a)
	}
	// Pass 1: collect type-level names (aliases, enums, sum types)
	tc.collect_pass1(a, tc.top_level_idx, parallel_declaration_indexes)
	tc.check_alias_declaration_cycles()
	tc.cache_fn_generic_params(a)
	// Pass 1 can parse callback aliases before later modules with same-named
	// types have been indexed. Rebuild name-derived caches from the complete
	// declaration table before collecting concrete signatures in pass 2.
	tc.type_cache.clear_c_type_entries()
	tc.invalidate_short_type_name_index()
	if isnil(tc.prepared_collect) {
		tc.check_c_struct_redeclarations(a)
		tc.check_c_fn_redeclarations(a)
	} else {
		// A preparation keeps what the checks saw, for the code collected after it.
		mut prepared := tc.prepared_collect
		tc.check_c_struct_redeclarations_of(a, tc.top_level_idx, mut prepared.c_structs)
		tc.check_c_fn_redeclarations_of(a, tc.top_level_idx, mut prepared.c_fns)
	}
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
	// The parse-heavy per-declaration computation fans out over the worker
	// pool; this serial walk then only replays the order-sensitive table
	// registrations. Empty when the pool is unavailable — then each fn_decl
	// computes inline exactly as before.
	mut pass2_preps, pass2_scopes := tc.collect_pass2_fn_preps_parallel()
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
	mut public_c_structs := map[u32]bool{}
	p2_fn_ns, p2_struct_ns := tc.collect_pass2(a, tc.top_level_idx, pass2_preps, fast_pass2_registration, mut
		public_c_structs)
	if !isnil(tc.prepared_collect) {
		mut prepared := tc.prepared_collect
		prepared.public_c_structs = public_c_structs.clone()
	}
	if parallel_pass2_ancillary {
		tc.defer_fn_ancillary = false
		tc.finish_pass2_ancillary_registrations()
	}
	// The registrations above copied what they keep of the preparations.
	unsafe { pass2_preps.free() }
	free_check_worker_scopes(checker_worker_pool(tc.a), pass2_scopes)
	tc.collect_deprecated_symbols()
	if p2_profile {
		tc.timing_profile('  [ttime]       p2 fns ${f64(p2_fn_ns) / 1000000.0:.2f} ms, structs ${f64(p2_struct_ns) / 1000000.0:.2f} ms')
	}
	tc.timing_profile('  [ttime]     ck c pass2     ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	ck_c_sw.restart()
	tc.resolve_inferred_global_types(a)
	tc.resolve_const_types()
	// Globals may depend on constants whose initializer types were still pending.
	tc.resolve_inferred_global_types(a)
	tc.static_associated_method_names = map[string]bool{}
	for name, _ in tc.fn_ret_types {
		if _, method := flat.decode_static_type_method_name(name) {
			tc.static_associated_method_names[method] = true
		}
	}
	tc.static_associated_signature_count = tc.fn_ret_types.len
	tc.build_const_suffixes()
	tc.build_struct_embed_index()
	if tc.building_v_fast {
		tc.cache_direct_struct_field_types()
	}
	tc.timing_profile('  [ttime]     ck c resolve   ${f64(ck_c_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	if !isnil(tc.visible_mutation_cache) {
		mut visible_mutation_cache := tc.visible_mutation_cache
		visible_mutation_cache.decl_index_ready = true
	}
	$if ownership ? {
		tc.ownership_after_collect()
	}
}

// collect_pass1 collects the type-level names (aliases, enums, sum types) of
// the top-level declarations `entries`, in their order.
fn (mut tc TypeChecker) collect_pass1(a &flat.FlatAst, entries []i32, parallel_declaration_indexes bool) {
	for tl_idx in entries {
		node := a.nodes[tl_idx]
		node_ref := a.node(flat.NodeId(tl_idx))
		if !parallel_declaration_indexes {
			tc.register_declaration_visibility(node, tc.cur_module)
		}
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.import_decl {
				tc.imports[node.typ_text()] = node.value
				tc.register_file_import(node.typ_text(), node.value)
				tc.register_selective_imports(node)
			}
			.enum_decl {
				qn_id := tc.qualify_decl_name_id(node.source_value_id())
				tc.enum_names[qn_id] = true
				mut fields := []string{}
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
					if f.kind == .enum_field {
						fields << f.value
					}
				}
				tc.enum_fields[qn_id] = fields
				if node.typ_text() == 'flag' {
					tc.flag_enums[qn_id] = true
				}
			}
			.struct_decl {
				qname_id := tc.qualify_decl_name_id(node.source_value_id())
				if qname_id !in tc.structs {
					tc.structs[qname_id] = []StructField{}
				}
				tc.struct_modules[qname_id] = tc.cur_module
				tc.struct_files[qname_id] = tc.cur_file
				if node.generic_params().len > 0 {
					param_ids := generic_param_ids(node.generic_params())
					tc.struct_generic_params[qname_id] = param_ids
					if qname_id != node.source_value_id() {
						tc.struct_generic_params[node.source_value_id()] = param_ids.clone()
					}
				}
				implements_types := struct_decl_implements_ids(node.typ)
				if implements_types.len > 0 {
					tc.struct_implements.set_ids(qname_id, implements_types)
				}
				if comma_attr_text_has(node.typ_text(), 'union') {
					tc.unions[qname_id] = true
				}
				if comma_attr_text_has(node.typ_text(), 'params') {
					tc.params_structs[qname_id] = true
				}
				if comma_attr_text_has(node.typ_text(), 'soa') {
					tc.soa_structs[qname_id] = true
				}
				if comma_attr_text_has(node.typ_text(), 'typedef') {
					tc.c_typedef_structs[qname_id] = true
					tc.c_typedef_structs[node.source_value_id()] = true
				}
			}
			.type_decl {
				param_ids := generic_param_ids(node.generic_params())
				if node.children_count > 0 {
					mut variants := []u32{}
					for i in 0 .. node.children_count {
						v := a.child_node(node_ref, i)
						variants << tc.qualify_type_text_with_generic_params(v.source_value_id(), param_ids)
					}
					qname_id := tc.qualify_decl_name_id(node.source_value_id())
					tc.sum_types.set_ids(qname_id, variants)
					if param_ids.len > 0 {
						tc.sum_generic_params[qname_id] = param_ids
						if qname_id != node.source_value_id() {
							tc.sum_generic_params[node.source_value_id()] = param_ids.clone()
						}
					}
				} else if node.typ != 0 {
					sum_variant_ids := flat.source_type_syntax(node.typ).union_parts()
					if sum_variant_ids.len > 1 {
						mut variants := []u32{}
						for part in sum_variant_ids {
							variants << tc.qualify_type_text_with_generic_params(part, param_ids)
						}
						qname_id := tc.qualify_decl_name_id(node.source_value_id())
						tc.sum_types.set_ids(qname_id, variants)
						if param_ids.len > 0 {
							tc.sum_generic_params[qname_id] = param_ids
							if qname_id != node.source_value_id() {
								tc.sum_generic_params[node.source_value_id()] = param_ids.clone()
							}
						}
						continue
					}
					qname_id := tc.qualify_decl_name_id(node.source_value_id())
					alias_target := if param_ids.len > 0 {
						tc.qualify_type_text_with_generic_params(node.typ, param_ids)
					} else {
						tc.qualify_type_text(node.typ)
					}
					tc.type_aliases.set_id(qname_id, alias_target)
					tc.type_alias_modules[qname_id] = tc.cur_module
					if param_ids.len > 0 {
						tc.type_alias_generic_params[qname_id] = param_ids
						if qname_id != node.source_value_id() {
							tc.type_alias_generic_params[node.source_value_id()] = param_ids.clone()
						}
					}
					if c_abi_fn := tc.c_abi_fn_ptr_type_from_id(node.typ) {
						tc.type_alias_c_abi_fns[qname_id] = c_abi_fn
					}
					if tc.cur_module in ['', 'main', 'builtin'] && !tc.type_aliases.contains(node.source_value_id()) {
						tc.type_aliases.set_id(node.source_value_id(), alias_target)
						tc.type_alias_modules[node.source_value_id()] = tc.cur_module
						if param_ids.len > 0 {
							tc.type_alias_generic_params[node.source_value_id()] = param_ids.clone()
						}
						if c_abi_fn := tc.c_abi_fn_ptr_type_from_id(node.typ) {
							tc.type_alias_c_abi_fns[node.source_value_id()] = c_abi_fn
						}
					}
				}
			}
			.interface_decl {
				qname_id := tc.qualify_decl_name_id(node.source_value_id())
				tc.interface_names[qname_id] = true
				if node.generic_params().len > 0 {
					param_ids := generic_param_ids(node.generic_params())
					tc.interface_generic_params[qname_id] = param_ids
					if tc.cur_module in ['', 'main', 'builtin'] && qname_id != node.source_value_id() {
						tc.interface_generic_params[node.source_value_id()] = param_ids.clone()
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
					// Pass 2 never prunes the short names; a superset keeps the call prescreen exact.
					tc.source_no_body_method_names[qname.all_after_last('.')] = true
					if tc.cur_module in ['', 'main'] {
						tc.source_no_body_fns[node.value] = true
						tc.source_no_body_method_names[node.value.all_after_last('.')] = true
					}
				}
			}
			else {}
		}
	}
}

// collect_pass2 collects the struct fields and function signatures of the
// top-level declarations `entries`, in their order, and returns the time it
// took on functions and on structs when the checker is verbose. `pass2_preps`
// holds the precomputed work of each entry, or nothing.
fn (mut tc TypeChecker) collect_pass2(a &flat.FlatAst, entries []i32, pass2_preps []Pass2FnPrep, fast_pass2_registration bool, mut public_c_structs map[u32]bool) (u64, u64) {
	p2_profile := tc.verbose
	mut p2_fn_ns := u64(0)
	mut p2_struct_ns := u64(0)
	mut p2_t0 := u64(0)
	// Keyed by the payloads of the lanes' arenas, which outlive this loop.
	mut promoted_types := &Pass2TypePromotion{}
	for pi, tl_idx in entries {
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
				lane_prep := if pi < pass2_preps.len && pass2_preps[pi].prepared {
					promote_pass2_fn_prep(pass2_preps[pi], mut promoted_types)
				} else {
					Pass2FnPrep{}
				}
				names_ready := lane_prep.prepared && lane_prep.names_ready
				qname := if names_ready {
					lane_prep.qname
				} else {
					tc.qualify_fn_name(node.value)
				}
				lowered_qname := if !fast_pass2_registration {
					''
				} else if names_ready {
					tc.store_c_name(qname, lane_prep.lowered_qname)
				} else {
					tc.cached_c_name(qname)
				}
				lowered_source_name := if !fast_pass2_registration {
					''
				} else if node.value == qname {
					lowered_qname
				} else if names_ready {
					tc.store_c_name(node.value, lane_prep.lowered_source_name)
				} else {
					tc.cached_c_name(node.value)
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
				prep := if lane_prep.prepared {
					lane_prep
				} else {
					tc.compute_pass2_fn_prep(node)
				}
				ret_type := prep.ret_type
				ptypes := prep.ptypes
				param_texts := prep.param_type_ids
				shared_params := prep.shared_params
				is_variadic := prep.is_variadic
				is_c_variadic := prep.is_c_variadic
				has_mut_receiver := prep.has_mut_receiver
				has_forwardable_ctx := prep.has_forwardable_ctx
				if fast_pass2_registration {
					// The signature arrays are immutable after registration. Keep one
					// master-arena copy shared by the semantic, lowered, legacy, and
					// builtin aliases instead of cloning it independently for each key.
					owned_ptypes := if prep.owned_lists { ptypes } else { ptypes.clone() }
					owned_shared_params := if shared_params.len > 0 && !prep.owned_lists {
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
						tc.fn_param_type_texts.set_ids(name, param_texts)
						tc.fn_type_files[name] = tc.cur_file
						tc.fn_type_modules[name] = tc.cur_module
						if node.generic_params().len > 0
							&& (name == qname || tc.cur_module in ['', 'main', 'builtin']) {
							tc.fn_generic_params[name] = generic_param_ids(node.generic_params())
						}
					}
				}
			}
			.struct_decl {
				mut fields := []StructField{}
				old_generic_params := tc.fn_context.generic_params
				tc.fn_context.generic_params = generic_param_ids(node.generic_params())
				mut field_c_abi_fns := map[string]string{}
				mut shared_field_names := []string{}
				mut shared_element_field_names := []string{}
				mut shadows_builtin_error_embed := false
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
					if f.kind != .field_decl {
						continue
					}
					field_typ_id := if f.typ != 0 { f.typ } else { f.source_value_id() }
					field_is_embed := source_field_decl_is_embed(f, field_typ_id)
					shadows_builtin_error := field_is_embed
						&& field_typ_id in [checker_source_error, checker_source_message_error]
						&& tc.unqualified_type_name_shadows_builtin_in_scope_id(field_typ_id, tc.cur_file, tc.cur_module)
					if shadows_builtin_error {
						shadows_builtin_error_embed = true
					}
					mut typ := if node.generic_params().len > 0 {
						tc.parse_scope_param_type(field_typ_id)
					} else {
						*tc.parse_type(field_typ_id)
					}
					if field_is_embed && field_typ_id in [checker_source_error,
						checker_source_message_error]
						&& !shadows_builtin_error {
						typ = Type(Struct{
							name_id: field_typ_id
						})
					} else if f.typ == 0
						|| (flat.source_type_existing_id(f.value) or { 0 }) == field_typ_id
						|| shadows_builtin_error {
						typ = tc.resolve_known_field_type(field_typ_id, typ)
					}
					if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text(field_typ_id) {
						field_c_abi_fns[f.value] = c_abi_fn
					}
					if param_source_type_is_shared(field_typ_id) {
						shared_field_names << f.value
					}
					clean_field_id := trimmed_source_type_id(field_typ_id)
					field_base_id := if flat.source_type_shape(clean_field_id).has(.pointer) {
						trimmed_source_type_id(flat.source_type_syntax(clean_field_id).pointer_raw_base())
					} else {
						clean_field_id
					}
					if flat.source_type_shape(field_base_id).has(.array_type)
						&& flat.source_type_shape(flat.source_type_syntax(field_base_id).raw_base()).has(.shared_type) {
						shared_element_field_names << f.value
					}
					fields << StructField{
						name:        f.value
						typ:         typ
						has_default: f.children_count > 0
						is_embed:    field_is_embed
						is_mut:      source_field_decl_is_mut(f)
						is_volatile: source_field_decl_is_volatile(f)
					}
				}
				tc.fn_context.generic_params = old_generic_params
				qname_id := tc.qualify_decl_name_id(node.source_value_id())
				// A `C.` struct denotes a single external C type, but several modules may
				// mirror it with partial or imprecise field views (e.g. `C.termios` in both
				// `term` and `term.termios`). codegen emits one C struct, so the field table
				// must be a single canonical view. Keep the most complete (superset) view so
				// it is deterministic regardless of module collection order, instead of
				// letting whichever declaration is collected last silently win.
				if flat.source_type_query(qname_id).has(.c_prefix) {
					scoped_name := c_struct_module_key(tc.cur_module, qname_id)
					if existing := tc.c_struct_scoped_fields[scoped_name] {
						if fields.len > existing.len {
							tc.c_struct_scoped_fields[scoped_name] = fields
						}
					} else {
						tc.c_struct_scoped_fields[scoped_name] = fields
					}
					is_public := node.op == .arrow
					if is_public && !public_c_structs[qname_id] {
						public_c_structs[qname_id] = true
					} else if (!is_public && public_c_structs[qname_id])
						|| (tc.structs[qname_id] or { []StructField{} }).len >= fields.len {
						continue
					}
				}
				tc.set_struct_fields(qname_id, fields, shared_field_names, shared_element_field_names,
					field_c_abi_fns, shadows_builtin_error_embed)
			}
			.c_fn_decl {
				c_name := if node.value.starts_with('C.') { node.value } else { 'C.${node.value}' }
				// `C.va_arg(T, ap)` has the type `T` whatever a declaration of the
				// macro says (translated C declares `fn C.va_arg(voidptr, voidptr) voidptr`).
				if c_name == 'C.va_arg' {
					continue
				}
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
					if flat.source_type_shape(child.typ).has(.variadic) {
						is_variadic = true
					}
					parsed_param_type := tc.parse_type(child.typ)
					ptypes << if child.is_mut {
						mut_param_semantic_type(*parsed_param_type)
					} else {
						*parsed_param_type
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
				iface_type_id := tc.qualify_decl_name_id(node.source_value_id())
				iface_generic_params := node.generic_params()
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
					if f.kind != .interface_field {
						continue
					}
					if f.op == .dot {
						mname := '${flat.source_type_name(iface_type_id)}.${f.value}'
						mut absm := tc.interface_abstract_methods[iface_type_id] or { []string{} }
						absm << f.value
						tc.interface_abstract_methods[iface_type_id] = absm
						ret_type := if iface_generic_params.len > 0 {
							tc.parse_scope_param_type(f.typ)
						} else {
							*tc.parse_type(f.typ)
						}
						mut ptypes := []Type{}
						mut param_texts := []u32{}
						mut shared_params := []bool{}
						mut param_mutability := [f.is_mut]
						mut is_variadic := false
						ptypes << Type(Pointer{
							base_type: tc.intern_type_reference(Type(Interface{
								name_id: iface_type_id
							}))
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
							if flat.source_type_shape(child.typ).has(.variadic) {
								is_variadic = true
							}
							parsed_param_type := if iface_generic_params.len > 0 {
								tc.parse_scope_param_type(child.typ)
							} else {
								*tc.parse_type(child.typ)
							}
							ptypes << if child.is_mut {
								mut_param_semantic_type(parsed_param_type)
							} else {
								parsed_param_type
							}
							param_texts << child.typ
							shared_params << param_source_type_is_shared(child.typ)
							param_mutability << child.is_mut
						}
						tc.register_fn_name_alias(mname, ret_type, ptypes, shared_params, is_variadic, false)
						tc.declaration_param_mutability[mname] = param_mutability
						if f.is_mut {
							tc.register_mut_receiver_method(mname)
						}
						if iface_generic_params.len > 0 {
							tc.register_fn_ret_type_text(mname, f.typ)
							tc.fn_param_type_texts.set_ids(mname, [iface_type_id])
							tc.fn_param_type_texts.append_ids(mname, param_texts)
						}
					} else if f.typ != 0 {
						mut fields := tc.interface_fields[iface_type_id] or { []StructField{} }
						fields << StructField{
							name:   f.value
							typ:    tc.parse_type(f.typ)
							is_mut: f.is_mut
						}
						tc.interface_fields[iface_type_id] = fields
					} else if f.value.len > 0 {
						tc.interface_embeds.append_ids(iface_type_id, [
							tc.qualify_type_name(f.source_value_id()),
						])
					}
				}
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := a.child_node(node_ref, i)
					if f.value.len > 0 && f.value.starts_with('C.') {
						tc.c_globals[f.value] = tc.parse_type(f.typ)
					} else if f.value.len > 0 {
						mut ft := *tc.parse_type(f.typ)
						if ft is Void && f.children_count > 0 {
							ft = tc.resolve_type(a.child(f, 0))
						}
						qname := tc.qualify_name(f.value)
						tc.file_scope.insert(f.value, ft)
						tc.global_names[f.value] = true
						tc.file_scope.insert(qname, ft)
						tc.global_names[qname] = true
						if param_source_type_is_shared(f.typ) {
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
						qname := tc.const_decl_key(f.value)
						tc.const_types[qname] = unknown_type_quoting('pending const', qname)
						tc.const_expr_short_names_count = -1
						tc.const_exprs[qname] = a.child(f, 0)
						tc.const_modules[qname] = tc.cur_module
						tc.const_files[qname] = tc.cur_file
					} else if f.kind == .const_field && f.typ != 0 {
						qname := tc.const_decl_key(f.value)
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
	return p2_fn_ns, p2_struct_ns
}

// set_struct_fields registers field metadata for a struct source spelling ID.
fn (mut tc TypeChecker) set_struct_fields(name_id u32, fields []StructField, shared_field_names []string, shared_element_field_names []string, field_c_abi_fns map[string]string, shadows_builtin_error_embed bool) {
	tc.structs[name_id] = fields
	tc.struct_modules[name_id] = tc.cur_module
	tc.struct_files[name_id] = tc.cur_file
	if shadows_builtin_error_embed {
		tc.struct_error_embeds_shadow_builtin[name_id] = true
	}
	for field_name in shared_field_names {
		tc.struct_shared_fields[name_id][field_name] = true
	}
	for field_name in shared_element_field_names {
		tc.struct_shared_element_fields[name_id][field_name] = true
	}
	for field_name, c_abi_fn in field_c_abi_fns {
		tc.struct_field_c_abi_fns[name_id][field_name] = c_abi_fn
	}
}

// Keep the module and type identities distinct without allocating a compound spelling.
fn c_struct_module_key(module_name string, qname_id u32) u64 {
	return (u64(flat.source_type_id(module_name)) << 32) | u64(qname_id)
}

// Seed the frozen cache once instead of making every checker batch walk the
// same large compiler structs. Generic applications still need substitution.
fn (tc &TypeChecker) cache_direct_struct_field_types() {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := tc.type_cache
	mut fields_by_name := map[u32]&StructFieldTypeEntries{}
	mut complete_owners := map[u32]bool{}
	for owner_id, fields in tc.structs {
		if flat.source_type_shape(owner_id).has(.bracketed) {
			continue
		}
		mut owner_fields := map[string]&Type{}
		mut complete := true
		embeds_indexed := tc.struct_embed_receivers.contains(owner_id)
		// Match the direct-field walk's first-declaration precedence.
		for i := fields.len - 1; i >= 0; i-- {
			field := fields[i]
			owner_fields[field.name] = tc.intern_type_reference(field.typ)
			if field.is_embed || (!embeds_indexed && embedded_field_type(field) != none) {
				complete = false
			}
		}
		if owner_fields.len > 0 {
			fields_by_name[owner_id] = &StructFieldTypeEntries{ values: owner_fields.move() }
		}
		if complete {
			complete_owners[owner_id] = true
		}
	}
	cache.struct_field_shared = fields_by_name.move()
	cache.struct_field_complete = complete_owners.move()
	cache.clear_struct_field_misses()
	cache.struct_field_recent_states = [256]i8{}
}

fn source_field_decl_is_mut(field flat.Node) bool {
	meta := field.generic_params()
	return meta.len > 0 && meta[0].contains('m')
}

// source_field_decl_is_volatile reports whether a `field_decl` node was declared
// `volatile` (the `v` flag the parser packs into its payload).
pub fn source_field_decl_is_volatile(field flat.Node) bool {
	meta := field.generic_params()
	return meta.len > 0 && meta[0].contains('v')
}

@[direct_array_access]
fn (mut tc TypeChecker) collect_deprecated_symbols() {
	tc.deprecated_symbols.clear()
	tc.deprecated_modules.clear()
	tc.unsafe_fns.clear()
	tc.collect_deprecated_symbols_of(tc.top_level_idx)
}

fn (mut tc TypeChecker) collect_deprecated_symbols_of(entries []i32) {
	mut module_name := ''
	mut file_name := ''
	for decl_id in entries {
		node := tc.a.nodes[decl_id]
		if node.kind == .file {
			module_name = ''
			file_name = node.value
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			attr_id := decl_id + 1
			if attr_id < tc.a.nodes.len {
				attr_node := tc.a.nodes[attr_id]
				if attr_node.kind == .directive
					&& attr_node.value == '@attributes:${decl_id}' {
					display_name := tc.diagnostic_module_name(node.value, file_name)
					if info := deprecation_info_from_attrs(display_name, attr_node.generic_params()) {
						tc.deprecated_modules[node.value] = info
					}
				}
			}
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
		name:    name
		message: message
		after:   after
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
	tc.resolve_inferred_global_types_of(a, tc.top_level_idx)
}

fn (mut tc TypeChecker) resolve_inferred_global_types_of(a &flat.FlatAst, entries []i32) {
	tc.cur_module = ''
	tc.cur_file = ''
	for tl_idx in entries {
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
					if f.value.len == 0 || f.value.starts_with('C.') || f.typ != 0
						|| f.children_count == 0 {
						continue
					}
					qname := tc.qualify_name(f.value)
					existing := tc.file_scope.lookup(qname) or { builtin_void_type }
					if existing !is Unknown && existing !is Void
						&& !type_contains_unknown(existing)
						&& !generic_semantic_type_has_placeholder(existing)
						&& !tc.type_text_has_generic_placeholder(existing.source_type_id()) {
						continue
					}
					initializer_id := a.child(f, 0)
					initializer := a.node(initializer_id)
					tc.invalidate_const_initializer_type(initializer_id)
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
								&& !tc.type_text_has_generic_placeholder(specialized.return_type.source_type_id()) {
								ft = specialized.return_type
								tc.remember_expr_type(initializer_id, ft)
							}
						}
					}
					if ft is Unknown || ft is Void || type_contains_unknown(ft)
						|| generic_semantic_type_has_placeholder(ft)
						|| tc.type_text_has_generic_placeholder(ft.source_type_id()) {
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

struct CStructDeclRecord {
	signature string
	file      string
	module_   string
}

// CStructRedeclarations is what check_c_struct_redeclarations has seen of the
// C structs so far: per module, and the public ones.
struct CStructRedeclarations {
mut:
	module_decls map[u64]CStructDeclRecord
	public_decls map[u32]CStructDeclRecord
}

fn (mut tc TypeChecker) check_c_struct_redeclarations(a &flat.FlatAst) {
	mut seen := CStructRedeclarations{}
	tc.check_c_struct_redeclarations_of(a, tc.top_level_idx, mut seen)
}

fn (mut tc TypeChecker) check_c_struct_redeclarations_of(a &flat.FlatAst, entries []i32, mut seen CStructRedeclarations) {
	for node_idx in entries {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				tc.enter_file(node.value)
			}
			.module_decl {
				tc.enter_module(node.value)
			}
			.struct_decl {
				qname_id := tc.qualify_type_name(node.source_value_id())
				if !flat.source_type_query(qname_id).has(.c_prefix) {
					continue
				}
				current := CStructDeclRecord{
					signature: tc.c_struct_decl_signature(a, node)
					file:      tc.cur_file
					module_:   tc.cur_module
				}
				module_key := c_struct_module_key(tc.cur_module, qname_id)
				if existing := seen.module_decls[module_key] {
					if !c_struct_decl_signatures_compatible(existing.signature, current.signature)
						&& !tc.c_struct_redeclaration_allowed(qname_id, existing.file, current.file,
							existing.module_, current.module_) {
						tc.record_c_struct_redeclaration(node_idx, node, qname_id)
					}
					seen.module_decls[module_key] = preferred_c_struct_decl(existing, current)
				} else {
					seen.module_decls[module_key] = current
				}
				if node.op != .arrow {
					continue
				}
				if existing := seen.public_decls[qname_id] {
					if existing.module_ != current.module_
						&& !c_struct_decl_signatures_compatible(existing.signature, current.signature)
						&& !tc.c_struct_redeclaration_allowed(qname_id, existing.file, current.file,
							existing.module_, current.module_) {
						tc.record_c_struct_redeclaration(node_idx, node, qname_id)
					}
					seen.public_decls[qname_id] = preferred_c_struct_decl(existing, current)
				} else {
					seen.public_decls[qname_id] = current
				}
			}
			else {}
		}
	}
}

fn preferred_c_struct_decl(existing CStructDeclRecord, current CStructDeclRecord) CStructDeclRecord {
	if c_struct_decl_signature_field_count(current.signature) > c_struct_decl_signature_field_count(existing.signature) {
		return current
	}
	return existing
}

fn (mut tc TypeChecker) record_c_struct_redeclaration(node_idx int, node flat.Node, qname_id u32) {
	node_id := flat.NodeId(node_idx)
	keyword := if comma_attr_text_has(node.typ_text(), 'union') { 'union' } else { 'struct' }
	tc.record_error_unfiltered_at(.duplicate_decl, 'cannot redeclare C struct `${flat.source_type_name(qname_id)}`', node_id,
		tc.declaration_keyword_name_pos(node_id, keyword))
}

const c_fn_abi_float_type = flat.source_type_id('float')
const c_fn_abi_void_pointer_type = flat.source_type_id('void*')

struct CFnDeclSignature {
	return_type u32
	params      []u32
	is_variadic bool
}

// CFnRedeclarations is what check_c_fn_redeclarations has seen of the C
// functions so far: the signature each has, and where it was declared.
struct CFnRedeclarations {
mut:
	signatures map[string]CFnDeclSignature
	modules    map[string]string
	positions  map[string]string
}

fn (mut tc TypeChecker) check_c_fn_redeclarations(a &flat.FlatAst) {
	mut seen := CFnRedeclarations{}
	tc.check_c_fn_redeclarations_of(a, tc.top_level_idx, mut seen)
}

fn (mut tc TypeChecker) check_c_fn_redeclarations_of(a &flat.FlatAst, entries []i32, mut seen CFnRedeclarations) {
	tc.cur_module = ''
	tc.cur_file = ''
	for node_idx in entries {
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
				mut params := []u32{}
				mut is_variadic := false
				for i in 0 .. node.children_count {
					child := a.child_node(&node, i)
					if child.kind != .param {
						if tc.prefix_param_scan {
							break
						}
						continue
					}
					if flat.source_type_shape(child.typ).has(.variadic) {
						is_variadic = true
					} else {
						params << flat.source_type_id(tc.c_extern_abi_type(tc.parse_type(child.typ)))
					}
				}
				signature := CFnDeclSignature{
					return_type: flat.source_type_id(tc.c_extern_abi_type(tc.parse_type(node.typ)))
					params:      params
					is_variadic: is_variadic
				}
				if existing := seen.signatures[name] {
					existing_module := seen.modules[name] or { '' }
					if existing_module == 'builtin' || existing_module == tc.cur_module {
						seen.signatures[name] = signature
						seen.modules[name] = tc.cur_module
						seen.positions[name] = tc.node_position_string(flat.NodeId(node_idx))
					} else if tc.cur_module != 'builtin'
						&& !c_fn_decl_signatures_compatible(existing, signature) {
						existing_pos := seen.positions[name] or { '' }
						location := if existing_pos.len > 0 { ' at ${existing_pos}' } else { '' }
						tc.record_error_unfiltered(.duplicate_decl, 'C function `${name}` was already declared with a different signature in module `${existing_module}`${location}', flat.NodeId(node_idx))
					}
				} else {
					seen.signatures[name] = signature
					seen.modules[name] = tc.cur_module
					seen.positions[name] = tc.node_position_string(flat.NodeId(node_idx))
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

fn c_fn_fixed_param_is_compatible_with_variadic(type_id u32) bool {
	// These types are changed by C's default argument promotions. A fixed
	// declaration would therefore not describe the same argument ABI.
	return type_id !in [flat.source_type_i8, flat.source_type_i16, flat.source_type_u8,
		flat.source_type_u16, c_fn_abi_float_type, flat.source_type_char, flat.source_type_bool]
}

fn c_fn_decl_param_prefix_compatible(a []u32, b []u32, count int) bool {
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

fn c_fn_decl_abi_types_compatible(a u32, b u32) bool {
	if a == b || (a in [flat.source_type_int, flat.source_type_i32] && b in [
		flat.source_type_int,
		flat.source_type_i32,
	]) {
		return true
	}
	return (a == c_fn_abi_void_pointer_type && flat.source_type_query(b).has(.c_pointer_suffix)) || (b == c_fn_abi_void_pointer_type && flat.source_type_query(a).has(.c_pointer_suffix))
}

fn (tc &TypeChecker) c_struct_redeclaration_allowed(qname_id u32, first_file string, second_file string, first_module string, second_module string) bool {
	if qname_id == checker_source_c_termios && tc.c_struct_decl_is_vlib_termios_shim(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_termios_shim(second_file, second_module) {
		return true
	}
	if qname_id == checker_source_c_winsize && tc.c_struct_decl_is_vlib_winsize_shim(first_file, first_module)
		&& tc.c_struct_decl_is_vlib_winsize_shim(second_file, second_module) {
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

fn (tc &TypeChecker) c_struct_decl_signature(a &flat.FlatAst, node flat.Node) string {
	// Attributes such as `@[typedef]` change how the C name is spelled, not the
	// aggregate's field layout. Only the struct/union distinction belongs in the
	// redeclaration signature.
	mut sig := if comma_attr_text_has(node.typ_text(), 'union') { 'union' } else { '' }
	mut has_fields := false
	for i in 0 .. node.children_count {
		f := a.child_node(&node, i)
		if f.kind != .field_decl {
			continue
		}
		has_fields = true
		field_name := if f.value.starts_with('@') { f.value[1..] } else { f.value }
		field_typ_id := if f.typ != 0 { f.typ } else { f.source_value_id() }
		sig += '|${field_name}:${tc.c_type(tc.parse_type(field_typ_id))}'
	}
	if !has_fields {
		return ''
	}
	return sig
}

fn c_struct_decl_signatures_compatible(a string, b string) bool {
	if a == '' || b == '' || a == b {
		return true
	}
	a_parts := a.split('|')
	b_parts := b.split('|')
	if a_parts.len == 0 || b_parts.len == 0 || a_parts[0] != b_parts[0] {
		return false
	}
	a_fields := a_parts[1..]
	b_fields := b_parts[1..]
	if c_struct_decl_fields_conflict(a_fields, b_fields) {
		return false
	}
	if a_parts[0] == 'union' {
		// C header bindings can expose different partial views of the same union.
		// The views are compatible while every field they share has the same type.
		return true
	}
	// Partial struct views are also common in C bindings. Keep equally complete
	// declarations strict so reordered layouts are still diagnosed.
	if a_fields.len == b_fields.len {
		return false
	}
	return c_struct_decl_fields_subset(a_fields, b_fields)
		|| c_struct_decl_fields_subset(b_fields, a_fields)
}

fn c_struct_decl_fields_subset(smaller []string, big []string) bool {
	for field in smaller {
		if field !in big {
			return false
		}
	}
	return true
}

fn c_struct_decl_fields_conflict(a []string, b []string) bool {
	for a_field in a {
		a_name := a_field.all_before(':')
		for b_field in b {
			if b_field.all_before(':') == a_name && a_field != b_field {
				return true
			}
		}
	}
	return false
}

fn c_struct_decl_signature_field_count(sig string) int {
	if sig == '' {
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
	tc.const_suffix_key_count = tc.const_types.len
	// Include the complete existing suffix table, including ambiguous suffixes.
	tc.const_selector_bloom = new_const_selector_bloom(tc.const_types, tc.const_suffixes)
	tc.const_selector_suffix_count = tc.const_suffixes.len
}

// prepare_check_name_filters takes short names of globals, constants, sum types and aliases
// before the semantic check, on the checker whose forks check the bodies.
// A filter is only consulted while its table still has the counted entries.
fn (mut tc TypeChecker) prepare_check_name_filters() {
	mut const_names := map[string]bool{}
	for name, _ in tc.const_exprs {
		const_names[short_name_view(name)] = true
	}
	tc.const_expr_short_names = const_names.move()
	tc.const_expr_short_names_count = tc.const_exprs.len
	if !isnil(tc.a) {
		tc.has_required_fields = false
		for field in tc.a.nodes {
			if field.kind == .field_decl && struct_field_has_attr(field, 'required') {
				tc.has_required_fields = true
				break
			}
		}
		tc.required_fields_ast_nodes = tc.a.nodes.len
	}
	// The import table is immutable during checking and in worker views. Keep
	// distinct meanings for each alias so const folding need not scan every file.
	mut import_modules := map[string][]string{}
	for _, info in tc.file_imports_by_file {
		if isnil(info) {
			continue
		}
		for alias, module_name in info.imports {
			if module_name !in import_modules[alias] {
				import_modules[alias] << module_name
			}
		}
	}
	tc.const_import_modules = import_modules.move()
	tc.const_import_files_count = tc.file_imports_by_file.len
	mut globals := map[string]bool{}
	for name, _ in tc.global_names {
		globals[short_name_view(name)] = true
	}
	tc.global_short_names = globals.move()
	tc.global_short_names_count = tc.global_names.len
	mut sums := map[u32]bool{}
	for name_id, _ in tc.sum_types.ids {
		sums[scope_unqualified_type_name(generic_base_type_id(name_id))] = true
	}
	tc.sum_type_short_names = sums.move()
	tc.sum_type_short_names_count = tc.sum_types.ids.len
	mut aliases := map[u32]bool{}
	for name_id, _ in tc.type_aliases.ids {
		aliases[scope_unqualified_type_name(name_id)] = true
	}
	tc.alias_short_names = aliases.move()
	tc.alias_short_names_count = tc.type_aliases.ids.len
	tc.global_name_bloom = NameBloom{}
	for name, _ in tc.global_names {
		tc.global_name_bloom.add(name)
	}
	tc.global_name_bloom.count = u64(tc.global_names.len)
	tc.call_flag_bloom = NameBloom{}
	$if ownership ? {
		// Ownership checks register fn literal signatures from the workers.
		return
	}
	// fn_variadic holds every fn, so only the entries that set a flag go in.
	for name, shared_params in tc.fn_shared_params {
		if shared_params.len > 0 {
			tc.call_flag_bloom.add(name)
		}
	}
	for name, is_variadic in tc.fn_variadic {
		if is_variadic {
			tc.call_flag_bloom.add(name)
		}
	}
	for name, is_c_variadic in tc.c_variadic_fns {
		if is_c_variadic {
			tc.call_flag_bloom.add(name)
		}
	}
	for name, has_ctx in tc.fn_implicit_veb_ctx {
		if has_ctx {
			tc.call_flag_bloom.add(name)
		}
	}
	tc.call_flag_lens = tc.call_flag_table_lens()
	tc.call_flag_bloom.count = call_flag_bloom_built
}

// call_flag_bloom_built is the call_flag_bloom count while it covers the flag tables.
const call_flag_bloom_built = u64(1)

// call_flag_table_lens are the sizes of the sparse tables that call_info reads
// besides the signatures.
@[inline]
fn (tc &TypeChecker) call_flag_table_lens() [4]int {
	return [tc.fn_shared_params.len, tc.fn_variadic.len, tc.c_variadic_fns.len,
		tc.fn_implicit_veb_ctx.len]!
}

// call_flag_tables_count is call_flag_bloom_built while no flag table changed
// size since prepare_check_name_filters, so that a table written directly
// (not through a registration, which disables the bloom) makes call_info read
// every table again.
@[inline]
fn (tc &TypeChecker) call_flag_tables_count() u64 {
	if tc.call_flag_lens[0] != tc.fn_shared_params.len || tc.call_flag_lens[1] != tc.fn_variadic.len
		|| tc.call_flag_lens[2] != tc.c_variadic_fns.len
		|| tc.call_flag_lens[3] != tc.fn_implicit_veb_ctx.len {
		return name_bloom_invalid_count
	}
	return call_flag_bloom_built
}

// disable_call_flag_bloom makes call_info read every flag table, after a flag
// table entry may have been set without call_flag_bloom.
@[inline]
fn (mut tc TypeChecker) disable_call_flag_bloom() {
	tc.call_flag_bloom.count = name_bloom_invalid_count
}

// NameBloom is a 4096-bit bloom filter over the names of a table that had
// `count` entries when it was filled. A clear bit proves that a name is not in
// the table, without hashing all of its bytes.
const name_bloom_invalid_count = u64(0xfffffffffffffffe)

struct NameBloom {
mut:
	count u64 = 0xffffffffffffffff
	bits  [64]u64
}

@[direct_array_access; inline]
fn name_bloom_bit(name string) u32 {
	if name.len == 0 {
		return 0
	}
	mut h := u32(name.len) * 0x9e3779b1 ^ u32(name[0]) * 0x85ebca6b
	h ^= u32(name[name.len - 1]) * 0xc2b2ae35 ^ u32(name[name.len / 2]) * 0x27d4eb2f
	return (h ^ (h >> 15)) & 4095
}

fn (mut bloom NameBloom) add(name string) {
	bit := name_bloom_bit(name)
	bloom.bits[bit >> 6] |= u64(1) << (bit & 63)
}

// excludes reports whether `name` is surely not in a table of `count` entries.
@[direct_array_access; inline]
fn (bloom &NameBloom) excludes(name string, count u64) bool {
	if count == name_bloom_invalid_count || bloom.count != count {
		return false
	}
	bit := name_bloom_bit(name)
	return bloom.bits[bit >> 6] & (u64(1) << (bit & 63)) == 0
}

// selector_names_no_const reports that `node.value` is no const's last name
// segment and no bare const key, so no selector with this field names a const.
@[inline]
fn (tc &TypeChecker) selector_names_no_const(field string) bool {
	if tc.const_suffix_key_count != tc.const_types.len {
		return false
	}
	if tc.const_selector_suffix_count == tc.const_suffixes.len
		&& tc.const_selector_bloom.excludes(field, tc.const_types.len) {
		return true
	}
	return field !in tc.const_suffixes && field !in tc.const_types
}

// ConstSelectorBloom is a two-probe bloom filter over the const keys and the
// const suffixes, sized to about 16 bits per name. The const tables hold far
// more names than a 4096-bit NameBloom can tell apart. It is never changed
// after it is built, so forks share it.
@[heap]
struct ConstSelectorBloom {
	count int // const_types.len when it was built
	mask  u32
	bits  []u64
}

// new_const_selector_bloom builds a ConstSelectorBloom over the keys of both tables.
fn new_const_selector_bloom(const_types map[string]Type, const_suffixes map[string]string) &ConstSelectorBloom {
	mut nbits := u32(4096)
	for u64(nbits) < u64(const_types.len + const_suffixes.len) * 16 && nbits < (u32(1) << 24) {
		nbits <<= 1
	}
	mask := nbits - 1
	mut bits := []u64{len: int(nbits >> 6)}
	for name, _ in const_types {
		const_selector_bloom_add(mut bits, mask, name)
	}
	for name, _ in const_suffixes {
		const_selector_bloom_add(mut bits, mask, name)
	}
	return &ConstSelectorBloom{
		count: const_types.len
		mask:  mask
		bits:  bits
	}
}

// const_selector_hash is FNV-1a over all bytes of `name`, with a final mix.
@[direct_array_access; inline]
fn const_selector_hash(name string) u32 {
	mut h := u32(0x811c9dc5)
	for i in 0 .. name.len {
		h = (h ^ u32(name[i])) * 0x01000193
	}
	return h ^ (h >> 15)
}

@[direct_array_access]
fn const_selector_bloom_add(mut bits []u64, mask u32, name string) {
	h := const_selector_hash(name)
	b1 := h & mask
	b2 := ((h * 0x9e3779b1) >> 8) & mask
	bits[b1 >> 6] |= u64(1) << (b1 & 63)
	bits[b2 >> 6] |= u64(1) << (b2 & 63)
}

// excludes reports whether `name` is surely not in the tables, which must
// still have `count` const keys.
@[direct_array_access; inline]
fn (bloom &ConstSelectorBloom) excludes(name string, count int) bool {
	if isnil(bloom) || bloom.count != count {
		return false
	}
	h := const_selector_hash(name)
	b1 := h & bloom.mask
	if bloom.bits[b1 >> 6] & (u64(1) << (b1 & 63)) == 0 {
		return true
	}
	b2 := ((h * 0x9e3779b1) >> 8) & bloom.mask
	return bloom.bits[b2 >> 6] & (u64(1) << (b2 & 63)) == 0
}

// selector_names_no_global reports that no global has the last name segment of
// `field` as its own, so no selector with this field names a global.
@[inline]
fn (tc &TypeChecker) selector_names_no_global(field string) bool {
	return tc.global_short_names_count == tc.global_names.len
		&& short_name_view(field) !in tc.global_short_names
}

// name_names_no_sum_type reports that no sum type has the last segment of the
// generic base of `name_id`, so neither it nor its qualified spelling is one.
@[inline]
fn (tc &TypeChecker) name_names_no_sum_type(name_id u32) bool {
	return tc.sum_type_short_names_count == tc.sum_types.ids.len
		&& scope_unqualified_type_name(generic_base_type_id(name_id)) !in tc.sum_type_short_names
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
	tc.resolve_const_types_of(tc.const_exprs)
}

// resolve_const_types_of resolves the types of the constants of `const_exprs`,
// which the others do not depend on.
fn (mut tc TypeChecker) resolve_const_types_of(const_exprs map[string]flat.NodeId) {
	if const_exprs.len == 0 {
		return
	}
	saved_module := tc.cur_module
	saved_file := tc.cur_file
	// Fixed-array constants have a complete storage type from their outer
	// literal shape. Publish that type before resolving their element values so
	// addressed references such as `&a[0]` can type-check inside `a` itself.
	for name, expr_id in const_exprs {
		tc.cur_module = tc.const_modules[name] or { '' }
		tc.cur_file = tc.const_files[name] or { '' }
		if fixed_type := tc.syntactic_fixed_array_const_type(expr_id) {
			tc.const_types[name] = fixed_type
		}
	}
	// Literal numeric initializers cannot observe another declaration's type.
	// Resolve them in the first pass only; dependent initializers still use the
	// original fixed-point loop. This set is local to this collection invocation.
	mut resolved_numeric := map[string]bool{}
	for iteration in 0 .. const_exprs.len {
		mut changed := false
		for name, expr_id in const_exprs {
			if iteration > 0 && resolved_numeric[name] {
				continue
			}
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
			if iteration == 0 && !type_contains_unknown(new_type)
				&& independent_numeric_const_initializer(tc.a, expr_id) {
				resolved_numeric[name] = true
			}
			old_type := tc.const_types[name] or { builtin_void_type }
			if old_type.source_type_id() != new_type.source_type_id() {
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
		builtin_void_type
	}
	return Type(ArrayFixed{
		elem_type: tc.intern_type_reference(elem_type)
		len:       int(literal.children_count)
	})
}

fn (tc &TypeChecker) syntactic_const_element_type(id flat.NodeId) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	return match node.kind {
		.struct_init { *tc.parse_type(node.source_value_id()) }
		.postfix { tc.syntactic_fixed_array_const_type(id)? }
		.int_literal { builtin_int_type }
		.float_literal { builtin_f64_type }
		.bool_literal { builtin_bool_type }
		.char_literal { builtin_rune_type }
		.string_literal { builtin_string_type }
		else { none }
	}
}

fn (mut tc TypeChecker) invalidate_const_initializer_type(expr_id flat.NodeId) {
	idx := int(expr_id)
	if idx < 0 || idx >= tc.a.nodes.len {
		return
	}
	tc.invalidate_checked_type_id(idx)
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
						elem_type: tc.intern_type_reference(elem_type)
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
					return *inner_type.base_type
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
		type_name_id := if mod_name.len > 0 && mod_name != 'main' && mod_name != 'builtin' {
			flat.source_type_composition_two(.qualified, flat.source_type_id(mod_name),
				checker_source_keywords_matcher_trie)
		} else {
			checker_source_keywords_matcher_trie
		}
		return Type(Struct{
			name_id: type_name_id
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

// const_decl_key returns the key of the const `name` declared in the current module.
// The consts of `main` and `builtin` share bare keys, so that main code can use the
// builtin consts unqualified. When both modules declare the same name, the main
// const keeps the bare key and the builtin const moves to `builtin.name`; builtin
// code finds it through shadowed_builtin_const_key.
fn (mut tc TypeChecker) const_decl_key(name string) string {
	qname := tc.qualify_name(name)
	if qname != name || name.contains('.') {
		return qname
	}
	owner := tc.const_modules[name] or { return name }
	owner_is_builtin := owner == 'builtin'
	if tc.cur_module == 'builtin' {
		return if owner_is_builtin { name } else { builtin_const_key(name) }
	}
	if owner_is_builtin {
		builtin_key := builtin_const_key(name)
		if typ := tc.const_types[name] {
			tc.const_types[builtin_key] = typ
		}
		if expr_id := tc.const_exprs[name] {
			tc.const_expr_short_names_count = -1
			tc.const_exprs[builtin_key] = expr_id
			tc.const_exprs.delete(name)
		}
		tc.const_modules[builtin_key] = owner
		if file := tc.const_files[name] {
			tc.const_files[builtin_key] = file
		}
	}
	return name
}

fn builtin_const_key(name string) string {
	return 'builtin.' + name
}

// shadowed_builtin_const_key returns the key of the builtin const `name`, when the
// current module is builtin and a main const of the same name holds the bare key.
pub fn (tc &TypeChecker) shadowed_builtin_const_key(name string) ?string {
	return tc.shadowed_builtin_const_key_in_module(name, tc.cur_module)
}

// shadowed_builtin_const_key_in_module is shadowed_builtin_const_key for code in
// `module_name`. Only main code sees the main const under the bare key, so the code
// of every other module gets the builtin const.
pub fn (tc &TypeChecker) shadowed_builtin_const_key_in_module(name string, module_name string) ?string {
	if is_main_module_name(module_name) || name.len == 0 {
		return none
	}
	owner := tc.const_modules[name] or { return none }
	if owner == 'builtin' {
		return none
	}
	key := builtin_const_key(name)
	if key in tc.const_types {
		return key
	}
	return none
}

fn is_main_module_name(module_name string) bool {
	return module_name.len == 0 || module_name == 'main'
}

// bare_const_key_is_visible reports whether code in the current module sees the
// const with the bare key `key` unqualified (see bare_const_key_is_visible_in_module).
fn (tc &TypeChecker) bare_const_key_is_visible(key string) bool {
	return tc.bare_const_key_is_visible_in_module(key, tc.cur_module)
}

// bare_const_key_is_visible_in_module reports whether code in `module_name` sees
// the const with the bare key `key` unqualified. The consts of main and builtin
// share bare keys (see const_decl_key); the main ones are main's own, so other
// modules do not see them, as V 0.5.2 reports `undefined ident` for them. A
// qualified key is always visible.
pub fn (tc &TypeChecker) bare_const_key_is_visible_in_module(key string, module_name string) bool {
	if is_main_module_name(module_name) || key.contains('.') {
		return true
	}
	owner := tc.const_modules[key] or { return true }
	return owner == 'builtin'
}

fn (tc &TypeChecker) const_key_for_name(name string) ?string {
	// Every key below is `name`, or ends in `.` and the last segment of `name`.
	if tc.const_suffix_key_count == tc.const_types.len
		&& short_name_view(name) !in tc.const_suffixes && name !in tc.const_types {
		return none
	}
	if key := tc.shadowed_builtin_const_key(name) {
		return key
	}
	qname := tc.qualify_name(name)
	if qname in tc.const_types && tc.bare_const_key_is_visible(qname) {
		return qname
	}
	if name in tc.const_types && tc.bare_const_key_is_visible(name) {
		return name
	}
	return tc.vsh_os_const_key(name)
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
	if tc.selector_names_no_const(node.value) {
		return none
	}
	resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
	qname := '${resolved}.${node.value}'
	if typ := tc.const_types[qname] {
		return tc.const_type_from_initializer(qname, typ)
	}
	if (resolved == 'main' || resolved == tc.cur_module) && tc.bare_const_key_is_visible(node.value) {
		if typ := tc.const_types[node.value] {
			return tc.const_type_from_initializer(node.value, typ)
		}
	}
	if key := tc.const_key_for_suffix(qname) {
		typ := tc.const_types[key] or { unknown_type_quoting('unknown const', key) }
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
	if tc.selector_names_no_global(node.value) {
		return none
	}
	resolved := tc.resolve_import_alias(base_node.value) or {
		if base_node.value == short_name_view(tc.cur_module) {
			tc.cur_module
		} else {
			base_node.value
		}
	}
	qname := '${resolved}.${node.value}'
	if qname in tc.global_names {
		return tc.file_scope.lookup(qname)
	}
	if resolved == 'main' || resolved == tc.cur_module {
		// A current-module const owns this name over the cross-module global_names bare hit; return none defers to const_type_for_selector. No shadow check here — node.value is a selector name, not a scope ident.
		if tc.qualify_name(node.value) in tc.const_types {
			return none
		}
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
	if name == '' || name.index_u8(`.`) >= 0 {
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

// shadowed_local_fn_key returns the signature key of the function that a local
// variable named `name` shadows, i.e. one that the current module can already
// call unprefixed. Functions of other modules are always called through their
// module prefix (`os.uname()`), so `uname := os.uname()` shadows nothing.
fn (tc &TypeChecker) shadowed_local_fn_key(name string) ?string {
	if name == '' || name.index_u8(`.`) >= 0 {
		return none
	}
	qualified := tc.qualify_fn_name(name)
	mut key := ''
	if qualified != name && qualified in tc.fn_ret_types {
		key = qualified
	} else if name in tc.fn_ret_types {
		key = name
	}
	// The signature key doubles as the declaration_visibility key, which records
	// the declaring module of every source declaration. Going through it also
	// skips `fn C.uname()`, which is registered under the lowered alias `uname`
	// as well, yet is only ever callable as `C.uname()`.
	if visibility := tc.declaration_visibility[key] {
		if visibility.kind == .fn_decl {
			if visibility.module_name == tc.cur_module
				|| (visibility.module_name in ['', 'main'] && tc.cur_module in ['', 'main']) {
				return key
			}
			// A public `builtin` function stays callable unprefixed from every
			// module, so a local of that name really does shadow it. Its private
			// helpers (`new_node` in `builtin/sorted_map.v`) are not callable
			// outside `builtin`.
			if visibility.module_name == 'builtin' && visibility.is_pub {
				return key
			}
		}
	}
	// A `.vsh` script calls the `os` functions unqualified, so a local of one of
	// those names shadows it as well - in the script itself, which is what
	// `vsh_script_file` asks, and not in a plain `.v` file that happens to be
	// compiled beside one. This asks `declaration_visibility`, which is forked
	// to the parallel checkers, rather than the script-mode resolver, whose
	// semantic-name index is not and would read empty in a worker.
	if tc.vsh_script_file() {
		os_key := 'os.${name}'
		if os_visibility := tc.declaration_visibility[os_key] {
			if os_visibility.kind == .fn_decl && os_visibility.is_pub {
				return os_key
			}
		}
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
	if name == '' || name.index_u8(`.`) >= 0 {
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
						if node.value.index_u8(`.`) >= 0 {
							continue
						}
						cache.local_fn_decl_index['${cur_module}\x01${node.value}'] = true
					}
					else {}
				}
			}
			scan_start = tc.top_level_idx_nodes_len
		}
		incremental_transform_scan := tc.top_level_idx_nodes_len > 0
			&& scan_start >= tc.top_level_idx_nodes_len
		tail_ids, use_ids := tc.tail_decl_ids_for(scan_start, tc.a.nodes.len)
		tail_count := if use_ids { tail_ids.len } else { tc.a.nodes.len - scan_start }
		for k in 0 .. tail_count {
			i := if use_ids { int(tail_ids[k]) } else { scan_start + k }
			node := tc.a.nodes[i]
			match node.kind {
				.module_decl {
					cur_module = node.value
				}
				.fn_decl {
					if node.value.index_u8(`.`) >= 0 {
						continue
					}
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
// QualifyNameCache memoizes both qualification interfaces per source and type-resolution context.
// Its table fingerprint invalidates entries while collection is still growing
// the declared-type tables; every checker fork gets its own instance.
pub struct QualifyNameCache {
pub mut:
	module               string
	file                 string
	resolution_type_mode bool
	fingerprint          int = -1
	entries              map[u32]u32
	texts                flat.TypeTextTable
	last_name            u32
	last_value           u32
	last_valid           bool
}

// qualify_table_fingerprint tracks growth of every declared-type table
// qualify_name consults, so the memo stays valid even in phases that can
// still register types (e.g. anonymous structs during fn-body checking).
fn (tc &TypeChecker) qualify_table_fingerprint() int {
	return tc.structs.len + tc.type_aliases.ids.len + tc.interface_names.len + tc.sum_types.ids.len + tc.enum_names.len + tc.flag_enums.len
}

// ownership_time_spent_us reports the accumulated ownership-analysis time for
// the dedicated benchmark stage. Compiled-out ownership (no `-d ownership`)
// cannot spend time, so this returns 0 by construction in plain builds.
pub fn (tc &TypeChecker) ownership_time_spent_us() i64 {
	return tc.ownership_time_ns / 1000
}

// qualify_name resolves a name to its module-qualified form in the current checker context.
@[inline]
pub fn (tc &TypeChecker) qualify_name(name string) string {
	// Builtin spellings cannot depend on imports, declarations or the current
	// module. Avoid the contextual cache and wrapper walk for these leaves.
	if is_builtin_type_name(name) {
		return name
	}
	if isnil(tc.qualify_name_cache) {
		return tc.qualify_name_uncached(name)
	}
	mut cache := tc.qualify_name_cache
	name_id := cache.texts.intern(name)
	return flat.source_type_name(tc.qualify_type_name_cached(name_id))
}

// same_string is `a == b`, without comparing the bytes of one string object
// with themselves (the checker's current file and module usually are).
@[inline]
fn same_string(a string, b string) bool {
	return a.len == b.len && (a.str == b.str || a == b)
}

const qualify_source_type_i128 = flat.source_type_id('i128')
const qualify_source_type_u128 = flat.source_type_id('u128')
const qualify_source_type_uint = flat.source_type_id('uint')
const qualify_source_type_map = flat.source_type_id('map')
const qualify_source_type_array = flat.source_type_id('array')
const qualify_source_type_invalid = flat.source_type_id('invalid')
const veb_result_source_type = flat.source_type_id('veb.Result')
const veb_context_source_type = flat.source_type_id('veb.Context')
const checker_source_runes_iterator = flat.source_type_id('RunesIterator')
const checker_source_builtin_runes_iterator = flat.source_type_id('builtin.RunesIterator')
const checker_source_error = flat.source_type_id('Error')
const checker_source_message_error = flat.source_type_id('MessageError')
const checker_source_result_marker = flat.source_type_id('!')
const checker_source_params = flat.source_type_id('params')
const checker_source_union = flat.source_type_id('union')
const checker_source_orm_query_data = flat.source_type_id('orm.QueryData')
const checker_source_chan = flat.source_type_id('chan')
const checker_source_struct = flat.source_type_id('struct')
const checker_source_js_number = flat.source_type_id('JS.Number')
const checker_source_generic_t = flat.source_type_id('T')
const checker_source_main_module = flat.source_type_id('main')

// qualify_type_name resolves a source spelling ID in the current file, module,
// and resolution context. Its cache is shared with qualify_name; spelling IDs
// remain distinct from checker-local semantic TypeId values.
pub fn (tc &TypeChecker) qualify_type_name(name_id u32) u32 {
	// Preserve qualify_name's exact builtin exclusions before cache invalidation.
	if flat.source_type_shape(name_id).has(.builtin_alias)
		|| name_id == flat.source_type_char || name_id == flat.source_type_nil
		|| name_id == flat.source_type_none || name_id == qualify_source_type_i128
		|| name_id == qualify_source_type_u128 || name_id == qualify_source_type_uint
		|| name_id == qualify_source_type_map || name_id == qualify_source_type_array {
		return name_id
	}
	if isnil(tc.qualify_name_cache) {
		return tc.qualify_type_name_uncached(name_id)
	}
	return tc.qualify_type_name_cached(name_id)
}

@[inline]
fn (tc &TypeChecker) qualify_type_name_cached(name_id u32) u32 {
	mut cache := tc.qualify_name_cache
	fingerprint := tc.qualify_table_fingerprint()
	if !same_string(cache.module, tc.cur_module) || !same_string(cache.file, tc.cur_file)
		|| cache.resolution_type_mode != tc.resolution_type_mode
		|| cache.fingerprint != fingerprint {
		cache.module = tc.cur_module
		cache.file = tc.cur_file
		cache.resolution_type_mode = tc.resolution_type_mode
		cache.fingerprint = fingerprint
		cache.entries.clear()
		cache.last_valid = false
	}
	if cache.last_valid && cache.last_name == name_id {
		return cache.last_value
	}
	if cached := cache.entries[name_id] {
		cache.last_name = name_id
		cache.last_value = cached
		cache.last_valid = true
		return cached
	}
	return tc.qualify_type_name_miss(name_id)
}

fn (tc &TypeChecker) qualify_type_name_miss(name_id u32) u32 {
	mut cache := tc.qualify_name_cache
	result_id := tc.qualify_type_name_uncached(name_id)
	cache.entries[name_id] = result_id
	cache.last_name = name_id
	cache.last_value = result_id
	cache.last_valid = true
	return result_id
}

fn (tc &TypeChecker) qualify_name_uncached(name string) string {
	return flat.source_type_name(tc.qualify_type_name_uncached(flat.source_type_id(name)))
}

fn (tc &TypeChecker) qualify_type_name_uncached(name_id u32) u32 {
	// Recurse through exact raw children, retaining whitespace and malformed
	// bracket behavior from source qualification.
	shape := flat.source_type_shape(name_id)
	if shape.has(.array_type) {
		raw_base := flat.source_type_syntax(name_id).raw_base()
		qualified_base := tc.qualify_type_name(raw_base)
		if qualified_base == raw_base { return name_id }
		return flat.source_type_composition_one(.array, qualified_base)
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(name_id)
		if syntax.first_bracket_closed() {
			qualified_base := tc.qualify_type_name(syntax.first_bracket_base())
			if qualified_base == syntax.first_bracket_base() { return name_id }
			return flat.source_type_composition_two(.concat, syntax.first_bracket_prefix(), qualified_base)
		}
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(name_id)
		if !syntax.bracket_closed() {
			// Preserve the original malformed-map slice failure on this error path.
			name := flat.source_type_name(name_id)
			bracket_end := find_matching_bracket(name, 3)
			return flat.source_type_composition_two(.map, tc.qualify_type_name(flat.source_type_id(name[4..bracket_end])), tc.qualify_type_name(flat.source_type_id(name[bracket_end + 1..])))
		}
		qualified_key := tc.qualify_type_name(syntax.bracket_raw_inner())
		qualified_value := tc.qualify_type_name(syntax.bracket_suffix())
		if qualified_key == syntax.bracket_raw_inner() && qualified_value == syntax.bracket_suffix() {
			return name_id
		}
		return flat.source_type_composition_two(.map, qualified_key, qualified_value)
	}
	if shape.has(.pointer) || shape.has(.option_type) || shape.has(.result_type) {
		kind := if shape.has(.pointer) {
			flat.SourceTypeComposition.pointer
		} else if shape.has(.option_type) {
			flat.SourceTypeComposition.option
		} else {
			flat.SourceTypeComposition.result
		}
		raw_base := flat.source_type_syntax(name_id).raw_base()
		qualified_base := tc.qualify_type_name(raw_base)
		if qualified_base == raw_base { return name_id }
		return flat.source_type_composition_one(kind, qualified_base)
	}
	if shape.has(.dotted) { return tc.resolve_imported_type_text(name_id) }
	if tc.cur_module.len == 0 || tc.cur_module == 'main' || tc.cur_module == 'builtin' {
		return name_id
	}
	if is_builtin_type_name_id(name_id) || name_id == qualify_source_type_uint { return name_id }
	if tc.unqualified_type_id_is_builtin(name_id) { return name_id }
	qualified_id := flat.source_type_composition_two(.qualified, tc.cur_module_source_id(), name_id)
	if qualified_id in tc.structs || qualified_id in tc.interface_names || tc.sum_types.contains(qualified_id)
		|| qualified_id in tc.enum_names || qualified_id in tc.flag_enums || tc.type_aliases.contains(qualified_id) {
		return qualified_id
	}
	// A declaration in the active source scope shadows an already-collected
	// unqualified symbol (notably errors.Error shadows builtin.Error).
	if tc.source_declares_type_in_scope(name_id, tc.cur_file, tc.cur_module) { return qualified_id }
	// Substituted generic arguments may belong to another module's caller.
	if tc.resolution_type_mode && (name_id in tc.structs || name_id in tc.interface_names
		|| tc.sum_types.contains(name_id) || name_id in tc.enum_names || name_id in tc.flag_enums
		|| tc.type_aliases.contains(name_id)) {
		return name_id
	}
	// Resolution must not invent a capitalized type absent from this module.
	if tc.resolution_type_mode && flat.source_type_syntax(name_id).first_capital()
		&& !tc.qualify_candidate_type_exists(qualified_id) {
		return name_id
	}
	return qualified_id
}

// qualify_decl_name gives a declaration its stable owner-module name without
// consulting declarations collected earlier in the same pass. Type declarations
// use qualify_decl_name_id; this text form remains for function names.
fn (tc &TypeChecker) qualify_decl_name(name string) string {
	if name.contains('.') {
		return flat.source_type_name(tc.resolve_imported_type_text(flat.source_type_id(name)))
	}
	if tc.cur_module.len == 0 || tc.cur_module in ['main', 'builtin'] {
		return name
	}
	return '${tc.cur_module}.${name}'
}

// qualify_decl_name_id is the spelling-ID twin of qualify_decl_name for type
// declarations.
fn (tc &TypeChecker) qualify_decl_name_id(name_id u32) u32 {
	if flat.source_type_shape(name_id).has(.dotted) {
		return tc.resolve_imported_type_text(name_id)
	}
	if tc.cur_module.len == 0 || tc.cur_module in ['main', 'builtin'] {
		return name_id
	}
	return flat.source_type_composition_two(.qualified, tc.cur_module_source_id(), name_id)
}

// cur_module_source_id interns the current module name, the qualifier of the
// spellings that declarations in that module receive.
@[inline]
fn (tc &TypeChecker) cur_module_source_id() u32 {
	return flat.source_type_id(tc.cur_module)
}

// qualify_resolution_type_name qualifies a source spelling ID for RESOLUTION (not
// registration): a bare capitalized name that does not exist under the current
// module but does exist globally (a generic arg substituted from another
// module's caller, e.g. main's `Foo` inside a json2 specialization) stays bare
// instead of becoming a nonexistent `json2.Foo`. Registration must never use
// this - it is order-dependent during collect.
fn (tc &TypeChecker) qualify_resolution_type_name(name_id u32) u32 {
	qualified_id := tc.qualify_type_name(name_id)
	if qualified_id != name_id {
		if flat.source_type_syntax(name_id).first_capital()
			&& !tc.qualify_candidate_type_exists(qualified_id)
			&& tc.qualify_candidate_type_exists(name_id) {
			return name_id
		}
	}
	return qualified_id
}

fn (tc &TypeChecker) qualify_candidate_type_exists(name_id u32) bool {
	return name_id in tc.structs || tc.sum_types.contains(name_id) || name_id in tc.interface_names
		|| name_id in tc.enum_names || tc.type_aliases.contains(name_id) || name_id in tc.struct_generic_params
}

// qualify_type_text qualifies a type text for registration: bare names always
// get the current module prefix (order-independent during collect).
fn (tc &TypeChecker) qualify_type_text(typ_id u32) u32 {
	return tc.qualify_type_text_impl(typ_id, false, []u32{})
}

fn (tc &TypeChecker) qualify_type_text_with_generic_params(typ_id u32, generic_params []u32) u32 {
	return tc.qualify_type_text_impl(typ_id, false, generic_params)
}

// qualify_resolution_type_text qualifies a type text in a resolution-only
// context (generic application args): a bare name substituted from another
// module's caller may stay bare when the module-qualified spelling does not
// exist. Never use for registration.
fn (tc &TypeChecker) qualify_resolution_type_text(typ_id u32) u32 {
	source_id := tc.resolved_inferred_type(typ_id) or { typ_id }
	return tc.qualify_type_text_impl(source_id, true, []u32{})
}

// parse_resolution_type parses type text that can mix declaration-local names with concrete
// generic arguments from another module.
pub fn (tc &TypeChecker) parse_resolution_type(source_id u32) Type {
	clean_source_id := trimmed_source_type_id(source_id)
	if context_independent_type(clean_source_id) {
		return *tc.parse_type(clean_source_id)
	}
	// Generic specialization uses `main.Type` as an internal lock for a
	// caller-owned program type. Resolve that lock before qualification strips
	// `main.` and lets a same-named import in the declaration file capture the
	// resulting bare name.
	syntax := flat.source_type_syntax(clean_source_id)
	if syntax.main_base() != 0 && !flat.source_type_shape(syntax.main_base()).has(.dotted) {
		if _ := tc.resolve_import_alias('main') {
		} else if exact := tc.type_from_known_symbol(syntax.main_base()) {
			return exact
		}
	}
	qualified_source_id := tc.qualify_resolution_type_text(clean_source_id)
	if flat.source_type_shape(qualified_source_id).has(.dotted) {
		if exact := tc.type_from_known_symbol(qualified_source_id) {
			return exact
		}
	}
	if isnil(tc.resolution_type_views) {
		if !isnil(tc.type_cache) && tc.type_cache.parse_enabled
			&& tc.type_cache.alias_parse_stack.len == 0 {
			// The uncached view below parses with no module or generic parameters.
			// Reuse that exact parse entry before allocating another checker view.
			mut cache := tc.type_cache
			if cached := parse_type_cache_get_mode(mut cache, tc.cur_file, '', qualified_source_id, []u32{}, false, tc.fast_parse_recent) {
				cache.parse_hits++
				return *cached
			}
		}
		mut direct_view := tc.fork_type_parse_view(tc.cur_file, '')
		direct_view.resolution_type_mode = false
		return *direct_view.parse_type(qualified_source_id)
	}
	mut views := unsafe { tc.resolution_type_views }
	if !isnil(views.recent_unscoped) && views.recent_unscoped_file == tc.cur_file {
		return *views.recent_unscoped.parse_type(qualified_source_id)
	}
	if cached := views.by_file[tc.cur_file] {
		views.recent_unscoped_file = tc.cur_file
		views.recent_unscoped = cached
		return *cached.parse_type(qualified_source_id)
	}
	mut unscoped := tc.fork_type_parse_view(tc.cur_file, '')
	unscoped.resolution_type_mode = false
	views.by_file[tc.cur_file] = unscoped
	views.recent_unscoped_file = tc.cur_file
	views.recent_unscoped = unscoped
	return *unscoped.parse_type(qualified_source_id)
}

// parse_resolution_type_in_file resolves type text using the imports and module
// of `file` without mutating the checker's current traversal cursor.
pub fn (tc &TypeChecker) parse_resolution_type_in_file(source_id u32, file string) Type {
	if context_independent_type(source_id) {
		return *tc.parse_type(source_id)
	}
	if !isnil(tc.resolution_type_views) {
		mut views := unsafe { tc.resolution_type_views }
		if !isnil(views.recent_scoped) && views.recent_scoped_file == file {
			return views.recent_scoped.parse_resolution_type(source_id)
		}
		if cached := views.scoped_by_file[file] {
			views.recent_scoped_file = file
			views.recent_scoped = cached
			return cached.parse_resolution_type(source_id)
		}
		module_name := tc.file_modules[file] or { '' }
		mut scoped := tc.fork_type_parse_view(file, module_name)
		views.scoped_by_file[file] = scoped
		views.recent_scoped_file = file
		views.recent_scoped = scoped
		return scoped.parse_resolution_type(source_id)
	}
	module_name := tc.file_modules[file] or { '' }
	mut scoped := tc.fork_type_parse_view(file, module_name)
	return scoped.parse_resolution_type(source_id)
}

// Primitive payloads and their exact wrappers cannot refer to declaration imports.
@[inline]
fn context_independent_type(source_id u32) bool {
	return source_id == 0 || flat.source_type_shape(source_id).has(.context_independent)
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

fn (tc &TypeChecker) qualify_type_text_impl(typ_id u32, resolution bool, generic_params []u32) u32 {
	clean_id := trimmed_source_type_id(typ_id)
	if clean_id == 0 {
		return typ_id
	}
	if clean_id in generic_params {
		return clean_id
	}
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if syntax.typeof_expr_prefix() {
		return clean_id
	}
	if shape.has(.pointer) {
		return flat.source_type_composition_one(.pointer, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.mutable_type) {
		inner := tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params)
		return if flat.source_type_shape(inner).has(.pointer) {
			inner
		} else {
			flat.source_type_composition_one(.pointer, inner)
		}
	}
	if shape.has(.shared_type) {
		return flat.source_type_composition_one(.shared_type, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.atomic_type) {
		return flat.source_type_composition_one(.atomic_type, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.option_type) {
		return flat.source_type_composition_one(.option, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.result_type) {
		return flat.source_type_composition_one(.result, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.variadic) {
		return flat.source_type_composition_one(.variadic_parameter, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if clean_id == checker_source_chan || clean_id == checker_source_thread {
		return clean_id
	}
	if shape.has(.channel_type) {
		if flat.source_type_shape(syntax.raw_base()).has(.mutable_type) {
			inner := flat.source_type_syntax(syntax.raw_base()).raw_base()
			return flat.source_type_composition_one(.mutable_channel, tc.qualify_type_text_impl(inner, resolution, generic_params))
		}
		return flat.source_type_composition_one(.channel, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.thread_type) {
		return flat.source_type_composition_one(.thread, tc.qualify_type_text_impl(syntax.raw_base(), resolution, generic_params))
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		key := tc.qualify_type_text_impl(syntax.bracket_raw_inner(), resolution, generic_params)
		value := tc.qualify_type_text_impl(syntax.bracket_suffix(), resolution, generic_params)
		return flat.source_type_composition_two(.map, key, value)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), tc.qualify_type_text_impl(syntax.bracket_suffix(), resolution, generic_params))
	}
	if syntax.tuple_like() && syntax.tuple_contains_comma() {
		mut parts := []u32{cap: syntax.checker_raw_tuple_components().len}
		for part in syntax.checker_raw_tuple_components() {
			parts << tc.qualify_type_text_impl(part, resolution, generic_params)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		return tc.qualify_fn_type_text(clean_id, resolution, generic_params)
	}
	if shape.has(.bracketed) && !shape.has(.leading_bracket) && syntax.bracket_closed() {
		base := tc.qualify_type_text_impl(syntax.generic_base(), resolution, generic_params)
		if syntax.bracket_inner_fixed_length() || is_builtin_type_name_id(syntax.generic_base()) {
			return flat.source_type_composition_two(.concat, base, syntax.bracket_from())
		}
		mut parts := []u32{cap: syntax.bracket_raw_parts().len + 1}
		parts << base
		for part in syntax.bracket_raw_parts() {
			parts << tc.qualify_type_text_impl(part, resolution, generic_params)
		}
		return flat.source_type_composition_two(.concat, flat.source_type_composition(.generic, parts), syntax.bracket_suffix())
	}
	if resolution && shape.has(.dotted) {
		// Keep concrete caller types locked while resolving a foreign generic body.
		// Generic bases retain their canonical specialization spelling below.
		if tc.cur_module != 'main' && tc.cur_module.len > 0
			&& syntax.main_base() != 0 && !flat.source_type_shape(syntax.main_base()).has(.dotted) {
			base_id := syntax.main_base()
			if _ := tc.resolve_import_alias('main') {
			} else if base_id !in tc.struct_generic_params && base_id !in tc.sum_generic_params
				&& base_id !in tc.interface_generic_params && base_id !in tc.type_alias_generic_params
				&& tc.qualify_candidate_type_exists(base_id) {
				return clean_id
			}
		}
		resolved := tc.resolve_imported_type_text(clean_id)
		if resolved != clean_id {
			return resolved
		}
		if tc.qualify_candidate_type_exists(clean_id) {
			return clean_id
		}
	}
	if !shape.has(.dotted) {
		if candidates := tc.selective_import_candidates(clean_id) {
			if resolved_id := tc.selective_import_known_type_candidate_id(candidates) {
				return resolved_id
			}
			if candidates.len == 1 {
				return candidates[0]
			}
		}
	}
	if resolution {
		return tc.qualify_resolution_type_name(clean_id)
	}
	return tc.qualify_type_name(clean_id)
}

// qualify_fn_type_text qualifies normalized parameter and result IDs.
fn (tc &TypeChecker) qualify_fn_type_text(typ_id u32, resolution bool, generic_params []u32) u32 {
	syntax := flat.source_type_syntax(typ_id)
	if !syntax.function_closed() {
		// Preserve the old bounds failure for malformed parameter lists. Successful
		// qualification never needs the spelling bytes to find a delimiter.
		typ := flat.source_type_name(typ_id)
		return flat.source_type_id(typ[typ.len + 1..])
	}
	mut params := []u32{cap: syntax.checker_function_params().len + 1}
	for part in syntax.checker_function_params() {
		clean_part := trimmed_source_type_id(part)
		is_mut := flat.source_type_shape(clean_part).has(.mutable_type)
		param_id := if is_mut {
			trimmed_source_type_id(flat.source_type_syntax(clean_part).raw_base())
		} else {
			clean_part
		}
		qualified := tc.qualify_type_text_impl(flat.source_type_syntax(param_id).parameter_type(), resolution, generic_params)
		params << if is_mut {
			flat.source_type_composition_one(.mutable_parameter, qualified)
		} else {
			qualified
		}
	}
	if syntax.function_result() != 0 {
		params << tc.qualify_type_text_impl(syntax.function_result(), resolution, generic_params)
		return flat.source_type_composition(.function_type, params)
	}
	return flat.source_type_composition(.void_function_type, params)
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
		imports:            map[string]string{}
		selective_imports:  map[u32][]u32{}
		imports_mask_ready: true
	}
	tc.file_imports_by_file[tc.cur_file] = info
	return info
}

fn (mut tc TypeChecker) register_file_import(alias string, module_name string) {
	tc.const_import_files_count = -1
	tc.file_imports[file_import_key(tc.cur_file, alias)] = module_name
	mut info := tc.file_import_info()
	info.imports[alias] = module_name
	info.imports_name_mask |= scope_name_bit(alias)
}

// removed_json_module_message explains an unresolved `import json`: the cJSON based
// `json` module was replaced by `json2`. vfmt migrates the usual calls, but leaves
// files it cannot rewrite safely unchanged, so the manual replacements follow.
fn removed_json_module_message(file string) string {
	return 'the `json` module was removed, use `json2` instead.\n' +
		'`v fmt -w ${file}` rewrites the usual `json` calls to `json2`, and leaves code it cannot rewrite safely unchanged.\n' +
		'To migrate by hand, import `json2` and replace `json.decode(T, s)` with `json2.decode[T](s)`, ' +
		'`json.encode(x)` with `json2.encode(x, escape_unicode: true, time_as_unix: true)`, and ' +
		'`json.encode_pretty(x)` with `json2.encode(x, prettify: true, legacy_layout: true, escape_unicode: true, time_as_unix: true)`.'
}

fn (mut tc TypeChecker) check_import_diagnostics() {
	// The module identity of each file asks the file system the same questions
	// about the same few directories; remember the answers for this pass.
	tc.fs_memo = &CheckerFsMemo{}
	defer {
		tc.fs_memo = unsafe { nil }
	}
	mut first_imports := map[string]token.Pos{}
	mut declaration_seen_in_file := false
	mut module_path_identity := ''
	mut module_directory := ''
	mut module_path_identity_checked := false
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			declaration_seen_in_file = false
			module_path_identity_checked = false
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
		has_source := node.pos.end > node.pos.offset
		if missing_path := tc.a.missing_imports[idx] {
			if missing_path == 'json' {
				tc.record_error_severity_at(.unknown_ident, removed_json_module_message(tc.cur_file), flat.NodeId(idx), node.pos, 'builder error:')
			} else {
				// The resolver knows whether a `modules/` directory would have
				// satisfied this import, and leaves the migration hint for it here.
				layout_hint := tc.a.missing_import_hints[idx]
				tc.record_error_severity_at(.unknown_ident, 'cannot import module "${missing_path}" (not found)${layout_hint}', flat.NodeId(idx), node.pos, 'builder error:')
			}
		}
		if has_source {
			tc.check_import_source_syntax(flat.NodeId(idx), node)
		}
		if module_base != tc.cur_module {
			if deprecation := tc.deprecated_modules[node.value] {
				tc.record_deprecation(flat.NodeId(idx), 'module', deprecation, node.pos)
			}
		}
		if tc.selective_import_has_missing_value_symbol(node, module_path)
			|| tc.selective_import_has_const(node, module_path) {
			tc.record_unused_import_warning(flat.NodeId(idx), node)
		}
		tc.check_selective_const_imports(node, module_path)
		tc.check_selective_type_imports(node, module_path)
		if declaration_seen_in_file && has_source {
			tc.record_error_at(.duplicate_decl, '`import x` can only be declared at the beginning of the file', flat.NodeId(idx), token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 'import'.len))
		}
		if explicit_alias && node.typ_text() == module_base {
			tc.record_error_at(.duplicate_decl, 'import alias `${module_path} as ${node.typ_text()}` is redundant', flat.NodeId(idx), tc.import_alias_pos(node))
		}
		// Compiler-injected runtime imports have no source span and may name the
		// current module (for example channel support while compiling `sync`).
		// Self-import diagnostics only apply to imports written by the user.
		if has_source && node.value != tc.cur_module && !module_path_identity_checked {
			module_path_identity = tc.current_file_module_path_identity() or { '' }
			module_directory = tc.memo_real_path(os.dir(tc.current_file_module_source_path()))
			module_path_identity_checked = true
		}
		resolved_directory := tc.a.resolved_module_dirs[node.value] or { module_directory }
		if has_source && (node.value == tc.cur_module
			|| (node.value == module_path_identity
				&& tc.memo_real_path(resolved_directory) == module_directory)) {
			tc.record_error_at(.duplicate_decl, 'cannot import `${module_path}` into a module with the same name', flat.NodeId(idx), tc.import_module_path_pos(node))
		}
		if has_source && node.typ_text() == tc.cur_module && !tc.current_file_uses_nested_module_path() {
			alias_pos := if explicit_alias {
				tc.import_alias_pos(node)
			} else {
				tc.import_module_basename_pos(node)
			}
			tc.record_error_at(.duplicate_decl, 'cannot import `${module_path}` as `${node.typ_text()}` into a module with the same name', flat.NodeId(idx), alias_pos)
		}
		if node.typ_text() != '_' {
			key := file_import_key(tc.cur_file, node.typ_text())
			if first_pos := first_imports[key] {
				first_line := tc.import_line_number(first_pos)
				pos := tc.import_module_path_pos(node)
				tc.record_error_at(.duplicate_decl, 'A module `${node.typ_text()}` was already imported on line ${first_line}`.', flat.NodeId(idx), pos)
			} else {
				first_imports[key] = node.pos
			}
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

// private_declaration returns the visibility of `name` when it is private to
// another module. Regular test files see private declarations; that path test
// runs only once a private declaration is found, since most lookups miss.
fn (tc &TypeChecker) private_declaration(name string) ?DeclarationVisibility {
	if name == '' {
		return none
	}
	visibility := tc.private_declaration_any_file(name)?
	if is_regular_v_test_file(tc.cur_file) {
		return none
	}
	return visibility
}

// private_type_declaration is private_declaration for the type spelled `type_id`;
// declaration visibility is keyed by symbol text.
fn (tc &TypeChecker) private_type_declaration(type_id u32) ?DeclarationVisibility {
	return tc.private_declaration(flat.source_type_name(type_id))
}

// private_declaration_any_file is `private_declaration` without the test-file
// exemption.
fn (tc &TypeChecker) private_declaration_any_file(name string) ?DeclarationVisibility {
	// A module's own C declaration is its local view of the external type.
	// The canonical visibility entry may belong to a different module's mirror.
	if name.starts_with('C.') {
		if name_id := flat.source_type_existing_id(name) {
			if module_id := flat.source_type_existing_id(tc.cur_module) {
				scoped_key := (u64(module_id) << 32) | u64(name_id)
				if scoped_key in tc.c_struct_scoped_fields {
					return none
				}
			}
		}
	}
	mut declaration_name := name
	if name.contains('.') {
		receiver_name := owner_name_view(visible_mutation_fn_lookup_name(name))
		if receiver_id := flat.source_type_existing_id(receiver_name) {
			if receiver_id in tc.interface_names {
				// Inherited methods use the base interface's declaration for access checks.
				declaration_name = tc.interface_method_signature_key(receiver_id,
					short_name_view(name)) or { name }
			}
		}
	}
	if visibility := tc.declaration_visibility[declaration_name] {
		if declaration_visibility_is_private(visibility, tc.cur_module) {
			return visibility
		}
		return none
	}
	// The first declared candidate decides: the generic-free spelling, then each
	// shorter owner path with its generic-free spelling. A repeated candidate
	// misses again, so the candidates need no list.
	if visibility := tc.declaration_visibility_candidate(visible_mutation_fn_lookup_name(declaration_name)) {
		return tc.private_visibility_or_none(visibility)
	}
	mut shortened := declaration_name
	for {
		dot := shortened.index_u8(`.`)
		if dot < 0 {
			break
		}
		tail := unsafe { shortened.substr_unsafe(dot + 1, shortened.len) }
		if tail.index_u8(`.`) < 0 {
			break
		}
		shortened = tail
		if visibility := tc.declaration_visibility_candidate(shortened) {
			return tc.private_visibility_or_none(visibility)
		}
		if visibility := tc.declaration_visibility_candidate(visible_mutation_fn_lookup_name(shortened)) {
			return tc.private_visibility_or_none(visibility)
		}
	}
	return none
}

fn (tc &TypeChecker) declaration_visibility_candidate(candidate string) ?DeclarationVisibility {
	if candidate.len == 0 {
		return none
	}
	if visibility := tc.declaration_visibility[candidate] {
		return visibility
	}
	return none
}

// private_visibility_or_none is the decision for a declared candidate: its
// visibility when it is private here, none when it is visible.
fn (tc &TypeChecker) private_visibility_or_none(visibility DeclarationVisibility) ?DeclarationVisibility {
	if declaration_visibility_is_private(visibility, tc.cur_module) {
		return visibility
	}
	return none
}

@[inline]
fn declaration_visibility_is_private(visibility DeclarationVisibility, current_module string) bool {
	same_main_module := visibility.module_name in ['', 'main'] && current_module in ['', 'main']
	return !visibility.is_pub && visibility.module_name != current_module && !same_main_module
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
	// The display name scans every file's module, so resolve it only for a diagnostic.
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind != .ident || child.value.len == 0 || !child.value[0].is_capital() {
			continue
		}
		mut symbol_name := ''
		for module_name in [node.value, module_path, module_path.all_after_last('.')] {
			name := '${module_name}.${child.value}'
			if tc.type_symbol_spelling_known(name) {
				symbol_name = name
				break
			}
		}
		if symbol_name.len == 0 {
			display_module_path := tc.diagnostic_module_display_name(module_path)
			tc.record_error_at(.unknown_type, 'module `${display_module_path}` has no type `${child.value}`', child_id, tc.node_value_diagnostic_pos(child_id))
		} else if _ := tc.private_declaration(symbol_name) {
			display_module_path := tc.diagnostic_module_display_name(module_path)
			tc.record_error_at(.unknown_type, 'module `${display_module_path}` type `${child.value}` is private', child_id, tc.node_value_diagnostic_pos(child_id))
		}
	}
}

fn (tc &TypeChecker) import_needs_usage_check(node &flat.Node, file string) bool {
	return (tc.diagnostic_files.len == 0 || file in tc.diagnostic_files)
		&& node.kind == .import_decl && node.pos.is_valid() && node.pos.end > node.pos.offset
		&& node.typ_text() != '_'
}

fn (tc &TypeChecker) unused_import_nodes_by_file() map[int][]int {
	mut file_ids := []int{}
	mut file := tc.cur_file
	for idx in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(idx))
		if node.kind == .file {
			file = node.value
		} else if tc.import_needs_usage_check(node, file) {
			file_ids << node.pos.id
		}
	}
	return tc.nodes_of_files(file_ids)
}

struct UnusedImportCandidate {
	idx         int
	file        string
	module_name string
}

fn (mut tc TypeChecker) check_unused_import_diagnostics() {
	mut candidates := []UnusedImportCandidate{}
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
		if !tc.import_needs_usage_check(&node, tc.cur_file) {
			continue
		}
		candidates << UnusedImportCandidate{
			idx:         idx
			file:        tc.cur_file
			module_name: tc.cur_module
		}
	}
	if candidates.len == 0 {
		return
	}
	final_file := tc.cur_file
	final_module := tc.cur_module
	// import_is_used looks at the nodes of the import's own file. One walk lists
	// the nodes of just the files with an import to check.
	nodes_by_file := tc.unused_import_nodes_by_file()
	for candidate in candidates {
		idx := candidate.idx
		node := tc.a.nodes[idx]
		tc.cur_file = candidate.file
		tc.cur_module = candidate.module_name
		if tc.import_is_used(flat.NodeId(idx), node, nodes_by_file[node.pos.id] or { []int{} }) {
			continue
		}
		if tc.errors.any(it.file == tc.cur_file && it.kind == .unknown_ident
			&& it.msg.starts_with('undefined variable')
			&& (it.node_value == node.typ_text()
				|| (it.node_value.len > 1 && node.typ_text().starts_with(it.node_value)))) {
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
	tc.cur_file = final_file
	tc.cur_module = final_module
}

// node_uses_qualified_attribute reports whether the attributes of `id`, or of
// its struct fields or enum values, name a schema of `qualifier` or
// `module_path`, as the dotted form `@[mod.Skip]` of a custom attribute does
// (vlang/rfcs#35).
fn (tc &TypeChecker) node_uses_qualified_attribute(id flat.NodeId, qualifier string, module_path string) bool {
	if qualifier.len == 0 && module_path.len == 0 {
		return false
	}
	if attribute_pieces_name_module(tc.declaration_attributes[int(id)], qualifier, module_path) {
		return true
	}
	node := tc.a.node(id)
	if node.kind !in [.struct_decl, .enum_decl] {
		// Only struct fields and enum values keep attributes in their metadata.
		// Other children use it for unrelated text, like the module path
		// `te.ui` that a file's `import te.ui` child stores.
		return false
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(node, i)
		if child.kind !in [.field_decl, .enum_field] {
			continue
		}
		mut meta := child.generic_params()
		if meta.len == 0 {
			continue
		}
		if child.kind == .field_decl {
			meta = meta[1..]
		}
		if attribute_pieces_name_module(meta, qualifier, module_path) {
			return true
		}
	}
	return false
}

// attribute_pieces_name_module reports whether an attribute piece is qualified
// with `qualifier` or `module_path`.
fn attribute_pieces_name_module(pieces []string, qualifier string, module_path string) bool {
	for raw in pieces {
		name := raw.all_before(':').trim_space()
		if idx := name.last_index('.') {
			if name[..idx] == qualifier || name[..idx] == module_path {
				return true
			}
		}
	}
	return false
}

// nodes_of_files lists, in order, the indices of the nodes in each of `file_ids`.
fn (tc &TypeChecker) nodes_of_files(file_ids []int) map[int][]int {
	mut max_id := -1
	for id in file_ids {
		max_id = int_max(max_id, id)
	}
	mut slots := []int{len: max_id + 1, init: -1}
	mut ids := []int{}
	for id in file_ids {
		if id >= 0 && slots[id] < 0 {
			slots[id] = ids.len
			ids << id
		}
	}
	mut lists := [][]int{len: ids.len}
	for node_idx in 0 .. tc.a.nodes.len {
		id := tc.a.nodes[node_idx].pos.id
		if id >= 0 && id <= max_id && slots[id] >= 0 {
			lists[slots[id]] << node_idx
		}
	}
	mut nodes_by_file := map[int][]int{}
	for i, id in ids {
		nodes_by_file[id] = lists[i]
	}
	return nodes_by_file
}

fn (mut tc TypeChecker) record_unused_import_warning(id flat.NodeId, node flat.Node) {
	module_path := tc.import_module_path_text(node)
	display_name := if node.typ_text() == module_path {
		module_path
	} else {
		'${node.typ_text()} (${module_path})'
	}
	// V1 reports unused imports from the parser, where only an explicit `-W`
	// turns a warning into an error; `-prod` alone does not.
	tc.record_warning_or_error_at(.unknown_ident, "module '${display_name}' is imported but never used. Use `import ${display_name} as _`, to silence this warning, or just remove the unused import line", id, tc.import_module_path_pos(node), tc.explicit_warns_are_errors)
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
	import_end := int_max(line_end, int_min(node.pos.end, source.len))
	mut cursor := open + 1
	for cursor < import_end && source[cursor] in [` `, `\t`, `\r`, `\n`] {
		cursor++
	}
	if cursor < import_end && source[cursor] == `}` {
		tc.record_error_at(.duplicate_decl, 'empty `${tc.import_module_path_text(node)}` import set, remove `{}`', id, token.new_span(node.pos.id, cursor, cursor + 1))
		return
	}
	mut close := -1
	for i := cursor; i < import_end; i++ {
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
	valid_escaped_name := cursor + 1 < close && source[cursor] == `@`
		&& is_import_ident_byte(source[cursor + 1])
	if cursor < close && !is_import_ident_byte(source[cursor]) && !valid_escaped_name {
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

// import_is_used reports whether anything in the import's file uses it;
// `file_nodes` lists the indices of that file's nodes, in order.
fn (tc &TypeChecker) import_is_used(import_id flat.NodeId, import_node flat.Node, file_nodes []int) bool {
	module_path := tc.import_module_path_text(import_node)
	module_path_id := flat.source_type_id(module_path)
	alias_id := import_node.typ
	alias_text := import_node.typ_text()
	module_c_prefix := '${module_path}.C.'
	mut selective_names := []string{cap: int(import_node.children_count)}
	mut selective_name_ids := []u32{cap: int(import_node.children_count)}
	for i in 0 .. import_node.children_count {
		child := tc.a.child_node(&import_node, i)
		if child.kind == .ident {
			selective_names << child.value
			selective_name_ids << flat.source_type_id(child.value)
		}
	}
	for idx in file_nodes {
		node := tc.a.nodes[idx]
		if idx == int(import_id) || node.kind == .import_decl || node.pos.id != import_node.pos.id {
			continue
		}
		mut is_selective_import_child := false
		for child_index in 0 .. import_node.children_count {
			if int(tc.a.child(&import_node, child_index)) == idx {
				is_selective_import_child = true
				break
			}
		}
		if is_selective_import_child {
			continue
		}
		value_text := node.value_text()
		if node.kind in [.call, .selector, .index, .infix, .assign, .selector_assign, .index_assign] {
			if resolved := tc.resolved_call_name(flat.NodeId(idx)) {
				if resolved.starts_with(module_c_prefix) {
					return true
				}
			}
		}
		if node.kind == .index && node.children_count >= 2 {
			parent_id := tc.direct_parent_id(flat.NodeId(idx))
			if int(parent_id) >= 0 {
				parent := tc.a.node(parent_id)
				if parent.kind in [.assign, .index_assign] && parent.op != .assign
					&& parent.children_count > 0 && tc.a.child(parent, 0) == flat.NodeId(idx) {
					if base_type := tc.expr_type(tc.a.child(&node, 0)) {
						if getter := tc.index_operator_call_info(base_type, '[]') {
							if getter.name.starts_with(module_c_prefix) { return true }
						}
					}
				}
			}
		}
		if node.kind == .for_in_stmt && node.value == '3' && node.children_count >= 3 {
			if container_type := tc.expr_type(tc.a.child(&node, 2)) {
				if info := tc.iterator_for_in_next_call_info(container_type) {
					if info.name.starts_with(module_c_prefix) {
						return true
					}
				}
			}
		}
		if node.kind == .selector && node.children_count > 0 {
			base := tc.a.child_node(&node, 0)
			if base.kind == .ident && base.value == alias_text {
				return true
			}
			if base.kind == .ident && base.value == 'C'
				&& tc.c_symbol_declared_in_module(module_path, 'C.${node.value}') {
				return true
			}
			selector_path := tc.expr_key(flat.NodeId(idx))
			if type_text_contains_qualified_import(selector_path, module_path) {
				return true
			}
		}
		if node.kind == .sql_expr {
			text := tc.sql_expression_import_text(node)
			if sql_text_contains_qualified_import(text, alias_text)
				|| sql_text_contains_qualified_import(text, module_path) {
				return true
			}
		}
		// A text without a dot holds no `alias.` or `module.` qualification.
		value_has_dot := value_text.index_u8(`.`) >= 0
		if node.kind != .sql_expr && (type_id_contains_qualified_import(node.typ, alias_id)
			|| (value_has_dot && type_text_contains_qualified_import(value_text, alias_text))
			|| type_id_contains_qualified_import(node.typ, module_path_id)
			|| (value_has_dot && type_text_contains_qualified_import(value_text, module_path))) {
			return true
		}
		// A module-qualified attribute name (`@[mod.Skip]`) is a use of the
		// module even though no selector expression names it (vlang/rfcs#35).
		if tc.node_uses_qualified_attribute(flat.NodeId(idx), alias_text, module_path) {
			return true
		}
		for constraint_id in node.generic_constraint_ids() {
			if type_id_contains_qualified_import(constraint_id, alias_id)
				|| type_id_contains_qualified_import(constraint_id, module_path_id) {
				return true
			}
			for name_id in selective_name_ids {
				if type_id_contains_symbol(constraint_id, name_id) {
					return true
				}
			}
		}
		if selective_names.len == 0 {
			continue
		}
		if node.kind == .ident && value_text in selective_names {
			return true
		}
		if node.kind == .call {
			// A body an incremental check left out has its calls put back.
			resolved := tc.resolved_call_name(flat.NodeId(idx)) or {
				tc.incremental_call_name(idx)
			}
			if resolved != '' {
				for name in selective_names {
					if resolved == '${import_node.value}.${name}'
						|| resolved.starts_with('${import_node.value}.${name}[') {
						return true
					}
				}
			}
		}
		for i, name in selective_names {
			if type_id_contains_symbol(node.typ, selective_name_ids[i])
				|| type_text_contains_symbol(value_text, name) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) c_symbol_declared_in_module(module_path string, name string) bool {
	for module_name in [module_path, module_path.all_after_last('.')] {
		if c_fn_module_signature_key(module_name, name) in tc.c_fn_module_ret_types {
			return true
		}
		if name_id := flat.source_type_existing_id(name) {
			if tc.type_alias_modules[name_id] or { '' } == module_name
				|| tc.struct_modules[name_id] or { '' } == module_name {
				return true
			}
		} else if module_name == '' {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) diagnostic_module_display_name(module_name string) string {
	for file, declared_module in tc.file_modules {
		if declared_module == module_name
			|| declared_module.all_after_last('.') == module_name.all_after_last('.') {
			return tc.diagnostic_module_name(module_name, file)
		}
	}
	return module_name
}

// sql_expression_import_text keeps the original literal boundaries. SQL's serialized
// tokens wrap raw literals in extra quotes, which cannot be lexed as ordinary source.
fn (tc &TypeChecker) sql_expression_import_text(node flat.Node) string {
	file := tc.a.source_files[node.pos.id] or { return node.value }
	source := tc.source_texts_by_file[file.name] or { return node.value }
	if node.pos.offset < 0 || node.pos.end > source.len || node.pos.end <= node.pos.offset {
		return node.value
	}
	return source[node.pos.offset..node.pos.end]
}

// SQL stores its expression as text; use V tokens to distinguish qualified names
// from raw/escaped literals and comments while retaining interpolation expressions.
fn sql_text_contains_qualified_import(text string, alias string) bool {
	if alias == '' {
		return false
	}
	mut files := token.FileSet.new()
	mut file := files.add_file('<sql import usage>', text.len)
	file.index_lines(text)
	mut lexer := scanner.new_scanner(pref.new_preferences(), .normal)
	lexer.init(file, text)
	mut qualified := ''
	mut after_dot := false
	for {
		tok := lexer.scan()
		if tok == .eof {
			break
		}
		if tok == .comment {
			continue
		}
		if tok == .dot && qualified.len > 0 {
			after_dot = true
			continue
		}
		if tok != .name && !tok.is_keyword() {
			qualified = ''
			after_dot = false
			continue
		}
		qualified = if after_dot { qualified + '.' + lexer.lit } else { lexer.lit }
		after_dot = false
		if qualified.starts_with('${alias}.') {
			return true
		}
	}
	return false
}

fn type_text_contains_qualified_import(text string, alias string) bool {
	if text.len <= alias.len || alias == '' {
		return false
	}
	// Find each `${alias}.` in turn, like an index search from past the previous
	// one, without copying the text or the needle.
	first := alias[0]
	last := text.len - alias.len - 1
	mut index := 0
	for index <= last {
		if text[index] == first && text[index + alias.len] == `.`
			&& unsafe { vmemcmp(text.str + index, alias.str, alias.len) } == 0 {
			if index == 0 || !is_type_symbol_byte(text[index - 1]) {
				return true
			}
			index += alias.len + 1
			continue
		}
		index++
	}
	return false
}

fn type_id_contains_qualified_import(type_id u32, alias_id u32) bool {
	if type_id == 0 || alias_id == 0 {
		return false
	}
	if !flat.source_type_shape(type_id).has(.dotted) {
		return false
	}
	for symbol in flat.source_type_syntax(type_id).symbols() {
		if flat.source_type_has_qualified_prefix(symbol, alias_id) {
			return true
		}
	}
	return false
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
	if node.kind == .import_decl {
		metadata := node.generic_params()
		if metadata.len > 0 && metadata[0].len > 0 {
			return metadata[0]
		}
	}
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
	return token.new_span(node.pos.id, alias_start, alias_start + node.typ_text().len)
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
		// The selected name and its candidates enter the import tables as spelling IDs.
		name_id := flat.source_type_id(child.value)
		tc.selective_import_files[tc.cur_file] = info
		if name_id in info.selective_imports {
			info.selective_imports[name_id] = []u32{}
			message := if tc.cur_module.len == 0 {
				'cannot register symbol `${child.value}`, it was already imported'
			} else {
				'ambiguous selective import `${child.value}`'
			}
			tc.record_error_unfiltered(.unknown_fn, message, child_id)
			continue
		}
		path_id := flat.source_type_id('${tc.import_module_path_text(node)}.${child.value}')
		alias_id := flat.source_type_id('${node.typ_text()}.${child.value}')
		info.selective_imports[name_id] = if alias_id != path_id {
			[path_id, alias_id]
		} else {
			[path_id]
		}
	}
}

// check_memos_env_enabled is read once: getenv takes the process-wide
// environment lock, which every fork would otherwise contend on.
const check_memos_env_enabled = os.getenv('V3_NO_CHECK_MEMOS') == ''

// check_memos_enabled gates the per-fork qualify/import memos, so a single
// binary can A/B them (`V3_NO_CHECK_MEMOS=1` disables).
fn check_memos_enabled() bool {
	return check_memos_env_enabled
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
	seen_len int             = -1
}

// Index source files, not resolved declarations: signature availability and
// module context can change during generic specialization.
struct SelectiveImportIndex {
mut:
	files_by_name map[u32][]string
	seen_len      int = -1
}

// selective_import_fallback_files returns the files that select `name_id`.
fn (tc &TypeChecker) selective_import_fallback_files(name_id u32) []string {
	mut index := tc.selective_import_index
	if isnil(index) {
		// Lightweight checker views without caches retain the same lookup rules.
		mut files := []string{}
		for file, info in tc.selective_import_files {
			if name_id in info.selective_imports {
				files << file
			}
		}
		return files
	}
	if index.seen_len != tc.selective_import_files.len {
		index.files_by_name.clear()
		for file, info in tc.selective_import_files {
			for selected_id, _ in info.selective_imports {
				index.files_by_name[selected_id] << file
			}
		}
		index.seen_len = tc.selective_import_files.len
	}
	return index.files_by_name[name_id] or { []string{} }
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
	// Most queried names are lexical bindings, not import aliases. The mask
	// rejects definite misses without hashing them; collisions still probe.
	// Hand-built checker views without the index keep their map lookup.
	if info.imports_mask_ready && info.imports_name_mask & scope_name_bit(alias) == 0 {
		return none
	}
	if mod := info.imports[alias] {
		return mod
	}
	return none
}

fn (tc &TypeChecker) resolve_selective_import_symbol(name string) ?string {
	// Selected names are interned when they are registered.
	name_id := flat.source_type_existing_id(name) or { return tc.vsh_os_fn_symbol(name) }
	candidates := tc.selective_import_candidates(name_id) or {
		// Generic specializations can be checked while their cloned file context is
		// active. Recover a selected symbol from the source file only when it has one
		// unambiguous declaration across the registered imports.
		if !tc.resolution_type_mode {
			return tc.vsh_os_fn_symbol(name)
		}
		resolved := tc.selective_import_fn_in_module_files(name_id) or { return none }
		if resolved.len > 0 {
			return resolved
		}
		return tc.vsh_os_fn_symbol(name)
	}
	for candidate_id in candidates {
		// Function tables are keyed by declaration names.
		candidate := flat.source_type_name(candidate_id)
		if tc.fn_signature_known(candidate) || candidate in tc.fn_ret_types
			|| candidate in tc.fn_param_types {
			return candidate
		}
	}
	return tc.vsh_os_fn_symbol(name)
}

// selective_import_fn_in_module_files returns the function that every file of
// the current module selecting `name_id` names, '' when none names one, and
// none when two files disagree.
fn (tc &TypeChecker) selective_import_fn_in_module_files(name_id u32) ?string {
	mut resolved := u32(0)
	for file in tc.selective_import_fallback_files(name_id) {
		if tc.file_modules[file] != tc.cur_module {
			continue
		}
		// Only matching imports need candidate values; copying every array here
		// accumulates allocations while resolving generic bodies.
		candidates := tc.file_selective_import_ids(file, name_id) or { []u32{} }
		for candidate_id in candidates {
			candidate := flat.source_type_name(candidate_id)
			if !tc.fn_signature_known(candidate) && candidate !in tc.fn_ret_types && candidate !in tc.fn_param_types {
				continue
			}
			if resolved != 0 && resolved != candidate_id {
				return none
			}
			resolved = candidate_id
		}
	}
	return flat.source_type_name(resolved)
}

// resolve_any_selective_import_fn resolves an unqualified selected function
// when every source file that selects the name agrees on the same declaration.
pub fn (tc &TypeChecker) resolve_any_selective_import_fn(name string) ?string {
	if name_id := flat.source_type_existing_id(name) {
		resolved := tc.selective_import_fn_in_module_files(name_id) or { return none }
		if resolved.len > 0 {
			return resolved
		}
	}
	return tc.vsh_os_fn_key(name)
}

// resolve_selective_import_type_symbol_id resolves a name selected by the
// current file to the spelling ID of the imported type.
fn (tc &TypeChecker) resolve_selective_import_type_symbol_id(name_id u32) ?u32 {
	candidates := tc.selective_import_candidates(name_id) or { return none }
	return tc.selective_import_known_type_candidate_id(candidates)
}

// resolve_selective_import_type_symbol_text_id resolves symbol text, such as an
// identifier from the AST, to the spelling ID of the selectively imported type.
fn (tc &TypeChecker) resolve_selective_import_type_symbol_text_id(name string) ?u32 {
	name_id := flat.source_type_existing_id(name) or { return none }
	return tc.resolve_selective_import_type_symbol_id(name_id)
}

// selective_import_known_type_candidate_id returns the spelling ID of the first
// candidate that names a collected type.
fn (tc &TypeChecker) selective_import_known_type_candidate_id(candidates []u32) ?u32 {
	for candidate_id in candidates {
		// Monomorphization erases generic struct templates from `structs`, but cgen
		// still has to resolve `Foo[Bar]` after `import foo { Foo }` to `foo.Foo`.
		if tc.type_symbol_known(candidate_id) || candidate_id in tc.struct_generic_params {
			return candidate_id
		}
	}
	return none
}

// vsh_os_fn_symbol resolves an unqualified call name inside a `.vsh` script to
// its `os` declaration. V script mode makes the `os` module global, but only as
// a last resort: a local, builtin or explicitly imported declaration with the
// same name keeps priority, exactly like in a plain `.v` file.
fn (tc &TypeChecker) vsh_os_fn_symbol(name string) ?string {
	if !tc.vsh_script_file() {
		return none
	}
	return tc.vsh_os_fn_key(name)
}

// vsh_os_fn_key applies the script-mode `os` lookup without requiring the script
// file itself to be the active one. Monomorphization collects candidate names for
// a generic call outside any file context, and a compilation only has script-mode
// symbols at all when it contains a `.vsh` script.
fn (tc &TypeChecker) vsh_os_fn_key(name string) ?string {
	if !tc.a.has_vsh_source || name == '' || name.contains('.') {
		return none
	}
	// Only a real V declaration shadows the script's `os` symbol. A bare name is
	// also registered for every `fn C.name()` declaration, and those must not hide
	// the V wrapper: `getenv('HOME')` in a script is `os.getenv`, not `C.getenv`.
	if name in tc.v_fn_semantic_names {
		return none
	}
	key := 'os.${name}'
	if tc.fn_signature_known(key) || key in tc.fn_ret_types || key in tc.fn_param_types {
		return key
	}
	return none
}

// vsh_os_const_key resolves an unqualified constant name inside a `.vsh` script
// to its `os` declaration, under the same last-resort rule as `os` functions.
fn (tc &TypeChecker) vsh_os_const_key(name string) ?string {
	if !tc.vsh_script_file() || name == '' || name.contains('.') {
		return none
	}
	key := 'os.${name}'
	if key in tc.const_types {
		return key
	}
	return none
}

// vsh_script_file reports whether the file being checked is a V script, whose
// `os` symbols are visible unqualified. The `.vsh` test is guarded by an
// AST-wide flag so non-script compilations never pay for it.
@[inline]
fn (tc &TypeChecker) vsh_script_file() bool {
	return tc.a.has_vsh_source
		&& (tc.cur_file.ends_with('.vsh') || tc.cur_file == tc.a.raw_vsh_file)
}

// selective_import_candidates returns the spelling IDs of the declarations that
// the current file can name with the selected `name_id`.
fn (tc &TypeChecker) selective_import_candidates(name_id u32) ?[]u32 {
	info := tc.current_file_import_info()
	if isnil(info) {
		return none
	}
	return info.selective_imports[name_id] or { return none }
}

// selective_import_text_candidates is selective_import_candidates for symbol
// text from the AST. Selected names are interned when they are registered, so
// a name that was never interned was never selected.
fn (tc &TypeChecker) selective_import_text_candidates(name string) ?[]u32 {
	name_id := flat.source_type_existing_id(name) or { return none }
	return tc.selective_import_candidates(name_id)
}

// file_selective_import_ids returns the spelling IDs of the declarations that
// `file` can name with the selected `name_id`; an empty list marks an ambiguous
// selection.
pub fn (tc &TypeChecker) file_selective_import_ids(file string, name_id u32) ?[]u32 {
	info := tc.selective_import_files[file] or { return none }
	return info.selective_imports[name_id] or { return none }
}

// file_selective_import_ids_by_name is file_selective_import_ids for symbol
// text from the AST.
pub fn (tc &TypeChecker) file_selective_import_ids_by_name(file string, name string) ?[]u32 {
	name_id := flat.source_type_existing_id(name) or { return none }
	return tc.file_selective_import_ids(file, name_id)
}

fn (tc &TypeChecker) type_symbol_known(name_id u32) bool {
	return tc.type_aliases.contains(name_id) || name_id in tc.structs || name_id in tc.interface_names
		|| name_id in tc.flag_enums || name_id in tc.enum_names || tc.sum_types.contains(name_id)
}

fn (tc &TypeChecker) type_symbol_spelling_known(name string) bool {
	name_id := flat.source_type_existing_id(name) or { return false }
	return tc.type_symbol_known(name_id)
}

fn (tc &TypeChecker) qualify_candidate_type_spelling_exists(name string) bool {
	name_id := flat.source_type_existing_id(name) or { return false }
	return tc.qualify_candidate_type_exists(name_id)
}

fn (tc &TypeChecker) unqualified_type_symbol_is_builtin(name string) bool {
	// A spelling that was never interned names no collected struct or enum.
	name_id := flat.source_type_existing_id(name) or {
		if name != 'IError' {
			return false
		}
		ierror_source_interface_name
	}
	return tc.unqualified_type_id_is_builtin(name_id)
}

fn (tc &TypeChecker) unqualified_type_id_is_builtin(name_id u32) bool {
	if mod_name := tc.struct_modules[name_id] {
		if mod_name != 'builtin' {
			return false
		}
	} else if name_id != ierror_source_interface_name && name_id !in tc.enum_names
		&& name_id !in tc.flag_enums {
		return false
	}
	// Builtin enum declarations use their unqualified source name, just like
	// builtin structs. Keep that name visible from imported modules unless a
	// declaration in the active scope shadows it.
	return !tc.unqualified_type_id_has_scoped_shadow(name_id)
}

fn (tc &TypeChecker) unqualified_type_id_has_scoped_shadow(name_id u32) bool {
	if name_id == 0 {
		return false
	}
	// The local probe must not intern a missing qualified spelling.
	name := flat.source_type_name(name_id)
	if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
		local := '${tc.cur_module}.${name}'
		if tc.type_symbol_spelling_known(local) {
			return true
		}
	}
	if tc.source_declares_type_in_scope(name_id, tc.cur_file, tc.cur_module) {
		return true
	}
	if _ := tc.file_selective_import_ids(tc.cur_file, name_id) {
		if candidates := tc.selective_import_candidates(name_id) {
			if resolved_id := tc.selective_import_known_type_candidate_id(candidates) {
				return !tc.unqualified_resolved_type_id_is_builtin(resolved_id)
			}
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) unqualified_resolved_type_id_is_builtin(name_id u32) bool {
	if mod_name := tc.struct_modules[name_id] {
		return mod_name == 'builtin'
	}
	return name_id == ierror_source_interface_name || name_id == ierror_source_builtin_interface_name
}

fn (tc &TypeChecker) type_from_known_symbol(name_id u32) ?Type {
	if target_id := tc.type_aliases.type_id(name_id) {
		return tc.parse_alias_type(name_id, target_id)
	}
	if name_id in tc.structs {
		return Type(Struct{
			name_id: name_id
		})
	}
	if name_id in tc.interface_names {
		return Type(Interface{
			name_id: name_id
		})
	}
	if name_id in tc.flag_enums {
		return Type(Enum{
			name_id: name_id
			is_flag: true
		})
	}
	if name_id in tc.enum_names {
		return Type(Enum{
			name_id: name_id
		})
	}
	if tc.sum_types.contains(name_id) {
		return Type(SumType{
			name_id: name_id
		})
	}
	return none
}

fn (tc &TypeChecker) selective_import_symbol_is_ambiguous(name string) bool {
	candidates := tc.selective_import_text_candidates(name) or { return false }
	return candidates.len == 0
}

fn (tc &TypeChecker) resolve_imported_type_text(typ_id u32) u32 {
	if !flat.source_type_shape(typ_id).has(.dotted) {
		return typ_id
	}
	syntax := flat.source_type_syntax(typ_id)
	if syntax.c_qualified() {
		return typ_id
	}
	if syntax.first_dot_prefix() == 0 {
		return typ_id
	}
	alias := flat.source_type_name(syntax.first_dot_prefix())
	// `main.Foo` is an explicit reference to a program-module type (bare-keyed),
	// not an import. Strip the prefix so an explicit generic argument locked to
	// `main` (see explicit_generic_concrete_arg_text) resolves to that program
	// type rather than staying a distinct `main.Foo` spelling or rebasing into
	// the active module. Only applies when `main` is not itself an import alias.
	if syntax.first_dot_prefix() == checker_source_main_module {
		if _ := tc.resolve_import_alias('main') {
		} else {
			rest_id := syntax.first_dot_suffix()
			// Transform qualifies main-module const/global references to
			// `main.<name>` for C codegen; the post-transform re-check must still
			// resolve them against the bare-keyed program symbol tables.
			if !flat.source_type_shape(rest_id).has(.dotted) {
				rest := flat.source_type_name(rest_id)
				if is_builtin_type_name_id(rest_id) || rest_id == qualify_source_type_uint || tc.qualify_candidate_type_exists(rest_id)
					|| rest in tc.const_types {
					return rest_id
				}
				if tc.file_scope.contains(rest) {
					return rest_id
				}
			}
		}
	}
	if resolved := tc.resolve_import_alias(alias) {
		if resolved != alias {
			expanded_id := flat.source_type_composition_two(.qualified, flat.source_type_id(resolved), syntax.first_dot_suffix())
			// A file's import map can bind a bare first path segment to a relative
			// candidate: inside `gpu.agx.file`, `gpu` maps to `gpu.agx.gpu`. Expanding
			// that against an already-resolved `gpu.agx.bo.Table` rebases the name onto
			// itself, once per remaining segment, until cgen emits a C type name no
			// typedef declares.
			//
			// Deciding by prefix alone is not enough: a file that imports `benchmark as
			// jj` beside `x.benchmark` has `benchmark` as an import value too, and the
			// source spelling `benchmark.Benchmark` does still have to expand, to
			// `x.benchmark.Benchmark`. So ask which of the two names the program
			// actually declares, and only keep the original when the expansion names
			// nothing and it does. When neither resolves, expansion stands, as before.
			if tc.import_type_text_resolves(expanded_id) {
				return expanded_id
			}
			if tc.import_type_text_resolves(typ_id) {
				return typ_id
			}
			return expanded_id
		}
	}
	return typ_id
}

// import_type_text_resolves reports whether `text_id` names something this program
// declares -- a type, or a function or method key. Used to choose between a
// spelling and its alias expansion.
fn (tc &TypeChecker) import_type_text_resolves(text_id u32) bool {
	if text_id == 0 {
		return false
	}
	if tc.qualify_candidate_type_exists(text_id) {
		return true
	}
	return tc.fn_signature_known(flat.source_type_name(text_id))
}

// imported_type_short_name returns the semantic short name for an active imported
// module prefix. Some legacy module collections intentionally retain short symbols;
// qualified source spelling still has to resolve to those symbols.
fn (tc &TypeChecker) imported_type_short_name(typ_id u32) ?u32 {
	if !flat.source_type_shape(typ_id).has(.dotted) { return none }
	syntax := flat.source_type_syntax(typ_id)
	if syntax.c_qualified() {
		return none
	}
	if syntax.first_dot_prefix() == 0 {
		return none
	}
	if _ := tc.resolve_import_alias(flat.source_type_name(syntax.first_dot_prefix())) {
		short_id := syntax.unqualified()
		// `main` and `builtin` declarations are also keyed by their short names, but
		// a module qualifier never reaches them: `baz.Foo` must not resolve to the
		// `Foo` of `main` when module `baz` declares no such type.
		if short_id in tc.first_type_declaration_ids {
			return none
		}
		return short_id
	}
	return none
}

// has_active_import reports whether has active import applies in types.
fn (tc &TypeChecker) has_active_import(alias string) bool {
	info := tc.current_file_import_info()
	if isnil(info) {
		return false
	}
	if info.imports_mask_ready && info.imports_name_mask & scope_name_bit(alias) == 0 {
		return false
	}
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
	text u32
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
			name:         name
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
	// v.workers: `C.pthread_join(thread C.pthread_t, ...)`). The
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
			name:             name
			ret_type:         ret_type
			shared_params:    shared_params
			is_variadic:      is_variadic
			implicit_veb_ctx: implicit_veb_ctx
			file:             tc.cur_file
			write_file:       write_file
		}
	} else {
		tc.register_generated_fn_return_type(name, ret_type)
		if write_file {
			tc.fn_type_files[name] = tc.cur_file
		}
		tc.register_fn_ancillary_owned(name, shared_params, is_variadic, implicit_veb_ctx)
	}
}

fn (mut tc TypeChecker) register_fn_ancillary_owned(name string, shared_params []bool, is_variadic bool, implicit_veb_ctx bool) {
	tc.disable_call_flag_bloom()
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

// register_generated_fn_return_type records a return signature and invalidates its transform memos.
pub fn (mut tc TypeChecker) register_generated_fn_return_type(name string, ret Type) {
	tc.fn_ret_types[name] = ret
	tc.fn_ret_types_revision++
}

// unregister_generated_fn_return_type removes a return signature and invalidates its transform memos.
pub fn (mut tc TypeChecker) unregister_generated_fn_return_type(name string) {
	tc.fn_ret_types.delete(name)
	tc.fn_ret_types_revision++
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
	tc.receiver_method_short_names = map[string]bool{}
	tc.collection_method_short_names = map[string]bool{}
	tc.generic_receiver_method_index = map[string][]string{}
	tc.receiver_method_suffix_index.reserve(u32(tc.fn_param_types.len * 3))
	for name, _ in tc.fn_param_types {
		tc.add_receiver_method_suffix_index(name)
	}
}

fn (mut tc TypeChecker) register_fn_ret_type_text(name string, text_id u32) {
	if tc.defer_fn_ancillary {
		tc.fn_ret_text_registrations << FnTextRegistration{
			name: name
			text: text_id
		}
		return
	}
	tc.fn_ret_type_texts.set_id(name, text_id)
}

fn (mut tc TypeChecker) add_receiver_method_suffix_index(name string) {
	if name == '' { return }
	// Plain functions and C-lowered aliases cannot form receiver.method keys.
	// Probe the declaration name before entering the type interner; dotted
	// semantic methods already install both their full and short suffixes.
	if tc.method_suffix_prescreen && !name.contains('.') { return }
	// Split the registered name at its last raw dot, exactly as the source-type
	// syntax view does, instead of interning and parsing every declaration
	// spelling. Map insertion owns these borrowed views of `name`.
	last_dot := name.last_index_u8(`.`)
	method := if last_dot < 0 {
		name
	} else {
		unsafe { name.substr_unsafe(last_dot + 1, name.len) }
	}
	receiver := if last_dot < 0 { name } else { unsafe { name.substr_unsafe(0, last_dot) } }
	tc.receiver_method_short_names[method] = true
	if method.len > 1 && method[0] == `@` {
		tc.receiver_method_short_names[unsafe { method.substr_unsafe(1, method.len) }] = true
	}
	if receiver.contains('[]') || receiver.contains('map[') {
		tc.collection_method_short_names[method] = true
		if method.len > 1 && method[0] == `@` {
			tc.collection_method_short_names[unsafe { method.substr_unsafe(1, method.len) }] = true
		}
	}
	receiver_bracket := receiver.index_u8(`[`)
	if receiver_bracket >= 0 && receiver.ends_with(']') {
		mut indexed := tc.generic_receiver_method_index[method] or { []string{} }
		if name !in indexed {
			indexed << name
			tc.generic_receiver_method_index[method] = indexed
			tc.clear_generic_receiver_pattern_cache()
		}
	}
	tc.set_receiver_method_suffix_index(name, name)
	if last_dot >= 0 && receiver_bracket > 0 {
		// `Foo[T].method` is also reachable as `Foo.method`.
		receiver_base := unsafe { receiver.substr_unsafe(0, receiver_bracket) }
		tc.set_receiver_method_suffix_index('${receiver_base}.${method}', name)
	}
	for i in 0 .. name.len - 1 {
		if name[i] == `.` {
			tc.set_receiver_method_suffix_index(unsafe { name.substr_unsafe(i + 1, name.len) }, name)
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
	if key == '' {
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
	if name == '' {
		return
	}
	lowered_name := tc.cached_c_name(name)
	tc.register_c_variadic_fn_with_lowered(name, lowered_name)
}

fn (mut tc TypeChecker) register_c_variadic_fn_with_lowered(name string, lowered_name string) {
	if name == '' {
		return
	}
	if tc.defer_fn_ancillary {
		tc.fn_c_variadic_registrations << FnNamePairRegistration{
			name:         name
			lowered_name: lowered_name
		}
		return
	}
	tc.disable_call_flag_bloom()
	tc.c_variadic_fns[name] = true
	if lowered_name != name {
		tc.c_variadic_fns[lowered_name] = true
	}
	if !name.starts_with('C.') {
		tc.c_variadic_fns['C.${name}'] = true
	}
}

fn (mut tc TypeChecker) insert_fn_param_binding(id flat.NodeId, p flat.Node) {
	if p.kind != .param || p.value.len == 0 {
		return
	}
	tc.check_local_binding_global_shadowing(id)
	parsed_type := tc.parse_scope_param_type(p.typ)
	translated_pointer_alias := p.is_mut && p.op != .amp && tc.node_is_in_translated_file(id)
		&& parsed_type is Pointer && parsed_type.base_type is Alias
		&& unalias_type(parsed_type.base_type) is Pointer
	typ := if translated_pointer_alias {
		parsed_type
	} else {
		mut_param_binding_type(parsed_type, p.is_mut, p.op == .amp)
	}
	owner := tc.cur_scope.insert_with_owner(p.value, typ)
	tc.initialize_pointer_parameter_binding(owner, typ)
	if p.is_mut {
		// An explicit `mut value &T` parameter mutates a caller-owned pointer slot,
		// so reads in the function body retain the semantic `&T` value. Ordinary
		// `mut value T` parameters still read through their implicit ABI pointer.
		tc.fn_context.mut_param_base_types[p.value] = if p.op == .amp {
			typ
		} else {
			mut_param_base_type(typ)
		}
		tc.fn_context.mut_param_owners[p.value] = owner
	}
	if param_source_type_is_shared(p.typ) {
		tc.mark_shared_binding_owner(p.value, owner)
		if unalias_and_unwrap_pointer_type(parsed_type) is Array {
			tc.mark_shared_array_binding_owner(p.value, owner)
		}
	} else if source_type_is_shared_array(p.typ) {
		tc.mark_shared_array_binding_owner(p.value, owner)
	}
}

fn source_type_is_shared_array(type_id u32) bool {
	mut clean_id := trimmed_source_type_id(type_id)
	if flat.source_type_shape(clean_id).has(.pointer) {
		clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).pointer_raw_base())
	}
	return flat.source_type_shape(clean_id).has(.array_type)
		&& flat.source_type_shape(flat.source_type_syntax(clean_id).raw_base()).has(.shared_type)
}

fn mut_param_type_is_allowed(typ &Type) bool {
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
		return *typ.base_type
	}
	return typ
}

// mut_param_semantic_type returns the type of a `mut` parameter declared as `typ` (`&T`).
// A `mut a ?T` is `&?&T`, whatever its payload: the option the callee gets refers to the
// caller's value, so changes made through it reach the caller, as in V 0.5.2. The call
// site builds that option (`transform_mut_optional_value_call_arg` for an option argument,
// `transform_mut_plain_value_optional_call_arg` for a plain value). Every other type is
// returned unchanged, including an option payload that is already a pointer.
pub fn mut_param_semantic_type(typ Type) Type {
	if typ !is Pointer {
		return typ
	}
	pointer_type := typ as Pointer
	declared_base := *pointer_type.base_type
	if declared_base !is OptionType {
		return typ
	}
	option_type := declared_base as OptionType
	if option_type.base_type is Pointer {
		return typ
	}
	return Type(Pointer{
		base_type: new_type_reference(Type(OptionType{
			base_type: new_type_reference(Type(Pointer{
				base_type: option_type.base_type
			}))
		}))
	})
}

// mut_optional_param_view_type returns `?T` for a `mut a ?T` parameter (`typ` is `&?T`).
// Its body reads it as the `?T` it declares, whatever the payload, although it is passed
// as `&?&T` (see mut_param_semantic_type): the transform reads the payload through the
// reference, so that reads copy it out, while changes made through `if mut v := a`, or
// through `a` smartcast to its payload, reach the caller.
fn mut_optional_param_view_type(typ Type) ?Type {
	if typ !is Pointer {
		return none
	}
	pointer_type := typ as Pointer
	declared_base := *pointer_type.base_type
	if declared_base !is OptionType {
		return none
	}
	option_type := declared_base as OptionType
	if option_type.base_type is Pointer {
		return none
	}
	return declared_base
}

fn mut_param_binding_type(typ Type, is_mut bool, is_explicit_reference bool) Type {
	if !is_mut || is_explicit_reference {
		return typ
	}
	if typ is Pointer {
		if view_type := mut_optional_param_view_type(typ) {
			return view_type
		}
		semantic_type := mut_param_semantic_type(typ)
		if semantic_type is Pointer {
			if unalias_type(semantic_type.base_type) is Interface {
				return semantic_type
			}
			return *semantic_type.base_type
		}
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
	for idx, node in tc.a.nodes {
		if node.kind == .file {
			tc.enter_file(node.value)
		} else if node.kind == .module_decl {
			tc.enter_module(node.value)
		} else if node.kind == .fn_decl {
			if !tc.should_annotate_fn(node, used_fns) {
				continue
			}
			tc.annotate_fn_node(flat.NodeId(idx), node)
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
			tc.annotate_fn_node(flat.NodeId(idx), node)
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
fn (mut tc TypeChecker) annotate_fn_node(id flat.NodeId, node flat.Node) {
	saved_fn_context := tc.fn_context
	tc.fn_context = new_function_check_context()
	// Lowered bodies still need their source parameter metadata during annotation.
	tc.fn_context.node_id = int(id)
	tc.fn_context.concrete_generic_receiver_specialization =
		fn_value_is_concrete_generic_receiver_specialization(node.value)
	tc.fn_context.generic_params = tc.infer_decl_generic_param_ids(node)
	tc.fn_context.return_type = tc.parse_type(node.typ)
	tc.cur_scope = tc.file_scope
	tc.push_scope()
	for pi in 0 .. node.children_count {
		param_id := tc.a.child(&node, pi)
		tc.insert_fn_param_binding(param_id, tc.a.node(param_id))
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
	is_fn       bool
	name        string
	qname       string
	file        string
	file_module string
	node_id     flat.NodeId
	position    token.Pos
mut:
	alive bool
}

// diagnose_unused_private_declarations reports private declarations that are not reachable from used code.
pub fn (mut tc TypeChecker) diagnose_unused_private_declarations(used_fns map[string]bool) {
	if tc.errors.len > 0 || !tc.has_main_module_fn_main() {
		return
	}
	tc.report_unused_private_declarations(used_fns, false)
}

// diagnose_unused_library_private_declarations reports the unused private
// declarations of a library, a module checked on its own, as an editor checks a
// module that no program imports, and tells whether the check is one. Its public
// declarations are what other programs use, and what a private one serves cannot
// be told without a program: one that is named at all counts as used, as one
// exported to C or kept with `@[markused]` does.
pub fn (mut tc TypeChecker) diagnose_unused_library_private_declarations() bool {
	if !tc.is_library_check() {
		return false
	}
	used, _ := tc.named_private_fns()
	tc.report_unused_private_declarations(used, true)
	return true
}

// is_library_check reports whether the checked files are a library: no main
// function, and no test or script, which are programs of their own.
fn (tc &TypeChecker) is_library_check() bool {
	if tc.has_main_module_fn_main() || tc.a.has_vsh_source || tc.has_c_test_harness_main() {
		return false
	}
	for file, _ in tc.diagnostic_files {
		if file.ends_with('_test.v') {
			return false
		}
	}
	return true
}

// diagnose_unused_private_declarations_with_errors reports, in a program with
// errors, the private functions and constants that nothing names. What is
// reachable cannot be told there: a call the checker could not resolve would
// leave its function looking unused. So a function that is called or named at
// all counts as used, and no notice is one that fixing the errors takes back.
pub fn (mut tc TypeChecker) diagnose_unused_private_declarations_with_errors() {
	if tc.diagnose_unused_library_private_declarations() || !tc.has_main_module_fn_main() {
		return
	}
	used, _ := tc.named_private_fns()
	tc.report_unused_private_declarations(used, false)
}

// report_unused_private_declarations records the notices; for a `library`, see
// unused_private_declarations.
fn (mut tc TypeChecker) report_unused_private_declarations(used_fns map[string]bool, library bool) {
	unused := tc.unused_private_declarations(used_fns, library)
	// The diagnostic filter and recorded file both read tc.cur_file; replay each
	// candidate's file so deferred emission matches the former in-walk emission.
	walk_end_file := tc.cur_file
	// record_notice_at finds a repeated notice by scanning every recorded one, which costs
	// n^2 comparisons for n unused declarations. This batch looks repeats up in a set.
	mut seen := map[string]bool{}
	recorded := if tc.notes_are_errors { tc.errors } else { tc.notices }
	for e in recorded {
		seen[notice_repeat_key(e.kind, e.msg, e.pos)] = true
	}
	for cand in unused {
		tc.cur_file = cand.file
		if cand.is_fn {
			tc.record_notice_once_at(.unknown_ident, 'unused function: `${cand.name}`', cand.node_id, tc.node_value_diagnostic_pos(cand.node_id), mut seen)
		} else {
			tc.record_notice_once_at(.unknown_ident, 'unused constant: `${cand.name}`', cand.node_id, cand.position, mut seen)
		}
	}
	tc.cur_file = walk_end_file
}

// notice_repeat_key identifies a notice by what record_notice_at compares: its kind,
// message and position.
fn notice_repeat_key(kind TypeErrorKind, msg string, pos token.Pos) string {
	return '${int(kind)}:${pos.id}:${pos.offset}:${pos.end}:${pos.meta}:${msg}'
}

// record_notice_once_at is record_notice_at for a batch of notices. `seen` holds the
// notice_repeat_key of every notice recorded so far, so a repeat is a lookup, not a scan.
fn (mut tc TypeChecker) record_notice_once_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos, mut seen map[string]bool) {
	if !tc.should_diagnose_notice(node) {
		return
	}
	key := notice_repeat_key(kind, msg, pos)
	if key in seen {
		return
	}
	seen[key] = true
	base := tc.make_type_error_at(kind, msg, node, pos)
	if tc.notes_are_errors {
		tc.errors << TypeError{
			...base
			severity: 'error:'
		}
		return
	}
	tc.notices << base
}

// used_fns_without_markused returns what a check can take as the used functions
// without running markused: the private functions that are never called but are
// named as values, such as callbacks. It returns none when a private function is
// not named at all, since only markused can tell whether that one is used (an
// exported one is). A function named only by unused code counts as used here,
// where a build, which knows what is reachable, can report it.
pub fn (mut tc TypeChecker) used_fns_without_markused() ?map[string]bool {
	if tc.errors.len > 0 || !tc.has_main_module_fn_main() {
		return map[string]bool{}
	}
	used, all_named := tc.named_private_fns()
	if !all_named {
		return none
	}
	return used
}

// named_private_fns returns the private functions that are never called but are
// named, as values such as callbacks, and whether every one of them is named.
fn (mut tc TypeChecker) named_private_fns() (map[string]bool, bool) {
	mut used := map[string]bool{}
	// Markused may run next, and resolves names in the current file and module.
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	mut uncalled := []UnusedDeclCandidate{}
	mut keys := map[string][]int{}
	for cand in tc.unused_private_declarations(map[string]bool{}, false) {
		tc.cur_file = cand.file
		if !cand.is_fn || !tc.should_diagnose(cand.node_id) {
			continue
		}
		keys[cand.name] << uncalled.len
		if cand.qname != cand.name {
			keys[cand.qname] << uncalled.len
		}
		uncalled << cand
	}
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	if uncalled.len == 0 {
		return used, true
	}
	mut named := []bool{len: uncalled.len}
	names_start := tc.prepared_names_start()
	if names_start > 0 {
		for key, hits in keys {
			if key in tc.prepared_collect.named {
				for idx in hits {
					named[idx] = true
				}
			}
		}
	}
	for node in tc.a.nodes[names_start..] {
		if node.kind !in [.ident, .selector] || node.value.len == 0 {
			continue
		}
		if hits := keys[node.value] {
			for idx in hits {
				named[idx] = true
			}
		}
		short_name := short_name_view(node.value)
		if short_name.len != node.value.len {
			if hits := keys[short_name] {
				for idx in hits {
					named[idx] = true
				}
			}
		}
	}
	mut all_named := true
	for idx, cand in uncalled {
		if !named[idx] {
			all_named = false
			continue
		}
		used[cand.qname] = true
	}
	return used, all_named
}

// diagnosed_files_declare_generics reports whether a file whose diagnostics are
// reported declares a generic function or method: only then can a concrete
// instance of the program's own code fail where the generic one did not.
pub fn (mut tc TypeChecker) diagnosed_files_declare_generics() bool {
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	mut found := false
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			continue
		}
		if node.kind == .fn_decl && tc.cur_file in tc.diagnostic_files
			&& tc.infer_decl_generic_param_ids(node).len > 0 {
			found = true
			break
		}
	}
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	return found
}

// diagnosed_files_call_library_generics reports whether a function of a file
// whose diagnostics are reported calls a generic function that a file outside
// of them declares, as `arrays.sum(foos)` does: the check of the instances that
// the program asks for checks that instance too (see
// check_program_lib_instances).
pub fn (tc &TypeChecker) diagnosed_files_call_library_generics() bool {
	mut diagnosed := false
	mut stack := []flat.NodeId{}
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			// Once per file: most top-level nodes are the library's.
			diagnosed = node.value in tc.diagnostic_files
			continue
		}
		if !diagnosed || node.kind != .fn_decl {
			continue
		}
		stack << flat.NodeId(idx)
		for stack.len > 0 {
			id := stack.pop()
			current := tc.a.nodes[int(id)]
			if current.kind == .call && tc.calls_library_generic(id) {
				return true
			}
			for i in 0 .. current.children_count {
				child := tc.a.child(&current, i)
				if int(child) >= 0 && int(child) < tc.a.nodes.len {
					stack << child
				}
			}
		}
	}
	return false
}

// calls_library_generic reports whether the call `id` resolves to a generic
// function that a file outside of the diagnosed ones declares.
fn (tc &TypeChecker) calls_library_generic(id flat.NodeId) bool {
	name := tc.resolved_call_name(id) or { return false }
	if (tc.fn_generic_params[name] or { return false }).len == 0 {
		return false
	}
	return (tc.fn_type_files[name] or { return false }) !in tc.diagnostic_files
}

// diagnosed_fn_keys returns every name that a function of a diagnosed file goes
// by in a set of used functions. A check monomorphizes from them alone: walking
// the rest of the program to find what is reachable would check the library
// bodies that the diagnostics server leaves out.
pub fn (mut tc TypeChecker) diagnosed_fn_keys() map[string]bool {
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	mut keys := map[string]bool{}
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
		if node.kind != .fn_decl || tc.cur_file !in tc.diagnostic_files {
			continue
		}
		qname := checker_qualified_fn_name(module_name, node.value)
		keys[node.value] = true
		keys[qname] = true
		keys[tc.cached_c_name(qname)] = true
	}
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	return keys
}

// unused_private_declarations returns the private functions that are not in
// `used_fns` and that nothing calls, and the private constants nothing names.
// Of a `library`, the public constants are left out, since other programs use
// them, and so are the functions exported to C or kept with `@[markused]`.
fn (mut tc TypeChecker) unused_private_declarations(used_fns map[string]bool, library bool) []UnusedDeclCandidate {
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
	// The files of each module, by the module identity the checker resolved, for
	// the skipped `$if` branch records, which are keyed by file.
	mut module_files := map[string][]string{}
	mut file_module := ''
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .file {
			tc.enter_file(node.value)
			file_module = if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
			module_files[file_module] << tc.cur_file
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if tc.translated_files[tc.cur_file] {
			continue
		}
		if node.kind == .fn_decl {
			if node.op == .arrow || node.value in ['main', 'init', 'cleanup']
				|| is_v_test_fn_name(node.value) || node.value.starts_with('__anon_fn_')
				|| node.value.contains('.') || node.is_static_type_method() {
				continue
			}
			// Only a declaration whose diagnostics show can be reported: skipping
			// the others here spares the reference scan their names.
			if !tc.should_diagnose(flat.NodeId(idx)) || tc.declaration_contains_error(node) {
				continue
			}
			if library && (tc.declaration_has_attribute(flat.NodeId(idx), 'export')
				|| tc.declaration_has_attribute(flat.NodeId(idx), 'markused')) {
				continue
			}
			qname := checker_qualified_fn_name(module_name, node.value)
			cname := tc.cached_c_name(qname)
			if used_fns[node.value] || used_fns[qname] || used_fns[cname] {
				continue
			}
			cand_idx := candidates.len
			candidates << UnusedDeclCandidate{
				is_fn:       true
				name:        node.value
				qname:       qname
				file:        tc.cur_file
				file_module: file_module
				node_id:     flat.NodeId(idx)
			}
			fn_keys[node.value] << cand_idx
			if qname != node.value {
				fn_keys[qname] << cand_idx
			}
			continue
		}
		if node.kind != .const_decl || (library && node.op == .arrow) {
			continue
		}
		if node.op == .arrow {
			continue
		}
		for i in 0 .. node.children_count {
			field_id := tc.a.child(&node, i)
			field := tc.a.node(field_id)
			if field.kind != .const_field || field.value.len == 0 || field.value.starts_with('C.')
				|| field.value.starts_with('_') || !tc.should_diagnose(field_id) {
				continue
			}
			qname := if module_name.len > 0 && module_name != 'main' {
				'${module_name}.${field.value}'
			} else {
				field.value
			}
			cand_idx := candidates.len
			candidates << UnusedDeclCandidate{
				is_fn:       false
				name:        field.value
				qname:       qname
				file:        tc.cur_file
				file_module: file_module
				node_id:     field_id
				position:    field.pos
			}
			const_keys[field.value] << cand_idx
			if qname != field.value {
				const_keys[qname] << cand_idx
			}
		}
	}
	if candidates.len == 0 {
		return []UnusedDeclCandidate{}
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
	// A private declaration named in a `$if` branch this build does not take is
	// used there (see used_in_skipped_comptime_branch).
	return candidates.filter(!it.alive && !tc.used_in_skipped_comptime_branch(it, module_files))
}

// used_in_skipped_comptime_branch reports whether a file of the candidate's own
// module spells its name in a `$if` branch this build does not take. Such a
// body is never parsed, so only the parser's record of the skipped names shows
// the use, and a private declaration cannot be used from any other module.
fn (tc &TypeChecker) used_in_skipped_comptime_branch(cand UnusedDeclCandidate, module_files map[string][]string) bool {
	if tc.a.comptime_skipped_decl_names.len == 0 {
		return false
	}
	for file in module_files[cand.file_module] {
		if flat.comptime_skipped_decl_key(file, cand.name) in tc.a.comptime_skipped_decl_names {
			return true
		}
	}
	return false
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
	if is_shared || tc.a.has_vsh_source || tc.has_c_test_harness_main() {
		return
	}
	// A source file without an explicit `module` declaration belongs to `main`.
	// Such a file has no module_decl node for the scan below to discover.
	if tc.has_main_module_fn_main() {
		return
	}
	for file, _ in tc.diagnostic_files {
		if file.ends_with('_test.v') || (tc.file_modules[file] or { '' }) in ['', 'main'] {
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
	mut has_export := false
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
		if tc.cur_file !in tc.diagnostic_files {
			continue
		}
		if node.kind == .fn_decl {
			// A program that exports functions is a library for C code, so it needs
			// no `main`. Only its own exports count: builtin exports C functions that
			// musl lacks (`backtrace`, `data_start`), and so does stbi.
			if !has_export && tc.is_exported_fn(tc.cur_module, node.value) {
				has_export = true
			}
			continue
		}
		if node.kind != .module_decl {
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
	if has_main || has_export || has_postinclude || int(first_module_id) < 0
		|| tc.translated_files[first_module_file] {
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
	if mod == '' || mod == 'main' || mod == 'builtin' {
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
		// A function literal's return type is resolved with its parameters bound.
		if node.kind != .fn_literal && inferred_type_source(node.typ) {
			_ = tc.resolve_inferred_type_id(node.typ)
		}
		// Only a spelling with `typeof(__expr_` is an inferred type source:
		// test that before interning every identifier's name.
		if node.kind == .ident
			&& (node.value.len == 0 || node.value.contains('typeof(__expr_')) {
			source_id := node.source_value_id()
			if inferred_type_source(source_id) {
				_ = tc.resolve_inferred_type_id(source_id)
			}
		}
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
						mut typ := builtin_void_type
						if node.children_count == 2 && node.typ != 0 {
							typ = tc.parse_type(node.typ)
							tc.annotate_expected_expr(rhs_id, typ)
						} else {
							raw_type := tc.resolve_type(rhs_id)
							typ = tc.mut_param_expr_base(rhs_id, raw_type) or { raw_type }
						}
						if typ !is MultiReturn && typ !is Void {
							owner := tc.cur_scope.insert_with_owner(lhs.value, typ)
							if lhs.value != '_' && tc.decl_lhs_is_mut(node, lhs_id) {
								tc.prepare_mut_local_owner_write()
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
			.lock_expr {
				tc.annotate_lock_expr(node)
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
					// Keep the return type specialized from the call's own arguments;
					// the open signature (`fn load[T](value &T) T`) would leave a local
					// such as `value := load(&x)` unknown for later calls in the body.
					if info := tc.annotate_call_expected_exprs(current_id, node) {
						tc.remember_expr_type(current_id, info.return_type)
					}
					if dsl_name.len > 0 {
						tc.pop_scope()
					}
					continue
				}
				tc.annotate_call_expected_exprs(current_id, node) or {}
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
				tc.prepare_mut_local_owner_write()
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
	tc.push_scope()
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		tc.insert_fn_param_binding(child_id, tc.a.node(child_id))
	}
	// The return type may name the parameters through `typeof(expr)`.
	tc.fn_context.return_type = tc.parse_type(node.typ)
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
	mut value_id := id
	for tc.valid_node_id(value_id) {
		value := tc.a.nodes[int(value_id)]
		if value.kind == .enum_val {
			_ = tc.resolve_expr(id, expected)
			break
		}
		if value.kind !in [.paren, .expr_stmt] || value.children_count != 1 {
			break
		}
		value_id = tc.a.child(&value, 0)
	}
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
		if parent.kind == .call {
			for i in 1 .. parent.children_count {
				if tc.call_arg_value(tc.a.child(parent, i)) == id {
					expected := tc.builtin_array_annotation_expected_type(parent, i)
					if expected !is Void {
						return expected
					}
					break
				}
			}
		}
		// `[1, 2]!` is the fixed array its literal builds: where a fixed array is
		// expected, the literal is typed in that context, as an untyped `[1, 2]` is.
		// Elsewhere it keeps its own type, so that `[5.0]!` for a `[]int` is reported
		// as a fixed array, not as an element.
		if parent.kind == .postfix && parent.op == .not && tc.a.node(id).kind == .array_literal {
			if expected_parent := tc.expected_context_for_expr(parent_id) {
				if unalias_type(contextual_payload_type(expected_parent) or { expected_parent }) is ArrayFixed {
					return expected_parent
				}
			}
		}
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
							*expected_map.key_type
						} else {
							*expected_map.value_type
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
					// Translated arrays decay to pointers to their elements. The
					// pointee is not the type of the whole array literal.
					if tc.node_is_in_translated_file(id) {
						return tc.expr_type(id)
					}
					other_type = other_type.base_type
				}
				clean_other := unalias_type(other_type)
				if array_like_elem_type(other_type) != none
					|| (clean_other is Enum && clean_other.is_flag) {
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

fn (mut tc TypeChecker) annotate_lock_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	mut locked_names := []string{}
	for i in 0 .. node.children_count - 1 {
		object_id := tc.a.child(&node, i)
		tc.annotate_node(object_id)
		lock_name := tc.shared_lock_key(object_id)
		if lock_name.len == 0 {
			continue
		}
		mut modes := (tc.fn_context.locked_shared_modes[lock_name] or { []u8{} }).clone()
		modes << tc.lock_object_mode(node, i)
		tc.fn_context.locked_shared_modes[lock_name] = modes
		locked_names << lock_name
	}
	tc.lock_depth++
	tc.annotate_node(tc.a.child(&node, node.children_count - 1))
	tc.lock_depth--
	for name in locked_names {
		mut modes := (tc.fn_context.locked_shared_modes[name] or { []u8{} }).clone()
		if modes.len > 0 {
			modes.delete_last()
		}
		if modes.len == 0 {
			tc.fn_context.locked_shared_modes.delete(name)
		} else {
			tc.fn_context.locked_shared_modes[name] = modes
		}
	}
}

fn (mut tc TypeChecker) annotate_struct_init_expected_exprs(node flat.Node) {
	init_source_id := node.source_value_id()
	init_type := tc.parse_type(init_source_id)
	init_struct := struct_type_from_type(init_type) or { return }
	init_name_id := tc.struct_init_field_lookup_name(init_source_id, init_struct.source_type_id())
	fields := tc.struct_fields_for_init(init_name_id)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field, 0)
		mut expected := builtin_void_type
		if field.value.len > 0 {
			expected = tc.struct_field_type(init_name_id, field.value) or { builtin_void_type }
		} else if i < fields.len {
			expected = fields[i].typ
		}
		tc.annotate_expected_expr(value_id, expected)
	}
}

// annotate_call_expected_exprs records expected types for the call arguments and
// returns the call info specialized from those arguments.
fn (mut tc TypeChecker) annotate_call_expected_exprs(id flat.NodeId, node flat.Node) ?CallInfo {
	for i in 1 .. node.children_count {
		expected := tc.builtin_array_annotation_expected_type(node, i)
		if expected !is Void {
			tc.annotate_expected_expr(tc.call_arg_value(tc.a.child(&node, i)), expected)
		}
	}
	previous_call_name := tc.cached_resolved_call(id) or { '' }
	mut info0 := tc.resolve_call_info(id, node) or { return none }
	if previous_call_name != info0.name && tc.c_backed_alias_method_name(previous_call_name) {
		// The transformed receiver has the C ABI type, but its V alias selected this method.
		info0 = if node.value.len > 0 {
			tc.explicit_generic_call_info(previous_call_name, true,
				flat.TypeTextList.from_names(split_generic_arg_list(node.value)).ids) or { tc.call_info(previous_call_name, true) }
		} else {
			tc.call_info(previous_call_name, true)
		}
	}
	info := tc.builtin_copy_call_info(id, node, tc.specialized_plain_generic_call_info(node,
		info0), false)
	if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
		tc.remember_resolved_call(id, info.name)
	}
	if tc.is_generic_enum_from_call(node) {
		tc.check_call_arg_types(id, node, info)
	}
	if !info.params_known || info.params.len == 0 {
		return info
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
	collapsed_target := if field_init_args > 0 {
		tc.collapsed_call_arg_type(node, info) or { builtin_void_type }
	} else {
		builtin_void_type
	}
	mut expanded_arg_offset := 0
	for i in 1 + info.arg_offset .. node.children_count {
		raw_arg := tc.a.child_node(&node, i)
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if raw_arg.kind == .field_init {
			tc.annotate_collapsed_field_expected_expr(arg_id, raw_arg.value, collapsed_target)
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
		mut expected := tc.builtin_array_annotation_expected_type(node, i)
		if expected is Void {
			expected = if info.is_variadic && param_idx == info.params.len - 1
				&& tc.spread_arg_child(arg_id) != none {
				info.params[param_idx]
			} else {
				tc.call_arg_expected_type(info, param_idx)
			}
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
	return info
}

fn (tc &TypeChecker) builtin_array_annotation_expected_type(node flat.Node, arg_index int) Type {
	if node.children_count == 0 {
		return builtin_void_type
	}
	mut callee := tc.a.node(tc.a.child(&node, 0))
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return builtin_void_type
	}
	if !((callee.value == 'insert' && arg_index in [1, 2])
		|| (callee.value == 'prepend' && arg_index == 1)) {
		return builtin_void_type
	}
	receiver_id := tc.a.child(callee, 0)
	receiver_type := tc.resolve_type(receiver_id)
	array_type := array_like_type_for_method(unalias_and_unwrap_pointer_type(receiver_type),
		callee.value) or { return builtin_void_type }
	if callee.value == 'insert' {
		return if arg_index == 1 {
			builtin_int_type
		} else if arg_index == 2 {
			arg_id := tc.call_arg_value(tc.a.child(&node, arg_index))
			if tc.expr_is_empty_bare_array_literal(arg_id) {
				// `array.insert(i, [])` is the zero-element bulk-insert form,
				// including when the receiver itself stores arrays.
				Type(array_type)
			} else if tc.a.node(arg_id).kind == .array_literal {
				// Non-empty array literals must keep their inferred nesting depth so
				// the builtin compatibility check can diagnose extra dimensions.
				builtin_void_type
			} else {
				*array_type.elem_type
			}
		} else {
			builtin_void_type
		}
	}
	if callee.value == 'prepend' && arg_index == 1 {
		arg_id := tc.call_arg_value(tc.a.child(&node, arg_index))
		if tc.a.node(arg_id).kind == .array_literal && !tc.expr_is_empty_bare_array_literal(arg_id) {
			return builtin_void_type
		}
		return *array_type.elem_type
	}
	return builtin_void_type
}

fn (tc &TypeChecker) call_arg_expected_type(info CallInfo, param_idx int) Type {
	expected := info.params[param_idx]
	if info.is_variadic && param_idx == info.params.len - 1 && expected is Array {
		return array_elem_type(expected)
	}
	return expected
}

fn (tc &TypeChecker) collapsed_call_arg_raw_param_idx(node flat.Node, info CallInfo) int {
	mut field_init_args := 0
	mut first_field := -1
	for i in 1 + info.arg_offset .. node.children_count {
		if tc.a.child_node(&node, i).kind != .field_init {
			continue
		}
		field_init_args++
		if first_field < 0 {
			first_field = i
		}
	}
	if first_field < 0 {
		return -1
	}
	recv_extra := if info.has_receiver { 1 } else { 0 }
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	actual_count := node.children_count - 1 - info.arg_offset - field_init_args + collapsed + recv_extra
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	arg_shift := if ctx_omitted { ctx_count } else { 0 }
	return first_field - 1 - info.arg_offset + recv_extra + arg_shift
}

fn (tc &TypeChecker) collapsed_call_arg_param_idx(node flat.Node, info CallInfo) int {
	param_idx := tc.collapsed_call_arg_raw_param_idx(node, info)
	if info.is_variadic && info.params.len > 0 && param_idx >= info.params.len - 1 {
		return info.params.len - 1
	}
	return param_idx
}

fn (tc &TypeChecker) collapsed_call_arg_variadic_elem_idx(node flat.Node, info CallInfo) int {
	if !info.is_variadic || info.params.len == 0 {
		return -1
	}
	raw_param_idx := tc.collapsed_call_arg_raw_param_idx(node, info)
	variadic_param_idx := info.params.len - 1
	if raw_param_idx < variadic_param_idx {
		return -1
	}
	return raw_param_idx - variadic_param_idx
}

fn (tc &TypeChecker) collapsed_call_arg_type(node flat.Node, info CallInfo) ?Type {
	param_idx := tc.collapsed_call_arg_param_idx(node, info)
	if param_idx < 0 || param_idx >= info.params.len {
		return none
	}
	target := info.params[param_idx]
	if info.is_variadic && param_idx == info.params.len - 1 && target is Array {
		return *target.elem_type
	}
	return target
}

fn (mut tc TypeChecker) annotate_collapsed_field_expected_expr(arg_id flat.NodeId, field_name string, target Type) {
	if expected := tc.collapsed_field_expected_type(field_name, target) {
		tc.annotate_expected_expr(arg_id, expected)
	}
}

fn (tc &TypeChecker) collapsed_field_expected_type(field_name string, target Type) ?Type {
	if field_name == '' {
		return none
	}
	param_struct := collapsed_field_struct_type(target) or { return none }
	return tc.struct_field_type(param_struct.source_type_id(), field_name)
}

fn (tc &TypeChecker) collapsed_field_owner(field_name string, target &Type) ?u32 {
	if field_name == '' {
		return none
	}
	param_struct := collapsed_field_struct_type(target) or { return none }
	if tc.struct_field_type(param_struct.source_type_id(), field_name) != none {
		return param_struct.source_type_id()
	}
	return none
}

fn collapsed_field_struct_type(target Type) ?Struct {
	return struct_type_from_type(unalias_and_unwrap_pointer_type(target))
}

fn collapsed_field_owner_display(owner string) string {
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
	mut_binding_id := if has_val { val_id } else { key_id }
	mut mut_binding_name := ''
	mut had_mut_base := false
	mut saved_mut_base := builtin_void_type
	mut had_mut_owner := false
	mut saved_mut_owner := ScopeBindingOwner{}
	if node.op == .amp && tc.valid_node_id(mut_binding_id) {
		binding := tc.a.node(mut_binding_id)
		if binding.kind == .ident {
			mut_binding_name = binding.value
			if base := tc.fn_context.mut_param_base_types[mut_binding_name] {
				had_mut_base = true
				saved_mut_base = base
			}
			if owner := tc.fn_context.mut_local_owners[mut_binding_name] {
				had_mut_owner = true
				saved_mut_owner = owner
			}
		}
	}
	if header == 4 {
		tc.insert_loop_var(key_id, tc.range_loop_var_type(container_id, tc.a.child(&node, 3)))
		tc.annotate_node(tc.a.child(&node, 3))
	} else {
		clean := tc.for_in_iterable_type(container_id)
		source_yields_ref := tc.for_in_iterable_yields_ref(container_id)
		yields_ref := node.op == .amp || source_yields_ref
		if clean is Array {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, builtin_int_type)
				if node.op == .amp {
					tc.insert_mut_loop_var_with_source(val_id, elem_type, source_yields_ref)
				} else {
					tc.insert_loop_var(val_id, elem_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var_with_source(key_id, elem_type, source_yields_ref)
				} else {
					tc.insert_loop_var(key_id, elem_type)
				}
			}
		} else if clean is ArrayFixed {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, builtin_int_type)
				if node.op == .amp {
					tc.insert_mut_loop_var_with_source(val_id, elem_type, source_yields_ref)
				} else {
					tc.insert_loop_var(val_id, elem_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var_with_source(key_id, elem_type, source_yields_ref)
				} else {
					tc.insert_loop_var(key_id, elem_type)
				}
			}
		} else if clean is Map {
			value_type := for_in_ref_binding_type(clean.value_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				if node.op == .amp {
					tc.insert_mut_loop_var_with_source(val_id, value_type, source_yields_ref)
				} else {
					tc.insert_loop_var(val_id, value_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var_with_source(key_id, value_type, source_yields_ref)
				} else {
					tc.insert_loop_var(key_id, value_type)
				}
			}
		} else if clean is String {
			if has_val {
				tc.insert_loop_var(key_id, builtin_int_type)
				tc.insert_loop_var(val_id, builtin_u8_type)
			} else {
				tc.insert_loop_var(key_id, builtin_u8_type)
			}
		} else if elem_type := tc.iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_loop_var(key_id, builtin_int_type)
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
	if mut_binding_name.len > 0 {
		if had_mut_base {
			tc.fn_context.mut_param_base_types[mut_binding_name] = saved_mut_base
		} else {
			tc.fn_context.mut_param_base_types.delete(mut_binding_name)
		}
		if had_mut_owner {
			tc.prepare_mut_local_owner_write()
			tc.fn_context.mut_local_owners[mut_binding_name] = saved_mut_owner
		} else {
			tc.prepare_mut_local_owner_write()
			tc.fn_context.mut_local_owners.delete(mut_binding_name)
		}
	}
}

fn (tc &TypeChecker) for_in_iterable_type(container_id flat.NodeId) Type {
	mut clean := unwrap_pointer(tc.resolve_type(container_id))
	for _ in 0 .. 8 {
		if clean is Alias {
			base := *clean.base_type
			pointed := unalias_type(unwrap_pointer(base))
			clean = if pointed is Array || pointed is ArrayFixed || pointed is Map {
				unwrap_pointer(base)
			} else {
				base
			}
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
	container := tc.a.nodes[int(tc.unwrap_paren_expr_id(container_id))]
	if container.kind == .ident && container.value in tc.fn_context.mut_param_base_types {
		// Mutable loop bindings use a pointer internally so writes reach the
		// container element. Iterating that binding still has ordinary value
		// semantics unless the source expression itself introduced a reference.
		return false
	}
	mut typ := tc.resolve_type(container_id)
	for _ in 0 .. 8 {
		if typ is Alias {
			typ = typ.base_type
			continue
		}
		if typ is OptionType {
			base := *typ.base_type
			clean_base := unalias_type(unwrap_pointer(base))
			if clean_base is Array || clean_base is ArrayFixed {
				typ = base
				continue
			}
		}
		break
	}
	if typ is Pointer {
		base := unalias_type(typ.base_type)
		return base is Array || base is ArrayFixed || base is Map
	}
	return false
}

// iterator_for_in_elem_type returns the element type yielded by an iterator-style `for in` value.
pub fn (tc &TypeChecker) iterator_for_in_elem_type(typ Type) ?Type {
	info := tc.iterator_for_in_next_call_info(typ) or { return none }
	return iterator_for_in_elem_type_from_next_return(info.return_type)
}

fn (tc &TypeChecker) iterator_unbounded_next_generic(typ &Type) ?u32 {
	info := tc.iterator_for_in_next_call_info(typ) or { return none }
	type_name := resolve_type_name_for_method(unwrap_pointer(typ))
	syntax := flat.source_type_syntax(type_name)
	owner_id := if syntax.application_valid() {
		syntax.application_base()
	} else {
		type_name
	}
	owner_params := tc.struct_generic_params[owner_id] or {
		tc.struct_generic_params[tc.qualify_type_name(owner_id)] or { []u32{} }
	}
	method_params := tc.fn_generic_params[info.name] or {
		mut found := []u32{}
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

// iterator_for_in_next_call_info returns call metadata for a compatible iterator `next` method,
// including visible methods on C structs declared in directly imported modules.
pub fn (tc &TypeChecker) iterator_for_in_next_call_info(typ Type) ?CallInfo {
	clean := unwrap_pointer(typ)
	name_id := clean.source_type_id()
	if clean is Interface {
		if info := tc.interface_receiver_method_call_info(name_id, 'next') {
			if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
				return info
			}
		}
	}
	if name_id == checker_source_runes_iterator || name_id == checker_source_builtin_runes_iterator {
		return CallInfo{
			name:         'RunesIterator.next'
			params:       tarr1(Type(Pointer{
				base_type: tc.intern_type_reference(clean)
			}))
			return_type:  Type(OptionType{
				base_type: tc.intern_type_reference(Type(rune_))
			})
			has_receiver: true
			params_known: true
		}
	}
	type_name := resolve_type_name_for_method(clean)
	if type_name == 0 {
		return none
	}
	c_method_name, ambiguous := tc.lookup_c_struct_receiver_method(typ, 'next')
	if ambiguous {
		return none
	}
	if c_method_name.len > 0 {
		if _ := tc.private_declaration(c_method_name) {
			return none
		}
		info := tc.call_info(c_method_name, true)
		if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
			return tc.specialize_generic_interface_method(name_id, info)
		}
		return none
	}
	if info := tc.resolve_generic_struct_method(type_name, 'next') {
		if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
			return tc.specialize_generic_interface_method(name_id, info)
		}
	}
	for method_name in receiver_method_name_candidates(clean, 'next', tc.cur_module) {
		if method_name !in tc.fn_ret_types {
			continue
		}
		info := tc.call_info(method_name, true)
		if _ := iterator_for_in_elem_type_from_next_return(info.return_type) {
			return tc.specialize_generic_interface_method(name_id, info)
		}
	}
	return none
}

// iterator_for_in_next_call_info_text returns specialized `next` call metadata for a source spelling ID.
pub fn (tc &TypeChecker) iterator_for_in_next_call_info_text(type_text_id u32) ?CallInfo {
	info := tc.iterator_for_in_next_call_info(tc.parse_type(type_text_id)) or { return none }
	receiver := if flat.source_type_shape(type_text_id).has(.pointer) {
		flat.source_type_syntax(type_text_id).pointer_raw_base()
	} else {
		type_text_id
	}
	return tc.specialize_generic_interface_method(receiver, info)
}

fn (tc &TypeChecker) specialize_generic_interface_method(type_name_id u32, info CallInfo) CallInfo {
	syntax := flat.source_type_syntax(type_name_id)
	args := syntax.application_args()
	if !syntax.application_valid() || args.len == 0 {
		return info
	}
	base_id := syntax.application_base()
	params := tc.interface_generic_params[base_id] or {
		tc.interface_generic_params[tc.qualify_type_name(base_id)] or { return info }
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
		params:      specialized_params
		return_type: tc.substitute_generic_type_values(info.return_type, concrete_types, params)
	}
}

// index_overload_call_info returns call metadata for the `[]` or `[]=` overload of `typ`.
pub fn (tc &TypeChecker) index_overload_call_info(typ Type, setter bool) ?CallInfo {
	method := if setter { '[]=' } else { '[]' }
	// The collected suffix index includes the bare name of every receiver method,
	// including generic declarations. A missing operator has no receiver to resolve.
	if tc.method_suffix_prescreen && tc.top_level_idx.len > 0
		&& method !in tc.receiver_method_suffix_index
		&& '@${method}' !in tc.receiver_method_suffix_index {
		return none
	}
	clean := unwrap_pointer(typ)
	type_name := resolve_type_name_for_method(clean)
	if type_name == 0 {
		return none
	}
	c_method_name, ambiguous := tc.lookup_c_struct_receiver_method(typ, method)
	if ambiguous {
		return none
	}
	if c_method_name.len > 0 {
		if _ := tc.private_declaration(c_method_name) {
			return none
		}
		info := tc.call_info(c_method_name, true)
		if info.params.len == 0 || tc.method_receiver_compatible(typ, info.params[0], c_method_name) {
			return info
		}
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

fn (tc &TypeChecker) specialized_index_overload_call_info(type_name_id u32, method string, info CallInfo) CallInfo {
	concrete := tc.concrete_method_signature_key(type_name_id, method) or {
		syntax := flat.source_type_syntax(type_name_id)
		if !syntax.application_valid() || syntax.application_args().len == 0 {
			return info
		}
		'${flat.source_type_name(type_name_id)}.${method}'
	}
	if concrete == info.name {
		return info
	}
	return CallInfo{
		name:                 concrete
		params:               info.params.clone()
		shared_params:        info.shared_params.clone()
		return_type:          info.return_type
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         info.params_known
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset:           info.arg_offset
	}
}

fn iterator_for_in_elem_type_from_next_return(ret Type) ?Type {
	if ret is OptionType {
		return *ret.base_type
	}
	return none
}

fn (tc &TypeChecker) range_loop_var_type(low_id flat.NodeId, high_id flat.NodeId) Type {
	low_type := tc.resolve_type(low_id)
	if tc.valid_node_id(high_id) {
		high_type := tc.resolve_type(high_id)
		if tc.range_endpoint_is_literal(low_id) && !tc.range_endpoint_is_literal(high_id)
			&& unalias_type(high_type).is_integer() {
			return high_type
		}
		if !tc.range_endpoint_is_literal(low_id) && tc.range_endpoint_is_literal(high_id)
			&& unalias_type(low_type).is_integer() {
			return low_type
		}
	}
	if unalias_type(low_type).is_integer() {
		return low_type
	}
	return builtin_int_type
}

// insert_loop_var updates insert loop var state for types.
fn (mut tc TypeChecker) insert_loop_var(id flat.NodeId, typ Type) ScopeBindingOwner {
	if int(id) < 0 {
		return ScopeBindingOwner{}
	}
	tc.check_local_binding_global_shadowing(id)
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
	tc.insert_mut_loop_var_with_source(id, typ, false)
}

fn (mut tc TypeChecker) insert_mut_loop_var_with_source(id flat.NodeId, typ Type, source_yields_ref bool) {
	owner := tc.insert_loop_var(id, typ)
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return
	}
	v := tc.a.nodes[int(id)]
	if v.kind != .ident || v.value.len == 0 || v.value == '_' {
		return
	}
	tc.prepare_mut_local_owner_write()
	tc.fn_context.mut_local_owners[v.value] = owner
	if typ is Pointer && !source_yields_ref {
		tc.fn_context.mut_param_base_types[v.value] = typ.base_type
	} else {
		tc.fn_context.mut_param_base_types.delete(v.value)
	}
}

fn for_in_ref_binding_type(typ Type, yields_ref bool) Type {
	clean := unalias_type(typ)
	if yields_ref && clean !is Pointer && clean !is OptionType {
		return Type(Pointer{
			base_type: new_type_reference(Type(typ))
		})
	}
	return typ
}

// expr_type returns the resolved type recorded for a node during annotate_types.
@[direct_array_access; inline]
pub fn (tc &TypeChecker) expr_type(id flat.NodeId) ?Type {
	idx := int(id)
	if idx < 0 || tc.a.nodes[idx].kind != .call {
		return tc.cached_expr_type(id)
	}
	return tc.call_expr_type(id)
}

// expr_type_reference returns a stable reference to the recorded expression type.
// It follows expr_type's call resolution and cache visibility; dense values keep
// their canonical pointers, while sparse map values receive independent wrappers.
@[direct_array_access; inline]
pub fn (tc &TypeChecker) expr_type_reference(id flat.NodeId) ?&Type {
	idx := int(id)
	if idx < 0 || tc.a.nodes[idx].kind != .call {
		typ := tc.cached_expr_type_ref(id)
		if !isnil(typ) {
			return typ
		}
		return none
	}
	return tc.call_expr_type_reference(id)
}

@[inline]
fn call_type_annotation_is_placeholder(typ u32) bool {
	return typ == flat.source_type_int || typ == qualify_source_type_array
		|| typ == qualify_source_type_map || typ == flat.source_type_unknown
}

@[direct_array_access]
fn (tc &TypeChecker) call_expr_type(id flat.NodeId) ?Type {
	if t := tc.resolved_call_type(id) {
		return t
	}
	if int(id) >= 0 {
		node := tc.a.nodes[int(id)]
		if node.kind == .call && node.typ != 0 && !call_type_annotation_is_placeholder(node.typ) {
			return *tc.parse_type(node.typ)
		}
	}
	cached_type_ref0 := tc.cached_expr_type_ref(id)
	if !isnil(cached_type_ref0) {
		return *cached_type_ref0
	}
	return none
}

@[direct_array_access]
fn (tc &TypeChecker) call_expr_type_reference(id flat.NodeId) ?&Type {
	if t := tc.resolved_call_type_reference(id) {
		return t
	}
	if int(id) >= 0 {
		node := tc.a.nodes[int(id)]
		if node.kind == .call && node.typ != 0 && !call_type_annotation_is_placeholder(node.typ) {
			return tc.parse_type(node.typ)
		}
	}
	cached_type_ref0 := tc.cached_expr_type_ref(id)
	if !isnil(cached_type_ref0) {
		return cached_type_ref0
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
	if node.children_count == 3 && tc.c_va_macro_call_name(node) == 'C.va_arg' {
		type_arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		type_name_source_id := tc.generic_call_type_arg_name(type_arg_id)
		if type_name_source_id != 0 {
			target_type := tc.parse_type(type_name_source_id)
			if target_type !is Unknown && !type_contains_unknown(target_type) {
				return *target_type
			}
		}
	}
	cached_type_ref1 := tc.cached_expr_type_ref(id)
	if !isnil(cached_type_ref1) && cached_type_ref1 !is Void {
		return *cached_type_ref1
	}
	if name := tc.cached_resolved_call(id) {
		if t := tc.fn_ret_types[name] {
			return t
		}
	}
	return none
}

@[direct_array_access]
fn (tc &TypeChecker) resolved_call_type_reference(id flat.NodeId) ?&Type {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .call {
		return none
	}
	if node.children_count == 3 && tc.c_va_macro_call_name(node) == 'C.va_arg' {
		type_arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		type_name_source_id := tc.generic_call_type_arg_name(type_arg_id)
		if type_name_source_id != 0 {
			target_type := tc.parse_type(type_name_source_id)
			if target_type !is Unknown && !type_contains_unknown(target_type) {
				return target_type
			}
		}
	}
	cached_type_ref1 := tc.cached_expr_type_ref(id)
	if !isnil(cached_type_ref1) {
		if cached_type_ref1 !is Void {
			return cached_type_ref1
		}
	}
	if name := tc.cached_resolved_call(id) {
		if t := tc.fn_ret_types[name] {
			return new_type_reference(t)
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
	tc.invalidate_checked_type_id(idx)
	if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		mut wtc := unsafe { tc }
		wtc.expr_type_set[idx] = false
	}
	if idx >= 0 && !isnil(tc.a) && idx < tc.a.nodes.len {
		tc.invalidate_lexical_parent_memo()
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

@[inline]
fn (tc &TypeChecker) cached_expr_type(id flat.NodeId) ?Type {
	cached_type_ref2 := tc.cached_expr_type_ref(id)
	if !isnil(cached_type_ref2) {
		return *cached_type_ref2
	}
	return none
}

// Dense slots point at immutable canonical types. Sparse map values need stable
// copies because later insertions can move the map backing storage. A nil pointer
// represents a cache miss without an optional-result wrapper on this hot path.
@[direct_array_access; inline]
fn (tc &TypeChecker) cached_expr_type_ref(id flat.NodeId) &Type {
	idx := int(id)
	if isnil(tc.storage_query_probe) && (!tc.parallel_check_sparse || tc.in_check_range(idx)) {
		if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
			return tc.expr_type_values[idx]
		}
		return unsafe { nil }
	}
	// The vc bootstrap compiler heap-copies a non-literal `&Type` return, which crashes on
	// nil, so a miss is returned as the literal nil instead of the propagated pointer.
	sparse := tc.cached_expr_type_ref_sparse(id)
	if isnil(sparse) {
		return unsafe { nil }
	}
	return sparse
}

@[direct_array_access]
fn (tc &TypeChecker) cached_expr_type_ref_sparse(id flat.NodeId) &Type {
	idx := int(id)
	if !isnil(tc.storage_query_probe) {
		if typ := tc.sparse_expr_type_values[idx] { return new_type_reference(typ) }
		if !isnil(tc.storage_query_probe.read_base) {
			base := tc.storage_query_probe.read_base.cached_expr_type_ref(id)
			if isnil(base) {
				return unsafe { nil }
			}
			return base
		}
		return unsafe { nil }
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			if idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
				return tc.expr_type_values[idx]
			}
			return unsafe { nil }
		}
		if typ := tc.sparse_expr_type_values[idx] { return new_type_reference(typ) }
		return unsafe { nil }
	}
	if idx >= 0 && idx < tc.expr_type_set.len && tc.expr_type_set[idx] {
		return tc.expr_type_values[idx]
	}
	return unsafe { nil }
}

// cached_resolved_call supports cached resolved call handling for TypeChecker.
@[direct_array_access]
fn (tc &TypeChecker) cached_resolved_call(id flat.NodeId) ?string {
	idx := int(id)
	if !isnil(tc.storage_query_probe) {
		if name := tc.sparse_resolved_call_names[idx] { return name }
		if !isnil(tc.fork_overlay) && idx >= tc.fork_overlay.base_node_count {
			if name := tc.fork_overlay.resolved_call_names[idx] { return name }
		}
		if !isnil(tc.storage_query_probe.read_base) {
			return tc.storage_query_probe.read_base.cached_resolved_call(id)
		}
		return none
	}
	if !isnil(tc.fork_overlay) && idx >= tc.fork_overlay.base_node_count {
		if name := tc.fork_overlay.resolved_call_names[idx] {
			return name
		}
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			if idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
				return tc.resolved_call_names[idx].value
			}
			return none
		}
		return tc.sparse_resolved_call_names[idx] or { none }
	}
	if idx >= 0 && idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
		return tc.resolved_call_names[idx].value
	}
	return none
}

// resolved_call_name returns the checker-resolved function name for a call node.
pub fn (tc &TypeChecker) resolved_call_name(id flat.NodeId) ?string {
	return tc.cached_resolved_call(id)
}

// resolved_call_may_store_globally reports whether the resolved callee or one of its
// transitive callees belongs to a source file annotated with `@[has_globals]` or `@[translated]`.
pub fn (tc &TypeChecker) resolved_call_may_store_globally(id flat.NodeId) bool {
	name := tc.cached_resolved_call(id) or { return false }
	return tc.cached_fn_may_store_globally(name)
}

// fn_value_may_store_globally reports whether invoking a function value can reach a
// declaration from a source file annotated with `@[has_globals]` or `@[translated]`. An unresolved function
// value is conservative because its body is opaque at this call site.
pub fn (tc &TypeChecker) fn_value_may_store_globally(id flat.NodeId) bool {
	name := tc.resolved_fn_value_name(id) or { return true }
	return tc.cached_fn_may_store_globally(name)
}

fn (tc &TypeChecker) cached_fn_may_store_globally(name string) bool {
	if !isnil(tc.post_check_dependencies) && !isnil(tc.global_store_cache) {
		mut cache := tc.global_store_cache
		if result := cache.values[name] {
			return result
		}
		mut visiting := map[string]bool{}
		result := tc.fn_may_store_globally(name, mut visiting)
		// A recursive branch can be false only because an ancestor is
		// already visiting. Cache completed root answers, never DFS branches.
		cache.values[name] = result
		return result
	}
	mut visiting := map[string]bool{}
	return tc.fn_may_store_globally(name, mut visiting)
}

fn (tc &TypeChecker) fn_may_store_globally(name string, mut visiting map[string]bool) bool {
	if visiting[name] {
		return false
	}
	visiting[name] = true
	file := tc.fn_type_files[name] or { return false }
	if tc.has_globals_files[file] || tc.translated_files[file] {
		return true
	}
	decl_module := tc.fn_type_modules[name] or { '' }
	decl := tc.global_store_fn_decl(name, decl_module) or { return false }
	if !isnil(tc.post_check_dependencies) {
		// A transform fork: other workers may be rewriting callee bodies in place,
		// so follow only the call graph recorded while checking.
		view := tc.post_check_dependencies
		for dependency in view.by_fn[decl.idx] or { []SymbolId{} } {
			index := int(dependency) - 1
			if index >= 0 && index < view.names.len
				&& tc.fn_may_store_globally(view.names[index], mut visiting) {
				return true
			}
		}
		return false
	}
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

fn (tc &TypeChecker) global_store_fn_decl(name string, fallback_mod string) ?VisibleMutationFnDecl {
	if !isnil(tc.post_check_dependencies) {
		index := tc.post_check_dependencies.declarations
		if !isnil(index) && index.decl_index_ready {
			lookup_name := visible_mutation_fn_lookup_name(name)
			if decl := index.global_decls[lookup_name] {
				if fallback_mod == '' || fallback_mod == decl.mod {
					return decl
				}
			}
			if decl := index.decls['${fallback_mod}\x01${lookup_name}'] {
				return decl
			}
		}
	}
	// Generated declarations may not exist in the frozen source index.
	return tc.visible_mutation_fn_decl(name, fallback_mod)
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
	if name == '' {
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
	if tc.a.noreturn_fns.len == 0 {
		return false
	}
	if name in tc.a.noreturn_fns {
		return true
	}
	// Only dotted suffixes can match a qualified declaration. Search complete
	// words for each preceding dot instead of examining every name byte here.
	mut end := name.last_index_u8(`.`)
	for end > 0 {
		// Both searches and the membership query only borrow these spans.
		prefix := unsafe { name.substr_unsafe(0, end) }
		dot := prefix.last_index_u8(`.`)
		if dot < 0 {
			break
		}
		suffix := unsafe { name.substr_unsafe(dot + 1, name.len) }
		if tc.a.noreturn_fns[suffix] {
			return true
		}
		end = dot
	}
	return false
}

// resolved_fn_value_name returns the checker-resolved function name for a function value node.
@[direct_array_access]
pub fn (tc &TypeChecker) resolved_fn_value_name(id flat.NodeId) ?string {
	idx := int(id)
	if !isnil(tc.storage_query_probe) {
		if name := tc.fork_fn_value_writes[idx] {
			if name.len == 0 { return none }
			return name
		}
		if !isnil(tc.fork_overlay) && idx >= tc.fork_overlay.base_node_count {
			if name := tc.fork_overlay.resolved_fn_values[idx] { return name }
		}
		if name := tc.sparse_resolved_fn_values[idx] { return name }
		if !isnil(tc.storage_query_probe.read_base) {
			return tc.storage_query_probe.read_base.resolved_fn_value_name(id)
		}
		return none
	}
	if !isnil(tc.fork_overlay) && idx >= tc.fork_overlay.base_node_count {
		if name := tc.fork_overlay.resolved_fn_values[idx] {
			return name
		}
	}
	if idx < 0 {
		return none
	}
	if !isnil(tc.fork_overlay) {
		if name := tc.fork_fn_value_writes[idx] {
			if name.len == 0 {
				// The fork cleared this entry (see clear_resolved_fn_value).
				return none
			}
			return name
		}
	}
	if tc.sparse_fn_value_filter_ready {
		bit := sparse_fn_value_filter_bit(idx)
		if tc.sparse_fn_value_filter[bit >> 6] & (u64(1) << (bit & 63)) == 0 {
			return none
		}
	}
	return tc.sparse_resolved_fn_values[idx] or { none }
}

@[inline]
fn sparse_fn_value_filter_bit(idx int) u32 {
	key := u32(idx)
	return (key ^ (key >> 8)) & 255
}

@[direct_array_access; inline]
fn (mut tc TypeChecker) record_sparse_fn_value_filter_key(idx int) {
	if tc.sparse_fn_value_filter_ready {
		bit := sparse_fn_value_filter_bit(idx)
		tc.sparse_fn_value_filter[bit >> 6] |= u64(1) << (bit & 63)
	}
}

// rebuild_sparse_fn_value_filter runs only after taking an owned snapshot.
// Fork source maps are otherwise read-only; later setters maintain the filter,
// and deletions may retain bits because a false positive still checks the map.
fn (mut tc TypeChecker) rebuild_sparse_fn_value_filter() {
	tc.sparse_fn_value_filter = [4]u64{}
	tc.sparse_fn_value_filter_ready = true
	for idx, _ in tc.sparse_resolved_fn_values {
		tc.record_sparse_fn_value_filter_key(idx)
	}
}

// set_resolved_fn_value records the resolved function-value target of a node.
// A transform fork keeps the write private (its snapshot of the parent stays
// read-only) until the merge replays it.
pub fn (mut tc TypeChecker) set_resolved_fn_value(idx int, name string) {
	if idx < 0 {
		return
	}
	if !isnil(tc.storage_query_probe) || !isnil(tc.fork_overlay) {
		tc.fork_fn_value_writes[idx] = name
		return
	}
	tc.sparse_resolved_fn_values[idx] = name
	tc.record_sparse_fn_value_filter_key(idx)
}

// clear_resolved_fn_value removes stale function-value metadata after a later
// transform proves that an identifier refers to a value declaration. A
// transform fork records a tombstone in its private writes instead, so the
// snapshot entry stays hidden and the merge replays the clear.
pub fn (mut tc TypeChecker) clear_resolved_fn_value(id flat.NodeId) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if !isnil(tc.storage_query_probe) {
		if !isnil(tc.fork_overlay) { tc.fork_overlay.resolved_fn_values.delete(idx) }
		tc.sparse_resolved_fn_values.delete(idx)
		tc.fork_fn_value_writes[idx] = ''
		return
	}
	if !isnil(tc.fork_overlay) {
		tc.fork_overlay.resolved_fn_values.delete(idx)
		// A tombstone is only meaningful when the parent snapshot actually has
		// an entry to hide; otherwise drop any own write so the cleared range
		// does not bloat the replayed write set with no-op tombstones.
		if tc.sparse_resolved_fn_values[idx] or { '' } != '' {
			tc.fork_fn_value_writes[idx] = ''
		} else {
			tc.fork_fn_value_writes.delete(idx)
		}
		return
	}
	if tc.sparse_resolved_fn_values.len > 0 {
		tc.sparse_resolved_fn_values.delete(idx)
	}
}

// fn_value_snapshot returns this checker's effective source-node fn-value
// entries: the shared store, with a transform fork's own writes and
// tombstones applied. New forks read from such a snapshot.
pub fn (tc &TypeChecker) fn_value_snapshot() map[int]string {
	mut snapshot := tc.sparse_resolved_fn_values.clone()
	if !isnil(tc.fork_overlay) {
		for idx, name in tc.fork_fn_value_writes {
			if name.len == 0 {
				snapshot.delete(idx)
			} else {
				snapshot[idx] = name
			}
		}
	}
	return snapshot
}

// fork_fn_value_replays_source reports whether a transform fork's private
// fn-value write at `idx` must be replayed into its parent on merge: only
// source nodes below the overlay boundary keep their ids. Transform-created
// nodes are published through the overlay under shifted ids; an unbounded
// overlay keeps every write private.
pub fn (tc &TypeChecker) fork_fn_value_replays_source(idx int) bool {
	return !isnil(tc.fork_overlay) && tc.fork_overlay.base_node_count >= 0
		&& idx < tc.fork_overlay.base_node_count
}

// apply_forked_fn_value replays one source-node fn-value write from a
// transform fork: an empty name is a tombstone (the fork cleared the entry),
// anything else replaces the entry. `name` must already be owned by an arena
// that outlives this checker.
pub fn (mut tc TypeChecker) apply_forked_fn_value(idx int, name string) {
	if name == '' {
		tc.clear_resolved_fn_value(flat.NodeId(idx))
		return
	}
	tc.set_resolved_fn_value(idx, tc.canonical_symbol(name))
}

// DependencyGraphView is the checker's post-check call graph with the symbol
// names its ids refer to. Transform forks share it read-only.
struct DependencyGraphView {
	by_fn        map[int][]SymbolId
	names        []string
	declarations &VisibleMutationCache = unsafe { nil }
}

@[heap]
struct FnGlobalStoreCache {
mut:
	values map[string]bool
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
		ptr := uint(name.str)
		slot := int((u64(ptr) >> 4 ^ u64(name.len)) & u64(type_cache_recent_slots - 1))
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

// lookup_canonical_symbol_unlocked returns the canonical spelling of an already
// interned name without taking the interner lock. It is only valid while no
// thread interns, e.g. across a pool phase that only reads the symbol table.
pub fn (tc &TypeChecker) lookup_canonical_symbol_unlocked(name string) ?string {
	if isnil(tc.symbols) || name.len == 0 {
		return none
	}
	symbols := unsafe { tc.symbols }
	id := symbols.ids[name] or { return none }
	index := int(id) - 1
	if index < 0 || index >= symbols.names.len {
		return none
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
	mut inferred := map[u32]Type{}
	for i in 0 .. actual_fn.params.len {
		tc.infer_generic_type_value_from_type(actual_fn.params[i].typ.source_type_id(), *expected_fn.params[i].typ, generic_params, mut inferred)
	}
	tc.infer_generic_type_value_from_type(actual_fn.return_type.source_type_id(), expected_fn.return_type, generic_params, mut inferred)
	mut concrete_types := []Type{cap: generic_params.len}
	for param in generic_params {
		concrete_types << (inferred[param] or { return false })
	}
	mut specialized_params := []FnParam{cap: actual_fn.params.len}
	for param in actual_fn.params {
		substituted := tc.substitute_generic_type_values(param.typ, concrete_types, generic_params)
		specialized_params << FnParam{
			typ:    tc.intern_type_reference(substituted)
			is_mut: param.is_mut
		}
	}
	specialized := Type(FnType{ params: specialized_params, is_variadic: actual_fn.is_variadic, return_type: tc.intern_type_reference(tc.substitute_generic_type_values(actual_fn.return_type, concrete_types, generic_params)) })
	return tc.fn_value_signature_compatible(specialized, expected)
}

// remember_method_value_call records that the method value `id` names the
// function `name`: the transform names so the instance of the generic method
// that `h.first[int]` names (see check_generic_method_value), which the value
// binds to its receiver.
pub fn (mut tc TypeChecker) remember_method_value_call(id flat.NodeId, name string) {
	tc.remember_resolved_call(id, name)
}

// remember_resolved_call supports remember resolved call handling for TypeChecker.
fn (mut tc TypeChecker) remember_resolved_call(id flat.NodeId, name string) {
	idx := int(id)
	if idx < 0 {
		return
	}
	symbol_id, canonical := tc.intern_symbol(name)
	tc.record_direct_dependency(symbol_id)
	if !isnil(tc.storage_query_probe) {
		tc.sparse_resolved_call_names[idx] = canonical
		return
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.resolved_call_names.len {
			tc.resolved_call_names[idx] = cached_name(canonical)
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
		tc.resolved_call_names[idx] = cached_name(canonical)
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
	// Parallel check workers write their private map, which
	// merge_worker_sparse_caches publishes into the master's; transform forks
	// write their private write set (see set_resolved_fn_value).
	tc.set_resolved_fn_value(idx, canonical)
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
	tc.invalidate_checked_type_id(int(id))
	kind := if int(id) < tc.a.nodes.len { tc.a.nodes[int(id)].kind } else { flat.NodeKind.empty }
	// An arithmetic node that mixes a 128-bit type with a narrower one arrives here
	// with the narrower operand's type. Every reader of the recorded type (printing,
	// `typeof`, a method receiver) then cut the value to 64 bits, so the 128-bit side
	// is what gets cached.
	cached_typ := if kind == .infix { tc.widen_mixed_integer_expr_type(id, typ) } else { typ }
	if should_cache_expr_type(kind, cached_typ) {
		idx := int(id)
		if !isnil(tc.storage_query_probe) {
			tc.sparse_expr_type_values[idx] = cached_typ
			return
		}
		if tc.parallel_check_sparse {
			if tc.in_check_range(idx) && idx < tc.expr_type_values.len {
				tc.expr_type_values[idx] = tc.intern_type_reference(cached_typ)
				tc.expr_type_set[idx] = true
				return
			}
			tc.sparse_expr_type_values[idx] = cached_typ
			return
		}
		if idx >= tc.expr_type_values.len {
			tc.extend_node_caches(tc.a.nodes.len)
		}
		if idx < tc.expr_type_values.len {
			tc.expr_type_values[idx] = tc.intern_type_reference(cached_typ)
			tc.expr_type_set[idx] = true
		}
	} else if tc.keep_placeholder_types && typ is Unknown {
		if typ.placeholder != 0 {
			tc.placeholder_types[int(id)] = typ
		}
	}
}

// should_cache_expr_type reports whether should cache expr type applies in types.
fn should_cache_expr_type(kind flat.NodeKind, typ &Type) bool {
	if typ is Void {
		return kind in [.sql_expr, .or_expr]
	}
	if typ is Unknown {
		// Every check that depends on an unresolved name must see it as unresolved,
		// so its type is kept, while other unknowns may still be resolved later.
		return typ.reason == unresolved_name_reason
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
	tc.check_custom_attribute_uses()
	tc.check_import_diagnostics()
	tc.check_c_js_generic_declarations()
	tc.check_duplicate_fn_declarations()
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
				tc.check_type_declaration_conflict(node_id, node)
				if comma_attr_text_has(node.typ_text(), 'typedef') && !node.value.starts_with('C.') {
					tc.record_error_at(.assignment_mismatch, '`typedef` attribute can only be used with C structs', node_id, tc.declaration_keyword_name_pos(node_id, 'struct'))
				}
				if tc.should_check_type_declaration_name(node_id, node)
					&& !pascal_case_name_is_valid(node.value) {
					tc.check_pascal_case_name(node_id, node.value, 'struct name', tc.declaration_keyword_name_pos(node_id, 'struct'))
				}
				tc.check_decl_type_strings(flat.NodeId(i), node)
				tc.check_struct_implements(flat.NodeId(i), node)
				tc.check_struct_field_defaults(node_id, node)
				if tc.declaration_has_attribute(node_id, 'attribute') {
					tc.check_attribute_schema_decl(node_id, node)
				}
			}
			.type_decl, .interface_decl {
				node_id := flat.NodeId(i)
				tc.check_attribute_marker_target(node_id, node)
				tc.check_type_declaration_conflict(node_id, node)
				if node.kind == .interface_decl {
					if tc.should_check_type_declaration_name(node_id, node)
						&& !pascal_case_name_is_valid(node.value) {
						tc.check_pascal_case_name(node_id, node.value, 'interface name', tc.source_line_declaration_pos(node_id))
					}
					tc.check_interface_member_names(node)
				} else {
					type_kind := if node.children_count > 0 {
						'sum type'
					} else if flat.source_type_syntax(node.typ).fn_prefix() {
						'fn type'
					} else {
						'type alias'
					}
					if tc.should_check_type_declaration_name(node_id, node)
						&& !pascal_case_name_is_valid(node.value) {
						tc.check_pascal_case_name(node_id, node.value, type_kind, tc.declaration_keyword_name_pos(node_id, 'type'))
					}
				}
				tc.check_decl_type_strings(flat.NodeId(i), node)
			}
			.enum_decl {
				node_id := flat.NodeId(i)
				tc.check_attribute_marker_target(node_id, node)
				tc.check_type_declaration_conflict(node_id, node)
				if tc.should_check_type_declaration_name(node_id, node)
					&& !pascal_case_name_is_valid(node.value) {
					tc.check_pascal_case_name(node_id, node.value, 'enum name', tc.declaration_keyword_name_pos(node_id, 'enum'))
				}
				tc.check_enum_backing_type(flat.NodeId(i), node)
				tc.check_enum_field_values(flat.NodeId(i), node)
			}
			.const_decl {
				tc.check_const_field_values(node)
			}
			.global_decl {
				if !tc.enable_globals && !tc.has_globals_files[tc.cur_file]
					&& !tc.node_is_from_translated_file(node) {
					tc.record_error_at(.duplicate_decl, 'use `v -enable-globals ...` to enable globals', flat.NodeId(i), tc.source_line_declaration_pos(flat.NodeId(i)))
				}
				tc.check_global_decl_semantics(flat.NodeId(i), node)
				tc.check_const_global_initializers(node)
			}
			.fn_decl {
				tc.check_attribute_marker_target(flat.NodeId(i), node)
				tc.check_fn_declaration_name(flat.NodeId(i), node)
				tc.check_method_field_name_collision(flat.NodeId(i), node)
				tc.check_main_fn_signature(flat.NodeId(i), node)
				tc.check_init_fn_signature(flat.NodeId(i), node)
				tc.check_str_method_signature(flat.NodeId(i), node)
				tc.check_free_method_signature(flat.NodeId(i), node)
				tc.check_sumtype_builtin_method_override(flat.NodeId(i), node)
				tc.check_test_fn_signature(flat.NodeId(i), node)
				tc.check_decl_type_strings(flat.NodeId(i), node)
				if tc.skips_library_body(node) {
					continue
				}
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
		fn_idx:   fn_idx
		range_lo: range_lo
		file:     file
		module:   module_name
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
		if node.typ_text() == module_name {
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
	// Each report is on the declaration itself: trusted library files have none.
	selected_files_only := tc.selected_files_only()
	mut c_js_fn_files := map[int]bool{}
	for declaration_kind in [flat.NodeKind.struct_decl, flat.NodeKind.fn_decl] {
		tc.cur_module = ''
		tc.cur_file = ''
		mut skip_file := false
		for index in tc.top_level_idx {
			node := tc.a.node(flat.NodeId(index))
			if node.kind == .file {
				tc.enter_file(node.value)
				skip_file = selected_files_only && !tc.diagnostic_files[node.value]
				continue
			}
			if skip_file {
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
			// Finding the source line of a declaration searches the text around
			// it, which cost ~115 ms per check over the libraries: only look at
			// a declaration that can be reported, and at a file that can hold one.
			if !tc.should_diagnose(flat.NodeId(index)) {
				continue
			}
			if !node.value.starts_with('C.') && !node.value.starts_with('JS.')
				&& !tc.file_declares_c_js_fn(node.pos.id, mut c_js_fn_files) {
				continue
			}
			if is_function && node.kind == .fn_decl && node.children_count > 0 {
				receiver := tc.a.child_node(node, 0)
				if receiver.kind == .param && receiver.op == .dot {
					continue
				}
			}
			namespace := tc.c_js_declaration_namespace(flat.NodeId(index), node) or { continue }
			if node.generic_params().len == 0
				&& !tc.declaration_source_line_has_generic(flat.NodeId(index), node) {
				continue
			}
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

fn (tc &TypeChecker) declaration_source_line_has_generic(id flat.NodeId, node &flat.Node) bool {
	pos := tc.source_line_declaration_pos(id)
	file := tc.a.source_files[pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	if pos.offset < 0 || pos.end > source.len || pos.offset >= pos.end {
		return false
	}
	line := source[pos.offset..pos.end]
	matched_name := if line.contains(node.value) {
		node.value
	} else {
		node.value.all_after_last('.')
	}
	name_start := line.index(matched_name) or { return false }
	after_name := line[name_start + matched_name.len..].trim_left(' \t')
	return after_name.starts_with('[')
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

// file_declares_c_js_fn reports whether the source of the file `file_id` has a
// `fn C.` or `fn JS.` in it: without one, no line of the file starts with them
// (see c_js_declaration_namespace). `files` keeps the answers.
fn (tc &TypeChecker) file_declares_c_js_fn(file_id int, mut files map[int]bool) bool {
	if declares := files[file_id] {
		return declares
	}
	file := tc.a.source_files[file_id] or { return false }
	source := tc.source_texts_by_file[file.name] or { '' }
	declares := source.contains('fn C.') || source.contains('fn JS.')
	files[file_id] = declares
	return declares
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
	// Only names declared more than once can be reported. Record each name's first
	// declaration and materialize an index list only for a repeated name, instead
	// of allocating and sorting a group for every function.
	mut first_index := map[string]int{}
	first_index.reserve(u32(tc.top_level_idx.len))
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
		first := first_index[key] or {
			first_index[key] = index
			continue
		}
		mut indexes := groups[key] or { [first] }
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
		if tc.fn_group_is_source_override(indexes) {
			continue
		}
		stored_name := tc.a.nodes[indexes[0]].value
		if stored_name.all_after_last('.') == 'init'
			|| tc.fn_group_name_conflicts_with_import(indexes, stored_name.all_after_last('.'))
			|| tc.fn_group_contains_builtin_declaration(indexes) {
			continue
		}
		display_name := if receiver, method := flat.decode_static_type_method_name(stored_name) {
			'${receiver}.${method}'
		} else {
			stored_name
		}
		tc.errors << TypeError{
			msg:        'redefinition of function `${display_name}`'
			kind:       .duplicate_decl
			node:       flat.empty_node
			node_value: key
			severity:   'builder error:'
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

fn (tc &TypeChecker) fn_group_is_source_override(indexes []int) bool {
	mut family := ''
	mut has_specialized_file := false
	mut seen_files := map[string]bool{}
	for index in indexes {
		node := tc.a.nodes[index]
		file := tc.a.source_files[node.pos.id] or { return false }
		if seen_files[file.name] {
			return false
		}
		seen_files[file.name] = true
		stem, specialized := source_override_stem(file.name)
		if family == '' {
			family = stem
		} else if stem != family {
			return false
		}
		has_specialized_file = has_specialized_file || specialized
	}
	return has_specialized_file
}

fn source_override_stem(file string) (string, bool) {
	if !file.ends_with('.v') {
		return file, false
	}
	stem := file[..file.len - 2]
	dot := stem.last_index_u8(`.`)
	if dot < 0 {
		return stem, false
	}
	suffix := stem[dot + 1..]
	if suffix in ['c', 'js', 'native', 'wasm', 'amd64', 'x64', 'x86_64', 'arm64', 'aarch64', 'x86',
		'i386', 'i486', 'i586', 'i686', 'x32', 'x86_32', 'ia-32', 'ia32', 'arm32', 'aarch32', 'arm',
		'armv7', 'armv7l', 'rv32', 'risc-v32', 'riscv32', 'rv64', 'risc-v64', 'risc-v', 'riscv',
		'riscv64', 'ppc', 'ppc32', 'powerpc', 'ppc64', 'ppc64le', 's390x', 'loongarch64', 'sparc64',
		'wasm32'] {
		return stem[..dot], true
	}
	return stem, false
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
	// A goto only depends on labels in its enclosing function or script. Find
	// those roots through the parsed parent index instead of walking every body.
	indexed := tc.direct_parent_index_trusted && tc.preflight_index_nodes_len == tc.a.nodes.len
	mut roots := map[int]bool{}
	if indexed {
		for index in tc.preflight_nodes(.goto_stmt) {
			mut current := flat.NodeId(index)
			for tc.valid_node_id(current) {
				node := tc.a.node(current)
				if node.kind in [.fn_decl, .file] {
					roots[int(current)] = true
					break
				}
				parent := tc.direct_parent_id(current)
				if parent == current {
					break
				}
				current = parent
			}
		}
	}
	for index in tc.top_level_idx {
		if indexed && !roots[index] {
			continue
		}
		node := tc.a.node(flat.NodeId(index))
		if node.kind == .fn_decl {
			tc.check_goto_boundary(flat.NodeId(index))
		}
	}
	for index in tc.top_level_idx {
		if indexed && !roots[index] {
			continue
		}
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
	// Only a `.vh` can be a cached module header: the check runs for every name
	// the program declares, so the path is normalized for those alone.
	if file.name.ends_with('.vh') {
		normalized := file.name.replace('\\', '/')
		if normalized.contains('/v3_module_cache_') {
			return false
		}
	}
	return tc.diagnostic_files.len == 0 || file.name in tc.diagnostic_files
}

fn (tc &TypeChecker) should_check_type_declaration_name(id flat.NodeId, node flat.Node) bool {
	return tc.should_check_source_name(id)
		&& !(tc.cur_module in ['', 'main'] && is_builtin_type_name(node.value))
}

fn snake_case_name_is_valid(name string) bool {
	if name.starts_with('C.') || name.starts_with('JS.') {
		return true
	}
	// Names with `__` are valid in ordinary files, but `check_snake_case_name` rejects
	// them in `@[generated]` ones, so they have to reach it.
	return (name.len <= 1 || (name[0] != `_` && !name.contains('._')))
		&& !util.contains_capital(name) && !name.contains('__')
}

fn pascal_case_name_is_valid(name string) bool {
	if name.starts_with('C.') || name.starts_with('JS.') {
		return true
	}
	short_name := name.all_after_last('.')
	return short_name.len == 0 || short_name[0].is_capital()
}

fn (mut tc TypeChecker) check_invalid_test_file_name(id flat.NodeId, node flat.Node) {
	if tc.is_test || node.value == 'main' || !tc.should_check_source_name(id) {
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

// relaxes_identifier_case reports whether the declaration `id` belongs to a
// `@[generated]` module. Code generators may keep their own spelling there:
// camelCase, a leading `_`, and type names that do not begin with a capital
// letter. Only type names may begin with one, since V's grammar relies on that
// to tell `Type{}`/`Type(x)` from values. Nothing else changes for such modules.
fn (tc &TypeChecker) relaxes_identifier_case(id flat.NodeId) bool {
	if tc.generated_files.len == 0 {
		return false
	}
	if tc.valid_node_id(id) {
		if file := tc.a.source_files[tc.a.node(id).pos.id] {
			return tc.generated_files[file.name]
		}
	}
	return tc.generated_files[tc.cur_file]
}

// ident_may_be_type reports whether the identifier `name`, the base of a selector
// `name.member`, can name a type. V type names begin with a capital letter. Only
// `@[generated]` files can use other type names, so there a name that does not begin
// with one is a type only when such a type is declared and no local shadows it.
fn (tc &TypeChecker) ident_may_be_type(name string) bool {
	if name.len == 0 {
		return false
	}
	if name[0].is_capital() {
		return true
	}
	if tc.generated_files.len == 0 || !tc.generated_files[tc.cur_file] {
		return false
	}
	if _ := tc.non_file_scope_type(name) {
		return false
	}
	return tc.type_name_known(flat.source_type_id(name))
}

// module_member_may_be_type reports whether `name` in `mod.name` can name a type of the
// module imported as `mod`. V type names begin with a capital letter. Only `@[generated]`
// modules can declare other type names, so such a name is a type only when the module
// really declares it: `os.args` stays a const even when the build has generated code.
fn (tc &TypeChecker) module_member_may_be_type(mod_alias string, name string) bool {
	if name.len == 0 {
		return false
	}
	if name[0].is_capital() {
		return true
	}
	if tc.generated_files.len == 0 {
		return false
	}
	module_name := tc.resolve_import_alias(mod_alias) or { mod_alias }
	qualified_id := flat.source_type_id('${module_name}.${name}')
	return tc.type_symbol_known(qualified_id) || tc.resolve_enum_name(qualified_id) != none
}

fn (mut tc TypeChecker) check_snake_case_name(id flat.NodeId, name string, identifier string, pos token.Pos) {
	if tc.translated_files[tc.cur_file] || name.starts_with('C.') || name.starts_with('JS.') {
		return
	}
	// Module names must keep matching their directory, even in generated code.
	if identifier != 'module name' && tc.relaxes_identifier_case(id) {
		tc.check_generated_value_name(id, name, identifier, pos)
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

// check_generated_value_name enforces the naming rules `@[generated]` keeps for names
// that are not types. Only type names can begin with an uppercase letter. The rest keep
// the generated C code valid: V joins names with `__` to build C names (`mod__fn`,
// `type__method`), C reserves names that begin with `_` and an uppercase letter, and
// the C code names its temporaries `_t1`, `_a2` and so on.
fn (mut tc TypeChecker) check_generated_value_name(id flat.NodeId, name string, identifier string, pos token.Pos) {
	short_name := name.all_after_last('.')
	// Like in ordinary files, the names the compiler itself declares are exempt.
	if short_name.len == 0 || short_name == '_' || short_name.starts_with('__v3_')
		|| source_name_is_numbered_string_symbol(short_name) {
		return
	}
	if short_name[0].is_capital() {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot start with an uppercase letter, only type names can', id, pos)
	} else if short_name.contains('__') {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot contain `__`, V uses it to build C names', id, pos)
	} else if short_name.len > 1 && short_name[0] == `_` && short_name[1].is_capital() {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot start with `_` and an uppercase letter, C reserves such names', id, pos)
	} else if generated_name_is_c_temporary_name(short_name) {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` is reserved for the temporaries of the generated C code', id, pos)
	}
}

// generated_name_is_c_temporary_name reports names like `_t1`: `_`, lowercase letters,
// then digits. The generated C code names its temporaries that way.
fn generated_name_is_c_temporary_name(name string) bool {
	if name.len < 2 || name[0] != `_` || !name[name.len - 1].is_digit() {
		return false
	}
	mut i := 1
	for i < name.len && name[i] >= `a` && name[i] <= `z` {
		i++
	}
	for i < name.len && name[i].is_digit() {
		i++
	}
	return i == name.len
}

// check_generated_type_name enforces the naming rules `@[generated]` keeps for type
// names. Their C names join the type and its members with `__`, so a type name cannot
// contain `__` or end with `_`: methods `_b` of `a` and `b` of `a_` would both become
// `a___b`. A type cannot share the name of a builtin function either, because the call
// `name(x)` would then be parsed as a cast.
fn (mut tc TypeChecker) check_generated_type_name(node_id flat.NodeId, node flat.Node, pos token.Pos) {
	if !tc.relaxes_identifier_case(node_id) || !tc.should_check_source_name(node_id) {
		return
	}
	name := node.value.all_after_last('.')
	// Anonymous structs get names like `AnonStruct__x2e_...` from the parser.
	if name.len == 0 || is_anonymous_aggregate_name(name) {
		return
	}
	identifier := match node.kind {
		.struct_decl {
			'struct name'
		}
		.interface_decl {
			'interface name'
		}
		.enum_decl {
			'enum name'
		}
		else {
			if node.children_count > 0 {
				'sum type name'
			} else if flat.source_type_syntax(node.typ).fn_prefix() {
				'fn type name'
			} else {
				'type alias name'
			}
		}
	}
	if name.contains('__') {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot contain `__`, V uses it to build C names', node_id, pos)
	} else if name.ends_with('_') {
		tc.record_error_at(.duplicate_decl, '${identifier} `${name}` cannot end with `_`, V uses `__` to build the C names of its members', node_id, pos)
	}
	if !name.contains('.') && (tc.fn_type_modules[name] or { '' }) == 'builtin' {
		tc.record_error_at(.duplicate_decl, 'type `${name}` has the same name as a builtin function', node_id, pos)
	}
}

fn source_name_is_numbered_string_symbol(name string) bool {
	return name.len > 5 && name.starts_with('_str_') && name[5..].bytes().all(it >= `0`
		&& it <= `9`)
}

fn (mut tc TypeChecker) check_pascal_case_name(id flat.NodeId, name string, identifier string, pos token.Pos) {
	if tc.translated_files[tc.cur_file] || name.starts_with('C.') || name.starts_with('JS.')
		|| tc.relaxes_identifier_case(id) {
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
	mut name := node.value.all_after_last('.')
	if node.is_static_type_method() {
		if _, method := flat.decode_static_type_method_name(node.value) {
			name = method
		}
	}
	if !node.value.contains('.') && !node.is_static_type_method() {
		if _ := tc.selective_import_text_candidates(name) {
			if !tc.cur_file.ends_with('.vsh') {
				tc.record_error_at(.duplicate_decl, 'cannot redefine imported function `${name}`', id,
					tc.type_name_diagnostic_pos(id, name))
			}
		}
	}
	if !node.value.contains('.') && !node.is_static_type_method()
		&& tc.cur_module in ['', 'main'] && is_builtin_type_name(name) {
		tc.record_error_at(.duplicate_decl, 'top level declaration cannot shadow builtin type', id, tc.fn_declaration_diagnostic_pos(node))
	}
	// `@[generated]` types need not start with a capital letter, so a function can
	// share a type's name; `name(x)` would then be parsed as a cast. The function can
	// be declared in an ordinary file of the module, so check every file.
	if !node.value.contains('.') && !node.is_static_type_method()
		&& tc.generated_files.len > 0 && tc.type_name_known_in_current_module(flat.source_type_id(name)) {
		tc.record_error_at(.duplicate_decl, 'function `${name}` has the same name as a type', id, tc.fn_declaration_diagnostic_pos(node))
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
	tc.check_snake_case_name(id, name, if node.value.contains('.') || node.is_static_type_method() {
		'method name'
	} else {
		'function name'
	}, tc.fn_declaration_diagnostic_pos(node))
}

fn (mut tc TypeChecker) check_fn_if_attribute_return(id flat.NodeId, node flat.Node) {
	if tc.parse_type(node.typ) is Void {
		return
	}
	// The attribute is looked for in the source of the declaration's file: a file
	// without any `@[if ` has none. (The header position is in the same file.)
	if !tc.source_file_has_if_attribute(node.pos.id) {
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

// source_file_has_if_attribute reports whether the source of the file with the
// id `file_id` has an `@[if ` attribute.
fn (mut tc TypeChecker) source_file_has_if_attribute(file_id int) bool {
	if known := tc.if_attribute_files[file_id] {
		return known
	}
	mut has := false
	if file := tc.a.source_files[file_id] {
		if source := tc.source_texts_by_file[file.name] {
			has = source.contains('@[if ')
		}
	}
	tc.if_attribute_files[file_id] = has
	return has
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

fn (tc &TypeChecker) fn_receiver_declared_type_pos(node flat.Node, receiver flat.Node) (string, token.Pos) {
	text, pos := tc.fn_receiver_source_text_pos(node)
	if text.len < 2 {
		return receiver.typ_text().trim_left('&'), pos
	}
	mut start := 1
	for start < text.len - 1 && text[start] in [` `, `\t`] {
		start++
	}
	for modifier in ['mut', 'shared'] {
		if text[start..].starts_with(modifier + ' ') {
			start += modifier.len
			for start < text.len - 1 && text[start] in [` `, `\t`] {
				start++
			}
			break
		}
	}
	if receiver.value != '_' && text[start..].starts_with(receiver.value) {
		start += receiver.value.len
		for start < text.len - 1 && text[start] in [` `, `\t`] {
			start++
		}
	}
	mut end := text.len - 1
	for end > start && text[end - 1] in [` `, `\t`] {
		end--
	}
	return text[start..end], token.new_span(pos.id, pos.offset + start, pos.offset + end)
}

fn (mut tc TypeChecker) check_sumtype_builtin_method_override(id flat.NodeId, node flat.Node) {
	if !tc.should_check_source_name(id) || tc.cur_module == 'builtin' || !node.value.contains('.') {
		return
	}
	method := node.value.all_after_last('.')
	if method !in ['type_idx', 'type_name'] {
		return
	}
	// The receiver is the type part of the method name text.
	receiver_id := flat.source_type_id(node.value.all_before_last('.'))
	qualified_id := tc.qualify_type_name(receiver_id)
	if tc.sum_types.contains(receiver_id) || tc.sum_types.contains(qualified_id) {
		tc.record_error_at(.duplicate_decl, 'method overrides built-in sum type method', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

// interface_internal_field_names are the members of every interface's C struct.
const interface_internal_field_names = ['_typ', '_object', '_object_is_boxed']

fn (mut tc TypeChecker) check_interface_member_names(node flat.Node) {
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.node(field_id)
		if field.kind != .interface_field || field.value.len == 0 {
			continue
		}
		if field.op != .dot && field.typ == 0 {
			// Embedded interfaces use their type name as `value`; they are not fields.
			continue
		}
		if field.op != .dot && field.value in interface_internal_field_names
			&& tc.relaxes_identifier_case(field_id) && tc.should_check_source_name(field_id) {
			// The C struct of an interface already has these members.
			tc.record_error_at(.duplicate_decl, 'field name `${field.value}` is reserved for the interface value itself', field_id, tc.node_value_diagnostic_pos(field_id))
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
		if field.kind == .interface_field && field.op != .dot && field.typ != 0
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
	// The receiver is the type part of the method name text.
	receiver_id := flat.source_type_id(receiver)
	qualified_receiver_id := tc.qualify_type_name(receiver_id)
	fields := tc.structs[qualified_receiver_id] or { tc.structs[receiver_id] or { return } }
	if fields.any(it.name == method && unalias_type(it.typ) is FnType)
		&& !tc.struct_has_invalid_reference_default(receiver_id) {
		tc.record_error_at(.duplicate_decl, 'type `${receiver.all_after_last('.')}` has both field and method named `${method}`', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (tc &TypeChecker) struct_has_invalid_reference_default(receiver_id u32) bool {
	decl := tc.source_struct_decl_for_name(receiver_id) or { return false }
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

// check_generated_parameter_name applies the `@[generated]` naming rules to a parameter.
// Ordinary parameter names are not checked, but generated code must not use the names
// of the C temporaries: `fn f(_t1 int)` would read an uninitialized `_t1` in C.
fn (mut tc TypeChecker) check_generated_parameter_name(id flat.NodeId, param flat.Node, pos token.Pos) {
	if param.value.len == 0 || param.value == '_' || !tc.relaxes_identifier_case(id)
		|| !tc.should_check_source_name(id) {
		return
	}
	tc.check_generated_value_name(id, param.value, 'parameter name', pos)
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
	name_pos := tc.type_declaration_name_pos(node_id)
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
	prefix := if tc.checker_fixture_mode { '' } else { 'invalid test signature: ' }
	if param_count != 0 {
		tc.record_error_at(.call_arg_mismatch, '${prefix}test functions should take 0 parameters', id, tc.fn_declaration_diagnostic_pos(node))
	}
	return_type_id := trimmed_source_type_id(node.typ)
	if return_type_id == 0 || return_type_id in [flat.source_type_void, checker_source_option_marker,
		checker_source_result_marker] {
		return
	}
	if !flat.source_type_syntax(return_type_id).option_result_void_prefix() {
		tc.record_error_at(.return_mismatch, '${prefix}test functions should either return nothing at all, or be marked to return `?` or `!`', id, tc.fn_declaration_diagnostic_pos(node))
	}
}

fn (mut tc TypeChecker) check_test_file_has_test_fn() {
	mut has_test_fn := false
	mut first_test_file_node := flat.NodeId(-1)
	mut first_source_node := flat.NodeId(-1)
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		if node.kind == .file && is_regular_v_test_file(node.value) {
			// The trailing file node has a source position even when the file
			// contains no active declarations; the leading marker does not.
			if int(first_test_file_node) < 0 && node.pos.is_valid() {
				first_test_file_node = flat.NodeId(i)
			}
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
	if int(first_test_file_node) < 0 || has_test_fn {
		return
	}
	if int(first_source_node) < 0 {
		first_source_node = first_test_file_node
	}
	first_node := tc.a.nodes[int(first_source_node)]
	mut pos := if first_node.kind == .fn_decl {
		tc.fn_declaration_diagnostic_pos(first_node)
	} else {
		first_node.pos
	}
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
	tc.cur_fn_ret_type = builtin_void_type
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
	file_id := int(pos.id)
	if file_id >= 0 && file_id < tc.malformed_const_source_states.len
		&& tc.malformed_const_source_states[file_id] == 1 {
		return none
	}
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
	if !source[line_start..pos.offset].trim_left(' \t').starts_with('import ') {
		return false
	}
	// Tokenize from the file start so comments opened on an earlier line stay
	// comments. Real separators end imports; quoted and commented semicolons do not.
	prefix := source[..pos.offset]
	mut files := token.FileSet.new()
	mut prefix_file := files.add_file('<script import prefix>', prefix.len)
	prefix_file.index_lines(prefix)
	mut lexer := scanner.new_scanner(pref.new_preferences(), .skip_interpolation)
	lexer.init(prefix_file, prefix)
	mut at_statement_start := true
	mut is_import := false
	for {
		tok := lexer.scan()
		if tok == .eof {
			break
		}
		if tok == .semicolon {
			// The scanner also inserts a semicolon at EOF; the incomplete prefix
			// must keep the import state rather than treating that as a separator.
			if lexer.pos < prefix.len && prefix[lexer.pos] in [`;`, `\n`, `\r`] {
				at_statement_start = true
				is_import = false
			}
			continue
		}
		if at_statement_start {
			is_import = tok == .key_import
			at_statement_start = false
		}
	}
	return is_import
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
	// The walk binds scopes and resolves names in every reachable body; for a
	// compiler build that is the whole program. All of that is scratch, so a
	// scoped check runs it on a disposable fork and keeps only the reached names.
	// Forks read the type cache through the overlay the scoped checks install.
	if !tc.scope_parallel_check_workers || isnil(tc.type_cache) || isnil(tc.type_cache.base) {
		tc.collect_selected_file_called_fns_walk()
		return
	}
	scope := check_worker_scope_begin(true)
	mut walker := tc.fork_for_parallel_check()
	// Reachability reads prior diagnostics when resolving erroneous receivers.
	// The owner stays idle, and this walk does not append diagnostics.
	walker.errors = tc.errors
	mut body_scopes := []voidptr{}
	if walker.selected_file_reachability_parallel_available() {
		walker.collect_selected_file_called_fns_walk()
	} else if !walker.collect_selected_file_called_fns_walk_parallel(tc, mut body_scopes) {
		walker.collect_selected_file_called_fns_walk()
	}
	check_worker_scope_leave(scope)
	mut called := map[string]bool{}
	called.reserve(u32(walker.selected_file_called_fns.len))
	for name, _ in walker.selected_file_called_fns {
		called[name.clone()] = true
	}
	tc.selected_file_called_fns = called.move()
	free_check_worker_scopes(checker_worker_pool(tc.a), body_scopes)
	check_worker_scope_free(scope)
}

fn (mut tc TypeChecker) collect_selected_file_called_fns_walk() {
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	saved_scope := tc.cur_scope
	saved_scope_pool_index := tc.scope_pool_index
	// Reachability owns a separate phase; preserve any caller memo unchanged.
	saved_memo := tc.body_resolve_memo
	tc.body_resolve_memo = unsafe { nil }
	defer { tc.body_resolve_memo = saved_memo }
	tc.collect_selected_file_root_called_fns()
	tc.collect_selected_file_called_fns_transitively()
	tc.cur_file = saved_file
	tc.cur_module = saved_module
	tc.cur_scope = saved_scope
	tc.scope_pool_index = saved_scope_pool_index
}

// collect_selected_file_root_called_fns seeds the called set and the worklist
// from the top-level code and the functions of the selected files.
fn (mut tc TypeChecker) collect_selected_file_root_called_fns() {
	tc.selected_file_called_fns = map[string]bool{}
	tc.selected_file_worklist = []string{}
	tc.cur_file = ''
	tc.cur_module = ''
	tc.cur_scope = tc.file_scope
	mut smartcast_keys := &SmartcastKeyFilter{}
	mut prev_top_level := -1
	for i in tc.top_level_idx {
		range_lo := prev_top_level + 1
		prev_top_level = i
		node := tc.a.node(flat.NodeId(i))
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
				// A fn subtree is the node range since the previous top-level node.
				smartcast_keys.arm(range_lo, i)
				tc.smartcast_key_filter = smartcast_keys
				tc.arm_body_resolve_memo(range_lo, i)
				tc.collect_selected_file_fn_body_called_fns(node)
				tc.disarm_body_resolve_memo()
				tc.smartcast_key_filter = unsafe { nil }
			}
			else {}
		}
	}
}

// collect_selected_decl_called_fns walks the body of `decl` for the functions it
// calls, with the smartcast keys of its node range in `smartcast_keys`.
fn (mut tc TypeChecker) collect_selected_decl_called_fns(decl SelectedFnDecl, mut smartcast_keys SmartcastKeyFilter) {
	tc.cur_file = decl.file
	tc.cur_module = decl.mod
	// selected_file_fn_decls takes the cost as the span since the previous
	// top-level node: the fn subtree.
	smartcast_keys.arm(decl.idx - decl.cost + 1, decl.idx)
	tc.smartcast_key_filter = smartcast_keys
	tc.arm_body_resolve_memo(decl.idx - decl.cost + 1, decl.idx)
	tc.collect_selected_file_fn_body_called_fns(tc.a.node(flat.NodeId(decl.idx)))
	tc.disarm_body_resolve_memo()
	tc.smartcast_key_filter = unsafe { nil }
}

// SelectedFnDecl locates the first fn_decl node for a qualified name, with the
// file/module context needed to walk its body.
struct SelectedFnDecl {
	idx  int
	file string
	mod  string
	cost int
}

fn (mut tc TypeChecker) collect_selected_file_called_fns_transitively() {
	// Index the first user-code fn_decl per qualified name once, then chase the
	// worklist of newly discovered called names. The former fixpoint re-scanned
	// every node per round; this walks each reachable body exactly once (the
	// first declaration wins for duplicate names, matching the old scan order).
	decls := tc.selected_file_fn_decls()
	tc.cur_scope = tc.file_scope
	mut visited := map[string]bool{}
	can_parallel := tc.selected_file_reachability_parallel_available()
	mut smartcast_keys := &SmartcastKeyFilter{}
	for tc.selected_file_worklist.len > 0 {
		if !can_parallel || tc.selected_file_worklist.len < min_parallel_selected_fn_bodies {
			name := tc.selected_file_worklist.pop()
			if name in visited {
				continue
			}
			visited[name] = true
			decl := decls[name] or { continue }
			tc.collect_selected_decl_called_fns(decl, mut smartcast_keys)
			continue
		}
		mut frontier := []SelectedFnDecl{}
		for tc.selected_file_worklist.len > 0 {
			name := tc.selected_file_worklist.pop()
			if name in visited {
				continue
			}
			visited[name] = true
			decl := decls[name] or { continue }
			frontier << decl
		}
		if tc.collect_selected_file_frontier_parallel(frontier) {
			continue
		}
		for decl in frontier {
			tc.collect_selected_decl_called_fns(decl, mut smartcast_keys)
		}
	}
}

// selected_file_fn_decls indexes the first user-code fn_decl per qualified name.
fn (tc &TypeChecker) selected_file_fn_decls() map[string]SelectedFnDecl {
	mut decls := map[string]SelectedFnDecl{}
	mut cur_file := ''
	mut cur_module := ''
	mut prev_top_level := -1
	for i in tc.top_level_idx {
		cost := int_max(i - prev_top_level, 1)
		prev_top_level = i
		node := tc.a.node(flat.NodeId(i))
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
						cost: cost
					}
				}
			}
			else {}
		}
	}
	return decls
}

// A binding or scope transition can change a pre-check resolution of an AST
// node that the reachability walker previously visited through its parent.
@[inline]
fn (tc &TypeChecker) selected_file_resolve_context_changed() {
	mut memo := tc.body_resolve_memo
	if !isnil(memo) && memo.active {
		memo.advance_context()
	}
}

fn (mut tc TypeChecker) collect_selected_file_top_level_called_fns(node &flat.Node) {
	tc.push_scope()
	tc.selected_file_resolve_context_changed()
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
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
	tc.selected_file_resolve_context_changed()
}

fn (mut tc TypeChecker) collect_selected_file_fn_body_called_fns(node &flat.Node) {
	tc.push_scope()
	tc.selected_file_resolve_context_changed()
	mut reached_params := false
	for i in 0 .. node.children_count {
		child := tc.a.child_node(node, i)
		if node.kind == .fn_literal && !reached_params && child.kind == .ident {
			// Captures precede parameters. Bind them before checking the body,
			// and keep scanning so prefix-only parameter scans see the parameters.
			if child.value.len > 0 {
				captured := tc.cur_scope.lookup(child.value) or { unknown_type('captured value') }
				tc.cur_scope.insert(child.value, captured)
				tc.selected_file_resolve_context_changed()
			}
			continue
		}
		if child.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		reached_params = true
		if child.value.len > 0 {
			tc.cur_scope.insert(child.value, tc.parse_type(child.typ))
			tc.selected_file_resolve_context_changed()
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind != .param {
			tc.collect_selected_file_node_called_fns(child_id)
		}
	}
	tc.pop_scope()
	tc.selected_file_resolve_context_changed()
}

fn (mut tc TypeChecker) collect_selected_file_node_called_fns(id flat.NodeId) {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return
	}
	node := tc.a.node(id)
	match node.kind {
		.block {
			tc.push_scope()
			tc.selected_file_resolve_context_changed()
			for i in 0 .. node.children_count {
				tc.collect_selected_file_node_called_fns(tc.a.child(node, i))
			}
			tc.pop_scope()
			tc.selected_file_resolve_context_changed()
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
		.fn_literal {
			tc.collect_selected_file_fn_body_called_fns(node)
			return
		}
		.lambda_expr {
			if node.children_count == 0 {
				return
			}
			tc.push_scope()
			tc.selected_file_resolve_context_changed()
			for i in 0 .. node.children_count - 1 {
				param := tc.a.child_node(node, i)
				tc.cur_scope.insert(param.value, unknown_type('lambda parameter'))
				tc.selected_file_resolve_context_changed()
			}
			tc.collect_selected_file_node_called_fns(tc.a.child(node, node.children_count - 1))
			tc.pop_scope()
			tc.selected_file_resolve_context_changed()
			return
		}
		.call {
			if name := tc.selected_file_call_name(node) {
				tc.enqueue_selected_file_fn(name)
				callee := tc.a.child_node(node, 0)
				if callee.kind in [.ident, .selector] && tc.fn_signature_known(name) {
					// A resolved callee already keeps its declaration. Visit its receiver
					// and the arguments without resolving the callee again as a fn value.
					for i in 0 .. callee.children_count {
						tc.collect_selected_file_node_called_fns(tc.a.child(callee, i))
					}
					for i in 1 .. node.children_count {
						tc.collect_selected_file_node_called_fns(tc.a.child(node, i))
					}
					return
				}
			}
		}
		.ident, .selector {
			// A callback or stored function value keeps its declaration's body
			// just like a direct call. Resolve it with the current lexical scope
			// so a local sharing a function or import name does not keep it.
			if name := tc.fn_value_decl_key(node) {
				tc.enqueue_selected_file_fn(name)
			} else if node.kind == .selector && node.children_count > 0
				&& tc.selected_file_method_suffix_known(node.value)
				&& tc.fn_value_shadowed_by_value(node)
				&& tc.selector_declared_value_type(node) == none {
				if name := tc.selected_file_receiver_method_name(tc.a.child(node, 0), node.value) {
					tc.enqueue_selected_file_fn(name)
				}
			}
		}
		else {}
	}

	for i in 0 .. node.children_count {
		tc.collect_selected_file_node_called_fns(tc.a.child(node, i))
	}
}

fn (mut tc TypeChecker) enqueue_selected_file_fn(name string) {
	if name !in tc.selected_file_called_fns {
		tc.selected_file_called_fns[name] = true
		tc.selected_file_worklist << name
	}
}

fn (mut tc TypeChecker) collect_selected_file_for_in_called_fns(node &flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		for i in 0 .. node.children_count {
			tc.collect_selected_file_node_called_fns(tc.a.child(node, i))
		}
		return
	}
	tc.push_scope()
	tc.selected_file_resolve_context_changed()
	key_id := tc.a.child(node, 0)
	val_id := tc.a.child(node, 1)
	container_id := tc.a.child(node, 2)
	tc.collect_selected_file_node_called_fns(container_id)
	has_val := int(val_id) >= 0
	if header == 4 {
		tc.insert_selected_file_decl_binding_type(key_id, tc.range_loop_var_type(container_id, tc.a.child(node, 3)))
		tc.collect_selected_file_node_called_fns(tc.a.child(node, 3))
	} else {
		clean := tc.for_in_iterable_type(container_id)
		yields_ref := node.op == .amp || tc.for_in_iterable_yields_ref(container_id)
		if clean is Array {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, builtin_int_type)
				tc.insert_selected_file_decl_binding_type(val_id, elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, elem_type)
			}
		} else if clean is ArrayFixed {
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, builtin_int_type)
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
				tc.insert_selected_file_decl_binding_type(key_id, builtin_int_type)
				tc.insert_selected_file_decl_binding_type(val_id, builtin_u8_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, builtin_u8_type)
			}
		} else if generic_name := tc.iterator_unbounded_next_generic(clean) {
			_ = generic_name
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, builtin_int_type)
				tc.insert_selected_file_decl_binding_type(val_id, unknown_type('unbounded iterator generic'))
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, unknown_type('unbounded iterator generic'))
			}
		} else if elem_type := tc.iterator_for_in_elem_type(clean) {
			if has_val {
				tc.insert_selected_file_decl_binding_type(key_id, builtin_int_type)
				tc.insert_selected_file_decl_binding_type(val_id, elem_type)
			} else {
				tc.insert_selected_file_decl_binding_type(key_id, elem_type)
			}
		} else {
			container := tc.a.node(container_id)
			if container.kind == .range {
				tc.insert_selected_file_decl_binding_type(key_id, tc.range_loop_var_type(tc.a.child(container, 0), tc.a.child(container, 1)))
			}
		}
	}
	for i in header .. node.children_count {
		tc.collect_selected_file_node_called_fns(tc.a.child(node, i))
	}
	tc.pop_scope()
	tc.selected_file_resolve_context_changed()
}

fn (mut tc TypeChecker) collect_selected_file_decl_assign_called_fns(node &flat.Node) {
	if node.children_count >= 3 {
		rhs_id := tc.a.child(node, 1)
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
		lhs_id := tc.a.child(node, i)
		rhs_id := tc.a.child(node, i + 1)
		tc.collect_selected_file_node_called_fns(rhs_id)
		tc.insert_selected_file_decl_binding(lhs_id, rhs_id, node)
		i += 2
	}
}

fn (mut tc TypeChecker) insert_selected_file_decl_binding(lhs_id flat.NodeId, rhs_id flat.NodeId, node &flat.Node) {
	if int(lhs_id) < 0 || int(lhs_id) >= tc.a.nodes.len {
		return
	}
	lhs := tc.a.node(lhs_id)
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	typ := if node.children_count == 2 && node.typ != 0 {
		*tc.parse_type(node.typ)
	} else {
		tc.decl_assign_inferred_type(rhs_id)
	}
	tc.insert_selected_file_decl_binding_type(lhs_id, typ)
}

fn (mut tc TypeChecker) insert_selected_file_decl_binding_type(lhs_id flat.NodeId, typ Type) {
	if int(lhs_id) < 0 || int(lhs_id) >= tc.a.nodes.len {
		return
	}
	lhs := tc.a.node(lhs_id)
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	if typ is MultiReturn || typ is Void || typ is Unknown {
		return
	}
	tc.cur_scope.insert(lhs.value, typ)
	tc.selected_file_resolve_context_changed()
}

fn (tc &TypeChecker) selected_file_call_name(node &flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(node, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		return tc.selected_file_call_base_name(tc.a.child_node(fn_node, 0))
	}
	return tc.selected_file_call_base_name(fn_node)
}

fn (tc &TypeChecker) selected_file_call_base_name(fn_node &flat.Node) ?string {
	match fn_node.kind {
		.ident {
			// A callable value, such as a function-typed parameter, shadows the
			// declared or selectively imported function sharing its name.
			if tc.bare_call_resolves_to_value(fn_node.value) {
				return none
			}
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
			base_id := tc.a.child(fn_node, 0)
			base_node := tc.a.node(base_id)
			if base_node.kind == .ident {
				if tc.cur_scope.contains(base_node.value) {
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

// bare_call_resolves_to_value reports whether call resolution selects a binding in
// scope for `name(...)` instead of a function declaration.
fn (tc &TypeChecker) bare_call_resolves_to_value(name string) bool {
	typ := tc.cur_scope.lookup(name) or { return false }
	if _ := fn_type_from_type(typ) {
		return true
	}
	return typ is Unknown || unresolved_generic_receiver_type(typ)
}

fn (tc &TypeChecker) selected_file_receiver_method_name(base_id flat.NodeId, method string) ?string {
	if method == '' || !tc.selected_file_method_suffix_known(method) {
		return none
	}
	base_type := tc.resolve_type(base_id)
	clean := unwrap_pointer(base_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name == 0 {
		return none
	}
	mut recv := clean
	if recv is Alias {
		recv = recv.base_type
	}
	if recv !is Array && recv !is Map {
		// Same two candidates, in the same order, as receiver_method_name_candidates;
		// the escaped name is built only when the plain one does not resolve.
		type_text := flat.source_type_name(type_name)
		plain := '${type_text}.${method}'
		if tc.fn_signature_known(plain)
			&& tc.method_can_be_called_on_receiver(base_type, method, plain) {
			return plain
		}
		escaped := '${type_text}.@${method}'
		if tc.fn_signature_known(escaped)
			&& tc.method_can_be_called_on_receiver(base_type, method, escaped) {
			return escaped
		}
	} else if tc.method_suffix_prescreen && tc.top_level_idx.len > 0 && method.index_u8(`.`) < 0
		&& method !in tc.collection_method_short_names {
		// No array or map receiver declares this method, so of all the
		// receiver_method_name_candidates only these can resolve (same order).
		if found := tc.selected_file_collection_method_without_collection_decl(base_type,
			recv, type_name, method)
		{
			return found
		}
	} else {
		for method_name in receiver_method_name_candidates(clean, method, tc.cur_module) {
			if !tc.fn_signature_known(method_name) {
				continue
			}
			if !tc.method_can_be_called_on_receiver(base_type, method, method_name) {
				continue
			}
			return method_name
		}
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

// selected_file_collection_method_without_collection_decl tries the candidates of
// receiver_method_name_candidates whose receiver is not spelled as an array or map
// type: the receiver's own name (plain, then escaped), then `array.m` or `map.m`.
fn (tc &TypeChecker) selected_file_collection_method_without_collection_decl(base_type Type, recv Type, type_name u32, method string) ?string {
	type_text := flat.source_type_name(type_name)
	plain := '${type_text}.${method}'
	if tc.fn_signature_known(plain) && tc.method_can_be_called_on_receiver(base_type, method, plain) {
		return plain
	}
	escaped := '${type_text}.@${method}'
	if tc.fn_signature_known(escaped)
		&& tc.method_can_be_called_on_receiver(base_type, method, escaped) {
		return escaped
	}
	generic := if recv is Array { 'array.${method}' } else { 'map.${method}' }
	if tc.fn_signature_known(generic)
		&& tc.method_can_be_called_on_receiver(base_type, method, generic) {
		return generic
	}
	return none
}

// The collected suffix index covers ordinary, escaped and generic receiver
// declarations. A missing suffix cannot resolve on any receiver; fields need
// no receiver-type or embedded-method queries in that case.
fn (tc &TypeChecker) selected_file_method_suffix_known(method string) bool {
	if !tc.method_suffix_prescreen || tc.top_level_idx.len == 0 {
		return true
	}
	if method.contains('.') {
		return method in tc.receiver_method_suffix_index
			|| '@${method}' in tc.receiver_method_suffix_index
	}
	return method in tc.receiver_method_short_names
}

fn (mut tc TypeChecker) check_export_attrs() {
	if tc.valid_diagnostic_fast {
		return
	}
	// Every error below needs an exported function or an `export` attribute:
	// without one, the walks over every declaration find nothing.
	if tc.a.export_fn_names.len == 0 && !tc.has_export_attribute() {
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
				// The parser records the exports of main and builtin under their bare
				// names, which `qname` already is there. In another module, the bare
				// name belongs to a different function: `dlmalloc.realloc` must not
				// take the export of builtin's `realloc`.
				export_name := tc.a.export_fn_names[qname] or {
					tc.a.export_fn_names['${cur_module}.${node.value}'] or { continue }
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
					if p.value.len == 0 || p.typ == 0 {
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

// has_export_attribute reports whether a declaration has an `export`
// attribute, with a value or without one.
fn (tc &TypeChecker) has_export_attribute() bool {
	for _, attrs in tc.declaration_attributes {
		for raw in attrs {
			if raw.trim_left(' \t').starts_with('export')
				&& attribute_name_is(raw, 'export') {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) declaration_attribute_value(node_id flat.NodeId, name string) ?string {
	for raw in tc.declaration_attributes[int(node_id)] {
		if !attribute_name_is(raw, name) || !raw.contains(':') {
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

// is_c_backend_test_file reports whether path names a test file the C backend compiles.
// It defers to pref, so the checker and the driver agree on every spelling, including
// architecture-qualified tests such as `foo_test.arm64.v`.
fn is_c_backend_test_file(path string) bool {
	file := path_leaf_view(path)
	if file.ends_with('_test.vv') {
		return true
	}
	return pref.is_test_file_for_backend(file, 'c')
}

fn is_regular_v_test_file(path string) bool {
	if path.ends_with('_test.v') || path.ends_with('_test.vv') {
		return true
	}
	// Every test file spelling (`_test.v`, `_test.vv`, `_test.<backend>.v`, ...)
	// contains `_test` in its leaf name; skip the allocating pref check otherwise.
	if !path_leaf_view(path).contains('_test') {
		return false
	}
	// Backend-qualified test files such as `foo_test.c.v` are test files
	// too: the test driver compiles them as tests, so the test-only
	// diagnostic relaxations must apply to them as well.
	return is_c_backend_test_file(path)
}

// is_exported_fn reports whether the function `name` declared in `module_name` has
// an `@[export]` attribute, looked up the way the export checks look it up.
fn (tc &TypeChecker) is_exported_fn(module_name string, name string) bool {
	return export_qualified_fn_name(module_name, name) in tc.a.export_fn_names
		|| '${module_name}.${name}' in tc.a.export_fn_names
}

fn export_qualified_fn_name(module_name string, name string) string {
	if name.contains('.') {
		return name
	}
	if module_name == '' || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	return '${module_name}.${name}'
}

fn export_natural_c_symbol(module_name string, name string) string {
	if module_name == 'builtin' && name == 'free' {
		return 'v_free'
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
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
	if name == '' {
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

// param_source_type_is_shared inspects a source annotation without decoding unpadded text.
fn param_source_type_is_shared(source_id u32) bool {
	return flat.source_type_shape(trimmed_source_type_id(source_id)).has(.shared_type)
}

fn decl_assign_is_shared_marker(value string) bool {
	return value == 'shared' || value.starts_with('shared:')
}

fn (tc &TypeChecker) implicit_veb_ctx_type() Type {
	return *tc.parse_type(checker_source_mut_context)
}

fn (tc &TypeChecker) fn_needs_implicit_veb_ctx(node flat.Node) bool {
	return tc.fn_is_veb_app_handler(node) && !tc.fn_has_veb_context_param(node)
}

fn (tc &TypeChecker) fn_is_veb_app_handler(node flat.Node) bool {
	return tc.fn_returns_veb_result(node) && tc.fn_has_receiver_param(node)
		&& !tc.fn_receiver_type_is_context(node) && tc.type_name_known_in_current_module(checker_source_context)
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
	if first.kind != .param || first.typ == 0 {
		return false
	}
	receiver := node.value.all_before_last('.').all_after_last('.')
	param_type := flat.source_type_syntax(flat.source_type_syntax(first.typ).pointer_raw_base()).unqualified()
	// A receiver spelling that was never interned cannot equal the parameter type.
	receiver_id := flat.source_type_existing_id(receiver) or { return false }
	return receiver_id == param_type
}

fn (tc &TypeChecker) fn_receiver_type_is_context(node flat.Node) bool {
	if !tc.fn_has_receiver_param(node) {
		return false
	}
	first := tc.a.child_node(&node, 0)
	return flat.source_type_syntax(flat.source_type_syntax(first.typ).pointer_raw_base()).unqualified() == checker_source_context
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
		if veb_receiver_source_type_id(p.typ) == checker_source_context {
			return true
		}
		if tc.source_type_embeds_veb_context(p.typ) {
			return true
		}
		if tc.is_veb_context_type(tc.parse_type(p.typ)) {
			return true
		}
	}
	return false
}

fn veb_receiver_source_type_id(type_id u32) u32 {
	mut clean_id := trimmed_source_type_id(type_id)
	if flat.source_type_shape(clean_id).has(.mutable_type) {
		clean_id = flat.source_type_syntax(clean_id).raw_base()
	}
	if flat.source_type_shape(clean_id).has(.pointer) {
		clean_id = flat.source_type_syntax(clean_id).pointer_raw_base()
	}
	return clean_id
}

fn (tc &TypeChecker) source_type_embeds_veb_context(raw_type_id u32) bool {
	type_name_id := veb_receiver_source_type_id(raw_type_id)
	if type_name_id == 0 {
		return false
	}
	qualified_id := tc.qualify_resolution_type_name(type_name_id)
	mut seen := map[u32]bool{}
	return tc.source_struct_embeds_veb_context(qualified_id, tc.cur_file, mut seen)
}

fn (tc &TypeChecker) source_struct_embeds_veb_context(type_name_id u32, source_file string, mut seen map[u32]bool) bool {
	if type_name_id == veb_context_source_type {
		return true
	}
	if seen[type_name_id] {
		return false
	}
	seen[type_name_id] = true
	decl_idx := tc.first_type_declaration_ids[type_name_id] or { return false }
	decl := tc.a.nodes[decl_idx]
	owner_module_id := if flat.source_type_shape(type_name_id).has(.dotted) {
		flat.source_type_syntax(type_name_id).qualifier()
	} else {
		checker_source_main_module
	}
	decl_file := if file := tc.a.source_files[decl.pos.id] { file.name } else { source_file }
	if decl.kind == .type_decl && decl.children_count == 0 && decl.typ != 0 {
		alias_target_source_id := tc.source_type_name_in_module(decl.typ, owner_module_id, decl_file)

		return tc.source_struct_embeds_veb_context(alias_target_source_id, decl_file, mut seen)
	}
	if decl.kind != .struct_decl {
		return false
	}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		if !source_field_decl_is_embed(field, field_type_id) {
			continue
		}
		embedded_name_source_id := tc.source_type_name_in_module(field_type_id, owner_module_id, decl_file)

		if tc.source_struct_embeds_veb_context(embedded_name_source_id, decl_file, mut seen) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) source_type_name_in_module(raw_type_id u32, module_id u32, source_file string) u32 {
	type_name_id := veb_receiver_source_type_id(raw_type_id)
	if flat.source_type_shape(type_name_id).has(.dotted) {
		return tc.resolve_imported_type_text_in_file(type_name_id, source_file)
	}
	if resolved_id := tc.resolve_selective_import_type_symbol_in_file_id(type_name_id, source_file) {
		return resolved_id
	}
	if module_id != 0 && module_id != checker_source_main_module && module_id != checker_source_builtin {
		return flat.source_type_composition_two(.qualified, module_id, type_name_id)
	}
	return type_name_id
}

fn (tc &TypeChecker) fn_returns_veb_result(node flat.Node) bool {
	raw_type_id := trimmed_source_type_id(node.typ)
	if raw_type_id == veb_result_source_type {
		return true
	}
	// Most declarations cannot return veb.Result. Avoid resolving unrelated
	// expression-derived return types such as `typeof(value)` during collection.
	if flat.source_type_syntax(raw_type_id).unqualified() != checker_source_result {
		return false
	}
	ret := tc.parse_type(raw_type_id)
	return ret.source_type_id() == veb_result_source_type
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
				display_type := param.typ_text().trim_left('&').all_after_last('.')
				tc.record_error_at(.call_arg_mismatch, 'veb app method `${method}` must declare context parameter `${param.value}` as mutable, e.g. `mut ${param.value} ${display_type}`', param_id, tc.node_value_diagnostic_pos(param_id))
			}
			continue
		}
		clean := unalias_type(param_type)
		if clean is String || clean.is_integer() || clean.source_type_id() == flat.source_type_bool || clean is Interface {
			continue
		}
		tc.record_error_at(.call_arg_mismatch, 'veb app method `${method}` parameter `${param.value}` has unsupported type `${param.typ_text()}`; parameters after the context are populated from strings and must be `string`, integer, or `bool`', param_id, tc.node_value_diagnostic_pos(param_id))
	}
}

// is_veb_context_type reports whether typ is veb.Context or embeds it.
pub fn (tc &TypeChecker) is_veb_context_type(typ &Type) bool {
	clean := unalias_type(unwrap_all_pointers(typ))
	if clean.source_type_id() == veb_context_source_type {
		return true
	}
	return tc.receiver_embeds(clean, Type(Struct{
		name_id: veb_context_source_type
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
			mut candidates := tc.known_type_name_candidates()
			if imported_name.contains('.') {
				module_prefix := imported_name.all_before_last('.') + '.'
				module_candidates := candidates.filter(it.starts_with(module_prefix))
				if module_candidates.len > 0 {
					candidates = module_candidates.clone()
				}
			}
			message := util.new_suggestion(imported_name, candidates).say(base)
			tc.record_error_at(.unknown_type, message, node_id, tc.type_diagnostic_pos(node_id, node.typ_text().trim_space()))
		}
		alias_name_id := trimmed_source_type_id(node.typ)
		alias_type := tc.parse_type(alias_name_id)
		if alias_type is Alias && alias_type.base_type is FnType {
			alias_name := flat.source_type_name(alias_name_id)
			// The base is a function type, not another alias, so unalias_type yields it directly;
			// the vc bootstrap compiler mis-generates a `Type(ptr)` cast of the payload field.
			original_type := unalias_type(alias_type).name().replace_once('fn(', 'fn (')
			tc.record_error_at(.unknown_type, 'type `${alias_name}` is an alias, use the original alias type `${original_type}` instead', node_id, tc.type_diagnostic_pos_id(node_id, alias_name_id))
		}
	}
	if node.kind == .fn_decl {
		tc.check_fn_decl_unmentioned_generic_types(node_id, node)
		tc.check_fn_bare_generic_signature_types(node_id, node)
	}
	if node.kind in [.fn_decl, .struct_decl, .interface_decl, .type_decl]
		&& node.generic_constraint_ids().len > 0 {
		tc.check_generic_constraint_decls(node_id, node)
	}
	if node.kind == .type_decl && node.children_count > 0 {
		tc.check_implicit_generic_sumtype_decl(node_id, node)
	}
	if node.kind == .type_decl
		&& (node.children_count > 0 || flat.source_type_syntax(node.typ).union_parts().len > 1) {
		tc.check_sum_type_decl(node_id, node)
	}
	if node.kind == .fn_decl && (node.value.contains('.') || node.is_static_type_method()) {
		is_marked_static := node.is_static_type_method()
		receiver_name := if is_marked_static {
			if receiver, _ := flat.decode_static_type_method_name(node.value) {
				receiver.all_after_last('.')
			} else {
				''
			}
		} else {
			node.value.all_before_last('.').all_after_last('.')
		}
		mut is_static := is_marked_static || node.children_count == 0
		if node.children_count > 0 {
			first := tc.a.child_node(&node, 0)
			is_static = is_marked_static || first.kind != .param || first.op != .dot
		}
		if is_static && !tc.type_name_known(flat.source_type_id(receiver_name)) {
			tc.record_error_at(.unknown_type, 'unknown type `${receiver_name}`', node_id, tc.type_diagnostic_pos(node_id, receiver_name))
		}
	}
	if node.kind == .type_decl && trimmed_source_type_shape(node.typ).has(.result_type) {
		tc.record_error_with_details_at(.unknown_type, 'cannot make an alias of Result type', node_id, tc.type_diagnostic_pos(node_id, node.typ_text().trim_space()), [
			'Result types cannot be stored and have to be unwrapped immediately',
		])
	}
	if node.kind == .type_decl && trimmed_source_type_id(node.typ) == flat.source_type_none {
		tc.record_error_at(.unknown_type, 'cannot create a type alias of `none` as it is a value', node_id, tc.type_diagnostic_pos(node_id, 'none'))
	}
	if node.kind == .struct_decl {
		if comma_attr_text_has(node.typ_text(), 'generic') && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic struct `${node.value}`', node_id)
		}
	} else {
		if node.generic_params().len > 0 && tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic declaration `${node.value}`', node_id)
		}
		if !invalid_generic_struct_alias && (!decl_generic_mentions_error
			|| tc.unmentioned_generic_names_in_type(node.typ, generic_param_id_set(generic_param_ids(node.generic_params()))).len == 0) {
			tc.check_type_string_for_unsupported_generics(node.typ, node_id, generic_params)
		}
	}
	mut explicit_decl_params := map[u32]bool{}
	if decl_generic_mentions_error {
		explicit_decl_params = generic_param_id_set(generic_param_ids(node.generic_params()))
		if explicit_decl_params.len == 0 {
			explicit_decl_params[checker_source_at_declaration] = true
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := tc.a.nodes[int(child_id)]
		// These nodes use `typ` for a non-type payload: a comptime loop source or an
		// offsetof field name. Their actual type inputs are validated separately.
		if child.kind in [.comptime_for, .offsetof_expr] {
			continue
		}
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
				&& tc.interface_metadata_name(field_type.source_type_id()) == tc.interface_metadata_name(node.source_value_id()) {
				tc.record_error_at(.assignment_mismatch, 'recursive interface fields are not allowed because they cannot be initialised', child_id, tc.type_diagnostic_pos_id(child_id, child.typ))
			}
		}
		if child.kind == .param && trimmed_source_type_shape(child.typ).has(.result_type) {
			param_type := unalias_type(tc.parse_type(child.typ))
			mut supported_result_callback := false
			if param_type is ResultType {
				supported_result_callback = unalias_type(param_type.base_type) is FnType
			}
			if !supported_result_callback {
				tc.record_error_at(.unknown_type, 'result type arguments are not supported', child_id, tc.type_diagnostic_pos(child_id, child.typ_text().trim_space()))
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
		if node.kind == .type_decl && child.source_value_id() != 0 {
			tc.check_type_string_for_unsupported_generics(child.source_value_id(), child_id, generic_params)
		}
		for j in 0 .. child.children_count {
			grandchild_id := tc.a.child(&child, j)
			if int(grandchild_id) < 0 {
				continue
			}
			grandchild := tc.a.nodes[int(grandchild_id)]
			if grandchild.kind in [.comptime_for, .offsetof_expr] {
				continue
			}
			if !decl_generic_mentions_error
				|| tc.unmentioned_generic_names_in_type(grandchild.typ, explicit_decl_params).len == 0 {
				tc.check_type_string_for_unsupported_generics(grandchild.typ, grandchild_id, generic_params)
			}
		}
	}
}

// check_written_nested_option_types reports every type written in the
// top-level declaration `top_id`, its body included, in which an option or a
// result directly wraps another option or result: `??int`, `!!int`, `?!int`.
// An option has a single `none` and a result a single error, so the outer one
// could not be told from the inner one, and V does not have such types (see
// #27171). `?T` with an option `T`, and `?Alias` of an option, are not written
// nested types: they collapse to the option itself.
// `stack` is the caller's scratch, reused for every declaration.
fn (mut tc TypeChecker) check_written_nested_option_types(top_id flat.NodeId, mut stack []WrittenTypeItem) {
	if !tc.should_diagnose(top_id) {
		return
	}
	stack.clear()
	stack << WrittenTypeItem{top_id, top_id}
	for stack.len > 0 {
		item := stack.pop()
		if !tc.valid_node_id(item.id) {
			continue
		}
		node := tc.a.node(item.id)
		anchor := if node.pos.end > 0 { item.id } else { item.anchor }
		if has_written_nested_option_result_type(node) {
			tc.report_written_nested_option_types(*node, anchor)
		}
		for i in 0 .. node.children_count {
			stack << WrittenTypeItem{tc.a.child(node, i), anchor}
		}
	}
}

// has_written_nested_option_result_type probes only operands consumed by
// report_written_nested_option_types. Declaration names are not type operands.
@[inline]
fn has_written_nested_option_result_type(node &flat.Node) bool {
	if node.typ != 0 && node.kind in [.param, .fn_decl, .interface_field, .field_decl, .type_decl,
		.global_decl, .const_field, .fn_literal, .array_init, .decl_assign, .c_fn_decl] && flat.source_type_syntax(node.typ).nested_option_result_marker() >= 0 {
		return true
	}
	uses_value_type := node.kind in [.ident, .struct_init, .map_init, .cast_expr, .is_expr, .as_expr,
		.array_init]
		|| (node.kind in [.sizeof_expr, .typeof_expr] && node.children_count == 0)
		|| (node.kind == .interface_field && node.op != .dot && node.typ == 0
			&& node.value.len > 0)
	if !uses_value_type {
		return false
	}
	// Raw names are a source/diagnostic boundary. Do not intern ordinary names
	// solely for this diagnostic walk; indexed operands keep their numeric query.
	if node.value.len > 0 {
		return (node.kind != .interface_field || node.value.contains('['))
			&& nested_option_result_marker(node.value) >= 0
	}
	value_id := node.source_value
	if value_id == 0 || (node.kind == .interface_field
		&& !flat.source_type_shape(value_id).has(.bracketed)) {
		return false
	}
	return flat.source_type_syntax(value_id).nested_option_result_marker() >= 0
}

fn (mut tc TypeChecker) report_written_nested_option_types(node flat.Node, anchor flat.NodeId) {
	// Only a spelling with a nested marker is reported, so only those need text.
	mut texts := []string{}
	for type_id in written_type_ids(node) {
		if flat.source_type_syntax(type_id).nested_option_result_marker() >= 0 {
			texts << flat.source_type_name(type_id)
		}
	}
	// The types written as names: a sum type variant, a generic type argument,
	// a struct literal, and the return type of a C function.
	if node.kind in [.ident, .struct_init] && node.value_text() !in texts {
		texts << node.value_text()
	}
	if node.kind == .c_fn_decl {
		texts << node.typ_text()
	}
	for text in texts {
		idx := nested_option_result_marker(text)
		if idx < 0 {
			continue
		}
		outer := text[idx]
		inner := text[idx + 1]
		// check_fn_receiver_and_operator_return reports it for a function's own
		// return type.
		if outer != inner && node.kind == .fn_decl && text == node.typ_text()
			&& trimmed_space(text[..idx]).len == 0 {
			continue
		}
		nested := nested_option_result_type_text(text, idx)
		pos := tc.nested_option_result_type_pos(node, anchor, text, idx, nested)
		msg := if outer != inner {
			'the type must be Option or Result'
		} else if outer == `?` {
			'nested option type `${nested}` is not supported'
		} else {
			'nested result type `${nested}` is not supported'
		}
		if tc.errors.any(it.pos == pos && it.msg == msg) {
			continue
		}
		details := if outer != inner {
			[]string{}
		} else if outer == `?` {
			['an option has a single `none`, so the outer option could not be told from the inner one; use `${nested[1..]}` instead']
		} else {
			['a result has a single error, so the outer result could not be told from the inner one; use `${nested[1..]}` instead']
		}
		tc.record_error_with_details_at(.unknown_type, msg, anchor, pos, details)
	}
}

// nested_option_result_marker is the index of the first option or result marker
// in the type text `text` that directly wraps another one, as the first `?` of
// `[]??int`, or -1.
fn nested_option_result_marker(text string) int {
	// This helper consumes raw source operands and rendered diagnostic text.
	// Routine spelling-ID queries use SourceTypeSyntax's cached marker instead.
	for i := 0; i + 1 < text.len; i++ {
		if text[i] in [`?`, `!`] && text[i + 1] in [`?`, `!`] {
			return i
		}
	}
	return -1
}

// nested_option_result_type_text is the type that starts at `idx` in the type
// text `text`: `??int` in `map[string]??int` or in `fn (??int) string`.
fn nested_option_result_type_text(text string, idx int) string {
	mut depth := 0
	for i in idx .. text.len {
		match text[i] {
			`(`, `[`, `{` {
				depth++
			}
			`)`, `]`, `}` {
				if depth == 0 {
					return trimmed_space(text[idx..i])
				}
				depth--
			}
			`,` {
				if depth == 0 {
					return trimmed_space(text[idx..i])
				}
			}
			else {}
		}
	}
	return trimmed_space(text[idx..])
}

// nested_option_result_type_pos is where the nested type `nested`, at `idx` in
// the type text `text` that `node` writes, is in the source. The return type of
// a function is the last type in its header; any other type is on the line of
// the node that writes it, after where that node starts. The parser drops the
// spaces and the parentheses of `? ?int` and `?(?int)`: then it is where the
// two markers are.
fn (tc &TypeChecker) nested_option_result_type_pos(node flat.Node, anchor flat.NodeId, text string, idx int, nested string) token.Pos {
	anchor_pos := tc.a.node(anchor).pos
	file := tc.a.source_files[anchor_pos.id] or { return anchor_pos }
	source := tc.source_texts_by_file[file.name] or { return anchor_pos }
	if anchor_pos.offset < 0 || anchor_pos.offset > source.len {
		return anchor_pos
	}
	is_return_type := node.kind == .fn_decl && text == node.typ_text()
	mut lo := anchor_pos.offset
	mut hi := anchor_pos.offset
	if is_return_type {
		header := tc.fn_declaration_diagnostic_pos(node)
		lo = int_max(header.offset, 0)
		hi = int_min(header.end, source.len)
	} else {
		for lo > 0 && source[lo - 1] != `\n` {
			lo--
		}
		for hi < source.len && source[hi] != `\n` {
			hi++
		}
	}
	if lo >= hi {
		return anchor_pos
	}
	region := source[lo..hi]
	from := anchor_pos.offset - lo
	if start := find_written_type_text(region, text, from, is_return_type) {
		return token.new_span(anchor_pos.id, lo + start + idx, lo + start + idx + nested.len)
	}
	if start := find_written_type_text(region, nested, from, is_return_type) {
		return token.new_span(anchor_pos.id, lo + start, lo + start + nested.len)
	}
	// The last markers in a header, or the first ones at or after `from`, or
	// else the last ones before it.
	mut found := -1
	mut found_end := -1
	for i := 0; i < region.len && (is_return_type || found < from); i++ {
		if region[i] !in [`?`, `!`] {
			continue
		}
		mut j := i + 1
		for j < region.len && region[j] in [` `, `\t`, `(`] {
			j++
		}
		if j < region.len && region[j] in [`?`, `!`] {
			found = i
			found_end = j + 1
		}
	}
	if found >= 0 {
		return token.new_span(anchor_pos.id, lo + found, lo + found_end)
	}
	return anchor_pos
}

// find_written_type_text is where `text` is in `region`: the last occurrence
// when `last`, else the first one at or after `from`, or else the first one.
fn find_written_type_text(region string, text string, from int, last bool) ?int {
	if last {
		return region.last_index(text)
	}
	if from >= 0 && from < region.len {
		if start := region.index_after(text, from) {
			return start
		}
	}
	return region.index(text)
}

fn (mut tc TypeChecker) check_type_alias_generic_struct_application(node_id flat.NodeId, node flat.Node) bool {
	clean_id := trimmed_source_type_id(node.typ)
	if clean_id == 0 {
		return false
	}
	syntax := flat.source_type_syntax(clean_id)
	is_generic := syntax.application_valid()
	lookup_id := if is_generic { syntax.application_base() } else { clean_id }
	qualified_id := tc.qualify_type_name(lookup_id)
	params := tc.struct_generic_params[lookup_id] or {
		tc.struct_generic_params[qualified_id] or { return false }
	}
	if params.len == 0 {
		return false
	}
	mut invalid := !is_generic
	if is_generic && node.generic_params().len == 0 {
		invalid = syntax.application_args().any(tc.type_text_has_generic_placeholder(it))
	}
	if !invalid {
		return false
	}
	short_name := flat.source_type_name(flat.source_type_syntax(lookup_id).unqualified())
	tc.record_error_at(.unknown_type, '${short_name} type is generic struct, must specify the generic type names, e.g. ${short_name}[int]', node_id, tc.type_diagnostic_pos_id(node_id, clean_id))
	return true
}

fn (tc &TypeChecker) fixed_array_map_alias_generic_params(typ_id u32) []u32 {
	clean_id := trimmed_source_type_id(typ_id)
	if !flat.source_type_shape(clean_id).has(.leading_bracket) {
		return []u32{}
	}
	syntax := flat.source_type_syntax(clean_id)
	if !syntax.bracket_closed() {
		return []u32{}
	}
	element_id := syntax.bracket_base()
	if !flat.source_type_shape(element_id).has(.map_type) {
		return []u32{}
	}
	mut counts := map[u32]int{}
	tc.collect_generic_param_candidates(element_id, mut counts)
	mut names := []u32{cap: counts.len}
	for name, _ in counts {
		names << name
	}
	sort_generic_param_ids(mut names)
	return names
}

fn (tc &TypeChecker) selective_imported_builtin_in_type(type_id u32) ?string {
	info := tc.current_file_import_info()
	if isnil(info) {
		return none
	}
	syntax := flat.source_type_syntax(type_id)
	mut best_offset := syntax.byte_len() + 1
	mut imported_id := u32(0)
	for name_id, candidates in info.selective_imports {
		if !is_builtin_type_name_id(name_id) || candidates.len != 1 {
			continue
		}
		index := syntax.symbols().index(name_id)
		if index < 0 {
			continue
		}
		offset := int(syntax.symbols_first_offsets()[index])
		if offset < best_offset {
			best_offset = offset
			imported_id = candidates[0]
		}
	}
	if imported_id == 0 {
		return none
	}
	// The selected declaration is reported in the diagnostic.
	return flat.source_type_name(imported_id)
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
	explicit_params := generic_param_id_set(generic_param_ids(explicit_names))
	mut mention_params := explicit_params.clone()
	if explicit_names.len == 0 {
		// A declaration signature is type-only context, so any unknown one-letter
		// capital is an omitted generic parameter, not an expression identifier.
		mention_params[checker_source_at_declaration] = true
	}
	mut has_unmentioned := false
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		child_type_id := if node.kind == .interface_decl && child.op != .dot && child.typ == 0 {
			child.source_value_id()
		} else {
			child.typ
		}
		if tc.unmentioned_generic_names_in_type(child_type_id, mention_params).len > 0 {
			has_unmentioned = true
		}
		if node.kind == .interface_decl && child.op == .dot {
			for j in 0 .. child.children_count {
				param := tc.a.child_node(child, j)
				if tc.unmentioned_generic_names_in_type(param.typ, mention_params).len > 0 {
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
		mut error_index := tc.errors.len
		for error_index > 0 {
			error_index--
			diagnostic := tc.errors[error_index]
			if diagnostic.kind == .unknown_type && diagnostic.pos.id == node.pos.id
				&& diagnostic.pos.offset >= node.pos.offset && diagnostic.pos.end <= node.pos.end {
				tc.errors.delete(error_index)
			}
		}
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
	mut diagnosed := map[u64]bool{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		child_type_id := if node.kind == .interface_decl && child.op != .dot && child.typ == 0 {
			child.source_value_id()
		} else {
			child.typ
		}
		for name_id in tc.unmentioned_generic_names_in_type(child_type_id, explicit_params) {
			key := (u64(u32(int(child_id))) << 32) | u64(name_id)
			if diagnosed[key] {
				continue
			}
			diagnosed[key] = true
			pos_type_id := if node.kind == .interface_decl && child.op != .dot && child.typ == 0 {
				syntax := flat.source_type_syntax(child_type_id)
				if syntax.application_valid() { syntax.application_base() } else { child_type_id }
			} else {
				child_type_id
			}
			pos_text := flat.source_type_name(pos_type_id)
			name := flat.source_type_name(name_id)
			tc.record_error_at(.unknown_type, 'generic type name `${name}` is not mentioned in ${kind} `${decl_name}`', child_id, tc.type_diagnostic_pos(child_id, pos_text))
		}
		if node.kind != .interface_decl || child.op != .dot {
			continue
		}
		for j in 0 .. child.children_count {
			param_id := tc.a.child(child, j)
			param := tc.a.node(param_id)
			for name_id in tc.unmentioned_generic_names_in_type(param.typ, explicit_params) {
				key := (u64(u32(int(param_id))) << 32) | u64(name_id)
				if diagnosed[key] {
					continue
				}
				diagnosed[key] = true
				name := flat.source_type_name(name_id)
				tc.record_error_at(.unknown_type, 'generic type name `${name}` is not mentioned in ${kind} `${decl_name}`', param_id, tc.type_diagnostic_pos_id(param_id, param.typ))
			}
		}
	}
	return true
}

fn (mut tc TypeChecker) check_fn_bare_generic_signature_types(node_id flat.NodeId, node flat.Node) {
	if node.generic_params().len > 0 {
		if name_id := tc.bare_generic_decl_type_name_id(node.typ) {
			name := flat.source_type_name(name_id)
			kind, params := tc.generic_decl_kind_and_params(name_id)
			if kind.len > 0 {
				tc.record_error_at(.unsupported_generic, 'return generic ${kind} `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${generic_param_ids_join(params, ', ')}]', node_id, tc.fn_return_type_diagnostic_pos(node))
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
	name_id := tc.bare_generic_decl_type_name_id(receiver.typ) or { return }
	name := flat.source_type_name(name_id)
	kind, params := tc.generic_decl_kind_and_params(name_id)
	if kind.len == 0 {
		return
	}
	tc.record_error_at(.unsupported_generic, 'generic ${kind} `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${generic_param_ids_join(params, ', ')}]', receiver_id, tc.type_diagnostic_pos_id(receiver_id, receiver.typ))
}

fn (tc &TypeChecker) fn_decl_has_bare_generic_signature_type(node flat.Node) bool {
	if node.generic_params().len > 0 && tc.bare_generic_decl_type_name_id(node.typ) != none {
		return true
	}
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	receiver := tc.a.child_node(&node, 0)
	return receiver.kind == .param && receiver.op == .dot
		&& tc.bare_generic_decl_type_name_id(receiver.typ) != none
}

fn (tc &TypeChecker) generic_decl_kind_and_params(name_id u32) (string, []u32) {
	qualified_id := tc.qualify_type_name(name_id)
	if name_id in tc.struct_generic_params {
		return 'struct', tc.struct_generic_params[name_id]
	}
	if qualified_id in tc.struct_generic_params {
		return 'struct', tc.struct_generic_params[qualified_id]
	}
	if name_id in tc.interface_generic_params {
		return 'interface', tc.interface_generic_params[name_id]
	}
	if qualified_id in tc.interface_generic_params {
		return 'interface', tc.interface_generic_params[qualified_id]
	}
	if name_id in tc.sum_generic_params {
		return 'sumtype', tc.sum_generic_params[name_id]
	}
	if qualified_id in tc.sum_generic_params {
		return 'sumtype', tc.sum_generic_params[qualified_id]
	}
	if name_id in tc.type_alias_generic_params {
		params := tc.type_alias_generic_params[name_id]
		kind := if unalias_type(tc.parse_type(name_id)) is FnType {
			'function'
		} else {
			'alias'
		}
		return kind, params
	}
	if qualified_id in tc.type_alias_generic_params {
		params := tc.type_alias_generic_params[qualified_id]
		kind := if unalias_type(tc.parse_type(qualified_id)) is FnType {
			'function'
		} else {
			'alias'
		}
		return kind, params
	}
	return '', []u32{}
}

fn (mut tc TypeChecker) check_fn_decl_unmentioned_generic_types(node_id flat.NodeId, node flat.Node) {
	mut explicit_params := generic_param_id_set(generic_param_ids(node.generic_params()))
	if node.value.contains('.') && node.children_count > 0 {
		receiver := tc.a.child_node(&node, 0)
		if receiver.kind == .param {
			mut receiver_type_id := trimmed_source_type_id(receiver.typ)
			for {
				shape := flat.source_type_shape(receiver_type_id)
				if !shape.has(.shared_type) && !shape.has(.atomic_type) {
					break
				}
				receiver_type_id = trimmed_source_type_id(flat.source_type_syntax(receiver_type_id).raw_base())
			}
			receiver_type := unwrap_pointer(tc.parse_type(receiver_type_id))
			// Generic struct and interface receiver parameters belong to the method declaration.
			// Alias and sum-type receivers still have to repeat their generic names,
			// matching the v1 declaration rules.
			if receiver_type is Struct || receiver_type is Interface {
				tc.collect_generic_receiver_params(node, mut explicit_params)
			}
		}
	}
	mut missing_ids := []flat.NodeId{}
	mut missing_names := []u32{}
	mut missing_types := []string{}
	mut diagnosed := map[u64]bool{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind != .param {
			if tc.prefix_param_scan {
				break
			}
			continue
		}
		if child.typ == 0 {
			continue
		}
		for name in tc.unmentioned_generic_names_in_type(child.typ, explicit_params) {
			key := (u64(u32(int(child_id))) << 32) | u64(name)
			if diagnosed[key] {
				continue
			}
			diagnosed[key] = true
			missing_ids << child_id
			missing_names << name
			missing_types << child.typ_text().trim_space()
		}
	}
	if node.typ != 0 {
		for name in tc.unmentioned_generic_names_in_type(node.typ, explicit_params) {
			key := (u64(u32(int(node_id))) << 32) | u64(name)
			if diagnosed[key] {
				continue
			}
			diagnosed[key] = true
			missing_ids << node_id
			missing_names << name
			missing_types << node.typ_text().trim_space()
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
	for i, name_id in missing_names {
		name := flat.source_type_name(name_id)
		tc.record_error_at(.unknown_type, 'generic type name `${name}` is not mentioned in fn `${node.value.all_after_last('.')}[${node.generic_params().join(', ')}]`', missing_ids[i], tc.type_diagnostic_pos(missing_ids[i], missing_types[i]))
	}
}

fn (mut tc TypeChecker) check_bare_generic_fntype_param(node_id flat.NodeId, type_id u32) {
	name_id := tc.bare_generic_decl_type_name_id(type_id) or { return }
	name := flat.source_type_name(name_id)
	qualified_id := tc.qualify_type_name(name_id)
	interface_params := tc.interface_generic_params[name_id] or {
		tc.interface_generic_params[qualified_id] or { []u32{} }
	}
	if interface_params.len > 0 {
		tc.record_error_at(.unsupported_generic, 'generic interface `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${generic_param_ids_join(interface_params, ', ')}]', node_id, tc.type_diagnostic_pos_id(node_id, type_id))
		return
	}
	params := tc.type_alias_generic_params[name_id] or {
		tc.type_alias_generic_params[qualified_id] or { return }
	}
	if params.len == 0 || unalias_type(tc.parse_type(name_id)) !is FnType {
		return
	}
	tc.record_error_at(.unsupported_generic, 'generic function `${name}` in fn declaration must specify the generic type names, e.g. ${name}[${generic_param_ids_join(params, ', ')}]', node_id, tc.type_diagnostic_pos_id(node_id, type_id))
}

fn (mut tc TypeChecker) check_implicit_generic_sumtype_decl(node_id flat.NodeId, node flat.Node) {
	decl_params := generic_param_ids(node.generic_params())
	if decl_params.len > 0 {
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			variant_id := trimmed_source_type_id(tc.a.node(child_id).source_value_id())
			syntax := flat.source_type_syntax(variant_id)
			if !syntax.application_valid() {
				continue
			}
			base_id := syntax.application_base()
			qualified_id := tc.qualify_type_name(syntax.application_base())
			struct_params := tc.struct_generic_params[base_id] or {
				tc.struct_generic_params[qualified_id] or { []u32{} }
			}
			if struct_params.len == 0 {
				continue
			}
			for arg_id in syntax.application_args() {
				if source_type_is_bare_generic_param(arg_id) && arg_id !in decl_params {
					arg := flat.source_type_name(arg_id)
					variant := flat.source_type_name(variant_id)
					tc.record_error_at(.unknown_type, 'generic type name `${arg}` of generic struct `${variant}` is not mentioned in sumtype `${node.value}[${node.generic_params().join(', ')}]`', child_id, tc.type_diagnostic_pos(node_id, variant))
				}
			}
		}
		return
	}
	mut requires_generic_params := false
	mut example_params := []u32{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		variant_id := trimmed_source_type_id(child.source_value_id())
		if variant_id == 0 {
			continue
		}
		syntax := flat.source_type_syntax(variant_id)
		is_generic := syntax.application_valid()
		lookup_id := if is_generic { syntax.application_base() } else { variant_id }
		if !is_generic && tc.concrete_type_declared_in_current_file(lookup_id) {
			continue
		}
		qualified_id := tc.qualify_type_name(lookup_id)
		params := tc.struct_generic_params[lookup_id] or {
			tc.struct_generic_params[qualified_id] or {
				tc.type_alias_generic_params[lookup_id] or {
					tc.type_alias_generic_params[qualified_id] or {
						tc.sum_generic_params[lookup_id] or {
							tc.sum_generic_params[qualified_id] or { []u32{} }
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
			kind := if lookup_id in tc.struct_generic_params || qualified_id in tc.struct_generic_params {
				'struct'
			} else if unalias_type(tc.parse_type(lookup_id)) is FnType {
				'fntype'
			} else {
				'sumtype'
			}
			lookup := flat.source_type_name(lookup_id)
			tc.record_error_at(.unknown_type, 'generic ${kind} `${lookup}` must specify generic type names, e.g. ${lookup}[${generic_param_ids_join(params, ', ')}]', child_id, tc.type_diagnostic_pos_id(node_id, variant_id))
			continue
		}
		for arg_id in syntax.application_args() {
			if source_type_is_bare_generic_param(arg_id) && !tc.type_name_known(arg_id) {
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
			example_params = [checker_source_generic_t]
		}
		tc.record_error_at(.unsupported_generic, 'generic sumtype `${node.value}` must specify generic type names, e.g. ${node.value}[${generic_param_ids_join(example_params, ', ')}]', node_id, tc.type_diagnostic_pos(node_id, node.value))
	}
}

fn (tc &TypeChecker) unmentioned_generic_names_in_type(typ_id u32, explicit_params map[u32]bool) []u32 {
	mut names := []u32{}
	for name_id in flat.source_type_syntax(typ_id).generic_mentions() {
		if explicit_params.len == 0 && name_id != checker_source_generic_t {
			continue
		}
		if explicit_params[name_id] || tc.type_name_known(name_id)
			|| tc.local_type_name_known(flat.source_type_name(name_id)) || name_id in names {
			continue
		}
		names << name_id
	}
	return names
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
	mut seen_interfaces := map[u32]bool{}
	mut reported_interfaces := map[u32]bool{}
	for implements_index, type_name_id in struct_decl_implements_ids(node.typ) {
		syntax := flat.source_type_syntax(type_name_id)
		is_generic := syntax.application_valid()
		args := syntax.application_args()
		raw_lookup_id := if is_generic { syntax.application_base() } else { type_name_id }
		lookup_id := tc.resolve_selective_import_type_symbol_id(raw_lookup_id) or { raw_lookup_id }
		qualified_id := tc.qualify_type_name(lookup_id)
		interface_params := tc.interface_generic_params[lookup_id] or {
			tc.interface_generic_params[qualified_id] or { []u32{} }
		}
		diagnostic_pos := tc.struct_implements_type_pos(node, type_name_id, implements_index)
		if interface_params.len > 0 && !is_generic {
			tc.record_error_at(.unknown_type, 'missing generic type on ${flat.source_type_name(lookup_id)}', node_id, diagnostic_pos)
			continue
		}
		if is_generic && interface_params.len > 0 {
			mut unknown_arg := ''
			decl_params := generic_param_ids(node.generic_params())
			for arg_id in args {
				if arg_id !in decl_params && tc.type_text_has_generic_placeholder(arg_id) {
					unknown_arg = flat.source_type_name(arg_id)
					break
				}
			}
			if unknown_arg.len > 0 {
				base_pos := token.new_span(diagnostic_pos.id, diagnostic_pos.offset, int_min(diagnostic_pos.end, diagnostic_pos.offset + flat.source_type_syntax(raw_lookup_id).byte_len()))
				tc.record_error_at(.unknown_type, 'unknown generic type ${unknown_arg}', node_id, base_pos)
				continue
			}
		}
		if lookup_id in tc.interface_names || qualified_id in tc.interface_names {
			interface_name_id := if lookup_id in tc.interface_names {
				lookup_id
			} else {
				qualified_id
			}
			interface_name := flat.source_type_name(interface_name_id)
			if seen_interfaces[interface_name_id] {
				if !reported_interfaces[interface_name_id] {
					tc.record_error_at(.assignment_mismatch, 'struct type ${node.value} cannot implement interface `${interface_name} more than once`', node_id, diagnostic_pos)
					reported_interfaces[interface_name_id] = true
				}
				continue
			}
			seen_interfaces[interface_name_id] = true
			actual := Type(Struct{
				name_id: node.source_value_id()
			})
			mut expected_name := interface_name_id
			if is_generic {
				mut parts := [interface_name_id]
				parts << args
				expected_name = flat.source_type_composition(.generic, parts)
			}
			expected := Interface{
				name_id: expected_name
			}
			if !tc.type_implements_interface(actual, expected) {
				tc.record_interface_implementation_error(.assignment_mismatch, actual, expected, node_id, tc.struct_declaration_name_pos(node))
			}
			continue
		}
		if is_generic && (lookup_id in tc.interface_names || qualified_id in tc.interface_names) {
			continue
		}
		$if ownership ? {
			// Ownership markers carry checker metadata without an interface declaration.
			// A declared type with the same name still follows normal implements checking.
			if !is_generic && flat.source_type_name(lookup_id) in ['Owned', 'Copy', 'Drop']
				&& !tc.is_known_type_text(lookup_id) {
				continue
			}
		}
		tc.record_error_at(.assignment_mismatch, '`${flat.source_type_name(type_name_id)}` is not an interface type', node_id, diagnostic_pos)
	}
}

fn (tc &TypeChecker) struct_implements_type_pos(node flat.Node, type_name_id u32, ordinal int) token.Pos {
	type_name := flat.source_type_name(type_name_id)
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

fn (mut tc TypeChecker) check_missing_struct_field_generic_type(node_id flat.NodeId, type_id u32, generic_params map[u32]bool) {
	name_id := tc.bare_generic_decl_type_name_id(type_id) or { return }
	name := flat.source_type_name(name_id)
	qualified_id := tc.qualify_type_name(name_id)
	mut kind := ''
	mut target_params := []u32{}
	if name_id in tc.struct_generic_params || qualified_id in tc.struct_generic_params {
		kind = 'struct'
		target_params = tc.struct_generic_params[name_id] or {
			tc.struct_generic_params[qualified_id] or { []u32{} }
		}
	} else if name_id in tc.interface_generic_params || qualified_id in tc.interface_generic_params {
		kind = 'interface'
		target_params = tc.interface_generic_params[name_id] or {
			tc.interface_generic_params[qualified_id] or { []u32{} }
		}
	} else if name_id in tc.sum_generic_params || qualified_id in tc.sum_generic_params {
		kind = 'sumtype'
		target_params = tc.sum_generic_params[name_id] or {
			tc.sum_generic_params[qualified_id] or { []u32{} }
		}
	} else if name_id in tc.type_alias_generic_params || qualified_id in tc.type_alias_generic_params {
		parsed := unalias_type(tc.parse_type(name_id))
		if parsed is FnType {
			kind = 'fn type'
		} else {
			kind = 'alias'
		}
		target_params = tc.type_alias_generic_params[name_id] or {
			tc.type_alias_generic_params[qualified_id] or { []u32{} }
		}
	} else {
		return
	}
	if kind == 'interface' && bare_generic_type_is_inside_container(type_id) {
		return
	}
	mut context_name := 'int'
	if target_params.len > 0 {
		context_name = flat.source_type_name(target_params[0])
	}
	context_names := generic_param_ids_from_set(generic_params)
	if context_names.len > 0 {
		context_name = flat.source_type_name(context_names[0])
	}
	node := tc.a.node(node_id)
	mut pos := if node.kind == .field_decl {
		tc.struct_field_type_pos(node)
	} else {
		tc.type_diagnostic_pos_id(node_id, type_id)
	}
	tc.record_error_at(.unknown_type, '`${name}` type is generic ${kind}, must specify the generic type names, e.g. ${name}[${context_name}], ${name}[int]', node_id, pos)
}

fn bare_generic_type_is_inside_container(type_id u32) bool {
	mut clean_id := trimmed_source_type_id(type_id)
	for {
		shape := flat.source_type_shape(clean_id)
		if !shape.has(.pointer) && !shape.has(.optional) { break }
		clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
	}
	// Each modifier is accepted once in the declaration query's original order.
	for modifier in [flat.SourceTypeShape.shared_type, .atomic_type, .mutable_type] {
		if flat.source_type_shape(clean_id).has(modifier) {
			clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
		}
	}
	shape := flat.source_type_shape(clean_id)
	return shape.has(.array_type) || shape.has(.variadic) || shape.has(.map_type)
		|| shape.has(.leading_bracket)
}

fn (tc &TypeChecker) bare_generic_decl_type_name_id(type_id u32) ?u32 {
	clean_id := trimmed_source_type_id(type_id)
	if clean_id == 0 { return none }
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.optional) || shape.has(.shared_type)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return tc.bare_generic_decl_type_name_id(syntax.raw_base())
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			if generic_name := tc.bare_generic_decl_type_name_id(syntax.bracket_raw_inner()) {
				return generic_name
			}
			return tc.bare_generic_decl_type_name_id(syntax.bracket_suffix())
		}
		return none
	}
	if shape.has(.leading_bracket) {
		if syntax.first_bracket_closed() && syntax.first_bracket_base() != 0 {
			return tc.bare_generic_decl_type_name_id(syntax.first_bracket_base())
		}
		return none
	}
	if syntax.application_valid() || !syntax.named_type() { return none }
	if scope_type_key(tc.cur_file, tc.cur_module, syntax.unqualified()) in tc.concrete_type_scope_keys {
		return none
	}
	qualified_id := tc.qualify_type_name(clean_id)
	if qualified_id != clean_id && tc.type_name_known_in_current_module(clean_id)
		&& qualified_id !in tc.struct_generic_params && qualified_id !in tc.sum_generic_params && qualified_id !in tc.type_alias_generic_params && qualified_id !in tc.interface_generic_params {
		return none
	}
	if clean_id in tc.struct_generic_params || qualified_id in tc.struct_generic_params
		|| clean_id in tc.sum_generic_params || qualified_id in tc.sum_generic_params
		|| clean_id in tc.type_alias_generic_params || qualified_id in tc.type_alias_generic_params
		|| clean_id in tc.interface_generic_params || qualified_id in tc.interface_generic_params {
		return clean_id
	}
	return none
}

fn (tc &TypeChecker) concrete_type_declared_in_current_file(name_id u32) bool {
	return scope_type_key(tc.cur_file, tc.cur_module, scope_unqualified_type_name(name_id)) in tc.concrete_type_scope_keys
}

fn (mut tc TypeChecker) check_recursive_alias_decls() {
	mut invalid := []u32{}
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
				if node.children_count == 0 && node.typ != 0
					&& tc.check_recursive_alias_decl(flat.NodeId(node_idx), node) {
					invalid << tc.qualify_decl_name_id(node.source_value_id())
				}
			}
			else {}
		}
	}
	// Invalid aliases must not reach interface-index preparation or type lowering,
	// both of which assume the alias graph is acyclic.
	for name_id in invalid {
		tc.type_aliases.set_id(name_id, flat.source_type_void)
	}
}

fn (mut tc TypeChecker) check_recursive_alias_decl(node_id flat.NodeId, node flat.Node) bool {
	start_id := tc.qualify_decl_name_id(node.source_value_id())
	if start_id == 0 || !tc.type_aliases.contains(start_id) {
		return false
	}
	mut seen := map[u32]bool{}
	if !tc.unguarded_alias_reaches(start_id, start_id, mut seen) {
		return false
	}
	if tc.should_diagnose(node_id) {
		tc.record_error(.unknown_type, 'alias `${node.value}` forms a recursive cycle', node_id)
	}
	return true
}

fn (tc &TypeChecker) unguarded_alias_reaches(start_id u32, current_id u32, mut seen map[u32]bool) bool {
	if current_id in seen {
		return false
	}
	seen[current_id] = true
	target := tc.type_aliases.type_id(current_id) or { return false }
	mut names := []u32{}
	collect_unguarded_alias_type_names(target, mut names)
	for name in names {
		dependency_source_id := tc.alias_dependency_key(name, current_id) or { continue }
		if dependency_source_id == start_id {
			return true
		}
		if tc.unguarded_alias_reaches(start_id, dependency_source_id, mut seen) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) alias_dependency_key(name_id u32, current_id u32) ?u32 {
	clean_id := trimmed_source_type_id(name_id)
	if clean_id == 0 {
		return none
	}
	if tc.type_aliases.contains(clean_id) {
		return clean_id
	}
	if !flat.source_type_shape(clean_id).has(.dotted)
		&& flat.source_type_shape(current_id).has(.dotted) {
		candidate := flat.source_type_composition_two(.qualified, flat.source_type_syntax(current_id).qualifier(), clean_id)
		if tc.type_aliases.contains(candidate) {
			return candidate
		}
	}
	return none
}

fn collect_unguarded_alias_type_names(type_id u32, mut names []u32) {
	mut clean_id := trimmed_source_type_id(type_id)
	if clean_id == 0 { return }
	for {
		shape := flat.source_type_shape(clean_id)
		if !shape.has(.pointer) && !shape.has(.optional) && !shape.has(.mutable_type)
			&& !shape.has(.shared_type) && !shape.has(.atomic_type)
			&& !shape.has(.channel_type) && !shape.has(.thread_type) {
			break
		}
		clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
	}
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	// A callback introduces an indirection boundary: aliases such as
	// `type Handlers = map[string]fn (Handlers)` are recursive but finite.
	if shape.has(.function_compact) || shape.has(.function_spaced) { return }
	if shape.has(.variadic) || shape.has(.array_type) {
		collect_unguarded_alias_type_names(syntax.raw_base(), mut names)
		return
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			collect_unguarded_alias_type_names(syntax.bracket_raw_inner(), mut names)
			collect_unguarded_alias_type_names(syntax.bracket_suffix(), mut names)
		}
		return
	}
	if shape.has(.leading_bracket) {
		if syntax.bracket_closed() {
			collect_unguarded_alias_type_names(syntax.bracket_suffix(), mut names)
		}
		return
	}
	if syntax.tuple_like() {
		for part in syntax.checker_tuple_components() {
			collect_unguarded_alias_type_names(part, mut names)
		}
		return
	}
	if syntax.application_valid() {
		push_generic_method_base_candidate(mut names, trimmed_source_type_id(syntax.application_base()))
		for arg in syntax.application_args() {
			collect_unguarded_alias_type_names(arg, mut names)
		}
		return
	}
	push_generic_method_base_candidate(mut names, clean_id)
}

fn (tc &TypeChecker) infer_decl_generic_params(node flat.Node) map[u32]bool {
	mut params := map[u32]bool{}
	for name in node.generic_params() {
		params[flat.source_type_id(name)] = true
	}
	tc.collect_generic_receiver_params(node, mut params)
	return params
}

fn (tc &TypeChecker) infer_decl_generic_param_ids(node flat.Node) []u32 {
	if node.generic_params().len == 0 && !tc.decl_receiver_may_carry_generic_params(node) {
		// The common non-generic declaration: skip the temporary map and sort.
		return []u32{}
	}
	params := tc.infer_decl_generic_params(node)
	return generic_param_ids_from_set(params)
}

// decl_receiver_may_carry_generic_params reports whether a method receiver
// type spelling can contribute generic parameters (`Foo[T]`), mirroring the
// early exits of collect_generic_receiver_params without allocating.
fn (tc &TypeChecker) decl_receiver_may_carry_generic_params(node flat.Node) bool {
	if node.kind != .fn_decl && node.kind != .c_fn_decl {
		return false
	}
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	receiver_id := tc.a.child(&node, 0)
	if int(receiver_id) < 0 {
		return false
	}
	receiver := tc.a.nodes[int(receiver_id)]
	if receiver.kind != .param {
		return false
	}
	if flat.source_type_shape(receiver.typ).has(.bracketed) {
		return true
	}
	// A bare generic-parameter spelling is the only bracket-free candidate the
	// full scan can report.
	mut clean_id := trimmed_source_type_id(receiver.typ)
	for {
		shape := flat.source_type_shape(clean_id)
		if !shape.has(.shared_type) && !shape.has(.atomic_type) { break }
		clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
	}
	for {
		shape := flat.source_type_shape(clean_id)
		if !shape.has(.pointer) && !shape.has(.optional) { break }
		clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
	}
	return source_type_is_bare_generic_param(clean_id)
}

// active_generic_param reports whether the source name `name` spells a type
// parameter of the function being checked.
fn (tc &TypeChecker) active_generic_param(name string) bool {
	return generic_param_ids_contain_name(tc.fn_context.generic_params, name)
}

// active_generic_param_id reports whether `name_id` is a type parameter of the
// function being checked.
fn (tc &TypeChecker) active_generic_param_id(name_id u32) bool {
	return name_id in tc.fn_context.generic_params
}

fn (tc &TypeChecker) node_has_enclosing_generic_param(node_id flat.NodeId, name string) bool {
	if name == '' || int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(node_id)]
	if name in node.generic_params() {
		return true
	}
	params := tc.enclosing_generic_params_by_node[int(node_id)] or { return false }
	return generic_param_ids_contain_name(params, name)
}

@[direct_array_access]
fn (tc &TypeChecker) collect_generic_receiver_params(node flat.Node, mut params map[u32]bool) {
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
	mut receiver_type_id := trimmed_source_type_id(receiver.typ)
	for {
		shape := flat.source_type_shape(receiver_type_id)
		if !shape.has(.shared_type) && !shape.has(.atomic_type) { break }
		receiver_type_id = trimmed_source_type_id(flat.source_type_syntax(receiver_type_id).raw_base())
	}
	if flat.source_type_shape(receiver_type_id).has(.pointer) {
		receiver_type_id = flat.source_type_syntax(receiver_type_id).pointer_raw_base()
	}
	// An owner spelling that was never interned cannot equal the receiver type.
	owner_id := flat.source_type_existing_id(owner_name_view(node.value)) or { return }
	if receiver_type_id != owner_id {
		return
	}
	syntax := flat.source_type_syntax(receiver_type_id)
	if syntax.application_valid() {
		receiver_args := syntax.application_args()
		decl_params := tc.struct_generic_params[syntax.application_base()] or {
			tc.struct_generic_params[flat.source_type_syntax(syntax.application_base()).unqualified()] or { []u32{} }
		}
		if receiver_args.len == decl_params.len {
			for arg in receiver_args {
				if arg in decl_params {
					params[arg] = true
				}
			}
		}
	}
	mut counts := map[u32]int{}
	if !tc.collect_generic_param_candidates(receiver.typ, mut counts) {
		return
	}
	for name, _ in counts {
		params[name] = true
	}
}

fn (tc &TypeChecker) collect_generic_param_candidates(typ_id u32, mut counts map[u32]int) bool {
	clean_id := trimmed_source_type_id(typ_id)
	if clean_id == 0 {
		return false
	}
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.optional) || shape.has(.shared_type)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return tc.collect_generic_param_candidates(syntax.raw_base(), mut counts)
	}
	if shape.has(.map_type) {
		mut found_context := false
		if syntax.bracket_closed() {
			if tc.collect_generic_param_candidates(syntax.bracket_inner(), mut counts) {
				found_context = true
			}
			if tc.collect_generic_param_candidates(syntax.bracket_base(), mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if shape.has(.leading_bracket) {
		return syntax.first_bracket_closed()
			&& tc.collect_generic_param_candidates(syntax.first_bracket_base(), mut counts)
	}
	if syntax.tuple_like() {
		mut found_context := false
		for part in syntax.checker_tuple_components() {
			if tc.collect_generic_param_candidates(part, mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		mut found_context := false
		if syntax.function_closed() {
			for param in syntax.function_params() {
				param_type := flat.source_type_syntax(trimmed_source_type_id(param)).last_space_word()
				if tc.collect_generic_param_candidates(param_type, mut counts) {
					found_context = true
				}
			}
			if tc.collect_generic_param_candidates(syntax.function_result(), mut counts) {
				found_context = true
			}
		}
		return found_context
	}
	if syntax.application_valid() {
		if syntax.bracket_closed() {
			for part in syntax.application_args() {
				tc.collect_generic_param_candidates(part, mut counts)
			}
		}
		return true
	}
	if syntax.bare_generic() && !tc.type_name_known(clean_id) {
		counts[clean_id] = (counts[clean_id] or { 0 }) + 1
	}
	return false
}

// check_type_string_for_unsupported_generics
// validates helper state for types.
fn (mut tc TypeChecker) check_type_string_for_unsupported_generics(typ_id u32, node_id flat.NodeId, generic_params map[u32]bool) {
	if flat.source_type_shape(typ_id).has(.builtin_alias) {
		return
	}
	clean_id := trimmed_source_type_id(typ_id)
	if clean_id == 0 {
		return
	}
	node := tc.a.node(node_id)
	if node.kind == .sql_expr && node.value.starts_with('querydata ') && clean_id == checker_source_orm_query_data {
		return
	}
	if clean_id in [flat.source_type_generic, checker_source_params, checker_source_union] {
		return
	}
	if clean_id == comptime_source_type_any {
		tc.record_invalid_any_decl_type(node_id)
		return
	}
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if syntax.bare_generic() {
		if node.kind == .ident {
			return
		}
		if node.kind == .type_decl {
			if clean_id !in generic_params && !tc.type_name_known(clean_id) {
				name := flat.source_type_name(clean_id)
				tc.record_error_at(.unknown_type, 'unknown aliased type `${name}`', node_id, tc.type_diagnostic_pos(node_id, name))
				return
			}
		}
	}
	if shape.has(.pointer) || shape.has(.optional) || shape.has(.shared_type) {
		tc.check_type_string_for_unsupported_generics(syntax.raw_base(), node_id, generic_params)
		return
	}
	if clean_id == checker_source_thread || clean_id == checker_source_chan {
		return
	}
	if shape.has(.thread_type) || shape.has(.channel_type) || shape.has(.variadic)
		|| shape.has(.array_type) {
		tc.check_type_string_for_unsupported_generics(syntax.raw_base(), node_id, generic_params)
		return
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			tc.check_type_string_for_unsupported_generics(syntax.bracket_inner(), node_id, generic_params)
			tc.check_type_string_for_unsupported_generics(syntax.bracket_base(), node_id, generic_params)
		}
		return
	}
	if shape.has(.leading_bracket) {
		if syntax.first_bracket_closed() {
			tc.check_type_string_for_unsupported_generics(syntax.first_bracket_base(), node_id, generic_params)
		}
		return
	}
	if syntax.tuple_like() {
		for part in syntax.checker_tuple_components() {
			tc.check_type_string_for_unsupported_generics(part, node_id, generic_params)
		}
		return
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		tc.check_fn_type_string_for_unsupported_generics(clean_id, node_id, generic_params)
		return
	}
	if syntax.application_valid() {
		if tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic type application `${flat.source_type_name(clean_id)}`', node_id)
			return
		}
		base_id := trimmed_source_type_id(syntax.application_base())
		if flat.source_type_syntax(base_id).named_type() && !tc.type_name_known(base_id) {
			tc.record_unknown_decl_type_id(base_id, node_id)
		}
		if syntax.bracket_closed() {
			for part in syntax.application_args() {
				tc.check_type_string_for_unsupported_generics(part, node_id, generic_params)
			}
		}
		return
	}
	clean := flat.source_type_name(clean_id)
	if syntax.bare_generic() && !tc.type_name_known(clean_id) {
		if tc.reject_unsupported_generics {
			tc.record_unsupported_generic('unsupported generic type parameter `${clean}`', node_id)
			return
		}
		if clean_id in generic_params || tc.active_generic_param_id(clean_id)
			|| tc.node_has_enclosing_generic_param(node_id, clean) {
			return
		}
	}
	if syntax.named_type() && tc.record_invalid_type_module_qualifier(clean, node_id) {
		return
	}
	if syntax.named_type() && !tc.type_name_known(clean_id) {
		if syntax.bare_generic() && tc.unmentioned_generic_type_was_reported(clean, node_id) {
			return
		}
		tc.record_unknown_decl_type_id(clean_id, node_id)
	}
}

fn (mut tc TypeChecker) check_sum_type_decl(node_id flat.NodeId, node flat.Node) {
	mut variants := []u32{}
	mut variant_ids := []flat.NodeId{}
	if node.children_count > 0 {
		for i in 0 .. node.children_count {
			variant_id := tc.a.child(&node, i)
			variant := tc.a.node(variant_id)
			variants << variant.source_value_id()
			variant_ids << variant_id
		}
	} else {
		for variant in flat.source_type_syntax(node.typ).union_parts() {
			variants << variant
			variant_ids << node_id
		}
	}
	qname_id := tc.qualify_type_name(node.source_value_id())
	params := tc.sum_params_for_base(qname_id)
	layout_id := if params.len == 0 {
		qname_id
	} else {
		generic_param_layout_name(qname_id, params)
	}
	mut seen := map[u32]bool{}
	mut result_reported := false
	for i, raw_variant_id in variants {
		variant_source_id := trimmed_source_type_id(raw_variant_id)
		variant_id := variant_ids[i]
		qualified := tc.qualify_type_text_with_generic_params(variant_source_id, params)
		variant_type := tc.parse_canonical_type(qualified)
		semantic_name_id := variant_type.source_type_id()
		variant_key := if variant_type is Unknown { variant_source_id } else { semantic_name_id }
		if seen[variant_key] {
			tc.record_error_at(.duplicate_decl, 'sum type ${node.value} cannot hold the type `${flat.source_type_name(flat.source_type_syntax(semantic_name_id).unqualified())}` more than once', variant_id, tc.type_diagnostic_pos_id(variant_id, variant_source_id))
			continue
		}
		seen[variant_key] = true
		if variant_type is ResultType && !result_reported {
			tc.record_error_at(.assignment_mismatch, 'sum type cannot hold a Result type', variant_id, tc.type_diagnostic_pos_id(variant_id, variant_source_id))
			result_reported = true
		}
		sum_name := tc.sum_base_name(node.source_value_id())
		variant_sum := tc.sum_base_name(variant_source_id)
		if variant_sum == sum_name {
			tc.record_error_at(.assignment_mismatch, 'sum type cannot hold itself', variant_id, tc.type_diagnostic_pos_id(variant_id, variant_source_id))
		} else {
			mut states := {
				layout_id: ValueLayoutState.active
			}
			mut path := ValueLayoutPath{ types: [layout_id] }
			if target := tc.value_type_cycle(variant_type, mut states, mut path) {
				mut diagnostic_id := variant_id
				variant_shape := flat.source_type_shape(variant_source_id)
				mut pos := if variant_shape.has(.function_compact) || variant_shape.has(.function_spaced) {
					tc.sum_type_variant_source_pos(node, flat.source_type_name(variant_source_id))
				} else {
					tc.type_diagnostic_pos_id(variant_id, variant_source_id)
				}
				mut message := 'sum type `${node.value}` cannot be defined recursively'
				if path.fields.len > 0 {
					first := path.fields[0]
					last := path.fields.last()
					fields := path.fields.map(it.name).join('.')
					relation := if path.types.len > 1 && path.types[path.types.len - 2] == last.owner {
						'closes'
					} else {
						'leads to'
					}
					message += ': field `${flat.source_type_name(first.owner)}.${fields}` ${relation} the cycle'
					if field_id := tc.value_layout_field_decl(last) {
						diagnostic_id = field_id
						pos = tc.node_value_diagnostic_pos(field_id)
					}
				}
				cycle := path.types.map(flat.source_type_name(it)).join(' -> ')
				name := flat.source_type_name(target)
				message += '; inline cycle: `${cycle}`; use `&${name}` or `[]${name}` to break the cycle'
				// Filter by the declaration while locating the diagnostic at the field.
				if tc.should_diagnose(variant_id) {
					base := tc.make_type_error_at(.assignment_mismatch, message, diagnostic_id, pos)
					file := if source := tc.a.source_files[pos.id] {
						source.name
					} else {
						base.file
					}
					tc.errors << TypeError{ ...base, file: file }
				}
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
					for variant_end > variant_start && source[variant_end - 1] in [
						` `,
						`\t`,
					] {
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

enum ValueLayoutState {
	active
	finite
}

struct ValueLayoutField {
	owner u32
	name  string
}

struct ValueLayoutPath {
mut:
	types  []u32
	fields []ValueLayoutField
}

fn (tc &TypeChecker) value_type_has_cycle(typ &Type, mut states map[u32]ValueLayoutState) bool {
	mut path := ValueLayoutPath{}
	if _ := tc.value_type_cycle(typ, mut states, mut path) { return true }
	return false
}

fn (tc &TypeChecker) value_type_cycle(typ &Type, mut states map[u32]ValueLayoutState, mut path ValueLayoutPath) ?u32 {
	name := match typ {
		Alias, OptionType, ResultType {
			return tc.value_type_cycle(typ.base_type, mut states, mut path)
		}
		ArrayFixed { return tc.value_type_cycle(typ.elem_type, mut states, mut path) }
		Struct, SumType { tc.recursive_struct_lookup_name(typ.source_type_id()) }
		else { return none }
	}
	if state := states[name] {
		if state == .active {
			path.types << name
			return name
		}
		return none
	}
	base := tc.sum_base_name(name)
	for ancestor, state in states {
		if state != .active || tc.sum_base_name(ancestor) != base { continue }
		if !tc.type_has_nested_instance(ancestor, name) {
			path.types << name
			return name
		}
	}
	states[name] = .active
	path.types << name
	for raw in tc.sum_types.type_ids(base) or { []u32{} } {
		variant := tc.concrete_sum_variant_name(name, raw)
		if target := tc.value_type_cycle(tc.parse_canonical_type_cached(variant), mut states, mut path) {
			return target
		}
	}
	for field in tc.struct_fields_for_type(name) {
		path.fields << ValueLayoutField{ owner: name, name: field.name }
		if target := tc.value_type_cycle(field.typ, mut states, mut path) { return target }
		path.fields.pop()
	}
	path.types.pop()
	states[name] = .finite
	return none
}

fn (tc &TypeChecker) value_layout_field_decl(field ValueLayoutField) ?flat.NodeId {
	decl_id := tc.source_struct_decl_id_for_name(field.owner)?
	decl := tc.a.node(decl_id)
	for i in 0 .. decl.children_count {
		child_id := tc.a.child(decl, i)
		child := tc.a.node(child_id)
		if child.kind == .field_decl && child.value == field.name { return child_id }
	}
	return none
}

fn (mut tc TypeChecker) record_invalid_any_decl_type(node_id flat.NodeId) {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return
	}
	node := tc.a.nodes[int(node_id)]
	if node.kind !in [.fn_decl, .c_fn_decl, .param, .field_decl, .interface_field, .type_decl,
		.ident] {
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
	tc.record_error_unfiltered_at(.unknown_type, msg, node_id, tc.type_diagnostic_pos(node_id,
		'any'))
}

fn (tc &TypeChecker) node_value_diagnostic_pos(node_id flat.NodeId) token.Pos {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return token.Pos{}
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	return closest_identifier_span(source, node.value_text(), node.pos.offset, node.pos.id) or { node.pos }
}

// record_unknown_decl_type_id reports an unknown type spelling written in a declaration.
fn (mut tc TypeChecker) record_unknown_decl_type_id(name_id u32, node_id flat.NodeId) {
	should_diagnose := tc.should_diagnose(node_id)
	// Diagnostics, positions and the import tables use the spelling text.
	name := flat.source_type_name(name_id)
	// Imported modules are normally outside the diagnostic-file set. Avoid
	// constructing source positions and the full type suggestion list for
	// their intentionally unresolved generic annotations. The sole exception
	// below is a bare annotation that only appears to resolve because a
	// selected main file declares the same type.
	mut report_import_scope_error := false
	if !should_diagnose && !flat.source_type_shape(name_id).has(.dotted)
		&& name_id !in [checker_source_error, checker_source_message_error,
			ierror_source_interface_name]
		&& tc.cur_module !in ['', 'main', 'builtin'] {
		// The qualified probe must not intern a missing spelling.
		report_import_scope_error = tc.selected_main_file_declares_type(name_id)
			&& !tc.qualify_candidate_type_spelling_exists('${tc.cur_module}.${name}')
			&& !tc.source_declares_type_in_scope(name_id, tc.cur_file, tc.cur_module)
	}
	if !should_diagnose && !report_import_scope_error {
		return
	}
	if should_diagnose && tc.record_invalid_type_module_qualifier(name, node_id) {
		return
	}
	if should_diagnose && !name.contains('.') {
		if candidates := tc.selective_import_candidates(name_id) {
			if candidates.len > 0 && candidates.all(!tc.type_symbol_known(it)) {
				// The selected declaration is reported in the diagnostic.
				qualified_candidate := flat.source_type_name(candidates[0])
				pos := tc.type_name_diagnostic_pos(node_id, name)
				if !tc.unknown_type_already_reported_on_line(qualified_candidate, pos) {
					tc.record_error_at(.unknown_type, 'unknown type `${qualified_candidate}`', node_id,
						pos)
				}
				return
			}
		}
	}
	mut pos := tc.type_diagnostic_pos(node_id, name)
	node := tc.a.node(node_id)
	if node.kind == .field_decl && tc.global_names[node.value] && node.typ != name_id
		&& pos.end > pos.offset + 1 {
		pos = token.new_span(pos.id, pos.offset, pos.offset + 1)
	}
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

fn (tc &TypeChecker) type_name_diagnostic_pos(node_id flat.NodeId, name string) token.Pos {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return token.Pos{}
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	return closest_text_span(source, name, node.pos.offset, node.pos.id) or { node.pos }
}

fn (mut tc TypeChecker) record_invalid_type_module_qualifier(name string, node_id flat.NodeId) bool {
	module_name := tc.unknown_type_module_qualifier(name, node_id) or { return false }
	mut msg := 'unknown module `${module_name}`'
	if suggested_alias := tc.import_alias_for_module_path(module_name, node_id) {
		msg += '; did you mean `${suggested_alias}`?'
	}
	tc.record_error_at(.unknown_type, msg, node_id,
		tc.qualified_module_diagnostic_pos(node_id, module_name))
	return true
}

fn (tc &TypeChecker) unknown_type_module_qualifier(name string, node_id flat.NodeId) ?string {
	if !name.contains('.') || name.starts_with('C.') || name.starts_with('JS.') {
		return none
	}
	// Dots in generic arguments belong to their types, not to the module
	// qualifier of the outer type (for example `datatypes.Set[time.Time]`).
	outer_name := name.all_before('[')
	if !outer_name.contains('.') {
		return none
	}
	module_name := outer_name.all_before_last('.')
	if module_name == tc.cur_module || tc.current_file_import_path_for_alias(module_name) != none
		|| tc.source_file_import_path_for_alias(module_name, node_id) != none {
		return none
	}
	return module_name
}

fn (tc &TypeChecker) source_file_import_path_for_alias(alias string, node_id flat.NodeId) ?string {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return none }
	return tc.file_import_alias_paths['${file.name}\x00${alias}'] or { return none }
}

fn (tc &TypeChecker) import_alias_for_module_path(module_path string, node_id flat.NodeId) ?string {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return none }
	prefix := '${file.name}\x00'
	for key, path in tc.file_import_alias_paths {
		if path == module_path && key.starts_with(prefix) {
			return key[prefix.len..]
		}
	}
	suffix := module_path.all_after_last('.')
	if tc.file_import_suffix_paths['${file.name}\x00${suffix}'] or { '' } == module_path {
		return suffix
	}
	if tc.source_file_import_path_for_alias(suffix, node_id) != none {
		return suffix
	}
	return none
}

fn (tc &TypeChecker) qualified_module_diagnostic_pos(node_id flat.NodeId, module_name string) token.Pos {
	if int(node_id) < 0 || int(node_id) >= tc.a.nodes.len {
		return token.Pos{}
	}
	node := tc.a.nodes[int(node_id)]
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	first_segment := module_name.all_before('.')
	highlight := if first_segment != module_name
		&& '${file.name}\x00${first_segment}' in tc.file_import_alias_paths {
		module_name[first_segment.len + 1..]
	} else {
		module_name
	}
	return closest_text_span(source, highlight, node.pos.offset, node.pos.id) or { node.pos }
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
	mut lookup_name := name
	if name.contains('.') {
		alias := name.all_before('.')
		if import_path := tc.current_file_import_path_for_alias(alias) {
			lookup_name = import_path + name[alias.len..]
		}
	}
	display_name := tc.diagnostic_qualified_name(lookup_name) or { lookup_name }
	base := 'unknown type `${display_name}`'
	if int(node_id) >= 0 && int(node_id) < tc.a.nodes.len {
		node := tc.a.nodes[int(node_id)]
		if node.kind == .type_decl && (flat.source_type_shape(node.typ).has(.function_compact) || flat.source_type_shape(node.typ).has(.function_spaced)) {
			return base
		}
		if node.kind == .type_decl && flat.source_type_shape(node.typ).has(.leading_bracket) {
			return '${base}.\nDid you mean `${node.value}`?'
		}
		if node.kind == .field_decl && flat.source_type_shape(node.typ).has(.leading_bracket) && flat.source_type_syntax(node.typ).fn_fragment() {
			suggestion := tc.parse_type(node.typ).name().replace_once('fn(', 'fn (')
			return '${base}.\nDid you mean `${suggestion}`?'
		}
	}
	contextual_candidates := tc.contextual_generic_sum_type_candidates(node_id)
	mut candidates := if contextual_candidates.len > 0 {
		contextual_candidates.clone()
	} else {
		tc.known_type_name_candidates()
	}
	if lookup_name.contains('.') {
		module_prefix := lookup_name.all_before_last('.') + '.'
		module_candidates := candidates.filter(it.starts_with(module_prefix))
		if module_candidates.len > 0 {
			candidates = module_candidates.clone()
		}
	}
	message := util.new_suggestion(lookup_name, candidates).say(base)
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
	syntax := flat.source_type_syntax(trimmed_source_type_id(node.typ))
	if !syntax.application_valid() {
		return []string{}
	}
	base_id := syntax.application_base()
	qualified_id := tc.qualify_type_name(base_id)
	params := tc.sum_generic_params[base_id] or {
		tc.sum_generic_params[qualified_id] or { return []string{} }
	}
	variants := tc.sum_types.type_ids(base_id) or { tc.sum_types.type_ids(qualified_id) or { return []string{} } }
	mut candidates := []string{cap: variants.len}
	for variant in variants {
		substituted := tc.substitute_generic_type(tc.parse_type(variant), syntax.application_args(), params).name()
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
	for name_id, _ in tc.structs {
		push_diagnostic_type_name(mut names, flat.source_type_name(name_id))
	}
	for name_id, target_id in tc.type_aliases.ids {
		target_shape := flat.source_type_shape(target_id)
		if target_shape.has(.function_compact) || target_shape.has(.function_spaced) {
			continue
		}
		push_diagnostic_type_name(mut names, flat.source_type_name(name_id))
	}
	for name_id, _ in tc.sum_types.ids {
		push_diagnostic_type_name(mut names, flat.source_type_name(name_id))
	}
	for name_id, _ in tc.enum_names {
		push_diagnostic_type_name(mut names, flat.source_type_name(name_id))
	}
	for name_id, _ in tc.interface_names {
		push_diagnostic_type_name(mut names, flat.source_type_name(name_id))
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

// type_diagnostic_pos_id is type_diagnostic_pos for the type spelled `name_id`.
fn (tc &TypeChecker) type_diagnostic_pos_id(node_id flat.NodeId, name_id u32) token.Pos {
	return tc.type_diagnostic_pos(node_id, flat.source_type_name(name_id))
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
			node.typ_text()
		}
		.ident {
			node.value_text()
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
	if name != '' && name !in candidates {
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

// closest_text_span finds the nearest occurrence without copying source suffixes.
// A match to the left wins a tie, including overlapping occurrences.
@[direct_array_access]
fn closest_text_span(source string, text string, anchor int, file_id int) ?token.Pos {
	if text == '' || source.len < text.len {
		return none
	}
	from := int_max(0, int_min(anchor, source.len))
	right_start := source.index_after_(text, from)
	if right_start == from {
		return token.new_span(file_id, from, from + text.len)
	}
	// Only the nearest match on either side can win. Bound the left scan by
	// the right match so repeated declaration types stay local to the anchor.
	left_limit := if right_start < 0 { 0 } else { int_max(from - (right_start - from), 0) }
	mut best_start := right_start
	for start := int_min(from - 1, source.len - text.len); start >= left_limit; start-- {
		if source[start] != text[0] {
			continue
		}
		mut matches := true
		for j in 1 .. text.len {
			if source[start + j] != text[j] {
				matches = false
				break
			}
		}
		if matches {
			best_start = start
			break
		}
	}
	if best_start < 0 {
		return none
	}
	return token.new_span(file_id, best_start, best_start + text.len)
}

fn closest_identifier_span(source string, name string, anchor int, file_id int) ?token.Pos {
	if name == '' || source.len < name.len {
		return none
	}
	// The anchor normally sits on (or immediately before) the identifier, so
	// search outward from it instead of scanning the file from the top: the
	// nearest word match on each side decides, exactly like the old full scan.
	// The right side is searched first so the left scan can stop once it is
	// further from the anchor than that match (a left match at the same distance
	// still wins): callers run this for every declaration, and an unbounded
	// walk to the start of the file made checking quadratic in program size.
	// A match exactly at the anchor is the nearest one possible, so it needs
	// no search at all.
	if identifier_word_match_at(source, name, anchor) {
		return token.new_span(file_id, anchor, anchor + name.len)
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
	left_limit := if right_start < 0 { 0 } else { int_max(anchor - (right_start - anchor), 0) }
	mut left_start := -1
	first := name[0]
	for i := int_min(anchor, source.len - name.len); i >= left_limit; i-- {
		// Without a right match this walks to the start of the file: most
		// bytes are told apart by the first one of the name alone.
		if source[i] != first || !identifier_word_match_at(source, name, i) {
			continue
		}
		left_start = i
		break
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

fn (tc &TypeChecker) selected_main_file_declares_type(name_id u32) bool {
	for file, selected in tc.diagnostic_files {
		if selected && !file.starts_with('generic:')
			&& tc.source_declares_type_in_scope(name_id, file, 'main') {
			return true
		}
	}
	return false
}

// check_fn_type_string_for_unsupported_generics
// validates helper state for types.
fn (mut tc TypeChecker) check_fn_type_string_for_unsupported_generics(typ_id u32, node_id flat.NodeId, generic_params map[u32]bool) {
	syntax := flat.source_type_syntax(typ_id)
	if !syntax.function_closed() {
		return
	}
	tc.check_type_string_for_unsupported_generics(syntax.function_result(), node_id, generic_params)
	for param in syntax.function_params() {
		param_type := flat.source_type_syntax(trimmed_source_type_id(param)).last_space_word()
		tc.check_type_string_for_unsupported_generics(param_type, node_id, generic_params)
	}
}

fn (tc &TypeChecker) generic_args_are_concrete(args_ids []u32) bool {
	for arg_source_id in args_ids {
		arg_source_id_2 := arg_source_id

		if tc.type_text_has_generic_placeholder(arg_source_id_2) {
			return false
		}
	}
	return true
}

// generic_placeholder_token_prescreen reports whether text can contain a bare
// single-capital-letter generic type component. It deliberately accepts a
// superset so the recursive parser can cheaply skip ordinary concrete types.
@[direct_array_access]
fn generic_placeholder_token_prescreen(text string) bool {
	for i in 0 .. text.len {
		ch := text[i]
		if ch < `A` || ch > `Z` {
			continue
		}
		prev_ident := i > 0 && placeholder_token_ident_char(text[i - 1])
		next_ident := i + 1 < text.len && placeholder_token_ident_char(text[i + 1])
		if !prev_ident && !next_ident {
			return true
		}
	}
	return false
}

@[inline]
fn placeholder_token_ident_char(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`)
		|| ch == `_`
}

fn (tc &TypeChecker) type_text_has_generic_placeholder(typ_id u32) bool {
	if !flat.source_type_query(typ_id).has(.generic_token) {
		return false
	}
	clean_id := trimmed_source_type_id(typ_id)
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if syntax.bare_generic() {
		return !tc.is_known_type_text(clean_id)
	}
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return tc.type_text_has_generic_placeholder(syntax.raw_base())
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return tc.type_text_has_generic_placeholder(syntax.bracket_inner())
			|| tc.type_text_has_generic_placeholder(syntax.bracket_base())
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return tc.type_text_has_generic_placeholder(syntax.bracket_base())
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if !syntax.function_closed() {
			return false
		}
		for param in syntax.function_params() {
			if tc.type_text_has_generic_placeholder(flat.source_type_syntax(param).parameter_type()) {
				return true
			}
		}
		return tc.type_text_has_generic_placeholder(syntax.function_result())
	}
	if syntax.application_valid() {
		for arg in syntax.application_args() {
			if tc.type_text_has_generic_placeholder(arg) {
				return true
			}
		}
	}
	return shape.has(.dotted) && source_type_is_bare_generic_param(syntax.unqualified())
		&& !tc.is_known_type_text(clean_id)
}

fn (tc &TypeChecker) type_text_has_unbound_generic_placeholder_ids(typ_id u32, bound []u32) bool {
	if !flat.source_type_query(typ_id).has(.generic_token) {
		return false
	}
	clean_id := trimmed_source_type_id(typ_id)
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if syntax.bare_generic() {
		return !tc.is_known_type_text(clean_id) && clean_id !in bound
	}
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return tc.type_text_has_unbound_generic_placeholder_ids(syntax.raw_base(), bound)
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return tc.type_text_has_unbound_generic_placeholder_ids(syntax.bracket_inner(), bound)
			|| tc.type_text_has_unbound_generic_placeholder_ids(syntax.bracket_base(), bound)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return tc.type_text_has_unbound_generic_placeholder_ids(syntax.bracket_base(), bound)
	}
	if syntax.application_valid() {
		for arg in syntax.application_args() {
			if tc.type_text_has_unbound_generic_placeholder_ids(arg, bound) {
				return true
			}
		}
	}
	return shape.has(.dotted) && source_type_is_bare_generic_param(syntax.unqualified())
		&& !tc.is_known_type_text(clean_id) && syntax.unqualified() !in bound
}

fn (tc &TypeChecker) type_text_has_generic_struct_placeholder_application(typ_id u32) bool {
	if !flat.source_type_query(typ_id).has(.generic_token) {
		return false
	}
	clean_id := trimmed_source_type_id(typ_id)
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.optional) || shape.has(.mutable_type)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return tc.type_text_has_generic_struct_placeholder_application(syntax.raw_base())
	}
	if shape.has(.leading_bracket) {
		return syntax.bracket_closed() && syntax.bracket_base() != 0
			&& tc.type_text_has_generic_struct_placeholder_application(syntax.bracket_base())
	}
	if !syntax.application_valid() {
		return false
	}
	base_id := syntax.application_base()
	qualified_id := tc.qualify_type_name(base_id)
	if (base_id in tc.struct_generic_params || qualified_id in tc.struct_generic_params)
		&& syntax.application_args().any(tc.type_text_has_generic_placeholder(it)) {
		return true
	}
	for arg in syntax.application_args() {
		if tc.type_text_has_generic_struct_placeholder_application(arg) {
			return true
		}
	}
	return false
}

// source_type_is_bare_generic_param preserves the untrimmed one-letter query.
fn source_type_is_bare_generic_param(id u32) bool {
	syntax := flat.source_type_syntax(id)
	return syntax.trimmed() == id && syntax.bare_generic()
}

// generic_type_application_parts splits a `Base[A, B]` type name into its base and its generic
// arguments. A map is spelled `map[K]V`, so its value type is a suffix *outside* the brackets and
// not a generic argument; it is appended as a second argument here. Without that, every match that
// goes through this function sees `map[string]FooBar` and `map[string]string` as the very same
// `map[string]` application, and the value type never participates in the comparison.
fn generic_type_application_parts(typ_id u32) (u32, []u32, bool) {
	syntax := flat.source_type_syntax(typ_id)
	if !syntax.application_valid() {
		return 0, []u32{}, false
	}
	return syntax.application_base(), syntax.application_args(), true
}

// v_int_literal_value parses a complete V integer literal — decimal, hex (`0x`), octal
// (`0o`), or binary (`0b`), with optional `_` digit separators — to its value. Returns
// none when `s` is not a whole integer literal (a const name, an expression, etc.), so
// const-length folding accepts `0xF & 6` / `[0b1100 >> 1]int`, not just decimal text.
fn v_int_literal_value(s string) ?int {
	if s == '' {
		return none
	}
	// Most literals have no separators; skip the copy `replace` makes even then.
	t := if s.contains_u8(`_`) { s.replace('_', '') } else { s }
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

fn unresolved_generic_receiver_type(typ &Type) bool {
	if typ is Unknown {
		if typ.placeholder != 0 {
			return true
		}
	}
	if typ is Struct {
		id := typ.source_type_id()
		syntax := flat.source_type_syntax(id)
		return syntax.trimmed() == id && syntax.bare_generic()
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

// generic_placeholder_from_unknown spells the type parameter that `typ` stands
// for, for display; compare `typ.placeholder` ids instead of this text.
fn generic_placeholder_from_unknown(typ Unknown) ?string {
	if typ.placeholder == 0 {
		return none
	}
	return flat.source_type_name(typ.placeholder)
}

fn (tc &TypeChecker) resolve_known_field_type(type_name_id u32, fallback Type) Type {
	qname_id := tc.qualify_type_name(type_name_id)
	allow_bare_symbol := qname_id == type_name_id
	if qname_id in tc.structs {
		return Type(Struct{
			name_id: qname_id
		})
	}
	if allow_bare_symbol && type_name_id in tc.structs {
		return Type(Struct{
			name_id: type_name_id
		})
	}
	if qname_id in tc.interface_names {
		return Type(Interface{
			name_id: qname_id
		})
	}
	if allow_bare_symbol && type_name_id in tc.interface_names {
		return Type(Interface{
			name_id: type_name_id
		})
	}
	if tc.type_aliases.contains(qname_id) {
		return Type(Alias{
			name_id:   qname_id
			base_type: tc.parse_type(tc.type_aliases.value_id(qname_id))
		})
	}
	if allow_bare_symbol && tc.type_aliases.contains(type_name_id) {
		return Type(Alias{
			name_id:   type_name_id
			base_type: tc.parse_type(tc.type_aliases.value_id(type_name_id))
		})
	}
	return fallback
}

// type_name_known returns type name known data for TypeChecker.
fn (tc &TypeChecker) type_name_known(typ_id u32) bool {
	if is_builtin_type_name_id(typ_id) || typ_id == flat.source_type_unknown {
		return true
	}
	if flat.source_type_query(typ_id).has(.c_prefix)
		|| (tc.is_js_backend && typ_id == checker_source_js_number)
		|| tc.active_generic_param_id(typ_id) {
		return true
	}
	shape := flat.source_type_shape(typ_id)
	if shape.has(.bracketed) && !shape.has(.leading_bracket) {
		generic_base_id := flat.source_type_syntax(typ_id).generic_base()
		if generic_base_id != typ_id {
			return tc.type_name_known(generic_base_id)
		}
	}
	qtyp_id := tc.qualify_type_name(typ_id)
	if !shape.has(.dotted) {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(typ_id) {
			return tc.type_symbol_known(resolved_id)
		}
	} else if short_source_id := tc.imported_type_short_name(typ_id) {
		if tc.type_aliases.contains(short_source_id) || short_source_id in tc.structs || short_source_id in tc.interface_names
			|| short_source_id in tc.enum_names || tc.sum_types.contains(short_source_id) {
			return true
		}
	}
	return tc.type_aliases.contains(qtyp_id) || qtyp_id in tc.structs || qtyp_id in tc.interface_names
		|| qtyp_id in tc.enum_names || tc.sum_types.contains(qtyp_id)
}

fn (tc &TypeChecker) type_name_known_in_current_module(typ_id u32) bool {
	qtyp_id := tc.qualify_type_name(typ_id)
	return tc.type_aliases.contains(qtyp_id) || qtyp_id in tc.structs || qtyp_id in tc.interface_names
		|| qtyp_id in tc.enum_names || tc.sum_types.contains(qtyp_id)
}

// should_check_named_type reports whether should check named type applies in types.
fn should_check_named_type(typ string) bool {
	return flat.source_type_syntax(flat.source_type_id(typ)).named_type()
}

fn (tc &TypeChecker) recursive_struct_declaration_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	needle := 'struct ${node.value.all_after_last('.')}'
	start := source.index(needle) or { return node.pos }
	return token.new_span(node.pos.id, start, start + needle.len)
}

// recursive_struct_lookup_name preserves a direct struct spelling before module qualification.
fn (tc &TypeChecker) recursive_struct_lookup_name(name_id u32) u32 {
	if name_id in tc.structs {
		return name_id
	}
	qualified_id := tc.qualify_type_name(name_id)
	return if qualified_id in tc.structs { qualified_id } else { name_id }
}

fn (tc &TypeChecker) struct_value_path_reaches(current_name_id u32, target_name_id u32, owner_decl int, mut seen map[u32]bool) bool {
	current_id := tc.recursive_struct_lookup_name(current_name_id)
	target_id := tc.recursive_struct_lookup_name(target_name_id)
	if current_id == target_id {
		return true
	}
	if seen[current_id] {
		return false
	}
	seen[current_id] = true
	if decl_id := tc.source_struct_decl_id_for_name(current_id) {
		if int(decl_id) > owner_decl {
			return false
		}
	}
	for field in tc.structs[current_id] or { []StructField{} } {
		field_type := unalias_type(field.typ)
		if field_type is Struct
			&& tc.struct_value_path_reaches(field_type.source_type_id(), target_id, owner_decl, mut seen) {
			return true
		}
	}
	return false
}

// check_attribute_marker_target reports `@[attribute]` on declarations that
// cannot declare a custom attribute schema (vlang/rfcs#35, slice 1): only
// structs can carry the marker.
fn (mut tc TypeChecker) check_attribute_marker_target(node_id flat.NodeId, node flat.Node) {
	if node.kind == .struct_decl || !tc.declaration_has_attribute(node_id, 'attribute') {
		return
	}
	kind := if node.kind == .fn_decl {
		'fn'
	} else if node.kind == .enum_decl {
		'enum'
	} else if node.kind == .interface_decl {
		'interface'
	} else if node.kind == .type_decl {
		'type'
	} else {
		'declaration'
	}
	tc.record_error_at(.assignment_mismatch, '`@[attribute]` can only be applied to struct declarations', node_id, tc.declaration_keyword_name_pos(node_id, kind))
}

// check_attribute_schema_decl validates a struct marked with `@[attribute]`,
// which declares a custom attribute schema (vlang/rfcs#35, slice 1). A schema
// is inert metadata: its fields hold immutable data, so `mut` fields and
// embeds are rejected here. Initialization, embedding elsewhere, and generic
// type argument uses are rejected at their own sites.
fn (mut tc TypeChecker) check_attribute_schema_decl(node_id flat.NodeId, node flat.Node) {
	display := node.value.all_after_last('.')
	if node.generic_params().len > 0 {
		tc.record_error_at(.assignment_mismatch, 'attribute schema `${display}` cannot have generic parameters', node_id, tc.struct_declaration_name_pos(node))
	}
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		if source_field_decl_is_mut(*field) {
			tc.record_error_at(.assignment_mismatch, 'attribute schema `${display}` cannot have `mut` fields', field_id, tc.struct_field_declaration_pos(*field))
		}
		if source_field_decl_is_embed(field, field_type_id) {
			tc.record_error_at(.assignment_mismatch, 'attribute schema `${display}` cannot embed structs', field_id, tc.struct_field_declaration_pos(*field))
			continue
		}
		if field.children_count > 0 {
			tc.record_error_at(.assignment_mismatch, 'attribute schema field `${display}.${field.value}` cannot have a default value', field_id, tc.struct_field_declaration_pos(*field))
		}
		clean_field_type_id := trimmed_source_type_id(field_type_id)
		if tc.attribute_field_kind(clean_field_type_id) == .unsupported {
			tc.record_error_at(.assignment_mismatch, 'attribute schema field `${display}.${field.value}` must be a string, integer, bool, or enum, not `${flat.source_type_name(clean_field_type_id)}`', field_id, tc.struct_field_declaration_pos(*field))
		}
	}
	for raw in tc.declaration_attributes[int(node_id)] {
		if !attribute_name_is(raw, 'attribute') || !raw.contains(':') {
			continue
		}
		target := attribute_string_unquote(raw.all_after(':').trim_space())
		if target.len > 0 && !is_attribute_target_name(target) {
			lo, hi := attribute_use_window(node)
			tc.record_error_at(.assignment_mismatch, 'unknown attribute target `${target}`', node_id, tc.attribute_use_pos(node, 'attribute', lo, hi))
		}
	}
}

// AttributeSchemaField is one field of a custom attribute schema.
struct AttributeSchemaField {
	name string
	// type_id is a source spelling ID, distinct from semantic TypeId.
	type_id u32
}

// AttributeSchema is a struct registered with `@[attribute]` whose uses are
// validated by check_custom_attribute_uses (vlang/rfcs#35, slice 2). Targets
// is empty when the schema may sit on any declaration.
struct AttributeSchema {
mut:
	name    string
	module  string
	fields  []AttributeSchemaField
	targets []string
}

// AttributeUse is one schema applied through an attribute, with the piece name
// that applied it (`Route` or `mod.Route`), which is what diagnostics quote.
struct AttributeUse {
	schema AttributeSchema
	base   string
}

// AttributeFieldKind classifies schema field types and attribute argument
// values. Only strings, integers, bools, and enums are supported (slice 2);
// everything else is AttributeFieldKind.unsupported.
enum AttributeFieldKind {
	string_
	integer
	boolean
	enum_
	unsupported
}

// attribute_target_kind_word names a declaration kind the way target
// restrictions spell it: 'fn', 'struct', 'enum', 'field', 'value' (enum
// value), 'interface'. Anything else reports as 'declaration'.
fn attribute_target_kind_word(kind flat.NodeKind) string {
	return match kind {
		.fn_decl { 'fn' }
		.struct_decl { 'struct' }
		.enum_decl { 'enum' }
		.field_decl { 'field' }
		.enum_field { 'value' }
		.interface_decl { 'interface' }
		.type_decl { 'type' }
		else { 'declaration' }
	}
}

// is_attribute_target_name reports whether `name` may restrict an attribute
// schema to a declaration kind.
fn is_attribute_target_name(name string) bool {
	return name in ['fn', 'struct', 'enum', 'field', 'value', 'interface', 'module']
}

// builtin_attribute_names enumerates the attribute names the compiler itself
// consumes, plus the ad-hoc names the standard library still parses by hand
// at comptime (orm, json, cbor, toml, vweb, ...). The warn-first pass for
// unregistered names (vlang/rfcs#35) never warns about these. The set is
// exact and case-sensitive: a name differing only in casing is unknown.
const builtin_attribute_names = [
	'_allow_multiple_values',
	'_constructor',
	'_destructor',
	'_linker_section',
	'_naked',
	'_pack',
	'_packed',
	'aligned',
	'alias',
	'assert_continues',
	'application',
	'attribute',
	'c',
	'callconv',
	'c_anon',
	'cbor',
	'cbor_rename_all',
	'cinit',
	'comptime',
	'comment',
	'console',
	'context_specific',
	'c_extern',
	'default',
	'deprecated',
	'deprecated_after',
	'delete',
	'direct_array_access',
	'email',
	'env',
	'explicit',
	'export',
	'flag',
	'footer',
	'freed',
	'fkey',
	'generated',
	'generic',
	'get',
	'has_default',
	'has_globals',
	'head',
	'heap',
	'host',
	'implicit',
	'ignore',
	'ignore_overflow',
	'index',
	'inline',
	'inner',
	'json',
	'json_as_number',
	'json_null',
	'keep_args_alive',
	'live',
	'long',
	'manualfree',
	'markused',
	'max',
	'min',
	'name',
	'must_use',
	'noalloc',
	'nocopy',
	'noinit',
	'noinline',
	'noreturn',
	'omitempty',
	'only',
	'optional',
	'options',
	'packed',
	'params',
	'patch',
	'post',
	'primary',
	'private',
	'put',
	'raw',
	'references',
	'repeats',
	'required',
	'serial',
	'serialize',
	'short',
	'skip',
	'soa',
	'spawn_stack',
	'sql',
	'sql_select',
	'sql_type',
	'str',
	'strict_map_index',
	'sumtype_to',
	'table',
	'tail',
	'toml',
	'translated',
	'trusted',
	'typedef',
	'unique',
	'unique_key',
	'union',
	'unsafe',
	'unscoped',
	'version',
	'wasm_import_namespace',
	'doc',
	'xdoc',
]

// is_builtin_attribute_name reports whether `name` names a compiler-known
// attribute, case-sensitively.
fn is_builtin_attribute_name(name string) bool {
	return name in builtin_attribute_names
}

// attribute_modules_compatible reports whether a schema declared in
// `schema_module` is visible to a bare use in `use_module`. Bare names
// resolve V-strict: the current module only.
fn attribute_modules_compatible(schema_module string, use_module string) bool {
	if schema_module == use_module {
		return true
	}
	return schema_module in ['', 'main'] && use_module in ['', 'main']
}

// attribute_string_literal reports whether `value` is a quoted string literal.
fn attribute_string_literal(value string) bool {
	return (value.len >= 2 && value[0] in [`'`, `"`] && value[value.len - 1] == value[0])
		|| (value.len >= 3 && value[0] == `r` && value[1] in [`'`, `"`]
			&& value[value.len - 1] == value[1])
}

// attribute_string_unquote strips one pair of surrounding quotes, if present.
fn attribute_string_unquote(value string) string {
	if value.len >= 3 && value[0] == `r` && value[1] in [`'`, `"`]
		&& value[value.len - 1] == value[1] {
		return value[2..value.len - 1]
	}
	if attribute_string_literal(value) {
		return value[1..value.len - 1]
	}
	return value
}

// attribute_integer_literal reports whether `value` spells an integer:
// decimal, hex, octal, or binary, with optional `-` and `_` separators.
fn attribute_integer_literal(value string) bool {
	mut text := value.replace('_', '')
	if text.starts_with('-') {
		text = text[1..]
	}
	if text.starts_with('0x') || text.starts_with('0X') {
		if text.len <= 2 {
			return false
		}
		for c in text[2..] {
			if !c.is_hex_digit() {
				return false
			}
		}
		return true
	}
	if text.starts_with('0o') || text.starts_with('0O') {
		if text.len <= 2 {
			return false
		}
		for c in text[2..] {
			if c < `0` || c > `7` {
				return false
			}
		}
		return true
	}
	if text.starts_with('0b') || text.starts_with('0B') {
		if text.len <= 2 {
			return false
		}
		for c in text[2..] {
			if c != `0` && c != `1` {
				return false
			}
		}
		return true
	}
	if text.len == 0 {
		return false
	}
	for c in text {
		if !c.is_digit() {
			return false
		}
	}
	return true
}

// attribute_arg_kind classifies a raw attribute argument value: quoted
// strings, integers, bools, and dot-prefixed enum values are valid;
// anything else (bare idents, floats, compound expressions) is invalid.
fn attribute_arg_kind(value string) AttributeFieldKind {
	if attribute_string_literal(value) {
		return .string_
	}
	if value == 'true' || value == 'false' {
		return .boolean
	}
	if attribute_integer_literal(value) {
		return .integer
	}
	if value.len > 1 && value[0] == `.` {
		return .enum_
	}
	return .unsupported
}

// attribute_field_kind maps a trimmed schema field source spelling ID to the
// argument kind it accepts. Only plain strings, integers, bools, and enums are supported;
// options, references, arrays, maps, and nested structs are unsupported.
fn (tc &TypeChecker) attribute_field_kind(type_id u32) AttributeFieldKind {
	clean := unalias_type(tc.parse_type(type_id))
	if clean is String {
		return .string_
	}
	if clean.is_integer() {
		return .integer
	}
	if clean.source_type_id() == flat.source_type_bool {
		return .boolean
	}
	if clean is Enum {
		return .enum_
	}
	return .unsupported
}

// attribute_enum_decl finds the enum declaration for `type_id`, trying the
// spelling as written, then the current-module qualification, then the short
// name.
fn (tc &TypeChecker) attribute_enum_decl(type_id u32, module string) ?flat.NodeId {
	clean_id := trimmed_source_type_id(type_id)
	short_id := flat.source_type_syntax(clean_id).unqualified()
	mod := if module in ['', 'main'] { 'main' } else { module }
	candidates := [clean_id,
		flat.source_type_composition_two(.qualified, flat.source_type_id(mod), short_id), short_id]
	for cand_id in candidates {
		if idx := tc.first_type_declaration_ids[cand_id] {
			if tc.a.nodes[idx].kind == .enum_decl {
				return flat.NodeId(idx)
			}
		}
	}
	return none
}

// attribute_enum_has_value reports whether enum `decl_id` declares `name`.
fn (tc &TypeChecker) attribute_enum_has_value(decl_id flat.NodeId, name string) bool {
	decl := tc.a.node(decl_id)
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(decl, i)
		if field.kind == .enum_field && field.value == name {
			return true
		}
	}
	return false
}

// collect_attribute_schemas registers every struct marked with `@[attribute]`
// by short name. Several modules may declare the same name; uses resolve
// V-strict in check_custom_attribute_uses.
fn (mut tc TypeChecker) collect_attribute_schemas() {
	tc.attribute_schemas = map[string][]AttributeSchema{}
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
			.struct_decl {
				node_id := flat.NodeId(i)
				if !tc.declaration_has_attribute(node_id, 'attribute') {
					continue
				}
				short := node.value.all_after_last('.')
				module := if node.value.contains('.') {
					node.value.all_before_last('.')
				} else {
					cur_module
				}
				mut schema := AttributeSchema{
					name:   short
					module: module
				}
				for j in 0 .. node.children_count {
					field := tc.a.child_node(&node, j)
					if field.kind != .field_decl {
						continue
					}
					type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
					if source_field_decl_is_embed(field, type_id) {
						continue
					}
					schema.fields << AttributeSchemaField{
						name:    field.value
						type_id: trimmed_source_type_id(type_id)
					}
				}
				for raw in tc.declaration_attributes[int(node_id)] {
					if !attribute_name_is(raw, 'attribute') || !raw.contains(':') {
						continue
					}
					target := attribute_string_unquote(raw.all_after(':').trim_space())
					if target.len > 0 {
						schema.targets << target
					}
				}
				tc.attribute_schemas[short] << schema
			}
			else {}
		}
	}
}

// attribute_use_window returns the source range that holds the attributes of a
// declaration: the groups written just before its keyword.
fn attribute_use_window(node flat.Node) (int, int) {
	anchor := int(node.pos.offset)
	return int_max(0, anchor - 400), anchor
}

// attribute_child_window returns the source range that holds the attributes of
// one struct field or enum value: from the end of the previous sibling to the
// start of the next one, so a neighbouring sibling's attribute can never be
// mistaken for this one's.
fn attribute_child_window(prev_end int, own_start int, next_start int, own_end int) (int, int) {
	lo := if prev_end > own_start { prev_end } else { own_start }
	hi := if next_start > own_start { next_start } else { own_end }
	return lo, hi
}

// attribute_use_pos locates the attribute named `base` inside the source
// range [lo, hi), so use-site errors point at the attribute rather than the
// declaration. Only whole-word matches followed by `:`, `(`, `;`, `]`, `,`, or
// the end of the group count, so `Route_method` pieces and string contents
// cannot match `Route`. Falls back to the node position.
fn (tc &TypeChecker) attribute_use_pos(node flat.Node, base string, lo int, hi int) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	// A field or enum value owns exactly the groups inside its span, so the
	// first match is its own. A declaration's groups precede its keyword, so
	// the match closest to the keyword is its own: an earlier declaration
	// using the same schema must not capture the position.
	nearest_to_end := node.kind !in [.field_decl, .enum_field]
	idx := attribute_find_in_range(source, lo, hi, base, nearest_to_end) or {
		return node.pos
	}
	return token.new_span(node.pos.id, idx, idx + base.len)
}

// attribute_find_in_range returns the offset of a `@[...]` group in
// source[lo..hi) that uses `base` as an attribute name: the first one, or the
// last one (closest to `hi`) when `nearest_to_end`.
fn attribute_find_in_range(source string, lo int, hi int, base string, nearest_to_end bool) ?int {
	start := int_max(0, lo)
	end := int_min(hi, source.len)
	if start >= end {
		return none
	}
	window := source[start..end]
	mut found := ?int(none)
	mut search_from := 0
	for search_from < window.len {
		rel := window[search_from..].index('@[') or { break }
		group_at := search_from + rel
		group_end_rel := window[group_at..].index(']') or { break }
		group := window[group_at..group_at + group_end_rel]
		if idx := attribute_whole_word_index(group, base) {
			mut k := idx + base.len
			for k < group.len && group[k] in [` `, `\t`, `\n`, `\r`] {
				k++
			}
			if k >= group.len || group[k] in [`:`, `(`, `;`, `]`, `,`] {
				found = start + group_at + idx
				if !nearest_to_end {
					return found
				}
			}
		}
		search_from = group_at + 2
	}
	return found
}

// attribute_whole_word_index finds `word` in `s` at a non-identifier
// boundary on both sides, so `Route` never matches inside `Route_method`.
fn attribute_whole_word_index(s string, word string) ?int {
	if word.len == 0 || word.len > s.len {
		return none
	}
	mut from := 0
	for from + word.len <= s.len {
		rel := s[from..].index(word) or { return none }
		i := from + rel
		before_ok := i == 0 || !(s[i - 1].is_alnum() || s[i - 1] == `_`)
		after_ok := i + word.len >= s.len
			|| !(s[i + word.len].is_alnum() || s[i + word.len] == `_`)
		if before_ok && after_ok {
			return i
		}
		from = i + 1
	}
	return none
}

// check_custom_attribute_uses validates every attribute use whose name
// matches a registered `@[attribute]` schema (vlang/rfcs#35, slice 2):
// target restrictions, argument counts and names, and argument types.
// Names that match no schema are reported by warn_unknown_attribute_names
// (vlang/rfcs#35, warn-first): they may be typos or legacy ad-hoc attributes,
// so they warn instead of erroring and nothing that compiles today stops
// compiling.
fn (mut tc TypeChecker) check_custom_attribute_uses() {
	if tc.valid_diagnostic_fast {
		return
	}
	tc.collect_attribute_schemas()
	mut warning_heads := map[string][]AttributeWarningHead{}
	// The warn-first pass below needs the registry even when it is empty:
	// with no schemas registered every non-builtin attribute name warns.
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
			.fn_decl, .struct_decl, .enum_decl, .type_decl, .interface_decl {
				node_id := flat.NodeId(i)
				if !tc.should_check_source_name(node_id) {
					continue
				}
				lo, hi := attribute_use_window(node)
				tc.check_attribute_use_pieces(node_id, node, cur_module, tc.declaration_attributes[int(node_id)], lo, hi, mut warning_heads)
			}
			else {}
		}
		if node.kind == .struct_decl {
			for j in 0 .. node.children_count {
				field_id := tc.a.child(&node, j)
				field := tc.a.child_node(&node, j)
				if field.kind != .field_decl {
					continue
				}
				meta := field.generic_params()
				if meta.len < 2 || !tc.should_check_source_name(field_id) {
					continue
				}
				lo, hi := attribute_child_window(
					if j > 0 {
						int(tc.a.child_node(&node, j - 1).pos.end)
					} else {
						int(node.pos.offset)
					},
					int(field.pos.offset),
					if j + 1 < node.children_count {
						int(tc.a.child_node(&node, j + 1).pos.offset)
					} else {
						int(node.pos.end)
					},
					int(field.pos.end),
				)
				tc.check_attribute_use_pieces(field_id, field, cur_module, meta[1..].clone(), lo, hi, mut warning_heads)
			}
		}
		if node.kind == .enum_decl {
			for j in 0 .. node.children_count {
				value_id := tc.a.child(&node, j)
				value := tc.a.child_node(&node, j)
				if value.kind != .enum_field {
					continue
				}
				meta := value.generic_params()
				if meta.len == 0 || !tc.should_check_source_name(value_id) {
					continue
				}
				lo, hi := attribute_child_window(
					if j > 0 {
						int(tc.a.child_node(&node, j - 1).pos.end)
					} else {
						int(node.pos.offset)
					},
					int(value.pos.offset),
					if j + 1 < node.children_count {
						int(tc.a.child_node(&node, j + 1).pos.offset)
					} else {
						int(node.pos.end)
					},
					int(value.pos.end),
				)
				tc.check_attribute_use_pieces(value_id, value, cur_module, meta.clone(), lo, hi, mut warning_heads)
			}
		}
	}
}

// check_attribute_use_pieces validates the attribute `pieces` on one
// declaration, field, or enum value against the registered schemas. `lo` and
// `hi` bound the source range the attributes of this node occupy.
fn (mut tc TypeChecker) check_attribute_use_pieces(node_id flat.NodeId, node flat.Node, module string, pieces []string, lo int, hi int, mut warning_heads map[string][]AttributeWarningHead) {
	// Schema interpretation needs the parser's no-argument identity: the legacy
	// strings alone make @['Schema'] indistinguishable from @[Schema].
	mut kinds_id := node.attribute_kinds_id()
	if node.kind !in [.field_decl, .enum_field] {
		kinds_id = tc.declaration_attribute_kinds[int(node_id)]
	}
	kinds := if kinds_id != 0 { flat.source_type_name(kinds_id).split(',') } else { []string{} }
	mut schema_pieces := []string{cap: pieces.len}
	for i, piece in pieces {
		if i < kinds.len && kinds[i].int() >= 0
			&& kinds[i].int() & flat.attribute_kind_no_arg != 0 {
			continue
		}
		schema_pieces << piece
	}
	uses := tc.attribute_use_schemas(schema_pieces, module)
	for use in uses {
		pos := tc.attribute_use_pos(node, use.base, lo, hi)
		if use.schema.targets.len > 0
			&& attribute_target_kind_word(node.kind) !in use.schema.targets {
			tc.record_error_at(.assignment_mismatch, 'attribute `${use.base}` cannot be applied to `${attribute_target_kind_word(node.kind)}`', node_id, pos)
			continue
		}
		tc.check_attribute_use_args(node_id, use.schema, use.base, schema_pieces, pos, module)
	}
	tc.warn_unknown_attribute_names(node_id, node, pieces, module, uses, mut warning_heads)
}

// AttributeWarningHead retains a real, unquoted source name, rather than a
// synthetic Name_key piece produced by flattening an attribute call.
struct AttributeWarningHead {
	name            string
	offset          int
	leading         bool
	legacy          bool
	previous_offset int
	previous_kind   token.Token
	first_kind      token.Token
mut:
	group_end          int
	declaration_offset int = -1
	declaration_end    int = -1
}

// attribute_warning_heads scans each source only when an unknown name needs a
// warning. Tokens keep strings, comments, and call arguments out of the index.
fn attribute_warning_heads(source string) []AttributeWarningHead {
	mut files := token.FileSet.new()
	mut file := files.add_file('<attribute warning names>', source.len)
	mut lexer := scanner.new_scanner(pref.new_preferences(), .skip_interpolation)
	lexer.init(file, source)
	mut heads := []AttributeWarningHead{}
	mut pending_start := 0
	mut attached_start := 0
	mut attached_end := 0
	mut in_group := false
	mut depth := 0
	mut expects_head := false
	mut name := ''
	mut name_offset := 0
	mut after_dot := false
	mut previous_kind := token.Token.eof
	mut previous_offset := -1
	mut previous_access := false
	mut group_start := 0
	mut group_leading := false
	mut group_legacy := false
	mut group_previous_kind := token.Token.eof
	mut group_previous_offset := -1
	mut group_first_kind := token.Token.eof
	for {
		kind := lexer.scan()
		if kind == .eof {
			break
		}
		if kind == .comment {
			continue
		}
		if !in_group {
			access := kind in [.key_pub, .key_mut]
				|| (kind == .name && lexer.lit == 'module_mut' && previous_kind == .key_pub)
			if kind in [.attribute, .lsbr] {
				in_group = true
				expects_head = true
				group_start = heads.len
				group_leading = previous_access
					|| previous_kind in [.eof, .semicolon, .lcbr, .rcbr, .colon]
				group_legacy = kind == .lsbr
				group_previous_kind = previous_kind
				group_previous_offset = previous_offset
				group_first_kind = .eof
				continue
			}
			if pending_start < heads.len && kind != .semicolon && !access {
				attached_start = pending_start
				attached_end = heads.len
				for i in attached_start .. attached_end {
					if heads[i].leading {
						heads[i].declaration_offset = lexer.pos
						heads[i].declaration_end = source.len
					}
				}
				pending_start = heads.len
			}
			if kind in [.lcbr, .rcbr, .assign, .semicolon] {
				for i in attached_start .. attached_end {
					heads[i].declaration_end = lexer.pos
				}
				attached_start = attached_end
			}
			previous_kind = kind
			previous_offset = lexer.pos
			previous_access = access
			continue
		}
		if group_first_kind == .eof {
			group_first_kind = kind
		}
		if depth > 0 {
			if kind == .lpar {
				depth++
			} else if kind == .rpar {
				depth--
			}
			continue
		}
		if name.len > 0 {
			if kind == .dot && !after_dot {
				after_dot = true
				continue
			}
			if after_dot && kind in [.name, .key_pub] {
				name += '.' + lexer.lit
				after_dot = false
				continue
			}
			heads << AttributeWarningHead{
				name:            name
				offset:          name_offset
				leading:         group_leading
				legacy:          group_legacy
				previous_offset: group_previous_offset
				previous_kind:   group_previous_kind
				first_kind:      group_first_kind
			}
			name = ''
			after_dot = false
		}
		match kind {
			.rsbr {
				in_group = false
				for i in group_start .. heads.len {
					heads[i].group_end = lexer.pos + 1
				}
				previous_kind = .rsbr
				previous_offset = lexer.pos
				previous_access = false
			}
			.lpar { depth++ }
			.comma, .semicolon { expects_head = true }
			else {
				if expects_head {
					expects_head = false
					if kind in [.name, .key_pub] {
						name = lexer.lit
						name_offset = lexer.pos
					}
				}
			}
		}
	}
	return heads
}

// attribute_warning_head_owns_node distinguishes leading groups from trailing
// child groups without a byte limit or borrowing a neighbouring child's name.
fn (tc &TypeChecker) attribute_warning_head_owns_node(head AttributeWarningHead, node flat.Node) bool {
	start := int(node.pos.offset)
	if node.kind !in [.field_decl, .enum_field] {
		return head.leading && start >= head.declaration_offset && start < head.declaration_end
	}
	if head.leading {
		return head.declaration_offset == start
	}
	if node.kind == .enum_field {
		mut end := int(node.pos.end)
		for i in 0 .. node.children_count {
			end = int_max(end, int(tc.a.child_node(&node, i).pos.end))
		}
		return head.previous_offset >= start && head.previous_offset < end
	}
	if head.offset < start || head.offset >= int(node.pos.end) {
		return false
	}
	if !head.legacy {
		return true
	}
	// Legacy field attributes finish the field. Brackets in its type or default
	// expression must not turn quoted metadata into a named attribute.
	if head.group_end != int(node.pos.end) {
		return false
	}
	for i in 0 .. node.children_count {
		if head.offset < int(tc.a.child_node(&node, i).pos.end) {
			return false
		}
	}
	if head.previous_kind == .name
		&& head.first_kind in [.name, .amp, .and, .question, .not, .lsbr, .lpar, .key_fn, .key_struct,
			.ellipsis, .key_mut, .key_shared, .key_atomic, .key_typeof] {
		return false
	}
	return true
}

// warn_unknown_attribute_names warns once per real attribute name. Whole-string
// metadata and flattened call arguments are not names, while independently
// written Name and Name_key attributes both remain eligible for warnings.
fn (mut tc TypeChecker) warn_unknown_attribute_names(node_id flat.NodeId, node flat.Node, pieces []string, module string, uses []AttributeUse, mut warning_heads map[string][]AttributeWarningHead) {
	mut matched := map[string]bool{}
	for use in uses {
		matched[use.base] = true
	}
	mut warned := map[string]bool{}
	for raw in pieces {
		name := raw.all_before(':').trim_space()
		if matched[name] || warned[name] || !is_attribute_warnable_name(name)
			|| is_builtin_attribute_name(name) {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		if file.name !in warning_heads {
			source := tc.source_texts_by_file[file.name] or { continue }
			warning_heads[file.name] = attribute_warning_heads(source)
		}
		mut offset := -1
		heads := warning_heads[file.name] or { []AttributeWarningHead{} }
		for head in heads {
			if head.name != name || !tc.attribute_warning_head_owns_node(head, node) {
				continue
			}
			offset = head.offset
			break
		}
		if offset < 0 {
			continue
		}
		hint := tc.attribute_unknown_hint(name, module)
		mut message := 'unknown attribute `${name}`'
		if hint.len > 0 {
			message = 'unknown attribute `${name}`.${hint}'
		}
		message += ' Unvalidated attributes are deprecated; declare a schema with `@[attribute]` or drop the attribute.'
		tc.record_warning_at(.assignment_mismatch, message, node_id,
			token.new_span(node.pos.id, offset, offset + name.len))
		warned[name] = true
	}
}

// attribute_unknown_hint returns the matching ` Did you mean \`X\`?` fragment
// for an unknown attribute name, or an empty string, per the RFC sample
// wording. An exact same-named schema in another module beats fuzzy
// matching: the qualified form is the fix per "Name resolution across
// modules". Otherwise a name sharing the first letter within the edit
// budget suggests the closest schema short name or builtin.
fn (tc &TypeChecker) attribute_unknown_hint(name string, module string) string {
	mut qualified := []string{}
	if name.contains('.') {
		qualifier := name.all_before_last('.')
		short := name.all_after_last('.')
		for schema in tc.attribute_schemas[short] or { []AttributeSchema{} } {
			if schema.module != qualifier {
				qualified << '${schema.module}.${short}'
			}
		}
	} else {
		for schema in tc.attribute_schemas[name] or { []AttributeSchema{} } {
			if !attribute_modules_compatible(schema.module, module) {
				qualified << '${schema.module}.${name}'
			}
		}
	}
	if qualified.len > 0 {
		return ' Did you mean `${qualified[0]}`?'
	}
	mut candidates := builtin_attribute_names.clone()
	for short, _ in tc.attribute_schemas {
		candidates << short
	}
	mut closest := ''
	mut closest_distance := name.len + 1
	lower_name := name.to_lower()
	for candidate in candidates {
		lower_candidate := candidate.to_lower()
		if lower_name.len == 0 || lower_candidate.len == 0 || lower_name[0] != lower_candidate[0] {
			continue
		}
		distance := strings.levenshtein_distance(lower_name, lower_candidate)
		max_distance := if name.len <= 3 { 1 } else { int_max(1, name.len / 3) }
		if distance <= max_distance && distance < closest_distance {
			closest = candidate
			closest_distance = distance
		}
	}
	if closest.len > 0 {
		return ' Did you mean `${closest}`?'
	}
	return ''
}

// attribute_use_schemas finds the schemas `pieces` uses, V-strict (RFC
// vlang/rfcs#35, "Name resolution across modules"): a bare `Name` sees the
// current module only, and a dotted `mod.Name` sees the schema `Name` of the
// module `mod`. Resolution is registry-driven rather than a name heuristic,
// so `Name_key` pieces attach to `Name` and a schema whose own name contains
// `_` resolves like any other.
fn (tc &TypeChecker) attribute_use_schemas(pieces []string, module string) []AttributeUse {
	mut names := []string{}
	for raw in pieces {
		name := raw.all_before(':').trim_space()
		if name.len > 0 && name !in names {
			names << name
		}
	}
	mut uses := []AttributeUse{}
	for name in names {
		mut qualifier := ''
		mut short := name
		if idx := name.last_index('.') {
			qualifier = name[..idx]
			short = name[idx + 1..]
		}
		for schema in tc.attribute_schemas[short] or { []AttributeSchema{} } {
			resolved := if qualifier.len > 0 {
				schema.module == qualifier
			} else {
				attribute_modules_compatible(schema.module, module)
			}
			if resolved {
				uses << AttributeUse{
					schema: schema
					base:   name
				}
				break
			}
		}
	}
	return uses
}

// check_attribute_use_args reconstructs one schema call from its flattened
// pieces and checks argument counts, names, and types against the schema.
// The parser flattens `@[Name(p0, key: v1, p1)]` into `Name: p0`,
// `Name_key: v1`, `Name_1: p1` (the first positional becomes the base value;
// later positionals are numbered from 1).
fn (mut tc TypeChecker) check_attribute_use_args(node_id flat.NodeId, schema AttributeSchema, base string, pieces []string, pos token.Pos, module string) {
	// Split the flattened pieces into parallel name/value lists first: the
	// legacy representation stores a dot-prefixed enum value (`.get`) as an empty piece
	// followed by a numbered positional holding the bare ident
	// (`Name_key: ` + `Name_1: get`), and the two must be reassembled below.
	mut names := []string{}
	mut raw_values := []string{}
	for raw in pieces {
		name := raw.all_before(':').trim_space()
		if name != base && !name.starts_with(base + '_') {
			continue
		}
		value := if raw.contains(':') { raw.all_after(':').trim_space() } else { '' }
		names << name
		raw_values << value
	}
	mut positional := []string{}
	mut named_order := []string{}
	mut named := map[string][]string{}
	mut idx := 0
	for idx < names.len {
		name := names[idx]
		value := raw_values[idx]
		if merged := attribute_dot_enum_value(base, names, raw_values, idx) {
			if name == base || is_attribute_numbered_suffix(base, name) {
				positional << merged
			} else {
				key := name[base.len + 1..]
				if key !in named {
					named_order << key
				}
				named[key] << merged
			}
			idx += 2
			continue
		}
		if name == base {
			if value.len > 0 {
				positional << value
			}
			idx++
			continue
		}
		if is_attribute_numbered_suffix(base, name) {
			positional << value
			idx++
			continue
		}
		key := name[base.len + 1..]
		if key !in named {
			named_order << key
		}
		named[key] << value
		idx++
	}
	// Numbered positionals (`Name_1`, ...) arrive in source order already.
	if positional.len > schema.fields.len {
		want := if schema.fields.len == 1 {
			'1 positional argument'
		} else {
			'${schema.fields.len} positional arguments'
		}
		got := if positional.len == 1 {
			'1 positional argument'
		} else {
			'${positional.len} positional arguments'
		}
		tc.record_error_at(.assignment_mismatch, 'attribute `${base}` expects ${want}, got ${got}', node_id, pos)
		positional = positional[..schema.fields.len]
	}
	mut bound := map[string]bool{}
	for positional_idx, value in positional {
		field := schema.fields[positional_idx]
		bound[field.name] = true
		tc.check_attribute_arg_value(node_id, schema, base, field, value, pos, module)
	}
	for key in named_order {
		values := named[key]
		field_idx := schema_field_index(schema, key)
		if field_idx < 0 {
			tc.record_error_at(.assignment_mismatch, 'unknown field `${key}` in attribute `${base}`', node_id, pos)
			continue
		}
		field := schema.fields[field_idx]
		if bound[field.name] || values.len > 1 {
			tc.record_error_at(.assignment_mismatch, 'duplicate field `${key}` in attribute `${base}`', node_id, pos)
		}
		bound[field.name] = true
		for value in values {
			tc.check_attribute_arg_value(node_id, schema, base, field, value, pos, module)
		}
	}
	for field in schema.fields {
		if !bound[field.name] {
			tc.record_error_at(.assignment_mismatch, 'attribute `${base}` is missing field `${field.name}`', node_id, pos)
		}
	}
}

// is_attribute_numbered_suffix reports whether `name` is a numbered
// positional piece (`Base_1`, ...) of the attribute `base`.
fn is_attribute_numbered_suffix(base string, name string) bool {
	if !name.starts_with(base + '_') {
		return false
	}
	suffix := name[base.len + 1..]
	return suffix.len > 0 && suffix.bytes().all(it.is_digit())
}

// is_bare_ident reports whether `s` is a plain identifier.
fn is_bare_ident(s string) bool {
	if s.len == 0 || !(s[0].is_letter() || s[0] == `_`) {
		return false
	}
	for c in s[1..] {
		if !(c.is_alnum() || c == `_`) {
			return false
		}
	}
	return true
}

// is_attribute_warnable_name reports whether `name` looks like an attribute
// name: a bare identifier or a `mod.Name` piece. Whole-string attributes and
// arbitrary fragments are not attribute names and never warn.
fn is_attribute_warnable_name(name string) bool {
	return is_bare_ident(name.all_after_last('.'))
}

// attribute_dot_enum_value reassembles a dot-prefixed enum value that the
// legacy representation splits into an empty piece plus a numbered positional holding the
// bare ident (`Name_key: ` + `Name_1: get` becomes `.get`). The merge only
// fires for a bare ident, so an empty piece followed by a quoted string,
// a number, or nothing keeps its own meaning.
fn attribute_dot_enum_value(base string, names []string, values []string, idx int) ?string {
	if values[idx].len > 0 || idx + 1 >= names.len {
		return none
	}
	if !is_attribute_numbered_suffix(base, names[idx + 1]) {
		return none
	}
	if !is_bare_ident(values[idx + 1]) {
		return none
	}
	return '.' + values[idx + 1]
}

// schema_field_index finds the schema field named `name`, or -1.
fn schema_field_index(schema AttributeSchema, name string) int {
	for idx, field in schema.fields {
		if field.name == name {
			return idx
		}
	}
	return -1
}

// check_attribute_arg_value checks one argument value against its schema
// field: literal kind first, then enum membership for enum fields. Fields
// whose type is unsupported are skipped here; the schema declaration itself
// already errors for those.
fn (mut tc TypeChecker) check_attribute_arg_value(node_id flat.NodeId, schema AttributeSchema, base string, field AttributeSchemaField, value string, pos token.Pos, module string) {
	kind := tc.attribute_field_kind(field.type_id)
	if kind == .unsupported {
		return
	}
	got := attribute_arg_kind(value)
	if got == .unsupported {
		tc.record_error_at(.assignment_mismatch, 'field `${field.name}` in attribute `${base}` expects ${attribute_kind_word(kind)}, got `${value}`', node_id, pos)
		return
	}
	if got != kind {
		// An enum value (`.get`) never matches a non-enum field and a plain
		// literal never matches an enum field.
		tc.record_error_at(.assignment_mismatch, 'field `${field.name}` in attribute `${base}` expects ${attribute_kind_word(kind)}, got `${value}`', node_id, pos)
		return
	}
	if kind == .enum_ {
		if decl_id := tc.attribute_enum_decl(field.type_id, module) {
			name := value[1..]
			if !tc.attribute_enum_has_value(decl_id, name) {
				enum_name := flat.source_type_name(flat.source_type_syntax(trimmed_source_type_id(field.type_id)).unqualified())
				tc.record_error_at(.assignment_mismatch, '`${value}` is not a value of enum `${enum_name}`', node_id, pos)
			}
		}
	}
}

// attribute_kind_word phrases an expected argument kind for diagnostics.
fn attribute_kind_word(kind AttributeFieldKind) string {
	return match kind {
		.string_ { 'a string' }
		.integer { 'an integer' }
		.boolean { 'a boolean' }
		.enum_ { 'an enum value' }
		.unsupported { 'a supported value' }
	}
}

// check_struct_field_defaults validates check struct field defaults state for types.
fn (mut tc TypeChecker) check_struct_field_defaults(node_id flat.NodeId, node flat.Node) {
	node_name_id := node.source_value_id()
	node_short_id := flat.source_type_syntax(node_name_id).unqualified()
	saved_generic_params := tc.fn_context.generic_params.clone()
	if node.generic_params().len > 0 {
		tc.fn_context.generic_params = generic_param_ids(node.generic_params())
	}
	mut seen_field_names := map[string]bool{}
	mut recursive_struct_reported := false
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		field_type_text := flat.source_type_name(field_type_id)
		clean_field_type_id := trimmed_source_type_id(field_type_id)
		if flat.source_type_shape(clean_field_type_id).has(.leading_bracket) {
			syntax := flat.source_type_syntax(clean_field_type_id)
			if syntax.bracket_closed() && syntax.bracket_inner() != 0 {
				bound := flat.source_type_syntax(syntax.bracket_inner())
				if bound.dollar_prefix() && !bound.define_prefix() {
					tc.record_error_at(.assignment_mismatch, 'only \$d() is supported as fixed array size quantifier at compile time', field_id, tc.fixed_array_bound_pos(*field, flat.source_type_name(syntax.bracket_inner())))
				}
			}
		}
		field_type_raw := tc.parse_type(field_type_id)
		field_type := unalias_type(field_type_raw)
		if flat.source_type_shape(field_type_id).has(.option_type) {
			alias_name_id := flat.source_type_syntax(field_type_id).raw_base()
			alias_name := flat.source_type_name(alias_name_id)
			qualified_alias_id := tc.qualify_type_name(alias_name_id)
			alias_target_id := tc.type_aliases.type_id(qualified_alias_id) or {
				tc.type_aliases.type_id(alias_name_id) or { u32(0) }
			}
			if flat.source_type_shape(alias_target_id).has(.option_type) {
				alias_target := flat.source_type_name(alias_target_id)
				tc.record_error_at(.assignment_mismatch, 'cannot use double options like `?${alias_target}`, `${field_type_text}` is a double option. use `${alias_name}` instead', field_id, tc.struct_field_type_pos(*field))
			}
		}
		if field_type is OptionType {
			recursive_type := unalias_type(field_type.base_type)
			if recursive_type is Struct
				&& tc.qualify_type_name(recursive_type.source_type_id()) == tc.qualify_type_name(node_name_id) {
				tc.record_error_at(.unknown_type, 'recursive struct is only possible with optional pointer (e.g. ?&${node.value.all_after_last('.')})', field_id, field.pos)
			}
		}
		is_embed := source_field_decl_is_embed(field, field_type_id)
		direct_recursive_default := !is_embed && field.children_count > 0 && field_type is Struct
			&& flat.source_type_syntax(field_type.source_type_id()).unqualified() == node_short_id
		if !recursive_struct_reported && field_type is Struct && !direct_recursive_default {
			mut seen := map[u32]bool{}
			if tc.struct_value_path_reaches(field_type.source_type_id(), node_name_id, int(node_id), mut seen) {
				pos := if is_embed {
					tc.recursive_struct_declaration_pos(node)
				} else {
					tc.type_declaration_name_pos(node_id)
				}
				tc.record_error_at(.unknown_type, 'invalid recursive struct `${node.value}`', node_id,
					pos)
				recursive_struct_reported = true
			}
		}
		if field_type_id == qualify_source_type_map {
			tc.record_error_at(.unknown_type, 'cannot use the map type without key and value definition', field_id, tc.struct_field_type_pos(*field))
		}
		if !is_embed && field.value in seen_field_names {
			tc.record_error_at(.duplicate_decl, 'field name `${field.value}` duplicate', field_id, tc.struct_field_declaration_pos(*field))
		} else if !is_embed {
			seen_field_names[field.value] = true
		}
		if !is_embed && node.value != 'C.' && !node.value.starts_with('C.')
			&& !comma_attr_text_has(node.typ_text(), 'c_anon')
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
			embedded_type_pos := tc.anonymous_struct_embedded_type_pos(*field)
			anonymous_named_field := is_anonymous && embedded_type_pos.offset > field.pos.offset
			if is_anonymous && !anonymous_named_field {
				tc.record_error_at(.assignment_mismatch, 'cannot embed non-struct `${field_type_text}`', field_id, tc.anonymous_struct_declaration_pos(node))
			}
			message := if is_anonymous {
				'cannot embed non-struct `${field_type_text}`'
			} else {
				'`${field_type_text}` (alias of `${field_type.name()}`) is not a struct'
			}
			diagnostic_pos := if anonymous_named_field {
				embedded_type_pos
			} else {
				tc.struct_field_declaration_pos(*field)
			}
			tc.record_error_at(.assignment_mismatch, message, field_id, diagnostic_pos)
		}
		if is_embed {
			// An attribute schema is inert metadata, so it cannot be embedded.
			if embedded_decl := tc.source_struct_decl_id_for_name(field_type_id) {
				if tc.declaration_has_attribute(embedded_decl, 'attribute') {
					embedded_name := flat.source_type_name(flat.source_type_syntax(trimmed_source_type_id(field_type_id)).unqualified())
					tc.record_error_at(.assignment_mismatch, 'cannot embed attribute schema `${embedded_name}`', field_id, tc.struct_field_declaration_pos(*field))
				}
			}
		}
		if !is_embed && field_type_raw is Alias && field_type is Struct && flat.source_type_query(field_type.source_type_id()).has(.c_prefix) && field_type.source_type_id() in tc.c_typedef_structs && (tc.structs[field_type.source_type_id()] or {
			[]StructField{}
		}).len == 0 {
			tc.record_error_at(.assignment_mismatch, 'cannot use opaque C struct `${field_type_text}` as a non-reference struct field; use `&${field_type_text}` instead', field_id, tc.struct_field_type_pos(*field))
		}
		if field.children_count > 0
			&& tc.struct_field_type_uses_comptime_define(field_id, field_type_id) {
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
				&& field_type.source_type_id() == flat.source_type_bool {
				tc.record_warning_at(.assignment_mismatch, 'unnecessary default value `false`: struct fields are zeroed by default', default_id, default.pos)
			}
		}
		if field.children_count == 0 && field_type is FnType && !node.value.starts_with('C.')
			&& !tc.translated_files[tc.cur_file]
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
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		field_type := flat.source_type_name(field_type_id)
		expected := tc.parse_type(field_type_id)
		clean_expected := unalias_type(expected)
		default_node := tc.a.nodes[int(default_id)]
		if default_node.kind == .struct_init && default_node.value_text() in node.generic_params() {
			tc.record_error_at(.unknown_type, 'unknown struct `${default_node.value_text()}`', default_id,
				default_node.pos)
			tc.record_error_at(.assignment_mismatch, 'incompatible initializer for field `${field.value}`: expected `${field_type}`, not `void`', default_id,
				default_node.pos)
			continue
		}
		if (default_node.kind == .nil_literal || tc.expr_is_unsafe_nil(default_id))
			&& clean_expected !is Pointer {
			continue
		}
		if clean_expected is OptionType && (default_node.kind in [.none_expr, .nil_literal]
			|| tc.expr_is_unsafe_nil(default_id)) {
			continue
		}
		if clean_expected is Struct
			&& flat.source_type_syntax(clean_expected.source_type_id()).unqualified() == node_short_id {
			tc.record_error_at(.assignment_mismatch, 'field `${field.value}` is part of `${node.value}`, they can not both have the same type', tc.a.child(&node, i), tc.struct_field_type_pos(field))
			continue
		}
		if clean_expected is Array {
			elem_type := unalias_type(clean_expected.elem_type)
			if elem_type is Struct
				&& flat.source_type_syntax(elem_type.source_type_id()).unqualified() == node_short_id {
				type_pos := tc.struct_field_type_pos(field)
				tc.record_error_at(.assignment_mismatch, 'cannot initialize array of same struct type that is being defined (recursion detected)', tc.a.child(&node, i), token.new_span(field.pos.id, field.pos.offset, type_pos.end))
				continue
			}
		}
		if !node.value.starts_with('C.')
			&& tc.record_null_constant_reference_error(.assignment_mismatch, default_id, expected) {
			continue
		}
		tc.annotate_expected_expr(default_id, expected)
		tc.check_node_with_expected_context(default_id, expected)
		actual := tc.resolve_expr(default_id, expected)
		if !tc.node_is_in_translated_file(default_id) && type_is_unsigned_integer(expected)
			&& tc.expr_is_negative_integer_literal(default_id) {
			tc.record_error_at(.assignment_mismatch, 'cannot assign negative value to unsigned integer type', default_id, default_node.pos)
		}
		if clean_expected !is Pointer && clean_expected !is Interface
			&& unalias_type(actual) is Pointer {
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
		if expected.source_type_id() == flat.source_type_voidptr && unalias_type(actual).is_integer() {
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
		unknown_struct_init := if default_node.kind == .struct_init {
			init_type_id := default_node.source_value_id()
			init_type_id != checker_source_struct
				&& !flat.source_type_shape(init_type_id).has(.channel_type)
				&& init_type_id != checker_source_chan && !tc.type_name_known(init_type_id)
		} else {
			false
		}
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

fn (tc &TypeChecker) struct_field_type_uses_comptime_define(field_id flat.NodeId, field_type_id u32) bool {
	if flat.source_type_syntax(field_type_id).define_fragment() {
		return true
	}
	source := tc.source_text_for_node(field_id)
	define_pos := source.index('\$d(') or { return false }
	assign_pos := source.index('=') or { source.len }
	return define_pos < assign_pos
}

fn (tc &TypeChecker) struct_field_type_pos(field flat.Node) token.Pos {
	if !field.pos.is_valid() || field.typ == 0 {
		return field.pos
	}
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	start := int_max(0, int_min(field.pos.offset, source.len))
	end := int_max(start, int_min(field.pos.end, source.len))
	if start >= end {
		return field.pos
	}
	relative := source[start..end].index(field.typ_text()) or { return field.pos }
	type_start := start + relative
	return token.new_span(field.pos.id, type_start, type_start + field.typ_text().len)
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

fn (tc &TypeChecker) anonymous_struct_embedded_type_pos(field flat.Node) token.Pos {
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	start := int_min(int_max(field.pos.offset, 0), source.len)
	end := int_min(int_max(field.pos.end, start), source.len)
	// An embed inside an anonymous aggregate declaration is parsed without a source
	// type id, so `typ_text()` is empty and the spelling lives in `value`. An empty
	// needle matches at the end of the span, which moved the diagnostic past the
	// type and hid the struct declaration error.
	mut type_text := field.typ_text()
	if type_text.len == 0 {
		type_text = field.value
	}
	if type_text.len == 0 {
		return field.pos
	}
	if relative := source[start..end].last_index(type_text) {
		type_start := start + relative
		return token.new_span(field.pos.id, type_start, type_start + type_text.len)
	}
	return field.pos
}

fn (tc &TypeChecker) anonymous_struct_declaration_pos(node flat.Node) token.Pos {
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
	if node.kind != .enum_decl || node.enum_backing_type_id() == 0 {
		return
	}
	backing_id := trimmed_source_type_id(node.enum_backing_type_id())
	if backing_id == 0 {
		return
	}
	backing := flat.source_type_name(backing_id)
	backing_type := tc.parse_type(backing_id)
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
		min:     0
		max:     0
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
						min:         0
						max:         255
						has_min:     true
						has_max:     true
						bits:        bits
						is_unsigned: true
					}
				}
				16 {
					EnumBackingValueBounds{
						min:         0
						max:         65535
						has_min:     true
						has_max:     true
						bits:        bits
						is_unsigned: true
					}
				}
				else {
					EnumBackingValueBounds{
						min:         0
						has_min:     true
						bits:        bits
						is_unsigned: true
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
			min:         0
			has_min:     true
			bits:        64
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
	is_flag := node.typ_text() == 'flag'
	mut field_values := map[string]i64{}
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
						tc.record_error_at(.assignment_mismatch, 'the default value for an enum has to be an integer', expr_id, tc.a.nodes[int(expr_id)].pos)
					}
				}
			}
			mut resolving := map[string]bool{}
			if resolved := tc.comptime_static_enum_field_value(expr_id, tc.cur_module, node.source_value_id(), mut field_values, field_exprs, mut resolving) {
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
	if literal == '' {
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
	value, parse_error := strconv.common_parse_uint2(magnitude, util.v_literal_parse_base(magnitude), 64)
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
	if literal == '' || literal[0] == `-` {
		return false
	}
	mut magnitude := literal
	if literal[0] == `+` {
		magnitude = literal[1..]
	}
	value, parse_error := strconv.common_parse_uint2(magnitude, util.v_literal_parse_base(magnitude), 64)
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
	mut field_values := map[string]i64{}
	mut next_value := 0
	mut next_value_known := true
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.child_node(&node, i)
		if field.kind != .enum_field {
			continue
		}
		if tc.should_check_source_name(field_id)
			&& (!field.value.starts_with('_') || tc.relaxes_identifier_case(field_id))
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
		mut value_known := next_value_known
		mut value_overflows_backing := false
		mut value_pos := field.pos
		if field.children_count > 0 {
			value_known = true
			value_id := tc.a.child(field, 0)
			value_pos = tc.a.node(value_id).pos
			if field_ref := tc.find_enum_value_in_node(value_id, node.value) {
				referenced := tc.enum_field_name(tc.qualify_decl_name_id(node.source_value_id()), field_ref) or {
					field_ref
				}
				if referenced != field.value && !seen_names[referenced] {
					tc.record_error_at(.unknown_ident, '`${node.value}.${referenced}` should be declared before using it', value_id, value_pos)
					continue
				}
			}
			if node.enum_backing_type_id() != 0 {
				if bounds := enum_backing_value_bounds(tc.parse_type(node.enum_backing_type_id())) {
					if literal := tc.integer_literal_source(value_id) {
						value_overflows_backing = enum_backing_literal_overflows(literal.replace('_', ''), bounds)
					}
				}
			}
			// `.base + 2` refers to an earlier field of this enum, so its `.base`
			// is not an enum shorthand that needs an enum-typed operand.
			tc.enum_field_value_depth++
			tc.check_node(value_id)
			tc.enum_field_value_depth--
			value_node := tc.a.node(value_id)
			if value_node.kind == .enum_val {
				if duplicate_name {
					continue
				}
				referenced := tc.enum_field_name(tc.qualify_decl_name_id(node.source_value_id()), value_node.value) or {
					value_node.value
				}
				if referenced == field.value {
					if !allow_multiple {
						tc.record_error_with_details_at(.duplicate_decl, 'enum value `${field.value}` is not allowed to reference itself', field_id, value_pos, [
							'use `@[_allow_multiple_values]` attribute to allow multiple enum values. Use only when needed',
						])
					}
					continue
				}
				if !seen_names[referenced] {
					tc.record_error_at(.unknown_ident, '`${node.value}.${value_node.value}` should be declared before using it', field_id, value_pos)
					continue
				}
			}
			value_type := tc.resolve_type(value_id)
			if value_type is Unknown {
				value_known = false
			} else if !value_type.is_integer() {
				if node.generic_params().len == 0 {
					tc.record_error_at(.assignment_mismatch, 'the default value for an enum has to be an integer', value_id, value_pos)
				}
				value_known = false
			}
			mut resolving := map[string]bool{}
			if tc.node_contains_runtime_call(value_id) {
				tc.record_error_at(.assignment_mismatch, 'the default value for an enum has to be an integer', value_id, value_pos)
				value_known = false
			} else {
				value = tc.comptime_static_enum_field_value(value_id, tc.cur_module, node.source_value_id(), mut field_values, field_exprs, mut resolving) or {
					value_known = false
					value
				}
				if !value_known {
					tc.check_symbolic_enum_exprs(value_id, node.source_value_id(), mut field_values, field_exprs)
				}
			}
			if node.typ_text() == 'flag' && !value_known && value_type.is_integer()
				&& !tc.node_contains_runtime_call(value_id) {
				tc.record_error_at(.assignment_mismatch, 'flag enum values must be known at compile time', value_id, value_pos)
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
		next_value_known = value_known
	}
}

// Calls and powers have no C integer-constant-expression equivalent. Symbolic
// arithmetic can still contain independent calls and powers that V can evaluate.
fn (mut tc TypeChecker) check_symbolic_enum_exprs(id flat.NodeId, enum_name u32, mut field_values map[string]i64, field_exprs map[string]flat.NodeId) {
	node := tc.a.node(id)
	if node.kind == .call || (node.kind == .infix && node.op == .power) {
		mut resolving := map[string]bool{}
		if _ := tc.comptime_static_enum_field_value(id, tc.cur_module, enum_name, mut field_values, field_exprs, mut resolving) {
			return
		}
		message := if node.kind == .call {
			'enum function calls must be known at compile time'
		} else {
			'enum powers must be known at compile time'
		}
		tc.record_error_at(.assignment_mismatch, message, id, node.pos)
		return
	}
	for i in 0 .. node.children_count {
		tc.check_symbolic_enum_exprs(tc.a.child(node, i), enum_name, mut field_values, field_exprs)
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
		if decl := tc.comptime_static_enum_helper_decl(id, tc.cur_module) {
			if tc.declaration_has_attribute(flat.NodeId(decl.idx), 'comptime') {
				for i in 1 .. node.children_count {
					if tc.node_contains_runtime_call(tc.a.child(&node, i)) {
						return true
					}
				}
				return false
			}
		}
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
		if attribute_name_is(raw, name) {
			return true
		}
	}
	return false
}

// attribute_name_is reports whether `raw.all_before(':').trim_space() == name`,
// without the substrings.
@[direct_array_access]
fn attribute_name_is(raw string, name string) bool {
	mut end := raw.index_u8(`:`)
	if end < 0 {
		end = raw.len
	}
	mut start := 0
	for start < end && is_trim_space_byte(raw[start]) {
		start++
	}
	for end > start && is_trim_space_byte(raw[end - 1]) {
		end--
	}
	if end - start != name.len {
		return false
	}
	for i in 0 .. name.len {
		if raw[start + i] != name[i] {
			return false
		}
	}
	return true
}

// autofree_enabled reports whether compatibility autofree lowering is active.
pub fn (tc &TypeChecker) autofree_enabled() bool {
	return tc.autofree_mode
}

// struct_module_for_type returns the module that declared the named struct.
pub fn (tc &TypeChecker) struct_module_for_type(name_id u32) string {
	syntax := flat.source_type_syntax(name_id)
	candidate_id := if syntax.application_valid() { syntax.application_base() } else { name_id }
	if module_name := tc.struct_modules[candidate_id] {
		return module_name
	}
	if flat.source_type_shape(candidate_id).has(.dotted) {
		return flat.source_type_name(flat.source_type_syntax(candidate_id).qualifier())
	}
	return ''
}

// type_has_declaration_attribute reports whether the declaration of `typ` has an attribute named `name`.
pub fn (tc &TypeChecker) type_has_declaration_attribute(typ &Type, name string) bool {
	clean := unalias_type(unwrap_pointer(typ))
	type_name_id := strip_generic_args_id(clean.source_type_id())
	if type_name_id == 0 {
		return false
	}
	syntax := flat.source_type_syntax(type_name_id)
	declaration_name_id := if syntax.application_valid() {
		syntax.application_base()
	} else {
		type_name_id
	}
	// Semantic type names already carry their declaring module. The short-name
	// index also contains unrelated declarations from imported modules.
	index := tc.first_type_declaration_ids[declaration_name_id] or { return false }
	return tc.declaration_has_attribute(flat.NodeId(index), name)
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
	type_text_source_id := tc.current_fn_param_type_text(name) or { return none }
	if !trimmed_source_type_shape(type_text_source_id).has(.pointer) {
		return none
	}
	param_type := tc.parse_type(type_text_source_id)
	clean := unalias_type(unwrap_pointer(param_type))
	if clean !is Struct {
		return none
	}
	struct_type := clean as Struct
	if tc.type_has_declaration_attribute(Type(struct_type), 'heap') {
		return none
	}
	return flat.source_type_name(flat.source_type_syntax(struct_type.source_type_id()).unqualified())
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

// expr_is_mut_struct_param reports whether `id` names a `mut` parameter of a struct
// type. Like a `mut` receiver, it is a reference in the generated C, so it can be
// compared with `nil` or cast to an integer address, as in V1.
fn (tc &TypeChecker) expr_is_mut_struct_param(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	// Parentheses and blocks do not change what the parameter is: `u64((h))` and
	// `unsafe { h }` are the reference too, as in mut_param_expr_base.
	if node.kind in [.block, .expr_stmt, .paren] && node.children_count > 0 {
		child_idx := if node.kind == .block { node.children_count - 1 } else { 0 }
		return tc.expr_is_mut_struct_param(tc.a.child(node, child_idx))
	}
	if node.kind != .ident || node.value !in tc.fn_context.mut_param_owners {
		return false
	}
	base := tc.fn_context.mut_param_base_types[node.value] or { return false }
	return tc.mut_param_binding_matches_lvalue(node.value) && struct_type_from_type(base) != none
}

fn (mut tc TypeChecker) record_non_heap_pointer_param_escape(id flat.NodeId) bool {
	if tc.unsafe_depth > 0 || !tc.valid_node_id(id) || tc.node_is_in_translated_file(id) {
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

fn (tc &TypeChecker) type_declaration_before(node_id flat.NodeId, name string) ?flat.NodeId {
	current_name := qualify_decl_name_in_module(name, tc.cur_module)
	if tc.first_type_declaration_ids.len > 0 {
		// The index is keyed by interned declaration spellings.
		current_id := flat.source_type_existing_id(current_name) or { return none }
		index := tc.first_type_declaration_ids[current_id] or { return none }
		return if index < int(node_id) { flat.NodeId(index) } else { none }
	}
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
			return flat.NodeId(idx)
		}
	}
	return none
}

fn (mut tc TypeChecker) check_type_declaration_conflict(node_id flat.NodeId, node flat.Node) {
	if node.value.starts_with('C.') {
		return
	}
	name_pos := tc.type_declaration_name_pos(node_id)
	tc.check_generated_type_name(node_id, node, name_pos)
	if node.value == 'IError' && tc.cur_module != 'builtin'
		&& node.kind in [.struct_decl, .interface_decl] {
		kind := if node.kind == .struct_decl { 'struct' } else { 'interface' }
		tc.record_error_at(.duplicate_decl, 'cannot register ${kind} `IError`, it is builtin interface type', node_id, name_pos)
		return
	}
	if _ := tc.selective_import_text_candidates(node.value) {
		kind := match node.kind {
			.struct_decl { 'struct' }
			.interface_decl { 'interface' }
			.enum_decl { 'enum' }
			else { 'alias' }
		}
		tc.record_error_at(.duplicate_decl, 'cannot register ${kind} `${node.value}`, this type was already imported', node_id, name_pos)
		return
	}
	previous_id := tc.type_declaration_before(node_id, node.value) or { flat.NodeId(-1) }
	has_previous := int(previous_id) >= 0
	is_builtin_collision := tc.cur_module in ['', 'main'] && is_builtin_type_name(node.value)
	if !has_previous && !is_builtin_collision {
		return
	}
	is_fn_alias := node.kind == .type_decl && node.children_count == 0
		&& flat.source_type_syntax(node.typ).fn_prefix()
	kind := match node.kind {
		.struct_decl { 'struct' }
		.interface_decl { 'interface' }
		.enum_decl { 'enum' }
		else {
			if is_fn_alias {
				'fn'
			} else if node.children_count > 0 || flat.source_type_syntax(node.typ).union_parts().len > 1 {
				'sum type'
			} else {
				'alias'
			}
		}
	}
	name := if is_fn_alias { tc.qualify_name(node.value) } else { node.value }
	pos := if node.kind == .type_decl && !is_fn_alias && !is_builtin_collision {
		tc.declaration_keyword_name_pos(node_id, 'type')
	} else if node.kind == .enum_decl && !is_builtin_collision {
		tc.enum_declaration_diagnostic_pos(node_id)
	} else {
		name_pos
	}
	message := 'cannot register ${kind} `${name}`, another type with this name exists'
	if node.kind == .struct_decl && has_previous {
		previous_node := tc.a.node(previous_id)
		mut previous_is_builtin := false
		if previous_file := tc.a.source_files[previous_node.pos.id] {
			previous_is_builtin = previous_file.name.replace('\\', '/').contains('vlib/builtin/')
		}
		previous_pos := if previous_is_builtin {
			tc.type_declaration_name_pos(previous_id)
		} else {
			token.new_span(previous_node.pos.id, previous_node.pos.offset, previous_node.pos.offset + 1)
		}
		detail := compiler_errors.formatted_error('details:', 'another declaration was found here',
			tc.a, previous_id, previous_pos)
		tc.record_error_with_details_at(.duplicate_decl, message, node_id, pos, [detail])
		return
	}
	tc.record_error_at(.duplicate_decl, message, node_id, pos)
}

fn (tc &TypeChecker) type_declaration_name_pos(node_id flat.NodeId) token.Pos {
	if !tc.valid_node_id(node_id) {
		return token.Pos{}
	}
	node := tc.a.node(node_id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	name := node.value.all_after_last('.')
	start := int_max(0, node.pos.offset)
	end := int_min(source.len, node.pos.end)
	if name.len == 0 {
		return node.pos
	}
	if node.kind == .type_decl {
		for offset := int_min(start - 1, source.len - name.len); offset >= 0; offset-- {
			if identifier_word_match_at(source, name, offset) {
				return token.new_span(node.pos.id, offset, offset + name.len)
			}
		}
		return node.pos
	}
	if start >= end || name.len > end - start {
		return node.pos
	}
	for offset in start .. end - name.len + 1 {
		if identifier_word_match_at(source, name, offset) {
			return token.new_span(node.pos.id, offset, offset + name.len)
		}
	}
	return node.pos
}

fn qualify_decl_name_in_module(name string, module_name string) string {
	if module_name == '' || module_name == 'main' || module_name == 'builtin'
		|| name.starts_with('${module_name}.') || name.starts_with('C.')
		|| name.starts_with('JS.') {
		return name
	}
	return '${module_name}.${name}'
}

// qualify_decl_name_in_module_id is the spelling-ID twin of qualify_decl_name_in_module.
fn qualify_decl_name_in_module_id(name_id u32, module_id u32) u32 {
	query := flat.source_type_query(name_id)
	if module_id == 0 || module_id == checker_source_main_module || module_id == checker_source_builtin
		|| flat.source_type_has_qualified_prefix(name_id, module_id) || query.has(.c_prefix)
		|| query.has(.js_prefix) {
		return name_id
	}
	return flat.source_type_composition_two(.qualified, module_id, name_id)
}

// qualify_decl_name_in_module_existing_id is qualify_decl_name_in_module_id for
// a probe: a qualified spelling that was never interned names no declaration,
// so it is looked up instead of interned.
fn qualify_decl_name_in_module_existing_id(name_id u32, module_name string) ?u32 {
	if module_name == '' || module_name == 'main' || module_name == 'builtin' {
		return name_id
	}
	query := flat.source_type_query(name_id)
	if query.has(.c_prefix) || query.has(.js_prefix) {
		return name_id
	}
	name := flat.source_type_name(name_id)
	if name.starts_with('${module_name}.') {
		return name_id
	}
	return flat.source_type_existing_id('${module_name}.${name}')
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
		if field.value == tc.cur_module && tc.cur_module !in ['', 'main']
			&& !tc.current_file_uses_nested_module_path() {
			tc.record_error_at(.duplicate_decl, 'duplicate of a module name `${qname}`', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		if field.value != '_' && tc.generated_files.len > 0
			&& tc.should_check_source_name(field_id)
			&& tc.type_name_known_in_current_module(field.source_value_id()) {
			tc.record_error_at(.duplicate_decl, 'const `${field.value}` has the same name as a type', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		if field.value == '_' {
			tc.record_error_at(.duplicate_decl, 'cannot use `_` as a const name', field_id, tc.node_value_diagnostic_pos(field_id))
		} else if tc.relaxes_identifier_case(field_id) {
			if tc.should_check_source_name(field_id) && !field.value.starts_with('C.') {
				tc.check_generated_value_name(field_id, field.value, 'const name', tc.node_value_diagnostic_pos(field_id))
			}
		} else if tc.should_check_source_name(field_id) && !field.value.starts_with('C.')
			&& field.value != field.value.to_lower() {
			tc.record_error_at(.duplicate_decl, 'const names cannot contain uppercase letters, use snake_case instead', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		if tc.has_active_import(field.value) {
			tc.record_error_at(.duplicate_decl, 'const `${field.value}` conflicts with imported module `${field.value}`', field_id, tc.node_value_diagnostic_pos(field_id))
		}
		expr_id := tc.a.child(field, 0)
		if cycle_id := tc.find_const_self_reference(expr_id, field.value) {
			if tc.addressed_fixed_array_const_self_reference(qname, expr_id, field.value) {
				tc.check_node(expr_id)
				continue
			}
			tc.const_types[qname] = builtin_void_type
			tc.record_error(.unknown_ident, 'cycle in constant `${field.value}`', cycle_id)
		}
		const_type := tc.const_types[qname] or { builtin_void_type }
		if field.typ != 0 || !tc.check_known_numeric_const_initializer(expr_id, const_type) {
			tc.check_node(expr_id)
		}
		if tc.requires_comptime_const(expr_id) {
			if value := tc.eval_comptime_const(expr_id) {
				tc.comptime_const_values[qname] = value
			} else {
				tc.record_error_at(.compile_error, 'cannot evaluate const `${field.value}`: ${err}', field_id, field.pos)
			}
		}
		if tc.resolve_type(expr_id) is MultiReturn {
			tc.record_error_at(.assignment_mismatch, 'const declarations do not support multiple return values yet', expr_id, tc.a.nodes[int(expr_id)].pos)
		}
		if field.typ == 0 && tc.resolve_type(expr_id).source_type_id() == flat.source_type_int
			&& tc.implicit_int_literal_overflows(expr_id) {
			tc.record_error_at(.assignment_mismatch, 'overflow in implicit type `int`, use explicit type casting instead', expr_id, tc.a.nodes[int(expr_id)].pos)
		}
	}
}

fn (tc &TypeChecker) addressed_fixed_array_const_self_reference(qname string, expr_id flat.NodeId, name string) bool {
	if tc.const_types[qname] or { builtin_void_type } !is ArrayFixed {
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
	if node.kind == .ident && node.value_text() == name {
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
		if field.kind != .field_decl {
			continue
		}
		if 'const' !in field.generic_params() {
			if field.children_count > 0 {
				tc.check_node(tc.a.child(field, 0))
			}
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

fn (mut tc TypeChecker) check_global_decl_semantics(node_id flat.NodeId, node flat.Node) {
	tc.check_decl_type_strings(node_id, node)
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.node(field_id)
		if field.kind != .field_decl || field.value.len == 0 || field.value.starts_with('C.') {
			continue
		}
		if field.typ != 0 && field.children_count > 0 {
			tc.record_null_constant_reference_error(.assignment_mismatch, tc.a.child(field, 0),
				tc.parse_type(field.typ))
		}
		pos := tc.node_value_diagnostic_pos(field_id)
		if field.value == '_' {
			tc.record_error_at(.duplicate_decl, 'cannot use `_` as a global name', field_id,
				pos)
		} else if tc.should_check_source_name(field_id) && !snake_case_name_is_valid(field.value) {
			tc.check_snake_case_name(field_id, field.value, 'global name', pos)
		}
		if tc.qualify_name(field.value) in tc.const_types {
			tc.record_error_at(.duplicate_decl, 'duplicate global and const `${field.value}`',
				field_id, pos)
		}
		if field.value == 'main' && tc.cur_module in ['', 'main'] {
			tc.record_error_at(.duplicate_decl, 'the `main` function is the program entry point, cannot redefine it',
				field_id, pos)
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
		'thread',
		'uint',
		'isize',
		'voidptr',
	]
}

fn (tc &TypeChecker) find_const_self_reference(id flat.NodeId, name string) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value_text() == name {
		return id
	}
	mut start := 0
	if node.kind == .call && node.children_count > 0 {
		mut callee := tc.a.child_node(&node, 0)
		if callee.kind == .index && callee.children_count > 0 {
			callee = tc.a.child_node(callee, 0)
		}
		if callee.kind == .ident && callee.value == name
			&& tc.fn_signature_known(tc.qualify_name(name)) {
			// A direct function call does not read the same-named constant.
			start = 1
		}
	}
	for i in start .. node.children_count {
		if found := tc.find_const_self_reference(tc.a.child(&node, i), name) {
			return found
		}
	}
	return none
}

fn (tc &TypeChecker) find_ident_in_node(id flat.NodeId, name string) ?flat.NodeId {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value_text() == name {
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

fn (tc &TypeChecker) fn_decl_is_noreturn(module_name string, name string) bool {
	if checker_qualified_fn_name(module_name, name) in tc.a.noreturn_fns {
		return true
	}
	short_module := module_name.all_after_last('.')
	return short_module != module_name
		&& checker_qualified_fn_name(short_module, name) in tc.a.noreturn_fns
}

fn (mut tc TypeChecker) check_noreturn_fn_semantics(id flat.NodeId, node flat.Node, module_name string) {
	if !tc.fn_decl_is_noreturn(module_name, node.value) {
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
		if child.kind != .expr_stmt {
			previous_never_returns = false
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

fn type_allows_implicit_return(typ &Type) bool {
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
		if callee.value in ['panic', 'exit'] && (tc.no_return_builtin_is_shadowed(callee.value)
			|| tc.module_declares_fn(tc.cur_module, callee.value)) {
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

pub fn (tc &TypeChecker) module_declares_fn(module_name string, name string) bool {
	if module_name in ['', 'main', 'builtin'] {
		return false
	}
	visibility := tc.declaration_visibility[checker_qualified_fn_name(module_name, name)] or {
		return false
	}
	return visibility.kind == .fn_decl
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
	// Keep the original map allocation aside: the clone can belong to a
	// disposable transform arena and must not escape into the master checker.
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
	mut enum_name := u32(0)
	if subject_type is Enum {
		enum_name = subject_type.source_type_id()
	} else {
		return false
	}
	// A `[flag]` enum value can hold combined or zero bits (`.read | .write`, `0`) that
	// no single-field branch covers, so listing every field is NOT exhaustive — such a
	// match needs an explicit `else`.
	if enum_name in tc.flag_enums {
		return false
	}
	all_fields := tc.comptime_static_enum_decl_value_cases(enum_name).map(it.name)
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
			if field := tc.match_enum_condition_field(cond, enum_name) {
				covered[field] = true
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
	if body_start < node.children_count {
		tail_id := tc.a.child(node, int(node.children_count) - 1)
		if tc.valid_node_id(tail_id) && tc.a.nodes[int(tail_id)].kind == .return_stmt {
			return true
		}
	}
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
	pattern_source_id := tc.match_type_pattern(cond) or { return tc.match_branch_definitely_returns(branch) }
	smartcast_type := if subject_type is SumType {
		tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_source_id) or {
			return tc.match_branch_definitely_returns(branch)
		}
	} else if is_ierror_type(subject_type) {
		tc.resolve_ierror_match_pattern(pattern_source_id) or {
			return tc.match_branch_definitely_returns(branch)
		}
	} else if subject_type is Interface {
		tc.resolve_interface_match_pattern(pattern_source_id) or {
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
		enum_name := tc.resolve_enum_name(subject_type.source_type_id()) or { subject_type.source_type_id() }
		if subject_type.is_flag || enum_name in tc.flag_enums {
			return false
		}
		fields := tc.comptime_static_enum_decl_value_cases(enum_name).map(it.name)
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
	mut sum_name_id := u32(0)
	if subject_type is SumType {
		sum_name_id = subject_type.source_type_id()
	} else {
		return false
	}
	base_id := tc.sum_base_name(sum_name_id)
	variants := tc.sum_types.type_ids(base_id) or { return false }
	if variants.len == 0 {
		return false
	}
	mut covered := map[u32]bool{}
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
			pattern_source_id := tc.match_type_pattern(cond) or { return false }
			qpattern := tc.qualify_type_name(pattern_source_id)
			mut matched := false
			for variant_id in variants {
				if tc.generic_type_name_matches(variant_id, pattern_source_id)
					|| tc.generic_type_name_matches(variant_id, qpattern) {
					covered[variant_id] = true
					matched = true
				}
			}
			if matched {
				continue
			}
			// Short-name fallback: only sound when exactly one variant carries
			// this short name — for `type S = a.Foo | b.Foo`, a branch matching
			// one `Foo` must not mark the other as covered.
			pattern_short := flat.source_type_syntax(pattern_source_id).unqualified()
			mut short_match := u32(0)
			mut short_count := 0
			for variant_id in variants {
				if flat.source_type_syntax(variant_id).unqualified() == pattern_short {
					short_count++
					short_match = variant_id
				}
			}
			if short_count == 1 {
				covered[short_match] = true
			}
		}
	}
	for variant_id in variants {
		if variant_id !in covered {
			return false
		}
	}
	return true
}

// unalias_type follows an alias chain to its underlying type, unwrapping each
// Alias to its base type until a non-alias type is reached and returned.
@[inline]
pub fn unalias_type(t &Type) &Type {
	mut current := t
	for current is Alias {
		current = current.base_type
	}
	return current
}

@[inline]
fn unalias_and_unwrap_pointer_type(t &Type) Type {
	mut cur := t
	for {
		match cur {
			Alias, Pointer { cur = cur.base_type }
			else { return cur }
		}
	}
	return cur
}

fn (tc &TypeChecker) match_enum_condition_field(cond &flat.Node, enum_name_id u32) ?string {
	match cond.kind {
		.enum_val {
			field := cond.value.all_after_last('.')
			if tc.enum_value_matches(cond.value, enum_name_id) {
				return tc.enum_field_name(enum_name_id, field)
			}
		}
		.selector {
			if typ := tc.enum_selector_type(cond) {
				if typ is Enum {
					cond_enum_name := tc.resolve_enum_name(typ.source_type_id()) or { typ.source_type_id() }
					if cond_enum_name == enum_name_id && tc.enum_has_field(enum_name_id, cond.value) {
						return tc.enum_field_name(enum_name_id, cond.value)
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
	typ           u32
	unaliased_typ u32
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
	value         i64
	has_value     bool
	typ           u32
	return_type   u32
	is_pub        bool
	has_is_pub    bool
	arg           string
	has_arg       bool
	has_attr_meta bool
	attr_kind     int
	param_names   []string
	param_types   flat.TypeTextList
	param_is_mut  []bool
	// param_is_mut_ref marks explicit `mut param &T` parameters.
	param_is_mut_ref []bool
	runtime_dispatch bool
	// value_from_c marks an enum member whose value V can not evaluate, because it, or
	// the explicit value it follows, comes from C (`a = C.FOO`). `value` is a guess then.
	value_from_c bool
	// Schema fields shadow VAttribute metadata only after this case is narrowed.
	attr_schema_narrowed bool
	// Process-owned interned schema identity; 0 represents legacy attributes.
	attr_schema_id u32
}

// Each scope owns an immutable set. Forks may share it; branch checks replace
// and restore the pointer instead of mutating a parent's narrowed schema set.
struct ComptimeAttributeNarrowState {
	names []string
}

struct ComptimeStaticAttributeCondNarrowing {
	decided bool
	value   bool
mut:
	then_branch bool
	else_branch bool
}

struct ComptimeStaticValueCases {
	known bool
	cases []ComptimeStaticValueCase
}

struct ComptimeStaticFieldDeclMeta {
	is_mut   bool
	is_pub   bool
	is_embed bool
	raw_typ  u32
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

fn (mut tc TypeChecker) check_comptime_for_global_shadowing(id flat.NodeId, node flat.Node) {
	parts := node.value.split('|')
	if parts.len != 2 || parts[0].len == 0 || !tc.global_names[parts[0]] {
		return
	}
	tc.record_global_shadow_error_at(id, parts[0], tc.comptime_for_variable_pos(node, parts[0]))
}

fn (mut tc TypeChecker) check_comptime_for_members(_id flat.NodeId, node flat.Node) {
	parts := node.value.split('|')
	if parts.len != 2 || parts[0].len == 0 || node.children_count == 0 {
		return
	}
	body_id := tc.a.child(&node, 0)
	if parts[1] == 'strings' && node.children_count == 2 {
		source_id := tc.a.child(&node, 1)
		source := tc.a.node(source_id)
		callee := if source.kind == .call && source.children_count > 0 {
			tc.a.child_node(source, 0)
		} else {
			&flat.Node{}
		}
		if callee.kind != .selector || callee.value !in ['split', 'split_any', 'fields']
			|| !tc.comptime_initializer_is_static(source_id) {
			tc.record_error_at(.condition_mismatch, '`\$for` string source must use `split`, `split_any` or `fields` on a compile-time-known string', source_id, source.pos)
			return
		}
		tc.push_scope()
		tc.cur_scope.insert(parts[0], string_)
		tc.check_comptime_static_body(body_id, parts[0], 'strings', ComptimeStaticFieldCases{}, ComptimeStaticValueCases{})
		tc.pop_scope()
		return
	}
	if _ := tc.comptime_struct_update_id(body_id) {
		return
	}
	if _ := tc.comptime_struct_update_source_pos(node) {
		return
	}
	if !is_regular_v_test_file(tc.cur_file)
		&& !tc.comptime_subtree_references_var(body_id, parts[0]) {
		tc.record_warning_at(.unknown_ident, 'unused variable: `${parts[0]}`', _id, tc.comptime_for_variable_pos(node, parts[0]))
	}
	if tc.check_comptime_for_source_type(_id, node) {
		return
	}
	source_is_type := tc.type_name_known(tc.instance_type_text(node.typ))
	source_is_value := if tc.cur_scope == unsafe { nil } {
		false
	} else {
		tc.cur_scope.lookup(node.typ_text()) != none
	}
	source_is_fn := node.typ_text() in tc.fn_ret_types || tc.qualify_fn_name(node.typ_text()) in tc.fn_ret_types
	if parts[1] == 'params' && source_is_type && !source_is_fn {
		tc.record_error_at(.unknown_type, 'iterating over `.params` is supported only for functions, and `${node.typ_text()}` is not a function', _id, tc.comptime_for_source_pos(node))
		return
	}
	if !source_is_type && !source_is_value && !source_is_fn && node.typ != 0
		&& flat.source_type_syntax(node.typ).first_capital() {
		tc.record_error_at(.unknown_type, '\$for expects a type name or variable name to be used here, but ${node.typ_text()} is not a type or variable name', _id, tc.comptime_for_source_pos(node))
		return
	}
	current_fn_id := flat.NodeId(tc.fn_context.node_id)
	defer {
		if tc.valid_node_id(current_fn_id) {
			tc.check_comptime_selectors_outside_loop(current_fn_id, _id, parts[0])
		}
	}
	tc.push_scope()
	tc.cur_scope.insert(parts[0], *tc.parse_type(comptime_static_metadata_type_id(parts[1])))
	saved_attribute_state := tc.comptime_attr_narrow_state
	if parts[1] == 'attributes' {
		tc.comptime_attr_narrow_state = &ComptimeAttributeNarrowState{ names: tc.comptime_attribute_schema_universe() }
	}
	defer {
		tc.comptime_attr_narrow_state = saved_attribute_state
	}
	defer {
		tc.pop_scope()
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
	deferred_cases := if parts[1] in ['methods', 'params', 'attributes'] {
		tc.comptime_static_deferred_cases(node.typ, parts[1])
	} else {
		ComptimeStaticValueCases{}
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
			tc.check_comptime_attribute_members(body_id, parts[0], deferred_cases)
		}
		else {}
	}

	// Method and parameter loops are resolved to concrete declarations by the transformer.
	// Their bodies can also reference metadata from an enclosing reflection loop, which does
	// not exist as a runtime local while checking the original generic body.
	if parts[1] in ['methods', 'params', 'attributes'] {
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
	for id in tc.preflight_nodes(.comptime_for) {
		node := tc.a.nodes[id]
		if node.children_count == 0 {
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
	if parts.len != 2 || node.typ == 0 {
		return false
	}
	mut parent_id := tc.direct_parent_id(id)
	for tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .comptime_for
			&& comptime_for_declares_var_in_value(parent.value, node.typ_text()) {
			return false
		}
		next_parent := tc.direct_parent_id(parent_id)
		if next_parent == parent_id {
			break
		}
		parent_id = next_parent
	}
	source_type := if typ := tc.cur_scope.lookup(node.typ_text()) {
		*unalias_type(unwrap_pointer(typ))
	} else {
		*unalias_type(unwrap_pointer(tc.parse_type(node.typ)))
	}
	mut message := ''
	if parts[1] == 'fields' && source_type !is Unknown && source_type !is Void
		&& source_type !is Struct && source_type !is Interface {
		message = 'iterating over .fields is supported only for structs and interfaces, and ${node.typ_text()} is neither'
	} else if parts[1] == 'values' && source_type !is Unknown && source_type !is Void
		&& source_type !is Enum {
		message = 'iterating over .values is supported only for enums, and ${node.typ_text()} is not an enum'
	} else if parts[1] == 'variants' && source_type !is Unknown && source_type !is Void
		&& source_type !is SumType {
		message = '${flat.source_type_name(flat.source_type_syntax(source_type.source_type_id()).unqualified())} is not Sum type to use with .variants'
	}
	if message == '' {
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
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	saved_scope := tc.cur_scope
	tc.cur_scope = tc.file_scope
	defer {
		tc.cur_file = saved_file
		tc.cur_module = saved_module
		tc.cur_scope = saved_scope
	}
	for index in tc.preflight_nodes(.comptime_for) {
		node := tc.a.nodes[index]
		file := tc.a.source_files[node.pos.id] or { continue }
		tc.enter_file(file.name)
		// Parameter and local sources need the enclosing function's lexical
		// scope. The semantic pass checks them once that scope is available.
		if !tc.type_name_known(node.typ) {
			continue
		}
		tc.check_comptime_for_source_type(flat.NodeId(index), node)
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
	needle := ' in ${node.typ_text()}'
	if relative := source[start..end].index(needle) {
		source_start := start + relative + ' in '.len
		return token.new_span(node.pos.id, source_start, source_start + node.typ_text().len)
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
				&& !tc.comptime_reflection_condition_type_known(flat.source_type_id(typ)) {
				tc.record_error_at(.unknown_type, 'unknown type `${typ}`', id, tc.comptime_condition_type_pos(*node, typ))
			}
			offset = int_max(type_end, type_start + 1)
		}
	}
	for i in 0 .. node.children_count {
		tc.check_comptime_reflection_condition_types(tc.a.child(node, i), loop_var)
	}
}

const checker_source_fn_keyword = flat.source_type_id('fn')

fn (tc &TypeChecker) comptime_reflection_condition_type_known(typ_id u32) bool {
	mut clean_id := trimmed_source_type_id(typ_id)
	mut shape := flat.source_type_shape(clean_id)
	if clean_id == checker_source_fn_keyword || shape.has(.function_compact)
		|| shape.has(.function_spaced) {
		return true
	}
	for shape.has(.pointer) || shape.has(.optional) {
		clean_id = trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
		shape = flat.source_type_shape(clean_id)
	}
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.array_type) {
		return tc.comptime_reflection_condition_type_known(syntax.raw_base())
	}
	if shape.has(.leading_bracket) {
		return syntax.bracket_closed() && syntax.bracket_base() != 0
			&& tc.comptime_reflection_condition_type_known(syntax.bracket_base())
	}
	if shape.has(.map_type) {
		return syntax.bracket_closed() && syntax.bracket_base() != 0
			&& tc.comptime_reflection_condition_type_known(syntax.bracket_inner())
			&& tc.comptime_reflection_condition_type_known(syntax.bracket_base())
	}
	return tc.type_name_known(clean_id)
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

// Attribute member validation walks every expression, including initializers and
// interpolation parts that the deferred semantic checker does not visit.
fn (mut tc TypeChecker) check_comptime_attribute_members(id flat.NodeId, var_name string, value_cases ComptimeStaticValueCases) {
	// Known declaration attributes do not narrow the loop variable. Track the
	// schema guard separately from the concrete cases used for reachability.
	narrow_schemas := tc.comptime_attribute_schema_universe()
	if !value_cases.known {
		tc.check_comptime_attribute_body(id, var_name, narrow_schemas, narrow_schemas)
		return
	}
	mut schemas := []string{}
	for item in value_cases.cases {
		name := flat.source_type_name(item.attr_schema_id)
		if name !in schemas {
			schemas << name
		}
	}
	tc.check_comptime_attribute_body(id, var_name, schemas, narrow_schemas)
}

fn (mut tc TypeChecker) check_comptime_attribute_body(id flat.NodeId, var_name string, schemas []string, narrow_schemas []string) {
	if !tc.valid_node_id(id) || schemas.len == 0 {
		return
	}
	node := tc.a.node(id)
	if node.kind == .comptime_for && node.value.all_before('|') == var_name {
		return
	}
	if node.kind == .comptime_if {
		// Schema fields in conditions need a separate transform/checker contract.
		// Keep this pass's accepted condition surface at the VAttribute members.
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
		for i in 0 .. int_min(2, node.children_count) {
			branch_schemas := tc.comptime_attribute_branch_schemas(node.value, var_name, schemas, i == 0)
			branch_narrow_schemas := tc.comptime_attribute_branch_schemas(node.value, var_name, narrow_schemas, i == 0)
			tc.check_comptime_attribute_body(tc.a.child(node, i), var_name, branch_schemas, branch_narrow_schemas)
		}
		return
	}
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			receiver := tc.a.child_node(callee, 0)
			if receiver.kind == .ident && receiver.value == var_name {
				// Method names are resolved by the semantic checker, not as fields.
				for i in 1 .. node.children_count {
					tc.check_comptime_attribute_body(tc.a.child(node, i), var_name, schemas, narrow_schemas)
				}
				return
			}
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident && base.value == var_name
			&& node.value !in comptime_attribute_members {
			member := node.value
			if !tc.comptime_attribute_has_member(member) {
				tc.record_error_at(.unknown_field, 'unknown field `${member}` from ${var_name}', id, node.pos)
			} else if '' in narrow_schemas {
				tc.record_error_at(.unknown_field, 'attribute field `${member}` requires narrowing with `$if ${var_name} is Schema`', id, node.pos)
			} else if _ := tc.comptime_attribute_member_type_for_schemas(member, schemas) {
				// Every possible schema exposes this field with the same type.
			} else {
				tc.record_error_at(.unknown_field, 'attribute field `${member}` is not available with one type on every narrowed schema', id, node.pos)
			}
		}
	}
	for i in 0 .. node.children_count {
		tc.check_comptime_attribute_body(tc.a.child(node, i), var_name, schemas, narrow_schemas)
	}
}

fn (tc &TypeChecker) comptime_attribute_schema_universe() []string {
	// The empty name represents legacy VAttribute instances, including attributes
	// on generic types whose concrete declaration is not known yet.
	mut names := ['']
	for _, schemas in tc.attribute_schemas {
		for schema in schemas {
			names << '${schema.module}.${schema.name}'
		}
	}
	return names
}

fn (tc &TypeChecker) comptime_attribute_has_member(member string) bool {
	for _, schemas in tc.attribute_schemas {
		for schema in schemas {
			for field in schema.fields {
				if field.name == member {
					return true
				}
			}
		}
	}
	return false
}

// comptime_attribute_union_member_type resolves fields from the current
// narrowed schemas. Multiple schemas must agree on the field's declared type.
fn (tc &TypeChecker) comptime_attribute_union_member_type(member string) ?Type {
	if isnil(tc.comptime_attr_narrow_state) {
		return none
	}
	return tc.comptime_attribute_member_type_for_schemas(member, tc.comptime_attr_narrow_state.names)
}

fn (tc &TypeChecker) comptime_attribute_narrowing_names() []string {
	if isnil(tc.comptime_attr_narrow_state) {
		return []string{}
	}
	return tc.comptime_attr_narrow_state.names
}

fn (tc &TypeChecker) comptime_attribute_member_type_for_schemas(member string, names []string) ?Type {
	if names.len == 0 || '' in names {
		return none
	}
	mut result := u32(0)
	for name in names {
		mut found := false
		for schema in tc.attribute_schemas[name.all_after_last('.')] or { []AttributeSchema{} } {
			if '${schema.module}.${schema.name}' != name {
				continue
			}
			for field in schema.fields {
				if field.name == member {
					if result != 0 && result != field.type_id {
						return none
					}
					result = field.type_id
					found = true
					break
				}
			}
		}
		if !found {
			return none
		}
	}
	if result == 0 {
		return none
	}
	return *tc.parse_type(result)
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

fn (tc &TypeChecker) comptime_static_deferred_cases(source_id u32, loop_kind string) ComptimeStaticValueCases {
	if loop_kind == 'methods' {
		return tc.comptime_static_method_cases(source_id)
	}
	// Parameter and attribute loops name a function or declaration, not a type.
	source := flat.source_type_name(source_id)
	if loop_kind == 'params' {
		return tc.comptime_static_param_cases(source)
	}
	if loop_kind == 'attributes' {
		return tc.comptime_static_attribute_cases(source)
	}
	return ComptimeStaticValueCases{}
}

fn (tc &TypeChecker) comptime_static_method_cases(source_id u32) ComptimeStaticValueCases {
	base_type := tc.instance_type_text(tc.comptime_static_for_base_type(source_id))
	clean_type_id := comptime_static_unwrap_type_id(base_type)
	syntax := flat.source_type_syntax(clean_type_id)
	is_generic := syntax.application_valid()
	generic_args_ids := syntax.application_args()
	if is_generic && !tc.generic_args_are_concrete(generic_args_ids) {
		return ComptimeStaticValueCases{}
	}
	lookup_type_id := if is_generic { syntax.application_base() } else { clean_type_id }
	// A forwarded main-owned type is locked as `main.Type` so an imported
	// helper cannot rebase it. The collected main symbol still has a bare key.
	canonical_lookup := if flat.source_type_query(lookup_type_id).has(.main_prefix) {
		tc.parse_type(lookup_type_id).source_type_id()
	} else {
		lookup_type_id
	}
	struct_name_id := tc.comptime_static_struct_name(canonical_lookup) or {
		return ComptimeStaticValueCases{}
	}
	generic_params := if is_generic {
		tc.struct_generic_params[struct_name_id] or {
			tc.struct_generic_params[flat.source_type_syntax(struct_name_id).unqualified()] or { []u32{} }
		}
	} else {
		[]u32{}
	}
	if is_generic && generic_params.len != generic_args_ids.len {
		return ComptimeStaticValueCases{}
	}
	mut wanted_module := tc.struct_modules[struct_name_id] or { '' }
	if wanted_module.len == 0 {
		if decl_file := tc.struct_files[struct_name_id] {
			wanted_module = tc.file_modules[decl_file] or { '' }
		}
	}
	if wanted_module.len == 0 {
		wanted_module = if flat.source_type_shape(struct_name_id).has(.dotted) {
			flat.source_type_name(flat.source_type_syntax(struct_name_id).qualifier())
		} else if tc.cur_module.len > 0 {
			tc.cur_module
		} else {
			'main'
		}
	}
	wanted_receiver := flat.source_type_name(flat.source_type_syntax(flat.source_type_syntax(struct_name_id).unqualified()).generic_base())
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
				mut param_types := []u32{}
				mut param_is_mut := []bool{}
				mut param_is_mut_ref := []bool{}
				for i in 1 .. candidate.children_count {
					param := tc.a.child_node(&candidate, i)
					if param.kind == .param {
						param_names << param.value
						param_types << subst_generic_text(param.typ, generic_args_ids, generic_params)
						param_is_mut << param.is_mut
						param_is_mut_ref << (param.is_mut && param.op == .amp
							&& flat.source_type_shape(param.typ).has(.pointer))
					}
				}
				return_type := subst_generic_text(if candidate.typ != 0 {
					candidate.typ
				} else {
					flat.source_type_void
				}, generic_args_ids, generic_params)
				cases << ComptimeStaticValueCase{
					name:             name
					location:         comptime_static_source_location(file_name, candidate.pos.offset, line_offsets_by_file[file_name],
						tc.a.source_files[candidate.pos.id] or { unsafe { nil } })
					typ:              comptime_static_method_type_text(param_types, return_type)
					return_type:      return_type
					is_pub:           candidate.op == .arrow
					has_is_pub:       true
					param_names:      param_names
					param_types:      flat.TypeTextList{ ids: param_types }
					param_is_mut:     param_is_mut
					param_is_mut_ref: param_is_mut_ref
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
			params := candidate.generic_params()
			mut cases := []ComptimeStaticValueCase{cap: params.len}
			kinds := if candidate.typ != 0 {
				candidate.typ_text().split(',')
			} else {
				[]string{}
			}
			mut consumed := []bool{len: params.len}
			for idx, raw in params {
				if consumed[idx] {
					continue
				}
				item := comptime_static_attribute_case(raw, if idx < kinds.len {
					kinds[idx].int()
				} else {
					0
				})
				if item.name.len == 0 {
					continue
				}
				mut schema_name := ''
				whole_string := item.attr_kind >= 0
					&& item.attr_kind & flat.attribute_kind_no_arg != 0
				if !whole_string {
					uses := tc.attribute_use_schemas([item.name], wanted.module_name)
					if uses.len > 0 {
						schema := uses[0].schema
						schema_name = '${schema.module}.${schema.name}'
						// Validated named/positional pieces belong to this one instance,
						// matching the transform's forward grouping and whole-string exclusion.
						for j in idx .. params.len {
							if consumed[j] || (j < kinds.len && kinds[j].int() >= 0
								&& kinds[j].int() & flat.attribute_kind_no_arg != 0) {
								continue
							}
							base := params[j].all_before(':').trim_space()
							if base == item.name || base.starts_with(item.name + '_') {
								consumed[j] = true
							}
						}
					}
				}
				cases << ComptimeStaticValueCase{
					...item
					attr_schema_id: flat.source_type_id(schema_name)
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

fn comptime_static_method_type_text(param_types []u32, return_type_id u32) u32 {
	if return_type_id == 0 || return_type_id == flat.source_type_void {
		return flat.source_type_composition(.void_function_type, param_types)
	}
	mut parts := param_types.clone()
	parts << return_type_id
	return flat.source_type_composition(.function_type, parts)
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

// comptime_static_source_location returns the `file:line:column` of a declaration,
// following the `#line` directives of its source `file`, when it has any.
fn comptime_static_source_location(path string, encoded_offset int, line_offsets []int, file &token.File) string {
	if path == '' || encoded_offset <= 0 || line_offsets.len == 0 {
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
	column := offset - line_offsets[line_index]
	if !isnil(file) && file.has_line_directives() {
		logical_file, logical_line := file.logical_line(line_index + 1)
		return '${logical_file}:${logical_line}:${column}'
	}
	return '${path}:${line_index + 1}:${column}'
}

fn comptime_static_attribute_case(raw string, kind int) ComptimeStaticValueCase {
	clean := trimmed_space(raw)
	colon := clean.index_u8(`:`)
	whole_string := kind >= 0 && kind & flat.attribute_kind_no_arg != 0
	has_arg := colon >= 0 && !whole_string
	mut name := if has_arg { trimmed_space(clean[..colon]) } else { clean }
	if whole_string {
		if kind & flat.attribute_kind_raw_string != 0 {
			name = name[2..name.len - 1]
		} else {
			name = comptime_static_unescape(name)
		}
	} else if name.len >= 2 && name[0] in [`'`, `"`] && name[name.len - 1] == name[0] {
		name = name[1..name.len - 1]
	}
	mut arg := if has_arg { trimmed_space(clean[colon + 1..]) } else { '' }
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

fn (tc &TypeChecker) comptime_deferred_decl_source(source string, allow_type bool) ?ComptimeDeferredDeclSource {
	clean := trimmed_space(source)
	if clean.len == 0 {
		return none
	}
	if !clean.contains('.') {
		if allow_type {
			if resolved_id := tc.resolve_selective_import_type_symbol_text_id(clean) {
				return comptime_deferred_qualified_source(flat.source_type_name(resolved_id))
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
		tc.check_comptime_for_global_shadowing(id, node)
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
	if node.kind == .expr_stmt && node.children_count == 1
		&& tc.expr_source_contains_embed_file(tc.a.child(&node, 0)) {
		tc.check_unused_expression_statement(id)
	}
	// A return that does not depend on the loop variable is checked as any other
	// return: its value has the same type for every member.
	if node.kind == .return_stmt && !tc.comptime_subtree_references_var(id, var_name) {
		tc.comptime_static_depth++
		tc.check_node(id)
		tc.comptime_static_depth--
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
		for i in 0 .. node.children_count {
			tc.check_generic_body_node_global_shadowing(tc.a.child(&node, i))
		}
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
	if node.kind == .for_stmt {
		tc.push_scope()
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases, value_cases)
		}
		tc.pop_scope()
		return
	}
	if node.kind == .for_in_stmt {
		header := node.value.int()
		if header < 3 || node.children_count < 3 {
			return
		}
		tc.push_scope()
		key_id := tc.a.child(&node, 0)
		val_id := tc.a.child(&node, 1)
		mut key_type := unknown_type('runtime loop variable in static comptime body')
		mut value_type := key_type
		// Metadata collections such as field.attrs have a known element type even
		// before the enclosing reflection loop is unrolled against a concrete type.
		if container_type := tc.comptime_static_metadata_expr_type(tc.a.child(&node, 2), var_name, loop_kind) {
			clean := unalias_type(unwrap_pointer(container_type))
			if clean is Array {
				value_type = clean.elem_type
				key_type = if int(val_id) >= 0 { builtin_int_type } else { value_type }
			}
		} else if container_type := tc.comptime_static_reflected_iterable_type(tc.a.child(&node,
			2), var_name, field_cases)
		{
			// `for v in obj.$(field.name)` iterates the reflected field itself; when
			// every field the loop can reach has the same type, so do its bindings.
			has_val := int(val_id) >= 0
			mut elem_type := Type(Unknown{})
			mut index_type := Type(int_)
			clean := unalias_type(unwrap_pointer(container_type))
			if clean is Array {
				elem_type = clean.elem_type
			} else if clean is ArrayFixed {
				elem_type = clean.elem_type
			} else if clean is Map {
				elem_type = clean.value_type
				index_type = clean.key_type
			} else if clean is String {
				elem_type = Type(u8_)
			}
			if elem_type !is Unknown {
				value_type = for_in_ref_binding_type(elem_type, node.op == .amp)
				key_type = if has_val { index_type } else { value_type }
			}
		}
		if node.op == .amp && value_type !is Unknown {
			if int(val_id) >= 0 {
				tc.insert_loop_var(key_id, key_type)
				tc.insert_mut_loop_var(val_id, value_type)
			} else {
				tc.insert_mut_loop_var(key_id, key_type)
			}
		} else {
			tc.insert_loop_var(key_id, key_type)
			tc.insert_loop_var(val_id, value_type)
		}
		for i in header .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases, value_cases)
		}
		tc.pop_scope()
		return
	}
	if node.kind == .match_stmt {
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(&node, i)
			branch := tc.a.node(branch_id)
			if branch.kind != .match_branch {
				continue
			}
			body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
			if body_start < 0 || body_start > branch.children_count {
				continue
			}
			tc.push_scope()
			for j in body_start .. branch.children_count {
				tc.check_comptime_static_body(tc.a.child(branch, j), var_name, loop_kind, field_cases, value_cases)
			}
			tc.pop_scope()
		}
		return
	}
	if node.kind == .select_stmt {
		for i in 0 .. node.children_count {
			branch := tc.a.child_node(&node, i)
			if branch.kind != .select_branch {
				continue
			}
			body_start := if branch.value == 'else' {
				0
			} else if branch.value in ['recv', 'recv_assign']
				|| branch.value.starts_with('recv_compound:') {
				2
			} else {
				1
			}
			tc.push_scope()
			if branch.value == 'recv' && branch.children_count >= 2 {
				tc.insert_loop_var(tc.a.child(branch, 0), unknown_type('select receive variable in static comptime body'))
			}
			if body_start <= branch.children_count {
				for j in body_start .. branch.children_count {
					tc.check_comptime_static_body(tc.a.child(branch, j), var_name, loop_kind, field_cases, value_cases)
				}
			}
			tc.pop_scope()
		}
		return
	}
	if node.kind in [.assign, .selector_assign, .index_assign] {
		for i in 0 .. node.children_count {
			tc.check_generic_body_node_global_shadowing(tc.a.child(&node, i))
		}
		tc.check_comptime_static_assignment(node, var_name, field_cases)
		return
	}
	if node.kind == .call {
		if node.children_count > 0 {
			tc.check_generic_body_node_global_shadowing(tc.a.child(&node, 0))
		}
		tc.check_comptime_static_call(id, node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	// A declaration whose RHS references the loop var is not checked here, but
	// its locals must still enter the scope: later statements in the unrolled
	// body use them (`mut fo := ...(field.attrs); ... fo.install_default(...)`),
	// and without a binding those uses report unknown identifiers.
	if node.kind == .decl_assign && node.children_count >= 2 {
		tc.check_decl_lhs_global_shadowing(node)
		for i := 0; i + 1 < int(node.children_count); i += 2 {
			rhs_id := tc.a.child(&node, i + 1)
			// The initializer itself is not semantically checked when it depends on
			// reflection metadata, but nested source bindings still need diagnostics.
			tc.check_generic_body_node_global_shadowing(rhs_id)
			lhs_id := tc.a.child(&node, i)
			lhs := tc.a.node(lhs_id)
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
				continue
			}
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
			tc.insert_decl_lhs(lhs_id, typ, tc.decl_lhs_is_mut(node, lhs_id))
		}
		return
	}
	// Reflection-dependent nodes without dedicated semantic handling are still
	// unrolled later, so inspect all source bindings they contain.
	tc.check_generic_body_node_global_shadowing(id)
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
	mut field_types := []u32{}
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
			tc.record_error_at(.assignment_mismatch, 'cannot assign to `${tc.source_text_for_node(lhs_id)}`: expected `${expected.name()}`, not `${actual_name}`', rhs_id, diagnostic_pos)
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
		tc.register_synth_type(id, builtin_void_type)
		return true
	}
	field_expr_id := tc.a.child(&node, 1)
	field_expr := tc.a.node(field_expr_id)
	if field_expr.kind != .selector {
		actual_type_id := if field_expr.kind == .ident && field_expr.value == loop_var
			&& loop_var != '' {
			comptime_source_type_field_data
		} else {
			tc.check_node(field_expr_id)
			tc.resolve_type(field_expr_id).source_type_id()
		}
		if actual_type_id != flat.source_type_string {
			actual_name := flat.source_type_name(actual_type_id)
			tc.record_error_at(.unknown_field, 'expected `string` instead of `${actual_name}` (e.g. `field.name`)', field_expr_id, field_expr.pos)
		}
		tc.record_error_at(.unknown_field, 'expected selector expression e.g. `\$(field.name)`', field_expr_id, field_expr.pos)
		if loop_var == '' {
			tc.record_invalid_comptime_selector_source_assignment(id, node)
		}
		tc.register_synth_type(id, builtin_void_type)
		return true
	}
	if field_expr.children_count == 0 {
		tc.register_synth_type(id, builtin_void_type)
		return true
	}
	field_var_id := tc.a.child(field_expr, 0)
	field_var := tc.a.node(field_var_id)
	field_var_name := tc.source_text_for_node(field_var_id)
	if loop_var == '' || field_var.kind != .ident || field_var.value != loop_var {
		tc.check_node(field_expr_id)
		mut field_expr_type := tc.resolve_type(field_expr_id)
		if loop_var == '' && field_var.kind == .ident
			&& tc.cur_scope.lookup(field_var.value) == none {
			tc.record_error_at(.unknown_field, '`${field_var.value}` does not return a value', field_expr_id, tc.node_value_diagnostic_pos(field_expr_id))
			field_expr_type = builtin_void_type
		}
		if field_expr_type !is String {
			tc.record_error_at(.unknown_field, 'expected `string` instead of `${field_expr_type.name()}` (e.g. `field.name`)', field_expr_id, tc.node_value_diagnostic_pos(field_expr_id))
		}
		if loop_var == '' {
			tc.record_error_at(.unknown_field, 'compile time field access can only be used when iterating over `T.fields`', field_var_id, field_var.pos)
		}
		tc.record_error_at(.unknown_ident, 'unknown `\$for` variable `${field_var_name}`', field_var_id, field_var.pos)
		tc.register_synth_type(id, builtin_void_type)
		return true
	}
	if field_expr.value != 'name' {
		tc.check_node(field_expr_id)
		actual := tc.resolve_type(field_expr_id)
		if actual !is String {
			tc.record_error_at(.unknown_field, 'expected `string` instead of `${actual.name()}` (e.g. `field.name`)', field_expr_id, tc.node_value_diagnostic_pos(field_expr_id))
			tc.register_synth_type(id, builtin_void_type)
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
				&& tc.struct_field_type(receiver_type.source_type_id(), field.name) != none
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
	first_source_id := field_cases.cases[0].typ
	for field in field_cases.cases[1..] {
		if field.typ != first_source_id {
			return none
		}
	}
	return *tc.parse_type(first_source_id)
}

// comptime_static_reflected_iterable_type returns the type a `for in` loop iterates when its
// container is a reflected field (`obj.$(field.name)`, optionally with an `or` fallback).
fn (tc &TypeChecker) comptime_static_reflected_iterable_type(id flat.NodeId, var_name string, field_cases ComptimeStaticFieldCases) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .or_expr && node.value !in ['?', '!'] && node.children_count > 0 {
		typ := tc.comptime_static_reflected_field_expr_type(tc.a.child(&node, 0), var_name,
			field_cases)?
		if typ is OptionType {
			return *typ.base_type
		}
		if typ is ResultType {
			return *typ.base_type
		}
		return typ
	}
	typ := tc.comptime_static_reflected_field_expr_type(id, var_name, field_cases)?
	if typ is OptionType || typ is ResultType {
		// Iterating an unhandled option is reported once the loop is unrolled.
		return none
	}
	return typ
}

fn (mut tc TypeChecker) check_comptime_static_deferred_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if !value_cases.known {
		return
	}
	mut then_cases := []ComptimeStaticValueCase{}
	mut else_cases := []ComptimeStaticValueCase{}
	for item in value_cases.cases {
		narrowing := if loop_kind == 'attributes' {
			tc.comptime_static_attribute_cond_narrowing(node.value, var_name, item)
		} else {
			ComptimeStaticAttributeCondNarrowing{}
		}
		condition_item := if narrowing.then_branch || narrowing.else_branch {
			ComptimeStaticValueCase{
				...item
				attr_schema_narrowed: item.attr_schema_id != 0
			}
		} else {
			item
		}
		cond := if narrowing.decided {
			narrowing.value.str()
		} else {
			tc.comptime_static_subst_deferred_cond(node.value, var_name, loop_kind, condition_item)
		}
		then_item := ComptimeStaticValueCase{
			...item
			attr_schema_narrowed: item.attr_schema_narrowed
				|| (narrowing.then_branch && item.attr_schema_id != 0)
		}
		else_item := ComptimeStaticValueCase{
			...item
			attr_schema_narrowed: item.attr_schema_narrowed
				|| (narrowing.else_branch && item.attr_schema_id != 0)
		}
		if comptime_text_references_var(cond, var_name) {
			then_cases << then_item
			else_cases << else_item
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or {
			then_cases << then_item
			else_cases << else_item
			continue
		}
		if taken {
			then_cases << then_item
		} else {
			else_cases << else_item
		}
	}
	if then_cases.len > 0 && node.children_count > 0 {
		saved := tc.comptime_attr_narrow_state
		if loop_kind == 'attributes' {
			tc.comptime_attr_narrow_state = &ComptimeAttributeNarrowState{ names: tc.comptime_attribute_branch_schemas(node.value, var_name, tc.comptime_attribute_narrowing_names(), true) }
		}
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, field_cases, ComptimeStaticValueCases{
			known: true
			cases: then_cases
		})
		tc.comptime_attr_narrow_state = saved
	}
	if else_cases.len > 0 && node.children_count > 1 {
		saved := tc.comptime_attr_narrow_state
		if loop_kind == 'attributes' {
			tc.comptime_attr_narrow_state = &ComptimeAttributeNarrowState{ names: tc.comptime_attribute_branch_schemas(node.value, var_name, tc.comptime_attribute_narrowing_names(), false) }
		}
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases, ComptimeStaticValueCases{
			known: true
			cases: else_cases
		})
		tc.comptime_attr_narrow_state = saved
	}
}

fn (mut tc TypeChecker) check_comptime_static_method_runtime_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	condition_id := tc.a.child(&node, 0)
	has_metadata := tc.comptime_static_method_condition_has_metadata(condition_id, var_name)
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
			dispatched := ComptimeStaticValueCase{
				...item
				runtime_dispatch: item.runtime_dispatch || has_metadata
			}
			then_cases << dispatched
			else_cases << dispatched
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
	if node.op !in [.eq, .ne, .lt, .le, .gt, .ge] {
		return none
	}
	if left := tc.comptime_static_method_int_value(tc.a.child(node, 0), var_name, item) {
		right := tc.comptime_static_method_int_value(tc.a.child(node, 1), var_name, item) or {
			return none
		}
		return match node.op {
			.eq { left == right }
			.ne { left != right }
			.lt { left < right }
			.le { left <= right }
			.gt { left > right }
			.ge { left >= right }
			else { false }
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
	first_source_id := value_cases.cases[0].return_type
	if first_source_id == 0 || first_source_id == flat.source_type_void {
		return none
	}
	for item in value_cases.cases[1..] {
		if item.return_type != first_source_id {
			return none
		}
	}
	return *tc.parse_type(first_source_id)
}

fn (tc &TypeChecker) comptime_static_subst_deferred_cond(cond string, var_name string, loop_kind string, item ComptimeStaticValueCase) string {
	mut result := cond
	if loop_kind != 'attributes' || !tc.comptime_attribute_narrow_has_field('name', item) {
		result = comptime_static_replace_unquoted(result, '${var_name}.name', comptime_static_string_literal(item.name))
	}
	if loop_kind == 'methods' {
		result = comptime_static_subst_method_param_cond(result, var_name, item)
		result = comptime_static_replace_unquoted(result, '${var_name}.args.len', item.param_names.len.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.params.len', item.param_names.len.str())
		result = comptime_static_replace_unquoted(result, '${var_name}.location', comptime_static_string_literal(item.location))
		result = comptime_static_replace_unquoted(result, '${var_name}.return_type', comptime_static_deferred_type(item.return_type))
		result = comptime_static_replace_unquoted(result, '${var_name}.typ', item.typ_text())
		if item.has_is_pub {
			result = comptime_static_replace_unquoted(result, '${var_name}.is_pub', item.is_pub.str())
		}
	} else if loop_kind == 'params' {
		result = comptime_static_replace_unquoted(result, '${var_name}.typ', comptime_static_deferred_type(item.typ))
	} else if loop_kind == 'attributes' && item.has_attr_meta {
		if !tc.comptime_attribute_narrow_has_field('has_arg', item) {
			result = comptime_static_replace_unquoted(result, '${var_name}.has_arg', item.has_arg.str())
		}
		if !tc.comptime_attribute_narrow_has_field('arg', item) {
			result = comptime_static_replace_unquoted(result, '${var_name}.arg', comptime_static_string_literal(item.arg))
		}
		if !tc.comptime_attribute_narrow_has_field('kind', item) {
			kind := comptime_static_attribute_kind_value(item.attr_kind)
			result = comptime_static_replace_unquoted(result, '${var_name}.kind ==.', '${kind} == .')
			result = comptime_static_replace_unquoted(result, '${var_name}.kind !=.', '${kind} != .')
			result = comptime_static_replace_unquoted(result, '${var_name}.kind', kind)
		}
		result = tc.comptime_static_subst_attribute_is_cond(result, var_name, flat.source_type_name(item.attr_schema_id))
	}
	return result
}

// Keep schema-field conditions unresolved here: the transform substitutes their
// bound values; guessing legacy metadata would prune the wrong branch.
fn (tc &TypeChecker) comptime_attribute_narrow_has_field(member string, item ComptimeStaticValueCase) bool {
	if !item.attr_schema_narrowed || item.attr_schema_id == 0 {
		return false
	}
	name := flat.source_type_name(item.attr_schema_id)
	for schema in tc.attribute_schemas[name.all_after_last('.')] or { []AttributeSchema{} } {
		if '${schema.module}.${schema.name}' != name {
			continue
		}
		for field in schema.fields {
			if field.name == member {
				return true
			}
		}
	}
	return false
}

// Match the transform's per-instance narrowing, including the short-circuit
// decisions of top-level is terms. Schema-field values stay unresolved here.
fn (tc &TypeChecker) comptime_static_attribute_cond_narrowing(cond string, var_name string, item ComptimeStaticValueCase) ComptimeStaticAttributeCondNarrowing {
	if tc.attribute_schemas.len == 0 || !cond.contains('is') {
		return ComptimeStaticAttributeCondNarrowing{}
	}
	clean := comptime_condition_strip_outer_parens(cond)
	is_disjunction := comptime_condition_top_level_index(clean, '||') >= 0
	op := if is_disjunction { '||' } else { '&&' }
	mut terms := []string{}
	mut rest := clean
	for {
		idx := comptime_condition_top_level_index(rest, op)
		if idx < 0 {
			break
		}
		terms << rest[..idx]
		rest = rest[idx + op.len..]
	}
	terms << rest
	mut result := ComptimeStaticAttributeCondNarrowing{}
	for term in terms {
		target, positive := tc.comptime_attribute_narrow_from_condition(term, var_name) or { continue }
		term_value := (flat.source_type_name(item.attr_schema_id) == target) == positive
		if is_disjunction {
			if term_value {
				return ComptimeStaticAttributeCondNarrowing{
					decided:     true
					value:       true
					then_branch: positive
				}
			}
			if !positive {
				result.else_branch = true
			}
		} else {
			if !term_value {
				return ComptimeStaticAttributeCondNarrowing{
					decided:     true
					value:       false
					else_branch: terms.len == 1 && !positive
				}
			}
			if positive {
				result.then_branch = true
			}
		}
	}
	return result
}

fn (tc &TypeChecker) comptime_static_subst_attribute_is_cond(cond string, var_name string, attr_schema string) string {
	if var_name == '' || !cond.contains(var_name) {
		return cond
	}
	mut out := strings.new_builder(cond.len)
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out.write_string(cond[offset..end])
			offset = end
			continue
		}
		start := offset
		if start + var_name.len <= cond.len && cond[start..start + var_name.len] == var_name
			&& (start == 0 || !comptime_cond_name_char(cond[start - 1])) {
			after_var := start + var_name.len
			if after_var >= cond.len || (!comptime_cond_name_char(cond[after_var]) && cond[after_var] != `.`) {
				mut i := after_var
				for i < cond.len && cond[i] == ` ` {
					i++
				}
				mut is_neg := false
				if i < cond.len && cond[i] == `!` {
					is_neg = true
					i++
					for i < cond.len && cond[i] == ` ` {
						i++
					}
				}
				if cond[i..].starts_with('is')
					&& (i + 2 >= cond.len || (!comptime_cond_name_char(cond[i + 2]) && cond[i + 2] != `.`)) {
					i += 2
					for i < cond.len && cond[i] == ` ` {
						i++
					}
					rhs_start := i
					for i < cond.len && (comptime_cond_name_char(cond[i]) || cond[i] == `.`) {
						i++
					}
					if rhs_start < i {
						rhs := cond[rhs_start..i]
						if target, _ := tc.comptime_attribute_narrow_from_condition('${var_name} is ${rhs}', var_name) {
							match_val := attr_schema == target
							value := if is_neg { !match_val } else { match_val }
							out.write_string(if value { 'true' } else { 'false' })
							offset = i
							continue
						}
					}
				}
			}
		}
		out.write_u8(cond[offset])
		offset++
	}
	return out.str()
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
				} else if index < item.param_types.ids.len {
					comptime_static_deferred_type(item.param_types.ids[index])
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

// comptime_attribute_branch_schemas computes possible schemas in either branch.
// Boolean structure is evaluated as sets, preserving the enclosing narrowing.
fn (tc &TypeChecker) comptime_attribute_branch_schemas(cond string, var_name string, schemas []string, taken bool) []string {
	clean := comptime_condition_strip_outer_parens(cond)
	for op in ['||', '&&'] {
		idx := comptime_condition_top_level_index(clean, op)
		if idx < 0 {
			continue
		}
		left := clean[..idx]
		right := clean[idx + op.len..]
		if (op == '&&' && taken) || (op == '||' && !taken) {
			first := tc.comptime_attribute_branch_schemas(left, var_name, schemas, taken)
			return tc.comptime_attribute_branch_schemas(right, var_name, first, taken)
		}
		mut result := tc.comptime_attribute_branch_schemas(left, var_name, schemas, taken)
		other := tc.comptime_attribute_branch_schemas(left, var_name, schemas, !taken)
		for name in tc.comptime_attribute_branch_schemas(right, var_name, other, taken) {
			if name !in result {
				result << name
			}
		}
		return result
	}
	if clean.starts_with('!') {
		return tc.comptime_attribute_branch_schemas(clean[1..], var_name, schemas, !taken)
	}
	if clean in ['true', 'false'] {
		return if (clean == 'true') == taken { schemas } else { []string{} }
	}
	if name, positive := tc.comptime_attribute_narrow_from_condition(clean, var_name) {
		mut result := []string{}
		for schema in schemas {
			if (schema == name) == (positive == taken) {
				result << schema
			}
		}
		return result
	}
	return schemas
}

fn (tc &TypeChecker) comptime_attribute_narrow_from_condition(cond string, var_name string) ?(string, bool) {
	clean := comptime_condition_strip_outer_parens(cond)
	if var_name == '' || !clean.starts_with(var_name) {
		return none
	}
	mut i := var_name.len
	if i < clean.len && (comptime_cond_name_char(clean[i]) || clean[i] == `.`) {
		return none
	}
	for i < clean.len && clean[i] == ` ` {
		i++
	}
	mut is_neg := false
	if i < clean.len && clean[i] == `!` {
		is_neg = true
		i++
	}
	if !clean[i..].starts_with('is ') {
		return none
	}
	name := clean[i + 3..].trim_space()
	for ch in name {
		if !comptime_cond_name_char(ch) && ch != `.` {
			return none
		}
	}
	short := name.all_after_last('.')
	qualifier := if name.contains('.') { name.all_before_last('.') } else { '' }
	for schema in tc.attribute_schemas[short] or { []AttributeSchema{} } {
		if (qualifier != '' && schema.module == qualifier)
			|| (qualifier == '' && attribute_modules_compatible(schema.module, tc.cur_module)) {
			return '${schema.module}.${schema.name}', !is_neg
		}
	}
	return none
}

fn comptime_static_deferred_type(typ_id u32) string {
	return flat.source_type_name(if typ_id == comptime_source_type_void_pointer {
		flat.source_type_voidptr
	} else {
		typ_id
	})
}

fn comptime_static_attribute_kind_value(recorded_kind int) string {
	kind := if recorded_kind >= 0 {
		recorded_kind & flat.attribute_kind_mask
	} else {
		recorded_kind
	}
	return match kind {
		1 { '.string' }
		2 { '.number' }
		3 { '.bool' }
		4 { '.comptime_define' }
		else { '.plain' }
	}
}

fn comptime_static_replace_unquoted(cond string, needle string, replacement string) string {
	if needle == '' || !cond.contains(needle) {
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

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &FnDiagnosticParam) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &SourceStructFieldDecl) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &ComptimeStaticFieldCase) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &ComptimeStaticValueCase) typ_text() string {
	return flat.source_type_name(value.typ)
}

// unaliased_typ_text resolves the indexed source type spelling.
@[inline]
fn (value &ComptimeStaticFieldCase) unaliased_typ_text() string {
	return flat.source_type_name(value.unaliased_typ)
}

// raw_typ_text resolves the indexed source type spelling.
@[inline]
fn (value &ComptimeStaticFieldDeclMeta) raw_typ_text() string {
	return flat.source_type_name(value.raw_typ)
}

// return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &CFnDeclSignature) return_type_text() string {
	return flat.source_type_name(value.return_type)
}

// return_type_text resolves the indexed source type spelling.
@[inline]
fn (value &ComptimeStaticValueCase) return_type_text() string {
	return flat.source_type_name(value.return_type)
}

// param_types_text resolves indexed types at a text integration boundary.
fn (value &ComptimeStaticValueCase) param_types_text() []string {
	return value.param_types.names()
}

// base_name_text resolves the indexed source type spelling.
@[inline]
fn (value &SqlOrmFieldTypeInfo) base_name_text() string {
	return flat.source_type_name(value.base_name)
}

// names_text resolves ordered implementation identities for a text consumer.
pub fn (index &InterfaceImplIndex) names_text() []string {
	return index.names.names()
}

// owner_text resolves the missing field owner source type spelling.
@[inline]
fn (field &MissingReferenceField) owner_text() string {
	return flat.source_type_name(field.owner)
}
