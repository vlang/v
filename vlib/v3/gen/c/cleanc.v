module c

import os
import strings
import v3.flat
import v3.gen.c.naming
import v3.types

struct ActiveLock {
	mutexes_var string
	lock_count  int
	unlock_fn   string
	scope_id    int
	loop_depth  int
	defer_depth int
}

struct LoopLabelState {
	label string
mut:
	had_prev   bool
	prev_depth int
}

struct FixedStorageConstRefItem {
	id     flat.NodeId
	file   string
	module string
}

struct EnumBackingInfo {
	c_name         string
	storage_c_type string
}

// FlatGen emits flat gen output used by c.
pub struct FlatGen {
mut:
	sb                             strings.Builder
	indent                         int
	a                              &flat.FlatAst = unsafe { nil }
	used_fns                       map[string]bool
	used_fn_names                  []string
	fn_gen_items                   []FlatFnGenItem
	fn_segs                        []string
	test_files                     map[string]bool
	cache_program_files            map[string]bool
	str_lits                       []string
	str_lit_ids                    map[string]int
	global_types                   map[string]types.Type
	enum_vals                      map[string]int
	enum_value_exprs               map[string]string
	defers                         []flat.NodeId
	fn_defers                      []flat.NodeId
	fn_defer_counts                map[int]string
	defer_capture_names            []string
	defer_capture_types            map[string]types.Type
	interfaces                     map[string][]string
	const_vals                     map[string]flat.NodeId
	const_modules                  map[string]string
	const_files                    map[string]string // const name -> declaring file (for import-alias type resolution)
	const_init_order               []string
	fixed_storage_consts           map[string]bool
	global_modules                 map[string]string
	global_files                   map[string]string      // qualified global name -> declaring file (for import-alias type resolution)
	global_inits                   map[string]flat.NodeId // qualified global name -> initializer value node
	global_init_order              []string               // qualified global names, in declaration order
	enum_backing_infos             map[string]EnumBackingInfo
	iface_impls                    map[string][]string // interface name -> implementing concrete type names
	iface_type_ids                 map[string]int      // "${iface}::${concrete}" -> 1-based type id
	ierror_method_emit_names       map[string]bool     // names/lowered names of concrete IError msg/code methods
	ierror_stack_pointer_aliases   []map[string]bool   // scoped local pointer aliases to stack subobjects
	local_pointer_storage_by_owner map[string]bool     // exact scope binding owner -> C storage is already a pointer
	local_c_type_by_owner          map[string]string   // exact scope binding owner -> emitted C declaration type
	local_shared_storage_by_owner  map[string]bool     // exact scope binding owner -> C storage is a shared wrapper pointer
	sum_name_lookup                map[string]string   // full/short sum type name -> canonical sum type name
	module_init_fns                []string            // C names of module-level `init()` fns, in source order
	module_init_fn_modules         map[string]string   // C init fn name -> V module name
	module_imports                 map[string][]string // module -> imported modules
	c_directives                   []CDirective
	inlined_c_structs              map[string]bool
	inlined_c_typedef_names        map[string]bool
	inlined_c_fns                  map[string]bool
	inlined_c_declared_fns         map[string]bool
	c_flags                        []string
	use_system_stdint              bool
	libc_compat_fns                map[string]bool
	tc                             &types.TypeChecker = unsafe { nil }
	has_builtins                   bool
	tmp_count                      int
	line_start                     bool
	field_name_set                 map[string]bool   // every struct field's C name (lazy) — for const/field collision checks
	modules                        map[string]string // alias -> full module name
	fn_ptr_types                   map[string]string // fn_ptr:ret|params -> typedef name
	multi_return_types             []types.Type
	multi_return_type_names        map[string]bool
	multi_return_types_ready       bool
	fixed_array_ret_wrappers       map[string]bool // bare fixed-array c_type name -> has a return wrapper struct
	emitted_fixed_array_typedefs   map[string]bool // bare fixed-array typedefs already written (shared across passes)
	concrete_optional_abi_fns      map[string]bool // emitted fn names whose option/result params use Optional_T ABI
	fixed_array_typedefs_needed    map[string]FixedArrayTypedefInfo
	fixed_array_typedefs_ready     bool
	fn_decl_param_types            map[string][]types.Type
	fn_decl_variadic               map[string]bool
	fn_decl_variadic_short_counts  map[string]int
	fn_decl_shared_params          map[string][]bool
	fn_shared_params_resolved      map[string][]bool
	has_shared_params              bool
	fn_decl_mut_receivers          map[string]bool
	fn_decl_ret_types              map[string]types.Type // fn decl name (and qualified variants) -> return type
	// Const dependency analysis follows helper calls. Keep declaration indexes so
	// resolving each call does not scan the whole flattened AST.
	fn_decl_nodes_by_name         map[string]flat.NodeId
	fn_decl_nodes_by_short        map[string]flat.NodeId
	fn_decl_nodes_by_module_short map[string]flat.NodeId
	// set of `${module}\x01${name}` for every non-generic fn decl, built once in
	// precompute_non_generic_fn_index. Replaces the former full-node scan in
	// non_generic_fn_decl_exists_in_module (O(nodes) per call, hot in cgen).
	non_generic_fn_names_by_module map[string]bool
	// indexes over tc.fn_generic_params keys, built once in
	// precompute_generic_fn_key_index. They replace the former full-map scan in
	// generic_plain_fn_base_for_call (O(generic fns) with two string allocations
	// per key, run for nearly every emitted call). The ordinal preserves the
	// map's iteration order so multi-match resolution stays byte-identical.
	generic_fn_keys_by_short     map[string][]string
	generic_fn_keys_by_cname     map[string][]string
	generic_fn_key_ordinal       map[string]int
	struct_decl_infos            map[string]StructDeclInfo
	struct_decl_short_infos      map[string]StructDeclInfo
	shared_type_names            map[string]SharedTypeInfo // __shared__ wrapper name -> wrapped type metadata
	needs_shared_runtime         bool
	const_runtime_inits          []string
	const_runtime_init_modules   []string
	runtime_inits                []string
	runtime_init_modules         []string
	compiler_vroot               string
	compiler_vexe                string
	c99_mode                     bool
	skip_generics                bool
	cur_fn_name                  string
	cur_param_names              []string
	cur_param_type_values        []types.Type
	cur_param_types              map[string]types.Type
	cur_concrete_optional_params map[string]bool
	cur_mut_params               map[string]bool
	cur_mut_param_owners         map[string]types.ScopeBindingOwner
	cur_fn_ret                   types.Type = types.Type(types.void_)
	cur_fn_ret_is_optional       bool
	cur_fn_ret_base              types.Type = types.Type(types.void_)
	active_locks                 []ActiveLock
	loop_depth                   int
	loop_label_depths            map[string]int
	goto_label_lock_scopes       map[string][]int
	pending_loop_label           string
	// in_return is true only while generating a `return` statement's value, so a bare
	// generic literal (`return Box{...}`) may adopt `cur_fn_ret`'s concrete instance —
	// but a literal in a local decl / argument elsewhere in the body does not.
	in_return                      bool
	cur_return_node_id             int = -1
	ownership_return_index         int
	ownership_seen_return_sources  map[string]bool
	ownership_propagation_index    int
	ownership_loop_control_index   int
	ownership_loop_iteration_index int
	ownership_scope_index          int
	cur_return_drops               []types.OwnershipDropEntry
	pending_return_scope_drops     []types.OwnershipDropEntry
	expected_expr_type             types.Type = types.Type(types.void_)
	expected_enum                  string
	needed_optional_types          map[string]string
	emitted_optional_types         map[string]bool
	emitted_fns                    map[string]bool
	array_method_cache             map[string]string
	param_types_cache              map[string][]types.Type        // (name|fallback) -> resolved param types
	embedded_fields_by_type        map[string][]types.StructField // type name -> its embedded fields (usually empty)
	param_types_by_short           map[string][]types.Type        // method short-name suffix -> param types (fallback index)
	generic_method_candidates      map[string][]GenericMethodCandidate
	spawn_wrapper_names            map[string]string
	spawn_wrapper_defs             []string
	callback_wrapper_names         map[string]string
	callback_wrapper_defs          []string
	parallel_used                  bool
	c_name_cache                   &CNameCache = unsafe { nil }
	// Body-independent postamble segments emitted on helper threads while the
	// fn-body workers run; spliced into the final output in the exact order
	// the serial postamble produces them.
	post_segs               []string
	emitted_fn_ptr_typedefs map[string]bool
	c_extern_refs           map[string]bool
	c_extern_refs_ready     bool
	parallel_prepared       bool
	post_str_lits_snapshot  int
	const_short_index       &ConstShortIndex = unsafe { nil }
	mut_recv_facts          &FnNameFactCache = unsafe { nil }
	want_parallel_prep      bool
	cache_split             bool
	// Set when the target is built with -prealloc / -d prealloc: the bump
	// arena's base block pointer must be thread-local (matching V1's cgen),
	// or every spawned thread would race on the same arena.
	prealloc               bool
	scope_parallel_workers bool
	worker_scope           voidptr
	parallel_worker_scopes []voidptr
}

struct FixedArrayTypedefInfo {
	arr    types.ArrayFixed
	module string
}

struct CDirective {
	module        string
	text          string
	before_import bool
}

struct CInlineHeader {
	text                 string
	preserved_directives []string
	preserved_c_fns      []string
	preserved_c_structs  []string
}

// was_parallel reports whether the last fn codegen actually ran across threads.
pub fn (g &FlatGen) was_parallel() bool {
	return g.parallel_used
}

pub fn (g &FlatGen) c_flags() []string {
	return g.c_flags.clone()
}

// set_c99_mode configures whether generated C should support strict C99 builds.
pub fn (mut g FlatGen) set_c99_mode(enabled bool) {
	g.c99_mode = enabled
}

// set_prealloc marks the build as using the -prealloc bump arena.
pub fn (mut g FlatGen) set_prealloc(on bool) {
	g.prealloc = on
}

// set_skip_generics removes generic-only metadata work when reachability proved
// that the generated program has no generic instantiations.
pub fn (mut g FlatGen) set_skip_generics(on bool) {
	g.skip_generics = on
}

fn (mut g FlatGen) push_scope() {
	g.tc.push_scope()
	g.ierror_stack_pointer_aliases << map[string]bool{}
}

fn (mut g FlatGen) pop_scope() {
	g.tc.pop_scope()
	if g.ierror_stack_pointer_aliases.len > 0 {
		g.ierror_stack_pointer_aliases.delete_last()
	}
}

fn (mut g FlatGen) declare_ierror_pointer_alias(name string, needs_copy bool) {
	if name.len == 0 {
		return
	}
	if g.ierror_stack_pointer_aliases.len == 0 {
		g.ierror_stack_pointer_aliases << map[string]bool{}
	}
	last := g.ierror_stack_pointer_aliases.len - 1
	g.ierror_stack_pointer_aliases[last][name] = needs_copy
}

fn (mut g FlatGen) assign_ierror_pointer_alias(name string, needs_copy bool) {
	if name.len == 0 {
		return
	}
	current_idx := g.ierror_stack_pointer_aliases.len - 1
	if idx := g.ierror_pointer_alias_scope_index(name) {
		previous := if name in g.ierror_stack_pointer_aliases[idx] {
			g.ierror_stack_pointer_aliases[idx][name]
		} else {
			false
		}
		g.ierror_stack_pointer_aliases[idx][name] = if idx == current_idx {
			needs_copy
		} else {
			previous || needs_copy
		}
		return
	}
	g.declare_ierror_pointer_alias(name, needs_copy)
}

fn (g &FlatGen) ierror_pointer_alias_scope_index(name string) ?int {
	if name.len == 0 {
		return none
	}
	mut scope := g.tc.cur_scope
	mut idx := g.ierror_stack_pointer_aliases.len - 1
	for scope != unsafe { nil } && scope != g.tc.file_scope && idx >= 0 {
		for existing in scope.names {
			if existing == name {
				return idx
			}
		}
		scope = scope.parent
		idx--
	}
	return none
}

fn (g &FlatGen) ierror_pointer_alias_needs_copy(name string) bool {
	idx := g.ierror_pointer_alias_scope_index(name) or { return false }
	if name in g.ierror_stack_pointer_aliases[idx] {
		return g.ierror_stack_pointer_aliases[idx][name]
	}
	return false
}

fn (mut g FlatGen) declare_local_pointer_storage(owner types.ScopeBindingOwner, is_pointer bool) {
	key := owner.storage_key()
	if key.len == 0 {
		return
	}
	if is_pointer {
		g.local_pointer_storage_by_owner[key] = true
	} else {
		g.local_pointer_storage_by_owner.delete(key)
	}
}

fn (mut g FlatGen) declare_local_c_type(owner types.ScopeBindingOwner, c_type string) {
	key := owner.storage_key()
	if key.len == 0 {
		return
	}
	if c_type.len > 0 {
		g.local_c_type_by_owner[key] = c_type
	} else {
		g.local_c_type_by_owner.delete(key)
	}
}

fn (g &FlatGen) local_storage_owner(name string) ?types.ScopeBindingOwner {
	if g.tc == unsafe { nil } || g.tc.cur_scope == unsafe { nil } {
		return none
	}
	owner := g.tc.cur_scope.lookup_owner(name) or { return none }
	$if ownership ? {
		if !g.tc.cur_scope.nearest_binding_owned_by(name, owner) {
			return none
		}
	}
	return owner
}

fn (g &FlatGen) local_storage_c_type(name string) ?string {
	if name.len == 0 || g.local_c_type_by_owner.len == 0 {
		return none
	}
	owner := g.local_storage_owner(name) or { return none }
	return g.local_c_type_by_owner[owner.storage_key()] or { none }
}

fn (mut g FlatGen) declare_local_shared_storage(owner types.ScopeBindingOwner, is_shared bool) {
	key := owner.storage_key()
	if key.len == 0 {
		return
	}
	if is_shared {
		g.local_shared_storage_by_owner[key] = true
	} else {
		g.local_shared_storage_by_owner.delete(key)
	}
}

fn (g &FlatGen) local_storage_is_shared(name string) bool {
	if name.len == 0 || g.local_shared_storage_by_owner.len == 0 {
		return false
	}
	owner := g.local_storage_owner(name) or { return false }
	return g.local_shared_storage_by_owner[owner.storage_key()] or { false }
}

fn (g &FlatGen) local_storage_is_pointer(name string) bool {
	if name.len == 0 {
		return false
	}
	// Pointer-storage locals are rare; when none are registered the two scope
	// chain walks below cannot change the answer (asked once per ident on the
	// call-emission path).
	if g.local_pointer_storage_by_owner.len == 0 {
		return false
	}
	owner := g.local_storage_owner(name) or { return false }
	return g.local_pointer_storage_by_owner[owner.storage_key()] or { false }
}

// new creates a FlatGen value for c.
pub fn FlatGen.new() FlatGen {
	return FlatGen{
		sb:                             strings.new_builder(4096)
		used_fns:                       map[string]bool{}
		fn_gen_items:                   []FlatFnGenItem{}
		fn_segs:                        []string{}
		test_files:                     map[string]bool{}
		cache_program_files:            map[string]bool{}
		str_lit_ids:                    map[string]int{}
		global_types:                   map[string]types.Type{}
		enum_vals:                      map[string]int{}
		enum_value_exprs:               map[string]string{}
		interfaces:                     map[string][]string{}
		const_vals:                     map[string]flat.NodeId{}
		const_modules:                  map[string]string{}
		const_files:                    map[string]string{}
		const_init_order:               []string{}
		fixed_storage_consts:           map[string]bool{}
		global_modules:                 map[string]string{}
		global_files:                   map[string]string{}
		global_inits:                   map[string]flat.NodeId{}
		global_init_order:              []string{}
		enum_backing_infos:             map[string]EnumBackingInfo{}
		iface_impls:                    map[string][]string{}
		iface_type_ids:                 map[string]int{}
		ierror_method_emit_names:       map[string]bool{}
		ierror_stack_pointer_aliases:   []map[string]bool{}
		local_pointer_storage_by_owner: map[string]bool{}
		local_c_type_by_owner:          map[string]string{}
		local_shared_storage_by_owner:  map[string]bool{}
		sum_name_lookup:                map[string]string{}
		module_init_fns:                []string{}
		module_init_fn_modules:         map[string]string{}
		module_imports:                 map[string][]string{}
		c_directives:                   []CDirective{}
		inlined_c_structs:              map[string]bool{}
		inlined_c_fns:                  map[string]bool{}
		inlined_c_declared_fns:         map[string]bool{}
		inlined_c_typedef_names:        map[string]bool{}
		c_flags:                        []string{}
		libc_compat_fns:                map[string]bool{}
		modules:                        map[string]string{}
		fn_ptr_types:                   map[string]string{}
		multi_return_types:             []types.Type{}
		multi_return_type_names:        map[string]bool{}
		fixed_array_ret_wrappers:       map[string]bool{}
		emitted_fixed_array_typedefs:   map[string]bool{}
		concrete_optional_abi_fns:      map[string]bool{}
		fixed_array_typedefs_needed:    map[string]FixedArrayTypedefInfo{}
		fn_decl_param_types:            map[string][]types.Type{}
		fn_decl_variadic:               map[string]bool{}
		fn_decl_variadic_short_counts:  map[string]int{}
		fn_decl_shared_params:          map[string][]bool{}
		fn_shared_params_resolved:      map[string][]bool{}
		fn_decl_mut_receivers:          map[string]bool{}
		fn_decl_ret_types:              map[string]types.Type{}
		fn_decl_nodes_by_name:          map[string]flat.NodeId{}
		fn_decl_nodes_by_short:         map[string]flat.NodeId{}
		fn_decl_nodes_by_module_short:  map[string]flat.NodeId{}
		non_generic_fn_names_by_module: map[string]bool{}
		generic_fn_keys_by_short:       map[string][]string{}
		generic_fn_keys_by_cname:       map[string][]string{}
		generic_fn_key_ordinal:         map[string]int{}
		struct_decl_infos:              map[string]StructDeclInfo{}
		struct_decl_short_infos:        map[string]StructDeclInfo{}
		shared_type_names:              map[string]SharedTypeInfo{}
		cur_param_names:                []string{}
		cur_param_type_values:          []types.Type{}
		cur_param_types:                map[string]types.Type{}
		cur_concrete_optional_params:   map[string]bool{}
		cur_mut_params:                 map[string]bool{}
		cur_mut_param_owners:           map[string]types.ScopeBindingOwner{}
		active_locks:                   []ActiveLock{}
		loop_label_depths:              map[string]int{}
		goto_label_lock_scopes:         map[string][]int{}
		ownership_seen_return_sources:  map[string]bool{}
		needed_optional_types:          map[string]string{}
		emitted_optional_types:         map[string]bool{}
		emitted_fns:                    map[string]bool{}
		array_method_cache:             map[string]string{}
		param_types_cache:              map[string][]types.Type{}
		embedded_fields_by_type:        map[string][]types.StructField{}
		param_types_by_short:           map[string][]types.Type{}
		generic_method_candidates:      map[string][]GenericMethodCandidate{}
		spawn_wrapper_names:            map[string]string{}
		spawn_wrapper_defs:             []string{}
		callback_wrapper_names:         map[string]string{}
		callback_wrapper_defs:          []string{}
		str_lits:                       []string{}
		defers:                         []flat.NodeId{}
		fn_defers:                      []flat.NodeId{}
		fn_defer_counts:                map[int]string{}
		defer_capture_names:            []string{}
		defer_capture_types:            map[string]types.Type{}
		const_runtime_inits:            []string{}
		const_runtime_init_modules:     []string{}
		runtime_inits:                  []string{}
		runtime_init_modules:           []string{}
		compiler_vroot:                 ''
		compiler_vexe:                  ''
		line_start:                     true
	}
}

// set_compiler_vexe sets the V executable path baked into generated test/runtime helpers.
pub fn (mut g FlatGen) set_compiler_vexe(path string) {
	g.compiler_vexe = path
}

// set_cache_split enables stable cache markers and string symbols in generated C.
// The v3 driver uses them to split one checked program into independently cached
// module objects without changing regular `-o file.c` output.
pub fn (mut g FlatGen) set_cache_split(enabled bool) {
	g.cache_split = enabled
}

// set_cache_program_files assigns entry-module source files to the program
// translation unit rather than an imported module cache object.
pub fn (mut g FlatGen) set_cache_program_files(files []string) {
	g.cache_program_files = map[string]bool{}
	for file in files {
		g.cache_program_files[file] = true
	}
}

// cache_external_input_files returns local include/embed inputs grouped by the
// module whose cached object incorporates their contents. Forced-include inputs
// affect every object and are kept in a configuration-wide group. The second
// result reports include forms whose dependencies cannot be resolved statically.
pub fn cache_external_input_files(a &flat.FlatAst, vroot string, source_modules map[string]bool) (map[string][]string, bool) {
	mut c_flags := []string{}
	mut cur_file := ''
	for node in a.nodes {
		if node.kind == .file {
			cur_file = node.value
			continue
		}
		if node.kind != .directive || node.value != 'flag' || node.typ.len == 0 {
			continue
		}
		flag := c_flag_arg(node.typ, vroot, cur_file)
		if flag.len > 0 && flag !in c_flags {
			c_flags << flag
		}
	}
	include_dirs := c_flag_include_dirs(c_flags)
	mut collect_modules := map[string]bool{}
	for module_name, enabled in source_modules {
		if enabled {
			collect_modules[module_name] = true
			collect_modules[module_name.all_after_last('.')] = true
		}
	}
	mut inputs := map[string][]string{}
	mut has_untracked_include := false
	mut cur_module := ''
	cur_file = ''
	for node in a.nodes {
		if node.kind == .file {
			cur_file = node.value
			cur_module = ''
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if !collect_modules[cur_module] {
			continue
		}
		if node.kind == .directive && node.value in ['include', 'insert'] && node.typ.len > 0 {
			include_arg := c_include_arg(node.typ, vroot, cur_file)
			if !c_include_arg_is_literal(include_arg) {
				has_untracked_include = true
				continue
			}
			for path in c_include_file_paths(include_arg, vroot, cur_file, include_dirs) {
				if !os.is_file(path) {
					continue
				}
				mut seen := map[string]bool{}
				mut files := []string{}
				if c_collect_external_input_tree(path, vroot, include_dirs, mut seen, mut files) {
					has_untracked_include = true
				}
				for file in files {
					c_add_cache_external_input(mut inputs, cur_module, file)
				}
				break
			}
			continue
		}
		if path := c_embed_external_input_path(a, node) {
			c_add_cache_external_input(mut inputs, cur_module, path)
		}
	}
	flag_inputs, flags_have_untracked_include := cache_c_flag_input_files_with_status(c_flags)
	if flags_have_untracked_include {
		has_untracked_include = true
	}
	for path in flag_inputs {
		c_add_cache_external_input(mut inputs, '__v3_c_flags__', path)
	}
	for module_name, paths in inputs {
		mut sorted := paths.clone()
		sorted.sort()
		inputs[module_name] = sorted
	}
	return inputs, has_untracked_include
}

// cache_c_flag_input_files returns forced include/macro files whose contents
// affect every cached object compiled with the supplied C flags.
pub fn cache_c_flag_input_files(flags []string) []string {
	files, _ := cache_c_flag_input_files_with_status(flags)
	return files
}

fn cache_c_flag_input_files_with_status(flags []string) ([]string, bool) {
	include_dirs := c_flag_include_dirs(flags)
	mut seen := map[string]bool{}
	mut files := []string{}
	mut has_untracked_include := false
	for flag in flags {
		for forced_input in c_forced_include_inputs(flag) {
			for path in c_include_file_paths('"${forced_input}"', '', '', include_dirs) {
				if !os.is_file(path) {
					continue
				}
				if c_collect_external_input_tree(path, '', include_dirs, mut seen, mut files) {
					has_untracked_include = true
				}
				break
			}
		}
	}
	files.sort()
	return files, has_untracked_include
}

fn c_forced_include_inputs(flag string) []string {
	mut inputs := []string{}
	tokens := tokenize_c_flag(flag)
	mut i := 0
	for i < tokens.len {
		token := tokens[i]
		if token in ['-include', '-imacros'] && i + 1 < tokens.len {
			inputs << tokens[i + 1].trim('"\'')
			i += 2
			continue
		}
		for prefix in ['-include=', '-imacros='] {
			if token.starts_with(prefix) && token.len > prefix.len {
				inputs << token[prefix.len..].trim('"\'')
			}
		}
		i++
	}
	return inputs
}

// tokenize_c_flag splits a C flag on unquoted whitespace while preserving quotes.
pub fn tokenize_c_flag(value string) []string {
	mut tokens := []string{}
	mut start := -1
	mut quote := u8(0)
	mut escaped := false
	for i, c in value.bytes() {
		if start < 0 {
			if c.is_space() {
				continue
			}
			start = i
		}
		if escaped {
			escaped = false
			continue
		}
		if c == `\\` {
			escaped = true
			continue
		}
		if quote != 0 {
			if c == quote {
				quote = 0
			}
			continue
		}
		if c in [`'`, `\"`] {
			quote = c
			continue
		}
		if c.is_space() {
			tokens << value[start..i]
			start = -1
		}
	}
	if start >= 0 {
		tokens << value[start..]
	}
	return tokens
}

fn c_add_cache_external_input(mut inputs map[string][]string, module_name string, path string) {
	if module_name.len == 0 || path.len == 0 || !os.is_file(path) {
		return
	}
	real_path := os.real_path(path)
	mut paths := inputs[module_name]
	if real_path !in paths {
		paths << real_path
		inputs[module_name] = paths
	}
}

fn c_collect_external_input_tree(path string, vroot string, include_dirs []string, mut seen map[string]bool, mut files []string) bool {
	if path.len == 0 || !os.is_file(path) {
		return false
	}
	real_path := os.real_path(path)
	if seen[real_path] {
		return false
	}
	seen[real_path] = true
	files << real_path
	text := os.read_file(real_path) or { return false }
	mut has_untracked_include := false
	mut in_block_comment := false
	for line in text.split_into_lines() {
		clean, next_in_block_comment := c_preprocessor_directive_scan_line(line, in_block_comment)
		in_block_comment = next_in_block_comment
		if c_directive_name(clean) != 'include' {
			continue
		}
		include_arg := c_include_arg(c_directive_arg(clean), vroot, real_path)
		if !c_include_arg_is_literal(include_arg) {
			has_untracked_include = true
			continue
		}
		for nested_path in c_include_file_paths(include_arg, vroot, real_path, include_dirs) {
			if !os.is_file(nested_path) {
				continue
			}
			if c_collect_external_input_tree(nested_path, vroot, include_dirs, mut seen, mut files) {
				has_untracked_include = true
			}
			break
		}
	}
	return has_untracked_include
}

fn c_embed_external_input_path(a &flat.FlatAst, node flat.Node) ?string {
	if node.kind != .struct_init || node.value != 'embed_file.EmbedFileData' {
		return none
	}
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		if field.kind != .field_init || field.value != 'apath' || field.children_count == 0 {
			continue
		}
		value := a.child_node(field, 0)
		if value.kind == .string_literal && value.value.len > 0 && os.is_file(value.value) {
			return os.real_path(value.value)
		}
	}
	return none
}

// set_scope_parallel_workers makes cgen helpers use disposable prealloc
// arenas. The caller must release them with free_parallel_worker_scopes after
// consuming the generated C output and cgen metadata.
pub fn (mut g FlatGen) set_scope_parallel_workers(enabled bool) {
	g.scope_parallel_workers = enabled
}

// free_parallel_worker_scopes releases scratch arenas retained by joined cgen
// helper threads.
pub fn (mut g FlatGen) free_parallel_worker_scopes() {
	$if prealloc {
		for scope in g.parallel_worker_scopes {
			if scope != unsafe { nil } {
				unsafe { prealloc_scope_free_after(scope) }
			}
		}
	}
	g.parallel_worker_scopes = []voidptr{}
}

// gen supports gen handling for FlatGen.
pub fn (mut g FlatGen) gen(a &flat.FlatAst) string {
	tc := types.TypeChecker.new(a)
	return g.gen_with_used(a, map[string]bool{}, &tc)
}

// gen_with_used emits with used output for c.
pub fn (mut g FlatGen) gen_with_used(a &flat.FlatAst, used_fns map[string]bool, tc &types.TypeChecker) string {
	return g.gen_with_used_options(a, used_fns, tc, false)
}

pub fn (mut g FlatGen) gen_with_used_test_options(a &flat.FlatAst, used_fns map[string]bool, tc &types.TypeChecker, no_parallel bool, test_files []string) string {
	g.test_files = map[string]bool{}
	for file in test_files {
		g.test_files[file] = true
	}
	return g.gen_with_used_options(a, used_fns, tc, no_parallel)
}

// gen_with_used_options emits with used options output for c.
pub fn (mut g FlatGen) gen_with_used_options(a &flat.FlatAst, used_fns map[string]bool, tc &types.TypeChecker, no_parallel bool) string {
	g.a = a
	g.used_fns = used_fns.clone()
	g.used_fn_names = []string{}
	g.fn_gen_items = []FlatFnGenItem{}
	g.fn_segs = []string{}
	g.str_lits = []string{}
	g.defers = []flat.NodeId{}
	g.fn_defers = []flat.NodeId{}
	g.fn_defer_counts.clear()
	g.defer_capture_names = []string{}
	g.defer_capture_types.clear()
	g.const_runtime_inits = []string{}
	g.const_runtime_init_modules = []string{}
	g.runtime_inits = []string{}
	g.runtime_init_modules = []string{}
	g.compiler_vroot = ''
	g.str_lit_ids.clear()
	g.global_types.clear()
	g.enum_vals.clear()
	g.enum_value_exprs.clear()
	g.interfaces.clear()
	g.const_vals.clear()
	g.const_modules.clear()
	g.const_files.clear()
	g.const_init_order = []string{}
	g.fixed_storage_consts.clear()
	g.global_modules.clear()
	g.global_files.clear()
	g.global_inits.clear()
	g.global_init_order = []string{}
	g.enum_backing_infos.clear()
	g.iface_impls.clear()
	g.iface_type_ids.clear()
	g.ierror_method_emit_names.clear()
	g.ierror_stack_pointer_aliases = []map[string]bool{}
	g.local_pointer_storage_by_owner.clear()
	g.local_c_type_by_owner.clear()
	g.local_shared_storage_by_owner.clear()
	g.sum_name_lookup.clear()
	g.module_init_fns = []string{}
	g.module_init_fn_modules.clear()
	g.module_imports.clear()
	g.c_directives = []CDirective{}
	g.inlined_c_structs.clear()
	g.inlined_c_fns.clear()
	g.inlined_c_declared_fns.clear()
	g.inlined_c_typedef_names.clear()
	g.c_flags = []string{}
	g.use_system_stdint = false
	g.libc_compat_fns.clear()
	g.modules.clear()
	g.fn_ptr_types.clear()
	g.multi_return_types = []types.Type{}
	g.multi_return_type_names.clear()
	g.multi_return_types_ready = false
	g.fixed_array_ret_wrappers.clear()
	g.emitted_fixed_array_typedefs.clear()
	g.concrete_optional_abi_fns.clear()
	g.fixed_array_typedefs_needed.clear()
	g.fixed_array_typedefs_ready = false
	g.fn_decl_param_types.clear()
	g.fn_decl_variadic.clear()
	g.fn_decl_variadic_short_counts.clear()
	g.fn_decl_shared_params.clear()
	g.fn_shared_params_resolved.clear()
	g.has_shared_params = false
	g.fn_decl_mut_receivers.clear()
	g.fn_decl_ret_types.clear()
	g.fn_decl_nodes_by_name.clear()
	g.fn_decl_nodes_by_short.clear()
	g.fn_decl_nodes_by_module_short.clear()
	g.non_generic_fn_names_by_module.clear()
	g.generic_fn_keys_by_short.clear()
	g.generic_fn_keys_by_cname.clear()
	g.generic_fn_key_ordinal.clear()
	g.struct_decl_infos.clear()
	g.struct_decl_short_infos.clear()
	g.shared_type_names.clear()
	g.needs_shared_runtime = false
	g.cur_param_names = []string{}
	g.cur_param_type_values = []types.Type{}
	g.cur_param_types.clear()
	g.cur_concrete_optional_params.clear()
	g.cur_mut_params.clear()
	g.cur_mut_param_owners.clear()
	g.active_locks = []ActiveLock{}
	g.loop_depth = 0
	g.loop_label_depths.clear()
	g.goto_label_lock_scopes.clear()
	g.pending_loop_label = ''
	g.needed_optional_types.clear()
	g.emitted_optional_types.clear()
	g.emitted_fns.clear()
	g.array_method_cache.clear()
	g.param_types_cache.clear()
	g.embedded_fields_by_type.clear()
	g.param_types_by_short.clear()
	g.generic_method_candidates.clear()
	g.spawn_wrapper_names.clear()
	g.spawn_wrapper_defs = []string{}
	g.callback_wrapper_names.clear()
	g.callback_wrapper_defs = []string{}
	g.parallel_used = false
	g.c_name_cache = &CNameCache{}
	g.post_segs = []string{}
	g.emitted_fn_ptr_typedefs.clear()
	g.c_extern_refs.clear()
	g.c_extern_refs_ready = false
	g.parallel_prepared = false
	g.post_str_lits_snapshot = 0
	g.const_short_index = &ConstShortIndex{}
	g.mut_recv_facts = &FnNameFactCache{}
	g.want_parallel_prep = false
	g.worker_scope = unsafe { nil }
	g.parallel_worker_scopes = []voidptr{}
	g.tc = unsafe { tc }
	if g.tc.a == unsafe { nil } {
		g.tc.collect(a)
	}
	g.has_builtins = g.tc.has_builtins
	g.collect_gen_info()
	g.precompute_shared_param_index()
	if !g.skip_generics {
		g.precompute_non_generic_fn_index()
		g.precompute_generic_fn_key_index()
	}
	// In the parallel path the fixed-storage scan runs on a helper thread,
	// overlapped with the fn-item collection and parallel pre-seeding.
	// The master emits selectors/inits itself (serial regions, postamble), so
	// its embedded-fields map must be populated even when the worker-fork prep
	// runs its own copy; do it before any helper thread can observe `g`.
	g.precompute_embedded_fields()
	parallel_prep_done := g.run_pre_dispatch_parallel(no_parallel)
	if !parallel_prep_done {
		g.collect_fixed_storage_consts()
		g.precompute_param_type_index()
		g.precompute_concrete_optional_abi_fns()
	}
	g.collect_shared_type_names()
	g.collect_interface_impls()
	g.precompute_sum_name_lookup()
	g.preseed_struct_fn_ptr_types()
	g.preseed_global_fn_ptr_types()
	g.preseed_fn_signature_fn_ptr_types()
	g.preseed_c_extern_fn_ptr_types()
	g.preseed_libc_compat_fns()
	if !g.skip_generics {
		g.precompute_generic_method_candidate_index()
	}
	// Decide fixed-array return wrappers before generating function bodies, so
	// signatures, returns and call sites all agree on the wrapped types.
	g.populate_fixed_array_ret_wrappers()
	const_code := g.precompute_consts()
	orig_sb := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(4096)
	g.line_start = true
	g.gen_fns_dispatch(no_parallel)
	fn_code := if g.fn_segs.len == 0 { g.sb.str() } else { '' }
	// `.str()` copies out of the builder; free the emptied backing array under -gc none.
	unsafe { g.sb.free() }
	g.sb = orig_sb
	g.line_start = orig_line_start
	mut known_output_len := g.sb.len + fn_code.len + const_code.len
	for segment in g.post_segs {
		known_output_len += segment.len
	}
	for segment in g.fn_segs {
		known_output_len += segment.len
	}
	// Leave headroom for the small body-dependent supplement emitted below.
	g.sb.ensure_cap(known_output_len + 1024 * 1024)
	if g.post_segs.len == 4 {
		// The body-independent postamble groups were already emitted during the
		// parallel region (emit_overlap_postamble_segments); splice them here in
		// the exact serial order, interleaved with the body-dependent pieces.
		g.sb.write_string(g.post_segs[0])
		// Anything the body workers registered beyond the pre-seeded state
		// (fn-ptr types, optionals) is emitted here — still ahead of every
		// declaration and body that could reference it. fn_ptr_typedefs keeps
		// a persistent emitted set, so with a complete pre-seed it emits
		// nothing; fixed-array and multi-return typedefs derive from the AST
		// and signatures only, which cannot change during the region.
		g.fn_ptr_typedefs()
		g.optional_typedefs()
		g.c_extern_forward_decls()
		g.sb.write_string(g.post_segs[1])
		g.callback_wrapper_decls()
		g.spawn_wrapper_decls()
		g.register_interface_strings()
		g.sb.write_string(g.post_segs[2])
		// Literals interned after the fork snapshot (worker novelties, the
		// synthetic main) — per-id definitions, order-independent.
		g.string_literals_from(g.post_str_lits_snapshot)
		g.sb.write_string(g.post_segs[3])
	} else {
		g.c99_feature_test_macros()
		g.emit_preserved_c_directives()
		g.preamble()
		g.emit_c_directives()
		g.enum_decls()
		g.type_alias_decls()
		g.type_forward_decls()
		// Forward-declare multi-return structs before fn-ptr typedefs, which may name a
		// multi-return as a by-value return type (full bodies come after struct_decls).
		g.multi_return_forward_decls()
		// Bare typedefs for primitive-element fixed arrays and wrapper structs for
		// fixed-array return types, before fn-ptr typedefs (which may name a fixed
		// array in param or return position) and the function declarations.
		g.fixed_array_early_typedefs()
		g.fn_ptr_typedefs()
		g.struct_decls()
		g.fixed_array_typedefs()
		g.multi_return_typedefs()
		g.optional_typedefs()
		g.c_extern_forward_decls()
		g.builtin_abi_decls()
		g.test_failure_helpers()
		g.global_decls()
		g.forward_decls()
		g.cached_header_forward_decls()
		g.interface_method_forward_decls()
		g.shared_dup_fns()
		g.enum_str_forward_decls()
		g.callback_wrapper_decls()
		g.spawn_wrapper_decls()
		g.register_interface_strings()
		g.string_literals()
		if !g.cache_split {
			g.interface_method_stubs()
		}
		g.enum_str_defs()
	}
	g.sb.write_string(const_code)
	// The final builder now owns a copy of the const code.
	unsafe { const_code.free() }
	if g.cache_split {
		g.writeln('/* V3CACHE_BODY_BEGIN */')
		// `_vinit` aggregates module and global initialization for the current
		// entry program, so it must never be retained in a reusable module object.
		g.writeln('/* V3CACHE_MODULE main */')
	}
	if g.const_runtime_inits.len > 0 || g.runtime_inits.len > 0 || g.module_init_fns.len > 0
		|| g.global_inits.len > 0 {
		g.writeln('void _vinit() {')
		mut emitted_const := []bool{len: g.const_runtime_inits.len}
		mut emitted_runtime := []bool{len: g.runtime_inits.len}
		init_fns := g.module_init_fn_map()
		for mod in g.ordered_startup_modules(init_fns) {
			g.emit_runtime_inits_for_module(mod, mut emitted_const, mut emitted_runtime)
			if init_fn := init_fns[mod] {
				g.writeln('\t${init_fn}();')
			}
		}
		g.emit_remaining_runtime_inits(mut emitted_const, mut emitted_runtime)
		g.writeln('}')
		g.writeln('')
	}
	if g.cache_split {
		g.interface_method_stubs()
	}
	if g.fn_segs.len > 0 {
		for segment in g.fn_segs {
			g.sb.write_string(segment)
			unsafe { segment.free() }
		}
		g.fn_segs = []string{}
	} else {
		g.sb.write_string(fn_code)
		// The final builder now owns a copy of the function code.
		unsafe { fn_code.free() }
	}
	if g.cache_split {
		g.writeln('/* V3CACHE_BODY_END */')
	}
	mut result := g.sb.str()
	// Keep only the returned C string, not the builder's copied backing array.
	unsafe { g.sb.free() }
	if g.cache_split {
		result = g.rewrite_cache_string_symbols(result)
	}
	return result
}

fn (mut g FlatGen) rewrite_cache_string_symbols(source string) string {
	mut symbols := []string{cap: g.str_lits.len}
	for value in g.str_lits {
		symbols << cache_string_symbol(value)
	}
	user_c_symbols := g.cache_user_c_string_symbols()
	mut out := strings.new_builder(source.len + g.str_lits.len * 8)
	mut i := 0
	for i < source.len {
		if source[i] in [`"`, `'`] {
			quote := source[i]
			start := i
			i++
			for i < source.len {
				if source[i] == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				i++
				if source[i - 1] == quote {
					break
				}
			}
			out.write_string(source[start..i])
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
			start := i
			i += 2
			for i < source.len && source[i] != `\n` {
				i++
			}
			out.write_string(source[start..i])
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
			start := i
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			if i + 1 < source.len {
				i += 2
			} else {
				i = source.len
			}
			out.write_string(source[start..i])
			continue
		}
		if c_identifier_start(source[i]) {
			start := i
			i++
			for i < source.len && c_identifier_continue(source[i]) {
				i++
			}
			identifier := source[start..i]
			if cache_numbered_string_symbol(identifier) && !user_c_symbols[identifier] {
				mut id := 0
				for digit in identifier[5..].bytes() {
					id = id * 10 + int(digit - `0`)
				}
				if id >= 0 && id < symbols.len {
					out.write_string(symbols[id])
					continue
				}
			}
			out.write_string(identifier)
			continue
		}
		out.write_u8(source[i])
		i++
	}
	return out.str()
}

fn (mut g FlatGen) cache_user_c_string_symbols() map[string]bool {
	mut symbols := map[string]bool{}
	for directive in g.c_directives {
		collect_cache_numbered_string_symbols(directive.text, mut symbols)
	}
	for name in g.inlined_c_fns.keys() {
		collect_cache_numbered_string_symbols(name, mut symbols)
	}
	for name in g.inlined_c_declared_fns.keys() {
		collect_cache_numbered_string_symbols(name, mut symbols)
	}
	referenced_symbols := g.c_extern_referenced_symbols()
	for name in referenced_symbols.keys() {
		collect_cache_numbered_string_symbols(name, mut symbols)
	}
	return symbols
}

fn collect_cache_numbered_string_symbols(source string, mut symbols map[string]bool) {
	mut i := 0
	for i < source.len {
		if source[i] in [`\"`, `'`] {
			quote := source[i]
			i++
			for i < source.len {
				if source[i] == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				i++
				if source[i - 1] == quote {
					break
				}
			}
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
			i += 2
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		}
		if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			if i + 1 < source.len {
				i += 2
			} else {
				i = source.len
			}
			continue
		}
		if !c_identifier_start(source[i]) {
			i++
			continue
		}
		start := i
		i++
		for i < source.len && c_identifier_continue(source[i]) {
			i++
		}
		identifier := source[start..i]
		if cache_numbered_string_symbol(identifier) {
			symbols[identifier] = true
		}
	}
}

fn cache_numbered_string_symbol(identifier string) bool {
	return identifier.len > 5 && identifier.starts_with('_str_')
		&& identifier[5..].bytes().all(it >= `0` && it <= `9`)
}

fn c_identifier_start(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
}

fn c_identifier_continue(c u8) bool {
	return c_identifier_start(c) || (c >= `0` && c <= `9`)
}

fn cache_string_symbol(value string) string {
	mut hash := u64(1469598103934665603)
	for c in value.bytes() {
		hash = (hash ^ u64(c)) * u64(1099511628211)
	}
	return '_v3_lit_${value.len}_${hash.hex()}'
}

// emit_overlap_postamble_segments emits the body-independent postamble groups
// into private builders while the parallel fn-body workers run. Every piece
// here reads only state that is complete before the region starts (collected
// tables, precomputed indexes, interned strings — prepare_parallel_items
// pre-seeds everything workers could add) and writes only master-private
// state, so running them concurrently with the workers is race-free and the
// spliced output is byte-identical to the serial postamble order.
fn (mut g FlatGen) emit_postamble_segments_a() {
	saved_sb := g.sb
	saved_line_start := g.line_start
	// Segment 0: preamble through multi-return typedefs, in the exact serial
	// order (these pieces share emission-dedup state, so their relative order
	// must not change).
	g.sb = strings.new_builder(262144)
	g.line_start = true
	g.c99_feature_test_macros()
	g.emit_preserved_c_directives()
	g.preamble()
	g.emit_c_directives()
	g.enum_decls()
	g.type_alias_decls()
	g.type_forward_decls()
	g.multi_return_forward_decls()
	g.fixed_array_early_typedefs()
	g.fn_ptr_typedefs()
	g.struct_decls()
	g.fixed_array_typedefs()
	g.multi_return_typedefs()
	g.post_segs = [g.sb.str()]
	unsafe { g.sb.free() }
	g.sb = saved_sb
	g.line_start = saved_line_start
}

fn (mut g FlatGen) emit_postamble_segments_b() {
	saved_sb := g.sb
	saved_line_start := g.line_start
	mut segs := []string{cap: 3}
	// Segment 1: ABI decls, globals and fn forward decls.
	g.sb = strings.new_builder(262144)
	g.line_start = true
	g.builtin_abi_decls()
	g.test_failure_helpers()
	g.global_decls()
	g.forward_decls()
	g.cached_header_forward_decls()
	g.interface_method_forward_decls()
	g.shared_dup_fns()
	g.enum_str_forward_decls()
	segs << g.sb.str()
	// Segment 2: interned string literal table (fixed before the region:
	// prepare_parallel_items pre-seeds every string the workers reference).
	g.sb = strings.new_builder(262144)
	g.line_start = true
	g.string_literals()
	segs << g.sb.str()
	// Segment 3: interface dispatch and enum string definitions.
	g.sb = strings.new_builder(65536)
	g.line_start = true
	if !g.cache_split {
		g.interface_method_stubs()
	}
	g.enum_str_defs()
	segs << g.sb.str()
	unsafe { g.sb.free() }
	g.sb = saved_sb
	g.line_start = saved_line_start
	g.post_segs = segs
	// The fused item walk normally collected C-extern references already. The
	// fallback here covers serial generation and small programs.
	_ = g.c_extern_referenced_symbols()
}

// node_kind_id supports node kind id handling for c.
@[inline]
fn node_kind_id(node flat.Node) int {
	mut kind_id := node.kind_id
	if kind_id == 0 && int(node.kind) != 0 {
		kind_id = int(node.kind)
	}
	return kind_id
}

// collect_gen_info updates collect gen info state for c.
fn (mut g FlatGen) collect_gen_info() {
	g.collect_c_flags_from_directives()
	g.use_system_stdint = g.translation_unit_uses_inttypes()
	mut cur_module := 'main'
	mut cur_file := ''
	mut seen_import_in_file := false
	mut nonshared_fn_short_names := []string{cap: 1024}
	mut nonshared_fn_full_names := []string{cap: 1024}
	mut nonshared_fn_file_ranks := []int{cap: 1024}
	mut nonshared_fn_node_indexes := []int{cap: 1024}
	mut canonical_shared_fn_short_names := map[string]bool{}
	mut preferred_shared_fn_file_ranks := map[string]int{}
	mut preferred_shared_fn_node_indexes := map[string]int{}
	mut preferred_shared_fn_params := map[string][]bool{}
	for node_idx in 0 .. g.a.nodes.len {
		node := g.a.nodes[node_idx]
		if node.kind == .string_literal {
			g.intern_string(node.value)
		} else if node.kind == .string_interp && node.children_count == 0 {
			g.intern_string('')
		}
		match node.kind {
			.file, .module_decl, .fn_decl, .struct_decl, .global_decl, .const_decl, .enum_decl,
			.interface_decl, .import_decl, .directive {}
			else {
				continue
			}
		}

		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			g.note_compiler_source_file(node.value)
			cur_module = 'main'
			g.tc.cur_module = cur_module
			g.tc.cur_file = cur_file
			seen_import_in_file = false
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 61 {
			full_name := qualify_name_in_module(cur_module, node.value)
			if g.skip_generics && g.has_used_fn_filter()
				&& !g.used_fn_contains_in_module(node.value, cur_module) {
				g.preseed_unused_fn_ptr_param_types(node, cur_module, cur_file)
				continue
			}
			g.register_fn_decl_node(node.value, cur_module, flat.NodeId(node_idx))
			typed_params := g.fn_node_param_types(node, cur_module)
			mut ptypes := []types.Type{cap: int(node.children_count)}
			mut shared_params := []bool{}
			mut decl_is_variadic := false
			mut first_param_is_mut := false
			mut seen_param := false
			mut param_idx := 0
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			for i in 0 .. node.children_count {
				child := g.a.child_node(&node, i)
				if node_kind_id(child) == 75 {
					if child.typ.starts_with('...') {
						decl_is_variadic = true
					}
					raw_pt := g.tc.parse_type(child.typ)
					mut pt := raw_pt
					if raw_pt !is types.Pointer && param_idx < typed_params.len {
						pt = typed_params[param_idx]
					}
					mut is_shared_param := false
					if child.typ.len > 0 && child.typ[0] in [`s`, ` `, `\t`, `\n`, `\r`] {
						if _ := shared_inner_type_text(child.typ) {
							is_shared_param = true
						}
					}
					if is_shared_param {
						for shared_params.len <= param_idx {
							shared_params << false
						}
						shared_params[param_idx] = true
					} else if shared_params.len > 0 {
						shared_params << false
					}
					param_idx++
					if !seen_param {
						first_param_is_mut = child.is_mut || raw_pt is types.Pointer
							|| pt is types.Pointer || child.typ.starts_with('&')
							|| child.typ.starts_with('mut ')
						seen_param = true
					}
					ptypes << pt
					if pt is types.FnType {
						ct := g.tc.c_type(pt)
						g.resolve_fn_ptr_type(ct)
					}
				}
			}
			ptypes = g.fn_param_types_with_implicit_veb_ctx(node, ptypes)
			if shared_params.len > 0 {
				shared_params = g.fn_shared_params_with_implicit_veb_ctx(node, shared_params)
			}
			g.register_fn_decl_param_types(node.value, full_name, ptypes, decl_is_variadic)
			if shared_params.len > 0 {
				g.register_fn_decl_shared_params(node.value, full_name, shared_params)
				file_rank := c_backend_fn_file_rank(cur_file)
				if full_name !in preferred_shared_fn_file_ranks
					|| file_rank > preferred_shared_fn_file_ranks[full_name] {
					preferred_shared_fn_file_ranks[full_name] = file_rank
					preferred_shared_fn_node_indexes[full_name] = node_idx
					preferred_shared_fn_params[full_name] = shared_params.clone()
				}
				if cur_module.len == 0 || cur_module == 'main' || cur_module == 'builtin' {
					canonical_shared_fn_short_names[node.value] = true
				}
			} else {
				nonshared_fn_short_names << node.value
				nonshared_fn_full_names << full_name
				nonshared_fn_file_ranks << c_backend_fn_file_rank(cur_file)
				nonshared_fn_node_indexes << node_idx
			}
			g.register_fn_decl_mut_receiver(node.value, full_name, first_param_is_mut)
			g.register_fn_decl_ret_type(node.value, full_name, node.typ)
			// Module-level `init()` functions run once at startup. Collect their C
			// names so _vinit can invoke them (V semantics).
			if node.value == 'init' && ptypes.len == 0
				&& (!g.has_used_fn_filter() || g.used_fn_contains_in_module(node.value, cur_module)) {
				init_cname := g.qualified_fn_name_in_module_c(cur_module, 'init')
				if init_cname !in g.module_init_fns {
					g.module_init_fns << init_cname
				}
				g.module_init_fn_modules[init_cname] = cur_module
			}
			continue
		}
		if g.collect_c_directive(cur_module, node, cur_file, !seen_import_in_file) {
			continue
		}
		if node.kind == .directive && node.value == 'flag' {
			continue
		}
		if node.kind == .directive && node.value == 'pkgconfig' {
			continue
		}
		if kind_id == 62 {
			full_name := qualify_name_in_module(cur_module, node.value)
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			g.register_struct_decl_info(node.value, full_name, cur_module, cur_file, node)
			continue
		}
		if kind_id == 64 {
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			for i in 0 .. node.children_count {
				f := g.a.child_node(&node, i)
				if f.value.starts_with('C.') {
					continue
				}
				mut ft := g.tc.parse_type(f.typ)
				if ft is types.Void && f.children_count > 0 {
					ft = g.tc.resolve_type(g.a.child(f, 0))
				}
				qname := qualify_name_in_module(cur_module, f.value)
				g.global_types[qname] = ft
				g.global_modules[f.value] = cur_module
				g.global_modules[qname] = cur_module
				g.global_files[qname] = cur_file
				if f.children_count > 0 {
					val_id := g.a.child(f, 0)
					if int(val_id) >= 0 {
						g.global_inits[qname] = val_id
						g.global_init_order << qname
					}
				}
				g.tc.file_scope.insert(f.value, ft)
				if qname != f.value {
					g.tc.file_scope.insert(qname, ft)
				}
			}
			continue
		}
		if kind_id == 67 {
			is_flag := enum_decl_is_flag(node)
			mut val := 0
			enum_name := qualify_name_in_module(cur_module, node.value)
			backing := enum_decl_backing_type(node) or { '' }
			if backing.len > 0 {
				g.register_enum_backing_info(enum_name, backing)
			}
			is_backed_enum := backing.len > 0
			mut field_exprs := map[string]flat.NodeId{}
			for i in 0 .. node.children_count {
				f := g.a.child_node(&node, i)
				if f.children_count > 0 {
					field_exprs[f.value] = g.a.child(f, 0)
				}
			}
			mut field_values := map[string]i64{}
			for i in 0 .. node.children_count {
				f := g.a.child_node(&node, i)
				if f.children_count > 0 {
					mut resolving := map[string]bool{}
					if enum_val := g.enum_field_expr_value_with_enum(g.a.child(f, 0), cur_module,
						node.value, mut field_values, field_exprs, mut resolving)
					{
						val = int(enum_val)
					}
				}
				key := '${enum_name}.${f.value}'
				if is_backed_enum {
					g.enum_value_exprs[key] = '${g.cname(enum_name)}__${g.cname(f.value)}'
					val++
				} else if is_flag {
					g.enum_vals[key] = 1 << val
					field_values[f.value] = i64(val)
					val++
				} else {
					g.enum_vals[key] = val
					field_values[f.value] = val
					val++
				}
			}
			continue
		}
		if kind_id == 70 {
			iface_name := qualify_name_in_module(cur_module, node.value)
			g.interfaces[iface_name] = g.tc.interface_abstract_method_names(iface_name)
			continue
		}
		if kind_id == 65 {
			for i in 0 .. node.children_count {
				f := g.a.child_node(&node, i)
				if node_kind_id(f) == 66 && f.children_count > 0 {
					qname := g.const_storage_name(cur_module, f.value)
					g.const_vals[qname] = g.a.child(f, 0)
					g.const_modules[qname] = cur_module
					g.const_files[qname] = cur_file
					if (cur_module.len == 0 || cur_module == 'main' || cur_module == 'builtin')
						&& f.value !in g.const_vals {
						g.const_vals[f.value] = g.a.child(f, 0)
						g.const_modules[f.value] = cur_module
						g.const_files[f.value] = cur_file
					}
				}
			}
			continue
		}
		if kind_id == 72 {
			seen_import_in_file = true
			alias := node.typ.clone()
			mod_name := node.value.clone()
			if alias.len > 0 && mod_name.len > 0 {
				g.modules[alias] = mod_name
			}
			if cur_module.len > 0 && mod_name.len > 0 {
				dep_module := mod_name
				if cur_module !in g.module_imports {
					g.module_imports[cur_module] = []string{}
				}
				if dep_module !in g.module_imports[cur_module] {
					g.module_imports[cur_module] << dep_module
				}
			}
			continue
		}
	}
	if g.has_shared_params {
		for full_name, flags in preferred_shared_fn_params {
			g.fn_decl_shared_params[full_name] = flags.clone()
			g.fn_decl_shared_params[g.cname(full_name)] = flags.clone()
		}
		for i, name in nonshared_fn_short_names {
			if name in g.fn_decl_shared_params {
				full_name := nonshared_fn_full_names[i]
				mut nonshared_is_preferred := full_name !in preferred_shared_fn_file_ranks
				if !nonshared_is_preferred {
					shared_rank := preferred_shared_fn_file_ranks[full_name]
					nonshared_rank := nonshared_fn_file_ranks[i]
					nonshared_is_preferred = nonshared_rank > shared_rank
						|| (nonshared_rank == shared_rank
						&& nonshared_fn_node_indexes[i] < preferred_shared_fn_node_indexes[full_name])
				}
				if nonshared_is_preferred {
					g.fn_decl_shared_params[full_name] = []bool{}
					g.fn_decl_shared_params[g.cname(full_name)] = []bool{}
				}
				if !canonical_shared_fn_short_names[name] {
					g.fn_decl_shared_params[name] = []bool{}
					cname := g.cname(name)
					if cname != name {
						g.fn_decl_shared_params[cname] = []bool{}
					}
				}
			}
		}
	}
	g.modules['strings'] = 'strings'
	g.collect_const_init_order_from_files()
}

fn (mut g FlatGen) preseed_unused_fn_ptr_param_types(node flat.Node, module_name string, file string) {
	typed_params := g.fn_node_param_types(node, module_name)
	g.tc.cur_module = module_name
	g.tc.cur_file = file
	mut param_idx := 0
	for i in 0 .. node.children_count {
		child := g.a.child_node(&node, i)
		if node_kind_id(child) != 75 {
			continue
		}
		raw_type := g.tc.parse_type(child.typ)
		param_type := if raw_type !is types.Pointer && param_idx < typed_params.len {
			typed_params[param_idx]
		} else {
			raw_type
		}
		param_idx++
		if param_type is types.FnType {
			g.resolve_fn_ptr_type(g.tc.c_type(param_type))
		}
	}
}

fn (mut g FlatGen) collect_c_flags_from_directives() {
	mut cur_file := ''
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			g.note_compiler_source_file(node.value)
			continue
		}
		if node.kind != .directive || node.typ.len == 0 {
			continue
		}
		if node.value == 'flag' {
			flag := c_flag_arg(node.typ, g.compiler_vroot, cur_file)
			if flag.len > 0 && flag !in g.c_flags {
				g.c_flags << flag
			}
			continue
		}
		if node.value == 'pkgconfig' {
			for flag in c_pkgconfig_flags(node.typ) {
				if flag.len > 0 && flag !in g.c_flags {
					g.c_flags << flag
				}
			}
		}
	}
}

fn (g &FlatGen) translation_unit_uses_inttypes() bool {
	mut cur_file := ''
	include_dirs := c_flag_include_dirs(g.c_flags)
	for node in g.a.nodes {
		if node_kind_id(node) == 77 {
			cur_file = node.value
			continue
		}
		if node.kind != .directive || node.value !in ['include', 'insert'] || node.typ.len == 0 {
			continue
		}
		include_arg := c_include_arg(node.typ, g.compiler_vroot, cur_file)
		if trimmed_space(include_arg) == '<inttypes.h>' {
			return true
		}
		mut seen := map[string]bool{}
		for path in c_include_file_paths(include_arg, g.compiler_vroot, cur_file, include_dirs) {
			if c_inline_header_tree_uses_inttypes(path, g.compiler_vroot, include_dirs, mut seen) {
				return true
			}
		}
	}
	return false
}

fn (mut g FlatGen) collect_c_directive(module_name string, node flat.Node, source_file string, before_import bool) bool {
	if node.kind != .directive {
		return false
	}
	if node.value in ['include', 'insert'] {
		if node.typ.len == 0 {
			return true
		}
		include_arg := c_include_arg(node.typ, g.compiler_vroot, source_file)
		if include_arg.len == 0 {
			return true
		}
		// These helper headers are superseded by the inline compiler helpers emitted in
		// builtin_abi_decls(); also including them would redefine the helpers.
		if include_arg.contains('prealloc_atomics.h') || include_arg.contains('filelock_helpers.h')
			|| include_arg.contains('stdatomic') {
			return true
		}
		include_dirs := c_flag_include_dirs(g.c_flags)
		if header := c_inline_header_text(include_arg, g.compiler_vroot, source_file, include_dirs,
			g.use_system_stdint)
		{
			header_text := header.text
			g.collect_inlined_c_structs(header_text)
			g.collect_inlined_c_fns(header_text)
			g.collect_inlined_c_declared_fns(header_text)
			g.collect_preserved_c_fns(header.preserved_c_fns)
			g.collect_preserved_c_structs(header.preserved_c_structs)
			for directive in header.preserved_directives {
				g.add_c_directive(module_name, directive, before_import)
			}
			if header_text.len > 0 {
				g.add_c_directive(module_name, header_text, before_import)
			}
		} else if c_should_preserve_uninlined_include(include_arg) || (g.cache_split
			&& include_arg in ['<mach/mach.h>', '<mach/task.h>', '<mach/mach_time.h>']) {
			g.collect_preserved_c_fns(c_preserved_system_include_declared_fns(include_arg))
			g.collect_preserved_c_structs(c_preserved_system_include_struct_names(include_arg))
			g.add_c_directive(module_name, '#include ${include_arg}', before_import)
		}
		return true
	}
	if node.value in ['define', 'undef', 'ifdef', 'ifndef', 'if', 'elif', 'else', 'endif', 'pragma',
		'error', 'warning'] {
		g.add_c_directive(module_name, c_preprocessor_directive_line(node.value, node.typ),
			before_import)
		return true
	}
	return false
}

fn c_inline_header_text(include_arg string, vroot string, source_file string, include_dirs []string, translation_unit_uses_inttypes bool) ?CInlineHeader {
	if replacement := c_system_include_replacement(include_arg, translation_unit_uses_inttypes) {
		return CInlineHeader{
			text: replacement
		}
	}
	mut seen := map[string]bool{}
	mut inlining := map[string]bool{}
	for path in c_include_file_paths(include_arg, vroot, source_file, include_dirs) {
		mut scan_seen := map[string]bool{}
		use_system_stdint := translation_unit_uses_inttypes
			|| c_inline_header_tree_uses_inttypes(path, vroot, include_dirs, mut scan_seen)
		if header := c_inline_header_file(path, vroot, include_dirs, false, use_system_stdint, mut
			seen, mut inlining)
		{
			return header
		}
	}
	return none
}

fn c_inline_header_file(path string, vroot string, include_dirs []string, conditional bool, use_system_stdint bool, mut seen map[string]bool, mut inlining map[string]bool) ?CInlineHeader {
	if path.len == 0 || !os.exists(path) {
		return none
	}
	real_path := os.real_path(path)
	if seen[real_path] || inlining[real_path] {
		return CInlineHeader{}
	}
	// A header first reached inside a false `#if` region would be invisible to
	// the C preprocessor, so only an unconditional inline may suppress later
	// copies; include guards make the re-emission a no-op.
	if !conditional {
		seen[real_path] = true
	}
	text := os.read_file(real_path) or { return none }
	inlining[real_path] = true
	header := c_inline_header_file_text(text, vroot, real_path, include_dirs, conditional,
		use_system_stdint, mut seen, mut inlining)
	inlining.delete(real_path)
	return header
}

fn c_inline_header_file_text(text string, vroot string, source_file string, include_dirs []string, conditional bool, use_system_stdint bool, mut seen map[string]bool, mut inlining map[string]bool) CInlineHeader {
	guard_name := c_header_guard_name(text)
	mut lines := []string{}
	mut preserved_directives := []string{}
	mut preserved_c_fns := []string{}
	mut preserved_c_structs := []string{}
	mut include_context := []string{}
	mut include_prefix := []string{}
	mut in_block_comment := false
	for line in text.split_into_lines() {
		clean, next_in_block_comment := c_preprocessor_directive_scan_line(line, in_block_comment)
		in_block_comment = next_in_block_comment
		if c_directive_name(clean) == 'include' {
			include_arg := c_include_arg(c_directive_arg(clean), vroot, source_file)
			if replacement := c_system_include_replacement(include_arg, use_system_stdint) {
				lines << replacement
				continue
			}
			nested_conditional := conditional
				|| !c_include_context_is_guard_only(include_context, guard_name)
			mut inlined := false
			for path in c_include_file_paths(include_arg, vroot, source_file, include_dirs) {
				if nested := c_inline_header_file(path, vroot, include_dirs, nested_conditional,
					use_system_stdint, mut seen, mut inlining)
				{
					if nested.text.len > 0 {
						lines << nested.text
					}
					for directive in nested.preserved_directives {
						preserved_directives << c_wrap_preserved_nested_directive(directive,
							include_context, include_prefix)
					}
					preserved_c_fns << nested.preserved_c_fns
					preserved_c_structs << nested.preserved_c_structs
					inlined = true
					break
				}
			}
			if !inlined && c_include_should_remain_in_inlined_text(include_arg) {
				lines << '#include ${include_arg}'
				preserved_c_fns << c_preserved_system_include_declared_fns(include_arg)
				preserved_c_structs << c_preserved_system_include_struct_names(include_arg)
			} else if !inlined && c_should_preserve_uninlined_include(include_arg) {
				preserved_directives << c_preserved_nested_include_directive(include_arg,
					include_context, include_prefix)
				preserved_c_fns << c_preserved_system_include_declared_fns(include_arg)
				preserved_c_structs << c_preserved_system_include_struct_names(include_arg)
			}
			continue
		}
		lines << line
		c_update_nested_include_context(clean, line, mut include_context)
		c_update_nested_include_prefix(clean, line, mut include_prefix)
	}
	return CInlineHeader{
		text:                 lines.join('\n')
		preserved_directives: preserved_directives
		preserved_c_fns:      preserved_c_fns
		preserved_c_structs:  preserved_c_structs
	}
}

// c_inline_header_tree_uses_inttypes detects whether an inlined include tree needs the real
// inttypes/stdint pair. Mixing the synthetic stdint typedefs with libc's inttypes header can
// redefine exact-width types with a different underlying C type on LP64 targets.
fn c_inline_header_tree_uses_inttypes(path string, vroot string, include_dirs []string, mut seen map[string]bool) bool {
	if path.len == 0 || !os.exists(path) {
		return false
	}
	real_path := os.real_path(path)
	if seen[real_path] {
		return false
	}
	seen[real_path] = true
	text := os.read_file(real_path) or { return false }
	mut in_block_comment := false
	for line in text.split_into_lines() {
		clean, next_in_block_comment := c_preprocessor_directive_scan_line(line, in_block_comment)
		in_block_comment = next_in_block_comment
		if c_directive_name(clean) != 'include' {
			continue
		}
		include_arg := c_include_arg(c_directive_arg(clean), vroot, real_path)
		if trimmed_space(include_arg) == '<inttypes.h>' {
			return true
		}
		for nested_path in c_include_file_paths(include_arg, vroot, real_path, include_dirs) {
			if c_inline_header_tree_uses_inttypes(nested_path, vroot, include_dirs, mut seen) {
				return true
			}
		}
	}
	return false
}

fn c_preprocessor_directive_scan_line(line string, in_block_comment bool) (string, bool) {
	mut in_comment := in_block_comment
	mut i := 0
	for i < line.len {
		if in_comment {
			end_rel := line[i..].index('*/') or { return '', true }
			i += end_rel + 2
			in_comment = false
			continue
		}
		if i + 1 < line.len && line[i] == `/` && line[i + 1] == `*` {
			in_comment = true
			i += 2
			continue
		}
		if i + 1 < line.len && line[i] == `/` && line[i + 1] == `/` {
			return '', in_comment
		}
		if line[i].is_space() {
			i++
			continue
		}
		if line[i] == `#` {
			return trimmed_space(line[i..]), in_comment
		}
		return '', in_comment
	}
	return '', in_comment
}

// c_header_guard_name returns the macro of a classic `#ifndef X` / `#define X`
// include guard when it opens the header, or '' when there is no such guard.
fn c_header_guard_name(text string) string {
	mut in_block_comment := false
	mut guard := ''
	for line in text.split_into_lines() {
		clean, next_in_block_comment := c_preprocessor_directive_scan_line(line, in_block_comment)
		in_block_comment = next_in_block_comment
		name := c_directive_name(clean)
		if name.len == 0 {
			continue
		}
		if guard.len == 0 {
			if name != 'ifndef' {
				return ''
			}
			guard = c_directive_arg(clean)
			if guard.len == 0 {
				return ''
			}
			continue
		}
		if name == 'define' {
			arg := c_directive_arg(clean)
			if arg == guard || arg.starts_with(guard + ' ') {
				return guard
			}
		}
		return ''
	}
	return ''
}

// c_include_context_is_guard_only reports whether the active `#if` context at an
// include site consists of nothing but the header's own include guard, i.e. the
// include is unconditionally reachable when the header itself is.
fn c_include_context_is_guard_only(context []string, guard_name string) bool {
	if context.len == 0 {
		return true
	}
	if context.len != 1 || guard_name.len == 0 {
		return false
	}
	clean := trimmed_space(context[0])
	return c_directive_name(clean) == 'ifndef' && c_directive_arg(clean) == guard_name
}

fn c_update_nested_include_context(clean string, line string, mut context []string) {
	if context.len > 0 && c_line_has_continuation(context[context.len - 1]) {
		context[context.len - 1] += '\n${line}'
		return
	}
	match c_directive_name(clean) {
		'if', 'ifdef', 'ifndef', 'elif', 'else' {
			context << line
		}
		'endif' {
			for context.len > 0 {
				name := c_directive_name(context[context.len - 1].trim_space())
				context.delete_last()
				if name in ['if', 'ifdef', 'ifndef'] {
					break
				}
			}
		}
		else {}
	}
}

fn c_update_nested_include_prefix(clean string, line string, mut prefix []string) {
	if prefix.len > 0 && c_line_has_continuation(prefix[prefix.len - 1]) {
		prefix[prefix.len - 1] += '\n${line}'
		return
	}
	match c_directive_name(clean) {
		'define', 'undef' {
			prefix << line
		}
		else {
			prefix.clear()
		}
	}
}

fn c_wrap_preserved_nested_directive(directive string, context []string, prefix []string) string {
	if context.len == 0 && prefix.len == 0 {
		return directive
	}
	mut lines := context.clone()
	lines << prefix
	lines << directive
	for _ in 0 .. c_nested_include_context_depth(context) {
		lines << '#endif'
	}
	return lines.join('\n')
}

fn c_line_has_continuation(line string) bool {
	return line.trim_right(' \t\r').ends_with('\\')
}

fn c_preserved_nested_include_directive(include_arg string, context []string, prefix []string) string {
	if context.len == 0 && prefix.len == 0 {
		return '#include ${include_arg}'
	}
	mut lines := context.clone()
	lines << prefix
	lines << '#include ${include_arg}'
	for _ in 0 .. c_nested_include_context_depth(context) {
		lines << '#endif'
	}
	return lines.join('\n')
}

fn c_nested_include_context_depth(context []string) int {
	mut depth := 0
	for line in context {
		if c_directive_name(line.trim_space()) in ['if', 'ifdef', 'ifndef'] {
			depth++
		}
	}
	return depth
}

fn c_flag_include_dirs(flags []string) []string {
	mut dirs := []string{}
	for flag in flags {
		tokens := tokenize_c_flag(flag)
		mut i := 0
		for i < tokens.len {
			tok := tokens[i]
			mut dir := ''
			if tok in ['-I', '-isystem'] {
				if i + 1 < tokens.len {
					dir = tokens[i + 1]
					i++
				}
			} else if tok.starts_with('-I') && tok.len > 2 {
				dir = tok[2..]
			} else if tok.starts_with('-isystem') && tok.len > '-isystem'.len {
				dir = tok['-isystem'.len..].trim_left('=')
			}
			dir = dir.trim('"\'')
			if dir.len > 0 && dir !in dirs {
				dirs << dir
			}
			i++
		}
	}
	return dirs
}

fn c_system_include_replacement(include_arg string, use_system_stdint bool) ?string {
	match trimmed_space(include_arg) {
		'<stdint.h>' {
			if use_system_stdint {
				return '#include <stdint.h>'
			}
			return c_stdint_header_text()
		}
		'<inttypes.h>' {
			// PRI*/SCN* format macros are implementation-specific; keep the real
			// standard header when it is referenced by an inlined C header.
			return '#include <inttypes.h>'
		}
		'<assert.h>' {
			// headerless build has no runtime assert support; NDEBUG semantics
			return '#ifndef assert\n#define assert(e) ((void)0)\n#endif'
		}
		else {
			return none
		}
	}
}

fn c_should_preserve_uninlined_include(include_arg string) bool {
	clean := trimmed_space(include_arg)
	if clean.len == 0 {
		return false
	}
	if clean[0] == `<` {
		return false
	}
	return true
}

fn c_include_should_remain_in_inlined_text(include_arg string) bool {
	clean := trimmed_space(include_arg)
	if clean.len == 0 {
		return false
	}
	if clean[0] == `"` {
		return true
	}
	// System headers whose macros have per-OS values (RTLD_*, CHAR_BIT) cannot be
	// replaced by inline declarations; keep the include in place inside its #if
	// context.
	return clean in ['<dlfcn.h>', '<limits.h>', '<arm_neon.h>']
}

fn c_preserved_system_include_declared_fns(include_arg string) []string {
	if include_arg in ['<mach/mach.h>', '<mach/task.h>', '<mach/mach_time.h>'] {
		return [
			'host_page_size',
			'host_statistics64',
			'mach_absolute_time',
			'mach_host_self',
			'mach_port_deallocate',
			'mach_task_self',
			'mach_timebase_info',
			'task_info',
		]
	}
	return []string{}
}

fn c_preserved_system_include_struct_names(include_arg string) []string {
	if include_arg in ['<mach/mach.h>', '<mach/task.h>', '<mach/mach_time.h>'] {
		return [
			'host_t',
			'mach_timebase_info_data_t',
			'task_basic_info',
			'task_t',
			'vm_size_t',
			'vm_statistics64_data_t',
		]
	}
	return []string{}
}

const c_cache_system_header_declared_fns = {
	'host_page_size':       true
	'host_statistics64':    true
	'mach_absolute_time':   true
	'mach_host_self':       true
	'mach_port_deallocate': true
	'mach_task_self':       true
	'mach_timebase_info':   true
	'task_info':            true
}

const c_cache_system_header_struct_names = {
	'host_t':                    true
	'mach_timebase_info_data_t': true
	'task_basic_info':           true
	'task_t':                    true
	'vm_size_t':                 true
	'vm_statistics64_data_t':    true
}

fn c_stdint_header_text() string {
	return '#if !defined(__V_HEADERLESS_STDINT_H) && !defined(_STDINT_H) && !defined(_STDINT_H_) && !defined(_STDINT) && !defined(_STDINT_H_INCLUDED) && !defined(_GCC_STDINT_H) && !defined(_MSC_STDINT_H_)
#define __V_HEADERLESS_STDINT_H
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
#ifndef _INTMAX_T
#define _INTMAX_T
#if defined(__APPLE__) && defined(__LP64__)
typedef long intmax_t;
#else
typedef long long intmax_t;
#endif
#endif
#ifndef _UINTMAX_T
#define _UINTMAX_T
#if defined(__APPLE__) && defined(__LP64__)
typedef unsigned long uintmax_t;
#else
typedef unsigned long long uintmax_t;
#endif
#endif
#ifndef INTMAX_MAX
#define INTMAX_MAX 9223372036854775807LL
#endif
#ifndef UINTMAX_MAX
#define UINTMAX_MAX 18446744073709551615ULL
#endif
#ifndef INT8_MIN
#define INT8_MIN (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN (-32767 - 1)
#endif
#ifndef INT32_MIN
#define INT32_MIN (-2147483647 - 1)
#endif
#ifndef INT64_MIN
#define INT64_MIN (-9223372036854775807LL - 1)
#endif
#ifndef INT8_MAX
#define INT8_MAX 127
#endif
#ifndef INT16_MAX
#define INT16_MAX 32767
#endif
#ifndef INT32_MAX
#define INT32_MAX 2147483647
#endif
#ifndef INT64_MAX
#define INT64_MAX 9223372036854775807LL
#endif
#ifndef UINT8_MAX
#define UINT8_MAX 255U
#endif
#ifndef UINT16_MAX
#define UINT16_MAX 65535U
#endif
#ifndef UINT32_MAX
#define UINT32_MAX 4294967295U
#endif
#ifndef UINT64_MAX
#define UINT64_MAX 18446744073709551615ULL
#endif
#ifndef INT32_C
#define INT32_C(c) c
#endif
#ifndef UINT32_C
#define UINT32_C(c) c ## U
#endif
#ifndef INT64_C
#define INT64_C(c) c ## LL
#endif
#ifndef UINT64_C
#define UINT64_C(c) c ## ULL
#endif
#endif'
}

fn (mut g FlatGen) collect_inlined_c_structs(text string) {
	for line in text.split_into_lines() {
		clean := trimmed_space(line)
		mut rest := ''
		mut requires_body := false
		if clean.starts_with('typedef struct ') {
			rest = clean['typedef struct '.len..]
		} else if clean.starts_with('typedef union ') {
			rest = clean['typedef union '.len..]
		} else if clean.starts_with('struct ') {
			rest = clean['struct '.len..]
			requires_body = true
		} else if clean.starts_with('union ') {
			rest = clean['union '.len..]
			requires_body = true
		} else {
			continue
		}
		tag := c_header_struct_tag(rest)
		if tag.len == 0 {
			continue
		}
		if requires_body {
			// Only an actual definition (`struct tm {` or `struct tm` with the
			// brace on the next line) may suppress the headerless fallback
			// body; `struct tm *fn(...)` declarations and `struct tm;` forward
			// declarations leave the type incomplete.
			after := trimmed_space(rest[tag.len..])
			if after.len > 0 && after[0] != `{` {
				continue
			}
		}
		g.inlined_c_structs[tag] = true
	}
	for alias in c_typedef_struct_aliases(text) {
		g.inlined_c_structs[alias] = true
		g.inlined_c_typedef_names[alias] = true
	}
	for alias in c_typedef_union_aliases(text) {
		g.inlined_c_structs[alias] = true
		g.inlined_c_typedef_names[alias] = true
	}
	for alias in c_typedef_fn_aliases(text) {
		g.inlined_c_structs[alias] = true
		g.inlined_c_typedef_names[alias] = true
	}
}

// c_typedef_fn_aliases collects the alias names of function typedefs such as
// `typedef int mbedtls_ssl_send_t(void *ctx, ...)` and function-pointer
// typedefs such as `typedef int (*cb_t)(void)`. V sources may declare these as
// opaque `struct C.name {}`, and emitting a `typedef struct name name;` guess
// would clash with the real typedef from the inlined header.
fn c_typedef_fn_aliases(text string) []string {
	mut aliases := []string{}
	for line in text.split_into_lines() {
		clean := trimmed_space(line)
		if !clean.starts_with('typedef ') {
			continue
		}
		rest := clean['typedef '.len..]
		if rest.starts_with('struct ') || rest.starts_with('union ') || rest.starts_with('enum ') {
			continue
		}
		paren := rest.index_u8(`(`)
		if paren <= 0 {
			continue
		}
		mut name := ''
		after := trimmed_space(rest[paren + 1..])
		if after.starts_with('*') {
			// `typedef int (*name)(args)`
			inner := trimmed_space(after[1..])
			end := inner.index_u8(`)`)
			if end > 0 {
				name = trimmed_space(inner[..end])
			}
		} else {
			// `typedef int name(args)` - the alias directly precedes `(`
			before := trimmed_space(rest[..paren])
			mut start_idx := before.len
			for start_idx > 0 && c_ident_char(before[start_idx - 1]) {
				start_idx--
			}
			if start_idx > 0 && start_idx < before.len {
				name = before[start_idx..]
			}
		}
		if name.len > 0 && c_header_struct_tag(name) == name {
			aliases << name
		}
	}
	return aliases
}

fn (mut g FlatGen) collect_inlined_c_fns(text string) {
	mut pending_static := false
	mut pending_definition := ''
	for line in text.split_into_lines() {
		clean := trimmed_space(line)
		if clean.len == 0 {
			continue
		}
		if pending_definition.len > 0 {
			if clean.starts_with('{') {
				g.inlined_c_fns[pending_definition] = true
				pending_definition = ''
				continue
			}
			if clean.ends_with(';') || clean.starts_with('#') {
				pending_definition = ''
			}
		}
		if clean.starts_with('static ') {
			name := c_header_fn_name(clean)
			if name.len > 0 {
				g.inlined_c_fns[name] = true
				pending_static = false
			} else {
				pending_static = c_static_fn_prefix_can_continue(clean)
			}
			continue
		}
		if pending_static {
			name := c_header_fn_name(clean)
			if name.len > 0 {
				g.inlined_c_fns[name] = true
				pending_static = false
				continue
			}
			if clean.ends_with(';') || clean.contains('{') || clean.starts_with('#') {
				pending_static = false
			}
		}
		name := c_header_defined_fn_name(clean)
		if name.len == 0 {
			continue
		}
		if clean.contains('{') {
			g.inlined_c_fns[name] = true
		} else {
			pending_definition = name
		}
	}
}

// c_strip_comments removes block and line comments so declaration scanning
// cannot be misled by prose in doc comments (e.g. an unbalanced `(`), while
// keeping line numbers stable for multi-line declaration accumulation.
fn c_strip_comments(text string) string {
	mut sb := strings.new_builder(text.len)
	mut i := 0
	mut in_block := false
	for i < text.len {
		c := text[i]
		if in_block {
			if c == `*` && i + 1 < text.len && text[i + 1] == `/` {
				in_block = false
				i += 2
				continue
			}
			if c == `\n` {
				sb.write_u8(`\n`)
			}
			i++
			continue
		}
		if c == `/` && i + 1 < text.len {
			if text[i + 1] == `*` {
				in_block = true
				i += 2
				continue
			}
			if text[i + 1] == `/` {
				for i < text.len && text[i] != `\n` {
					i++
				}
				continue
			}
		}
		sb.write_u8(c)
		i++
	}
	return sb.str()
}

fn (mut g FlatGen) collect_inlined_c_declared_fns(text string) {
	// Header declarations often span several lines (one parameter per line);
	// accumulate a pending declaration until its terminating `;` so those are
	// collected too, not just single-line prototypes.
	mut pending := ''
	for line in c_strip_comments(text).split_into_lines() {
		clean := line.trim_space()
		if clean.len > 0 && clean[0] == `#` && c_directive_name(clean) == 'define' {
			// Any macro (object- or function-like) named like a `fn C.x` makes
			// an emitted extern prototype wrong after preprocessing; the
			// header's definition is authoritative.
			arg := c_directive_arg(clean)
			mut name_end := 0
			for name_end < arg.len && c_ident_char(arg[name_end]) {
				name_end++
			}
			if name_end > 0 {
				g.inlined_c_declared_fns[arg[..name_end]] = true
			}
		}
		if pending.len > 0 {
			if clean.len == 0 || clean[0] == `#` || clean.contains('{') || clean.contains('}')
				|| pending.len > 4096 {
				pending = ''
			} else {
				pending += ' ' + clean
				if pending.ends_with(';') {
					name := c_header_declared_fn_name(pending)
					if name.len > 0 {
						g.inlined_c_declared_fns[name] = true
					}
					pending = ''
				}
				continue
			}
		}
		name := c_header_declared_fn_name(clean)
		if name.len > 0 {
			g.inlined_c_declared_fns[name] = true
			continue
		}
		if c_header_declared_fn_start(clean) {
			pending = clean
		}
	}
}

// c_header_declared_fn_start reports whether a line looks like the opening of
// a multi-line function declaration: it introduces a parameter list that is
// not yet terminated on the same line.
fn c_header_declared_fn_start(line string) bool {
	if line.len == 0 || line[0] == `#` || line.ends_with(';') || !line.contains('(') {
		return false
	}
	if line.starts_with('typedef ') || line.contains('=') || line.contains('{')
		|| line.contains('}') || line.contains(')') {
		return false
	}
	for prefix in ['return ', 'if ', 'if(', 'for ', 'for(', 'while ', 'while(', 'switch ', 'switch(',
		'case ', 'do ', 'else '] {
		if line.starts_with(prefix) {
			return false
		}
	}
	return c_header_fn_name(line).len > 0
}

fn (mut g FlatGen) collect_preserved_c_fns(names []string) {
	for name in names {
		g.inlined_c_declared_fns[name] = true
	}
}

fn (mut g FlatGen) collect_preserved_c_structs(names []string) {
	for name in names {
		g.inlined_c_structs[name] = true
	}
}

fn c_static_fn_prefix_can_continue(line string) bool {
	return line in ['static', 'static inline', 'static __inline', 'static __inline__']
		|| line.starts_with('static inline ') || line.starts_with('static __inline ')
		|| line.starts_with('static __inline__ ')
}

fn c_header_struct_tag(rest string) string {
	mut end := 0
	for end < rest.len {
		c := rest[end]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			end++
			continue
		}
		break
	}
	return rest[..end]
}

fn c_typedef_struct_aliases(text string) []string {
	return c_typedef_aggregate_aliases(text, 'struct')
}

fn c_typedef_union_aliases(text string) []string {
	return c_typedef_aggregate_aliases(text, 'union')
}

fn c_typedef_aggregate_aliases(text string, kind string) []string {
	mut aliases := []string{}
	prefix := 'typedef ${kind}'
	mut start := 0
	for start < text.len {
		rel_idx := text[start..].index(prefix) or { break }
		idx := start + rel_idx
		mut pos := idx + prefix.len
		if pos < text.len && c_ident_char(text[pos]) {
			start = pos + 1
			continue
		}
		for pos < text.len && text[pos].is_space() {
			pos++
		}
		mut had_tag := false
		if pos < text.len && text[pos] != `{` {
			tag := c_header_struct_tag(text[pos..])
			if tag.len == 0 {
				start = pos + 1
				continue
			}
			had_tag = true
			pos += tag.len
			for pos < text.len && text[pos].is_space() {
				pos++
			}
		}
		if pos >= text.len || text[pos] != `{` {
			// Bodyless alias form: `typedef struct tag Alias;` also names the
			// alias, so a `struct C.Alias {}` guess typedef must be suppressed.
			if had_tag {
				semi_rel := text[pos..].index_u8(`;`)
				if semi_rel >= 0 {
					for alias in c_typedef_declarator_aliases(text[pos..pos + semi_rel]) {
						aliases << alias
					}
					start = pos + semi_rel + 1
					continue
				}
			}
			start = pos + 1
			continue
		}
		close_idx := c_matching_brace_end(text, pos)
		if close_idx < 0 {
			break
		}
		semi_rel_idx := text[close_idx + 1..].index_u8(`;`)
		if semi_rel_idx < 0 {
			break
		}
		semi_idx := close_idx + 1 + semi_rel_idx
		for alias in c_typedef_declarator_aliases(text[close_idx + 1..semi_idx]) {
			aliases << alias
		}
		start = semi_idx + 1
	}
	return aliases
}

fn c_matching_brace_end(text string, open_idx int) int {
	mut depth := 0
	for i in open_idx .. text.len {
		if text[i] == `{` {
			depth++
		} else if text[i] == `}` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn c_typedef_declarator_aliases(decl string) []string {
	mut aliases := []string{}
	for part in decl.split(',') {
		alias := c_last_ident(part)
		if alias.len > 0 {
			aliases << alias
		}
	}
	return aliases
}

fn c_last_ident(text string) string {
	mut end := text.len
	for end > 0 && !c_ident_char(text[end - 1]) {
		end--
	}
	mut start := end
	for start > 0 && c_ident_char(text[start - 1]) {
		start--
	}
	if start == end {
		return ''
	}
	return text[start..end]
}

fn c_header_fn_name(line string) string {
	paren := line.index_u8(`(`)
	if paren < 0 {
		return ''
	}
	mut end := paren
	for end > 0 && line[end - 1].is_space() {
		end--
	}
	mut start := end
	for start > 0 && c_ident_char(line[start - 1]) {
		start--
	}
	if start == end {
		return ''
	}
	name := line[start..end]
	if name in ['if', 'for', 'while', 'switch'] {
		return ''
	}
	return name
}

fn c_header_declared_fn_name(line string) string {
	if line.len == 0 || line[0] == `#` || !line.ends_with(';') || !line.contains('(') {
		return ''
	}
	if line.starts_with('typedef ') || line.contains('=') || line.contains('{')
		|| line.contains('}') {
		return ''
	}
	if macro_name := c_header_macro_wrapped_declared_fn_name(line) {
		return macro_name
	}
	// Reject function-pointer variable declarations (`int (*fp)(void);`) and
	// functions returning function pointers, where the identifier before the
	// first `(` is not the declared name. A `(*` later in the parameter list
	// is fine - fn-pointer parameters do not change where the name sits.
	paren_idx := line.index_u8(`(`)
	mut after := paren_idx + 1
	for after < line.len && line[after].is_space() {
		after++
	}
	if after < line.len && line[after] == `*` {
		return ''
	}
	for prefix in ['return ', 'if ', 'if(', 'for ', 'for(', 'while ', 'while(', 'switch ', 'switch(',
		'case ', 'do ', 'else '] {
		if line.starts_with(prefix) {
			return ''
		}
	}
	paren := line.index_u8(`(`)
	mut end := paren
	for end > 0 && line[end - 1].is_space() {
		end--
	}
	mut start := end
	for start > 0 && c_ident_char(line[start - 1]) {
		start--
	}
	if start == 0 {
		return ''
	}
	return c_header_fn_name(line)
}

fn c_header_macro_wrapped_declared_fn_name(line string) ?string {
	open := line.index_u8(`(`)
	if open <= 0 {
		return none
	}
	macro_name := line[..open].trim_space()
	if macro_name.len == 0 || macro_name.contains(' ') || macro_name.contains('\t') {
		return none
	}
	for c in macro_name.bytes() {
		if !((c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`) {
			return none
		}
	}
	close := typeof_display_type_name_matching_paren(line, open)
	if close < 0 || close + 1 >= line.len {
		return none
	}
	declarator := line[close + 1..].trim_space()
	if !declarator.ends_with(';') || !declarator.contains('(') {
		return none
	}
	name := c_header_fn_name(declarator)
	if name.len == 0 {
		return none
	}
	return name
}

fn c_header_defined_fn_name(line string) string {
	if line.len == 0 || line[0] == `#` || line.ends_with(';') || !line.contains('(')
		|| line.contains('=') || line.contains('(*') {
		return ''
	}
	for prefix in ['typedef ', 'struct ', 'union ', 'enum ', 'extern ', 'return ', 'if ', 'if(',
		'for ', 'for(', 'while ', 'while(', 'switch ', 'switch(', 'case ', 'do ', 'else '] {
		if line.starts_with(prefix) {
			return ''
		}
	}
	return c_header_fn_name(line)
}

fn c_ident_char(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch == `_`
}

fn c_include_file_path(include_arg string, vroot string, source_file string) string {
	clean := trimmed_space(include_arg)
	if clean.len < 2 {
		return ''
	}
	if clean[0] == `<` {
		return ''
	}
	mut path := ''
	if clean[0] == `"` && clean[clean.len - 1] == `"` {
		path = clean[1..clean.len - 1]
	} else {
		path = clean
	}
	path = c_resolve_pseudo_paths(path, vroot, source_file)
	if path.len == 0 || os.is_abs_path(path) {
		return path
	}
	if source_file.len == 0 {
		return path
	}
	return os.join_path_single(os.dir(source_file), path)
}

fn c_include_arg_is_literal(include_arg string) bool {
	clean := trimmed_space(include_arg)
	if clean.len < 2 {
		return false
	}
	return (clean[0] == `"` && clean[clean.len - 1] == `"`)
		|| (clean[0] == `<` && clean[clean.len - 1] == `>`)
}

fn c_include_file_paths(include_arg string, vroot string, source_file string, include_dirs []string) []string {
	clean := trimmed_space(include_arg)
	if clean.len < 2 {
		return []string{}
	}
	mut raw_path := clean
	mut search_source_dir := true
	if clean[0] == `"` && clean[clean.len - 1] == `"` {
		raw_path = clean[1..clean.len - 1]
	} else if clean[0] == `<` && clean[clean.len - 1] == `>` {
		raw_path = clean[1..clean.len - 1]
		search_source_dir = false
	}
	mut paths := []string{}
	if search_source_dir {
		first := c_include_file_path(include_arg, vroot, source_file)
		if first.len > 0 {
			paths << first
		}
	}
	resolved_path := c_resolve_pseudo_paths(raw_path, vroot, source_file)
	if os.is_abs_path(resolved_path) {
		if resolved_path !in paths {
			paths << resolved_path
		}
		return paths
	}
	for dir in include_dirs {
		if dir.len == 0 {
			continue
		}
		path := os.join_path_single(dir, resolved_path)
		if path !in paths {
			paths << path
		}
	}
	return paths
}

fn (mut g FlatGen) add_c_directive(module_name string, text string, before_import bool) {
	if text.len == 0 {
		return
	}
	g.c_directives << CDirective{
		module:        module_name
		text:          text
		before_import: before_import
	}
}

fn c_preprocessor_directive_line(name string, raw string) string {
	clean := trimmed_space(raw)
	if clean.len == 0 {
		return '#${name}'
	}
	return '#${name} ${clean}'
}

// note_compiler_source_file supports note compiler source file handling for FlatGen.
fn (mut g FlatGen) note_compiler_source_file(path string) {
	if g.compiler_vroot.len > 0 || path.len == 0 {
		return
	}
	mut full_path := path
	if !os.is_abs_path(full_path) {
		full_path = os.abs_path(full_path)
	}
	full_path = os.real_path(full_path)
	normalized := full_path.replace('\\', '/')
	suffix := '/cmd/v/v.v'
	if normalized.ends_with(suffix) {
		g.compiler_vroot = normalized[..normalized.len - suffix.len]
		return
	}
	vlib_idx := normalized.index('/vlib/') or { return }
	if vlib_idx > 0 {
		g.compiler_vroot = normalized[..vlib_idx]
	}
}

// collect_const_init_order_from_files converts collect const init order from files data for c.
fn (mut g FlatGen) collect_const_init_order_from_files() {
	mut seen := map[string]bool{}
	g.const_init_order = []string{}
	for node_idx in g.tc.top_level_idx {
		node := g.a.nodes[node_idx]
		if node_kind_id(node) != 77 || node.children_count == 0 {
			continue
		}
		mut cur_module := 'main'
		for i in 0 .. node.children_count {
			child := g.a.child_node(&node, i)
			kind_id := node_kind_id(child)
			if kind_id == 73 {
				cur_module = child.value
				continue
			}
			if kind_id != 65 {
				continue
			}
			for j in 0 .. child.children_count {
				field := g.a.child_node(child, j)
				if node_kind_id(field) != 66 || field.children_count == 0 {
					continue
				}
				qname := g.const_storage_name(cur_module, field.value)
				if qname in g.const_vals && !seen[qname] {
					seen[qname] = true
					g.const_init_order << qname
				}
			}
		}
	}
}

// ordered_module_init_fns supports ordered module init fns handling for FlatGen.
fn (g &FlatGen) ordered_module_init_fns() []string {
	module_to_init := g.module_init_fn_map()
	mut result := []string{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for init_fn in g.module_init_fns {
		mod := g.module_init_fn_modules[init_fn] or { '' }
		g.visit_module_init(mod, module_to_init, mut visiting, mut visited, mut result)
	}
	return result
}

fn (g &FlatGen) module_init_fn_map() map[string]string {
	mut module_to_init := map[string]string{}
	for init_fn in g.module_init_fns {
		mod := g.module_init_fn_modules[init_fn] or { '' }
		module_to_init[mod] = init_fn
	}
	return module_to_init
}

fn (g &FlatGen) ordered_startup_modules(module_to_init map[string]string) []string {
	mut module_order := []string{}
	for init_fn in g.module_init_fns {
		mod := g.module_init_fn_modules[init_fn] or { '' }
		if mod !in module_order {
			module_order << mod
		}
	}
	for mod in g.const_runtime_init_modules {
		if mod !in module_order {
			module_order << mod
		}
	}
	for mod in g.runtime_init_modules {
		if mod !in module_order {
			module_order << mod
		}
	}
	mut startup_modules := map[string]bool{}
	for mod in module_order {
		startup_modules[mod] = true
	}
	for mod, _ in module_to_init {
		startup_modules[mod] = true
	}
	mut result := []string{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for mod in module_order {
		g.visit_startup_module(mod, startup_modules, mut visiting, mut visited, mut result)
	}
	return result
}

fn (g &FlatGen) visit_startup_module(mod string, startup_modules map[string]bool, mut visiting map[string]bool, mut visited map[string]bool, mut result []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	for dep in g.module_imports[mod] or { []string{} } {
		dep_module := g.startup_dependency_module(dep, startup_modules)
		g.visit_startup_module(dep_module, startup_modules, mut visiting, mut visited, mut result)
	}
	visiting.delete(mod)
	visited[mod] = true
	if mod in startup_modules {
		result << mod
	}
}

fn (g &FlatGen) startup_dependency_module(dep string, startup_modules map[string]bool) string {
	if dep in startup_modules || dep in g.module_imports {
		return dep
	}
	short := startup_module_key(dep)
	if short in startup_modules || short in g.module_imports {
		return short
	}
	return dep
}

fn (mut g FlatGen) emit_runtime_inits_for_module(mod string, mut emitted_const []bool, mut emitted_runtime []bool) {
	for i, ri in g.const_runtime_inits {
		if !emitted_const[i] && i < g.const_runtime_init_modules.len
			&& g.const_runtime_init_modules[i] == mod {
			g.writeln(ri)
			emitted_const[i] = true
		}
	}
	for i, ri in g.runtime_inits {
		if !emitted_runtime[i] && i < g.runtime_init_modules.len && g.runtime_init_modules[i] == mod {
			g.writeln(ri)
			emitted_runtime[i] = true
		}
	}
}

fn (mut g FlatGen) emit_remaining_runtime_inits(mut emitted_const []bool, mut emitted_runtime []bool) {
	for i, ri in g.const_runtime_inits {
		if !emitted_const[i] {
			g.writeln(ri)
			emitted_const[i] = true
		}
	}
	for i, ri in g.runtime_inits {
		if !emitted_runtime[i] {
			g.writeln(ri)
			emitted_runtime[i] = true
		}
	}
}

fn (mut g FlatGen) queue_const_runtime_init(line string) {
	g.const_runtime_inits << line
	g.const_runtime_init_modules << g.tc.cur_module
}

fn (mut g FlatGen) queue_runtime_init(line string) {
	g.runtime_inits << line
	g.runtime_init_modules << g.tc.cur_module
}

// visit_module_init updates visit module init state for FlatGen.
fn (g &FlatGen) visit_module_init(mod string, module_to_init map[string]string, mut visiting map[string]bool, mut visited map[string]bool, mut result []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	for dep in g.module_imports[mod] or { []string{} } {
		dep_module := if dep in module_to_init || dep in g.module_imports {
			dep
		} else {
			startup_module_key(dep)
		}
		g.visit_module_init(dep_module, module_to_init, mut visiting, mut visited, mut result)
	}
	visiting.delete(mod)
	visited[mod] = true
	if init_fn := module_to_init[mod] {
		result << init_fn
	}
}

fn (mut g FlatGen) ordered_c_directives() []string {
	mut directives_by_module := map[string][]CDirective{}
	mut module_order := []string{}
	for directive in g.c_directives {
		if directive.module !in directives_by_module {
			directives_by_module[directive.module] = []CDirective{}
			module_order << directive.module
		}
		directives_by_module[directive.module] << directive
	}
	mut result := []string{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for mod in module_order {
		g.visit_c_directive_module(mod, directives_by_module, mut visiting, mut visited, mut result)
	}
	return dedupe_top_level_c_includes(result)
}

fn (mut g FlatGen) emit_c_directives() {
	mut emitted := false
	for directive in g.ordered_c_directives() {
		if c_contains_preserved_system_include_directive(directive) {
			continue
		}
		g.writeln(directive)
		emitted = true
	}
	if emitted {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_preserved_c_directives() {
	mut emitted := false
	mut emitted_includes := map[string]bool{}
	mut has_mach_headers := false
	directives := g.ordered_c_directives()
	for i, directive in directives {
		if !c_contains_preserved_system_include_directive(directive) {
			continue
		}
		if directive.contains('<mach/mach.h>') {
			has_mach_headers = true
		}
		clean := trimmed_space(directive)
		if directive.contains('\n') {
			g.emit_preserved_c_directive(directive)
			emitted = true
			continue
		}
		prefix := if c_lifted_include_skips_context(directive) {
			[]string{}
		} else {
			c_lifted_include_context_prefix(directives, i)
		}
		if c_is_preserved_system_include_directive(clean) {
			// Dedupe on the include *together with* its lifted guard context: the
			// same header may legitimately appear under different guards (e.g. one
			// `#ifdef __linux__` block and one `#ifdef __APPLE__` block), and each
			// occurrence needs its own context emitted. Keying on the raw include
			// line alone would drop the second, differently-guarded include.
			key := prefix.join('\n') + '\x00' + clean
			if emitted_includes[key] {
				continue
			}
			emitted_includes[key] = true
		}
		for line in prefix {
			g.writeln(line)
			emitted = true
		}
		g.emit_preserved_c_directive(directive)
		emitted = true
		for _ in 0 .. c_lifted_include_context_depth(prefix) {
			g.writeln('#endif')
		}
	}
	refs := g.c_extern_referenced_symbols()
	if !has_mach_headers && (refs['C.task_info'] || refs['task_info']
		|| refs['C.mach_task_self'] || refs['mach_task_self']) {
		g.writeln('#ifdef __APPLE__')
		g.emit_preserved_c_directive('#include <mach/mach.h>')
		g.emit_preserved_c_directive('#include <mach/task.h>')
		g.writeln('#endif')
		emitted = true
	}
	if emitted {
		g.writeln('')
	}
}

fn c_lifted_include_skips_context(directive string) bool {
	return trimmed_space(directive) == '#include <mach/mach_time.h>'
}

fn (mut g FlatGen) emit_preserved_c_directive(directive string) {
	if c_preserved_directive_needs_mach_panic_alias(directive) {
		g.writeln('#define panic mach_panic')
		g.writeln(directive)
		g.writeln('#undef panic')
		return
	}
	g.writeln(directive)
}

fn c_preserved_directive_needs_mach_panic_alias(directive string) bool {
	for line in directive.split_into_lines() {
		clean := trimmed_space(line)
		if clean == '#include <mach/mach.h>' || clean == '#include <mach/mach_time.h>' {
			return true
		}
	}
	return false
}

fn c_lifted_include_context_prefix(directives []string, include_index int) []string {
	mut prefix := []string{}
	for i := include_index - 1; i >= 0; i-- {
		clean := trimmed_space(directives[i])
		if c_is_preserved_system_include_directive(clean) {
			continue
		}
		if !c_is_liftable_include_context_directive(clean) {
			break
		}
		prefix << directives[i]
	}
	prefix.reverse_in_place()
	return prefix
}

fn c_lifted_include_context_depth(prefix []string) int {
	mut depth := 0
	for directive in prefix {
		clean := trimmed_space(directive)
		if clean.starts_with('#ifdef') || clean.starts_with('#ifndef') || clean.starts_with('#if ') {
			depth++
		}
	}
	return depth
}

fn c_is_liftable_include_context_directive(directive string) bool {
	clean := trimmed_space(directive)
	if clean.len == 0 || clean.contains('\n') || clean.starts_with('#endif') {
		return false
	}
	return clean.starts_with('#define') || clean.starts_with('#undef')
		|| clean.starts_with('#ifdef') || clean.starts_with('#ifndef') || clean.starts_with('#if ')
		|| clean.starts_with('#elif') || clean.starts_with('#else')
}

fn c_is_preserved_system_include_directive(directive string) bool {
	clean := trimmed_space(directive)
	return clean.starts_with('#include <') && clean.ends_with('>') && !clean.contains('\n')
}

fn c_contains_preserved_system_include_directive(directive string) bool {
	if c_is_preserved_system_include_directive(directive) {
		return true
	}
	if !directive.contains('\n') {
		return false
	}
	mut has_include := false
	for line in directive.split_into_lines() {
		clean := trimmed_space(line)
		if clean.len == 0 {
			continue
		}
		if c_is_preserved_system_include_directive(clean) {
			has_include = true
			continue
		}
		if clean == '#endif' || c_is_liftable_include_context_directive(clean) {
			continue
		}
		return false
	}
	return has_include
}

fn (g &FlatGen) visit_c_directive_module(mod string, directives_by_module map[string][]CDirective, mut visiting map[string]bool, mut visited map[string]bool, mut result []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	directives := directives_by_module[mod] or { []CDirective{} }
	for directive in directives {
		if directive.before_import {
			result << directive.text
		}
	}
	for dep in g.module_imports[mod] or { []string{} } {
		if dep in directives_by_module {
			g.visit_c_directive_module(dep, directives_by_module, mut visiting, mut visited, mut
				result)
		}
	}
	visiting.delete(mod)
	visited[mod] = true
	for directive in directives {
		if !directive.before_import {
			result << directive.text
		}
	}
}

fn dedupe_top_level_c_includes(directives []string) []string {
	mut result := []string{}
	mut seen_includes := map[string]bool{}
	mut depth := 0
	for directive in directives {
		clean := trimmed_space(directive)
		if depth == 0 && c_directive_name(clean) == 'include' {
			if clean in seen_includes {
				continue
			}
			seen_includes[clean] = true
		}
		result << directive
		name := c_directive_name(clean)
		if name in ['if', 'ifdef', 'ifndef'] {
			depth++
		} else if name == 'endif' && depth > 0 {
			depth--
		}
	}
	return result
}

fn c_directive_name(text string) string {
	if text.len == 0 || text[0] != `#` {
		return ''
	}
	body := trimmed_space(text[1..])
	if body.len == 0 {
		return ''
	}
	idx := body.index_u8(` `)
	if idx < 0 {
		return body
	}
	return body[..idx]
}

fn c_directive_arg(text string) string {
	if text.len == 0 || text[0] != `#` {
		return ''
	}
	body := trimmed_space(text[1..])
	mut idx := 0
	for idx < body.len && !body[idx].is_space() {
		idx++
	}
	if idx >= body.len {
		return ''
	}
	return trimmed_space(body[idx..])
}

fn c_include_arg(raw string, vroot string, source_file string) string {
	mut clean := c_directive_arg_for_target(raw.trim_space()) or { return '' }
	clean = c_resolve_pseudo_paths(clean.trim_space(), vroot, source_file)
	if clean.len == 0 {
		return ''
	}
	if clean[0] == `<` {
		end := clean.index_u8(`>`)
		if end > 0 {
			return clean[..end + 1]
		}
		return clean
	}
	if clean[0] == `"` {
		mut i := 1
		for i < clean.len {
			if clean[i] == `"` {
				return clean[..i + 1]
			}
			i++
		}
	}
	hash := clean.index_u8(`#`)
	if hash > 0 {
		return trimmed_space(clean[..hash])
	}
	return clean
}

fn c_flag_arg(raw string, vroot string, source_file string) string {
	clean := c_directive_arg_for_target(raw.trim_space()) or { return '' }
	if clean.len == 0 {
		return ''
	}
	resolved := c_resolve_pseudo_paths(clean, vroot, source_file)
	return c_resolve_relative_flag_paths(resolved, source_file)
}

// c_resolve_relative_flag_paths rewrites relative path arguments in a `#flag`
// directive (e.g. `-I ./thirdparty`, or a bare `./foo.c`) to absolute paths,
// resolved against the directory of the source file that carried the directive.
// V1 does the same: a project's `#flag` paths are relative to its own module dir,
// not to the compiler's build/working directory.
fn c_resolve_relative_flag_paths(flag string, source_file string) string {
	if source_file.len == 0 || !flag.contains('/') {
		return flag
	}
	base_dir := os.dir(source_file)
	if base_dir.len == 0 {
		return flag
	}
	mut out := []string{}
	for tok in flag.fields() {
		out << c_resolve_flag_path_token(tok, base_dir)
	}
	return out.join(' ')
}

fn c_resolve_flag_path_token(tok string, base_dir string) string {
	for prefix in ['-I', '-L'] {
		if tok.starts_with(prefix) && tok.len > prefix.len {
			path := tok[prefix.len..]
			if c_flag_path_is_relative(path) {
				return prefix + os.real_path(os.join_path_single(base_dir, path))
			}
			return tok
		}
	}
	if !tok.starts_with('-') && c_flag_path_is_relative(tok) {
		return os.real_path(os.join_path_single(base_dir, tok))
	}
	return tok
}

fn c_flag_path_is_relative(p string) bool {
	if p.len == 0 || os.is_abs_path(p) {
		return false
	}
	return p.starts_with('./') || p.starts_with('../') || p.contains('/')
}

fn c_directive_arg_for_target(raw string) ?string {
	parts := raw.fields()
	if parts.len == 0 {
		return none
	}
	if c_flag_has_target_prefix(parts[0]) {
		if !c_flag_target_enabled(parts[0]) || parts.len < 2 {
			return none
		}
		return parts[1..].join(' ')
	}
	return raw
}

fn c_resolve_pseudo_paths(raw string, vroot string, source_file string) string {
	mut result := raw
	if result.contains('@VEXEROOT') && vroot.len > 0 {
		result = result.replace('@VEXEROOT', vroot)
	}
	if result.contains('@VROOT') {
		result = result.replace('@VROOT', '@VMODROOT')
	}
	if result.contains('@VMODROOT') {
		result = result.replace('@VMODROOT', c_vmod_root_for_file(source_file))
	}
	if result.contains('@DIR') {
		dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
		result = result.replace('@DIR', os.real_path(dir))
	}
	return result
}

fn c_vmod_root_for_file(source_file string) string {
	mut dir := if source_file.len > 0 { os.dir(source_file) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return os.real_path(dir)
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return os.real_path(dir)
		}
		dir = parent
	}
	return os.real_path(dir)
}

fn c_pkgconfig_flags(raw string) []string {
	name := trimmed_space(raw)
	if name.len == 0 {
		return []string{}
	}
	// The package name comes straight from source text and is interpolated into a
	// shell command, so reject anything that is not a plain pkg-config name/flag to
	// avoid command injection (e.g. `#pkgconfig foo; touch /tmp/pwned`).
	if !c_pkgconfig_arg_is_safe(name) {
		return []string{}
	}
	result := os.execute('pkg-config --cflags --libs ${name}')
	if result.exit_code != 0 {
		return []string{}
	}
	return trimmed_space(result.output).fields()
}

fn c_pkgconfig_arg_is_safe(raw string) bool {
	for ch in raw {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) {
			continue
		}
		if ch in [` `, `\t`, `_`, `-`, `.`, `+`, `:`, `/`] {
			continue
		}
		return false
	}
	return true
}

fn c_flag_has_target_prefix(target string) bool {
	return target in ['darwin', 'macos', 'linux', 'windows', 'freebsd', 'openbsd', 'netbsd',
		'solaris', 'termux', 'wasm32_emscripten']
}

fn c_flag_target_enabled(target string) bool {
	match target {
		'darwin', 'macos' {
			$if macos {
				return true
			}
			return false
		}
		'linux' {
			$if linux {
				return true
			}
			return false
		}
		'windows' {
			$if windows {
				return true
			}
			return false
		}
		'freebsd' {
			$if freebsd {
				return true
			}
			return false
		}
		'openbsd' {
			$if openbsd {
				return true
			}
			return false
		}
		'netbsd' {
			$if netbsd {
				return true
			}
			return false
		}
		'solaris' {
			$if solaris {
				return true
			}
			return false
		}
		'termux' {
			$if termux {
				return true
			}
			return false
		}
		'wasm32_emscripten' {
			$if wasm32_emscripten {
				return true
			}
			return false
		}
		else {
			return true
		}
	}
}

// fn_decl_module_key returns the collision-proof per-module key for a declared
// fn/method. Method names (`Recv.method`) are dotted but not module-qualified,
// so name-based keys collide when two modules declare the same receiver/method
// pair (e.g. `io.NotExpected.msg` vs `os.NotExpected.msg`).
fn fn_decl_module_key(module_name string, name string) string {
	return '${module_name}\x01${name}'
}

// register_fn_decl_param_types updates register fn decl param types state for c.
fn (mut g FlatGen) register_fn_decl_param_types(name string, full_name string, ptypes []types.Type, is_variadic bool) {
	module_key := fn_decl_module_key(g.tc.cur_module, name)
	g.fn_decl_param_types[module_key] = ptypes.clone()
	g.fn_decl_variadic[module_key] = is_variadic
	short_name := name.all_after_last('.')
	g.fn_decl_variadic_short_counts[short_name] = g.fn_decl_variadic_short_counts[short_name] + 1
	if name !in g.fn_decl_param_types {
		g.fn_decl_param_types[name] = ptypes.clone()
		g.fn_decl_variadic[name] = is_variadic
	}
	cname := g.cname(name)
	if cname !in g.fn_decl_param_types {
		g.fn_decl_param_types[cname] = ptypes.clone()
		g.fn_decl_variadic[cname] = is_variadic
	}
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
		dotted_name := '${g.tc.cur_module}.${name}'
		if dotted_name !in g.fn_decl_param_types {
			g.fn_decl_param_types[dotted_name] = ptypes.clone()
			g.fn_decl_variadic[dotted_name] = is_variadic
		}
		cdotted_name := g.cname(dotted_name)
		if cdotted_name !in g.fn_decl_param_types {
			g.fn_decl_param_types[cdotted_name] = ptypes.clone()
			g.fn_decl_variadic[cdotted_name] = is_variadic
		}
	}
	if full_name !in g.fn_decl_param_types {
		g.fn_decl_param_types[full_name] = ptypes.clone()
		g.fn_decl_variadic[full_name] = is_variadic
	}
	cfull_name := g.cname(full_name)
	if cfull_name !in g.fn_decl_param_types {
		g.fn_decl_param_types[cfull_name] = ptypes.clone()
		g.fn_decl_variadic[cfull_name] = is_variadic
	}
}

fn (mut g FlatGen) register_fn_decl_shared_params(name string, full_name string, flags []bool) {
	mut has_shared := false
	for flag in flags {
		if flag {
			has_shared = true
			break
		}
	}
	if has_shared {
		g.has_shared_params = true
	}
	// All-false declarations are registered too: their exact-name entry is what
	// stops fn_param_is_shared's short-name fallback from borrowing the shared
	// flags of an unrelated same-named declaration in another module.
	if name !in g.fn_decl_shared_params {
		g.fn_decl_shared_params[name] = flags.clone()
	}
	cname := g.cname(name)
	if cname !in g.fn_decl_shared_params {
		g.fn_decl_shared_params[cname] = flags.clone()
	}
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
		dotted_name := '${g.tc.cur_module}.${name}'
		if dotted_name !in g.fn_decl_shared_params {
			g.fn_decl_shared_params[dotted_name] = flags.clone()
		}
		cdotted_name := g.cname(dotted_name)
		if cdotted_name !in g.fn_decl_shared_params {
			g.fn_decl_shared_params[cdotted_name] = flags.clone()
		}
	}
	if full_name !in g.fn_decl_shared_params {
		g.fn_decl_shared_params[full_name] = flags.clone()
	}
	cfull_name := g.cname(full_name)
	if cfull_name !in g.fn_decl_shared_params {
		g.fn_decl_shared_params[cfull_name] = flags.clone()
	}
}

fn (mut g FlatGen) register_fn_decl_mut_receiver(name string, full_name string, is_mut bool) {
	if !is_mut {
		return
	}
	g.fn_decl_mut_receivers[name] = true
	g.fn_decl_mut_receivers[g.cname(name)] = true
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
		dotted_name := '${g.tc.cur_module}.${name}'
		g.fn_decl_mut_receivers[dotted_name] = true
		g.fn_decl_mut_receivers[g.cname(dotted_name)] = true
	}
	g.fn_decl_mut_receivers[full_name] = true
	g.fn_decl_mut_receivers[g.cname(full_name)] = true
}

// register_fn_decl_ret_type indexes a fn decl's return type by its name (and qualified
// variants), so the return type can be looked up in O(1) instead of scanning all AST
// nodes per call (see fn_decl_return_type_for_call_name).
fn (mut g FlatGen) register_fn_decl_ret_type(name string, full_name string, ret_typ string) {
	rt := g.tc.parse_type(ret_typ)
	if name !in g.fn_decl_ret_types {
		g.fn_decl_ret_types[name] = rt
	}
	if g.tc.cur_module.len > 0 && g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
		dotted_name := '${g.tc.cur_module}.${name}'
		if dotted_name !in g.fn_decl_ret_types {
			g.fn_decl_ret_types[dotted_name] = rt
		}
	}
	if full_name !in g.fn_decl_ret_types {
		g.fn_decl_ret_types[full_name] = rt
	}
	cname := g.cname(name)
	if cname != name && cname !in g.fn_decl_ret_types {
		g.fn_decl_ret_types[cname] = rt
	}
}

fn (mut g FlatGen) register_fn_decl_node(name string, module_name string, id flat.NodeId) {
	if name !in g.fn_decl_nodes_by_name {
		g.fn_decl_nodes_by_name[name] = id
	}
	short := name.all_after_last('.')
	if short !in g.fn_decl_nodes_by_short {
		g.fn_decl_nodes_by_short[short] = id
	}
	module_key := '${module_name}\x01${short}'
	if module_key !in g.fn_decl_nodes_by_module_short {
		g.fn_decl_nodes_by_module_short[module_key] = id
	}
}

// register_struct_decl_info updates register struct decl info state for c.
fn (mut g FlatGen) register_struct_decl_info(name string, full_name string, module_name string, source_file string, node flat.Node) {
	info := StructDeclInfo{
		node:      node
		module:    module_name
		file:      source_file
		full_name: full_name
	}
	g.struct_decl_infos[full_name] = info
	if name !in g.struct_decl_short_infos {
		g.struct_decl_short_infos[name] = info
	}
}

// enum_value_for_type supports enum value for type handling for FlatGen.
fn (g &FlatGen) enum_value_for_type(type_name string, field_name string) ?int {
	if type_name.len == 0 || field_name.len == 0 {
		return none
	}
	key := '${type_name}.${field_name}'
	if val := g.enum_vals[key] {
		return val
	}
	if !type_name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		qkey := '${g.tc.cur_module}.${type_name}.${field_name}'
		if val := g.enum_vals[qkey] {
			return val
		}
	}
	if !type_name.contains('.') {
		mut found := 0
		mut ok := false
		for ename, val in g.enum_vals {
			if !ename.ends_with('.${type_name}.${field_name}') {
				continue
			}
			if ok {
				return none
			}
			found = val
			ok = true
		}
		if ok {
			return found
		}
	}
	return none
}

fn (g &FlatGen) enum_value_expr_for_key(key string) ?string {
	if expr := g.enum_value_exprs[key] {
		return expr
	}
	if val := g.enum_vals[key] {
		return '${val}'
	}
	return none
}

fn (g &FlatGen) enum_value_expr_for_type(type_name string, field_name string) ?string {
	if type_name.len == 0 || field_name.len == 0 {
		return none
	}
	key := '${type_name}.${field_name}'
	if expr := g.enum_value_exprs[key] {
		return expr
	}
	if !type_name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		qkey := '${g.tc.cur_module}.${type_name}.${field_name}'
		if expr := g.enum_value_exprs[qkey] {
			return expr
		}
	}
	if !type_name.contains('.') {
		mut found := ''
		mut ok := false
		for ename, expr in g.enum_value_exprs {
			if !ename.ends_with('.${type_name}.${field_name}') {
				continue
			}
			if ok {
				return none
			}
			found = expr
			ok = true
		}
		if ok {
			return found
		}
	}
	if val := g.enum_value_for_type(type_name, field_name) {
		return '${val}'
	}
	return none
}

fn (g &FlatGen) enum_selector_base_name(name string) ?string {
	if name in g.tc.enum_names || name in g.tc.flag_enums {
		return name
	}
	qname := g.tc.qualify_name(name)
	if qname in g.tc.enum_names || qname in g.tc.flag_enums {
		return qname
	}
	if target := g.enum_selector_alias_target(name) {
		return target
	}
	if target := g.enum_selector_alias_target(qname) {
		return target
	}
	if name.contains('.') || g.tc.cur_file.len == 0 {
		return none
	}
	candidates := g.tc.file_selective_imports['${g.tc.cur_file}\n${name}'] or { return none }
	for candidate in candidates {
		if candidate in g.tc.enum_names || candidate in g.tc.flag_enums {
			return candidate
		}
		if target := g.enum_selector_alias_target(candidate) {
			return target
		}
	}
	return none
}

fn (g &FlatGen) enum_selector_alias_target(name string) ?string {
	mut cur := name
	for _ in 0 .. 16 {
		target := g.tc.type_aliases[cur] or { return none }
		if target == cur {
			return none
		}
		if target in g.tc.enum_names || target in g.tc.flag_enums {
			return target
		}
		cur = target
	}
	return none
}

// expr_to_string converts expr to string data for c.
fn (mut g FlatGen) expr_to_string(id flat.NodeId) string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	g.line_start = true
	g.gen_expr(id)
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

// const_block_init_to_string renders a lowered const initializer block as a
// braced statement sequence assigning the final expression to the const.
fn (mut g FlatGen) const_block_init_to_string(qname string, val_node flat.Node) string {
	orig := g.sb
	orig_line_start := g.line_start
	orig_indent := g.indent
	g.sb = strings.new_builder(256)
	g.line_start = true
	g.indent = 1
	g.writeln('{')
	g.push_scope()
	g.indent++
	for i in 0 .. int(val_node.children_count) - 1 {
		g.gen_node(g.a.child(&val_node, i))
	}
	g.write('${qname} = ')
	last_id := g.a.child(&val_node, int(val_node.children_count) - 1)
	last := g.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count > 0 {
		g.gen_expr(g.a.child(&last, 0))
	} else {
		g.gen_expr(last_id)
	}
	g.writeln(';')
	g.indent--
	g.pop_scope()
	g.writeln('}')
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	g.indent = orig_indent
	return result
}

// interface_value_to_string captures, as a string, the boxed interface value the direct return
// path emits (`(Iface){._typ = N, ._object = ...}`) — so a deferred return can save it into a
// temp without dropping `_typ`/`_object`. Mirrors that path: box a concrete value, else (already
// boxed by the transform) emit it as-is.
fn (mut g FlatGen) interface_value_to_string(id flat.NodeId, expected types.Type) string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	// Box mid-statement (no leading indent), matching the direct return path.
	g.line_start = false
	if !g.gen_interface_value_expr(id, expected) {
		g.gen_expr(id)
	}
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

// fixed_array_copy_source_string captures gen_fixed_array_copy_source as a string, so a deferred
// optional/fixed-array return can embed the memcpy source when saving the value into a temp.
fn (mut g FlatGen) fixed_array_copy_source_string(value_id flat.NodeId, field_type types.Type) string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	// Emit mid-statement (no leading indent), matching the direct return path.
	g.line_start = false
	g.gen_fixed_array_copy_source(value_id, field_type)
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

// expr_to_string_with_expected_type converts expr to string with expected type data for c.
fn (mut g FlatGen) expr_to_string_with_expected_type(id flat.NodeId, expected types.Type) string {
	orig := g.sb
	orig_line_start := g.line_start
	g.sb = strings.new_builder(64)
	g.line_start = true
	g.gen_expr_with_expected_type(id, expected)
	result := g.sb.str()
	g.sb = orig
	g.line_start = orig_line_start
	return result
}

fn (mut g FlatGen) gen_amp_c_string_literal(id flat.NodeId, node flat.Node) bool {
	if node.kind == .char_literal && node.value.starts_with('c:') {
		// `&c'...'` always denotes the C string pointer; emit the literal
		// directly so a byte-valued expected type can't deref a single-char
		// `c'\n'` into `*"\n"` here.
		g.write('"${node.value[2..]}"')
		return true
	}
	if node.kind != .char_literal && node.kind != .string_literal {
		return false
	}
	expr := g.expr_to_string(id)
	if expr.len >= 2 && expr[0] == `"` && expr[expr.len - 1] == `"` {
		g.write(expr)
		return true
	}
	return false
}

fn (mut g FlatGen) gen_expr_as_string(id flat.NodeId) {
	typ := g.usable_expr_type(id)
	if g.gen_map_str_expr(id, typ) {
		return
	}
	if typ is types.Pointer && typ.base_type is types.String {
		if g.gen_current_mut_param_value_read(id, typ.base_type) {
			return
		}
		g.write('*(')
		g.gen_expr(id)
		g.write(')')
		return
	}
	g.gen_expr(id)
}

fn (mut g FlatGen) gen_map_str_expr(id flat.NodeId, typ types.Type) bool {
	clean := map_str_clean_type(typ)
	if clean !is types.Map {
		return false
	}
	alias_name := map_str_alias_name(typ)
	if alias_name.len > 0 {
		prefix_sid := g.intern_string('${alias_name}(')
		g.write('string__plus(string__plus(_str_${prefix_sid}, ')
	}
	node := g.a.nodes[int(id)]
	if node.kind == .map_init && typ !is types.Pointer {
		tmp := '__map_str_tmp_${g.tmp_count}'
		g.tmp_count++
		g.write('({ map ${tmp} = ')
		g.gen_expr_with_expected_type(id, clean)
		g.write(';')
		key_kind := map_str_kind(g.tc, clean.key_type)
		val_kind := map_str_kind(g.tc, clean.value_type)
		fixed_len := map_str_fixed_len(clean.value_type)
		g.write(' v3_map_str(${tmp}, ${key_kind}, ${val_kind}, ${fixed_len}); })')
		if alias_name.len > 0 {
			suffix_sid := g.intern_string(')')
			g.write('), _str_${suffix_sid})')
		}
		return true
	}
	g.write('v3_map_str(')
	if typ is types.Pointer {
		needs_paren := g.a.nodes[int(id)].kind !in [.ident, .selector, .call]
		g.write('*')
		if needs_paren {
			g.write('(')
		}
		g.gen_expr(id)
		if needs_paren {
			g.write(')')
		}
	} else {
		g.gen_expr(id)
	}
	key_kind := map_str_kind(g.tc, clean.key_type)
	val_kind := map_str_kind(g.tc, clean.value_type)
	fixed_len := map_str_fixed_len(clean.value_type)
	g.write(', ${key_kind}, ${val_kind}, ${fixed_len})')
	if alias_name.len > 0 {
		suffix_sid := g.intern_string(')')
		g.write('), _str_${suffix_sid})')
	}
	return true
}

fn map_str_clean_type(typ types.Type) types.Type {
	clean := types.unwrap_pointer(typ)
	if clean is types.Alias {
		return clean.base_type
	}
	return clean
}

fn map_str_alias_name(typ types.Type) string {
	clean := types.unwrap_pointer(typ)
	if clean is types.Alias {
		if clean.base_type is types.Map {
			return clean.name.all_after_last('.')
		}
	}
	return ''
}

fn map_str_kind(tc &types.TypeChecker, typ types.Type) int {
	clean := if typ is types.Alias { typ.base_type } else { typ }
	if clean is types.String {
		return 1
	}
	if clean is types.Rune {
		return 4
	}
	if clean is types.ISize || clean is types.Char {
		return 2
	}
	if clean is types.USize {
		return 3
	}
	if clean is types.Primitive {
		if clean.props.has(.float) {
			return if tc.c_type(types.Type(clean)) == 'float' { 8 } else { 5 }
		}
		name := types.Type(clean).name()
		if name in ['i8', 'i16', 'i32', 'i64', 'int'] {
			return 2
		}
		if name in ['u8', 'byte'] {
			return 3
		}
		if name in ['u16', 'u32', 'u64'] {
			return 3
		}
		if name == 'bool' {
			return 7
		}
	}
	if fixed := array_fixed_type(clean) {
		elem := if fixed.elem_type is types.Alias {
			fixed.elem_type.base_type
		} else {
			fixed.elem_type
		}
		if elem is types.Primitive && elem.props.has(.float) {
			return if tc.c_type(types.Type(elem)) == 'float' { 9 } else { 6 }
		}
	}
	return 0
}

fn map_str_fixed_len(typ types.Type) int {
	if fixed := array_fixed_type(typ) {
		if fixed.len > 0 {
			return fixed.len
		}
		if fixed.len_expr.len > 0 && fixed.len_expr.int().str() == fixed.len_expr {
			return fixed.len_expr.int()
		}
	}
	return 0
}

// gen_cast_from_mut_param_address emits pointer casts for `&param` where `param`
// is a mutable V parameter already represented as a C pointer.
fn (mut g FlatGen) gen_cast_from_mut_param_address(id flat.NodeId, ct string) bool {
	node := g.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return false
	}
	child_id := g.a.child(&node, 0)
	child := g.a.nodes[int(child_id)]
	if child.kind != .ident || !g.current_param_is_mut(child.value) {
		return false
	}
	param_type := g.current_param_type(child.value) or { return false }
	if param_type !is types.Pointer {
		return false
	}
	g.write('(${ct})(')
	g.gen_expr(child_id)
	g.write(')')
	return true
}

fn (mut g FlatGen) gen_pointer_cast_from_map_value_address(id flat.NodeId, target types.Pointer) bool {
	if map_str_clean_type(target.base_type) !is types.Map {
		return false
	}
	return g.gen_map_pointer_cast_from_value_address(id)
}

fn (mut g FlatGen) gen_map_pointer_cast_from_value_address(id flat.NodeId) bool {
	actual0 := if int(id) >= 0 && int(id) < g.a.nodes.len && g.a.nodes[int(id)].typ.len > 0 {
		g.tc.parse_type(g.a.nodes[int(id)].typ)
	} else {
		g.usable_expr_type(id)
	}
	actual := map_str_clean_type(actual0)
	if actual !is types.Map {
		return false
	}
	if g.expr_is_addressable(id) {
		g.write('&')
		g.gen_expr(id)
		return true
	}
	ct := g.tc.c_type(actual)
	g.write('({${ct} _t${g.tmp_count} = ')
	g.gen_expr(id)
	g.write('; &_t${g.tmp_count};})')
	g.tmp_count++
	return true
}

fn (mut g FlatGen) map_pointer_cast_from_value_address_string(id flat.NodeId, seen []string) ?string {
	actual0 := if int(id) >= 0 && int(id) < g.a.nodes.len && g.a.nodes[int(id)].typ.len > 0 {
		g.tc.parse_type(g.a.nodes[int(id)].typ)
	} else {
		g.usable_expr_type(id)
	}
	actual := map_str_clean_type(actual0)
	if actual !is types.Map {
		return none
	}
	child0 := g.const_expr_to_string(id, seen)
	child := if trimmed_space(child0).len == 0 { '0' } else { child0 }
	if g.expr_is_addressable(id) {
		return '&(${child})'
	}
	ct := g.tc.c_type(actual)
	tmp := '_t${g.tmp_count}'
	g.tmp_count++
	return '({${ct} ${tmp} = ${child}; &${tmp};})'
}

fn (mut g FlatGen) gen_current_mut_param_address(id flat.NodeId) bool {
	node := g.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return false
	}
	child_id := g.a.child(&node, 0)
	child := g.a.nodes[int(child_id)]
	if child.kind != .ident || !g.current_param_is_mut(child.value) {
		return false
	}
	param_type := g.current_param_type(child.value) or { return false }
	if param_type !is types.Pointer {
		return false
	}
	g.write(g.cname(child.value))
	return true
}

fn (mut g FlatGen) gen_current_mut_param_value_read(id flat.NodeId, expected types.Type) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	if expected is types.Pointer {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind != .ident || !g.current_param_is_mut(node.value) {
		return false
	}
	param_type := g.current_param_type(node.value) or { return false }
	if param_type is types.Pointer {
		if !g.type_names_match(select_receive_unalias_type(param_type.base_type),
			select_receive_unalias_type(expected)) {
			return false
		}
	} else {
		return false
	}
	g.write('*')
	g.gen_expr(id)
	return true
}

// gen_expr_with_expected_type emits expr with expected type output for c.
fn (mut g FlatGen) gen_expr_with_expected_type(id flat.NodeId, expected types.Type) {
	old_expected := g.expected_expr_type
	old_expected_enum := g.expected_enum
	g.expected_expr_type = expected
	if expected is types.Enum {
		g.expected_enum = expected.name
	}
	node := g.a.nodes[int(id)]
	if g.is_ierror_type_name(expected.name()) && node.kind == .none_expr {
		g.write(g.ierror_none_literal_string())
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.is_ierror_type_name(expected.name()) && g.expr_is_error_call(id) {
		g.gen_ierror_from_error_call(node)
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if node.kind == .dump_expr {
		if node.children_count > 0 {
			g.gen_expr_with_expected_type(g.a.child(&node, 0), expected)
		} else {
			g.write('0')
		}
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if expected is types.MultiReturn && node.kind == .if_expr {
		g.gen_if_expr_stmt(node)
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if expected is types.MultiReturn && node.kind == .block {
		if g.gen_multi_return_block_expr(&node, expected) {
			g.expected_expr_type = old_expected
			g.expected_enum = old_expected_enum
			return
		}
	}
	mut actual := g.usable_expr_type(id)
	if node.kind == .ident {
		if param_type := g.current_param_type(node.value) {
			actual = param_type
		}
	}
	if expected is types.String && g.gen_map_str_expr(id, actual) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.gen_current_mut_param_value_read(id, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if node.kind == .cast_expr && node.children_count > 0
		&& g.cast_alias_matches_expected_storage(node.value, expected) {
		g.gen_expr_with_expected_type(g.a.child(&node, 0), expected)
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if expected is types.OptionType || expected is types.ResultType {
		if node.kind == .none_expr || g.expr_is_optional_literal(id, expected)
			|| actual is types.OptionType || actual is types.ResultType {
			g.gen_expr(id)
		} else {
			g.gen_optional_arg(id, expected)
		}
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if _ := fn_type_from(expected) {
		if g.gen_callback_fn_value_for_expected_type(id, expected) {
			g.expected_expr_type = old_expected
			g.expected_enum = old_expected_enum
			return
		}
		if call_name := g.callback_direct_fn_value_name(id, expected) {
			g.write(g.callback_c_fn_name(call_name))
			g.expected_expr_type = old_expected
			g.expected_enum = old_expected_enum
			return
		}
	}
	if expected is types.Array && node.kind == .array_literal {
		g.gen_array_literal_value(node, expected.elem_type)
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if fixed := array_fixed_type(expected) {
		if node.kind == .array_literal {
			g.gen_fixed_array_literal_value(node, fixed)
			g.expected_expr_type = old_expected
			g.expected_enum = old_expected_enum
			return
		}
		if node.kind == .postfix && node.op == .not && node.children_count == 1 {
			child_id := g.a.child(&node, 0)
			child := g.a.nodes[int(child_id)]
			if child.kind == .array_literal {
				g.gen_fixed_array_literal_value(child, fixed)
				g.expected_expr_type = old_expected
				g.expected_enum = old_expected_enum
				return
			}
		}
	}
	if expected !is types.Pointer && expected !is types.Void && expected !is types.OptionType
		&& expected !is types.ResultType && actual is types.Pointer
		&& g.type_names_match(actual.base_type, expected) {
		needs_paren := node.kind !in [.ident, .selector, .call, .index]
		g.write('*')
		if needs_paren {
			g.write('(')
		}
		g.gen_expr(id)
		if needs_paren {
			g.write(')')
		}
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.gen_sum_pointer_value_expr(id, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.gen_interface_value_expr(id, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.gen_sum_constructor_call_with_expected_type(id, node, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	if g.gen_sum_value_expr(id, expected) {
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	clean_expected := select_receive_unalias_type(expected)
	if node.kind == .prefix && node.op in [.minus, .plus] && node.children_count > 0
		&& clean_expected is types.Primitive && clean_expected.props.has(.float)
		&& clean_expected.size == 32 {
		child_id := g.a.child(&node, 0)
		child := g.a.nodes[int(child_id)]
		if child.kind == .float_literal {
			g.write(g.op_str(node.op))
			g.gen_expr_with_expected_type(child_id, expected)
			g.expected_expr_type = old_expected
			g.expected_enum = old_expected_enum
			return
		}
	}
	if node.kind == .float_literal && clean_expected is types.Primitive
		&& clean_expected.props.has(.float) && clean_expected.size == 32 {
		g.write('(float)(')
		g.gen_expr(id)
		g.write(')')
		g.expected_expr_type = old_expected
		g.expected_enum = old_expected_enum
		return
	}
	g.gen_expr(id)
	g.expected_expr_type = old_expected
	g.expected_enum = old_expected_enum
}

fn (g &FlatGen) cast_alias_matches_expected_storage(alias_name string, expected types.Type) bool {
	if alias_name.len == 0 {
		return false
	}
	mut target_name := g.tc.type_aliases[alias_name] or { '' }
	if target_name.len == 0 {
		target_name = g.tc.type_aliases[g.tc.qualify_name(alias_name)] or { '' }
	}
	if target_name.len == 0 {
		return false
	}
	target := select_receive_unalias_type(g.tc.parse_type(target_name))
	expected_base := select_receive_unalias_type(expected)
	return g.type_names_match(target, expected_base)
		|| g.tc.c_type(target) == g.tc.c_type(expected_base)
}

fn (mut g FlatGen) gen_sum_pointer_value_expr(id flat.NodeId, expected types.Type) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	mut sum_type0 := types.Type(types.void_)
	if expected is types.Pointer {
		sum_type0 = expected.base_type
	} else {
		return false
	}

	if sum_type0 is types.Alias {
		sum_type0 = sum_type0.base_type
	}
	if sum_type0 !is types.SumType {
		return false
	}
	sum_type := sum_type0 as types.SumType
	node := g.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return false
	}
	child_id := g.a.child(&node, 0)
	child := g.a.nodes[int(child_id)]
	actual0 := g.sum_cast_actual_type(child_id)
	mut actual_type := if actual0 is types.Alias { actual0.base_type } else { actual0 }
	if actual_type is types.Pointer {
		actual_type = actual_type.base_type
	}
	if actual_type is types.SumType {
		if child.kind == .ident && child.value.starts_with('__sum_ref_')
			&& g.type_names_match(actual_type, sum_type0) {
			ct := g.tc.c_type(sum_type0)
			g.write('(${ct}*)memdup(&')
			g.gen_expr(child_id)
			g.write(', sizeof(${ct}))')
			return true
		}
		return false
	}
	sum_name := g.resolve_sum_name(sum_type.name)
	variant := g.resolve_variant(sum_name, actual_type.name())
	variants := g.tc.sum_types[sum_name] or { return false }
	if variant !in variants {
		return false
	}
	ct := g.tc.c_type(sum_type0)
	g.write('(${ct}*)memdup(&')
	g.gen_sum_cast_expr(sum_type, child_id)
	g.write(', sizeof(${ct}))')
	return true
}

fn (mut g FlatGen) gen_sum_constructor_call_with_expected_type(id flat.NodeId, node flat.Node, expected types.Type) bool {
	_ = id
	sum_type0 := if expected is types.Alias { expected.base_type } else { expected }
	if sum_type0 !is types.SumType || node.kind != .call || node.children_count < 2 {
		return false
	}
	sum_type := sum_type0 as types.SumType
	callee := g.a.child_node(&node, 0)
	if !g.call_callee_names_sum_base(callee, sum_type.name) {
		return false
	}
	g.gen_sum_cast_expr(sum_type, g.a.child(&node, 1))
	return true
}

fn (g &FlatGen) call_callee_names_sum_base(callee flat.Node, sum_name string) bool {
	_ = g
	base := generic_sum_base_name(sum_name)
	short_base := base.all_after_last('.')
	if callee.kind == .ident {
		return callee.value == base || callee.value == short_base
	}
	if callee.kind == .selector {
		return callee.value == short_base || callee.value == base
	}
	if callee.kind == .index && callee.children_count > 0 {
		return g.call_callee_names_sum_base(g.a.child_node(&callee, 0), sum_name)
	}
	return false
}

fn generic_sum_base_name(name string) string {
	bracket := name.index_u8(`[`)
	if bracket > 0 {
		return name[..bracket]
	}
	return name
}

// gen_sum_value_expr emits sum value expr output for c.
fn (mut g FlatGen) gen_sum_value_expr(id flat.NodeId, expected types.Type) bool {
	sum_type0 := if expected is types.Alias { expected.base_type } else { expected }
	if sum_type0 !is types.SumType {
		return false
	}
	sum_type := sum_type0 as types.SumType
	raw_actual0 := g.sum_cast_actual_type(id)
	raw_actual_type := if raw_actual0 is types.Alias { raw_actual0.base_type } else { raw_actual0 }
	if raw_actual_type is types.SumType {
		return false
	}
	if declared := g.selector_declared_type(id) {
		declared0 := if declared is types.Alias { declared.base_type } else { declared }
		if declared0 is types.SumType && g.type_names_match(declared0, sum_type0) {
			return false
		}
	}
	sum_name := g.resolve_sum_name(sum_type.name)
	variants := g.tc.sum_types[sum_name] or { return false }
	mut actual_type := raw_actual0
	mut variant := g.resolve_variant(sum_name, actual_type.name())
	if variant !in variants {
		actual_type = raw_actual_type
		variant = g.resolve_variant(sum_name, actual_type.name())
	}
	if variant !in variants {
		return false
	}
	ct := g.tc.c_type(sum_type0)
	idx := g.sum_type_index(sum_name, variant)
	field := g.sum_field_name(variant)
	if g.variant_references_sum(variant, sum_name) {
		inner_ct := g.value_c_type(g.tc.parse_type(variant))
		g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup(')
		g.gen_sum_variant_memdup_source(id, actual_type)
		g.write(', sizeof(${inner_ct}))}')
		return true
	}
	g.write('(${ct}){.typ = ${idx}, .${field} = ')
	g.gen_expr(id)
	g.write('}')
	return true
}

fn (g &FlatGen) sum_cast_actual_type(id flat.NodeId) types.Type {
	mut actual_type := g.tc.resolve_type(id)
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return actual_type
	}
	node := g.a.nodes[int(id)]
	if node.kind == .call {
		declared := g.declared_call_return_type(id)
		if declared !is types.Void && declared !is types.Unknown {
			return declared
		}
	}
	if node.kind == .ident {
		if param_type := g.current_param_type(node.value) {
			return param_type
		}
		if param_type := g.current_param_map_type(node.value) {
			return param_type
		}
		// The local's declared type wins over checker expected-type
		// propagation (`return bare` in a `!Sum` fn annotates `bare` as the
		// sum itself, hiding that it still needs wrapping). Only consulted
		// when the propagated type is a sum - the case that miswraps.
		clean_resolved := if actual_type is types.Alias {
			actual_type.base_type
		} else {
			actual_type
		}
		if clean_resolved is types.SumType && g.tc != unsafe { nil }
			&& g.tc.cur_scope != unsafe { nil } {
			if scope_type := g.tc.cur_scope.lookup(node.value) {
				if scope_type !is types.Void && scope_type !is types.Unknown {
					return scope_type
				}
			}
		}
	}
	if node.kind == .struct_init && node.value.len > 0 {
		// A variant literal (`SNull{}`) may carry the checker's expected-type
		// propagation (the sum type itself); the literal names its own type.
		lit_type := g.tc.parse_type(node.value)
		if lit_type !is types.Unknown {
			return lit_type
		}
	}
	return actual_type
}

// gen_sum_cast_expr emits sum cast expr output for c.
fn (mut g FlatGen) gen_sum_cast_expr(target_type types.SumType, inner_id flat.NodeId) {
	inner := g.a.nodes[int(inner_id)]
	actual_type := g.sum_cast_actual_type(inner_id)
	actual_clean := types.unwrap_pointer(actual_type)
	variant_name0 := if inner.kind == .struct_init {
		inner.value
	} else {
		actual_clean.name()
	}
	variant_name := g.resolve_variant(target_type.name, variant_name0)
	idx := g.sum_type_index(target_type.name, variant_name)
	field := g.sum_field_name(variant_name)
	ct := g.tc.c_type(target_type)
	variant_type := g.tc.parse_type(variant_name)
	variant_is_pointer_arg := actual_type is types.Pointer
		&& g.type_names_match(actual_type.base_type, variant_type)
	if g.variant_references_sum(variant_name, target_type.name) {
		inner_ct := g.value_c_type(variant_type)
		if variant_is_pointer_arg {
			g.write('(${ct}){.typ = ${idx}, .${field} = ')
			if g.pointer_variant_arg_needs_heap_copy(inner) {
				g.write('(${inner_ct}*)memdup(')
				g.gen_expr(inner_id)
				g.write(', sizeof(${inner_ct}))')
			} else {
				g.gen_expr(inner_id)
			}
			g.write('}')
		} else if inner.kind == .struct_init
			&& g.resolve_sum_name(inner.value) == g.resolve_sum_name(target_type.name) {
			g.write('(${ct}){')
			for si in 0 .. inner.children_count {
				sf := g.a.child_node(&inner, si)
				if si > 0 {
					g.write(', ')
				}
				g.write('.${g.cname(sf.value)} = ')
				g.gen_lowered_sum_field_value(target_type.name, sf)
			}
			g.write('}')
		} else if inner.kind == .struct_init {
			g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup(&(${inner_ct}){')
			for si in 0 .. inner.children_count {
				sf := g.a.child_node(&inner, si)
				if si > 0 {
					g.write(', ')
				}
				g.write('.${g.cname(sf.value)} = ')
				g.gen_expr(g.a.child(sf, 0))
			}
			g.write('}, sizeof(${inner_ct}))}')
		} else {
			g.write('(${ct}){.typ = ${idx}, .${field} = (${inner_ct}*)memdup(')
			g.gen_sum_variant_memdup_source(inner_id, variant_type)
			g.write(', sizeof(${inner_ct}))}')
		}
	} else {
		g.write('(${ct}){.typ = ${idx}, .${field} = ')
		if variant_is_pointer_arg {
			g.write('*')
		}
		g.gen_expr(inner_id)
		g.write('}')
	}
}

fn (mut g FlatGen) gen_sum_variant_memdup_source(value_id flat.NodeId, inner_type types.Type) {
	if fixed := array_fixed_type(inner_type) {
		source := g.fixed_array_runtime_copy_source_expr(value_id, fixed)
		if trimmed_space(source).len > 0 {
			g.write(source)
			return
		}
	}
	inner_ct := g.value_c_type(inner_type)
	g.write('(${inner_ct}[]){')
	g.gen_expr_with_expected_type(value_id, inner_type)
	g.write('}')
}

// pointer_variant_arg_needs_heap_copy supports pointer_variant_arg_needs_heap_copy handling in c.
fn (g &FlatGen) pointer_variant_arg_needs_heap_copy(node flat.Node) bool {
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return false
	}
	child_id := g.a.child(&node, 0)
	child := g.a.nodes[int(child_id)]
	if child.kind != .ident {
		return false
	}
	if _ := g.current_param_type(child.value) {
		return true
	}
	if _ := g.current_param_map_type(child.value) {
		return true
	}
	if _ := g.tc.cur_scope.lookup(child.value) {
		return true
	}
	return false
}

// selector_declared_type supports selector declared type handling for FlatGen.
fn (g &FlatGen) selector_declared_type(id flat.NodeId) ?types.Type {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := g.a.child(&node, 0)
	base_type0 := types.unwrap_pointer(g.tc.resolve_type(base_id))
	base_type := if base_type0 is types.Alias { base_type0.base_type } else { base_type0 }
	if base_type is types.Struct {
		return g.struct_field_type(base_type.name, node.value)
	}
	return none
}

fn (g &FlatGen) sum_type_name_for_type(base_type0 types.Type) ?string {
	mut clean := types.unwrap_pointer(base_type0)
	if clean is types.Alias {
		clean = clean.base_type
	}
	if clean is types.SumType {
		sum_name := g.resolve_sum_name(clean.name)
		if sum_name in g.tc.sum_types {
			return sum_name
		}
	}
	if clean is types.Struct {
		sum_name := g.resolve_sum_name(clean.name)
		if sum_name in g.tc.sum_types {
			return sum_name
		}
	}
	return none
}

fn (g &FlatGen) sum_shared_field_type(base_type0 types.Type, field string) ?types.Type {
	return g.sum_shared_field_type_inner(base_type0, field, []string{})
}

fn (g &FlatGen) sum_shared_field_type_inner(base_type0 types.Type, field string, seen []string) ?types.Type {
	sum_name := g.sum_type_name_for_type(base_type0) or { return none }
	if sum_name in seen {
		return none
	}
	variants := g.tc.sum_types[sum_name] or { return none }
	if variants.len == 0 {
		return none
	}
	mut common_type := types.Type(types.void_)
	mut has_common := false
	mut next_seen := seen.clone()
	next_seen << sum_name
	for variant in variants {
		variant_field_type := g.sum_variant_shared_field_type(variant, field, next_seen) or {
			return none
		}
		if !has_common {
			common_type = variant_field_type
			has_common = true
			continue
		}
		if g.tc.c_type(variant_field_type) != g.tc.c_type(common_type) {
			return none
		}
	}
	if g.tc.c_type(common_type) == 'void' {
		return none
	}
	return common_type
}

fn (g &FlatGen) sum_variant_shared_field_type(variant string, field string, seen []string) ?types.Type {
	if variant_field_type := g.struct_field_type(variant, field) {
		return variant_field_type
	}
	if nested_sum := g.sum_type_name_for_type(g.tc.parse_type(variant)) {
		return g.sum_shared_field_type_inner(g.tc.parse_type(nested_sum), field, seen)
	}
	return none
}

fn (mut g FlatGen) gen_sum_shared_field_selector(base_id flat.NodeId, base_type0 types.Type, field string) bool {
	sum_name := g.sum_type_name_for_type(base_type0) or { return false }
	common_type := g.sum_shared_field_type(base_type0, field) or { return false }
	ct := g.tc.c_type(common_type)
	sum_ct := g.tc.c_type(g.tc.parse_type(sum_name))
	g.write('({ ${sum_ct} __sum = ')
	if base_type0 is types.Pointer {
		g.write('*(')
		g.gen_expr(base_id)
		g.write(')')
	} else {
		g.gen_expr(base_id)
	}
	g.writeln('; ${ct} __field = {0};')
	g.gen_sum_shared_field_switch('__sum', sum_name, field, []string{})
	g.write('__field; })')
	return true
}

fn (mut g FlatGen) gen_sum_type_tag_selector(base_id flat.NodeId, base_type0 types.Type, op flat.Op) bool {
	sum_name := g.sum_type_name_for_type(base_type0) or { return false }
	sum_ct := g.tc.c_type(g.tc.parse_type(sum_name))
	g.write('({ ${sum_ct} __sum = ')
	if op == .arrow || base_type0 is types.Pointer {
		g.write('*(')
		g.gen_expr(base_id)
		g.write(')')
	} else {
		g.gen_expr(base_id)
	}
	g.write('; __sum.typ; })')
	return true
}

fn (mut g FlatGen) gen_sum_shared_field_switch(sum_var string, sum_name string, field string, seen []string) {
	if sum_name in seen {
		return
	}
	variants := g.tc.sum_types[sum_name] or { return }
	mut next_seen := seen.clone()
	next_seen << sum_name
	g.writeln('switch (${sum_var}.typ) {')
	for variant in variants {
		idx := g.sum_type_index(sum_name, variant)
		sum_field := g.sum_field_name(variant)
		if _ := g.struct_field_type(variant, field) {
			g.writeln('case ${idx}: if (${sum_var}.${sum_field} != NULL) __field = ${sum_var}.${sum_field}->${c_field_name(field)}; break;')
		} else if nested_sum := g.sum_type_name_for_type(g.tc.parse_type(variant)) {
			nested_ct := g.tc.c_type(g.tc.parse_type(nested_sum))
			nested_var := '__nested_sum_${next_seen.len}'
			g.writeln('case ${idx}: if (${sum_var}.${sum_field} != NULL) { ${nested_ct} ${nested_var} = *${sum_var}.${sum_field};')
			g.gen_sum_shared_field_switch(nested_var, nested_sum, field, next_seen)
			g.writeln('} break;')
		}
	}
	g.writeln('default: break; }')
}

fn (g &FlatGen) c_typedef_cast_call_name(node flat.Node) string {
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	callee := g.a.child_node(&node, 0)
	match callee.kind {
		.ident {
			if callee.value.contains('__') {
				return callee.value
			}
		}
		.selector {
			if callee.children_count > 0 {
				base := g.a.child_node(callee, 0)
				if base.kind == .ident && base.value == 'C' {
					return callee.value
				}
			}
		}
		else {}
	}

	return ''
}

// gen_expr_with_possible_enum_type emits expr with possible enum type output for c.
fn (mut g FlatGen) gen_expr_with_possible_enum_type(id flat.NodeId, expected types.Type) {
	node := g.a.nodes[int(id)]
	mut is_signed_numeric_literal := false
	if node.kind == .prefix && node.op in [.minus, .plus] && node.children_count > 0 {
		child := g.a.child_node(&node, 0)
		is_signed_numeric_literal = child.kind in [.int_literal, .float_literal]
	}
	if expected is types.Enum || node.kind in [.int_literal, .float_literal]
		|| is_signed_numeric_literal {
		g.gen_expr_with_expected_type(id, expected)
		return
	}
	g.gen_expr(id)
}

fn (g &FlatGen) expected_expr_is_optional_struct() bool {
	if g.expected_expr_type is types.Struct {
		return g.expected_expr_type.name.starts_with('Optional')
	}
	return false
}

fn (mut g FlatGen) type_name_c_type(type_name string) string {
	if _ := g.tc.cur_scope.lookup(type_name) {
		return g.cname(type_name)
	}
	if type_name.starts_with('fn_ptr:') {
		return g.resolve_fn_ptr_type(type_name)
	}
	t := g.tc.parse_type(type_name)
	ct := if t is types.OptionType || t is types.ResultType {
		g.optional_type_name(t)
	} else if t is types.Enum {
		g.enum_value_c_type(t)
	} else {
		g.tc.c_type(t)
	}
	if ct.starts_with('fn_ptr:') {
		return g.resolve_fn_ptr_type(ct)
	}
	return ct
}

fn (mut g FlatGen) sizeof_target(value string) string {
	if value.starts_with('fn_ptr:') {
		return g.resolve_fn_ptr_type(value)
	}
	if value.starts_with('[]') || value == 'array' {
		return 'Array'
	}
	if fixed_target := c_fixed_array_typedef_sizeof_target(value) {
		return fixed_target
	}
	// Values of explicitly backed enums use their declared C typedef instead of the
	// integer ABI type. `sizeof` must therefore follow value storage semantics too.
	parsed := g.tc.parse_type(value)
	if parsed is types.Enum {
		return g.value_sizeof_target(parsed)
	}
	if value.contains('.') {
		parts := value.split('.')
		if parts.len > 1 {
			if g.cur_scope_has_local_name(parts[0]) {
				return sizeof_selector_target(parts[0], parts[1..])
			}
			if global := g.sizeof_global_selector_base(parts[0]) {
				return sizeof_selector_target(global, parts[1..])
			}
		}
	}
	if fixed := array_fixed_type(g.tc.parse_type(value)) {
		c_elem, dims := g.fixed_array_decl_parts(fixed)
		return '${c_elem}${dims}'
	}
	return g.type_name_c_type(value)
}

fn c_fixed_array_typedef_sizeof_target(value string) ?string {
	if !value.starts_with('Array_fixed_') {
		return none
	}
	payload := value['Array_fixed_'.len..]
	if !payload.contains('_') {
		return none
	}
	elem := payload.all_before_last('_')
	len := payload.all_after_last('_')
	if elem.len == 0 || len.len == 0 {
		return none
	}
	return '${elem}[${len}]'
}

fn sizeof_selector_target(base string, fields []string) string {
	mut expr := c_name(base)
	for field in fields {
		expr += '.${c_field_name(field)}'
	}
	return expr
}

fn (g &FlatGen) cur_scope_has_local_name(name string) bool {
	mut scope := g.tc.cur_scope
	for scope != unsafe { nil } && scope != g.tc.file_scope {
		for existing in scope.names {
			if existing == name {
				return true
			}
		}
		scope = scope.parent
	}
	return false
}

fn (g &FlatGen) sizeof_global_selector_base(name string) ?string {
	if name.len == 0 || name.contains('.') {
		return none
	}
	current_qname := qualify_name_in_module(g.tc.cur_module, name)
	if current_qname in g.global_types {
		return current_qname
	}
	if mod := g.global_modules[name] {
		if mod.len == 0 || mod == 'main' || mod == 'builtin' || mod == g.tc.cur_module {
			return if mod.len > 0 && mod != 'main' && mod != 'builtin' {
				'${mod}.${name}'
			} else {
				name
			}
		}
	}
	return none
}

// optional_none_type supports optional none type handling for FlatGen.
fn (mut g FlatGen) optional_none_type(id flat.NodeId) types.Type {
	if g.expected_expr_type is types.OptionType || g.expected_expr_type is types.ResultType {
		return g.expected_expr_type
	}
	if int(id) >= 0 && int(id) < g.a.nodes.len {
		node := g.a.nodes[int(id)]
		if node.typ.starts_with('?') || node.typ.starts_with('!') {
			return g.tc.parse_type(node.typ)
		}
	}
	if typ := g.tc.expr_type(id) {
		if typ is types.OptionType || typ is types.ResultType {
			return typ
		}
	}
	if g.cur_fn_ret_is_optional {
		return g.cur_fn_ret
	}
	return types.Type(types.OptionType{
		base_type: types.Type(types.void_)
	})
}

// array_index_info supports array index info handling for c.
fn array_index_info(t types.Type) (bool, bool, types.Array) {
	if t is types.Array {
		return true, false, t
	}
	if t is types.Alias {
		base := t.base_type
		if base is types.Array {
			return true, false, base
		}
	}
	if t is types.Pointer {
		base := t.base_type
		if base is types.Array {
			return true, true, base
		}
		if base is types.Alias {
			alias_base := base.base_type
			if alias_base is types.Array {
				return true, true, alias_base
			}
		}
	}
	return false, false, types.Array{}
}

// valid_node_id supports valid node id handling for FlatGen.
fn (g &FlatGen) valid_node_id(id flat.NodeId) bool {
	return g.a != unsafe { nil } && int(id) >= 0 && int(id) < g.a.nodes.len
}

// const_storage_name supports const storage name handling for FlatGen.
fn (g &FlatGen) const_storage_name(module_name string, name string) string {
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !name.contains('.') {
		return '${module_name}.${name}'
	}
	return name
}

// const_primary_name supports const primary name handling for FlatGen.
fn (g &FlatGen) const_primary_name(name string) string {
	mod := if name in g.const_modules { g.const_modules[name] } else { '' }
	qname := g.const_storage_name(mod, name)
	if qname != name && qname in g.const_vals {
		return qname
	}
	return name
}

// is_const_alias_name reports whether is const alias name applies in c.
fn (g &FlatGen) is_const_alias_name(name string) bool {
	return g.const_primary_name(name) != name
}

// const_ref_name supports const ref name handling for FlatGen.
fn (g &FlatGen) const_ref_name(name string) string {
	if !name.contains('.') && !name.contains('__') {
		cur_qname := g.const_storage_name(g.tc.cur_module, name)
		if cur_qname in g.const_vals {
			return cur_qname
		}
		if name in g.const_vals {
			mod := g.const_modules[name] or { '' }
			if mod.len == 0 || mod == g.tc.cur_module || mod == 'builtin'
				|| (g.tc.cur_module in ['', 'main', 'builtin'] && mod in ['', 'main', 'builtin']) {
				return g.const_primary_name(name)
			}
		}
		if unique := g.unique_const_ref_name(name) {
			return unique
		}
		return ''
	}
	if name in g.const_vals {
		return g.const_primary_name(name)
	}
	if name.contains('.') {
		if name in g.const_vals {
			return g.const_primary_name(name)
		}
	}
	sep := if name.contains('.') {
		'.'
	} else if name.contains('__') {
		'__'
	} else {
		return ''
	}
	short_name := name.all_after_last(sep)
	if short_name !in g.const_vals {
		return ''
	}
	resolved := g.const_primary_name(short_name)
	mod := if resolved in g.const_modules { g.const_modules[resolved] } else { '' }
	if mod.len == 0 {
		return resolved
	}
	ref_mod := name.all_before_last(sep)
	if ref_mod == mod || ref_mod == mod.all_after_last('.') {
		return resolved
	}
	return ''
}

fn (g &FlatGen) unique_const_ref_name(short_name string) ?string {
	// Iterating every const per query cost ~70us a call; the short-name index
	// is built once (const_vals is complete after collect_gen_info) and maps a
	// short name to its unique primary const ('' = ambiguous).
	if isnil(g.const_short_index) {
		return g.unique_const_ref_name_scan(short_name)
	}
	mut idx := g.const_short_index
	if !idx.built {
		idx.built = true
		for name, _ in g.const_vals {
			if !name.contains('.') {
				continue
			}
			short := name.all_after_last('.')
			resolved := g.const_primary_name(name)
			if existing := idx.entries[short] {
				if existing != resolved {
					idx.entries[short] = ''
				}
				continue
			}
			idx.entries[short] = resolved
		}
	}
	found := idx.entries[short_name] or { return none }
	if found.len == 0 {
		return none
	}
	return found
}

fn (g &FlatGen) unique_const_ref_name_scan(short_name string) ?string {
	mut found := ''
	for name, _ in g.const_vals {
		if !name.contains('.') || name.all_after_last('.') != short_name {
			continue
		}
		resolved := g.const_primary_name(name)
		if found.len > 0 && found != resolved {
			return none
		}
		found = resolved
	}
	if found.len == 0 {
		return none
	}
	return found
}

// const_ref_name_from_node converts const ref name from node data for c.
fn (g &FlatGen) const_ref_name_from_node(node flat.Node) string {
	if node.kind == .paren && node.children_count > 0 {
		return g.const_ref_name_from_node(g.a.child_node(&node, 0))
	}
	if node.kind == .ident {
		return g.const_ref_name(node.value)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := g.a.child_node(&node, 0)
		if base.kind == .ident {
			return g.const_ref_name('${base.value}.${node.value}')
		}
	}
	return ''
}

fn (g &FlatGen) build_unique_const_ref_names() map[string]string {
	mut unique_index := map[string]string{}
	for name, _ in g.const_vals {
		if !name.contains('.') {
			continue
		}
		short_name := name.all_after_last('.')
		resolved := g.const_primary_name(name)
		if short_name in unique_index {
			if unique_index[short_name] != resolved {
				unique_index[short_name] = ''
			}
		} else {
			unique_index[short_name] = resolved
		}
	}
	return unique_index
}

fn (g &FlatGen) const_ref_name_fast_for_collect(name string, unique_index map[string]string) string {
	if name.contains('.') || name.contains('__') {
		return g.const_ref_name(name)
	}
	cur_qname := g.const_storage_name(g.tc.cur_module, name)
	if cur_qname in g.const_vals {
		return cur_qname
	}
	if name in g.const_vals {
		mod := g.const_modules[name] or { '' }
		if mod.len == 0 || mod == g.tc.cur_module || mod == 'builtin'
			|| (g.tc.cur_module in ['', 'main', 'builtin'] && mod in ['', 'main', 'builtin']) {
			return g.const_primary_name(name)
		}
	}
	if name in unique_index {
		return unique_index[name]
	}
	return ''
}

fn (g &FlatGen) const_ref_name_from_node_cached_for_collect(node flat.Node, unique_index map[string]string, mut cache map[string]string) string {
	if node.kind == .paren {
		if node.children_count == 0 {
			return ''
		}
		return g.const_ref_name_from_node_cached_for_collect(g.a.child_node(&node, 0),
			unique_index, mut cache)
	}
	if node.kind == .ident {
		cache_key := '${g.tc.cur_module}|ident|${node.value}'
		if cache_key in cache {
			return cache[cache_key]
		}
		const_name := g.const_ref_name_fast_for_collect(node.value, unique_index)
		cache[cache_key] = const_name
		return const_name
	}
	if node.kind == .selector {
		if node.children_count == 0 {
			return ''
		}
		base := g.a.child_node(&node, 0)
		if base.kind != .ident {
			return ''
		}
		cache_key := '${g.tc.cur_module}|selector|${base.value}|${node.value}'
		if cache_key in cache {
			return cache[cache_key]
		}
		const_name := g.const_ref_name_fast_for_collect('${base.value}.${node.value}', unique_index)
		cache[cache_key] = const_name
		return const_name
	}
	return ''
}

fn (g &FlatGen) fixed_storage_candidate_short_name(name string) string {
	if name.contains('.') {
		return name.all_after_last('.')
	}
	if name.contains('__') {
		return name.all_after_last('__')
	}
	return name
}

fn (g &FlatGen) add_fixed_storage_candidate_ref(name string, mut refs map[string]bool, mut shorts map[string]bool) {
	if name.len == 0 {
		return
	}
	refs[name] = true
	short_name := g.fixed_storage_candidate_short_name(name)
	if short_name.len > 0 {
		shorts[short_name] = true
	}
}

fn (mut g FlatGen) collect_fixed_storage_const_candidates(mut candidates map[string]bool, mut refs map[string]bool, mut shorts map[string]bool, mut storage_cache map[string]bool, mut primary_cache map[string]string) {
	for const_name, _ in g.const_vals {
		if !g.const_ref_has_fixed_array_literal_storage(const_name, mut storage_cache) {
			continue
		}
		primary := g.const_primary_name_cached(const_name, mut primary_cache)
		candidates[primary] = true
		g.add_fixed_storage_candidate_ref(const_name, mut refs, mut shorts)
		g.add_fixed_storage_candidate_ref(primary, mut refs, mut shorts)
	}
}

fn (g &FlatGen) const_ref_node_may_match_fixed_candidate(node &flat.Node, ident_refs map[string]bool, shorts map[string]bool) bool {
	if node.kind == .paren {
		if node.children_count == 0 {
			return false
		}
		return g.const_ref_node_may_match_fixed_candidate(g.a.child_node(node, 0), ident_refs,
			shorts)
	}
	if node.kind == .ident {
		if node.value in ident_refs {
			return true
		}
		if node.value.contains('.') {
			return node.value.all_after_last('.') in shorts
		}
		if node.value.contains('__') {
			return node.value.all_after_last('__') in shorts
		}
		return false
	}
	if node.kind == .selector {
		if node.children_count == 0 {
			return false
		}
		if node.value !in shorts {
			return false
		}
		base := g.a.child_node(node, 0)
		if base.kind != .ident {
			return false
		}
		return true
	}
	return false
}

fn (g &FlatGen) const_primary_name_cached(name string, mut cache map[string]string) string {
	if name in cache {
		return cache[name]
	}
	primary := g.const_primary_name(name)
	cache[name] = primary
	return primary
}

fn (mut g FlatGen) const_ref_has_fixed_array_literal_storage(const_name string, mut cache map[string]bool) bool {
	if const_name in cache {
		return cache[const_name]
	}
	mut has_fixed := false
	if val_id := g.const_vals[const_name] {
		if _ := g.const_array_literal_storage_type_for_name(const_name, val_id, g.const_value_type(const_name,
			val_id))
		{
			has_fixed = true
		}
	}
	cache[const_name] = has_fixed
	return has_fixed
}

fn (g &FlatGen) fixed_storage_candidate_primary_from_matched_node_for_collect(node flat.Node, fixed_storage_candidates map[string]bool, unique_index map[string]string, mut ref_cache map[string]string, mut primary_cache map[string]string) string {
	const_name := g.const_ref_name_from_node_cached_for_collect(node, unique_index, mut ref_cache)
	if const_name.len == 0 {
		return ''
	}
	primary := g.const_primary_name_cached(const_name, mut primary_cache)
	if primary !in fixed_storage_candidates {
		return ''
	}
	return primary
}

fn (mut g FlatGen) collect_fixed_storage_consts() {
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	mut cur_module := 'main'
	mut cur_file := ''
	mut fixed_storage_candidates := map[string]bool{}
	mut fixed_candidate_refs := map[string]bool{}
	mut fixed_candidate_shorts := map[string]bool{}
	mut dynamic_uses := map[string]bool{}
	mut indexed_candidates := map[string]bool{}
	mut fixed_safe_refs := map[int]bool{}
	mut fixed_storage_cache := map[string]bool{}
	mut primary_name_cache := map[string]string{}
	g.collect_fixed_storage_const_candidates(mut fixed_storage_candidates, mut
		fixed_candidate_refs, mut fixed_candidate_shorts, mut fixed_storage_cache, mut
		primary_name_cache)
	if fixed_storage_candidates.len == 0 {
		g.tc.cur_module = old_module
		g.tc.cur_file = old_file
		return
	}
	unique_const_ref_names := g.build_unique_const_ref_names()
	mut const_ref_name_cache := map[string]string{}
	mut ref_items := []FixedStorageConstRefItem{}
	mut call_base_items := []FixedStorageConstRefItem{}
	mut index_base_items := []FixedStorageConstRefItem{}
	mut fixed_candidate_idents := fixed_candidate_refs.clone()
	for short_name, _ in fixed_candidate_shorts {
		fixed_candidate_idents[short_name] = true
	}
	for idx := 0; idx < g.a.nodes.len; idx++ {
		node := unsafe { &g.a.nodes[idx] }
		mut kind_id := node.kind_id
		if kind_id == 0 && int(node.kind) != 0 {
			kind_id = int(node.kind)
		}
		if kind_id == 77 {
			cur_file = node.value
			cur_module = 'main'
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		match node.kind {
			.index {
				if node.children_count == 0 {
					continue
				}
				base_id := g.a.child(node, 0)
				base_node := g.a.node(base_id)
				if g.const_ref_node_may_match_fixed_candidate(base_node, fixed_candidate_idents,
					fixed_candidate_shorts)
				{
					g.mark_const_ref_descendants(mut fixed_safe_refs, base_id)
					index_base_items << FixedStorageConstRefItem{
						id:     base_id
						file:   cur_file
						module: cur_module
					}
				}
			}
			.selector {
				if node.value == 'len' && node.children_count > 0 {
					base_id := g.a.child(node, 0)
					base_node := g.a.node(base_id)
					if g.const_ref_node_may_match_fixed_candidate(base_node,
						fixed_candidate_idents, fixed_candidate_shorts)
					{
						g.mark_const_ref_descendants(mut fixed_safe_refs, base_id)
					}
				}
				if g.const_ref_node_may_match_fixed_candidate(node, fixed_candidate_idents,
					fixed_candidate_shorts)
				{
					ref_items << FixedStorageConstRefItem{
						id:     flat.NodeId(idx)
						file:   cur_file
						module: cur_module
					}
				}
			}
			.call {
				if node.children_count == 0 {
					continue
				}
				fn_node := g.a.child_node(node, 0)
				if fn_node.kind == .selector && fn_node.children_count > 0 {
					base_id := g.a.child(fn_node, 0)
					base_node := g.a.node(base_id)
					if g.const_ref_node_may_match_fixed_candidate(base_node,
						fixed_candidate_idents, fixed_candidate_shorts)
					{
						call_base_items << FixedStorageConstRefItem{
							id:     base_id
							file:   cur_file
							module: cur_module
						}
					}
				}
			}
			.ident, .paren {
				if g.const_ref_node_may_match_fixed_candidate(node, fixed_candidate_idents,
					fixed_candidate_shorts)
				{
					ref_items << FixedStorageConstRefItem{
						id:     flat.NodeId(idx)
						file:   cur_file
						module: cur_module
					}
				}
			}
			else {}
		}
	}
	for item in call_base_items {
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		node := g.a.nodes[int(item.id)]
		primary := g.fixed_storage_candidate_primary_from_matched_node_for_collect(node,
			fixed_storage_candidates, unique_const_ref_names, mut const_ref_name_cache, mut
			primary_name_cache)
		if primary.len > 0 {
			dynamic_uses[primary] = true
		}
	}
	for item in ref_items {
		if fixed_safe_refs[int(item.id)] or { false } {
			continue
		}
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		node := g.a.nodes[int(item.id)]
		primary := g.fixed_storage_candidate_primary_from_matched_node_for_collect(node,
			fixed_storage_candidates, unique_const_ref_names, mut const_ref_name_cache, mut
			primary_name_cache)
		if primary.len > 0 {
			dynamic_uses[primary] = true
		}
	}
	for item in index_base_items {
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		node := g.a.nodes[int(item.id)]
		primary := g.fixed_storage_candidate_primary_from_matched_node_for_collect(node,
			fixed_storage_candidates, unique_const_ref_names, mut const_ref_name_cache, mut
			primary_name_cache)
		if primary.len > 0 {
			indexed_candidates[primary] = true
		}
	}
	for const_name, _ in indexed_candidates {
		if dynamic_uses[const_name] {
			continue
		}
		g.fixed_storage_consts[const_name] = true
	}
	g.tc.cur_module = old_module
	g.tc.cur_file = old_file
}

fn (mut g FlatGen) mark_const_ref_descendants(mut ids map[int]bool, id flat.NodeId) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	ids[int(id)] = true
	node := g.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		g.mark_const_ref_descendants(mut ids, g.a.child(&node, 0))
	}
}

fn (mut g FlatGen) const_storage_type_from_node(node flat.Node) ?types.Type {
	const_name := g.const_ref_name_from_node(node)
	if const_name.len == 0 {
		return none
	}
	val_id := g.const_vals[const_name] or { return none }
	fallback := g.const_value_type(const_name, val_id)
	if fallback is types.ArrayFixed {
		return fallback
	}
	if !g.fixed_storage_consts[g.const_primary_name(const_name)] {
		return none
	}
	return g.const_array_literal_storage_type_for_name(const_name, val_id, fallback)
}

fn (mut g FlatGen) gen_const_fixed_storage_len(node flat.Node) bool {
	if const_type := g.const_storage_type_from_node(node) {
		if fixed := array_fixed_type(const_type) {
			g.write(g.fixed_array_len_value(fixed))
			return true
		}
	}
	return false
}

fn (mut g FlatGen) const_value_type(const_name string, val_id flat.NodeId) types.Type {
	old_module := g.tc.cur_module
	if mod := g.const_modules[const_name] {
		g.tc.cur_module = mod
	}
	fallback := g.tc.resolve_type(val_id)
	g.tc.cur_module = old_module
	return fallback
}

fn (mut g FlatGen) const_storage_type_for_value(name string, val_id flat.NodeId, fallback types.Type) types.Type {
	if !g.fixed_storage_consts[g.const_primary_name(name)] {
		return fallback
	}
	if fixed := g.const_array_literal_storage_type_for_name(name, val_id, fallback) {
		return fixed
	}
	return fallback
}

fn (mut g FlatGen) const_array_literal_storage_type_for_name(name string, val_id flat.NodeId, fallback types.Type) ?types.Type {
	old_module := g.tc.cur_module
	if mod := g.const_modules[name] {
		g.tc.cur_module = mod
	}
	defer {
		g.tc.cur_module = old_module
	}
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return none
	}
	node := g.a.nodes[int(val_id)]
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	if fallback is types.ArrayFixed {
		return fallback
	}
	mut elem_type := types.Type(types.void_)
	if fallback is types.Array {
		elem_type = fallback.elem_type
	} else {
		elem_type = g.tc.resolve_type(g.a.child(&node, 0))
	}
	if elem_type is types.Array || elem_type is types.Map || elem_type is types.Void
		|| elem_type is types.Unknown {
		return none
	}
	return types.Type(types.ArrayFixed{
		elem_type: elem_type
		len:       node.children_count
	})
}

// const_expr_to_string converts const expr to string data for c.
fn (mut g FlatGen) const_expr_to_string(id flat.NodeId, seen []string) string {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return '0'
	}
	node := g.a.nodes[int(id)]
	return match node.kind {
		.ident, .selector {
			const_name := g.const_ref_name_from_node(node)
			if const_name.len > 0 && const_name !in seen {
				mut next_seen := seen.clone()
				next_seen << const_name
				old_module := g.tc.cur_module
				old_file := g.tc.cur_file
				if mod := g.const_modules[const_name] {
					g.tc.cur_module = mod
				}
				if file := g.const_files[const_name] {
					g.tc.cur_file = file
				}
				dep_expr := g.const_expr_to_string(g.const_vals[const_name], next_seen)
				g.tc.cur_file = old_file
				g.tc.cur_module = old_module
				if trimmed_space(dep_expr).len > 0 {
					return dep_expr
				}
			}
			if node.kind == .selector && node.children_count > 0 {
				base := g.a.child_node(&node, 0)
				if base.kind == .ident {
					fn_name := '${base.value}.${node.value}'
					if fn_name in g.tc.fn_ret_types || fn_name in g.tc.fn_param_types {
						return g.cname(fn_name)
					}
				}
			}
			g.expr_to_string(id)
		}
		.infix {
			lhs := g.const_expr_to_string(g.a.child(&node, 0), seen)
			rhs := g.const_expr_to_string(g.a.child(&node, 1), seen)
			// An int-literal shift by >= 31 would be performed at C `int` width
			// and wrap (`1 << 51`); widen the lhs so the shift happens in 64 bits.
			if node.op == .left_shift && g.shift_needs_64bit_widening(&node) {
				'((u64)(${lhs})) << (${rhs})'
			} else if node.op == .right_shift_unsigned {
				// `>>>` must stay a logical shift in const initializers too;
				// op_str would map it to a plain arithmetic `>>`. The operands
				// are constant expressions, so repeating the rhs in the clamp
				// is side-effect free (statement expressions are not valid in
				// static initializers).
				ut, bits := g.unsigned_shift_type_parts(g.usable_expr_type(g.a.child(&node, 0)))
				'((u64)(${rhs}) >= ${bits} ? (${ut})0 : (${ut})((${ut})(${lhs}) >> (${rhs})))'
			} else {
				'(${lhs}) ${g.op_str(node.op)} (${rhs})'
			}
		}
		.prefix {
			child := g.const_expr_to_string(g.a.child(&node, 0), seen)
			'${g.op_str(node.op)}(${child})'
		}
		.paren {
			child := g.const_expr_to_string(g.a.child(&node, 0), seen)
			'(${child})'
		}
		.cast_expr {
			target_type := g.tc.parse_type(node.value)
			mut ct := g.tc.c_type(target_type)
			if ct.starts_with('fn_ptr:') {
				ct = g.resolve_fn_ptr_type(ct)
			}
			if node.value in g.interfaces || g.tc.qualify_name(node.value) in g.interfaces {
				return '(${ct}){0}'
			}
			if target_type is types.SumType {
				inner_id := g.a.child(&node, 0)
				inner := g.a.nodes[int(inner_id)]
				variant_name0 := if inner.kind == .struct_init {
					inner.value
				} else {
					g.tc.resolve_type(inner_id).name()
				}
				variant_name := g.resolve_variant(target_type.name, variant_name0)
				idx := g.sum_type_index(target_type.name, variant_name)
				field := g.sum_field_name(variant_name)
				inner_val := g.const_expr_to_string(inner_id, seen)
				inner_ct := g.value_c_type(g.tc.parse_type(variant_name))
				payload := if trimmed_space(inner_val).len == 0 { '0' } else { inner_val }
				return '(${ct}){.typ = ${idx}, .${field} = (${inner_ct}[]){${payload}}}'
			}
			if ct == 'map*' {
				child_id := g.a.child(&node, 0)
				if map_addr := g.map_pointer_cast_from_value_address_string(child_id, seen) {
					return map_addr
				}
				child_node := g.a.nodes[int(child_id)]
				child0 := g.const_expr_to_string(child_id, seen)
				child := if trimmed_space(child0).len == 0 { '0' } else { child0 }
				if child_node.kind == .prefix && child_node.op == .amp {
					return '(${ct})(${child})'
				}
				return '&(${child})'
			}
			if target_type !is types.Primitive && target_type !is types.Char
				&& target_type !is types.Rune && target_type !is types.ISize
				&& target_type !is types.USize && target_type !is types.Pointer
				&& target_type !is types.Enum {
				return g.expr_to_string(id)
			}
			child0 := g.const_expr_to_string(g.a.child(&node, 0), seen)
			child := if trimmed_space(child0).len == 0 { '0' } else { child0 }
			'(${ct})(${child})'
		}
		.array_literal {
			mut parts := []string{}
			for i in 0 .. node.children_count {
				parts << g.const_expr_to_string(g.a.child(&node, i), seen)
			}
			'{${parts.join(', ')}}'
		}
		.struct_init {
			ct := g.struct_init_c_type_name(node.value)
			sum_name := g.resolve_sum_name(node.value)
			is_sum_literal := sum_name in g.tc.sum_types
			mut parts := []string{}
			for i in 0 .. node.children_count {
				field := g.a.child_node(&node, i)
				if field.kind == .field_init && field.children_count > 0 {
					val_id := g.a.child(field, 0)
					val_node := g.a.nodes[int(val_id)]
					val := if field.value.len == 0 {
						const_val := g.const_expr_to_string(val_id, seen)
						if trimmed_space(const_val).len > 0 {
							const_val
						} else {
							if ftyp := g.struct_field_type_at(node.value, i) {
								g.expr_to_string_with_expected_type(val_id, ftyp)
							} else {
								g.expr_to_string(val_id)
							}
						}
					} else if is_sum_literal && field.value != 'typ' {
						mut variant := ''
						if field.typ.starts_with('&') {
							variant = field.typ[1..]
						} else if field.typ.len > 0 {
							variant = field.typ
						} else {
							for v in g.tc.sum_types[sum_name] {
								if g.sum_field_name(v) == field.value {
									variant = v
									break
								}
							}
						}
						variant = g.resolve_variant(sum_name, variant)
						inner_ct := g.value_c_type(g.tc.parse_type(variant))
						const_val := g.const_expr_to_string(val_id, seen)
						payload := if trimmed_space(const_val).len > 0 {
							const_val
						} else {
							g.expr_to_string_with_expected_type(val_id, g.tc.parse_type(variant))
						}
						'(${inner_ct}[]){${payload}}'
					} else if ftyp := g.struct_field_type(node.value, field.value) {
						if val_node.kind == .enum_val {
							g.expr_to_string_with_expected_type(val_id, ftyp)
						} else {
							const_val := g.const_expr_to_string(val_id, seen)
							if trimmed_space(const_val).len > 0 {
								const_val
							} else {
								g.expr_to_string_with_expected_type(val_id, ftyp)
							}
						}
					} else {
						const_val := g.const_expr_to_string(val_id, seen)
						if trimmed_space(const_val).len > 0 {
							const_val
						} else {
							g.expr_to_string(val_id)
						}
					}
					if field.value.len == 0 {
						parts << val
					} else {
						parts << '.${g.cname(field.value)} = ${val}'
					}
				} else {
					parts << g.const_expr_to_string(g.a.child(&node, i), seen)
				}
			}
			'(${ct}){${parts.join(', ')}}'
		}
		.string_literal {
			'(string){"${c_escape(node.value)}", ${node.value.len}, 1}'
		}
		.typeof_expr {
			type_name := g.typeof_type_name(node)
			'(string){"${c_escape(type_name)}", ${type_name.len}, 1}'
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .enum_val, .sizeof_expr {
			g.expr_to_string(id)
		}
		.offsetof_expr {
			ct := g.sizeof_target(node.value)
			'offsetof(${ct}, ${g.cname(node.typ)})'
		}
		else {
			g.expr_to_string(id)
		}
	}
}

// const_ident_c_name converts const ident c name data for c.
fn (g &FlatGen) const_ident_c_name(name string) string {
	if name.contains('.') {
		return g.cname(name)
	}
	mod := if name in g.const_modules { g.const_modules[name] } else { '' }
	if mod.len > 0 && mod != 'main' {
		return g.cname('${mod}.${name}')
	}
	if (mod == '' || mod == 'main') && name in g.const_modules {
		return g.cname('main.${name}')
	}
	return g.cname(name)
}

// fixed_array_len_expr supports fixed array len expr handling for FlatGen.
fn (mut g FlatGen) fixed_array_len_expr(type_name string, fallback int) string {
	if type_name.len > 0 {
		typ := g.tc.parse_type(type_name)
		if typ is types.ArrayFixed {
			return g.fixed_array_len_value(typ)
		}
	}
	mut raw_len := ''
	if type_name.starts_with('[') {
		idx := type_name.index_u8(`]`)
		if idx > 1 {
			raw_len = type_name[1..idx]
		}
	} else if type_name.contains('[') && type_name.ends_with(']') {
		idx := type_name.index_u8(`[`)
		if idx >= 0 && idx < type_name.len - 1 {
			raw_len = type_name[idx + 1..type_name.len - 1]
		}
	}
	return g.fixed_array_len_raw(raw_len, fallback)
}

// fixed_array_len_value supports fixed array len value handling for FlatGen.
fn (mut g FlatGen) fixed_array_len_value(arr types.ArrayFixed) string {
	// Prefer the evaluated integer length: a const-expression size (`[segs + 1]f32`)
	// otherwise reaches the raw fallback and is c_name-mangled into garbage.
	if v := g.tc.fixed_array_len_value(arr) {
		return v.str()
	}
	return g.fixed_array_len_raw(arr.len_expr, arr.len)
}

// fixed_array_len_is_zero supports fixed array len is zero handling for FlatGen.
fn (mut g FlatGen) fixed_array_len_is_zero(arr types.ArrayFixed) bool {
	if value := g.tc.fixed_array_len_value(arr) {
		return value == 0
	}
	return trimmed_space(g.fixed_array_len_value(arr)) == '0'
}

// fixed_array_len_raw supports fixed array len raw handling for FlatGen.
fn (mut g FlatGen) fixed_array_len_raw(raw_len string, fallback int) string {
	if raw_len.len == 0 {
		return '${fallback}'
	}
	// A literal or const-expression size (`8`, `SEGS + 1`, `1 << 2`, `8 >>> 1`) folds to an
	// integer; emit that literal so the C dimension is always valid — `>>>` has no C form,
	// so a digit-leading expression like `8 >>> 1` must not be passed through raw — and a
	// non-numeric expr isn't c_name-mangled (`SEGS_+_1`) into an undeclared identifier.
	if v := g.tc.const_int_value(raw_len, []string{}) {
		return v.str()
	}
	clean_len := raw_len.replace('_', '')
	if clean_len.len > 0 && clean_len[0] >= `0` && clean_len[0] <= `9` {
		return clean_len
	}
	const_name := g.const_ref_name(raw_len)
	if const_name.len > 0 {
		expr := g.const_expr_to_string(g.const_vals[const_name], []string{})
		if trimmed_space(expr).len > 0 {
			return expr
		}
		return g.const_ident_c_name(const_name)
	}
	return g.cname(raw_len)
}

fn (mut g FlatGen) fixed_array_decl_parts(arr types.ArrayFixed) (string, string) {
	len_expr := g.fixed_array_len_value(arr)
	if arr.elem_type is types.ArrayFixed {
		base_ct, suffix := g.fixed_array_decl_parts(arr.elem_type)
		return base_ct, '[${len_expr}]${suffix}'
	}
	elem_ct := g.fixed_array_elem_c_type(arr.elem_type)
	return elem_ct, '[${len_expr}]'
}

fn (mut g FlatGen) fixed_array_elem_c_type(elem types.Type) string {
	if elem is types.ArrayFixed {
		return g.fixed_array_c_type(elem)
	}
	if elem is types.OptionType || elem is types.ResultType {
		return g.optional_type_name(elem)
	}
	return g.value_c_type(elem)
}

fn (mut g FlatGen) fixed_array_c_type(arr types.ArrayFixed) string {
	len_text := if arr.len_expr.len > 0 { arr.len_expr } else { arr.len.str() }
	len_name := naming.type_name_part(len_text)
	return 'Array_fixed_${naming.type_name_part(g.fixed_array_elem_c_type(arr.elem_type))}_${len_name}'
}

// infix_can_skip_child_parens reports whether a child infix operand needs no
// surrounding parentheses. For associative logical chains (`||`, `&&`) a child of
// the same operator is safe unparenthesised; this keeps long lowered chains (e.g.
// a `match` over hundreds of enum values → `a || b || c || ...`) from nesting
// parentheses past the C compiler's bracket-depth limit.
fn infix_can_skip_child_parens(parent_op flat.Op, child_op flat.Op) bool {
	return (parent_op == .logical_or && child_op == .logical_or)
		|| (parent_op == .logical_and && child_op == .logical_and)
}

// assoc_infix_chain_len counts how many same-operator infix nodes hang off the left
// spine of `node` (its nesting depth). Capped early since only "very deep" matters.
fn (g &FlatGen) assoc_infix_chain_len(node flat.Node) int {
	op := node.op
	mut cur := node
	mut depth := 0
	for {
		if cur.children_count < 1 {
			break
		}
		lhs_id := g.a.child(&cur, 0)
		if !g.valid_node_id(lhs_id) {
			break
		}
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .infix && lhs.op == op {
			depth++
			if depth > 101 {
				break
			}
			cur = lhs
		} else {
			break
		}
	}
	return depth
}

// gen_assoc_infix_chain emits a left-nested `||`/`&&` chain iteratively, producing the
// same flat `a || b || c …` C as the recursive path but without growing the stack per
// link (a big match's condition chain can be hundreds deep).
fn (mut g FlatGen) gen_assoc_infix_chain(node flat.Node) {
	op := node.op
	op_s := g.op_str(op)
	mut operands := []flat.NodeId{cap: 256}
	mut cur := node
	for {
		operands << g.a.child(&cur, 1)
		lhs_id := g.a.child(&cur, 0)
		lhs := g.a.nodes[int(lhs_id)]
		if lhs.kind == .infix && lhs.op == op && g.valid_node_id(g.a.child(&lhs, 0)) {
			cur = lhs
		} else {
			operands << lhs_id
			break
		}
	}
	for i := operands.len - 1; i >= 0; i-- {
		if i != operands.len - 1 {
			g.write(' ${op_s} ')
		}
		oid := operands[i]
		onode := g.a.nodes[int(oid)]
		if onode.kind == .infix && !infix_can_skip_child_parens(op, onode.op) {
			g.write('(')
			g.gen_expr(oid)
			g.write(')')
		} else {
			g.gen_expr(oid)
		}
	}
}

// gen_expr emits expr output for c.
fn (mut g FlatGen) gen_expr(id flat.NodeId) {
	if int(id) < 0 {
		g.write('0')
		return
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			v := node.value.replace('_', '')
			if v.starts_with('0o') {
				g.write('0${v[2..]}')
			} else {
				g.write(v)
			}
		}
		.float_literal {
			g.write(node.value.replace('_', ''))
		}
		.bool_literal {
			g.write(node.value)
		}
		.char_literal {
			v := node.value
			if v.starts_with('c:') {
				cv := v[2..]
				// A `c'...'` literal is a C string pointer (`C.fputs(c'\n', f)`).
				// Only when a single-character literal is used where a byte-sized
				// value is expected (`data[0] = c'g'`) is it dereferenced to its
				// first byte (reference cgen emits `*"g"` there).
				expected_ct := g.value_c_type(g.expected_expr_type)
				if byte_value := c_char_literal_byte_value(cv) {
					if g.expected_expr_type !is types.Pointer
						&& expected_ct in ['u8', 'i8', 'char', 'u16', 'i16', 'u32', 'i32', 'int', 'u64', 'i64', 'rune', 'usize', 'isize'] {
						if byte_value > 0x7f {
							g.write('((u8)*"${cv}")')
						} else {
							g.write('*"${cv}"')
						}
					} else {
						g.write('"${cv}"')
					}
				} else {
					g.write('"${cv}"')
				}
			} else if v.len == 0 {
				g.write("' '")
			} else if v.len == 1 {
				if v[0] == `\\` {
					g.write("'\\\\'")
				} else if v[0] == `'` {
					g.write("'\\''")
				} else {
					g.write("'${v}'")
				}
			} else if v.starts_with('\\') {
				if codepoint := char_escape_codepoint(v) {
					g.write(codepoint.str())
				} else {
					g.write("'${v}'")
				}
			} else {
				runes := v.runes()
				if runes.len == 0 {
					g.write('0')
				} else {
					g.write(int(runes[0]).str())
				}
			}
		}
		.string_literal {
			sid := g.intern_string(node.value)
			g.write('_str_${sid}')
		}
		.string_interp {
			g.gen_string_interp(node)
		}
		.dump_expr {
			if node.children_count > 0 {
				g.gen_expr(g.a.child(&node, 0))
			} else {
				g.write('0')
			}
		}
		.ident {
			if c_fn_name := g.test_user_main_fn_value_c_name(id, node) {
				g.write(c_fn_name)
				return
			}
			looked_up := g.tc.cur_scope.lookup(node.value) or { types.Type(types.void_) }
			is_local := looked_up !is types.Void
			const_name := if !is_local { g.const_ref_name(node.value) } else { '' }
			if const_name.len > 0 {
				g.write(g.const_ident_c_name(const_name))
			} else if node.value in g.global_modules {
				mod := g.global_modules[node.value]
				if mod.len > 0 && mod != 'main' && mod != 'builtin' {
					g.write(g.cname('${mod}.${node.value}'))
				} else {
					g.write(g.cname(node.value))
				}
			} else if fn_c_name := g.ident_fn_value_c_name(id, node) {
				g.write(fn_c_name)
			} else if g.local_storage_is_shared(node.value) {
				g.write(g.cname(node.value))
				g.write('->val')
			} else {
				g.write(g.cname(node.value))
			}
		}
		.enum_val {
			if expr := g.enum_value_expr_for_key(node.value) {
				g.write(expr)
				return
			}
			if node.typ.len > 0 {
				short_name := node.value.trim_left('.').all_after_last('.')
				if expr := g.enum_value_expr_for_type(node.typ, short_name) {
					g.write(expr)
					return
				}
			}
			if g.expected_enum.len > 0 {
				ekey := '${g.expected_enum}.${node.value}'
				if expr := g.enum_value_expr_for_key(ekey) {
					g.write(expr)
					return
				}
				if !g.expected_enum.contains('.') && g.tc.cur_module.len > 0
					&& g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
					qkey := '${g.tc.cur_module}.${g.expected_enum}.${node.value}'
					if expr := g.enum_value_expr_for_key(qkey) {
						g.write(expr)
						return
					}
				}
			}
			for ename, expr in g.enum_value_exprs {
				if ename.ends_with('.${node.value}') {
					g.write(expr)
					return
				}
			}
			for ename, eval in g.enum_vals {
				if ename.ends_with('.${node.value}') {
					g.write('${eval}')
					return
				}
			}
			g.write('0')
		}
		.call {
			if g.string_plus_call_is_nested(id, node) {
				g.gen_owned_string_plus_chain(id)
				return
			}
			// A call to a fixed-array-returning function yields the wrapper struct;
			// unwrap `.ret_arr` so the result behaves as the array value everywhere
			// (indexing, arg passing, memcpy into a destination).
			ret_t := g.declared_call_return_type(id)
			if ret_fixed := array_fixed_type(ret_t) {
				if g.tc.c_type(ret_fixed) in g.fixed_array_ret_wrappers {
					g.write('(')
					g.gen_call(id, node)
					g.write(').ret_arr')
					return
				}
			}
			g.gen_call(id, node)
		}
		.spawn_expr {
			g.gen_spawn_expr(node)
		}
		.lock_expr {
			g.gen_lock_expr(id, node)
		}
		.select_stmt {
			g.gen_select(id, node, true)
		}
		.infix {
			// A very long left-nested `||`/`&&` chain (e.g. from a big match condition or
			// a `!in [...]` over many values) would recurse once per link and overflow the
			// stack; emit those iteratively. Only pathologically long chains take this path,
			// so ordinary code keeps the existing per-node generation unchanged.
			if (node.op == .logical_or || node.op == .logical_and)
				&& g.assoc_infix_chain_len(node) > 100 {
				g.gen_assoc_infix_chain(node)
				return
			}
			lhs_id := g.a.child(&node, 0)
			rhs_id := g.a.child(&node, 1)
			old_expected_enum := g.expected_enum
			lhs_type := g.usable_expr_type(lhs_id)
			rhs_type := g.usable_expr_type(rhs_id)
			if node.op == .right_shift_unsigned {
				g.gen_unsigned_right_shift(lhs_id, rhs_id, lhs_type)
				g.expected_enum = old_expected_enum
				return
			}
			// An int-literal shift by >= 31 would be performed at C `int` width
			// and wrap (`u64(1 << 40)`); widen the lhs so the shift is 64-bit.
			if node.op == .left_shift && g.shift_needs_64bit_widening(&node) {
				g.write('((u64)(')
				g.gen_expr(lhs_id)
				g.write(') << (')
				g.gen_expr(rhs_id)
				g.write('))')
				g.expected_enum = old_expected_enum
				return
			}
			if node.op == .arrow && lhs_type is types.Channel {
				elem_ct := g.tc.c_type(lhs_type.elem_type)
				g.write('sync__Channel__push(')
				g.gen_expr(lhs_id)
				g.write(', &(${elem_ct}[]){')
				g.gen_expr_with_expected_type(rhs_id, lhs_type.elem_type)
				g.write('})')
				g.expected_enum = old_expected_enum
				return
			}
			if g.gen_array_infix_eq(node, lhs_id, rhs_id, lhs_type, rhs_type) {
				g.expected_enum = old_expected_enum
				return
			}
			if lhs_type is types.String || rhs_type is types.String {
				if g.gen_string_infix_fallback(node, lhs_id, rhs_id) {
					g.expected_enum = old_expected_enum
					return
				}
			}
			lhs_node := g.a.nodes[int(lhs_id)]
			rhs_node := g.a.nodes[int(rhs_id)]
			if node.op in [.eq, .ne]
				&& ((rhs_node.kind == .char_literal && rhs_node.value.starts_with('c:')
				&& lhs_type !is types.Pointer)
				|| (lhs_node.kind == .char_literal && lhs_node.value.starts_with('c:')
				&& rhs_type !is types.Pointer)) {
				if lhs_node.kind == .char_literal && lhs_node.value.starts_with('c:') {
					g.write('*')
				}
				g.gen_expr(lhs_id)
				g.write(' ${g.op_str(node.op)} ')
				if rhs_node.kind == .char_literal && rhs_node.value.starts_with('c:') {
					g.write('*')
				}
				g.gen_expr(rhs_id)
				g.expected_enum = old_expected_enum
				return
			}
			if lhs_type is types.Enum {
				g.expected_enum = lhs_type.name
			} else if rhs_type is types.Enum {
				g.expected_enum = rhs_type.name
			}
			if lhs_type is types.Struct {
				op_name := match node.op {
					.minus { '__minus' }
					.plus { '__plus' }
					.eq { '__eq' }
					.ne { '__ne' }
					.lt { '__lt' }
					.gt { '__gt' }
					.le { '__le' }
					.ge { '__ge' }
					else { '' }
				}

				if op_name.len > 0 {
					method_name := '${lhs_type.name}${op_name}'
					if method_name in g.tc.fn_param_types {
						panic('internal error: struct operator overload reached C backend after transform: ${lhs_type.name} op=${node.op}')
					}
				}
				g.gen_expr(lhs_id)
				g.write(' ${g.op_str(node.op)} ')
				g.gen_expr_with_possible_enum_type(rhs_id, lhs_type)
			} else {
				// In a comparison, a small-int arithmetic operand must wrap at
				// its V width first: C promotes `u8 + u8` to int, so
				// `a + b == 0` would see 256 where V semantics require 0.
				is_comparison := node.op in [.eq, .ne, .lt, .gt, .le, .ge]
				if is_comparison
					&& g.gen_small_int_arith_operand_truncated(lhs_id, lhs_node, lhs_type) {
				} else if lhs_node.kind == .infix
					&& !infix_can_skip_child_parens(node.op, lhs_node.op) {
					g.write('(')
					g.gen_expr_with_possible_enum_type(lhs_id, rhs_type)
					g.write(')')
				} else {
					g.gen_expr_with_possible_enum_type(lhs_id, rhs_type)
				}
				g.write(' ${g.op_str(node.op)} ')
				if is_comparison
					&& g.gen_small_int_arith_operand_truncated(rhs_id, rhs_node, rhs_type) {
				} else if rhs_node.kind == .infix
					&& !infix_can_skip_child_parens(node.op, rhs_node.op) {
					g.write('(')
					g.gen_expr_with_possible_enum_type(rhs_id, lhs_type)
					g.write(')')
				} else {
					g.gen_expr_with_possible_enum_type(rhs_id, lhs_type)
				}
			}
			g.expected_enum = old_expected_enum
		}
		.prefix {
			child_id := g.a.child(&node, 0)
			child := g.a.nodes[int(child_id)]
			if node.value == 'shared' {
				g.gen_expr(child_id)
				return
			}
			if node.op == .arrow {
				child_type0 := g.usable_expr_type(child_id)
				child_type := concrete_receiver_type(child_type0)
				if child_type is types.Channel {
					elem_ct := g.tc.c_type(child_type.elem_type)
					tmp := g.tmp_name()
					g.write('({${elem_ct} ${tmp} = (${elem_ct}){0}; sync__Channel__pop(')
					if child_type0 is types.Pointer {
						g.write('*(')
						g.gen_expr(child_id)
						g.write(')')
					} else {
						g.gen_expr(child_id)
					}
					g.write(', &${tmp}); ${tmp};})')
					return
				}
				g.gen_expr(child_id)
				return
			}
			if node.op == .mul && child.kind == .ident {
				if typ := g.current_param_type(child.value) {
					if typ !is types.Pointer {
						g.gen_expr(child_id)
						return
					}
				} else if typ := g.current_param_map_type(child.value) {
					if typ !is types.Pointer {
						g.gen_expr(child_id)
						return
					}
				}
			}
			if node.op == .mul && child.kind == .paren && child.children_count > 0 {
				inner_id := g.a.child(&child, 0)
				inner := g.a.node(inner_id)
				if inner.kind == .ident {
					g.write('*')
					g.gen_expr(inner_id)
					return
				}
			}
			if node.op == .amp && g.gen_amp_c_string_literal(child_id, child) {
				return
			} else if node.op == .amp && g.gen_current_mut_param_address(id) {
				return
			} else if node.op == .amp && node.typ.len > 0
				&& g.gen_sum_pointer_value_expr(id, g.tc.parse_type(node.typ)) {
				return
			} else if node.op == .amp && child.kind == .struct_init {
				g.gen_heap_struct_init(child)
			} else if node.op == .amp && child.kind == .assoc {
				g.gen_heap_assoc_expr(child)
			} else if node.op == .amp && child.kind == .cast_expr {
				target_type := g.tc.parse_type(child.value)
				ct := g.cast_c_type(target_type)
				cast_arg := g.a.child_node(&child, 0)
				if cast_arg.kind == .nil_literal {
					g.write('(${ct}*)NULL')
					return
				}
				if target_type is types.SumType {
					g.write('(${ct}*)memdup(&')
					g.gen_sum_cast_expr(target_type, g.a.child(&child, 0))
					g.write(', sizeof(${ct}))')
					return
				}
				g.write('(${ct}*)(')
				g.gen_expr(g.a.child(&child, 0))
				g.write(')')
			} else if node.op == .amp && child.kind == .call {
				fn_child := g.a.child_node(&child, 0)
				if fn_child.kind == .selector {
					base_child := g.a.child_node(fn_child, 0)
					if base_child.kind == .ident && base_child.value == 'C' {
						c_struct_prefix := if fn_child.value.len > 0 && fn_child.value[0] >= `a`
							&& fn_child.value[0] <= `z` && !fn_child.value.ends_with('_t') {
							'struct '
						} else {
							''
						}
						g.write('(${c_struct_prefix}${fn_child.value}*)(')
						if child.children_count > 1 {
							g.gen_expr(g.a.child(&child, 1))
						} else {
							g.write('0')
						}
						g.write(')')
					} else {
						g.gen_prefix_op_operand(node.op, child_id)
					}
				} else {
					g.gen_prefix_op_operand(node.op, child_id)
				}
			} else {
				g.gen_prefix_op_operand(node.op, child_id)
			}
		}
		.in_expr {
			// NOTE: range membership, inline-array-literal membership, dynamic- and
			// fixed-array membership, and `!in` negation are lowered by the
			// transformer (transform.transform_in_expr). Map membership stays as an
			// in_expr so each backend can lower it directly.
			lhs_id := g.a.child(&node, 0)
			rhs_id := g.a.child(&node, 1)
			rhs := g.a.nodes[int(rhs_id)]
			rhs_type := g.usable_expr_type(rhs_id)
			clean_rhs := types.unwrap_pointer(rhs_type)
			if clean_rhs is types.Map {
				c_key := g.map_key_temp_c_type(clean_rhs.key_type)
				is_ptr := rhs_type is types.Pointer
				if is_ptr {
					g.write('map__exists(')
				} else {
					g.write('map__exists(&')
				}
				g.gen_expr(rhs_id)
				g.write(', &(${c_key}[]){')
				g.gen_expr(lhs_id)
				g.write('})')
			} else if rhs.kind == .array_literal {
				if rhs.children_count == 0 {
					g.write('false')
				} else {
					lhs_type := g.usable_expr_type(lhs_id)
					g.write('(')
					for i in 0 .. rhs.children_count {
						if i > 0 {
							g.write(' || ')
						}
						elem_id := g.a.child(&rhs, i)
						elem_type := g.usable_expr_type(elem_id)
						if lhs_type is types.String || elem_type is types.String {
							g.write('string__eq(')
							g.gen_expr(lhs_id)
							g.write(', ')
							g.gen_expr(elem_id)
							g.write(')')
						} else {
							g.gen_expr(lhs_id)
							g.write(' == ')
							g.gen_expr(elem_id)
						}
					}
					g.write(')')
				}
			} else if clean_rhs is types.Array {
				fn_name := array_membership_fn_name(clean_rhs.elem_type, false)
				g.write('${fn_name}(')
				// A `mut []T` param (or any `&[]T`) is a pointer in C; the membership
				// helper takes the array by value, so dereference it first.
				if rhs_type is types.Pointer {
					g.write('*')
				}
				g.gen_expr(rhs_id)
				g.write(', ')
				g.gen_expr(lhs_id)
				g.write(')')
			} else if clean_rhs is types.ArrayFixed {
				fn_name := array_membership_fn_name(clean_rhs.elem_type, true)
				len_expr := g.fixed_array_len_value(clean_rhs)
				g.write('${fn_name}(')
				g.gen_expr(rhs_id)
				g.write(', ${len_expr}, ')
				g.gen_expr(lhs_id)
				g.write(')')
			} else if clean_rhs is types.Struct && clean_rhs.name == 'array' {
				lhs_type := g.usable_expr_type(lhs_id)
				fn_name := array_membership_fn_name(lhs_type, false)
				g.write('${fn_name}(')
				g.gen_expr(rhs_id)
				g.write(', ')
				g.gen_expr(lhs_id)
				g.write(')')
			} else {
				panic('internal error: non-map membership reached C backend in ${g.cur_fn_name}: rhs=${rhs_type.name()} kind=${rhs.kind} value=${rhs.value}')
			}
		}
		.postfix {
			child_id := g.a.child(&node, 0)
			child := g.a.nodes[int(child_id)]
			if node.op in [.inc, .dec] {
				if atomic_type := g.atomic_selector_type(child_id) {
					op := if node.op == .inc { 'add' } else { 'sub' }
					g.write('atomic_fetch_${op}_${g.atomic_helper_suffix(atomic_type)}(&(')
					g.gen_expr(child_id)
					g.write('), 1)')
					return
				}
			}
			if child.kind == .ident && g.current_param_is_mut(child.value) {
				g.write('(*')
				g.gen_expr(child_id)
				g.write(')')
			} else {
				g.gen_expr(child_id)
			}
			g.write(g.op_str(node.op))
		}
		.paren {
			g.write('(')
			g.gen_expr(g.a.child(&node, 0))
			g.write(')')
		}
		.selector {
			base_id := g.a.child(&node, 0)
			base := g.a.nodes[int(base_id)]
			if base.kind == .typeof_expr {
				if node.value == 'name' {
					g.gen_typeof_name(base)
					return
				}
				if node.value == 'idx' {
					g.write(g.typeof_type_index(base).str())
					return
				}
			}
			base_type0 := g.usable_expr_type(base_id)
			if base_type0 is types.Channel && node.value in ['closed', 'len', 'cap'] {
				if node.value == 'closed' {
					g.write('(atomic_load_u16(&')
					g.gen_expr(base_id)
					g.write('->closed) != 0)')
				} else if node.value == 'cap' {
					g.write('((int)(')
					g.gen_expr(base_id)
					g.write('->cap))')
				} else {
					g.write('sync__Channel__len(')
					g.gen_expr(base_id)
					g.write(')')
				}
				return
			}
			base_is_local := if base.kind == .ident {
				g.selector_base_is_value(base.value)
			} else {
				false
			}
			// A method used as a value (e.g. `game.draw` passed as a callback) rather
			// than a field access — bind the receiver and yield a wrapper function.
			if g.gen_method_value_closure(base_id, base_type0, node.value) {
				return
			}
			mut enum_selector_qbase := if base.kind == .ident && base.value != 'C' && !base_is_local {
				g.enum_selector_base_name(base.value) or { '' }
			} else {
				''
			}
			// Fully qualified enum value: `mod.Enum.field` — the base is itself a
			// selector over a module ident, not a plain ident. Enum type names are
			// capitalized and module names are not, which filters out ordinary
			// `a.b.c` field chains before any lookup.
			if enum_selector_qbase.len == 0 && base.kind == .selector && base.children_count > 0
				&& base.value.len > 0 && base.value[0] >= `A` && base.value[0] <= `Z` {
				base_base := g.a.child_node(&base, 0)
				if base_base.kind == .ident && base_base.value != 'C' && base_base.value.len > 0
					&& base_base.value[0] >= `a` && base_base.value[0] <= `z`
					&& !g.selector_base_is_value(base_base.value) {
					enum_selector_qbase = g.enum_selector_base_name('${base_base.value}.${base.value}') or {
						''
					}
				}
			}
			if base.kind == .ident && base.value == 'C' {
				g.write(c_winapi_wide_export_name(node.value))
			} else if enum_selector_qbase.len > 0 {
				ekey := '${enum_selector_qbase}.${node.value}'
				if expr := g.enum_value_expr_for_key(ekey) {
					g.write(expr)
				} else {
					g.write('0')
				}
			} else if g.gen_local_shared_value_selector(base_id, node.value) {
				// handled
			} else if g.gen_shared_field_value_selector(base_id, base_type0, node.value, node.op) {
				// handled
			} else if node.value == 'len' && g.gen_const_fixed_storage_len(base) {
				// handled
			} else if base_type0 is types.String && node.value == 'len' {
				// A smartcast variant base is a deref (`*f._string`); without parens
				// the member access would bind first (`*f._string.len`).
				str_needs_paren := base.kind !in [.ident, .selector, .call, .index]
				if str_needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if str_needs_paren {
					g.write(')')
				}
				g.write('.len')
			} else if types.unwrap_pointer(base_type0) is types.Array && node.value == 'len' {
				needs_paren := base.kind !in [.ident, .selector, .call]
				if needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if needs_paren {
					g.write(')')
				}
				if base_type0 is types.Pointer {
					g.write('->len')
				} else {
					g.write('.len')
				}
			} else if node.value == '__v_sum_type_tag__'
				&& g.gen_sum_type_tag_selector(base_id, base_type0, node.op) {
				// handled
			} else if g.gen_sum_shared_field_selector(base_id, base_type0, node.value) {
				// handled
			} else if base.kind == .call && base.children_count == 2
				&& g.c_typedef_cast_call_name(base).len > 0 {
				cast_name := g.c_typedef_cast_call_name(base)
				cast_arg_id := g.a.child(&base, 1)
				g.write('((${g.cname(cast_name)}*)')
				g.gen_expr(cast_arg_id)
				g.write(')->${g.cname(node.value)}')
			} else if base.kind == .cast_expr && base.children_count > 0
				&& (base.value.starts_with('C.') || base.value.starts_with('&C.')
				|| (base.value.contains('__') && !base.value.starts_with('&'))) {
				cast_child_id := g.a.child(&base, 0)
				cast_type := g.tc.parse_type(base.value)
				if cast_type is types.Pointer {
					ct := g.cast_c_type(cast_type)
					g.write('((${ct})')
					g.gen_expr(cast_child_id)
					g.write(')->${g.cname(node.value)}')
				} else {
					cast_name := if base.value.starts_with('C.') {
						base.value[2..]
					} else {
						base.value
					}
					g.write('((${g.cname(cast_name)}*)')
					g.gen_expr(cast_child_id)
					g.write(')->${g.cname(node.value)}')
				}
			} else if base.kind == .cast_expr && base.children_count > 0 {
				needs_paren := base.kind !in [.ident, .selector]
				if needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if needs_paren {
					g.write(')')
				}
				if node.op == .arrow || base_type0 is types.Pointer {
					g.write('->')
				} else {
					g.write('.')
				}
				g.write(g.cname(node.value))
			} else if node.value == 'len' && base.kind == .ident {
				base_type := g.tc.resolve_type(base_id)
				if fixed := array_fixed_type(types.unwrap_pointer(base_type)) {
					g.write(g.fixed_array_len_value(fixed))
				} else {
					raw_type := g.tc.cur_scope.lookup(base.value) or { base_type }
					g.gen_expr(base_id)
					if raw_type is types.Pointer {
						g.write('->len')
					} else {
						g.write('.len')
					}
				}
			} else if base.kind == .ident && !base_is_local && g.selector_base_is_module(base.value) {
				mod := g.selector_base_module(base.value) or { '' }
				short_mod := if mod.contains('.') {
					mod.all_after_last('.')
				} else {
					mod
				}
				// A module-level const is stored under the importing module's full path
				// (e.g. `v3.gen.wasm`), matching its function naming. Reference it by that
				// exact storage name rather than the short alias, otherwise we'd emit an
				// undeclared `wasm__x` for a const defined as `v3__gen__wasm__x`.
				full_qname := g.const_storage_name(mod, node.value)
				if full_qname in g.const_vals {
					g.write(g.cname(full_qname))
				} else {
					g.write(g.cname('${short_mod}.${node.value}'))
				}
			} else if base.kind == .selector && base.children_count > 0
				&& g.is_module_qualified_enum(base) {
				inner_base := g.a.child_node(&base, 0)
				mod := g.import_alias_module(inner_base.value) or { inner_base.value }
				short_mod := if mod.contains('.') {
					mod.all_after_last('.')
				} else {
					mod
				}
				qname := '${short_mod}.${base.value}'
				if qname in g.tc.enum_names || base.value in g.tc.enum_names {
					ekey := '${qname}.${node.value}'
					ekey2 := '${base.value}.${node.value}'
					if expr := g.enum_value_expr_for_key(ekey) {
						g.write(expr)
					} else if expr := g.enum_value_expr_for_key(ekey2) {
						g.write(expr)
					} else {
						g.write(g.cname('${qname}.${node.value}'))
					}
				} else {
					g.write(g.cname('${qname}.${node.value}'))
				}
			} else if embedded := g.direct_embedded_field_for_selector(base_type0, node.value) {
				needs_paren := base.kind !in [.ident, .selector]
				if needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if needs_paren {
					g.write(')')
				}
				if node.op == .arrow || base_type0 is types.Pointer {
					g.write('->')
				} else {
					g.write('.')
				}
				g.write(g.cname(embedded.name))
			} else if embedded_path := g.embedded_field_path_for_promoted_selector(base_type0,
				node.value)
			{
				needs_paren := base.kind !in [.ident, .selector]
				if needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if needs_paren {
					g.write(')')
				}
				mut is_ptr := node.op == .arrow || base_type0 is types.Pointer
				mut embedded_owner := types.unwrap_pointer(base_type0)
				for embedded in embedded_path {
					op := if is_ptr { '->' } else { '.' }
					g.write('${op}${g.cname(embedded.name)}')
					is_ptr = embedded.typ is types.Pointer
					embedded_owner = types.unwrap_pointer(embedded.typ)
				}
				final_op := if is_ptr { '->' } else { '.' }
				g.write('${final_op}${g.cname(node.value)}')
				if embedded_owner is types.Struct {
					if _ := g.shared_field_info(embedded_owner.name, node.value) {
						g.write('->val')
					}
				}
			} else {
				needs_paren := base.kind !in [.ident, .selector]
				if needs_paren {
					g.write('(')
				}
				g.gen_expr(base_id)
				if needs_paren {
					g.write(')')
				}
				mut is_ptr := false
				if base.kind == .ident {
					if typ := g.tc.cur_scope.lookup(base.value) {
						is_ptr = typ is types.Pointer
					}
				} else if base.kind == .selector {
					if declared := g.selector_declared_type(base_id) {
						is_ptr = declared is types.Pointer
					} else {
						resolved := g.tc.resolve_type(base_id)
						is_ptr = resolved is types.Pointer
					}
				} else {
					resolved := g.tc.resolve_type(base_id)
					is_ptr = resolved is types.Pointer
				}
				if node.op == .arrow || is_ptr {
					g.write('->')
				} else {
					g.write('.')
				}
				g.write(g.cname(node.value))
			}
		}
		.index {
			base_id := g.a.child(&node, 0)
			mut base_type := g.usable_expr_type(base_id)
			if node.value == 'range' {
				g.gen_slice_expr(node, base_id, base_type)
			} else if base_type is types.Map {
				c_key := g.map_key_temp_c_type(base_type.key_type)
				c_val := g.value_c_type(base_type.value_type)
				g.write('(*(${c_val}*)map__get(&')
				g.gen_expr(base_id)
				g.write(', &(${c_key}[]){')
				g.gen_expr(g.a.child(&node, 1))
				g.write('}, ')
				g.gen_default_value_addr_for_type(base_type.value_type)
				g.write('))')
			} else {
				mut index_base_type := base_type
				if fixed_lit := g.fixed_array_literal_index_type(base_id, node) {
					g.gen_expr_with_expected_type(base_id, types.Type(fixed_lit))
					g.write('[')
					g.gen_expr(g.a.child(&node, 1))
					g.write(']')
					return
				}
				initial_is_fixed_array_index, _, _ := fixed_array_index_info(index_base_type)
				if !initial_is_fixed_array_index {
					base_node := g.a.nodes[int(base_id)]
					if const_type := g.const_storage_type_from_node(base_node) {
						const_is_fixed, _, _ := fixed_array_index_info(const_type)
						if const_is_fixed {
							index_base_type = const_type
						}
					}
				}
				is_fixed_array_index, fixed_is_ptr, _ := fixed_array_index_info(index_base_type)
				if is_fixed_array_index {
					if fixed_is_ptr {
						g.write('(*')
						g.gen_expr(base_id)
						g.write(')')
					} else {
						g.gen_expr(base_id)
					}
					g.write('[')
					g.gen_expr(g.a.child(&node, 1))
					g.write(']')
				} else {
					is_array_index, is_ptr, arr_type := array_index_info(index_base_type)
					if is_array_index {
						index_type := if node.typ.starts_with('?') || node.typ.starts_with('!') {
							g.tc.parse_type(node.typ)
						} else {
							arr_type.elem_type
						}
						c_elem := g.value_c_type(index_type)
						g.write('(*(${c_elem}*)array_get(')
						if is_ptr {
							g.write('*')
						}
						g.gen_expr(base_id)
						g.write(', ')
						g.gen_expr(g.a.child(&node, 1))
						g.write('))')
					} else {
						is_runtime_array, runtime_is_ptr :=
							runtime_array_struct_index_info(index_base_type)
						base_node := g.a.nodes[int(base_id)]
						local_is_runtime_array := if base_node.kind == .ident {
							local_ct := g.local_storage_c_type(base_node.value) or { '' }
							local_ct == 'Array' || local_ct == 'array'
						} else {
							false
						}
						if is_runtime_array || local_is_runtime_array {
							mut index_type := g.usable_expr_type(id)
							if index_type is types.Unknown || index_type is types.Void {
								if node.typ.len > 0 && node.typ != 'unknown' {
									index_type = g.tc.parse_type(node.typ)
								}
							}
							c_elem := g.value_c_type(index_type)
							g.write('(*(${c_elem}*)array_get(')
							if runtime_is_ptr {
								g.write('*')
							}
							g.gen_expr(base_id)
							g.write(', ')
							g.gen_expr(g.a.child(&node, 1))
							g.write('))')
						} else if base_type is types.String {
							// Parenthesize the base: a smartcast sum variant yields a deref
							// like `*v._string`, and `*v._string.str[i]` would bind as
							// `*(v._string.str[i])`. `(*v._string).str[i]` is what we want.
							g.write('(')
							g.gen_expr(base_id)
							g.write(').str[')
							g.gen_expr(g.a.child(&node, 1))
							g.write(']')
						} else if base_type is types.Pointer {
							ptr_type := base_type
							if ptr_type.base_type is types.Void {
								g.write('((u8*)')
								g.gen_expr(base_id)
								g.write(')[')
								g.gen_expr(g.a.child(&node, 1))
								g.write(']')
							} else {
								g.gen_expr(base_id)
								g.write('[')
								g.gen_expr(g.a.child(&node, 1))
								g.write(']')
							}
						} else {
							g.gen_expr(base_id)
							g.write('[')
							g.gen_expr(g.a.child(&node, 1))
							g.write(']')
						}
					}
				}
			}
		}
		.array_init {
			raw_init_type := g.tc.parse_type(node.value)
			init_type := raw_init_type
			if init_type is types.ArrayFixed {
				ct := g.tc.c_type(raw_init_type)
				g.write('(${ct}){0}')
			} else {
				c_elem := g.sizeof_target(node.value)
				g.write('array_new(sizeof(${c_elem}), 0, 0)')
			}
		}
		.map_init {
			g.gen_map_init(id, node)
		}
		.sql_expr {
			panic('internal error: SQL expression reached C backend after transform')
		}
		.cast_expr {
			target_type := g.tc.parse_type(node.value)
			mut ct := g.cast_c_type(target_type)
			if ct.starts_with('fn_ptr:') {
				ct = g.resolve_fn_ptr_type(ct)
			}
			cast_arg := g.a.child_node(&node, 0)
			if cast_arg.kind == .nil_literal && target_type !is types.Pointer {
				g.gen_default_value_for_type(target_type)
				return
			}
			if node.value in g.interfaces || g.tc.qualify_name(node.value) in g.interfaces {
				g.write('(${ct}){0}')
			} else if target_type is types.SumType {
				g.gen_sum_cast_expr(target_type, g.a.child(&node, 0))
			} else if target_type is types.Pointer
				&& g.gen_pointer_cast_fixed_array_literal(g.a.child(&node, 0), target_type, ct) {
				return
			} else if target_type is types.Pointer
				&& g.gen_pointer_cast_from_map_value_address(g.a.child(&node, 0), target_type) {
				return
			} else if ct == 'map*' {
				child_id := g.a.child(&node, 0)
				child_node := g.a.nodes[int(child_id)]
				if child_node.kind == .call && child_node.children_count > 0 {
					callee := g.a.child_node(&child_node, 0)
					if callee.kind == .ident
						&& callee.value in ['array_get', 'array__get', 'map__get', 'map__get_check'] {
						g.write('(${ct})')
						g.gen_expr(child_id)
						return
					}
				}
				if g.gen_map_pointer_cast_from_value_address(child_id) {
					return
				}
				if child_node.kind == .prefix && child_node.op == .amp {
					g.write('(${ct})(')
					g.gen_expr(child_id)
					g.write(')')
				} else {
					g.write('&(')
					g.gen_expr(child_id)
					g.write(')')
				}
				return
			} else if target_type is types.Pointer
				&& g.gen_cast_from_mut_param_address(g.a.child(&node, 0), ct) {
				return
			} else if fixed := array_fixed_type(target_type) {
				literal := g.fixed_array_compound_literal_expr(g.a.child(&node, 0), fixed)
				if trimmed_space(literal).len > 0 {
					g.write(literal)
				} else {
					g.write('(${ct})(')
					g.gen_expr(g.a.child(&node, 0))
					g.write(')')
				}
			} else {
				g.write('(${ct})(')
				g.gen_expr(g.a.child(&node, 0))
				g.write(')')
			}
		}
		.struct_init {
			g.gen_struct_init(node)
		}
		.if_expr {
			g.gen_if_expr(node)
		}
		.array_literal {
			g.write('{')
			for i in 0 .. node.children_count {
				if i > 0 {
					g.write(', ')
				}
				g.gen_expr(g.a.child(&node, i))
			}
			g.write('}')
		}
		.nil_literal {
			g.write('NULL')
		}
		.none_expr {
			if g.is_ierror_type_name(g.expected_expr_type.name()) {
				g.write(g.ierror_none_literal_string())
			} else {
				ct := g.optional_type_name(g.optional_none_type(id))
				g.write('(${ct}){.ok = false}')
			}
		}
		.or_expr {
			g.gen_or_expr(node)
		}
		.block {
			if node.children_count > 1 {
				g.write('({')
				for bi in 0 .. node.children_count - 1 {
					g.gen_node(g.a.child(&node, bi))
				}
				last_id := g.a.child(&node, node.children_count - 1)
				last := g.a.nodes[int(last_id)]
				if last.kind == .expr_stmt {
					g.gen_expr(g.a.child(&last, 0))
				} else if int(last.kind) >= int(flat.NodeKind.int_literal)
					&& int(last.kind) <= int(flat.NodeKind.in_expr) {
					// A bare expression value (lowered const initializers end the
					// block with one); gen_node would emit nothing for it.
					g.gen_expr(last_id)
				} else {
					g.gen_node(last_id)
				}
				g.write(';})')
			} else if node.children_count > 0 {
				last_id := g.a.child(&node, 0)
				last := g.a.nodes[int(last_id)]
				if last.kind == .expr_stmt {
					g.gen_expr(g.a.child(&last, 0))
				} else {
					g.gen_expr(last_id)
				}
			}
		}
		.is_expr {
			expr_id := g.a.child(&node, 0)
			expr_type := g.tc.resolve_type(expr_id)
			clean0 := types.unwrap_pointer(expr_type)
			clean := if clean0 is types.Alias { clean0.base_type } else { clean0 }
			if clean is types.SumType {
				idx := g.sum_type_index(clean.name, node.value)
				g.write('(')
				if expr_type.is_pointer() {
					g.gen_expr(expr_id)
					g.write('->typ == ${idx}')
				} else {
					g.gen_expr(expr_id)
					g.write('.typ == ${idx}')
				}
				g.write(')')
			} else if clean is types.Interface {
				idx := if g.is_ierror_type_name(clean.name) {
					g.ierror_type_id_for_pattern(node.value)
				} else {
					g.iface_type_id_for_pattern(clean.name, node.value)
				}
				if idx == 0 {
					g.write('0')
					return
				}
				g.write('(')
				if expr_type.is_pointer() {
					g.gen_expr(expr_id)
					g.write('->_typ == ${idx}')
				} else {
					g.gen_expr(expr_id)
					g.write('._typ == ${idx}')
				}
				g.write(')')
			} else if g.is_ierror_type_name(types.Type(clean).name()) {
				idx := g.ierror_type_id_for_pattern(node.value)
				if idx == 0 {
					g.write('0')
					return
				}
				g.write('(')
				if expr_type.is_pointer() {
					g.gen_expr(expr_id)
					g.write('->_typ == ${idx}')
				} else {
					g.gen_expr(expr_id)
					g.write('._typ == ${idx}')
				}
				g.write(')')
			} else {
				g.write('1')
			}
		}
		.as_expr {
			expr_id := g.a.child(&node, 0)
			expr_type0 := g.usable_expr_type(expr_id)
			expr_type := if expr_type0 is types.Unknown || expr_type0 is types.Void {
				g.tc.resolve_type(expr_id)
			} else {
				expr_type0
			}
			clean := types.unwrap_pointer(expr_type)
			if clean is types.SumType {
				qv := g.resolve_variant(clean.name, node.value)
				field := g.sum_field_name(qv)
				if g.variant_references_sum(qv, clean.name) {
					g.write('(*')
					if expr_type.is_pointer() {
						g.gen_expr(expr_id)
						g.write('->${field})')
					} else {
						g.gen_expr(expr_id)
						g.write('.${field})')
					}
				} else {
					if expr_type.is_pointer() {
						g.gen_expr(expr_id)
						g.write('->${field}')
					} else {
						g.gen_expr(expr_id)
						g.write('.${field}')
					}
				}
			} else if clean is types.OptionType {
				if clean.base_type is types.Void {
					g.gen_expr(expr_id)
				} else {
					g.gen_expr(expr_id)
					if expr_type.is_pointer() {
						g.write('->value')
					} else {
						g.write('.value')
					}
				}
			} else if clean is types.ResultType {
				if clean.base_type is types.Void {
					g.gen_expr(expr_id)
				} else {
					g.gen_expr(expr_id)
					if expr_type.is_pointer() {
						g.write('->value')
					} else {
						g.write('.value')
					}
				}
			} else if clean is types.Interface || g.is_ierror_type_name(types.Type(clean).name()) {
				target := g.tc.parse_type(node.value)
				if target is types.Pointer {
					g.write('(${g.tc.c_type(target)})')
					g.gen_expr(expr_id)
					if expr_type.is_pointer() {
						g.write('->_object')
					} else {
						g.write('._object')
					}
				} else {
					g.write('(*(${g.tc.c_type(target)}*)')
					g.gen_expr(expr_id)
					if expr_type.is_pointer() {
						g.write('->_object)')
					} else {
						g.write('._object)')
					}
				}
			} else {
				g.gen_expr(expr_id)
			}
		}
		.sizeof_expr {
			g.write('sizeof(${g.sizeof_target(node.value)})')
		}
		.typeof_expr {
			g.gen_typeof_name(node)
		}
		.offsetof_expr {
			ct := g.type_name_c_type(node.value)
			g.write('offsetof(${ct}, ${g.cname(node.typ)})')
		}
		.assoc {
			g.gen_assoc_expr(node)
		}
		.empty {
			g.write('0')
		}
		else {}
	}
}

fn (mut g FlatGen) gen_pointer_cast_fixed_array_literal(arg_id flat.NodeId, target_type types.Pointer, ct string) bool {
	if int(arg_id) < 0 || int(arg_id) >= g.a.nodes.len {
		return false
	}
	mut literal_id := arg_id
	mut arg := g.a.nodes[int(arg_id)]
	if arg.kind == .postfix && arg.op == .not && arg.children_count > 0 {
		literal_id = g.a.child(&arg, 0)
		arg = g.a.nodes[int(literal_id)]
	}
	if arg.kind != .array_literal {
		return false
	}
	elem_ct := g.value_c_type(target_type.base_type)
	g.write('(${ct})((${elem_ct}[]){')
	for i in 0 .. arg.children_count {
		if i > 0 {
			g.write(', ')
		}
		g.gen_expr_with_expected_type(g.a.child(&arg, i), target_type.base_type)
	}
	g.write('})')
	return true
}

fn (mut g FlatGen) gen_typeof_name(node flat.Node) {
	type_name := g.typeof_type_name(node)
	sid := g.intern_string(type_name)
	g.write('_str_${sid}')
}

fn (g &FlatGen) typeof_type_name(node flat.Node) string {
	if node.value.len > 0 {
		return typeof_display_type_name(node.value)
	}
	if node.children_count == 0 {
		return ''
	}
	expr_id := g.a.child(&node, 0)
	expr_type := g.usable_expr_type(expr_id)
	if expr_type !is types.Unknown && expr_type !is types.Void {
		return typeof_display_resolved_type_name(expr_type)
	}
	resolved := g.tc.resolve_type(expr_id)
	if resolved !is types.Unknown && resolved !is types.Void {
		return typeof_display_resolved_type_name(resolved)
	}
	return ''
}

fn typeof_display_resolved_type_name(typ types.Type) string {
	if typ is types.ArrayFixed {
		len_text := if typ.len_expr.len > 0 { typ.len_expr } else { typ.len.str() }
		return '[${len_text}]' + typeof_display_type_name(typ.elem_type.name())
	}
	return typeof_display_type_name(typ.name())
}

// typeof_display_type_name canonicalizes internal suffix-form fixed-array
// texts (`[]int[3]`) back to V syntax (`[][3]int`) for `typeof(x).name`.
fn typeof_display_type_name(name string) string {
	if name.starts_with('[]') {
		return '[]' + typeof_display_type_name(name[2..])
	}
	if name.starts_with('&') {
		return '&' + typeof_display_type_name(name[1..])
	}
	if name.starts_with('?') || name.starts_with('!') {
		return name[..1] + typeof_display_type_name(name[1..])
	}
	if name.starts_with('mut ') {
		return 'mut ' + typeof_display_type_name(name[4..])
	}
	if name.starts_with('shared ') {
		return 'shared ' + typeof_display_type_name(name[7..])
	}
	if name.starts_with('chan ') {
		return 'chan ' + typeof_display_type_name(name[5..])
	}
	if name.starts_with('map[') {
		close := typeof_display_type_name_matching_bracket(name, 3)
		if close > 3 && close < name.len - 1 {
			key := typeof_display_type_name(name[4..close])
			value := typeof_display_type_name(name[close + 1..])
			return 'map[${key}]${value}'
		}
	}
	if name.starts_with('fn(') || name.starts_with('fn (') {
		return typeof_display_fn_type_name(name)
	}
	if name.ends_with(']') && !name.starts_with('[') && !name.starts_with('map[') {
		outer_open := name.index_u8(`[`)
		if outer_open > 0
			&& typeof_display_type_name_matching_bracket(name, outer_open) == name.len - 1 {
			args_text := name[outer_open + 1..name.len - 1]
			if !typeof_display_fixed_array_len_text(args_text) {
				return name[..outer_open] + '[' + typeof_display_type_name_list(args_text) + ']'
			}
		}
		if open_idx := name.last_index('[') {
			if open_idx > 0 {
				len_text := name[open_idx + 1..name.len - 1]
				if typeof_display_fixed_array_len_text(len_text) {
					return '[${len_text}]' + typeof_display_type_name(name[..open_idx])
				}
			}
		}
	}
	return name
}

fn typeof_display_fixed_array_len_text(text string) bool {
	clean := text.trim_space()
	if clean.len == 0 || clean.contains(',') || clean.contains('[') || clean.contains(']') {
		return false
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') || clean.starts_with('chan ')
		|| clean.starts_with('shared ') || clean.starts_with('atomic ') || clean.starts_with('mut ')
		|| clean.starts_with('thread ') {
		return false
	}
	if clean[0] >= `0` && clean[0] <= `9` {
		return true
	}
	if clean[0] == `(` && clean.ends_with(')') {
		return typeof_display_fixed_array_len_text(clean[1..clean.len - 1])
	}
	if types.is_builtin_type_name(clean) {
		return false
	}
	for i, ch in clean {
		if ch in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`] || ((ch == `-` || ch == `&`) && i > 0) {
			return true
		}
	}
	last := clean.all_after_last('.')
	return last.len > 0 && last[0] >= `a` && last[0] <= `z`
}

fn typeof_display_fn_type_name(name string) string {
	clean := name.trim_space()
	open := clean.index_u8(`(`)
	close := typeof_display_type_name_matching_paren(clean, open)
	if close < 0 {
		return name
	}
	params := typeof_display_type_name_list(clean[open + 1..close])
	mut result := 'fn (${params})'
	ret := clean[close + 1..].trim_space()
	if ret.len == 0 {
		return result
	}
	if ret.starts_with('(') {
		ret_close := typeof_display_type_name_matching_paren(ret, 0)
		if ret_close == ret.len - 1 {
			return result + ' (' + typeof_display_type_name_list(ret[1..ret_close]) + ')'
		}
	}
	return result + ' ' + typeof_display_type_name(ret)
}

fn typeof_display_type_name_matching_paren(text string, open int) int {
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

fn typeof_display_type_name_matching_bracket(text string, open int) int {
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

fn typeof_display_type_name_list(text string) string {
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
					parts << typeof_display_type_name(text[start..i].trim_space())
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < text.len {
		parts << typeof_display_type_name(text[start..].trim_space())
	}
	return parts.join(', ')
}

fn (g &FlatGen) typeof_type_index(node flat.Node) int {
	type_name := g.typeof_type_name(node)
	if type_name.len == 0 {
		return 0
	}
	// Builtin types keep V's stable ast `*_type_idx` values (int==8, string==21, ...), so
	// comparisons against `v.ast` constants behave like the reference compiler.
	builtin_idx := builtin_ast_type_idx(type_name)
	if builtin_idx > 0 {
		return builtin_idx
	}
	mut variants := []string{cap: 2}
	variants << type_name
	if !type_name.contains('.') && g.tc.cur_module.len > 0
		&& g.tc.cur_module !in ['', 'main', 'builtin'] {
		variants << '${g.tc.cur_module}.${type_name}'
	}
	mut sum_names := []string{}
	if g.tc.cur_module.len > 0 {
		sum_names << '${g.tc.cur_module}.Primitive'
	}
	sum_names << 'orm.Primitive'
	sum_names << 'Primitive'
	for sum_name in sum_names {
		if sum_name !in g.tc.sum_types {
			continue
		}
		for variant in variants {
			idx := g.sum_type_index(sum_name, variant)
			if idx != 0 {
				return idx
			}
		}
	}
	for sum_name, _ in g.tc.sum_types {
		for variant in variants {
			idx := g.sum_type_index(sum_name, variant)
			if idx != 0 {
				return idx
			}
		}
	}
	return 0
}

// builtin_ast_type_idx maps a builtin type name to V's stable ast `*_type_idx` value
// (vlib/v/ast/types.v), so `typeof[T]().idx` comparisons against `v.ast` constants behave
// like the reference compiler. Returns 0 for non-builtin types.
fn builtin_ast_type_idx(name string) int {
	return match name {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8', 'byte' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'bool' { 19 }
		'none' { 20 }
		'string' { 21 }
		'rune' { 22 }
		'float literal' { 27 }
		'int literal' { 28 }
		'thread' { 29 }
		'nil' { 31 }
		else { 0 }
	}
}

fn (mut g FlatGen) gen_string_infix_fallback(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) bool {
	match node.op {
		.plus {
			g.write('string__plus(')
			g.gen_expr_as_string(lhs_id)
			g.write(', ')
			g.gen_expr_as_string(rhs_id)
			g.write(')')
		}
		.eq {
			g.write('string__eq(')
			g.gen_expr_as_string(lhs_id)
			g.write(', ')
			g.gen_expr_as_string(rhs_id)
			g.write(')')
		}
		.ne {
			g.write('!string__eq(')
			g.gen_expr_as_string(lhs_id)
			g.write(', ')
			g.gen_expr_as_string(rhs_id)
			g.write(')')
		}
		.lt {
			g.write('string__lt(')
			g.gen_expr_as_string(lhs_id)
			g.write(', ')
			g.gen_expr_as_string(rhs_id)
			g.write(')')
		}
		.gt {
			g.write('string__lt(')
			g.gen_expr_as_string(rhs_id)
			g.write(', ')
			g.gen_expr_as_string(lhs_id)
			g.write(')')
		}
		.le {
			g.write('!string__lt(')
			g.gen_expr_as_string(rhs_id)
			g.write(', ')
			g.gen_expr_as_string(lhs_id)
			g.write(')')
		}
		.ge {
			g.write('!string__lt(')
			g.gen_expr_as_string(lhs_id)
			g.write(', ')
			g.gen_expr_as_string(rhs_id)
			g.write(')')
		}
		else {
			return false
		}
	}

	return true
}

fn (mut g FlatGen) gen_array_infix_eq(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type) bool {
	if node.op !in [.eq, .ne] {
		return false
	}
	if lhs_type is types.Pointer || rhs_type is types.Pointer {
		return false
	}
	if g.gen_fixed_array_infix_eq(node, lhs_id, rhs_id, lhs_type, rhs_type) {
		return true
	}
	mut lhs_arr := types.Array{
		elem_type: types.Type(types.void_)
	}
	mut rhs_arr := types.Array{
		elem_type: types.Type(types.void_)
	}
	mut lhs_is_arr := false
	mut rhs_is_arr := false
	if arr := array_like_type(types.unwrap_pointer(lhs_type)) {
		lhs_arr = arr
		lhs_is_arr = true
	}
	if arr := array_like_type(types.unwrap_pointer(rhs_type)) {
		rhs_arr = arr
		rhs_is_arr = true
	}
	if !lhs_is_arr && !rhs_is_arr {
		return false
	}
	if !lhs_is_arr {
		lhs_arr = rhs_arr
	}
	if !rhs_is_arr {
		rhs_arr = lhs_arr
	}
	elem_type := if lhs_arr.elem_type.name() != 'unknown' {
		lhs_arr.elem_type
	} else {
		rhs_arr.elem_type
	}
	if node.op == .ne {
		g.write('!')
	}
	if elem_type is types.String {
		g.write('array_eq_string(')
	} else {
		g.write('array_eq_raw(')
	}
	g.gen_array_value_arg(lhs_id, lhs_type)
	g.write(', ')
	g.gen_array_value_arg(rhs_id, rhs_type)
	if elem_type !is types.String {
		g.write(', sizeof(${g.sizeof_target(g.tc.c_type(elem_type))})')
	}
	g.write(')')
	return true
}

fn (mut g FlatGen) gen_fixed_array_infix_eq(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type) bool {
	mut lhs_fixed := types.ArrayFixed{
		elem_type: types.Type(types.void_)
	}
	mut rhs_fixed := types.ArrayFixed{
		elem_type: types.Type(types.void_)
	}
	mut lhs_is_fixed := false
	mut rhs_is_fixed := false
	if fixed := array_fixed_type(types.unwrap_pointer(lhs_type)) {
		lhs_fixed = fixed
		lhs_is_fixed = true
	}
	if fixed := array_fixed_type(types.unwrap_pointer(rhs_type)) {
		rhs_fixed = fixed
		rhs_is_fixed = true
	}
	if !lhs_is_fixed && !rhs_is_fixed {
		return false
	}
	fixed := if lhs_is_fixed { lhs_fixed } else { rhs_fixed }
	if !lhs_is_fixed && !g.expr_can_be_fixed_array_literal(lhs_id) {
		return false
	}
	if !rhs_is_fixed && !g.expr_can_be_fixed_array_literal(rhs_id) {
		return false
	}
	if node.op == .ne {
		g.write('!')
	}
	g.write('(memcmp(')
	g.gen_expr_with_expected_type(lhs_id, types.Type(fixed))
	g.write(', ')
	g.gen_expr_with_expected_type(rhs_id, types.Type(fixed))
	g.write(', sizeof(${g.tc.c_type(types.Type(fixed))})) == 0)')
	return true
}

fn (g &FlatGen) expr_can_be_fixed_array_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .array_literal {
		return true
	}
	if node.kind == .postfix && node.op == .not && node.children_count == 1 {
		child := g.a.nodes[int(g.a.child(&node, 0))]
		return child.kind == .array_literal
	}
	return false
}

fn (mut g FlatGen) gen_array_value_arg(id flat.NodeId, typ types.Type) {
	if typ is types.Pointer {
		g.write('*')
	}
	g.gen_expr(id)
}

fn (g &FlatGen) fixed_array_literal_index_type(base_id flat.NodeId, node flat.Node) ?types.ArrayFixed {
	if int(base_id) < 0 || int(base_id) >= g.a.nodes.len {
		return none
	}
	base := g.a.nodes[int(base_id)]
	mut literal := base
	if base.kind == .postfix && base.op == .not && base.children_count == 1 {
		child := g.a.nodes[int(g.a.child(&base, 0))]
		if child.kind != .array_literal {
			return none
		}
		literal = child
	} else if base.kind != .array_literal {
		return none
	}
	mut elem_type := g.tc.parse_type(node.typ)
	if elem_type is types.Void && literal.children_count > 0 {
		elem_type = g.usable_expr_type(g.a.child(&literal, 0))
	}
	if elem_type is types.Void {
		return none
	}
	return types.ArrayFixed{
		elem_type: elem_type
		len:       int(literal.children_count)
	}
}

fn char_escape_codepoint(s string) ?int {
	if s.starts_with('\\u{') {
		end := s.index('}') or { return none }
		return parse_hex_codepoint(s[3..end])
	}
	if s.starts_with('\\u') && s.len >= 6 {
		return parse_hex_codepoint(s[2..6])
	}
	if s.starts_with('\\U') && s.len >= 10 {
		return parse_hex_codepoint(s[2..10])
	}
	return none
}

fn c_char_literal_byte_value(s string) ?int {
	if s.len == 1 {
		return int(s[0])
	}
	if s.len < 2 || s[0] != `\\` {
		return none
	}
	if s.len == 2 {
		return int(s[1])
	}
	if s[1] == `x` {
		value := parse_hex_codepoint(s[2..]) or { return none }
		if value <= 0xff {
			return value
		}
		return none
	}
	if s[1] < `0` || s[1] > `7` || s.len > 4 {
		return none
	}
	mut value := 0
	for digit in s[1..].bytes() {
		if digit < `0` || digit > `7` {
			return none
		}
		value = value * 8 + int(digit - `0`)
	}
	if value <= 0xff {
		return value
	}
	return none
}

fn parse_hex_codepoint(hex string) ?int {
	if hex.len == 0 {
		return none
	}
	mut value := 0
	for ch in hex.bytes() {
		digit := if ch >= `0` && ch <= `9` {
			int(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			int(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			int(ch - `A`) + 10
		} else {
			return none
		}
		value = value * 16 + digit
	}
	return value
}

fn array_membership_fn_name(elem_type types.Type, fixed bool) string {
	prefix := if fixed { 'fixed_array_contains_' } else { 'array_contains_' }
	elem_name := elem_type.name()
	suffix := match elem_name {
		'string' { 'string' }
		'u8', 'byte' { 'u8' }
		else { 'int' }
	}

	return prefix + suffix
}

fn (g &FlatGen) is_module_qualified_enum(base flat.Node) bool {
	if base.kind != .selector || base.children_count == 0 {
		return false
	}
	inner_base := g.a.child_node(&base, 0)
	if inner_base.kind != .ident || !g.has_import_alias(inner_base.value) {
		return false
	}
	mod := g.import_alias_module(inner_base.value) or { inner_base.value }
	short_mod := if mod.contains('.') { mod.all_after_last('.') } else { mod }
	qname := '${short_mod}.${base.value}'
	return qname in g.tc.enum_names || base.value in g.tc.enum_names
}

fn (mut g FlatGen) preamble() {
	g.writeln('typedef signed char i8;')
	g.writeln('typedef short i16;')
	g.writeln('typedef int i32;')
	g.writeln('typedef long long i64;')
	g.writeln('typedef unsigned char u8;')
	g.writeln('typedef unsigned char byte;')
	g.writeln('typedef unsigned short u16;')
	g.writeln('typedef unsigned int u32;')
	g.writeln('typedef unsigned long long u64;')
	g.writeln('#ifdef _MSC_VER')
	g.writeln('#ifdef _WIN64')
	g.writeln('typedef unsigned __int64 size_t;')
	g.writeln('typedef __int64 ptrdiff_t;')
	g.writeln('typedef unsigned __int64 uintptr_t;')
	g.writeln('typedef __int64 intptr_t;')
	g.writeln('#else')
	g.writeln('typedef unsigned int size_t;')
	g.writeln('typedef int ptrdiff_t;')
	g.writeln('typedef unsigned int uintptr_t;')
	g.writeln('typedef int intptr_t;')
	g.writeln('#endif')
	g.writeln('#else')
	g.writeln('typedef __SIZE_TYPE__ size_t;')
	g.writeln('typedef __PTRDIFF_TYPE__ ptrdiff_t;')
	g.writeln('typedef __UINTPTR_TYPE__ uintptr_t;')
	g.writeln('typedef __INTPTR_TYPE__ intptr_t;')
	g.writeln('#endif')
	g.writeln('#if !defined(_TIME_T) && !defined(_TIME_T_DEFINED) && !defined(__time_t_defined) && !defined(_BSD_TIME_T_DEFINED_) && !defined(_TIME_T_DECLARED)')
	g.writeln('typedef long long time_t;')
	g.writeln('#endif')
	g.writeln('#ifndef __bool_true_false_are_defined')
	g.writeln('#ifdef _MSC_VER')
	g.writeln('typedef unsigned char bool;')
	g.writeln('#else')
	g.writeln('typedef _Bool bool;')
	g.writeln('#endif')
	g.writeln('#define __bool_true_false_are_defined 1')
	g.writeln('#endif')
	g.writeln('typedef void* voidptr;')
	g.writeln('typedef int int_literal;')
	g.writeln('typedef double float_literal;')
	g.writeln('struct sync__Channel;')
	g.writeln('typedef struct sync__Channel* chan;')
	g.writeln('#ifndef true')
	g.writeln('#define true 1')
	g.writeln('#endif')
	g.writeln('#ifndef false')
	g.writeln('#define false 0')
	g.writeln('#endif')
	g.headerless_libc_preamble()
	g.write_arch_macros()
	g.writeln('')
	if !g.has_builtins {
		g.writeln('typedef struct {')
		g.writeln('\tchar* str;')
		g.writeln('\tint len;')
		g.writeln('\tint is_lit;')
		g.writeln('} string;')
		g.writeln('')
	}
	g.writeln('#define elem_size element_size')
	g.writeln('#define c_name types__c_name')
	if g.has_builtins {
		return
	}
	g.writeln('typedef struct Array { void* data; int len; int cap; int elem_size; } Array;')
	g.writeln('')
}

fn (mut g FlatGen) c99_feature_test_macros() {
	if !g.c99_mode {
		return
	}
	g.writeln('#if defined(__linux__) && !defined(_GNU_SOURCE)')
	g.writeln('#define _GNU_SOURCE')
	g.writeln('#endif')
	g.writeln('#if defined(__linux__) && !defined(_POSIX_C_SOURCE)')
	g.writeln('#define _POSIX_C_SOURCE 200809L')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_libc_preamble() {
	g.collect_preserved_c_fns(c_headerless_libc_declared_fns)
	g.writeln('#ifndef NULL')
	g.writeln('#define NULL ((void*)0)')
	g.writeln('#endif')
	g.writeln('#ifndef offsetof')
	g.writeln('#if defined(_MSC_VER) && !defined(__clang__)')
	g.writeln('#define offsetof(type, member) ((size_t)&(((type*)0)->member))')
	g.writeln('#else')
	g.writeln('#define offsetof(type, member) __builtin_offsetof(type, member)')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifndef UINTPTR_MAX')
	g.writeln('#if defined(__LP64__) || defined(_WIN64)')
	g.writeln('#define UINTPTR_MAX 18446744073709551615ULL')
	g.writeln('#else')
	g.writeln('#define UINTPTR_MAX 4294967295U')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifndef EOF')
	g.writeln('#define EOF (-1)')
	g.writeln('#endif')
	g.writeln('#ifndef RAND_MAX')
	g.writeln('#define RAND_MAX 2147483647')
	g.writeln('#endif')
	g.writeln('#ifndef FLT_EPSILON')
	g.writeln('#define FLT_EPSILON 1.19209290e-7F')
	g.writeln('#endif')
	g.writeln('#ifndef DBL_EPSILON')
	g.writeln('#define DBL_EPSILON 2.2204460492503131e-16')
	g.writeln('#endif')
	g.writeln('#ifndef FLT_MAX')
	g.writeln('#ifdef __FLT_MAX__')
	g.writeln('#define FLT_MAX __FLT_MAX__')
	g.writeln('#else')
	g.writeln('#define FLT_MAX 3.4028234663852886e+38F')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifndef DBL_MAX')
	g.writeln('#ifdef __DBL_MAX__')
	g.writeln('#define DBL_MAX __DBL_MAX__')
	g.writeln('#else')
	g.writeln('#define DBL_MAX 1.7976931348623158e+308')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifndef SEEK_SET')
	g.writeln('#define SEEK_SET 0')
	g.writeln('#endif')
	g.writeln('#ifndef SEEK_CUR')
	g.writeln('#define SEEK_CUR 1')
	g.writeln('#endif')
	g.writeln('#ifndef SEEK_END')
	g.writeln('#define SEEK_END 2')
	g.writeln('#endif')
	g.writeln('#ifndef _IOFBF')
	g.writeln('#define _IOFBF 0')
	g.writeln('#endif')
	g.writeln('#ifndef _IOLBF')
	g.writeln('#define _IOLBF 1')
	g.writeln('#endif')
	g.writeln('#ifndef _IONBF')
	g.writeln('#define _IONBF 2')
	g.writeln('#endif')
	g.headerless_windows_sdk_types()
	g.writeln('#if !defined(__FILE_defined) && !defined(_FILE_DEFINED) && !defined(_FILEDEFED) && !defined(__DEFINED_FILE) && !defined(_FILE_DECLARED) && !defined(__FILE_DECLARED)')
	g.writeln('typedef struct FILE FILE;')
	g.writeln('#endif')
	g.writeln('typedef struct DIR DIR;')
	g.writeln('#if !defined(_SSIZE_T) && !defined(_SSIZE_T_DEFINED) && !defined(__ssize_t_defined) && !defined(_SSIZE_T_DECLARED)')
	g.writeln('typedef intptr_t ssize_t;')
	g.writeln('#endif')
	g.writeln('extern char** environ;')
	g.writeln('void* malloc(size_t size);')
	g.writeln('void* calloc(size_t count, size_t size);')
	g.writeln('void* realloc(void* ptr, size_t size);')
	g.writeln('void free(void* ptr);')
	g.writeln('int fprintf(FILE* stream, const char* format, ...);')
	g.writeln('int fseek(FILE* stream, long offset, int whence);')
	g.writeln('char* getenv(const char* name);')
	g.writeln('int setenv(const char* name, const char* value, int overwrite);')
	g.writeln('void abort(void);')
	for name in c_function_like_macro_decl_names() {
		g.writeln('#ifdef ${name}')
		g.writeln('#undef ${name}')
		g.writeln('#endif')
	}
	g.writeln('void* memset(void* s, int c, size_t n);')
	g.writeln('void* memcpy(void* dest, const void* src, size_t n);')
	g.writeln('void* memmove(void* dest, const void* src, size_t n);')
	g.writeln('int memcmp(const void* s1, const void* s2, size_t n);')
	g.writeln('size_t strlen(const char* s);')
	g.writeln('int strcmp(const char* s1, const char* s2);')
	g.writeln('int strncmp(const char* s1, const char* s2, size_t n);')
	g.writeln('char* strncpy(char* dest, const char* src, size_t n);')
	g.writeln('double floor(double x);')
	g.writeln('double ceil(double x);')
	g.writeln('float floorf(float x);')
	g.writeln('float ceilf(float x);')
	g.writeln('double sqrt(double x);')
	g.writeln('double pow(double x, double y);')
	g.writeln('double ldexp(double x, int exp);')
	g.writeln('double fmod(double x, double y);')
	g.writeln('double cos(double x);')
	g.writeln('double acos(double x);')
	g.writeln('double fabs(double x);')
	g.writeln('#ifndef _WIN32')
	g.writeln('int open(const char* path, int flags, ...);')
	g.writeln('ssize_t read(int fd, void* buf, size_t count);')
	g.writeln('int fork(void);')
	g.writeln('int dup2(int oldfd, int newfd);')
	g.writeln('int execlp(const char* file, const char* arg, ...);')
	g.writeln('int execvp(const char* file, char* const argv[]);')
	g.writeln('void _exit(int status);')
	g.writeln('#endif')
	g.writeln('int access(const char* path, int mode);')
	g.writeln('char* realpath(const char* path, char* resolved_path);')
	g.writeln('char* strrchr(const char* s, int c);')
	g.writeln('char* strstr(const char* haystack, const char* needle);')
	g.writeln('int snprintf(char* str, size_t size, const char* format, ...);')
	g.writeln('int fcntl(int fd, int cmd, ...);')
	g.writeln('int pipe(int* pipefds);')
	g.writeln('int close(int fd);')
	g.writeln('void* signal(int sig, void* handler);')
	g.writeln('int atexit(void (*f)(void));')
	g.writeln('#ifdef __APPLE__')
	g.writeln('const char* _dyld_get_image_name(unsigned int image_index);')
	g.writeln('#endif')
	g.writeln('#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__DragonFly__)')
	g.writeln('extern FILE* __stdinp;')
	g.writeln('extern FILE* __stdoutp;')
	g.writeln('extern FILE* __stderrp;')
	g.writeln('#define stdin __stdinp')
	g.writeln('#define stdout __stdoutp')
	g.writeln('#define stderr __stderrp')
	g.writeln('int* __error(void);')
	g.writeln('#define errno (*__error())')
	g.writeln('#elif defined(__OpenBSD__)')
	g.writeln('struct __sFstub { long _stub; };')
	g.writeln('extern struct __sFstub __stdin[];')
	g.writeln('extern struct __sFstub __stdout[];')
	g.writeln('extern struct __sFstub __stderr[];')
	g.writeln('#define stdin ((FILE*)__stdin)')
	g.writeln('#define stdout ((FILE*)__stdout)')
	g.writeln('#define stderr ((FILE*)__stderr)')
	g.writeln('extern int errno;')
	g.writeln('#elif defined(__NetBSD__)')
	g.writeln('#if defined(__LP64__) || defined(_LP64)')
	g.writeln('struct __netbsd_FILE_stub { unsigned char _opaque[152]; };')
	g.writeln('#else')
	g.writeln('struct __netbsd_FILE_stub { unsigned char _opaque[88]; };')
	g.writeln('#endif')
	g.writeln('extern struct __netbsd_FILE_stub __sF[];')
	g.writeln('#define stdin ((FILE*)&__sF[0])')
	g.writeln('#define stdout ((FILE*)&__sF[1])')
	g.writeln('#define stderr ((FILE*)&__sF[2])')
	g.writeln('extern int errno;')
	g.writeln('#elif defined(__ANDROID__)')
	g.writeln('extern FILE* stdin;')
	g.writeln('extern FILE* stdout;')
	g.writeln('extern FILE* stderr;')
	g.writeln('int* __errno(void);')
	g.writeln('#define errno (*__errno())')
	g.writeln('#elif defined(__linux__)')
	g.writeln('extern FILE* stdin;')
	g.writeln('extern FILE* stdout;')
	g.writeln('extern FILE* stderr;')
	g.writeln('int* __errno_location(void);')
	g.writeln('#define errno (*__errno_location())')
	g.writeln('#elif defined(_WIN32)')
	g.writeln('extern FILE* stdin;')
	g.writeln('extern FILE* stdout;')
	g.writeln('extern FILE* stderr;')
	g.writeln('int* _errno(void);')
	g.writeln('#define errno (*_errno())')
	g.writeln('#else')
	g.writeln('extern FILE* stdin;')
	g.writeln('extern FILE* stdout;')
	g.writeln('extern FILE* stderr;')
	g.writeln('extern int errno;')
	g.writeln('#endif')
	g.writeln('typedef int pid_t;')
	g.writeln('#if !defined(_OFF_T) && !defined(_OFF_T_DEFINED) && !defined(__off_t_defined) && !defined(_BSD_OFF_T_DEFINED_) && !defined(_OFF_T_DECLARED)')
	g.writeln('typedef long long off_t;')
	g.writeln('#endif')
	g.writeln('#if !defined(_BITS_PTHREADTYPES_COMMON_H) && !defined(_PTHREAD_H) && !defined(_PTHREADTYPES_H_) && !defined(_PTHREADTYPES_H) && !defined(__pthread_t_defined)')
	g.writeln('typedef void* pthread_t;')
	g.writeln('typedef union { unsigned char _opaque[64]; long long _align; } pthread_attr_t;')
	g.writeln('typedef union { unsigned char _opaque[128]; long long _align; } pthread_mutex_t;')
	g.writeln('typedef union { unsigned char _opaque[128]; long long _align; } pthread_cond_t;')
	g.writeln('typedef union { unsigned char _opaque[256]; long long _align; } pthread_rwlock_t;')
	g.writeln('typedef union { unsigned char _opaque[64]; long long _align; } pthread_rwlockattr_t;')
	g.writeln('typedef union { unsigned char _opaque[64]; long long _align; } pthread_condattr_t;')
	g.writeln('typedef union { unsigned char _opaque[16]; long long _align; } pthread_once_t;')
	g.writeln('typedef unsigned long pthread_key_t;')
	g.writeln('#endif')
	g.writeln('typedef union { unsigned char _opaque[128]; long long _align; } sem_t;')
	g.writeln('#if !defined(__sigset_t_defined) && !defined(_SIGSET_T_DECLARED) && !defined(_SIGSET_T_DEFINED) && !defined(_SIGSET_T)')
	g.writeln('typedef union { unsigned char _opaque[128]; long long _align; } sigset_t;')
	g.writeln('#endif')
	g.headerless_stdarg_decls()
	g.writeln('#ifndef PTHREAD_MUTEX_INITIALIZER')
	g.writeln('#ifdef __APPLE__')
	g.writeln('#define PTHREAD_MUTEX_INITIALIZER { ._opaque = { 0xa7, 0xab, 0xaa, 0x32 } }')
	g.writeln('#else')
	g.writeln('#define PTHREAD_MUTEX_INITIALIZER { 0 }')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('int pthread_attr_init(pthread_attr_t* attr);')
	g.writeln('int pthread_attr_destroy(pthread_attr_t* attr);')
	g.writeln('int pthread_attr_setstacksize(void* attr, size_t stacksize);')
	g.writeln('int pthread_mutex_init(void* mutex, void* attr);')
	g.writeln('int pthread_mutex_lock(void* mutex);')
	g.writeln('int pthread_mutex_unlock(void* mutex);')
	g.writeln('int pthread_mutex_destroy(void* mutex);')
	g.writeln('int pthread_rwlockattr_init(void* attr);')
	g.writeln('int pthread_rwlockattr_destroy(void* attr);')
	g.writeln('int pthread_rwlock_init(void* rwlock, void* attr);')
	g.writeln('int pthread_rwlock_rdlock(void* rwlock);')
	g.writeln('int pthread_rwlock_wrlock(void* rwlock);')
	g.writeln('int pthread_rwlock_tryrdlock(void* rwlock);')
	g.writeln('int pthread_rwlock_trywrlock(void* rwlock);')
	g.writeln('int pthread_rwlock_unlock(void* rwlock);')
	g.writeln('int pthread_rwlock_destroy(void* rwlock);')
	g.writeln('int pthread_create(void* thread, void* attr, void* start_routine, void* arg);')
	g.writeln('int pthread_join(void* thread, void** retval);')
	g.writeln('int pthread_detach(void* thread);')
	g.writeln('int pthread_cond_init(void* cond, void* attr);')
	g.writeln('int pthread_cond_destroy(void* cond);')
	g.writeln('int pthread_cond_wait(void* cond, void* mutex);')
	g.writeln('int pthread_cond_signal(void* cond);')
	g.writeln('int pthread_cond_broadcast(void* cond);')
	g.writeln('void* malloc(size_t size);')
	g.writeln('void* calloc(size_t count, size_t size);')
	g.writeln('void* realloc(void* ptr, size_t size);')
	g.writeln('void free(void* ptr);')
	g.writeln('int fprintf(FILE* stream, const char* format, ...);')
	g.writeln('int fflush(FILE* stream);')
	// Signature shape covers both the BSD (thunk before compar) and GNU
	// (compar before arg) qsort_r orders; callers pass fn pointers as void*.
	g.writeln('void qsort_r(void* base, size_t nel, size_t width, void* a, void* b);')
	g.writeln('#ifdef __linux__')
	g.writeln('int pthread_rwlockattr_setkind_np(void* attr, int kind);')
	g.writeln('#endif')
	g.writeln('typedef struct SRWLOCK { void* Ptr; } SRWLOCK;')
	g.writeln('typedef struct CONDITION_VARIABLE { void* Ptr; } CONDITION_VARIABLE;')
	g.writeln('typedef void* atomic_uintptr_t;')
	g.writeln('#if !defined(__cplusplus) && !defined(_WCHAR_T) && !defined(_WCHAR_T_DEFINED) && !defined(__WCHAR_T) && !defined(__wchar_t_defined) && !defined(_BSD_WCHAR_T_DEFINED_) && !defined(_WCHAR_T_DECLARED)')
	g.writeln('#ifdef __WCHAR_TYPE__')
	g.writeln('typedef __WCHAR_TYPE__ wchar_t;')
	g.writeln('#elif defined(_WIN32)')
	g.writeln('typedef unsigned short wchar_t;')
	g.writeln('#else')
	g.writeln('typedef unsigned int wchar_t;')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifndef FD_SET')
	g.writeln('#ifndef FD_SETSIZE')
	g.writeln('#define FD_SETSIZE 1024')
	g.writeln('#endif')
	g.headerless_fd_set_struct()
	g.writeln('#endif')
	g.headerless_windows_console_structs()
	g.headerless_winsize_struct()
	g.headerless_addrinfo_struct()
	g.headerless_sockaddr_structs()
	g.headerless_epoll_structs()
	g.headerless_kevent_struct()
	g.headerless_dirent_struct()
	g.headerless_statvfs_struct()
	g.headerless_timeval_struct()
	g.headerless_rusage_struct()
	g.headerless_timespec_struct()
	g.headerless_darwin_task_info_struct()
	g.headerless_utsname_struct()
	g.headerless_stat_struct()
	g.writeln('int stat(char* path, struct stat* buf);')
	g.headerless_tm_struct()
	g.writeln('struct utimbuf { time_t actime; time_t modtime; };')
	g.writeln('time_t mktime(struct tm* timeptr);')
	g.writeln('struct tm* localtime(time_t* timer);')
	g.writeln('int utime(char* filename, struct utimbuf* times);')
	g.writeln('int stat(char* path, struct stat* buf);')
	g.writeln('FILE* fopen(const char* path, const char* mode);')
	g.writeln('FILE* freopen(const char* path, const char* mode, FILE* stream);')
	g.writeln('int fclose(FILE* stream);')
	g.writeln('size_t fread(void* ptr, size_t size, size_t nitems, FILE* stream);')
	g.writeln('size_t fwrite(const void* ptr, size_t size, size_t nitems, FILE* stream);')
	g.writeln('int fseek(FILE* stream, long offset, int whence);')
	g.writeln('long ftell(FILE* stream);')
	g.writeln('#if !defined(_WIN32)')
	g.writeln('int fseeko(FILE* stream, long long offset, int whence);')
	g.writeln('long long ftello(FILE* stream);')
	g.writeln('#endif')
	g.writeln('int remove(const char* path);')
	g.writeln('int rename(const char* from, const char* to);')
	g.writeln('#if !defined(_MODE_T) && !defined(__mode_t_defined) && !defined(_MODE_T_DECLARED)')
	g.writeln('typedef u32 mode_t;')
	g.writeln('#endif')
	g.writeln('time_t time(time_t* tloc);')
	g.writeln('i32 fileno(FILE* stream);')
	g.writeln('i32 ftruncate(i32 fd, u64 length);')
	g.writeln('#if !defined(_WIN32)')
	g.writeln('i32 mkdir(char* path, u32 mode);')
	g.writeln('i32 chmod(char* path, u32 mode);')
	g.writeln('i32 symlink(char* target, char* linkpath);')
	g.writeln('#endif')
	g.headerless_termios_struct()
	g.headerless_platform_constants()
}

fn c_function_like_macro_decl_names() []string {
	return [
		'memset',
		'memcpy',
		'memmove',
		'memcmp',
		'strlen',
		'strcmp',
		'strncmp',
		'strncpy',
	]
}

const c_headerless_libc_declared_fns = [
	'malloc',
	'calloc',
	'realloc',
	'free',
	'fprintf',
	'fseek',
	'getenv',
	'setenv',
	'abort',
	'memset',
	'memcpy',
	'memmove',
	'memcmp',
	'strlen',
	'strcmp',
	'strncmp',
	'strncpy',
	'floor',
	'ceil',
	'floorf',
	'ceilf',
	'sqrt',
	'pow',
	'ldexp',
	'fmod',
	'cos',
	'acos',
	'fabs',
	'open',
	'read',
	'fork',
	'dup2',
	'execlp',
	'execvp',
	'_exit',
	'access',
	'realpath',
	'strrchr',
	'strstr',
	'snprintf',
	'stat',
	'fcntl',
	'pipe',
	'close',
	'signal',
	'atexit',
	'_dyld_get_image_name',
	'__error',
	'__errno',
	'__errno_location',
	'_errno',
	'pthread_attr_init',
	'pthread_attr_destroy',
	'pthread_attr_setstacksize',
	'pthread_mutex_init',
	'pthread_mutex_lock',
	'pthread_mutex_unlock',
	'pthread_mutex_destroy',
	'pthread_create',
	'pthread_join',
	'pthread_detach',
	'pthread_cond_init',
	'pthread_cond_destroy',
	'pthread_cond_wait',
	'pthread_cond_signal',
	'pthread_cond_broadcast',
	'malloc',
	'calloc',
	'realloc',
	'free',
	'clock',
	'fprintf',
	'fflush',
	'qsort_r',
]

fn (mut g FlatGen) headerless_windows_sdk_types() {
	g.writeln('#ifndef WINAPI')
	g.writeln('#if defined(_WIN32) && (defined(__i386__) || defined(_M_IX86))')
	g.writeln('#define WINAPI __stdcall')
	g.writeln('#else')
	g.writeln('#define WINAPI')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('#ifdef _WIN32')
	g.writeln('#if !defined(_WINDEF_) && !defined(_MINWINDEF_)')
	g.writeln('typedef unsigned long DWORD;')
	g.writeln('typedef int BOOL;')
	g.writeln('typedef void* HANDLE;')
	g.writeln('#endif')
	g.writeln('#if !defined(_SECURITY_ATTRIBUTES_DEFINED) && !defined(_SECURITY_ATTRIBUTES)')
	g.writeln('#define _SECURITY_ATTRIBUTES_DEFINED')
	g.writeln('typedef struct SECURITY_ATTRIBUTES { DWORD nLength; void* lpSecurityDescriptor; BOOL bInheritHandle; } SECURITY_ATTRIBUTES;')
	g.writeln('#endif')
	g.writeln('#if !defined(_OVERLAPPED_) && !defined(_OVERLAPPED_DECLARED)')
	g.writeln('#define _OVERLAPPED_DECLARED')
	g.writeln('typedef struct OVERLAPPED { uintptr_t Internal; uintptr_t InternalHigh; union { struct { DWORD Offset; DWORD OffsetHigh; }; void* Pointer; }; HANDLE hEvent; } OVERLAPPED;')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_stdarg_decls() {
	g.writeln('#ifndef va_start')
	g.writeln('#if defined(_MSC_VER) && !defined(__clang__)')
	g.writeln('typedef char* va_list;')
	g.writeln('#define __V_VA_ALIGN(type) ((sizeof(type) + sizeof(void*) - 1) & ~(sizeof(void*) - 1))')
	g.writeln('#define va_start(ap, last) ((void)((ap) = (va_list)&(last) + __V_VA_ALIGN(last)))')
	g.writeln('#define va_arg(ap, type) (*(type*)(((ap) += __V_VA_ALIGN(type)) - __V_VA_ALIGN(type)))')
	g.writeln('#define va_end(ap) ((void)((ap) = (va_list)0))')
	g.writeln('#define va_copy(dst, src) ((void)((dst) = (src)))')
	g.writeln('#else')
	g.writeln('typedef __builtin_va_list va_list;')
	g.writeln('#define va_start(ap, last) __builtin_va_start(ap, last)')
	g.writeln('#define va_arg(ap, type) __builtin_va_arg(ap, type)')
	g.writeln('#define va_end(ap) __builtin_va_end(ap)')
	g.writeln('#define va_copy(dst, src) __builtin_va_copy(dst, src)')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_fd_set_struct() {
	g.writeln('#ifdef _WIN32')
	g.writeln('typedef uintptr_t SOCKET;')
	g.writeln('struct fd_set { unsigned int fd_count; SOCKET fd_array[FD_SETSIZE]; };')
	g.writeln('typedef struct fd_set fd_set;')
	g.writeln('static inline void v_fd_zero(fd_set* set) { set->fd_count = 0; }')
	g.writeln('static inline void v_fd_set(SOCKET fd, fd_set* set) { for (unsigned int i = 0; i < set->fd_count; i++) { if (set->fd_array[i] == fd) { return; } } if (set->fd_count < FD_SETSIZE) { set->fd_array[set->fd_count++] = fd; } }')
	g.writeln('static inline int v_fd_isset(SOCKET fd, fd_set* set) { for (unsigned int i = 0; i < set->fd_count; i++) { if (set->fd_array[i] == fd) { return 1; } } return 0; }')
	g.writeln('#define FD_ZERO(set) v_fd_zero(set)')
	g.writeln('#define FD_SET(fd, set) v_fd_set((SOCKET)(fd), set)')
	g.writeln('#define FD_ISSET(fd, set) v_fd_isset((SOCKET)(fd), set)')
	g.writeln('#elif defined(__APPLE__)')
	g.writeln('#define __V_FD_BITS 32')
	g.writeln('struct fd_set { unsigned int fds_bits[FD_SETSIZE / __V_FD_BITS]; };')
	g.writeln('typedef struct fd_set fd_set;')
	g.writeln('#define FD_ZERO(set) memset((set), 0, sizeof(*(set)))')
	g.writeln('#define FD_SET(fd, set) ((set)->fds_bits[(fd) / __V_FD_BITS] |= (1U << ((fd) % __V_FD_BITS)))')
	g.writeln('#define FD_ISSET(fd, set) (((set)->fds_bits[(fd) / __V_FD_BITS] & (1U << ((fd) % __V_FD_BITS))) != 0)')
	g.writeln('#else')
	g.writeln('#define __V_FD_BITS (8 * (int)sizeof(unsigned long))')
	g.writeln('struct fd_set { unsigned long fds_bits[FD_SETSIZE / __V_FD_BITS]; };')
	g.writeln('typedef struct fd_set fd_set;')
	g.writeln('#define FD_ZERO(set) memset((set), 0, sizeof(*(set)))')
	g.writeln('#define FD_SET(fd, set) ((set)->fds_bits[(fd) / __V_FD_BITS] |= (1UL << ((fd) % __V_FD_BITS)))')
	g.writeln('#define FD_ISSET(fd, set) (((set)->fds_bits[(fd) / __V_FD_BITS] & (1UL << ((fd) % __V_FD_BITS))) != 0)')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_windows_console_structs() {
	g.writeln('#if defined(_WIN32)')
	g.writeln('typedef struct COORD { i16 X; i16 Y; } COORD;')
	g.writeln('typedef struct SMALL_RECT { u16 Left; u16 Top; u16 Right; u16 Bottom; } SMALL_RECT;')
	g.writeln('typedef union uChar { u16 UnicodeChar; u8 AsciiChar; } uChar;')
	g.writeln('typedef struct KEY_EVENT_RECORD { int bKeyDown; u16 wRepeatCount; u16 wVirtualKeyCode; u16 wVirtualScanCode; uChar uChar; u32 dwControlKeyState; } KEY_EVENT_RECORD;')
	g.writeln('typedef struct MOUSE_EVENT_RECORD { COORD dwMousePosition; u32 dwButtonState; u32 dwControlKeyState; u32 dwEventFlags; } MOUSE_EVENT_RECORD;')
	g.writeln('typedef struct WINDOW_BUFFER_SIZE_RECORD { COORD dwSize; } WINDOW_BUFFER_SIZE_RECORD;')
	g.writeln('typedef struct MENU_EVENT_RECORD { u32 dwCommandId; } MENU_EVENT_RECORD;')
	g.writeln('typedef struct FOCUS_EVENT_RECORD { int bSetFocus; } FOCUS_EVENT_RECORD;')
	g.writeln('typedef union Event { KEY_EVENT_RECORD KeyEvent; MOUSE_EVENT_RECORD MouseEvent; WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent; MENU_EVENT_RECORD MenuEvent; FOCUS_EVENT_RECORD FocusEvent; } Event;')
	g.writeln('typedef struct INPUT_RECORD { u16 EventType; Event Event; } INPUT_RECORD;')
	g.writeln('typedef struct CONSOLE_SCREEN_BUFFER_INFO { COORD dwSize; COORD dwCursorPosition; u16 wAttributes; SMALL_RECT srWindow; COORD dwMaximumWindowSize; } CONSOLE_SCREEN_BUFFER_INFO;')
	g.writeln('typedef struct CHAR_INFO { uChar Char; u16 Attributes; } CHAR_INFO;')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_winsize_struct() {
	g.writeln('struct winsize { unsigned short ws_row; unsigned short ws_col; unsigned short ws_xpixel; unsigned short ws_ypixel; };')
}

fn (mut g FlatGen) headerless_addrinfo_struct() {
	g.writeln('#if defined(_WIN32)')
	g.writeln('struct addrinfo { int ai_flags; int ai_family; int ai_socktype; int ai_protocol; size_t ai_addrlen; char* ai_canonname; void* ai_addr; struct addrinfo* ai_next; };')
	g.writeln('#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)')
	g.writeln('struct addrinfo { int ai_flags; int ai_family; int ai_socktype; int ai_protocol; unsigned int ai_addrlen; char* ai_canonname; void* ai_addr; struct addrinfo* ai_next; };')
	g.writeln('#else')
	g.writeln('struct addrinfo { int ai_flags; int ai_family; int ai_socktype; int ai_protocol; unsigned int ai_addrlen; void* ai_addr; char* ai_canonname; struct addrinfo* ai_next; };')
	g.writeln('#endif')
	g.writeln('typedef struct addrinfo addrinfo;')
}

fn (mut g FlatGen) headerless_sockaddr_structs() {
	g.writeln('#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)')
	g.writeln('struct sockaddr { u8 sa_len; u8 sa_family; char sa_data[14]; };')
	g.writeln('struct sockaddr_in { u8 sin_len; u8 sin_family; u16 sin_port; u32 sin_addr; char sin_zero[8]; };')
	g.writeln('struct sockaddr_in6 { u8 sin6_len; u8 sin6_family; u16 sin6_port; u32 sin6_flowinfo; u8 sin6_addr[16]; u32 sin6_scope_id; };')
	g.writeln('#if !defined(_SYS_UN_H) && !defined(_SYS_UN_H_)')
	g.writeln('struct sockaddr_un { u8 sun_len; u8 sun_family; char sun_path[104]; };')
	g.writeln('#endif')
	g.writeln('#else')
	g.writeln('struct sockaddr { u16 sa_family; char sa_data[14]; };')
	g.writeln('struct sockaddr_in { u16 sin_family; u16 sin_port; u32 sin_addr; char sin_zero[8]; };')
	g.writeln('struct sockaddr_in6 { u16 sin6_family; u16 sin6_port; u32 sin6_flowinfo; u8 sin6_addr[16]; u32 sin6_scope_id; };')
	g.writeln('#if !defined(_SYS_UN_H) && !defined(_SYS_UN_H_)')
	g.writeln('struct sockaddr_un { u16 sun_family; char sun_path[108]; };')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_epoll_structs() {
	g.writeln('#if defined(__linux__) || defined(__ANDROID__)')
	g.writeln('typedef union epoll_data { void* ptr; int fd; u32 u32; u64 u64; } epoll_data_t;')
	g.writeln('struct epoll_event { u32 events; epoll_data_t data; } __attribute__((packed));')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_kevent_struct() {
	g.writeln('#if defined(__NetBSD__)')
	g.writeln('struct kevent { uintptr_t ident; u32 filter; u32 flags; u32 fflags; i64 data; void* udata; u64 ext[4]; };')
	g.writeln('#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)')
	g.writeln('struct kevent { uintptr_t ident; i16 filter; u16 flags; u32 fflags; intptr_t data; void* udata; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_dirent_struct() {
	g.writeln('#if defined(__APPLE__)')
	g.writeln('struct dirent { u64 d_ino; u64 d_seekoff; u16 d_reclen; u16 d_namlen; u8 d_type; char d_name[1024]; };')
	g.writeln('#elif defined(__FreeBSD__)')
	g.writeln('struct dirent { u64 d_ino; i64 d_seekoff; u16 d_reclen; u8 d_type; u8 __pad0; u16 d_namlen; u16 __pad1; char d_name[256]; };')
	g.writeln('#elif defined(__OpenBSD__)')
	g.writeln('struct dirent { u64 d_ino; i64 d_seekoff; u16 d_reclen; u8 d_type; u8 d_namlen; u8 __d_padding[4]; char d_name[256]; };')
	g.writeln('#elif defined(__NetBSD__)')
	g.writeln('struct dirent { u64 d_ino; u16 d_reclen; u16 d_namlen; u8 d_type; char d_name[512]; };')
	g.writeln('#elif defined(__DragonFly__)')
	g.writeln('struct dirent { u64 d_ino; u16 d_namlen; u8 d_type; u8 __unused1; u32 __unused2; char d_name[256]; };')
	g.writeln('#elif defined(__linux__) && (defined(__i386__) || defined(__arm__))')
	g.writeln('struct dirent { unsigned long d_ino; long d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };')
	g.writeln('#else')
	g.writeln('struct dirent { u64 d_ino; i64 d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_statvfs_struct() {
	g.writeln('#ifdef __NetBSD__')
	g.writeln('struct statvfs { unsigned long f_flag; unsigned long f_bsize; unsigned long f_frsize; unsigned long f_iosize; u64 f_blocks; u64 f_bfree; u64 f_bavail; u64 f_bresvd; u64 f_files; u64 f_ffree; u64 f_favail; u64 f_fresvd; u64 f_syncreads; u64 f_syncwrites; u64 f_asyncreads; u64 f_asyncwrites; struct { i32 __fsid_val[2]; } f_fsidx; unsigned long f_fsid; unsigned long f_namemax; u32 f_owner; u64 f_spare[4]; char f_fstypename[32]; char f_mntonname[1024]; char f_mntfromname[1024]; char f_mntfromlabel[1024]; };')
	g.writeln('#else')
	g.writeln('struct statvfs { unsigned long f_bsize; unsigned long f_frsize; unsigned long f_blocks; unsigned long f_bfree; unsigned long f_bavail; unsigned long f_files; unsigned long f_ffree; unsigned long f_favail; unsigned long f_fsid; unsigned long f_flag; unsigned long f_namemax; int __f_spare[6]; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_timeval_struct() {
	g.writeln('#if !defined(__timeval_defined) && !defined(_STRUCT_TIMEVAL) && !defined(_TIMEVAL_DEFINED) && !defined(_TIMEVAL_DECLARED)')
	g.writeln('#ifdef _WIN32')
	g.writeln('struct timeval { long tv_sec; long tv_usec; };')
	g.writeln('#else')
	g.writeln('struct timeval { long tv_sec; long tv_usec; };')
	g.writeln('#endif')
	g.writeln('#endif')
	g.writeln('typedef struct timeval timeval;')
}

fn (mut g FlatGen) headerless_rusage_struct() {
	g.writeln('struct rusage { struct timeval ru_utime; struct timeval ru_stime; long ru_maxrss; long ru_ixrss; long ru_idrss; long ru_isrss; long ru_minflt; long ru_majflt; long ru_nswap; long ru_inblock; long ru_oublock; long ru_msgsnd; long ru_msgrcv; long ru_nsignals; long ru_nvcsw; long ru_nivcsw; };')
}

fn (mut g FlatGen) headerless_timespec_struct() {
	if !g.inlined_c_structs['timespec'] {
		g.writeln('#if !defined(_STRUCT_TIMESPEC) && !defined(_TIMESPEC_DEFINED) && !defined(_TIMESPEC_DECLARED) && !defined(__timespec_defined)')
		g.writeln('#ifdef _WIN32')
		g.writeln('struct timespec { i64 tv_sec; long tv_nsec; };')
		g.writeln('#else')
		g.writeln('struct timespec { long tv_sec; long tv_nsec; };')
		g.writeln('#endif')
		g.writeln('#endif')
	}
	g.writeln('typedef struct timespec timespec;')
	g.writeln('#if !defined(_CLOCK_T) && !defined(__clock_t_defined) && !defined(_CLOCK_T_DECLARED) && !defined(_CLOCK_T_DEFINED)')
	g.writeln('typedef long clock_t;')
	g.writeln('#endif')
	g.writeln('#ifndef CLOCKS_PER_SEC')
	g.writeln('#define CLOCKS_PER_SEC 1000000')
	g.writeln('#endif')
	g.writeln('clock_t clock(void);')
}

fn (mut g FlatGen) headerless_darwin_task_info_struct() {
	g.writeln('#ifdef __APPLE__')
	g.writeln('#ifndef _MACH_TASK_INFO_H_')
	g.writeln('typedef unsigned int task_t;')
	g.writeln('#pragma pack(push, 4)')
	g.writeln('struct task_basic_info { i32 suspend_count; u64 virtual_size; u64 resident_size; struct { i32 seconds; i32 microseconds; } user_time; struct { i32 seconds; i32 microseconds; } system_time; i32 policy; };')
	g.writeln('#pragma pack(pop)')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_tm_struct() {
	if !g.inlined_c_structs['tm'] {
		g.writeln('#if !defined(_STRUCT_TM) && !defined(_TM_DEFINED) && !defined(_TM_DECLARED) && !defined(__tm_defined)')
		g.writeln('#ifdef _WIN32')
		g.writeln('struct tm { int tm_sec; int tm_min; int tm_hour; int tm_mday; int tm_mon; int tm_year; int tm_wday; int tm_yday; int tm_isdst; };')
		g.writeln('#else')
		g.writeln('struct tm { int tm_sec; int tm_min; int tm_hour; int tm_mday; int tm_mon; int tm_year; int tm_wday; int tm_yday; int tm_isdst; long tm_gmtoff; const char* tm_zone; };')
		g.writeln('#endif')
		g.writeln('#endif')
	}
	g.writeln('typedef struct tm tm;')
}

fn (mut g FlatGen) headerless_termios_struct() {
	g.writeln('#if defined(__APPLE__)')
	g.writeln('struct termios { size_t c_iflag; size_t c_oflag; size_t c_cflag; size_t c_lflag; u8 c_cc[20]; size_t c_ispeed; size_t c_ospeed; };')
	g.writeln('#elif defined(__linux__) || defined(__ANDROID__)')
	g.writeln('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_line; u8 c_cc[32]; int c_ispeed; int c_ospeed; };')
	g.writeln('#elif defined(__sun)')
	g.writeln('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_cc[20]; };')
	g.writeln('#elif defined(__QNX__) || defined(__QNXNTO__)')
	g.writeln('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_cc[20]; u32 reserved[3]; int c_ispeed; int c_ospeed; };')
	g.writeln('#else')
	g.writeln('struct termios { int c_iflag; int c_oflag; int c_cflag; int c_lflag; u8 c_cc[20]; int c_ispeed; int c_ospeed; };')
	g.writeln('#endif')
	g.writeln('typedef struct termios termios;')
}

fn (mut g FlatGen) headerless_utsname_struct() {
	g.writeln('#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)')
	g.writeln('struct utsname { char sysname[256]; char nodename[256]; char release[256]; char version[256]; char machine[256]; };')
	g.writeln('#else')
	g.writeln('struct utsname { char sysname[65]; char nodename[65]; char release[65]; char version[65]; char machine[65]; char domainname[65]; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_stat_struct() {
	g.writeln('#ifdef __APPLE__')
	g.writeln('struct stat { int st_dev; unsigned short st_mode; unsigned short st_nlink; u64 st_ino; u32 st_uid; u32 st_gid; int st_rdev; i64 st_atime; i64 st_atimensec; i64 st_mtime; i64 st_mtimensec; i64 st_ctime; i64 st_ctimensec; i64 st_birthtime; i64 st_birthtimensec; i64 st_size; i64 st_blocks; int st_blksize; u32 st_flags; u32 st_gen; int st_lspare; i64 st_qspare[2]; };')
	g.writeln('#elif defined(__linux__)')
	g.headerless_linux_stat_struct()
	g.writeln('#elif defined(_WIN32)')
	g.writeln('struct stat { u64 st_dev; u64 st_ino; u32 st_mode; u64 st_nlink; u32 st_uid; u32 st_gid; u64 st_rdev; u64 st_size; int st_atime; int st_mtime; int st_ctime; };')
	g.writeln('#elif defined(__FreeBSD__)')
	g.headerless_freebsd_stat_struct()
	g.writeln('#elif defined(__OpenBSD__)')
	g.headerless_openbsd_stat_struct()
	g.writeln('#elif defined(__NetBSD__)')
	g.headerless_netbsd_stat_struct()
	g.writeln('#elif defined(__DragonFly__)')
	g.headerless_dragonfly_stat_struct()
	g.writeln('#elif defined(__sun)')
	g.headerless_solaris_stat_struct()
	g.writeln('#elif defined(__QNX__) || defined(__QNXNTO__)')
	g.headerless_qnx_stat_struct()
	g.writeln('#else')
	g.writeln('#error unsupported headerless Unix struct stat layout for this platform')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_freebsd_stat_struct() {
	g.writeln('#if defined(__i386__)')
	g.writeln('struct stat { u64 st_dev; u64 st_ino; u64 st_nlink; u16 st_mode; i16 st_bsdflags; u32 st_uid; u32 st_gid; i32 st_padding1; u64 st_rdev; i32 st_atim_ext; i64 st_atime; long st_atimensec; i32 st_mtim_ext; i64 st_mtime; long st_mtimensec; i32 st_ctim_ext; i64 st_ctime; long st_ctimensec; i32 st_btim_ext; i64 st_birthtime; long st_birthtimensec; i64 st_size; i64 st_blocks; i32 st_blksize; u32 st_flags; u64 st_gen; u64 st_filerev; u64 st_spare[9]; };')
	g.writeln('#else')
	g.writeln('struct stat { u64 st_dev; u64 st_ino; u64 st_nlink; u16 st_mode; i16 st_bsdflags; u32 st_uid; u32 st_gid; i32 st_padding1; u64 st_rdev; i64 st_atime; long st_atimensec; i64 st_mtime; long st_mtimensec; i64 st_ctime; long st_ctimensec; i64 st_birthtime; long st_birthtimensec; i64 st_size; i64 st_blocks; i32 st_blksize; u32 st_flags; u64 st_gen; u64 st_filerev; u64 st_spare[9]; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_openbsd_stat_struct() {
	g.writeln('struct stat { u32 st_mode; i32 st_dev; u64 st_ino; u32 st_nlink; u32 st_uid; u32 st_gid; i32 st_rdev; i64 st_atime; long st_atimensec; i64 st_mtime; long st_mtimensec; i64 st_ctime; long st_ctimensec; i64 __st_birthtime; long __st_birthtimensec; i64 st_size; i64 st_blocks; i32 st_blksize; u32 st_flags; u32 st_gen; };')
}

fn (mut g FlatGen) headerless_netbsd_stat_struct() {
	g.writeln('struct stat { u64 st_dev; u32 st_mode; u64 st_ino; u32 st_nlink; u32 st_uid; u32 st_gid; u64 st_rdev; i64 st_atime; long st_atimensec; i64 st_mtime; long st_mtimensec; i64 st_ctime; long st_ctimensec; i64 st_birthtime; long st_birthtimensec; i64 st_size; i64 st_blocks; i32 st_blksize; u32 st_flags; u32 st_gen; u32 st_spare[2]; };')
}

fn (mut g FlatGen) headerless_dragonfly_stat_struct() {
	g.writeln('struct stat { u64 st_ino; u32 st_nlink; u32 st_dev; u16 st_mode; u16 st_padding1; u32 st_uid; u32 st_gid; u32 st_rdev; i64 st_atime; long st_atimensec; i64 st_mtime; long st_mtimensec; i64 st_ctime; long st_ctimensec; i64 st_size; i64 st_blocks; u32 __old_st_blksize; u32 st_flags; u32 st_gen; i32 st_lspare; i64 st_blksize; i64 st_qspare2; };')
}

fn (mut g FlatGen) headerless_solaris_stat_struct() {
	g.writeln('#if defined(_LP64)')
	g.writeln('struct stat { u64 st_dev; u64 st_ino; u32 st_mode; u32 st_nlink; u32 st_uid; u32 st_gid; u64 st_rdev; i64 st_size; long st_atime; long st_atimensec; long st_mtime; long st_mtimensec; long st_ctime; long st_ctimensec; long st_blksize; i64 st_blocks; char st_fstype[16]; };')
	g.writeln('#else')
	g.writeln('struct stat { unsigned long st_dev; long st_pad1[3]; unsigned long st_ino; u32 st_mode; u32 st_nlink; u32 st_uid; u32 st_gid; unsigned long st_rdev; long st_pad2[2]; long st_size; long st_pad3; long st_atime; long st_atimensec; long st_mtime; long st_mtimensec; long st_ctime; long st_ctimensec; long st_blksize; long st_blocks; char st_fstype[16]; long st_pad4[8]; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_qnx_stat_struct() {
	g.writeln('#if _FILE_OFFSET_BITS - 0 == 64')
	g.writeln('struct stat { u64 st_ino; i64 st_size; u64 st_dev; u64 st_rdev; u32 st_uid; u32 st_gid; long st_mtime; long st_atime; long st_ctime; u32 st_mode; u32 st_nlink; long st_blocksize; i32 st_nblocks; long st_blksize; i64 st_blocks; };')
	g.writeln('#elif defined(__BIGENDIAN__)')
	g.writeln('struct stat { unsigned long st_ino_hi; unsigned long st_ino; long st_size_hi; long st_size; unsigned long st_dev; unsigned long st_rdev; u32 st_uid; u32 st_gid; long st_mtime; long st_atime; long st_ctime; u32 st_mode; u32 st_nlink; long st_blocksize; i32 st_nblocks; long st_blksize; long st_blocks_hi; long st_blocks; };')
	g.writeln('#else')
	g.writeln('struct stat { unsigned long st_ino; unsigned long st_ino_hi; long st_size; long st_size_hi; unsigned long st_dev; unsigned long st_rdev; u32 st_uid; u32 st_gid; long st_mtime; long st_atime; long st_ctime; u32 st_mode; u32 st_nlink; long st_blocksize; i32 st_nblocks; long st_blksize; long st_blocks; long st_blocks_hi; };')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_linux_stat_struct() {
	g.writeln('#if defined(__x86_64__) && !defined(__ILP32__)')
	g.writeln('struct stat { u64 st_dev; u64 st_ino; u64 st_nlink; u32 st_mode; u32 st_uid; u32 st_gid; int __pad0; u64 st_rdev; i64 st_size; i64 st_blksize; i64 st_blocks; i64 st_atime; i64 st_atimensec; i64 st_mtime; i64 st_mtimensec; i64 st_ctime; i64 st_ctimensec; i64 __glibc_reserved[3]; };')
	g.writeln('#elif defined(__aarch64__) || (defined(__riscv) && __riscv_xlen == 64) || defined(__loongarch_lp64)')
	g.writeln('struct stat { u64 st_dev; u64 st_ino; u32 st_mode; u32 st_nlink; u32 st_uid; u32 st_gid; u64 st_rdev; unsigned long __pad1; i64 st_size; int st_blksize; int __pad2; i64 st_blocks; i64 st_atime; i64 st_atimensec; i64 st_mtime; i64 st_mtimensec; i64 st_ctime; i64 st_ctimensec; unsigned int __glibc_reserved[2]; };')
	g.writeln('#elif defined(__i386__) || defined(__arm__)')
	g.writeln('struct stat { u64 st_dev; unsigned short __pad1; unsigned long st_ino; u32 st_mode; unsigned long st_nlink; u32 st_uid; u32 st_gid; u64 st_rdev; unsigned short __pad2; long st_size; long st_blksize; long st_blocks; long st_atime; unsigned long st_atimensec; long st_mtime; unsigned long st_mtimensec; long st_ctime; unsigned long st_ctimensec; unsigned long __glibc_reserved4; unsigned long __glibc_reserved5; };')
	g.writeln('#else')
	g.writeln('#error unsupported Linux struct stat layout for this architecture')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_platform_constants() {
	g.writeln('#define STDIN_FILENO 0')
	g.writeln('#define STDOUT_FILENO 1')
	g.writeln('#define STDERR_FILENO 2')
	g.writeln('#define F_OK 0')
	g.writeln('#define WEXITSTATUS(status) (((status) >> 8) & 0xff)')
	g.writeln('#define WTERMSIG(status) ((status) & 0x7f)')
	g.writeln('#define WIFEXITED(status) (WTERMSIG(status) == 0)')
	g.writeln('#define WIFSIGNALED(status) ((((status) & 0x7f) + 1) >= 2)')
	g.writeln('#define WNOHANG 1')
	g.writeln('#define LOCK_SH 1')
	g.writeln('#define LOCK_EX 2')
	g.writeln('#define LOCK_NB 4')
	g.writeln('#define LOCK_UN 8')
	g.writeln('#define ENOENT 2')
	g.writeln('#define RUSAGE_SELF 0')
	g.writeln('#ifdef __APPLE__')
	g.headerless_darwin_constants()
	g.writeln('#elif defined(_WIN32)')
	g.headerless_windows_constants()
	g.writeln('#elif defined(__FreeBSD__)')
	g.headerless_bsd_constants('0x00100000', '12', '13', '4', '47', '28', '0x00020000', '0x0800',
		true)
	g.writeln('#elif defined(__OpenBSD__)')
	g.headerless_bsd_constants('0x10000', '8', '9', '3', '28', '24', '0x0400', '', false)
	g.writeln('#elif defined(__NetBSD__)')
	g.headerless_bsd_constants('0x00400000', '8', '9', '3', '28', '24', '0x0400', '0x0800', false)
	g.writeln('#elif defined(__DragonFly__)')
	g.headerless_bsd_constants('0x00020000', '8', '9', '4', '47', '28', '0x0400', '0x0800', false)
	g.writeln('#elif defined(__sun)')
	g.headerless_solaris_constants()
	g.writeln('#elif defined(__QNX__) || defined(__QNXNTO__)')
	g.headerless_qnx_constants()
	g.writeln('#elif defined(__linux__) || defined(__ANDROID__)')
	g.headerless_linux_constants()
	g.writeln('#else')
	g.writeln('#error unsupported headerless C platform constants')
	g.writeln('#endif')
	g.headerless_signal_constants()
	g.headerless_ptrace_constants()
	g.headerless_sysctl_constants()
	g.writeln('#ifndef MSG_NOSIGNAL')
	g.writeln('#define MSG_NOSIGNAL 0')
	g.writeln('#endif')
	g.writeln('#ifndef SO_NOSIGPIPE')
	g.writeln('#define SO_NOSIGPIPE 0')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_signal_constants() {
	g.writeln('#define SIGKILL 9')
	g.writeln('#define SIGTERM 15')
	g.writeln('#define SIGPIPE 13')
	g.writeln('#define SIG_IGN ((void*)1)')
	g.writeln('#if defined(__linux__) || defined(__ANDROID__)')
	g.writeln('#define SIGSTOP 19')
	g.writeln('#define SIGCONT 18')
	g.writeln('#define SIG_BLOCK 0')
	g.writeln('#else')
	g.writeln('#define SIGSTOP 17')
	g.writeln('#define SIGCONT 19')
	g.writeln('#define SIG_BLOCK 1')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_ptrace_constants() {
	g.writeln('#if defined(__linux__) || defined(__ANDROID__)')
	g.writeln('#define PTRACE_ATTACH 16')
	g.writeln('#define PTRACE_DETACH 17')
	g.writeln('#elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)')
	g.writeln('#define PT_TRACE_ME 0')
	g.writeln('#define PT_ATTACH 10')
	g.writeln('#define PT_DETACH 11')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_sysctl_constants() {
	g.writeln('#if defined(__FreeBSD__)')
	g.writeln('#define CTL_KERN 1')
	g.writeln('#define CTL_VM 2')
	g.writeln('#define KERN_PROC 14')
	g.writeln('#define KERN_PROC_PID 1')
	g.writeln('#define KERN_PROC_ARGS 7')
	g.writeln('#define KERN_PROC_PATHNAME 12')
	g.writeln('#define KERN_PROC_INC_THREAD 0x10')
	g.writeln('#elif defined(__OpenBSD__)')
	g.writeln('#define CTL_KERN 1')
	g.writeln('#define CTL_VM 2')
	g.writeln('#define KERN_PROC 66')
	g.writeln('#define KERN_PROC_PID 1')
	g.writeln('#define KERN_PROC_ARGS 55')
	g.writeln('#define KERN_PROC_ARGV 1')
	g.writeln('#define VM_UVMEXP 4')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_mmap_constants(map_anonymous string) {
	g.writeln('#define PROT_READ 0x1')
	g.writeln('#define PROT_WRITE 0x2')
	g.writeln('#define PROT_EXEC 0x4')
	g.writeln('#define MAP_PRIVATE 0x0002')
	g.writeln('#define MAP_ANONYMOUS ${map_anonymous}')
	g.writeln('#define MAP_ANON MAP_ANONYMOUS')
	g.writeln('#define MAP_FAILED ((void*)-1)')
}

fn (mut g FlatGen) headerless_linux_syscall_constants() {
	g.writeln('#ifndef SYS_getrandom')
	g.writeln('#if defined(__x86_64__)')
	g.writeln('#define SYS_getrandom 318')
	g.writeln('#elif defined(__i386__)')
	g.writeln('#define SYS_getrandom 355')
	g.writeln('#elif defined(__arm__)')
	g.writeln('#define SYS_getrandom 384')
	g.writeln('#elif defined(__aarch64__) || (defined(__riscv) && __riscv_xlen == 64) || defined(__loongarch_lp64)')
	g.writeln('#define SYS_getrandom 278')
	g.writeln('#else')
	g.writeln('#define SYS_getrandom 278')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_linux_sysconf_constants() {
	g.writeln('#if defined(__ANDROID__)')
	g.writeln('#define _SC_PAGESIZE 0x0027')
	g.writeln('#define _SC_NPROCESSORS_ONLN 0x0061')
	g.writeln('#define _SC_PHYS_PAGES 0x0062')
	g.writeln('#define _SC_AVPHYS_PAGES 0x0063')
	g.writeln('#else')
	g.writeln('#define _SC_PAGESIZE 30')
	g.writeln('#define _SC_NPROCESSORS_ONLN 84')
	g.writeln('#define _SC_PHYS_PAGES 85')
	g.writeln('#define _SC_AVPHYS_PAGES 86')
	g.writeln('#endif')
}

fn (mut g FlatGen) headerless_kqueue_common_constants() {
	g.writeln('#define EVFILT_READ (-1)')
	g.writeln('#define EVFILT_WRITE (-2)')
	g.writeln('#define EV_ADD 0x0001')
	g.writeln('#define EV_DELETE 0x0002')
	g.writeln('#define EV_ENABLE 0x0004')
	g.writeln('#define EV_DISABLE 0x0008')
	g.writeln('#define EV_ONESHOT 0x0010')
	g.writeln('#define EV_CLEAR 0x0020')
	g.writeln('#define EV_RECEIPT 0x0040')
	g.writeln('#define EV_DISPATCH 0x0080')
	g.writeln('#define EV_EOF 0x8000')
	g.writeln('#define EV_ERROR 0x4000')
	g.writeln('#define EV_SET(kevp, a, b, c, d, e, f) do { struct kevent* __kevp = (struct kevent*)(kevp); __kevp->ident = (uintptr_t)(a); __kevp->filter = (b); __kevp->flags = (c); __kevp->fflags = (d); __kevp->data = (intptr_t)(e); __kevp->udata = (void*)(f); } while (0)')
}

fn (mut g FlatGen) headerless_darwin_kqueue_constants() {
	g.headerless_kqueue_common_constants()
	g.writeln('#define EVFILT_AIO (-3)')
	g.writeln('#define EVFILT_VNODE (-4)')
	g.writeln('#define EVFILT_PROC (-5)')
	g.writeln('#define EVFILT_SIGNAL (-6)')
	g.writeln('#define EVFILT_TIMER (-7)')
	g.writeln('#define EVFILT_MACHPORT (-8)')
	g.writeln('#define EVFILT_FS (-9)')
	g.writeln('#define EVFILT_USER (-10)')
	g.writeln('#define EVFILT_VM (-12)')
	g.writeln('#define EVFILT_EXCEPT (-15)')
	g.writeln('#define EVFILT_SYSCOUNT 18')
	g.writeln('#define EV_UDATA_SPECIFIC 0x0100')
	g.writeln('#define EV_DISPATCH2 (EV_DISPATCH | EV_UDATA_SPECIFIC)')
	g.writeln('#define EV_VANISHED 0x0200')
	g.writeln('#define EV_SYSFLAGS 0xF000')
	g.writeln('#define EV_FLAG0 0x1000')
	g.writeln('#define EV_FLAG1 0x2000')
}

fn (mut g FlatGen) headerless_darwin_constants() {
	g.writeln('#define O_RDONLY 0x0000')
	g.writeln('#define O_WRONLY 0x0001')
	g.writeln('#define O_RDWR 0x0002')
	g.writeln('#define O_NONBLOCK 0x0004')
	g.writeln('#define O_APPEND 0x0008')
	g.writeln('#define O_SYNC 0x0080')
	g.writeln('#define O_CREAT 0x0200')
	g.writeln('#define O_TRUNC 0x0400')
	g.writeln('#define O_EXCL 0x0800')
	g.writeln('#define O_NOCTTY 0x20000')
	g.writeln('#define O_CLOEXEC 0x01000000')
	g.writeln('#define F_GETFD 1')
	g.writeln('#define F_SETFD 2')
	g.writeln('#define F_GETFL 3')
	g.writeln('#define F_SETFL 4')
	g.writeln('#define F_SETLK 8')
	g.writeln('#define F_SETLKW 9')
	g.writeln('#define FD_CLOEXEC 1')
	g.writeln('#define F_RDLCK 1')
	g.writeln('#define F_UNLCK 2')
	g.writeln('#define F_WRLCK 3')
	g.writeln('#define EACCES 13')
	g.writeln('#define EFAULT 14')
	g.writeln('#define EINTR 4')
	g.writeln('#define EINVAL 22')
	g.writeln('#define EAGAIN 35')
	g.writeln('#define EWOULDBLOCK 35')
	g.writeln('#define EINPROGRESS 36')
	g.writeln('#define EBUSY 16')
	g.writeln('#define EDEADLK 11')
	g.writeln('#define ETIMEDOUT 60')
	g.writeln('#define EPROTONOSUPPORT 43')
	g.writeln('#define EAFNOSUPPORT 47')
	g.writeln('#define EADDRNOTAVAIL 49')
	g.writeln('#define EAI_SYSTEM 11')
	g.writeln('#define CLOCK_REALTIME 0')
	g.writeln('#define CLOCK_MONOTONIC 6')
	g.writeln('#define _SC_PAGESIZE 29')
	g.writeln('#define _SC_NPROCESSORS_ONLN 58')
	g.writeln('#define _SC_PHYS_PAGES 200')
	g.writeln('#define KERN_SUCCESS 0')
	g.writeln('#define MACH_TASK_BASIC_INFO_COUNT 12')
	g.writeln('#if defined(__arm__) || defined(__arm64__)')
	g.writeln('#define TASK_BASIC_INFO 18')
	g.writeln('#elif defined(__LP64__)')
	g.writeln('#define TASK_BASIC_INFO 5')
	g.writeln('#else')
	g.writeln('#define TASK_BASIC_INFO 4')
	g.writeln('#endif')
	g.headerless_mmap_constants('0x1000')
	g.headerless_darwin_kqueue_constants()
	g.writeln('#define FIONREAD 0x4004667f')
	g.writeln('#define TIOCGWINSZ 0x40087468')
	g.writeln('#define TCSANOW 0')
	g.writeln('#define TCSADRAIN 1')
	g.writeln('#define TCSAFLUSH 2')
	g.writeln('#define IGNBRK 1')
	g.writeln('#define BRKINT 2')
	g.writeln('#define PARMRK 8')
	g.writeln('#define INPCK 16')
	g.writeln('#define ISTRIP 32')
	g.writeln('#define ICRNL 256')
	g.writeln('#define IXON 512')
	g.writeln('#define OPOST 1')
	g.writeln('#define CS8 768')
	g.writeln('#define ISIG 128')
	g.writeln('#define ICANON 256')
	g.writeln('#define ECHO 8')
	g.writeln('#define IEXTEN 1024')
	g.writeln('#define TOSTOP 4194304')
	g.writeln('#define VMIN 16')
	g.writeln('#define VTIME 17')
	g.writeln('#define PTHREAD_PROCESS_PRIVATE 2')
	g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 0')
	g.writeln('#define S_IFMT 0170000')
	g.writeln('#define S_IFIFO 0010000')
	g.writeln('#define S_IFCHR 0020000')
	g.writeln('#define S_IFDIR 0040000')
	g.writeln('#define S_IFBLK 0060000')
	g.writeln('#define S_IFREG 0100000')
	g.writeln('#define S_IFLNK 0120000')
	g.writeln('#define S_IFSOCK 0140000')
	g.writeln('#define S_IRUSR 0000400')
	g.writeln('#define S_IWUSR 0000200')
	g.writeln('#define S_IXUSR 0000100')
	g.writeln('#define S_IRGRP 0000040')
	g.writeln('#define S_IWGRP 0000020')
	g.writeln('#define S_IXGRP 0000010')
	g.writeln('#define S_IROTH 0000004')
	g.writeln('#define S_IWOTH 0000002')
	g.writeln('#define S_IXOTH 0000001')
	g.writeln('#define S_IRWXU 0000700')
	g.writeln('#define S_IRWXG 0000070')
	g.writeln('#define S_IRWXO 0000007')
	g.writeln('#define S_ISUID 0004000')
	g.writeln('#define S_ISGID 0002000')
	g.writeln('#define S_ISVTX 0001000')
	g.writeln('#ifndef EEXIST')
	g.writeln('#define EEXIST 17')
	g.writeln('#endif')
	g.writeln('#ifndef S_ISDIR')
	g.writeln('#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)')
	g.writeln('#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)')
	g.writeln('#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)')
	g.writeln('#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)')
	g.writeln('#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)')
	g.writeln('#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)')
	g.writeln('#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)')
	g.writeln('#endif')
	g.headerless_darwin_net_constants()
	g.writeln('#define SIG_ERR ((void (*)(int))-1)')
	g.writeln('struct flock { off_t l_start; off_t l_len; pid_t l_pid; short l_type; short l_whence; };')
}

fn (mut g FlatGen) headerless_bsd_constants(o_cloexec string, f_setlk string, f_setlkw string, clock_monotonic string, sc_pagesize string, af_inet6 string, msg_nosignal string, so_nosigpipe string, flock_sysid bool) {
	g.writeln('#define O_RDONLY 0x0000')
	g.writeln('#define O_WRONLY 0x0001')
	g.writeln('#define O_RDWR 0x0002')
	g.writeln('#define O_NONBLOCK 0x0004')
	g.writeln('#define O_APPEND 0x0008')
	g.writeln('#define O_SYNC 0x0080')
	g.writeln('#define O_CREAT 0x0200')
	g.writeln('#define O_TRUNC 0x0400')
	g.writeln('#define O_EXCL 0x0800')
	g.writeln('#define O_NOCTTY 0x8000')
	g.writeln('#define O_CLOEXEC ${o_cloexec}')
	g.writeln('#define F_GETFD 1')
	g.writeln('#define F_SETFD 2')
	g.writeln('#define F_GETFL 3')
	g.writeln('#define F_SETFL 4')
	g.writeln('#define F_SETLK ${f_setlk}')
	g.writeln('#define F_SETLKW ${f_setlkw}')
	g.writeln('#define FD_CLOEXEC 1')
	g.writeln('#define F_RDLCK 1')
	g.writeln('#define F_UNLCK 2')
	g.writeln('#define F_WRLCK 3')
	g.writeln('#define EACCES 13')
	g.writeln('#define EFAULT 14')
	g.writeln('#define EINTR 4')
	g.writeln('#define EINVAL 22')
	g.writeln('#define EAGAIN 35')
	g.writeln('#define EWOULDBLOCK 35')
	g.writeln('#define EINPROGRESS 36')
	g.writeln('#define EBUSY 16')
	g.writeln('#define EDEADLK 11')
	g.writeln('#define ETIMEDOUT 60')
	g.writeln('#define EPROTONOSUPPORT 43')
	g.writeln('#define EAFNOSUPPORT 47')
	g.writeln('#define EADDRNOTAVAIL 49')
	g.writeln('#define EAI_SYSTEM 11')
	g.writeln('#define CLOCK_REALTIME 0')
	g.writeln('#define CLOCK_MONOTONIC ${clock_monotonic}')
	g.writeln('#define _SC_PAGESIZE ${sc_pagesize}')
	g.headerless_mmap_constants('0x1000')
	g.headerless_kqueue_common_constants()
	g.writeln('#define FIONREAD 0x4004667f')
	g.writeln('#define TIOCGWINSZ 0x40087468')
	g.writeln('#define TCSANOW 0')
	g.writeln('#define TCSADRAIN 1')
	g.writeln('#define TCSAFLUSH 2')
	g.writeln('#define IGNBRK 1')
	g.writeln('#define BRKINT 2')
	g.writeln('#define PARMRK 8')
	g.writeln('#define INPCK 16')
	g.writeln('#define ISTRIP 32')
	g.writeln('#define ICRNL 256')
	g.writeln('#define IXON 512')
	g.writeln('#define OPOST 1')
	g.writeln('#define CS8 768')
	g.writeln('#define ISIG 128')
	g.writeln('#define ICANON 256')
	g.writeln('#define ECHO 8')
	g.writeln('#define IEXTEN 1024')
	g.writeln('#define TOSTOP 4194304')
	g.writeln('#define VMIN 16')
	g.writeln('#define VTIME 17')
	g.writeln('#define PTHREAD_PROCESS_PRIVATE 0')
	g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 0')
	g.writeln('#define S_IFMT 0170000')
	g.writeln('#define S_IFIFO 0010000')
	g.writeln('#define S_IFCHR 0020000')
	g.writeln('#define S_IFDIR 0040000')
	g.writeln('#define S_IFBLK 0060000')
	g.writeln('#define S_IFREG 0100000')
	g.writeln('#define S_IFLNK 0120000')
	g.writeln('#define S_IFSOCK 0140000')
	g.writeln('#define S_IRUSR 0000400')
	g.writeln('#define S_IWUSR 0000200')
	g.writeln('#define S_IXUSR 0000100')
	g.writeln('#define S_IRGRP 0000040')
	g.writeln('#define S_IWGRP 0000020')
	g.writeln('#define S_IXGRP 0000010')
	g.writeln('#define S_IROTH 0000004')
	g.writeln('#define S_IWOTH 0000002')
	g.writeln('#define S_IXOTH 0000001')
	g.writeln('#define S_IRWXU 0000700')
	g.writeln('#define S_IRWXG 0000070')
	g.writeln('#define S_IRWXO 0000007')
	g.writeln('#define S_ISUID 0004000')
	g.writeln('#define S_ISGID 0002000')
	g.writeln('#define S_ISVTX 0001000')
	g.writeln('#ifndef EEXIST')
	g.writeln('#define EEXIST 17')
	g.writeln('#endif')
	g.writeln('#ifndef S_ISDIR')
	g.writeln('#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)')
	g.writeln('#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)')
	g.writeln('#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)')
	g.writeln('#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)')
	g.writeln('#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)')
	g.writeln('#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)')
	g.writeln('#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)')
	g.writeln('#endif')
	g.headerless_bsd_net_constants(af_inet6, msg_nosignal, so_nosigpipe)
	g.writeln('#define SIG_ERR ((void (*)(int))-1)')
	if flock_sysid {
		g.writeln('struct flock { off_t l_start; off_t l_len; pid_t l_pid; short l_type; short l_whence; int l_sysid; };')
	} else {
		g.writeln('struct flock { off_t l_start; off_t l_len; pid_t l_pid; short l_type; short l_whence; };')
	}
}

fn (mut g FlatGen) headerless_solaris_constants() {
	g.writeln('#define O_RDONLY 0')
	g.writeln('#define O_WRONLY 1')
	g.writeln('#define O_RDWR 2')
	g.writeln('#define O_APPEND 0x08')
	g.writeln('#define O_SYNC 0x10')
	g.writeln('#define O_NONBLOCK 0x80')
	g.writeln('#define O_CREAT 0x100')
	g.writeln('#define O_TRUNC 0x200')
	g.writeln('#define O_EXCL 0x400')
	g.writeln('#define O_NOCTTY 0x800')
	g.writeln('#define O_CLOEXEC 0x800000')
	g.writeln('#define F_GETFD 1')
	g.writeln('#define F_SETFD 2')
	g.writeln('#define F_GETFL 3')
	g.writeln('#define F_SETFL 4')
	g.writeln('#define F_SETLK 6')
	g.writeln('#define F_SETLKW 7')
	g.writeln('#define FD_CLOEXEC 1')
	g.writeln('#define F_RDLCK 1')
	g.writeln('#define F_WRLCK 2')
	g.writeln('#define F_UNLCK 3')
	g.writeln('#define EINTR 4')
	g.writeln('#define EINVAL 22')
	g.writeln('#define EAGAIN 11')
	g.writeln('#define EWOULDBLOCK EAGAIN')
	g.writeln('#define EINPROGRESS 150')
	g.writeln('#define EBUSY 16')
	g.writeln('#define EDEADLK 45')
	g.writeln('#define ETIMEDOUT 145')
	g.writeln('#define EPROTONOSUPPORT 120')
	g.writeln('#define EAFNOSUPPORT 124')
	g.writeln('#define EADDRNOTAVAIL 126')
	g.writeln('#define EAI_SYSTEM 11')
	g.writeln('#define CLOCK_REALTIME 0')
	g.writeln('#define CLOCK_MONOTONIC 4')
	g.writeln('#define _SC_PAGESIZE 11')
	g.headerless_mmap_constants('0x100')
	g.writeln('#define FIONREAD 0x4004667f')
	g.writeln("#define TIOCGWINSZ (('T' << 8) | 104)")
	g.writeln("#define TCSANOW (('T' << 8) | 14)")
	g.writeln("#define TCSADRAIN (('T' << 8) | 15)")
	g.writeln("#define TCSAFLUSH (('T' << 8) | 16)")
	g.writeln('#define IGNBRK 0000001')
	g.writeln('#define BRKINT 0000002')
	g.writeln('#define PARMRK 0000010')
	g.writeln('#define INPCK 0000020')
	g.writeln('#define ISTRIP 0000040')
	g.writeln('#define ICRNL 0000400')
	g.writeln('#define IXON 0002000')
	g.writeln('#define OPOST 0000001')
	g.writeln('#define CS8 0000060')
	g.writeln('#define ISIG 0000001')
	g.writeln('#define ICANON 0000002')
	g.writeln('#define ECHO 0000010')
	g.writeln('#define IEXTEN 0100000')
	g.writeln('#define TOSTOP 0000400')
	g.writeln('#define VMIN 4')
	g.writeln('#define VTIME 5')
	g.writeln('#define PTHREAD_PROCESS_PRIVATE 0')
	g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 0')
	g.writeln('#define S_IFMT 0170000')
	g.writeln('#define S_IFIFO 0010000')
	g.writeln('#define S_IFCHR 0020000')
	g.writeln('#define S_IFDIR 0040000')
	g.writeln('#define S_IFBLK 0060000')
	g.writeln('#define S_IFREG 0100000')
	g.writeln('#define S_IFLNK 0120000')
	g.writeln('#define S_IFSOCK 0140000')
	g.writeln('#define S_IRUSR 0000400')
	g.writeln('#define S_IWUSR 0000200')
	g.writeln('#define S_IXUSR 0000100')
	g.writeln('#define S_IRGRP 0000040')
	g.writeln('#define S_IWGRP 0000020')
	g.writeln('#define S_IXGRP 0000010')
	g.writeln('#define S_IROTH 0000004')
	g.writeln('#define S_IWOTH 0000002')
	g.writeln('#define S_IXOTH 0000001')
	g.writeln('#define S_IRWXU 0000700')
	g.writeln('#define S_IRWXG 0000070')
	g.writeln('#define S_IRWXO 0000007')
	g.writeln('#define S_ISUID 0004000')
	g.writeln('#define S_ISGID 0002000')
	g.writeln('#define S_ISVTX 0001000')
	g.writeln('#ifndef EEXIST')
	g.writeln('#define EEXIST 17')
	g.writeln('#endif')
	g.writeln('#ifndef S_ISDIR')
	g.writeln('#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)')
	g.writeln('#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)')
	g.writeln('#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)')
	g.writeln('#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)')
	g.writeln('#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)')
	g.writeln('#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)')
	g.writeln('#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)')
	g.writeln('#endif')
	g.headerless_solaris_net_constants()
	g.writeln('#define SIG_ERR ((void (*)(int))-1)')
	g.writeln('struct flock { short l_type; short l_whence; off_t l_start; off_t l_len; int l_sysid; pid_t l_pid; long l_pad[4]; };')
}

fn (mut g FlatGen) headerless_qnx_constants() {
	g.writeln('#define O_RDONLY 000000')
	g.writeln('#define O_WRONLY 000001')
	g.writeln('#define O_RDWR 000002')
	g.writeln('#define O_APPEND 000010')
	g.writeln('#define O_SYNC 000040')
	g.writeln('#define O_NONBLOCK 000200')
	g.writeln('#define O_CREAT 000400')
	g.writeln('#define O_TRUNC 001000')
	g.writeln('#define O_EXCL 002000')
	g.writeln('#define O_NOCTTY 004000')
	g.writeln('#define O_CLOEXEC 020000')
	g.writeln('#define F_GETFD 1')
	g.writeln('#define F_SETFD 2')
	g.writeln('#define F_GETFL 3')
	g.writeln('#define F_SETFL 4')
	g.writeln('#define F_SETLK 6')
	g.writeln('#define F_SETLKW 7')
	g.writeln('#define FD_CLOEXEC 1')
	g.writeln('#define F_RDLCK 1')
	g.writeln('#define F_WRLCK 2')
	g.writeln('#define F_UNLCK 3')
	g.writeln('#define EINTR 4')
	g.writeln('#define EINVAL 22')
	g.writeln('#define EAGAIN 11')
	g.writeln('#define EWOULDBLOCK EAGAIN')
	g.writeln('#define EINPROGRESS 236')
	g.writeln('#define EBUSY 16')
	g.writeln('#define EDEADLK 45')
	g.writeln('#define ETIMEDOUT 260')
	g.writeln('#define EPROTONOSUPPORT 243')
	g.writeln('#define EAFNOSUPPORT 247')
	g.writeln('#define EADDRNOTAVAIL 249')
	g.writeln('#define EAI_SYSTEM 11')
	g.writeln('#define CLOCK_REALTIME 0')
	g.writeln('#define CLOCK_MONOTONIC 2')
	g.writeln('#define _SC_PAGESIZE 11')
	g.headerless_mmap_constants('0x00080000')
	g.writeln('#define FIONREAD 0x4004667f')
	g.writeln('#define TIOCGWINSZ 0x40087468')
	g.writeln('#define TCSANOW 0x0001')
	g.writeln('#define TCSADRAIN 0x0002')
	g.writeln('#define TCSAFLUSH 0x0004')
	g.writeln('#define IGNBRK 0x00000001')
	g.writeln('#define BRKINT 0x00000002')
	g.writeln('#define PARMRK 0x00000008')
	g.writeln('#define INPCK 0x00000010')
	g.writeln('#define ISTRIP 0x00000020')
	g.writeln('#define ICRNL 0x00000100')
	g.writeln('#define IXON 0x00000400')
	g.writeln('#define OPOST 0x00000001')
	g.writeln('#define CS8 0x30')
	g.writeln('#define ISIG 0x00000001')
	g.writeln('#define ICANON 0x00000002')
	g.writeln('#define ECHO 0x00000008')
	g.writeln('#define IEXTEN 0x00008000')
	g.writeln('#define TOSTOP 0x00000100')
	g.writeln('#define VMIN 16')
	g.writeln('#define VTIME 17')
	g.writeln('#define PTHREAD_PROCESS_PRIVATE 0')
	g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 0')
	g.writeln('#define S_IFMT 0170000')
	g.writeln('#define S_IFIFO 0010000')
	g.writeln('#define S_IFCHR 0020000')
	g.writeln('#define S_IFDIR 0040000')
	g.writeln('#define S_IFBLK 0060000')
	g.writeln('#define S_IFREG 0100000')
	g.writeln('#define S_IFLNK 0120000')
	g.writeln('#define S_IFSOCK 0140000')
	g.writeln('#define S_IRUSR 0000400')
	g.writeln('#define S_IWUSR 0000200')
	g.writeln('#define S_IXUSR 0000100')
	g.writeln('#define S_IRGRP 0000040')
	g.writeln('#define S_IWGRP 0000020')
	g.writeln('#define S_IXGRP 0000010')
	g.writeln('#define S_IROTH 0000004')
	g.writeln('#define S_IWOTH 0000002')
	g.writeln('#define S_IXOTH 0000001')
	g.writeln('#define S_IRWXU 0000700')
	g.writeln('#define S_IRWXG 0000070')
	g.writeln('#define S_IRWXO 0000007')
	g.writeln('#define S_ISUID 0004000')
	g.writeln('#define S_ISGID 0002000')
	g.writeln('#define S_ISVTX 0001000')
	g.writeln('#ifndef EEXIST')
	g.writeln('#define EEXIST 17')
	g.writeln('#endif')
	g.writeln('#ifndef S_ISDIR')
	g.writeln('#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)')
	g.writeln('#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)')
	g.writeln('#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)')
	g.writeln('#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)')
	g.writeln('#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)')
	g.writeln('#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)')
	g.writeln('#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)')
	g.writeln('#endif')
	g.headerless_qnx_net_constants()
	g.writeln('#define SIG_ERR ((void (*)(int))-1)')
	g.writeln('struct flock { short l_type; short l_whence; int l_zero1; off_t l_start; off_t l_len; pid_t l_pid; unsigned int l_sysid; };')
}

fn (mut g FlatGen) headerless_windows_constants() {
	g.writeln('#define O_RDONLY 0x0000')
	g.writeln('#define O_WRONLY 0x0001')
	g.writeln('#define O_RDWR 0x0002')
	g.writeln('#define O_APPEND 0x0008')
	g.writeln('#define O_CREAT 0x0100')
	g.writeln('#define O_TRUNC 0x0200')
	g.writeln('#define O_EXCL 0x0400')
	g.writeln('#define O_BINARY 0x8000')
	g.writeln('#define _O_BINARY 0x8000')
	g.writeln('#define F_GETFD 1')
	g.writeln('#define F_SETFD 2')
	g.writeln('#define F_GETFL 3')
	g.writeln('#define F_SETFL 4')
	g.writeln('#define FD_CLOEXEC 1')
	g.writeln('#define EINTR 4')
	g.writeln('#define EINVAL 22')
	g.writeln('#define EAGAIN 11')
	g.writeln('#define EWOULDBLOCK 11')
	g.writeln('#define EINPROGRESS 10036')
	g.writeln('#define EBUSY 16')
	g.writeln('#define EDEADLK 36')
	g.writeln('#define ETIMEDOUT 10060')
	g.writeln('#define EPROTONOSUPPORT 10043')
	g.writeln('#define EAFNOSUPPORT 10047')
	g.writeln('#define EADDRNOTAVAIL 10049')
	g.writeln('#define EAI_SYSTEM 11')
	g.writeln('#define CLOCK_REALTIME 0')
	g.writeln('#define CLOCK_MONOTONIC 1')
	g.writeln('#define _SC_PAGESIZE 30')
	g.writeln('#define FIONREAD 0x4004667f')
	g.writeln('#define FIONBIO 0x8004667eU')
	g.writeln('#define TCSANOW 0')
	g.writeln('#define TCSADRAIN 1')
	g.writeln('#define TCSAFLUSH 2')
	g.writeln('#define PTHREAD_PROCESS_PRIVATE 0')
	g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 0')
	g.writeln('#define S_IFMT 0170000')
	g.writeln('#define S_IFIFO 0010000')
	g.writeln('#define S_IFCHR 0020000')
	g.writeln('#define S_IFDIR 0040000')
	g.writeln('#define S_IFBLK 0060000')
	g.writeln('#define S_IFREG 0100000')
	g.writeln('#define S_IFLNK 0120000')
	g.writeln('#define S_IFSOCK 0140000')
	g.writeln('#define S_IREAD 0000400')
	g.writeln('#define S_IWRITE 0000200')
	g.writeln('#define S_IEXEC 0000100')
	g.writeln('#define S_IRUSR 0000400')
	g.writeln('#define S_IWUSR 0000200')
	g.writeln('#define S_IXUSR 0000100')
	g.headerless_windows_net_constants()
	g.writeln('#define SIG_ERR ((void (*)(int))-1)')
	g.writeln('#define GENERIC_READ 0x80000000U')
	g.writeln('#define GENERIC_WRITE 0x40000000U')
	g.writeln('#define FILE_SHARE_READ 0x00000001U')
	g.writeln('#define FILE_SHARE_WRITE 0x00000002U')
	g.writeln('#define FILE_SHARE_DELETE 0x00000004U')
	g.writeln('#define OPEN_EXISTING 3')
	g.writeln('#define OPEN_ALWAYS 4')
	g.writeln('#define FILE_ATTRIBUTE_NORMAL 0x00000080U')
	g.writeln('#define FILE_ATTRIBUTE_DIRECTORY 0x00000010U')
	g.writeln('#define INVALID_FILE_ATTRIBUTES 0xffffffffU')
	g.writeln('#define LOCKFILE_FAIL_IMMEDIATELY 0x00000001U')
	g.writeln('#define LOCKFILE_EXCLUSIVE_LOCK 0x00000002U')
	g.writeln('#define MAXDWORD 0xffffffffU')
	g.writeln('#define MEM_COMMIT 0x00001000U')
	g.writeln('#define MEM_RESERVE 0x00002000U')
	g.writeln('#define PAGE_READWRITE 0x04U')
	g.writeln('#define PAGE_EXECUTE_READ 0x20U')
	g.writeln('#define TLS_OUT_OF_INDEXES 0xffffffffU')
	g.writeln('#define TRUE 1')
	g.writeln('#define FALSE 0')
	g.writeln('#define INFINITE 0xffffffffU')
	g.writeln('#define INVALID_HANDLE_VALUE ((void*)-1)')
	g.writeln('#define STD_INPUT_HANDLE 0xfffffff6U')
	g.writeln('#define STD_OUTPUT_HANDLE 0xfffffff5U')
	g.writeln('#define STD_ERROR_HANDLE 0xfffffff4U')
	g.writeln('#define ENABLE_PROCESSED_INPUT 0x0001')
	g.writeln('#define ENABLE_LINE_INPUT 0x0002')
	g.writeln('#define ENABLE_ECHO_INPUT 0x0004')
	g.writeln('#define ENABLE_WINDOW_INPUT 0x0008')
	g.writeln('#define ENABLE_MOUSE_INPUT 0x0010')
	g.writeln('#define ENABLE_EXTENDED_FLAGS 0x0080')
	g.writeln('#define STARTF_USESTDHANDLES 0x00000100U')
	g.writeln('#define CREATE_NEW_PROCESS_GROUP 0x00000200U')
	g.writeln('#define CREATE_UNICODE_ENVIRONMENT 0x00000400U')
	g.writeln('#define NORMAL_PRIORITY_CLASS 0x00000020U')
	g.writeln('#define CREATE_NO_WINDOW 0x08000000U')
	g.writeln('#define HANDLE_FLAG_INHERIT 0x00000001U')
	g.writeln('#define CTRL_BREAK_EVENT 1')
	g.writeln('#define STILL_ACTIVE 259')
	g.writeln('#define KEY_EVENT 0x0001')
	g.writeln('#define MOUSE_EVENT 0x0002')
	g.writeln('#define WINDOW_BUFFER_SIZE_EVENT 0x0004')
	g.writeln('#define MENU_EVENT 0x0008')
	g.writeln('#define FOCUS_EVENT 0x0010')
	g.writeln('#define MOUSE_MOVED 0x0001')
	g.writeln('#define DOUBLE_CLICK 0x0002')
	g.writeln('#define MOUSE_WHEELED 0x0004')
	g.writeln('#define VK_BACK 0x08')
	g.writeln('#define VK_RETURN 0x0d')
	g.writeln('#define VK_PRIOR 0x21')
	g.writeln('#define VK_NEXT 0x22')
	g.writeln('#define VK_END 0x23')
	g.writeln('#define VK_HOME 0x24')
	g.writeln('#define VK_LEFT 0x25')
	g.writeln('#define VK_UP 0x26')
	g.writeln('#define VK_RIGHT 0x27')
	g.writeln('#define VK_DOWN 0x28')
	g.writeln('#define VK_INSERT 0x2d')
	g.writeln('#define VK_DELETE 0x2e')
	g.writeln('#define ERROR_ACCESS_DENIED 5')
	g.writeln('#define ERROR_CLASS_ALREADY_EXISTS 1410')
	g.writeln('#define HWND_MESSAGE ((void*)-3)')
	g.writeln('#define MB_ERR_INVALID_CHARS 0x00000008U')
	g.writeln('#define GMEM_MOVEABLE 0x0002U')
	g.writeln('#define CF_UNICODETEXT 13')
}

fn (mut g FlatGen) headerless_linux_constants() {
	g.writeln('#define O_RDONLY 0')
	g.writeln('#define O_WRONLY 1')
	g.writeln('#define O_RDWR 2')
	g.writeln('#define O_CREAT 0100')
	g.writeln('#define O_EXCL 0200')
	g.writeln('#define O_NOCTTY 0400')
	g.writeln('#define O_TRUNC 01000')
	g.writeln('#define O_APPEND 02000')
	g.writeln('#define O_NONBLOCK 04000')
	g.writeln('#define O_SYNC 04010000')
	g.writeln('#define O_CLOEXEC 02000000')
	g.writeln('#define TFD_NONBLOCK O_NONBLOCK')
	g.writeln('#define TFD_CLOEXEC O_CLOEXEC')
	g.writeln('#define F_GETFD 1')
	g.writeln('#define F_SETFD 2')
	g.writeln('#define F_GETFL 3')
	g.writeln('#define F_SETFL 4')
	g.writeln('#define F_SETLK 6')
	g.writeln('#define F_SETLKW 7')
	g.writeln('#define FD_CLOEXEC 1')
	g.writeln('#define F_RDLCK 0')
	g.writeln('#define F_WRLCK 1')
	g.writeln('#define F_UNLCK 2')
	g.writeln('#define EINTR 4')
	g.writeln('#define EIO 5')
	g.writeln('#define EBADF 9')
	g.writeln('#define EINVAL 22')
	g.writeln('#define EAGAIN 11')
	g.writeln('#define EWOULDBLOCK 11')
	g.writeln('#define ENOMEM 12')
	g.writeln('#define EFAULT 14')
	g.writeln('#define EINPROGRESS 115')
	g.writeln('#define ESPIPE 29')
	g.writeln('#define EBUSY 16')
	g.writeln('#define EDEADLK 35')
	g.writeln('#define ETIMEDOUT 110')
	g.writeln('#define EOVERFLOW 75')
	g.writeln('#define EPROTONOSUPPORT 93')
	g.writeln('#define EAFNOSUPPORT 97')
	g.writeln('#define EADDRNOTAVAIL 99')
	g.writeln('#define EAI_SYSTEM -11')
	g.writeln('#define CLOCK_REALTIME 0')
	g.writeln('#define CLOCK_MONOTONIC 1')
	g.headerless_linux_sysconf_constants()
	g.headerless_mmap_constants('0x20')
	g.headerless_linux_syscall_constants()
	g.writeln('#define FIONREAD 0x541b')
	g.writeln('#define TIOCGWINSZ 0x5413')
	g.writeln('#define EPOLLIN 0x001')
	g.writeln('#define EPOLLPRI 0x002')
	g.writeln('#define EPOLLOUT 0x004')
	g.writeln('#define EPOLLERR 0x008')
	g.writeln('#define EPOLLHUP 0x010')
	g.writeln('#define EPOLLRDHUP 0x2000')
	g.writeln('#define EPOLLEXCLUSIVE (1U << 28)')
	g.writeln('#define EPOLLWAKEUP (1U << 29)')
	g.writeln('#define EPOLLONESHOT (1U << 30)')
	g.writeln('#define EPOLLET (1U << 31)')
	g.writeln('#define EPOLL_CTL_ADD 1')
	g.writeln('#define EPOLL_CTL_DEL 2')
	g.writeln('#define EPOLL_CTL_MOD 3')
	g.writeln('#define TCSANOW 0')
	g.writeln('#define TCSADRAIN 1')
	g.writeln('#define TCSAFLUSH 2')
	g.writeln('#define IGNBRK 0000001')
	g.writeln('#define BRKINT 0000002')
	g.writeln('#define PARMRK 0000010')
	g.writeln('#define INPCK 0000020')
	g.writeln('#define ISTRIP 0000040')
	g.writeln('#define ICRNL 0000400')
	g.writeln('#define IXON 0002000')
	g.writeln('#define OPOST 0000001')
	g.writeln('#define CS8 0000060')
	g.writeln('#define ISIG 0000001')
	g.writeln('#define ICANON 0000002')
	g.writeln('#define ECHO 0000010')
	g.writeln('#define IEXTEN 0100000')
	g.writeln('#define TOSTOP 0000400')
	g.writeln('#define VMIN 6')
	g.writeln('#define VTIME 5')
	g.writeln('#define PTHREAD_PROCESS_PRIVATE 0')
	g.writeln('#define PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 2')
	g.writeln('#define S_IFMT 00170000')
	g.writeln('#define S_IFIFO 0010000')
	g.writeln('#define S_IFCHR 0020000')
	g.writeln('#define S_IFDIR 0040000')
	g.writeln('#define S_IFBLK 0060000')
	g.writeln('#define S_IFREG 0100000')
	g.writeln('#define S_IFLNK 0120000')
	g.writeln('#define S_IFSOCK 0140000')
	g.writeln('#define S_IRUSR 0000400')
	g.writeln('#define S_IWUSR 0000200')
	g.writeln('#define S_IXUSR 0000100')
	g.writeln('#define S_IRGRP 0000040')
	g.writeln('#define S_IWGRP 0000020')
	g.writeln('#define S_IXGRP 0000010')
	g.writeln('#define S_IROTH 0000004')
	g.writeln('#define S_IWOTH 0000002')
	g.writeln('#define S_IXOTH 0000001')
	g.writeln('#define S_IRWXU 0000700')
	g.writeln('#define S_IRWXG 0000070')
	g.writeln('#define S_IRWXO 0000007')
	g.writeln('#define S_ISUID 0004000')
	g.writeln('#define S_ISGID 0002000')
	g.writeln('#define S_ISVTX 0001000')
	g.writeln('#ifndef EEXIST')
	g.writeln('#define EEXIST 17')
	g.writeln('#endif')
	g.writeln('#ifndef S_ISDIR')
	g.writeln('#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)')
	g.writeln('#define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)')
	g.writeln('#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)')
	g.writeln('#define S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)')
	g.writeln('#define S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)')
	g.writeln('#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)')
	g.writeln('#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)')
	g.writeln('#endif')
	g.headerless_linux_net_constants()
	g.writeln('#define SIG_ERR ((void (*)(int))-1)')
	g.writeln('struct flock { short l_type; short l_whence; off_t l_start; off_t l_len; pid_t l_pid; };')
}

fn (mut g FlatGen) headerless_darwin_net_constants() {
	g.writeln('#define AF_UNSPEC 0')
	g.writeln('#define AF_UNIX 1')
	g.writeln('#define AF_INET 2')
	g.writeln('#define AF_INET6 30')
	g.writeln('#define SOCK_STREAM 1')
	g.writeln('#define SOCK_DGRAM 2')
	g.writeln('#define SOCK_RAW 3')
	g.writeln('#define SOCK_SEQPACKET 5')
	g.writeln('#define IPPROTO_IP 0')
	g.writeln('#define IPPROTO_ICMP 1')
	g.writeln('#define IPPROTO_TCP 6')
	g.writeln('#define IPPROTO_UDP 17')
	g.writeln('#define IPPROTO_IPV6 41')
	g.writeln('#define IPPROTO_ICMPV6 58')
	g.writeln('#define IPPROTO_RAW 255')
	g.writeln('#define SOL_SOCKET 0xffff')
	g.writeln('#define SO_DEBUG 0x0001')
	g.writeln('#define SO_REUSEADDR 0x0004')
	g.writeln('#define SO_KEEPALIVE 0x0008')
	g.writeln('#define SO_DONTROUTE 0x0010')
	g.writeln('#define SO_BROADCAST 0x0020')
	g.writeln('#define SO_LINGER 0x0080')
	g.writeln('#define SO_OOBINLINE 0x0100')
	g.writeln('#define SO_SNDBUF 0x1001')
	g.writeln('#define SO_RCVBUF 0x1002')
	g.writeln('#define SO_SNDLOWAT 0x1003')
	g.writeln('#define SO_RCVLOWAT 0x1004')
	g.writeln('#define SO_SNDTIMEO 0x1005')
	g.writeln('#define SO_RCVTIMEO 0x1006')
	g.writeln('#define SO_ERROR 0x1007')
	g.writeln('#define SO_TYPE 0x1008')
	g.writeln('#define SO_NOSIGPIPE 0x1022')
	g.writeln('#define TCP_NODELAY 1')
	g.writeln('#define IP_HDRINCL 2')
	g.writeln('#define IP_MULTICAST_IF 9')
	g.writeln('#define IP_MULTICAST_TTL 10')
	g.writeln('#define IP_MULTICAST_LOOP 11')
	g.writeln('#define IP_ADD_MEMBERSHIP 12')
	g.writeln('#define IP_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_MULTICAST_IF 9')
	g.writeln('#define IPV6_MULTICAST_HOPS 10')
	g.writeln('#define IPV6_MULTICAST_LOOP 11')
	g.writeln('#define IPV6_ADD_MEMBERSHIP 12')
	g.writeln('#define IPV6_JOIN_GROUP 12')
	g.writeln('#define IPV6_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_LEAVE_GROUP 13')
	g.writeln('#define IPV6_V6ONLY 27')
	g.writeln('#define AI_PASSIVE 0x00000001')
	g.writeln('#define MSG_DONTWAIT 0x80')
}

fn (mut g FlatGen) headerless_bsd_net_constants(af_inet6 string, msg_nosignal string, so_nosigpipe string) {
	g.writeln('#define AF_UNSPEC 0')
	g.writeln('#define AF_UNIX 1')
	g.writeln('#define AF_INET 2')
	g.writeln('#define AF_INET6 ${af_inet6}')
	g.writeln('#define SOCK_STREAM 1')
	g.writeln('#define SOCK_DGRAM 2')
	g.writeln('#define SOCK_RAW 3')
	g.writeln('#define SOCK_SEQPACKET 5')
	g.writeln('#if defined(__FreeBSD__)')
	g.writeln('#define SOCK_NONBLOCK 0x20000000')
	g.writeln('#endif')
	g.writeln('#define IPPROTO_IP 0')
	g.writeln('#define IPPROTO_ICMP 1')
	g.writeln('#define IPPROTO_TCP 6')
	g.writeln('#define IPPROTO_UDP 17')
	g.writeln('#define IPPROTO_IPV6 41')
	g.writeln('#define IPPROTO_ICMPV6 58')
	g.writeln('#define IPPROTO_RAW 255')
	g.writeln('#define SOL_SOCKET 0xffff')
	g.writeln('#define SO_DEBUG 0x0001')
	g.writeln('#define SO_REUSEADDR 0x0004')
	g.writeln('#define SO_KEEPALIVE 0x0008')
	g.writeln('#define SO_DONTROUTE 0x0010')
	g.writeln('#define SO_BROADCAST 0x0020')
	g.writeln('#define SO_LINGER 0x0080')
	g.writeln('#define SO_OOBINLINE 0x0100')
	g.writeln('#define SO_REUSEPORT 0x0200')
	if so_nosigpipe.len > 0 {
		g.writeln('#define SO_NOSIGPIPE ${so_nosigpipe}')
	}
	g.writeln('#define SO_SNDBUF 0x1001')
	g.writeln('#define SO_RCVBUF 0x1002')
	g.writeln('#define SO_SNDLOWAT 0x1003')
	g.writeln('#define SO_RCVLOWAT 0x1004')
	g.writeln('#define SO_SNDTIMEO 0x1005')
	g.writeln('#define SO_RCVTIMEO 0x1006')
	g.writeln('#define SO_ERROR 0x1007')
	g.writeln('#define SO_TYPE 0x1008')
	g.writeln('#define TCP_NODELAY 1')
	g.writeln('#define IP_HDRINCL 2')
	g.writeln('#define IP_MULTICAST_IF 9')
	g.writeln('#define IP_MULTICAST_TTL 10')
	g.writeln('#define IP_MULTICAST_LOOP 11')
	g.writeln('#define IP_ADD_MEMBERSHIP 12')
	g.writeln('#define IP_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_MULTICAST_IF 9')
	g.writeln('#define IPV6_MULTICAST_HOPS 10')
	g.writeln('#define IPV6_MULTICAST_LOOP 11')
	g.writeln('#define IPV6_ADD_MEMBERSHIP 12')
	g.writeln('#define IPV6_JOIN_GROUP 12')
	g.writeln('#define IPV6_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_LEAVE_GROUP 13')
	g.writeln('#define IPV6_V6ONLY 27')
	g.writeln('#define AI_PASSIVE 0x00000001')
	g.writeln('#define MSG_DONTWAIT 0x80')
	g.writeln('#define MSG_NOSIGNAL ${msg_nosignal}')
}

fn (mut g FlatGen) headerless_solaris_net_constants() {
	g.writeln('#define AF_UNSPEC 0')
	g.writeln('#define AF_UNIX 1')
	g.writeln('#define AF_INET 2')
	g.writeln('#define AF_INET6 26')
	g.writeln('#define SOCK_STREAM 2')
	g.writeln('#define SOCK_DGRAM 1')
	g.writeln('#define SOCK_RAW 4')
	g.writeln('#define SOCK_SEQPACKET 6')
	g.writeln('#define IPPROTO_IP 0')
	g.writeln('#define IPPROTO_ICMP 1')
	g.writeln('#define IPPROTO_TCP 6')
	g.writeln('#define IPPROTO_UDP 17')
	g.writeln('#define IPPROTO_IPV6 41')
	g.writeln('#define IPPROTO_ICMPV6 58')
	g.writeln('#define IPPROTO_RAW 255')
	g.writeln('#define SOL_SOCKET 0xffff')
	g.writeln('#define SO_DEBUG 0x0001')
	g.writeln('#define SO_REUSEADDR 0x0004')
	g.writeln('#define SO_KEEPALIVE 0x0008')
	g.writeln('#define SO_DONTROUTE 0x0010')
	g.writeln('#define SO_BROADCAST 0x0020')
	g.writeln('#define SO_LINGER 0x0080')
	g.writeln('#define SO_OOBINLINE 0x0100')
	g.writeln('#define SO_SNDBUF 0x1001')
	g.writeln('#define SO_RCVBUF 0x1002')
	g.writeln('#define SO_SNDLOWAT 0x1003')
	g.writeln('#define SO_RCVLOWAT 0x1004')
	g.writeln('#define SO_SNDTIMEO 0x1005')
	g.writeln('#define SO_RCVTIMEO 0x1006')
	g.writeln('#define SO_ERROR 0x1007')
	g.writeln('#define SO_TYPE 0x1008')
	g.writeln('#define TCP_NODELAY 1')
	g.writeln('#define IP_HDRINCL 2')
	g.writeln('#define IP_MULTICAST_IF 0x10')
	g.writeln('#define IP_MULTICAST_TTL 0x11')
	g.writeln('#define IP_MULTICAST_LOOP 0x12')
	g.writeln('#define IP_ADD_MEMBERSHIP 0x13')
	g.writeln('#define IP_DROP_MEMBERSHIP 0x14')
	g.writeln('#define IPV6_MULTICAST_IF 0x6')
	g.writeln('#define IPV6_MULTICAST_HOPS 0x7')
	g.writeln('#define IPV6_MULTICAST_LOOP 0x8')
	g.writeln('#define IPV6_ADD_MEMBERSHIP 0x9')
	g.writeln('#define IPV6_JOIN_GROUP 0x9')
	g.writeln('#define IPV6_DROP_MEMBERSHIP 0xa')
	g.writeln('#define IPV6_LEAVE_GROUP 0xa')
	g.writeln('#define IPV6_V6ONLY 0x27')
	g.writeln('#define AI_PASSIVE 0x0008')
	g.writeln('#define MSG_DONTWAIT 0x80')
	g.writeln('#define MSG_NOSIGNAL 0x200')
}

fn (mut g FlatGen) headerless_qnx_net_constants() {
	g.writeln('#define AF_UNSPEC 0')
	g.writeln('#define AF_UNIX 1')
	g.writeln('#define AF_INET 2')
	g.writeln('#define AF_INET6 24')
	g.writeln('#define SOCK_STREAM 1')
	g.writeln('#define SOCK_DGRAM 2')
	g.writeln('#define SOCK_RAW 3')
	g.writeln('#define SOCK_SEQPACKET 5')
	g.writeln('#define IPPROTO_IP 0')
	g.writeln('#define IPPROTO_ICMP 1')
	g.writeln('#define IPPROTO_TCP 6')
	g.writeln('#define IPPROTO_UDP 17')
	g.writeln('#define IPPROTO_IPV6 41')
	g.writeln('#define IPPROTO_ICMPV6 58')
	g.writeln('#define IPPROTO_RAW 255')
	g.writeln('#define SOL_SOCKET 0xffff')
	g.writeln('#define SO_DEBUG 0x0001')
	g.writeln('#define SO_REUSEADDR 0x0004')
	g.writeln('#define SO_KEEPALIVE 0x0008')
	g.writeln('#define SO_DONTROUTE 0x0010')
	g.writeln('#define SO_BROADCAST 0x0020')
	g.writeln('#define SO_LINGER 0x0080')
	g.writeln('#define SO_OOBINLINE 0x0100')
	g.writeln('#define SO_SNDBUF 0x1001')
	g.writeln('#define SO_RCVBUF 0x1002')
	g.writeln('#define SO_SNDLOWAT 0x1003')
	g.writeln('#define SO_RCVLOWAT 0x1004')
	g.writeln('#define SO_SNDTIMEO 0x1005')
	g.writeln('#define SO_RCVTIMEO 0x1006')
	g.writeln('#define SO_ERROR 0x1007')
	g.writeln('#define SO_TYPE 0x1008')
	g.writeln('#define TCP_NODELAY 1')
	g.writeln('#define IP_HDRINCL 2')
	g.writeln('#define IP_MULTICAST_IF 9')
	g.writeln('#define IP_MULTICAST_TTL 10')
	g.writeln('#define IP_MULTICAST_LOOP 11')
	g.writeln('#define IP_ADD_MEMBERSHIP 12')
	g.writeln('#define IP_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_MULTICAST_IF 9')
	g.writeln('#define IPV6_MULTICAST_HOPS 10')
	g.writeln('#define IPV6_MULTICAST_LOOP 11')
	g.writeln('#define IPV6_ADD_MEMBERSHIP 12')
	g.writeln('#define IPV6_JOIN_GROUP 12')
	g.writeln('#define IPV6_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_LEAVE_GROUP 13')
	g.writeln('#define IPV6_V6ONLY 27')
	g.writeln('#define AI_PASSIVE 0x00000001')
	g.writeln('#define MSG_DONTWAIT 0x80')
	g.writeln('#define MSG_NOSIGNAL 0x0800')
}

fn (mut g FlatGen) headerless_linux_net_constants() {
	g.writeln('#define AF_UNSPEC 0')
	g.writeln('#define AF_UNIX 1')
	g.writeln('#define AF_INET 2')
	g.writeln('#define AF_INET6 10')
	g.writeln('#define SOCK_STREAM 1')
	g.writeln('#define SOCK_DGRAM 2')
	g.writeln('#define SOCK_RAW 3')
	g.writeln('#define SOCK_SEQPACKET 5')
	g.writeln('#define SOCK_NONBLOCK 04000')
	g.writeln('#define IPPROTO_IP 0')
	g.writeln('#define IPPROTO_ICMP 1')
	g.writeln('#define IPPROTO_TCP 6')
	g.writeln('#define IPPROTO_UDP 17')
	g.writeln('#define IPPROTO_IPV6 41')
	g.writeln('#define IPPROTO_ICMPV6 58')
	g.writeln('#define IPPROTO_RAW 255')
	g.writeln('#define SOL_SOCKET 1')
	g.writeln('#define SO_DEBUG 1')
	g.writeln('#define SO_REUSEADDR 2')
	g.writeln('#define SO_TYPE 3')
	g.writeln('#define SO_ERROR 4')
	g.writeln('#define SO_DONTROUTE 5')
	g.writeln('#define SO_BROADCAST 6')
	g.writeln('#define SO_SNDBUF 7')
	g.writeln('#define SO_RCVBUF 8')
	g.writeln('#define SO_KEEPALIVE 9')
	g.writeln('#define SO_OOBINLINE 10')
	g.writeln('#define SO_LINGER 13')
	g.writeln('#define SO_REUSEPORT 15')
	g.writeln('#define SO_RCVLOWAT 18')
	g.writeln('#define SO_SNDLOWAT 19')
	g.writeln('#define SO_RCVTIMEO 20')
	g.writeln('#define SO_SNDTIMEO 21')
	g.writeln('#define TCP_NODELAY 1')
	g.writeln('#define TCP_DEFER_ACCEPT 9')
	g.writeln('#define TCP_QUICKACK 12')
	g.writeln('#define TCP_FASTOPEN 23')
	g.writeln('#define IP_HDRINCL 3')
	g.writeln('#define IP_MULTICAST_IF 32')
	g.writeln('#define IP_MULTICAST_TTL 33')
	g.writeln('#define IP_MULTICAST_LOOP 34')
	g.writeln('#define IP_ADD_MEMBERSHIP 35')
	g.writeln('#define IP_DROP_MEMBERSHIP 36')
	g.writeln('#define IPV6_MULTICAST_IF 17')
	g.writeln('#define IPV6_MULTICAST_HOPS 18')
	g.writeln('#define IPV6_MULTICAST_LOOP 19')
	g.writeln('#define IPV6_ADD_MEMBERSHIP 20')
	g.writeln('#define IPV6_JOIN_GROUP 20')
	g.writeln('#define IPV6_DROP_MEMBERSHIP 21')
	g.writeln('#define IPV6_LEAVE_GROUP 21')
	g.writeln('#define IPV6_V6ONLY 26')
	g.writeln('#define AI_PASSIVE 0x0001')
	g.writeln('#define SOMAXCONN 4096')
	g.writeln('#define MSG_DONTWAIT 0x40')
	g.writeln('#define MSG_NOSIGNAL 0x4000')
}

fn (mut g FlatGen) headerless_windows_net_constants() {
	g.writeln('#define AF_UNSPEC 0')
	g.writeln('#define AF_UNIX 1')
	g.writeln('#define AF_INET 2')
	g.writeln('#define AF_INET6 23')
	g.writeln('#define SOCK_STREAM 1')
	g.writeln('#define SOCK_DGRAM 2')
	g.writeln('#define SOCK_RAW 3')
	g.writeln('#define SOCK_SEQPACKET 5')
	g.writeln('#define SOCKET_ERROR (-1)')
	g.writeln('#define WSAEWOULDBLOCK 10035')
	g.writeln('#define IPPROTO_IP 0')
	g.writeln('#define IPPROTO_ICMP 1')
	g.writeln('#define IPPROTO_TCP 6')
	g.writeln('#define IPPROTO_UDP 17')
	g.writeln('#define IPPROTO_IPV6 41')
	g.writeln('#define IPPROTO_ICMPV6 58')
	g.writeln('#define IPPROTO_RAW 255')
	g.writeln('#define SOL_SOCKET 0xffff')
	g.writeln('#define SO_DEBUG 0x0001')
	g.writeln('#define SO_REUSEADDR 0x0004')
	g.writeln('#define SO_KEEPALIVE 0x0008')
	g.writeln('#define SO_DONTROUTE 0x0010')
	g.writeln('#define SO_BROADCAST 0x0020')
	g.writeln('#define SO_LINGER 0x0080')
	g.writeln('#define SO_OOBINLINE 0x0100')
	g.writeln('#define SO_SNDBUF 0x1001')
	g.writeln('#define SO_RCVBUF 0x1002')
	g.writeln('#define SO_SNDLOWAT 0x1003')
	g.writeln('#define SO_RCVLOWAT 0x1004')
	g.writeln('#define SO_SNDTIMEO 0x1005')
	g.writeln('#define SO_RCVTIMEO 0x1006')
	g.writeln('#define SO_ERROR 0x1007')
	g.writeln('#define SO_TYPE 0x1008')
	g.writeln('#define TCP_NODELAY 1')
	g.writeln('#define IP_HDRINCL 2')
	g.writeln('#define IP_MULTICAST_IF 9')
	g.writeln('#define IP_MULTICAST_TTL 10')
	g.writeln('#define IP_MULTICAST_LOOP 11')
	g.writeln('#define IP_ADD_MEMBERSHIP 12')
	g.writeln('#define IP_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_MULTICAST_IF 9')
	g.writeln('#define IPV6_MULTICAST_HOPS 10')
	g.writeln('#define IPV6_MULTICAST_LOOP 11')
	g.writeln('#define IPV6_ADD_MEMBERSHIP 12')
	g.writeln('#define IPV6_JOIN_GROUP 12')
	g.writeln('#define IPV6_DROP_MEMBERSHIP 13')
	g.writeln('#define IPV6_LEAVE_GROUP 13')
	g.writeln('#define IPV6_V6ONLY 27')
	g.writeln('#define AI_PASSIVE 0x00000001')
	g.writeln('#define MSG_DONTWAIT 0')
	g.writeln('#define MSG_NOSIGNAL 0')
}

fn (mut g FlatGen) write_arch_macros() {
	g.writeln('#ifndef __V_architecture')
	g.writeln('#define __V_architecture 0')
	g.writeln('#endif')
	g.writeln('#if defined(__x86_64__) || defined(_M_AMD64)')
	g.writeln('#define __V_amd64 1')
	g.writeln('#undef __V_architecture')
	g.writeln('#define __V_architecture 1')
	g.writeln('#endif')
	g.writeln('#if defined(__aarch64__) || defined(__arm64__) || defined(_M_ARM64)')
	g.writeln('#define __V_arm64 1')
	g.writeln('#undef __V_architecture')
	g.writeln('#define __V_architecture 2')
	g.writeln('#endif')
	g.writeln('#if defined(__arm__) || defined(_M_ARM)')
	g.writeln('#define __V_arm32 1')
	g.writeln('#undef __V_architecture')
	g.writeln('#define __V_architecture 3')
	g.writeln('#endif')
	g.writeln('#if defined(__riscv) && __riscv_xlen == 64')
	g.writeln('#define __V_rv64 1')
	g.writeln('#undef __V_architecture')
	g.writeln('#define __V_architecture 4')
	g.writeln('#endif')
	g.writeln('#if defined(__riscv) && __riscv_xlen == 32')
	g.writeln('#define __V_rv32 1')
	g.writeln('#undef __V_architecture')
	g.writeln('#define __V_architecture 5')
	g.writeln('#endif')
	g.writeln('#if defined(__i386__) || defined(_M_IX86)')
	g.writeln('#define __V_x86 1')
	g.writeln('#undef __V_architecture')
	g.writeln('#define __V_architecture 6')
	g.writeln('#endif')
}

fn (mut g FlatGen) libc_compat_decls() {
	if g.libc_compat_fns['gettid'] {
		g.writeln('#ifdef __linux__')
		g.writeln('#ifndef SYS_gettid')
		g.writeln('#if defined(__x86_64__)')
		g.writeln('#define SYS_gettid 186')
		g.writeln('#elif defined(__aarch64__)')
		g.writeln('#define SYS_gettid 178')
		g.writeln('#elif defined(__i386__)')
		g.writeln('#define SYS_gettid 224')
		g.writeln('#elif defined(__arm__)')
		g.writeln('#define SYS_gettid 224')
		g.writeln('#elif defined(__riscv) && __riscv_xlen == 64')
		g.writeln('#define SYS_gettid 178')
		g.writeln('#elif defined(__loongarch_lp64)')
		g.writeln('#define SYS_gettid 178')
		g.writeln('#else')
		g.writeln('#error unsupported Linux gettid syscall number for this architecture')
		g.writeln('#endif')
		g.writeln('#endif')
		if !g.libc_compat_fns[c_libc_compat_syscall_decl_key] {
			g.writeln('long syscall(long number, ...);')
		}
		g.writeln('static inline u32 v3_gettid(void) {')
		g.writeln('\treturn (u32)syscall(SYS_gettid);')
		g.writeln('}')
		g.writeln('#endif')
		g.writeln('')
	}
}

fn (mut g FlatGen) prealloc_atomic_compat_decls() {
	g.writeln('static inline int v_prealloc_atomic_add_i32(int *ptr, int delta) { return __atomic_add_fetch(ptr, delta, 5); }')
	g.writeln('static inline int v_prealloc_atomic_load_i32(int *ptr) { return __atomic_add_fetch(ptr, 0, 5); }')
	g.writeln('#ifdef __TINYC__')
	g.writeln('static inline int v_prealloc_atomic_store_i32(int *ptr, int val) { return (int)__atomic_exchange_4((u32*)ptr, (u32)val, 5); }')
	g.writeln('static inline int v_prealloc_atomic_cas_i32(int *ptr, int expected, int desired) { u32 e = (u32)expected; return __atomic_compare_exchange_4((u32*)ptr, &e, (u32)desired, 5, 5); }')
	g.writeln('#else')
	g.writeln('static inline int v_prealloc_atomic_store_i32(int *ptr, int val) { return __atomic_exchange_n(ptr, val, 5); }')
	g.writeln('static inline int v_prealloc_atomic_cas_i32(int *ptr, int expected, int desired) { return __atomic_compare_exchange_n(ptr, &expected, desired, 0, 5, 5); }')
	g.writeln('#endif')
}

fn (mut g FlatGen) atomic_builtin_compat_decls() {
	// Atomic helpers. We use compiler __atomic_* builtins (memory order 5 == __ATOMIC_SEQ_CST).
	// clang/gcc inline the generic _n / RMW builtins. tcc only implements the inline
	// __atomic_{add,sub,fetch}_* RMW builtins; for load/store/exchange/cas it has no generic
	// _n form, so we route those to the sized __atomic_*_N libcalls (resolved from libc).
	g.writeln('static inline byte atomic_fetch_add_byte(void* ptr, byte delta) { return __atomic_fetch_add((byte*)ptr, delta, 5); }')
	g.writeln('static inline u16 atomic_fetch_add_u16(void* ptr, u16 delta) { return __atomic_fetch_add((u16*)ptr, delta, 5); }')
	g.writeln('static inline u32 atomic_fetch_add_u32(void* ptr, u32 delta) { return __atomic_fetch_add((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_add_u64(void* ptr, u64 delta) { return __atomic_fetch_add((u64*)ptr, delta, 5); }')
	g.writeln('static inline void* atomic_fetch_add_ptr(void* ptr, void* delta) { return (void*)(uintptr_t)__atomic_fetch_add((uintptr_t*)ptr, (uintptr_t)delta, 5); }')
	g.writeln('static inline byte atomic_fetch_sub_byte(void* ptr, byte delta) { return __atomic_fetch_sub((byte*)ptr, delta, 5); }')
	g.writeln('static inline u16 atomic_fetch_sub_u16(void* ptr, u16 delta) { return __atomic_fetch_sub((u16*)ptr, delta, 5); }')
	g.writeln('static inline u32 atomic_fetch_sub_u32(void* ptr, u32 delta) { return __atomic_fetch_sub((u32*)ptr, delta, 5); }')
	g.writeln('static inline u64 atomic_fetch_sub_u64(void* ptr, u64 delta) { return __atomic_fetch_sub((u64*)ptr, delta, 5); }')
	g.writeln('static inline void* atomic_fetch_sub_ptr(void* ptr, void* delta) { return (void*)(uintptr_t)__atomic_fetch_sub((uintptr_t*)ptr, (uintptr_t)delta, 5); }')
	g.writeln('static inline byte atomic_load_byte(void* ptr) { return __atomic_fetch_add((byte*)ptr, 0, 5); }')
	g.writeln('static inline u16 atomic_load_u16(void* ptr) { return __atomic_fetch_add((u16*)ptr, 0, 5); }')
	g.writeln('static inline u32 atomic_load_u32(void* ptr) { return __atomic_fetch_add((u32*)ptr, 0, 5); }')
	g.writeln('static inline u64 atomic_load_u64(void* ptr) { return __atomic_fetch_add((u64*)ptr, 0, 5); }')
	g.writeln('#ifdef __TINYC__')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return (void*)(uintptr_t)__atomic_fetch_add((uintptr_t*)ptr, (uintptr_t)0, 5); }')
	g.writeln('static inline byte atomic_exchange_byte(void* ptr, byte val) { return __atomic_exchange_1((byte*)ptr, val, 5); }')
	g.writeln('static inline u16 atomic_exchange_u16(void* ptr, u16 val) { return __atomic_exchange_2((u16*)ptr, val, 5); }')
	g.writeln('static inline u32 atomic_exchange_u32(void* ptr, u32 val) { return __atomic_exchange_4((u32*)ptr, val, 5); }')
	g.writeln('static inline u64 atomic_exchange_u64(void* ptr, u64 val) { return __atomic_exchange_8((u64*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __atomic_store_1((byte*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __atomic_store_2((u16*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __atomic_store_4((u32*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __atomic_store_8((u64*)ptr, val, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline void* atomic_exchange_ptr(void* ptr, void* val) { return (void*)(size_t)__atomic_exchange_4((u32*)ptr, (u32)(size_t)val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_4((u32*)ptr, (u32)(size_t)val, 5); }')
	g.writeln('#else')
	g.writeln('static inline void* atomic_exchange_ptr(void* ptr, void* val) { return (void*)(size_t)__atomic_exchange_8((u64*)ptr, (u64)(size_t)val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_8((u64*)ptr, (u64)(size_t)val, 5); }')
	g.writeln('#endif')
	g.writeln('static inline bool atomic_compare_exchange_strong_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_1((byte*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_2((u16*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_4((u32*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_8((u64*)ptr, expected, desired, 5, 5); }')
	g.writeln('#if UINTPTR_MAX == 0xFFFFFFFF')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_4((u32*)ptr, (u32*)expected, (u32)desired, 5, 5); }')
	g.writeln('#else')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_8((u64*)ptr, (u64*)expected, (u64)desired, 5, 5); }')
	g.writeln('#endif')
	g.writeln('static inline bool atomic_compare_exchange_weak_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_1((byte*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_2((u16*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_4((u32*)ptr, expected, desired, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_8((u64*)ptr, expected, desired, 5, 5); }')
	g.writeln('#else')
	g.writeln('static inline void* atomic_load_ptr(void* ptr) { return __atomic_load_n((void**)ptr, 5); }')
	g.writeln('static inline byte atomic_exchange_byte(void* ptr, byte val) { return __atomic_exchange_n((byte*)ptr, val, 5); }')
	g.writeln('static inline u16 atomic_exchange_u16(void* ptr, u16 val) { return __atomic_exchange_n((u16*)ptr, val, 5); }')
	g.writeln('static inline u32 atomic_exchange_u32(void* ptr, u32 val) { return __atomic_exchange_n((u32*)ptr, val, 5); }')
	g.writeln('static inline u64 atomic_exchange_u64(void* ptr, u64 val) { return __atomic_exchange_n((u64*)ptr, val, 5); }')
	g.writeln('static inline void* atomic_exchange_ptr(void* ptr, void* val) { return __atomic_exchange_n((void**)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_byte(void* ptr, byte val) { __atomic_store_n((byte*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u16(void* ptr, u16 val) { __atomic_store_n((u16*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u32(void* ptr, u32 val) { __atomic_store_n((u32*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_u64(void* ptr, u64 val) { __atomic_store_n((u64*)ptr, val, 5); }')
	g.writeln('static inline void atomic_store_ptr(void* ptr, void* val) { __atomic_store_n((void**)ptr, val, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_n((byte*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_n((u16*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_n((u32*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_n((u64*)ptr, expected, desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_strong_ptr(void* ptr, void* expected, ptrdiff_t desired) { return __atomic_compare_exchange_n((void**)ptr, (void**)expected, (void*)desired, 0, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_byte(void* ptr, byte* expected, byte desired) { return __atomic_compare_exchange_n((byte*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u16(void* ptr, u16* expected, u16 desired) { return __atomic_compare_exchange_n((u16*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u32(void* ptr, u32* expected, u32 desired) { return __atomic_compare_exchange_n((u32*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('static inline bool atomic_compare_exchange_weak_u64(void* ptr, u64* expected, u64 desired) { return __atomic_compare_exchange_n((u64*)ptr, expected, desired, 1, 5, 5); }')
	g.writeln('#endif')
	g.writeln('static inline bool atomic_compare_exchange_weak_ptr(void* ptr, void* expected, ptrdiff_t desired) { return atomic_compare_exchange_strong_ptr(ptr, expected, desired); }')
	// tcc's arm64 backend rejects inline asm ("ARM asm not implemented"), even the
	// empty compiler-barrier form. cpu_relax is only a spin-loop hint; the real memory
	// ordering in those loops comes from the atomic ops, so a no-op is safe under tcc.
	g.writeln('#ifdef __TINYC__')
	g.writeln('static inline void cpu_relax(void) { }')
	g.writeln('#else')
	g.writeln('static inline void cpu_relax(void) { __asm__ __volatile__("" ::: "memory"); }')
	g.writeln('#endif')
}

fn (mut g FlatGen) builtin_abi_decls() {
	if !g.has_builtins {
		return
	}
	g.libc_compat_decls()
	g.writeln('#ifndef __linux__')
	g.writeln('#define pthread_rwlockattr_setkind_np(attr, kind) 0')
	g.writeln('#endif')
	g.filelock_compat_decls()
	g.writeln('#define array_new(elem_size, len, cap) __new_array((len), (cap), (elem_size))')
	g.writeln('#define array_push array__push')
	g.writeln('void array__push_many(array* a, void* val, int size);')
	g.writeln('#define array_push_many_ptr(a, val, size) array__push_many((a), (void*)(val), (size))')
	g.writeln('#define array_get array__get')
	g.writeln('#define array_set(a, i, ...) array__set(&(a), (i), __VA_ARGS__)')
	g.writeln('array array__clone(array* a);')
	g.writeln('#define array_slice array__slice')
	g.writeln('#define array_delete array__delete')
	g.writeln('#define array_ensure_cap array__ensure_cap')
	g.writeln('#define map__get_or_set map__get_and_set')
	g.writeln('#ifndef V_COMMIT_HASH')
	g.writeln('#define V_COMMIT_HASH ""')
	g.writeln('#endif')
	g.writeln('#ifndef memory_order_relaxed')
	g.writeln('#define memory_order_relaxed 0')
	g.writeln('#define memory_order_acquire 2')
	g.writeln('#define memory_order_release 3')
	g.writeln('#define memory_order_acq_rel 4')
	g.writeln('#define memory_order_seq_cst 5')
	g.writeln('#endif')
	// tcc has no `__atomic_thread_fence` builtin. On the architectures where
	// thirdparty/stdatomic/nix/atomic.S provides `_V_atomic_thread_fence`, route to
	// that shim; on x86_64 Unix the assembly file provides `__atomic_thread_fence`
	// directly, so keep the normal name there. clang/gcc keep the builtin.
	g.writeln('#if defined(__TINYC__) && (defined(__i386__) || defined(__arm__) || defined(__aarch64__) || defined(__riscv) || (defined(__x86_64__) && defined(_WIN32)))')
	g.writeln('extern void _V_atomic_thread_fence(int order);')
	g.writeln('#define atomic_thread_fence(order) _V_atomic_thread_fence(order)')
	g.writeln('#define __atomic_thread_fence(order) _V_atomic_thread_fence(order)')
	g.writeln('#else')
	g.writeln('#if defined(__TINYC__) && defined(__x86_64__) && !defined(_WIN32)')
	g.writeln('extern void __atomic_thread_fence(int order);')
	g.writeln('#endif')
	g.writeln('#define atomic_thread_fence(order) __atomic_thread_fence(order)')
	g.writeln('#endif')
	// Weak fallbacks for the heap-tracking hooks. A program that provides real
	// implementations (e.g. a `vheap_alloc`/`vheap_free` from a linked C file, as
	// some projects do) overrides these without a redefinition/static-vs-non-static
	// clash against that file's own non-static prototype.
	g.writeln('__attribute__((weak)) void vheap_alloc(void* p, u64 n) { (void)p; (void)n; }')
	g.writeln('__attribute__((weak)) void vheap_free(void* p) { (void)p; }')
	g.prealloc_atomic_compat_decls()
	g.atomic_builtin_compat_decls()
	g.writeln('static inline double math__abs(double a) { return a < 0 ? -a : a; }')
	g.writeln('static inline double math__min(double a, double b) { return a < b ? a : b; }')
	g.writeln('static const u64 _wyp[4] = {0x2d358dccaa6c78a5ull, 0x8bb84b93962eacc9ull, 0x4b33a62ed433d4a3ull, 0x4d5a2da51de1aa47ull};')
	g.writeln('static inline u64 _wymix(u64 a, u64 b) { u64 ha = a >> 32, hb = b >> 32, la = (u32)a, lb = (u32)b, hi, lo; u64 rh = ha * hb, rm0 = ha * lb, rm1 = hb * la, rl = la * lb, t = rl + (rm0 << 32), c = t < rl; lo = t + (rm1 << 32); c += lo < t; hi = rh + (rm0 >> 32) + (rm1 >> 32) + c; return lo ^ hi; }')
	g.writeln('static inline u64 wyhash64(u64 a, u64 b) { a ^= _wyp[0]; b ^= _wyp[1]; a *= 0xa0761d6478bd642full; b *= 0xe7037ed1a0b428dbull; return (a ^ (a >> 32)) ^ (b ^ (b >> 32)); }')
	g.writeln('static inline u64 wyhash(const void* key, size_t len, u64 seed, const u64* secret) { const unsigned char* p = (const unsigned char*)key; u64 h = seed ^ secret[0] ^ (u64)len; for (size_t i = 0; i < len; i++) h = wyhash64(h ^ (u64)p[i], secret[(i + 1) & 3]); return h; }')
	g.writeln('#define v_signal_with_handler_cast(sig, handler) signal((sig), ((void (*)(int))(handler)))')
	g.writeln('string string__clone(string a);')
	g.writeln('void string__free(string* s);')
	g.writeln('string string__plus(string s, string a);')
	g.writeln('string int__str(int n);')
	g.writeln('string i64__str(i64 n);')
	g.writeln('string u64__str(u64 nn);')
	g.writeln('string f64__str(double x);')
	g.writeln('string rune__str(u32 c);')
	g.writeln('u8* malloc_noscan(ptrdiff_t n);')
	g.writeln('void* memdup(void* src, ptrdiff_t sz);')
	g.writeln('static inline Array* v3_heap_array(Array value) { return (Array*)memdup(&value, sizeof(Array)); }')
	g.writeln('static inline string v3_c_lit(const char* s, int len) { return (string){.str = (u8*)s, .len = len, .is_lit = 1}; }')
	g.writeln("static inline string v3_string_pad(string s, int width, int left) { if (width < 0) { left = 1; width = -width; } if (s.len >= width) return s; int pad = width - s.len; u8* out = malloc_noscan((ptrdiff_t)width + 1); if (left) { memcpy(out, s.str, (size_t)s.len); memset(out + s.len, ' ', (size_t)pad); } else { memset(out, ' ', (size_t)pad); memcpy(out + pad, s.str, (size_t)s.len); } out[width] = 0; return (string){.str = out, .len = width, .is_lit = 0}; }")
	g.writeln("static inline string v3_string_upper_ascii(string s) { u8* out = malloc_noscan((ptrdiff_t)s.len + 1); for (int i = 0; i < s.len; ++i) { u8 c = s.str[i]; out[i] = c >= 'a' && c <= 'f' ? (u8)(c - ('a' - 'A')) : c; } out[s.len] = 0; return (string){.str = out, .len = s.len, .is_lit = 0}; }")
	g.writeln('static inline string v3_char_string(int c) { return rune__str((u32)c); }')
	g.writeln('static inline string v3_chan_str(chan ch, string elem) { if (ch == NULL) return string__plus(string__plus(v3_c_lit("chan ", 5), elem), v3_c_lit("(nil)", 5)); string out = string__plus(string__plus(v3_c_lit("chan ", 5), elem), v3_c_lit("{\\n    cap: ", 11)); out = string__plus(out, int__str(ch->cap)); out = string__plus(out, ch->closed != 0 ? v3_c_lit(", closed: true\\n}", 16) : v3_c_lit(", closed: false\\n}", 17)); return out; }')
	g.writeln('static inline double v3_f64_fixed_value(double x, int precision) { if (precision == 0) return x < 0.0 ? ceil(x - 0.5) : floor(x + 0.5); if (precision == 6) { double scale = 1000000.0; double ax = fabs(x) * scale; double base = floor(ax); double frac = ax - base; if (frac == 0.5) { double rounded = floor(ax + 0.5) / scale; return x < 0.0 ? -rounded : rounded; } } return x; }')
	g.writeln('static inline string v3_f64_fixed(double x, int precision) { if (precision > 16) { char base[128]; int b = snprintf(base, sizeof(base), "%.16g", x); if (b >= 0 && b < (int)sizeof(base)) { int dot = -1; int has_exp = 0; for (int i = 0; i < b; ++i) { if (base[i] == \'.\') dot = i; if (base[i] == \'e\' || base[i] == \'E\') has_exp = 1; } if (!has_exp) { int frac = dot >= 0 ? b - dot - 1 : 0; if (frac <= precision) { int n = b + (dot < 0 ? 1 : 0) + (precision - frac); u8* out = malloc_noscan(n + 1); memcpy(out, base, b); int pos = b; if (dot < 0) out[pos++] = \'.\'; while (frac++ < precision) out[pos++] = \'0\'; out[pos] = 0; return (string){.str = out, .len = n, .is_lit = 0}; } } } } double y = v3_f64_fixed_value(x, precision); char tmp[128]; int n = snprintf(tmp, sizeof(tmp), "%.*f", precision, y); if (n < 0) return v3_c_lit("", 0); if (n < (int)sizeof(tmp)) { u8* out = malloc_noscan(n + 1); memcpy(out, tmp, n + 1); return (string){.str = out, .len = n, .is_lit = 0}; } u8* out = malloc_noscan(n + 1); snprintf((char*)out, (size_t)n + 1, "%.*f", precision, y); return (string){.str = out, .len = n, .is_lit = 0}; }')
	g.writeln('static inline string v3_int_zpad(int n, int width) { string s = int__str(n); if (n < 0) return s; while (s.len < width) s = string__plus(v3_c_lit("0", 1), s); return s; }')
	g.writeln('static inline string v3_i64_zpad(i64 n, int width) { string s = i64__str(n); if (n < 0) return s; while (s.len < width) s = string__plus(v3_c_lit("0", 1), s); return s; }')
	g.writeln('static inline string v3_u64_zpad(u64 n, int width) { string s = u64__str(n); while (s.len < width) s = string__plus(v3_c_lit("0", 1), s); return s; }')
	g.writeln("static inline string v3_string_zpad(string s, int width) { if (s.len >= width) return s; int sign = s.len > 0 && s.str[0] == '-'; int pad = width - s.len; u8* out = malloc_noscan((ptrdiff_t)width + 1); int pos = 0; if (sign) out[pos++] = '-'; memset(out + pos, '0', (size_t)pad); pos += pad; memcpy(out + pos, s.str + sign, (size_t)(s.len - sign)); out[width] = 0; return (string){.str = out, .len = width, .is_lit = 0}; }")
	// Length-aware JSON string escaper: honors string.len so embedded NUL bytes are
	// escaped rather than truncating like a C NUL-terminated string. ASCII
	// codes are used to avoid escaping quirks; 92=\ 34=" 98=b 102=f 110=n 114=r 116=t 117=u 48=0.
	g.writeln('static inline string v3_json_encode_string(string s) { const char* hex = "0123456789abcdef"; u8* out = malloc_noscan((ptrdiff_t)s.len * 6 + 8); int p = 0; out[p++] = 34; for (int i = 0; i < s.len; i++) { u8 c = s.str[i]; if (c == 34) { out[p++]=92; out[p++]=34; } else if (c == 92) { out[p++]=92; out[p++]=92; } else if (c == 8) { out[p++]=92; out[p++]=98; } else if (c == 12) { out[p++]=92; out[p++]=102; } else if (c == 10) { out[p++]=92; out[p++]=110; } else if (c == 13) { out[p++]=92; out[p++]=114; } else if (c == 9) { out[p++]=92; out[p++]=116; } else if (c < 32) { out[p++]=92; out[p++]=117; out[p++]=48; out[p++]=48; out[p++]=hex[(c>>4)&15]; out[p++]=hex[c&15]; } else { out[p++]=c; } } out[p++] = 34; out[p] = 0; return (string){.str = out, .len = p, .is_lit = 0}; }')
	if g.has_cjson() {
		g.json_number_token_helpers()
	}
	g.writeln('static inline i64 v3_map_signed(void* p, int bytes) { if (bytes == 1) return *(signed char*)p; if (bytes == 2) return *(short*)p; if (bytes == 8) return *(long long*)p; return *(int*)p; }')
	g.writeln('static inline u64 v3_map_unsigned(void* p, int bytes) { if (bytes == 1) return *(unsigned char*)p; if (bytes == 2) return *(unsigned short*)p; if (bytes == 8) return *(unsigned long long*)p; return *(unsigned int*)p; }')
	g.writeln('static inline string v3_f32_array_str(float* vals, int n) { string out = v3_c_lit("[", 1); for (int i = 0; i < n; ++i) { if (i > 0) out = string__plus(out, v3_c_lit(", ", 2)); out = string__plus(out, f64__str((double)vals[i])); } return string__plus(out, v3_c_lit("]", 1)); }')
	g.writeln('static inline string v3_f64_array_str(double* vals, int n) { string out = v3_c_lit("[", 1); for (int i = 0; i < n; ++i) { if (i > 0) out = string__plus(out, v3_c_lit(", ", 2)); out = string__plus(out, f64__str(vals[i])); } return string__plus(out, v3_c_lit("]", 1)); }')
	g.writeln('static inline string v3_map_str_piece(void* p, int kind, int bytes, int fixed_len) {')
	g.writeln('\tif (kind == 1) { return string__plus(string__plus(v3_c_lit("\'", 1), *(string*)p), v3_c_lit("\'", 1)); }')
	g.writeln('\tif (kind == 2) { return v3_i64_zpad(v3_map_signed(p, bytes), 0); }')
	g.writeln('\tif (kind == 3) { return u64__str(v3_map_unsigned(p, bytes)); }')
	g.writeln('\tif (kind == 4) { u32 r = bytes == 1 ? (u32)(*(u8*)p) : *(u32*)p; return string__plus(string__plus(v3_c_lit("`", 1), rune__str(r)), v3_c_lit("`", 1)); }')
	g.writeln('\tif (kind == 5) { if (bytes == (int)sizeof(float)) return f64__str((double)*(float*)p); return f64__str(*(double*)p); }')
	g.writeln('\tif (kind == 6) { if (fixed_len == 0 && bytes == (int)sizeof(Array)) { Array a = *(Array*)p; if (a.element_size == (int)sizeof(float)) return v3_f32_array_str((float*)a.data, a.len); if (a.element_size == (int)sizeof(double)) return v3_f64_array_str((double*)a.data, a.len); } if (fixed_len > 0 && bytes == fixed_len * (int)sizeof(float)) return v3_f32_array_str((float*)p, fixed_len); int n = fixed_len > 0 ? fixed_len : bytes / (int)sizeof(double); return v3_f64_array_str((double*)p, n); }')
	g.writeln('\tif (kind == 8) { return f64__str((double)*(float*)p); }')
	g.writeln('\tif (kind == 9) { int n = fixed_len > 0 ? fixed_len : bytes / (int)sizeof(float); return v3_f32_array_str((float*)p, n); }')
	g.writeln('\tif (kind == 7) { return *(bool*)p ? v3_c_lit("true", 4) : v3_c_lit("false", 5); }')
	g.writeln('\treturn v3_c_lit("<map value>", 11);')
	g.writeln('}')
	g.writeln('static inline string v3_map_str(map m, int key_kind, int val_kind, int val_fixed_len) {')
	g.writeln('\tstring out = v3_c_lit("{", 1); bool first = true;')
	g.writeln('\tfor (int i = 0; i < m.key_values.len; ++i) {')
	g.writeln('\t\tif (m.key_values.deletes != 0 && m.key_values.all_deleted != 0 && m.key_values.all_deleted[i] != 0) continue;')
	g.writeln('\t\tif (!first) out = string__plus(out, v3_c_lit(", ", 2));')
	g.writeln('\t\tvoid* key = (void*)(m.key_values.keys + i * m.key_values.key_bytes);')
	g.writeln('\t\tvoid* val = (void*)(m.key_values.values + i * m.key_values.value_bytes);')
	g.writeln('\t\tout = string__plus(out, v3_map_str_piece(key, key_kind, m.key_values.key_bytes, 0));')
	g.writeln('\t\tout = string__plus(out, v3_c_lit(": ", 2));')
	g.writeln('\t\tout = string__plus(out, v3_map_str_piece(val, val_kind, m.value_bytes, val_fixed_len));')
	g.writeln('\t\tfirst = false;')
	g.writeln('\t}')
	g.writeln('\treturn string__plus(out, v3_c_lit("}", 1));')
	g.writeln('}')
	g.writeln('static inline int array_index_int(Array a, int val) { for (int i = 0; i < a.len; i++) if (((int*)a.data)[i] == val) return i; return -1; }')
	g.writeln('static inline int array_last_index_int(Array a, int val) { for (int i = a.len - 1; i >= 0; i--) if (((int*)a.data)[i] == val) return i; return -1; }')
	g.writeln('static inline bool array_contains_int(Array a, int val) { return array_index_int(a, val) >= 0; }')
	g.writeln('static inline int array_index_u8(Array a, u8 val) { for (int i = 0; i < a.len; i++) if (((u8*)a.data)[i] == val) return i; return -1; }')
	g.writeln('static inline int array_last_index_u8(Array a, u8 val) { for (int i = a.len - 1; i >= 0; i--) if (((u8*)a.data)[i] == val) return i; return -1; }')
	g.writeln('static inline bool array_contains_u8(Array a, u8 val) { return array_index_u8(a, val) >= 0; }')
	g.writeln('static inline int array_index_string(Array a, string val) { string* data = (string*)a.data; for (int i = 0; i < a.len; i++) if (data[i].len == val.len && memcmp(data[i].str, val.str, val.len) == 0) return i; return -1; }')
	g.writeln('static inline int array_last_index_string(Array a, string val) { string* data = (string*)a.data; for (int i = a.len - 1; i >= 0; i--) if (data[i].len == val.len && memcmp(data[i].str, val.str, val.len) == 0) return i; return -1; }')
	g.writeln('static inline bool array_contains_string(Array a, string val) { return array_index_string(a, val) >= 0; }')
	g.writeln('static inline int array_last_index_raw(Array a, const void* val) { for (int i = a.len - 1; i >= 0; i--) if (memcmp((u8*)a.data + (size_t)i * (size_t)a.element_size, val, (size_t)a.element_size) == 0) return i; return -1; }')
	g.writeln('static inline bool array_eq_raw(Array a, Array b, int elem_size) { return a.len == b.len && (a.len == 0 || memcmp(a.data, b.data, (size_t)a.len * elem_size) == 0); }')
	g.writeln('static inline bool array_eq_string(Array a, Array b) { if (a.len != b.len) return false; string* ad = (string*)a.data; string* bd = (string*)b.data; for (int i = 0; i < a.len; i++) if (ad[i].len != bd[i].len || memcmp(ad[i].str, bd[i].str, ad[i].len) != 0) return false; return true; }')
	g.writeln('static inline bool array_eq_array(Array a, Array b, int depth) { if (a.len != b.len || a.element_size != b.element_size) return false; if (depth <= 1 || a.element_size != sizeof(Array)) { if (a.element_size == sizeof(string)) return array_eq_string(a, b); return array_eq_raw(a, b, a.element_size); } Array* ad = (Array*)a.data; Array* bd = (Array*)b.data; for (int i = 0; i < a.len; i++) { if (!array_eq_array(ad[i], bd[i], depth - 1)) return false; } return true; }')
	g.writeln('static inline bool v3_map_map_eq(map a, map b);')
	g.writeln('static inline bool v3_map_value_eq(void* a, void* b, int value_bytes) { if (value_bytes == sizeof(string)) { string sa = *(string*)a; string sb = *(string*)b; return sa.len == sb.len && (sa.len == 0 || memcmp(sa.str, sb.str, sa.len) == 0); } if (value_bytes == sizeof(map)) { return v3_map_map_eq(*(map*)a, *(map*)b); } if (value_bytes == sizeof(string) + sizeof(map)) { string sa = *(string*)a; string sb = *(string*)b; if (!(sa.len == sb.len && (sa.len == 0 || memcmp(sa.str, sb.str, sa.len) == 0))) return false; map ma = *(map*)((u8*)a + sizeof(string)); map mb = *(map*)((u8*)b + sizeof(string)); return v3_map_map_eq(ma, mb); } if (value_bytes == sizeof(Array)) { Array aa = *(Array*)a; Array bb = *(Array*)b; if (aa.element_size != bb.element_size) return false; if (aa.element_size == sizeof(string)) return array_eq_string(aa, bb); if (aa.element_size == sizeof(Array)) return array_eq_array(aa, bb, 8); return array_eq_raw(aa, bb, aa.element_size); } return memcmp(a, b, value_bytes) == 0; }')
	g.writeln('static inline bool v3_map_map_eq(map a, map b) { if (a.len != b.len) return false; for (int i = 0; i < a.key_values.len; ++i) { if (a.key_values.deletes != 0 && a.key_values.all_deleted != 0 && a.key_values.all_deleted[i] != 0) continue; void* ak = (void*)(a.key_values.keys + i * a.key_values.key_bytes); void* av = (void*)(a.key_values.values + i * a.key_values.value_bytes); bool found = false; for (int j = 0; j < b.key_values.len; ++j) { if (b.key_values.deletes != 0 && b.key_values.all_deleted != 0 && b.key_values.all_deleted[j] != 0) continue; void* bk = (void*)(b.key_values.keys + j * b.key_values.key_bytes); if (a.key_eq_fn(ak, bk)) { void* bv = (void*)(b.key_values.values + j * b.key_values.value_bytes); if (!v3_map_value_eq(av, bv, a.value_bytes)) return false; found = true; break; } } if (!found) return false; } return true; }')
	g.writeln('static inline bool fixed_array_contains_string(const string* a, int len, string val) { for (int i = 0; i < len; i++) if (a[i].len == val.len && memcmp(a[i].str, val.str, val.len) == 0) return true; return false; }')
	g.writeln('static inline bool fixed_array_contains_u8(const u8* a, int len, u8 val) { for (int i = 0; i < len; i++) if (a[i] == val) return true; return false; }')
	g.writeln('static inline bool fixed_array_contains_int(const int* a, int len, int val) { for (int i = 0; i < len; i++) if (a[i] == val) return true; return false; }')
	g.writeln('static inline string Array_str(Array a) { if (a.element_size == 1) { u8* buf = (u8*)malloc((size_t)a.len + 1); if (a.len > 0) memcpy(buf, a.data, (size_t)a.len); buf[a.len] = 0; return (string){buf, a.len, 0}; } return (string){(u8*)"[]", 2, 1}; }')
	g.writeln('#ifndef max_int')
	g.writeln('#define max_int max_i32')
	g.writeln('#endif')
	g.writeln('#ifndef min_int')
	g.writeln('#define min_int min_i32')
	g.writeln('#endif')
	g.writeln('')
}

fn (g &FlatGen) has_cjson() bool {
	for flag in g.c_flags {
		if flag.contains('cJSON') {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) json_number_token_helpers() {
	g.writeln('static inline const u8* v3_json_skip_space(const u8* p, const u8* end) { while (p < end && (*p == 32 || *p == 9 || *p == 10 || *p == 13)) p++; return p; }')
	g.writeln('static inline const u8* v3_json_skip_string(const u8* p, const u8* end) { if (p >= end || *p != 34) return p; p++; while (p < end) { if (*p == 92) { p++; if (p < end) p++; continue; } if (*p == 34) return p + 1; p++; } return p; }')
	g.writeln('static const u8* v3_json_preserve_number_tokens_inner(const u8* p, const u8* end, cJSON* item);')
	g.writeln('static const u8* v3_json_preserve_number_tokens_inner(const u8* p, const u8* end, cJSON* item) {')
	g.writeln('\tp = v3_json_skip_space(p, end); if (p >= end) return p;')
	g.writeln('\tif (*p == 123) { cJSON* child = item != NULL ? item->child : NULL; p++; p = v3_json_skip_space(p, end); while (p < end && *p != 125) { p = v3_json_skip_string(p, end); p = v3_json_skip_space(p, end); if (p < end && *p == 58) p++; p = v3_json_preserve_number_tokens_inner(p, end, child); if (child != NULL) child = child->next; p = v3_json_skip_space(p, end); if (p < end && *p == 44) { p++; p = v3_json_skip_space(p, end); } else { break; } } return p < end && *p == 125 ? p + 1 : p; }')
	g.writeln('\tif (*p == 91) { cJSON* child = item != NULL ? item->child : NULL; p++; p = v3_json_skip_space(p, end); while (p < end && *p != 93) { p = v3_json_preserve_number_tokens_inner(p, end, child); if (child != NULL) child = child->next; p = v3_json_skip_space(p, end); if (p < end && *p == 44) { p++; p = v3_json_skip_space(p, end); } else { break; } } return p < end && *p == 93 ? p + 1 : p; }')
	g.writeln('\tif (*p == 34) return v3_json_skip_string(p, end);')
	g.writeln('\tconst u8* start = p; while (p < end && *p != 44 && *p != 93 && *p != 125 && *p != 32 && *p != 9 && *p != 10 && *p != 13) p++; if (item != NULL && cJSON_IsNumber(item) && item->valuestring == NULL) { size_t len = (size_t)(p - start); char* raw = (char*)cJSON_malloc(len + 1); if (raw != NULL) { memcpy(raw, start, len); raw[len] = 0; item->valuestring = raw; } } return p;')
	g.writeln('}')
	g.writeln('static inline void v3_json_preserve_number_tokens(const u8* json, int len, cJSON* root) { if (json != NULL && len > 0 && root != NULL) v3_json_preserve_number_tokens_inner(json, json + len, root); }')
}

fn (mut g FlatGen) filelock_compat_decls() {
	if !g.libc_compat_fns['filelock'] {
		return
	}
	g.writeln('#ifndef V_OS_FILELOCK_HELPERS_H')
	g.writeln('#ifdef _WIN32')
	g.writeln('BOOL LockFileEx(HANDLE handle, DWORD flags, DWORD reserved, DWORD low, DWORD high, OVERLAPPED* overlap);')
	g.writeln('BOOL UnlockFileEx(HANDLE handle, DWORD reserved, DWORD low, DWORD high, OVERLAPPED* overlap);')
	g.writeln('static inline int v_filelock_lock(HANDLE handle, int exclusive, int immediate, u64 start, u64 len) { OVERLAPPED overlap; memset(&overlap, 0, sizeof(overlap)); overlap.Offset = (DWORD)(start & 0xffffffffULL); overlap.OffsetHigh = (DWORD)(start >> 32); DWORD flags = immediate ? LOCKFILE_FAIL_IMMEDIATELY : 0; if (exclusive) { flags |= LOCKFILE_EXCLUSIVE_LOCK; } DWORD low = len == 0 ? MAXDWORD : (DWORD)(len & 0xffffffffULL); DWORD high = len == 0 ? MAXDWORD : (DWORD)(len >> 32); return LockFileEx(handle, flags, 0, low, high, &overlap) ? 0 : -1; }')
	g.writeln('static inline int v_filelock_unlock(HANDLE handle, u64 start, u64 len) { OVERLAPPED overlap; memset(&overlap, 0, sizeof(overlap)); overlap.Offset = (DWORD)(start & 0xffffffffULL); overlap.OffsetHigh = (DWORD)(start >> 32); DWORD low = len == 0 ? MAXDWORD : (DWORD)(len & 0xffffffffULL); DWORD high = len == 0 ? MAXDWORD : (DWORD)(len >> 32); return UnlockFileEx(handle, 0, low, high, &overlap) ? 0 : -1; }')
	g.writeln('#else')
	g.writeln('static inline int v_filelock_lock(int fd, int exclusive, int immediate, u64 start, u64 len) { struct flock fl; memset(&fl, 0, sizeof(fl)); fl.l_type = exclusive ? F_WRLCK : F_RDLCK; fl.l_whence = SEEK_SET; fl.l_start = (off_t)start; fl.l_len = len == 0 ? 0 : (off_t)len; return fcntl(fd, immediate ? F_SETLK : F_SETLKW, &fl); }')
	g.writeln('static inline int v_filelock_unlock(int fd, u64 start, u64 len) { struct flock fl; memset(&fl, 0, sizeof(fl)); fl.l_type = F_UNLCK; fl.l_whence = SEEK_SET; fl.l_start = (off_t)start; fl.l_len = len == 0 ? 0 : (off_t)len; return fcntl(fd, F_SETLK, &fl); }')
	g.writeln('#endif')
	g.writeln('#endif')
}

fn (mut g FlatGen) collect_fixed_array_typedefs_needed() map[string]FixedArrayTypedefInfo {
	if g.fixed_array_typedefs_ready {
		return g.fixed_array_typedefs_needed
	}
	mut needed := map[string]FixedArrayTypedefInfo{}
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	for name, ret_type in g.tc.fn_ret_types {
		g.tc.cur_module = module_from_qualified_name(name)
		g.collect_fixed_array_typedef(ret_type, g.tc.cur_module, mut needed)
	}
	for name, param_types in g.tc.fn_param_types {
		g.tc.cur_module = module_from_qualified_name(name)
		for param_type in param_types {
			g.collect_fixed_array_typedef(param_type, g.tc.cur_module, mut needed)
		}
	}
	for name, fields in g.tc.structs {
		g.tc.cur_module = g.fixed_array_typedef_type_module(name, old_module)
		for field in fields {
			g.collect_fixed_array_typedef(field.typ, g.tc.cur_module, mut needed)
		}
	}
	for name, fields in g.tc.interface_fields {
		g.tc.cur_module = module_from_qualified_name(name)
		for field in fields {
			g.collect_fixed_array_typedef(field.typ, g.tc.cur_module, mut needed)
		}
	}
	for name, typ in g.global_types {
		g.tc.cur_module = g.global_modules[name] or { old_module }
		g.collect_fixed_array_typedef(typ, g.tc.cur_module, mut needed)
	}
	for _, typ in g.tc.c_globals {
		g.tc.cur_module = old_module
		g.collect_fixed_array_typedef(typ, g.tc.cur_module, mut needed)
	}
	for name, typ in g.tc.const_types {
		g.tc.cur_module = g.const_modules[name] or { old_module }
		g.collect_fixed_array_typedef(typ, g.tc.cur_module, mut needed)
	}
	mut cur_file := old_file
	mut cur_module := old_module
	for node in g.a.nodes {
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			cur_module = g.tc.file_modules[cur_file] or { old_module }
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}
		g.tc.cur_file = cur_file
		g.tc.cur_module = cur_module
		g.collect_fixed_array_typedef_text(node.typ, cur_module, mut needed)
		match node.kind {
			.array_init, .array_literal, .cast_expr, .sizeof_expr, .typeof_expr {
				g.collect_fixed_array_typedef_text(node.value, cur_module, mut needed)
			}
			else {}
		}
	}
	g.tc.cur_module = old_module
	g.tc.cur_file = old_file
	g.fixed_array_typedefs_needed = needed.move()
	g.fixed_array_typedefs_ready = true
	return g.fixed_array_typedefs_needed
}

fn (mut g FlatGen) fixed_array_typedefs() {
	needed := g.collect_fixed_array_typedefs_needed()
	old_len := g.emitted_fixed_array_typedefs.len
	for name, info in needed {
		g.emit_fixed_array_typedef(name, info, needed, mut g.emitted_fixed_array_typedefs)
	}
	// Return wrappers for non-early element types (struct/`string`/nested fixed array):
	// their bare typedef (above) and element definitions are available now, so a C
	// function returning `[N]Foo`/`[N]string` can return the wrapper struct instead of the
	// raw array type C rejects. Sorted for deterministic output.
	mut wrapper_names := []string{}
	for name, _ in needed {
		wrapper_names << name
	}
	wrapper_names.sort()
	mut emitted_wrapper := false
	for name in wrapper_names {
		if name !in g.fixed_array_ret_wrappers {
			continue
		}
		info := needed[name] or { continue }
		if fixed_array_typedef_is_early(info.arr) {
			continue
		}
		// Completes the struct forward-declared in fixed_array_early_typedefs().
		g.emit_fixed_array_ret_wrapper(name, info, true)
		emitted_wrapper = true
	}
	if g.emitted_fixed_array_typedefs.len > old_len || emitted_wrapper {
		g.writeln('')
	}
}

// fixed_array_typedef_is_early reports whether a fixed array's bare typedef can be
// emitted before struct definitions: its element chain must bottom out in a
// primitive/pointer/enum (not a struct or `string`, whose definitions come later).
fn fixed_array_typedef_is_early(arr types.ArrayFixed) bool {
	elem := arr.elem_type
	if elem is types.ArrayFixed {
		return fixed_array_typedef_is_early(elem)
	}
	return fixed_array_elem_is_early_complete(elem)
}

// populate_fixed_array_ret_wrappers records which fixed-array types get a return
// wrapper struct. It must run before function bodies are generated, so that fn
// signatures, return statements and call sites all agree on whether a given
// fixed-array return is wrapped. EVERY fixed-array type is wrapped, because C
// functions and fn pointers cannot return a raw array type regardless of the element:
// primitive/pointer/enum element wrappers are emitted early (before structs), while
// struct/`string`/nested element wrappers are emitted by fixed_array_typedefs(), after
// the element type is defined.
fn (mut g FlatGen) populate_fixed_array_ret_wrappers() {
	old_module := g.tc.cur_module
	for name, ret_type in g.tc.fn_ret_types {
		g.tc.cur_module = module_from_qualified_name(name)
		g.collect_fixed_array_return_wrapper(ret_type)
		g.collect_fn_type_fixed_array_return_wrappers(ret_type)
	}
	for name, param_types in g.tc.fn_param_types {
		g.tc.cur_module = module_from_qualified_name(name)
		for param_type in param_types {
			g.collect_fn_type_fixed_array_return_wrappers(param_type)
		}
	}
	for name, fields in g.tc.structs {
		g.tc.cur_module = g.fixed_array_typedef_type_module(name, old_module)
		for field in fields {
			g.collect_fn_type_fixed_array_return_wrappers(field.typ)
		}
	}
	for name, fields in g.tc.interface_fields {
		g.tc.cur_module = module_from_qualified_name(name)
		for field in fields {
			g.collect_fn_type_fixed_array_return_wrappers(field.typ)
		}
	}
	for name, typ in g.global_types {
		g.tc.cur_module = g.global_modules[name] or { old_module }
		g.collect_fn_type_fixed_array_return_wrappers(typ)
	}
	for _, typ in g.tc.c_globals {
		g.tc.cur_module = old_module
		g.collect_fn_type_fixed_array_return_wrappers(typ)
	}
	for name, typ in g.tc.const_types {
		g.tc.cur_module = g.const_modules[name] or { old_module }
		g.collect_fn_type_fixed_array_return_wrappers(typ)
	}
	g.tc.cur_module = old_module
}

fn (mut g FlatGen) collect_fixed_array_return_wrapper(typ types.Type) {
	if typ is types.ArrayFixed {
		g.fixed_array_ret_wrappers[g.tc.c_type(typ)] = true
	} else if typ is types.Alias {
		g.collect_fixed_array_return_wrapper(typ.base_type)
	}
}

fn (mut g FlatGen) collect_fn_type_fixed_array_return_wrappers(typ types.Type) {
	if typ is types.FnType {
		g.collect_fixed_array_return_wrapper(typ.return_type)
		for param in typ.params {
			g.collect_fn_type_fixed_array_return_wrappers(param)
		}
	} else if typ is types.Pointer {
		g.collect_fn_type_fixed_array_return_wrappers(typ.base_type)
	} else if typ is types.Alias {
		g.collect_fn_type_fixed_array_return_wrappers(typ.base_type)
	} else if typ is types.OptionType {
		g.collect_fn_type_fixed_array_return_wrappers(typ.base_type)
	} else if typ is types.ResultType {
		g.collect_fn_type_fixed_array_return_wrappers(typ.base_type)
	} else if typ is types.Array {
		g.collect_fn_type_fixed_array_return_wrappers(typ.elem_type)
	} else if typ is types.ArrayFixed {
		g.collect_fn_type_fixed_array_return_wrappers(typ.elem_type)
	} else if typ is types.Map {
		g.collect_fn_type_fixed_array_return_wrappers(typ.key_type)
		g.collect_fn_type_fixed_array_return_wrappers(typ.value_type)
	} else if typ is types.MultiReturn {
		for item in typ.types {
			g.collect_fn_type_fixed_array_return_wrappers(item)
		}
	}
}

// emit_fixed_array_ret_wrapper writes the one-field `struct { T ret_arr[N]; }` wrapper
// for a fixed-array return type. The element type's C definition must already be emitted.
// `tagged` completes a previously forward-declared named struct (`struct X { ... };`),
// used for non-early element types whose wrapper is referenced by an fn-pointer typedef
// emitted earlier; otherwise a fresh anonymous typedef is written.
fn (mut g FlatGen) emit_fixed_array_ret_wrapper(name string, info FixedArrayTypedefInfo, tagged bool) {
	arr := info.arr
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	elem_ct := g.fixed_array_elem_c_type(arr.elem_type)
	len_expr := g.fixed_array_len_value(arr)
	g.tc.cur_module = old_module
	wname := fixed_array_ret_wrapper_name(name)
	if tagged {
		g.writeln('struct ${wname} { ${elem_ct} ret_arr[${len_expr}]; };')
	} else {
		g.writeln('typedef struct { ${elem_ct} ret_arr[${len_expr}]; } ${wname};')
	}
}

// emit_fixed_array_ret_wrapper_forward forward-declares a named return-wrapper struct so an
// fn-pointer typedef can name it as a (by-value) return type before the wrapper's element
// type is defined; the struct body is completed later by emit_fixed_array_ret_wrapper.
fn (mut g FlatGen) emit_fixed_array_ret_wrapper_forward(name string) {
	wname := fixed_array_ret_wrapper_name(name)
	g.writeln('typedef struct ${wname} ${wname};')
}

// fixed_array_early_typedefs emits, before the fn-ptr typedef block, the bare
// typedefs for fixed arrays whose element chain is a primitive/pointer/enum, plus
// a one-field struct wrapper `struct { T ret_arr[N]; }` for each fixed-array
// return type. fn-ptr typedefs may name a fixed array in param position (bare
// typedef) or return position (wrapper); both must therefore be defined first. C
// functions cannot return raw array types, hence the wrapper (as V1 does). Bare
// typedefs of struct/`string`-element fixed arrays are deferred to
// fixed_array_typedefs(), after the struct definitions.
fn (mut g FlatGen) fixed_array_early_typedefs() {
	needed := g.collect_fixed_array_typedefs_needed()
	mut names := []string{}
	for name, _ in needed {
		names << name
	}
	names.sort()
	mut emitted_any := false
	for name in names {
		info := needed[name] or { continue }
		if !fixed_array_typedef_is_early(info.arr) {
			continue
		}
		g.emit_fixed_array_typedef(name, info, needed, mut g.emitted_fixed_array_typedefs)
		emitted_any = true
	}
	// Wrapper structs for fixed-array return types. Primitive/pointer/enum element wrappers
	// are fully defined here. Struct/`string`/nested element wrappers can't be defined yet
	// (their element type comes later), but an fn-pointer typedef in the next block may name
	// one as a return type, so forward-declare the named struct now and complete it in
	// fixed_array_typedefs(), after the element definitions.
	for name in names {
		if name !in g.fixed_array_ret_wrappers {
			continue
		}
		info := needed[name] or { continue }
		if fixed_array_typedef_is_early(info.arr) {
			g.emit_fixed_array_ret_wrapper(name, info, false)
		} else {
			g.emit_fixed_array_ret_wrapper_forward(name)
		}
		emitted_any = true
	}
	if emitted_any {
		g.writeln('')
	}
}

// fixed_array_ret_wrapper_name is the struct name wrapping a fixed-array return.
fn fixed_array_ret_wrapper_name(bare_c_name string) string {
	return '_v_ret_${bare_c_name}'
}

// fixed_array_elem_is_early_complete reports whether a fixed-array element type's
// C definition is available before the fn_ptr/return-wrapper typedef block (i.e.
// it is a primitive, pointer, or enum — not a struct or nested fixed array, whose
// bare typedefs/definitions are emitted later).
fn fixed_array_elem_is_early_complete(elem types.Type) bool {
	return elem is types.Primitive || elem is types.Pointer || elem is types.Enum
}

// fn_return_type_name is the C type to write for a function/fn-ptr return type,
// substituting the fixed-array wrapper struct when one exists.
fn (mut g FlatGen) fn_return_type_name(t types.Type) string {
	if fixed := array_fixed_type(t) {
		bare := g.tc.c_type(fixed)
		if bare in g.fixed_array_ret_wrappers {
			return fixed_array_ret_wrapper_name(bare)
		}
	}
	ct := g.optional_type_name(t)
	// A function/fn-ptr-valued return (`fn f() fn () int`) has the internal `fn_ptr:...`
	// encoding for its C type; map it to the shared `_fn_ptr_N` typedef, since a C function
	// cannot be declared returning that raw encoding (it would emit invalid C).
	if ct.starts_with('fn_ptr:') {
		return g.resolve_fn_ptr_type(ct)
	}
	return ct
}

// fn_ptr_return_ct maps a fixed-array return c_type name (string form, used by the
// fn-ptr typedef machinery) to its wrapper struct name when one exists.
fn (g &FlatGen) fn_ptr_return_ct(ct string) string {
	if ct.starts_with('Array_fixed_') && ct in g.fixed_array_ret_wrappers {
		return fixed_array_ret_wrapper_name(ct)
	}
	return ct
}

// emit_ready_fixed_array_typedefs emits, during the topological struct emission,
// any fixed-array bare typedef whose element type is now fully defined (i.e. its
// element struct has been emitted). Struct fields reference the typedef name
// (`Array_fixed_vec__Vec4_f32 x`), so the typedef must precede any struct that
// uses it — which a single later pass cannot guarantee.
fn (mut g FlatGen) emit_ready_fixed_array_typedefs(needed map[string]FixedArrayTypedefInfo, emitted_structs map[string]bool) {
	for name, info in needed {
		if g.emitted_fixed_array_typedefs[name] {
			continue
		}
		if g.fixed_array_elem_defined(info.arr, emitted_structs) {
			g.emit_fixed_array_typedef(name, info, needed, mut g.emitted_fixed_array_typedefs)
		}
	}
}

// fixed_array_elem_defined reports whether a fixed array's element type is fully
// available: a primitive/pointer/enum (always), or a struct/`string` already
// emitted. Aliases are unwrapped to their underlying type first (`SimdFloat4` ->
// `vec.Vec4[f32]` struct), so an alias to a not-yet-emitted struct is not treated
// as ready.
fn (g &FlatGen) fixed_array_elem_defined(arr types.ArrayFixed, emitted_structs map[string]bool) bool {
	return g.fixed_array_type_defined(arr.elem_type, emitted_structs)
}

fn (g &FlatGen) fixed_array_type_defined(typ0 types.Type, emitted_structs map[string]bool) bool {
	mut typ := typ0
	for typ is types.Alias {
		typ = typ.base_type
	}
	if typ is types.ArrayFixed {
		return g.fixed_array_type_defined(typ.elem_type, emitted_structs)
	}
	if typ is types.Struct {
		return g.tc.c_type(typ) in emitted_structs
	}
	if typ is types.String {
		return 'string' in emitted_structs
	}
	return true
}

fn (mut g FlatGen) fixed_array_typedef_type_module(name string, fallback string) string {
	if info := g.struct_decl_infos[name] {
		return info.module
	}
	mod := module_from_qualified_name(name)
	if mod.len > 0 {
		return mod
	}
	return fallback
}

fn module_from_qualified_name(name string) string {
	if name.contains('.') {
		return name.all_before_last('.')
	}
	if name.contains('__') {
		return name.all_before_last('__').replace('__', '.')
	}
	return ''
}

fn fixed_array_typedef_module_priority(module_name string) int {
	if module_name.len == 0 || module_name == 'C' {
		return 0
	}
	if module_name == 'main' || module_name == 'builtin' {
		return 1
	}
	return 2
}

fn (mut g FlatGen) collect_fixed_array_typedef(typ types.Type, source_module string, mut needed map[string]FixedArrayTypedefInfo) {
	if typ is types.ArrayFixed {
		old_module := g.tc.cur_module
		g.tc.cur_module = source_module
		name := g.fixed_array_c_type(typ)
		g.tc.cur_module = old_module
		existing_priority := if name in needed {
			fixed_array_typedef_module_priority(needed[name].module)
		} else {
			-1
		}
		current_priority := fixed_array_typedef_module_priority(source_module)
		if name !in needed || current_priority > existing_priority {
			needed[name] = FixedArrayTypedefInfo{
				arr:    typ
				module: source_module
			}
		}
		g.collect_fixed_array_typedef(typ.elem_type, source_module, mut needed)
	} else if typ is types.Pointer {
		g.collect_fixed_array_typedef(typ.base_type, source_module, mut needed)
	} else if typ is types.Alias {
		alias_module := module_from_qualified_name(typ.name)
		base_module := if alias_module.len > 0 { alias_module } else { source_module }
		g.collect_fixed_array_typedef(typ.base_type, base_module, mut needed)
	} else if typ is types.OptionType {
		g.collect_fixed_array_typedef(typ.base_type, source_module, mut needed)
	} else if typ is types.ResultType {
		g.collect_fixed_array_typedef(typ.base_type, source_module, mut needed)
	} else if typ is types.Array {
		g.collect_fixed_array_typedef(typ.elem_type, source_module, mut needed)
	} else if typ is types.Map {
		g.collect_fixed_array_typedef(typ.key_type, source_module, mut needed)
		g.collect_fixed_array_typedef(typ.value_type, source_module, mut needed)
	} else if typ is types.FnType {
		for param in typ.params {
			g.collect_fixed_array_typedef(param, source_module, mut needed)
		}
		g.collect_fixed_array_typedef(typ.return_type, source_module, mut needed)
	} else if typ is types.MultiReturn {
		for item in typ.types {
			g.collect_fixed_array_typedef(item, source_module, mut needed)
		}
	}
}

fn (mut g FlatGen) collect_fixed_array_typedef_text(type_text string, source_module string, mut needed map[string]FixedArrayTypedefInfo) {
	if type_text.len == 0 || !fixed_array_type_text_may_need_typedef(type_text) {
		return
	}
	clean := trimmed_space(type_text)
	if clean.len == 0 {
		return
	}
	typ := g.tc.parse_type(clean)
	if fixed_array_typedef_has_non_decimal_len(typ) {
		return
	}
	g.collect_fixed_array_typedef(typ, source_module, mut needed)
}

fn fixed_array_type_text_may_need_typedef(type_text string) bool {
	for i in 0 .. type_text.len - 1 {
		if type_text[i] == `[` && type_text[i + 1] >= `0` && type_text[i + 1] <= `9` {
			return true
		}
	}
	return false
}

fn fixed_array_typedef_has_non_decimal_len(typ types.Type) bool {
	if typ is types.ArrayFixed {
		if typ.len_expr.len > 0 {
			return true
		}
		return fixed_array_typedef_has_non_decimal_len(typ.elem_type)
	}
	if typ is types.Pointer {
		return fixed_array_typedef_has_non_decimal_len(typ.base_type)
	}
	if typ is types.Alias {
		return fixed_array_typedef_has_non_decimal_len(typ.base_type)
	}
	if typ is types.OptionType {
		return fixed_array_typedef_has_non_decimal_len(typ.base_type)
	}
	if typ is types.ResultType {
		return fixed_array_typedef_has_non_decimal_len(typ.base_type)
	}
	if typ is types.Array {
		return fixed_array_typedef_has_non_decimal_len(typ.elem_type)
	}
	if typ is types.Map {
		return fixed_array_typedef_has_non_decimal_len(typ.key_type)
			|| fixed_array_typedef_has_non_decimal_len(typ.value_type)
	}
	if typ is types.FnType {
		for param in typ.params {
			if fixed_array_typedef_has_non_decimal_len(param) {
				return true
			}
		}
		return fixed_array_typedef_has_non_decimal_len(typ.return_type)
	}
	if typ is types.MultiReturn {
		for item in typ.types {
			if fixed_array_typedef_has_non_decimal_len(item) {
				return true
			}
		}
	}
	return false
}

fn (mut g FlatGen) emit_fixed_array_typedef(name string, info FixedArrayTypedefInfo, needed map[string]FixedArrayTypedefInfo, mut emitted map[string]bool) {
	if emitted[name] {
		return
	}
	arr := info.arr
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	g.emit_fixed_array_elem_deps(arr.elem_type, needed, mut emitted)
	elem_ct := g.fixed_array_elem_c_type(arr.elem_type)
	len_expr := g.fixed_array_len_value(arr)
	g.writeln('typedef ${elem_ct} ${name}[${len_expr}];')
	g.tc.cur_module = old_module
	emitted[name] = true
}

fn (mut g FlatGen) emit_fixed_array_elem_deps(elem types.Type, needed map[string]FixedArrayTypedefInfo, mut emitted map[string]bool) {
	if elem is types.ArrayFixed {
		inner_name := g.fixed_array_c_type(elem)
		if inner := needed[inner_name] {
			g.emit_fixed_array_typedef(inner_name, inner, needed, mut emitted)
		}
	} else if elem is types.OptionType {
		g.emit_fixed_array_optional_elem_deps(elem, needed, mut emitted)
	} else if elem is types.ResultType {
		g.emit_fixed_array_optional_elem_deps(elem, needed, mut emitted)
	} else if elem is types.Alias {
		g.emit_fixed_array_elem_deps(elem.base_type, needed, mut emitted)
	}
}

fn (mut g FlatGen) emit_fixed_array_optional_elem_deps(elem types.Type, needed map[string]FixedArrayTypedefInfo, mut emitted map[string]bool) {
	base := if elem is types.OptionType {
		elem.base_type
	} else if elem is types.ResultType {
		elem.base_type
	} else {
		return
	}
	if base is types.ArrayFixed {
		g.emit_fixed_array_elem_deps(base, needed, mut emitted)
	}
	opt_name := g.optional_type_name(elem)
	if opt_name != 'Optional' {
		val_ct, _ := g.optional_value_ct(elem)
		g.emit_optional_typedef(opt_name, val_ct)
	}
}

fn (mut g FlatGen) global_decls() {
	old_module := g.tc.cur_module
	for name, typ in g.global_types {
		if mod := g.global_modules[name] {
			g.tc.cur_module = mod
		} else {
			g.tc.cur_module = old_module
		}
		decl_typ := g.global_storage_type(name, typ)
		is_fn_capture := name.contains('__anon_fn_')
		if decl_typ is types.ArrayFixed {
			c_elem, dims := g.fixed_array_decl_parts(decl_typ)
			init := if g.has_zero_sized_leading_init_slot(decl_typ) { '' } else { ' = {0}' }
			if is_fn_capture {
				g.writeln('#if defined(__TINYC__)')
				g.writeln('i32 pthread_key_create(u64* key, void (*dtor)(void*));')
				g.writeln('void* pthread_getspecific(u64 key);')
				g.writeln('i32 pthread_setspecific(u64 key, const void* const_ptr);')
				g.writeln('static u64 ${g.cname(name)}_key;')
				g.writeln('static void ${g.cname(name)}_key_init(void) __attribute__((constructor));')
				g.writeln('static void ${g.cname(name)}_key_init(void) { pthread_key_create(&${g.cname(name)}_key, free); }')
				g.writeln('static ${c_elem} (*${g.cname(name)}_slot(void))${dims} { void* p = pthread_getspecific(${g.cname(name)}_key); if (!p) { p = calloc(1, sizeof(*${g.cname(name)}_slot())); pthread_setspecific(${g.cname(name)}_key, p); } return p; }')
				g.writeln('#define ${g.cname(name)} (*${g.cname(name)}_slot())')
				g.writeln('#else')
				g.writeln('_Thread_local ${c_elem} ${g.cname(name)}${dims}${init};')
				g.writeln('#endif')
			} else {
				g.writeln('${c_elem} ${g.cname(name)}${dims}${init};')
			}
			continue
		}
		mut ct := g.tc.c_type(decl_typ)
		if ct == 'Optional' {
			if concrete_ct := g.global_init_optional_c_type(name) {
				ct = concrete_ct
			}
		}
		if ct == 'void' {
			continue
		}
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		if name.starts_with('C.') {
			continue
		}
		init := if g.can_use_global_brace_zero_init(decl_typ, ct) { ' = {0}' } else { '' }
		// Capturing fn literals are immediately consumed callbacks in the V3
		// frontend. Their lifted capture slots must nevertheless be per-thread:
		// a process-global slot lets concurrent invocations overwrite one
		// another before the callback runs. Native C compilers use TLS directly;
		// TinyCC uses pthread keys because it does not implement `_Thread_local`.
		if is_fn_capture {
			cname := g.cname(name)
			g.writeln('#if defined(__TINYC__)')
			g.writeln('i32 pthread_key_create(u64* key, void (*dtor)(void*));')
			g.writeln('void* pthread_getspecific(u64 key);')
			g.writeln('i32 pthread_setspecific(u64 key, const void* const_ptr);')
			g.writeln('static u64 ${cname}_key;')
			g.writeln('static void ${cname}_key_init(void) __attribute__((constructor));')
			g.writeln('static void ${cname}_key_init(void) { pthread_key_create(&${cname}_key, free); }')
			g.writeln('static ${ct}* ${cname}_slot(void) { void* p = pthread_getspecific(${cname}_key); if (!p) { p = calloc(1, sizeof(${ct})); pthread_setspecific(${cname}_key, p); } return (${ct}*)p; }')
			g.writeln('#define ${cname} (*${cname}_slot())')
			g.writeln('#else')
			g.writeln('_Thread_local ${ct} ${cname}${init};')
			g.writeln('#endif')
			continue
		}
		// With -prealloc the arena base block is per-thread (lazily initialized
		// on first allocation in each thread); a shared pointer would make all
		// threads bump the same block without synchronization. cc gets real
		// TLS; tcc implements no _Thread_local, so it gets a pthread-key
		// emulation behind an lvalue macro. The key setup needs no
		// synchronization: the first allocation always happens on the main
		// thread, long before any `spawn`. The key APIs are declared manually
		// (V's generated C declares its own `pthread_t`, so <pthread.h> would
		// conflict); the key out-param is stored in an 8-byte zeroed slot,
		// which stays correct where pthread_key_t is 4 bytes (little-endian).
		if g.prealloc && name == 'g_memory_block' {
			g.writeln('#if defined(__TINYC__)')
			// Shapes must match vlib's own C.pthread_* extern declarations
			// exactly (u64/i32), or tcc rejects the redefinition when a module
			// also declares them.
			g.writeln('i32 pthread_key_create(u64* key, void (*dtor)(void*));')
			g.writeln('void* pthread_getspecific(u64 key);')
			g.writeln('i32 pthread_setspecific(u64 key, const void* const_ptr);')
			g.writeln('static u64 g_memory_block_key = 0;')
			g.writeln('static int g_memory_block_key_ready = 0;')
			g.writeln('static ${ct}* g_memory_block_slot(void) {')
			g.writeln('	void* p;')
			g.writeln('	if (!g_memory_block_key_ready) {')
			g.writeln('		pthread_key_create(&g_memory_block_key, 0);')
			g.writeln('		g_memory_block_key_ready = 1;')
			g.writeln('	}')
			g.writeln('	p = pthread_getspecific(g_memory_block_key);')
			g.writeln('	if (p == 0) {')
			g.writeln('		p = calloc(1, sizeof(${ct}));')
			g.writeln('		pthread_setspecific(g_memory_block_key, p);')
			g.writeln('	}')
			g.writeln('	return (${ct}*)p;')
			g.writeln('}')
			g.writeln('#define g_memory_block (*g_memory_block_slot())')
			g.writeln('#else')
			g.writeln('_Thread_local ${ct} ${g.cname(name)}${init};')
			g.writeln('#endif')
			continue
		}
		g.writeln('${ct} ${g.cname(name)}${init};')
	}
	g.tc.cur_module = old_module
	if g.global_types.len > 0 {
		g.writeln('')
	}
	g.emit_global_inits()
}

fn (mut g FlatGen) global_storage_type(name string, typ types.Type) types.Type {
	if typ is types.Struct && typ.name == 'Optional' {
		if val_id := g.global_inits[name] {
			init_type := g.usable_expr_type(val_id)
			if init_type is types.OptionType || init_type is types.ResultType {
				return init_type
			}
			if init_type is types.Struct && init_type.name.starts_with('Optional_') {
				return init_type
			}
		}
	}
	return typ
}

fn (mut g FlatGen) global_init_optional_c_type(name string) ?string {
	val_id := g.global_inits[name] or { return none }
	init_type := g.usable_expr_type(val_id)
	if init_type is types.OptionType || init_type is types.ResultType {
		return g.optional_type_name(init_type)
	}
	if init_type is types.Struct && init_type.name.starts_with('Optional_') {
		return g.tc.c_type(init_type)
	}
	decl_type := g.declared_call_return_type(val_id)
	if decl_type is types.OptionType || decl_type is types.ResultType {
		return g.optional_type_name(decl_type)
	}
	if decl_type is types.Struct && decl_type.name.starts_with('Optional_') {
		return g.tc.c_type(decl_type)
	}
	return none
}

fn (mut g FlatGen) test_failure_helpers() {
	g.writeln('static void v3_eprint_lit(const char* s) {')
	g.writeln('\tfprintf(stderr, "%s", s);')
	g.writeln('}')
	g.writeln('')
}

// emit_global_inits queues assignments for `__global x = expr` declarations into
// _vinit. The C globals are emitted zero-initialized above; their initializer
// expressions (often function calls like `new_timers(...)`) cannot be C static
// initializers, so they must run at startup. Without this, such globals stay
// NULL/zero and the first access segfaults.
//
// Plain initializers are emitted as `name = expr;`. Fixed-array globals are
// copied from a generated compound literal with `memmove`, since C arrays are
// not assignable. `&Struct{}` is emitted as a self-contained heap allocation
// (`(T*)memdup(&(T){...}, sizeof(T))`), so it is safe. Other prefix/array
// initializers that would need a dropped temporary are skipped, leaving the
// global zero/NULL -- no regression versus never initializing globals at all.
fn (mut g FlatGen) emit_global_inits() {
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_file = old_file
	}
	for qname in g.global_init_order {
		val_id := g.global_inits[qname] or { continue }
		if int(val_id) < 0 {
			continue
		}
		// g_main_argc/g_main_argv are filled in by main's preamble (from argc/argv)
		// *before* _vinit runs, and are zero by default in C anyway. Re-emitting their
		// `= 0` initializer here would clobber the real argv, leaving os.args empty.
		cqname := g.cname(qname)
		if cqname == 'g_main_argc' || cqname == 'g_main_argv' {
			continue
		}
		if mod := g.global_modules[qname] {
			g.tc.cur_module = mod
		} else {
			g.tc.cur_module = old_module
		}
		// Type texts in the initializer may be import-alias qualified
		// (`json.Any` under `import x.json2 as json`); alias resolution is
		// file-scoped, so parse_type needs the declaring file's context.
		if file := g.global_files[qname] {
			g.tc.cur_file = file
		} else {
			g.tc.cur_file = old_file
		}
		if typ := g.global_types[qname] {
			if typ is types.ArrayFixed {
				target := g.cname(qname)
				g.queue_fixed_array_runtime_init(target, val_id, typ)
				continue
			}
		}
		if !g.is_safe_global_init(val_id) {
			continue
		}
		tmp_sb := g.sb
		tmp_line_start := g.line_start
		g.sb = strings.new_builder(64)
		g.line_start = true
		g.gen_expr(val_id)
		expr_str := g.sb.str()
		g.sb = tmp_sb
		g.line_start = tmp_line_start
		if trimmed_space(expr_str).len == 0 {
			continue
		}
		target := g.cname(qname)
		g.queue_runtime_init('\t${target} = ${expr_str};')
		if typ := g.global_types[qname] {
			if typ is types.Map {
				g.queue_map_literal_sets(target, val_id, typ)
			}
		}
	}
	g.tc.cur_module = old_module
}

// is_safe_global_init reports whether a global initializer can be emitted as a
// self-contained `name = expr;` assignment in _vinit, i.e. without auxiliary
// declarations/temporaries that the global context cannot host.
fn (g &FlatGen) is_safe_global_init(val_id flat.NodeId) bool {
	if int(val_id) < 0 {
		return false
	}
	node := g.a.nodes[int(val_id)]
	if node.kind == .prefix {
		// `&Struct{}` becomes an inline `(T*)memdup(&(T){...}, sizeof(T))`, which is
		// self-contained; allow it. Other prefixes (e.g. `&local`) would need a
		// dropped temporary, so skip them.
		if node.op == .amp && node.children_count > 0 {
			child := g.a.nodes[int(g.a.child(&node, 0))]
			return child.kind == .struct_init || child.kind == .assoc
		}
		return false
	}
	return match node.kind {
		.array_literal, .array_init {
			// Array literals need a backing temp the transformer drops for globals;
			// leave them zero/NULL instead of emitting a reference to an undeclared
			// symbol.
			false
		}
		else {
			true
		}
	}
}

fn (g &FlatGen) const_get_deps(val_id flat.NodeId) []string {
	mut deps := []string{}
	mut visited_fns := map[string]bool{}
	g.const_collect_deps_inner(val_id, mut deps, mut visited_fns, map[string]bool{})
	return deps
}

fn (g &FlatGen) const_collect_deps(val_id flat.NodeId, mut deps []string) {
	mut visited_fns := map[string]bool{}
	g.const_collect_deps_inner(val_id, mut deps, mut visited_fns, map[string]bool{})
}

// const_collect_deps_inner walks a const initializer (recursing into called helper
// bodies) collecting the consts it reads. `shadowed` holds names bound by the current
// helper's parameters/locals so an identifier that shadows a const is not mistaken for
// a dependency (which could invent a false dependency cycle).
fn (g &FlatGen) const_collect_deps_inner(val_id flat.NodeId, mut deps []string, mut visited_fns map[string]bool, shadowed map[string]bool) {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(val_id)]
	match node.kind {
		.fn_literal, .lambda_expr {
			// Nested functions have their own lexical scope and are not executed merely
			// because the enclosing helper is called.
			return
		}
		.fn_decl {
			mut fn_shadowed := shadowed.clone()
			for i in 0 .. node.children_count {
				child := g.a.child_node(&node, i)
				if child.kind == .param && child.value.len > 0 {
					fn_shadowed[child.value] = true
				}
			}
			g.const_collect_scope_children(node, 0, mut deps, mut visited_fns, mut fn_shadowed)
			return
		}
		.block {
			mut block_shadowed := shadowed.clone()
			g.const_collect_scope_children(node, 0, mut deps, mut visited_fns, mut block_shadowed)
			return
		}
		.decl_assign {
			g.const_collect_decl_assign_deps(node, mut deps, mut visited_fns, shadowed)
			return
		}
		.if_expr {
			g.const_collect_if_deps(node, mut deps, mut visited_fns, shadowed)
			return
		}
		.for_stmt {
			g.const_collect_for_deps(node, mut deps, mut visited_fns, shadowed)
			return
		}
		.for_in_stmt {
			g.const_collect_for_in_deps(node, mut deps, mut visited_fns, shadowed)
			return
		}
		.match_branch {
			g.const_collect_match_branch_deps(node, mut deps, mut visited_fns, shadowed)
			return
		}
		else {}
	}

	if node.kind == .ident || node.kind == .selector {
		if !g.const_ref_base_shadowed(node, shadowed) {
			const_name := g.const_ref_name_from_node(node)
			if const_name.len > 0 {
				deps << const_name
			}
		}
	}
	if node.kind == .call && node.children_count > 0
		&& !g.const_ref_base_shadowed(g.a.child_node(&node, 0), shadowed) {
		callee := g.a.child_node(&node, 0)
		mut callee_name := ''
		mut callee_module := ''
		if callee.kind == .ident {
			callee_name = callee.value
		} else if callee.kind == .selector {
			callee_name = callee.value
			if callee.children_count > 0 {
				base_id := g.a.child(callee, 0)
				base := g.a.nodes[int(base_id)]
				if base.kind == .ident && base.value in g.modules {
					// Resolve an import alias (`import some.mod as m` makes the base
					// ident `m`) to the real module name so the module match compares
					// against the actual `module` declaration.
					callee_module = g.modules[base.value]
				} else {
					receiver := g.const_call_receiver_type_name(base_id)
					if receiver.len > 0 {
						callee_name = '${receiver}.${callee.value}'
					} else if base.kind == .ident {
						callee_module = base.value
					}
				}
			}
		}
		if resolved := g.tc.resolved_call_name(val_id) {
			callee_name = resolved
		}
		// Key the visited set by the qualified name so `a.foo` and `b.foo` are treated
		// as distinct bodies rather than one being skipped.
		visit_key := if callee_module.len > 0 {
			'${callee_module}.${callee_name}'
		} else {
			callee_name
		}
		if callee_name.len > 0 && !visited_fns[visit_key] {
			visited_fns[visit_key] = true
			if target := g.const_fn_decl_node(callee_name, callee_module) {
				// A callee starts a fresh lexical scope; bindings in the caller do not
				// shadow const references inside the called helper.
				g.const_collect_deps_inner(target, mut deps, mut visited_fns, map[string]bool{})
			}
		}
	}
	for i in 0 .. node.children_count {
		g.const_collect_deps_inner(g.a.child(&node, i), mut deps, mut visited_fns, shadowed)
	}
}

fn (g &FlatGen) const_fn_decl_node(callee_name string, callee_module string) ?flat.NodeId {
	if g.fn_decl_nodes_by_name.len == 0 && g.fn_decl_nodes_by_short.len == 0
		&& g.fn_decl_nodes_by_module_short.len == 0 {
		return g.const_fn_decl_node_scan(callee_name, callee_module)
	}
	short := callee_name.all_after_last('.')
	if callee_module.len > 0 {
		if id := g.fn_decl_nodes_by_module_short['${callee_module}\x01${short}'] {
			return id
		}
		module_short := callee_module.all_after_last('.')
		if module_short != callee_module {
			if id := g.fn_decl_nodes_by_module_short['${module_short}\x01${short}'] {
				return id
			}
		}
	}
	// A declaration value can itself be qualified (for example `Type.method`).
	// Try the complete callee name and each dotted suffix, selecting the earliest
	// declaration just like the former AST-order scan.
	mut exact_target := -1
	mut suffix := callee_name
	for {
		if id := g.fn_decl_nodes_by_name[suffix] {
			if exact_target < 0 || int(id) < exact_target {
				exact_target = int(id)
			}
		}
		dot := suffix.index('.') or { break }
		suffix = suffix[dot + 1..]
	}
	if exact_target >= 0 {
		return flat.NodeId(exact_target)
	}
	return g.fn_decl_nodes_by_short[short] or { none }
}

fn (g &FlatGen) const_fn_decl_node_scan(callee_name string, callee_module string) ?flat.NodeId {
	short := callee_name.all_after_last('.')
	mut cur_module := ''
	mut module_target := -1
	mut exact_target := -1
	mut suffix_target := -1
	for i, candidate in g.a.nodes {
		if candidate.kind == .module_decl {
			cur_module = candidate.value
			continue
		}
		if candidate.kind != .fn_decl
			|| (candidate.value != callee_name && candidate.value.all_after_last('.') != short) {
			continue
		}
		if module_target < 0 && callee_module.len > 0
			&& (cur_module == callee_module || cur_module == callee_module.all_after_last('.')) {
			module_target = i
		}
		if exact_target < 0
			&& (candidate.value == callee_name || callee_name.ends_with('.${candidate.value}')) {
			exact_target = i
		}
		if suffix_target < 0 {
			suffix_target = i
		}
	}
	target := if module_target >= 0 {
		module_target
	} else if exact_target >= 0 {
		exact_target
	} else {
		suffix_target
	}
	if target >= 0 {
		return flat.NodeId(target)
	}
	return none
}

fn (g &FlatGen) const_call_receiver_type_name(base_id flat.NodeId) string {
	resolved := types.unwrap_pointer(g.tc.resolve_type(base_id))
	name := resolved.name()
	if name.len > 0 && name !in ['void', 'unknown'] {
		return name
	}
	base := g.a.nodes[int(base_id)]
	if base.typ.len > 0 && base.typ !in ['void', 'unknown'] {
		return base.typ.trim_left('&')
	}
	if base.kind == .struct_init {
		return base.value.trim_left('&')
	}
	return ''
}

fn (g &FlatGen) const_collect_scope_children(node flat.Node, start int, mut deps []string, mut visited_fns map[string]bool, mut shadowed map[string]bool) {
	for i in start .. node.children_count {
		child_id := g.a.child(&node, i)
		child := g.a.nodes[int(child_id)]
		if child.kind == .decl_assign {
			g.const_collect_decl_assign_deps(child, mut deps, mut visited_fns, shadowed)
			g.const_add_decl_assign_bindings(child, mut shadowed)
		} else {
			g.const_collect_deps_inner(child_id, mut deps, mut visited_fns, shadowed)
		}
	}
}

fn (g &FlatGen) const_decl_assign_is_multi_return(node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	if _ := g.multi_return_expr_type_for_lhs_count(g.a.child(&node, 1), node.children_count - 1) {
		return true
	}
	return false
}

fn (g &FlatGen) const_collect_decl_assign_deps(node flat.Node, mut deps []string, mut visited_fns map[string]bool, shadowed map[string]bool) {
	if node.children_count < 2 {
		return
	}
	if g.const_decl_assign_is_multi_return(node) {
		g.const_collect_deps_inner(g.a.child(&node, 1), mut deps, mut visited_fns, shadowed)
		return
	}
	mut i := 1
	for i < node.children_count {
		g.const_collect_deps_inner(g.a.child(&node, i), mut deps, mut visited_fns, shadowed)
		i += 2
	}
}

fn (g &FlatGen) const_add_decl_assign_bindings(node flat.Node, mut shadowed map[string]bool) {
	if node.children_count < 2 {
		return
	}
	if g.const_decl_assign_is_multi_return(node) {
		for i in 0 .. node.children_count {
			if i == 1 {
				continue
			}
			g.const_add_shadow_binding(g.a.child_node(&node, i), mut shadowed)
		}
		return
	}
	mut i := 0
	for i < node.children_count {
		g.const_add_shadow_binding(g.a.child_node(&node, i), mut shadowed)
		i += 2
	}
}

fn (g &FlatGen) const_add_shadow_binding(node flat.Node, mut shadowed map[string]bool) {
	if node.kind == .ident && node.value.len > 0 && node.value != '_' {
		shadowed[node.value] = true
	}
}

fn (g &FlatGen) const_collect_if_deps(node flat.Node, mut deps []string, mut visited_fns map[string]bool, shadowed map[string]bool) {
	if node.children_count == 0 {
		return
	}
	cond_id := g.a.child(&node, 0)
	cond := g.a.nodes[int(cond_id)]
	if cond.kind == .decl_assign {
		mut then_shadowed := shadowed.clone()
		g.const_collect_decl_assign_deps(cond, mut deps, mut visited_fns, shadowed)
		g.const_add_decl_assign_bindings(cond, mut then_shadowed)
		if node.children_count > 1 {
			g.const_collect_deps_inner(g.a.child(&node, 1), mut deps, mut visited_fns,
				then_shadowed)
		}
	} else {
		g.const_collect_deps_inner(cond_id, mut deps, mut visited_fns, shadowed)
		if node.children_count > 1 {
			g.const_collect_deps_inner(g.a.child(&node, 1), mut deps, mut visited_fns, shadowed)
		}
	}
	for i in 2 .. node.children_count {
		g.const_collect_deps_inner(g.a.child(&node, i), mut deps, mut visited_fns, shadowed)
	}
}

fn (g &FlatGen) const_collect_for_deps(node flat.Node, mut deps []string, mut visited_fns map[string]bool, shadowed map[string]bool) {
	mut loop_shadowed := shadowed.clone()
	if node.children_count > 0 {
		init_id := g.a.child(&node, 0)
		init := g.a.nodes[int(init_id)]
		if init.kind == .decl_assign {
			g.const_collect_decl_assign_deps(init, mut deps, mut visited_fns, shadowed)
			g.const_add_decl_assign_bindings(init, mut loop_shadowed)
		} else {
			g.const_collect_deps_inner(init_id, mut deps, mut visited_fns, shadowed)
		}
	}
	header_end := if node.children_count < 3 {
		node.children_count
	} else {
		3
	}
	for i in 1 .. header_end {
		g.const_collect_deps_inner(g.a.child(&node, i), mut deps, mut visited_fns, loop_shadowed)
	}
	if node.children_count > 3 {
		mut body_shadowed := loop_shadowed.clone()
		g.const_collect_scope_children(node, 3, mut deps, mut visited_fns, mut body_shadowed)
	}
}

fn (g &FlatGen) const_collect_for_in_deps(node flat.Node, mut deps []string, mut visited_fns map[string]bool, shadowed map[string]bool) {
	body_start := node.value.int()
	header_end := if body_start < node.children_count {
		body_start
	} else {
		node.children_count
	}
	for i in 2 .. header_end {
		g.const_collect_deps_inner(g.a.child(&node, i), mut deps, mut visited_fns, shadowed)
	}
	mut body_shadowed := shadowed.clone()
	if node.children_count > 0 {
		g.const_add_shadow_binding(g.a.child_node(&node, 0), mut body_shadowed)
	}
	if node.children_count > 1 {
		g.const_add_shadow_binding(g.a.child_node(&node, 1), mut body_shadowed)
	}
	if body_start < node.children_count {
		g.const_collect_scope_children(node, body_start, mut deps, mut visited_fns, mut
			body_shadowed)
	}
}

fn (g &FlatGen) const_collect_match_branch_deps(node flat.Node, mut deps []string, mut visited_fns map[string]bool, shadowed map[string]bool) {
	condition_count := if node.value == 'else' { 0 } else { node.value.int() }
	body_start := if condition_count < node.children_count {
		condition_count
	} else {
		node.children_count
	}
	for i in 0 .. body_start {
		g.const_collect_deps_inner(g.a.child(&node, i), mut deps, mut visited_fns, shadowed)
	}
	if condition_count < node.children_count {
		mut branch_shadowed := shadowed.clone()
		g.const_collect_scope_children(node, condition_count, mut deps, mut visited_fns, mut
			branch_shadowed)
	}
}

// const_ref_base_shadowed reports whether `node` (an ident, or a selector whose base
// is an ident) refers to a name bound by the current helper scope rather than a const.
fn (g &FlatGen) const_ref_base_shadowed(node flat.Node, shadowed map[string]bool) bool {
	if shadowed.len == 0 {
		return false
	}
	if node.kind == .ident {
		return shadowed[node.value]
	}
	if node.kind == .selector && node.children_count > 0 {
		base := g.a.child_node(&node, 0)
		return base.kind == .ident && shadowed[base.value]
	}
	return false
}

fn (g &FlatGen) const_refs_other_const(val_id flat.NodeId) bool {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(val_id)]
	if node.kind == .ident || node.kind == .selector {
		return g.const_ref_name_from_node(node).len > 0
	}
	for i in 0 .. node.children_count {
		if g.const_refs_other_const(g.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) emit_const(name string, val_id flat.NodeId) {
	old_module := g.tc.cur_module
	old_file := g.tc.cur_file
	defer {
		g.tc.cur_file = old_file
	}
	if name in g.const_modules {
		g.tc.cur_module = g.const_modules[name]
	}
	// Import-alias qualified type texts (`json.Any`) in the initializer resolve
	// per file, so parse_type needs the declaring file's context.
	if file := g.const_files[name] {
		g.tc.cur_file = file
	}
	val_node := g.a.nodes[int(val_id)]
	if val_node.kind == .empty {
		g.tc.cur_module = old_module
		return
	}
	v_type := if val_node.kind == .offsetof_expr {
		types.Type(types.usize_)
	} else {
		g.const_storage_type_for_value(name, val_id, g.tc.resolve_type(val_id))
	}
	ct := g.tc.c_type(v_type)
	qname := g.const_ident_c_name(name)
	if qname == 'builtin__error_sentinel' {
		type_id := g.ierror_type_id_for_pattern('MessageError')
		object_name := '${qname}__object'
		message := '(string){"error", 5, 1}'
		g.writeln('MessageError ${object_name} = (MessageError){.msg = ${message}};')
		g.writeln('IError ${qname} = (IError){._typ = ${type_id}, ._object = &${object_name}, .message = ${message}, .code = 0};')
		g.tc.cur_module = old_module
		return
	}
	if val_node.kind == .block && val_node.children_count > 0 {
		// A lowered const initializer (`.map()` chains): leading statements
		// compute temps, the last child is the value expression.
		if ct != 'void' {
			g.writeln('${ct} ${qname};')
			g.queue_const_runtime_init(g.const_block_init_to_string(qname, val_node))
		}
		g.tc.cur_module = old_module
		return
	}
	expr_str := if v_type is types.Array && val_node.kind == .array_literal {
		g.expr_to_string_with_expected_type(val_id, v_type)
	} else if g.is_const_expr(val_id) {
		g.const_expr_to_string(val_id, []string{})
	} else {
		g.expr_to_string(val_id)
	}
	if trimmed_space(expr_str).len == 0 {
		g.tc.cur_module = old_module
		return
	}
	mut is_static_const := g.is_const_expr(val_id) && !g.const_expr_needs_runtime_storage(expr_str)
	if v_type is types.Array {
		is_static_const = false
	}
	if v_type is types.ArrayFixed && v_type.elem_type is types.ArrayFixed {
		is_static_const = false
	}
	if !is_static_const {
		if v_type is types.ArrayFixed {
			c_elem, dims := g.fixed_array_decl_parts(v_type)
			g.writeln('${c_elem} ${qname}${dims};')
			g.queue_const_fixed_array_runtime_init(qname, val_id, v_type)
		} else if ct != 'void' {
			g.writeln('${ct} ${qname};')
			// The initializer is not a compile-time constant (e.g. `os.args =
			// arguments()`), so it cannot be a C static initializer. Run it at startup
			// in _vinit; otherwise the const stays zero/empty and first use is wrong.
			g.queue_const_runtime_init('\t${qname} = ${expr_str};')
			if v_type is types.Map {
				g.queue_const_map_literal_sets(qname, val_id, v_type)
			}
		}
		g.tc.cur_module = old_module
		return
	}
	if v_type is types.String {
		g.writeln('string ${qname} = ${expr_str};')
	} else if v_type is types.ArrayFixed {
		c_elem, dims := g.fixed_array_decl_parts(v_type)
		g.writeln('const ${c_elem} ${qname}${dims} = ${expr_str};')
	} else if v_type is types.Primitive || v_type is types.Char || v_type is types.Rune
		|| v_type is types.ISize || v_type is types.USize || v_type is types.Enum
		|| ct in ['bool', 'char', 'i8', 'i16', 'i32', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'float', 'double', 'isize', 'usize'] {
		if qname == 'max_len' && ct == 'int' {
			g.writeln('enum { ${qname} = ${expr_str} };')
		} else if g.name_collides_with_struct_field(qname) {
			// A `#define` whose name matches a struct field would wrongly expand every
			// `.field` access; emit a real `const` variable instead (C keeps member and
			// ordinary-identifier namespaces separate, so there is no collision).
			g.writeln('static const ${ct} ${qname} = ${expr_str};')
		} else {
			g.writeln('#define ${qname} (${expr_str})')
		}
	} else {
		g.writeln('const ${ct} ${qname} = ${expr_str};')
	}
	g.tc.cur_module = old_module
}

// name_collides_with_struct_field reports whether a name is the C name of any struct
// field, building the set lazily on first use.
fn (mut g FlatGen) name_collides_with_struct_field(name string) bool {
	if g.field_name_set.len == 0 {
		for _, fields in g.tc.structs {
			for f in fields {
				g.field_name_set[c_field_name(f.name)] = true
			}
		}
		// Guard against an all-fieldless program re-scanning every call.
		g.field_name_set[''] = true
	}
	return name in g.field_name_set
}

fn (g &FlatGen) const_expr_needs_runtime_storage(expr string) bool {
	return expr.contains('array_new(') || expr.contains('new_map(') || expr.contains('({')
		|| expr.contains('__map_')
}

fn (mut g FlatGen) queue_map_literal_sets(target string, val_id flat.NodeId, map_type types.Map) {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(val_id)]
	if node.kind != .map_init {
		return
	}
	c_key := g.map_key_temp_c_type(map_type.key_type)
	c_val := g.value_c_type(map_type.value_type)
	for i := 0; i + 1 < node.children_count; i += 2 {
		key := g.expr_to_string_with_expected_type(g.a.child(&node, i), map_type.key_type)
		val := g.expr_to_string_with_expected_type(g.a.child(&node, i + 1), map_type.value_type)
		g.queue_runtime_init('\tmap__set(&${target}, &(${c_key}[]){${key}}, &(${c_val}[]){${val}});')
	}
}

fn (mut g FlatGen) queue_const_map_literal_sets(target string, val_id flat.NodeId, map_type types.Map) {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(val_id)]
	if node.kind != .map_init {
		return
	}
	c_key := g.map_key_temp_c_type(map_type.key_type)
	c_val := g.value_c_type(map_type.value_type)
	for i := 0; i + 1 < node.children_count; i += 2 {
		key := g.expr_to_string_with_expected_type(g.a.child(&node, i), map_type.key_type)
		val := g.expr_to_string_with_expected_type(g.a.child(&node, i + 1), map_type.value_type)
		g.queue_const_runtime_init('\tmap__set(&${target}, &(${c_key}[]){${key}}, &(${c_val}[]){${val}});')
	}
}

fn (mut g FlatGen) queue_fixed_array_runtime_init(target string, val_id flat.NodeId, fixed types.ArrayFixed) bool {
	expr := g.fixed_array_runtime_copy_source_expr(val_id, fixed)
	if trimmed_space(expr).len == 0 {
		return false
	}
	g.queue_runtime_init('\tmemmove(${target}, ${expr}, sizeof(${target}));')
	return true
}

fn (mut g FlatGen) queue_const_fixed_array_runtime_init(target string, val_id flat.NodeId, fixed types.ArrayFixed) bool {
	expr := g.fixed_array_runtime_copy_source_expr(val_id, fixed)
	if trimmed_space(expr).len == 0 {
		return false
	}
	g.queue_const_runtime_init('\tmemmove(${target}, ${expr}, sizeof(${target}));')
	return true
}

fn (mut g FlatGen) fixed_array_runtime_copy_source_expr(val_id flat.NodeId, fixed types.ArrayFixed) string {
	literal := g.fixed_array_compound_literal_expr(val_id, fixed)
	if trimmed_space(literal).len > 0 {
		return literal
	}
	return g.fixed_array_copy_source_string(val_id, types.Type(fixed))
}

fn (mut g FlatGen) fixed_array_compound_literal_expr(val_id flat.NodeId, fixed types.ArrayFixed) string {
	init := g.fixed_array_initializer_string(val_id, fixed)
	if trimmed_space(init).len == 0 {
		return ''
	}
	return '(${g.fixed_array_c_type(fixed)})${init}'
}

fn (mut g FlatGen) fixed_array_initializer_string(val_id flat.NodeId, fixed types.ArrayFixed) string {
	mut builder := strings.new_builder(64)
	if !g.write_fixed_array_initializer(mut builder, val_id, fixed) {
		unsafe { builder.free() }
		return ''
	}
	result := builder.str()
	unsafe { builder.free() }
	return result
}

fn (mut g FlatGen) write_fixed_array_initializer(mut builder strings.Builder, val_id flat.NodeId, fixed types.ArrayFixed) bool {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(val_id)]
	if node.kind in [.ident, .selector] {
		const_name := g.const_ref_name_from_node(node)
		if const_name.len > 0 {
			if const_id := g.const_vals[const_name] {
				return g.write_fixed_array_initializer(mut builder, const_id, fixed)
			}
		}
	}
	if node.kind == .postfix && node.children_count > 0 {
		return g.write_fixed_array_initializer(mut builder, g.a.child(&node, 0), fixed)
	}
	if node.kind == .cast_expr && node.children_count > 0 {
		return g.write_fixed_array_initializer(mut builder, g.a.child(&node, 0), fixed)
	}
	if node.kind == .array_init && node.children_count == 0 {
		builder.write_string('{0}')
		return true
	}
	if node.kind != .array_literal {
		return false
	}
	builder.write_u8(`{`)
	for i in 0 .. node.children_count {
		if i > 0 {
			builder.write_string(', ')
		}
		child_id := g.a.child(&node, i)
		if fixed.elem_type is types.ArrayFixed {
			g.write_fixed_array_initializer(mut builder, child_id, fixed.elem_type)
		} else {
			g.write_fixed_array_elem_initializer(mut builder, child_id, fixed.elem_type)
		}
	}
	builder.write_u8(`}`)
	return true
}

fn (mut g FlatGen) write_fixed_array_elem_initializer(mut builder strings.Builder, val_id flat.NodeId, elem_type types.Type) {
	if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
		builder.write_u8(`0`)
		return
	}
	node := g.a.nodes[int(val_id)]
	if g.is_const_expr(val_id) && !(node.kind == .prefix && node.op == .amp) {
		const_val := g.const_expr_to_string(val_id, []string{})
		if trimmed_space(const_val).len > 0 {
			builder.write_string(const_val)
			return
		}
	}
	expr := g.expr_to_string_with_expected_type(val_id, elem_type)
	if trimmed_space(expr).len > 0 {
		builder.write_string(expr)
		return
	}
	builder.write_u8(`0`)
}

fn (mut g FlatGen) precompute_consts() string {
	old_sb := g.sb
	old_line_start := g.line_start
	g.sb = strings.new_builder(1024)
	g.line_start = true
	mut emitted := map[string]bool{}
	mut deferred := []string{}
	mut names := g.const_init_order.clone()
	for name, _ in g.const_vals {
		if g.is_const_alias_name(name) || name in names {
			continue
		}
		names << name
	}
	names = g.ordered_const_init_names(names)
	for name in names {
		val_id := g.const_vals[name] or { continue }
		if g.is_const_alias_name(name) {
			continue
		}
		if int(val_id) < 0 || int(val_id) >= g.a.nodes.len {
			continue
		}
		old_module := g.tc.cur_module
		if name in g.const_modules {
			g.tc.cur_module = g.const_modules[name]
		}
		deps := g.const_get_deps(val_id)
		g.tc.cur_module = old_module
		mut all_met := true
		for dep in deps {
			if dep !in emitted {
				all_met = false
				break
			}
		}
		if !all_met {
			deferred << name
		} else {
			g.emit_const(name, val_id)
			emitted[name] = true
		}
	}
	for _ in 0 .. 20 {
		if deferred.len == 0 {
			break
		}
		mut remaining := []string{}
		for name in deferred {
			val_id := g.const_vals[name]
			deps := g.const_get_deps(val_id)
			mut all_met := true
			for dep in deps {
				if dep !in emitted {
					all_met = false
					break
				}
			}
			if all_met {
				g.emit_const(name, val_id)
				emitted[name] = true
			} else {
				remaining << name
			}
		}
		deferred = remaining.clone()
	}
	for name in deferred {
		g.emit_const(name, g.const_vals[name])
	}
	if g.const_vals.len > 0 {
		g.writeln('')
	}
	result := g.sb.str()
	// `.str()` copies out of the temporary const builder.
	unsafe { g.sb.free() }
	g.sb = old_sb
	g.line_start = old_line_start
	return result
}

fn (g &FlatGen) ordered_const_init_names(names []string) []string {
	mut names_by_module := map[string][]string{}
	mut module_order := []string{}
	for name in names {
		mod := g.const_modules[name] or { '' }
		if mod !in names_by_module {
			names_by_module[mod] = []string{}
			module_order << mod
		}
		names_by_module[mod] << name
	}
	mut result := []string{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for mod in module_order {
		g.visit_const_init_module(mod, names_by_module, mut visiting, mut visited, mut result)
	}
	return result
}

fn (g &FlatGen) visit_const_init_module(mod string, names_by_module map[string][]string, mut visiting map[string]bool, mut visited map[string]bool, mut result []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	for dep in g.module_imports[mod] or { []string{} } {
		dep_module := if dep in names_by_module || dep in g.module_imports {
			dep
		} else {
			startup_module_key(dep)
		}
		if dep_module in names_by_module {
			g.visit_const_init_module(dep_module, names_by_module, mut visiting, mut visited, mut
				result)
		}
	}
	visiting.delete(mod)
	visited[mod] = true
	if module_names := names_by_module[mod] {
		result << module_names
	}
}

fn startup_module_key(mod string) string {
	if mod.contains('.') {
		return mod.all_after_last('.')
	}
	return mod
}

fn (g &FlatGen) is_const_expr(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	return match node.kind {
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .enum_val,
		.sizeof_expr, .offsetof_expr {
			true
		}
		.prefix {
			if node.op == .amp {
				false
			} else {
				g.is_const_expr(g.a.child(&node, 0))
			}
		}
		.infix {
			g.is_const_expr(g.a.child(&node, 0)) && g.is_const_expr(g.a.child(&node, 1))
		}
		.paren {
			g.is_const_expr(g.a.child(&node, 0))
		}
		.cast_expr {
			g.is_const_expr(g.a.child(&node, 0))
		}
		.ident {
			g.const_ref_name(node.value).len > 0
		}
		.selector {
			const_name := g.const_ref_name_from_node(node)
			if const_name.len > 0 {
				true
			} else if node.children_count > 0 {
				base := g.a.child_node(&node, 0)
				base.kind == .ident && base.value == 'C'
			} else {
				false
			}
		}
		.array_literal {
			mut all_const := true
			for ci in 0 .. node.children_count {
				if !g.is_const_expr(g.a.child(&node, ci)) {
					all_const = false
					break
				}
			}
			all_const
		}
		.struct_init {
			mut all_const := true
			for ci in 0 .. node.children_count {
				child := g.a.child_node(&node, ci)
				if child.kind == .field_init {
					if ftyp := g.struct_field_type(node.value, child.value) {
						if ftyp is types.Array || ftyp is types.Map {
							all_const = false
							break
						}
					}
				}
				if child.children_count > 0 && !g.is_const_expr(g.a.child(child, 0)) {
					all_const = false
					break
				}
			}
			all_const
		}
		else {
			false
		}
	}
}

fn (g &FlatGen) is_string_plus_call(node flat.Node) bool {
	if node.kind != .call || node.children_count != 3 {
		return false
	}
	callee := g.a.child_node(&node, 0)
	return callee.kind == .ident && callee.value == 'string__plus'
}

fn (g &FlatGen) string_plus_call_is_nested(_id flat.NodeId, node flat.Node) bool {
	if !g.is_string_plus_call(node) {
		return false
	}
	lhs := g.a.child_node(&node, 1)
	rhs := g.a.child_node(&node, 2)
	return g.is_string_plus_call(lhs) || g.is_string_plus_call(rhs)
}

fn (g &FlatGen) collect_string_plus_parts(id flat.NodeId, mut parts []flat.NodeId) {
	node := g.a.nodes[int(id)]
	if g.is_string_plus_call(node) {
		g.collect_string_plus_parts(g.a.child(&node, 1), mut parts)
		g.collect_string_plus_parts(g.a.child(&node, 2), mut parts)
		return
	}
	parts << id
}

// Nested string concatenation owns each intermediate result. Emit the chain as
// ordered statements and release every superseded accumulator.
fn (mut g FlatGen) gen_owned_string_plus_chain(id flat.NodeId) {
	mut parts := []flat.NodeId{}
	g.collect_string_plus_parts(id, mut parts)
	if parts.len < 3 {
		g.gen_call(id, g.a.nodes[int(id)])
		return
	}
	tmp := g.tmp_count
	g.tmp_count++
	g.write('({')
	for i, part in parts {
		g.write(' string __str_plus_part_${tmp}_${i} = ')
		g.gen_expr_as_string(part)
		g.write(';')
	}
	g.write(' string __str_plus_acc_${tmp}_1 = string__plus(__str_plus_part_${tmp}_0, __str_plus_part_${tmp}_1);')
	mut previous := '__str_plus_acc_${tmp}_1'
	for i := 2; i < parts.len; i++ {
		next := '__str_plus_acc_${tmp}_${i}'
		g.write(' string ${next} = string__plus(${previous}, __str_plus_part_${tmp}_${i});')
		g.write(' string__free(&${previous});')
		previous = next
	}
	g.write(' ${previous}; })')
}

fn (g &FlatGen) is_runtime_assignable(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	return match node.kind {
		.string_literal, .string_interp {
			true
		}
		.call {
			g.is_runtime_assignable_call(&node)
		}
		.ident {
			true
		}
		.or_expr {
			true
		}
		.infix {
			if node.children_count >= 2 {
				lhs_type := g.tc.resolve_type(g.a.child(&node, 0))
				rhs_type := g.tc.resolve_type(g.a.child(&node, 1))
				lhs_type is types.String || rhs_type is types.String
			} else {
				false
			}
		}
		.cast_expr, .prefix, .struct_init, .map_init {
			true
		}
		else {
			false
		}
	}
}

fn (g &FlatGen) is_runtime_assignable_call(node &flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee_id := g.a.child(node, 0)
	if int(callee_id) < 0 {
		return false
	}
	callee := g.a.nodes[int(callee_id)]
	return callee.kind == .ident || callee.kind == .selector
}

// gen_small_int_arith_operand_truncated emits a comparison operand wrapped in
// a cast to its own sub-int type when it is an arithmetic expression whose C
// evaluation would be integer-promoted (u8/u16/i8/i16). Returns false when the
// operand does not need truncation (caller emits it normally).
fn (mut g FlatGen) gen_small_int_arith_operand_truncated(id flat.NodeId, node flat.Node, typ types.Type) bool {
	mut inner := node
	for inner.kind == .paren && inner.children_count > 0 {
		inner = g.a.nodes[int(g.a.child(&inner, 0))]
	}
	if inner.kind != .infix {
		return false
	}
	if inner.op !in [.plus, .minus, .mul, .left_shift] {
		return false
	}
	mut ct := g.value_c_type(typ)
	if ct !in ['u8', 'u16', 'i8', 'i16'] {
		// The annotated infix type is often widened; fall back to the operand
		// types (`u8 + u8` must wrap at 8 bits even if annotated as int).
		if inner.children_count < 2 {
			return false
		}
		lct := g.value_c_type(g.usable_expr_type(g.a.child(&inner, 0)))
		if lct !in ['u8', 'u16', 'i8', 'i16'] {
			return false
		}
		rct := g.value_c_type(g.usable_expr_type(g.a.child(&inner, 1)))
		if rct != lct {
			return false
		}
		ct = lct
	}
	g.write('(${ct})(')
	g.gen_expr(id)
	g.write(')')
	return true
}

// shift_needs_64bit_widening reports whether `lhs << rhs` has an int-literal
// lhs and a constant shift count that overflows C's 32-bit `int` arithmetic.
fn (g &FlatGen) shift_needs_64bit_widening(node &flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	lhs := g.a.child_node(node, 0)
	if lhs.kind != .int_literal {
		return false
	}
	rhs_value := g.shift_count_const_value(g.a.child(node, 1), []string{}) or { return false }
	return rhs_value >= 31
}

fn (g &FlatGen) shift_count_const_value(id flat.NodeId, seen []string) ?int {
	if !g.valid_node_id(id) {
		return none
	}
	node := g.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return g.tc.const_int_value(node.value, seen)
		}
		.ident, .selector {
			name := g.const_ref_name_from_node(node)
			if name.len == 0 || name in seen {
				return none
			}
			module_name := g.const_modules[name] or { g.tc.cur_module }
			return g.tc.const_int_value_in_module(name, module_name, seen)
		}
		.paren {
			if node.children_count > 0 {
				return g.shift_count_const_value(g.a.child(&node, 0), seen)
			}
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := g.shift_count_const_value(g.a.child(&node, 0), seen) or { return none }
			return match node.op {
				.plus { value }
				.minus { -value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := g.shift_count_const_value(g.a.child(&node, 0), seen) or { return none }
			right := g.shift_count_const_value(g.a.child(&node, 1), seen) or { return none }
			if node.op in [.div, .mod] && right == 0 {
				return none
			}
			return match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div { left / right }
				.mod { left % right }
				.pipe { left | right }
				.xor { left ^ right }
				.amp { left & right }
				else { none }
			}
		}
		else {}
	}

	return none
}

// gen_prefix_op_operand writes a prefix operator and its operand, adding
// parentheses when the operand is a binary expression: `!(a && b)` — without
// them the `!` would bind to the first operand only.
fn (mut g FlatGen) gen_prefix_op_operand(op flat.Op, child_id flat.NodeId) {
	g.write(g.op_str(op))
	child := g.a.nodes[int(child_id)]
	needs_paren := child.kind in [.infix, .in_expr]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(child_id)
	if needs_paren {
		g.write(')')
	}
}

// gen_unsigned_right_shift emits `>>>`: a logical shift that reinterprets the
// lhs bit pattern as the same-width unsigned type, then casts the result back,
// so sign bits are shifted out instead of extended. Shift counts >= the type
// width (UB in C, wrapped on ARM) yield 0, matching V semantics.
fn (mut g FlatGen) gen_unsigned_right_shift(lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type types.Type) {
	g.gen_unsigned_right_shift_from_text(g.expr_to_string(lhs_id), rhs_id, lhs_type)
}

// gen_unsigned_right_shift_from_text is gen_unsigned_right_shift with the lhs
// already rendered as a C expression (used by `>>>=` to shift through a
// pointer temp so the lvalue is evaluated exactly once).
// unsigned_shift_parts maps an operand's C type to the unsigned counterpart
// used for `>>>` logical shifts and its bit-width text. isize/usize lower to
// ptrdiff_t/size_t in C and are pointer-width, so their unsigned view and bit
// width come from size_t, not a fixed 64.
fn unsigned_shift_parts(ct string) (string, string) {
	return match ct {
		'i8', 'u8' { 'u8', '8' }
		'i16', 'u16' { 'u16', '16' }
		'int', 'i32', 'u32' { 'u32', '32' }
		'i64', 'u64' { 'u64', '64' }
		'isize', 'usize', 'ptrdiff_t', 'size_t' { 'size_t', '(sizeof(size_t) * 8)' }
		else { 'u64', '64' }
	}
}

fn unsigned_shift_unalias_type(typ types.Type) types.Type {
	if typ is types.Alias {
		return unsigned_shift_unalias_type(typ.base_type)
	}
	return typ
}

fn (mut g FlatGen) unsigned_shift_type_parts(typ types.Type) (string, string) {
	return unsigned_shift_parts(g.value_c_type(unsigned_shift_unalias_type(typ)))
}

fn (mut g FlatGen) gen_unsigned_right_shift_from_text(lhs_text string, rhs_id flat.NodeId, lhs_type types.Type) {
	ut, bits := g.unsigned_shift_type_parts(lhs_type)
	// The result stays in the unsigned counterpart: casting back to a signed
	// type would sign-extend under C's integer promotion, so
	// `i8(-1) >>> 0 == u8(255)` would compare -1 against 255. A `>>>=`
	// assignment converts back implicitly when storing into the target.
	lhs_tmp := g.tmp_name()
	rhs_tmp := g.tmp_name()
	g.write('({ ${ut} ${lhs_tmp} = (${ut})(${lhs_text}); u64 ${rhs_tmp} = (u64)(')
	g.gen_expr(rhs_id)
	g.write('); ${rhs_tmp} >= ${bits} ? (${ut})0 : (${ut})(${lhs_tmp} >> ${rhs_tmp}); })')
}

fn (g &FlatGen) op_str(op flat.Op) string {
	return match op {
		.plus { '+' }
		.minus { '-' }
		.mul { '*' }
		.div { '/' }
		.mod { '%' }
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		.amp { '&' }
		.pipe { '|' }
		.xor { '^' }
		.left_shift { '<<' }
		.right_shift { '>>' }
		.right_shift_unsigned { '>>' }
		.logical_and { '&&' }
		.logical_or { '||' }
		.not { '!' }
		.bit_not { '~' }
		.assign { '=' }
		.plus_assign { '+=' }
		.minus_assign { '-=' }
		.mul_assign { '*=' }
		.div_assign { '/=' }
		.mod_assign { '%=' }
		.amp_assign { '&=' }
		.pipe_assign { '|=' }
		.xor_assign { '^=' }
		.left_shift_assign { '<<=' }
		.right_shift_assign { '>>=' }
		.right_shift_unsigned_assign { '>>=' }
		.inc { '++' }
		.dec { '--' }
		.dot { '.' }
		.arrow { '->' }
		.none { '' }
		.gated_index { '' }
	}
}

fn (mut g FlatGen) write(s string) {
	if g.line_start {
		g.write_indent()
	}
	if s.len == 0 {
		if g.indent > 0 {
			g.line_start = false
		}
		return
	}
	g.sb.write_string(s)
	g.line_start = s[s.len - 1] == `\n`
}

fn (mut g FlatGen) writeln(s string) {
	if s.len > 0 {
		if g.line_start {
			g.write_indent()
		}
		g.sb.write_string(s)
	}
	g.sb.write_string('\n')
	g.line_start = true
}

fn (mut g FlatGen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}
