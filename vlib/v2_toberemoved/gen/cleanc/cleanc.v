// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.pref
import v2.types
import os
import strings
import time

pub struct Gen {
mut:
	files                   []ast.File
	flat                    &ast.FlatAst       = unsafe { nil }
	env                     &types.Environment = unsafe { nil }
	pref                    &pref.Preferences  = unsafe { nil }
	sb                      strings.Builder
	indent                  int
	cur_fn_scope            &types.Scope = unsafe { nil }
	cur_fn_name             string
	cur_fn_c_name           string
	cur_fn_ret_type         string
	cur_fn_c_ret_type       string
	expected_init_expr_type string
	cur_module              string
	cur_fn_scope_miss_key   string
	// Per-return drop snapshots for the current fn, populated by the
	// checker. Indexed positionally by `cur_fn_return_index`, which the
	// ReturnStmt handler increments. Reset at every fn entry. Empty for
	// fns the checker didn't see (e.g., plain V without `-d ownership`).
	cur_fn_return_drops [][]types.DropEntry
	cur_fn_return_index int
	// Set true while gen_return_if_branch synthesizes ReturnStmts for
	// `return if cond { x } else { y }`. The synthesized statements go
	// through the same ReturnStmt handler, but they represent the SAME
	// source-level return — emitting drops for them again would consume
	// the next snapshot (belonging to a later source return) and break
	// the parallel counter the checker relies on.
	suppress_return_drop_emit bool
	emitted_types             map[string]bool
	fn_param_is_ptr           map[string][]bool
	fn_param_types            map[string][]string
	fn_return_types           map[string]string
	v_fn_return_types         map[string]string
	runtime_local_types       map[string]string
	runtime_decl_types        map[string]string
	runtime_fn_pointer_types  map[string]types.Type
	cur_fn_returned_idents    map[string]bool
	active_generic_types      map[string]types.Type
	cur_fn_generic_params     map[string]string
	// Comptime $for field iteration state
	comptime_field_var      string // variable name (e.g., 'field')
	comptime_field_name     string // current field name (e.g., 'id')
	comptime_field_type     string // current field C type name
	comptime_field_raw_type types.Type = types.Struct{} // raw types.Type for comptime checks
	comptime_field_attrs    []string // current field attributes
	comptime_field_idx      int      // current field index
	comptime_field_is_embed bool     // current field is embedded
	comptime_continue_label string   // label for continue inside unrolled comptime field loops
	comptime_val_var        string   // the struct variable being decoded (e.g., 'val')
	comptime_val_type       string   // C type of val (e.g., 'Slack')
	// Comptime $for method iteration state
	comptime_method_var           string   // loop variable name (e.g., 'method')
	comptime_method_name          string   // current method name (e.g., 'index')
	comptime_method_attrs         []string // current method attributes
	comptime_method_return_type   ast.Expr = ast.empty_expr // current method return type (AST expr)
	comptime_method_args          []ast.Parameter // current method params (excluding receiver)
	comptime_method_idx           int             // current method index
	comptime_method_receiver_type string          // receiver C type (e.g., 'main__App')
	comptime_method_struct_name   string          // receiver struct V name (e.g., 'App')

	fixed_array_fields          map[string]bool
	fixed_array_field_elem      map[string]string
	fixed_array_globals         map[string]bool
	tuple_aliases               map[string][]string
	struct_field_types          map[string]string
	enum_value_to_enum          map[string]string
	enum_type_fields            map[string]map[string]bool
	array_aliases               map[string]bool
	map_aliases                 map[string]bool
	result_aliases              map[string]bool
	option_aliases              map[string]bool
	alias_base_types            map[string]string
	fn_type_aliases             map[string]bool
	emitted_result_structs      map[string]bool
	emitted_option_structs      map[string]bool
	embedded_field_owner        map[string]string
	collected_fixed_array_types map[string]FixedArrayInfo
	collected_map_types         map[string]MapTypeInfo
	fixed_array_ret_wrappers    map[string]string
	sum_type_variants           map[string][]string
	// Interface method signatures: interface_name -> [(method_name, cast_signature), ...]
	interface_methods                 map[string][]InterfaceMethodInfo
	interface_data_fields             map[string][]InterfaceDataFieldInfo
	interface_decls                   map[string]ast.InterfaceDecl
	emitted_interface_bodies          map[string]bool
	interface_wrapper_specs           map[string]InterfaceWrapperSpec
	needed_interface_wrappers         map[string]bool
	ierror_wrapper_bases              map[string]bool
	needed_ierror_wrapper_bases       map[string]bool
	tmp_counter                       int
	cur_fn_mut_params                 map[string]bool   // names of mut params in current function
	global_var_modules                map[string]string // global var name → module name
	global_var_types                  map[string]string // global var name → C type string
	module_storage_vars               map[string]string // module-prefixed storage symbol -> declaring module
	c_extern_module_storage           map[string]string // module-prefixed C extern symbol -> raw C name
	primitive_type_aliases            map[string]bool   // type names that are aliases for primitive types
	emit_modules                      map[string]bool   // when set, emit consts/globals/fns only for these modules
	type_modules                      map[string]bool   // when set, alias/type helpers may reference only these modules
	emit_files                        map[string]bool   // when set, emit consts/globals/fns only for these source files
	export_const_symbols              bool
	cache_bundle_name                 string
	cached_init_calls                 []string
	exported_const_seen               map[string]bool
	exported_const_symbols            []ExportedConstSymbol
	cur_file_name                     string
	cur_import_modules                map[string]string
	is_module_ident_cache             map[string]bool    // per-function cache for is_module_ident results
	not_local_var_cache               map[string]bool    // per-function negative cache for get_local_var_c_type
	resolved_module_names             map[string]string  // per-function cache for resolve_module_name
	cached_env_scopes                 map[string]voidptr // cache of env_scope results (avoids repeated locking)
	selector_field_type_cache         map[string]string
	selector_field_type_miss          map[string]bool
	struct_field_lookup_cache         map[string]string
	struct_field_lookup_miss          map[string]bool
	struct_type_lookup_cache          map[string]types.Struct
	struct_type_lookup_miss           map[string]bool
	struct_decl_info_cache            map[string]StructDeclInfo
	struct_decl_info_miss             map[string]bool
	flat_struct_decl_exact            map[string]FlatStructDeclInfo
	flat_struct_decl_short_by_mod     map[string]FlatStructDeclInfo
	flat_struct_decl_short            map[string]FlatStructDeclInfo
	flat_struct_decl_indexed          bool
	alias_base_lookup_cache           map[string]string
	alias_base_lookup_miss            map[string]bool
	declared_type_names_all           map[string]bool
	declared_type_names_by_mod        map[string]bool
	emit_file_modules                 map[string]bool
	declared_type_names_in_emit_files map[string]bool
	source_module_names               map[string]bool
	imported_symbols_index            map[string]string // "file_name\x01symbol_name" -> importing module (built once from g.files)
	v_method_return_index             map[string]string // method short name -> unique V return type (or v_method_ret_ambiguous)
	ierror_base_index                 map[string]string // base -> qualified concrete base from the smallest `*__base__msg` fn (built once)

	const_exprs                           map[string]string // const name → C expression string (for inlining)
	const_types                           map[string]string // const name → C type string
	const_c_names                         map[string]string // generated const name → collision-free C symbol name
	runtime_const_targets                 map[string]bool   // module-scoped consts initialized in __v_init_consts_*
	used_fn_keys                          map[string]bool
	force_emit_fn_names                   map[string]bool   // function C names that must be emitted regardless of mark_used
	export_fn_names                       map[string]string // V-qualified name → export name (from @[export:] attribute)
	called_fn_names                       map[string]bool
	called_specialized_names              map[string]string // base generic C name -> called specialized C name
	called_specialized_names_indexed      bool
	declared_fn_names                     map[string]bool // C function names that have a prototype/body head emitted
	should_emit_fn_decl_cache             map[string]bool
	generic_body_scan_cache               map[string]bool
	collect_generic_scan_calls            bool
	generic_call_spec_scan_only           bool
	generic_scan_called_names             map[string]bool
	generic_spec_index                    map[string][]string                // fn_name → matching keys in env.generic_types
	generic_fn_decl_index                 map[string]GenericFnDeclInfo       // generic fn C/base name → source location
	specialized_fn_bases                  map[string]bool                    // base C name with at least one _T_ specialization
	specialized_receiver_methods          map[string]string                  // receiver|method -> single matching specialized method
	specialized_receiver_method_ambiguous map[string]bool                    // receiver|method keys with multiple matches
	specialized_receiver_method_miss      map[string]bool                    // receiver|method keys with no matching specialized method
	specialized_receiver_method_indexed   bool                               // true after existing signature maps have been indexed
	late_generic_specs                    map[string][]map[string]types.Type // additional comptime-discovered specs
	anon_fn_defs                          []string        // lifted anonymous function definitions
	late_struct_defs                      []string        // struct definitions discovered during pass 5 codegen
	pending_late_body_keys                map[string]bool // body_keys in late_struct_defs but not yet flushed to g.sb
	late_generic_str_instances            []string        // c_names of late generic struct instances needing str macro check
	pass5_start_pos                       int             // position in sb where pass 5 starts
	deferred_m_includes                   []string        // Objective-C .m file #include lines deferred until after type definitions
	spawned_fns                           map[string]bool // spawn wrapper names already emitted
	spawn_wrapper_defs                    []string        // spawn wrapper struct + function definitions
	emitted_trampolines                   map[string]bool // bound method trampoline names already emitted
	trampoline_defs                       []string        // bound method trampoline definitions
	// @[live] hot code reloading
	live_fns                []LiveFnInfo                     // @[live] functions detected during code generation
	live_source_file        string                           // source file containing @[live] functions
	test_fn_names           []string                         // test function names collected in Pass 4
	has_main                bool                             // whether a main() function was found in Pass 4
	fn_owner_file           map[string]int                   // fn_key -> first file index (for parallel dedup)
	global_owner_file       map[string]int                   // global_name -> first file index (for parallel dedup)
	generic_struct_bindings map[string]map[string]types.Type // struct_name -> {T: concrete_type}
	// Multi-instantiation support: maps base struct C name (e.g. "json2__Node") to
	// a list of (suffix, bindings) pairs for each distinct concrete instantiation.
	// E.g. [("json2__ValueInfo", {T: ValueInfo}), ("json2__StructFieldInfo", {T: StructFieldInfo})]
	generic_struct_instances   map[string][]GenericStructInstance
	generic_setup_snapshot     GenericSetupSnapshot
	has_generic_setup_snapshot bool
	c_file_fn_keys             map[string]bool // fn_key -> emitted from a .c.v file, so plain .v fallback should be skipped
	c_struct_types             map[string]bool // C struct names declared with `struct C.Name`
	typedef_c_types            map[string]bool // C struct names with @[typedef] attribute (emit without 'struct' prefix)
	blocked_fn_keys            map[string]bool // worker-only fn keys reserved to other pass5 chunks
	cached_vhash               string          // cached git short hash for @VHASH/@VCURRENTHASH
	pass5_worker_id            int
	pass5_file_times           []Pass5FileTime
	// When a large file is split across workers, only the worker that owns its
	// globals takes file-level dedup ownership; the others block the file's fns.
	// During its assigned slice a non-owning worker sets these so gen_fn_decl can
	// bypass blocked_fn_keys for fns owned by exactly explicit_slice_file (the slice
	// it is explicitly responsible for) without unblocking anything reached from
	// another file. See gen_file_range / explicit_slice_emit_allows.
	explicit_slice_active bool
	explicit_slice_file   int
}

struct Pass5FileTime {
	file      string
	ms        i64
	cost      int
	worker_id int
}

struct GenericStructInstance {
	params_key string                // e.g. "json2__ValueInfo" — unique key per instantiation
	bindings   map[string]types.Type // e.g. {T: ValueInfo}
	c_name     string                // full C struct name, e.g. "json2__Node_T_json2__StructFieldInfo"
}

pub struct GenericSetupSnapshot {
mut:
	ready                       bool
	tuple_aliases               map[string][]string
	array_aliases               map[string]bool
	map_aliases                 map[string]bool
	result_aliases              map[string]bool
	option_aliases              map[string]bool
	collected_fixed_array_types map[string]FixedArrayInfo
	collected_map_types         map[string]MapTypeInfo
	generic_body_scan_cache     map[string]bool
	generic_spec_index          map[string][]string
	late_generic_specs          map[string][]map[string]types.Type
	generic_struct_bindings     map[string]map[string]types.Type
	generic_struct_instances    map[string][]GenericStructInstance
}

struct LiveFnInfo {
	c_name   string // C function name (e.g., 'main__frame', 'Game__update_model')
	ret_type string // C return type (e.g., 'void', 'string')
	params   string // C parameter list (e.g., 'Game* game')
	args     string // argument names for forwarding (e.g., 'game')
	is_void  bool   // true if return type is void
}

struct ExportedConstSymbol {
	name  string
	typ   string
	value string
}

struct StructDeclInfo {
	decl      ast.StructDecl
	mod       string
	file_name string
}

struct FlatStructDeclInfo {
	file_idx  int
	stmt_idx  int
	mod       string
	file_name string
}

struct InterfaceDeclInfo {
	decl      ast.InterfaceDecl
	mod       string
	file_name string
}

fn (mut g Gen) set_struct_info_context(info StructDeclInfo) {
	g.cur_module = info.mod
	g.cur_file_name = info.file_name
}

fn (mut g Gen) set_interface_info_context(info InterfaceDeclInfo) {
	g.cur_module = info.mod
	g.cur_file_name = info.file_name
}

struct FixedArrayInfo {
	elem_type string
	size      int
}

struct GenericFnSpecialization {
	name          string
	generic_types map[string]types.Type
}

struct GenericFnDeclInfo {
	file_idx int
	stmt_idx int
}

struct MapTypeInfo {
	key_c_type   string
	value_c_type string
}

const primitive_types = ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
	'bool', 'rune', 'byte', 'voidptr', 'charptr', 'usize', 'isize', 'void', 'char', 'byteptr',
	'float_literal', 'int_literal']

fn is_empty_stmt(s ast.Stmt) bool {
	return s is ast.EmptyStmt
}

fn generic_signature_struct_type_name(raw_name string) string {
	mut name := raw_name.trim_space()
	if name.starts_with('&') {
		name = name[1..].trim_space()
	}
	for name.ends_with('*') {
		name = name[..name.len - 1].trim_space()
	}
	if name == '' || !name.contains('_T_') {
		return ''
	}
	if name.contains('(*)') {
		return ''
	}
	if name in primitive_types || name in ['string', 'array', 'map', 'void*', 'char*', 'u8*'] {
		return ''
	}
	if name.starts_with('Array_') || name.starts_with('Map_') || name.starts_with('Tuple_')
		|| name.starts_with('_option_') || name.starts_with('_result_') {
		return ''
	}
	return name
}

fn (mut g Gen) emit_forward_typedef_for_signature_type(raw_name string) bool {
	name := generic_signature_struct_type_name(raw_name)
	if name == '' || name in g.emitted_types {
		return false
	}
	g.emitted_types[name] = true
	g.sb.writeln('typedef struct ${name} ${name};')
	return true
}

fn (mut g Gen) emit_forward_typedefs_for_signature_types() {
	mut emitted_any := false
	mut fn_names := g.fn_return_types.keys()
	for fn_name in g.fn_param_types.keys() {
		if fn_name !in fn_names {
			fn_names << fn_name
		}
	}
	fn_names.sort()
	for fn_name in fn_names {
		if ret_type := g.fn_return_types[fn_name] {
			emitted_any = g.emit_forward_typedef_for_signature_type(ret_type) || emitted_any
		}
		if param_types := g.fn_param_types[fn_name] {
			for param_type in param_types {
				emitted_any = g.emit_forward_typedef_for_signature_type(param_type) || emitted_any
			}
		}
	}
	if emitted_any {
		g.sb.writeln('')
	}
}

fn (mut g Gen) get_v_hash() string {
	if g.cached_vhash.len > 0 {
		return g.cached_vhash
	}
	vroot := if g.pref != unsafe { nil } && g.pref.vroot.len > 0 {
		g.pref.vroot
	} else {
		''
	}
	if vroot.len > 0 {
		result := os.execute('git -C "${vroot}" rev-parse --short=7 HEAD')
		if result.exit_code == 0 {
			g.cached_vhash = result.output.trim_space()
			return g.cached_vhash
		}
	}
	g.cached_vhash = 'unknown'
	return g.cached_vhash
}

fn stmt_has_valid_data(stmt ast.Stmt) bool {
	if stmt is ast.ReturnStmt {
		return true
	}
	return unsafe { (&u64(&stmt))[1] } != 0
}

fn expr_has_valid_data(expr ast.Expr) bool {
	return unsafe { (&u64(&expr))[1] } != 0
}

fn type_has_valid_data(typ types.Type) bool {
	return unsafe { (&u64(&typ))[1] } != 0
}

fn count_substr_in_string(s string, needle string) int {
	if needle.len == 0 {
		return 0
	}
	mut count := 0
	mut i := 0
	for i <= s.len - needle.len {
		if s[i..i + needle.len] == needle {
			count++
			i += needle.len
		} else {
			i++
		}
	}
	return count
}

pub fn Gen.new(files []ast.File) &Gen {
	mut g := new_gen_with_env_and_pref_impl(unsafe { nil }, unsafe { nil })
	g.files = files
	return g
}

pub fn Gen.new_with_env(files []ast.File, env &types.Environment) &Gen {
	mut g := new_gen_with_env_and_pref_impl(env, unsafe { nil })
	g.files = files
	return g
}

pub fn Gen.new_with_env_and_pref(files []ast.File, env &types.Environment, p &pref.Preferences) &Gen {
	mut g := new_gen_with_env_and_pref_impl(env, p)
	g.files = files
	return g
}

pub fn Gen.new_with_env_pref_and_flat(flat &ast.FlatAst, env &types.Environment, p &pref.Preferences) &Gen {
	mut g := new_gen_with_env_and_pref_impl(env, p)
	g.flat = unsafe { flat }
	return g
}

fn new_gen_with_env_and_pref_impl(env &types.Environment, p &pref.Preferences) &Gen {
	return &Gen{
		files:                             []ast.File{}
		env:                               unsafe { env }
		pref:                              unsafe { p }
		sb:                                strings.new_builder(10_000)
		fn_param_is_ptr:                   map[string][]bool{}
		fn_param_types:                    map[string][]string{}
		fn_return_types:                   map[string]string{}
		v_fn_return_types:                 map[string]string{}
		runtime_local_types:               map[string]string{}
		runtime_decl_types:                map[string]string{}
		runtime_fn_pointer_types:          map[string]types.Type{}
		cur_fn_returned_idents:            map[string]bool{}
		active_generic_types:              map[string]types.Type{}
		cur_fn_generic_params:             map[string]string{}
		cur_fn_scope_miss_key:             ''
		selector_field_type_cache:         map[string]string{}
		selector_field_type_miss:          map[string]bool{}
		struct_field_lookup_cache:         map[string]string{}
		struct_field_lookup_miss:          map[string]bool{}
		struct_type_lookup_cache:          map[string]types.Struct{}
		struct_type_lookup_miss:           map[string]bool{}
		struct_decl_info_cache:            map[string]StructDeclInfo{}
		struct_decl_info_miss:             map[string]bool{}
		flat_struct_decl_exact:            map[string]FlatStructDeclInfo{}
		flat_struct_decl_short_by_mod:     map[string]FlatStructDeclInfo{}
		flat_struct_decl_short:            map[string]FlatStructDeclInfo{}
		flat_struct_decl_indexed:          false
		alias_base_lookup_cache:           map[string]string{}
		alias_base_lookup_miss:            map[string]bool{}
		declared_type_names_all:           map[string]bool{}
		declared_type_names_by_mod:        map[string]bool{}
		emit_file_modules:                 map[string]bool{}
		declared_type_names_in_emit_files: map[string]bool{}
		cur_import_modules:                map[string]string{}

		fixed_array_fields:                    map[string]bool{}
		fixed_array_field_elem:                map[string]string{}
		fixed_array_globals:                   map[string]bool{}
		tuple_aliases:                         map[string][]string{}
		struct_field_types:                    map[string]string{}
		enum_value_to_enum:                    map[string]string{}
		enum_type_fields:                      map[string]map[string]bool{}
		array_aliases:                         map[string]bool{}
		map_aliases:                           map[string]bool{}
		result_aliases:                        map[string]bool{}
		option_aliases:                        map[string]bool{}
		alias_base_types:                      map[string]string{}
		fn_type_aliases:                       map[string]bool{}
		emitted_result_structs:                map[string]bool{}
		emitted_option_structs:                map[string]bool{}
		embedded_field_owner:                  map[string]string{}
		fixed_array_ret_wrappers:              map[string]string{}
		emit_modules:                          map[string]bool{}
		type_modules:                          map[string]bool{}
		source_module_names:                   map[string]bool{}
		imported_symbols_index:                map[string]string{}
		v_method_return_index:                 map[string]string{}
		ierror_base_index:                     map[string]string{}
		exported_const_seen:                   map[string]bool{}
		exported_const_symbols:                []ExportedConstSymbol{}
		emitted_interface_bodies:              map[string]bool{}
		interface_data_fields:                 map[string][]InterfaceDataFieldInfo{}
		interface_wrapper_specs:               map[string]InterfaceWrapperSpec{}
		needed_interface_wrappers:             map[string]bool{}
		ierror_wrapper_bases:                  map[string]bool{}
		needed_ierror_wrapper_bases:           map[string]bool{}
		c_file_fn_keys:                        map[string]bool{}
		module_storage_vars:                   map[string]string{}
		c_extern_module_storage:               map[string]string{}
		runtime_const_targets:                 map[string]bool{}
		const_c_names:                         map[string]string{}
		used_fn_keys:                          map[string]bool{}
		force_emit_fn_names:                   map[string]bool{}
		called_fn_names:                       map[string]bool{}
		called_specialized_names:              map[string]string{}
		called_specialized_names_indexed:      false
		declared_fn_names:                     map[string]bool{}
		should_emit_fn_decl_cache:             map[string]bool{}
		generic_body_scan_cache:               map[string]bool{}
		generic_scan_called_names:             map[string]bool{}
		generic_fn_decl_index:                 map[string]GenericFnDeclInfo{}
		specialized_fn_bases:                  map[string]bool{}
		specialized_receiver_methods:          map[string]string{}
		specialized_receiver_method_ambiguous: map[string]bool{}
		specialized_receiver_method_miss:      map[string]bool{}
		specialized_receiver_method_indexed:   false
		c_struct_types:                        map[string]bool{}
		typedef_c_types:                       map[string]bool{}
		blocked_fn_keys:                       map[string]bool{}
	}
}

fn (g &Gen) has_flat() bool {
	return g.flat != unsafe { nil } && g.flat.files.len > 0
}

fn (mut g Gen) gen_file(file ast.File) {
	g.set_file_module(file)
	file_name := g.cur_file_name
	file_module := g.cur_module
	file_import_modules := g.cur_import_modules.clone()
	mut global_indices := []int{}
	mut fn_indices := []int{}
	for i in 0 .. file.stmts.len {
		stmt_ptr := &file.stmts[i]
		// Collect top-level items first. Self-hosted cleanc can disturb the active
		// stmt iteration state after the first emitted body in a file, so discovery
		// and emission need to be split.
		if (*stmt_ptr) is ast.GlobalDecl {
			global_indices << i
			continue
		}
		if (*stmt_ptr) is ast.FnDecl {
			fn_indices << i
		}
	}
	for gi in global_indices {
		stmt_ptr := &file.stmts[gi]
		g.gen_global_decl((*stmt_ptr) as ast.GlobalDecl)
	}
	for fi in fn_indices {
		// Re-set file/module context before each function body emission,
		// because body generation can modify g.cur_file_name and g.cur_module
		// (e.g. via find_generic_fn_decl_by_base_name, resolve_method_on_embedded_decl).
		g.restore_file_module_context(file_name, file_module, file_import_modules)
		stmt_ptr := &file.stmts[fi]
		fn_decl := (*stmt_ptr) as ast.FnDecl
		g.gen_fn_decl_ptr(&fn_decl)
	}
}

// gen_file_range emits only the FnDecls at the given statement indices of `file`
// (and, when emit_globals is set, the file's GlobalDecls). Parallel Pass 5 uses
// this to split a single large file's functions across several workers so one
// huge file (e.g. ssa/builder.v) cannot pin the whole parallel phase. Each
// function is still emitted by exactly one worker (the index ranges partition
// the file's FnDecls), and only the first range emits globals. Globals are
// forward-declared for every file in gen_pass5_pre (gen_file_extern_globals),
// so the single definition's position within the merged output is irrelevant.
// explicit_slice_emit_allows reports whether a fn that is in blocked_fn_keys may
// still be emitted because the worker is currently emitting its explicit FnDecl
// slice of the file that owns this fn. The bypass is scoped to the slice's own
// file (explicit_slice_file) so it restores exactly the fns the slice would have
// emitted under file-level ownership and nothing transitively reached from another
// file (which remains blocked and is emitted by its own owning worker).
fn (g &Gen) explicit_slice_emit_allows(fn_key string) bool {
	if !g.explicit_slice_active {
		return false
	}
	if owner := g.fn_owner_file[fn_key] {
		return owner == g.explicit_slice_file
	}
	return false
}

fn (mut g Gen) gen_file_range(file ast.File, fn_stmt_indices []int, emit_globals bool) {
	g.set_file_module(file)
	file_name := g.cur_file_name
	file_module := g.cur_module
	file_import_modules := g.cur_import_modules.clone()
	if emit_globals {
		for i in 0 .. file.stmts.len {
			stmt_ptr := &file.stmts[i]
			if (*stmt_ptr) is ast.GlobalDecl {
				g.gen_global_decl((*stmt_ptr) as ast.GlobalDecl)
			}
		}
	}
	for fi in fn_stmt_indices {
		g.restore_file_module_context(file_name, file_module, file_import_modules)
		stmt_ptr := &file.stmts[fi]
		fn_decl := (*stmt_ptr) as ast.FnDecl
		g.gen_fn_decl_ptr(&fn_decl)
	}
}

fn (mut g Gen) gen_file_cursor(fc ast.FileCursor) {
	g.set_file_cursor_module(fc)
	file_name := g.cur_file_name
	file_module := g.cur_module
	file_import_modules := g.cur_import_modules.clone()
	stmts := fc.stmts()
	mut global_indices := []int{}
	mut fn_indices := []int{}
	for i in 0 .. stmts.len() {
		stmt := stmts.at(i)
		match stmt.kind() {
			.stmt_global_decl {
				global_indices << i
			}
			.stmt_fn_decl {
				fn_indices << i
			}
			else {}
		}
	}
	for gi in global_indices {
		g.gen_global_decl(stmts.at(gi).global_decl(true))
	}
	for fi in fn_indices {
		g.restore_file_module_context(file_name, file_module, file_import_modules)
		fn_decl := stmts.at(fi).fn_decl()
		g.gen_fn_decl_ptr(&fn_decl)
	}
}

fn (mut g Gen) gen_file_cursor_range(fc ast.FileCursor, fn_stmt_indices []int, emit_globals bool) {
	g.set_file_cursor_module(fc)
	file_name := g.cur_file_name
	file_module := g.cur_module
	file_import_modules := g.cur_import_modules.clone()
	stmts := fc.stmts()
	if emit_globals {
		for i in 0 .. stmts.len() {
			stmt := stmts.at(i)
			if stmt.kind() == .stmt_global_decl {
				g.gen_global_decl(stmt.global_decl(true))
			}
		}
	}
	for fi in fn_stmt_indices {
		g.restore_file_module_context(file_name, file_module, file_import_modules)
		fn_decl := stmts.at(fi).fn_decl()
		g.gen_fn_decl_ptr(&fn_decl)
	}
}

// set_emit_modules limits code emission to the provided module names.
// Type declarations and forward declarations are still emitted for all modules.
pub fn (mut g Gen) set_emit_modules(modules []string) {
	g.emit_modules = map[string]bool{}
	for module_name in modules {
		if module_name != '' {
			g.emit_modules[module_name] = true
		}
	}
}

pub fn (mut g Gen) set_type_modules(modules []string) {
	g.type_modules = map[string]bool{}
	for module_name in modules {
		if module_name != '' {
			g.type_modules[module_name] = true
		}
	}
}

// set_emit_files limits body/const/global emission to the provided source files.
// Type declarations and forward declarations are still emitted for all files.
pub fn (mut g Gen) set_emit_files(files []string) {
	g.emit_files = map[string]bool{}
	for file in files {
		if file != '' {
			g.emit_files[os.norm_path(file)] = true
			g.emit_files[os.norm_path(os.abs_path(file))] = true
		}
	}
	g.collect_emit_file_indexes()
}

fn (mut g Gen) collect_source_module_names() {
	g.source_module_names = map[string]bool{}
	// Index `import mod { sym }` selective imports per file so imported_symbol_c_type is an O(1)
	// lookup instead of rescanning all files' imports/symbols (it was the hottest codegen fn).
	mut imp_idx := map[string]string{}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.source_module_names[flat_file_module_name(fc)] = true
			imports := fc.imports()
			for j in 0 .. imports.len() {
				import_stmt := imports.at(j).import_stmt()
				if import_stmt.symbols.len == 0 {
					continue
				}
				mod_name := import_stmt.name.all_after_last('.').replace('.', '_')
				for symbol in import_stmt.symbols {
					sn := symbol.name()
					if sn == '' {
						continue
					}
					key := '${fc.name()}\x01${sn}'
					if key !in imp_idx { // first occurrence wins, matching the original scan order
						imp_idx[key] = mod_name
					}
				}
			}
		}
		g.imported_symbols_index = imp_idx.move()
		return
	}
	for file in g.files {
		g.source_module_names[file_module_name(file)] = true
		for import_stmt in file.imports {
			if import_stmt.symbols.len == 0 {
				continue
			}
			mod_name := import_stmt.name.all_after_last('.').replace('.', '_')
			for symbol in import_stmt.symbols {
				sn := symbol.name()
				if sn == '' {
					continue
				}
				key := '${file.name}\x01${sn}'
				if key !in imp_idx { // first occurrence wins, matching the original scan order
					imp_idx[key] = mod_name
				}
			}
		}
	}
	g.imported_symbols_index = imp_idx.move()
}

fn (mut g Gen) collect_emit_file_indexes() {
	g.emit_file_modules = map[string]bool{}
	g.declared_type_names_in_emit_files = map[string]bool{}
	if g.emit_files.len == 0 {
		return
	}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			file_name := fc.name()
			if !(os.norm_path(file_name) in g.emit_files
				|| os.norm_path(os.abs_path(file_name)) in g.emit_files) {
				continue
			}
			module_name := flat_file_module_name(fc)
			g.emit_file_modules[module_name] = true
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_struct_decl, .stmt_enum_decl, .stmt_interface_decl, .stmt_type_decl {
						g.record_emit_file_type_name(stmt.name(), module_name)
					}
					else {}
				}
			}
		}
		return
	}
	for file in g.files {
		if !(os.norm_path(file.name) in g.emit_files
			|| os.norm_path(os.abs_path(file.name)) in g.emit_files) {
			continue
		}
		module_name := file_module_name(file)
		g.emit_file_modules[module_name] = true
		for stmt in file.stmts {
			match stmt {
				ast.StructDecl {
					g.record_emit_file_type_name(stmt.name, module_name)
				}
				ast.EnumDecl {
					g.record_emit_file_type_name(stmt.name, module_name)
				}
				ast.InterfaceDecl {
					g.record_emit_file_type_name(stmt.name, module_name)
				}
				ast.TypeDecl {
					g.record_emit_file_type_name(stmt.name, module_name)
				}
				else {}
			}
		}
	}
}

fn flat_file_module_name(fc ast.FileCursor) string {
	mod_name := fc.mod()
	if mod_name != '' {
		return mod_name.replace('.', '_')
	}
	return 'main'
}

fn (mut g Gen) record_emit_file_type_name(decl_name string, module_name string) {
	if decl_name == '' {
		return
	}
	alias_name := type_decl_name_in_module(decl_name, module_name)
	g.declared_type_names_in_emit_files[alias_name] = true
	if (module_name == '' || module_name == 'main' || module_name == 'builtin')
		&& decl_name != alias_name {
		g.declared_type_names_in_emit_files[decl_name] = true
	}
}

fn (mut g Gen) collect_force_emit_sort_fns() {
	if g.emit_files.len == 0 || g.cache_bundle_name.len > 0 {
		return
	}
	old_file := g.cur_file_name
	old_module := g.cur_module
	old_import_modules := g.cur_import_modules.clone()
	mut changed := false
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			if g.cur_module != 'main' {
				continue
			}
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_fn_decl || !stmt.name().starts_with('__sort_cmp_') {
					continue
				}
				decl := stmt.fn_decl_signature()
				fn_name := g.get_fn_name(decl)
				if fn_name != '' {
					if fn_name !in g.force_emit_fn_names {
						changed = true
					}
					g.force_emit_fn_names[fn_name] = true
				}
			}
		}
	} else {
		for file in g.files {
			g.set_file_module(file)
			if g.cur_module != 'main' {
				continue
			}
			for stmt in file.stmts {
				if stmt is ast.FnDecl && stmt.name.starts_with('__sort_cmp_') {
					fn_name := g.get_fn_name(stmt)
					if fn_name != '' {
						if fn_name !in g.force_emit_fn_names {
							changed = true
						}
						g.force_emit_fn_names[fn_name] = true
					}
				}
			}
		}
	}
	if changed {
		g.should_emit_fn_decl_cache = map[string]bool{}
	}
	g.cur_file_name = old_file
	g.cur_module = old_module
	g.cur_import_modules = old_import_modules.clone()
}

// set_cached_init_calls sets cache-init functions to invoke from generated main().

// set_used_fn_keys limits function emission to declaration keys marked as used.

fn is_builtin_map_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/map.v')
}

fn is_builtin_string_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/string.v')
}

fn (mut g Gen) emit_live_reload_infrastructure() {
	// Don't emit live infrastructure in shared library mode (only impl_live_ bodies needed)
	if g.pref != unsafe { nil } && g.pref.is_shared_lib {
		return
	}
	// Skip in cached module sources — __v_live_init belongs only in the main module.
	if g.cache_bundle_name.len > 0 {
		return
	}
	if g.live_fns.len == 0 {
		if g.has_live_reload_functions() {
			g.sb.writeln('')
			g.sb.writeln('void __v_live_init(void) {}')
		}
		return
	}

	g.sb.writeln('')
	g.sb.writeln('// ===== @[live] hot code reloading infrastructure =====')
	g.sb.writeln('#include <dlfcn.h>')
	g.sb.writeln('')

	// Mutex for thread-safe function pointer updates
	g.sb.writeln('static pthread_mutex_t __live_fn_mutex = PTHREAD_MUTEX_INITIALIZER;')
	g.sb.writeln('')

	// Function pointer globals (initialized to impl_live_ versions)
	for lf in g.live_fns {
		if lf.params.len > 0 {
			g.sb.writeln('static ${lf.ret_type} (*__live_ptr_${lf.c_name})(${lf.params}) = &impl_live_${lf.c_name};')
		} else {
			g.sb.writeln('static ${lf.ret_type} (*__live_ptr_${lf.c_name})(void) = &impl_live_${lf.c_name};')
		}
	}
	g.sb.writeln('')

	// Wrapper functions that call through pointers with mutex protection
	for lf in g.live_fns {
		params := if lf.params.len > 0 { lf.params } else { 'void' }
		g.sb.writeln('${lf.ret_type} ${lf.c_name}(${params}) {')
		g.sb.writeln('\tpthread_mutex_lock(&__live_fn_mutex);')
		if lf.is_void {
			if lf.args.len > 0 {
				g.sb.writeln('\t__live_ptr_${lf.c_name}(${lf.args});')
			} else {
				g.sb.writeln('\t__live_ptr_${lf.c_name}();')
			}
		} else {
			if lf.args.len > 0 {
				g.sb.writeln('\t${lf.ret_type} __live_ret = __live_ptr_${lf.c_name}(${lf.args});')
			} else {
				g.sb.writeln('\t${lf.ret_type} __live_ret = __live_ptr_${lf.c_name}();')
			}
		}
		g.sb.writeln('\tpthread_mutex_unlock(&__live_fn_mutex);')
		if !lf.is_void {
			g.sb.writeln('\treturn __live_ret;')
		}
		g.sb.writeln('}')
		g.sb.writeln('')
	}

	// v_bind_live_symbols: called after dlopen to update function pointers
	g.sb.writeln('static void __v_bind_live_symbols(void* __live_lib) {')
	for lf in g.live_fns {
		g.sb.writeln('\tvoid* __sym_${lf.c_name} = dlsym(__live_lib, "impl_live_${lf.c_name}");')
		g.sb.writeln('\tif (__sym_${lf.c_name}) {')
		g.sb.writeln('\t\t__live_ptr_${lf.c_name} = __sym_${lf.c_name};')
		g.sb.writeln('\t}')
	}
	g.sb.writeln('}')
	g.sb.writeln('')

	// Background reloader thread
	source_file := g.live_source_file
	dylib_path := '/tmp/_v2_live_reload.dylib'
	// c_path := '/tmp/_v2_live_reload.c'
	// Resolve absolute path to v2 binary at compile time
	v2_path := if g.pref != unsafe { nil } && g.pref.vroot.len > 0 {
		g.pref.vroot + '/cmd/v2/v2'
	} else {
		'./v2'
	}
	g.sb.writeln('static void* __v_live_reloader_thread(void* __arg) {')
	g.sb.writeln('\t(void)__arg;')
	g.sb.writeln('\tchar* __live_src = "${source_file}";')
	g.sb.writeln('\tlong long __live_last_mtime = 0;')
	g.sb.writeln('\tstruct stat __live_st;')
	g.sb.writeln('\t// Get initial mtime')
	g.sb.writeln('\tif (stat(__live_src, &__live_st) == 0) {')
	g.sb.writeln('\t\t__live_last_mtime = __live_st.st_mtimespec.tv_sec;')
	g.sb.writeln('\t}')
	g.sb.writeln('\tint __live_reload_count = 0;')
	g.sb.writeln('\twhile (1) {')
	g.sb.writeln('\t\tusleep(250000); // 250ms')
	g.sb.writeln('\t\tif (stat(__live_src, &__live_st) != 0) continue;')
	g.sb.writeln('\t\tlong long __live_new_mtime = __live_st.st_mtimespec.tv_sec;')
	g.sb.writeln('\t\tif (__live_new_mtime <= __live_last_mtime) continue;')
	g.sb.writeln('\t\t__live_last_mtime = __live_new_mtime;')
	g.sb.writeln('\t\tprintf("[live] source changed, recompiling...\\n");')
	g.sb.writeln('\t\tfflush(stdout);')

	// Recompile V source directly to shared library
	g.sb.writeln('\t\tint __live_rc = system("${v2_path} -backend cleanc -gc none -nocache -shared -o ${dylib_path} ${source_file} 2>/dev/null");')
	g.sb.writeln('\t\tif (__live_rc != 0) {')
	g.sb.writeln('\t\t\tprintf("[live] recompilation failed (exit %d)\\n", __live_rc);')
	g.sb.writeln('\t\t\tfflush(stdout);')
	g.sb.writeln('\t\t\tcontinue;')
	g.sb.writeln('\t\t}')

	// Step 3: dlopen and rebind
	g.sb.writeln('\t\tvoid* __live_lib = dlopen("${dylib_path}", RTLD_NOW | RTLD_LOCAL);')
	g.sb.writeln('\t\tif (!__live_lib) {')
	g.sb.writeln('\t\t\tprintf("[live] dlopen failed: %s\\n", dlerror());')
	g.sb.writeln('\t\t\tfflush(stdout);')
	g.sb.writeln('\t\t\tcontinue;')
	g.sb.writeln('\t\t}')
	g.sb.writeln('\t\tpthread_mutex_lock(&__live_fn_mutex);')
	g.sb.writeln('\t\t__v_bind_live_symbols(__live_lib);')
	g.sb.writeln('\t\tpthread_mutex_unlock(&__live_fn_mutex);')
	g.sb.writeln('\t\t__live_reload_count++;')
	g.sb.writeln('\t\tprintf("[live] reload #%d complete\\n", __live_reload_count);')
	g.sb.writeln('\t\tfflush(stdout);')
	g.sb.writeln('\t}')
	g.sb.writeln('\treturn 0;')
	g.sb.writeln('}')
	g.sb.writeln('')

	// __v_live_init: called from main() to start the reloader thread
	g.sb.writeln('void __v_live_init(void) {')
	g.sb.writeln('\tpthread_t __live_thread;')
	g.sb.writeln('\tpthread_create(&__live_thread, 0, __v_live_reloader_thread, 0);')
	g.sb.writeln('\tpthread_detach(__live_thread);')
	g.sb.writeln('}')
	g.sb.writeln('')
	g.sb.writeln('// ===== end @[live] infrastructure =====')
	g.sb.writeln('')
}

fn should_keep_builtin_map_decl(decl ast.FnDecl) bool {
	base_keep := decl.name in ['new_map', 'move', 'clear', 'key_to_index', 'meta_less',
		'meta_greater', 'ensure_extra_metas', 'ensure_extra_metas_grow', 'set', 'expand', 'rehash',
		'reserve', 'cached_rehash', 'get_and_set', 'get', 'get_check', 'exists', 'delete', 'keys',
		'values', 'clone', 'free', 'key', 'value', 'has_index', 'zeros_to_end', 'new_dense_array']
	return base_keep || decl.name.starts_with('map_eq_') || decl.name.starts_with('map_clone_')
		|| decl.name.starts_with('map_free_')
}

fn should_keep_builtin_string_decl(decl ast.FnDecl) bool {
	return decl.name in ['eq', 'plus', 'plus_two', 'substr', 'substr_unsafe', 'repeat', 'free',
		'vstring', 'vstring_with_len', 'vstring_literal', 'vstring_literal_with_len', 'runes',
		'join', 'compare_strings', 'compare_strings_by_len', 'compare_lower_strings']
}

fn should_always_emit_for_markused(path string) bool {
	if path.ends_with('.vh') {
		return true
	}
	return is_builtin_runtime_keep_file(path)
}

fn is_builtin_runtime_keep_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/map.c.v')
		|| normalized.ends_with('vlib/builtin/builtin.c.v')
		|| normalized.ends_with('vlib/builtin/builtin.v')
		|| normalized.ends_with('vlib/builtin/cfns_wrapper.c.v')
		|| normalized.ends_with('vlib/builtin/allocation.c.v')
		|| normalized.ends_with('vlib/builtin/prealloc.c.v')
		|| normalized.ends_with('vlib/builtin/panicing.c.v')
		|| normalized.ends_with('vlib/builtin/chan_option_result.v')
		|| normalized.ends_with('vlib/builtin/int.v') || normalized.ends_with('vlib/builtin/rune.v')
		|| normalized.ends_with('vlib/builtin/float.c.v')
		|| normalized.ends_with('vlib/builtin/utf8.v')
		|| normalized.ends_with('vlib/builtin/utf8.c.v')
		|| normalized.ends_with('vlib/strings/builder.c.v')
		|| normalized.ends_with('vlib/strconv/utilities.v')
		|| normalized.ends_with('vlib/strconv/utilities.c.v')
		|| normalized.ends_with('vlib/strconv/ftoa.c.v')
		|| normalized.ends_with('vlib/strconv/f32_str.c.v')
		|| normalized.ends_with('vlib/strconv/f64_str.c.v')
		|| normalized.ends_with('vlib/math/bits/bits.v')
		|| normalized.ends_with('vlib/math/bits/bits.c.v')
		|| normalized.ends_with('vlib/sokol/memory/memory.c.v')
}

fn (g &Gen) should_emit_module(module_name string) bool {
	if g.emit_modules.len == 0 {
		return true
	}
	return module_name in g.emit_modules
}

fn (g &Gen) should_emit_current_file() bool {
	if !g.should_emit_module(g.cur_module) {
		return false
	}
	if g.emit_files.len == 0 {
		return true
	}
	return os.norm_path(g.cur_file_name) in g.emit_files
		|| os.norm_path(os.abs_path(g.cur_file_name)) in g.emit_files
}

fn (g &Gen) cgen_stats_enabled() bool {
	return g.pref != unsafe { nil } && g.pref.stats
}

fn (g &Gen) cgen_stats_scope_label() string {
	if g.cache_bundle_name.len > 0 {
		return 'cache:${g.cache_bundle_name}'
	}
	if g.emit_modules.len == 0 {
		return 'full'
	}
	if g.emit_files.len > 0 {
		return 'files:${g.emit_files.len}'
	}
	if g.emit_modules.len == 1 && 'main' in g.emit_modules {
		return 'main'
	}
	return 'modules:${g.emit_modules.len}'
}

fn (g &Gen) print_cgen_step_time(stats_enabled bool, scope string, step string, elapsed time.Duration) {
	if !stats_enabled {
		return
	}
	println('   - C Gen/${scope} ${step}: ${elapsed.milliseconds()}ms')
}

fn (g &Gen) mark_cgen_step(stats_enabled bool, scope string, mut sw time.StopWatch, stage_start time.Duration, step string) time.Duration {
	if !stats_enabled {
		g.print_cgen_mem(step)
		return stage_start
	}
	now := sw.elapsed()
	g.print_cgen_step_time(true, scope, step, time.Duration(now - stage_start))
	g.print_cgen_mem(step)
	return now
}

// gen generates C source from the transformed AST files (sequential).
pub fn (mut g Gen) gen() string {
	g.gen_passes_1_to_4()
	g.gen_pass5()
	return g.gen_finalize()
}

// gen_passes_1_to_4 runs setup and passes 1-4 (types, structs, constants, forward declarations).
pub fn (mut g Gen) gen_passes_1_to_4() {
	stats_enabled := g.cgen_stats_enabled()
	stats_scope := g.cgen_stats_scope_label()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()

	g.write_preamble()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.preamble')
	g.collect_source_module_names()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.source_module_names')
	g.collect_typedef_c_types()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.typedef_c_types')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.generic_fn_decl_index')
	if g.has_generic_setup_snapshot {
		g.apply_generic_setup_snapshot()
	}
	g.collect_generic_struct_bindings()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.generic_struct_bindings')
	g.collect_module_type_names()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.module_type_names')
	g.collect_emit_file_indexes()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.emit_file_indexes')
	g.collect_runtime_aliases()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.runtime_aliases')
	// V2 does not fully monomorphize generic structs yet, so the unsuffixed
	// stdatomic.AtomicVal body has to use one concrete storage type. Keep it on
	// `int`: the generated stdatomic receiver methods are also pinned to `int`,
	// which keeps atomic counters on a supported add/sub path while bool flags
	// still store/load through C's scalar conversions.
	g.generic_struct_bindings['stdatomic__AtomicVal'] = {
		'T': types.Type(types.int_)
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.discover_generic_specs')
	g.collect_force_emit_sort_fns()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.force_emit_sort_fns')
	g.collect_fn_signatures_to_fixed_point()
	g.build_v_method_return_index()
	g.build_ierror_base_index()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.fn_signatures')
	g.collect_c_file_fn_keys()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.c_file_fn_keys')
	g.collect_runtime_const_targets()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.runtime_const_targets')
	g.register_builder_methods()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.register_builder_methods')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start, 'setup')

	g.collect_global_storage_names()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'collect globals')

	// Pass 1: Forward declarations for all structs/unions/sumtypes/interfaces (needed for mutual references)
	g.emit_pass1_forward_decls()
	g.sb.writeln('')
	g.emit_runtime_aliases()
	g.sb.writeln('')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 1 forward decls')

	// Pass 2: Emit type declarations in dependency-safe buckets.
	// Emit enums first, then type aliases/sum types.
	// Interface bodies are emitted later, after structs/tuple aliases are available.
	g.emit_pass2_type_decls()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 2 type declarations')

	// Emit pointer element typedefs (e.g. 'typedef Color* Colorptr;') now that
	// enums and type aliases have been defined.
	g.emit_pointer_typedefs()
	// Fixed arrays over aliases can be emitted once the aliases from pass 2 exist.
	g.emit_deferred_fixed_array_aliases()

	// Pass 3: Full struct definitions (use named struct/union to match forward decls)
	// Collect all struct decls, then emit in dependency order
	all_structs, all_interfaces := g.collect_struct_and_interface_infos()
	// Emit structs with only primitive/resolved fields first, then the rest.
	// Interleave option/result wrapper emission as soon as their payload types are complete.
	// Also emit fixed array typedefs as soon as their element types are defined.
	// Repeat until no more progress (simple topo sort with wrapper side-effects).
	for info in all_structs {
		g.set_struct_info_context(info)
		if g.struct_is_leaf(info.decl) {
			g.gen_struct_decl(info.decl)
		}
	}
	// Emit fixed array typedefs whose element types are now defined (leaf structs).
	g.emit_deferred_fixed_array_aliases()
	for _ in 0 .. (all_structs.len * 2) {
		mut progressed := false
		for info in all_structs {
			g.set_struct_info_context(info)
			name := g.get_struct_name(info.decl)
			body_key := 'body_${name}'
			if body_key in g.emitted_types {
				continue
			}
			// Check if all field types are already defined
			if g.struct_fields_resolved(info.decl) {
				g.gen_struct_decl(info.decl)
				progressed = true
			}
		}
		if g.emit_ready_option_result_structs() {
			progressed = true
		}
		// Emit fixed array typedefs whose element types are now resolved.
		g.emit_deferred_fixed_array_aliases()
		if !progressed {
			break
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3 struct definitions')

	// Pass 3.1: Emit deferred (non-primitive) fixed array typedefs now that struct defs exist
	g.emit_deferred_fixed_array_aliases()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.1 fixed arrays')

	// Pass 3.25: Tuple aliases (multiple-return lowering support)
	g.emit_tuple_aliases()
	if g.tuple_aliases.len > 0 {
		g.sb.writeln('')
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.25 tuple aliases')

	// Pass 3.3: Emit interface bodies after structs and tuple aliases.
	for _ in 0 .. (all_interfaces.len * 2) {
		mut progressed := false
		for info in all_interfaces {
			g.set_interface_info_context(info)
			name := g.get_interface_name(info.decl)
			body_key := 'body_${name}'
			if body_key in g.emitted_types {
				continue
			}
			if g.interface_fields_resolved(info.decl) {
				g.gen_interface_decl(info.decl)
				progressed = true
			}
		}
		if !progressed {
			break
		}
	}
	// Emit any remaining interfaces before retrying structs/wrappers. This keeps
	// option/result wrappers for interface payloads available before by-value users.
	for info in all_interfaces {
		g.set_interface_info_context(info)
		g.gen_interface_decl(info.decl)
	}
	// Retry any structs that were waiting on interface bodies.
	for _ in 0 .. all_structs.len {
		mut progressed := false
		for info in all_structs {
			g.set_struct_info_context(info)
			name := g.get_struct_name(info.decl)
			body_key := 'body_${name}'
			if body_key in g.emitted_types {
				continue
			}
			if g.struct_fields_resolved(info.decl) {
				g.gen_struct_decl(info.decl)
				progressed = true
			}
		}
		if g.emit_ready_option_result_structs() {
			progressed = true
		}
		g.emit_deferred_fixed_array_aliases()
		if !progressed {
			break
		}
	}
	// Emit any remaining structs as a last resort for cyclic declarations.
	for info in all_structs {
		g.set_struct_info_context(info)
		g.gen_struct_decl(info.decl)
	}
	_ = g.emit_ready_option_result_structs()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.3 interfaces')

	// Pass 3.4: Emit option/result struct definitions (needs IError + tuple types defined)
	g.emit_option_result_structs()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.4 option/result structs')

	// Pass 3.45: Retry tuple aliases now that interface bodies and wrapper
	// payload types have been emitted.
	g.emit_tuple_aliases()
	if g.tuple_aliases.len > 0 {
		g.sb.writeln('')
	}
	g.emit_option_result_structs()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.45 late tuple aliases')

	// Pass 3.5: Wrapper structs for functions returning fixed arrays.
	g.emit_fixed_array_return_wrappers()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.5 fixed-array return wrappers')

	// Recursive array equality helper for nested arrays and string arrays.
	// In cached-core builds, the body lives in the builtin cache unit. The
	// prototype is emitted here from structured generator state; single-TU
	// builds also emit the body directly.
	g.sb.writeln('bool string__eq(string a, string b);')
	g.sb.writeln('bool __v2_array_eq(array a, array b);')
	if g.should_emit_module('builtin') {
		g.sb.writeln('bool __v2_array_eq(array a, array b) {')
		g.sb.writeln('	if (a.len != b.len) return false;')
		g.sb.writeln('	if (a.len == 0) return true;')
		g.sb.writeln('	if (a.element_size != b.element_size) return false;')
		g.sb.writeln('	if (a.element_size == sizeof(array)) {')
		g.sb.writeln('		for (int i = 0; i < a.len; i++) {')
		g.sb.writeln('			if (!__v2_array_eq(((array*)a.data)[i], ((array*)b.data)[i])) return false;')
		g.sb.writeln('		}')
		g.sb.writeln('		return true;')
		g.sb.writeln('	}')
		g.sb.writeln('	if (a.element_size == sizeof(string)) {')
		g.sb.writeln('		for (int i = 0; i < a.len; i++) {')
		g.sb.writeln('			if (!string__eq(((string*)a.data)[i], ((string*)b.data)[i])) return false;')
		g.sb.writeln('		}')
		g.sb.writeln('		return true;')
		g.sb.writeln('	}')
		g.sb.writeln('	return memcmp(a.data, b.data, a.len * a.element_size) == 0;')
		g.sb.writeln('}')
	}
	g.sb.writeln('')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.6 array helpers')

	g.emit_forward_typedefs_for_signature_types()

	// Pass 4: Function forward declarations
	g.emit_pass4_fn_forward_decls()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4 fn forward declarations')

	g.sb.writeln('')
	if g.has_live_reload_functions() {
		g.sb.writeln('void __v_live_init(void);')
	}
	g.emit_cached_init_call_decls()
	g.emit_ierror_wrapper_decls()
	g.collect_interface_wrapper_specs()
	g.emit_interface_method_wrapper_decls()
	g.emit_interface_clone_decls()
	g.emit_array_interface_repeat_decls()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4 helper declarations')

	g.emit_enum_from_string_helpers()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4.1 enum from_string')

	// Pass 4.5: Emit constants after function forward declarations so const
	// initializers can reference functions by name (for example function-pointer
	// tables).
	g.emit_pass45_const_decls()
	g.sb.writeln('')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4.5 const declarations')

	// Emit deferred Objective-C .m includes now that all types are defined.
	g.emit_deferred_m_includes()
}

fn (mut g Gen) collect_global_storage_names() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_global_decl {
					continue
				}
				g.collect_global_storage_names_from_decl(stmt.global_decl(false))
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.GlobalDecl {
				g.collect_global_storage_names_from_decl(stmt)
			}
		}
	}
}

fn (mut g Gen) collect_global_storage_names_from_decl(decl ast.GlobalDecl) {
	for field in decl.fields {
		storage_name := module_storage_field_c_name(g.cur_module, decl, field)
		qualified_name := module_storage_c_name(g.cur_module, if field.name.starts_with('C.') {
			field.name.all_after('C.')
		} else {
			field.name
		})
		if module_storage_field_is_c_extern(decl, field) {
			g.c_extern_module_storage[qualified_name] = storage_name
		} else {
			g.module_storage_vars[storage_name] = g.cur_module
		}
		if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			g.global_var_modules[field.name] = g.cur_module
		}
	}
}

fn (mut g Gen) emit_pass1_forward_decls() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_struct_decl {
						decl := stmt.struct_decl()
						if decl.language == .c {
							continue
						}
						g.emit_struct_forward_decl(decl)
					}
					.stmt_type_decl {
						decl := stmt.type_decl()
						if decl.variants.len > 0 {
							g.emit_sum_type_forward_decl(decl)
						}
					}
					.stmt_interface_decl {
						g.emit_interface_forward_decl(stmt.interface_decl())
					}
					else {}
				}
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.StructDecl {
				if stmt.language == .c {
					continue
				}
				g.emit_struct_forward_decl(stmt)
			} else if stmt is ast.TypeDecl {
				if stmt.variants.len > 0 {
					g.emit_sum_type_forward_decl(stmt)
				}
			} else if stmt is ast.InterfaceDecl {
				g.emit_interface_forward_decl(stmt)
			}
		}
	}
}

fn (mut g Gen) emit_struct_forward_decl(decl ast.StructDecl) {
	name := g.get_struct_name(decl)
	runtime_generic_params := if decl.generic_params.len > 0 {
		g.generic_struct_runtime_param_names(name, name)
	} else {
		[]string{}
	}
	if runtime_generic_params.len > 0 && name !in g.generic_struct_bindings
		&& name !in g.generic_struct_instances {
		return
	}
	if name in g.emitted_types {
		return
	}
	g.emitted_types[name] = true
	keyword := if decl.is_union { 'union' } else { 'struct' }
	g.sb.writeln('typedef ${keyword} ${name} ${name};')
	if decl.generic_params.len > 0 {
		instances := g.generic_struct_instances[name]
		for inst in instances {
			if inst.c_name == name {
				continue
			}
			if inst.c_name !in g.emitted_types {
				g.emitted_types[inst.c_name] = true
				g.sb.writeln('typedef ${keyword} ${inst.c_name} ${inst.c_name};')
			}
		}
	}
}

fn (mut g Gen) emit_sum_type_forward_decl(decl ast.TypeDecl) {
	name := g.get_type_decl_name(decl)
	if name !in g.emitted_types {
		g.emitted_types[name] = true
		g.sb.writeln('typedef struct ${name} ${name};')
	}
}

fn (mut g Gen) emit_interface_forward_decl(decl ast.InterfaceDecl) {
	name := g.get_interface_name(decl)
	if name !in g.emitted_types {
		g.emitted_types[name] = true
		g.sb.writeln('typedef struct ${name} ${name};')
	}
}

fn (mut g Gen) emit_pass2_type_decls() {
	if g.has_flat() {
		g.emit_pass2_type_decls_flat()
		return
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.EnumDecl {
				g.gen_enum_decl(stmt)
			}
		}
	}
	g.prescan_type_aliases_and_interfaces()
	g.emit_tuple_aliases()
	if g.tuple_aliases.len > 0 {
		g.sb.writeln('')
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.TypeDecl && stmt.variants.len == 0 && stmt.base_type !is ast.EmptyExpr
				&& !type_decl_base_is_fn_type(stmt) {
				g.gen_type_alias(stmt)
			}
		}
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.TypeDecl {
				if stmt.variants.len == 0 && stmt.base_type !is ast.EmptyExpr
					&& type_decl_base_is_fn_type(stmt) {
					g.gen_type_alias(stmt)
				} else if stmt.variants.len > 0 {
					g.gen_sum_type_decl(stmt)
				}
			}
		}
	}
}

fn (mut g Gen) emit_pass2_type_decls_flat() {
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() == .stmt_enum_decl {
				g.gen_enum_decl(stmt.enum_decl(true))
			}
		}
	}
	g.prescan_type_aliases_and_interfaces()
	g.emit_tuple_aliases()
	if g.tuple_aliases.len > 0 {
		g.sb.writeln('')
	}
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() != .stmt_type_decl {
				continue
			}
			decl := stmt.type_decl()
			if decl.variants.len == 0 && decl.base_type !is ast.EmptyExpr
				&& !type_decl_base_is_fn_type(decl) {
				g.gen_type_alias(decl)
			}
		}
	}
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() != .stmt_type_decl {
				continue
			}
			decl := stmt.type_decl()
			if decl.variants.len == 0 && decl.base_type !is ast.EmptyExpr
				&& type_decl_base_is_fn_type(decl) {
				g.gen_type_alias(decl)
			} else if decl.variants.len > 0 {
				g.gen_sum_type_decl(decl)
			}
		}
	}
}

fn (mut g Gen) prescan_type_aliases_and_interfaces() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_type_decl {
						decl := stmt.type_decl()
						if decl.base_type !is ast.EmptyExpr {
							_ = g.expr_type_to_c(decl.base_type)
						}
					}
					.stmt_interface_decl {
						decl := stmt.interface_decl()
						for field in decl.fields {
							_ = g.interface_method_info(field)
						}
					}
					else {}
				}
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.TypeDecl {
				if stmt.base_type !is ast.EmptyExpr {
					_ = g.expr_type_to_c(stmt.base_type)
				}
			} else if stmt is ast.InterfaceDecl {
				for field in stmt.fields {
					_ = g.interface_method_info(field)
				}
			}
		}
	}
}

fn (mut g Gen) collect_struct_and_interface_infos() ([]StructDeclInfo, []InterfaceDeclInfo) {
	mut all_structs := []StructDeclInfo{}
	mut all_interfaces := []InterfaceDeclInfo{}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_struct_decl {
						decl := stmt.struct_decl()
						if decl.language == .c {
							continue
						}
						all_structs << StructDeclInfo{
							decl:      decl
							mod:       g.cur_module
							file_name: fc.name()
						}
					}
					.stmt_interface_decl {
						decl := stmt.interface_decl()
						g.collect_interface_decl_info(decl, fc.name(), mut all_interfaces)
					}
					else {}
				}
			}
		}
		return all_structs, all_interfaces
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.StructDecl {
				if stmt.language == .c {
					continue
				}
				all_structs << StructDeclInfo{
					decl:      stmt
					mod:       g.cur_module
					file_name: file.name
				}
			} else if stmt is ast.InterfaceDecl {
				g.collect_interface_decl_info(stmt, file.name, mut all_interfaces)
			}
		}
	}
	return all_structs, all_interfaces
}

fn (mut g Gen) collect_interface_decl_info(decl ast.InterfaceDecl, file_name string, mut all_interfaces []InterfaceDeclInfo) {
	for field in decl.fields {
		_ = g.interface_method_info(field)
	}
	iface_c_name := if g.cur_module != '' && g.cur_module != 'builtin' {
		'${g.cur_module}__${decl.name}'
	} else {
		decl.name
	}
	g.interface_decls[iface_c_name] = decl
	all_interfaces << InterfaceDeclInfo{
		decl:      decl
		mod:       g.cur_module
		file_name: file_name
	}
}

fn (mut g Gen) emit_pass4_fn_forward_decls() {
	if g.has_flat() {
		for fi in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(fi)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_fn_decl {
					continue
				}
				g.emit_fn_forward_decl_for_decl(stmt.fn_decl_signature(), stmt.list_at(3).len(), fi)
			}
		}
		return
	}
	for fi, file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.FnDecl {
				decl := stmt as ast.FnDecl
				g.emit_fn_forward_decl_for_decl(decl, decl.stmts.len, fi)
			}
		}
	}
}

fn (mut g Gen) emit_fn_forward_decl_for_decl(decl ast.FnDecl, body_len int, file_idx int) {
	if !g.should_emit_fn_decl_cached(g.cur_module, decl) {
		return
	}
	if decl.language == .js {
		return
	}
	if decl.language == .c && body_len == 0 {
		// C extern declarations — their prototypes come from #include/#insert headers.
		return
	}
	if g.should_skip_backend_generic_fn(decl) {
		return
	}
	if g.generic_fn_param_names(decl).len > 0 {
		gfn_name := g.get_fn_name(decl)
		specs := g.generic_fn_specializations_for_emit_scope_with_receiver_bindings(decl)
		if specs.len > 0 {
			prev_generic_types := g.active_generic_types.clone()
			for spec in specs {
				g.active_generic_types = spec.generic_types.clone()
				g.record_fn_owner_for_current_file(spec.name, file_idx)
				g.gen_fn_head_with_name(decl, spec.name)
				g.sb.writeln(';')
			}
			g.active_generic_types = prev_generic_types.clone()
		} else {
			if gfn_name != '' && generic_fn_has_macro_fallback(decl) {
				g.emit_generic_fn_macro(gfn_name, decl)
			}
		}
		return
	}
	recv_gp := receiver_generic_param_names(decl)
	if recv_gp.len > 0 {
		all_bindings := g.get_all_receiver_generic_bindings(decl)
		if all_bindings.len > 0 {
			prev_generic_types := g.active_generic_types.clone()
			for bindings in all_bindings {
				g.active_generic_types = bindings.clone()
				gfn_name := g.get_fn_name(decl)
				if gfn_name != '' {
					g.record_fn_owner_for_current_file(gfn_name, file_idx)
					g.gen_fn_head_with_name(decl, gfn_name)
					g.sb.writeln(';')
				}
			}
			g.active_generic_types = prev_generic_types.clone()
			return
		} else if bindings := g.get_receiver_generic_bindings(decl) {
			prev_generic_types := g.active_generic_types.clone()
			g.active_generic_types = bindings.clone()
			gfn_name := g.get_fn_name(decl)
			if gfn_name != '' {
				g.record_fn_owner_for_current_file(gfn_name, file_idx)
				g.gen_fn_head_with_name(decl, gfn_name)
				g.sb.writeln(';')
			}
			g.active_generic_types = prev_generic_types.clone()
			return
		}
		return
	}
	fn_name := g.get_fn_name(decl)
	if fn_name == '' {
		return
	}
	fn_key := 'fn_${fn_name}'
	if g.should_skip_plain_v_fallback_fn(fn_key) {
		return
	}
	if fn_name == 'main' {
		g.has_main = true
	}
	if decl.name.starts_with('test_') && !decl.is_method && decl.typ.params.len == 0 {
		g.test_fn_names << fn_name
	}
	g.record_fn_owner_for_current_file(fn_name, file_idx)
	if g.env != unsafe { nil } {
		if fn_scope := g.env.get_fn_scope(g.cur_module, fn_name) {
			g.cur_fn_scope = fn_scope
		}
	}
	g.gen_fn_head_with_name(decl, fn_name)
	g.sb.writeln(';')
}

fn (mut g Gen) emit_pass45_const_decls() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			if !g.should_emit_current_file() {
				continue
			}
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() == .stmt_const_decl {
					g.gen_const_decl(stmt.const_decl())
				}
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		if !g.should_emit_current_file() {
			continue
		}
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.ConstDecl {
				g.gen_const_decl(stmt)
			}
		}
	}
}

fn (mut g Gen) record_fn_owner_for_current_file(fn_name string, file_idx int) {
	if fn_name == '' || !g.should_emit_current_file() {
		return
	}
	fn_key := 'fn_${fn_name}'
	if fn_key !in g.fn_owner_file {
		g.fn_owner_file[fn_key] = file_idx
	}
}

fn (mut g Gen) collect_c_file_fn_keys() {
	g.c_file_fn_keys = map[string]bool{}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			if !fc.name().ends_with('.c.v') {
				continue
			}
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_fn_decl {
					continue
				}
				decl := stmt.fn_decl_signature()
				if decl.language == .js {
					continue
				}
				if decl.language == .c && stmt.list_at(3).len() == 0 {
					continue
				}
				if decl.typ.generic_params.len > 0 {
					continue
				}
				fn_name := g.get_fn_name(decl)
				if fn_name == '' {
					continue
				}
				g.c_file_fn_keys['fn_${fn_name}'] = true
			}
		}
		return
	}
	for file in g.files {
		if !file.name.ends_with('.c.v') {
			continue
		}
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				decl := stmt as ast.FnDecl
				if decl.language == .js {
					continue
				}
				if decl.language == .c && decl.stmts.len == 0 {
					continue
				}
				if decl.typ.generic_params.len > 0 {
					continue
				}
				fn_name := g.get_fn_name(decl)
				if fn_name == '' {
					continue
				}
				g.c_file_fn_keys['fn_${fn_name}'] = true
			}
		}
	}
}

fn (g &Gen) should_skip_plain_v_fallback_fn(fn_key string) bool {
	return g.cur_file_name.ends_with('.v') && !g.cur_file_name.ends_with('.c.v')
		&& fn_key in g.c_file_fn_keys
}

// gen_finalize runs post-pass-5 finalization and returns the complete C source string.
pub fn (mut g Gen) gen_finalize() string {
	stats_enabled := g.cgen_stats_enabled()
	stats_scope := g.cgen_stats_scope_label()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()

	// Generate test runner main if this is a test file (has test_ functions but no main)
	// Skip when generating cached module sources (cache_bundle_name is set) - main belongs only in the main module source
	if !g.has_main && g.test_fn_names.len > 0 && g.cache_bundle_name.len == 0 {
		g.sb.writeln('')
		g.sb.writeln('int main(int ___argc, char** ___argv) {')
		g.sb.writeln('\tg_main_argc = ___argc;')
		g.sb.writeln('\tg_main_argv = (void*)___argv;')
		for init_call in g.cached_init_calls {
			g.sb.writeln('\t${init_call}();')
		}
		// Runtime constants like builtin.max_int must be initialized before module
		// init() functions, since init paths can allocate and use array bounds.
		for fn_name, _ in g.fn_return_types {
			if fn_name.contains('__v_init_consts_') && fn_name != '__v_init_consts_main' {
				g.sb.writeln('\t${fn_name}();')
			}
		}
		// Call module init() functions and __v_init_consts_main — test files have
		// no main() function, so the transformer's injected init calls are not present.
		for fn_name, _ in g.fn_return_types {
			// Module init functions: MODULE__init (e.g., rand__init)
			// Skip methods (Type__method patterns where Type is capitalized)
			if fn_name.ends_with('__init') && count_substr_in_string(fn_name, '__') == 1 {
				first_char := fn_name[0]
				if first_char >= `a` && first_char <= `z` {
					if params := g.fn_param_is_ptr[fn_name] {
						if params.len == 0 {
							g.sb.writeln('\t${fn_name}();')
						}
					} else {
						g.sb.writeln('\t${fn_name}();')
					}
				}
			}
		}
		if '__v_init_consts_main' in g.fn_return_types {
			g.sb.writeln('\t__v_init_consts_main();')
		}
		g.sb.writeln('\tg_main_argc = ___argc;')
		g.sb.writeln('\tg_main_argv = (void*)___argv;')
		for test_fn in g.test_fn_names {
			msg_run := 'Running test: ${test_fn}...'
			msg_ok := '  OK'
			g.sb.writeln('\tprintln(${c_static_v_string_expr(msg_run)});')
			g.sb.writeln('\t${test_fn}();')
			g.sb.writeln('\tprintln(${c_static_v_string_expr(msg_ok)});')
		}
		msg_all := 'All ${g.test_fn_names.len} tests passed.'
		g.sb.writeln('\tprintln(${c_static_v_string_expr(msg_all)});')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'finalize test main')

	array_contains_specs := g.missing_array_contains_fallback_specs()
	late_array_contains_decls := array_contains_fallback_decls(array_contains_specs)
	g.emit_array_contains_fallbacks(array_contains_specs)
	g.emit_missing_runtime_fallbacks()
	g.emit_cached_module_init_function()
	g.emit_exported_const_symbols()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'finalize fallbacks')

	mut out := ''
	// Emit deferred str macros for late-discovered generic struct instances.
	// At this point fn_return_types is fully populated (pass 4 complete).
	for inst_name in g.late_generic_str_instances {
		str_fn := '${inst_name}__str'
		if str_fn !in g.fn_return_types {
			label := '${inst_name}{}'
			g.late_struct_defs << '#define ${inst_name}__str(v) ((string){.str = "${label}", .len = ${label.len}, .is_lit = 1})\n#define ${inst_name}_str(v) ${inst_name}__str(v)\n'
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'finalize late str macros')
	has_late_defs := g.anon_fn_defs.len > 0 || g.spawn_wrapper_defs.len > 0
		|| g.trampoline_defs.len > 0 || g.late_struct_defs.len > 0
		|| g.pending_late_body_keys.len > 0
	if late_array_contains_decls.len > 0 || has_late_defs {
		full := g.sb.str()
		stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
			'finalize snapshot')
		mut out_sb := strings.new_builder(full.len + late_array_contains_decls.len + 4096)
		unsafe { out_sb.write_ptr(full.str, g.pass5_start_pos) }
		if has_late_defs {
			// Late-discovered generic struct definitions (discovered during setup/pass 4 codegen)
			for def in g.late_struct_defs {
				out_sb.write_string(def)
			}
			// Mark pending late body keys as emitted now that they're in the output.
			for key, _ in g.pending_late_body_keys {
				g.emitted_types[key] = true
			}
			g.pending_late_body_keys = map[string]bool{}
			// Emit any option/result wrappers that were deferred because their payload
			// types were only in late_struct_defs. Temporarily swap sb with out_sb.
			g.sb = out_sb
			g.emit_option_result_structs()
			out_sb = g.sb
			g.sb = strings.new_builder(0)
		}
		if late_array_contains_decls.len > 0 {
			out_sb.write_string(late_array_contains_decls)
		}
		if has_late_defs {
			mut seen_spawn_defs := map[string]bool{}
			for def in g.spawn_wrapper_defs {
				if def in seen_spawn_defs {
					continue
				}
				seen_spawn_defs[def] = true
				out_sb.write_string(def)
			}
			for def in g.anon_fn_defs {
				out_sb.write_string(def)
			}
			for def in g.trampoline_defs {
				out_sb.write_string(def)
			}
		}
		if g.pass5_start_pos < full.len {
			unsafe { out_sb.write_ptr(full.str + g.pass5_start_pos, full.len - g.pass5_start_pos) }
		}
		out = out_sb.str()
	} else {
		out = g.sb.str()
	}
	_ = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start, 'finalize output')
	return out
}

// gen_pass5 generates Pass 5 (function bodies, globals, etc.) sequentially.
fn (mut g Gen) gen_pass5() {
	stats_enabled := g.cgen_stats_enabled()
	stats_scope := g.cgen_stats_scope_label()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()

	g.pass5_start_pos = g.sb.len
	g.selector_field_type_cache = map[string]string{}
	g.selector_field_type_miss = map[string]bool{}
	g.struct_field_lookup_cache = map[string]string{}
	g.struct_field_lookup_miss = map[string]bool{}
	g.ensure_flat_struct_decl_index()
	g.ensure_called_specialized_name_index()
	g.collect_force_emit_str_fns()
	g.emit_pass5_extern_consts_for_non_emit_files()
	g.emit_pass5_extern_globals()
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			if g.should_emit_current_file() {
				g.gen_file_cursor(fc)
			}
		}
	} else {
		for file in g.files {
			g.set_file_module(file)
			if g.should_emit_current_file() {
				g.gen_file(file)
			}
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 5 files')
	g.gen_pass5_post()
	_ = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start, 'pass 5 post')
}

// gen_pass5_pre runs Pass 5 sequential pre-work: extern globals and extern consts
// for non-emitted modules. Returns the list of file indices that need gen_file().
pub fn (mut g Gen) gen_pass5_pre() []int {
	g.pass5_start_pos = g.sb.len
	g.selector_field_type_cache = map[string]string{}
	g.selector_field_type_miss = map[string]bool{}
	g.struct_field_lookup_cache = map[string]string{}
	g.struct_field_lookup_miss = map[string]bool{}
	g.ensure_flat_struct_decl_index()
	g.ensure_called_specialized_name_index()
	g.collect_force_emit_str_fns()
	g.emit_pass5_extern_consts_for_non_emit_files()
	g.emit_pass5_extern_globals()
	// Collect emittable file indices. Also build global_owner_file: assign each
	// global to the first file that declares it so parallel workers can avoid
	// emitting duplicate definitions.
	if g.has_flat() {
		return g.collect_pass5_emit_indices_flat()
	}
	mut emit_indices := []int{cap: g.files.len}
	for fi, file in g.files {
		g.set_file_module(file)
		if g.should_emit_current_file() {
			emit_indices << fi
			for stmt in file.stmts {
				if stmt is ast.GlobalDecl {
					for field in stmt.fields {
						gname := if g.cur_module != '' && g.cur_module != 'main'
							&& g.cur_module != 'builtin' {
							'${g.cur_module}__${field.name}'
						} else {
							field.name
						}
						if gname !in g.global_owner_file {
							g.global_owner_file[gname] = fi
						}
					}
				}
			}
		}
	}
	return emit_indices
}

fn (mut g Gen) emit_pass5_extern_consts_for_non_emit_files() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			if g.should_emit_current_file() {
				continue
			}
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() == .stmt_const_decl {
					g.gen_const_decl_extern(stmt.const_decl())
				}
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		if !g.should_emit_current_file() {
			g.gen_file_extern_consts(file)
		}
	}
}

fn (mut g Gen) emit_pass5_extern_globals() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() == .stmt_global_decl {
					g.gen_global_decl_extern(stmt.global_decl(true))
				}
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		g.gen_file_extern_globals(file)
	}
}

fn (mut g Gen) collect_pass5_emit_indices_flat() []int {
	mut emit_indices := []int{cap: g.flat.files.len}
	for fi in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(fi)
		g.set_file_cursor_module(fc)
		if !g.should_emit_current_file() {
			continue
		}
		emit_indices << fi
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() != .stmt_global_decl {
				continue
			}
			decl := stmt.global_decl(false)
			for field in decl.fields {
				gname := if g.cur_module != '' && g.cur_module != 'main'
					&& g.cur_module != 'builtin' {
					'${g.cur_module}__${field.name}'
				} else {
					field.name
				}
				if gname !in g.global_owner_file {
					g.global_owner_file[gname] = fi
				}
			}
		}
	}
	return emit_indices
}

// gen_pass5_post runs post-Pass-5 finalization (interface wrappers, live reload, map helpers).
pub fn (mut g Gen) gen_pass5_post() {
	g.emit_forced_helpers_from_non_emit_files()
	g.emit_needed_ierror_wrappers()
	g.emit_needed_interface_method_wrappers()
	g.emit_interface_clone_helpers()
	g.emit_option_string_clone_helper()
	g.emit_array_interface_repeat_helpers()
	g.emit_live_reload_infrastructure()
	if g.cache_bundle_name.len == 0 {
		g.emit_map_str_functions()
		g.emit_map_eq_functions()
	}
}

fn (mut g Gen) emit_forced_helpers_from_non_emit_files() {
	if g.emit_files.len == 0 || g.force_emit_fn_names.len == 0 {
		return
	}
	old_file := g.cur_file_name
	old_module := g.cur_module
	old_import_modules := g.cur_import_modules.clone()
	mut emitted_file_fns := map[string]bool{}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			if !g.should_emit_current_file() {
				continue
			}
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() == .stmt_fn_decl && stmt.name().starts_with('__sort_cmp_') {
					decl := stmt.fn_decl_signature()
					fn_name := g.get_fn_name(decl)
					if fn_name != '' {
						emitted_file_fns[fn_name] = true
					}
				}
			}
		}
		mut emitted := map[string]bool{}
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			if fc.name().ends_with('.vh') {
				continue
			}
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_fn_decl || !stmt.name().starts_with('__sort_cmp_') {
					continue
				}
				decl_sig := stmt.fn_decl_signature()
				fn_name := g.get_fn_name(decl_sig)
				if fn_name == '' || fn_name !in g.force_emit_fn_names || fn_name in emitted {
					continue
				}
				if fn_name in emitted_file_fns {
					continue
				}
				if 'fn_${fn_name}' in g.fn_owner_file {
					continue
				}
				decl := stmt.fn_decl()
				g.gen_fn_decl(decl)
				emitted[fn_name] = true
			}
		}
		g.cur_file_name = old_file
		g.cur_module = old_module
		g.cur_import_modules = old_import_modules.clone()
		return
	}
	for file in g.files {
		g.set_file_module(file)
		if !g.should_emit_current_file() {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name.starts_with('__sort_cmp_') {
				fn_name := g.get_fn_name(stmt)
				if fn_name != '' {
					emitted_file_fns[fn_name] = true
				}
			}
		}
	}
	mut emitted := map[string]bool{}
	for file in g.files {
		g.set_file_module(file)
		if file.name.ends_with('.vh') {
			continue
		}
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.FnDecl {
				decl := stmt as ast.FnDecl
				if !decl.name.starts_with('__sort_cmp_') {
					continue
				}
				fn_name := g.get_fn_name(decl)
				if fn_name == '' || fn_name !in g.force_emit_fn_names || fn_name in emitted {
					continue
				}
				if fn_name in emitted_file_fns {
					continue
				}
				if 'fn_${fn_name}' in g.fn_owner_file {
					continue
				}
				g.gen_fn_decl(decl)
				emitted[fn_name] = true
			}
		}
	}
	g.cur_file_name = old_file
	g.cur_module = old_module
	g.cur_import_modules = old_import_modules.clone()
}

fn (mut g Gen) late_generic_fn_specializations(node ast.FnDecl) []GenericFnSpecialization {
	generic_params := g.generic_fn_param_names(node)
	if generic_params.len == 0 || g.env == unsafe { nil } {
		return []GenericFnSpecialization{}
	}
	mut specs := []GenericFnSpecialization{}
	mut seen := map[string]bool{}
	for key in g.generic_spec_index[node.name] {
		if key !in g.late_generic_specs || !g.generic_key_matches_decl(node, key) {
			continue
		}
		for generic_types in g.late_generic_specs[key] {
			mut skip_spec := false
			mut runtime_specializable := true
			mut normalized_generic_types := generic_types.clone()
			for param_name in generic_params {
				concrete0 := generic_types[param_name] or {
					skip_spec = true
					break
				}
				concrete := normalize_generic_concrete_type(concrete0)
				normalized_generic_types[param_name] = concrete
				if concrete.name() == 'void' || concrete.name() == param_name
					|| type_contains_generic_placeholder(concrete)
					|| !generic_concrete_type_is_runtime_specializable(concrete) {
					skip_spec = true
					break
				}
				if !g.generic_concrete_type_is_runtime_specializable(concrete) {
					runtime_specializable = false
				}
			}
			if skip_spec
				|| !g.generic_specialization_belongs_to_emit_modules(normalized_generic_types) {
				continue
			}
			spec_name := g.specialized_fn_name(node, normalized_generic_types)
			if spec_name == '' || spec_name in seen {
				continue
			}
			if !runtime_specializable && spec_name !in g.called_fn_names {
				continue
			}
			seen[spec_name] = true
			specs << GenericFnSpecialization{
				name:          spec_name
				generic_types: normalized_generic_types.clone()
			}
		}
	}
	return specs
}

fn (mut g Gen) generic_types_from_specialized_fn_name(node ast.FnDecl, fn_name string) ?map[string]types.Type {
	generic_params := g.generic_fn_param_names(node)
	if generic_params.len == 0 {
		return none
	}
	mut prev_generic_types := g.active_generic_types.move()
	g.active_generic_types = map[string]types.Type{}
	base_name := g.get_fn_name(node)
	g.active_generic_types = prev_generic_types.move()
	prefix := '${base_name}_T_'
	mut suffix := ''
	if fn_name.starts_with(prefix) {
		suffix = fn_name[prefix.len..]
	} else {
		specialized_base := generic_call_base_name_for_specialization(fn_name)
		specialized_prefix := '${specialized_base}_T_'
		if specialized_fn_decl_base_name(fn_name) != base_name
			|| !fn_name.starts_with(specialized_prefix) {
			return none
		}
		suffix = fn_name[specialized_prefix.len..]
	}
	tokens := g.split_specialization_suffix(suffix, generic_params.len) or { return none }
	mut generic_types := map[string]types.Type{}
	for i, param_name in generic_params {
		concrete := g.concrete_type_from_specialization_token(tokens[i])
		if type_contains_generic_placeholder(concrete)
			|| !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return none
		}
		generic_types[param_name] = concrete
	}
	if g.specialized_fn_name(node, generic_types) != fn_name {
		return none
	}
	return generic_types
}

fn (mut g Gen) split_specialization_suffix(suffix string, parts int) ?[]string {
	if parts <= 0 || suffix == '' {
		return none
	}
	if parts == 1 {
		concrete := g.concrete_type_from_specialization_token(suffix)
		if type_contains_generic_placeholder(concrete)
			|| !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return none
		}
		return [suffix]
	}
	for i := 1; i < suffix.len - 1; i++ {
		if suffix[i] != `_` {
			continue
		}
		head := suffix[..i]
		concrete := g.concrete_type_from_specialization_token(head)
		if type_contains_generic_placeholder(concrete)
			|| !g.generic_concrete_type_is_runtime_specializable(concrete) {
			continue
		}
		tail := suffix[i + 1..]
		if tail_tokens := g.split_specialization_suffix(tail, parts - 1) {
			mut tokens := [head]
			tokens << tail_tokens
			return tokens
		}
	}
	return none
}

// Pass5WorkItem is one unit of parallel Pass 5 work. A small file is one item
// covering the whole file (fn_indices empty => use gen_file). A large file is
// split into several items, each owning a contiguous slice of the file's FnDecl
// statement indices; only the first slice (emit_globals) emits the file globals.
pub struct Pass5WorkItem {
pub:
	file_idx     int
	fn_indices   []int // empty => emit the whole file (gen_file)
	emit_globals bool
	cost         int
}

// pass5_split_threshold is the per-item codegen-cost ceiling. Files costing more
// than this are split into sub-file items so no single file pins a worker. The
// largest single self-host files (ssa/builder.v ~545k, transformer.v ~446k,
// gen/cleanc/fn.v ~386k) otherwise serialize the whole parallel phase.
const pass5_split_threshold = 100_000

// pass5_file_fn_indices returns the statement indices of `file_idx`'s top-level FnDecls.
fn (g &Gen) pass5_file_fn_indices(file_idx int) []int {
	mut out := []int{}
	if g.has_flat() {
		if file_idx < 0 || file_idx >= g.flat.files.len {
			return out
		}
		stmts := g.flat.file_cursor(file_idx).stmts()
		for i in 0 .. stmts.len() {
			if stmts.at(i).kind() == .stmt_fn_decl {
				out << i
			}
		}
		return out
	}
	for i, stmt in g.files[file_idx].stmts {
		if stmt is ast.FnDecl {
			out << i
		}
	}
	return out
}

fn (g &Gen) pass5_stmt_cost(file_idx int, stmt_idx int) int {
	if g.has_flat() {
		if file_idx < 0 || file_idx >= g.flat.files.len {
			return 1
		}
		stmts := g.flat.file_cursor(file_idx).stmts()
		if stmt_idx < 0 || stmt_idx >= stmts.len() {
			return 1
		}
		return cleanc_stmt_cursor_codegen_cost(stmts.at(stmt_idx))
	}
	if file_idx < 0 || file_idx >= g.files.len || stmt_idx < 0
		|| stmt_idx >= g.files[file_idx].stmts.len {
		return 1
	}
	return cleanc_stmt_codegen_cost(g.files[file_idx].stmts[stmt_idx])
}

fn (g &Gen) pass5_file_name(file_idx int) string {
	if g.has_flat() {
		if file_idx < 0 || file_idx >= g.flat.files.len {
			return ''
		}
		return g.flat.file_cursor(file_idx).name()
	}
	if file_idx < 0 || file_idx >= g.files.len {
		return ''
	}
	return g.files[file_idx].name
}

// build_pass5_work_items turns the emittable file indices into balanced work
// items, splitting any file whose codegen cost exceeds pass5_split_threshold
// into contiguous FnDecl-index slices.
pub fn (g &Gen) build_pass5_work_items(emit_indices []int) []Pass5WorkItem {
	mut items := []Pass5WorkItem{cap: emit_indices.len}
	for fi in emit_indices {
		file_cost := g.pass5_file_cost(fi)
		if file_cost <= pass5_split_threshold {
			items << Pass5WorkItem{
				file_idx:     fi
				emit_globals: true
				cost:         file_cost
			}
			continue
		}
		fn_indices := g.pass5_file_fn_indices(fi)
		if fn_indices.len <= 1 {
			// Nothing to split — a single (giant) function or no functions.
			items << Pass5WorkItem{
				file_idx:     fi
				emit_globals: true
				cost:         file_cost
			}
			continue
		}
		// Greedily pack contiguous functions into slices of ~pass5_split_threshold cost.
		mut slice_start := 0
		mut slice_cost := 0
		mut first_slice := true
		for k, idx in fn_indices {
			fn_cost := g.pass5_stmt_cost(fi, idx)
			slice_cost += fn_cost
			is_last := k == fn_indices.len - 1
			if slice_cost >= pass5_split_threshold || is_last {
				items << Pass5WorkItem{
					file_idx:     fi
					fn_indices:   fn_indices[slice_start..k + 1]
					emit_globals: first_slice
					cost:         slice_cost
				}
				first_slice = false
				slice_start = k + 1
				slice_cost = 0
			}
		}
	}
	return items
}

// gen_pass5_work_items emits each assigned work item. Whole-file items go through
// gen_file; split items emit only their FnDecl slice via gen_file_range.
pub fn (mut g Gen) gen_pass5_work_items(items []Pass5WorkItem) {
	stats_enabled := g.cgen_stats_enabled()
	for item in items {
		if !stats_enabled {
			g.gen_pass5_work_item(item)
			continue
		}
		mut sw := time.new_stopwatch()
		g.gen_pass5_work_item(item)
		elapsed_ms := sw.elapsed().milliseconds()
		if elapsed_ms > 0 {
			suffix := if item.fn_indices.len == 0 { '' } else { ' [${item.fn_indices.len}fns]' }
			g.pass5_file_times << Pass5FileTime{
				file:      g.pass5_file_name(item.file_idx) + suffix
				ms:        elapsed_ms
				cost:      item.cost
				worker_id: g.pass5_worker_id
			}
		}
	}
}

fn (mut g Gen) gen_pass5_work_item(item Pass5WorkItem) {
	if item.fn_indices.len == 0 {
		if g.has_flat() {
			g.gen_file_cursor(g.flat.file_cursor(item.file_idx))
		} else {
			g.gen_file(g.files[item.file_idx])
		}
		return
	}
	g.explicit_slice_active = true
	g.explicit_slice_file = item.file_idx
	if g.has_flat() {
		g.gen_file_cursor_range(g.flat.file_cursor(item.file_idx), item.fn_indices,
			item.emit_globals)
	} else {
		g.gen_file_range(g.files[item.file_idx], item.fn_indices, item.emit_globals)
	}
	g.explicit_slice_active = false
}

// gen_pass5_files generates function bodies for a range of file indices.
// Used by parallel dispatch — each worker calls this with its assigned chunk.
pub fn (mut g Gen) gen_pass5_files(file_indices []int) {
	stats_enabled := g.cgen_stats_enabled()
	for fi in file_indices {
		file_name := g.pass5_file_name(fi)
		if stats_enabled {
			mut sw := time.new_stopwatch()
			if g.has_flat() {
				g.gen_file_cursor(g.flat.file_cursor(fi))
			} else {
				g.gen_file(g.files[fi])
			}
			elapsed_ms := sw.elapsed().milliseconds()
			if elapsed_ms > 0 {
				g.pass5_file_times << Pass5FileTime{
					file:      file_name
					ms:        elapsed_ms
					cost:      g.pass5_file_cost(fi)
					worker_id: g.pass5_worker_id
				}
			}
		} else {
			if g.has_flat() {
				g.gen_file_cursor(g.flat.file_cursor(fi))
			} else {
				g.gen_file(g.files[fi])
			}
		}
	}
}

pub fn (g &Gen) print_pass5_file_times(limit int) {
	if !g.cgen_stats_enabled() || g.pass5_file_times.len == 0 {
		return
	}
	mut times := g.pass5_file_times.clone()
	for i := 1; i < times.len; i++ {
		mut j := i
		for j > 0 && times[j - 1].ms < times[j].ms {
			times[j - 1], times[j] = times[j], times[j - 1]
			j--
		}
	}
	stats_scope := g.cgen_stats_scope_label()
	n := if times.len < limit { times.len } else { limit }
	for item in times[..n] {
		println('   - C Gen/${stats_scope} pass 5 file ${item.ms}ms worker=${item.worker_id} cost=${item.cost} ${item.file}')
	}
}

// pass5_file_cost estimates relative codegen work for balancing parallel chunks.
pub fn (g &Gen) pass5_file_cost(file_idx int) int {
	if g.has_flat() {
		if file_idx < 0 || file_idx >= g.flat.files.len {
			return 1
		}
		mut cost := 1
		stmts := g.flat.file_cursor(file_idx).stmts()
		for i in 0 .. stmts.len() {
			cost += cleanc_stmt_cursor_codegen_cost(stmts.at(i))
		}
		return cost
	}
	if file_idx < 0 || file_idx >= g.files.len {
		return 1
	}
	mut cost := 1
	for stmt in g.files[file_idx].stmts {
		cost += cleanc_stmt_codegen_cost(stmt)
	}
	return cost
}

fn cleanc_stmts_codegen_cost(stmts []ast.Stmt) int {
	mut cost := 0
	for stmt in stmts {
		cost += cleanc_stmt_codegen_cost(stmt)
	}
	return cost
}

fn cleanc_cursor_stmts_codegen_cost(stmts ast.CursorList) int {
	mut cost := 0
	for i in 0 .. stmts.len() {
		cost += cleanc_stmt_cursor_codegen_cost(stmts.at(i))
	}
	return cost
}

fn cleanc_cursor_children_expr_codegen_cost(c ast.Cursor, start int) int {
	mut cost := 0
	for i in start .. c.edge_count() {
		cost += cleanc_expr_cursor_codegen_cost(c.edge(i))
	}
	return cost
}

fn cleanc_cursor_children_stmt_codegen_cost(c ast.Cursor, start int) int {
	mut cost := 0
	for i in start .. c.edge_count() {
		cost += cleanc_stmt_cursor_codegen_cost(c.edge(i))
	}
	return cost
}

fn cleanc_stmt_cursor_codegen_cost(stmt ast.Cursor) int {
	if !stmt.is_valid() {
		return 1
	}
	match stmt.kind() {
		.stmt_fn_decl {
			return 50 + cleanc_cursor_stmts_codegen_cost(stmt.list_at(3))
		}
		.stmt_assign {
			return 12 + cleanc_cursor_children_expr_codegen_cost(stmt, 0)
		}
		.stmt_expr {
			return 4 + cleanc_expr_cursor_codegen_cost(stmt.edge(0))
		}
		.stmt_return {
			return 8 + cleanc_cursor_children_expr_codegen_cost(stmt, 0)
		}
		.stmt_for {
			return 30 + cleanc_stmt_cursor_codegen_cost(stmt.edge(0)) +
				cleanc_expr_cursor_codegen_cost(stmt.edge(1)) +
				cleanc_stmt_cursor_codegen_cost(stmt.edge(2)) +
				cleanc_cursor_children_stmt_codegen_cost(stmt, 3)
		}
		.stmt_for_in {
			return 30 + cleanc_cursor_children_expr_codegen_cost(stmt, 0)
		}
		.stmt_block {
			return 4 + cleanc_cursor_children_stmt_codegen_cost(stmt, 0)
		}
		.stmt_defer {
			return 10 + cleanc_cursor_children_stmt_codegen_cost(stmt, 0)
		}
		.stmt_const_decl {
			return 8 + cleanc_expr_cursor_codegen_cost(stmt.edge(0))
		}
		.stmt_global_decl {
			return 20 + stmt.list_at(1).len() * 8
		}
		.stmt_struct_decl {
			return 8 + stmt.list_at(4).len()
		}
		.stmt_interface_decl {
			return 8 + stmt.list_at(3).len()
		}
		.stmt_type_decl {
			return 8 + stmt.list_at(3).len()
		}
		.stmt_comptime, .stmt_label {
			return 4 + cleanc_stmt_cursor_codegen_cost(stmt.edge(0))
		}
		else {
			return 1
		}
	}
}

fn cleanc_stmt_codegen_cost(stmt ast.Stmt) int {
	match stmt {
		ast.FnDecl {
			return 50 + cleanc_stmts_codegen_cost(stmt.stmts)
		}
		ast.AssignStmt {
			mut cost := 12
			for expr in stmt.lhs {
				cost += cleanc_expr_codegen_cost(expr)
			}
			for expr in stmt.rhs {
				cost += cleanc_expr_codegen_cost(expr)
			}
			return cost
		}
		ast.ExprStmt {
			return 4 + cleanc_expr_codegen_cost(stmt.expr)
		}
		ast.ReturnStmt {
			mut cost := 8
			for expr in stmt.exprs {
				cost += cleanc_expr_codegen_cost(expr)
			}
			return cost
		}
		ast.ForStmt {
			return 30 + cleanc_stmt_codegen_cost(stmt.init) + cleanc_expr_codegen_cost(stmt.cond) +
				cleanc_stmt_codegen_cost(stmt.post) + cleanc_stmts_codegen_cost(stmt.stmts)
		}
		ast.ForInStmt {
			return 30 + cleanc_expr_codegen_cost(stmt.expr)
		}
		ast.BlockStmt {
			return 4 + cleanc_stmts_codegen_cost(stmt.stmts)
		}
		ast.DeferStmt {
			return 10 + cleanc_stmts_codegen_cost(stmt.stmts)
		}
		ast.ConstDecl {
			mut cost := 8
			for field in stmt.fields {
				cost += cleanc_expr_codegen_cost(field.value)
			}
			return cost
		}
		ast.GlobalDecl {
			return 20 + stmt.fields.len * 8
		}
		ast.StructDecl {
			return 8 + stmt.fields.len
		}
		ast.InterfaceDecl {
			return 8 + stmt.fields.len
		}
		ast.TypeDecl {
			return 8 + stmt.variants.len
		}
		ast.ComptimeStmt {
			return 4 + cleanc_stmt_codegen_cost(stmt.stmt)
		}
		ast.LabelStmt {
			return 4 + cleanc_stmt_codegen_cost(stmt.stmt)
		}
		else {
			return 1
		}
	}
}

fn cleanc_expr_cursor_codegen_cost(expr ast.Cursor) int {
	if !expr.is_valid() {
		return 1
	}
	match expr.kind() {
		.aux_list {
			return cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.aux_field_init {
			return 4 + cleanc_expr_cursor_codegen_cost(expr.edge(0))
		}
		.expr_call {
			return 18 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_call_or_cast {
			return 14 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_init {
			return 10 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_array_init {
			return 6 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_map_init {
			return 10 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_if {
			return 18 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_match {
			return 20 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_infix {
			return 6 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_prefix, .expr_postfix, .expr_selector, .expr_keyword_operator, .expr_range {
			return 4 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_index, .expr_cast, .expr_as_cast, .expr_tuple {
			return 6 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_paren, .expr_modifier {
			return cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_or, .expr_unsafe {
			return 12 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_lock, .expr_fn_literal, .expr_lambda, .expr_sql {
			return 20 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		.expr_comptime {
			return 8 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
		else {
			return 1 + cleanc_cursor_children_expr_codegen_cost(expr, 0)
		}
	}
}

fn cleanc_exprs_codegen_cost(exprs []ast.Expr) int {
	mut cost := 0
	for expr in exprs {
		cost += cleanc_expr_codegen_cost(expr)
	}
	return cost
}

fn cleanc_expr_codegen_cost(expr ast.Expr) int {
	match expr {
		ast.CallExpr {
			if orm_create_call_can_emit(expr) {
				return 30 + cleanc_expr_codegen_cost(expr.lhs)
			}
			return 18 + cleanc_expr_codegen_cost(expr.lhs) + cleanc_exprs_codegen_cost(expr.args)
		}
		ast.CallOrCastExpr {
			return 14 + cleanc_expr_codegen_cost(expr.lhs) + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.InitExpr {
			mut cost := 10
			for field in expr.fields {
				cost += cleanc_expr_codegen_cost(field.value)
			}
			return cost
		}
		ast.ArrayInitExpr {
			return 6 + cleanc_exprs_codegen_cost(expr.exprs) + cleanc_expr_codegen_cost(expr.init) +
				cleanc_expr_codegen_cost(expr.cap) + cleanc_expr_codegen_cost(expr.len)
		}
		ast.MapInitExpr {
			return 10 + cleanc_exprs_codegen_cost(expr.keys) + cleanc_exprs_codegen_cost(expr.vals)
		}
		ast.IfExpr {
			return 18 + cleanc_expr_codegen_cost(expr.cond) +
				cleanc_stmts_codegen_cost(expr.stmts) + cleanc_expr_codegen_cost(expr.else_expr)
		}
		ast.MatchExpr {
			mut cost := 20 + cleanc_expr_codegen_cost(expr.expr)
			for branch in expr.branches {
				cost += cleanc_exprs_codegen_cost(branch.cond) +
					cleanc_stmts_codegen_cost(branch.stmts)
			}
			return cost
		}
		ast.InfixExpr {
			return 6 + cleanc_expr_codegen_cost(expr.lhs) + cleanc_expr_codegen_cost(expr.rhs)
		}
		ast.PrefixExpr {
			return 4 + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.PostfixExpr {
			return 4 + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.SelectorExpr {
			return 4 + cleanc_expr_codegen_cost(expr.lhs)
		}
		ast.IndexExpr {
			return 6 + cleanc_expr_codegen_cost(expr.lhs) + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.CastExpr {
			return 6 + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.AsCastExpr {
			return 6 + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.ParenExpr {
			return cleanc_expr_codegen_cost(expr.expr)
		}
		ast.ModifierExpr {
			return cleanc_expr_codegen_cost(expr.expr)
		}
		ast.OrExpr {
			return 12 + cleanc_expr_codegen_cost(expr.expr) + cleanc_stmts_codegen_cost(expr.stmts)
		}
		ast.UnsafeExpr {
			return 12 + cleanc_stmts_codegen_cost(expr.stmts)
		}
		ast.LockExpr {
			return 20 + cleanc_exprs_codegen_cost(expr.lock_exprs) +
				cleanc_exprs_codegen_cost(expr.rlock_exprs) + cleanc_stmts_codegen_cost(expr.stmts)
		}
		ast.FieldInit {
			return 4 + cleanc_expr_codegen_cost(expr.value)
		}
		ast.Tuple {
			return 6 + cleanc_exprs_codegen_cost(expr.exprs)
		}
		ast.KeywordOperator {
			return 4 + cleanc_exprs_codegen_cost(expr.exprs)
		}
		ast.RangeExpr {
			return 4 + cleanc_expr_codegen_cost(expr.start) + cleanc_expr_codegen_cost(expr.end)
		}
		ast.ComptimeExpr {
			return 8 + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.FnLiteral {
			return 20 + cleanc_exprs_codegen_cost(expr.captured_vars) +
				cleanc_stmts_codegen_cost(expr.stmts)
		}
		ast.LambdaExpr {
			return 20 + cleanc_expr_codegen_cost(expr.expr)
		}
		ast.SqlExpr {
			return 20 + cleanc_expr_codegen_cost(expr.expr)
		}
		else {
			return 1
		}
	}
}

// new_pass5_worker creates a worker Gen for parallel Pass 5.
// file_indices specifies which files this worker will process.
// Functions owned by files outside this range are pre-marked as emitted
// to prevent duplicate emission across workers.
pub fn (g &Gen) new_pass5_worker(file_indices []int, worker_id int) &Gen {
	// Build a set of file indices this worker owns
	mut owned_files := map[int]bool{}
	for fi in file_indices {
		owned_files[fi] = true
	}
	// Clone emitted_types and reserve functions/globals owned by other workers.
	mut worker_emitted := g.emitted_types.clone()
	mut blocked_fn_keys := map[string]bool{}
	for fn_key, owner_fi in g.fn_owner_file {
		if owner_fi !in owned_files {
			blocked_fn_keys[fn_key] = true
		}
	}
	// Block globals owned by other workers to prevent duplicate definitions.
	for gname, owner_fi in g.global_owner_file {
		if owner_fi !in owned_files {
			worker_emitted['global_${gname}'] = true
		}
	}
	return &Gen{
		files:                  g.files
		flat:                   unsafe { g.flat }
		env:                    unsafe { g.env }
		pref:                   unsafe { g.pref }
		imported_symbols_index: g.imported_symbols_index.clone()
		v_method_return_index:  g.v_method_return_index.clone()
		ierror_base_index:      g.ierror_base_index.clone()
		fn_owner_file:          g.fn_owner_file.clone()
		sb:                     strings.new_builder(64_000)
		// Read-only lookup maps — clone to avoid COW data races
		fn_param_is_ptr:                       g.fn_param_is_ptr.clone()
		fn_param_types:                        g.fn_param_types.clone()
		fn_return_types:                       g.fn_return_types.clone()
		v_fn_return_types:                     g.v_fn_return_types.clone()
		struct_field_types:                    g.struct_field_types.clone()
		enum_value_to_enum:                    g.enum_value_to_enum.clone()
		enum_type_fields:                      g.enum_type_fields.clone()
		array_aliases:                         g.array_aliases.clone()
		map_aliases:                           g.map_aliases.clone()
		result_aliases:                        g.result_aliases.clone()
		option_aliases:                        g.option_aliases.clone()
		alias_base_types:                      g.alias_base_types.clone()
		fixed_array_fields:                    g.fixed_array_fields.clone()
		fixed_array_field_elem:                g.fixed_array_field_elem.clone()
		fixed_array_globals:                   g.fixed_array_globals.clone()
		fixed_array_ret_wrappers:              g.fixed_array_ret_wrappers.clone()
		tuple_aliases:                         g.tuple_aliases.clone()
		sum_type_variants:                     g.sum_type_variants.clone()
		embedded_field_owner:                  g.embedded_field_owner.clone()
		primitive_type_aliases:                g.primitive_type_aliases.clone()
		emit_modules:                          g.emit_modules.clone()
		type_modules:                          g.type_modules.clone()
		emit_files:                            g.emit_files.clone()
		emitted_result_structs:                g.emitted_result_structs.clone()
		emitted_option_structs:                g.emitted_option_structs.clone()
		interface_methods:                     g.interface_methods.clone()
		interface_data_fields:                 g.interface_data_fields.clone()
		emitted_interface_bodies:              g.emitted_interface_bodies.clone()
		interface_wrapper_specs:               g.interface_wrapper_specs.clone()
		ierror_wrapper_bases:                  g.ierror_wrapper_bases.clone()
		collected_fixed_array_types:           g.collected_fixed_array_types.clone()
		collected_map_types:                   g.collected_map_types.clone()
		c_file_fn_keys:                        g.c_file_fn_keys.clone()
		global_var_modules:                    g.global_var_modules.clone()
		global_var_types:                      g.global_var_types.clone()
		const_exprs:                           g.const_exprs.clone()
		const_types:                           g.const_types.clone()
		const_c_names:                         g.const_c_names.clone()
		module_storage_vars:                   g.module_storage_vars.clone()
		c_extern_module_storage:               g.c_extern_module_storage.clone()
		runtime_const_targets:                 g.runtime_const_targets.clone()
		export_const_symbols:                  g.export_const_symbols
		cache_bundle_name:                     g.cache_bundle_name
		cached_init_calls:                     g.cached_init_calls.clone()
		used_fn_keys:                          g.used_fn_keys.clone()
		force_emit_fn_names:                   g.force_emit_fn_names.clone()
		export_fn_names:                       g.export_fn_names.clone()
		called_fn_names:                       g.called_fn_names.clone()
		called_specialized_names:              g.called_specialized_names.clone()
		called_specialized_names_indexed:      g.called_specialized_names_indexed
		declared_fn_names:                     g.declared_fn_names.clone()
		should_emit_fn_decl_cache:             g.should_emit_fn_decl_cache.clone()
		generic_body_scan_cache:               g.generic_body_scan_cache.clone()
		fn_type_aliases:                       g.fn_type_aliases.clone()
		generic_spec_index:                    g.generic_spec_index.clone()
		generic_fn_decl_index:                 g.generic_fn_decl_index.clone()
		specialized_fn_bases:                  g.specialized_fn_bases.clone()
		specialized_receiver_methods:          g.specialized_receiver_methods.clone()
		specialized_receiver_method_ambiguous: g.specialized_receiver_method_ambiguous.clone()
		specialized_receiver_method_miss:      g.specialized_receiver_method_miss.clone()
		specialized_receiver_method_indexed:   g.specialized_receiver_method_indexed
		late_generic_specs:                    g.late_generic_specs.clone()
		generic_scan_called_names:             map[string]bool{}
		generic_struct_bindings:               g.generic_struct_bindings.clone()
		generic_struct_instances:              g.generic_struct_instances.clone()
		c_struct_types:                        g.c_struct_types.clone()
		typedef_c_types:                       g.typedef_c_types.clone()
		// Per-worker mutable state (starts fresh).
		// Each worker gets a unique tmp_counter offset to avoid name collisions
		// for generated trampolines (_bound_method_N, _bound_recv_N, etc.).
		tmp_counter:                   (worker_id + 1) * 100_000
		pass5_worker_id:               worker_id
		emitted_types:                 worker_emitted
		blocked_fn_keys:               blocked_fn_keys
		runtime_local_types:           map[string]string{}
		runtime_decl_types:            map[string]string{}
		runtime_fn_pointer_types:      map[string]types.Type{}
		cur_fn_returned_idents:        map[string]bool{}
		active_generic_types:          map[string]types.Type{}
		cur_fn_generic_params:         map[string]string{}
		cur_fn_scope_miss_key:         ''
		cur_import_modules:            map[string]string{}
		is_module_ident_cache:         map[string]bool{}
		not_local_var_cache:           map[string]bool{}
		resolved_module_names:         map[string]string{}
		cur_fn_mut_params:             map[string]bool{}
		cached_env_scopes:             map[string]voidptr{}
		selector_field_type_cache:     map[string]string{}
		selector_field_type_miss:      map[string]bool{}
		struct_field_lookup_cache:     map[string]string{}
		struct_field_lookup_miss:      map[string]bool{}
		struct_type_lookup_cache:      map[string]types.Struct{}
		struct_type_lookup_miss:       map[string]bool{}
		struct_decl_info_cache:        map[string]StructDeclInfo{}
		struct_decl_info_miss:         map[string]bool{}
		flat_struct_decl_exact:        g.flat_struct_decl_exact
		flat_struct_decl_short_by_mod: g.flat_struct_decl_short_by_mod
		flat_struct_decl_short:        g.flat_struct_decl_short
		flat_struct_decl_indexed:      g.flat_struct_decl_indexed
		alias_base_lookup_cache:       map[string]string{}
		alias_base_lookup_miss:        map[string]bool{}
		needed_interface_wrappers:     map[string]bool{}
		needed_ierror_wrapper_bases:   map[string]bool{}
		spawned_fns:                   map[string]bool{}
		exported_const_seen:           map[string]bool{}
		exported_const_symbols:        []ExportedConstSymbol{}
	}
}

// merge_pass5_worker merges a parallel worker's output into the main Gen.
pub fn (mut g Gen) merge_pass5_worker(w &Gen) {
	// Append worker's generated C code
	mut ww := unsafe { w }
	worker_output := ww.sb.str()
	if worker_output.len > 0 {
		g.sb.write_string(worker_output)
	}
	// Merge accumulator arrays
	g.anon_fn_defs << w.anon_fn_defs
	g.live_fns << w.live_fns
	if w.live_source_file.len > 0 {
		g.live_source_file = w.live_source_file
	}
	g.spawn_wrapper_defs << w.spawn_wrapper_defs
	g.trampoline_defs << w.trampoline_defs
	g.exported_const_symbols << w.exported_const_symbols
	g.pass5_file_times << w.pass5_file_times
	// Merge accumulator maps
	for k, v in w.needed_interface_wrappers {
		g.needed_interface_wrappers[k] = v
	}
	for k, v in w.needed_ierror_wrapper_bases {
		g.needed_ierror_wrapper_bases[k] = v
	}
	for k, v in w.called_fn_names {
		g.called_fn_names[k] = v
		if v {
			g.remember_called_specialized_name(k)
		}
	}
	// Merge emitted_types so post-pass dedup (e.g. array_contains fallbacks) works
	for k, v in w.emitted_types {
		g.emitted_types[k] = v
	}
}

fn is_c_identifier_like(name string) bool {
	if name.len == 0 {
		return false
	}
	for ch in name {
		if !(ch.is_letter() || ch.is_digit() || ch == `_`) {
			return false
		}
	}
	return true
}

fn type_decl_base_is_fn_type(node ast.TypeDecl) bool {
	if node.base_type is ast.Type {
		return node.base_type is ast.FnType
	}
	return false
}

fn (g &Gen) has_live_reload_functions() bool {
	if g.cache_bundle_name.len > 0 {
		return false
	}
	if g.pref != unsafe { nil } && g.pref.is_shared_lib {
		return false
	}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			if g.cursor_stmts_have_live_reload_function(g.flat.file_cursor(i).stmts()) {
				return true
			}
		}
		return false
	}
	for file in g.files {
		if g.stmts_have_live_reload_function(file.stmts) {
			return true
		}
	}
	return false
}

fn (g &Gen) cursor_stmts_have_live_reload_function(stmts ast.CursorList) bool {
	for i in 0 .. stmts.len() {
		if g.cursor_stmt_has_live_reload_function(stmts.at(i)) {
			return true
		}
	}
	return false
}

fn (g &Gen) cursor_stmt_has_live_reload_function(stmt ast.Cursor) bool {
	match stmt.kind() {
		.stmt_fn_decl {
			decl := stmt.fn_decl_signature()
			return decl.attributes.has('live') && decl.name != 'main'
		}
		.stmt_expr {
			return g.expr_cursor_has_live_reload_function(stmt.edge(0))
		}
		else {
			return false
		}
	}
}

fn (g &Gen) stmts_have_live_reload_function(stmts []ast.Stmt) bool {
	for stmt in stmts {
		if g.stmt_has_live_reload_function(stmt) {
			return true
		}
	}
	return false
}

fn (g &Gen) stmt_has_live_reload_function(stmt ast.Stmt) bool {
	if stmt is ast.FnDecl {
		return stmt.attributes.has('live') && stmt.name != 'main'
	}
	if stmt is ast.ExprStmt {
		return g.expr_has_live_reload_function(stmt.expr)
	}
	return false
}

fn (g &Gen) expr_cursor_has_live_reload_function(expr ast.Cursor) bool {
	if expr.kind() == .expr_comptime {
		inner := expr.edge(0)
		if inner.kind() == .expr_if {
			return g.active_comptime_if_cursor_has_live_reload_function(inner)
		}
		return g.expr_cursor_has_live_reload_function(inner)
	}
	return false
}

fn (g &Gen) expr_has_live_reload_function(expr ast.Expr) bool {
	if expr is ast.ComptimeExpr {
		if expr.expr is ast.IfExpr {
			return g.active_comptime_if_has_live_reload_function(expr.expr)
		}
		return g.expr_has_live_reload_function(expr.expr)
	}
	return false
}

fn (g &Gen) if_expr_cursor_body_has_live_reload_function(node ast.Cursor) bool {
	for i in 2 .. node.edge_count() {
		if g.cursor_stmt_has_live_reload_function(node.edge(i)) {
			return true
		}
	}
	return false
}

fn (g &Gen) active_comptime_if_cursor_has_live_reload_function(node ast.Cursor) bool {
	if g.eval_comptime_cond_cursor(node.edge(0)) {
		return g.if_expr_cursor_body_has_live_reload_function(node)
	}
	else_expr := node.edge(1)
	if else_expr.kind() == .expr_if {
		if else_expr.edge(0).kind() == .expr_empty {
			return g.if_expr_cursor_body_has_live_reload_function(else_expr)
		}
		return g.active_comptime_if_cursor_has_live_reload_function(else_expr)
	}
	return false
}

fn (g &Gen) active_comptime_if_has_live_reload_function(node ast.IfExpr) bool {
	if g.eval_comptime_cond(node.cond) {
		return g.stmts_have_live_reload_function(node.stmts)
	}
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			return g.stmts_have_live_reload_function(else_if.stmts)
		}
		return g.active_comptime_if_has_live_reload_function(else_if)
	}
	return false
}

fn (g &Gen) runtime_fn_referenced(name string) bool {
	return name in g.called_fn_names || 'builtin__${name}' in g.called_fn_names
}

fn (g &Gen) runtime_any_fn_referenced(names []string) bool {
	for name in names {
		if g.runtime_fn_referenced(name) {
			return true
		}
	}
	return false
}

fn (g &Gen) runtime_fallback_needed(name string) bool {
	return 'fn_${name}' !in g.emitted_types && g.runtime_fn_referenced(name)
}

fn c_is_ident_char(ch u8) bool {
	return ch.is_letter() || ch.is_digit() || ch == `_`
}

fn c_skip_spaces(csrc string, start int) int {
	mut i := start
	for i < csrc.len && csrc[i].is_space() {
		i++
	}
	return i
}

fn c_previous_non_space(csrc string, start int) int {
	mut i := start - 1
	for i >= 0 && csrc[i].is_space() {
		i--
	}
	return i
}

fn c_is_runtime_symbol_context(csrc string, start int, end int) bool {
	next := c_skip_spaces(csrc, end)
	prev := c_previous_non_space(csrc, start)
	if next < csrc.len && csrc[next] == `(` {
		return true
	}
	if prev >= 0 && csrc[prev] == `&` {
		return true
	}
	if next < csrc.len && csrc[next] in [`,`, `)`, `;`] && prev >= 0
		&& csrc[prev] in [`(`, `,`, `=`] {
		return true
	}
	return false
}

fn c_source_references_runtime_symbol(csrc string, name string) bool {
	mut i := 0
	for i < csrc.len {
		ch := csrc[i]
		if ch == `/` && i + 1 < csrc.len && csrc[i + 1] == `/` {
			i += 2
			for i < csrc.len && csrc[i] != `\n` {
				i++
			}
			continue
		}
		if ch == `/` && i + 1 < csrc.len && csrc[i + 1] == `*` {
			i += 2
			for i + 1 < csrc.len && !(csrc[i] == `*` && csrc[i + 1] == `/`) {
				i++
			}
			i += 2
			continue
		}
		if ch == `"` || ch == `'` {
			quote := ch
			i++
			for i < csrc.len {
				if csrc[i] == `\\` {
					i += 2
					continue
				}
				if csrc[i] == quote {
					i++
					break
				}
				i++
			}
			continue
		}
		if ch.is_letter() || ch == `_` {
			start := i
			for i < csrc.len && c_is_ident_char(csrc[i]) {
				i++
			}
			if csrc[start..i] == name {
				if c_is_runtime_symbol_context(csrc, start, i) {
					return true
				}
			}
			continue
		}
		i++
	}
	return false
}

fn (g &Gen) runtime_fallback_needed_in_source(name string, csrc string) bool {
	return 'fn_${name}' !in g.emitted_types
		&& (g.runtime_fn_referenced(name) || c_source_references_runtime_symbol(csrc, name))
}

fn freestanding_heap_runtime_helper_names() []string {
	return [
		'new_array_from_c_array',
		'new_array_from_c_array_no_alloc',
		'new_array_from_c_array_noscan',
		'new_array_from_array_and_c_array',
		'builtin__new_array_from_array_and_c_array',
		'__new_array_with_default_noscan',
		'new_map',
		'new_map_init',
		'new_map_init_noscan_key',
		'new_map_init_noscan_value',
		'new_map_init_noscan_key_value',
		'new_dense_array',
		'DenseArray__has_index',
		'DenseArray__key',
		'DenseArray__value',
		'DenseArray__zeros_to_end',
		'IError__str',
		'Array_int_contains',
		'Array_string_contains',
		'Array_string_index',
		'Array_int_str',
		'__v2_array_eq',
		'array__push',
		'array__push_many',
		'array__push_noscan',
		'array__eq',
		'array__insert',
		'array__insert_many',
		'array__prepend',
		'array__prepend_many',
		'array__delete',
		'array__delete_many',
		'array__clear',
		'array__slice',
		'array__slice_ni',
		'array__clone',
		'array__clone_to_depth',
		'array__repeat',
		'array__repeat_to_depth',
		'array__pop',
		'array__pop_left',
		'array__first',
		'array__get',
		'array__last',
		'array__contains',
		'array__sort',
		'array__sort_with_compare',
		'array__bytestr',
		'map__clear',
		'map__clone',
		'map__delete',
		'map__exists',
		'map__free',
		'map__get',
		'map__get_and_set',
		'map__get_check',
		'map__has_index',
		'map__key',
		'map__move',
		'map__reserve',
		'map__set',
		'map__value',
		'map__values',
		'map__keys',
		'string__clone',
		'string__contains',
		'string__eq',
		'string__ends_with',
		'string__free',
		'string__index',
		'string__lt',
		'string__compare',
		'string__plus',
		'string__plus_two',
		'string__plus_many',
		'string__repeat',
		'string__runes',
		'string__split',
		'string__starts_with',
		'string__str',
		'string__substr',
		'string__substr_unsafe',
		'string__substr_or',
		'string__to_lower',
		'string__bytes',
		'compare_strings',
		'compare_strings_by_len',
		'compare_lower_strings',
		'int__str',
		'strings__new_builder',
		'strings__Builder__write_string',
		'strings__Builder__str',
	]
}

fn (mut g Gen) emit_freestanding_missing_heap_runtime_helper(helper string) {
	g.sb.writeln('')
	g.sb.writeln('_Static_assert(0, "${freestanding_missing_heap_runtime_message}: ${helper}");')
}

fn (g &Gen) freestanding_missing_heap_runtime_helpers(csrc string) []string {
	if !g.is_freestanding_target() || g.pref == unsafe { nil } || !g.pref.skip_builtin {
		return []
	}
	mut missing := []string{}
	for name in freestanding_heap_runtime_helper_names() {
		if g.runtime_fallback_needed_in_source(name, csrc) {
			missing << name
		}
	}
	return missing
}

fn (mut g Gen) emit_missing_runtime_fallbacks() {
	generated_csrc := g.sb.after(0)
	need_at_least_one_fallback := g.runtime_fallback_needed_in_source('__at_least_one',
		generated_csrc)
	need_arguments_fallback := g.runtime_fallback_needed('arguments')
	need_eprint_fallback := g.runtime_fallback_needed('eprint')
	need_writeln_to_fd_fallback := 'fn__writeln_to_fd' !in g.emitted_types
		&& (g.runtime_fn_referenced('_writeln_to_fd')
		|| g.runtime_any_fn_referenced(['println', 'eprintln']))
	need_write_buf_to_fd_fallback := 'fn__write_buf_to_fd' !in g.emitted_types
		&& (g.runtime_fn_referenced('_write_buf_to_fd')
		|| need_writeln_to_fd_fallback || need_eprint_fallback
		|| g.runtime_any_fn_referenced(['print', 'println', 'eprintln']))
	need_flush_stdout_fallback := 'fn_flush_stdout' !in g.emitted_types
		&& (g.runtime_fn_referenced('flush_stdout') || need_eprint_fallback)
	need_flush_stderr_fallback := 'fn_flush_stderr' !in g.emitted_types
		&& (g.runtime_fn_referenced('flush_stderr') || need_eprint_fallback)
	need_v_panic_fallback := 'fn_v_panic' !in g.emitted_types && (g.runtime_fn_referenced('panic')
		|| g.runtime_fallback_needed_in_source('v_panic', generated_csrc))
	mut need_array_int_contains_fallback := g.runtime_fallback_needed_in_source('Array_int_contains',
		generated_csrc)
	mut need_array_string_contains_fallback := g.runtime_fallback_needed_in_source('Array_string_contains',
		generated_csrc)
	mut need_array_string_index_fallback := g.runtime_fallback_needed_in_source('Array_string_index',
		generated_csrc)
	mut need_array_int_str_fallback := g.runtime_fallback_needed_in_source('Array_int_str',
		generated_csrc)
	mut need_densearray_zeros_to_end_fallback := g.runtime_fallback_needed_in_source('DenseArray__zeros_to_end',
		generated_csrc)
	need_sort_cmp_int_fallback := g.runtime_fallback_needed_in_source('__sort_cmp_int_asc',
		generated_csrc)
	need_sort_cmp_repindex_fallback := g.runtime_fallback_needed_in_source('__sort_cmp_RepIndex_by_idx_asc',
		generated_csrc)
	need_bits_leading_zeros_64_fallback := g.runtime_fallback_needed_in_source('bits__leading_zeros_64',
		generated_csrc)
	need_bits_trailing_zeros_32_fallback := g.runtime_fallback_needed_in_source('bits__trailing_zeros_32',
		generated_csrc)
	need_bits_trailing_zeros_64_fallback := g.runtime_fallback_needed_in_source('bits__trailing_zeros_64',
		generated_csrc)
	need_bits_rotate_left_32_fallback := g.runtime_fallback_needed_in_source('bits__rotate_left_32',
		generated_csrc)
	need_memdup_fallback := g.runtime_fallback_needed_in_source('memdup', generated_csrc)
	need_f64_abs_fallback := g.runtime_fallback_needed_in_source('f64_abs', generated_csrc)
	need_f32_str_fallback := g.runtime_fallback_needed_in_source('f32__str', generated_csrc)
	need_f32_strg_fallback := g.runtime_fallback_needed_in_source('f32__strg', generated_csrc)
	need_f64_strg_fallback := g.runtime_fallback_needed_in_source('f64__strg', generated_csrc)
		|| ('fn_f64__strg' !in g.emitted_types && need_f32_strg_fallback)
	need_f64_str_fallback := g.runtime_fallback_needed_in_source('f64__str', generated_csrc)
		|| ('fn_f64__str' !in g.emitted_types && (need_f32_str_fallback || need_f64_strg_fallback))
	need_malloc_noscan_fallback :=
		g.runtime_fallback_needed_in_source('malloc_noscan', generated_csrc)
		|| need_memdup_fallback
		|| (need_f64_str_fallback && !g.is_freestanding_target())
	for helper in g.freestanding_missing_heap_runtime_helpers(generated_csrc) {
		g.emit_freestanding_missing_heap_runtime_helper(helper)
	}
	if g.is_freestanding_target() && g.pref != unsafe { nil } && g.pref.skip_builtin {
		need_array_int_contains_fallback = false
		need_array_string_contains_fallback = false
		need_array_string_index_fallback = false
		need_array_int_str_fallback = false
		need_densearray_zeros_to_end_fallback = false
	}
	if need_at_least_one_fallback {
		g.emitted_types['fn___at_least_one'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) u64 __at_least_one(u64 how_many) {')
		g.sb.writeln('\treturn how_many == 0 ? 1 : how_many;')
		g.sb.writeln('}')
	}
	if need_arguments_fallback {
		g.emitted_types['fn_arguments'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) Array_string arguments() {')
		if g.is_freestanding_target() {
			g.sb.writeln('\t_Static_assert(0, "${freestanding_missing_alloc_hook_message}");')
			g.sb.writeln('\treturn (Array_string){0};')
		} else {
			g.sb.writeln('\tu8** argv = (u8**)g_main_argv;')
			g.sb.writeln('\tArray_string res = __new_array_with_default_noscan(0, g_main_argc, sizeof(string), NULL);')
			g.sb.writeln('\tfor (int i = 0; i < g_main_argc; i += 1) {')
			g.sb.writeln('\t\tarray__push((array*)&res, &(string[1]){tos_clone(argv[i])});')
			g.sb.writeln('\t}')
			g.sb.writeln('\treturn res;')
		}
		g.sb.writeln('}')
	}
	if need_write_buf_to_fd_fallback {
		g.emitted_types['fn__write_buf_to_fd'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void _write_buf_to_fd(int fd, u8* buf, int buf_len) {')
		g.sb.writeln('\tif (buf_len <= 0) {')
		g.sb.writeln('\t\treturn;')
		g.sb.writeln('\t}')
		if g.is_freestanding_target() && !g.has_freestanding_hook_capability('output') {
			g.sb.writeln('\t_Static_assert(0, "${freestanding_missing_output_hook_message}");')
			g.sb.writeln('\treturn;')
		} else {
			g.sb.writeln('\tu8* ptr = buf;')
			g.sb.writeln('\tisize remaining_bytes = buf_len;')
			g.sb.writeln('\twhile (remaining_bytes > 0) {')
			if g.has_freestanding_hook_capability('output') {
				g.sb.writeln('\t\tisize written = v_platform_write(fd, ptr, remaining_bytes);')
			} else if g.target_os_name() == 'windows' {
				g.sb.writeln('\t\tHANDLE handle = GetStdHandle(fd == 2 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);')
				g.sb.writeln('\t\tDWORD win_written = 0;')
				g.sb.writeln('\t\tisize written = 0;')
				g.sb.writeln('\t\tif (handle != NULL && handle != INVALID_HANDLE_VALUE && WriteFile(handle, ptr, (DWORD)remaining_bytes, &win_written, NULL)) {')
				g.sb.writeln('\t\t\twritten = (isize)win_written;')
				g.sb.writeln('\t\t}')
			} else if g.target_os_name() == 'cross' {
				g.sb.writeln('#if defined(_WIN32)')
				g.sb.writeln('\t\tHANDLE handle = GetStdHandle(fd == 2 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);')
				g.sb.writeln('\t\tDWORD win_written = 0;')
				g.sb.writeln('\t\tisize written = 0;')
				g.sb.writeln('\t\tif (handle != NULL && handle != INVALID_HANDLE_VALUE && WriteFile(handle, ptr, (DWORD)remaining_bytes, &win_written, NULL)) {')
				g.sb.writeln('\t\t\twritten = (isize)win_written;')
				g.sb.writeln('\t\t}')
				g.sb.writeln('#else')
				g.sb.writeln('\t\tisize written = write(fd, ptr, remaining_bytes);')
				g.sb.writeln('#endif')
			} else {
				g.sb.writeln('\t\tisize written = write(fd, ptr, remaining_bytes);')
			}
			g.sb.writeln('\t\tif (written <= 0) {')
			g.sb.writeln('\t\t\treturn;')
			g.sb.writeln('\t\t}')
			g.sb.writeln('\t\tptr += written;')
			g.sb.writeln('\t\tremaining_bytes -= written;')
			g.sb.writeln('\t}')
		}
		g.sb.writeln('}')
	}
	if need_writeln_to_fd_fallback {
		g.emitted_types['fn__writeln_to_fd'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void _writeln_to_fd(int fd, string s) {')
		g.sb.writeln("\tu8 lf = '\\n';")
		g.sb.writeln('\t_write_buf_to_fd(fd, s.str, s.len);')
		g.sb.writeln('\t_write_buf_to_fd(fd, &lf, 1);')
		g.sb.writeln('}')
	}
	if need_array_int_contains_fallback {
		g.emitted_types['fn_Array_int_contains'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) bool Array_int_contains(Array_int a, int v) {')
		g.sb.writeln('\tfor (int i = 0; i < a.len; i += 1) {')
		g.sb.writeln('\t\tif (*((int*)array__get(a, i)) == v) {')
		g.sb.writeln('\t\t\treturn true;')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn false;')
		g.sb.writeln('}')
	}
	if need_array_string_contains_fallback {
		g.emitted_types['fn_Array_string_contains'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) bool Array_string_contains(Array_string a, string v) {')
		g.sb.writeln('\tfor (int i = 0; i < a.len; i += 1) {')
		g.sb.writeln('\t\tif (string__eq(*((string*)array__get(a, i)), v)) {')
		g.sb.writeln('\t\t\treturn true;')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn false;')
		g.sb.writeln('}')
	}
	if need_array_string_index_fallback {
		g.emitted_types['fn_Array_string_index'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int Array_string_index(Array_string a, string v) {')
		g.sb.writeln('\tfor (int i = 0; i < a.len; i += 1) {')
		g.sb.writeln('\t\tif (string__eq(*((string*)array__get(a, i)), v)) {')
		g.sb.writeln('\t\t\treturn i;')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn -1;')
		g.sb.writeln('}')
	}
	if need_array_int_str_fallback {
		g.emitted_types['fn_Array_int_str'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string Array_int_str(Array_int a) {')
		g.sb.writeln('\tstrings__Builder sb = strings__new_builder(32);')
		g.sb.writeln('\tstrings__Builder__write_string(&sb, (string){.str = "[", .len = 1, .is_lit = 1});')
		g.sb.writeln('\tfor (int i = 0; i < a.len; i += 1) {')
		g.sb.writeln('\t\tif (i > 0) {')
		g.sb.writeln('\t\t\tstrings__Builder__write_string(&sb, (string){.str = ", ", .len = 2, .is_lit = 1});')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t\tstrings__Builder__write_string(&sb, int__str(*((int*)array__get(a, i))));')
		g.sb.writeln('\t}')
		g.sb.writeln('\tstrings__Builder__write_string(&sb, (string){.str = "]", .len = 1, .is_lit = 1});')
		g.sb.writeln('\treturn strings__Builder__str(&sb);')
		g.sb.writeln('}')
	}
	if need_densearray_zeros_to_end_fallback {
		g.emitted_types['fn_DenseArray__zeros_to_end'] = true
		tmp_value_alloc := g.c_heap_malloc_call('d->value_bytes')
		tmp_key_alloc := g.c_heap_malloc_call('d->key_bytes')
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void DenseArray__zeros_to_end(DenseArray* d) {')
		g.sb.writeln('\tvoid* tmp_value = ${tmp_value_alloc};')
		g.sb.writeln('\tvoid* tmp_key = ${tmp_key_alloc};')
		g.sb.writeln('\tint count = 0;')
		g.sb.writeln('\tfor (int i = 0; i < d->len; i += 1) {')
		g.sb.writeln('\t\tif (DenseArray__has_index(d, i)) {')
		g.sb.writeln('\t\t\tif (count != i) {')
		g.sb.writeln('\t\t\t\tmemcpy(tmp_key, DenseArray__key(d, count), d->key_bytes);')
		g.sb.writeln('\t\t\t\tmemcpy(DenseArray__key(d, count), DenseArray__key(d, i), d->key_bytes);')
		g.sb.writeln('\t\t\t\tmemcpy(DenseArray__key(d, i), tmp_key, d->key_bytes);')
		g.sb.writeln('\t\t\t\tmemcpy(tmp_value, DenseArray__value(d, count), d->value_bytes);')
		g.sb.writeln('\t\t\t\tmemcpy(DenseArray__value(d, count), DenseArray__value(d, i), d->value_bytes);')
		g.sb.writeln('\t\t\t\tmemcpy(DenseArray__value(d, i), tmp_value, d->value_bytes);')
		g.sb.writeln('\t\t\t}')
		g.sb.writeln('\t\t\tcount += 1;')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t}')
		g.sb.writeln('\t${g.c_heap_free_call('tmp_value')};')
		g.sb.writeln('\t${g.c_heap_free_call('tmp_key')};')
		g.sb.writeln('\td->deletes = 0;')
		g.sb.writeln('\t${g.c_heap_free_call('d->all_deleted')};')
		g.sb.writeln('\td->len = count;')
		g.sb.writeln('\tint old_cap = d->cap;')
		g.sb.writeln('\td->cap = count < 8 ? 8 : count;')
		if g.has_freestanding_hook_capability('alloc') || g.is_freestanding_target() {
			g.sb.writeln('\td->values = ${g.c_heap_realloc_call('d->values',
				'd->value_bytes * d->cap')};')
			g.sb.writeln('\td->keys = ${g.c_heap_realloc_call('d->keys', 'd->key_bytes * d->cap')};')
		} else {
			g.sb.writeln('\td->values = realloc_data(d->values, d->value_bytes * old_cap, d->value_bytes * d->cap);')
			g.sb.writeln('\td->keys = realloc_data(d->keys, d->key_bytes * old_cap, d->key_bytes * d->cap);')
		}
		g.sb.writeln('}')
	}
	if need_sort_cmp_int_fallback {
		g.emitted_types['fn___sort_cmp_int_asc'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int __sort_cmp_int_asc(int* a, int* b) {')
		g.sb.writeln('\tif (*a < *b) return -1;')
		g.sb.writeln('\tif (*a > *b) return 1;')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}
	if need_sort_cmp_repindex_fallback {
		g.emitted_types['fn___sort_cmp_RepIndex_by_idx_asc'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int __sort_cmp_RepIndex_by_idx_asc(RepIndex* a, RepIndex* b) {')
		g.sb.writeln('\tif (a->idx < b->idx) return -1;')
		g.sb.writeln('\tif (a->idx > b->idx) return 1;')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}
	if need_bits_leading_zeros_64_fallback {
		g.emitted_types['fn_bits__leading_zeros_64'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int bits__leading_zeros_64(u64 x) {')
		g.sb.writeln('\tif (x == 0) {')
		g.sb.writeln('\t\treturn 64;')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn __builtin_clzll(x);')
		g.sb.writeln('}')
	}
	if need_bits_trailing_zeros_32_fallback {
		g.emitted_types['fn_bits__trailing_zeros_32'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int bits__trailing_zeros_32(u32 x) {')
		g.sb.writeln('\tif (x == 0) {')
		g.sb.writeln('\t\treturn 32;')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn __builtin_ctz(x);')
		g.sb.writeln('}')
	}
	if need_bits_trailing_zeros_64_fallback {
		g.emitted_types['fn_bits__trailing_zeros_64'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int bits__trailing_zeros_64(u64 x) {')
		g.sb.writeln('\tif (x == 0) {')
		g.sb.writeln('\t\treturn 64;')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn __builtin_ctzll(x);')
		g.sb.writeln('}')
	}
	if need_bits_rotate_left_32_fallback {
		g.emitted_types['fn_bits__rotate_left_32'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) u32 bits__rotate_left_32(u32 x, int k) {')
		g.sb.writeln('\tu32 s = ((u32)k) & 31;')
		g.sb.writeln('\treturn (x << s) | (x >> ((32 - s) & 31));')
		g.sb.writeln('}')
	}
	if need_eprint_fallback {
		g.emitted_types['fn_eprint'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void eprint(string s) {')
		g.sb.writeln('\tflush_stdout();')
		g.sb.writeln('\tflush_stderr();')
		g.sb.writeln('\t_write_buf_to_fd(2, s.str, s.len);')
		g.sb.writeln('\tflush_stderr();')
		g.sb.writeln('}')
	}
	if need_flush_stdout_fallback {
		g.emitted_types['fn_flush_stdout'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void flush_stdout() {')
		if g.is_freestanding_target() && !g.has_freestanding_hook_capability('output') {
			g.sb.writeln('\t_Static_assert(0, "${freestanding_missing_output_hook_message}");')
		} else if !g.has_freestanding_hook_capability('output') {
			g.sb.writeln('\tfflush(stdout);')
		}
		g.sb.writeln('}')
	}
	if need_flush_stderr_fallback {
		g.emitted_types['fn_flush_stderr'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void flush_stderr() {')
		if g.is_freestanding_target() && !g.has_freestanding_hook_capability('output') {
			g.sb.writeln('\t_Static_assert(0, "${freestanding_missing_output_hook_message}");')
		} else if !g.has_freestanding_hook_capability('output') {
			g.sb.writeln('\tfflush(stderr);')
		}
		g.sb.writeln('}')
	}
	if need_malloc_noscan_fallback {
		g.emitted_types['fn_malloc_noscan'] = true
		malloc_noscan_alloc := g.c_heap_malloc_call('n')
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) u8* malloc_noscan(isize n) {')
		g.sb.writeln('\treturn (u8*)${malloc_noscan_alloc};')
		g.sb.writeln('}')
	}
	if need_memdup_fallback {
		g.emitted_types['fn_memdup'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void* memdup(void* src, isize sz) {')
		g.sb.writeln('\tif (sz <= 0) {')
		g.sb.writeln('\t\treturn NULL;')
		g.sb.writeln('\t}')
		if g.has_freestanding_hook_capability('alloc') || g.is_freestanding_target() {
			memdup_alloc := g.c_heap_malloc_call('sz')
			g.sb.writeln('\tvoid* res = ${memdup_alloc};')
		} else {
			g.sb.writeln('\tvoid* res = malloc_noscan(sz);')
		}
		g.sb.writeln('\tmemcpy(res, src, sz);')
		g.sb.writeln('\treturn res;')
		g.sb.writeln('}')
	}
	if need_v_panic_fallback && g.has_freestanding_hook_capability('panic') {
		g.emitted_types['fn_v_panic'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void v_panic(string s) {')
		g.sb.writeln('\tv_platform_panic(s.str, s.len);')
		g.sb.writeln('\tfor (;;) {}')
		g.sb.writeln('}')
	} else if need_v_panic_fallback && g.is_freestanding_target() {
		g.emitted_types['fn_v_panic'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void v_panic(string s) {')
		g.sb.writeln('\t_Static_assert(0, "${freestanding_missing_panic_hook_message}");')
		g.sb.writeln('\tfor (;;) {}')
		g.sb.writeln('}')
	}
	if need_f64_abs_fallback {
		g.emitted_types['fn_f64_abs'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) f64 f64_abs(f64 a) {')
		g.sb.writeln('\treturn a < 0 ? -a : a;')
		g.sb.writeln('}')
	}
	if need_f64_str_fallback {
		g.emitted_types['fn_f64__str'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string f64__str(f64 x) {')
		if g.is_freestanding_target() {
			g.sb.writeln('\treturn ${g.c_freestanding_missing_format_string_expr()};')
		} else {
			g.sb.writeln('\tchar buf[64];')
			g.sb.writeln('\tint len = snprintf(buf, sizeof(buf), "%.15g", x);')
			g.sb.writeln('\tif (len < 0) {')
			g.sb.writeln('\t\treturn (string){.str = (u8*)"", .len = 0, .is_lit = 1};')
			g.sb.writeln('\t}')
			g.sb.writeln('\tif (len >= (int)sizeof(buf)) {')
			g.sb.writeln('\t\tlen = (int)sizeof(buf) - 1;')
			g.sb.writeln('\t}')
			g.sb.writeln('\tu8* out = (u8*)malloc_noscan(len + 1);')
			g.sb.writeln('\tmemcpy(out, buf, len);')
			g.sb.writeln('\tout[len] = 0;')
			g.sb.writeln('\treturn (string){.str = out, .len = len, .is_lit = 0};')
		}
		g.sb.writeln('}')
	}
	if need_f64_strg_fallback {
		g.emitted_types['fn_f64__strg'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string f64__strg(f64 x) {')
		g.sb.writeln('\treturn f64__str(x);')
		g.sb.writeln('}')
	}
	if need_f32_str_fallback {
		g.emitted_types['fn_f32__str'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string f32__str(f32 x) {')
		g.sb.writeln('\treturn f64__str((f64)x);')
		g.sb.writeln('}')
	}
	if need_f32_strg_fallback {
		g.emitted_types['fn_f32__strg'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string f32__strg(f32 x) {')
		g.sb.writeln('\treturn f64__strg((f64)x);')
		g.sb.writeln('}')
	}
}

fn is_c_runtime_function(name string) bool {
	return name in ['free', 'malloc', 'realloc', 'calloc', 'memcmp', 'memcpy', 'memmove', 'memset',
		'memchr', 'memmem', 'strlen', 'strcmp', 'strncmp', 'snprintf', 'sprintf', 'printf', 'fprintf',
		'asprintf', 'atoi', 'atoll', 'atof', 'qsort', 'popen', 'realpath', 'chmod', 'exit',
		'backtrace_symbols', 'backtrace_symbols_fd', 'proc_pidpath', 'wyhash', 'wyhash64']
}

fn shallow_copy_exprs(exprs []ast.Expr) []ast.Expr {
	mut out := []ast.Expr{cap: exprs.len}
	for i in 0 .. exprs.len {
		out << exprs[i]
	}
	return out
}

const c_keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double',
	'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long', 'register',
	'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
	'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex', '_Imaginary', 'unix',
	'linux']

const c_stdlib_fns = ['malloc', 'calloc', 'realloc', 'free', 'atoi', 'atof', 'atol', 'memcpy',
	'memset', 'memmove', 'strlen', 'strcpy', 'strcat', 'strcmp', 'memcmp', 'exit']

fn escape_c_keyword(name string) string {
	// V uses @keyword to escape reserved identifiers (e.g. @type -> type).
	// Strip the @ prefix first, then check if it's a C keyword.
	n := if name.len > 0 && name[0] == `@` { name[1..] } else { name }
	if n in c_keywords {
		return '_${n}'
	}
	return n
}

fn c_local_name(name string) string {
	if name == 'array' {
		return '_v_array'
	}
	return escape_c_keyword(name)
}

fn sanitize_fn_ident(name string) string {
	return match name {
		'+' { 'op_plus' }
		'-' { 'op_minus' }
		'*' { 'op_mul' }
		'/' { 'op_div' }
		'%' { 'op_mod' }
		'==' { 'op_eq' }
		'!=' { 'op_ne' }
		'<' { 'op_lt' }
		'>' { 'op_gt' }
		'<=' { 'op_le' }
		'>=' { 'op_ge' }
		'|' { 'op_pipe' }
		'^' { 'op_xor' }
		else { name }
	}
}

fn legacy_operator_fn_ident(name string) string {
	return match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'%' { 'mod' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'>' { 'gt' }
		'<=' { 'le' }
		'>=' { 'ge' }
		'|' { 'pipe' }
		'^' { 'xor' }
		else { name }
	}
}

fn (mut g Gen) is_module_ident(name string) bool {
	if cached := g.is_module_ident_cache[name] {
		return cached
	}
	mut result := false
	if g.cur_fn_scope != unsafe { nil } {
		if obj := g.cur_fn_scope.lookup_parent(name, 0) {
			result = obj is types.Module
			g.is_module_ident_cache[name] = result
			return result
		}
	}
	if name in g.cur_import_modules {
		g.is_module_ident_cache[name] = true
		return true
	}
	if g.env != unsafe { nil } {
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(name, 0) {
				result = obj is types.Module
			}
		}
	}
	g.is_module_ident_cache[name] = result
	return result
}

// resolve_module_name resolves a module alias to its real module name.
// Returns the input name unchanged if it's not a module or can't be resolved.
fn (mut g Gen) resolve_module_name(name string) string {
	// Check cache first (module name resolution is constant per-function).
	if cached := g.resolved_module_names[name] {
		return cached
	}
	mut result := name
	if g.cur_fn_scope != unsafe { nil } {
		if obj := g.cur_fn_scope.lookup_parent(name, 0) {
			if obj is types.Module {
				mod := unsafe { &types.Module(&obj) }
				if mod.name != '' {
					result = mod.name
				}
			}
			g.resolved_module_names[name] = result
			return result
		}
	}
	if resolved := g.cur_import_modules[name] {
		result = resolved
		g.resolved_module_names[name] = result
		return result
	}
	if g.env != unsafe { nil } {
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Module {
					mod := unsafe { &types.Module(&obj) }
					if mod.name != '' {
						result = mod.name
					}
				}
			}
		}
		g.resolved_module_names[name] = result
		return result
	}
	g.resolved_module_names[name] = result
	return result
}

fn (g &Gen) is_current_or_imported_module(module_name string) bool {
	if module_name == g.cur_module {
		return true
	}
	for _, imported_module in g.cur_import_modules {
		if imported_module == module_name {
			return true
		}
	}
	return false
}

fn sanitize_c_number_literal(lit string) string {
	mut s := lit
	if s.contains('_') {
		s = s.replace('_', '')
	}
	// Convert V octal prefix 0o to C octal prefix 0
	if s.starts_with('0o') || s.starts_with('0O') {
		s = '0${s[2..]}'
	}
	return s
}

fn strip_literal_quotes(raw string) string {
	if raw.len < 2 {
		return raw
	}
	first := raw[0]
	last := raw[raw.len - 1]
	if first == last && first in [`'`, `"`] {
		return raw[1..raw.len - 1]
	}
	return raw
}

// process_line_continuations strips V line continuation sequences from string content.
// A `\` immediately before a newline (0x0a) in V source means line continuation:
// the `\`, the newline, and any leading whitespace on the next line are removed.
fn process_line_continuations(val string) string {
	mut i := 0
	for i < val.len {
		if val[i] == `\\` && i + 1 < val.len && val[i + 1] == `\n` {
			// Found `\` + newline: strip `\`, newline, and leading whitespace
			mut sb := strings.new_builder(val.len)
			sb.write_string(val[..i])
			mut j := i + 2 // skip `\` and newline
			for j < val.len && val[j] in [` `, `\t`] {
				j++
			}
			sb.write_string(val[j..])
			// Process recursively for multiple continuations
			return process_line_continuations(sb.str())
		}
		// Skip past V escape sequences (e.g., `\n`, `\t`, `\\`)
		if val[i] == `\\` && i + 1 < val.len {
			i += 2
			continue
		}
		i++
	}
	return val
}

fn escape_char_literal_content(raw string) string {
	mut sb := strings.new_builder(raw.len + 4)
	for ch in raw {
		if ch == `'` {
			sb.write_u8(`\\`)
			sb.write_u8(`'`)
		} else {
			sb.write_u8(ch)
		}
	}
	return sb.str()
}

fn is_c_hex_digit(ch u8) bool {
	return (ch >= `0` && ch <= `9`) || (ch >= `a` && ch <= `f`) || (ch >= `A` && ch <= `F`)
}

fn hex_nibble_value(ch u8) u32 {
	if ch >= `0` && ch <= `9` {
		return u32(ch - `0`)
	}
	if ch >= `a` && ch <= `f` {
		return u32(ch - `a`) + 10
	}
	if ch >= `A` && ch <= `F` {
		return u32(ch - `A`) + 10
	}
	return 0
}

fn escape_c_string_literal_segment(raw string) string {
	mut sb := strings.new_builder(raw.len + 8)
	mut i := 0
	for i < raw.len {
		ch := raw[i]
		// V scanner stores escape sequences as raw pairs: `\` + char.
		// Process them as units to avoid breaking `\\` + `"` sequences.
		if ch == `\\` && i + 1 < raw.len {
			next := raw[i + 1]
			if next == `"` {
				// V escape \" → emit C escape \" (same representation)
				sb.write_u8(`\\`)
				sb.write_u8(`"`)
				i += 2
				continue
			}
			if next == `x` && i + 3 < raw.len && is_c_hex_digit(raw[i + 2])
				&& is_c_hex_digit(raw[i + 3]) {
				sb.write_string(raw[i..i + 4])
				i += 4
				// C hex escapes are greedy, so split adjacent literals before the
				// next hex digit to keep the escape length fixed at two digits.
				if i < raw.len && is_c_hex_digit(raw[i]) {
					sb.write_string('""')
				}
				continue
			}
			// V \uXXXX universal char → emit UTF-8 byte sequence as \xNN escapes.
			// C99/C11 forbid universal character names for code points below 0xA0
			// (except $, @, `), so we can't pass them through to C verbatim.
			if next == `u` && i + 5 < raw.len && is_c_hex_digit(raw[i + 2])
				&& is_c_hex_digit(raw[i + 3]) && is_c_hex_digit(raw[i + 4])
				&& is_c_hex_digit(raw[i + 5]) {
				cp := u32(hex_nibble_value(raw[i + 2])) << 12 | u32(hex_nibble_value(raw[i + 3])) << 8 | u32(hex_nibble_value(raw[
					i + 4])) << 4 | u32(hex_nibble_value(raw[i + 5]))
				if cp < 0x80 {
					sb.write_string('\\x${cp:02x}')
				} else if cp < 0x800 {
					b0 := 0xC0 | (cp >> 6)
					b1 := 0x80 | (cp & 0x3F)
					sb.write_string('\\x${b0:02x}\\x${b1:02x}')
				} else {
					b0 := 0xE0 | (cp >> 12)
					b1 := 0x80 | ((cp >> 6) & 0x3F)
					b2 := 0x80 | (cp & 0x3F)
					sb.write_string('\\x${b0:02x}\\x${b1:02x}\\x${b2:02x}')
				}
				i += 6
				if i < raw.len && is_c_hex_digit(raw[i]) {
					sb.write_string('""')
				}
				continue
			}
			// All other V escapes (\n, \t, \\, \0, etc.) → pass through as-is
			sb.write_u8(ch)
			sb.write_u8(next)
			i += 2
			continue
		}
		if ch == `"` {
			// Unescaped " (e.g. from single-quoted V strings) → escape for C
			sb.write_u8(`\\`)
			sb.write_u8(`"`)
		} else if ch == `\r` {
			sb.write_u8(`\\`)
			sb.write_u8(`r`)
		} else {
			sb.write_u8(ch)
		}
		i++
	}
	return sb.str()
}

// c_string_literal_content_to_c converts raw bytes into a valid C string literal.
// Multiline content is emitted as adjacent literals, separated by real newlines:
// "line1\n"
// "line2"
fn c_string_literal_content_to_c(raw string) string {
	newline_idx := raw.index('\n') or { -1 }
	if newline_idx < 0 {
		escaped := escape_c_string_literal_segment(raw)
		return '"${escaped}"'
	}
	mut sb := strings.new_builder(raw.len + 32)
	mut start := 0
	for i := 0; i < raw.len; i++ {
		if raw[i] != `\n` {
			continue
		}
		escaped := escape_c_string_literal_segment(raw[start..i])
		sb.write_u8(`"`)
		sb.write_string(escaped)
		sb.write_string('\\n"')
		sb.write_u8(`\n`)
		start = i + 1
	}
	escaped_tail := escape_c_string_literal_segment(raw[start..])
	sb.write_u8(`"`)
	sb.write_string(escaped_tail)
	sb.write_u8(`"`)
	return sb.str()
}

fn c_v_string_expr_from_ptr_len(ptr_expr string, len_expr string, is_lit bool) string {
	return '(string){.str = ${ptr_expr}, .len = ${len_expr}, .is_lit = ${if is_lit {
		1
	} else {
		0
	}}}'
}

fn c_static_v_string_expr_from_c_literal(c_lit string) string {
	return c_v_string_expr_from_ptr_len(c_lit, 'sizeof(${c_lit}) - 1', true)
}

fn mangle_alias_component(name string) string {
	mut s := name.replace('*', 'ptr')
	s = s.replace('&', 'ref')
	s = s.replace('@', 'at_')
	s = s.replace(' ', '_')
	s = s.replace('.', '__')
	return s
}

fn zero_value_for_type(t string) string {
	trimmed := t.trim_space()
	if trimmed == '' {
		return '0'
	}
	if trimmed == 'string' {
		return c_empty_v_string_expr()
	}
	if trimmed.ends_with('*') {
		return '0'
	}
	if trimmed in primitive_types || trimmed in ['void*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
		return '0'
	}
	return '((${trimmed}){0})'
}

fn split_top_level_csv(s string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	for i, ch in s {
		if ch == `(` || ch == `[` || ch == `<` {
			depth++
		} else if ch == `)` || ch == `]` || ch == `>` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			part := s[start..i].trim_space()
			if part != '' {
				parts << part
			}
			start = i + 1
		}
	}
	last := s[start..].trim_space()
	if last != '' {
		parts << last
	}
	return parts
}

fn (mut g Gen) gen_keyword(node ast.Keyword) {
	match node.tok {
		.key_nil {
			g.sb.write_string('NULL')
		}
		.key_none {
			g.sb.write_string('0')
		}
		.key_true {
			g.sb.write_string('true')
		}
		.key_false {
			g.sb.write_string('false')
		}
		.key_struct {
			g.sb.write_string('struct')
		}
		else {
			g.sb.write_string('0')
		}
	}
}

fn strip_expr_wrappers(expr ast.Expr) ast.Expr {
	return match expr {
		ast.ParenExpr {
			strip_expr_wrappers(expr.expr)
		}
		ast.ModifierExpr {
			strip_expr_wrappers(expr.expr)
		}
		ast.CastExpr {
			strip_expr_wrappers(expr.expr)
		}
		ast.AsCastExpr {
			strip_expr_wrappers(expr.expr)
		}
		else {
			expr
		}
	}
}

fn (mut g Gen) gen_keyword_operator(node ast.KeywordOperator) {
	match node.op {
		.key_sizeof {
			if node.exprs.len > 0 {
				g.sb.write_string('sizeof(')
				g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
				g.sb.write_string(')')
			} else {
				g.sb.write_string('0')
			}
		}
		.key_typeof {
			// typeof should be lowered to StringLiteral by the transformer.
			// Fallback: emit a placeholder string.
			if node.exprs.len > 0 {
				mut type_name := ''
				if raw_type := g.get_raw_type(node.exprs[0]) {
					type_name = g.types_type_to_v(raw_type)
				}
				if type_name == '' {
					type_name = g.expr_type_to_c(node.exprs[0])
				}
				g.sb.write_string(c_static_v_string_expr(type_name))
			} else {
				g.sb.write_string(c_empty_v_string_expr())
			}
		}
		.key_offsetof {
			if node.exprs.len >= 2 {
				g.sb.write_string('offsetof(')
				g.sb.write_string(g.expr_type_to_c(node.exprs[0]))
				g.sb.write_string(', ')
				field_expr := node.exprs[1]
				if field_expr is ast.Ident {
					g.sb.write_string(field_expr.name)
				} else {
					g.expr(field_expr)
				}
				g.sb.write_string(')')
			} else {
				g.sb.write_string('0')
			}
		}
		.key_isreftype {
			g.sb.write_string('0')
		}
		.key_likely {
			if node.exprs.len > 0 {
				g.sb.write_string('__builtin_expect((')
				g.expr(node.exprs[0])
				g.sb.write_string('), 1)')
			} else {
				g.sb.write_string('1')
			}
		}
		.key_unlikely {
			if node.exprs.len > 0 {
				g.sb.write_string('__builtin_expect((')
				g.expr(node.exprs[0])
				g.sb.write_string('), 0)')
			} else {
				g.sb.write_string('0')
			}
		}
		.key_dump {
			// dump(expr) - just evaluate the expression
			if node.exprs.len > 0 {
				g.expr(node.exprs[0])
			} else {
				g.sb.write_string('0')
			}
		}
		.key_spawn {
			g.gen_spawn_expr(node)
		}
		.key_go {
			// go calls are lowered by the transformer to goroutines__goroutine_create.
			// If we reach here, fall back to spawn behavior.
			g.gen_spawn_expr(node)
		}
		else {
			g.sb.write_string('/* KeywordOperator: ${node.op} */ 0')
		}
	}
}

// gen_spawn_expr generates C code for `spawn call()` / `go call()`.
// This creates a pthread that executes the given function call in a new thread.
// For method calls: `spawn obj.method(args)` generates a wrapper struct containing
// the function pointer, receiver, and arguments, a wrapper function that unpacks
// them and calls the actual function, and a pthread_create call.
fn (mut g Gen) gen_spawn_expr(node ast.KeywordOperator) {
	if node.exprs.len == 0 {
		g.sb.write_string('0')
		return
	}
	spawn_call := node.exprs[0]
	mut call_lhs := ast.empty_expr
	mut call_args := []ast.Expr{}
	if spawn_call is ast.CallExpr {
		call_lhs = spawn_call.lhs
		call_args = spawn_call.args.clone()
	} else if spawn_call is ast.CallOrCastExpr {
		call_lhs = spawn_call.lhs
		call_args = [spawn_call.expr]
	} else {
		g.sb.write_string('/* spawn: unsupported expr type */ 0')
		return
	}
	// Resolve the C function name
	c_name := g.resolve_call_name(call_lhs, call_args.len)
	if c_name == '' {
		g.sb.write_string('/* spawn: could not resolve fn name */ 0')
		return
	}
	// Determine if this is a method call (lhs is SelectorExpr)
	mut is_method := false
	mut receiver_expr := ast.empty_expr
	if call_lhs is ast.SelectorExpr {
		sel := call_lhs as ast.SelectorExpr
		sel_lhs := sel.lhs
		if !(sel_lhs is ast.Ident && g.is_module_ident((sel_lhs as ast.Ident).name)) {
			is_method = true
			receiver_expr = sel_lhs
		}
	}
	// Build a unique name for the wrapper (deduplication key)
	wrapper_name := c_name.replace('__', '_')
	wrapper_struct_name := 'thread_arg_${wrapper_name}'
	wrapper_fn_name := '${wrapper_name}_thread_wrapper'
	// Emit the wrapper struct and function if not already done
	if wrapper_name !in g.spawned_fns {
		g.spawned_fns[wrapper_name] = true
		sig_param_types := g.fn_param_types[c_name] or { []string{} }
		mut sig_idx := 0
		// Get the receiver C type
		mut receiver_c_type := ''
		if is_method {
			if sig_idx < sig_param_types.len {
				receiver_c_type = normalize_signature_type_name(sig_param_types[sig_idx], '')
			}
			if receiver_c_type == '' {
				receiver_c_type = g.get_expr_type(receiver_expr)
			}
			if receiver_c_type == '' || receiver_c_type == 'int' {
				receiver_c_type = g.method_receiver_base_type(receiver_expr) + '*'
			}
			sig_idx++
		}
		// Get argument C types
		mut arg_c_types := []string{}
		for arg in call_args {
			mut arg_type := if sig_idx < sig_param_types.len {
				normalize_signature_type_name(sig_param_types[sig_idx], '')
			} else {
				''
			}
			if arg_type == '' {
				arg_type = g.get_expr_type(arg)
			}
			if arg_type == '' {
				arg_type = 'int'
			}
			arg_c_types << arg_type
			sig_idx++
		}
		// Build wrapper struct definition
		mut def := strings.new_builder(512)
		def.writeln('typedef struct ${wrapper_struct_name} {')
		if is_method {
			def.writeln('\t${receiver_c_type} arg0;')
		}
		for i, arg_type in arg_c_types {
			arg_idx := if is_method { i + 1 } else { i }
			def.writeln('\t${arg_type} arg${arg_idx};')
		}
		def.writeln('} ${wrapper_struct_name};')
		// Build wrapper function
		def.writeln('static void* ${wrapper_fn_name}(${wrapper_struct_name} *arg) {')
		def.write_string('\t${c_name}(')
		mut param_idx := 0
		if is_method {
			def.write_string('arg->arg0')
			param_idx++
		}
		for i, _ in call_args {
			if param_idx > 0 {
				def.write_string(', ')
			}
			arg_idx := if is_method { i + 1 } else { i }
			def.write_string('arg->arg${arg_idx}')
			param_idx++
		}
		def.writeln(');')
		def.writeln('\t${g.c_heap_free_call('arg')};')
		def.writeln('\treturn 0;')
		def.writeln('}')
		g.spawn_wrapper_defs << def.str()
	}
	// Generate the inline spawn code
	tmp := g.tmp_counter
	g.tmp_counter++
	arg_tmp := '_spawn_arg_${tmp}'
	spawn_arg_alloc := g.c_heap_malloc_call('sizeof(${wrapper_struct_name})')
	g.sb.writeln('({')
	g.write_indent()
	g.sb.writeln('\t${wrapper_struct_name} *${arg_tmp} = (${wrapper_struct_name}*)${spawn_arg_alloc};')
	if is_method {
		g.write_indent()
		g.sb.write_string('\t${arg_tmp}->arg0 = ')
		g.expr(receiver_expr)
		g.sb.writeln(';')
	}
	for i, arg in call_args {
		arg_idx := if is_method { i + 1 } else { i }
		g.write_indent()
		g.sb.write_string('\t${arg_tmp}->arg${arg_idx} = ')
		g.expr(arg)
		g.sb.writeln(';')
	}
	g.write_indent()
	g.sb.writeln('\tpthread_t _spawn_thr_${tmp};')
	g.write_indent()
	g.sb.writeln('\tpthread_create(&_spawn_thr_${tmp}, NULL, (void*)${wrapper_fn_name}, ${arg_tmp});')
	g.write_indent()
	g.sb.writeln('\tpthread_detach(_spawn_thr_${tmp});')
	g.write_indent()
	g.sb.write_string('\t0; })')
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}
