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
	files []ast.File
	env   &types.Environment = unsafe { nil }
	pref  &pref.Preferences  = unsafe { nil }
mut:
	sb                     strings.Builder
	indent                 int
	cur_fn_scope           &types.Scope = unsafe { nil }
	cur_fn_name            string
	cur_fn_ret_type        string
	cur_fn_c_ret_type      string
	cur_module             string
	emitted_types          map[string]bool
	fn_param_is_ptr        map[string][]bool
	fn_param_types         map[string][]string
	fn_return_types        map[string]string
	runtime_local_types    map[string]string
	cur_fn_returned_idents map[string]bool
	active_generic_types   map[string]types.Type

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
	emitted_result_structs      map[string]bool
	emitted_option_structs      map[string]bool
	embedded_field_owner        map[string]string
	collected_fixed_array_types map[string]FixedArrayInfo
	collected_map_types         map[string]MapTypeInfo
	fixed_array_ret_wrappers    map[string]string
	sum_type_variants           map[string][]string
	// Interface method signatures: interface_name -> [(method_name, cast_signature), ...]
	interface_methods           map[string][]InterfaceMethodInfo
	interface_data_fields       map[string][]InterfaceDataFieldInfo
	interface_decls             map[string]ast.InterfaceDecl
	emitted_interface_bodies    map[string]bool
	interface_wrapper_specs     map[string]InterfaceWrapperSpec
	needed_interface_wrappers   map[string]bool
	ierror_wrapper_bases        map[string]bool
	needed_ierror_wrapper_bases map[string]bool
	tmp_counter                 int
	cur_fn_mut_params           map[string]bool   // names of mut params in current function
	global_var_modules          map[string]string // global var name → module name
	global_var_types            map[string]string // global var name → C type string
	primitive_type_aliases      map[string]bool   // type names that are aliases for primitive types
	emit_modules                map[string]bool   // when set, emit consts/globals/fns only for these modules
	export_const_symbols        bool
	cache_bundle_name           string
	cached_init_calls           []string
	exported_const_seen         map[string]bool
	exported_const_symbols      []ExportedConstSymbol
	cur_file_name               string
	is_module_ident_cache       map[string]bool    // per-function cache for is_module_ident results
	not_local_var_cache         map[string]bool    // per-function negative cache for get_local_var_c_type
	resolved_module_names       map[string]string  // per-function cache for resolve_module_name
	cached_env_scopes           map[string]voidptr // cache of env_scope results (avoids repeated locking)

	const_exprs           map[string]string // const name → C expression string (for inlining)
	const_types           map[string]string // const name → C type string
	runtime_const_targets map[string]bool   // module-scoped consts initialized in __v_init_consts_*
	used_fn_keys          map[string]bool
	force_emit_fn_names   map[string]bool   // function C names that must be emitted regardless of mark_used
	export_fn_names       map[string]string // V-qualified name → export name (from @[export:] attribute)
	called_fn_names       map[string]bool
	generic_spec_index    map[string][]string // fn_name → matching keys in env.generic_types
	anon_fn_defs          []string            // lifted anonymous function definitions
	pass5_start_pos       int                 // position in sb where pass 5 starts
	deferred_m_includes   []string            // Objective-C .m file #include lines deferred until after type definitions
	spawned_fns           map[string]bool     // spawn wrapper names already emitted
	spawn_wrapper_defs    []string            // spawn wrapper struct + function definitions
	emitted_trampolines   map[string]bool     // bound method trampoline names already emitted
	trampoline_defs       []string            // bound method trampoline definitions
	// @[live] hot code reloading
	live_fns                []LiveFnInfo                     // @[live] functions detected during code generation
	live_source_file        string                           // source file containing @[live] functions
	test_fn_names           []string                         // test function names collected in Pass 4
	has_main                bool                             // whether a main() function was found in Pass 4
	fn_owner_file           map[string]int                   // fn_key -> first file index (for parallel dedup)
	global_owner_file       map[string]int                   // global_name -> first file index (for parallel dedup)
	generic_struct_bindings map[string]map[string]types.Type // struct_name -> {T: concrete_type}
	c_file_fn_keys          map[string]bool                  // fn_key -> emitted from a .c.v file, so plain .v fallback should be skipped
	typedef_c_types         map[string]bool                  // C struct names with @[typedef] attribute (emit without 'struct' prefix)
	blocked_fn_keys         map[string]bool                  // worker-only fn keys reserved to other pass5 chunks
	cached_vhash            string // cached git short hash for @VHASH/@VCURRENTHASH
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
	decl ast.StructDecl
	mod  string
}

struct InterfaceDeclInfo {
	decl ast.InterfaceDecl
	mod  string
}

struct FixedArrayInfo {
	elem_type string
	size      int
}

struct GenericFnSpecialization {
	name          string
	generic_types map[string]types.Type
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
	return unsafe { (&u64(&stmt))[1] } != 0
}

fn expr_has_valid_data(expr ast.Expr) bool {
	return unsafe { (&u64(&expr))[1] } != 0
}

fn type_has_valid_data(typ types.Type) bool {
	return unsafe { (&u64(&typ))[1] } != 0
}

pub fn Gen.new(files []ast.File) &Gen {
	return Gen.new_with_env_and_pref(files, unsafe { nil }, unsafe { nil })
}

pub fn Gen.new_with_env(files []ast.File, env &types.Environment) &Gen {
	return Gen.new_with_env_and_pref(files, env, unsafe { nil })
}

pub fn Gen.new_with_env_and_pref(files []ast.File, env &types.Environment, p &pref.Preferences) &Gen {
	return &Gen{
		files:                  files
		env:                    unsafe { env }
		pref:                   unsafe { p }
		sb:                     strings.new_builder(10_000)
		fn_param_is_ptr:        map[string][]bool{}
		fn_param_types:         map[string][]string{}
		fn_return_types:        map[string]string{}
		runtime_local_types:    map[string]string{}
		cur_fn_returned_idents: map[string]bool{}
		active_generic_types:   map[string]types.Type{}

		fixed_array_fields:          map[string]bool{}
		fixed_array_field_elem:      map[string]string{}
		fixed_array_globals:         map[string]bool{}
		tuple_aliases:               map[string][]string{}
		struct_field_types:          map[string]string{}
		enum_value_to_enum:          map[string]string{}
		enum_type_fields:            map[string]map[string]bool{}
		array_aliases:               map[string]bool{}
		map_aliases:                 map[string]bool{}
		result_aliases:              map[string]bool{}
		option_aliases:              map[string]bool{}
		alias_base_types:            map[string]string{}
		emitted_result_structs:      map[string]bool{}
		emitted_option_structs:      map[string]bool{}
		embedded_field_owner:        map[string]string{}
		fixed_array_ret_wrappers:    map[string]string{}
		emit_modules:                map[string]bool{}
		exported_const_seen:         map[string]bool{}
		exported_const_symbols:      []ExportedConstSymbol{}
		emitted_interface_bodies:    map[string]bool{}
		interface_data_fields:       map[string][]InterfaceDataFieldInfo{}
		interface_wrapper_specs:     map[string]InterfaceWrapperSpec{}
		needed_interface_wrappers:   map[string]bool{}
		ierror_wrapper_bases:        map[string]bool{}
		needed_ierror_wrapper_bases: map[string]bool{}
		c_file_fn_keys:              map[string]bool{}
		runtime_const_targets:       map[string]bool{}
		used_fn_keys:                map[string]bool{}
		called_fn_names:             map[string]bool{}
		blocked_fn_keys:             map[string]bool{}
	}
}

fn (mut g Gen) gen_file(file ast.File) {
	g.set_file_module(file)
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
		g.set_file_module(file)
		stmt_ptr := &file.stmts[fi]
		fn_decl_ptr := &((*stmt_ptr) as ast.FnDecl)
		g.gen_fn_decl_ptr(fn_decl_ptr)
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
		// No @[live] functions — emit a no-op __v_live_init
		g.sb.writeln('')
		g.sb.writeln('void __v_live_init(void) {}')
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
		'join']
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
		|| normalized.ends_with('vlib/sokol/memory/memory.c.v')
}

fn (g &Gen) should_emit_module(module_name string) bool {
	if g.emit_modules.len == 0 {
		return true
	}
	return module_name in g.emit_modules
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
		return stage_start
	}
	now := sw.elapsed()
	g.print_cgen_step_time(true, scope, step, time.Duration(now - stage_start))
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
	g.collect_typedef_c_types()
	g.collect_module_type_names()
	g.collect_runtime_aliases()
	g.collect_generic_struct_bindings()
	// Force eventbus generic structs to use T=string binding.
	// Without full monomorphization, eventbus methods assume T=string
	// (see fn.v hardcoded eventbus workaround).
	string_binding := {
		'T': types.Type(types.string_)
	}
	for eb_name in ['eventbus__EventHandler', 'eventbus__EventBus', 'eventbus__Publisher',
		'eventbus__Subscriber', 'eventbus__Registry'] {
		g.generic_struct_bindings[eb_name] = string_binding.clone()
	}
	// Force stdatomic AtomicVal to T=f64 (methods use f64 return/param types,
	// but struct binding may have been set to a user-defined struct type).
	g.generic_struct_bindings['stdatomic__AtomicVal'] = {
		'T': types.Type(types.f64_)
	}
	g.collect_fn_signatures()
	g.collect_c_file_fn_keys()
	g.collect_runtime_const_targets()
	g.register_builder_methods()
	g.build_generic_spec_index()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start, 'setup')

	// Pre-collect all global variable names so they can be module-qualified
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.GlobalDecl {
				for field in stmt.fields {
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
						g.global_var_modules[field.name] = g.cur_module
					}
				}
			}
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'collect globals')

	// Pass 1: Forward declarations for all structs/unions/sumtypes/interfaces (needed for mutual references)
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
				name := g.get_struct_name(stmt)
				if name in g.emitted_types {
					continue
				}
				g.emitted_types[name] = true
				keyword := if stmt.is_union { 'union' } else { 'struct' }
				g.sb.writeln('typedef ${keyword} ${name} ${name};')
			} else if stmt is ast.TypeDecl {
				if stmt.variants.len > 0 {
					// Sum type needs forward struct declaration
					name := g.get_type_decl_name(stmt)
					if name !in g.emitted_types {
						g.emitted_types[name] = true
						g.sb.writeln('typedef struct ${name} ${name};')
					}
				}
			} else if stmt is ast.InterfaceDecl {
				name := g.get_interface_name(stmt)
				if name !in g.emitted_types {
					g.emitted_types[name] = true
					g.sb.writeln('typedef struct ${name} ${name};')
				}
			}
		}
	}
	g.sb.writeln('')
	g.emit_runtime_aliases()
	g.sb.writeln('')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 1 forward decls')

	// Pass 2: Emit type declarations in dependency-safe buckets.
	// Emit enums first, then type aliases/sum types.
	// Interface bodies are emitted later, after structs/tuple aliases are available.
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
	// Pre-scan type aliases and interfaces so tuple aliases used in function-pointer
	// typedefs (for example `fn () (int, int)`) are registered before we emit them.
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
			if stmt is ast.TypeDecl {
				if stmt.variants.len == 0 && stmt.base_type !is ast.EmptyExpr {
					g.gen_type_alias(stmt)
				} else if stmt.variants.len > 0 {
					g.gen_sum_type_decl(stmt)
				}
			}
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 2 type declarations')

	// Pass 3: Full struct definitions (use named struct/union to match forward decls)
	// Collect all struct decls, then emit in dependency order
	mut all_structs := []StructDeclInfo{}
	mut all_interfaces := []InterfaceDeclInfo{}
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
					decl: stmt
					mod:  g.cur_module
				}
			} else if stmt is ast.InterfaceDecl {
				for field in stmt.fields {
					_ = g.interface_method_info(field)
				}
				iface_c_name := if g.cur_module != '' && g.cur_module != 'builtin' {
					'${g.cur_module}__${stmt.name}'
				} else {
					stmt.name
				}
				g.interface_decls[iface_c_name] = stmt
				all_interfaces << InterfaceDeclInfo{
					decl: stmt
					mod:  g.cur_module
				}
			}
		}
	}
	// Emit structs with only primitive/resolved fields first, then the rest.
	// Interleave option/result wrapper emission as soon as their payload types are complete.
	// Also emit fixed array typedefs as soon as their element types are defined.
	// Repeat until no more progress (simple topo sort with wrapper side-effects).
	for info in all_structs {
		g.cur_module = info.mod
		if g.struct_is_leaf(info.decl) {
			g.gen_struct_decl(info.decl)
		}
	}
	// Emit fixed array typedefs whose element types are now defined (leaf structs).
	g.emit_deferred_fixed_array_aliases()
	for _ in 0 .. (all_structs.len * 2) {
		mut progressed := false
		for info in all_structs {
			g.cur_module = info.mod
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
			g.cur_module = info.mod
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
		g.cur_module = info.mod
		g.gen_interface_decl(info.decl)
	}
	// Retry any structs that were waiting on interface bodies.
	for _ in 0 .. all_structs.len {
		mut progressed := false
		for info in all_structs {
			g.cur_module = info.mod
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
		g.cur_module = info.mod
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
	// In cached-core builds, the body lives in the builtin cache unit and is
	// forward-declared (by top_level_c_decls) in the main TU.  In single-TU
	// builds, the body is emitted here directly.
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

	// Pass 4: Function forward declarations
	for fi, file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			if stmt is ast.FnDecl {
				if !g.should_emit_fn_decl(g.cur_module, stmt) {
					continue
				}
				if stmt.language == .js {
					continue
				}
				if stmt.language == .c && stmt.stmts.len == 0 {
					// C extern declarations — their prototypes come from #include/#insert headers.
					continue
				}
				if g.cur_module == 'eventbus' && stmt.is_method
					&& receiver_generic_param_names(stmt).len > 0 {
					prev_generic_types := g.active_generic_types.clone()
					string_types := {
						'T': types.Type(types.string_)
					}
					g.active_generic_types = string_types.clone()
					if stmt.name in ['subscribe_method', 'unsubscribe_method'] {
						spec_name := g.specialized_fn_name(stmt, string_types)
						if spec_name != '' {
							g.gen_fn_head_with_name(stmt, spec_name)
							g.sb.writeln(';')
							g.active_generic_types = prev_generic_types.clone()
							continue
						}
					} else {
						fn_name := g.get_fn_name(stmt)
						if fn_name != '' {
							g.gen_fn_head_with_name(stmt, fn_name)
							g.sb.writeln(';')
							g.active_generic_types = prev_generic_types.clone()
							continue
						}
					}
					g.active_generic_types = prev_generic_types.clone()
				}
				// Generic functions: emit as macros for known simple functions
				if g.generic_fn_param_names(stmt).len > 0 {
					specs := g.generic_fn_specializations(stmt)
					if specs.len > 0 {
						prev_generic_types := g.active_generic_types.clone()
						for spec in specs {
							g.active_generic_types = spec.generic_types.clone()
							g.gen_fn_head_with_name(stmt, spec.name)
							g.sb.writeln(';')
						}
						g.active_generic_types = prev_generic_types.clone()
					} else {
						gfn_name := g.get_fn_name(stmt)
						if gfn_name != '' {
							g.emit_generic_fn_macro(gfn_name, stmt)
						}
					}
					continue
				}
				fn_name := g.get_fn_name(stmt)
				if fn_name == '' {
					continue
				}
				fn_key := 'fn_${fn_name}'
				if g.should_skip_plain_v_fallback_fn(fn_key) {
					continue
				}
				if fn_name == 'main' {
					g.has_main = true
				}
				if stmt.name.starts_with('test_') && !stmt.is_method && stmt.typ.params.len == 0 {
					g.test_fn_names << fn_name
				}
				// Record first file index for each function (for parallel dedup)
				if fn_key !in g.fn_owner_file {
					g.fn_owner_file[fn_key] = fi
				}
				if g.env != unsafe { nil } {
					if fn_scope := g.env.get_fn_scope(g.cur_module, fn_name) {
						g.cur_fn_scope = fn_scope
					}
				}
				g.gen_fn_head(stmt)
				g.sb.writeln(';')
			}
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4 fn forward declarations')

	g.sb.writeln('')
	// Forward declaration for live reload init (always emitted — no-op if no @[live] functions)
	g.sb.writeln('void __v_live_init(void);')
	g.emit_cached_init_call_decls()
	g.emit_ierror_wrapper_decls()
	g.collect_interface_wrapper_specs()
	g.emit_interface_method_wrapper_decls()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4 helper declarations')

	// Pass 4.5: Emit constants after function forward declarations so const
	// initializers can reference functions by name (for example function-pointer
	// tables).
	for file in g.files {
		g.set_file_module(file)
		if !g.should_emit_module(g.cur_module) {
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
	g.sb.writeln('')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4.5 const declarations')

	// Emit deferred Objective-C .m includes now that all types are defined.
	g.emit_deferred_m_includes()
}

fn (mut g Gen) collect_c_file_fn_keys() {
	g.c_file_fn_keys = map[string]bool{}
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
		// Call module init() functions and __v_init_consts_main — test files have
		// no main() function, so the transformer's injected init calls are not present.
		for fn_name, _ in g.fn_return_types {
			// Module init functions: MODULE__init (e.g., rand__init)
			// Skip methods (Type__method patterns where Type is capitalized)
			if fn_name.ends_with('__init') && fn_name.count('__') == 1 {
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
		// Call all module const initialization functions
		for fn_name, _ in g.fn_return_types {
			if fn_name.contains('__v_init_consts_') {
				g.sb.writeln('\t${fn_name}();')
			}
		}
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

	g.emit_missing_array_contains_fallbacks()
	g.emit_missing_runtime_fallbacks()
	g.emit_cached_module_init_function()
	g.emit_exported_const_symbols()

	mut out := ''
	if g.anon_fn_defs.len > 0 || g.spawn_wrapper_defs.len > 0 || g.trampoline_defs.len > 0 {
		full := g.sb.str()
		mut out_sb := strings.new_builder(full.len + 4096)
		unsafe { out_sb.write_ptr(full.str, g.pass5_start_pos) }
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
		if g.pass5_start_pos < full.len {
			unsafe { out_sb.write_ptr(full.str + g.pass5_start_pos, full.len - g.pass5_start_pos) }
		}
		out = out_sb.str()
	} else {
		out = g.sb.str()
	}
	return out
}

// gen_pass5 generates Pass 5 (function bodies, globals, etc.) sequentially.
fn (mut g Gen) gen_pass5() {
	g.pass5_start_pos = g.sb.len
	g.collect_force_emit_str_fns()
	// Pre-pass: emit extern forward declarations for all globals across all modules
	for file in g.files {
		g.set_file_module(file)
		g.gen_file_extern_globals(file)
	}
	for file in g.files {
		g.set_file_module(file)
		if !g.should_emit_module(g.cur_module) {
			g.gen_file_extern_consts(file)
			continue
		}
		g.gen_file(file)
	}
	g.gen_pass5_post()
}

// gen_pass5_pre runs Pass 5 sequential pre-work: extern globals and extern consts
// for non-emitted modules. Returns the list of file indices that need gen_file().
pub fn (mut g Gen) gen_pass5_pre() []int {
	g.pass5_start_pos = g.sb.len
	g.collect_force_emit_str_fns()
	for file in g.files {
		g.set_file_module(file)
		g.gen_file_extern_globals(file)
	}
	// Emit extern consts for non-emitted modules, collect emittable file indices.
	// Also build global_owner_file: assign each global to the first file that declares it
	// so parallel workers can avoid emitting duplicate definitions.
	mut emit_indices := []int{cap: g.files.len}
	for fi, file in g.files {
		g.set_file_module(file)
		if !g.should_emit_module(g.cur_module) {
			g.gen_file_extern_consts(file)
		} else {
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

// gen_pass5_post runs post-Pass-5 finalization (interface wrappers, live reload, map helpers).
pub fn (mut g Gen) gen_pass5_post() {
	g.emit_needed_ierror_wrappers()
	g.emit_needed_interface_method_wrappers()
	g.emit_live_reload_infrastructure()
	if g.cache_bundle_name.len == 0 {
		g.emit_map_str_functions()
		g.emit_map_eq_functions()
	}
}

// gen_pass5_files generates function bodies for a range of file indices.
// Used by parallel dispatch — each worker calls this with its assigned chunk.
pub fn (mut g Gen) gen_pass5_files(file_indices []int) {
	for fi in file_indices {
		g.gen_file(g.files[fi])
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
		files: g.files
		env:   unsafe { g.env }
		pref:  unsafe { g.pref }
		sb:    strings.new_builder(64_000)
		// Read-only lookup maps — clone to avoid COW data races
		fn_param_is_ptr:             g.fn_param_is_ptr.clone()
		fn_param_types:              g.fn_param_types.clone()
		fn_return_types:             g.fn_return_types.clone()
		struct_field_types:          g.struct_field_types.clone()
		enum_value_to_enum:          g.enum_value_to_enum.clone()
		enum_type_fields:            g.enum_type_fields.clone()
		array_aliases:               g.array_aliases.clone()
		map_aliases:                 g.map_aliases.clone()
		result_aliases:              g.result_aliases.clone()
		option_aliases:              g.option_aliases.clone()
		alias_base_types:            g.alias_base_types.clone()
		fixed_array_fields:          g.fixed_array_fields.clone()
		fixed_array_field_elem:      g.fixed_array_field_elem.clone()
		fixed_array_globals:         g.fixed_array_globals.clone()
		fixed_array_ret_wrappers:    g.fixed_array_ret_wrappers.clone()
		tuple_aliases:               g.tuple_aliases.clone()
		sum_type_variants:           g.sum_type_variants.clone()
		embedded_field_owner:        g.embedded_field_owner.clone()
		primitive_type_aliases:      g.primitive_type_aliases.clone()
		emit_modules:                g.emit_modules.clone()
		emitted_result_structs:      g.emitted_result_structs.clone()
		emitted_option_structs:      g.emitted_option_structs.clone()
		interface_methods:           g.interface_methods.clone()
		interface_data_fields:       g.interface_data_fields.clone()
		emitted_interface_bodies:    g.emitted_interface_bodies.clone()
		interface_wrapper_specs:     g.interface_wrapper_specs.clone()
		ierror_wrapper_bases:        g.ierror_wrapper_bases.clone()
		collected_fixed_array_types: g.collected_fixed_array_types.clone()
		collected_map_types:         g.collected_map_types.clone()
		c_file_fn_keys:              g.c_file_fn_keys.clone()
		global_var_modules:          g.global_var_modules.clone()
		const_exprs:                 g.const_exprs.clone()
		const_types:                 g.const_types.clone()
		runtime_const_targets:       g.runtime_const_targets.clone()
		used_fn_keys:                g.used_fn_keys.clone()
		force_emit_fn_names:         g.force_emit_fn_names.clone()
		export_fn_names:             g.export_fn_names.clone()
		called_fn_names:             g.called_fn_names.clone()
		generic_spec_index:          g.generic_spec_index.clone()
		typedef_c_types:             g.typedef_c_types.clone()
		// Per-worker mutable state (starts fresh).
		// Each worker gets a unique tmp_counter offset to avoid name collisions
		// for generated trampolines (_bound_method_N, _bound_recv_N, etc.).
		tmp_counter:                 (worker_id + 1) * 100_000
		emitted_types:               worker_emitted
		blocked_fn_keys:             blocked_fn_keys
		runtime_local_types:         map[string]string{}
		cur_fn_returned_idents:      map[string]bool{}
		active_generic_types:        map[string]types.Type{}
		is_module_ident_cache:       map[string]bool{}
		not_local_var_cache:         map[string]bool{}
		resolved_module_names:       map[string]string{}
		cur_fn_mut_params:           map[string]bool{}
		cached_env_scopes:           map[string]voidptr{}
		needed_interface_wrappers:   map[string]bool{}
		needed_ierror_wrapper_bases: map[string]bool{}
		spawned_fns:                 map[string]bool{}
		exported_const_seen:         map[string]bool{}
		exported_const_symbols:      []ExportedConstSymbol{}
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
	// Merge accumulator maps
	for k, v in w.needed_interface_wrappers {
		g.needed_interface_wrappers[k] = v
	}
	for k, v in w.needed_ierror_wrapper_bases {
		g.needed_ierror_wrapper_bases[k] = v
	}
	for k, v in w.called_fn_names {
		g.called_fn_names[k] = v
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

fn (mut g Gen) emit_missing_runtime_fallbacks() {
	if 'fn___at_least_one' !in g.emitted_types {
		g.emitted_types['fn___at_least_one'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) u64 __at_least_one(u64 how_many) {')
		g.sb.writeln('\treturn how_many == 0 ? 1 : how_many;')
		g.sb.writeln('}')
	}
	if 'fn_arguments' !in g.emitted_types {
		g.emitted_types['fn_arguments'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) Array_string arguments() {')
		g.sb.writeln('\tu8** argv = (u8**)g_main_argv;')
		g.sb.writeln('\tArray_string res = __new_array_with_default_noscan(0, g_main_argc, sizeof(string), NULL);')
		g.sb.writeln('\tfor (int i = 0; i < g_main_argc; i += 1) {')
		g.sb.writeln('\t\tarray__push((array*)&res, &(string[1]){tos_clone(argv[i])});')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn res;')
		g.sb.writeln('}')
	}
	if 'fn__write_buf_to_fd' !in g.emitted_types {
		g.emitted_types['fn__write_buf_to_fd'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void _write_buf_to_fd(int fd, u8* buf, int buf_len) {')
		g.sb.writeln('\tif (buf_len <= 0) {')
		g.sb.writeln('\t\treturn;')
		g.sb.writeln('\t}')
		g.sb.writeln('\tu8* ptr = buf;')
		g.sb.writeln('\tisize remaining_bytes = buf_len;')
		g.sb.writeln('\twhile (remaining_bytes > 0) {')
		g.sb.writeln('\t\tisize written = write(fd, ptr, remaining_bytes);')
		g.sb.writeln('\t\tif (written <= 0) {')
		g.sb.writeln('\t\t\treturn;')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t\tptr += written;')
		g.sb.writeln('\t\tremaining_bytes -= written;')
		g.sb.writeln('\t}')
		g.sb.writeln('}')
	}
	if 'fn__writeln_to_fd' !in g.emitted_types {
		g.emitted_types['fn__writeln_to_fd'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void _writeln_to_fd(int fd, string s) {')
		g.sb.writeln("\tu8 lf = '\\n';")
		g.sb.writeln('\t_write_buf_to_fd(fd, s.str, s.len);')
		g.sb.writeln('\t_write_buf_to_fd(fd, &lf, 1);')
		g.sb.writeln('}')
	}
	if 'fn_Array_int_contains' !in g.emitted_types {
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
	if 'fn_Array_string_contains' !in g.emitted_types {
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
	if 'fn_Array_string_index' !in g.emitted_types {
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
	if 'fn_Array_int_str' !in g.emitted_types {
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
	if 'fn_DenseArray__zeros_to_end' !in g.emitted_types {
		g.emitted_types['fn_DenseArray__zeros_to_end'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void DenseArray__zeros_to_end(DenseArray* d) {')
		g.sb.writeln('\tvoid* tmp_value = malloc(d->value_bytes);')
		g.sb.writeln('\tvoid* tmp_key = malloc(d->key_bytes);')
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
		g.sb.writeln('\tfree(tmp_value);')
		g.sb.writeln('\tfree(tmp_key);')
		g.sb.writeln('\td->deletes = 0;')
		g.sb.writeln('\tfree(d->all_deleted);')
		g.sb.writeln('\td->len = count;')
		g.sb.writeln('\tint old_cap = d->cap;')
		g.sb.writeln('\td->cap = count < 8 ? 8 : count;')
		g.sb.writeln('\td->values = realloc_data(d->values, d->value_bytes * old_cap, d->value_bytes * d->cap);')
		g.sb.writeln('\td->keys = realloc_data(d->keys, d->key_bytes * old_cap, d->key_bytes * d->cap);')
		g.sb.writeln('}')
	}
	if 'fn___sort_cmp_int_asc' !in g.emitted_types {
		g.emitted_types['fn___sort_cmp_int_asc'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int __sort_cmp_int_asc(int* a, int* b) {')
		g.sb.writeln('\tif (*a < *b) return -1;')
		g.sb.writeln('\tif (*a > *b) return 1;')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}
	if 'fn___sort_cmp_RepIndex_by_idx_asc' !in g.emitted_types {
		g.emitted_types['fn___sort_cmp_RepIndex_by_idx_asc'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int __sort_cmp_RepIndex_by_idx_asc(RepIndex* a, RepIndex* b) {')
		g.sb.writeln('\tif (a->idx < b->idx) return -1;')
		g.sb.writeln('\tif (a->idx > b->idx) return 1;')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}
	if 'fn_bits__leading_zeros_64' !in g.emitted_types {
		g.emitted_types['fn_bits__leading_zeros_64'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int bits__leading_zeros_64(u64 x) {')
		g.sb.writeln('\tif (x == 0) {')
		g.sb.writeln('\t\treturn 64;')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn __builtin_clzll(x);')
		g.sb.writeln('}')
	}
	if 'fn_bits__trailing_zeros_32' !in g.emitted_types {
		g.emitted_types['fn_bits__trailing_zeros_32'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int bits__trailing_zeros_32(u32 x) {')
		g.sb.writeln('\tif (x == 0) {')
		g.sb.writeln('\t\treturn 32;')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn __builtin_ctz(x);')
		g.sb.writeln('}')
	}
	if 'fn_bits__trailing_zeros_64' !in g.emitted_types {
		g.emitted_types['fn_bits__trailing_zeros_64'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) int bits__trailing_zeros_64(u64 x) {')
		g.sb.writeln('\tif (x == 0) {')
		g.sb.writeln('\t\treturn 64;')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn __builtin_ctzll(x);')
		g.sb.writeln('}')
	}
	if 'fn_bits__rotate_left_32' !in g.emitted_types {
		g.emitted_types['fn_bits__rotate_left_32'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) u32 bits__rotate_left_32(u32 x, int k) {')
		g.sb.writeln('\tu32 s = ((u32)k) & 31;')
		g.sb.writeln('\treturn (x << s) | (x >> ((32 - s) & 31));')
		g.sb.writeln('}')
	}
	if 'fn_eprint' !in g.emitted_types {
		g.emitted_types['fn_eprint'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void eprint(string s) {')
		g.sb.writeln('\tflush_stdout();')
		g.sb.writeln('\tflush_stderr();')
		g.sb.writeln('\t_write_buf_to_fd(2, s.str, s.len);')
		g.sb.writeln('\tflush_stderr();')
		g.sb.writeln('}')
	}
	if 'fn_flush_stdout' !in g.emitted_types {
		g.emitted_types['fn_flush_stdout'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void flush_stdout() {')
		g.sb.writeln('\tfflush(stdout);')
		g.sb.writeln('}')
	}
	if 'fn_flush_stderr' !in g.emitted_types {
		g.emitted_types['fn_flush_stderr'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void flush_stderr() {')
		g.sb.writeln('\tfflush(stderr);')
		g.sb.writeln('}')
	}
	if 'fn_malloc_noscan' !in g.emitted_types {
		g.emitted_types['fn_malloc_noscan'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) u8* malloc_noscan(isize n) {')
		g.sb.writeln('\treturn malloc(n);')
		g.sb.writeln('}')
	}
	if 'fn_memdup' !in g.emitted_types {
		g.emitted_types['fn_memdup'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) void* memdup(void* src, isize sz) {')
		g.sb.writeln('\tif (sz <= 0) {')
		g.sb.writeln('\t\treturn NULL;')
		g.sb.writeln('\t}')
		g.sb.writeln('\tvoid* res = malloc_noscan(sz);')
		g.sb.writeln('\tmemcpy(res, src, sz);')
		g.sb.writeln('\treturn res;')
		g.sb.writeln('}')
	}
	if 'fn_f64_abs' !in g.emitted_types {
		g.emitted_types['fn_f64_abs'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) f64 f64_abs(f64 a) {')
		g.sb.writeln('\treturn a < 0 ? -a : a;')
		g.sb.writeln('}')
	}
	if 'fn_f64__strg' !in g.emitted_types {
		g.emitted_types['fn_f64__strg'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string f64__strg(f64 x) {')
		g.sb.writeln('\treturn f64__str(x);')
		g.sb.writeln('}')
	}
	if 'fn_f32__str' !in g.emitted_types {
		g.emitted_types['fn_f32__str'] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) string f32__str(f32 x) {')
		g.sb.writeln('\treturn f64__str((f64)x);')
		g.sb.writeln('}')
	}
	if 'fn_f32__strg' !in g.emitted_types {
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

fn sanitize_fn_ident(name string) string {
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
		// Get the receiver C type
		mut receiver_c_type := ''
		if is_method {
			receiver_c_type = g.get_expr_type(receiver_expr)
			if receiver_c_type == '' || receiver_c_type == 'int' {
				receiver_c_type = g.method_receiver_base_type(receiver_expr) + '*'
			}
		}
		// Get argument C types
		mut arg_c_types := []string{}
		for arg in call_args {
			mut arg_type := g.get_expr_type(arg)
			if arg_type == '' {
				arg_type = 'int'
			}
			arg_c_types << arg_type
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
		def.writeln('\tfree(arg);')
		def.writeln('\treturn 0;')
		def.writeln('}')
		g.spawn_wrapper_defs << def.str()
	}
	// Generate the inline spawn code
	tmp := g.tmp_counter
	g.tmp_counter++
	arg_tmp := '_spawn_arg_${tmp}'
	g.sb.writeln('({')
	g.write_indent()
	g.sb.writeln('\t${wrapper_struct_name} *${arg_tmp} = (${wrapper_struct_name}*)malloc(sizeof(${wrapper_struct_name}));')
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
