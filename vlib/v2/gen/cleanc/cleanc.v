// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.pref
import v2.types
import strings
import time

pub struct Gen {
	files []ast.File
	env   &types.Environment = unsafe { nil }
	pref  &pref.Preferences  = unsafe { nil }
mut:
	sb                  strings.Builder
	indent              int
	cur_fn_scope        &types.Scope = unsafe { nil }
	cur_fn_name         string
	cur_fn_ret_type     string
	cur_module          string
	emitted_types       map[string]bool
	fn_param_is_ptr     map[string][]bool
	fn_param_types      map[string][]string
	fn_return_types     map[string]string
	runtime_local_types map[string]string

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
	emitted_result_structs      map[string]bool
	emitted_option_structs      map[string]bool
	embedded_field_owner        map[string]string
	collected_fixed_array_types map[string]FixedArrayInfo
	collected_map_types         map[string]MapTypeInfo
	sum_type_variants           map[string][]string
	// Interface method signatures: interface_name -> [(method_name, cast_signature), ...]
	interface_methods           map[string][]InterfaceMethodInfo
	interface_wrapper_specs     map[string]InterfaceWrapperSpec
	needed_interface_wrappers   map[string]bool
	ierror_wrapper_bases        map[string]bool
	needed_ierror_wrapper_bases map[string]bool
	tmp_counter                 int
	cur_fn_mut_params           map[string]bool   // names of mut params in current function
	global_var_modules          map[string]string // global var name → module name
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

	const_exprs     map[string]string // const name → C expression string (for inlining)
	used_fn_keys    map[string]bool
	called_fn_names map[string]bool
	anon_fn_defs    []string // lifted anonymous function definitions
	pass5_start_pos int      // position in sb where pass 5 starts
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

struct FixedArrayInfo {
	elem_type string
	size      int
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

pub fn Gen.new(files []ast.File) &Gen {
	return Gen.new_with_env_and_pref(files, unsafe { nil }, unsafe { nil })
}

pub fn Gen.new_with_env(files []ast.File, env &types.Environment) &Gen {
	return Gen.new_with_env_and_pref(files, env, unsafe { nil })
}

pub fn Gen.new_with_env_and_pref(files []ast.File, env &types.Environment, p &pref.Preferences) &Gen {
	return &Gen{
		files:               files
		env:                 unsafe { env }
		pref:                unsafe { p }
		sb:                  strings.new_builder(10_000)
		fn_param_is_ptr:     map[string][]bool{}
		fn_param_types:      map[string][]string{}
		fn_return_types:     map[string]string{}
		runtime_local_types: map[string]string{}

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
		emitted_result_structs:      map[string]bool{}
		emitted_option_structs:      map[string]bool{}
		embedded_field_owner:        map[string]string{}
		emit_modules:                map[string]bool{}
		exported_const_seen:         map[string]bool{}
		exported_const_symbols:      []ExportedConstSymbol{}
		interface_wrapper_specs:     map[string]InterfaceWrapperSpec{}
		needed_interface_wrappers:   map[string]bool{}
		ierror_wrapper_bases:        map[string]bool{}
		needed_ierror_wrapper_bases: map[string]bool{}
		used_fn_keys:                map[string]bool{}
		called_fn_names:             map[string]bool{}
	}
}

fn (mut g Gen) gen_file(file ast.File) {
	g.set_file_module(file)
	for stmt in file.stmts {
		// Skip struct/enum/type/interface/const decls - already emitted in earlier passes
		if stmt is ast.StructDecl || stmt is ast.EnumDecl || stmt is ast.TypeDecl
			|| stmt is ast.ConstDecl || stmt is ast.InterfaceDecl {
			continue
		}
		g.gen_stmt(stmt)
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

fn should_keep_builtin_map_decl(decl ast.FnDecl) bool {
	base_keep := decl.name in ['new_map', 'move', 'clear', 'key_to_index', 'meta_less',
		'meta_greater', 'ensure_extra_metas', 'set', 'expand', 'rehash', 'reserve', 'cached_rehash',
		'get_and_set', 'get', 'get_check', 'exists', 'delete', 'keys', 'values', 'clone', 'free',
		'key', 'value', 'has_index', 'zeros_to_end', 'str']
	return base_keep || decl.name.starts_with('map_eq_') || decl.name.starts_with('map_clone_')
		|| decl.name.starts_with('map_free_')
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
		|| normalized.ends_with('vlib/builtin/chan_option_result.v')
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

// gen generates C source from the transformed AST files.
pub fn (mut g Gen) gen() string {
	stats_enabled := g.cgen_stats_enabled()
	stats_scope := g.cgen_stats_scope_label()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()

	g.write_preamble()
	g.collect_module_type_names()
	g.collect_runtime_aliases()
	g.collect_fn_signatures()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup')

	// Pre-collect all global variable names so they can be module-qualified
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
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

	// Pass 2: Enum declarations, type aliases, interface structs, and sum type structs
	// (before struct definitions that may reference them)
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				g.gen_enum_decl(stmt)
			} else if stmt is ast.TypeDecl {
				if stmt.variants.len == 0 && stmt.base_type !is ast.EmptyExpr {
					g.gen_type_alias(stmt)
				} else if stmt.variants.len > 0 {
					g.gen_sum_type_decl(stmt)
				}
			} else if stmt is ast.InterfaceDecl {
				g.gen_interface_decl(stmt)
			}
		}
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 2 type declarations')

	// Pass 3: Full struct definitions (use named struct/union to match forward decls)
	// Collect all struct decls, then emit in dependency order
	mut all_structs := []StructDeclInfo{}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language == .c {
					continue
				}
				all_structs << StructDeclInfo{
					decl: stmt
					mod:  g.cur_module
				}
			}
		}
	}
	// Emit structs with only primitive/resolved fields first, then the rest.
	// Interleave option/result wrapper emission as soon as their payload types are complete.
	// Repeat until no more progress (simple topo sort with wrapper side-effects).
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
		if !progressed {
			break
		}
	}
	// Emit any remaining structs (circular deps - just emit them)
	for info in all_structs {
		g.cur_module = info.mod
		g.gen_struct_decl(info.decl)
	}
	_ = g.emit_ready_option_result_structs()
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

	// Pass 3.3: Emit option/result struct definitions (needs IError + tuple types defined)
	g.emit_option_result_structs()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.3 option/result structs')

	// Pass 3.5: Emit constants before function declarations/bodies, so macros are available.
	for file in g.files {
		g.set_file_module(file)
		if !g.should_emit_module(g.cur_module) {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.ConstDecl {
				g.gen_const_decl(stmt)
			}
		}
	}
	g.sb.writeln('')
	// Recursive array equality helper for nested arrays and string arrays
	g.sb.writeln('bool string__eq(string a, string b);')
	g.sb.writeln('static inline bool __v2_array_eq(array a, array b) {')
	g.sb.writeln('	if (a.len != b.len || a.element_size != b.element_size) return false;')
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
	g.sb.writeln('')
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 3.5 const declarations')

	// Pass 4: Function forward declarations
	mut test_fn_names := []string{}
	mut has_main := false
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				if !g.should_emit_fn_decl(g.cur_module, stmt) {
					continue
				}
				if stmt.language == .js {
					continue
				}
				if stmt.language == .c && stmt.stmts.len == 0 {
					continue
				}
				// Skip generic functions - they have unresolved type params
				if stmt.typ.generic_params.len > 0 {
					continue
				}
				fn_name := g.get_fn_name(stmt)
				if fn_name == '' {
					continue
				}
				if fn_name == 'main' {
					has_main = true
				}
				if stmt.name.starts_with('test_') && !stmt.is_method && stmt.typ.params.len == 0 {
					test_fn_names << fn_name
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
	g.emit_cached_init_call_decls()
	g.emit_ierror_wrapper_decls()
	g.collect_interface_wrapper_specs()
	g.emit_interface_method_wrapper_decls()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 4 helper declarations')

	// Pass 5: Everything else (function bodies, consts, globals, etc.)
	g.pass5_start_pos = g.sb.len
	// Pre-pass: emit extern forward declarations for all globals across all modules
	// to avoid ordering issues (e.g. rand__default_rng used before its definition).
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
	g.emit_needed_ierror_wrappers()
	g.emit_needed_interface_method_wrappers()
	// Map str/eq functions are type-specific (Map_int_int_str, Map_string_int_map_eq, etc.)
	// and depend on which concrete map types the user program uses. They must be emitted
	// in the main compilation unit, NOT in the cache (which is shared across programs).
	if g.cache_bundle_name.len == 0 {
		g.emit_map_str_functions()
		g.emit_map_eq_functions()
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'pass 5 file bodies')

	// Generate test runner main if this is a test file (has test_ functions but no main)
	// Skip when generating cached module sources (cache_bundle_name is set) - main belongs only in the main module source
	if !has_main && test_fn_names.len > 0 && g.cache_bundle_name.len == 0 {
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
		if '__v_init_consts_main' in g.fn_return_types {
			g.sb.writeln('\t__v_init_consts_main();')
		}
		for test_fn in test_fn_names {
			msg_run := 'Running test: ${test_fn}...'
			msg_ok := '  OK'
			g.sb.writeln('\tprintln(${c_static_v_string_expr(msg_run)});')
			g.sb.writeln('\t${test_fn}();')
			g.sb.writeln('\tprintln(${c_static_v_string_expr(msg_ok)});')
		}
		msg_all := 'All ${test_fn_names.len} tests passed.'
		g.sb.writeln('\tprintln(${c_static_v_string_expr(msg_all)});')
		g.sb.writeln('\treturn 0;')
		g.sb.writeln('}')
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'test main synthesis')

	g.emit_missing_array_contains_fallbacks()
	g.emit_cached_module_init_function()
	g.emit_exported_const_symbols()
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'final helper emission')

	mut out := ''
	if g.anon_fn_defs.len > 0 {
		full := g.sb.str()
		mut out_sb := strings.new_builder(full.len + 4096)
		unsafe { out_sb.write_ptr(full.str, g.pass5_start_pos) }
		for def in g.anon_fn_defs {
			out_sb.write_string(def)
		}
		if g.pass5_start_pos < full.len {
			unsafe { out_sb.write_ptr(full.str + g.pass5_start_pos, full.len - g.pass5_start_pos) }
		}
		out = out_sb.str()
	} else {
		out = g.sb.str()
	}
	if stats_enabled {
		g.print_cgen_step_time(true, stats_scope, 'string materialization', time.Duration(stats_sw.elapsed() - stage_start))
		g.print_cgen_step_time(true, stats_scope, 'total', stats_sw.elapsed())
	}
	return out
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

fn is_c_runtime_function(name string) bool {
	return name in ['free', 'malloc', 'realloc', 'calloc', 'memcmp', 'memcpy', 'memmove', 'memset',
		'memchr', 'memmem', 'strlen', 'strcmp', 'strncmp', 'snprintf', 'sprintf', 'printf', 'fprintf',
		'asprintf', 'atoi', 'atoll', 'atof', 'qsort', 'popen', 'realpath', 'chmod', 'exit',
		'backtrace_symbols', 'backtrace_symbols_fd', 'proc_pidpath', 'wyhash', 'wyhash64']
}

fn shallow_copy_exprs(exprs []ast.Expr) []ast.Expr {
	mut out := []ast.Expr{cap: exprs.len}
	for expr in exprs {
		out << expr
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
	if name in c_keywords {
		return '_${name}'
	}
	return name
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
		else {
			g.sb.write_string('/* KeywordOperator: ${node.op} */ 0')
		}
	}
}

fn (mut g Gen) write_indent() {
	for _ in 0 .. g.indent {
		g.sb.write_string('\t')
	}
}
